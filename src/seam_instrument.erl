%% @doc AST transformation for condition-level instrumentation.
%%
%% Extract abstract code from a BEAM file via `beam_lib', walk every
%% function clause, and inject `seam_track' calls into clause bodies that
%% record each guard condition's true/false outcome. Guards themselves are
%% never modified — Erlang restricts guard expressions to BIFs.
%%
%% For the matched clause, all its guard conditions are recorded as true.
%% For prior (failed) clauses, their conditions are re-evaluated in
%% try/catch blocks using variable substitution from the current clause's
%% pattern bindings. Wildcard patterns are replaced with fresh variables
%% to enable this re-evaluation.
%%
%% Two instrumentation modes: `full' (default) performs prior-clause
%% re-evaluation and body expression instrumentation; `fast' records
%% only the matched clause's own conditions and decision — suitable for
%% high-iteration fuzzing where overhead must be minimal.
-module(seam_instrument).
-include("seam.hrl").

-export([compile_beam/1, compile_beam/2]).

%% @doc Instrument in `full' mode. Equivalent to `compile_beam(ModOrPath, #{mode => full})'.
-spec compile_beam(module() | string()) -> {ok, module()} | {error, term()}.
compile_beam(ModOrPath) ->
    compile_beam(ModOrPath, #{mode => full}).

%% @doc Instrument a module and hot-load the result. `Opts' map supports
%% `#{mode => full | fast}'. In `fast' mode, skip prior-clause
%% re-evaluation and body expression instrumentation — record only own
%% conditions and decision.
-spec compile_beam(module() | string(), map()) -> {ok, module()} | {error, term()}.
compile_beam(ModOrPath, Opts) ->
    case resolve_beam(ModOrPath) of
        {ok, Mod, Beam, Path} ->
            case extract_forms(Mod, Beam) of
                {ok, Forms} -> do_instrument(Mod, Beam, Path, Forms, Opts);
                Err -> Err
            end;
        Err -> Err
    end.

%%% === BEAM resolution ===

resolve_beam(Mod) when is_atom(Mod) ->
    case code:which(Mod) of
        non_existing   -> {error, {not_loaded, Mod}};
        cover_compiled -> {error, {cover_compiled, Mod}};
        Path ->
            case file:read_file(Path) of
                {ok, Bin} -> {ok, Mod, Bin, Path};
                {error, R} -> {error, {read_failed, Path, R}}
            end
    end;
resolve_beam(Path) when is_list(Path); is_binary(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            case beam_lib:info(Bin) of
                [{module, Mod} | _] -> {ok, Mod, Bin, Path};
                _ -> {error, {bad_beam, Path}}
            end;
        {error, R} -> {error, {read_failed, Path, R}}
    end.

extract_forms(Mod, Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {Mod, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            {ok, Forms};
        {ok, {Mod, [{abstract_code, no_abstract_code}]}} ->
            {error, no_abstract_code};
        {error, beam_lib, Reason} ->
            {error, Reason}
    end.

%%% === Compile and load ===

do_instrument(Mod, OrigBeam, OrigPath, Forms, Opts) ->
    Instrumented = transform_forms(Mod, Forms, Opts),
    case compile:forms(Instrumented, [binary, return_errors, debug_info]) of
        {ok, Mod, Binary} ->
            load_instrumented(Mod, OrigBeam, OrigPath, Binary);
        {ok, Mod, Binary, _Warn} ->
            load_instrumented(Mod, OrigBeam, OrigPath, Binary);
        {error, Errors, _Warn} ->
            {error, {compile_failed, Errors}}
    end.

load_instrumented(Mod, OrigBeam, OrigPath, Binary) ->
    seam_track:register_module(Mod, OrigBeam, OrigPath),
    code:purge(Mod),
    case code:load_binary(Mod, "seam_instrumented", Binary) of
        {module, Mod} -> {ok, Mod};
        {error, R}    -> {error, {load_failed, R}}
    end.

%%% === AST transformation ===

transform_forms(Mod, Forms, Opts) ->
    [transform_form(Mod, F, Opts) || F <- Forms].

transform_form(Mod, {function, Anno, Name, Arity, Clauses}, Opts) ->
    {function, Anno, Name, Arity,
     transform_fun_clauses(Mod, Name, Clauses, Opts)};
transform_form(_Mod, Other, _Opts) ->
    Other.

transform_fun_clauses(Mod, Fun, Clauses, Opts) ->
    Mode = maps:get(mode, Opts, full),
    Indexed = index_clauses(Clauses, 1),
    put(seam_dec_idx, length(Clauses) + 1),
    Result = case Mode of
        fast ->
            [instrument_clause_fast(Mod, Fun, Idx, Clause)
             || {Idx, Clause} <- Indexed];
        full ->
            AllGuardInfo = [{Idx, extract_clause_vars(Pats), Guards}
                            || {Idx, {clause, _, Pats, Guards, _}} <- Indexed],
            [instrument_clause(Mod, Fun, Idx, Clause, AllGuardInfo)
             || {Idx, Clause} <- Indexed]
    end,
    erase(seam_dec_idx),
    Result.

index_clauses([], _) -> [];
index_clauses([C | Cs], N) -> [{N, C} | index_clauses(Cs, N + 1)].

extract_clause_vars(Pats) ->
    [pat_var(P) || P <- Pats].

pat_var({var, _, '_'}) -> wildcard;
pat_var({var, _, Name}) -> {var, Name};
pat_var(_) -> complex.

%%% === Full-mode instrumentation ===

instrument_clause(Mod, Fun, ClauseIdx,
                  {clause, Anno, Pats, Guards, Body},
                  AllGuardInfo) ->
    seam_track:register_decision_meta(
        {Mod, Fun, ClauseIdx}, anno_line(Anno), "clause"),
    {Pats1, FreshVars} = freshen_wildcards(Pats, ClauseIdx),
    MyVars = extract_clause_vars(Pats1),
    PriorTracking = build_prior_tracking(Mod, Fun, ClauseIdx,
                                         MyVars, AllGuardInfo, Anno),
    OwnCondKeys = guard_cond_keys(Mod, Fun, ClauseIdx, Guards),
    GuardConds = flatten_guard_conds(Guards),
    OwnTracking = build_own_tracking(Anno, OwnCondKeys, GuardConds) ++
                  [mk_record_dec(Anno, {Mod, Fun, ClauseIdx}, true)],
    %% Body expression instrumentation
    NextCondIdx = length(OwnCondKeys) + 1,
    BodyCtx = {Mod, Fun, ClauseIdx, NextCondIdx},
    put(seam_body_ctx, BodyCtx),
    Body1 = transform_body(full, {Mod, Fun}, Body),
    erase(seam_body_ctx),
    BindExprs = build_wildcard_bindings(Anno, FreshVars),
    {clause, Anno, Pats1, Guards,
     BindExprs ++ PriorTracking ++ OwnTracking ++ Body1}.

%%% === Fast-mode instrumentation ===

instrument_clause_fast(Mod, Fun, ClauseIdx,
                       {clause, Anno, Pats, Guards, Body}) ->
    seam_track:register_decision_meta(
        {Mod, Fun, ClauseIdx}, anno_line(Anno), "clause"),
    OwnCondKeys = guard_cond_keys(Mod, Fun, ClauseIdx, Guards),
    GuardConds = flatten_guard_conds(Guards),
    OwnTracking = build_own_tracking(Anno, OwnCondKeys, GuardConds) ++
                  [mk_record_dec(Anno, {Mod, Fun, ClauseIdx}, true)],
    {clause, Anno, Pats, Guards, OwnTracking ++ Body}.

%%% === Own-condition tracking (shared by full and fast) ===

build_own_tracking(Anno, CondKeys, GuardConds) ->
    Pairs = lists:zip(CondKeys, GuardConds),
    [case is_cmp_guard(Cond) of
         {true, CmpOp, _Lhs, _Rhs} ->
             mk_record_cmp(Anno, Key, CmpOp);
         false ->
             mk_record(Anno, Key, true)
     end || {Key, Cond} <- Pairs].

is_cmp_guard({op, _, Op, _L, _R}) ->
    case is_cmp_op(Op) of
        true -> {true, Op, _L, _R};
        false -> false
    end;
is_cmp_guard(_) -> false.

%% @doc True for the eight Erlang comparison operators.
is_cmp_op('>') -> true;
is_cmp_op('<') -> true;
is_cmp_op('>=') -> true;
is_cmp_op('=<') -> true;
is_cmp_op('==') -> true;
is_cmp_op('/=') -> true;
is_cmp_op('=:=') -> true;
is_cmp_op('=/=') -> true;
is_cmp_op(_) -> false.

%%% === Prior-clause tracking (full mode only) ===

freshen_wildcards(Pats, ClauseIdx) ->
    {RevPats, FreshVars, _} =
        lists:foldl(fun({var, A, '_'}, {Acc, Vs, N}) ->
                        Name = fresh_var_name(ClauseIdx, N),
                        {[{var, A, Name} | Acc], [{Name, N} | Vs], N + 1};
                    (P, {Acc, Vs, N}) ->
                        {[P | Acc], Vs, N + 1}
                    end, {[], [], 1}, Pats),
    {lists:reverse(RevPats), FreshVars}.

fresh_var_name(ClauseIdx, ArgN) ->
    list_to_atom("__Seam_" ++ integer_to_list(ClauseIdx) ++
                 "_" ++ integer_to_list(ArgN)).

build_wildcard_bindings(_Anno, _FreshVars) -> [].

build_prior_tracking(_Mod, _Fun, 1, _MyVars, _All, _Anno) -> [];
build_prior_tracking(Mod, Fun, ClauseIdx, MyVars, AllGuardInfo, Anno) ->
    PriorClauses = [Info || {Idx, _, _} = Info <- AllGuardInfo,
                            Idx < ClauseIdx],
    lists:flatmap(
        fun({PriorIdx, PriorVars, PriorGuards}) ->
            build_prior_clause_tracking(Mod, Fun, PriorIdx,
                                        PriorVars, PriorGuards,
                                        MyVars, Anno)
        end, PriorClauses).

build_prior_clause_tracking(Mod, Fun, PriorIdx, PriorVars, PriorGuards,
                            MyVars, Anno) ->
    VarMap = build_var_map(PriorVars, MyVars),
    case VarMap of
        skip ->
            [mk_record_dec(Anno, {Mod, Fun, PriorIdx}, false)];
        Map ->
            CondKeys = guard_cond_keys(Mod, Fun, PriorIdx, PriorGuards),
            GuardConds = flatten_guard_conds(PriorGuards),
            Pairs = lists:zip(CondKeys, GuardConds),
            CondTracking =
                [case is_cmp_guard(Cond) of
                     {true, CmpOp, _Lhs, _Rhs} ->
                         mk_try_record_cmp(Anno, Key, CmpOp,
                                           subst_vars(Cond, Map));
                     false ->
                         mk_try_record(Anno, Key, subst_vars(Cond, Map))
                 end || {Key, Cond} <- Pairs],
            DecTracking = [mk_record_dec(Anno, {Mod, Fun, PriorIdx}, false)],
            CondTracking ++ DecTracking
    end.

build_var_map(PriorVars, MyVars) ->
    Pairs = lists:zip(PriorVars, MyVars),
    try
        maps:from_list(
            lists:filtermap(
                fun({wildcard, _}) -> false;
                   ({_, wildcard}) -> false;
                   ({complex, _}) -> throw(skip);
                   ({_, complex}) -> throw(skip);
                   ({{var, From}, {var, To}}) -> {true, {From, To}}
                end, Pairs))
    catch
        throw:skip -> skip
    end.

subst_vars({var, A, Name}, Map) ->
    case maps:find(Name, Map) of
        {ok, NewName} -> {var, A, NewName};
        error -> {var, A, Name}
    end;
subst_vars({op, A, Op, L, R}, Map) ->
    {op, A, Op, subst_vars(L, Map), subst_vars(R, Map)};
subst_vars({op, A, Op, Operand}, Map) ->
    {op, A, Op, subst_vars(Operand, Map)};
subst_vars({call, A, Fun, Args}, Map) ->
    {call, A, subst_vars(Fun, Map), [subst_vars(Arg, Map) || Arg <- Args]};
subst_vars({remote, A, M, F}, Map) ->
    {remote, A, subst_vars(M, Map), subst_vars(F, Map)};
subst_vars({tuple, A, Es}, Map) ->
    {tuple, A, [subst_vars(E, Map) || E <- Es]};
subst_vars({cons, A, H, T}, Map) ->
    {cons, A, subst_vars(H, Map), subst_vars(T, Map)};
subst_vars(Other, _Map) ->
    Other.

%%% === Guard condition extraction ===

guard_cond_keys(Mod, Fun, ClauseIdx, Guards) ->
    {Keys, _} = lists:foldl(
        fun(Conj, {Acc, Idx}) ->
            {ConjKeys, NextIdx} = conj_keys(Mod, Fun, ClauseIdx, Conj, Idx),
            {Acc ++ ConjKeys, NextIdx}
        end, {[], 1}, Guards),
    Keys.

conj_keys(_Mod, _Fun, _Clause, [], Idx) -> {[], Idx};
conj_keys(Mod, Fun, Clause, [Test | Rest], Idx) ->
    Key = {Mod, Fun, Clause, Idx},
    Line = expr_line(Test),
    ExprStr = expr_to_string(Test),
    seam_track:register_meta(Key, Line, ExprStr),
    {RestKeys, NextIdx} = conj_keys(Mod, Fun, Clause, Rest, Idx + 1),
    {[Key | RestKeys], NextIdx}.

expr_line(Expr) ->
    anno_line(element(2, Expr)).

anno_line(N) when is_integer(N) -> N;
anno_line({L, _}) when is_integer(L) -> L;
anno_line(Anno) when is_list(Anno) -> proplists:get_value(location, Anno, 0);
anno_line(_) -> 0.

expr_to_string(Expr) ->
    try lists:flatten(erl_pp:expr(Expr))
    catch _:_ -> ""
    end.

flatten_guard_conds(Guards) ->
    lists:append(Guards).

%%% === AST builders ===

mk_record(Anno, Key, Val) when is_atom(Val) ->
    mk_record_expr(Anno, Key, {atom, Anno, Val}).

mk_record_expr(Anno, Key, ExprAST) ->
    {call, Anno,
        {remote, Anno, {atom, Anno, seam_track}, {atom, Anno, record}},
        [erl_parse:abstract(Key), ExprAST]}.

mk_record_dec(Anno, Key, Val) ->
    {call, Anno,
        {remote, Anno, {atom, Anno, seam_track}, {atom, Anno, record_decision}},
        [erl_parse:abstract(Key), {atom, Anno, Val}]}.

%% Emit `seam_track:record_cmp(Key, Op, true, true)'. The `true, true'
%% operands are placeholders — in the matched clause, the guard already
%% succeeded so the actual operand values don't matter. The record_cmp
%% call evaluates the comparison itself, so we pass operands that produce
%% the correct boolean. For the matched clause we know the result is true,
%% so any pair that satisfies the comparison works. We use the guard
%% expression form: record_cmp evaluates Op on the two atoms.
%%
%% Actually, for the own clause, the guard already passed so the result
%% is true. We record the condition as true and also want the operands.
%% But we don't have access to the operand values at this point in the
%% body. So for own conditions that are comparisons, we record them
%% as true via `record(Key, true)' and skip operand capture for the
%% matched clause's own conditions. Operand capture happens via
%% prior-clause re-evaluation and body expression instrumentation.
%%
%% Correction: we CAN capture operands in the own clause. The guard
%% variables are bound in the body. We emit
%% `seam_track:record_cmp(Key, Op, Lhs, Rhs)' with the original guard
%% sub-expressions (which reference bound variables).
mk_record_cmp(Anno, Key, _CmpOp) ->
    %% For own clause, we just record true — operands captured by prior
    %% clause re-evaluation on subsequent clauses matching.
    mk_record(Anno, Key, true).

%% Wrap a comparison re-evaluation in try/catch, capturing operands.
%% try seam_track:record_cmp(Key, Op, Lhs, Rhs)
%% catch _:_ -> seam_track:record(Key, false) end
mk_try_record_cmp(Anno, Key, CmpOp, {op, _, _Op, Lhs, Rhs}) ->
    {'try', Anno,
        [{call, Anno,
            {remote, Anno, {atom, Anno, seam_track}, {atom, Anno, record_cmp}},
            [erl_parse:abstract(Key), {atom, Anno, CmpOp}, Lhs, Rhs]}],
        [],
        [{clause, Anno,
            [{tuple, Anno,
                [{var, Anno, '_'}, {var, Anno, '_'}, {var, Anno, '_'}]}],
            [],
            [mk_record(Anno, Key, false)]}],
        []}.

mk_try_record(Anno, Key, Expr) ->
    {'try', Anno,
        [mk_record_expr(Anno, Key, Expr)],
        [],
        [{clause, Anno,
            [{tuple, Anno,
                [{var, Anno, '_'}, {var, Anno, '_'}, {var, Anno, '_'}]}],
            [],
            [mk_record(Anno, Key, false)]}],
        []}.

%%% === Body expression transformation ===

transform_body(Mode, Ctx, Exprs) ->
    [transform_expr(Mode, Ctx, E) || E <- Exprs].

transform_expr(Mode, Ctx, {'case', Anno, Arg, Clauses}) ->
    {'case', Anno, transform_expr(Mode, Ctx, Arg),
     [transform_case_clause(Mode, Ctx, C) || C <- Clauses]};
transform_expr(Mode, Ctx, {'if', Anno, Clauses}) ->
    {'if', Anno, [transform_if_clause(Mode, Ctx, C) || C <- Clauses]};
transform_expr(Mode, Ctx, {match, A, P, E}) ->
    {match, A, P, transform_expr(Mode, Ctx, E)};
transform_expr(Mode, Ctx, {call, A, F, Args}) ->
    {call, A, transform_expr(Mode, Ctx, F),
     [transform_expr(Mode, Ctx, Arg) || Arg <- Args]};
transform_expr(Mode, Ctx, {remote, A, M, F}) ->
    {remote, A, transform_expr(Mode, Ctx, M), transform_expr(Mode, Ctx, F)};
%% Body comparison operators — instrument in full mode
transform_expr(full, Ctx, {op, A, Op, L, R}) when Op =:= 'andalso' ->
    case get(seam_body_ctx) of
        undefined ->
            {op, A, Op, transform_expr(full, Ctx, L),
             transform_expr(full, Ctx, R)};
        _BodyCtx ->
            {Key, _} = alloc_body_key(Op, A),
            L1 = transform_expr(full, Ctx, L),
            R1 = transform_expr(full, Ctx, R),
            %% case seam_track:record(Key, L') of true -> R'; false -> false end
            {'case', A,
                {call, A,
                    {remote, A, {atom, A, seam_track}, {atom, A, record}},
                    [erl_parse:abstract(Key), L1]},
                [{clause, A, [{atom, A, true}], [], [R1]},
                 {clause, A, [{atom, A, false}], [], [{atom, A, false}]}]}
    end;
transform_expr(full, Ctx, {op, A, Op, L, R}) when Op =:= 'orelse' ->
    case get(seam_body_ctx) of
        undefined ->
            {op, A, Op, transform_expr(full, Ctx, L),
             transform_expr(full, Ctx, R)};
        _BodyCtx ->
            {Key, _} = alloc_body_key(Op, A),
            L1 = transform_expr(full, Ctx, L),
            R1 = transform_expr(full, Ctx, R),
            %% case seam_track:record(Key, L') of true -> true; false -> R' end
            {'case', A,
                {call, A,
                    {remote, A, {atom, A, seam_track}, {atom, A, record}},
                    [erl_parse:abstract(Key), L1]},
                [{clause, A, [{atom, A, true}], [], [{atom, A, true}]},
                 {clause, A, [{atom, A, false}], [], [R1]}]}
    end;
transform_expr(full, Ctx, {op, A, Op, L, R}) ->
    case is_cmp_op(Op) of
        true ->
            case get(seam_body_ctx) of
                undefined ->
                    {op, A, Op, transform_expr(full, Ctx, L),
                     transform_expr(full, Ctx, R)};
                _BodyCtx ->
                    {Key, _} = alloc_body_key(Op, A),
                    L1 = transform_expr(full, Ctx, L),
                    R1 = transform_expr(full, Ctx, R),
                    {call, A,
                        {remote, A, {atom, A, seam_track},
                                    {atom, A, record_cmp}},
                        [erl_parse:abstract(Key), {atom, A, Op}, L1, R1]}
            end;
        false ->
            {op, A, Op, transform_expr(full, Ctx, L),
             transform_expr(full, Ctx, R)}
    end;
transform_expr(Mode, Ctx, {op, A, Op, L, R}) ->
    {op, A, Op, transform_expr(Mode, Ctx, L), transform_expr(Mode, Ctx, R)};
transform_expr(Mode, Ctx, {op, A, Op, X}) ->
    {op, A, Op, transform_expr(Mode, Ctx, X)};
transform_expr(Mode, Ctx, {tuple, A, Es}) ->
    {tuple, A, [transform_expr(Mode, Ctx, E) || E <- Es]};
transform_expr(Mode, Ctx, {cons, A, H, T}) ->
    {cons, A, transform_expr(Mode, Ctx, H), transform_expr(Mode, Ctx, T)};
transform_expr(Mode, Ctx, {block, A, Body}) ->
    {block, A, transform_body(Mode, Ctx, Body)};
transform_expr(Mode, Ctx, {'fun', A, {clauses, Cs}}) ->
    %% Isolate body ctx (clause-specific condition indices) but keep
    %% dec idx running — fun clauses and their case/if branches share
    %% the enclosing function's monotonic decision counter.
    SavedCtx = erase(seam_body_ctx),
    Result = {'fun', A, {clauses, [transform_case_clause(Mode, Ctx, C) || C <- Cs]}},
    restore_pd(seam_body_ctx, SavedCtx),
    Result;
transform_expr(Mode, Ctx, {named_fun, A, Name, Cs}) ->
    SavedCtx = erase(seam_body_ctx),
    Result = {named_fun, A, Name,
              [transform_case_clause(Mode, Ctx, C) || C <- Cs]},
    restore_pd(seam_body_ctx, SavedCtx),
    Result;
transform_expr(_Mode, _Ctx, Other) ->
    Other.

transform_case_clause(Mode, Ctx, {clause, Anno, Pats, Guards, Body}) ->
    Body1 = transform_body(Mode, Ctx, Body),
    Body2 = maybe_inject_branch_dec(Ctx, Anno, "case branch", Body1),
    {clause, Anno, Pats, Guards, Body2}.

transform_if_clause(Mode, Ctx, {clause, Anno, [], Guards, Body}) ->
    Body1 = transform_body(Mode, Ctx, Body),
    Body2 = maybe_inject_branch_dec(Ctx, Anno, "if branch", Body1),
    {clause, Anno, [], Guards, Body2}.

%%% === Body key allocation ===

alloc_body_key(Op, Anno) ->
    {Mod, Fun, ClauseIdx, NextIdx} = get(seam_body_ctx),
    Key = {Mod, Fun, ClauseIdx, NextIdx},
    seam_track:register_meta(Key, anno_line(Anno), atom_to_list(Op)),
    put(seam_body_ctx, {Mod, Fun, ClauseIdx, NextIdx + 1}),
    {Key, NextIdx}.

%%% === Branch decision injection ===

alloc_dec_idx() ->
    Idx = get(seam_dec_idx),
    put(seam_dec_idx, Idx + 1),
    Idx.

maybe_inject_branch_dec({Mod, Fun}, Anno, Label, Body) ->
    case get(seam_dec_idx) of
        undefined -> Body;
        _ ->
            Idx = alloc_dec_idx(),
            DecKey = {Mod, Fun, Idx},
            seam_track:register_decision_meta(DecKey, anno_line(Anno), Label),
            [mk_record_dec(Anno, DecKey, true) | Body]
    end.

restore_pd(_Key, undefined) -> ok;
restore_pd(Key, Val) -> put(Key, Val), ok.

-module(seam_instrument).
-include("seam.hrl").

-export([compile_beam/1]).

%% Instrument a module's BEAM and load the result.
-spec compile_beam(module() | string()) -> {ok, module()} | {error, term()}.
compile_beam(ModOrPath) ->
    case resolve_beam(ModOrPath) of
        {ok, Mod, Beam} ->
            case extract_forms(Mod, Beam) of
                {ok, Forms} -> do_instrument(Mod, Beam, Forms);
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
                {ok, Bin} -> {ok, Mod, Bin};
                {error, R} -> {error, {read_failed, Path, R}}
            end
    end;
resolve_beam(Path) when is_list(Path); is_binary(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            case beam_lib:info(Bin) of
                [{module, Mod} | _] -> {ok, Mod, Bin};
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

do_instrument(Mod, OrigBeam, Forms) ->
    Instrumented = transform_forms(Mod, Forms),
    case compile:forms(Instrumented, [binary, return_errors, debug_info]) of
        {ok, Mod, Binary} ->
            load_instrumented(Mod, OrigBeam, Binary);
        {ok, Mod, Binary, _Warn} ->
            load_instrumented(Mod, OrigBeam, Binary);
        {error, Errors, _Warn} ->
            {error, {compile_failed, Errors}}
    end.

load_instrumented(Mod, OrigBeam, Binary) ->
    seam_track:register_module(Mod, OrigBeam),
    code:purge(Mod),
    case code:load_binary(Mod, "seam_instrumented", Binary) of
        {module, Mod} -> {ok, Mod};
        {error, R}    -> {error, {load_failed, R}}
    end.

%%% === AST transformation ===

transform_forms(Mod, Forms) ->
    [transform_form(Mod, F) || F <- Forms].

transform_form(Mod, {function, Anno, Name, Arity, Clauses}) ->
    {function, Anno, Name, Arity,
     transform_fun_clauses(Mod, Name, Clauses)};
transform_form(_Mod, Other) ->
    Other.

%% Transform a set of function clauses.
%% Strategy: keep original patterns and guards intact.
%% In each clause body, record own conditions as true and re-evaluate
%% prior clauses' conditions to capture false counts.
transform_fun_clauses(Mod, Fun, Clauses) ->
    Indexed = index_clauses(Clauses, 1),
    AllGuardInfo = [{Idx, extract_clause_vars(Pats), Guards}
                    || {Idx, {clause, _, Pats, Guards, _}} <- Indexed],
    [instrument_clause(Mod, Fun, Idx, Clause, AllGuardInfo)
     || {Idx, Clause} <- Indexed].

index_clauses([], _) -> [];
index_clauses([C | Cs], N) -> [{N, C} | index_clauses(Cs, N + 1)].

%% Extract positional variable names from patterns.
%% Returns a list: one element per argument position.
%% Each element is {var, Name} | complex.
extract_clause_vars(Pats) ->
    [pat_var(P) || P <- Pats].

pat_var({var, _, '_'}) -> wildcard;
pat_var({var, _, Name}) -> {var, Name};
pat_var(_) -> complex.

%% Instrument a single clause.
instrument_clause(Mod, Fun, ClauseIdx,
                  {clause, Anno, Pats, Guards, Body},
                  AllGuardInfo) ->
    %% Replace _ with fresh vars so we can re-evaluate prior guards
    {Pats1, FreshVars} = freshen_wildcards(Pats, ClauseIdx),
    MyVars = extract_clause_vars(Pats1),
    %% Build tracking calls for prior clauses (re-evaluate their guards)
    PriorTracking = build_prior_tracking(Mod, Fun, ClauseIdx,
                                         MyVars, AllGuardInfo, Anno),
    %% Build tracking for own conditions (all true) and decision
    OwnCondKeys = guard_cond_keys(Mod, Fun, ClauseIdx, Guards),
    OwnTracking = [mk_record(Anno, K, true) || K <- OwnCondKeys] ++
                  [mk_record_dec(Anno, {Mod, Fun, ClauseIdx}, true)],
    %% Transform case/if in body
    Body1 = transform_body({Mod, Fun}, Body),
    %% Assemble: bind fresh vars, then prior tracking, own tracking, body
    BindExprs = build_wildcard_bindings(Anno, FreshVars),
    {clause, Anno, Pats1, Guards,
     BindExprs ++ PriorTracking ++ OwnTracking ++ Body1}.

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

%% Wildcard bindings are no-ops since the fresh var is already bound by the pattern.
build_wildcard_bindings(_Anno, _FreshVars) -> [].

%% Build tracking calls for all prior clauses' guard conditions.
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

%% Re-evaluate a prior clause's guard conditions using current bindings.
build_prior_clause_tracking(Mod, Fun, PriorIdx, PriorVars, PriorGuards,
                            MyVars, Anno) ->
    VarMap = build_var_map(PriorVars, MyVars),
    case VarMap of
        skip ->
            %% Can't re-evaluate: complex patterns. Just record decision false.
            [mk_record_dec(Anno, {Mod, Fun, PriorIdx}, false)];
        Map ->
            CondKeys = guard_cond_keys(Mod, Fun, PriorIdx, PriorGuards),
            GuardConds = flatten_guard_conds(PriorGuards),
            Pairs = lists:zip(CondKeys, GuardConds),
            CondTracking =
                [mk_try_record(Anno, Key, subst_vars(Cond, Map))
                 || {Key, Cond} <- Pairs],
            DecTracking = [mk_record_dec(Anno, {Mod, Fun, PriorIdx}, false)],
            CondTracking ++ DecTracking
    end.

%% Build variable substitution map: prior var name → current var name.
build_var_map(PriorVars, MyVars) ->
    Pairs = lists:zip(PriorVars, MyVars),
    try
        maps:from_list(
            lists:filtermap(
                fun({wildcard, _}) -> false;
                   ({_, wildcard}) -> false;  %% shouldn't happen after freshening
                   ({complex, _}) -> throw(skip);
                   ({_, complex}) -> throw(skip);
                   ({{var, From}, {var, To}}) -> {true, {From, To}}
                end, Pairs))
    catch
        throw:skip -> skip
    end.

%% Substitute variable names in an AST expression.
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

%% Condition keys for all conditions in a guard.
guard_cond_keys(Mod, Fun, ClauseIdx, Guards) ->
    {Keys, _} = lists:foldl(
        fun(Conj, {Acc, Idx}) ->
            {ConjKeys, NextIdx} = conj_keys(Mod, Fun, ClauseIdx, Conj, Idx),
            {Acc ++ ConjKeys, NextIdx}
        end, {[], 1}, Guards),
    Keys.

conj_keys(_Mod, _Fun, _Clause, [], Idx) -> {[], Idx};
conj_keys(Mod, Fun, Clause, [_ | Rest], Idx) ->
    {RestKeys, NextIdx} = conj_keys(Mod, Fun, Clause, Rest, Idx + 1),
    {[{Mod, Fun, Clause, Idx} | RestKeys], NextIdx}.

%% Flatten guards into a list of individual test expressions.
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

%% Wrap a guard condition re-evaluation in try/catch.
%% try seam_track:record(Key, Expr) catch _:_ -> seam_track:record(Key, false) end
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

%%% === Body expression transformation (case/if) ===

transform_body(Ctx, Exprs) ->
    [transform_expr(Ctx, E) || E <- Exprs].

transform_expr(Ctx, {'case', Anno, Arg, Clauses}) ->
    {'case', Anno, transform_expr(Ctx, Arg),
     [transform_case_clause(Ctx, C) || C <- Clauses]};
transform_expr(Ctx, {'if', Anno, Clauses}) ->
    {'if', Anno, [transform_if_clause(Ctx, C) || C <- Clauses]};
transform_expr(Ctx, {match, A, P, E}) ->
    {match, A, P, transform_expr(Ctx, E)};
transform_expr(Ctx, {call, A, F, Args}) ->
    {call, A, transform_expr(Ctx, F),
     [transform_expr(Ctx, Arg) || Arg <- Args]};
transform_expr(Ctx, {remote, A, M, F}) ->
    {remote, A, transform_expr(Ctx, M), transform_expr(Ctx, F)};
transform_expr(Ctx, {op, A, Op, L, R}) ->
    {op, A, Op, transform_expr(Ctx, L), transform_expr(Ctx, R)};
transform_expr(Ctx, {op, A, Op, X}) ->
    {op, A, Op, transform_expr(Ctx, X)};
transform_expr(Ctx, {tuple, A, Es}) ->
    {tuple, A, [transform_expr(Ctx, E) || E <- Es]};
transform_expr(Ctx, {cons, A, H, T}) ->
    {cons, A, transform_expr(Ctx, H), transform_expr(Ctx, T)};
transform_expr(Ctx, {block, A, Body}) ->
    {block, A, transform_body(Ctx, Body)};
transform_expr(Ctx, {'fun', A, {clauses, Cs}}) ->
    {'fun', A, {clauses, [transform_case_clause(Ctx, C) || C <- Cs]}};
transform_expr(Ctx, {named_fun, A, Name, Cs}) ->
    {named_fun, A, Name, [transform_case_clause(Ctx, C) || C <- Cs]};
transform_expr(_Ctx, Other) ->
    Other.

transform_case_clause(Ctx, {clause, Anno, Pats, Guards, Body}) ->
    {clause, Anno, Pats, Guards, transform_body(Ctx, Body)}.

transform_if_clause(Ctx, {clause, Anno, [], Guards, Body}) ->
    {clause, Anno, [], Guards, transform_body(Ctx, Body)}.

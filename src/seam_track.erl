%% @doc Runtime storage for coverage data.
%%
%% Owns eight named ETS tables: conditions, decisions, MC/DC vectors,
%% module metadata, condition metadata, discoveries, operands, and edges.
%% All counter updates use `ets:update_counter/4' for atomicity under
%% concurrent access. Called directly by instrumented code at runtime —
%% keep the hot path minimal.
-module(seam_track).
-include("seam.hrl").

-export([init/0, destroy/0, reset/0, reset/1]).
-export([record/2, record_decision/2, record_vector/3]).
-export([record_cmp/4]).
-export([take_new/0]).
-export([operands/1]).
-export([edges/0, edges/1, reset_edge_state/0]).
-export([conditions/0, conditions/1, decisions/0, decisions/1]).
-export([modules/0, register_module/3, unregister_module/1]).
-export([register_meta/3, meta/1]).
-export([register_decision_meta/3, decision_meta/1]).

%% @doc Create all ETS tables. Call once at startup.
-spec init() -> ok.
init() ->
    Opts = [named_table, public, set, {write_concurrency, true}],
    ets:new(?SEAM_CONDITIONS, Opts),
    ets:new(?SEAM_DECISIONS,  Opts),
    ets:new(?SEAM_MODULES,    [named_table, public, set]),
    ets:new(?SEAM_VECTORS,    [named_table, public, bag, {write_concurrency, true}]),
    ets:new(?SEAM_META,       [named_table, public, set]),
    ets:new(?SEAM_DISCOVERIES, [named_table, public, set, {write_concurrency, true}]),
    ets:new(?SEAM_OPERANDS,   [named_table, public, set, {write_concurrency, true}]),
    ets:new(?SEAM_EDGES,      [named_table, public, set, {write_concurrency, true}]),
    ets:new(?SEAM_DECISION_META, [named_table, public, set]),
    ok.

%% @doc Drop all ETS tables.
-spec destroy() -> ok.
destroy() ->
    Tables = [?SEAM_CONDITIONS, ?SEAM_DECISIONS, ?SEAM_MODULES,
              ?SEAM_VECTORS, ?SEAM_META, ?SEAM_DISCOVERIES,
              ?SEAM_OPERANDS, ?SEAM_EDGES, ?SEAM_DECISION_META],
    lists:foreach(fun(T) -> catch ets:delete(T) end, Tables),
    ok.

%% @doc Zero all counters and clear discoveries, operands, edges.
-spec reset() -> ok.
reset() ->
    ets:delete_all_objects(?SEAM_CONDITIONS),
    ets:delete_all_objects(?SEAM_DECISIONS),
    ets:delete_all_objects(?SEAM_VECTORS),
    ets:delete_all_objects(?SEAM_DISCOVERIES),
    ets:delete_all_objects(?SEAM_OPERANDS),
    ets:delete_all_objects(?SEAM_EDGES),
    ets:delete_all_objects(?SEAM_DECISION_META),
    ok.

%% @doc Zero counters for a single module. Return error if not compiled.
-spec reset(module()) -> ok | {error, {not_compiled, module()}}.
reset(Mod) ->
    case ets:lookup(?SEAM_MODULES, Mod) of
        [] -> {error, {not_compiled, Mod}};
        _  ->
            delete_by_module(?SEAM_CONDITIONS, 4, Mod),
            delete_by_module(?SEAM_DECISIONS, 3, Mod),
            delete_decision_meta(Mod),
            ok
    end.

%% @doc Record a condition evaluation. Atomic increment. On 0-to-1
%% transition, insert a discovery. Return `Result' unchanged.
-spec record(cond_key(), boolean()) -> boolean().
record(Key, Result) ->
    Cnt = ets:update_counter(?SEAM_CONDITIONS,
        {Key, Result}, {2, 1}, {{Key, Result}, 0}),
    case Cnt of
        1 -> ets:insert(?SEAM_DISCOVERIES, {{condition, Key, Result}});
        _ -> ok
    end,
    Result.

%% @doc Record a decision outcome. Atomic increment. On 0-to-1
%% transition, insert a discovery. When `Result' is `true', track
%% decision-to-decision edges.
-spec record_decision(decision_key(), boolean()) -> boolean().
record_decision(Key, Result) ->
    Cnt = ets:update_counter(?SEAM_DECISIONS,
        {Key, Result}, {2, 1}, {{Key, Result}, 0}),
    case Cnt of
        1 -> ets:insert(?SEAM_DISCOVERIES, {{decision, Key, Result}});
        _ -> ok
    end,
    case Result of
        true ->
            case get(seam_prev_decision) of
                undefined -> ok;
                Prev ->
                    EdgeCnt = ets:update_counter(?SEAM_EDGES,
                        {Prev, Key}, {2, 1}, {{Prev, Key}, 0}),
                    case EdgeCnt of
                        1 -> ets:insert(?SEAM_DISCOVERIES, {{edge, Prev, Key}});
                        _ -> ok
                    end
            end,
            put(seam_prev_decision, Key);
        false -> ok
    end,
    Result.

%% @doc Record a comparison condition. Evaluate `Op' on `Lhs' and `Rhs',
%% record the boolean result, store the operand pair, return the result.
-spec record_cmp(cond_key(), atom(), term(), term()) -> boolean().
record_cmp(Key, Op, Lhs, Rhs) ->
    Result = eval_cmp(Op, Lhs, Rhs),
    record(Key, Result),
    ets:insert(?SEAM_OPERANDS, {{Key, Result}, Op, Lhs, Rhs}),
    Result.

%% @doc Store a full test vector for post-hoc MC/DC analysis.
-spec record_vector(decision_key(), [boolean()], boolean()) -> ok.
record_vector(DecKey, CondVals, Outcome) ->
    ets:insert(?SEAM_VECTORS, {DecKey, CondVals, Outcome}),
    ok.

%% @doc Consume all discoveries since the last call. Returns the list
%% and clears the discoveries table.
-spec take_new() -> [discovery()].
take_new() ->
    Items = [D || {D} <- ets:tab2list(?SEAM_DISCOVERIES)],
    ets:delete_all_objects(?SEAM_DISCOVERIES),
    Items.

%% @doc Operand data for `Mod'. Returns a map from condition key to a map
%% of `#{true => {Op, Lhs, Rhs}, false => {Op, Lhs, Rhs}}'.
-spec operands(module()) -> #{cond_key() => #{boolean() => {atom(), term(), term()}}}.
operands(Mod) ->
    ets:foldl(fun({{Key, Bool}, Op, Lhs, Rhs}, Acc) ->
        case Key of
            {M, _, _, _} when M =:= Mod ->
                Inner = maps:get(Key, Acc, #{}),
                maps:put(Key, Inner#{Bool => {Op, Lhs, Rhs}}, Acc);
            _ -> Acc
        end
    end, #{}, ?SEAM_OPERANDS).

%% @doc All edge transitions as `#{{From, To} => Count}'.
-spec edges() -> #{{decision_key(), decision_key()} => non_neg_integer()}.
edges() ->
    ets:foldl(fun({{From, To}, Cnt}, Acc) ->
        Acc#{{From, To} => Cnt}
    end, #{}, ?SEAM_EDGES).

%% @doc Edge transitions filtered to a module.
-spec edges(module()) -> #{{decision_key(), decision_key()} => non_neg_integer()}.
edges(Mod) ->
    maps:filter(fun({{M1, _, _}, {M2, _, _}}, _) ->
        M1 =:= Mod orelse M2 =:= Mod
    end, edges()).

%% @doc Clear per-process edge state. Call between test iterations.
-spec reset_edge_state() -> ok.
reset_edge_state() ->
    erase(seam_prev_decision),
    ok.

%% @doc All condition coverage as `#{cond_key() => {TrueCount, FalseCount}}'.
-spec conditions() -> #{cond_key() => {non_neg_integer(), non_neg_integer()}}.
conditions() ->
    fold_pairs(?SEAM_CONDITIONS).

%% @doc Condition coverage filtered to a single module.
-spec conditions(module()) -> #{cond_key() => {non_neg_integer(), non_neg_integer()}}.
conditions(Mod) ->
    maps:filter(fun({M, _, _, _}, _) -> M =:= Mod end, conditions()).

%% @doc All decision coverage as `#{decision_key() => {SuccessCount, FailureCount}}'.
-spec decisions() -> #{decision_key() => {non_neg_integer(), non_neg_integer()}}.
decisions() ->
    fold_pairs(?SEAM_DECISIONS).

%% @doc Decision coverage filtered to a single module.
-spec decisions(module()) -> #{decision_key() => {non_neg_integer(), non_neg_integer()}}.
decisions(Mod) ->
    maps:filter(fun({M, _, _}, _) -> M =:= Mod end, decisions()).

%% @doc Stash the original BEAM binary and its filesystem path for
%% later restoration. The path is passed to `code:load_binary/3' when
%% the module is restored, so `code:which/1' returns a valid filename.
-spec register_module(module(), binary(), string()) -> ok.
register_module(Mod, OrigBeam, OrigPath) ->
    ets:insert(?SEAM_MODULES, {Mod, OrigBeam, OrigPath}),
    ok.

%% @doc Remove a module from the tracked set.
-spec unregister_module(module()) -> ok.
unregister_module(Mod) ->
    ets:delete(?SEAM_MODULES, Mod),
    ok.

%% @doc List all currently instrumented modules.
-spec modules() -> [module()].
modules() ->
    [M || {M, _, _} <- ets:tab2list(?SEAM_MODULES)].

%% @doc Store condition metadata (source line and expression text).
-spec register_meta(cond_key(), pos_integer(), string()) -> ok.
register_meta(Key, Line, ExprStr) ->
    ets:insert(?SEAM_META, {Key, Line, ExprStr}),
    ok.

%% @doc Retrieve all condition metadata for a module.
-spec meta(module()) -> [{cond_key(), pos_integer(), string()}].
meta(Mod) ->
    ets:foldl(fun({{M, _, _, _} = K, Line, Expr}, Acc) when M =:= Mod ->
        [{K, Line, Expr} | Acc];
    (_, Acc) -> Acc
    end, [], ?SEAM_META).

%% @doc Store decision metadata (source line and label).
-spec register_decision_meta(decision_key(), pos_integer(), string()) -> ok.
register_decision_meta(Key, Line, Label) ->
    ets:insert(?SEAM_DECISION_META, {Key, Line, Label}),
    ok.

%% @doc Retrieve all decision metadata for a module.
-spec decision_meta(module()) -> [{decision_key(), pos_integer(), string()}].
decision_meta(Mod) ->
    ets:foldl(fun({{M, _, _} = K, Line, Label}, Acc) when M =:= Mod ->
        [{K, Line, Label} | Acc];
    (_, Acc) -> Acc
    end, [], ?SEAM_DECISION_META).

%%% Internal

fold_pairs(Tab) ->
    ets:foldl(fun({{Key, true}, Cnt}, Acc) ->
        maps:update_with(Key,
            fun({_, F}) -> {Cnt, F} end,
            {Cnt, 0}, Acc);
    ({{Key, false}, Cnt}, Acc) ->
        maps:update_with(Key,
            fun({T, _}) -> {T, Cnt} end,
            {0, Cnt}, Acc)
    end, #{}, Tab).

eval_cmp('>', L, R)   -> L > R;
eval_cmp('<', L, R)   -> L < R;
eval_cmp('>=', L, R)  -> L >= R;
eval_cmp('=<', L, R)  -> L =< R;
eval_cmp('==', L, R)  -> L == R;
eval_cmp('/=', L, R)  -> L /= R;
eval_cmp('=:=', L, R) -> L =:= R;
eval_cmp('=/=', L, R) -> L =/= R.

delete_decision_meta(Mod) ->
    ets:foldl(fun({{M, _, _} = Key, _, _}, _) when M =:= Mod ->
        ets:delete(?SEAM_DECISION_META, Key);
    (_, Acc) -> Acc
    end, ok, ?SEAM_DECISION_META).

delete_by_module(Tab, KeyArity, Mod) ->
    ets:foldl(fun({CompKey, _}, _) ->
        {Key, _Bool} = CompKey,
        M = element(1, Key),
        case M =:= Mod andalso tuple_size(Key) =:= KeyArity of
            true -> ets:delete(Tab, CompKey);
            false -> ok
        end
    end, ok, Tab).

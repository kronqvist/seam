%% @doc Runtime storage for coverage data.
%%
%% Owns five named ETS tables: conditions, decisions, MC/DC vectors,
%% module metadata, and condition metadata. All counter updates use
%% `ets:update_counter/4' for atomicity under concurrent access. Called
%% directly by instrumented code at runtime — keep the hot path minimal.
-module(seam_track).
-include("seam.hrl").

-export([init/0, destroy/0, reset/0]).
-export([record/2, record_decision/2, record_vector/3]).
-export([conditions/0, conditions/1, decisions/0, decisions/1]).
-export([modules/0, register_module/2, unregister_module/1]).
-export([register_meta/3, meta/1]).

%% @doc Create all ETS tables. Call once at startup.
-spec init() -> ok.
init() ->
    Opts = [named_table, public, set, {write_concurrency, true}],
    ets:new(?SEAM_CONDITIONS, Opts),
    ets:new(?SEAM_DECISIONS,  Opts),
    ets:new(?SEAM_MODULES,    [named_table, public, set]),
    ets:new(?SEAM_VECTORS,    [named_table, public, bag, {write_concurrency, true}]),
    ets:new(?SEAM_META,       [named_table, public, set]),
    ok.

%% @doc Drop all ETS tables.
-spec destroy() -> ok.
destroy() ->
    lists:foreach(fun(T) ->
        catch ets:delete(T)
    end, [?SEAM_CONDITIONS, ?SEAM_DECISIONS, ?SEAM_MODULES, ?SEAM_VECTORS, ?SEAM_META]),
    ok.

%% @doc Zero all counters. Table structure preserved.
-spec reset() -> ok.
reset() ->
    ets:delete_all_objects(?SEAM_CONDITIONS),
    ets:delete_all_objects(?SEAM_DECISIONS),
    ets:delete_all_objects(?SEAM_VECTORS),
    ok.

%% @doc Record a condition evaluation. Atomic increment. Return `Result'
%% unchanged (passthrough).
-spec record(cond_key(), boolean()) -> boolean().
record(Key, Result) ->
    ets:update_counter(?SEAM_CONDITIONS,
        {Key, Result}, {2, 1}, {{Key, Result}, 0}),
    Result.

%% @doc Record a decision (whole guard) outcome. Atomic increment. Passthrough.
-spec record_decision(decision_key(), boolean()) -> boolean().
record_decision(Key, Result) ->
    ets:update_counter(?SEAM_DECISIONS,
        {Key, Result}, {2, 1}, {{Key, Result}, 0}),
    Result.

%% @doc Store a full test vector for post-hoc MC/DC analysis.
-spec record_vector(decision_key(), [boolean()], boolean()) -> ok.
record_vector(DecKey, CondVals, Outcome) ->
    ets:insert(?SEAM_VECTORS, {DecKey, CondVals, Outcome}),
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

%% @doc Stash the original BEAM binary for later restoration by {@link seam:stop/0}.
-spec register_module(module(), binary()) -> ok.
register_module(Mod, OrigBeam) ->
    ets:insert(?SEAM_MODULES, {Mod, OrigBeam}),
    ok.

%% @doc Remove a module from the tracked set.
-spec unregister_module(module()) -> ok.
unregister_module(Mod) ->
    ets:delete(?SEAM_MODULES, Mod),
    ok.

%% @doc List all currently instrumented modules.
-spec modules() -> [module()].
modules() ->
    [M || {M, _} <- ets:tab2list(?SEAM_MODULES)].

%% @doc Store condition metadata (source line and expression text) for
%% report generation.
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

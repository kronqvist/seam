-module(seam_track).
-include("seam.hrl").

-export([init/0, destroy/0, reset/0]).
-export([record/2, record_decision/2, record_vector/3]).
-export([conditions/0, conditions/1, decisions/0, decisions/1]).
-export([modules/0, register_module/2, unregister_module/1]).

%% Create all ETS tables. Call once at startup.
-spec init() -> ok.
init() ->
    Opts = [named_table, public, set, {write_concurrency, true}],
    ets:new(?SEAM_CONDITIONS, Opts),
    ets:new(?SEAM_DECISIONS,  Opts),
    ets:new(?SEAM_MODULES,    [named_table, public, set]),
    ets:new(?SEAM_VECTORS,    [named_table, public, bag, {write_concurrency, true}]),
    ok.

%% Drop all ETS tables.
-spec destroy() -> ok.
destroy() ->
    lists:foreach(fun(T) ->
        catch ets:delete(T)
    end, [?SEAM_CONDITIONS, ?SEAM_DECISIONS, ?SEAM_MODULES, ?SEAM_VECTORS]),
    ok.

%% Zero all counters; preserve table structure.
-spec reset() -> ok.
reset() ->
    ets:delete_all_objects(?SEAM_CONDITIONS),
    ets:delete_all_objects(?SEAM_DECISIONS),
    ets:delete_all_objects(?SEAM_VECTORS),
    ok.

%% Record a single condition evaluation. Atomic increment.
-spec record(cond_key(), boolean()) -> boolean().
record(Key, Result) ->
    ets:update_counter(?SEAM_CONDITIONS,
        {Key, Result}, {2, 1}, {{Key, Result}, 0}),
    Result.

%% Record a decision (whole guard) outcome. Atomic increment.
-spec record_decision(decision_key(), boolean()) -> boolean().
record_decision(Key, Result) ->
    ets:update_counter(?SEAM_DECISIONS,
        {Key, Result}, {2, 1}, {{Key, Result}, 0}),
    Result.

%% Store a full test vector for MC/DC analysis.
-spec record_vector(decision_key(), [boolean()], boolean()) -> ok.
record_vector(DecKey, CondVals, Outcome) ->
    ets:insert(?SEAM_VECTORS, {DecKey, CondVals, Outcome}),
    ok.

%% Return all condition coverage as a map.
-spec conditions() -> #{cond_key() => {non_neg_integer(), non_neg_integer()}}.
conditions() ->
    fold_pairs(?SEAM_CONDITIONS).

%% Condition coverage for a single module.
-spec conditions(module()) -> #{cond_key() => {non_neg_integer(), non_neg_integer()}}.
conditions(Mod) ->
    maps:filter(fun({M, _, _, _}, _) -> M =:= Mod end, conditions()).

%% Return all decision coverage as a map.
-spec decisions() -> #{decision_key() => {non_neg_integer(), non_neg_integer()}}.
decisions() ->
    fold_pairs(?SEAM_DECISIONS).

%% Decision coverage for a single module.
-spec decisions(module()) -> #{decision_key() => {non_neg_integer(), non_neg_integer()}}.
decisions(Mod) ->
    maps:filter(fun({M, _, _}, _) -> M =:= Mod end, decisions()).

%% Register an instrumented module, stashing its original BEAM binary.
-spec register_module(module(), binary()) -> ok.
register_module(Mod, OrigBeam) ->
    ets:insert(?SEAM_MODULES, {Mod, OrigBeam}),
    ok.

%% Remove a module from tracking.
-spec unregister_module(module()) -> ok.
unregister_module(Mod) ->
    ets:delete(?SEAM_MODULES, Mod),
    ok.

%% All tracked modules.
-spec modules() -> [module()].
modules() ->
    [M || {M, _} <- ets:tab2list(?SEAM_MODULES)].

%%% Internal

%% Fold ETS entries of form {{Key, Bool}, Count} into #{Key => {True, False}}.
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

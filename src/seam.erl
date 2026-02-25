%% @doc Public API for Seam condition-level coverage.
%%
%% Thin facade over `seam_instrument', `seam_track', `seam_analyse', and
%% `seam_report'. Start the server, instrument modules, exercise them,
%% then query or export the results.
-module(seam).
-include("seam.hrl").

-export([start/0, stop/0, reset/0]).
-export([compile_beam/1]).
-export([condition_coverage/1, decision_coverage/1]).
-export([analyse/2]).
-export([report/2]).
-export([export/1, import/1]).

%% @doc Create ETS tracking tables. Idempotent.
-spec start() -> ok.
start() ->
    case ets:whereis(?SEAM_CONDITIONS) of
        undefined -> seam_track:init();
        _ -> ok
    end.

%% @doc Restore all instrumented modules to their originals and destroy ETS tables.
-spec stop() -> ok.
stop() ->
    lists:foreach(fun restore_module/1, seam_track:modules()),
    seam_track:destroy().

%% @doc Zero all condition, decision, and vector counters. Table structure preserved.
-spec reset() -> ok.
reset() ->
    seam_track:reset().

%% @doc Instrument a module and hot-load the result. Accept a module atom
%% (must be loaded with `debug_info') or a path to a `.beam' file.
%% The original binary is stashed for restoration by `stop/0'.
-spec compile_beam(module() | string()) -> {ok, module()} | {error, term()}.
compile_beam(ModOrPath) ->
    seam_instrument:compile_beam(ModOrPath).

%% @doc Return condition coverage for `Mod'.
%% Result: `#{cond_key() => {TrueCount, FalseCount}}'.
-spec condition_coverage(module()) -> #{cond_key() => {non_neg_integer(), non_neg_integer()}}.
condition_coverage(Mod) ->
    seam_track:conditions(Mod).

%% @doc Return decision coverage for `Mod'.
%% Result: `#{decision_key() => {SuccessCount, FailureCount}}'.
-spec decision_coverage(module()) -> #{decision_key() => {non_neg_integer(), non_neg_integer()}}.
decision_coverage(Mod) ->
    seam_track:decisions(Mod).

%% @doc Query coverage by level. `condition' and `decision' correspond to
%% the two coverage maps.
-spec analyse(module(), condition | decision) -> {ok, map()}.
analyse(Mod, condition) -> {ok, condition_coverage(Mod)};
analyse(Mod, decision)  -> {ok, decision_coverage(Mod)}.

%% @doc Generate a coverage report as an iolist. `text' for plain text,
%% `html' for source-annotated HTML.
-spec report(module(), text | html) -> iolist().
report(Mod, text) -> seam_report:text(Mod);
report(Mod, html) -> seam_report:html(Mod).

%% @doc Serialize all coverage data to `Path' as a binary term. Merge later
%% with {@link import/1}.
-spec export(string()) -> ok | {error, term()}.
export(Path) ->
    Data = #{conditions => seam_track:conditions(),
             decisions  => seam_track:decisions(),
             modules    => seam_track:modules()},
    file:write_file(Path, term_to_binary(Data)).

%% @doc Read coverage data from `Path' and merge into current counters.
%% Commutative.
-spec import(string()) -> ok | {error, term()}.
import(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            #{conditions := Conds, decisions := Decs} = binary_to_term(Bin),
            merge_coverage(Conds, Decs);
        Err -> Err
    end.

merge_coverage(Conds, Decs) ->
    maps:foreach(fun(Key, {T, F}) ->
        bump_n(?SEAM_CONDITIONS, Key, true, T),
        bump_n(?SEAM_CONDITIONS, Key, false, F)
    end, Conds),
    maps:foreach(fun(Key, {S, F}) ->
        bump_n(?SEAM_DECISIONS, Key, true, S),
        bump_n(?SEAM_DECISIONS, Key, false, F)
    end, Decs),
    ok.

bump_n(_Tab, _Key, _Bool, 0) -> ok;
bump_n(Tab, Key, Bool, N) ->
    ets:update_counter(Tab, {Key, Bool}, {2, N}, {{Key, Bool}, 0}).

%%% Internal

restore_module(Mod) ->
    case ets:lookup(?SEAM_MODULES, Mod) of
        [{Mod, OrigBeam}] ->
            code:purge(Mod),
            code:load_binary(Mod, "restored", OrigBeam),
            seam_track:unregister_module(Mod);
        [] -> ok
    end.

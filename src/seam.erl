-module(seam).
-include("seam.hrl").

-export([start/0, stop/0, reset/0]).
-export([compile_beam/1]).
-export([condition_coverage/1, decision_coverage/1]).
-export([analyse/2]).
-export([report/2]).
-export([export/1, import/1]).

%% Initialize tracking tables. Idempotent.
-spec start() -> ok.
start() ->
    case ets:whereis(?SEAM_CONDITIONS) of
        undefined -> seam_track:init();
        _ -> ok
    end.

%% Restore original modules, destroy tables.
-spec stop() -> ok.
stop() ->
    lists:foreach(fun restore_module/1, seam_track:modules()),
    seam_track:destroy().

%% Zero all counters.
-spec reset() -> ok.
reset() ->
    seam_track:reset().

%% Instrument and load a module.
-spec compile_beam(module() | string()) -> {ok, module()} | {error, term()}.
compile_beam(ModOrPath) ->
    seam_instrument:compile_beam(ModOrPath).

%% Condition coverage for a module: #{cond_key() => {TrueCnt, FalseCnt}}.
-spec condition_coverage(module()) -> map().
condition_coverage(Mod) ->
    seam_track:conditions(Mod).

%% Decision coverage for a module: #{decision_key() => {SuccessCnt, FailureCnt}}.
-spec decision_coverage(module()) -> map().
decision_coverage(Mod) ->
    seam_track:decisions(Mod).

%% Cover-compatible analyse interface.
-spec analyse(module(), condition | decision) -> {ok, map()}.
analyse(Mod, condition) -> {ok, condition_coverage(Mod)};
analyse(Mod, decision)  -> {ok, decision_coverage(Mod)}.

%% Generate a coverage report.
-spec report(module(), text | html) -> iolist().
report(Mod, text) -> seam_report:text(Mod);
report(Mod, html) -> seam_report:html(Mod).

%% Export coverage data to file.
-spec export(string()) -> ok | {error, term()}.
export(Path) ->
    Data = #{conditions => seam_track:conditions(),
             decisions  => seam_track:decisions(),
             modules    => seam_track:modules()},
    file:write_file(Path, term_to_binary(Data)).

%% Import and merge coverage data from file.
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

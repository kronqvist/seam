%% @doc Public API for Seam condition-level coverage.
%%
%% Thin facade over `seam_instrument', `seam_track', `seam_analyse', and
%% `seam_report'. Naming follows `cover' where functionality overlaps.
%% Start the server, instrument modules, exercise them, then query or
%% export the results.
%%
%% Seam-specific additions beyond `cover':
%% <ul>
%%   <li>`compile_beam/2' — `#{mode => full | fast}' for lightweight fuzzing</li>
%%   <li>`take_new/0', `has_new/0' — incremental novelty detection</li>
%%   <li>`reset_edge_state/0' — clear per-process edge tracking</li>
%%   <li>`analyse(Mod, operand)' — comparison operand capture</li>
%%   <li>`analyse(Mod, edge)' — decision-to-decision edge coverage</li>
%% </ul>
-module(seam).
-include("seam.hrl").

-export([start/0, stop/0, reset/0, reset/1]).
-export([compile_beam/1, compile_beam/2]).
-export([modules/0, is_compiled/1]).
-export([analyse/1, analyse/2]).
-export([analyse_to_file/1, analyse_to_file/2]).
-export([take_new/0, has_new/0, reset_edge_state/0]).
-export([export/1, export/2, import/1]).

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

%% @doc Zero all counters, discoveries, operands, and edges.
-spec reset() -> ok.
reset() ->
    seam_track:reset().

%% @doc Zero counters for a single module. Error if not compiled.
-spec reset(module()) -> ok | {error, {not_compiled, module()}}.
reset(Mod) ->
    seam_track:reset(Mod).

%% @doc Instrument in full mode. Equivalent to `compile_beam(Mod, #{mode => full})'.
-spec compile_beam(module() | string()) -> {ok, module()} | {error, term()}.
compile_beam(ModOrPath) ->
    seam_instrument:compile_beam(ModOrPath).

%% @doc Instrument with options. `#{mode => full | fast}'.
-spec compile_beam(module() | string(), map()) -> {ok, module()} | {error, term()}.
compile_beam(ModOrPath, Opts) ->
    seam_instrument:compile_beam(ModOrPath, Opts).

%% @doc List all currently instrumented modules.
-spec modules() -> [module()].
modules() ->
    seam_track:modules().

%% @doc Check whether `Mod' is seam-compiled. Return `{file, File}'
%% if compiled, `false' otherwise. Mirrors `cover:is_compiled/1'.
-spec is_compiled(module()) -> {file, string()} | false.
is_compiled(Mod) ->
    case ets:lookup(?SEAM_MODULES, Mod) of
        [{Mod, _Beam, OrigPath}] -> {file, OrigPath};
        [] -> false
    end.

%% @doc Analyse `Mod' at the default (condition) level.
-spec analyse(module()) -> {ok, map()}.
analyse(Mod) ->
    analyse(Mod, condition).

%% @doc Query coverage by level: `condition', `decision', `edge', or `operand'.
-spec analyse(module(), condition | decision | edge | operand) -> {ok, map()}.
analyse(Mod, condition) -> {ok, seam_track:conditions(Mod)};
analyse(Mod, decision)  -> {ok, seam_track:decisions(Mod)};
analyse(Mod, edge)      -> {ok, seam_track:edges(Mod)};
analyse(Mod, operand)   -> {ok, seam_track:operands(Mod)}.

%% @doc Write a text coverage report to `Mod.SEAM.out'. Return `{ok, OutFile}'.
-spec analyse_to_file(module()) -> {ok, string()} | {error, term()}.
analyse_to_file(Mod) ->
    analyse_to_file(Mod, []).

%% @doc Write a coverage report to file. Options: `[html]' for HTML output,
%% `[{outfile, Path}]' for custom path.
-spec analyse_to_file(module(), list()) -> {ok, string()} | {error, term()}.
analyse_to_file(Mod, Opts) ->
    IsHtml = lists:member(html, Opts),
    OutFile = case proplists:get_value(outfile, Opts) of
        undefined ->
            Ext = case IsHtml of true -> ".SEAM.html"; false -> ".SEAM.out" end,
            atom_to_list(Mod) ++ Ext;
        Path -> Path
    end,
    Content = case IsHtml of
        true  -> seam_report:html(Mod);
        false -> seam_report:text(Mod)
    end,
    case file:write_file(OutFile, Content) of
        ok -> {ok, OutFile};
        Err -> Err
    end.

%% @doc Consume and return all new coverage discoveries since the last call.
-spec take_new() -> [discovery()].
take_new() ->
    seam_track:take_new().

%% @doc O(1) check: any new coverage since the last `take_new/0'?
-spec has_new() -> boolean().
has_new() ->
    ets:info(?SEAM_DISCOVERIES, size) > 0.

%% @doc Clear per-process edge tracking. Call between test iterations.
-spec reset_edge_state() -> ok.
reset_edge_state() ->
    seam_track:reset_edge_state().

%% @doc Serialize all coverage data to `Path'. Merge later with `import/1'.
-spec export(string()) -> ok | {error, term()}.
export(Path) ->
    Data = #{conditions => seam_track:conditions(),
             decisions  => seam_track:decisions(),
             modules    => seam_track:modules()},
    file:write_file(Path, term_to_binary(Data)).

%% @doc Serialize coverage data for a single module.
-spec export(string(), module()) -> ok | {error, term()}.
export(Path, Mod) ->
    Data = #{conditions => seam_track:conditions(Mod),
             decisions  => seam_track:decisions(Mod),
             modules    => [Mod]},
    file:write_file(Path, term_to_binary(Data)).

%% @doc Read coverage data from `Path' and merge into current counters.
-spec import(string()) -> ok | {error, term()}.
import(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            #{conditions := Conds, decisions := Decs} = binary_to_term(Bin),
            merge_coverage(Conds, Decs);
        Err -> Err
    end.

%%% Internal

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

restore_module(Mod) ->
    case ets:lookup(?SEAM_MODULES, Mod) of
        [{Mod, OrigBeam, OrigPath}] ->
            code:purge(Mod),
            code:load_binary(Mod, OrigPath, OrigBeam),
            seam_track:unregister_module(Mod);
        [] -> ok
    end.

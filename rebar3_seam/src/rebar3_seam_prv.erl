-module(rebar3_seam_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, seam).
-define(DEPS, [compile]).
-define(NAMESPACE, default).

%% -- Provider registration --------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, ?NAMESPACE},
        {bare, true},
        {deps, ?DEPS},
        {profiles, [test]},
        {short_desc, "Run tests with seam condition coverage"},
        {desc, "Instrument project modules, run eunit, report condition coverage."},
        {opts, cli_opts()}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

cli_opts() ->
    [{verbose,      $v, "verbose",      boolean,       "Print per-module table"},
     {min_coverage, $m, "min_coverage", integer,       "Fail below this coverage %"},
     {reset,        $r, "reset",        boolean,       "Reset counters before run"},
     {import,       $i, "import",       string,        "Import prior seam.dat"},
     {suite,        $s, "suite",        string,        "Comma-separated modules"},
     {level,        $l, "level",        atom,          "condition | decision"},
     {mode,         $M, "mode",         atom,          "full | fast"},
     {output,       $o, "output",       atom,          "html | text"}].

%% -- Lifecycle ---------------------------------------------------------------

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, term()}.
do(State) ->
    Opts = merge_opts(State),
    ensure_seam_on_path(),
    seam:start(),
    maybe_reset(Opts),
    maybe_import(Opts),
    Apps = rebar_state:project_apps(State),
    Mods = instrument_apps(Apps, Opts),
    TestResult = run_tests(Apps, Opts),
    OutDir = output_dir(State),
    filelib:ensure_dir(filename:join(OutDir, "x")),
    generate_reports(Mods, Opts, OutDir),
    Stats = gather_stats(Mods, Opts),
    print_summary(Stats, Opts),
    export_data(OutDir),
    ThresholdResult = check_threshold(Stats, Opts),
    seam:stop(),
    finalise(TestResult, ThresholdResult, State).

-spec format_error(term()) -> iolist().
format_error(eunit_failed) ->
    "EUnit tests failed";
format_error({min_coverage, Pct, Threshold}) ->
    io_lib:format("Coverage ~.1f% below minimum ~B%", [Pct, Threshold]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% -- Option merging ----------------------------------------------------------

merge_opts(State) ->
    Defaults = #{verbose      => false,
                 min_coverage => 0,
                 reset        => false,
                 import       => undefined,
                 suite        => all,
                 level        => condition,
                 mode         => full,
                 output       => html,
                 excl_mods    => []},
    RbarOpts = maps:from_list(rebar_state:get(State, seam, [])),
    {CliArgs, _} = rebar_state:command_parsed_args(State),
    CliOpts = cli_to_map(CliArgs),
    maps:merge(maps:merge(Defaults, RbarOpts), CliOpts).

cli_to_map(Args) ->
    lists:foldl(fun cli_kv/2, #{}, Args).

cli_kv({suite, S}, Acc) ->
    Mods = [list_to_atom(string:trim(T)) || T <- string:split(S, ",", all)],
    Acc#{suite => Mods};
cli_kv({K, V}, Acc) ->
    Acc#{K => V}.

%% -- Path setup --------------------------------------------------------------

ensure_seam_on_path() ->
    case code:which(seam_track) of
        non_existing ->
            rebar_api:abort("seam not found on code path", []);
        Path ->
            code:add_pathz(filename:dirname(Path))
    end.

%% -- Optional steps ----------------------------------------------------------

maybe_reset(#{reset := true})  -> seam:reset();
maybe_reset(_)                 -> ok.

maybe_import(#{import := undefined}) -> ok;
maybe_import(#{import := Path})      -> seam:import(Path).

%% -- Instrumentation ---------------------------------------------------------

instrument_apps(Apps, Opts) ->
    lists:flatmap(fun(App) -> instrument_app(App, Opts) end, Apps).

instrument_app(App, Opts) ->
    EbinDir = rebar_app_info:ebin_dir(App),
    Beams = filelib:wildcard(filename:join(EbinDir, "*.beam")),
    ExclMods = maps:get(excl_mods, Opts),
    Mode = maps:get(mode, Opts),
    lists:filtermap(
        fun(Beam) -> try_instrument(Beam, ExclMods, Mode) end,
        Beams).

try_instrument(BeamPath, ExclMods, Mode) ->
    Mod = list_to_atom(filename:basename(BeamPath, ".beam")),
    case should_skip(Mod, ExclMods) of
        true  -> false;
        false -> do_instrument(Mod, Mode)
    end.

should_skip(Mod, ExclMods) ->
    lists:member(Mod, ExclMods) orelse is_test_module(Mod).

is_test_module(Mod) ->
    S = atom_to_list(Mod),
    lists:suffix("_tests", S) orelse lists:suffix("_SUITE", S).

do_instrument(Mod, Mode) ->
    case seam:compile_beam(Mod, #{mode => Mode}) of
        {ok, Mod} ->
            {true, Mod};
        {error, Reason} ->
            rebar_api:warn("Skipping ~p: ~p", [Mod, Reason]),
            false
    end.

%% -- Test execution ----------------------------------------------------------

run_tests(Apps, Opts) ->
    Targets = test_targets(Apps, Opts),
    case eunit:test(Targets, []) of
        ok    -> ok;
        error -> {error, eunit_failed}
    end.

test_targets(Apps, #{suite := all}) ->
    [{application, binary_to_atom(rebar_app_info:name(App))}
     || App <- Apps];
test_targets(_Apps, #{suite := Mods}) ->
    Mods.

%% -- Reports -----------------------------------------------------------------

output_dir(State) ->
    BaseDir = rebar_dir:base_dir(State),
    filename:join([BaseDir, "seam"]).

generate_reports(Mods, Opts, OutDir) ->
    OutputFmt = maps:get(output, Opts),
    lists:foreach(
        fun(Mod) -> generate_report(Mod, OutputFmt, OutDir) end,
        Mods).

generate_report(Mod, html, OutDir) ->
    OutFile = filename:join(OutDir, atom_to_list(Mod) ++ ".SEAM.html"),
    case seam:analyse_to_file(Mod, [html, {outfile, OutFile}]) of
        {ok, _}         -> ok;
        {error, Reason} -> rebar_api:warn("Report failed for ~p: ~p", [Mod, Reason])
    end;
generate_report(Mod, text, OutDir) ->
    OutFile = filename:join(OutDir, atom_to_list(Mod) ++ ".SEAM.out"),
    case seam:analyse_to_file(Mod, [{outfile, OutFile}]) of
        {ok, _}         -> ok;
        {error, Reason} -> rebar_api:warn("Report failed for ~p: ~p", [Mod, Reason])
    end.

%% -- Statistics & summary ----------------------------------------------------

gather_stats(Mods, Opts) ->
    SummaryFn = case maps:get(level, Opts) of
        condition -> fun seam_analyse:condition_summary/1;
        decision  -> fun seam_analyse:decision_summary/1
    end,
    [{Mod, Cov, Total}
     || Mod <- Mods,
        {Cov, Total} <- [SummaryFn(Mod)]].

print_summary(Stats, Opts) ->
    {TotalCov, TotalAll} = sum_stats(Stats),
    TotalPct = pct(TotalCov, TotalAll),
    Level = maps:get(level, Opts),
    io:format("~n  Seam ~s coverage: ~.1f% (~B/~B)~n",
              [Level, TotalPct, TotalCov, TotalAll]),
    case maps:get(verbose, Opts) of
        true  -> print_table(Stats, TotalCov, TotalAll, TotalPct);
        false -> ok
    end,
    io:format("~n").

print_table(Rows, TotalCov, TotalAll, TotalPct) ->
    io:format("~n  ~-30s ~10s ~8s ~8s~n", ["Module", "Covered", "Total", "%"]),
    io:format("  ~s~n", [lists:duplicate(58, $-)]),
    lists:foreach(fun print_row/1, Rows),
    io:format("  ~s~n", [lists:duplicate(58, $-)]),
    io:format("  ~-30s ~10B ~8B ~7.1f%~n", ["TOTAL", TotalCov, TotalAll, TotalPct]).

print_row({Mod, Cov, Total}) ->
    io:format("  ~-30s ~10B ~8B ~7.1f%~n",
              [atom_to_list(Mod), Cov, Total, pct(Cov, Total)]).

sum_stats(Stats) ->
    lists:foldl(fun({_, C, T}, {AC, AT}) -> {AC + C, AT + T} end,
                {0, 0}, Stats).

pct(_, 0) -> 100.0;
pct(Cov, Total) -> Cov / Total * 100.

%% -- Export ------------------------------------------------------------------

export_data(OutDir) ->
    Path = filename:join(OutDir, "seam.dat"),
    seam:export(Path).

%% -- Threshold enforcement ---------------------------------------------------

check_threshold(Stats, #{min_coverage := Min}) when Min > 0 ->
    {TotalCov, TotalAll} = sum_stats(Stats),
    Pct = pct(TotalCov, TotalAll),
    case Pct >= Min of
        true  -> ok;
        false -> {error, {min_coverage, Pct, Min}}
    end;
check_threshold(_, _) ->
    ok.

%% -- Finalise ----------------------------------------------------------------

finalise(ok, ok, State) ->
    {ok, State};
finalise({error, eunit_failed}, _, _State) ->
    {error, {?MODULE, eunit_failed}};
finalise(ok, {error, {min_coverage, Pct, Threshold}}, _State) ->
    {error, {?MODULE, {min_coverage, Pct, Threshold}}}.

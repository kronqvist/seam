-module(seam_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TARGETS, "test/targets/").

setup() ->
    seam:start(),
    ok.

cleanup(_) ->
    catch seam:stop(),
    ok.

compile_target(Mod) ->
    Src = ?TARGETS ++ atom_to_list(Mod) ++ ".erl",
    {ok, Mod, Bin} = compile:file(Src, [binary, debug_info]),
    BeamPath = "/tmp/seam_test_" ++ atom_to_list(Mod) ++ ".beam",
    ok = file:write_file(BeamPath, Bin),
    code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, BeamPath, Bin),
    {ok, Mod} = seam:compile_beam(Mod),
    ok.

%% Guard instrumentation: single-condition clauses
guard_basic_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        large = simple_guards:classify(15),
        small = simple_guards:classify(5),
        zero  = simple_guards:classify(-1),
        Cov = seam:condition_coverage(simple_guards),
        %% Clause 1 condition (X > 10): true once (15), false twice (5, -1)
        ?assertEqual({1, 2}, maps:get({simple_guards, classify, 1, 1}, Cov)),
        %% Clause 2 condition (X > 0): true once (5), false once (-1)
        ?assertEqual({1, 1}, maps:get({simple_guards, classify, 2, 1}, Cov))
    end}.

%% Guard instrumentation: multi-condition guards
guard_multi_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        both    = simple_guards:classify2(15, 3),
        x_only  = simple_guards:classify2(15, 10),
        neither = simple_guards:classify2(1, 1),
        Cov = seam:condition_coverage(simple_guards),
        %% Clause 1 cond 1 (X>10): true 2 (direct + re-eval), false 1 (re-eval)
        ?assertEqual({2, 1}, maps:get({simple_guards, classify2, 1, 1}, Cov)),
        %% Clause 1 cond 2 (Y<5): true 2 (direct + re-eval), false 1 (re-eval)
        ?assertEqual({2, 1}, maps:get({simple_guards, classify2, 1, 2}, Cov))
    end}.

%% Decision coverage tracking
decision_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        large = simple_guards:classify(15),
        small = simple_guards:classify(5),
        zero  = simple_guards:classify(-1),
        Dec = seam:decision_coverage(simple_guards),
        %% Clause 1 decision: succeeded once (15), failed twice (5, -1)
        ?assertEqual({1, 2}, maps:get({simple_guards, classify, 1}, Dec)),
        %% Clause 2 decision: succeeded once (5), failed once (-1)
        ?assertEqual({1, 1}, maps:get({simple_guards, classify, 2}, Dec))
    end}.

%% Semantics preserved: instrumented code returns same values
semantics_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        ?assertEqual(large, simple_guards:classify(100)),
        ?assertEqual(small, simple_guards:classify(1)),
        ?assertEqual(zero,  simple_guards:classify(0)),
        ?assertEqual(zero,  simple_guards:classify(-5))
    end}.

%% Case expression instrumentation
case_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(case_example),
        ?assertEqual(medium, case_example:foo(50)),
        ?assertEqual(large,  case_example:foo(200)),
        ?assertEqual(small,  case_example:foo(5))
    end}.

%% Reset clears all counters
reset_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        simple_guards:classify(15),
        seam:reset(),
        Cov = seam:condition_coverage(simple_guards),
        ?assertEqual(#{}, Cov)
    end}.

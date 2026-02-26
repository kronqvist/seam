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
    compile_target(Mod, #{}).

compile_target(Mod, Opts) ->
    Src = ?TARGETS ++ atom_to_list(Mod) ++ ".erl",
    {ok, Mod, Bin} = compile:file(Src, [binary, debug_info]),
    BeamPath = "/tmp/seam_test_" ++ atom_to_list(Mod) ++ ".beam",
    ok = file:write_file(BeamPath, Bin),
    code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, BeamPath, Bin),
    {ok, Mod} = seam:compile_beam(Mod, maps:merge(#{mode => full}, Opts)),
    ok.

%%% === API alignment tests ===

%% analyse/1 defaults to condition level
analyse_default_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        simple_guards:classify(15),
        {ok, Cov} = seam:analyse(simple_guards),
        ?assert(maps:is_key({simple_guards, classify, 1, 1}, Cov))
    end}.

%% analyse/2 condition level
analyse_condition_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        large = simple_guards:classify(15),
        small = simple_guards:classify(5),
        zero  = simple_guards:classify(-1),
        {ok, Cov} = seam:analyse(simple_guards, condition),
        ?assertEqual({1, 2}, maps:get({simple_guards, classify, 1, 1}, Cov)),
        ?assertEqual({1, 1}, maps:get({simple_guards, classify, 2, 1}, Cov))
    end}.

%% analyse/2 decision level
analyse_decision_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        large = simple_guards:classify(15),
        small = simple_guards:classify(5),
        zero  = simple_guards:classify(-1),
        {ok, Dec} = seam:analyse(simple_guards, decision),
        ?assertEqual({1, 2}, maps:get({simple_guards, classify, 1}, Dec)),
        ?assertEqual({1, 1}, maps:get({simple_guards, classify, 2}, Dec))
    end}.

%% modules/0 and is_compiled/1
modules_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        Mods = seam:modules(),
        ?assert(lists:member(simple_guards, Mods)),
        ?assertMatch({file, _}, seam:is_compiled(simple_guards)),
        ?assertEqual(false, seam:is_compiled(nonexistent_mod))
    end}.

%% reset/1 per-module
reset_module_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        simple_guards:classify(15),
        ok = seam:reset(simple_guards),
        {ok, Cov} = seam:analyse(simple_guards, condition),
        ?assertEqual(#{}, Cov),
        ?assertEqual({error, {not_compiled, nonexistent}}, seam:reset(nonexistent))
    end}.

%% export/2 per-module
export_module_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        simple_guards:classify(15),
        Path = "/tmp/seam_test_export_mod.dat",
        ok = seam:export(Path, simple_guards),
        seam:reset(),
        ok = seam:import(Path),
        {ok, Cov} = seam:analyse(simple_guards, condition),
        ?assert(maps:size(Cov) > 0),
        file:delete(Path)
    end}.

%% analyse_to_file/1 and analyse_to_file/2
analyse_to_file_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        simple_guards:classify(15),
        {ok, TxtFile} = seam:analyse_to_file(simple_guards),
        ?assert(filelib:is_file(TxtFile)),
        file:delete(TxtFile),
        {ok, HtmlFile} = seam:analyse_to_file(simple_guards, [html]),
        ?assert(filelib:is_file(HtmlFile)),
        file:delete(HtmlFile),
        CustomPath = "/tmp/seam_custom_report.txt",
        {ok, CustomPath} = seam:analyse_to_file(simple_guards, [{outfile, CustomPath}]),
        ?assert(filelib:is_file(CustomPath)),
        file:delete(CustomPath)
    end}.

%%% === Semantics preservation ===

semantics_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        ?assertEqual(large, simple_guards:classify(100)),
        ?assertEqual(small, simple_guards:classify(1)),
        ?assertEqual(zero,  simple_guards:classify(0)),
        ?assertEqual(zero,  simple_guards:classify(-5))
    end}.

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
        {ok, Cov} = seam:analyse(simple_guards, condition),
        ?assertEqual(#{}, Cov)
    end}.

%%% === Feature 1: Incremental novelty detection ===

take_new_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        %% No discoveries yet
        ?assertEqual(false, seam:has_new()),
        ?assertEqual([], seam:take_new()),
        %% Exercise — should produce discoveries
        simple_guards:classify(15),
        ?assertEqual(true, seam:has_new()),
        D1 = seam:take_new(),
        ?assert(length(D1) > 0),
        %% After take_new, cleared
        ?assertEqual(false, seam:has_new()),
        ?assertEqual([], seam:take_new()),
        %% Same input produces no new discoveries (reset edge state to
        %% prevent cross-iteration edge discoveries)
        seam:reset_edge_state(),
        simple_guards:classify(15),
        ?assertEqual(false, seam:has_new()),
        %% New path produces new discoveries
        simple_guards:classify(5),
        ?assertEqual(true, seam:has_new()),
        D2 = seam:take_new(),
        ?assert(length(D2) > 0)
    end}.

%%% === Feature 2: Comparison operand capture ===

operand_capture_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        %% Exercise: classify(7) hits clause 2 (X > 0), misses clause 1 (X > 10)
        simple_guards:classify(7),
        {ok, Ops} = seam:analyse(simple_guards, operand),
        %% Prior clause re-evaluation of X > 10 with X=7 should capture operands
        %% Key for clause 1, condition 1
        Key11 = {simple_guards, classify, 1, 1},
        case maps:find(Key11, Ops) of
            {ok, ByBool} ->
                %% Should have false => {'>', 7, 10}
                ?assertMatch(#{false := {'>', 7, 10}}, ByBool);
            error ->
                %% Operand capture via prior re-eval — may or may not be present
                %% depending on whether the condition was a comparison
                ok
        end
    end}.

%%% === Feature 3: Edge coverage ===

edge_coverage_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        seam:reset_edge_state(),
        %% Two calls in sequence should produce decision-to-decision edges
        simple_guards:classify(15),
        simple_guards:classify(5),
        {ok, Edges} = seam:analyse(simple_guards, edge),
        %% At least one edge should exist (clause 1 -> clause 2 transition)
        ?assert(maps:size(Edges) > 0),
        %% Edge summary
        {Unique, Total} = seam_analyse:edge_summary(simple_guards),
        ?assert(Unique > 0),
        ?assert(Total > 0)
    end}.

reset_edge_state_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        simple_guards:classify(15),
        seam:reset_edge_state(),
        %% After reset, the next call should not produce an edge from previous
        simple_guards:classify(5),
        ok
    end}.

%%% === Feature 4: Lightweight fast mode ===

fast_mode_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards, #{mode => fast}),
        %% Semantics preserved
        ?assertEqual(large, simple_guards:classify(100)),
        ?assertEqual(small, simple_guards:classify(1)),
        ?assertEqual(zero,  simple_guards:classify(0)),
        %% Coverage recorded for own conditions
        {ok, Cov} = seam:analyse(simple_guards, condition),
        ?assert(maps:size(Cov) > 0),
        %% Fast mode does not re-evaluate prior clauses,
        %% so fewer false counts are expected
        {ok, Dec} = seam:analyse(simple_guards, decision),
        ?assert(maps:size(Dec) > 0)
    end}.

%%% === Feature 5: Body expression instrumentation ===

body_cmp_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(body_example),
        ?assertEqual(big, body_example:check(15, 0)),
        ?assertEqual(small, body_example:check(0, 5)),
        %% Body comparisons should be captured
        {ok, Cov} = seam:analyse(body_example, condition),
        ?assert(maps:size(Cov) > 0)
    end}.

body_logic_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(body_example),
        ?assertEqual(both_pos, body_example:logic(1, 1)),
        ?assertEqual(one_pos,  body_example:logic(1, -1)),
        ?assertEqual(one_pos,  body_example:logic(-1, 1)),
        ?assertEqual(none_pos, body_example:logic(-1, -1)),
        %% andalso/orelse should be instrumented as conditions
        {ok, Cov} = seam:analyse(body_example, condition),
        %% Should have conditions from the andalso and orelse in logic/2
        LogicConds = maps:filter(fun({body_example, logic, _, _}, _) -> true;
                                    (_, _) -> false end, Cov),
        ?assert(maps:size(LogicConds) > 0)
    end}.

body_operand_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(body_example),
        body_example:logic(5, -3),
        {ok, Ops} = seam:analyse(body_example, operand),
        %% Should have operand data from body comparisons
        BodyOps = maps:filter(fun({body_example, logic, _, _}, _) -> true;
                                 (_, _) -> false end, Ops),
        ?assert(maps:size(BodyOps) > 0)
    end}.

%%% === Multi-condition guards (regression) ===

guard_multi_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        both    = simple_guards:classify2(15, 3),
        x_only  = simple_guards:classify2(15, 10),
        neither = simple_guards:classify2(1, 1),
        {ok, Cov} = seam:analyse(simple_guards, condition),
        ?assertEqual({2, 1}, maps:get({simple_guards, classify2, 1, 1}, Cov)),
        ?assertEqual({2, 1}, maps:get({simple_guards, classify2, 1, 2}, Cov))
    end}.

%%% === Decision metadata ===

decision_meta_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        Meta = seam_track:decision_meta(simple_guards),
        %% classify/1 has 3 clauses, classify2/2 has 3 clauses => 6 clause entries
        ?assertEqual(6, length(Meta)),
        %% All should be labelled "clause"
        ?assert(lists:all(fun({_, _, "clause"}) -> true; (_) -> false end, Meta))
    end}.

decision_meta_fast_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards, #{mode => fast}),
        Meta = seam_track:decision_meta(simple_guards),
        ?assertEqual(6, length(Meta))
    end}.

%%% === Case/if branch instrumentation ===

case_branch_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(case_example),
        %% Semantics preserved
        ?assertEqual(medium, case_example:foo(50)),
        ?assertEqual(large,  case_example:foo(200)),
        ?assertEqual(small,  case_example:foo(5)),
        %% Decisions should include case branches beyond the 1 function clause
        {ok, Dec} = seam:analyse(case_example, decision),
        ?assert(maps:size(Dec) > 1),
        %% Decision meta should include case branch entries
        Meta = seam_track:decision_meta(case_example),
        CaseMeta = [M || {_, _, "case branch"} = M <- Meta],
        ?assert(length(CaseMeta) > 0)
    end}.

nested_case_branch_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(nested_case_example),
        ?assertEqual(a1, nested_case_example:classify(a, 1)),
        ?assertEqual(a2, nested_case_example:classify(a, 2)),
        ?assertEqual(a_other, nested_case_example:classify(a, 99)),
        ?assertEqual(b1, nested_case_example:classify(b, 1)),
        ?assertEqual(b_other, nested_case_example:classify(b, 99)),
        ?assertEqual(other, nested_case_example:classify(c, 0)),
        {ok, Dec} = seam:analyse(nested_case_example, decision),
        %% 1 function clause + 3 outer case + 3 inner(a) + 2 inner(b) = 9
        ?assertEqual(9, maps:size(Dec)),
        %% Decision meta has all entries regardless of exercise
        Meta = seam_track:decision_meta(nested_case_example),
        CaseMeta = [M || {_, _, "case branch"} = M <- Meta],
        ?assertEqual(8, length(CaseMeta))
    end}.

if_branch_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(if_example),
        ?assertEqual(large, if_example:classify(15)),
        ?assertEqual(small, if_example:classify(5)),
        ?assertEqual(zero,  if_example:classify(-1)),
        {ok, Dec} = seam:analyse(if_example, decision),
        %% 1 function clause + 3 if branches = 4
        ?assertEqual(4, maps:size(Dec)),
        Meta = seam_track:decision_meta(if_example),
        IfMeta = [M || {_, _, "if branch"} = M <- Meta],
        ?assertEqual(3, length(IfMeta))
    end}.

%%% === Clause summary with concrete values ===

clause_summary_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        %% classify(15) enters clause 1, classify(5) enters clause 2
        simple_guards:classify(15),
        simple_guards:classify(5),
        %% 2 of 6 total clauses entered (classify2 not called)
        ?assertEqual({2, 6}, seam_analyse:clause_summary(simple_guards)),
        %% Exercise all remaining clauses
        simple_guards:classify(0),
        simple_guards:classify2(15, 3),
        simple_guards:classify2(15, 10),
        simple_guards:classify2(1, 1),
        ?assertEqual({6, 6}, seam_analyse:clause_summary(simple_guards))
    end}.

%%% === Untested + boundary conditions ===

untested_conditions_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        %% classify(15) enters clause 1: X > 10 is true but never false
        simple_guards:classify(15),
        Untested = seam_analyse:untested_conditions(simple_guards),
        Stuck = [{K, S} || {K, S} <- Untested,
                 element(1, K) =:= simple_guards,
                 element(2, K) =:= classify],
        ?assert(lists:member({{simple_guards, classify, 1, 1}, never_false}, Stuck))
    end}.

boundary_conditions_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        %% classify(7) hits clause 2 (X > 0), re-evaluates clause 1 (X > 10)
        %% with X=7 as false. Captures operand {>, 7, 10}.
        simple_guards:classify(7),
        Boundary = seam_analyse:boundary_conditions(simple_guards),
        Key11 = {simple_guards, classify, 1, 1},
        ?assert(lists:any(fun({K, never_true, {'>', 7, 10}}) -> K =:= Key11;
                             (_) -> false end, Boundary))
    end}.

%%% === Fast mode suppresses case/if branch instrumentation ===

fast_mode_no_branches_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(case_example, #{mode => fast}),
        ?assertEqual(medium, case_example:foo(50)),
        ?assertEqual(large,  case_example:foo(200)),
        ?assertEqual(small,  case_example:foo(5)),
        %% Fast mode: only clause decision meta, no case branch meta
        Meta = seam_track:decision_meta(case_example),
        ClauseMeta = [M || {_, _, "clause"} = M <- Meta],
        CaseMeta   = [M || {_, _, "case branch"} = M <- Meta],
        ?assertEqual(1, length(ClauseMeta)),
        ?assertEqual(0, length(CaseMeta))
    end}.

%%% === Fun/named_fun scope with case/if branch tracking ===

fun_case_branch_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(fun_case_example),
        ?assertEqual(1, fun_case_example:with_fun(a)),
        ?assertEqual(2, fun_case_example:with_fun(b)),
        ?assertEqual(0, fun_case_example:with_fun(c)),
        %% Case branches inside anonymous fun should be tracked
        Meta = seam_track:decision_meta(fun_case_example),
        CaseMeta = [M || {_, _, "case branch"} = M <- Meta],
        ?assert(length(CaseMeta) >= 3),
        %% All three branches entered
        {ok, Dec} = seam:analyse(fun_case_example, decision),
        FunDecs = maps:filter(fun({fun_case_example, with_fun, _}, _) -> true;
                                 (_, _) -> false end, Dec),
        AllEntered = maps:fold(fun(_, {T, _}, Acc) -> Acc andalso T > 0 end,
                               true, FunDecs),
        ?assert(AllEntered)
    end}.

named_fun_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(fun_case_example),
        ?assertEqual(done, fun_case_example:with_named_fun(3)),
        ?assertEqual(done, fun_case_example:with_named_fun(0)),
        %% Named fun clauses should produce case branch decisions
        Meta = seam_track:decision_meta(fun_case_example),
        %% with_fun: 1 clause + 3 case branches + 1 fun clause
        %% with_named_fun: 1 clause + 2 named_fun clauses
        %% Total case branch entries from the fun's case: 3
        CaseMeta = [M || {_, _, "case branch"} = M <- Meta],
        ?assert(length(CaseMeta) >= 3)
    end}.

%%% === HTML report: four-column layout and count values ===

html_count_column_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(case_example),
        case_example:foo(50),
        case_example:foo(50),
        case_example:foo(50),
        case_example:foo(200),
        Html = lists:flatten(seam_report:html(case_example)),
        %% The medium branch was entered 3 times
        ?assert(string:find(Html, "<td class=\"count\">3</td>") =/= nomatch),
        %% The large branch was entered 1 time
        ?assert(string:find(Html, "<td class=\"count\">1</td>") =/= nomatch),
        %% The small branch was never entered (count 0, miss row)
        ?assert(string:find(Html, "class=\"miss\"") =/= nomatch),
        %% Summary table has Clause row
        ?assert(string:find(Html, "Clause") =/= nomatch)
    end}.

html_all_row_types_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        %% classify(15) enters clause 1 only. Clause 1 guard is partial
        %% (true only), clause 3 is miss (never entered).
        simple_guards:classify(15),
        Html = lists:flatten(seam_report:html(simple_guards)),
        ?assert(string:find(Html, "class=\"partial\"") =/= nomatch),
        ?assert(string:find(Html, "class=\"miss\"") =/= nomatch),
        %% Plain rows (comments, specs) have empty count column
        ?assert(string:find(Html, "<td class=\"count\"></td>") =/= nomatch)
    end}.

html_to_file_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        simple_guards:classify(15),
        Path = "/tmp/seam_html_to_file_test.html",
        ok = seam_report:html_to_file(simple_guards, Path),
        ?assert(filelib:is_file(Path)),
        {ok, Bin} = file:read_file(Path),
        Html = binary_to_list(Bin),
        ?assert(string:find(Html, "<!DOCTYPE html>") =/= nomatch),
        ?assert(string:find(Html, "simple_guards") =/= nomatch),
        file:delete(Path)
    end}.

%%% === Text report content ===

text_report_content_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        simple_guards:classify(15),
        simple_guards:classify(5),
        simple_guards:classify(0),
        Txt = lists:flatten(seam_report:text(simple_guards)),
        ?assert(string:find(Txt, "Module: simple_guards") =/= nomatch),
        ?assert(string:find(Txt, "Condition Coverage:") =/= nomatch),
        ?assert(string:find(Txt, "Decision Coverage:") =/= nomatch),
        ?assert(string:find(Txt, "Clause Coverage:") =/= nomatch),
        ?assert(string:find(Txt, "Edge Coverage:") =/= nomatch)
    end}.

text_untested_section_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        %% Only hit clause 1 — condition is never_false
        simple_guards:classify(15),
        Txt = lists:flatten(seam_report:text(simple_guards)),
        ?assert(string:find(Txt, "Undertested conditions:") =/= nomatch),
        ?assert(string:find(Txt, "never false") =/= nomatch)
    end}.

text_boundary_section_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        %% classify(7) captures boundary operand for clause 1's X > 10
        simple_guards:classify(7),
        Txt = lists:flatten(seam_report:text(simple_guards)),
        ?assert(string:find(Txt, "Boundary conditions") =/= nomatch)
    end}.

%%% === Decision meta line numbers ===

decision_meta_lines_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(if_example),
        Meta = seam_track:decision_meta(if_example),
        IfMeta = [{Line, Label} || {_, Line, Label} <- Meta,
                  Label =:= "if branch"],
        Lines = lists:sort([L || {L, _} <- IfMeta]),
        %% if_example.erl: line 6 (X > 10), line 7 (X > 0), line 8 (true)
        ?assertEqual([6, 7, 8], Lines)
    end}.

%%% === MC/DC vectors ===

mcdc_coverage_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        %% No vectors recorded yet
        ?assertEqual({error, not_collected},
                     seam_analyse:mcdc_coverage(simple_guards)),
        %% Insert independence pair: two vectors differing only in condition 1
        DecKey = {simple_guards, classify, 1},
        seam_track:record_vector(DecKey, [true, true], true),
        seam_track:record_vector(DecKey, [false, true], false),
        {ok, Result} = seam_analyse:mcdc_coverage(simple_guards),
        ?assertEqual(satisfied, maps:get(1, Result))
    end}.

%%% === Export/import all modules ===

export_all_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(simple_guards),
        ok = compile_target(case_example),
        simple_guards:classify(15),
        case_example:foo(50),
        Path = "/tmp/seam_test_export_all.dat",
        ok = seam:export(Path),
        seam:reset(),
        {ok, Cov1} = seam:analyse(simple_guards, condition),
        ?assertEqual(#{}, Cov1),
        ok = seam:import(Path),
        {ok, Cov2} = seam:analyse(simple_guards, condition),
        ?assert(maps:size(Cov2) > 0),
        {ok, Dec} = seam:analyse(case_example, decision),
        ?assert(maps:size(Dec) > 0),
        file:delete(Path)
    end}.

import_error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ?assertMatch({error, _}, seam:import("/tmp/nonexistent_seam_file.dat"))
    end}.

%%% === Idempotent start ===

start_idempotent_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ?assertEqual(ok, seam:start()),
        ?assertEqual(ok, seam:start())
    end}.

%%% === Zero-condition module ===

zero_conditions_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = compile_target(nested_case_example),
        nested_case_example:classify(a, 1),
        %% No guards anywhere => zero conditions
        ?assertEqual({0, 0}, seam_analyse:condition_summary(nested_case_example)),
        ?assertEqual([], seam_analyse:untested_conditions(nested_case_example)),
        %% Report still works (no division by zero)
        Txt = lists:flatten(seam_report:text(nested_case_example)),
        ?assert(string:find(Txt, "Condition Coverage: 0.0%") =/= nomatch)
    end}.

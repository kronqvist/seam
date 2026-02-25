-module(seam_report).

-export([text/1, html/1]).

%% Plain-text coverage report for a module.
-spec text(module()) -> iolist().
text(Mod) ->
    {CondCov, CondTotal} = seam_analyse:condition_summary(Mod),
    {DecCov, DecTotal} = seam_analyse:decision_summary(Mod),
    Untested = seam_analyse:untested_conditions(Mod),
    CondPct = pct(CondCov, CondTotal),
    DecPct = pct(DecCov, DecTotal),
    [io_lib:format("Module: ~s~n", [Mod]),
     io_lib:format("Condition Coverage: ~.1f% (~p/~p conditions evaluated both ways)~n",
                   [CondPct, CondCov, CondTotal]),
     io_lib:format("Decision Coverage:  ~.1f% (~p/~p decisions tried both outcomes)~n",
                   [DecPct, DecCov, DecTotal]),
     format_untested(Untested)].

format_untested([]) -> [];
format_untested(Items) ->
    ["\nUndertested conditions:\n" |
     [format_untested_item(I) || I <- Items]].

format_untested_item({{Mod, Fun, Clause, Cond}, Status}) ->
    StatusStr = case Status of
        never_true  -> "never true";
        never_false -> "never false"
    end,
    io_lib:format("  ~s:~s/clause ~p, condition ~p -- ~s~n",
                  [Mod, Fun, Clause, Cond, StatusStr]).

%% HTML coverage report with colour-coded summary.
-spec html(module()) -> iolist().
html(Mod) ->
    {CondCov, CondTotal} = seam_analyse:condition_summary(Mod),
    {DecCov, DecTotal} = seam_analyse:decision_summary(Mod),
    Untested = seam_analyse:untested_conditions(Mod),
    CondPct = pct(CondCov, CondTotal),
    DecPct = pct(DecCov, DecTotal),
    ["<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">\n",
     "<title>Seam: ", atom_to_list(Mod), "</title>\n",
     "<style>",
     "body{font-family:monospace;margin:2em}",
     ".green{color:#080} .red{color:#a00} .yellow{color:#a80}",
     "table{border-collapse:collapse} td,th{padding:4px 12px;border:1px solid #ccc}",
     "</style></head><body>\n",
     "<h2>Seam Coverage: ", atom_to_list(Mod), "</h2>\n",
     "<table><tr><th>Metric</th><th>Covered</th><th>Total</th><th>%</th></tr>\n",
     row("Condition", CondCov, CondTotal, CondPct),
     row("Decision", DecCov, DecTotal, DecPct),
     "</table>\n",
     html_untested(Untested),
     "</body></html>\n"].

row(Label, Cov, Total, Pct) ->
    Class = colour_class(Pct),
    io_lib:format("<tr><td>~s</td><td>~p</td><td>~p</td>"
                  "<td class=\"~s\">~.1f%</td></tr>\n",
                  [Label, Cov, Total, Class, Pct]).

html_untested([]) -> [];
html_untested(Items) ->
    ["<h3>Undertested Conditions</h3>\n<ul>\n",
     [io_lib:format("<li class=\"red\">~s:~s/clause ~p, cond ~p &mdash; ~s</li>\n",
                    [M, F, C, Cd, status_str(S)])
      || {{M, F, C, Cd}, S} <- Items],
     "</ul>\n"].

status_str(never_true)  -> "never true";
status_str(never_false) -> "never false".

colour_class(Pct) when Pct >= 80.0 -> "green";
colour_class(Pct) when Pct >= 50.0 -> "yellow";
colour_class(_) -> "red".

pct(_, 0) -> 0.0;
pct(Cov, Total) -> Cov / Total * 100.0.

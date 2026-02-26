%% @doc Coverage report generation.
%%
%% Plain-text summaries and source-annotated HTML reports. The HTML report
%% uses a four-column table (line number, execution count, condition badges,
%% source) with colour-coded rows. The count column shows how many times
%% each clause or branch was entered — the line-level analogue of
%% `cover:analyse_to_file/2'. Guard lines additionally carry per-condition
%% badges showing true/false counts for MC/DC analysis. Text reports
%% include operand data for stuck conditions and edge coverage summary.
-module(seam_report).
-include("seam.hrl").

-export([text/1, html/1, html_to_file/2]).

%% @doc Plain-text coverage summary for `Mod'.
-spec text(module()) -> iolist().
text(Mod) ->
    {CondCov, CondTotal} = seam_analyse:condition_summary(Mod),
    {DecCov, DecTotal} = seam_analyse:decision_summary(Mod),
    {ClauseCov, ClauseTotal} = seam_analyse:clause_summary(Mod),
    Untested = seam_analyse:untested_conditions(Mod),
    Boundary = seam_analyse:boundary_conditions(Mod),
    {UniqueEdges, TotalTrans} = seam_analyse:edge_summary(Mod),
    CondPct = pct(CondCov, CondTotal),
    DecPct = pct(DecCov, DecTotal),
    ClausePct = pct(ClauseCov, ClauseTotal),
    [io_lib:format("Module: ~s~n", [Mod]),
     io_lib:format("Condition Coverage: ~.1f% (~p/~p conditions evaluated both ways)~n",
                   [CondPct, CondCov, CondTotal]),
     io_lib:format("Decision Coverage:  ~.1f% (~p/~p decisions tried both outcomes)~n",
                   [DecPct, DecCov, DecTotal]),
     io_lib:format("Clause Coverage:    ~.1f% (~p/~p clauses/branches entered)~n",
                   [ClausePct, ClauseCov, ClauseTotal]),
     io_lib:format("Edge Coverage:      ~p unique edges, ~p total transitions~n",
                   [UniqueEdges, TotalTrans]),
     format_untested(Untested),
     format_boundary(Boundary)].

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

format_boundary([]) -> [];
format_boundary(Items) ->
    ["\nBoundary conditions (closest miss operands):\n" |
     [format_boundary_item(I) || I <- Items]].

format_boundary_item({{Mod, Fun, Clause, Cond}, Status, {Op, Lhs, Rhs}}) ->
    StatusStr = case Status of
        never_true  -> "never true";
        never_false -> "never false"
    end,
    io_lib:format("  ~s:~s/clause ~p, condition ~p -- ~s  (last: ~p ~s ~p)~n",
                  [Mod, Fun, Clause, Cond, StatusStr, Lhs, Op, Rhs]).

%% @doc Source-annotated HTML report with execution counts and condition badges.
-spec html(module()) -> iolist().
html(Mod) ->
    {CondCov, CondTotal} = seam_analyse:condition_summary(Mod),
    {DecCov, DecTotal} = seam_analyse:decision_summary(Mod),
    {ClauseCov, ClauseTotal} = seam_analyse:clause_summary(Mod),
    CondPct = pct(CondCov, CondTotal),
    DecPct = pct(DecCov, DecTotal),
    ClausePct = pct(ClauseCov, ClauseTotal),
    Cov = seam_track:conditions(Mod),
    Meta = seam_track:meta(Mod),
    ByLine = group_meta_by_line(Meta, Cov),
    Dec = seam_track:decisions(Mod),
    DecMeta = seam_track:decision_meta(Mod),
    DecByLine = group_dec_meta_by_line(DecMeta, Dec),
    SrcLines = read_source(Mod),
    [html_header(Mod),
     summary_table(CondCov, CondTotal, CondPct,
                   DecCov, DecTotal, DecPct,
                   ClauseCov, ClauseTotal, ClausePct),
     source_table(SrcLines, ByLine, DecByLine),
     html_footer()].

%% @doc Write the HTML report for `Mod' to `Path'.
-spec html_to_file(module(), string()) -> ok | {error, term()}.
html_to_file(Mod, Path) ->
    file:write_file(Path, html(Mod)).

%%% Source reading

read_source(Mod) ->
    case find_source(Mod) of
        {ok, Path} ->
            case file:read_file(Path) of
                {ok, Bin} -> string:split(binary_to_list(Bin), "\n", all);
                _ -> []
            end;
        error -> []
    end.

find_source(Mod) ->
    case code:which(Mod) of
        Path when is_list(Path) ->
            case beam_lib:chunks(Path, [compile_info]) of
                {ok, {Mod, [{compile_info, Info}]}} ->
                    case proplists:get_value(source, Info) of
                        undefined -> guess_source(Mod);
                        Src -> check_exists(Src, Mod)
                    end;
                _ -> guess_source(Mod)
            end;
        _ -> guess_source(Mod)
    end.

check_exists(Path, Mod) ->
    case filelib:is_file(Path) of
        true -> {ok, Path};
        false -> guess_source(Mod)
    end.

guess_source(Mod) ->
    Name = atom_to_list(Mod) ++ ".erl",
    Candidates = ["src/" ++ Name, "test/targets/" ++ Name, Name],
    case lists:dropwhile(fun(P) -> not filelib:is_file(P) end, Candidates) of
        [P | _] -> {ok, P};
        [] -> error
    end.

%%% Metadata grouping

group_meta_by_line(Meta, Cov) ->
    lists:foldl(fun({Key, Line, ExprStr}, Acc) ->
        {T, F} = maps:get(Key, Cov, {0, 0}),
        Entry = {Key, ExprStr, T, F},
        maps:update_with(Line, fun(L) -> L ++ [Entry] end, [Entry], Acc)
    end, #{}, Meta).

group_dec_meta_by_line(DecMeta, Dec) ->
    lists:foldl(fun({Key, Line, Label}, Acc) ->
        {T, _F} = maps:get(Key, Dec, {0, 0}),
        Entry = {Key, Label, T},
        maps:update_with(Line, fun(L) -> L ++ [Entry] end, [Entry], Acc)
    end, #{}, DecMeta).

%%% HTML generation

html_header(Mod) ->
    ["<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n",
     "<title>Seam: ", atom_to_list(Mod), "</title>\n",
     "<style>\n", css(), "</style>\n",
     "</head>\n<body>\n",
     "<h1>Seam Coverage: <code>", atom_to_list(Mod), "</code></h1>\n"].

html_footer() ->
    "</body>\n</html>\n".

css() ->
    "body { font-family: system-ui, sans-serif; margin: 2em; }\n"
    "h1 { font-size: 1.4em; }\n"
    "h2 { font-size: 1.1em; margin-top: 2em; }\n"
    "table.summary { border-collapse: collapse; margin-bottom: 2em; }\n"
    "table.summary td, table.summary th { padding: 4px 14px; border: 1px solid #ccc; }\n"
    "table.summary .green { color: #1a7f37; }\n"
    "table.summary .yellow { color: #9a6700; }\n"
    "table.summary .red { color: #cf222e; }\n"
    "table.source { border-collapse: collapse; font-size: 13px; width: 100%; }\n"
    "table.source td { padding: 0 8px; white-space: pre; font-family: monospace; }\n"
    "table.source td.line { text-align: right; color: #888; border-right: 1px solid #ddd; "
        "user-select: none; width: 1%; }\n"
    "table.source td.line a { color: #888; text-decoration: none; }\n"
    "table.source td.line a:hover { text-decoration: underline; }\n"
    "td.count { text-align: right; color: #555; width: 1%; "
        "border-right: 1px solid #ddd; font-size: 12px; }\n"
    "td.annot { font-size: 11px; color: #555; width: 1%; white-space: nowrap; }\n"
    "tr.hit td.line { background: #cdffd8; }\n"
    "tr.hit td.count { background: #cdffd8; color: #1a7f37; }\n"
    "tr.hit td.src  { background: #e6ffed; }\n"
    "tr.miss td.line { background: #ffdce0; }\n"
    "tr.miss td.count { background: #ffdce0; color: #cf222e; }\n"
    "tr.miss td.src  { background: #ffeef0; }\n"
    "tr.partial td.line { background: #fff3cd; }\n"
    "tr.partial td.count { background: #fff3cd; color: #9a6700; }\n"
    "tr.partial td.src  { background: #fff8e1; }\n"
    ".cond { display: inline-block; margin-right: 8px; padding: 1px 4px; "
        "border-radius: 3px; font-size: 11px; }\n"
    ".cond-full { background: #cdffd8; color: #1a7f37; }\n"
    ".cond-partial { background: #fff3cd; color: #9a6700; }\n"
    ".cond-none { background: #ffdce0; color: #cf222e; }\n".

summary_table(CondCov, CondTotal, CondPct, DecCov, DecTotal, DecPct,
              ClauseCov, ClauseTotal, ClausePct) ->
    ["<table class=\"summary\">\n",
     "<tr><th>Metric</th><th>Covered</th><th>Total</th><th>%</th></tr>\n",
     summary_row("Condition", CondCov, CondTotal, CondPct),
     summary_row("Decision", DecCov, DecTotal, DecPct),
     summary_row("Clause", ClauseCov, ClauseTotal, ClausePct),
     "</table>\n"].

summary_row(Label, Cov, Total, Pct) ->
    Class = colour_class(Pct),
    io_lib:format("<tr><td>~s</td><td>~p</td><td>~p</td>"
                  "<td class=\"~s\">~.1f%</td></tr>\n",
                  [Label, Cov, Total, Class, Pct]).

source_table([], _ByLine, _DecByLine) ->
    "<p><em>Source file not found.</em></p>\n";
source_table(SrcLines, ByLine, DecByLine) ->
    ["<h2>Source</h2>\n",
     "<table class=\"source\">\n<tbody>\n",
     source_rows(SrcLines, ByLine, DecByLine, 1),
     "</tbody>\n</table>\n"].

source_rows([], _ByLine, _DecByLine, _N) -> [];
source_rows([Line | Rest], ByLine, DecByLine, N) ->
    Row = source_row(N, Line,
                     maps:get(N, ByLine, []),
                     maps:get(N, DecByLine, [])),
    [Row | source_rows(Rest, ByLine, DecByLine, N + 1)].

%% Four-column layout: line | count | condition badges | source.
%% Plain line — no instrumentation data.
source_row(N, Src, [], []) ->
    io_lib:format("<tr>"
                  "<td class=\"line\" id=\"L~p\"><a href=\"#L~p\">~p</a></td>"
                  "<td class=\"count\"></td>"
                  "<td class=\"annot\"></td>"
                  "<td class=\"src\">~s</td>"
                  "</tr>\n", [N, N, N, escape(Src)]);
%% Decision-only line — show count, no condition badges.
source_row(N, Src, [], DecEntries) ->
    Count = line_count(DecEntries),
    Class = case Count > 0 of true -> "hit"; false -> "miss" end,
    io_lib:format("<tr class=\"~s\">"
                  "<td class=\"line\" id=\"L~p\"><a href=\"#L~p\">~p</a></td>"
                  "<td class=\"count\">~p</td>"
                  "<td class=\"annot\"></td>"
                  "<td class=\"src\">~s</td>"
                  "</tr>\n",
                  [Class, N, N, N, Count, escape(Src)]);
%% Condition line — show count from decisions + condition badges.
source_row(N, Src, Conditions, DecEntries) ->
    Count = line_count(DecEntries),
    Class = line_class(Conditions),
    Annots = [cond_badge(C) || C <- Conditions],
    io_lib:format("<tr class=\"~s\">"
                  "<td class=\"line\" id=\"L~p\"><a href=\"#L~p\">~p</a></td>"
                  "<td class=\"count\">~s</td>"
                  "<td class=\"annot\">~s</td>"
                  "<td class=\"src\">~s</td>"
                  "</tr>\n",
                  [Class, N, N, N, format_count(Count), Annots, escape(Src)]).

%% @doc Maximum true-count across all decisions on a line.
line_count(DecEntries) ->
    lists:foldl(fun({_, _, T}, Max) -> max(T, Max) end, 0, DecEntries).

%% @doc Format a count for display. Zero shows as "0"; no decisions shows empty.
format_count(0) -> "0";
format_count(N) -> integer_to_list(N).

line_class(Conditions) ->
    AllFull = lists:all(fun({_, _, T, F}) -> T > 0 andalso F > 0 end, Conditions),
    AnyHit = lists:any(fun({_, _, T, F}) -> T > 0 orelse F > 0 end, Conditions),
    case {AllFull, AnyHit} of
        {true, _} -> "hit";
        {_, true} -> "partial";
        _ -> "miss"
    end.

cond_badge({_Key, ExprStr, T, F}) ->
    Class = case {T > 0, F > 0} of
        {true, true}  -> "cond-full";
        {true, false} -> "cond-partial";
        {false, true} -> "cond-partial";
        {false, false} -> "cond-none"
    end,
    Trimmed = string:trim(ExprStr),
    io_lib:format("<span class=\"cond ~s\" title=\"true:~p false:~p\">~s</span>",
                  [Class, T, F, escape(Trimmed)]).

escape(Str) ->
    lists:flatmap(fun($<) -> "&lt;";
                     ($>) -> "&gt;";
                     ($&) -> "&amp;";
                     ($") -> "&quot;";
                     (C)  -> [C]
                  end, Str).

colour_class(Pct) when Pct >= 80.0 -> "green";
colour_class(Pct) when Pct >= 50.0 -> "yellow";
colour_class(_) -> "red".

pct(_, 0) -> 0.0;
pct(Cov, Total) -> Cov / Total * 100.0.

%% @doc Coverage metrics computed from `seam_track' data.
%%
%% Condition summary, decision summary, untested condition detection,
%% boundary condition analysis, edge summary, and MC/DC independence
%% pair computation. All functions are pure queries over the ETS
%% tables — no side effects.
-module(seam_analyse).
-include("seam.hrl").

-export([condition_summary/1, decision_summary/1, clause_summary/1]).
-export([untested_conditions/1]).
-export([boundary_conditions/1]).
-export([edge_summary/1]).
-export([mcdc_coverage/1]).

%% @doc Count of conditions evaluated both true and false vs total conditions.
-spec condition_summary(module()) -> {Covered :: non_neg_integer(),
                                       Total :: non_neg_integer()}.
condition_summary(Mod) ->
    Cov = seam_track:conditions(Mod),
    Total = maps:size(Cov),
    Covered = maps:fold(fun(_, {T, F}, Acc) ->
        case T > 0 andalso F > 0 of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, Cov),
    {Covered, Total}.

%% @doc Count of decisions observed both succeeding and failing vs total decisions.
-spec decision_summary(module()) -> {Covered :: non_neg_integer(),
                                      Total :: non_neg_integer()}.
decision_summary(Mod) ->
    Dec = seam_track:decisions(Mod),
    Total = maps:size(Dec),
    Covered = maps:fold(fun(_, {S, F}, Acc) ->
        case S > 0 andalso F > 0 of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, Dec),
    {Covered, Total}.

%% @doc Count of decisions entered at least once vs total decisions.
%% Reachability metric: measures clause/branch entry, not both-sides coverage.
-spec clause_summary(module()) -> {Reached :: non_neg_integer(),
                                    Total :: non_neg_integer()}.
clause_summary(Mod) ->
    Dec = seam_track:decisions(Mod),
    Total = maps:size(Dec),
    Reached = maps:fold(fun(_, {T, _}, Acc) ->
        case T > 0 of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, Dec),
    {Reached, Total}.

%% @doc Conditions stuck on one side: never true or never false.
-spec untested_conditions(module()) -> [{cond_key(), never_true | never_false}].
untested_conditions(Mod) ->
    Cov = seam_track:conditions(Mod),
    maps:fold(fun(Key, {T, F}, Acc) ->
        case {T, F} of
            {0, _} -> [{Key, never_true} | Acc];
            {_, 0} -> [{Key, never_false} | Acc];
            _      -> Acc
        end
    end, [], Cov).

%% @doc Stuck conditions annotated with the most recent operand miss.
%% Cross-references `untested_conditions/1' with `seam_track:operands/1'
%% to report what values nearly flipped a stuck condition.
-spec boundary_conditions(module()) ->
    [{cond_key(), never_true | never_false, {atom(), term(), term()}}].
boundary_conditions(Mod) ->
    Untested = untested_conditions(Mod),
    Ops = seam_track:operands(Mod),
    lists:filtermap(fun({Key, Status}) ->
        case maps:find(Key, Ops) of
            {ok, ByBool} ->
                %% For never_true, look for a false operand (closest miss)
                %% For never_false, look for a true operand
                MissBool = case Status of
                    never_true  -> false;
                    never_false -> true
                end,
                case maps:find(MissBool, ByBool) of
                    {ok, OpTriple} -> {true, {Key, Status, OpTriple}};
                    error -> false
                end;
            error -> false
        end
    end, Untested).

%% @doc Edge summary: `{UniqueEdges, TotalTransitions}'.
-spec edge_summary(module()) -> {non_neg_integer(), non_neg_integer()}.
edge_summary(Mod) ->
    Edges = seam_track:edges(Mod),
    Unique = maps:size(Edges),
    Total = maps:fold(fun(_, Cnt, Acc) -> Acc + Cnt end, 0, Edges),
    {Unique, Total}.

%% @doc Compute MC/DC independence pairs from collected test vectors.
%% Return `{error, not_collected}' if no vectors exist for `Mod'.
-spec mcdc_coverage(module()) -> {ok, map()} | {error, not_collected}.
mcdc_coverage(Mod) ->
    case ets:whereis(?SEAM_VECTORS) of
        undefined -> {error, not_collected};
        _ ->
            Vectors = ets:match_object(?SEAM_VECTORS, {{Mod, '_', '_'}, '_', '_'}),
            case Vectors of
                [] -> {error, not_collected};
                _  -> {ok, compute_mcdc(Vectors)}
            end
    end.

compute_mcdc(Vectors) ->
    ByDecision = group_by_decision(Vectors),
    maps:fold(fun(_DecKey, Vecs, Acc) ->
        NumConds = case Vecs of
            [{_, Cs, _} | _] -> length(Cs);
            _ -> 0
        end,
        CondResults = check_mcdc_conditions(Vecs, NumConds),
        maps:merge(Acc, CondResults)
    end, #{}, ByDecision).

group_by_decision(Vectors) ->
    lists:foldl(fun({DecKey, CondVals, Outcome}, Acc) ->
        maps:update_with(DecKey,
            fun(L) -> [{DecKey, CondVals, Outcome} | L] end,
            [{DecKey, CondVals, Outcome}], Acc)
    end, #{}, Vectors).

check_mcdc_conditions(Vecs, NumConds) ->
    lists:foldl(fun(CondIdx, Acc) ->
        Status = find_independence_pair(Vecs, CondIdx),
        maps:put(CondIdx, Status, Acc)
    end, #{}, lists:seq(1, NumConds)).

find_independence_pair(Vecs, CondIdx) ->
    Pairs = [{A, B} || {_, CA, OA} = A <- Vecs,
                        {_, CB, OB} = B <- Vecs,
                        OA =/= OB,
                        differs_only_at(CA, CB, CondIdx)],
    case Pairs of
        [{_, _} | _] -> satisfied;
        [] -> unsatisfied
    end.

differs_only_at(As, Bs, Idx) ->
    differs_only_at(As, Bs, Idx, 1).

differs_only_at([], [], _Idx, _Pos) -> true;
differs_only_at([A | As], [B | Bs], Idx, Pos) ->
    case Pos =:= Idx of
        true  -> A =/= B andalso differs_only_at(As, Bs, Idx, Pos + 1);
        false -> A =:= B andalso differs_only_at(As, Bs, Idx, Pos + 1)
    end.

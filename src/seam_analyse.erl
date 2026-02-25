%% @doc Coverage metrics computed from `seam_track' data.
%%
%% Condition summary, decision summary, untested condition detection, and
%% MC/DC independence pair computation. All functions are pure queries over
%% the ETS tables — no side effects.
-module(seam_analyse).
-include("seam.hrl").

-export([condition_summary/1, decision_summary/1]).
-export([untested_conditions/1]).
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

%% @doc Compute MC/DC independence pairs from collected test vectors.
%% Return `{error, not_collected}' if no vectors exist for `Mod'.
%% Each condition maps to `satisfied' or `unsatisfied'.
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

%% For each condition index, find an independence pair:
%% two vectors that differ only in that condition and have different outcomes.
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

-module(body_example).
-export([check/2, logic/2]).

%% Body comparison as case scrutinee.
check(X, Y) ->
    case X > Y of
        true  -> big;
        false -> small
    end.

%% Body andalso/orelse with nested comparisons.
logic(A, B) ->
    case A > 0 andalso B > 0 of
        true  -> both_pos;
        false ->
            case A > 0 orelse B > 0 of
                true  -> one_pos;
                false -> none_pos
            end
    end.

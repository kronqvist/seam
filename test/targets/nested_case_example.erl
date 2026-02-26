-module(nested_case_example).
-export([classify/2]).

classify(X, Y) ->
    case X of
        a ->
            case Y of
                1 -> a1;
                2 -> a2;
                _ -> a_other
            end;
        b ->
            case Y of
                1 -> b1;
                _ -> b_other
            end;
        _ ->
            other
    end.

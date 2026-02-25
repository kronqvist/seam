-module(case_example).
-export([foo/1]).

foo(X) ->
    case X of
        N when N > 10, N < 100 -> medium;
        N when N >= 100        -> large;
        _                      -> small
    end.

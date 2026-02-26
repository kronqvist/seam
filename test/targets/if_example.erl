-module(if_example).
-export([classify/1]).

classify(X) ->
    if
        X > 10 -> large;
        X > 0  -> small;
        true   -> zero
    end.

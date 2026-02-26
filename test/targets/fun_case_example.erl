-module(fun_case_example).
-export([with_fun/1, with_named_fun/1]).

with_fun(X) ->
    F = fun(Y) -> case Y of a -> 1; b -> 2; _ -> 0 end end,
    F(X).

with_named_fun(N) ->
    F = fun Loop(0) -> done; Loop(I) when I > 0 -> Loop(I - 1) end,
    F(N).

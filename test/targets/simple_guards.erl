-module(simple_guards).
-export([classify/1, classify2/2]).

classify(X) when X > 10 -> large;
classify(X) when X > 0  -> small;
classify(_) -> zero.

classify2(X, Y) when X > 10, Y < 5 -> both;
classify2(X, _) when X > 10         -> x_only;
classify2(_, _)                     -> neither.

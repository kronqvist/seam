-module(rebar3_seam).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_seam_prv:init(State),
    {ok, State1}.

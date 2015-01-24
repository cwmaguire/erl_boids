-module(erl_boids).

-export([init/3]).

init(_Type, Req, Opts) ->
    {upgrade, protocol, cowboy_websocket, Req, Opts}.

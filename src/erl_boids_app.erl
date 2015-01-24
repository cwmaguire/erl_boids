%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% Demonstrate Cowboy routing:
%% - root path
%% - static files
%% - Host and path [...]
%%
%% Demonstrate getting the [...] porting of the routing rule binding
%% from the request
-module(erl_boids_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(ANY_HOST, '_').
-define(NO_OPTIONS, []).

start(_Type, _Args) ->

    Paths = [{"/animate", animate_ws_handler, ?NO_OPTIONS},
             {"/[...]", cowboy_static, {priv_dir, erl_boids, "static"}}],

    Routes = [{?ANY_HOST, Paths}],

    Dispatch = cowboy_router:compile(Routes),
    _ = cowboy:start_http(my_http_listener, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    erl_boids_sup:start_link().

stop(_State) ->
	ok.

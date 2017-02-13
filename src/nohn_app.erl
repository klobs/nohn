-module(nohn_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", nohn_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener, 100,
		[{port, 8080},{ip,{127,0,0,1}}],
        #{env => #{dispatch => Dispatch}}
    ),
	nohn_sup:start_link().

stop(_State) ->
	ok.

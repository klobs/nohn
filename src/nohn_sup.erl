-module(nohn_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	% {ChildId, StartFunc, Restart, Shutdown, Type, Modules}.
	Procs = [{nohn_fetcher, {nohn_fetcher, start_link, []}, permanent, 3000, worker, [nohn_fetcher]}
			],
	{ok, {{one_for_one, 1, 5}, Procs}}.

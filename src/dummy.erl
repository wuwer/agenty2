-module(dummy).

-export([start_link/0, write/0, test/0]).

start_link() ->
	{ok, spawn_link(?MODULE, write, [])}.

write() ->
	timer:sleep(5000),
	io:format("~p~n", ["dummy worker lives1"]).

test() ->
	io:format("~p~n", ["shoud work..."]).


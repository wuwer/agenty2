-module(seresye_test_worker).

-export([start_link/0, test_seresye/0]).

start_link() ->
	{ok, spawn_link(?MODULE, test_seresye, [])}.

test_seresye() ->
	Name = mind,
	seresye:start(Name),
	seresye:add_rule(Name, {simplerules, father}),
	seresye:assert(Name, {male, bob}),
	seresye:assert(Name, {parent, bob, alice}),
%%	seresye:assert(Name, {said, word, hello}),
	Kb = seresye:get_kb(Name),
	io:format("~p~n", [Kb]),
	exit(normal).



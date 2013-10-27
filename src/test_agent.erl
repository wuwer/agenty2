-module(test_agent).
-import_all(agent).
-behaviour(agent).
-export([init/2, execute/1, registration_info/1, quit/1]).
-include("agent.hrl").

init(Name, _Args) -> 
	io:format("Test Agent initialized~n", []),	
	#agent_state{name = Name}.

registration_info(Name) ->
	#agent_info{module = ?MODULE, name = Name, pid = self()}.

execute(State) ->
	io:format("The test agent is now executing~n"),
	State.

quit(_State) ->
	exit(normal).



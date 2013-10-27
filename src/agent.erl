-module(agent).
-export([behaviour_info/1, start_link/3, init/3, main_loop/2, execute/1, quit/1]).
-include("agent.hrl").

behaviour_info(callbacks) -> [{init,2}, {execute,1}, {registration_info,1}, {quit,1}];
behaviour_info(_) -> undefined.

%% Starts a process of a new agent. The calling process becomes the agents supervisor
%% Calls init/1 on the concrete agent module
start_link(CallbackModule, Name, InitArgs) -> 
	%%spawn_link needs to create the process using the function of the main loop
	spawn_link(?MODULE, init, [CallbackModule, Name, InitArgs]).

init(CallbackModule, Name, InitArgs) ->
	%% Call the init function to get state
	State = CallbackModule:init(Name, InitArgs),
	%% Call registration_info
	ams:register_agent(CallbackModule:registration_info(Name)),
	%% Use the registration info to register in the ams
	%% Wait for messages
	main_loop(CallbackModule, State).

execute(Pid) ->
	Pid ! execute.

quit(Pid) ->
	Pid ! quit.

main_loop(CallbackModule, State) ->
	receive
		execute ->
			State = CallbackModule:execute(State);
		quit ->
			ams:unregister_agent(State#agent_state.name),
			CallbackModule:quit(State)
	end,
	main_loop(CallbackModule, State).



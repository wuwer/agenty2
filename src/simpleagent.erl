-module(simpleagent).

-behaviour(agent).

-export([start/0, start2/0, stop/0, test_acl/0]). % API

-export([code_change/3, handle_acl/2, handle_call/3,
         handle_cast/2, handle_info/2, init/2, terminate/2]).

-include_lib("exat/include/acl.hrl").  
-include_lib("exat/include/fipa_ontology.hrl").

%%API

%% 					//agent_name, callback_module, parameters
start() -> 
	io:format("~p~n", ["starting seresye:simplemind"]),
	seresye:start(simplemind),
	io:format("~p~n", ["adding rules to seresye process"]),
	seresye:add_rule(simplemind, {simplerules, father}),
	io:format("~p~n", ["starting simpleagent"]),
%%  differend than the documentation specifies, but similar to the examples
	agent:new(simpleagent, ?MODULE, []).
%%			[{"localhost", 7778, <<"simpleagent">>}]).
%%			[{behaviour, simplebehaviour},
%%				{rationality, { simplemind, semantics}}]).

start2() -> 
	io:format("~p~n", ["starting simpleagent2"]),
	X = agent:new(simpleagent2, ?MODULE, 
		[{behaviour, simplebehaviour},
			{rationality, { simplemind, semantics}}]),
	X.

%% 	agent:new(simpleagent, ?MODULE, 
%% 		[{behaviour, simplebehaviour},
%% 			{rationality, { simplemind, fipa_semantics_simple}}]).

stop() -> agent:stop(simpleagent).

test_acl() ->
	io:format("~p~n", ["attemtping to send an acl request to self"]),
	{Ip, Port, Name} = {localhost, 7779, <<"simpleagent">>},
	Addr = list_to_binary(lists:flatten(
				io_lib:format("http://~s:~b", [Ip, Port]))),
	io:format("addr ~p~n", [Addr]),
	Dest = #'agent-identifier'{name = Name, addresses = [Addr]},
	HelloMsg = #aclmessage{sender = <<"simpleagent">>,
			receiver = Dest, content = <<"hello">>},
	spawn(fun() -> _Resp = acl:inform(HelloMsg) end),
	ok.
%%	Msg = #aclmessage{sender ='simpleagent@localhost:7779/acc', receiver ='simpleagent@localhost:7779/acc',
%%			content = <<"hello">>},
%%	Msg = #aclmessage{sender ='simpleagent', receiver ='simpleagent@localhost:7778',
	%		content = <<"hello">>},
%%	io:format("~p~n", [Msg]),
%%	acl:inform(Msg).

%%agents callback

handle_acl(Msg, State) ->
	io:format("simpleagent receives a message: ~p~n", [Msg]),
	io:format("simpleagent is in state: ~p~n", [State]),
	{noreply, State}.
%% handle_acl(#aclmessage{speechact = 'QUERY-REF',
%%                        sender = Sender, content = <<"ping">>} =
%%                Msg,
%%            State) ->
%%     io:format("Got ping query from ~p: ~p~n~nInforming "
%%               "that I'm alive!~n~n",
%%               [Sender, Msg]),
%%     spawn(fun () -> acl:reply(Msg, 'INFORM', <<"alive">>) end),
%%     {noreply, State};
%% handle_acl(#aclmessage{} = Msg, State) ->
%% 	io:format("unknown ~p~n", [Msg]),
%%     {noreply, State}.

%% gen_server callbacks

%% returns the initial State (not fsm state, general State)?
init(Name, Params) -> 
	NewState = start,
	io:format("[init]: agent ~p is started with state: ~p ~n", [Name, NewState]),
	io:format("[init]: agent ~p is started with parameters: ~p ~n", [Name,Params]),
	{ok, []}.



handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Call, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_, State, _) -> {ok, State}.

terminate(_, _) -> ok.


-module(ams).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Agent management functions called from outside
-export([create_agent/3, execute_agent/1, quit_agent/1]).

%% Agent management functions called by an agent 
-export([register_agent/1, unregister_agent/1, list_agents/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {agent_list}). 
-include("agent.hrl").

%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	{ok, #state{agent_list = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({register_agent, AgentInfo}, _From, #state{agent_list = AgentList}) ->
    Reply = ok,
	{reply, Reply, #state{agent_list = [AgentInfo|AgentList]} };

handle_call({unregister_agent, Name}, _From, #state{agent_list = AgentList}) ->
	io:format("Attempting to unregister an agent from the ams!~n"),
	%% TODO: check if an agent with that name exists
    Reply = ok,
	{value, _AgentInfo, AgentList1} = lists:keytake(Name, 3, AgentList),
	{reply, Reply, #state{agent_list = AgentList1} };

handle_call(list_agents, _From, State = #state{agent_list = AgentList}) ->
    Reply = AgentList,
    {reply, Reply, State}.



%%handle_call(_Request, _From, State) ->
%%    Reply = ok,
%%    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%%

handle_cast({create_agent, Module, Name, Arguments}, State) ->
	agent:start_link(Module, Name, Arguments),
	{noreply, State};

handle_cast({execute_agent, Name}, State = #state{agent_list = AgentList}) ->
	AgentInfo = lists:keyfind(Name, 3, AgentList),
	agent:execute(AgentInfo#agent_info.pid),
	{noreply, State};

handle_cast({quit_agent, Name}, State = #state{agent_list = AgentList}) ->
	AgentInfo = lists:keyfind(Name, 3, AgentList),
	agent:quit(AgentInfo#agent_info.pid),
	{noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions called by an agent
%%%===================================================================
%%register_agent(#agent_info{name = Name, pid = Pid}) -> ok.
register_agent(AgentInfo) -> 
	gen_server:call(?MODULE, {register_agent, AgentInfo}).

list_agents() -> 
	gen_server:call(?MODULE, list_agents).

unregister_agent(Name) -> 
	gen_server:call(?MODULE, {unregister_agent, Name}).

%%%===================================================================
%%% Internal functions called by a client
%%%===================================================================

%% Module - name of the module implementing the agent callbacks
%% Arguments - List of arguments passed to the init method
create_agent(Module, Name, Arguments) -> 
	gen_server:cast(?MODULE, {create_agent, Module, Name, Arguments}).

execute_agent(Name) ->
	gen_server:cast(?MODULE, {execute_agent, Name}).

quit_agent(Name) ->
	gen_server:cast(?MODULE, {quit_agent, Name}).

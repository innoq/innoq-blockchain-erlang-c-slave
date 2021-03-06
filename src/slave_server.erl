%%%-------------------------------------------------------------------
%%% @author Christoph Iserlohn <ci@Christophs-MacBook-Pro-5.local>
%%% @copyright (C) 2018, Christoph Iserlohn
%%% @doc
%%%
%%% @end
%%% Created : 19 Apr 2018 by Christoph Iserlohn <ci@Christophs-MacBook-Pro-5.local>
%%%-------------------------------------------------------------------
-module(slave_server).

-behaviour(gen_server).

%% API
-export([start_link/0, mine/5, mine_async/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

mine(JSON_Start, JSON_End, From, To, Leading_Zeros) ->
    A = gen_server:call(?MODULE, {mine, JSON_Start, JSON_End, From, To, Leading_Zeros}),
    io:format("~p", [A]).

mine_async(JSON_Start, JSON_End, From, To, Leading_Zeros) ->
    gen_server:cast(?MODULE, {mine, self(), JSON_Start, JSON_End, From, To, Leading_Zeros}),
    receive
        A -> io:format("~p", [A])
        after 5000 -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
		      {error, Error :: {already_started, pid()}} |
		      {error, Error :: term()} |
		      ignore.
start_link() ->
    gen_server:start_link({global, node()}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
			      {stop, Reason :: term()} |
			      ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, MasterNode} = application:get_env(slave, master_node),
    io:format("Connecting to master node ~p\n", [MasterNode]),
    net_adm:ping(MasterNode),
    NodeName = node(),
    {LoadFactor, _} = string:to_integer(os:getenv("LOADFACTOR", "1")),
    io:format("NodeName: ~p, Loadfactor: ~p\n", [NodeName, LoadFactor]),
    receive
    after 1000 ->
	    gen_server:cast({global, mining}, {node_up, {NodeName, LoadFactor}})
    end,
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
			 {reply, Reply :: term(), NewState :: term()} |
			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
			 {reply, Reply :: term(), NewState :: term(), hibernate} |
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_call({mine, JSON_Start, JSON_End, From, To, Leading_Zeros}, _From, State) ->
    Response = block_finder:find_block(JSON_Start, JSON_End, From, To, Leading_Zeros),
    {reply, Response, State};

handle_call(_Request, _From, State) ->
    {reply, {error}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_cast({mine, JSON_Start, JSON_End, From, To, Leading_Zeros}, State) ->
    io:format("mining from ~p to ~p. leading zero: ~p\n", [From, To - 1, Leading_Zeros]),
    case block_finder:find_block(JSON_Start, JSON_End, From, To, Leading_Zeros) of
	{proof_found, Block, Sha} ->
	    io:format("proof found - sha: ~p\n", [Sha]),
	    gen_server:cast({global, mining}, {proof_found, node(), Block, Sha});
	{no_proof_found, Message} ->
	    io:format("no proof found.\n", []),
	    gen_server:cast({global, mining}, {no_proof_found, node(), Message})
    end,	
    {noreply, State};

handle_cast(Request, State) ->
    io:format("unknown handle_cast called with: ~p\n", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
				      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

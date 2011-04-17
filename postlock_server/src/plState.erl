%%%-------------------------------------------------------------------
%%% File    : plState.erl
%%% Author  : Peter Neumark
%%% Description : 
%%%
%%% Created :  7 Dec 2010 by Peter Neumark
%%%-------------------------------------------------------------------
-module(plState).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% for development
-export([drop_tables/1]).

% Needed for default table attributes.
-include("plRegistry.hrl").
% Needed for mnesia records used by plState.
-include("plState.hrl").
% Required for #pl_client_msg
-include("plMessage.hrl").

-record(state, {
    session_server,
    sessionid,
    transaction_runner,
    storage
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ServerArgs) ->
    gen_server:start_link(?MODULE, [self() | ServerArgs], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([SessionServer, SessionId]) ->
    process_flag(trap_exit, true),
    make_session_tables(SessionId), 
    create_root_node(SessionId),
    TransactionRunner = spawn_link(plTransactionRunner, init, [self()]),
    Storage = plObject:new_state(plStorageEts),
    {ok, #state{
        session_server = SessionServer,
        sessionid = SessionId,
        transaction_runner = TransactionRunner,
        storage = Storage
    }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_num_public_objects}, _From, State) ->
    % STUB
    Reply = 0,
    {reply, Reply, State};

handle_call({get_object, Oid}, _From, State) ->
    {reply, plObject:get_object(Oid, State#state.storage), State};
    %{reply, get_object(State#state.sessionid, Oid), State};

handle_call(Request, _From, State) ->
    io:format("plState:handle_call got ~p~n",[Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({transaction_result, {{plStorageTerm, Storage}, Transaction}}, State) ->
    Iter = plStorageTerm:iterator(Storage),
    NewState = merge_transaction_result(plStorageTerm:next(Iter), State),

    gen_server:cast(State#state.session_server, {deliver_message, #pl_client_msg{
        from=1,
        to=2,
        type="participant_message",
        body=Transaction
    }}),
    gen_server:cast(State#state.session_server, {deliver_message, #pl_client_msg{
        from=1,
        to=3,
        type="participant_message",
        body=Transaction
    }}),
    {noreply, NewState};
handle_cast(Msg, State) ->
    io:format("plState:handle_cast got ~p~n",[Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({participant_message, #pl_client_msg{}=Msg}, State) ->
    case Msg#pl_client_msg.type of
        "transaction" ->
            State#state.transaction_runner ! {transaction, Msg#pl_client_msg.body}
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

sessionid_to_tablenames(SessionId) ->
    [
    erlang:list_to_atom("pl_objects_" ++ erlang:integer_to_list(SessionId)),
    erlang:list_to_atom("pl_transformations_" ++ erlang:integer_to_list(SessionId))
    ].

make_session_tables(SessionId) ->
    [ObjTable, TransTable] = sessionid_to_tablenames(SessionId),
	mnesia:create_table(ObjTable, 
        [{record_name, postlock_object} | [{attributes, record_info(fields,postlock_object)}|
        ?POSTLOCK_DEFAULT_TABLE_ARGS]]),
	mnesia:create_table(TransTable, 
        [{record_name, postlock_transformation} | [{attributes, record_info(fields,postlock_transformation)}|
        ?POSTLOCK_DEFAULT_TABLE_ARGS]]),
    error_logger:info_report(["Tables created successfully for session", SessionId]).

create_root_node(SessionId) ->
    [ObjTable|_] = sessionid_to_tablenames(SessionId),
    Root = #postlock_object{
        oid="0.0",
        content=#postlock_content_dict{}
    },
    mnesia:transaction(fun() -> mnesia:write(ObjTable, Root, write) end),
    error_logger:info_report(["Root node created for session", SessionId]).

get_object(SessionId, Oid) ->
    [ObjTable|_]= sessionid_to_tablenames(SessionId),
    {atomic, ObjData} = mnesia:transaction(fun() -> mnesia:read(ObjTable, Oid, read) end),
    case ObjData of
        [] ->  undefined;
        [Obj] -> Obj
    end.

drop_tables(SessionId) ->
    [mnesia:delete_table(T) || T <- sessionid_to_tablenames(SessionId)].

merge_transaction_result(none, State) -> State;
merge_transaction_result({Oid, Obj, Action, Iter}, State) ->
    case Action of
        store ->
            plObject:store(Obj, State#state.storage);
        delete ->
            plObject:delete(Oid, State#state.storage);
        none ->
            ok
    end,
    Next = plStorageTerm:next(Iter),
    merge_transaction_result(Next, State).

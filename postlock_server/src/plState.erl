%%%-------------------------------------------------------------------
%%% File    : plState.erl
%%% Author  : Peter Neumark <neumark@postlock.org>,
%%%           Akos Toth-Mate <akos@tothmate.com>
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
-export([drop_transaction_table/1]).

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
    storage,
    state_version
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
    create_trasaction_table(SessionId), 
    TransactionRunner = spawn_link(plTransactionRunner, init, [self()]),
    Storage = plObject:new_state(plStorageEts),
    {ok, #state{
        session_server = SessionServer,
        sessionid = SessionId,
        transaction_runner = TransactionRunner,
        storage = Storage,
        state_version = 0
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

handle_call({get_state_version}, _From, State) ->
    {reply, State#state.state_version, State};

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
handle_cast({transaction_result, Transaction, {plStorageTerm, Storage}}, State) ->
    MsgId = State#state.state_version + 1,
    State1 = State#state{state_version = MsgId},
    store_transaction(MsgId, Transaction, State1#state.sessionid),

    Iter = plStorageTerm:iterator(Storage),
    State2 = merge_transaction_result(plStorageTerm:next(Iter), State1),

    gen_server:cast(State2#state.session_server, {broadcast, #pl_client_msg{
        id = MsgId,
        from = 1,
        type = "transaction",
        body = Transaction
    }}),
    {noreply, State2};
handle_cast(Msg, State) ->
    io:format("plState:handle_cast got ~p~n",[Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({participant_message, Msg}, State) ->
    case Msg#pl_client_msg.type of
        "transaction" ->
            TransTable = transaction_table_name(State#state.sessionid),
            State#state.transaction_runner ! {transaction, Msg, TransTable}
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

transaction_table_name(SessionId) ->
    erlang:list_to_atom("pl_transactions_" ++ erlang:integer_to_list(SessionId)).

create_trasaction_table(SessionId) ->
    TransTable = transaction_table_name(SessionId),
    mnesia:create_table(TransTable, 
        [{record_name, postlock_transaction} | [{attributes, record_info(fields, postlock_transaction)}|
        ?POSTLOCK_DEFAULT_TABLE_ARGS]]),
    error_logger:info_report(["Tables created successfully for session", SessionId]).

drop_transaction_table(SessionId) ->
    mnesia:delete_table(transaction_table_name(SessionId)).

store_transaction(MsgId, Transaction, SessionId) ->
    TransTable = transaction_table_name(SessionId),
    {ok, {array, Ops}} = plMessage:json_get_value([ops], Transaction),
    Row = #postlock_transaction{
        id = MsgId,
        ops = Ops
    },
    mnesia:transaction(fun() ->
        mnesia:write(TransTable, Row, write)
    end).

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


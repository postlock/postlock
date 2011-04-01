%%%-------------------------------------------------------------------
%%% File    : plGateway.erl
%%% Author  : Peter Neumark <neumark@postlock.org>
%%% Description : 
%%% A plGateway process handles the dialog with the each client.
%%% YAWS gives us the JSON-encoded message from the client, then
%%% it's up to plGateway to:
%%% - Authenticate new clients
%%% - Forward messages addressed to other postlock participants 
%%%   on behalf of the client (send).
%%% - Forward messages addressed to the client sent by other
%%%   participants (receive).
%%% - If the client disconnects, notify plSession and exit.
%%%
%%% The plGateway module is a gen_fsm server with the following states:
%%% (details in: http://www.erlang.org/doc/man/gen_fsm.html )
%%% 1. idle: the client is not connected
%%% 2. auth: the client is undergoing authentication
%%%    (cannot yet send/receive messages).
%%% 3. connected: the client has authenticated itself,
%%%    it is assumed that the underlying [web]socket connection
%%%    is in connected state. When this is no longer the case,
%%%    plGateway transitions back into the idle state.
%%%
%%% The wire protocol used between plGateway and the client is 
%%% documented at: 
%%% https://github.com/postlock/postlock/wiki/Client-Server-websocket-protocol
%%%
%%% plGateway is an implementation of a FSM (note that
%%% the server and client are in the same state). The following
%%% demonstrates the successful connection buildup sequence.
%%% If an unexpected message is received or no message is
%%% sent in the timeout window, then the state is set back
%%% to idle (or the client/server disconnects).
%%% 
%%% plGateway                         Client
%%% ---------                         ------
%%% =========== state: idle ================
%%%   {type: "client_connect"}
%%%  <-------------------------------------
%%%   {type: "server_connect"}
%%%  ------------------------------------->
%%%  // at this point we can be sure that the
%%%  // websocket connection works both ways
%%% =========== state: auth ================
%%%   {type: "auth_challenge", msg: <challenge>}
%%%  ------------------------------------->
%%%   {type: "auth_response", msg: <response>}
%%%  <-------------------------------------
%%%   {type: "auth_result", msg: <user_data>}
%%%  ------------------------------------->
%%% =========== state: connected ================
%%%  // from this point, messages are forwarded
%%%  // to the session server for processing or
%%%  // delivery to the destination participant.
%%% 
%%% Created :  7 Dec 2010 by Peter Neumark <neumark@postlock.org>
%%%-------------------------------------------------------------------
-module(plGateway).
-behaviour(gen_fsm).

%% gen_fsm callbacks
-export([start_link/1,
        % state functions
        idle/2, idle/3,
        auth/2, auth/3,
        connected/2, connected/3,
        % other functions
        websocket_owner/2,
        % standard gen_fsm exports
        handle_event/3, handle_sync_event/4, handle_info/3, terminate/3,
        code_change/4, init/1]).
% Required to parse YAWS Args term
-include("yaws_api.hrl").
% Required for #pl_client_msg
-include("plMessage.hrl").
% Required for #pl_participant
-include("plSession.hrl").
% Required for error codes
-include("plError.hrl").

-record(state, {
          % 'id' field of next client message
          next_message_id = 0,
          application_process,
          % settings requested by the client (eg. through HTTP GET)
          client_options,
          % The participant's data
          participant,
          % The PID of the process which owns the websocket, used to
          % push data to the client.
          websocket_owner,
          % PID of the session server.
          session_server,
          % Used to store temporary data
          scratch
}).
% Eventually, this should be configurable.
-define(DEFAULT_TIMEOUT, 100000). 
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(ServerData = [_SessionServer, _ApplicationProcess, _ParticipantId, _Connection]) ->
    gen_fsm:start_link(?MODULE, ServerData, []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([SessionServer, ApplicationProcess, ParticipantId, {websocket, Arg}]) ->
    % Don't die on 'EXIT' signal
    process_flag(trap_exit, true),
    % Initialize websocket
    case connect_websocket(ParticipantId, Arg#arg.headers) of
        {ok, WebSocketOwner} ->
            {ok, idle, #state{
                   participant = #pl_participant{
                      id = ParticipantId,
                       process_id = self()},
                   websocket_owner = WebSocketOwner,
                   session_server = SessionServer,
                   client_options = gen_server:call(ApplicationProcess,
                       {sanitize_client_options, yaws_api:parse_query(Arg)}),
                   application_process = ApplicationProcess
                   % TODO: handle timeout event
             }, ?DEFAULT_TIMEOUT}; 
        {error, Reason} ->
            {stop, {websocket_error, Reason}}
    end.

%%--------------------------------------------------------------------
%% state: idle 
%%--------------------------------------------------------------------
idle({client_message, Msg}, State) when Msg#pl_client_msg.type == "client_connect" ->
    % put websocket in active mode
    State#state.websocket_owner ! {set_active_mode},
    % send server_connect message
    State#state.websocket_owner ! {send, finalize_client_msg(#pl_client_msg{type="server_connect"})},
    % send auth_challenge, which puts client in auth state.
    RequestedAuthType = case lists:keyfind("auth_type", 1, State#state.client_options) of
        {"auth_type", Type} -> erlang:list_to_atom(Type);
        false -> default
    end,
    {ChallengeFun, AuthFun} = plAuthlib:get_auth_impl(RequestedAuthType),
    AuthChallenge = ChallengeFun(),
    State#state.websocket_owner ! {send, finalize_client_msg(#pl_client_msg{
        type="auth_challenge",
        body=AuthChallenge})},
    {next_state, auth, State#state{scratch={AuthChallenge, AuthFun}}, ?DEFAULT_TIMEOUT};

idle(Event, State) ->
    io:format("plGateway:idle/2 got unexpected event ~p~n", [Event]),
    {next_state, idle, State}.

idle({get_websocket_owner}, _From, State) ->
    {reply, State#state.websocket_owner, idle, State};

idle(Event, _From, State) ->
    % NOT USED
    io:format("plGateway:idle/3 got unexpected event ~p~n", [Event]),
    {reply, ok, idle, State}.

%%--------------------------------------------------------------------
%% state: auth
%%--------------------------------------------------------------------

auth({client_message, Msg}, State) when Msg#pl_client_msg.type == "auth_response" ->
    % Pass auth response to callback:
    {AuthChallenge, AuthFun} = State#state.scratch,
    {Result, ClientResponse} = case AuthFun(AuthChallenge, Msg#pl_client_msg.body, 
        State#state.application_process) of
        {ok, Username} ->
            % update participant data in state
            UpdatedPData = (State#state.participant)#pl_participant{
                username = Username,
                status = authenticated
                },
            NewState = State#state{participant = UpdatedPData},
            % update user data in session server
            gen_server:cast(State#state.session_server, 
                {update_participant_data, UpdatedPData}),
           {{next_state, connected, NewState},
            {struct, [
                    {"result", "success"},
                    {"participant_id", (State#state.participant)#pl_participant.id},
                    {"session_id", gen_server:call(State#state.session_server, {get_session_id})}
            ]}};
        {error, Reason} ->
            % Inform session server that we are stopping
            on_disconnect(State, auth_failure, Reason),
            {{stop, normal, State},
             {struct, [
                    {"result", "failure"},
                    {"error", ?ERROR2JSON(?PL_ERR_AUTH_FAILURE)}
             ]}}
    end,
    % send auth_response to client
    State#state.websocket_owner ! {send, finalize_client_msg(#pl_client_msg{
                    type="auth_result",
                    body=ClientResponse})},
    Result;

auth(Event, State) ->
    io:format("plGateway:auth/2 got unexpected event ~p~n", [Event]),
    {next_state, auth, State}.

auth(Event, _From, State) ->
    io:format("plGateway:auth/3 got unexpected event ~p~n", [Event]),
    {reply, ok, auth, State}.

%%--------------------------------------------------------------------
%% state: connected
%%--------------------------------------------------------------------
connected({client_message, #pl_client_msg{} = Msg}, State) ->
    gen_server:cast(State#state.session_server, {deliver_message, Msg}),
    {next_state, connected, State};

connected(Event, State) ->
    io:format("plGateway:connected/2 got unexpected event ~p~n", [Event]),
    {next_state, connected, State}.

connected(Event, _From, State) ->
    io:format("plGateway:connected/3 got unexpected event ~p~n", [Event]),
    {reply, ok, connected, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, StateName, State) ->
    case Pid == State#state.websocket_owner of
        % TCP connection was closed.
        true ->
            on_disconnect(State, remote_disconnect, []),
            {stop, normal, State};
        false ->
            io:format("Linked process with PID ~p died! Reason: ~p~nStateName: ~p~nState:~p~n",[Pid, Reason, StateName, State]),
            {next_state, StateName, State}
    end;

handle_info({participant_message, #pl_client_msg{} = Msg},
    connected=StateName, #state{websocket_owner=WS}=State) ->
    WS ! {send, finalize_client_msg(Msg)},
    {next_state, StateName, State};

handle_info(Info, StateName, State) ->
    io:format("plGateway got unexpected message ~p in state ~p (state data ~p)~n", [Info, StateName, State]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
on_disconnect(State, Reason, Details) ->
    gen_server:cast(State#state.session_server, 
        {disconnect, {(State#state.participant)#pl_participant.id, Reason, Details}}).

finalize_client_msg(#pl_client_msg{} = Msg) ->
    plMessage:finalize(?RECORD2JSON(pl_client_msg, Msg)).

%% == Websocket handling functions ==
connect_websocket(ParticipantId, ArgsHeaders) ->
    case get_upgrade_header(ArgsHeaders) of 
	"WebSocket" ->
	    WebSocketOwner = spawn_link(?MODULE, websocket_owner, [ParticipantId, self()]),
	    {ok, WebSocketOwner};
    BadHeader ->
        {error, {bad_header, BadHeader}}
    end.

websocket_owner(ParticipantId, Gateway) ->
    Reason = receive
        {ok, WebSocket} ->
            % Uncomment the following line and recompile to start debugging:
            % plDebug:start(),
            self() ! yaws_api:websocket_receive(WebSocket),
            listen_loop({WebSocket, ParticipantId, Gateway});
        BadValue ->
            {websocket_error, BadValue}
    end,
    exit(Reason).

read_client_message(Gateway, ParticipantId, RawMsg) when is_binary(RawMsg) ->
    read_client_message(Gateway, ParticipantId, erlang:binary_to_list(RawMsg));

read_client_message(Gateway, ParticipantId, RawMsg) ->
    try
        {ok, Json} = json:decode_string(RawMsg),
        {ok, Type} = plMessage:json_get_value([type],Json),
        % Participant 0 is always the session server
        % and the default recipient of messages
        {_, To} = plMessage:json_get_value([to],Json, 0),
        {_, Body} = plMessage:json_get_value([body],Json, undefined),
        % To use seq_tracer, uncomment the following line:
        % plDebug:trace_this(),
        gen_fsm:send_event(Gateway, {client_message, 
            #pl_client_msg{
                from=ParticipantId,
                to=To,
                type=Type,
                body=Body
            }})
    catch 
        error:{badmatch, _} -> 
            % If we receive a bad message, do
            % not forward to Gateway.
            % TODO: send client an error message in response.
            io:format("Got unparseable client msg: ~p~n", RawMsg)
    end.

listen_loop(LD={WebSocket,ParticipantId,Gateway}) ->
    % TODO add 'after' clause with a configurable timeout.
    receive
    {ok, [Message]} -> % used in 'passive mode'
        read_client_message(Gateway, ParticipantId, Message),
        listen_loop(LD);
	{tcp, _WS, DataFrame} -> % used in 'active mode'
        % Try to decode all JSON messages 
        % received through the websocket.
        [read_client_message(Gateway, ParticipantId, Message)
            || Message <- yaws_websockets:unframe_all(DataFrame, [])],
        listen_loop(LD);
    {send, ToBeSent} ->
        yaws_api:websocket_send(WebSocket, ToBeSent),
        listen_loop(LD);
    {set_active_mode} ->
        yaws_api:websocket_setopts(WebSocket, [{active, true}]),
        listen_loop(LD);
	{tcp_closed, WebSocket} ->
        remote_disconnect;
	Any ->
        {unexpected_message, Any}
    end.

% From YAWS example at http://yaws.hyber.org/websockets.yaws
get_upgrade_header(#headers{other=L}) ->
    lists:foldl(fun({http_header,_,K0,_,V}, undefined) ->
                        K = case is_atom(K0) of
                                true ->
                                    atom_to_list(K0);
                                false ->
                                    K0
                            end,
                        case string:to_lower(K) of
                            "upgrade" ->
                                V;
                            _ ->
                                undefined
                        end;
                   (_, Acc) ->
                        Acc
                end, undefined, L).


%%%-------------------------------------------------------------------
%%% File    : plSession.erl
%%% Author  : Peter Neumark <neumark@postlock.org>
%%% Description : This module implements the session server. It is
%%% a gen_server with the following responsibilities:
%%% - maintains the list of participants
%%% - maps participant ids to erlang pids
%%% - delivers messages to a participant
%%% - spanws new plGateway process for connecting client
%%%
%%% Created : 15 Feb 2011 by Peter Neumark <neumark@postlock.org>
%%%-------------------------------------------------------------------
-module(plSession).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SYSTEM_USER, "postlock_system").
-define(SESSION_SERVER_PARTICIPANT_ID, 0).

-record(pl_participant, {
    id,                % participant id
    username = unknown,% defined after authentication
    process_id,        % erlang PID
    joined = now(),    %
    status,
    participant_data
}).

% needed for #pl_client_msg
-include("plMessage.hrl").

-record(state, {
    % session id
    session_id,
    % maps participants to plGateway PIDs
    participants=gb_trees:empty(),
    % application process, used for eg: authentication
    application_process,
    % used when a new participant connects
    next_participant_id = 2 % 0 and 1 are currently reserved.
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ServerData = [_SessionId, _ApplicationProcess]) ->
    gen_server:start_link(?MODULE, ServerData, []).

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
init([SessionId, ApplicationProcess]) ->
    % Don't die on 'EXIT' signal
    process_flag(trap_exit, true),
    State0 = #state{
        session_id=SessionId,
        application_process=ApplicationProcess},
    case add_system_participants(State0) of
    {ok, NewState} ->
        {ok, NewState};
    {error, Reason} ->
        {stop, {error_starting_system_participants, Reason}}
    end.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%% Handle incoming connection requests from new pariticpants.
handle_call({connect_client, Connection}, _From, State) ->
    ParticipantId = State#state.next_participant_id,
    {Reply, NewState} = case plGateway:start_link([
        self(), State#state.application_process, ParticipantId, Connection]) of
        {ok, Gateway} -> 
            NS = add_participant(State, #pl_participant{
                % No username yet, that comes after authentication.
                id = ParticipantId,
                process_id = Gateway,
                status = connected
            }),
            % update next participant id
            NS1 = NS#state{next_participant_id = ParticipantId + 1},
            WSOwner = gen_fsm:sync_send_event(Gateway, {get_websocket_owner}),
            {{ok, {websocket, WSOwner, passive}},
              NS1};
        Err = {error, _Reason} ->
            {Err, State}
    end,
    {reply, Reply, NewState};
%% Returns the application server. 
handle_call({get_application_process}, _From, State) ->
   {reply, State#state.application_process, State};

%% Returns session id to caller.
handle_call({get_session_id}, _From, #state{session_id=Sid}=State) ->
   {reply, Sid, State};

handle_call(Request, From, State) ->
    error_logger:warning_report(["plSession:handle_call/3: unhandled message",
        {request, Request},
        {from, From},
        {state, State}]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% Update participant ID.
handle_cast({update_participant_data, PData}, 
    #state{participants = P} = State) ->
    % Only username update is currently supported
    % TODO: check for case when Id doesnt refer to valid participant.
    {id, Id} = lists:keyfind(id, 1, PData),
    {username, Username} = lists:keyfind(username, 1, PData),
    % TODO: handle case in which lookup returns none
    {value, Participant} = gb_trees:lookup(Id, P),
    NewState = State#state{participants = 
                    gb_trees:update(Id,Participant#pl_participant{
                        username=Username,
                        status=authenticated
                    },P)}, 
    {noreply, NewState};

%% Called when a client disconnects.
handle_cast(
    {disconnect, 
        {ParticipantId,
         Reason,
         Details}}, 
    #state{participants = Participants} = State) ->
    NewState = case gb_trees:lookup(ParticipantId, Participants) of
        % update participant status to 'disconnecting'
        {value, Participant}->
            State#state{participants = gb_trees:update(ParticipantId,
                Participant#pl_participant{
                    status = disconnecting,
                    participant_data = {Reason, Details}
                }, Participants)};
        none ->
            error_logger:error_report(["disconnect message received from nonexisting participant"]), 
            State
    end,
    {noreply, NewState};

%% Delivers messages between participants
handle_cast({deliver_message, #pl_client_msg{to=To}=Msg}, 
    #state{participants=Participants}=State) ->
    case To of 
        ?SESSION_SERVER_PARTICIPANT_ID ->
            io:format("Session server got message ~p~n", [Msg]);
        _OtherParticipant ->
            % TODO: send error if deliver_message returns {error, _}
            deliver_message(Msg, Participants)
    end,
    {noreply, State};
 
handle_cast(Msg, State) ->
    error_logger:warning_report(["plSession:handle_cast/2: unhandled message",
        {message, Msg},
        {state, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    {noreply, on_exit(Pid, Reason, State)};

handle_info(Info, State) ->
    error_logger:warning_report(["plSession:handle_info/2: unhandled message",
        {message, Info},
        {state, State}]),
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

%%====================================================================
%%  Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: add_system_participants(#state) -> {ok, Participants} |
%%                         {error, Reason} |
%% Description: spawns the postlock 'system participants', which are
%% the mandatory participants for any functioning session.
%% These are:
%%   Session Server (this process)
%%   State Server (lives in plState, spawned here)
%%--------------------------------------------------------------------
add_system_participants(#state{session_id=SessionId}=State) ->
    %% TODO: start a configurable set of servers
    %% instead of a hardcoded list.
    %% attempt to spawn state server
    case plState:start_link([SessionId]) of
        {ok, Pid} -> {ok, 
            lists:foldl( 
                fun({ParticipantId, ParticipantPid}, S) ->
                    add_participant(S, #pl_participant{
                        id=ParticipantId,
                        username=?SYSTEM_USER,
                        process_id=ParticipantPid,
                        status=authenticated
                    })
                end,
                State,
                [{?SESSION_SERVER_PARTICIPANT_ID, self()}, {1, Pid}])};
            Err = {error, _Reason} -> Err
    end.    

add_participant(#state{participants=Participants}=State, 
    #pl_participant{id=ParticipantId}=NewParticipant) ->
    State#state{participants=gb_trees:insert(
        ParticipantId,
        NewParticipant,
        Participants)}.

deliver_message(#pl_client_msg{to=To}=Msg, Participants) ->
    case gb_trees:is_defined(To, Participants) of
       true ->
           #pl_participant{process_id = Pid} = gb_trees:get(To, Participants),
           Pid ! {participant_message, Msg};
       false ->
           {error, {no_such_participant, To}}
    end.

on_exit(Pid, Reason, State) ->
    NewState = case pid_to_participant(Pid, State#state.participants) of
        none ->
            error_logger:error_report(
                ["Linked process died which was not a participant. Pid: ", Pid]),
            State;
        Participant ->
            on_participant_disconnect(Participant, Reason),
            State#state{participants =
                gb_trees:delete(
                    Participant#pl_participant.id,
                    State#state.participants)}
    end,
    NewState.

pid_to_participant(Pid, Participants) ->
    pid_to_participant_1(Pid, 
        gb_trees:next(
            gb_trees:iterator(Participants))).

pid_to_participant_1(_Pid, none) ->
    none;
pid_to_participant_1(Pid, {_Key, Val, Iter}) ->
    case Val#pl_participant.process_id == Pid of
        true ->  Val;
        false -> pid_to_participant_1(Pid, gb_trees:next(Iter))
    end.

on_participant_disconnect(Participant, Reason) ->
    case {Participant#pl_participant.status, Reason} of
        {disconnecting, normal} -> 
            error_logger:info_report(["plGateway process exited gracefully",
            [{pid, Participant#pl_participant.process_id},
             {participant_id, Participant#pl_participant.id},
             {exit_reason, Participant#pl_participant.participant_data}]]);
        {Status, Reason} ->
            error_logger:info_report(["plGateway process crashed",
                [{participant, Participant},
                 {reason, Reason},
                 {status, Status}]])
    end.

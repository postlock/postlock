%%%-------------------------------------------------------------------
%%% File    : plTransactionRunner.erl
%%% Author  : Peter Neumark <neumark@postlock.org>
%%% Description : Executes transactions received from plState.
%%%
%%% Created :  27 Mar 2011 by Peter Neumark <neumark@postlock.org>
%%%-------------------------------------------------------------------
-module(plTransactionRunner).
%% init/1 must be exported because it is called by spawn/3.
-export([init/1]).

init(StateServerPid) ->
    listen_loop(StateServerPid).

listen_loop(StateServerPid) ->
    receive
        {transaction, T} ->
            Result = process_transaction(T, StateServerPid),
            % returns the result of the transaction to state server
            gen_server:cast(StateServerPid, {transaction_result, Result});
        BadValue ->
            error_logger:error_report(["plTransactionRunner received unexpected message: ", BadValue]),
            {bad_message, BadValue}
    end.

process_transaction(T, _StateServerPid) ->
    % TODO: error handling
    {ok, {array, Commands}} = plMessage:json_get_value([ops], T),
    lists:foldl(fun run_command/2, plObject:new_state(plStorageTerm), Commands).

run_command(Command, Objects={Mod, State}) ->
    {ok, Cmd} = plMessage:json_get_value([cmd], Command),
    {ok, {array, Params}} = plMessage:json_get_value([params], Command),
    % TODO: differentiate between different commands
    [H|_] = Params,
    {ok, Type} = plMessage:json_get_value([type], H),
    % TODO: type dependent creation
    Obj = plObject:new_obj(plTypeDict),
    plObject:create(Obj, {plStorageTerm, State}),
    Objects.

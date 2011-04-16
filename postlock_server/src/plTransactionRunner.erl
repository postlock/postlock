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
            {Result, _pid} = process_transaction(T, StateServerPid),
            % returns the result of the transaction to state server
            gen_server:cast(StateServerPid, {transaction_result, Result});
        BadValue ->
            error_logger:error_report(["plTransactionRunner received unexpected message: ", BadValue]),
            {bad_message, BadValue}
    end.

% TODO: error handling
process_transaction(T, StateServerPid) ->
    {ok, {array, Commands}} = plMessage:json_get_value([ops], T),
    lists:foldl(fun process_command/2, {plObject:new_state(plStorageTerm), StateServerPid}, Commands).

process_command(Command, {Objects, StateServerPid}) ->
    {ok, Cmd} = plMessage:json_get_value([cmd], Command),
    {ok, Params} = plMessage:json_get_value([params], Command),
    {execute_command(Cmd, Params, Objects, StateServerPid), StateServerPid}.

execute_command("create", Params, Objects, _StateServerPid) ->
    {ok, Type} = plMessage:json_get_value([type], Params),
    {ok, Oid} = plMessage:json_get_value([oid], Params),
    case Type of
        "data" -> Obj = plObject:new_obj(plTypeData, Oid);
        "dict" -> Obj = plObject:new_obj(plTypeDict, Oid);
        "list" -> Obj = plObject:new_obj(plTypeList, Oid)
    end,
    plObject:store(Obj, Objects);
execute_command("modify", Params, Objects, StateServerPid) ->
    {ok, Oid} = plMessage:json_get_value([oid], Params),
    {ok, Cmd} = plMessage:json_get_value([cmd], Params),
    {ok, Value} = plMessage:json_get_value([value], Params),
    CurrentObject = case plObject:is_set(Oid, Objects) of
        true -> plObject:get_object(Oid, Objects);
        false -> gen_server:call(StateServerPid, {get_object, Oid})
    end,
    ModifiedObject = plObject:execute(CurrentObject, {erlang:list_to_atom(Cmd), Value}),
    plObject:store(ModifiedObject, Objects).


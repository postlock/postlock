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

-include_lib("stdlib/include/qlc.hrl").

%% #postlock_transaction
-include("plState.hrl").

init(StateServerPid) ->
    listen_loop(StateServerPid).

listen_loop(StateServerPid) ->
    receive
        {transaction, Transaction, TransTable} ->
            {ok, FromStateVersion} = plMessage:json_get_value([state_version], Transaction),
            CurrentStateVersion = gen_server:call(StateServerPid, {get_state_version}),
            PreviousTransactions = if FromStateVersion < CurrentStateVersion ->
                get_transactions(TransTable, FromStateVersion + 1);
                true -> []
            end,
            {ModifiedTransaction, ResultStorage} = process_transaction(Transaction, StateServerPid),
            % returns the result of the transaction to state server
            gen_server:cast(StateServerPid, {transaction_result, ModifiedTransaction, ResultStorage}),
            listen_loop(StateServerPid);
        BadValue ->
            error_logger:error_report(["plTransactionRunner received unexpected message: ", BadValue]),
            {bad_message, BadValue}
    end.

% TODO: error handling
process_transaction(Transaction, StateServerPid) ->
    {ok, {array, Commands}} = plMessage:json_get_value([ops], Transaction),
    {Result, _Pid} = lists:foldl(fun process_command/2, {plObject:new_state(plStorageTerm), StateServerPid}, Commands),
    {Transaction, Result}.

process_command(Command, {Objects, StateServerPid}) ->
    {ok, Cmd} = plMessage:json_get_value([cmd], Command),
    Oid = case plMessage:json_get_value([oid], Command) of
        {ok, Value} -> Value;
        _ -> undefined
    end,
    {ok, {array, Params}} = plMessage:json_get_value([params], Command),
    {execute_command(Cmd, Oid, Params, Objects, StateServerPid), StateServerPid}.

execute_command("create", _, _Params=[Oid,Type|_], Objects, _StateServerPid) ->
    case Type of
        "data" -> Obj = plObject:new_obj(plTypeData, Oid);
        "dict" -> Obj = plObject:new_obj(plTypeDict, Oid);
        "list" -> Obj = plObject:new_obj(plTypeList, Oid)
    end,
    plObject:store(Obj, Objects);
execute_command("delete", _, _Params=[Oid|_], Objects, _StateServerPid) ->
    plObject:delete(Oid, Objects);
execute_command(Cmd, Oid, Params, Objects, StateServerPid) ->
    CurrentObject = case plObject:is_set(Oid, Objects) of
        true -> plObject:get_object(Oid, Objects);
        false -> gen_server:call(StateServerPid, {get_object, Oid})
    end,
    ExecuteParams = lists:foldl(fun (Param, Acc) -> erlang:append_element(Acc, Param) end, {erlang:list_to_atom(Cmd)}, Params),
    ModifiedObject = plObject:execute(CurrentObject, ExecuteParams),
    plObject:store(ModifiedObject, Objects).

get_transactions(TransTable, FromStateVersion) ->
    Query = qlc:q([T || T <- mnesia:table(TransTable), T#postlock_transaction.id >= FromStateVersion]),
    {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Query) end),
    Result.
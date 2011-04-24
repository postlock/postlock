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
            {ModifiedTransaction, ResultStorage} = process_transaction(Transaction, PreviousTransactions, StateServerPid),
            % returns the result of the transaction to state server
            gen_server:cast(StateServerPid, {transaction_result, ModifiedTransaction, ResultStorage}),
            listen_loop(StateServerPid);
        BadValue ->
            error_logger:error_report(["plTransactionRunner received unexpected message: ", BadValue]),
            {bad_message, BadValue}
    end.

% TODO: error handling
process_transaction(Transaction, PreviousTransactions, StateServerPid) ->
    {ok, {array, Operations}} = plMessage:json_get_value([ops], Transaction),
    {Result, _, _} = lists:foldl(fun process_operation/2, {plObject:new_state(plStorageTerm), PreviousTransactions, StateServerPid}, Operations),
    {Transaction, Result}.

process_operation(Operation, {Objects, PreviousTransactions, StateServerPid}) ->
    {Oid, Cmd, Params} = get_operation_data(Operation),
    {TransformedCmd, TransdormedParams} = transform_operation(Oid, Cmd, Params, PreviousTransactions, StateServerPid),
    {execute_operation(Oid, TransformedCmd, TransdormedParams, Objects, StateServerPid), PreviousTransactions, StateServerPid}.

transform_operation(_, Cmd, Params, [], _) ->
    {Cmd, Params};
transform_operation(Oid, Cmd, Params, [Transaction|RemainingTransactions], StateServerPid) ->
    case Oid of
        undefined ->
            transform_operation(Oid, Cmd, Params, RemainingTransactions, StateServerPid);
        _ ->
            {_Oid, TransformedCmd, TransformedParams, _StateServerPid} = lists:foldl(fun perform_ot/2, {Oid, Cmd, Params, StateServerPid}, Transaction#postlock_transaction.ops),
            transform_operation(Oid, TransformedCmd, TransformedParams, RemainingTransactions, StateServerPid)
    end.

% TODO: return OT success/failure
perform_ot(_Operation, {undefined,  Cmd, Params, StateServerPid}) ->
    % TODO: create/delete OTs
    {undefined, Cmd, Params, StateServerPid};
perform_ot(Operation, {Oid, Cmd, Params, StateServerPid}) ->
    {OldOid, OldCmd, OldParams} = get_operation_data(Operation),
    case OldOid of
        Oid ->
            Object = gen_server:call(StateServerPid, {get_object, Oid}),
            OldOp = wrap_object_arguments(OldCmd, OldParams),
            Op = wrap_object_arguments(Cmd, Params),
            {_Result, Op1, _Op2} = plObject:xform(Object, OldOp, Op),
            {TransformedCmd, TransformedParams} = extract_object_arguments(Op1),
            {Oid, TransformedCmd, TransformedParams, StateServerPid};
        _ ->
            {Oid, Cmd, Params, StateServerPid}
    end.

execute_operation(_, create, _Params=[Oid,Type|_], Objects, _StateServerPid) ->
    case Type of
        "data" -> Obj = plObject:new_obj(plTypeData, Oid);
        "dict" -> Obj = plObject:new_obj(plTypeDict, Oid);
        "list" -> Obj = plObject:new_obj(plTypeList, Oid)
    end,
    plObject:store(Obj, Objects);
execute_operation(_, delete, _Params=[Oid|_], Objects, _StateServerPid) ->
    plObject:delete(Oid, Objects);
execute_operation(Oid, Cmd, Params, Objects, StateServerPid) ->
    CurrentObject = case plObject:is_set(Oid, Objects) of
        true -> plObject:get_object(Oid, Objects);
        false -> gen_server:call(StateServerPid, {get_object, Oid})
    end,
    ModifiedObject = plObject:execute(CurrentObject, wrap_object_arguments(Cmd, Params)),
    plObject:store(ModifiedObject, Objects).

get_transactions(TransTable, FromStateVersion) ->
    Query = qlc:q([T || T <- mnesia:table(TransTable), T#postlock_transaction.id >= FromStateVersion]),
    {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Query) end),
    Result.

get_operation_data(Operation) ->
    {ok, Cmd} = plMessage:json_get_value([cmd], Operation),
    Oid = case plMessage:json_get_value([oid], Operation) of
        {ok, Value} -> Value;
        _ -> undefined
    end,
    {ok, {array, Params}} = plMessage:json_get_value([params], Operation),
    {Oid, list_to_atom(Cmd), Params}.

wrap_object_arguments(Cmd, Params) ->
    lists:foldl(fun (Param, Acc) -> erlang:append_element(Acc, Param) end, {Cmd}, Params).

extract_object_arguments(Operation) ->
    [Cmd|Params] = tuple_to_list(Operation),
    {Cmd, Params}.
%%%-------------------------------------------------------------------
%%% File    : plTransactionRunner.erl
%%% Author  : Peter Neumark <neumark@postlock.org>, 
%%%           Akos Toth-Mate <akos@tothmate.com>
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
            try process_transaction(Transaction, CurrentStateVersion, PreviousTransactions, StateServerPid) of
                {ResultStorage, ModifiedTransaction} ->
                    gen_server:cast(StateServerPid, {transaction_result, ModifiedTransaction, ResultStorage})
            catch
                % TODO: propagate this
                ot_error -> ok
            end,
            listen_loop(StateServerPid);
        BadValue ->
            error_logger:error_report(["plTransactionRunner received unexpected message: ", BadValue]),
            {bad_message, BadValue}
    end.

% TODO: error handling
process_transaction(Transaction, CurrentStateVersion, PreviousTransactions, StateServerPid) ->
    {ok, {array, Operations}} = plMessage:json_get_value([ops], Transaction),
    {Result, ModifiedOps, _, _} = lists:foldl(fun process_operation/2, {plObject:new_state(plStorageTerm), [], PreviousTransactions, StateServerPid}, Operations),
    ModifiedTransaction0 = json:obj_store("state_version", CurrentStateVersion, json:obj_new()),
    ModifiedTransaction1 = json:obj_store("ops", {array, ModifiedOps}, ModifiedTransaction0),
    {Result, ModifiedTransaction1}.

process_operation(Operation, {Objects, ModifiedOps, PreviousTransactions, StateServerPid}) ->
    {Oid, Cmd, Params} = get_operation_data(Operation),
    {TransformedCmd, TransdormedParams} = transform_operation(Oid, Cmd, Params, PreviousTransactions, StateServerPid),

    ModifiedOp0 = json:obj_store("cmd", atom_to_list(TransformedCmd), json:obj_new()),
    ModifiedOp1 = case Oid of
        undefined -> ModifiedOp0;
        _ -> json:obj_store("oid", Oid, ModifiedOp0)
    end,
    ModifiedOp2 = json:obj_store("params", {array, TransdormedParams}, ModifiedOp1),
    ModifiedOps1 = lists:append(ModifiedOps, [ModifiedOp2]),

    Result = execute_operation(Oid, TransformedCmd, TransdormedParams, Objects, StateServerPid),
    {Result, ModifiedOps1, PreviousTransactions, StateServerPid}.

transform_operation(_, Cmd, Params, [], _) ->
    {Cmd, Params};
transform_operation(Oid, Cmd, Params, [Transaction|RemainingTransactions], StateServerPid) ->
    case Oid of
        undefined ->
            % TODO: create/delete OTs
            transform_operation(Oid, Cmd, Params, RemainingTransactions, StateServerPid);
        _ ->
            {_Oid, TransformedCmd, TransformedParams, _StateServerPid} = lists:foldl(fun perform_ot/2, {Oid, Cmd, Params, StateServerPid}, Transaction#postlock_transaction.ops),
            transform_operation(Oid, TransformedCmd, TransformedParams, RemainingTransactions, StateServerPid)
    end.

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
            {OtResult, _Op1, Op2} = plObject:xform(Object, OldOp, Op),
            case OtResult of
                ok ->
                    {TransformedCmd, TransformedParams} = extract_object_arguments(Op2),
                    {Oid, TransformedCmd, TransformedParams, StateServerPid};
                fail ->
                    throw(ot_error)
            end;
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

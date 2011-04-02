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

process_transaction(_T, _StateServerPid) ->
    % STUB
    ok.

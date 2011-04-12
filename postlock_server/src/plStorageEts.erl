%%%-------------------------------------------------------------------
%%% File    : plStorageEts.erl
%%% Author  : Peter Neumark <neumark@postlock.org>
%%% Description : Stores state in ETS.
%%% Created in ETS.
%%%
%%% Created :  27 Mar 2011 by Peter Neumark <neumark@postlock.org>
%%%-------------------------------------------------------------------
-module(plStorageEts).
-export([
    % standard storage implementation functions
    delete/2,
    new_state/0,
    store/2,
    get_object/2,
    is_set/2,
    destroy/1
]).
-define(ETS_NAME, ?MODULE).
-define(ETS_OPTIONS, [{keypos, 2}]).
-record(ets_obj, {
    oid,    % key for the ETS table
    obj     % the object itself
}).

new_state() ->
    ets:new(?ETS_NAME, ?ETS_OPTIONS).

store(Obj, State) -> ets:insert(State, #ets_obj{
    obj=Obj,
    oid=plObject:get_oid(Obj)}),
    State. % TODO: throw exception in case of error

get_object(Oid, Tid) ->
    case ets:lookup(Tid, Oid) of
        [] -> undefined;
        [#ets_obj{obj=Obj}] -> Obj
    end.

delete(_Oid, _State) ->
    %TODO
    ok.

is_set(Oid, State) ->
    get_object(Oid, State) =/= undefined.

destroy(Tid) ->
    ets:delete(Tid).



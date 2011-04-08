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
    insert/2,
    update/2,
    get_object/2,
    is_set/2
]).
-include_lib("eunit/include/eunit.hrl").
-define(ETS_NAME, ?MODULE).
-define(ETS_OPTIONS, [{keypos, 2}]).
-record(ets_obj, {
    oid,    % key for the ETS table
    obj     % the object itself
}).

new_state() ->
    %% state is simply a gb_tree
    ets:new(?ETS_NAME, ?ETS_OPTIONS).

insert(Obj, State) -> write(Obj, State).
update(Obj, State) -> write(Obj, State).
write(Obj, State) -> ets:insert(State, #ets_obj{
    obj=Obj,
    oid=plObject:get_oid(Obj)}),
    Obj. % TODO: throw exception in case of error
% STUBS
get_object(Oid, Tid) ->
    case ets:lookup(Tid, Oid) of
        [] -> undefined;
        [#ets_obj{obj=Obj}] -> Obj
    end.

delete(_Oid, _State) ->
    %TODO
    ok.

is_set(_Oid, _State) ->
    %TODO
    ok.

%%%-------------------------------------------------------------------
%%% EUnit tests
%%%-------------------------------------------------------------------
read_write_test_() ->
    Data = plObject:new_obj(plTypeData, 0),
    State = plObject:new_state(?MODULE),
    plObject:insert(Data, State),
    Retrieved = plObject:get_object(plObject:get_oid(Data), State),
    %%[{#ets_obj{oid='$1', _ = '_'},[],['$1']}])]),
    ?assert(Retrieved =:= Data).

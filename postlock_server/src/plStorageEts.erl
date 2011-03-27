%%%-------------------------------------------------------------------
%%% File    : plStorageEts.erl
%%% Author  : Peter Neumark <neumark@postlock.org>
%%% Description : Stores state in ETS.
%%% Created in ETS.
%%%
%%% Created :  27 Mar 2011 by Peter Neumark <neumark@postlock.org>
%%%-------------------------------------------------------------------
-module(plStorageTerm).
-export([
    % standard storage implementation functions
    delete/2,
    new_state/0,
    create/2,
    update/2,
    get/2,
    is_set/2
]).
-define(ETS_NAME, ?MODULE).
-define(ETS_OPTIONS, []).
-record(ets_obj, {
    oid,    % key for the ETS table
    obj     % the object itself
}).

new_state() ->
    %% state is simply a gb_tree
    ets:new(?ETS_NAME, ?ETS_OPTIONS).

create(Obj, State) -> write(Obj, State).
update(Obj, State) -> write(Obj, State).
write(Obj, State) -> ets:insert(State, #ets_obj{
    obj=Obj,
    oid=plObject:get_oid(Obj)}).

get(Oid, State) ->
    %TODO
    ok.

delete(Oid, State) ->
    %TODO
    ok.

is_set(Oid, State) ->
    %TODO
    ok.

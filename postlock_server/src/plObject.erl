%%%-------------------------------------------------------------------
%%% File    : plObject.erl
%%% Author  : Peter Neumark <neumark@postlock.org>
%%% Description : A wrapper around the object type / state storage
%%% implementations. Use the plObject functions to call the correct
%%% code depending on the type of data.
%%%
%%% Created :  27 Mar 2011 by Peter Neumark <neumark@postlock.org>
%%%-------------------------------------------------------------------
-module(plObject).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-define(STORAGE_MODULES, [plStorageEts, plStorageTerm]).
%%%-------------------------------------------------------------------
%%% Functions for dealing with object types.
%%%-------------------------------------------------------------------

new_obj(Mod, Oid) -> {Mod, apply(Mod, new_obj, [Oid])}.
get_oid({Mod, Obj}) -> apply(Mod, get_oid, [Obj]).
execute({Mod, Obj}, Cmd) -> {Mod, apply(Mod, execute, [Cmd, Obj])}.
xform({Mod, _Obj}, Op1, Op2) -> {Mod, apply(Mod, xform, [Op1, Op2])}.
%%%-------------------------------------------------------------------
%%% Functions for dealing with storage implementations.
%%%-------------------------------------------------------------------
get_object(Oid, {Mod, State}) -> apply(Mod, get_object, [Oid, State]).
is_set(Oid, {Mod, State}) -> apply(Mod, is_set, [Oid, State]).
new_state(Mod) -> {Mod, apply(Mod, new_state, [])}.
delete(Oid, {Mod, State}) -> {Mod, apply(Mod, delete, [Oid, State])}.
store(Obj, {Mod, State}) -> {Mod, apply(Mod, store, [Obj, State])}.
% only needed by ETS so far
destroy({Mod, State}) -> case erlang:function_exported(Mod, destroy, 1) of
        true -> apply(Mod, destroy, [State]);
        false -> noop
    end.
%%%-------------------------------------------------------------------
%%% Unit tests
%%%-------------------------------------------------------------------
storage_test_() ->
    [{"Storage test for module "++ atom_to_list(Mod),
    fun() ->
        Data = plObject:new_obj(plTypeData, 0),
        State = plObject:new_state(Mod),
        State2 = plObject:store(Data, State),
        Retrieved = plObject:get_object(plObject:get_oid(Data), State2),
        ?assert(Retrieved =:= Data)
     end} || Mod <- ?STORAGE_MODULES].


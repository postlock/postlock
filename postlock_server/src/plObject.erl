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
%%%-------------------------------------------------------------------
%%% Functions for dealing with object types.
%%%-------------------------------------------------------------------
new_obj(Mod) -> {Mod, apply(Mod, new_obj, [])}.
get_oid({Mod, Obj}) -> {Mod, apply(Mod, get_oid, [Obj])}.
execute({Mod, Obj}, Cmd) -> {Mod, apply(Mod, execute, [Cmd, Obj])}.
%%%-------------------------------------------------------------------
%%% Functions for dealing with storage implementations.
%%%-------------------------------------------------------------------
new_state(Mod) -> {Mod, apply(Mod, new_state, [])}.
delete(Oid, {Mod, State}) -> {Mod, apply(Mod, delete, [Oid, State])}.
create(Obj, {Mod, State}) -> {Mod, apply(Mod, create, [Obj, State])}.
update(Obj, {Mod, State}) -> {Mod, apply(Mod, update, [Obj, State])}.
get(Oid, {Mod, State}) -> {Mod, apply(Mod, get, [Oid, State])}.
is_set(Oid, {Mod, State}) -> {Mod, apply(Mod, is_set, [Oid, State])}.
% only needed by ETS so far
destroy({Mod, State}) -> apply(Mod, destroy, []).

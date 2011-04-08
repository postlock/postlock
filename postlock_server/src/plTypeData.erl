%%%-------------------------------------------------------------------
%%% File    : plDataType.erl
%%% Author  : Akos Toth-Mate <akos@tothmate.com>
%%% Description : simple value store data type
%%%
%%% Created :  27 Mar 2011 by Akos Toth-Mate <akos@tothmate.com>
%%%-------------------------------------------------------------------

-module(plTypeData).
-export([new_obj/1, execute/2, xform/2, get_oid/1]).
-define(DEFAULT_VALUE, undefined).

get_oid({Oid, _}) -> Oid.

new_obj(Oid) ->
    {Oid, ?DEFAULT_VALUE}.

execute({set, Value}, {Oid, _OldVal}) ->
    {Oid, Value}.

xform({_Op, Value1}, {set, _Value2}) when _Op == set; _Op == unsafe_set -> 
    {fail, {set, Value1}, nop};
xform({_Op, _Value1}, {unsafe_set, Value2}) when _Op == set; _Op == unsafe_set ->
    {ok, nop, {set, Value2}}.

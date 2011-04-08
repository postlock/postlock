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
    {Oid, Value};
execute({unsafe_set, Value}, Data) ->
    execute({set, Value}, Data).

xform({set, Value1}, {set, _Value2}) -> 
    {fail, {set, Value1}, nop};
xform({unsafe_set, Value1}, {set, Value2}) -> 
    xform({set, Value1}, {set, Value2});
xform({set, _Value1}, {unsafe_set, Value2}) ->
    {ok, nop, {set, Value2}};
xform({unsafe_set, Value1}, {unsafe_set, Value2}) ->
    xform({set, Value1}, {unsafe_set, Value2}).

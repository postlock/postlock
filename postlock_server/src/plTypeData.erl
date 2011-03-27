%%%-------------------------------------------------------------------
%%% File    : plDataType.erl
%%% Author  : Akos Toth-Mate <akos@tothmate.com>
%%% Description : simple value store data type
%%%
%%% Created :  27 Mar 2011 by Akos Toth-Mate <akos@tothmate.com>
%%%-------------------------------------------------------------------

-module(plTypeData).
-export([new/0, apply/2, xform/2]).

new() ->
    undefined.

apply({set, Value}, _) ->
    Value.

xform({_Op, Value1}, {set, _Value2}) when _Op == set; _Op == unsafe_set -> 
    {fail, {set, Value1}, nop};
xform({_Op, _Value1}, {unsafe_set, Value2}) when _Op == set; _Op == unsafe_set ->
    {ok, nop, {set, Value2}}.

%%%-------------------------------------------------------------------
%%% File    : plDataType.erl
%%% Author  : Akos Toth-Mate <akos@tothmate.com>
%%% Description : simple value store data type
%%%
%%% Created :  27 Mar 2011 by Akos Toth-Mate <akos@tothmate.com>
%%%-------------------------------------------------------------------

-module(plTypeData).
-export([new/0, apply/2, xform/2]).
-compile({no_auto_import, [apply/2]}).

new() ->
    undefined.

apply({set, Value}, _) ->
    Value;
apply({unsafe_set, Value}, Data) ->
    apply({set, Value}, Data).

xform({set, Value1}, {set, _Value2}) -> 
    {fail, {set, Value1}, nop};
xform({unsafe_set, Value1}, {set, Value2}) -> 
    xform({set, Value1}, {set, Value2});
xform({set, _Value1}, {unsafe_set, Value2}) ->
    {ok, nop, {set, Value2}};
xform({unsafe_set, Value1}, {unsafe_set, Value2}) ->
    xform({set, Value1}, {unsafe_set, Value2}).

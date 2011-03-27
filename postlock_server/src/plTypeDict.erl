%%%-------------------------------------------------------------------
%%% File    : plTypeDict.erl
%%% Author  : Akos Toth-Mate <akos@tothmate.com>
%%% Description : dictionary data type
%%%
%%% Created :  27 Mar 2011 by Akos Toth-Mate <akos@tothmate.com>
%%%-------------------------------------------------------------------

-module(plTypeDict).
-export([new/0, apply/2, xform/2]).

new() ->
    {?MODULE, dict:new()}.

apply({set, Key, Value}, {?MODULE, Dict}) ->
    {?MODULE, dict:store(Key, Value, Dict)};
apply({remove, Key, Value}, {?MODULE, Dict}) ->
    {?MODULE, dict:erase(Key, Value, Dict)}.

xform({_Op, Key, Value1}, {set, Key, _Value2}) when _Op == set; _Op == unsafe_set ->
    {{fail, {set, Key, Value1}}, nop};
xform({_Op, Key, _Value1}, {unsafe_set, Key, Value2}) when _Op == set; _Op == unsafe_set ->
    {{ok, nop}, {set, Key, Value2}};
xform({_Op, Key, Value}, {remove, Key}) when _Op == set; _Op == unsafe_set ->
    {{fail, {set, Key, Value}}, nop};
xform({_Op, Key, _Value}, {unsafe_remove, Key}) when _Op == set; _Op == unsafe_set ->
    {{ok, nop}, {remove, Key}};
xform({_Op1, Key}, {_Op2, Key, Value}) when _Op1 == remove; _Op1 == unsafe_remove, _Op2 == set; _Op2 == unsafe_set ->
    {{ok, nop}, {set, Key, Value}};
xform({_, _Key1, _}, {_, _Key2, _}) -> % covers: (unsafe_)remove vs (unsafe_)remove, different keys
    {{ok, nop}, nop}.
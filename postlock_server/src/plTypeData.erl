%%%-------------------------------------------------------------------
%%% File    : plDataType.erl
%%% Author  : Akos Toth-Mate <akos@tothmate.com>
%%% Description : implementation of simple data type
%%%
%%% Created :  27 Mar 2011 by Peter Neumark <akos@tothmate.com>
%%%-------------------------------------------------------------------

-module(plTypeData).
-export([create/1, apply/2, xform/2]).

create(Value) ->
    {?MODULE, Value}.

apply({set, NewValue}, {?MODULE, _oldValue}) ->
    {?MODULE, NewValue}.

xform({_Op, Value1}, {set, _Value2}) when _Op == set; _Op == unsafe_set -> 
    {{fail, {set, Value1}}, nop};
xform({_Op, _Value1}, {unsafe_set, Value2}) when _Op == set; _Op == unsafe_set ->
    {{ok, nop}, {set, Value2}}.
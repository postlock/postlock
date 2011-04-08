%%%-------------------------------------------------------------------
%%% File    : plTypeDict.erl
%%% Author  : Akos Toth-Mate <akos@tothmate.com>
%%% Description : dictionary data type
%%%
%%% Created :  27 Mar 2011 by Akos Toth-Mate <akos@tothmate.com>
%%%-------------------------------------------------------------------

-module(plTypeDict).
-export([new_obj/0, execute/2, xform/2]).

new_obj() ->
    dict:new().

execute({set, Key, Value}, Dict) ->
    dict:store(Key, Value, Dict);
execute({unsafe_set, Key, Value}, Dict) ->
    execute({set, Key, Value}, Dict);
execute({remove, Key, Value}, Dict) ->
    dict:erase(Key, Value, Dict);
execute({unsafe_remove, Key, Value}, Dict) ->
    execute({remove, Key, Value}, Dict).

xform({set, Key, Value1}, {set, Key, _Value2}) ->
    {fail, {set, Key, Value1}, nop};
xform({unsafe_set, Key, Value1}, {set, Key, Value2}) ->
    xform({set, Key, Value1}, {set, Key, Value2});
xform({set, Key, _Value1}, {unsafe_set, Key, Value2}) ->
    {ok, nop, {set, Key, Value2}};
xform({unsafe_set, Key, Value1}, {unsafe_set, Key, Value2}) ->
    xform({set, Key, Value1}, {unsafe_set, Key, Value2});
xform({set, Key, Value}, {remove, Key}) ->
    {fail, {set, Key, Value}, nop};
xform({unsafe_set, Key, Value}, {remove, Key}) ->
    xform({set, Key, Value}, {remove, Key});
xform({set, Key, _Value}, {unsafe_remove, Key}) ->
    {ok, nop, {remove, Key}};
xform({unsafe_set, Key, Value}, {unsafe_remove, Key}) ->
    xform({set, Key, Value}, {unsafe_remove, Key});
xform({remove, Key}, {set, Key, Value}) ->
    {ok, nop, {set, Key, Value}};
xform({unsafe_remove, Key}, {set, Key, Value}) ->
    xform({remove, Key}, {set, Key, Value});
xform({remove, Key}, {unsafe_set, Key, Value}) ->
    xform({remove, Key}, {set, Key, Value});
xform({unsafe_remove, Key}, {unsafe_set, Key, Value}) ->
    xform({remove, Key}, {set, Key, Value});
xform({_, _Key1, _}, {_, _Key2, _}) -> % covers: (unsafe_)remove vs (unsafe_)remove, different keys
    {ok, nop, nop}.

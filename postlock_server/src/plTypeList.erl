%%%-------------------------------------------------------------------
%%% File    : plTypeList.erl
%%% Author  : Akos Toth-Mate <akos@tothmate.com>
%%% Description : list data type
%%%
%%% Created :  27 Mar 2011 by Akos Toth-Mate <akos@tothmate.com>
%%%-------------------------------------------------------------------

-module(plTypeList).
-export([new_obj/1, get_oid/1, execute/2, xform/2]).

new_obj(Oid) ->
    {Oid, []}.

get_oid({Oid, _}) ->
    Oid.

execute({insert, Position, Value}, {Oid, List}) ->
    {List0, List1} = lists:split(Position, List), 
    {Oid, List0 ++ [Value] ++ List1};
execute({remove, Position}, {Oid, List}) ->
    {List0, List1} = lists:split(Position, List),
    {Oid, List0 ++ lists:nthtail(1, List1)}.

xform({insert, Position1, Value1}, {insert, Position2, Value2}) when Position1 =< Position2 ->
    {ok, {insert, Position1, Value1}, {insert, Position2 + 1, Value2}};
xform({insert, Position1, Value1}, {insert, Position2, Value2}) ->
    {ok, {insert, Position1 + 1, Value1}, {insert, Position2, Value2}};
xform({insert, Position1, Value1}, {remove, Position2}) when Position1 =< Position2 ->
    {ok, {insert, Position1, Value1}, {remove, Position2 + 1}};
xform({insert, Position1, Value1}, {remove, Position2}) ->
    {ok, {insert, Position1 - 1, Value1}, {remove, Position2}};
xform({remove, Position1}, {insert, Position2, Value2}) when Position1 < Position2 ->
    {ok, {remove, Position1}, {insert, Position2 - 1, Value2}};
xform({remove, Position1}, {insert, Position2, Value2}) ->
    {ok, {remove, Position1 + 1}, {insert, Position2, Value2}};
xform({remove, _Position}, {remove, _Position}) ->
    {ok, nop, nop};
xform({remove, Position1}, {remove, Position2}) when Position1 < Position2 ->
    {ok, {remove, Position1}, {remove, Position2 - 1}};
xform({remove, Position1}, {remove, Position2}) when Position1 > Position2 ->
    {ok, {remove, Position1 - 1}, {remove, Position2}}.

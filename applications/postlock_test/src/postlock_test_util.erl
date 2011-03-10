%%%-------------------------------------------------------------------
%%% File    : postlock_test_util.erl
%%% Author  : Akos Toth-Mate 
%%% Description : 
%%%
%%% Created :  10 Mar 2011 by Akos Toth-Mate 
%%%-------------------------------------------------------------------
-module(postlock_test_util).

-export([md5_string/1]).

md5_string(S) ->
    hex(binary_to_list(crypto:md5(S))).

digit_to_xchar(D) when (D >= 0) and (D < 10) ->
    D + 48;
digit_to_xchar(D) ->
    D + 87.

hex(S) ->
    hex(S, []).
hex([], Res) ->
    lists:reverse(Res);
hex([N | Ns], Res) ->
    hex(Ns, [digit_to_xchar(N rem 16),
    digit_to_xchar(N div 16) | Res]).
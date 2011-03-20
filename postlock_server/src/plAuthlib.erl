%%%-------------------------------------------------------------------
%%% File    : plAuthlib.erl
%%% Author  : Peter Neumark <neumark@postlock.org>
%%% Description :
%%% Created :  16 Mar 2011 by Peter Neumark <neumark@postlock.org>
%%%-------------------------------------------------------------------
-module(plAuthlib).
-export([get_auth_impl/1]).
% Eventually, this should be configurable.
-define(DEFAULT_AUTH_IMPL, trivial). 

get_auth_impl(Impl) ->
    case Impl of
        trivial ->
            {fun trivial_auth_challenge/0,
             fun trivial_authenticate/3};
        digest -> 
            {fun digest_auth_challenge/0,
             fun digest_authenticate/3};
        _ -> get_auth_impl(?DEFAULT_AUTH_IMPL)
    end.

trivial_auth_challenge() ->
    {struct, [
        {"challenge_type", "trivial"}
    ]}.

trivial_authenticate(_Challenge, Response, ApplicationProcess) ->
    try 
        {ok, Username} = plMessage:json_get_value([username], Response),
        {ok, Password} = plMessage:json_get_value([password], Response),
        case {gen_server:call(ApplicationProcess, 
            {get_password, Username}), Password} of
            {{ok, Pass}, Pass} -> {ok, Username};
            {{ok, _RealPass}, _OurPass} -> {error, bad_password};
            {{error, _} = Err, _} -> Err
        end
    catch 
        error:{badmatch, _} -> {error, bad_response}
    end.

digest_auth_challenge() ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    {struct, [
        {challenge_type, "digest"},
        {realm, "aaa"},
        {uri, "/backend.yaws"},
        {qop, "auth"},
        {algorithm, "MD5"},
        {nonce, float_to_list(random:uniform())},
        {opaque, float_to_list(random:uniform())}
    ]}.

digest_authenticate(Challenge, Response, ApplicationProcess) ->
    try
        {ok, Realm} = plMessage:json_get_value([realm], Challenge),
        {ok, Uri} = plMessage:json_get_value([uri], Challenge),
        {ok, Qop} = plMessage:json_get_value([qop], Challenge),
        {ok, Nonce} = plMessage:json_get_value([nonce], Challenge),
        {ok, Opaque} = plMessage:json_get_value([opaque], Challenge),

        {ok, Username} = plMessage:json_get_value([username], Response),
        {ok, ResponseHash} = plMessage:json_get_value([response], Response),
        {ok, Nc} = plMessage:json_get_value([nc], Response),
        {ok, Cnonce} = plMessage:json_get_value([cnonce], Response),
        {ok, ResponseOpaque} = plMessage:json_get_value([opaque], Response),

        case gen_server:call(ApplicationProcess, 
            {get_password, Username}) of
            {ok, Password} -> 
                A1 = md5_string(string:join([Username, Realm, Password], ":")),
                A2 = md5_string(string:join(["POST", Uri], ":")),
                ExpectedResponseHash = md5_string(string:join([A1, Nonce, Nc, Cnonce, Qop, A2], ":")),
                case ResponseOpaque == Opaque andalso ResponseHash == ExpectedResponseHash of
                    true -> {ok, Username};
                    false -> {error, bad_password}
                end;
            {error, _} = Err -> Err
        end
    catch 
        error:{badmatch, E} -> 
            io:format("**** digest auth exception **** ~p~n", [E]),
            {error, bad_response}
    end.

%%% ----------------
%%% utility functions for auth_digest implementation
%%% ----------------

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

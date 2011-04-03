%%% ----------------------------------------------------------
%%% File:   plError.hrl
%%% Author: Peter Neumark
%%% Description: Error code macros.
%%% Created :  20 Jan 2011 by Peter Neumark
%%% ----------------------------------------------------------
-hrl_author('neumark').

-ifndef(PL_ERROR_HRL).
-define(PL_ERROR_HRL,1).
-define(ERROR2JSON(Err),
    {struct, [
        {"code", element(1,Err)},
        {"message", element(2, Err)}
    ]}
).
%%% ----------------------------------------------------------
%%% 1XX ERRORS: issued by plGateway
%%% ----------------------------------------------------------
-define(PL_ERR_UNEXPECTED_MESSAGE_ID,
    {101, "Received message had incorrect message id."}).
-define(PL_ERR_MALFORMED_MESSAGE,
    {102, "Error parsing message."}).
-define(PL_ERR_AUTH_FAILURE,
    {103, "Authentication failed."}).

-endif. %PL_ERROR_HRL


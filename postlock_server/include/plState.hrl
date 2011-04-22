%%% ----------------------------------------------------------
%%% File:   plRegistry.hrl
%%% Author: Peter Neumark
%%% Description: Contains records and constants used by
%%% plState module.
%%%
%%% ----------------------------------------------------------
-hrl_author('neumark').

-ifndef(PL_STATE_HRL).
-define(PL_STATE_HRL,1).
%%% ----------------------------------------------------------
%%% postlock_object - this is where the session data is 
%%% stored.
%%% ----------------------------------------------------------
-record(postlock_object, {
    oid,                     % OID (key)
    content,                % opaque, content depends upon
                            % determines type
    last_transformation,    % The last transformation committed on 
                            % the object
    permissions             % not used yet
}).

%%% ----------------------------------------------------------
%%% Various postlock object types: 
%%% ----------------------------------------------------------
-record(postlock_content_dict, {
    children = gb_trees:empty() % contains a gb_tree
}).
-record(postlock_content_data, {
    data                    % contains a string (list)
}).
-record(postlock_content_list, {
    children = []           % contains a list of OIDs
}).

-record(postlock_transaction, {
    id,
    ops
}).


%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
-endif. %PL_STATE_HRL


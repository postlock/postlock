%%%-------------------------------------------------------------------
%%% File    : plStorageTerm.erl
%%% Author  : Peter Neumark <neumark@postlock.org>
%%% Description : Stores state in an erlang term.
%%% The current implementation uses a gb_tree, which contains
%%% #obj records. This means for each object the
%%% gb_tree contains the mapping:
%%% oid -> {action, object}, where action is the action taken on
%%% the object (if any).
%%% 
%%%
%%% Created :  27 Mar 2011 by Peter Neumark <neumark@postlock.org>
%%%-------------------------------------------------------------------
-module(plStorageTerm).
-export([
    % standard storage implementation functions
    delete/2,
    new_state/0,
    create/2,
    update/2,
    get/2,
    is_set/2,
    destroy/1
]).

%% These records are used internally by this module.
-record(obj, { % Obj contains all data associated with one object.
    action = none,   % what happened to object: 
                     % create | delete | modify | none
    object           % the object itself
}).

new_state() ->
    %% state is simply a gb_tree
    gb_trees:empty().

create(Obj, State) -> set(Obj, State, create).
update(Obj, State) -> set(Obj, State, modify).

set(Obj, State, Action) ->
    Oid = plObject:get_oid(Obj),
    Val = #obj{action=Action, object=Obj},
    case gb_trees:is_defined(Oid, State) of
        true -> gb_trees:update(Oid, Val, State);
        false -> gb_trees:insert(Oid, Val, State)
    end.

get(Oid, State) ->
    %% Returns undefined if Oid is not in state.
    case is_set(Oid, State) of
        false -> undefined;
        true -> 
            Obj = gb_trees:get(Oid, State),
            Obj#obj.object
    end.

delete(Oid, State) ->
    %% doesn't actually delete the object from the
    %% gb_tree, just sets the action to delete
    case gb_trees:is_defined(Oid, State) of
        false -> 
            gb_trees:insert(Oid, #obj{action=delete}, State);
        true ->  
            gb_trees:update(Oid, #obj{action=delete}, State)
    end.

is_set(Oid, State) ->
    %% Oid is set if such an object is stored
    %% in the gb_tree and its action is not
    %% delete
    case gb_trees:is_defined(Oid, State) of
        false -> false;
        true ->  
            Obj = gb_trees:get(Oid, State),
            Obj#obj.action =/= delete
    end.

destroy(_State) -> ok.

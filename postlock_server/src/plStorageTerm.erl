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
    insert/2,
    update/2,
    get_object/2,
    is_set/2,
    % function for iterating over stored objects
    get_all_objects/1
]).


%% These records are used internally by this module.
-record(obj, { % Obj contains all data associated with one object.
    action,          % what happened to object: insert | delete | update
    object           % the object itself
}).

new_state() ->
    %% state is simply a gb_tree
    gb_trees:empty().

insert(Obj, State) -> set(Obj, State, insert).
update(Obj, State) -> set(Obj, State, update).

set(Obj, State, Action) ->
    Oid = plObject:get_oid(Obj),
    Val = #obj{action=Action, object=Obj},
    case gb_trees:is_defined(Oid, State) of
        true -> gb_trees:update(Oid, Val, State);
        false -> gb_trees:insert(Oid, Val, State)
    end.

get_object(Oid, State) ->
    %% Oid is set if such an object is stored
    %% in the gb_tree and its action is not
    %% delete
    case gb_trees:is_defined(Oid, State) of
        false -> undefined;
        true ->  
            Obj = gb_trees:get(Oid, State),
            case Obj#obj.action of
                delete -> undefined;
                _ -> Obj#obj.object
            end
            
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
            case Obj#obj.action of
                delete -> false;
                _ -> true
            end
    end.

% Quick n dirty, not all that efficient...
get_all_objects(State) ->
    [{Oid, Action, Obj} || 
        {Oid, #obj{action=Action, object=Obj}} <- gb_trees:to_list(State)].

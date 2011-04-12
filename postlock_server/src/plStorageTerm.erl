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
    store/2,
    get_object/2,
    is_set/2,
    % functions for iterating over stored objects
    iterator/1,
    next/1
]).
-include_lib("eunit/include/eunit.hrl").


%% These records are used internally by this module.
-record(obj, { % Obj contains all data associated with one object.
    action,          % what happened to object: store | delete 
    object           % the object itself
}).

new_state() ->
    %% state is simply a gb_tree
    gb_trees:empty().

store(Obj, State) ->
    Oid = plObject:get_oid(Obj),
    Val = #obj{action=store, object=Obj},
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

% Tothmate's efficient solution (nice!)
iterator(State) ->
    gb_trees:iterator(State).

next(Iter) ->
    Next = gb_trees:next(Iter),
    case Next of
        {Oid, Object, Iter1} -> {Oid, Object#obj.object, Object#obj.action, Iter1};
        none -> none
    end.
%%%-------------------------------------------------------------------
%%% Unit tests
%%%-------------------------------------------------------------------
get_objects_test_() ->
    % This test performs the following actions:
    %          OID: |   0   |   1   |   2   |   3   |
    % action: store |   X   |   X   |   X   |       |
    % action: delete|       |   X   |   X   |   X   |
    %               ---------------------------------
    % Since the last action 'wins', the final actions
    % should be:    |   s   |   d   |   d   |   d   |

    Contents = 
        [{store, plObject:new_obj(plTypeData, X)} 
            || X <- lists:seq(0,2)] ++
        [{delete, X} 
            || X <- lists:seq(1,3)],
        State = lists:foldl(
            fun({Fun, Arg}, State) ->
                apply(?MODULE, Fun, [Arg, State])
            end, new_state(), Contents),
    ObjectList = get_all_objects(State),
    % the first element should have the action of store.
    [?_assert(erlang:element(2, lists:keysearch(0, 1, ObjectList)) 
        =:= {0, store, plObject:new_obj(plTypeData, 0)})|
        % the remaining element should have the action of delete.
        [?_assert(erlang:element(2, lists:keysearch(X, 1, ObjectList)) 
            =:= {X, delete, undefined}) || X <- lists:seq(1,3)]].


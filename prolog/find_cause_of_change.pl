
:- module(find_cause,
    [
      find_causing_action/5,
      find_causing_action/4
%     find_causing_action/1
    ]).

:- use_module(library('action_effects_ext')).
:- use_module(library('action_effects')).
% :- use_module(library('knowrob_actions')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs_computable')).
:- use_module(library('knowrob_owl')).

:- rdf_db:rdf_register_ns(knowrob, 'http://ias.cs.tum.edu/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#', [keep(true)]).
:- rdf_db:rdf_register_ns(owl, 'http://www.w3.org/2002/07/owl#', [keep(true)]).

:- rdf_meta
    find_causing_action(r,r,r,r,r),
    find_causing_action(r,r,r,r),
%   find_causing_action(r).
    test_projection(r,r,r,r),
    actionset_success(r,r,r,r,r),
    test_projection(r,r,r,r,r),
    actionset_success(r,r,r,r,r,r),

    actionset_objectActedOn(r,r),
    actionset_toLocation(r,r),
    actionset_fromLocation(r,r),
    action_objectActedOn(r,r),
    action_toLocation(r,r),
    action_fromLocation(r,r).



%% clean_projection_cache (see also: knowrob_actions/prolog/action_effects.pl)
%
% remove all triples and object instances
% that have been asserted as part of the projection methods
clean_projection_cache :-
  rdf_retractall(_, _, _, knowrob_projection),
  rdf_retractall(_, knowrob_projection, _).



%% actionset_objectActedOn
%
% finds all action descriptions,
% that may feature ObjInst as an objectActedOn
actionset_objectActedOn(ObjInst, ActionSet) :-
  setof(Action, ObjActOnType^(
    owl_individual_of(ObjInst, ObjActOnType),
    action_objectActedOn(Action, ObjActOnType)), SuperActionSet), !,
  % include actions which only inherit objectActedOn
  setof(Action, SuperAction^(member(SuperAction, SuperActionSet),
    owl_subclass_of(Action, SuperAction)), ActionSet).

%% action_objectActedOn (see also: knowrob_actions/prolog/knowrob_actions.pl)
%
% checks if the TBOX description of Action includes Object as an objectActedOn
action_objectActedOn(Action, Object) :-
        owl_subclass_of(Action, knowrob:'Event'),
        owl_direct_subclass_of(Action, Sup),
        owl_direct_subclass_of(Sup, Sup2),
        owl_restriction(Sup2,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#objectActedOn', some_values_from(Object))).
action_objectActedOn(Action, Object) :-
        owl_subclass_of(Action, knowrob:'Event'),
        owl_direct_subclass_of(Action, Sup),
        owl_restriction(Sup,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#objectActedOn', some_values_from(Object))).



%% actionset_toLocation
%
% finds all action descriptions,
% that may feature ObjInst as a toLocation
actionset_toLocation(ObjInst, ActionSet) :-
  setof(Action, ObjType^(
    owl_individual_of(ObjInst, ObjType),
    action_toLocation(Action, ObjType)), ActionSet).

%% action_toLocation (see also: knowrob_actions/prolog/knowrob_actions.pl)
%
% checks if the TBOX description of Action includes Object as a toLocation
action_toLocation(Action, Object) :-
        owl_subclass_of(Action, knowrob:'Event'),
        owl_direct_subclass_of(Action, Sup),
        owl_direct_subclass_of(Sup, Sup2),
        owl_restriction(Sup2,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#toLocation', some_values_from(Object))).
action_toLocation(Action, Object) :-
        owl_subclass_of(Action, knowrob:'Event'),
        owl_direct_subclass_of(Action, Sup),
        owl_restriction(Sup,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#toLocation', some_values_from(Object))).



%% actionset_fromLocation
%
% finds all action descriptions,
% that may feature ObjInst as a fromLocation
actionset_fromLocation(ObjInst, ActionSet) :-
  setof(Action, ObjType^(
    owl_individual_of(ObjInst, ObjType),
    action_fromLocation(Action, ObjType)), ActionSet).

%% action_fromLocation (see also: knowrob_actions/prolog/knowrob_actions.pl)
%
% checks if the TBOX description of Action includes Object as a fromLocation
action_fromLocation(Action, Object) :-
        owl_subclass_of(Action, knowrob:'Event'),
        owl_direct_subclass_of(Action, Sup),
        owl_direct_subclass_of(Sup, Sup2),
        owl_restriction(Sup2,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#fromLocation', some_values_from(Object))).
action_fromLocation(Action, Object) :-
        owl_subclass_of(Action, knowrob:'Event'),
        owl_direct_subclass_of(Action, Sup),
        owl_restriction(Sup,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#fromLocation', some_values_from(Object))).



% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% Querying for Actions which induce certain changes to the world
%
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 

%% find_causing_action: State Change Actions
%
% TODO: Description
%
% @param Obj        Object which undergoes the state change
% @param Prop       Property that changes
% @param FromValue  Previous property value
% @param ToValue    New property value
% @param ResultSet  Set of known actions which are able to induce this change

find_causing_action(Obj, Prop, FromValue, ToValue, ResultSet) :-

  owl_has(Obj, Prop, FromValue),
  actionset_objectActedOn(Obj, ActionSet), !,
  actionset_projection_success(ActionSet, Obj, Prop, ToValue, ResultSet).


%% actionset_projection_success 
%
% TODO: Description
%
% @param ActionSet  Set of actions to be tested 
% @param ...        (refer to find_causing_action: State Change Actions)

actionset_projection_success([], _, _, _, []).
actionset_projection_success(ActionSet, Obj, Prop, ToValue, ResultSet) :-
  append([Head], Tail, ActionSet),
  actionset_projection_success(Tail, Obj, Prop, ToValue, RS), 
  % if: action projection results in correct changes
  (test_projection(Head, Obj, Prop, ToValue) ->
    % then: add to ResultSet and clean cache
    print('correct changes induced \n'),
    append(RS, Head, ResultSet), clean_projection_cache;
    % else: clean cache
    print('does not induce correct changes \n'), clean_projection_cache, ResultSet = RS).


%% test_projection 
%
% TODO: Description
%
% @param Action     Action whose results are tested 
% @param ...        (refer to find_causing_action: State Change Actions)

test_projection(Action, Obj, Prop, ToValue) :-
  rdf_instance_from_class(Action, knowrob_projection, ActionInst),
  rdf_assert(ActionInst, knowrob:'objectActedOn', Obj, knowrob_projection),
  print('created: '), print(ActionInst), print(' for effect testing \n'),
  project_action_effects(ActionInst),!,
  owl_has(Obj, Prop, ToValue).




%% find_causing_action: Adding something new to an object/container/etc.
%
% The Object Instance is known, but a new property was added to it
% (something was put onto/into the object)
%
% @param Obj        Known object which something is added to 
% @param Relation   Relation between known and new object (eg. contains)
% @param addedObj   newly added Object 
% @param ResultSet  Set of known actions which are able to induce this change

find_causing_action(Obj, Relation, AddedObj, ResultSet) :-
  actionset_toLocation(Obj, ActionSet), !.
  owl_individual_of(AddedObj, AddedObjType),
% TODO:
% objectActedOn either Inst of addedObjType
  owl_instance_of_class(SourceObj, knowrob_projection, AddedObjType);
% or Inst of Obj containing addedObjType
  (owl_has(AddedObjType, knowrob:'contains', ContainedObjType),
    owl_instance_of_class(SourceObj, knowrob_projection, ContainedObjType)), 
  % try project action effects
  actionset_projection_success(ActionSet, SourceObj, Obj, Relation, AddedObj, ResultSet).
  

actionset_projection_success([], _, _, _, _, []).
actionset_projection_success(ActionSet, ObjActedOn, ToLocation, Relation, ObjOfComp, ResultSet) :-
  append([Head], Tail, ActionSet),
  actionset_projection_success(Tail, ObjActedOn, ToLocation, Relation, ObjOfComp, RS), 
  % if: action projection results in correct changes
  (test_projection(Head, ObjActedOn, ToLocation, Relation, ObjOfComp) ->
    % then: add to ResultSet and clean cache
    print('correct changes induced \n'),
    append(RS, Head, ResultSet), clean_projection_cache;
    % else: clean cache
    print('does not induce correct changes \n'), clean_projection_cache, ResultSet = RS).

   
test_projection(Action, ObjActedOn, ToLocation, Relation, ObjOfComp) :-
  rdf_instance_from_class(Action, knowrob_projection, ActionInst),
  rdf_assert(ActionInst, knowrob:'objectActedOn', ObjActedOn, knowrob_projection),
  rdf_assert(ActionInst, knowrob:'toLocation', ToLocation, knowrob_projection),
  print('created: '), print(ActionInst), print(' for effect testing \n'),
  project_action_effects(ActionInst),!,
  % check wether new object related to ToLocation is of the same type as ObjOfComp
  owl_has(ToLocation, Relation, NewObj), owl_has(NewObj, rdf:type, Type),
  (Type\='http://www.w3.org/2002/07/owl#NamedIndividual'), owl_has(ObjOfComp, rdf:type, Type).


%% find_causing_action: Removing something from a object/container/etc.

% Creating something new
%find_causing_action(Obj) :-
   



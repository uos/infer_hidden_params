
:- module(find_cause,
    [
      find_causing_action/5
%     find_causing_action/3,
%     find_causing_action/1
    ]).

:- use_module(library('action_effects_ext')).
:- use_module(library('action_effects')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs_computable')).
:- use_module(library('knowrob_owl')).

:- rdf_db:rdf_register_ns(knowrob, 'http://ias.cs.tum.edu/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#', [keep(true)]).
:- rdf_db:rdf_register_ns(owl, 'http://www.w3.org/2002/07/owl#', [keep(true)]).

:- rdf_meta
    find_causing_action(r,r,r,r,r),
%   find_causing_action(r,r,r),
%   find_causing_action(r).
    actionset_success(r,r,r,r,r,r),
    actionset_objectActedOn(r,r),
    test_projection(r,r,r,r,r).



%% clean_projection_cache (see also: knowrob_actions/prolog/action_effects.pl)
%
% remove all triples and object instances
% that have been asserted as part of the projection methods
%
clean_projection_cache :-
  rdf_retractall(_, _, _, knowrob_projection),
  rdf_retractall(_, knowrob_projection, _).


% finds all action descriptions, that may feature Obj as an objectActedOn
% TODO: PropertyType as variable
actionset_objectActedOn(Obj, ActionSet) :-
  setof(Action, Descr^ObjActOnType^(
    owl_has(Action, rdfs:subClassOf, Descr),
    owl_has(Descr, owl:onProperty, knowrob:'objectActedOn'),
    owl_has(Descr, owl:someValuesFrom, ObjActOnType),
    owl_individual_of(Obj, ObjActOnType)), ActionSet),
    print(ActionSet).
% TODO: extend to collection 



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

  actionset_objectActedOn(Obj, ActionSet), !,
  actionset_projection_success(ActionSet, Obj, Prop, FromValue, ToValue, ResultSet).


%% actionset_projection_success 
%
% TODO: Description
%
% @param ActionSet  Set of actions to be tested 
% @param ...        (refer to find_causing_action: State Change Actions)

actionset_projection_success([], _, _, _, _, []).
actionset_projection_success(ActionSet, Obj, Prop, FromValue, ToValue, ResultSet) :-
  append([Head], Tail, ActionSet),
  actionset_projection_success(Tail, Obj, Prop, FromValue, ToValue, RS), 
  % if: action projection results in correct changes
  (test_projection(Head, Obj, Prop, FromValue, ToValue) ->
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

test_projection(Action, Obj, Prop, FromValue, ToValue) :-
  rdf_instance_from_class(Action, knowrob_projection, ActionInst),
  rdf_assert(ActionInst, knowrob:'objectActedOn', Obj, knowrob_projection),
  print('created: '), print(ActionInst), print(' for effect testing \n'),
  project_action_effects(ActionInst),!,
  owl_has(Obj, Prop, ToValue).
% TODO: check status before action



%% find_causing_action: Adding something new to an object/container/etc.
%
% The Object Instance is known, but a new property was added to it
% (something was put onto/into the object, temperature was changed)
%
% probably: combine with find_causing_action for State Change Actions with unknown FromValue
%
% TODO: Description
%
% @param Obj        Known object which undergoes some change
% @param Prop       Property that changes
% @param ToValue    New property value
% @param ResultSet  Set of known actions which are able to induce this change

% find_causing_action(Obj, Prop, ToValue, ResultSet) :-


% Creating something new
%find_causing_action(Obj) :-
   



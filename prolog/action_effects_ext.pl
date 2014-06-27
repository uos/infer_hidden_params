
:- module(action_effects_ext,
    [
      project_action_effects/1
    ]).

% :- use_module(library('action_effects')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs_computable')).
:- use_module(library('knowrob_owl')).

:- rdf_db:rdf_register_ns(knowrob, 'http://ias.cs.tum.edu/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(drink_world, 'http://infer_hidden_params/drink_world.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(door_map, 'http://infer_hidden_params/door_map.owl#', [keep(true)]).

:- rdf_meta
    project_action_effects(r),
    remove_object_prop(r,r,r).


% remove all assertions of sub-properties of Property with Value from Obj
remove_object_prop(Obj, Property, Value) :-
(findall(Prop, (rdfs_subproperty_of(Prop, Property)), Props),
  findall(P, (member(P,Props), rdf_retractall(Obj, P, Value)), _)),!.



% % % % % % % % % % % % % % % % % % % %
% Filling Process
project_action_effects(Action) :-

  owl_individual_of(Action, knowrob:'FillingProcess'),

  owl_has(Action, knowrob:objectActedOn, SourceVessel),
  owl_has(Action, knowrob:toLocation, TargetVessel),
  owl_has(SourceVessel, knowrob:contains, SourceContent),
  ((owl_has(SourceContent, rdf:type, 'http://www.w3.org/2002/07/owl#Class'), SourceType=SourceContent);
    (owl_has(SourceContent, rdf:type, SourceType), dif(SourceType, 'http://www.w3.org/2002/07/owl#NamedIndividual'))),
  \+ (owl_has(TargetVessel, knowrob:'contains', TargetContent), owl_has(TargetContent, rdf:type, SourceType)),!,

  % new objects
  rdf_instance_from_class(SourceType, knowrob_projection, NewContent),
  % TODO: copy properties

  % remove previous content relations
  remove_object_prop(TargetVessel, knowrob:contains, _),

  % new relations
  rdf_assert(Action, knowrob:outputsRemaining, SourceContent,knowrob_projection),
  rdf_assert(Action, knowrob:outputsCreated, NewContent, knowrob_projection),
  
  rdf_assert(TargetVessel, knowrob:contains, NewContent, knowrob_projection),

  rdf_assert(NewContent, knowrob:createdBy, Action, knowrob_projection),   

  print(TargetVessel), print(' filled with '), print(NewContent), print('\n').



% % % % % % % % % % % % % % % % % % % %
% Cutting off a piece (see also: knowrob_actions/prolog/action_effects.pl)
project_action_effects(Action) :-

  owl_individual_of(Action, knowrob:'CuttingOffAPiece'),
  \+ owl_has(Action, knowrob:outputsCreated, _),


  owl_has(Action, knowrob:objectActedOn, Obj),
  rdf_has(Obj, rdf:type, ObjType),
  ObjType \= 'http://www.w3.org/2002/07/owl#NamedIndividual',!,

  % new objects
  rdf_instance_from_class(ObjType, knowrob_projection, Slice),

  % new relations
  rdf_assert(Action, knowrob:outputsRemaining, Obj, knowrob_projection),
  rdf_assert(Action, knowrob:outputsCreated, Slice, knowrob_projection),

  % extended by: relation from new object to Action
  rdf_assert(Slice, knowrob:createdBy, Action, knowrob_projection),

  print(Obj),print(' -> '), print(Slice), print('\n').


    
% % % % % % % % % % % % % % % % % % % %
% Opening a Door 
project_action_effects(Action) :-

  owl_individual_of(Action, knowrob:'OpeningADoor'),

  owl_has(Action, knowrob:objectActedOn, Obj),
  \+ owl_has(Obj, knowrob:stateOfObject, knowrob:'ObjectStateOpen'),!,

  % remove asserted stateOfObject
  remove_object_prop(Obj, knowrob:stateOfObject, knowrob:'ObjectStateClosed'),
  % new relations
  rdf_assert(Obj, knowrob:'stateOfObject', knowrob:'ObjectStateOpen', knowrob_projection),

  rdf_assert(Action, knowrob:'objectOfStateChange', Obj, knowrob_projection),
  rdf_assert(Action, knowrob:'fromState', knowrob:'ObjectStateClosed', knowrob_projection),
  rdf_assert(Action, knowrob:'toState',   knowrob:'ObjectStateOpen', knowrob_projection),

  print(Obj),print(' opened'), print('\n').



% % % % % % % % % % % % % % % % % % % %
% Entering a Room 




% % % % % % % % % % % % % % % % % % % % 
%
% Querying missing Actions
%
% % % % % % % % % % % % % % % % % % % %

% State Change
%find_causing_action(Obj, Prop, FromValue, ToValue) :-

%  findall(Action,
%    (owl_has(Action, rdfs:SubClassOf, Temp), owl_has(Temp, owl:onProperty, knowrob:'ObjectActedOn'),
%    owl_has(Temp, owl:someValuesFrom, ObjActOnType), individual_of_subtype(Obj, ObjActOnType)), Actions).

% Adding something new to an object/container/etc.
%find_causing_action(Obj, Prop, ToValue) :-


% Creating something new
%find_causing_action(Obj) :-
   

% true if Indv is an individual of ObjType or one of the subClasses of ObjType
individual_of_subtype(Indv, ObjType) :-
  owl_individual_of(Indv, ObjType);
  owl_subclass_of(SubObjType, ObjType), owl_individual_of(Indv, SubObjType).




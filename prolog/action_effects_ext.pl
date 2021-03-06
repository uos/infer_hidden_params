
:- module(action_effects_ext,
    [
      action_effects:project_action_effects/1
    ]).

:- multifile(action_effects:project_action_effects/1).

:- use_module(library('action_effects')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs_computable')).
:- use_module(library('knowrob_owl')).

:- rdf_db:rdf_register_ns(knowrob, 'http://ias.cs.tum.edu/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#', [keep(true)]).
:- rdf_db:rdf_register_ns(owl, 'http://www.w3.org/2002/07/owl#', [keep(true)]).

:- rdf_meta
    project_action_effects(r),
    remove_object_prop(r,r,r),
    rooms_connected_by(r,-).

% remove all assertions of sub-properties of Property with Value from Obj
% (see also: knowrob_actions/prolog/action_effects.pl)
remove_object_prop(Obj, Property, Value) :-
(findall(Prop, (rdfs_subproperty_of(Prop, Property)), Props),
  findall(P, (member(P,Props), rdf_retractall(Obj, P, Value)), _)),!.


% % % % % % % % % % % % % % % % % % % %
% Filling Process
action_effects:project_action_effects(Action) :-

  owl_individual_of(Action, knowrob:'FillingProcess'),

  owl_has(Action, knowrob:objectActedOn, SourceVessel),
  owl_has(Action, knowrob:toLocation, TargetVessel),
  owl_has(SourceVessel, knowrob:'contains', SourceContent),
  (owl_has(SourceContent, rdf:type, 'http://www.w3.org/2002/07/owl#Class') -> SourceType=SourceContent;
    (owl_has(SourceContent, rdf:type, SourceType), SourceType\='http://www.w3.org/2002/07/owl#NamedIndividual')),
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
  rdf_assert(NewContent, knowrob:'in-ContGeneric', TargetVessel, knowrob_projection),

  print(TargetVessel), print(' filled with '), print(NewContent), print('\n').



% % % % % % % % % % % % % % % % % % % %
% DrinkingUp 
action_effects:project_action_effects(Action) :-

  owl_individual_of(Action, knowrob:'DrinkingUp').
  %TODO



% % % % % % % % % % % % % % % % % % % %
% DumpingAContainersContent
action_effects:project_action_effects(Action) :-

  owl_individual_of(Action, knowrob:'DumpingAContainersContent'),

  owl_has(Action, knowrob:objectActedOn, Container),
  owl_has(Container, knowrob:'contains', Content),

  % remove content relation
  remove_object_prop(Container, knowrob:contains, _),
 
  % new relations
  rdf_assert(Action, knowrob:'inputsDestroyed', Content, knowrob_projection),

  print(Container), print(' emptied \n'). 



% % % % % % % % % % % % % % % % % % % %
% Cutting off a piece (see also: knowrob_actions/prolog/action_effects.pl)
action_effects:project_action_effects(Action) :-

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
% Opening Door
action_effects:project_action_effects(Action) :-

  owl_individual_of(Action, knowrob:'OpeningADoor'),

  owl_has(Action, knowrob:objectActedOn, Obj),
  \+ owl_has(Obj, knowrob:stateOfObject, knowrob:'ObjectStateLocked'),
  \+ owl_has(Obj, knowrob:stateOfObject, knowrob:'ObjectStateOpen'),!,

  % remove asserted stateOfObject
  remove_object_prop(Obj, knowrob:stateOfObject, knowrob:'ObjectStateClosed'),
  % new relations
  rdf_assert(Obj, knowrob:'stateOfObject', knowrob:'ObjectStateOpen', knowrob_projection),

  rdf_assert(Action, knowrob:'objectOfStateChange', Obj, knowrob_projection),
  rdf_assert(Action, knowrob:'fromState', knowrob:'ObjectStateClosed', knowrob_projection),
  rdf_assert(Action, knowrob:'toState',   knowrob:'ObjectStateOpen', knowrob_projection),

  print(Obj), print(' opened'), print('\n'),

  (owl_has(Action, knowrob:performedBy, Person), owl_has(Action, knowrob:toLocation, Room)) ->
  (rdf_assert(Person, knowrob:'insideOf', Room),
  print(Person), print(' entered '), print(Room), print('\n'));true.


% rooms_connected_by
rooms_connected_by(Door, Rooms) :-
  findall(Room, (owl_individual_of(Door, knowrob:'DoorwayCovering'),
    owl_has(Portal, knowrob:intendedCovering, Door),
    owl_has(Portal, knowrob:betweenContainers, Room)), RoomList),
  sort(RoomList, Rooms).

% % % % % % % % % % % % % % % % % % % %
% Opening A Locked Door
action_effects:project_action_effects(Action) :-

  owl_individual_of(Action, knowrob:'OpeningALockedDoor'),

  owl_has(Action, knowrob:objectActedOn, Obj),
  owl_has(Obj, knowrob:stateOfObject, knowrob:'ObjectStateLocked'),
  \+ owl_has(Obj, knowrob:stateOfObject, knowrob:'ObjectStateOpen'),!,
  
  % if not set set search for toLocation and performedBy and assert
  ((owl_has(Action, knowrob:toLocation, Room),
    owl_has(Action, knowrob:performedBy, Person),
    owl_has(Room, knowrob:facilityIntendedForPersonType, Person));
  (rooms_connected_by(Obj, Rooms), member(Room, Rooms),
    owl_has(Room, knowrob:facilityIntendedForPersonType, Person),
    rdf_assert(Action, knowrob:toLocation, Room),
    rdf_assert(Action, knowrob:facilityIntendedForPersonType, Person))),

  % remove asserted stateOfObject
  remove_object_prop(Obj, knowrob:stateOfObject, knowrob:'ObjectStateLocked'),
  % new relations
  rdf_assert(Obj, knowrob:'stateOfObject', knowrob:'ObjectStateOpen', knowrob_projection),

  rdf_assert(Action, knowrob:'objectOfStateChange', Obj, knowrob_projection),
  rdf_assert(Action, knowrob:'fromState', knowrob:'ObjectStateClosed', knowrob_projection),
  rdf_assert(Action, knowrob:'toState',   knowrob:'ObjectStateOpen', knowrob_projection),
  print(Obj), print(' opened'), print('\n'),

  rdf_assert(Person, knowrob:'insideOf', Room),
  print(Person), print(' entered '), print(Room), print('\n').



% % % % % % % % % % % % % % % % % % % %
% Closing A Door 
action_effects:project_action_effects(Action) :-

  owl_individual_of(Action, knowrob:'ClosingADoor'),

  owl_has(Action, knowrob:objectActedOn, Obj),
  \+ owl_has(Obj, knowrob:stateOfObject, knowrob:'ObjectStateClosed'),!,

  % remove asserted stateOfObject
  remove_object_prop(Obj, knowrob:stateOfObject, knowrob:'ObjectStateOpen'),
  % new relations
  rdf_assert(Obj, knowrob:'stateOfObject', knowrob:'ObjectStateClosed', knowrob_projection),

  rdf_assert(Action, knowrob:'objectOfStateChange', Obj, knowrob_projection),
  rdf_assert(Action, knowrob:'fromState', knowrob:'ObjectStateOpen', knowrob_projection),
  rdf_assert(Action, knowrob:'toState',   knowrob:'ObjectStateClosed', knowrob_projection),

  print(Obj), print(' closed'), print('\n'),

  (owl_has(Action, knowrob:performedBy, Person), owl_has(Action, knowrob:fromLocation, Room)) ->
  (remove_object_prop(Person, knowrob:'insideOf', Room),
  print(Person), print(' left '), print(Room), print('\n'));true.



% % % % % % % % % % % % % % % % % % % %
% Locking A Door
action_effects:project_action_effects(Action) :-

  owl_individual_of(Action, knowrob:'LockingADoor'),

  owl_has(Action, knowrob:objectActedOn, Obj),
  \+ owl_has(Obj, knowrob:stateOfObject, knowrob:'ObjectStateLocked'),!,

  % if not set set search for fromLocation and performedBy and assert
  ((owl_has(Action, knowrob:fromLocation, Room),
    owl_has(Action, knowrob:performedBy, Person),
    owl_has(Room, knowrob:facilityIntendedForPersonType, Person));
  (rooms_connected_by(Obj, Rooms), member(Room, Rooms),
    owl_has(Room, knowrob:facilityIntendedForPersonType, Person),
    rdf_assert(Action, knowrob:fromLocation, Room),
    rdf_assert(Action, knowrob:facilityIntendedForPersonType, Person))),

  % remove asserted stateOfObject
  remove_object_prop(Obj, knowrob:stateOfObject, _),
  % new relations
  rdf_assert(Obj, knowrob:'stateOfObject', knowrob:'ObjectStateLocked', knowrob_projection),

  rdf_assert(Action, knowrob:'objectOfStateChange', Obj, knowrob_projection),
  rdf_assert(Action, knowrob:'fromState', knowrob:'ObjectStateOpen', knowrob_projection),
  rdf_assert(Action, knowrob:'toState',   knowrob:'ObjectStateLocked', knowrob_projection),
  print(Obj), print(' locked'), print('\n'),

  remove_object_prop(Person, knowrob:'insideOf', Room),
  print(Person), print(' left '), print(Room), print('\n').

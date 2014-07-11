
:- module(find_cause,
    [
      find_causing_action/4,
      find_causing_action/2,
      find_cause_of_stateChange/5,
      find_cause_of_stateChange/4,
      find_cause_of_disappearance/2,
      find_cause_of_disappearance/4
    ]).

:- use_module(library('action_effects_ext')).
:- use_module(library('action_effects')).
% :- use_module(library('knowrob_actions')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs_computable')).
:- use_module(library('knowrob_owl')).

:- rdf_db:rdf_register_ns(knowrob,'http://ias.cs.tum.edu/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#', [keep(true)]).
:- rdf_db:rdf_register_ns(owl, 'http://www.w3.org/2002/07/owl#', [keep(true)]).

:- rdf_meta
    find_causing_action(r,r,r,r),
    find_causing_action(r,r),
    
    find_cause_of_stateChange(r,r,r,r,?),
    find_cause_of_stateChange(r,r,r,?),

    find_cause_of_disappearance(r,?),
    test_projection_for_disappearance(r,r),
    find_cause_of_disappearance(r,r,r,?),
    test_projection_for_disappearance(r,r,r,r),

    create_action_inst(r,r,r,r,?),
    getAction_objectActedOn(?,r),
    getObject_objectActedOn(r,?),

    test_projection(r,r,r,r,r),
    actionset_projection_success(r,r,r,r,r,r),
    test_projection(r,r,r),
    actionset_projection_success(r,r,r),

    actionset_objectActedOn(r,r),
    actionset_toLocation(r,r),
    actionset_fromLocation(r,r),
    action_objectActedOn(r,r),
    action_toLocation(r,r),
    action_fromLocation(r,r),
    objectset_propVal(r,r,r),
    object_propVal(r,r,r).


% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% Helper Functions
%
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 


%% clean_projection_cache (see also: knowrob_actions/prolog/action_effects.pl)
%
% remove all triples and object instances
% that have been asserted as part of the projection methods

clean_projection_cache :-
  rdf_retractall(_, _, _, knowrob_projection),
  rdf_retractall(_, knowrob_projection, _).


%% create_action_inst
%
% creates an ActionInst with the given ObjActOns, To- and FromLocations
%
% @param Action       Type of Action to be created
% @param ObjActOnSet  List of Objects/ObjectInstances to be objectsActedOn
% @param ToLocSet     List of Locations/LocationInstances to be toLocations
% @param FromLocSet   List of Locations/LocationInstances to be fromLocations
% @param ActionInst   created ActionInstance 

create_action_inst(Action, ObjActOnSet, ToLocSet, FromLocSet, ActionInst) :-
  rdf_instance_from_class(Action, knowrob_projection, ActionInst),
  forall(member(ObjType, ObjActOnSet),
    ((owl_individual_of(ObjType, owl:'Class') ->
      rdf_instance_from_class(ObjType, knowrob_projection, ObjActOn);
      (ObjActOn = ObjType)),
    rdf_assert(ActionInst, knowrob:'objectActedOn', ObjActOn, knowrob_projection))),
  forall(member(ToLocType, ToLocSet),
    ((owl_individual_of(ToLocType, owl:'Class') ->
      rdf_instance_from_class(ToLocType, knowrob_projection, ToLoc);
      (ToLoc = ToLocType)),
    rdf_assert(ActionInst, knowrob:'toLocation', ToLoc, knowrob_projection))),
  forall(member(FromLocType, FromLocSet),
    ((owl_individual_of(FromLocType, owl:'Class') ->
      rdf_instance_from_class(FromLocType, knowrob_projection, FromLoc);
      (FromLoc = FromLocType)),
    rdf_assert(ActionInst, knowrob:'fromLocation', FromLoc, knowrob_projection))).



%% getAction_objActedOn
%
% checks which Action's TBOX descriptions
% include the given Object as an objectActedOn (direct or inherited)

getAction_objectActedOn(Action, Object) :-
        owl_subclass_of(Action, knowrob:'Event'),
        owl_subclass_of(Action, Sup),
        owl_restriction(Sup,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#objectActedOn', some_values_from(SupObject))),
        owl_subclass_of(Object, SupObject).


%% getObject_objActedOn
%
% checks which Objects are definded as objectsActedOn
% in the given Action's TBOX description
%
% Note: if Action defines a more specific objectActedOn than the one inherited by its Superclass
% only such is returned

getObject_objectActedOn(Action, Object) :-
  owl_subclass_of(Action, knowrob:'Event'),
  findall(Obj,
    (owl_subclass_of(Action, Sup),
    owl_restriction(Sup,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#objectActedOn', some_values_from(Obj)))
    ), ObjSet),
  findall(SubObj,
    (member(SubObj, ObjSet), member(A, ObjSet), SubObj\=A,
    \+ owl_subclass_of(A, SubObj)
    ), SubObjSet),
  member(SubObj, SubObjSet),
  owl_subclass_of(Object, SubObj).


%% reduce_set_bySuperClass
%
% returns only those classes of a set
% who are of type SuperClass
reduce_set_bySuperClass(Set, SuperClass, NewSet) :-
  setof(Indiv, (member(Indiv, Set),
    owl_subclass_of(Indiv, SuperClass)), NewSet).



%% actionset_objectActedOn
%
% finds all action descriptions,
% that may feature ObjInst as an objectActedOn
actionset_objectActedOn(ObjInst, ActionSet) :-
  setof(Action, ObjActOnType^(
    owl_individual_of(ObjInst, ObjActOnType),
    action_objectActedOn(Action, ObjActOnType)), SuperActionSet), 
  % include actions which only inherit objectActedOn
  setof(Action, SuperAction^(member(SuperAction, SuperActionSet),
    owl_subclass_of(Action, SuperAction)), ActionSet).

%% action_objActedOn (see also: knowrob_actions/prolog/knowrob_actions.pl)
%
% checks if the TBOX description of Action includes Object as an objectActedOn
action_objectActedOn(Action, Object) :-
        owl_subclass_of(Action, knowrob:'Action'),
        owl_direct_subclass_of(Action, Sup),
        owl_direct_subclass_of(Sup, Sup2),
        owl_restriction(Sup2,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#objectActedOn', some_values_from(Object))).
action_objectActedOn(Action, Object) :-
        owl_subclass_of(Action, knowrob:'Action'),
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
        owl_subclass_of(Action, knowrob:'Translocation'),
        owl_direct_subclass_of(Action, Sup),
        owl_direct_subclass_of(Sup, Sup2),
        owl_restriction(Sup2,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#toLocation', some_values_from(Object))).
action_toLocation(Action, Object) :-
        owl_subclass_of(Action, knowrob:'Translocation'),
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
        owl_subclass_of(Action, knowrob:'Movement-TranslationEvent'),
        owl_direct_subclass_of(Action, Sup),
        owl_direct_subclass_of(Sup, Sup2),
        owl_restriction(Sup2,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#fromLocation', some_values_from(Object))).
action_fromLocation(Action, Object) :-
        owl_subclass_of(Action, knowrob:'Movement-TranslationEvent'),
        owl_direct_subclass_of(Action, Sup),
        owl_restriction(Sup,restriction('http://ias.cs.tum.edu/kb/knowrob.owl#fromLocation', some_values_from(Object))).



%% objectset_propVal
%
% finds all object descriptions,
% that feature the given Property with given Value
objectset_propVal(Property, Value, ObjectSet) :-
  setof(Object, object_propVal(Object, Property, Value), ObjectSet).

%% object_propVal
%
% checks if the TBOX description of Object includes Property with Value
object_propVal(Object, Property, Value) :-
        owl_subclass_of(Object, knowrob:'EnduringThing-Localized'),
        owl_direct_subclass_of(Object, Sup),
        owl_direct_subclass_of(Sup, Sup2),
        owl_restriction(Sup2, restriction(Property,has_value(Value))).
object_propVal(Object, Property, Value) :-
        owl_subclass_of(Object, knowrob:'EnduringThing-Localized'),
        owl_direct_subclass_of(Object, Sup),
        owl_restriction(Sup,restriction(Property,has_value(Value))).



% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% Querying for Actions which induce certain changes to the world
%
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 


%% find_cause_of_stateChange
%
% finds actions that may be responsible for the change 
% of an objects property value
%
% @param ObjInst    Object which undergoes the state change
% @param Prop       Property that changes
% @param FromValue  Previous property value (optional)
% @param ToValue    New property value
% @param ResultSet  Set of known actions which are able to induce this change

find_cause_of_stateChange(ObjInst, Prop, FromValue, ToValue, ResultSet) :-
  owl_has(ObjInst, Prop, FromValue),
  find_cause_of_stateChange(ObjInst, Prop, ToValue, ResultSet).
  
find_cause_of_stateChange(ObjInst, Prop, ToValue, ResultSet) :-
  % findall actions which are IntrinsicStateChangeEvents and may act on ObjInst
  setof(Action, Object^(
    owl_subclass_of(Action, knowrob:'IntrinsicStateChangeEvent'),
    owl_individual_of(ObjInst, Object),
    getAction_objectActedOn(Action, Object)), ActionSet),
  findall(Action,
    (member(Action, ActionSet),
    test_projection_for_stateChange(Action, ObjInst, Prop, ToValue)
  ), ResultSet),
  clean_projection_cache.

% tests wether performing Action with Object as objectActedOn
% results in Property changing to Value 
test_projection_for_stateChange(Action, Object, Property, Value) :-
  append([Object], [], ObjList),
  create_action_inst(Action, ObjList, [], [], ActInst),
  project_action_effects(ActInst),
  owl_has(Object, Property, Value).





%% find_causing_action: Adding something new to an object/container/etc.
%
% The Object Instance is known, but a new property was added to it
% (something was put onto/into the object)
%
% @param Obj        Known object which something is added to 
% @param Relation   Relation between known and new object (eg. contains)
% @param addedObj   newly added Object 
% @param PairResSet Set of known actions which are able to induce this change,
%                   with corresponding objectActedOn

find_causing_action(Obj, Relation, AddedObj, PairResSet) :-
  actionset_toLocation(Obj, ActionSet), !,
  setof(Pair, SourceObjType^Action^AddedObjType^ObjectSet^ResultSet^(
  owl_has(AddedObj, rdf:type, AddedObjType), (AddedObjType\='http://www.w3.org/2002/07/owl#NamedIndividual'),
  % SourceObjType either AddedObjType or Obj containing AddedObjType
  (SourceObjType = AddedObjType;
  objectset_propVal(knowrob:'contains', AddedObjType, ObjectSet), member(SourceObjType, ObjectSet)),
% gives instance in World containing Orange_Juice
% owl_has(SourceObjType, knowrob:'contains', AddedObjType)),
  % try project action effects
  actionset_projection_success(ActionSet, SourceObjType, Obj, Relation, AddedObj, ResultSet),
  (ResultSet\=[]),(member(Action, ResultSet); Action=ResultSet),
  pairs_keys_values([Pair], [Action], [SourceObjType])
  ), PairResSet).
  

%% actionset_projection_success 
%
% TODO: Description
%
% @param ActionSet      Set of actions to be tested 
% @param ObjActedOnType Class of the object the action acts on
% @param ToLocation	The actions ToLocation
% @param Relation       Relation between ToLocation and ObjActedOn (eg. contains)
% @param ObjOfComp      Object of the same class as the new object produced by action
% @param ResultSet	Set of actions generating the desired result  
% TODO: use findall
actionset_projection_success([], _, _, _, _, []).
actionset_projection_success(ActionSet, ObjActedOnType, ToLocation, Relation, ObjOfComp, ResultSet) :-
  append([Head], Tail, ActionSet),
  actionset_projection_success(Tail, ObjActedOnType, ToLocation, Relation, ObjOfComp, RS), 
  % if: action projection results in correct changes
  (test_projection(Head, ObjActedOnType, ToLocation, Relation, ObjOfComp) ->
    % then: add to ResultSet and clean cache
    append(RS, Head, ResultSet), clean_projection_cache;
    % else: clean cache
    clean_projection_cache, ResultSet = RS).

   
%% test_projection 
%
% TODO: Description
%
% @param Action     Action whose results are tested 
% @param ...        (refer to actionset_projection_success/6)
test_projection(Action, ObjActedOnType, ToLocation, Relation, ObjOfComp) :-
  % create possible SourceObject
  rdf_instance_from_class(ObjActedOnType, knowrob_projection, ObjActedOn),
  rdf_instance_from_class(Action, knowrob_projection, ActionInst),
  rdf_assert(ActionInst, knowrob:'objectActedOn', ObjActedOn, knowrob_projection),
  rdf_assert(ActionInst, knowrob:'toLocation', ToLocation, knowrob_projection),
  project_action_effects(ActionInst),!,
  % check wether new object related to ToLocation is of the same type as ObjOfComp
  owl_has(ToLocation, Relation, NewObj), owl_has(NewObj, rdf:type, Type),
  (Type\='http://www.w3.org/2002/07/owl#NamedIndividual'), owl_has(ObjOfComp, rdf:type, Type).



%% find_causing_action: Something new appears/was created
%
% TODO: Description
%
% shortcomings: some Actions TBox-Descriptions include outputsCreated property
%   others only add this property via the project_action_effects(Action) predicate
% here: only actions are found for whom the project_action_effects predicate is definded
%
% @param Obj  new Object

find_causing_action(Obj, PairResSet) :-
  % TODO: default: movingSomethingSomewhere
  % all actions where outputs are created
  setof(Action, owl_subclass_of(Action, knowrob:'CreationEvent'), ActionSet),
  % TODO: eliminate Descriptions
  % project effects of all actions
  actionset_projection_success(ActionSet, Obj, PairResSet),
  clean_projection_cache.


%% actionset_projection_success 
%
% TODO: Description
%
% @param ActionSet      Set of actions to be tested 
% @param ObjOfComp	
actionset_projection_success([], _, []).
actionset_projection_success(ActionSet, ObjOfComp, PairResultSet) :-
  print(ActionSet),
  findall(Pair, (pairs_keys_values([Pair],[Action],[ObjActOnType]),
    member(Action, ActionSet),
    % test ObjOfComp as ObjActedOn
    ((owl_has(ObjOfComp, rdf:type, ObjActOnType),
      (ObjActOnType\='http://www.w3.org/2002/07/owl#NamedIndividual'),
      test_projection(Action, [ObjActOnType], ObjOfComp));
    % try determining objActedOn in correspondance to Action
    (findall(ObjType, action_objectActedOn(Action, ObjType), ObjActOnTypeList),
      test_projection(Action, ObjActOnTypeList, ObjOfComp),
      member(ObjActOnType, ObjActOnTypeList)))
    ), PairResultSet).
   

%% test_projection 
%
% TODO: Description
%
% @param Action     Action whose results are tested 
% @param ...        (refer to actionset_projection_success/6)
test_projection(Action, ObjActedOnSet, ObjOfComp) :-
  % create all possible ObjActedOn
  rdf_instance_from_class(Action, knowrob_projection, ActionInst),
  forall(member(ObjType, ObjActedOnSet),
    (rdf_instance_from_class(ObjType, knowrob_projection, ObjActedOn),
    rdf_assert(ActionInst, 'http://ias.cs.tum.edu/kb/knowrob.owl#objectActedOn', ObjActedOn, knowrob_projection))),
  project_action_effects(ActionInst),
  % check wether any of new objects created is of the same type as ObjOfComp
  owl_has(ActionInst, knowrob:'outputsCreated', NewObj), owl_has(NewObj, rdf:type, Type),
  (Type\='http://www.w3.org/2002/07/owl#NamedIndividual'), owl_has(ObjOfComp, rdf:type, Type).


   
%% find_cause_of_disappearance
%
% finds actions that may be responsible for the disappearance of an object
%
% @param ObjInst    Instance of Object which was observed previously
% @param ResultSet  Set of actions possibly causing this change

find_cause_of_disappearance(ObjInst, ResultSet) :- 
  % findall actions which are DestructionEvents and may act on ObjInstance
  setof(Action, Object^(
    owl_subclass_of(Action, knowrob:'DestructionEvent'),
    owl_individual_of(ObjInst, Object),
    getAction_objectActedOn(Action, Object)), ActionSet),
  findall(Action, 
    (member(Action, ActionSet),
    test_projection_for_disappearance(Action, ObjInst)
  ), ResultSet),
  clean_projection_cache.
  %TODO: add Movement as possible cause

% tests wether performing Action with Object as objectActedOn
% results in Object being destroyed
test_projection_for_disappearance(Action, Object) :-
  append([Object], [], ObjList),
  create_action_inst(Action, ObjList, [], [], ActInst),
  project_action_effects(ActInst),
  owl_has(ActInst, knowrob:'inputsDestroyed', Object).



%% find_cause_of_disappearance (from an objects surface/container etc.) 
%
% finds actions that may be responsible for the disappearance 
% of parts of an object (eg. content of a container)
%
% @param ObjInst     Known object of which some part disappeared
% @param Relation    Relation the disappeared part had to the object (eg. knowrob:'contains')
% @param ObjDisInst  Object which disappeared
% @param ResultSet   Set of actions possibly causing this change

find_cause_of_disappearance(ObjInst, Relation, ObjDisInst, ResultSet) :-
  % check wether Relation is present in knowledgebase
  owl_has(ObjInst, Relation, ObjDisInst),
  % findall actions which are DestructionEvents and may act on ObjInst
  setof(Action, Object^(
    owl_subclass_of(Action, knowrob:'DestructionEvent'),
    owl_individual_of(ObjInst, Object),
    getAction_objectActedOn(Action, Object)), ActionSet),
  findall(Action,
    (member(Action, ActionSet),
    test_projection_for_disappearance(Action, ObjInst, Relation, ObjDisInst)
  ), ResultSet),
  clean_projection_cache.

% tests wether performing Action with Object as objectActedOn
% results in DestructObject being destroyed
test_projection_for_disappearance(Action, Object, Relation, DestructObj) :-
  append([Object], [], ObjList),
  create_action_inst(Action, ObjList, [], [], ActInst),
  project_action_effects(ActInst),
  owl_has(ActInst, knowrob:'inputsDestroyed', DestructObj),
  \+ owl_has(Object, Relation, DestructObj).


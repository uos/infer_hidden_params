:- module(validate_cause,
    [
      find_action_inst/5,
      find_latest_action_inst/5,
      object_available/2,
      object_possibly_available/2
    ]).

:- use_module(library('comp_temporal')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs_computable')).
:- use_module(library('knowrob_owl')).

:- rdf_db:rdf_register_ns(knowrob,'http://ias.cs.tum.edu/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#', [keep(true)]).
:- rdf_db:rdf_register_ns(owl, 'http://www.w3.org/2002/07/owl#', [keep(true)]).

:- rdf_meta
    find_action_inst(r,r,r,r,?),
    find_latest_action_inst(r,r,r,r,?),
    object_available(r,?),
    object_possibly_available(r,?).



%% find_action_inst
%
% finds existing ActionInst with the given ObjActOns, To- and FromLocations
%
% @param Action       Type of Action to be found 
% @param ObjActOnSet  List of Objects/ObjectInstances being objectsActedOn
% @param ToLocSet     List of Locations/LocationInstances being toLocations
% @param FromLocSet   List of Locations/LocationInstances being fromLocations
% @param ActionInst   found ActionInstance 

find_action_inst(Action, ObjActOnSet, ToLocSet, FromLocSet, ActionInst) :-
  owl_individual_of(ActionInst, Action),
  forall(member(Obj, ObjActOnSet),
    (owl_has(ActionInst, knowrob:'objectActedOn', Obj);
     (owl_has(ActionInst, knowrob:'objectActedOn', ObjInst), owl_individual_of(ObjInst, Obj)))),
  forall(member(ToLoc, ToLocSet),
    (owl_has(ActionInst, knowrob:'toLocation', ToLoc);
     (owl_has(ActionInst, knowrob:'toLoctaion', ToLocInst), owl_individual_of(ToLocInst, ToLoc)))),
  forall(member(FromLoc, FromLocSet),
    (owl_has(ActionInst, knowrob:'toLocation', FromLoc);
     (owl_has(ActionInst, knowrob:'toLoctaion', FromLocInst), owl_individual_of(FromLocInst, FromLoc)))).

find_latest_action_inst(Action, ObjActOnSet, ToLocSet, FromLocSet, ActionInst) :-
  findall(ActInst, find_action_inst(Action, ObjActOnSet, ToLocSet, FromLocSet, ActInst), ActList),
  member(ActionInst, ActList),
  owl_has(ActionInst, knowrob:startTime, After),
  forall(member(Act, ActList), (Act = ActionInst;
    (owl_has(Act, knowrob:startTime, Pre), comp_after(Pre, After)))).

%% object_available
%
% checks wether an Object of a certain Class is available
%
% @param ObjClass  Object Class to be searched for
% @param ObjInst   found Object Instance

object_available(ObjClass, ObjInst) :-
  owl_individual_of(ObjInst, ObjClass). 


%% object_possibly_available
%
% checks wether an Object of more general class than the one given is available 
% which might turn out to be of the given class if taking a closer look
%
% @param ObjClass  Object Class to be searched for
% @param ObjInst   found Object Instance

object_possibly_available(ObjClass, ObjInst) :-
  owl_subclass_of(ObjClass, Sup),
  (Sup\='http://www.w3.org/2002/07/owl#Thing'),
  owl_has(ObjInst, rdf:type, Sup).

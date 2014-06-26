
:- module(create_individual,
    [
      create_individual/1,
      create_individual/3
    ]).

% :- use_module(library('action_effects')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs_computable')).
:- use_module(library('knowrob_owl')).

:- rdf_db:rdf_register_ns(knowrob, 'http://ias.cs.tum.edu/kb/knowrob.owl#', [keep(true)]).

:- rdf_meta
  create_individual(r),
  create_individual(r,r,r),
  add_property(r,r,r).


% % % % % % % % % % % % % % % % % % % %
% Create Individual
% Creates Individual of ObjClass with zero to more properties 
%
% @param ObjClass   Class of Object to be created
% @param Property   List of Properties
% @param PropValue  List of corresponding Property Values 
%
create_individual(ObjClass) :-
  rdf_instance_from_class(ObjClass, ObjInst),
  print('created '), print(ObjInst), print('\n').

create_individual(ObjClass, Property, PropValue) :-
  rdf_instance_from_class(ObjClass, ObjInst),
  print('created '), print(ObjInst), print('\n'),
  add_property(ObjInst, Property, PropValue).

add_property(_, [], []).
add_property(ObjInst, Property, PropValue) :-
  append([PropHead], PropTail, Property),
  append([ValHead], ValTail, PropValue),
  add_property(ObjInst, PropTail, ValTail),
  rdf_assert(ObjInst, PropHead, ValHead),
  print('added '), print(PropHead), print(': '), print(ValHead), print('\n'). 
%TODO: check wether properties of Individual fit Class properties 


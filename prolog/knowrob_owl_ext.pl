
:- module(knowrob_owl_ext,
    [
      subclass_by_prop_val/4
    ]).

:- use_module(library('knowrob_owl')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs_computable')).
:- use_module(library('knowrob_owl')).

:- rdf_db:rdf_register_ns(knowrob, 'http://ias.cs.tum.edu/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#', [keep(true)]).
:- rdf_db:rdf_register_ns(owl, 'http://www.w3.org/2002/07/owl#', [keep(true)]).

:- rdf_meta
    subclass_by_prop_val(r,r,r,-).


%% subclass_by_prop_val
%
subclass_by_prop_val(SuperClass, Property, Value, Class) :-
  owl_subclass_of(Class, SuperClass),
  owl_subclass_of(Class, Sup),
  owl_restriction(Sup, restriction(Property, has_value(Value))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dependencies 

:- register_ros_package(ias_knowledge_base).
:- register_ros_package(infer_hidden_params).

:- use_module(library('action_effects_ext')).
:- use_module(library('create_individual')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse OWL files, register name spaces

:- owl_parser:owl_parse('/home/student/a/ahaeuser/ros-hydro/dry/stacks/knowrob_own/infer_hidden_params/owl/cheese_world.owl', false, false, true).
:- owl_parser:owl_parse('/home/student/a/ahaeuser/ros-hydro/dry/stacks/knowrob_own/infer_hidden_params/owl/drink_world.owl', false, false, true).
:- owl_parser:owl_parse('/home/student/a/ahaeuser/ros-hydro/dry/stacks/knowrob_own/infer_hidden_params/owl/door_map.owl', false, false, true).

:- rdf_db:rdf_register_ns(cheese_world, 'http://infer_hidden_params/cheese_world.owl#',     [keep(true)]).
:- rdf_db:rdf_register_ns(cheese_world, 'http://infer_hidden_params/drink_world.owl#',     [keep(true)]).
:- rdf_db:rdf_register_ns(door_map, 'http://infer_hidden_params/door_map.owl#',     [keep(true)]).


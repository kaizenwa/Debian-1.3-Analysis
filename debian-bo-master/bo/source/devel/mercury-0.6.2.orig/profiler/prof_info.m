%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% prof_info.m

% Main author: petdr.
%
% Declare the main data structures for mercury__profile and their access
% predicates, the actual types are exported as well.  This is because some
% predicates need to access entire data structure.
% XXX Should maybe changed at a later date.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module prof_info.

:- interface.

:- import_module list, map, require, std_util.

%-----------------------------------------------------------------------------%


	% prof: Data structure which contains ALL the relevant info for use
	%	in generating the output.
:- type prof.

	% addrdecl = map(label_name, label_address)
:- type addrdecl	==	map(string, int).

	% prof_node_map = map(label_address, prof_node)
:- type prof_node_map	==	map(int, prof_node).

	% cycle_map == map(predicate_name, cycle it is in).
:- type cycle_map	==	map(string, int).

	% prof_node: Contains all the info used for output, for a single pred.
:- type prof_node.

:- type pred_info.

:- type prof_node_type 
		--->	predicate
		;	cycle.


%-----------------------------------------------------------------------------%


	% *** Get prof_node from via predicate name *** %

:- pred get_prof_node(string, addrdecl, prof_node_map, prof_node).
:- mode get_prof_node(in, in, in, out) is det.

:- pred update_prof_node(string,prof_node,addrdecl,prof_node_map,prof_node_map).
:- mode update_prof_node(in, in, in, in, out) is det.


	% *** Initialise prof predicates *** %

:- pred prof_node_init(string, prof_node).
:- mode prof_node_init(in, out) is det.

:- pred prof_node__init_cycle(string, int, int, float, list(pred_info), int, 
							int, prof_node).
:- mode prof_node__init_cycle(in, in, in, in, in, in, in, out) is det.


	% *** Access prof predicates *** %

:- pred prof__get_entire(prof, int,int,int, addrdecl, prof_node_map, cycle_map).
:- mode prof__get_entire(in, out, out, out, out, out, out) is det.

:- pred prof_get_addrdeclmap(prof, addrdecl).
:- mode prof_get_addrdeclmap(in, out) is det.

:- pred prof_get_profnodemap(prof, prof_node_map).
:- mode prof_get_profnodemap(in, out) is det.


	% *** Update prof predicates *** %

:- pred prof__set_entire(int,int,int, addrdecl, prof_node_map, cycle_map, prof).
:- mode prof__set_entire(in, in, in, in, in, in, out) is det.

:- pred prof_set_profnodemap(prof_node_map, prof, prof).
:- mode prof_set_profnodemap(in, in, out) is det.

:- pred prof_set_cyclemap(cycle_map, prof, prof).
:- mode prof_set_cyclemap(in, in, out) is det.


	% *** Special prof_node predicates *** %

:- pred prof_node__type(prof_node, prof_node_type).
:- mode prof_node__type(in, out) is det.


	% *** Access Predicate for prof_node *** %

:- pred prof_node__get_entire_pred(prof_node, string, int, int, float,
					list(pred_info), list(pred_info),
					int, int, list(string)).
:- mode prof_node__get_entire_pred(in, out, out, out, out, out, out, out, out, 
								out) is det.

:- pred prof_node__get_entire_cycle(prof_node, string, int, int, float,
					list(pred_info), int, int).
:- mode prof_node__get_entire_cycle(in,out,out,out,out,out,out,out) is det.

:- pred prof_node_get_pred_name(prof_node, string).
:- mode prof_node_get_pred_name(in, out) is det.

:- pred prof_node_get_cycle_number(prof_node, int).
:- mode prof_node_get_cycle_number(in, out) is det.

:- pred prof_node_get_initial_counts(prof_node, int).
:- mode prof_node_get_initial_counts(in, out) is det.

:- pred prof_node_get_propagated_counts(prof_node, float).
:- mode prof_node_get_propagated_counts(in, out) is det.

:- pred prof_node_get_parent_list(prof_node, list(pred_info)).
:- mode prof_node_get_parent_list(in, out) is det.

:- pred prof_node_get_child_list(prof_node, list(pred_info)).
:- mode prof_node_get_child_list(in, out) is det.

:- pred prof_node_get_total_calls(prof_node, int).
:- mode prof_node_get_total_calls(in, out) is det.

:- pred prof_node_get_self_calls(prof_node, int).
:- mode prof_node_get_self_calls(in, out) is det.


	% *** Update prof_node predicates *** %

:- pred prof_node__set_cycle_num(int, prof_node, prof_node).
:- mode prof_node__set_cycle_num(in, in, out) is det.

:- pred prof_node_set_initial_counts(int, prof_node, prof_node).
:- mode prof_node_set_initial_counts(in, in, out) is det.

:- pred prof_node_set_propagated_counts(float, prof_node, prof_node).
:- mode prof_node_set_propagated_counts(in, in, out) is det.

:- pred prof_node_concat_to_parent(string, int, prof_node, prof_node).
:- mode prof_node_concat_to_parent(in, in, in, out) is det.

:- pred prof_node_concat_to_child(string, int, prof_node, prof_node).
:- mode prof_node_concat_to_child(in, in, in, out) is det.

:- pred prof_node_set_total_calls(int, prof_node, prof_node).
:- mode prof_node_set_total_calls(in, in, out) is det.

:- pred prof_node_set_self_calls(int, prof_node, prof_node).
:- mode prof_node_set_self_calls(in, in, out) is det.

:- pred prof_node_concat_to_name_list(string, prof_node, prof_node).
:- mode prof_node_concat_to_name_list(in, in, out) is det.

:- pred prof_node__concat_to_member(string, int, prof_node, prof_node).
:- mode prof_node__concat_to_member(in, in, in, out) is det.


	% *** Init  predicates for pred_info *** %

:- pred pred_info__init(string, int, pred_info).
:- mode pred_info__init(in, in, out) is det.


	% *** Access predicates for pred_info *** %

:- pred pred_info__get_entire(pred_info, string, int).
:- mode pred_info__get_entire(in, out, out) is det.

:- pred pred_info_get_pred_name(pred_info, string).
:- mode pred_info_get_pred_name(in, out) is det.

:- pred pred_info_get_counts(pred_info, int).
:- mode pred_info_get_counts(in, out) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- implementation.


:- type prof --->
		prof(
			int,			% Hertz of the system clock
			int,			% No. of clock ticks between 
						% each prof signal.
			int,			% Total counts of the profile
						% run
			addrdecl,		% Map between label name and
						% label addr used to find key
						% to look up prof_node_map.
			prof_node_map,		% Map between label addresses
						% and all the relevant data 
						% about that predicate
			cycle_map		% Map between predicate name
						% and it's cycle number.
		).


:- type prof_node
	 ---> 
			% A node which consists of just one predicate.
		pred_node(
			string, 		% current predicate (label)
			int,			% cycle number
			int, 			% self counts
			float, 			% propagated counts
			list(pred_info), 	% Parent pred and the number
						% of times it calls this
						% predicate
			list(pred_info),	% Child pred and the number of
						% times they are called from
						% this predicate.
			int,			% total count of times this 
						% predicate called.
			int,			% Number of self recursive 
						% calls of this routine
			list(string)		% Alternative names for this
						% predicate eg. Labels with
						% different names but the same
						% address.
		)
	 ;
			% A node which is built up with more then one predicate
			% and is a cycle.
		cycle_node(
			string, 		% cycle name
			int,			% cycle number
			int, 			% self counts
			float, 			% propagated counts
			list(pred_info), 	% Cycle members plus total calls
						% to that predicated
			int,			% total count of times this 
						% predicate called.
			int			% Number of calls to fellow
						% cycle members
		).

:- type pred_info --->
		pred_info(
			string,			% predicate (label)     
			int			% count     (to or from)
		).

:- type junk		==	unit.


%-----------------------------------------------------------------------------%


% get_prof_node:
%  	Gets the prof_node given a label name.
%
get_prof_node(Pred, AddrMap, ProfNodeMap, ProfNode) :-
	map__lookup(AddrMap, Pred, Key),
	map__lookup(ProfNodeMap, Key, ProfNode).


update_prof_node(Pred, ProfNode, AddrMap, ProfNodeMap0, ProfNodeMap) :-
	map__lookup(AddrMap, Pred, Key),
	map__det_update(ProfNodeMap0, Key, ProfNode, ProfNodeMap).


%-----------------------------------------------------------------------------%


% *** Access prof predicates *** %

prof__get_entire(prof(A, B, C, D, E, F), A, B, C, D, E, F).

prof_get_addrdeclmap(prof(_, _, _, AddrDeclMap, _, _), AddrDeclMap).

prof_get_profnodemap(prof(_, _, _, _, ProfNodeMap, _), ProfNodeMap).


%-----------------------------------------------------------------------------%


% *** Update prof predicates *** %

prof__set_entire(A, B, C, D, E, F, prof(A, B, C, D, E, F)).

prof_set_profnodemap(ProfNodeMap, prof(A, B, C, D, _, F), 
					prof(A, B, C, D, ProfNodeMap, F)).

prof_set_cyclemap(CycleMap, prof(A, B, C, D, E, _), 
					prof(A, B, C, D, E, CycleMap)).


%-----------------------------------------------------------------------------%


% *** Initialise predicates *** %

prof_node_init(PredName, pred_node(PredName, 0, 0, 0.0, [], [], 0, 0, [])).

prof_node__init_cycle(A, B, C, D, E, F, G, cycle_node(A, B, C, D, E, F, G)).


%-----------------------------------------------------------------------------%


% *** Special prof_node predicates *** %

prof_node__type(pred_node(_,_,_,_,_,_,_,_,_), predicate).
prof_node__type(cycle_node(_,_,_,_,_,_,_), cycle).


%-----------------------------------------------------------------------------%


% *** Access prof_node predicates *** %

prof_node__get_entire_pred(pred_node(A,B,C,D,E,F,G,H,I),A,B,C,D,E,F,G,H,I).
prof_node__get_entire_pred(cycle_node(_,_,_,_,_,_,_),_,_,_,_,_,_,_,_,_) :-
	error("prof_node__get_entire_pred: not a pred\n").

prof_node__get_entire_cycle(cycle_node(A,B,C,D,E,F,G),A,B,C,D,E,F,G).
prof_node__get_entire_cycle(pred_node(_,_,_,_,_,_,_,_,_),_,_,_,_,_,_,_) :-
	error("prof_node__get_entire_cycle: not a cycle\n").

prof_node_get_pred_name(pred_node(Name, _, _, _, _, _, _, _, _), Name).
prof_node_get_pred_name(cycle_node(Name, _, _, _, _, _, _), Name).

prof_node_get_cycle_number(pred_node(_, Cycle, _, _, _, _, _, _, _), Cycle).
prof_node_get_cycle_number(cycle_node(_, Cycle, _, _, _, _, _), Cycle).

prof_node_get_initial_counts(pred_node(_, _, Count, _, _, _, _, _, _), Count).
prof_node_get_initial_counts(cycle_node(_, _, Count, _, _, _, _), Count).

prof_node_get_propagated_counts(pred_node(_, _, _, Count, _, _, _, _, _), 
									Count).
prof_node_get_propagated_counts(cycle_node(_, _, _, Count, _, _, _), Count).

prof_node_get_parent_list(pred_node(_, _, _, _, PList, _, _, _, _), PList).
prof_node_get_parent_list(cycle_node(_, _, _, _, _, _, _), _) :-
	error("prof_node_get_parent_list: cycle_node has no parent list\n").

prof_node_get_child_list(pred_node(_, _, _, _, _, Clist, _, _, _), Clist).
prof_node_get_child_list(cycle_node(_, _, _, _, _, _, _), _) :-
	error("prof_node_get_child_list: cycle_node has no child list\n").

prof_node_get_total_calls(pred_node(_, _, _, _, _, _, Calls, _, _), Calls).
prof_node_get_total_calls(cycle_node(_, _, _, _, _, Calls, _), Calls).

prof_node_get_self_calls(pred_node(_, _, _, _, _, _, _, Calls, _), Calls).
prof_node_get_self_calls(cycle_node(_, _, _, _, _, _, Calls), Calls).


%-----------------------------------------------------------------------------%


% *** Update prof_node predicates *** %

prof_node__set_cycle_num(Cycle, pred_node(A, _, C, D, E, F, G, H, I), 
				pred_node(A, Cycle, C, D, E, F, G, H, I)).
prof_node__set_cycle_num(Cycle, cycle_node(A, _, C, D, E, F, G), 
				cycle_node(A, Cycle, C, D, E, F, G)).

prof_node_set_initial_counts(Count, pred_node(A, B, _, D, E, F, G, H, I), 
				pred_node(A, B, Count, D, E, F, G, H, I)).
prof_node_set_initial_counts(Count, cycle_node(A, B, _, D, E, F, G), 
				cycle_node(A, B, Count, D, E, F, G)).

prof_node_set_propagated_counts(Count, pred_node(A, B, C, _, E, F, G, H, I),
				 pred_node(A, B, C, Count, E, F, G, H, I)).
prof_node_set_propagated_counts(Count, cycle_node(A, B, C, _, E, F, G),
				 cycle_node(A, B, C, Count, E, F, G)).

prof_node_concat_to_parent(Name,Count, pred_node(A, B, C, D, PList, F, G, H, I),
	pred_node(A, B, C, D, [pred_info(Name,Count) | PList], F, G, H, I)).
prof_node_concat_to_parent(_, _, cycle_node(_, _, _, _, _, _, _), _) :-
	error("prof_node_concat_to_parent: cycle_node has no parents\n").

prof_node_concat_to_child(Name, Count, pred_node(A, B, C, D, E, CList, G, H, I),
	pred_node(A, B, C, D, E, [pred_info(Name,Count) | CList], G, H, I)).
prof_node_concat_to_child(_, _, cycle_node(_, _, _, _, _, _, _), _) :-
	error("prof_node_concat_to_child: cycle_node has no child\n").

prof_node_set_total_calls(Calls, pred_node(A, B, C, D, E, F, _, H, I),
				pred_node(A, B, C, D, E, F, Calls, H, I)).
prof_node_set_total_calls(Calls, cycle_node(A, B, C, D, E, _, G),
				cycle_node(A, B, C, D, E, Calls, G)).

prof_node_set_self_calls(Calls, pred_node(A, B, C, D, E, F, G, _, I),
				pred_node(A, B, C, D, E, F, G, Calls, I)).
prof_node_set_self_calls(Calls, cycle_node(A, B, C, D, E, F, _),
				cycle_node(A, B, C, D, E, F, Calls)).

prof_node_concat_to_name_list(Name, pred_node(A, B, C, D, E, F, G, H, NL), 
			pred_node(A, B, C, D, E, F, G, H, [Name | NL])).
prof_node_concat_to_name_list(_, cycle_node(_, _, _, _, _, _, _), _) :-
	error("prof_node_concat_to_name_list: cycle_node has no namelist\n").

prof_node__concat_to_member(Name, Count, cycle_node(A, B, C, D, CList, F, G),
		cycle_node(A, B, C, D, [pred_info(Name,Count) | CList], F, G)).
prof_node__concat_to_member(_, _, pred_node(_, _, _, _, _, _, _, _, _), _) :-
		error("prof_node_concat_to_member: pred_node has no members\n").


%-----------------------------------------------------------------------------%


% *** Init predicates for pred_info *** %

pred_info__init(Name, Count, pred_info(Name, Count)).


%-----------------------------------------------------------------------------%


% *** Access predicates for pred_info *** %

pred_info__get_entire(pred_info(A, B), A, B).

pred_info_get_pred_name(pred_info(Name, _), Name).

pred_info_get_counts(pred_info(_, Count), Count).


%-----------------------------------------------------------------------------%

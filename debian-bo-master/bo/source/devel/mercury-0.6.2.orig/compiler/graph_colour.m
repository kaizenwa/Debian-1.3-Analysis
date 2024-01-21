%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: graph_colour.m
% main author: conway.
%
% This file contains functionality to find a 'good' colouring of a graph.
% The predicate graph_colour__group_elements(set(set(T)), set(set(T))),
% takes a set of sets each containing elements that touch, and returns
% a set of sets each containing elements that can be assigned the same
% colour, ensuring that touching elements have different colours.
% ("Good" means using as few colours as possible.)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module graph_colour.

:- interface.

:- import_module set.

:- pred graph_colour__group_elements(set(set(T)), set(set(T))).
:- mode graph_colour__group_elements(in, out) is det.

:- implementation.

:- import_module list, require.

graph_colour__group_elements(Constraints, Colours) :-
	set__power_union(Constraints, AllVars),
	set__init(EmptySet),
	set__delete(Constraints, EmptySet, Constraints1),
	set__to_sorted_list(Constraints1, ConstraintList),
	graph_colour__find_all_colours(ConstraintList, AllVars, ColourList),
	set__list_to_set(ColourList, Colours),
	true.
%	% performance reducing sanity check....
%	(
%		set__power_union(Colours, AllColours),
%		(set__member(Var, AllVars) => set__member(Var, AllColours))
%	->
%		error("graph_colour__group_elements: sanity check failed")
%	;
%		true
%	).

%------------------------------------------------------------------------------%

:- pred graph_colour__find_all_colours(list(set(T)), set(T), list(set(T))).
:- mode graph_colour__find_all_colours(in, in, out) is det.

	% Iterate the assignment of a new colour untill all constraints
	% are satisfied.
graph_colour__find_all_colours(ConstraintList, Vars, ColourList) :-
	(
		ConstraintList = []
	->
		ColourList = []
	;
		graph_colour__next_colour(Vars, ConstraintList,
					RemainingConstraints, Colour),
		set__difference(Vars, Colour, RestVars),
		graph_colour__find_all_colours(RemainingConstraints, RestVars,
					ColourList0),
		ColourList = [Colour|ColourList0]
	).

%------------------------------------------------------------------------------%

:- pred graph_colour__next_colour(set(T), list(set(T)), list(set(T)), set(T)).
:- mode graph_colour__next_colour(in, in, out, out) is det.

graph_colour__next_colour(Vars, ConstraintList, Remainder, SameColour) :-
	(
			% If there are any constraints left to be
			% satisfied,
		ConstraintList \= []
	->
			% Select a variable to assign a colour,
		graph_colour__choose_var(Vars, Var, Vars1),
			% and divide the constraints into those that
			% may be the same colour as that var and those
			% that may not.
		graph_colour__divide_constraints(Var, ConstraintList,
				WereContaining, NotContaining, Vars1, RestVars),
		(
				% if there are sets that can
				% share a colour with the selected var,
			NotContaining \= []
		->
			(
					% and if there is at least
					% one variable that can share
					% a colour with the selected
					% variable,
				\+ set__empty(RestVars)
			->
					% then recusively use the remaining
					% constraints to assign a colour
					% to one of the remaining vars,
					% and assemble the constraint
					% residues.
				graph_colour__next_colour(RestVars,
					NotContaining, ResidueSets,
							SameColour0),
					% add this variable to the
					% variables of the current
					% colour.
				set__insert(SameColour0, Var, SameColour)
			;
					% There were no variables left
					% that could share a colour, so
					% create a singleton set containing
					% this variable.
				set__singleton_set(SameColour, Var),
				ResidueSets = NotContaining
			)
		;
				% There were no more constraints
				% which could be satisfied by assigning
				% any variable a colour the same as the
				% current variable, so create a signleton
				% set with the current var, and assign
				% the residue to the empty set.
			set__singleton_set(SameColour, Var),
			ResidueSets = []
		),
			% The remaining constraints are the residue
			% sets that could not be satisfied by assigning
			% any variable to the current colour, and the
			% constraints that were already satisfied by
			% the assignment of the current variable to
			% this colour.
		list__append(ResidueSets, WereContaining, Remainder)
	;
			% If there were no constraints, then no colours
			% were needed.
		Remainder = [],
		set__init(SameColour)
	).

%------------------------------------------------------------------------------%

:- pred graph_colour__divide_constraints(T, list(set(T)), list(set(T)),
				list(set(T)), set(T), set(T)).
:- mode graph_colour__divide_constraints(in, in, out, out, in, out) is det.

% graph_colour__divide_constraints takes a var and a list of sets of var,
% and divides the list into two lists: a list of sets containing the
% given variable and a list of sets not containing that variable. The
% sets in the list containing the variable have that variable removed.
% Additionally, a set of variables is threaded through the computation,
% and any variables that were in sets that also contained the given
% variables are removed from the threaded set.

graph_colour__divide_constraints(_Var, [], [], [], Vars, Vars).
graph_colour__divide_constraints(Var, [S|Ss], C, NC, Vars0, Vars) :-
	graph_colour__divide_constraints(Var, Ss, C0, NC0, Vars0, Vars1),
	(
		set__member(Var, S)
	->
		set__delete(S, Var, T),
		(
			set__empty(T)
		->
			C = C0
		;
			C = [T|C0]
		),
		NC = NC0,
		set__difference(Vars1, T, Vars)
	;
		C = C0,
		NC = [S|NC0],
		Vars = Vars1
	).

%------------------------------------------------------------------------------%

:- pred graph_colour__choose_var(set(T), T, set(T)).
:- mode graph_colour__choose_var(in, out, out) is det.

% graph_colour__choose_var/3, given a set of variables, chooses
% one, returns it and the set with that variable removed. The
% use of higher order preds could be used to make the heuristic
% for which variable to choose user-defined.

graph_colour__choose_var(Vars, Var, Vars1) :-
	set__to_sorted_list(Vars, VarList),
	(
		VarList = [VarA|Vars1A]
	->
		Var = VarA,
		set__list_to_set(Vars1A, Vars1)
	;
		error("graph_colour__choose_var: no vars!")
	).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

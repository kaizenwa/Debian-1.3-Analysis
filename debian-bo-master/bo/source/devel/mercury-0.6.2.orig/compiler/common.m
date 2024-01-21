%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Original author: squirrel (Jane Anna Langley).
% Some bugs fixed by fjh.
% Extensive revision by zs.
%
% This module attempts to optimise out instances where a variable is
% decomposed and then soon after reconstructed from the parts. If possible
% we would like to "short-circuit" this process.
%
% IMPORTANT: This module does a small subset of the job of compile-time
% garbage collection, but it does so without paying attention to uniqueness
% information, since the compiler does not yet have such information.
% Once we implement ctgc, the assumptions made by this module will have
% to be revisited.
%
%---------------------------------------------------------------------------%

:- module common.
:- interface.

:- import_module hlds_module, hlds_pred.

:- pred common__optimise_common_subexpressions(module_info, module_info).
:- mode common__optimise_common_subexpressions(in, out) is det.

:- pred common__optimise_in_proc(proc_info, proc_info,
	module_info, module_info).
:- mode common__optimise_in_proc(in, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, hlds_data, quantification, mode_util, type_util.
:- import_module globals, options.
:- import_module term, map, set, list, eqvclass, require, std_util.

:- type structure	---> structure(var, type, cons_id, list(var)).
:- type struct_map	==   map(cons_id, list(structure)).
:- type common_info	---> common(map(var, type), eqvclass(var), struct_map).

%---------------------------------------------------------------------------%

common__optimise_common_subexpressions(ModuleInfo0, ModuleInfo) :-
	module_info_predids(ModuleInfo0, Pred_Ids),
	common__optimise_in_preds(Pred_Ids, ModuleInfo0, ModuleInfo).

:- pred common__optimise_in_preds(list(pred_id), module_info, module_info).
:- mode common__optimise_in_preds(in, in, out) is det.

common__optimise_in_preds([], ModuleInfo, ModuleInfo).
common__optimise_in_preds([PredId|PredIds], ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_non_imported_procids(PredInfo, ProcIds),
	common__optimise_in_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo1),
	common__optimise_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred common__optimise_in_procs(list(proc_id), pred_id,
	module_info, module_info).
:- mode common__optimise_in_procs(in, in, in, out) is det.

common__optimise_in_procs([], _PredId, ModuleInfo, ModuleInfo).
common__optimise_in_procs([ProcId | ProcIds], PredId,
		ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	common__optimise_in_proc(ProcInfo0, ProcInfo, ModuleInfo0, ModuleInfo1),

	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo2),
	common__optimise_in_procs(ProcIds, PredId, ModuleInfo2, ModuleInfo).

common__optimise_in_proc(ProcInfo0, ProcInfo, ModuleInfo0, ModuleInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_variables(ProcInfo0, Varset0),
	eqvclass__init(VarEqv0),
	map__init(StructMap0),
	CommonInfo0 = common(VarTypes0, VarEqv0, StructMap0),
	common__optimise_in_goal_pair(Goal0, Goal1, CommonInfo0, _),
	( Goal1 = Goal0 ->
		ProcInfo = ProcInfo0,
		ModuleInfo = ModuleInfo0
	;
		proc_info_headvars(ProcInfo0, HeadVars),
		implicitly_quantify_clause_body(HeadVars, Goal1, Varset0,
			VarTypes0, Goal2, Varset, VarTypes, _Warnings),
		recompute_instmap_delta(Goal2, Goal, ModuleInfo0, ModuleInfo),
		proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
		proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
		proc_info_set_variables(ProcInfo2, Varset, ProcInfo)
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Strips the goal_info out of the hlds__goal pair
	% so that the following predicate can operate
	% deterministically on the hlds__goal_expr

:- pred common__optimise_in_goal_pair(hlds__goal, hlds__goal,
	common_info, common_info).
:- mode common__optimise_in_goal_pair(in, out, in, out) is det.

common__optimise_in_goal_pair(Goal0 - GoalInfo, Goal - GoalInfo,
		CommonInfo0, CommonInfo) :-
	common__optimise_in_goal(Goal0, Goal, CommonInfo0, CommonInfo).

%---------------------------------------------------------------------------%

	% Recursively performs the common subexpression optimisation
	% delegating work where necessary to the following predicates.

	% We cannot return any information from a branched goal unless
	% it is valid in all branches. At the moment, we ignore any
	% information gathered inside such goals. Later we could compute
	% the intersection of the information from the various branches,
	% but in most cases we'd only get back the state at the start
	% of the branched goal.

:- pred common__optimise_in_goal(hlds__goal_expr, hlds__goal_expr,
	common_info, common_info).
:- mode common__optimise_in_goal(in, out, in, out) is det.

common__optimise_in_goal(conj(Goals0), conj(Goals), CommonInfo0, CommonInfo) :-
	common__optimise_over_conjunction(Goals0, [], Goals1,
		CommonInfo0, CommonInfo),
	list__reverse(Goals1, Goals).

common__optimise_in_goal(disj(Goals0, SM), disj(Goals, SM),
		CommonInfo0, CommonInfo0) :-
	common__optimise_over_disjunction(Goals0, [], Goals1,
		CommonInfo0, _CommonInfo),
	list__reverse(Goals1, Goals).

common__optimise_in_goal(switch(Var, Fail, Cases0, SM),
		switch(Var, Fail, Cases, SM), CommonInfo0, CommonInfo0) :-
	common__optimise_over_switch(Cases0, [], Cases1,
		CommonInfo0, _CommonInfo),
	list__reverse(Cases1, Cases).

common__optimise_in_goal(if_then_else(Vars, If0, Then0, Else0, SM),
		if_then_else(Vars, If, Then, Else, SM),
		CommonInfo0, CommonInfo0) :-

		%% optimise into the three "subgoals"
	common__optimise_in_goal_pair(If0, If, CommonInfo0, CommonInfo1),
	common__optimise_in_goal_pair(Then0, Then, CommonInfo1, _CommonInfo2),
	common__optimise_in_goal_pair(Else0, Else, CommonInfo0, _CommonInfo3).

common__optimise_in_goal(higher_order_call(A, B, C, D, E),
			higher_order_call(A, B, C, D, E),
		CommonInfo, CommonInfo).

common__optimise_in_goal(call(A, B, C, D, E, F), call(A, B, C, D, E, F),
		CommonInfo, CommonInfo).

common__optimise_in_goal(not(Goal0), not(Goal), CommonInfo0, CommonInfo0) :-
	common__optimise_in_goal_pair(Goal0, Goal, CommonInfo0, _CommonInfo).

common__optimise_in_goal(some(Vars, Goal0), some(Vars, Goal),
		CommonInfo0, CommonInfo0) :-
	common__optimise_in_goal_pair(Goal0, Goal, CommonInfo0, _CommonInfo).

common__optimise_in_goal(
		unify(Left0, Right0, UMode, Unification0, UContext),
		unify(Left, Right, UMode, Unification, UContext),
		CommonInfo0, CommonInfo) :-
	common__optimise_unification(Unification0, Left0, Right0,
		Unification, Left, Right, CommonInfo0, CommonInfo).

	% We can't do any common subexpression elimination on pragma
	% c_code.
common__optimise_in_goal(pragma_c_code(A,B,C,D,E,F), 
		pragma_c_code(A,B,C,D,E,F),
		CommonInfo, CommonInfo).


%---------------------------------------------------------------------------%

	% Optimises over conjunctions and collects deconstructions
	% as it finds them.

:- pred common__optimise_over_conjunction(list(hlds__goal), list(hlds__goal),
	list(hlds__goal), common_info, common_info).
% :- mode common__optimise_over_conjunction(in, di, uo, in, out) is det.
:- mode common__optimise_over_conjunction(in, in, out, in, out) is det.

common__optimise_over_conjunction([], RevSofar, RevSofar,
		CommonInfo, CommonInfo).
common__optimise_over_conjunction([Goal0 | Goals0], RevSofar,
		RevGoals, CommonInfo0, CommonInfo) :-
	common__optimise_in_goal_pair(Goal0, Goal, CommonInfo0, CommonInfo1),
	common__optimise_over_conjunction(Goals0, [Goal | RevSofar],
		RevGoals, CommonInfo1, CommonInfo).

%---------------------------------------------------------------------------%

:- pred common__optimise_over_disjunction(list(hlds__goal), list(hlds__goal),
	list(hlds__goal), common_info, common_info).
:- mode common__optimise_over_disjunction(in, in, out, in, out) is det.

common__optimise_over_disjunction([], RevSofar, RevSofar,
		CommonInfo, CommonInfo).
common__optimise_over_disjunction([Goal0 | Goals0], RevSofar,
		RevGoals, CommonInfo0, CommonInfo) :-
	common__optimise_in_goal_pair(Goal0, Goal, CommonInfo0, _CommonInfo1),
	common__optimise_over_disjunction(Goals0, [Goal | RevSofar],
		RevGoals, CommonInfo0, CommonInfo).

%---------------------------------------------------------------------------%

:- pred common__optimise_over_switch(list(case), list(case), list(case),
	common_info, common_info).
:- mode common__optimise_over_switch(in, in, out, in, out) is det.

common__optimise_over_switch([], RevSofar, RevSofar,
		CommonInfo, CommonInfo).
common__optimise_over_switch([case(ConsId, Goal0) | Cases0], RevSofar,
		RevCases, CommonInfo0, CommonInfo) :-
	common__optimise_in_goal_pair(Goal0, Goal, CommonInfo0, _CommonInfo1),
	common__optimise_over_switch(Cases0, [case(ConsId, Goal) | RevSofar],
		RevCases, CommonInfo0, CommonInfo).

%---------------------------------------------------------------------------%

	% If we find a deconstruction or a construction we cannot optimize,
	% record the details of the memory cell in CommonInfo.

	% If we find a construction that constructs a cell identical to one
	% we have seen before, replace the construction with an assignment
	% from the variable unified with that cell.

:- pred common__optimise_unification(unification, var, unify_rhs,
	unification, var, unify_rhs, common_info, common_info).
:- mode common__optimise_unification(in, in, in, out, out, out, in, out) is det.

common__optimise_unification(Unification0, Left0, Right0,
		Unification, Left, Right, CommonInfo0, CommonInfo) :-
	(
		Unification0 = construct(Var, ConsId, ArgVars, _),
		(
			common__find_matching_cell(Var, ConsId, ArgVars,
				CommonInfo0, OldVar)
		->
			Unification = assign(Var, OldVar),
			Left = Left0,
			Right = var(OldVar),
			common__record_equivalance(Var, OldVar,
				CommonInfo0, CommonInfo)
		;
			Unification = Unification0,
			Left = Left0,
			Right = Right0,
			common__record_cell(Var, ConsId, ArgVars,
				CommonInfo0, CommonInfo)
		)
	;
		Unification0 = deconstruct(Var, ConsId, Vars, _, _),
		Unification = Unification0,
		Left = Left0,
		Right = Right0,
		common__record_cell(Var, ConsId, Vars, CommonInfo0, CommonInfo)
	;
		Unification0 = assign(Var1, Var2),
		Unification = Unification0,
		Left = Left0,
		Right = Right0,
		common__record_equivalance(Var1, Var2, CommonInfo0, CommonInfo)
	;
		Unification0 = simple_test(Var1, Var2),
		Unification = Unification0,
		Left = Left0,
		Right = Right0,
		common__record_equivalance(Var1, Var2, CommonInfo0, CommonInfo)
	;
		Unification0 = complicated_unify(_, _),
		Unification = Unification0,
		Left = Left0,
		Right = Right0,
		CommonInfo = CommonInfo0
	).

%---------------------------------------------------------------------------%

:- pred common__find_matching_cell(var, cons_id, list(var), common_info, var).
:- mode common__find_matching_cell(in, in, in, in, out) is semidet.

common__find_matching_cell(Var, ConsId, ArgVars, CommonInfo, OldVar) :-
	CommonInfo = common(_VarTypes, _VarEqv, StructMap),
	map__search(StructMap, ConsId, Structs),
	common__find_matching_cell_2(Structs, Var, ConsId, ArgVars,
		CommonInfo, OldVar).

:- pred common__find_matching_cell_2(list(structure), var, cons_id, list(var),
	common_info, var).
:- mode common__find_matching_cell_2(in, in, in, in, in, out) is semidet.

common__find_matching_cell_2([Struct | Structs], Var, ConsId, ArgVars,
		CommonInfo, OldVar) :-
	Struct = structure(StructVar, StructType, StructConsId, StructArgVars),
	(
		CommonInfo = common(VarTypes, VarEqv, _StructMap),

		% Are the arguments the same (or equivalent) variables?
		ConsId = StructConsId,
		common__vars_are_equivalent(ArgVars, StructArgVars, VarEqv),

		% Two structures of the same shape may have different types
		% and therefore different representations.
		map__lookup(VarTypes, Var, VarType),
		common__compatible_types(VarType, StructType)
	->
		OldVar = StructVar
	;
		common__find_matching_cell_2(Structs, Var, ConsId, ArgVars,
			CommonInfo, OldVar)
	).

%---------------------------------------------------------------------------%

	% Two structures have compatible representations if the top
	% level of their types are unifiable.  % For example, if we have
	%
	%	:- type maybe_err(T) --> ok(T) ; err(string).
	%
	%	:- pred p(maybe_err(foo)::in, maybe_err(bar)::out) is semidet.
	%	p(err(X), err(X)).
	%
	% then we want to reuse the `err(X)' in the first arg rather than
	% constructing a new copy of it for the second arg.
	% The two occurrences of `err(X)' have types `maybe_err(int)'
	% and `maybe(float)', but we know that they have the same 
	% representation.

:- pred common__compatible_types(type, type).
:- mode common__compatible_types(in, in) is semidet.

common__compatible_types(Type1, Type2) :-
	type_to_type_id(Type1, TypeId1, _),
	type_to_type_id(Type2, TypeId2, _),
	TypeId1 = TypeId2.

%---------------------------------------------------------------------------%

:- pred common__vars_are_equivalent(list(var), list(var), eqvclass(var)).
:- mode common__vars_are_equivalent(in, in, in) is semidet.

common__vars_are_equivalent([], [], _VarEqv).
common__vars_are_equivalent([Arg | Args], [Struct | Structs], VarEqv) :-
	% write('looking for equivalance of '),
	% write(Arg),
	% write(' and '),
	% write(Struct),
	% nl,
	(
		Arg = Struct
	;
		eqvclass__is_member(VarEqv, Arg),
		eqvclass__is_member(VarEqv, Struct),
		eqvclass__same_eqvclass(VarEqv, Arg, Struct)
	),
	% write('they are equivalent'),
	% nl,
	common__vars_are_equivalent(Args, Structs, VarEqv).

%---------------------------------------------------------------------------%

:- pred common__record_cell(var, cons_id, list(var), common_info, common_info).
:- mode common__record_cell(in, in, in, in, out) is det.

common__record_cell(Var, ConsId, ArgVars, CommonInfo0, CommonInfo) :-
	( ArgVars = [] ->
		% Constants do not have memory cells to reuse,
		% at least in the memory models we are interested in.
		CommonInfo = CommonInfo0
	;
		CommonInfo0 = common(VarTypes, VarEqv, StructMap0),
		( map__search(StructMap0, ConsId, StructList0Prime) ->
			StructList0 = StructList0Prime
		;
			StructList0 = []
		),
		map__lookup(VarTypes, Var, VarType),
		Struct = structure(Var, VarType, ConsId, ArgVars),

		% Insert the new cell at the front of the list. If it hides
		% an equivalent cell, at least the reuse of this cell will
		% require saving its address over fewer calls.

		StructList = [Struct | StructList0],
		map__set(StructMap0, ConsId, StructList, StructMap),
		CommonInfo = common(VarTypes, VarEqv, StructMap)
	).

%---------------------------------------------------------------------------%

:- pred common__record_equivalance(var, var, common_info, common_info).
:- mode common__record_equivalance(in, in, in, out) is det.

common__record_equivalance(Var1, Var2, CommonInfo0, CommonInfo) :-
	CommonInfo0 = common(VarTypes, VarEqv0, StructMap0),
	% write('ensuring equivalence of '),
	% write(Var1),
	% write(' and '),
	% write(Var2),
	% nl,
	eqvclass__ensure_equivalence(VarEqv0, Var1, Var2, VarEqv),
	CommonInfo = common(VarTypes, VarEqv, StructMap0).

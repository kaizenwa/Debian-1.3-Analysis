%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Switch detection - when a disjunction contains disjuncts that unify the
% same input variable with different function symbols, replace (part of)
% the disjunction with a switch.
%
% Main author: fjh.
%
%-----------------------------------------------------------------------------%

:- module switch_detection.

:- interface.

:- import_module hlds_module, hlds_pred.

:- pred detect_switches(module_info, module_info, io__state, io__state).
:- mode detect_switches(in, out, di, uo) is det.

:- pred detect_switches_in_proc(proc_id, pred_id, module_info, module_info).
:- mode detect_switches_in_proc(in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, hlds_data, prog_data, instmap.
:- import_module modes, mode_util, type_util, det_util.
:- import_module int, list, assoc_list, map, set, std_util, term, require.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `detect_switches_in_goal'
	% for each procedure body.

detect_switches(ModuleInfo0, ModuleInfo1) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	detect_switches_in_preds(PredIds, ModuleInfo0, ModuleInfo1).

:- pred detect_switches_in_preds(list(pred_id), module_info, module_info,
	io__state, io__state).
:- mode detect_switches_in_preds(in, in, out, di, uo) is det.

detect_switches_in_preds([], ModuleInfo, ModuleInfo) --> [].
detect_switches_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	detect_switches_in_pred(PredId, PredInfo, ModuleInfo0, ModuleInfo1),
	detect_switches_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred detect_switches_in_pred(pred_id, pred_info, module_info, module_info,
	io__state, io__state).
:- mode detect_switches_in_pred(in, in, in, out, di, uo) is det.

detect_switches_in_pred(PredId, PredInfo0, ModuleInfo0, ModuleInfo,
		IOstate, IOstate) :-
	pred_info_non_imported_procids(PredInfo0, ProcIds),
	detect_switches_in_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo).

:- pred detect_switches_in_procs(list(proc_id), pred_id,
	module_info, module_info).
:- mode detect_switches_in_procs(in, in, in, out) is det.

detect_switches_in_procs([], _PredId, ModuleInfo, ModuleInfo).
detect_switches_in_procs([ProcId | ProcIds], PredId, ModuleInfo0, ModuleInfo) :-
	detect_switches_in_proc(ProcId, PredId, ModuleInfo0, ModuleInfo1),
	detect_switches_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

detect_switches_in_proc(ProcId, PredId, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

		% To process each ProcInfo, we get the goal,
		% initialize the instmap based on the modes of the head vars,
		% and pass these to `detect_switches_in_goal'.
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_vartypes(ProcInfo0, VarTypes),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo0, Goal),

	proc_info_set_goal(ProcInfo0, Goal, ProcInfo),
	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Given a goal, and the instmap on entry to that goal,
	% replace disjunctions with switches whereever possible.

:- pred detect_switches_in_goal(hlds__goal, instmap, map(var, type),
	module_info, hlds__goal).
:- mode detect_switches_in_goal(in, in, in, in, out) is det.

detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal) :-
	detect_switches_in_goal_1(Goal0, InstMap0, VarTypes, ModuleInfo,
		Goal, _InstMap).

	% This version is the same as the above except that it returns
	% the resulting instmap on exit from the goal, which is
	% computed by applying the instmap delta specified in the
	% goal's goalinfo.

:- pred detect_switches_in_goal_1(hlds__goal, instmap, map(var, type),
	module_info, hlds__goal, instmap).
:- mode detect_switches_in_goal_1(in, in, in, in, out, out) is det.

detect_switches_in_goal_1(Goal0 - GoalInfo, InstMap0, VarTypes, ModuleInfo,
		Goal - GoalInfo, InstMap) :-
	detect_switches_in_goal_2(Goal0, GoalInfo, InstMap0,
		VarTypes, ModuleInfo, Goal),
	update_instmap(Goal0 - GoalInfo, InstMap0, InstMap).

	% Here we process each of the different sorts of goals.

:- pred detect_switches_in_goal_2(hlds__goal_expr, hlds__goal_info, instmap,
		map(var, type), module_info, hlds__goal_expr).
:- mode detect_switches_in_goal_2(in, in, in, in, in, out) is det.

detect_switches_in_goal_2(disj(Goals0, SM), GoalInfo, InstMap0,
		VarTypes, ModuleInfo, Goal) :-
	( Goals0 = [] ->
		Goal = disj([], SM)
	;
		goal_info_get_nonlocals(GoalInfo, NonLocals),
		set__to_sorted_list(NonLocals, NonLocalsList),
		detect_switches_in_disj(NonLocalsList, Goals0, GoalInfo,
			SM, InstMap0, VarTypes, NonLocalsList, ModuleInfo,
			[], Goal)
	).

detect_switches_in_goal_2(conj(Goals0), _GoalInfo, InstMap0,
		VarTypes, ModuleInfo, conj(Goals)) :-
	detect_switches_in_conj(Goals0, InstMap0, VarTypes, ModuleInfo, Goals).

detect_switches_in_goal_2(not(Goal0), _GoalInfo, InstMap0,
		VarTypes, ModuleInfo, not(Goal)) :-
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal).

detect_switches_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, SM),
		_GoalInfo, InstMap0, VarTypes, ModuleInfo,
		if_then_else(Vars, Cond, Then, Else, SM)) :-
	detect_switches_in_goal_1(Cond0, InstMap0, VarTypes, ModuleInfo, Cond,
		InstMap1),
	detect_switches_in_goal(Then0, InstMap1, VarTypes, ModuleInfo, Then),
	detect_switches_in_goal(Else0, InstMap0, VarTypes, ModuleInfo, Else).

detect_switches_in_goal_2(some(Vars, Goal0), _GoalInfo, InstMap0,
		VarTypes, ModuleInfo, some(Vars, Goal)) :-
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal).

detect_switches_in_goal_2(higher_order_call(A,B,C,D,E), _, _, _, _,
		higher_order_call(A,B,C,D,E)).

detect_switches_in_goal_2(call(A,B,C,D,E,F), _, _, _, _,
		call(A,B,C,D,E,F)).

detect_switches_in_goal_2(unify(A,RHS0,C,D,E), __GoalInfo, InstMap0,
		VarTypes, ModuleInfo, unify(A,RHS,C,D,E)) :-
	( RHS0 = lambda_goal(PredOrFunc, Vars, Modes, Det, Goal0) ->
		% we need to insert the initial insts for the lambda
		% variables in the instmap before processing the lambda goal
		mode_list_get_initial_insts(Modes, ModuleInfo, Insts),
		assoc_list__from_corresponding_lists(Vars, Insts, VarInsts),
		instmap_delta_from_assoc_list(VarInsts, InstmapDelta),
		instmap__apply_instmap_delta(InstMap0, InstmapDelta, InstMap1),
		detect_switches_in_goal(Goal0, InstMap1, VarTypes, ModuleInfo,
			Goal),
		RHS = lambda_goal(PredOrFunc, Vars, Modes, Det, Goal)
	;
		RHS = RHS0
	).

detect_switches_in_goal_2(switch(Var, CanFail, Cases0, SM), _, InstMap,
		VarTypes, ModuleInfo, switch(Var, CanFail, Cases, SM)) :-
	detect_switches_in_cases(Cases0, InstMap, VarTypes, ModuleInfo, Cases).

detect_switches_in_goal_2(pragma_c_code(A,B,C,D,E,F), _, _, _, _,
		pragma_c_code(A,B,C,D,E,F)).

%-----------------------------------------------------------------------------%

	% This is the interesting bit - we've found a non-empty
	% disjunction, and we've got a list of the non-local variables
	% of that disjunction. Now for each non-local variable, we
	% check whether there is a partition of the disjuncts such that
	% each group of disjunctions can only succeed if the variable
	% is bound to a different functor.

:- type cases == map(cons_id, list(hlds__goal)).

:- type sorted_case_list == assoc_list(cons_id, list(hlds__goal)).
	% the sorted_case_list should always be sorted on cons_id -
	% `delete_unreachable_cases' relies on this.

:- type again ---> again(var, list(hlds__goal), sorted_case_list).

:- pred detect_switches_in_disj(list(var), list(hlds__goal), hlds__goal_info,
	store_map, instmap, map(var, type), list(var), module_info,
	list(again), hlds__goal_expr).
:- mode detect_switches_in_disj(in, in, in, in, in, in, in, in, in, out) is det.

detect_switches_in_disj([Var | Vars], Goals0, GoalInfo, SM, InstMap,
		VarTypes, AllVars, ModuleInfo, Again0, Goal) :-
	% can we do at least a partial switch on this variable?
	(
		instmap__lookup_var(InstMap, Var, VarInst0),
		inst_is_bound(ModuleInfo, VarInst0),
		partition_disj(Goals0, Var, Left, CasesList)
	->
		% are there any disjuncts that are not part of the switch?
		(
			Left = []
		->
			cases_to_switch(CasesList, Var, VarTypes, GoalInfo,
				SM, InstMap, ModuleInfo, Goal)
		;
			detect_switches_in_disj(Vars, Goals0, GoalInfo,
				SM, InstMap, VarTypes, AllVars, ModuleInfo,
				[again(Var, Left, CasesList) | Again0], Goal)
		)
	;
		detect_switches_in_disj(Vars, Goals0, GoalInfo, SM, InstMap,
			VarTypes, AllVars, ModuleInfo, Again0, Goal)
	).
detect_switches_in_disj([], Goals0, GoalInfo, SM, InstMap,
		VarTypes, AllVars, ModuleInfo, AgainList0, disj(Goals, SM)) :-
	(
		AgainList0 = [],
		detect_sub_switches_in_disj(Goals0, InstMap, VarTypes,
			ModuleInfo, Goals)
	;
		AgainList0 = [Again | AgainList1],
		select_best_switch(AgainList1, Again, BestAgain),
		BestAgain = again(Var, Left0, CasesList),
		cases_to_switch(CasesList, Var, VarTypes, GoalInfo, SM, InstMap,
			ModuleInfo, SwitchGoal),
		detect_switches_in_disj(AllVars, Left0, GoalInfo, SM, InstMap,
			VarTypes, AllVars, ModuleInfo, [], Left),
		goal_to_disj_list(Left - GoalInfo, LeftList),
		Goals = [SwitchGoal - GoalInfo | LeftList]
	).

:- pred select_best_switch(list(again), again, again).
:- mode select_best_switch(in, in, out) is det.

select_best_switch([], BestAgain, BestAgain).
select_best_switch([Again | AgainList], BestAgain0, BestAgain) :-
	(
		Again = again(_, _, CasesList),
		BestAgain0 = again(_, _, BestCasesList),
		list__length(CasesList, Length),
		list__length(BestCasesList, BestLength),
		Length < BestLength
	->
		BestAgain1 = BestAgain0
	;
		BestAgain1 = Again
	),
	select_best_switch(AgainList, BestAgain1, BestAgain).

:- pred detect_sub_switches_in_disj(list(hlds__goal), instmap, map(var, type),
	module_info, list(hlds__goal)).
:- mode detect_sub_switches_in_disj(in, in, in, in, out) is det.

detect_sub_switches_in_disj([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_sub_switches_in_disj([Goal0 | Goals0], InstMap, VarTypes, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal(Goal0, InstMap, VarTypes, ModuleInfo, Goal),
	detect_sub_switches_in_disj(Goals0, InstMap, VarTypes, ModuleInfo,
		Goals).

:- pred detect_switches_in_cases(list(case), instmap, map(var, type),
	module_info, list(case)).
:- mode detect_switches_in_cases(in, in, in, in, out) is det.

detect_switches_in_cases([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_switches_in_cases([Case0 | Cases0], InstMap, VarTypes, ModuleInfo,
		[Case | Cases]) :-
	Case0 = case(Functor, Goal0),
	detect_switches_in_goal(Goal0, InstMap, VarTypes, ModuleInfo, Goal),
	Case = case(Functor, Goal),
	detect_switches_in_cases(Cases0, InstMap, VarTypes, ModuleInfo, Cases).

:- pred detect_switches_in_conj(list(hlds__goal), instmap, map(var, type),
	module_info, list(hlds__goal)).
:- mode detect_switches_in_conj(in, in, in, in, out) is det.

detect_switches_in_conj([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_switches_in_conj([Goal0 | Goals0], InstMap0, VarTypes, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal_1(Goal0, InstMap0, VarTypes, ModuleInfo, Goal,
		InstMap1),
	detect_switches_in_conj(Goals0, InstMap1, VarTypes, ModuleInfo, Goals).

%-----------------------------------------------------------------------------%

	% partition_disj(Goals, Var, GoalInfo, VarTypes, ModuleInfo,
	%	Left, Cases):
	% Attempts to partition the disjunction `Goals' into a switch on `Var'.
	% If at least partially successful, returns the resulting `Cases', with
	% any disjunction goals not fitting into the switch in Left.

	% Given the list of goals in a disjunction, and an input variable
	% to switch on, we attempt to partition the goals into a switch.
	% For each constructor id, we record the list of disjuncts
	% which unify the variable with that constructor.
	% We partition the goals by abstractly interpreting the unifications
	% at the start of each disjunction, to build up a substitution.

:- pred partition_disj(list(hlds__goal), var, list(hlds__goal),
	sorted_case_list).
:- mode partition_disj(in, in, out, out) is semidet.

partition_disj(Goals0, Var, Left, CasesAssocList) :-
	map__init(Cases0),
	partition_disj_trial(Goals0, Var, [], Left, Cases0, Cases),
	map__to_assoc_list(Cases, CasesAssocList),
	CasesAssocList = [_, _ | _].	% there must be more than one case

:- pred partition_disj_trial(list(hlds__goal), var,
	list(hlds__goal), list(hlds__goal), cases, cases).
:- mode partition_disj_trial(in, in, in, out, in, out) is det.

partition_disj_trial([], _Var, Left, Left, Cases, Cases).
partition_disj_trial([Goal0 | Goals], Var, Left0, Left, Cases0, Cases) :-
	goal_to_conj_list(Goal0, ConjList0),
	Goal0 = _ - GoalInfo,
	map__init(Substitution),
	find_bind_var_for_switch(ConjList0, Substitution, Var,
			ConjList, _NewSubstitution, MaybeFunctor),
	(
		MaybeFunctor = yes(Functor),
		Left1 = Left0,
		conj_list_to_goal(ConjList, GoalInfo, Goal),
		( map__search(Cases0, Functor, DisjList0) ->
			DisjList1 = [Goal | DisjList0]
		;
			DisjList1 = [Goal]
		),
		map__set(Cases0, Functor, DisjList1, Cases1)
	;
		MaybeFunctor = no,
		Left1 = [Goal0 | Left0],
		Cases1 = Cases0
	),
	partition_disj_trial(Goals, Var, Left1, Left, Cases1, Cases).

	% find_bind_var_for_switch(Goals0, Subst0, Var, Goals, Subst,
	%	MaybeFunctor):
	%	Searches through Goals0 looking for a deconstruction
	%	unification with `Var'. If found, sets `MaybeFunctor'
	% 	to `yes(Functor)', where Functor is the
	%	functor which `Var' gets unified, and
	%	sets `Goals' to be `Goals0' with that deconstruction
	%	unification made deterministic. If not found, sets
	%	`MaybeFunctor' to `no', and computes `Subst' as the
	%	resulting substitution from interpreting through the goal.

:- pred find_bind_var_for_switch(list(hlds__goal), substitution, var,
	list(hlds__goal), substitution, maybe(cons_id)).
:- mode find_bind_var_for_switch(in, in, in, out, out, out) is det.

find_bind_var_for_switch([], Substitution, _Var, [], Substitution, no).
find_bind_var_for_switch([Goal0 - GoalInfo | Goals0], Substitution0, Var,
		[Goal - GoalInfo | Goals], Substitution, MaybeFunctor) :-
	( Goal0 = conj(SubGoals0) ->
		find_bind_var_for_switch(SubGoals0, Substitution0, Var,
			SubGoals, Substitution1, MaybeFunctor1),
		Goal = conj(SubGoals),
		( MaybeFunctor1 = yes(_) ->
			Goals = Goals0,
			Substitution = Substitution1,
			MaybeFunctor = MaybeFunctor1
		;
			find_bind_var_for_switch(Goals0, Substitution1, Var,
				Goals, Substitution, MaybeFunctor)
		)
	; Goal0 = unify(A, B, C, UnifyInfo0, E) ->
			% check whether the unification is a deconstruction
			% unification on Var or a variable aliased to Var
		(
			UnifyInfo0 = deconstruct(UnifyVar, Functor, F, G, _),
			term__apply_rec_substitution(term__variable(Var),
				Substitution0, term__variable(Var1)),
			term__apply_rec_substitution(term__variable(UnifyVar),
				Substitution0, term__variable(UnifyVar1)),
			Var1 = UnifyVar1
		->
			MaybeFunctor = yes(Functor),
				% The deconstruction unification now becomes
				% deterministic, since the test will get
				% carried out in the switch.
			UnifyInfo = deconstruct(UnifyVar, Functor, F, G,
				cannot_fail),
			Goal = unify(A, B, C, UnifyInfo, E),
			Goals = Goals0,
			Substitution = Substitution0
		;
			% otherwise abstractly interpret the unification
			% and continue
			Goal = Goal0,
			( interpret_unify(A, B, Substitution0, Substitution1) ->
				Substitution2 = Substitution1
			;
				% the unification must fail - just ignore it
				Substitution2 = Substitution0
			),
			find_bind_var_for_switch(Goals0, Substitution2, Var,
				Goals, Substitution, MaybeFunctor)
		)
	;
		Goal = Goal0,
		Goals = Goals0,
		Substitution = Substitution0,
		MaybeFunctor = no
	).

:- pred cases_to_switch(sorted_case_list, var, map(var, type), hlds__goal_info,
	store_map, instmap, module_info, hlds__goal_expr).
:- mode cases_to_switch(in, in, in, in, in, in, in, out) is det.

cases_to_switch(CasesList, Var, VarTypes, GoalInfo, SM, InstMap, ModuleInfo,
		Goal) :-
	instmap__lookup_var(InstMap, Var, VarInst),
	( inst_is_bound_to_functors(ModuleInfo, VarInst, Functors) ->
		functors_to_cons_ids(Functors, ConsIds0),
		list__sort(ConsIds0, ConsIds),
		delete_unreachable_cases(CasesList, ConsIds, CasesList1),
		( list__same_length(Functors, CasesList1) ->
			CanFail = cannot_fail
		;
			CanFail = can_fail
		)
	;
		map__lookup(VarTypes, Var, Type),
		( switch_covers_all_cases(CasesList, Type, ModuleInfo) ->
			CanFail = cannot_fail
		;
			CanFail = can_fail
		),
		CasesList1 = CasesList
	),
	fix_case_list(CasesList1, GoalInfo, Cases0),
	detect_switches_in_cases(Cases0, InstMap, VarTypes, ModuleInfo, Cases),

	% We turn switches with no arms into fail, since this avoids having
	% the code generator flush the control variable of the switch.
	% We can't easily eliminate switches with one arm, since the
	% code of the arm will have the unification between the variable
	% and the function symbol as det. The gain would be minimal to
	% nonexistent anyway.
	(
		Cases = [],
		map__init(Empty),
		Goal = disj([], Empty)
	;
		Cases = [_ | _],
		Goal = switch(Var, CanFail, Cases, SM)
	).

:- pred delete_unreachable_cases(sorted_case_list, list(cons_id),
	sorted_case_list).
:- mode delete_unreachable_cases(in, in, out) is det.

	% Given a list of cases, and a list of the possible cons_ids
	% that the switch variable could be bound to, select out only
	% those cases whose cons_id occurs in the list of cases
	% We assume that the list of cases and the list of cons_ids
	% are sorted, so that we can do this using a simple sorted merge.

delete_unreachable_cases([], _, []).
delete_unreachable_cases([_ | _], [], []).
delete_unreachable_cases([Case | Cases0], [ConsId | ConsIds], Cases) :-
	Case = CaseConsId - _DisjList,
	( CaseConsId = ConsId ->
		Cases = [Case | Cases1],
		delete_unreachable_cases(Cases0, ConsIds, Cases1)
	; compare(<, CaseConsId, ConsId) ->
		delete_unreachable_cases(Cases0, [ConsId | ConsIds], Cases)
	;
		delete_unreachable_cases([Case | Cases0], ConsIds, Cases)
	).

	% Given a list of bound_insts, get the corresponding list of cons_ids
	% 
:- pred functors_to_cons_ids(list(bound_inst), list(cons_id)).
:- mode functors_to_cons_ids(in, out) is det.

functors_to_cons_ids([], []).
functors_to_cons_ids([Functor | Functors], [ConsId | ConsIds]) :-
	Functor = functor(ConsId, _ArgInsts),
	functors_to_cons_ids(Functors, ConsIds).

	% check whether a switch handles all the possible
	% constants/functors for the type

:- pred switch_covers_all_cases(assoc_list(cons_id, list(hlds__goal)),
	type, module_info).
:- mode switch_covers_all_cases(in, in, in) is semidet.

switch_covers_all_cases(CasesList, Type, _ModuleInfo) :-
	Type = term__functor(term__atom("character"), [], _),
	list__length(CasesList, 127).	% XXX should be 256
		% NU-Prolog only allows chars '\001' .. '\0177'.
		% Currently we assume the same set.

switch_covers_all_cases(CasesList, Type, ModuleInfo) :-
	type_to_type_id(Type, TypeId, _),
	module_info_types(ModuleInfo, TypeTable),
	map__search(TypeTable, TypeId, TypeDefn),
	hlds_data__get_type_defn_body(TypeDefn, TypeBody),
	TypeBody = du_type(_, ConsTable, _),
	map__keys(ConsTable, Constructors),
	list__same_length(CasesList, Constructors).

	% convert the assoc_list(cons_id, list(hlds__goal) back into
	% a plain list(case).

:- pred fix_case_list(assoc_list(cons_id, list(hlds__goal)), hlds__goal_info,
	list(case)).
:- mode fix_case_list(in, in, out) is det.

fix_case_list([], _, []).
fix_case_list([Functor - DisjList | Cases0], GoalInfo,
		[case(Functor, Goal) | Cases]) :-
	disj_list_to_goal(DisjList, GoalInfo, Goal),
	fix_case_list(Cases0, GoalInfo, Cases).

%-----------------------------------------------------------------------------%

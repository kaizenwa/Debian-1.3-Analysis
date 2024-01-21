%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% det_analysis.m - the determinism analysis pass.

% Main authors: conway, fjh, zs.

% This pass has three components:
%
%	o Segregate the procedures into those that have determinism
%		declarations, and those that don't
%
%	o A step of performing a local inference pass on each procedure
%		without a determinism declaration is iterated until
%		a fixpoint is reached
%
%	o A checking step is performed on all the procedures that have
%		determinism declarations to ensure that they are at
%		least as deterministic as their declaration. This uses
%		a form of the local inference pass.
%
% If we are to avoid global inference for predicates with
% declarations, then it must be an error, not just a warning,
% if the determinism checking step detects that the determinism
% annotation was wrong.  If we were to issue just a warning, then
% we would have to override the determinism annotation, and that
% would force us to re-check the inferred determinism for all
% calling predicates.
%
% Alternately, we could leave it as a warning, but then we would
% have to _make_ the predicate deterministic (or semideterministic)
% by inserting run-time checking code which calls error/1 if the
% predicate really isn't deterministic (semideterministic).

% Determinism has three components:
%
%	whether a goal can fail
%	whether a goal has more than one possible solution
%	whether a goal occurs in a context where only the first solution
%		is required
%
% The first two components are synthesized attributes: they are inferred
% bottom-up.  The last component is an inherited attribute: it is
% propagated top-down.

%-----------------------------------------------------------------------------%

:- module det_analysis.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_data, io.

	% Perform determinism inference for local predicates with no
	% determinism declarations, and determinism checking for all other
	% predicates.
:- pred determinism_pass(module_info, module_info, io__state, io__state).
:- mode determinism_pass(in, out, di, uo) is det.

	% Check the determinism of a single procedure
	% (only works if the determinism of the procedures it calls
	% has already been inferred).
:- pred determinism_check_proc(proc_id, pred_id, module_info, module_info,
	io__state, io__state).
:- mode determinism_check_proc(in, in, in, out, di, uo) is det.

	% The tables for computing the determinism of compound goals
	% from the determinism of their components.

:- pred det_conjunction_maxsoln(soln_count, soln_count, soln_count).
:- mode det_conjunction_maxsoln(in, in, out) is det.

:- pred det_conjunction_canfail(can_fail, can_fail, can_fail).
:- mode det_conjunction_canfail(in, in, out) is det.

:- pred det_disjunction_maxsoln(soln_count, soln_count, soln_count).
:- mode det_disjunction_maxsoln(in, in, out) is det.

:- pred det_disjunction_canfail(can_fail, can_fail, can_fail).
:- mode det_disjunction_canfail(in, in, out) is det.

:- pred det_switch_maxsoln(soln_count, soln_count, soln_count).
:- mode det_switch_maxsoln(in, in, out) is det.

:- pred det_switch_canfail(can_fail, can_fail, can_fail).
:- mode det_switch_canfail(in, in, out) is det.

:- pred det_negation_det(determinism, maybe(determinism)).
:- mode det_negation_det(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, prog_data, det_report, det_util.
:- import_module mode_util, globals, options, passes_aux.
:- import_module hlds_out, mercury_to_mercury, instmap.
:- import_module bool, list, map, set, std_util, require.

%-----------------------------------------------------------------------------%

determinism_pass(ModuleInfo0, ModuleInfo) -->
	{ determinism_declarations(ModuleInfo0, DeclaredProcs,
		UndeclaredProcs) },
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(debug_det, Debug),
	( { UndeclaredProcs = [] } ->
		{ ModuleInfo1 = ModuleInfo0 }
	;
		maybe_write_string(Verbose,
			"% Doing determinism inference...\n"),
		global_inference_pass(ModuleInfo0, UndeclaredProcs, Debug,
			ModuleInfo1),
		maybe_write_string(Verbose, "% done.\n")
	),
	maybe_write_string(Verbose, "% Doing determinism checking...\n"),
	global_final_pass(ModuleInfo1, DeclaredProcs, Debug, ModuleInfo),
	maybe_write_string(Verbose, "% done.\n").

determinism_check_proc(ProcId, PredId, ModuleInfo0, ModuleInfo) -->
	globals__io_lookup_bool_option(debug_det, Debug),
	global_final_pass(ModuleInfo0, [proc(PredId, ProcId)], Debug,	
		ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred global_inference_pass(module_info, pred_proc_list, bool, module_info,
	io__state, io__state).
:- mode global_inference_pass(in, in, in, out, di, uo) is det.

	% Iterate until a fixpoint is reached. This can be expensive
	% if a module has many predicates with undeclared determinisms.
	% If this ever becomes a problem, we should switch to doing
	% iterations only on strongly connected components of the
	% dependency graph.

global_inference_pass(ModuleInfo0, ProcList, Debug, ModuleInfo) -->
	global_inference_single_pass(ProcList, Debug, ModuleInfo0, ModuleInfo1,
		[], Msgs, unchanged, Changed),
	maybe_write_string(Debug, "% Inference pass complete\n"),
	( { Changed = changed } ->
		global_inference_pass(ModuleInfo1, ProcList, Debug, ModuleInfo)
	;
		% We have arrived at a fixpoint. Therefore all the messages we
		% have are based on the final determinisms of all procedures,
		% which means it is safe to print them.
		det_report_and_handle_msgs(Msgs, ModuleInfo1, ModuleInfo)
	).

:- pred global_inference_single_pass(pred_proc_list, bool,
	module_info, module_info, list(det_msg), list(det_msg),
	maybe_changed, maybe_changed, io__state, io__state).
:- mode global_inference_single_pass(in, in, in, out, in, out, in, out, di, uo)
	is det.

global_inference_single_pass([], _, ModuleInfo, ModuleInfo, Msgs, Msgs,
		Changed, Changed) --> [].
global_inference_single_pass([proc(PredId, ProcId) | PredProcs], Debug,
		ModuleInfo0, ModuleInfo, Msgs0, Msgs, Changed0, Changed) -->
	globals__io_get_globals(Globals),
	{ det_infer_proc(PredId, ProcId, ModuleInfo0, ModuleInfo1, Globals,
		Detism0, Detism, ProcMsgs) },
	( { Detism = Detism0 } ->
		( { Debug = yes } ->
			io__write_string("% Inferred old detism "),
			mercury_output_det(Detism),
			io__write_string(" for "),
			hlds_out__write_pred_proc_id(ModuleInfo1,
				PredId, ProcId),
			io__write_string("\n")
		;
			[]
		),
		{ Changed1 = Changed0 }
	;
		( { Debug = yes } ->
			io__write_string("% Inferred new detism "),
			mercury_output_det(Detism),
			io__write_string(" for "),
			hlds_out__write_pred_proc_id(ModuleInfo1,
				PredId, ProcId),
			io__write_string("\n")
		;
			[]
		),
		{ Changed1 = changed }
	),
	{ list__append(ProcMsgs, Msgs0, Msgs1) },
	global_inference_single_pass(PredProcs, Debug,
		ModuleInfo1, ModuleInfo, Msgs1, Msgs, Changed1, Changed).

:- pred global_final_pass(module_info, pred_proc_list, bool,
	module_info, io__state, io__state).
:- mode global_final_pass(in, in, in, out, di, uo) is det.

global_final_pass(ModuleInfo0, ProcList, Debug, ModuleInfo) -->
	global_inference_single_pass(ProcList, Debug, ModuleInfo0, ModuleInfo1,
		[], Msgs, unchanged, _),
	det_report_and_handle_msgs(Msgs, ModuleInfo1, ModuleInfo2),
	global_checking_pass(ProcList, ModuleInfo2, ModuleInfo).

%-----------------------------------------------------------------------------%

:- type soln_context	--->	all_solns ; first_soln.

	% Infer the determinism of a procedure.

:- pred det_infer_proc(pred_id, proc_id, module_info, module_info, globals,
	determinism, determinism, list(det_msg)).
:- mode det_infer_proc(in, in, in, out, in, out, out, out) is det.

det_infer_proc(PredId, ProcId, ModuleInfo0, ModuleInfo, Globals,
		Detism0, Detism, Msgs) :-

		% Get the proc_info structure for this procedure
	module_info_preds(ModuleInfo0, Preds0),
	map__lookup(Preds0, PredId, Pred0),
	pred_info_procedures(Pred0, Procs0),
	map__lookup(Procs0, ProcId, Proc0),

		% Remember the old inferred determinism of this procedure
	proc_info_inferred_determinism(Proc0, Detism0),

		% Work out whether the procedure occurs in a single-solution
		% context or not.  Currently we only assume so if
		% the predicate has an explicit determinism declaration
		% that says so.
	(
		proc_info_declared_determinism(Proc0, yes(DeclaredDetism)),
		determinism_components(DeclaredDetism, _, at_most_many_cc)
	->
		SolnContext = first_soln
	;	
		SolnContext = all_solns
	),

		% Infer the determinism of the goal
	proc_info_goal(Proc0, Goal0),
	proc_info_get_initial_instmap(Proc0, ModuleInfo0, InstMap0),
	det_info_init(ModuleInfo0, PredId, ProcId, Globals, DetInfo),
	det_infer_goal(Goal0, InstMap0, SolnContext, DetInfo,
			Goal, Detism1, Msgs),

		% Take the worst of the old and new detisms.
		% This is needed to prevent loops on p :- not(p)
		% at least if the initial assumed detism is det.
	determinism_components(Detism0, CanFail0, MaxSoln0),
	determinism_components(Detism1, CanFail1, MaxSoln1),
	det_switch_canfail(CanFail0, CanFail1, CanFail),
	det_switch_maxsoln(MaxSoln0, MaxSoln1, MaxSoln),
	determinism_components(Detism, CanFail, MaxSoln),

		% Save the newly inferred information
	proc_info_set_goal(Proc0, Goal, Proc1),
	proc_info_set_inferred_determinism(Proc1, Detism, Proc),

		%  Put back the new proc_info structure.
	map__set(Procs0, ProcId, Proc, Procs),
	pred_info_set_procedures(Pred0, Procs, Pred),
	map__set(Preds0, PredId, Pred, Preds),
	module_info_set_preds(ModuleInfo0, Preds, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Infers the determinism of `Goal0' and returns this in `Detism'.
	% It annotates the goal and all its subgoals with their determinism
	% and returns the annotated goal in `Goal'.

:- pred det_infer_goal(hlds__goal, instmap, soln_context, det_info,
	hlds__goal, determinism, list(det_msg)).
:- mode det_infer_goal(in, in, in, in, out, out, out) is det.

det_infer_goal(Goal0 - GoalInfo0, InstMap0, SolnContext0, DetInfo,
		Goal - GoalInfo, Detism, Msgs) :-
	goal_info_get_nonlocals(GoalInfo0, NonLocalVars),
	goal_info_get_instmap_delta(GoalInfo0, DeltaInstMap),

	% If a goal has no output variables, then the goal is in
	% single-solution context

	( det_no_output_vars(NonLocalVars, InstMap0, DeltaInstMap, DetInfo) ->
		OutputVars = no,
		SolnContext = first_soln
	;
		OutputVars = yes,
		SolnContext = SolnContext0
	),

	det_infer_goal_2(Goal0, GoalInfo0, InstMap0, SolnContext, DetInfo,
		NonLocalVars, DeltaInstMap, Goal1, InternalDetism0, Msgs1),

	determinism_components(InternalDetism0, InternalCanFail,
				InternalSolns0),
	(
		% if mode analysis notices that a goal cannot succeed,
		% then determinism analysis should notice this too

		instmap_delta_is_unreachable(DeltaInstMap)
	->
		InternalSolns = at_most_zero
	;
		InternalSolns = InternalSolns0
	),
	determinism_components(InternalDetism, InternalCanFail, InternalSolns),

	(
		% If a goal with multiple solutions has no output variables,
		% then it really it has only one solution
		% (we will need to do pruning)

		( InternalSolns = at_most_many
		; InternalSolns = at_most_many_cc
		),
		OutputVars = no
	->
		Solns = at_most_one
	;
		% If a goal with multiple solutions occurs in a single-solution
		% context, then we will need to do pruning

		InternalSolns = at_most_many,
		SolnContext = first_soln
	->
		Solns = at_most_many_cc
	;
		Solns = InternalSolns
	),
	determinism_components(Detism, InternalCanFail, Solns),
	goal_info_set_determinism(GoalInfo0, Detism, GoalInfo),

	% See how we should introduce the commit operator, if one is needed.

	(
		% do we need a commit?
		Detism \= InternalDetism,

		% for disjunctions, we want to use a semidet
		% or cc_nondet disjunction which avoids creating a
		% choice point at all, rather than wrapping a
		% some [] around a nondet disj, which would
		% create a choice point and then prune it.
		Goal1 \= disj(_, _),	

		% do we already have a commit?
		Goal1 \= some(_, _)
	->
		% a commit needed - we must introduce an explicit `some'
		% so that the code generator knows to insert the appropriate
		% code for pruning
		goal_info_set_determinism(GoalInfo0, InternalDetism, InnerInfo),
		Goal = some([], Goal1 - InnerInfo),
		Msgs = Msgs1
	;
		% either no commit needed, or a `some' already present
		Goal = Goal1,
		Msgs = Msgs1
	).

%-----------------------------------------------------------------------------%

:- pred det_infer_goal_2(hlds__goal_expr, hlds__goal_info, instmap,
	soln_context, det_info, set(var), instmap_delta,
	hlds__goal_expr, determinism, list(det_msg)).
:- mode det_infer_goal_2(in, in, in, in, in, in, in, out, out, out) is det.

	% The determinism of a conjunction is the worst case of the elements
	% of that conjuction.

det_infer_goal_2(conj(Goals0), _, InstMap0, SolnContext, DetInfo, _, _,
		conj(Goals), Detism, Msgs) :-
	det_infer_conj(Goals0, InstMap0, SolnContext, DetInfo,
		Goals, Detism, Msgs).

det_infer_goal_2(disj(Goals0, SM), _, InstMap0, SolnContext, DetInfo, _, _,
		disj(Goals, SM), Detism, Msgs) :-
	det_infer_disj(Goals0, InstMap0, SolnContext, DetInfo,
		can_fail, at_most_zero, Goals, Detism, Msgs).

	% The determinism of a switch is the worst of the determinism of each
	% of the cases. Also, if only a subset of the constructors are handled,
	% then it is semideterministic or worse - this is determined
	% in switch_detection.m and handled via the SwitchCanFail field.

det_infer_goal_2(switch(Var, SwitchCanFail, Cases0, SM), _,
		InstMap0, SolnContext, DetInfo, _, _,
		switch(Var, SwitchCanFail, Cases, SM), Detism, Msgs) :-
	det_infer_switch(Cases0, InstMap0, SolnContext, DetInfo,
		cannot_fail, at_most_zero, Cases, CasesDetism, Msgs),
	determinism_components(CasesDetism, CasesCanFail, CasesSolns),
	det_conjunction_canfail(SwitchCanFail, CasesCanFail, CanFail),
	determinism_components(Detism, CanFail, CasesSolns).

	% For calls, just look up the determinism entry associated with
	% the called predicate.
	% This is the point at which annotations start changing
	% when we iterate to fixpoint for global determinism inference.

det_infer_goal_2(call(PredId, ModeId, A, B, C, N), GoalInfo, _, SolnContext,
		DetInfo, _, _,
		call(PredId, ModeId, A, B, C, N), Detism, Msgs) :-
	det_lookup_detism(DetInfo, PredId, ModeId, Detism),
	%
	% Make sure we don't try to call a committed-choice pred
	% from a non-committed-choice context.
	%
	determinism_components(Detism, _, NumSolns),
	(
		NumSolns = at_most_many_cc,
		SolnContext \= first_soln
	->
		Msgs = [cc_pred_in_wrong_context(GoalInfo, Detism,
				PredId, ModeId)]
	;
		Msgs = []
	).

det_infer_goal_2(higher_order_call(PredVar, ArgVars, Types, Modes, Det),
		GoalInfo, _InstMap0, SolnContext,
		_MiscInfo, _NonLocalVars, _DeltaInstMap,
		higher_order_call(PredVar, ArgVars, Types, Modes, Det),
		Det, Msgs) :-
	determinism_components(Det, _, NumSolns),
	(
		NumSolns = at_most_many_cc,
		SolnContext \= first_soln
	->
		Msgs = [higher_order_cc_pred_in_wrong_context(GoalInfo, Det)]
	;
		Msgs = []
	).

	% unifications are either deterministic or semideterministic.
	% (see det_infer_unify).
det_infer_goal_2(unify(LT, RT0, M, U, C), GoalInfo, InstMap0, _SolnContext,
		DetInfo, _, _, unify(LT, RT, M, U, C), UnifyDet, Msgs) :-
	(
		RT0 = lambda_goal(PredOrFunc, Vars, Modes, LambdaDeclaredDet,
				Goal0)
	->
		(
			determinism_components(LambdaDeclaredDet, _,
				at_most_many_cc)
		->
			LambdaSolnContext = first_soln
		;	
			LambdaSolnContext = all_solns
		),
		det_infer_goal(Goal0, InstMap0, LambdaSolnContext, DetInfo,
				Goal, LambdaInferredDet, Msgs1),
		det_check_lambda(LambdaDeclaredDet, LambdaInferredDet,
				Goal, GoalInfo, DetInfo, Msgs2),
		list__append(Msgs1, Msgs2, Msgs),
		RT = lambda_goal(PredOrFunc, Vars, Modes, LambdaDeclaredDet,
				Goal)
	;
		RT = RT0,
		Msgs = []
	),
	det_infer_unify(U, UnifyDet).

det_infer_goal_2(if_then_else(Vars, Cond0, Then0, Else0, SM), _GoalInfo0,
		InstMap0, SolnContext, DetInfo, _NonLocalVars, _DeltaInstMap,
		if_then_else(Vars, Cond, Then, Else, SM), Detism, Msgs) :-

	% We process the goal right-to-left, doing the `then' before
	% the condition of the if-then-else, so that we can propagate
	% the SolnContext correctly.

	% First process the `then' part
	update_instmap(Cond0, InstMap0, InstMap1),
	det_infer_goal(Then0, InstMap1, SolnContext, DetInfo,
		Then, ThenDetism, ThenMsgs),
	determinism_components(ThenDetism, ThenCanFail, ThenMaxSoln),

	% Next, work out the right soln_context to use for the condition.
	% The condition is in a first_soln context if and only if the goal
	% as a whole was in a first_soln context and the `then' part
	% cannot fail.
	(
		ThenCanFail = cannot_fail,
		SolnContext = first_soln
	->
		CondSolnContext = first_soln
	;
		CondSolnContext = all_solns
	),

	% Process the `condition' part
	det_infer_goal(Cond0, InstMap0, CondSolnContext, DetInfo,
		Cond, CondDetism, CondMsgs),
	determinism_components(CondDetism, CondCanFail, CondMaxSoln),

	% Process the `else' part
	det_infer_goal(Else0, InstMap0, SolnContext, DetInfo,
		Else, ElseDetism, ElseMsgs),
	determinism_components(ElseDetism, ElseCanFail, ElseMaxSoln),

	% Finally combine the results from the three parts
	( CondCanFail = cannot_fail ->
		% A -> B ; C is equivalent to A, B if A cannot fail
		det_conjunction_maxsoln(CondMaxSoln, ThenMaxSoln, MaxSoln),
		det_conjunction_canfail(CondCanFail, ThenCanFail, CanFail)
	; CondMaxSoln = at_most_zero ->
		% A -> B ; C is equivalent to ~A, C if A cannot succeed
		det_negation_det(CondDetism, MaybeNegDetism),
		(
			MaybeNegDetism = no,
			error("cannot find determinism of negated condition")
		;
			MaybeNegDetism = yes(NegDetism)
		),
		determinism_components(NegDetism, NegCanFail, NegMaxSoln),
		det_conjunction_maxsoln(NegMaxSoln, ElseMaxSoln, MaxSoln),
		det_conjunction_canfail(NegCanFail, ElseCanFail, CanFail)
	;
		det_conjunction_maxsoln(CondMaxSoln, ThenMaxSoln, CTMaxSoln),
		det_switch_maxsoln(CTMaxSoln, ElseMaxSoln, MaxSoln),
		det_switch_canfail(ThenCanFail, ElseCanFail, CanFail)
	),

	determinism_components(Detism, CanFail, MaxSoln),
	list__append(ThenMsgs, ElseMsgs, AfterMsgs),
	list__append(CondMsgs, AfterMsgs, Msgs).

	% Negations are almost always semideterministic.  It is an error for
	% a negation to further instantiate any non-local variable. Such
	% errors will be reported by the mode analysis.
	%
	% Question: should we warn about the negation of goals that either
	% cannot succeed or cannot fail?
	% Answer: yes, probably, but it's not a high priority.

det_infer_goal_2(not(Goal0), _, InstMap0, _SolnContext, DetInfo, _, _,
		not(Goal), Det, Msgs) :-
	det_infer_goal(Goal0, InstMap0, first_soln, DetInfo,
		Goal, NegDet, Msgs),
	det_negation_det(NegDet, MaybeDet),
	(
		MaybeDet = no,
		error("inappropriate determinism inside a negation")
	;
		MaybeDet = yes(Det)
	).

	% Existential quantification may require a cut to throw away solutions,
	% but we cannot rely on explicit quantification to detect this.
	% Therefore cuts are handled in det_infer_goal.

det_infer_goal_2(some(Vars, Goal0), _, InstMap0, SolnContext, DetInfo, _, _,
		some(Vars, Goal), Det, Msgs) :-
	det_infer_goal(Goal0, InstMap0, SolnContext, DetInfo,
		Goal, Det, Msgs).

	% pragma c_codes are handled in the same way as predicate calls
det_infer_goal_2(pragma_c_code(C_Code, IsRecursive, PredId, ProcId, Args,
			ArgNameMap), 
		GoalInfo, _, SolnContext, DetInfo, _, _,
		pragma_c_code(C_Code, IsRecursive, PredId, ProcId, Args,
			ArgNameMap),
		Detism, Msgs) :-
	det_lookup_detism(DetInfo, PredId, ProcId, Detism),
	determinism_components(Detism, _, NumSolns),
	( NumSolns = at_most_many_cc, SolnContext \= first_soln ->
		Msgs = [cc_pred_in_wrong_context(GoalInfo, Detism,
				PredId, ProcId)]
	;
		Msgs = []
	).

%-----------------------------------------------------------------------------%

:- pred det_infer_conj(list(hlds__goal), instmap, soln_context, det_info,
		list(hlds__goal), determinism, list(det_msg)).
:- mode det_infer_conj(in, in, in, in, out, out, out) is det.

det_infer_conj([], _InstMap0, _SolnContext, _DetInfo, [], det, []).
det_infer_conj([Goal0 | Goals0], InstMap0, SolnContext, DetInfo, 
		[Goal | Goals], Detism, Msgs) :-

	% We should look to see when we get to a not_reached point
	% and optimize away the remaining elements of the conjunction.
	% But that optimization is done in the code generation anyway.

	% We infer the determinisms right-to-left, so that we can propagate
	% the SolnContext properly.

	%
	% First, process the second and subsequent conjuncts.
	%
	update_instmap(Goal0, InstMap0, InstMap1),
	det_infer_conj(Goals0, InstMap1, SolnContext, DetInfo,
			Goals, DetismB, MsgsB),
	determinism_components(DetismB, CanFailB, MaxSolnsB),

	%
	% Next, work out whether the first conjunct is in a first_soln context
	% or not. We obviously need all its solutions if we need all the
	% solutions of the conjunction. However, even if we need only the
	% first solution of the conjunction, we may need to generate more
	% than one solution of the first conjunct if the later conjuncts
	% may possibly fail.
	%
	( 
		CanFailB = cannot_fail,
		SolnContext = first_soln
	->
		SolnContextA = first_soln
	;
		SolnContextA = all_solns
	),
	%
	% Process the first conjunct.
	%
	det_infer_goal(Goal0, InstMap0, SolnContextA, DetInfo,
			Goal, DetismA, MsgsA),
	determinism_components(DetismA, CanFailA, MaxSolnsA),

	%
	% Finally combine the results computed above.
	%
	det_conjunction_canfail(CanFailA, CanFailB, CanFail),
	det_conjunction_maxsoln(MaxSolnsA, MaxSolnsB, MaxSolns),
	determinism_components(Detism, CanFail, MaxSolns),
	list__append(MsgsA, MsgsB, Msgs).

:- pred det_infer_disj(list(hlds__goal), instmap, soln_context, det_info,
	can_fail, soln_count, list(hlds__goal), determinism, list(det_msg)).
:- mode det_infer_disj(in, in, in, in, in, in, out, out, out) is det.

det_infer_disj([], _InstMap0, _SolnContext, _DetInfo, CanFail, MaxSolns,
		[], Detism, []) :-
	determinism_components(Detism, CanFail, MaxSolns).
det_infer_disj([Goal0 | Goals0], InstMap0, SolnContext, DetInfo, CanFail0,
		MaxSolns0, [Goal | Goals1], Detism, Msgs) :-
	det_infer_goal(Goal0, InstMap0, SolnContext, DetInfo,
			Goal, Detism1, Msgs1),
	determinism_components(Detism1, CanFail1, MaxSolns1),
	det_disjunction_canfail(CanFail0, CanFail1, CanFail2),
	det_disjunction_maxsoln(MaxSolns0, MaxSolns1, MaxSolns2),
	det_infer_disj(Goals0, InstMap0, SolnContext, DetInfo, CanFail2,
		MaxSolns2, Goals1, Detism, Msgs2),
	list__append(Msgs1, Msgs2, Msgs).

:- pred det_infer_switch(list(case), instmap, soln_context, det_info,
	can_fail, soln_count, list(case), determinism, list(det_msg)).
:- mode det_infer_switch(in, in, in, in, in, in, out, out, out) is det.

det_infer_switch([], _InstMap0, _SolnContext, _DetInfo, CanFail, MaxSolns,
		[], Detism, []) :-
	determinism_components(Detism, CanFail, MaxSolns).
det_infer_switch([Case0 | Cases0], InstMap0, SolnContext, DetInfo, CanFail0,
		MaxSolns0, [Case | Cases], Detism, Msgs) :-
	% Technically, we should update the instmap to reflect the
	% knowledge that the var is bound to this particular
	% constructor, but we wouldn't use that information here anyway,
	% so we don't bother.
	Case0 = case(ConsId, Goal0),
	det_infer_goal(Goal0, InstMap0, SolnContext, DetInfo,
			Goal, Detism1, Msgs1),
	Case = case(ConsId, Goal),
	determinism_components(Detism1, CanFail1, MaxSolns1),
	det_switch_canfail(CanFail0, CanFail1, CanFail2),
	det_switch_maxsoln(MaxSolns0, MaxSolns1, MaxSolns2),
	det_infer_switch(Cases0, InstMap0, SolnContext, DetInfo, CanFail2,
		MaxSolns2, Cases, Detism, Msgs2),
	list__append(Msgs1, Msgs2, Msgs).

	% Deconstruction unifications are deterministic if the type
	% only has one constructor, or if the variable is known to be
	% already bound to the appropriate functor.
	% 
	% This is handled (modulo bugs) by modes.m, which sets
	% the determinism field in the deconstruct(...) to semidet for
	% those deconstruction unifications which might fail.
	% But switch_detection.m may set it back to det again, if it moves
	% the functor test into a switch instead.

:- pred det_infer_unify(unification, determinism).
:- mode det_infer_unify(in, out) is det.

det_infer_unify(deconstruct(_, _, _, _, CanFail), Detism) :-
	determinism_components(Detism, CanFail, at_most_one).
det_infer_unify(assign(_, _), det).
det_infer_unify(construct(_, _, _, _), det).
det_infer_unify(simple_test(_, _), semidet).
det_infer_unify(complicated_unify(_, CanFail), Detism) :-
	determinism_components(Detism, CanFail, at_most_one).

%-----------------------------------------------------------------------------%

% For the at_most_zero, at_most_one, at_most_many,
% we're just doing abstract interpretation to count
% the number of solutions.  Similarly, for the can_fail
% and cannot_fail components, we're doing abstract
% interpretation to count the possible number of failures.
% If the num_solns is at_most_many_cc, this means that
% the goal might have many logical solutions if there were no
% pruning, but that the goal occurs in a single-solution
% context, so only the first solution will be returned.

det_conjunction_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_one,     at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_many_cc, at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_many,    at_most_zero).

det_conjunction_maxsoln(at_most_one,     at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_one,     at_most_one,     at_most_one).
det_conjunction_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_conjunction_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_conjunction_maxsoln(at_most_many_cc, at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_conjunction_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_conjunction_maxsoln(at_most_many_cc, at_most_many,    _) :-
    % if the first conjunct could be cc pruned,
    % the second conj ought to have been cc pruned too
	error("det_conjunction_maxsoln: many_cc , many").

det_conjunction_maxsoln(at_most_many,    at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_conjunction_maxsoln(at_most_many,    at_most_many_cc, at_most_many).
det_conjunction_maxsoln(at_most_many,    at_most_many,    at_most_many).

det_conjunction_canfail(can_fail,    can_fail,    can_fail).
det_conjunction_canfail(can_fail,    cannot_fail, can_fail).
det_conjunction_canfail(cannot_fail, can_fail,    can_fail).
det_conjunction_canfail(cannot_fail, cannot_fail, cannot_fail).

det_disjunction_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_disjunction_maxsoln(at_most_zero,    at_most_one,     at_most_one).
det_disjunction_maxsoln(at_most_zero,    at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_zero,    at_most_many,    at_most_many).

det_disjunction_maxsoln(at_most_one,     at_most_zero,    at_most_one).
det_disjunction_maxsoln(at_most_one,     at_most_one,     at_most_many).
det_disjunction_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_disjunction_maxsoln(at_most_many_cc, at_most_zero,    at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_many,    _) :-
    % if the first disjunct could be cc pruned,
    % the second disjunct ought to have been cc pruned too
    error("det_disjunction_maxsoln: cc in first case, not cc in second case").

det_disjunction_maxsoln(at_most_many,    at_most_zero,    at_most_many).
det_disjunction_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_disjunction_maxsoln(at_most_many,    at_most_many_cc, _) :-
    % if the first disjunct could be cc pruned,
    % the second disjunct ought to have been cc pruned too
    error("det_disjunction_maxsoln: cc in second case, not cc in first case").
det_disjunction_maxsoln(at_most_many,    at_most_many,    at_most_many).

det_disjunction_canfail(can_fail,    can_fail,    can_fail).
det_disjunction_canfail(can_fail,    cannot_fail, cannot_fail).
det_disjunction_canfail(cannot_fail, can_fail,    cannot_fail).
det_disjunction_canfail(cannot_fail, cannot_fail, cannot_fail).

det_switch_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_switch_maxsoln(at_most_zero,    at_most_one,     at_most_one).
det_switch_maxsoln(at_most_zero,    at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_zero,    at_most_many,    at_most_many).

det_switch_maxsoln(at_most_one,     at_most_zero,    at_most_one).
det_switch_maxsoln(at_most_one,     at_most_one,     at_most_one).
det_switch_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_switch_maxsoln(at_most_many_cc, at_most_zero,    at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_many,    _) :-
	% if the first case could be cc pruned,
	% the second case ought to have been cc pruned too
	error("det_switch_maxsoln: cc in first case, not cc in second case").

det_switch_maxsoln(at_most_many,    at_most_zero,    at_most_many).
det_switch_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_switch_maxsoln(at_most_many,    at_most_many_cc, _) :-
	% if the first case could be cc pruned,
	% the second case ought to have been cc pruned too
	error("det_switch_maxsoln: cc in second case, not cc in first case").
det_switch_maxsoln(at_most_many,    at_most_many,    at_most_many).

det_switch_canfail(can_fail,    can_fail,    can_fail).
det_switch_canfail(can_fail,    cannot_fail, can_fail).
det_switch_canfail(cannot_fail, can_fail,    can_fail).
det_switch_canfail(cannot_fail, cannot_fail, cannot_fail).

det_negation_det(det,		yes(failure)).
det_negation_det(semidet,	yes(semidet)).
det_negation_det(multidet,	no).
det_negation_det(nondet,	no).
det_negation_det(cc_multidet,	no).
det_negation_det(cc_nondet,	no).
det_negation_det(erroneous,	yes(erroneous)).
det_negation_det(failure,	yes(det)).

%-----------------------------------------------------------------------------%

	% determinism_declarations takes a module_info as input and
	% returns two lists of procedure ids, the first being those
	% with determinism declarations, and the second being those without.

:- pred determinism_declarations(module_info, pred_proc_list, pred_proc_list).
:- mode determinism_declarations(in, out, out) is det.

determinism_declarations(ModuleInfo, DeclaredProcs, UndeclaredProcs) :-
	get_all_pred_procs(ModuleInfo, PredProcs),
	segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs).

	% get_all_pred_procs takes a module_info and returns a list
	% of all the procedures ids for that module.

:- pred get_all_pred_procs(module_info, pred_proc_list).
:- mode get_all_pred_procs(in, out) is det.

get_all_pred_procs(ModuleInfo, PredProcs) :-
	module_info_predids(ModuleInfo, PredIds),
	module_info_preds(ModuleInfo, Preds),
	get_all_pred_procs_2(Preds, PredIds, [], PredProcs).

:- pred get_all_pred_procs_2(pred_table, list(pred_id),
				pred_proc_list, pred_proc_list).
:- mode get_all_pred_procs_2(in, in, in, out) is det.

get_all_pred_procs_2(_Preds, [], PredProcs, PredProcs).
get_all_pred_procs_2(Preds, [PredId|PredIds], PredProcs0, PredProcs) :-
	map__lookup(Preds, PredId, Pred),
	pred_info_non_imported_procids(Pred, ProcIds),
	fold_pred_modes(PredId, ProcIds, PredProcs0, PredProcs1),
	get_all_pred_procs_2(Preds, PredIds, PredProcs1, PredProcs).

:- pred fold_pred_modes(pred_id, list(proc_id), pred_proc_list, pred_proc_list).
:- mode fold_pred_modes(in, in, in, out) is det.

fold_pred_modes(_PredId, [], PredProcs, PredProcs).
fold_pred_modes(PredId, [ProcId|ProcIds], PredProcs0, PredProcs) :-
	fold_pred_modes(PredId, ProcIds, [proc(PredId, ProcId) | PredProcs0],
		PredProcs).

	% segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs)
	% splits the list of procedures PredProcs into DeclaredProcs and
	% UndeclaredProcs.

:- pred segregate_procs(module_info, pred_proc_list, pred_proc_list,
	pred_proc_list).
:- mode segregate_procs(in, in, out, out) is det.

segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs) :-
	segregate_procs_2(ModuleInfo, PredProcs, [], DeclaredProcs,
					[], UndeclaredProcs).

:- pred segregate_procs_2(module_info, pred_proc_list, pred_proc_list,
			pred_proc_list, pred_proc_list, pred_proc_list).
:- mode segregate_procs_2(in, in, in, out, in, out) is det.

segregate_procs_2(_ModuleInfo, [], DeclaredProcs, DeclaredProcs,
				UndeclaredProcs, UndeclaredProcs).
segregate_procs_2(ModuleInfo, [proc(PredId, ProcId) | PredProcs],
		DeclaredProcs0, DeclaredProcs,
		UndeclaredProcs0, UndeclaredProcs) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, Pred),
	pred_info_procedures(Pred, Procs),
	map__lookup(Procs, ProcId, Proc),
	proc_info_declared_determinism(Proc, MaybeDetism),
	(
		MaybeDetism = no,
		UndeclaredProcs1 = [proc(PredId, ProcId) | UndeclaredProcs0],
		DeclaredProcs1 = DeclaredProcs0
	;
		MaybeDetism = yes(_),
		DeclaredProcs1 = [proc(PredId, ProcId) | DeclaredProcs0],
		UndeclaredProcs1 = UndeclaredProcs0
	),
	segregate_procs_2(ModuleInfo, PredProcs, DeclaredProcs1, DeclaredProcs,
		UndeclaredProcs1, UndeclaredProcs).

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: disj_gen.m:
%
% Main authors: conway, zs.
%
% The predicates of this module generate code for disjunctions.
%
% The handling of model_det and model_semi disjunctions is almost identical.
% The handling of model_non disjunctions is also quite similar.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module disj_gen.

:- interface.

:- import_module hlds_goal, llds, code_info.

:- pred disj_gen__generate_det_disj(list(hlds__goal), store_map,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_det_disj(in, in, out, in, out) is det.

:- pred disj_gen__generate_semi_disj(list(hlds__goal), store_map,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_semi_disj(in, in, out, in, out) is det.

:- pred disj_gen__generate_non_disj(list(hlds__goal), store_map,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_non_disj(in, in, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_data, code_gen, code_util, options, globals.
:- import_module bool, set, tree, list, map, std_util, require.

%---------------------------------------------------------------------------%

disj_gen__generate_det_disj(Goals, StoreMap, Code) -->
	disj_gen__generate_pruned_disj(Goals, StoreMap, Code).

disj_gen__generate_semi_disj(Goals, StoreMap, Code) -->
	( { Goals = [] } ->
		code_info__generate_failure(Code)
	;
		disj_gen__generate_pruned_disj(Goals, StoreMap, Code)
	).

%---------------------------------------------------------------------------%

:- pred disj_gen__generate_pruned_disj(list(hlds__goal), store_map,
	code_tree, code_info, code_info).
:- mode disj_gen__generate_pruned_disj(in, in, out, in, out) is det.

disj_gen__generate_pruned_disj(Goals, StoreMap, Code) -->
		% If we are using constraints, save the current solver state
		% before the first disjunct.
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals, reclaim_heap_on_semidet_failure,
		ReclaimHeap) },
	{ globals__lookup_bool_option(Globals, constraints, Constraints) },
	code_info__maybe_save_ticket(Constraints, SaveTicketCode),

		% Rather than saving the heap pointer here,
		% we delay saving it until we get to the first
		% disjunct that might allocate some heap space.
	{ SavedHP = no },	% we haven't yet saved the heap pointer
	{ MustRestoreHP = no },	% we won't need to restore yet

		% Generate all the disjuncts
	code_info__get_next_label(EndLabel),
	disj_gen__generate_pruned_disjuncts(Goals, StoreMap, EndLabel, no,
		SavedHP, MustRestoreHP, ReclaimHeap, Constraints, GoalsCode),

		% Remake the code_info using the store map for the
		% variable locations at the end of the disjunction.
	code_info__remake_with_store_map(StoreMap),

	{ Code = tree(SaveTicketCode, GoalsCode) }.

%---------------------------------------------------------------------------%

:- pred disj_gen__generate_pruned_disjuncts(list(hlds__goal), store_map,
	label, bool, bool, bool, bool, bool, code_tree, code_info, code_info).
:- mode disj_gen__generate_pruned_disjuncts(in, in, in, in, in, in, in, in, out,
	in, out) is det.

	% To generate code for a det or semidet disjunction,
	% we generate a chain of goals if-then-else style
	% until we come to a goal without a resume point.
	% That goal is the last in the chain that we need to
	% generate code for. (This is figured out by the liveness pass.)
	%
	% For a semidet disj, this goal will be semidet,
	% and will be followed by no other goal.
	% For a det disj, this goal will be det,
	% and may be followed by other goals.
	%
	% XXX We ought not to restore anything in the first disjunct.

disj_gen__generate_pruned_disjuncts([], _, _, _, _, _, _, _, _) -->
	{ error("Empty pruned disjunction!") }.
disj_gen__generate_pruned_disjuncts([Goal0 | Goals], StoreMap, EndLabel,
		HaveTempFrame0, SavedHP, MustRestoreHP, ReclaimHeap,
		Constraints, Code) -->
	{ Goal0 = GoalExpr0 - GoalInfo0 },
	{ goal_info_get_code_model(GoalInfo0, CodeModel) },
	{ goal_info_get_resume_point(GoalInfo0, Resume) },
	(
		{ Resume = resume_point(ResumeVars, ResumeLocs) }
	->
		% Emit code for a non-last disjunct, including setting things
		% up for the execution of the next disjunct.

		code_info__push_resume_point_vars(ResumeVars),
		code_info__make_known_failure_cont(ResumeVars, ResumeLocs, no,
			HaveTempFrame0, HaveTempFrame, ModContCode),
			% The next line is to enable Goal to pass the
			% pre_goal_update sanity check
		{ goal_info_set_resume_point(GoalInfo0, no_resume_point,
			GoalInfo) },
		{ Goal = GoalExpr0 - GoalInfo },

			% Reset the heap pointer to recover memory allocated
			% by the previous disjunct, if necessary
		code_info__maybe_get_old_hp(MustRestoreHP, RestoreHPCode),

			% If this disjunct might allocate heap space, then
			% we must restore the HP on entry to the next one.
		{ ReclaimHeap = yes, code_util__goal_may_allocate_heap(Goal) ->
			MustRestoreHP_Next = yes
		;
			MustRestoreHP_Next = no
		},

			% If we are going to need to restore the HP,
			% and we haven't saved it already, then we must
			% save it now.
		( { MustRestoreHP_Next = yes, SavedHP = no } ->
			code_info__save_hp(SaveHPCode),
			{ SavedHP_Next = yes }
		;
			{ SaveHPCode = empty },
			{ SavedHP_Next = SavedHP }
		),

			% Reset the solver state if necessary
		code_info__maybe_restore_ticket(Constraints,
			RestoreTicketCode),

		code_info__grab_code_info(CodeInfo),

			% generate the disjunct as a semi-deterministic goal
		{ CodeModel = model_semi ->
			true
		;
			error("pruned disj non-last goal is not semidet")
		},
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		code_info__generate_branch_end(CodeModel, StoreMap, SaveCode),
			% Kill any variables made zombies by the goal
			% XXX should not be necessary, since the state
			% we set up will be discarded anyway
		code_info__pickup_zombies(Zombies),
		code_info__make_vars_dead(Zombies),

		{ BranchCode = node([
			goto(label(EndLabel)) -
				"skip to end of pruned disj"
		]) },

		code_info__slap_code_info(CodeInfo),
		code_info__pop_resume_point_vars,
		code_info__restore_failure_cont(RestoreContCode),

		disj_gen__generate_pruned_disjuncts(Goals, StoreMap, EndLabel,
			HaveTempFrame, SavedHP_Next, MustRestoreHP_Next,
			ReclaimHeap, Constraints, RestCode),

		{ Code = tree(ModContCode, 
			 tree(RestoreHPCode,
			 tree(SaveHPCode,
			 tree(RestoreTicketCode,
			 tree(GoalCode,
			 tree(SaveCode,
			 tree(BranchCode,
			 tree(RestoreContCode,
			      RestCode)))))))) }
	;
		% Emit code for the last disjunct

			% Restore the heap pointer if necessary,
			% and pop the temp stack that we saved it on
			% if we saved it
		code_info__maybe_get_old_hp(MustRestoreHP, RestoreHPCode),
		code_info__maybe_pop_stack(SavedHP, UnSaveHPCode),

			% Restore the solver state if necessary
		code_info__maybe_restore_ticket_and_pop(Constraints, 
			RestorePopTicketCode),

			% Generate the goal
		code_gen__generate_goal(CodeModel, Goal0, GoalCode),
		code_info__generate_branch_end(CodeModel, StoreMap, SaveCode),
			% Kill any variables made zombies by the goal
			% XXX should not be necessary, since we are not
			% coming out from under a restore anyway
		code_info__pickup_zombies(Zombies),
		code_info__make_vars_dead(Zombies),

		{ EndCode = node([
			label(EndLabel) - "End of pruned disj"
		]) },
		{ Code = tree(RestoreHPCode,
			 tree(UnSaveHPCode,
			 tree(RestorePopTicketCode,
			 tree(GoalCode,
			 tree(SaveCode,
			      EndCode))))) }
	).

%---------------------------------------------------------------------------%

disj_gen__generate_non_disj(Goals, StoreMap, Code) -->

		% Sanity check
	{
		Goals = [],
		error("empty disjunction shouldn't be non-det")
	;
		Goals = [_],
		error("singleton disjunction")
	;
		Goals = [_, _ | _]
	},

		% If we are using constraints, save the current solver state
		% before the first disjunct.
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals, constraints, Constraints) },
	code_info__maybe_save_ticket(Constraints, SaveTicketCode),

		% With non-det disjunctions, we must recover memory across
		% all disjuncts, since we can backtract to disjunct N
		% even after control leaves disjunct N-1.
	{ globals__lookup_bool_option(Globals, reclaim_heap_on_nondet_failure,
		ReclaimHeap) },
	code_info__maybe_save_hp(ReclaimHeap, SaveHeapCode),

	code_info__get_next_label(EndLabel),
	disj_gen__generate_non_disjuncts(Goals, StoreMap, EndLabel, no,
		ReclaimHeap, Constraints, GoalsCode),

		% since we don't know which disjunct we have come from
		% we must set the current failure continuation to unkown.

	code_info__unset_failure_cont(FlushResumeVarsCode),
	code_info__remake_with_store_map(StoreMap),
	{ Code = tree(SaveTicketCode,
		 tree(SaveHeapCode,
		 tree(GoalsCode,
		      FlushResumeVarsCode))) }.

%---------------------------------------------------------------------------%

	% XXX We ought not to restore anything in the first disjunct.
	%
	% XXX We ought to be able to pass information between the calls to
	% make_known_failure_cont and restore_failure_cont, in order to
	% prevent the repeated construction and discarding of a temporary
	% nondet frame.

:- pred disj_gen__generate_non_disjuncts(list(hlds__goal), store_map, label,
	bool, bool, bool, code_tree, code_info, code_info).
:- mode disj_gen__generate_non_disjuncts(in, in, in, in, in, in, out, in, out)
	is det.

disj_gen__generate_non_disjuncts([], _, _, _, _, _, _) -->
	{ error("empty nondet disjunction!") }.
disj_gen__generate_non_disjuncts([Goal0 | Goals], StoreMap, EndLabel,
		HaveTempFrame0, ReclaimHeap, Constraints, Code) -->

	{ Goal0 = GoalExpr0 - GoalInfo0 },
	{ goal_info_get_resume_point(GoalInfo0, Resume) },
	(
		{ Resume = resume_point(ResumeVars, ResumeLocs) }
	->
		% Emit code for a non-last disjunct, including setting things
		% up for the execution of the next disjunct.

		code_info__push_resume_point_vars(ResumeVars),
		code_info__make_known_failure_cont(ResumeVars, ResumeLocs, yes,
			HaveTempFrame0, HaveTempFrame, ModContCode),
			% The next line is to enable Goal to pass the
			% pre_goal_update sanity check
		{ goal_info_set_resume_point(GoalInfo0, no_resume_point,
			GoalInfo) },
		{ Goal = GoalExpr0 - GoalInfo },

			% Reset the heap pointer to recover memory allocated
			% by the previous disjunct, if necessary
		code_info__maybe_get_old_hp(ReclaimHeap, RestoreHPCode),

			% Reset the solver state if necessary
		code_info__maybe_restore_ticket(Constraints,
			RestoreTicketCode),

		code_info__grab_code_info(CodeInfo),

		code_gen__generate_goal(model_non, Goal, GoalCode),
		code_info__generate_branch_end(model_non, StoreMap, SaveCode),

			% make sure every variable in the resume set is in its
			% stack slot
		code_info__flush_resume_vars_to_stack(FlushResumeVarsCode),

		% make sure the redoip of the top frame points to the
		% right label
		% XXX code missing


			% Kill any variables made zombies by the goal
			% XXX should not be necessary, since the state
			% we set up will be discarded anyway
		code_info__pickup_zombies(Zombies),
		code_info__make_vars_dead(Zombies),

		{ BranchCode = node([
			goto(label(EndLabel)) -
				"skip to end of nondet disj"
		]) },

		code_info__slap_code_info(CodeInfo),
		code_info__pop_resume_point_vars,
		code_info__restore_failure_cont(RestoreContCode),

		disj_gen__generate_non_disjuncts(Goals, StoreMap, EndLabel,
			HaveTempFrame, ReclaimHeap, Constraints, RestCode),

		{ Code = tree(ModContCode, 
			 tree(RestoreHPCode,
			 tree(RestoreTicketCode,
			 tree(GoalCode,
			 tree(SaveCode,
			 tree(FlushResumeVarsCode,
			 tree(BranchCode,
			 tree(RestoreContCode,
			      RestCode)))))))) }
	;
		% Emit code for the last disjunct

		{ Goals = [] ->
			true
		;
			error("disj_gen__generate_non_disjuncts: last disjunct followed by others")
		},

			% Restore the heap pointer if necessary,
			% and pop the temp stack that we saved it on
			% if we saved it
		code_info__maybe_get_old_hp(ReclaimHeap, RestoreHPCode),
		code_info__maybe_pop_stack(ReclaimHeap, UnSaveHPCode),

			% Restore the solver state if necessary
		code_info__maybe_restore_ticket_and_pop(Constraints,
			RestorePopTicketCode),

		code_gen__generate_goal(model_non, Goal0, GoalCode),
		code_info__generate_branch_end(model_non, StoreMap, SaveCode),

			% Kill any variables made zombies by the goal
			% XXX should not be necessary, since we are not
			% coming out from under a restore anyway
		code_info__pickup_zombies(Zombies),
		code_info__make_vars_dead(Zombies),

		{ EndCode = node([
			label(EndLabel) - "End of pruned disj"
		]) },
		{ Code = tree(RestoreHPCode,
			 tree(UnSaveHPCode,
			 tree(RestorePopTicketCode,
			 tree(GoalCode,
			 tree(SaveCode,
			      EndCode))))) }
	).

%---------------------------------------------------------------------------%

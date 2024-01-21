%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Code generation - do middle recursion optimization
% Main authors: zs and conway.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module middle_rec.

:- interface.

:- import_module hlds_goal, llds, code_info.

:- pred middle_rec__match_and_generate(hlds__goal, code_tree,
	code_info, code_info).
:- mode middle_rec__match_and_generate(in, out, in, out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_module, hlds_data.
:- import_module code_gen, unify_gen, code_util, code_aux, opt_util.
:- import_module bool, set, int, std_util, tree, list, assoc_list, require.

%---------------------------------------------------------------------------%

middle_rec__match_and_generate(Goal, Instrs, CodeInfo0, CodeInfo) :-
	Goal = GoalExpr - GoalInfo,
	(
		GoalExpr = switch(Var, cannot_fail, [Case1, Case2], SM),
		Case1 = case(ConsId1, Goal1),
		Case2 = case(ConsId2, Goal2),
		(
			code_aux__contains_only_builtins(Goal1),
			code_aux__contains_simple_recursive_call(Goal2,
				CodeInfo0, _)
		->
			middle_rec__generate_switch(Var, ConsId1, Goal1, Goal2,
				SM, GoalInfo, Instrs, CodeInfo0, CodeInfo)
		;
			code_aux__contains_only_builtins(Goal2),
			code_aux__contains_simple_recursive_call(Goal1,
				CodeInfo0, _)
		->
			middle_rec__generate_switch(Var, ConsId2, Goal2, Goal1,
				SM, GoalInfo, Instrs, CodeInfo0, CodeInfo)
		;
			fail
		)
	;
		GoalExpr = if_then_else(Vars, Cond, Then, Else, SM),
		(
			code_aux__contains_only_builtins(Cond),
			code_aux__contains_only_builtins(Then),
			code_aux__contains_simple_recursive_call(Else,
				CodeInfo0, no)
		->
			semidet_fail,
			middle_rec__generate_ite(Vars, Cond, Then, Else,
				in_else, SM, GoalInfo, Instrs,
				CodeInfo0, CodeInfo)
		;
			code_aux__contains_only_builtins(Cond),
			code_aux__contains_simple_recursive_call(Then,
				CodeInfo0, no),
			code_aux__contains_only_builtins(Else)
		->
			semidet_fail,
			middle_rec__generate_ite(Vars, Cond, Then, Else,
				in_then, SM, GoalInfo, Instrs,
				CodeInfo0, CodeInfo)
		;
			fail
		)
	).

:- type ite_rec	--->	in_then ; in_else.

%---------------------------------------------------------------------------%

:- pred middle_rec__generate_ite(list(var), hlds__goal, hlds__goal,
	hlds__goal, ite_rec, store_map, hlds__goal_info,
	code_tree, code_info, code_info).
:- mode middle_rec__generate_ite(in, in, in, in, in, in, in, out, in, out)
	is det.

middle_rec__generate_ite(_Vars, _Cond, _Then, _Else, _Rec, _IteGoalInfo, _SM,
		Instrs) -->
	( { semidet_fail } ->
		{ Instrs = empty }
	;
		{ error("middle_rec__generate_ite reached") }
	).

%---------------------------------------------------------------------------%

:- pred middle_rec__generate_switch(var, cons_id, hlds__goal, hlds__goal,
	store_map, hlds__goal_info, code_tree, code_info, code_info).
:- mode middle_rec__generate_switch(in, in, in, in, in, in, out, in, out)
	is semidet.

middle_rec__generate_switch(Var, BaseConsId, Base, Recursive, StoreMap,
		SwitchGoalInfo, Instrs) -->
	code_info__get_stack_slots(StackSlots),
	code_info__get_varset(VarSet),
	{ code_aux__explain_stack_slots(StackSlots, VarSet, SlotsComment) },
	code_info__get_module_info(ModuleInfo),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, no,
		EntryLabel) },

	code_aux__pre_goal_update(SwitchGoalInfo, no),
	unify_gen__generate_tag_test(Var, BaseConsId, branch_on_success,
		BaseLabel, EntryTestCode),
	{ tree__flatten(EntryTestCode, EntryTestListList) },
	{ list__condense(EntryTestListList, EntryTestList) },

	code_info__grab_code_info(CodeInfo),
	code_gen__generate_goal(model_det, Base, BaseGoalCode),
	code_info__generate_branch_end(model_det, StoreMap, BaseSaveCode),
	code_info__slap_code_info(CodeInfo),
	code_gen__generate_goal(model_det, Recursive, RecGoalCode),
	code_info__generate_branch_end(model_det, StoreMap, RecSaveCode),

	code_aux__post_goal_update(SwitchGoalInfo),
	code_info__remake_with_store_map(StoreMap),

	code_info__get_arginfo(ArgModes),
	code_info__get_headvars(HeadVars),
	{ assoc_list__from_corresponding_lists(HeadVars, ArgModes, Args) },
	code_info__setup_call(Args, callee, EpilogCode),

	{ code_gen__output_args(Args, LiveArgs) },

	{ BaseCode = tree(BaseGoalCode, tree(BaseSaveCode, EpilogCode)) },
	{ RecCode = tree(RecGoalCode, tree(RecSaveCode, EpilogCode)) },
	{ LiveValCode = [livevals(LiveArgs) - ""] },

	{ tree__flatten(BaseCode, BaseListList) },
	{ list__condense(BaseListList, BaseList) },
	{ tree__flatten(RecCode, RecListList) },
	{ list__condense(RecListList, RecList) },

	{ list__append(BaseList, RecList, AvoidList) },
	{ middle_rec__find_unused_register(AvoidList, AuxReg) },

	{ middle_rec__split_rec_code(RecList, BeforeList0, AfterList) },
	{ middle_rec__add_counter_to_livevals(BeforeList0, AuxReg,
		BeforeList) },

	{ middle_rec__generate_downloop_test(EntryTestList,
		Loop1Label, Loop1Test) },

	code_info__get_next_label(Loop1Label),
	code_info__get_next_label(Loop2Label),
	code_info__get_total_stackslot_count(FrameSize),

	{ FrameSize = 0 ->
		MaybeIncrSp = [],
		MaybeDecrSp = [],
		InitAuxReg = [assign(AuxReg, const(int_const(0)))
				- "initialize counter register"],
		IncrAuxReg = [assign(AuxReg, binop((+),
				lval(AuxReg),
				const(int_const(1))))
				- "increment loop counter"],
		DecrAuxReg = [assign(AuxReg, binop((-),
				lval(AuxReg),
				const(int_const(1))))
				- "decrement loop counter"],
		TestAuxReg = [if_val(binop((>),
				lval(AuxReg), const(int_const(0))),
				label(Loop2Label))
				- "test on upward loop"]
	;
		MaybeIncrSp = [incr_sp(FrameSize, PredName) - ""],
		MaybeDecrSp = [decr_sp(FrameSize) - ""],
		InitAuxReg =  [assign(AuxReg, lval(sp))
				- "initialize counter register"],
		IncrAuxReg = [],
		DecrAuxReg = [],
		TestAuxReg = [if_val(binop((>),
				lval(sp), lval(AuxReg)),
				label(Loop2Label))
				- "test on upward loop"]
	},

	% Even though the recursive call is followed by some goals
	% in the HLDS, these goals may generate no LLDS code, so
	% it is in fact possible for AfterList to be empty.
	% There is no point in testing BeforeList for empty,
	% since if it is, the code is an infinite loop anyway.

	{ AfterList = [] ->
		list__condense([
			[
				label(EntryLabel) - "Procedure entry point",
				comment(SlotsComment) - ""
			],
			EntryTestList,
			[
				label(Loop1Label)
					- "start of the down loop"
			],
			BeforeList,
			Loop1Test,
			[
				label(BaseLabel)
					- "start of base case"
			],
			BaseList,
			LiveValCode,
			[
				goto(succip)	
				- "exit from base case"
			]
		], InstrList)
	;
		% The instruction list we are constructing has two copies
		% of BaseList. If this list of instructions defines any
		% labels, we must either not apply this version of the
		% optimization, or we must consistently substitute the
		% labels (which will be referred to only from within the
		% BaseList instructions themselves). We choose the former
		% course.
		middle_rec__find_labels(BaseList, BaseLabels),
		BaseLabels = [],
		list__condense([
			[
				label(EntryLabel) - "Procedure entry point",
				comment(SlotsComment) - ""
			],
			EntryTestList,
			InitAuxReg,
			[
				label(Loop1Label)
					- "start of the down loop"
			],
			MaybeIncrSp,
			IncrAuxReg,
			BeforeList,
			Loop1Test,
			BaseList,
			[
				label(Loop2Label) - ""
			],
			AfterList,
			MaybeDecrSp,
			DecrAuxReg,
			TestAuxReg,
			LiveValCode,
			[
				goto(succip)	
					- "exit from recursive case",
				label(BaseLabel)
					- "start of base case"
			],
			BaseList,
			LiveValCode,
			[
				goto(succip)	
				- "exit from base case"
			]
		], InstrList)
	},
	{ Instrs = node(InstrList) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred middle_rec__generate_downloop_test(list(instruction), label,
	list(instruction)).
:- mode middle_rec__generate_downloop_test(in, in, out) is det.

middle_rec__generate_downloop_test([], _, _) :-
	error("middle_rec__generate_downloop_test on empty list").
middle_rec__generate_downloop_test([Instr0 | Instrs0], Target, Instrs) :-
	( Instr0 = if_val(Test, _OldTarget) - _Comment ->
		( Instrs0 = [] ->
			true
		;
			error("middle_rec__generate_downloop_test: if_val followed by other instructions")
		),
		code_util__neg_rval(Test, NewTest),
		Instrs = [if_val(NewTest, label(Target)) - "test on downward loop"]
	;
		middle_rec__generate_downloop_test(Instrs0, Target, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%---------------------------------------------------------------------------%

:- pred middle_rec__split_rec_code(list(instruction),
	list(instruction), list(instruction)).
:- mode middle_rec__split_rec_code(in, out, out) is det.

middle_rec__split_rec_code([], _, _) :-
	error("did not find call in middle_rec__split_rec_code").
middle_rec__split_rec_code([Instr0 | Instrs1], Before, After) :-
	( Instr0 = call(_, _, _, _) - _ ->
		(
			opt_util__skip_comments(Instrs1, Instrs2),
			Instrs2 = [Instr2 | Instrs3],
			Instr2 = label(_) - _
		->
			Before = [],
			After = Instrs3
		;
			error("call not followed by label in middle_rec__split_rec_code")
		)
	;
		middle_rec__split_rec_code(Instrs1, Before1, After),
		Before = [Instr0 | Before1]
	).

%---------------------------------------------------------------------------%

:- pred middle_rec__add_counter_to_livevals(list(instruction), lval,
						list(instruction)).
:- mode middle_rec__add_counter_to_livevals(in, in, out) is det.

middle_rec__add_counter_to_livevals([], _Lval, []).
middle_rec__add_counter_to_livevals([I0|Is0], Lval, [I|Is]) :-
	(
		I0 = livevals(Lives0) - Comment
	->
		set__insert(Lives0, Lval, Lives),
		I = livevals(Lives) - Comment
	;
		I = I0
	),
	middle_rec__add_counter_to_livevals(Is0, Lval, Is).

%---------------------------------------------------------------------------%

:- pred middle_rec__find_unused_register(list(instruction), lval).
:- mode middle_rec__find_unused_register(in, out) is det.

middle_rec__find_unused_register(Instrs, UnusedReg) :-
	set__init(Used0),
	middle_rec__find_used_registers(Instrs, Used0, Used1),
	set__to_sorted_list(Used1, UsedList),
	middle_rec__find_unused_register_2(UsedList, 1, UnusedReg).

:- pred middle_rec__find_unused_register_2(list(int), int, lval).
:- mode middle_rec__find_unused_register_2(in, in, out) is det.

middle_rec__find_unused_register_2([], N, reg(r(N))).
middle_rec__find_unused_register_2([H | T], N, Reg) :-
	( N < H ->
		Reg = reg(r(N))
	;
		N1 is N + 1,
		middle_rec__find_unused_register_2(T, N1, Reg)
	).

:- pred middle_rec__find_used_registers(list(instruction), set(int), set(int)).
:- mode middle_rec__find_used_registers(in, di, uo) is det.

middle_rec__find_used_registers([], Used, Used).
middle_rec__find_used_registers([Instr - _ | Instrs], Used0, Used) :-
	middle_rec__find_used_registers_instr(Instr, Used0, Used1),
	middle_rec__find_used_registers(Instrs, Used1, Used).

:- pred middle_rec__find_used_registers_instr(instr, set(int), set(int)).
:- mode middle_rec__find_used_registers_instr(in, di, uo) is det.

middle_rec__find_used_registers_instr(comment(_), Used, Used).
middle_rec__find_used_registers_instr(livevals(LvalSet), Used0, Used) :-
	set__to_sorted_list(LvalSet, LvalList),
	middle_rec__find_used_registers_lvals(LvalList, Used0, Used).
middle_rec__find_used_registers_instr(block(_, _, Instrs), Used0, Used) :-
	middle_rec__find_used_registers(Instrs, Used0, Used).
middle_rec__find_used_registers_instr(assign(Lval, Rval), Used0, Used) :-
	middle_rec__find_used_registers_lval(Lval, Used0, Used1),
	middle_rec__find_used_registers_rval(Rval, Used1, Used).
middle_rec__find_used_registers_instr(call(_, _, _, _), Used, Used).
middle_rec__find_used_registers_instr(mkframe(_, _, _), Used, Used).
middle_rec__find_used_registers_instr(modframe(_), Used, Used).
middle_rec__find_used_registers_instr(label(_), Used, Used).
middle_rec__find_used_registers_instr(goto(_), Used, Used).
middle_rec__find_used_registers_instr(computed_goto(Rval, _), Used0, Used) :-
	middle_rec__find_used_registers_rval(Rval, Used0, Used).
middle_rec__find_used_registers_instr(c_code(_), Used, Used).
middle_rec__find_used_registers_instr(if_val(Rval, _), Used0, Used) :-
	middle_rec__find_used_registers_rval(Rval, Used0, Used).
middle_rec__find_used_registers_instr(incr_hp(Lval, _, Rval), Used0, Used) :-
	middle_rec__find_used_registers_lval(Lval, Used0, Used1),
	middle_rec__find_used_registers_rval(Rval, Used1, Used).
middle_rec__find_used_registers_instr(mark_hp(Lval), Used0, Used) :-
	middle_rec__find_used_registers_lval(Lval, Used0, Used).
middle_rec__find_used_registers_instr(restore_hp(Rval), Used0, Used) :-
	middle_rec__find_used_registers_rval(Rval, Used0, Used).
middle_rec__find_used_registers_instr(store_ticket(Lval), Used0, Used) :-
	middle_rec__find_used_registers_lval(Lval, Used0, Used).
middle_rec__find_used_registers_instr(restore_ticket(Rval), Used0, Used) :-
	middle_rec__find_used_registers_rval(Rval, Used0, Used).
middle_rec__find_used_registers_instr(discard_ticket, Used, Used).
middle_rec__find_used_registers_instr(incr_sp(_, _), Used, Used).
middle_rec__find_used_registers_instr(decr_sp(_), Used, Used).
middle_rec__find_used_registers_instr(pragma_c(_, Ins, _, Outs), Used0, Used) :-
	insert_pragma_c_input_registers(Ins, Used0, Used1),
	insert_pragma_c_output_registers(Outs, Used1, Used).

:- pred middle_rec__find_used_registers_lvals(list(lval), set(int), set(int)).
:- mode middle_rec__find_used_registers_lvals(in, di, uo) is det.

middle_rec__find_used_registers_lvals([], Used, Used).
middle_rec__find_used_registers_lvals([Lval | Lvals], Used0, Used) :-
	middle_rec__find_used_registers_lval(Lval, Used0, Used1),
	middle_rec__find_used_registers_lvals(Lvals, Used1, Used).

:- pred middle_rec__find_used_registers_lval(lval, set(int), set(int)).
:- mode middle_rec__find_used_registers_lval(in, di, uo) is det.

middle_rec__find_used_registers_lval(Lval, Used0, Used) :-
	( Lval = reg(r(N)) ->
		copy(N, N1),
		set__insert(Used0, N1, Used)
	; Lval = field(_, Rval, FieldNum) ->
		middle_rec__find_used_registers_rval(Rval, Used0, Used1),
		middle_rec__find_used_registers_rval(FieldNum, Used1, Used)
	; Lval = lvar(_) ->
		error("lvar found in middle_rec__find_used_registers_lval")
	;
		Used = Used0
	).

:- pred middle_rec__find_used_registers_rval(rval, set(int), set(int)).
:- mode middle_rec__find_used_registers_rval(in, di, uo) is det.

middle_rec__find_used_registers_rval(Rval, Used0, Used) :-
	(
		Rval = lval(Lval),
		middle_rec__find_used_registers_lval(Lval, Used0, Used)
	;
		Rval = var(_),
		error("var found in middle_rec__find_used_registers_rval")
	;
		Rval = create(_, MaybeRvals, _, _),
		middle_rec__find_used_registers_maybe_rvals(MaybeRvals,
			Used0, Used)
	;
		Rval = mkword(_, Rval1),
		middle_rec__find_used_registers_rval(Rval1, Used0, Used)
	;
		Rval = const(_),
		Used = Used0
	;
		Rval = unop(_, Rval1),
		middle_rec__find_used_registers_rval(Rval1, Used0, Used)
	;
		Rval = binop(_, Rval1, Rval2),
		middle_rec__find_used_registers_rval(Rval1, Used0, Used1),
		middle_rec__find_used_registers_rval(Rval2, Used1, Used)
	).

:- pred middle_rec__find_used_registers_maybe_rvals(list(maybe(rval)),
	set(int), set(int)).
:- mode middle_rec__find_used_registers_maybe_rvals(in, di, uo) is det.

middle_rec__find_used_registers_maybe_rvals([], Used, Used).
middle_rec__find_used_registers_maybe_rvals([MaybeRval | MaybeRvals],
		Used0, Used) :-
	(
		MaybeRval = no,
		Used1 = Used0
	;
		MaybeRval = yes(Rval),
		middle_rec__find_used_registers_rval(Rval, Used0, Used1)
	),
	middle_rec__find_used_registers_maybe_rvals(MaybeRvals, Used1, Used).

:- pred insert_pragma_c_input_registers(list(pragma_c_input), 
	set(int), set(int)).
:- mode insert_pragma_c_input_registers(in, di, uo) is det.

insert_pragma_c_input_registers([], Used, Used).
insert_pragma_c_input_registers([Input|Inputs], Used0, Used) :-	
	Input = pragma_c_input(_, _, Rval),
	middle_rec__find_used_registers_rval(Rval, Used0, Used1),
	insert_pragma_c_input_registers(Inputs, Used1, Used).

:- pred insert_pragma_c_output_registers(list(pragma_c_output), 
	set(int), set(int)).
:- mode insert_pragma_c_output_registers(in, di, uo) is det.

insert_pragma_c_output_registers([], Used, Used).
insert_pragma_c_output_registers([Output|Outputs], Used0, Used) :-	
	Output = pragma_c_output(Lval, _, _),
	middle_rec__find_used_registers_lval(Lval, Used0, Used1),
	insert_pragma_c_output_registers(Outputs, Used1, Used).

%---------------------------------------------------------------------------%

	% Find all the labels defined in an instruction sequence.

:- pred middle_rec__find_labels(list(instruction), list(label)).
:- mode middle_rec__find_labels(in, out) is det.

middle_rec__find_labels(Instrs, Label2) :-
	middle_rec__find_labels_2(Instrs, [], Label2).

:- pred middle_rec__find_labels_2(list(instruction), list(label), list(label)).
:- mode middle_rec__find_labels_2(in, in, out) is det.

middle_rec__find_labels_2([], Labels, Labels).
middle_rec__find_labels_2([Instr - _ | Instrs], Labels0, Labels) :-
	( Instr = label(Label) ->
		Labels1 = [Label | Labels0]
	; Instr = block(_, _, Block) ->
		middle_rec__find_labels_2(Block, Labels0, Labels1)
	;
		Labels1 = Labels0
	),
	middle_rec__find_labels_2(Instrs, Labels1, Labels).

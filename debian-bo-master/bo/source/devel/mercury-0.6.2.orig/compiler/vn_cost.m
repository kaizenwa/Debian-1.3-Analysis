%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% vn_cost.m - predicates to discover the costs of instruction sequences.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module vn_cost.

:- interface.

:- import_module vn_type, llds.
:- import_module bool, list, int, io.

:- pred vn_cost__block_cost(list(instruction), vn_params, bool, int,
	io__state, io__state).
:- mode vn_cost__block_cost(in, in, in, out, di, uo) is det.

:- pred vn_cost__lval_cost(lval, vn_params, int).
:- mode vn_cost__lval_cost(in, in, out) is det.

:- pred vn_cost__rval_cost(rval, vn_params, int).
:- mode vn_cost__rval_cost(in, in, out) is det.

:- implementation.

:- import_module vn_debug.
:- import_module require, string, std_util.

vn_cost__block_cost(Instr, Params, PrintInstr, Cost) -->
	vn_cost__block_cost_2(Instr, Params, PrintInstr, 0, Cost).

:- pred vn_cost__block_cost_2(list(instruction), vn_params, bool, int, int,
	io__state, io__state).
:- mode vn_cost__block_cost_2(in, in, in, in, out, di, uo) is det.

vn_cost__block_cost_2([], _, _, Cost, Cost) --> [].
vn_cost__block_cost_2([Instr | Instrs], Params, PrintInstr, CostBefore, Cost)
		-->
	{ Instr = Uinstr - _ },
	{ vn_cost__instr_cost(Uinstr, Params, InstrCost) },
	{ Uinstr = if_val(_, _) ->
		% We can now count earlier instructions twice
		% to favor code sequences that move code after ifs.
		% Code that saves common subexpressions in the Rval
		% before the if is now handled specially.
		CostNow1 is CostBefore + InstrCost,
		CostNow is 2 * CostNow1
	;
		CostNow is CostBefore + InstrCost
	},
	(
		{ PrintInstr = yes },
		vn_debug__cost_detail_msg(Uinstr, InstrCost, CostNow)
	;
		{ PrintInstr = no }
	),
	vn_cost__block_cost_2(Instrs, Params, PrintInstr, CostNow, Cost).

:- pred vn_cost__instr_cost(instr, vn_params, int).
:- mode vn_cost__instr_cost(in, in, out) is det.

vn_cost__instr_cost(Uinstr, Params, Cost) :-
	(
		Uinstr = comment(_),
		Cost = 0
	;
		Uinstr = livevals(_),
		Cost = 0
	;
		Uinstr = block(_, _, _),
		error("block found in vn_block_cost")
	;
		Uinstr = assign(Lval, Rval),
		vn_cost__lval_cost(Lval, Params, LvalCost),
		vn_cost__rval_cost(Rval, Params, RvalCost),
		vn_type__costof_assign(Params, AssignCost),
		(
			% Is this an assignment that speeds up future accesses?
			% If yes, do not count a cost for the assignment.
			% Basically, assignments to registers are free
			% unless they merely shuffle registers around.
			LvalCost = 0,
			Rval = lval(_),
			RvalCost > 0
		->
			Cost = RvalCost
		;
			% Tagging a value has the same cost as the assignment
			% itself, so don't count this cost twice.
			Rval = mkword(_, _)
		->
			Cost is RvalCost + LvalCost
		;
			Cost is RvalCost + LvalCost + AssignCost
		)
	;
		Uinstr = call(_, _, _, _),
		Cost = 0
	;
		Uinstr = mkframe(_, _, _),
		Cost = 0
	;
		Uinstr = modframe(_),
		Cost = 0
	;
		Uinstr = label(_),
		Cost = 0
	;
		Uinstr = goto(_),
		Cost = 0
	;
		Uinstr = computed_goto(Rval, _),
		vn_cost__rval_cost(Rval, Params, RvalCost),
		Cost = RvalCost
	;
		Uinstr = c_code(_),
		error("c_code found in vn_block_cost")
	;
		Uinstr = if_val(Rval, _),
		vn_cost__rval_cost(Rval, Params, RvalCost),
		Cost = RvalCost
	;
		Uinstr = incr_hp(Lval, MaybeTag, Rval),
		vn_type__costof_assign(Params, AssignCost),
		vn_cost__lval_cost(Lval, Params, LvalCost),
		vn_cost__rval_cost(Rval, Params, RvalCost),
		Cost1 is RvalCost + LvalCost + 3 * AssignCost,
		(
			MaybeTag = yes(_),
			vn_type__costof_intops(Params, Cost2)
		;
			MaybeTag = no,
			Cost2 = 0
		),
		Cost is Cost1 + Cost2
	;
		Uinstr = mark_hp(Lval),
		vn_cost__lval_cost(Lval, Params, LvalCost),
		Cost = LvalCost
	;
		Uinstr = restore_hp(Rval),
		vn_cost__rval_cost(Rval, Params, RvalCost),
		Cost = RvalCost
	;
		Uinstr = store_ticket(Lval),
		vn_cost__lval_cost(Lval, Params, LvalCost),
		Cost = LvalCost
	;
		Uinstr = restore_ticket(Rval),
		vn_cost__rval_cost(Rval, Params, RvalCost),
		Cost = RvalCost
	;
		Uinstr = discard_ticket,
		Cost = 0
	;
		Uinstr = incr_sp(_, _),
		Cost = 0
	;
		Uinstr = decr_sp(_),
		Cost = 0
	;
		Uinstr = pragma_c(_, _, _, _),
		error("pragma_c found in vn_block_cost")
	).

vn_cost__lval_cost(Lval, Params, Cost) :-
	(
		Lval = reg(Reg),
		(
			Reg = r(RegRno),
			vn_type__real_r_regs(Params, MaxRealRegRno),
			( RegRno =< MaxRealRegRno ->
				Cost = 0
			;
				vn_type__costof_stackref(Params, Cost)
			)
		;
			Reg = f(RegFno),
			vn_type__real_f_regs(Params, MaxRealRegFno),
			( RegFno =< MaxRealRegFno ->
				Cost = 0
			;
				vn_type__costof_stackref(Params, Cost)
			)
		)
	;
		Lval = temp(TempReg),
		(
			TempReg = r(TempRno),
			vn_type__real_r_temps(Params, MaxRealTempRno),
			( TempRno =< MaxRealTempRno ->
				Cost = 0
			;
				vn_type__costof_stackref(Params, Cost)
			)
		;
			TempReg = f(TempFno),
			vn_type__real_f_temps(Params, MaxRealTempFno),
			( TempFno =< MaxRealTempFno ->
				Cost = 0
			;
				vn_type__costof_stackref(Params, Cost)
			)
		)
	;
		Lval = stackvar(_),
		vn_type__costof_stackref(Params, StackrefCost),
		Cost = StackrefCost
	;
		Lval = framevar(_),
		vn_type__costof_stackref(Params, StackrefCost),
		Cost = StackrefCost
	;
		Lval = succfr(Rval),
		vn_type__costof_stackref(Params, StackrefCost),
		vn_cost__rval_cost(Rval, Params, RvalCost),
		Cost is RvalCost + StackrefCost
	;
		Lval = prevfr(Rval),
		vn_type__costof_stackref(Params, StackrefCost),
		vn_cost__rval_cost(Rval, Params, RvalCost),
		Cost is RvalCost + StackrefCost
	;
		Lval = redoip(Rval),
		vn_type__costof_stackref(Params, StackrefCost),
		vn_cost__rval_cost(Rval, Params, RvalCost),
		Cost is RvalCost + StackrefCost
	;
		Lval = succip(Rval),
		vn_cost__rval_cost(Rval, Params, RvalCost),
		vn_type__costof_stackref(Params, StackrefCost),
		Cost is RvalCost + StackrefCost
	;
		Lval = succip,
		Cost = 0
	;
		Lval = maxfr,
		Cost = 0
	;
		Lval = curfr,
		Cost = 0
	;
		Lval = hp,
		Cost = 0
	;
		Lval = sp,
		Cost = 0
	;
		Lval = field(_, Rval1, Rval2),
		vn_cost__rval_cost(Rval1, Params, RvalCost1),
		vn_cost__rval_cost(Rval2, Params, RvalCost2),
		vn_type__costof_heapref(Params, HeaprefCost),
		Cost is RvalCost1 + RvalCost2 + HeaprefCost
	;
		Lval = lvar(_),
		error("lvar found in lval_cost")
	).

vn_cost__rval_cost(Rval, Params, Cost) :-
	(
		Rval = lval(Lval),
		vn_cost__lval_cost(Lval, Params, LvalCost),
		Cost = LvalCost
	;
		Rval = var(_),
		error("var found in rval_cost")
	;
		Rval = create(_, _, _, _),
		Cost = 0
	;
		Rval = mkword(_, Rval1),
		vn_cost__rval_cost(Rval1, Params, RvalCost),
		vn_type__costof_intops(Params, OpsCost),
		Cost is RvalCost + OpsCost
	;
		Rval = const(_),
		Cost = 0
	;
		Rval = unop(_, Rval1),
		vn_cost__rval_cost(Rval1, Params, RvalCost),
		vn_type__costof_intops(Params, OpsCost),
		Cost is RvalCost + OpsCost
	;
		Rval = binop(_, Rval1, Rval2),
		vn_cost__rval_cost(Rval1, Params, RvalCost1),
		vn_cost__rval_cost(Rval2, Params, RvalCost2),
		vn_type__costof_intops(Params, OpsCost),
		Cost is RvalCost1 + RvalCost2 + OpsCost
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: code_exprn.m
% main author: conway.
%
% This module defines a series of predicates that operate on the
% abstract 'exprn_info' structure which maintains information about
% the contents of registers, and manages the cached expressions for
% variables.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module code_exprn.

:- interface.

:- import_module llds, list, varset, std_util, assoc_list, tree, options.

:- type exprn_info.

%	code_exprn__init_state(Arguments, Varset, Opts, ExprnInfo)
%		Produces an initial state of the ExprnInfo given
%		an association list of variables and lvalues. The initial
%		state places the given variables at their corresponding
%		locations. The Varset parameter contains a mapping from
%		variables to names, which is used when code is generated
%		to provide meaningful comments. Opts gives the table of
%		options; this is used to decide what expressions are
%		considered constants.

:- pred code_exprn__init_state(assoc_list(var, rval), varset, option_table,
	exprn_info).
:- mode code_exprn__init_state(in, in, in, out) is det.

%	code_exprn__clobber_regs(CriticalVars, ExprnInfo0, ExprnInfo)
%		Modifies the state ExprnInfo0 to produce ExprnInfo
%		in which all variables stored in registers are clobbered.
%		If any variables in CriticalVars are stored only in
%		registers, and are not stored on the stack, then this
%		predicate will abort.

:- pred code_exprn__clobber_regs(list(var), exprn_info, exprn_info).
:- mode code_exprn__clobber_regs(in, in, out) is det.

%	code_exprn__set_var_location(Var, Lval, ExprnInfo0, ExprnInfo)
%		Modifies ExprnInfo0 to produce ExprnInfo in which
%		Var is *magically* stored in Lval.

:- pred code_exprn__set_var_location(var, lval, exprn_info, exprn_info).
:- mode code_exprn__set_var_location(in, in, in, out) is det.

:- pred code_exprn__maybe_set_var_location(var, lval, exprn_info, exprn_info).
:- mode code_exprn__maybe_set_var_location(in, in, in, out) is det.

:- pred code_exprn__lval_in_use(lval, exprn_info, exprn_info).
:- mode code_exprn__lval_in_use(in, in, out) is semidet.

%	code_exprn__var_becomes_dead(Var, ExprnInfo0, ExprnInfo)
%		Frees any code generator resources used by Var
%		in ExprnInfo0 to produce ExprnInfo (in the implementation,
%		any cached expressions which still need those resources
%		will inherit them appropriately).

:- pred code_exprn__var_becomes_dead(var, exprn_info, exprn_info).
:- mode code_exprn__var_becomes_dead(in, in, out) is det.

%	code_exprn__cache_exprn(Var, Rval, ExprnInfo0, ExprnInfo)
%		Produces a modified ExprnInfo0, ExprnInfo
%		which indicates that when a value of Var is needed,
%		code to evaluate Rval should be produced.

:- pred code_exprn__cache_exprn(var, rval, exprn_info, exprn_info).
:- mode code_exprn__cache_exprn(in, in, in, out) is det.

%	code_exprn__place_var(Var, Lval, Code, ExprnInfo0, ExprnInfo)
%		Produces Code and a modified version of ExprnInfo0,
%		ExprnInfo which places the value of Var in Lval.

:- pred code_exprn__place_var(var, lval, code_tree, exprn_info, exprn_info).
:- mode code_exprn__place_var(in, in, out, in, out) is det.

%	code_exprn__place_vars(StoreMap, Code, ExprnInfo0, ExprnInfo)
%		Produces Code and a modified version of ExprnInfo0,
%		ExprnInfo which places the value of each variable
%		mentioned in the store map into the corresponding location.

:- pred code_exprn__place_vars(assoc_list(var, lval), code_tree,
	exprn_info, exprn_info).
:- mode code_exprn__place_vars(in, out, in, out) is det.

%	code_exprn__produce_var(Var, Rval, Code, ExprnInfo0, ExprnInfo)
%		Produces a code fragment Code to evaluate Var and
%		provide it as Rval (which may be a const, etc, or an lval).

:- pred code_exprn__produce_var(var, rval, code_tree, exprn_info, exprn_info).
:- mode code_exprn__produce_var(in, out, out, in, out) is det.

%	code_exprn__produce_var_in_reg(Var, Rval, Code, ExprnInfo0, ExprnInfo)
%		Produces a code fragment Code to evaluate Var and
%		provide it as an Rval of the form lval(reg(_)).

:- pred code_exprn__produce_var_in_reg(var, rval, code_tree,
						exprn_info, exprn_info).
:- mode code_exprn__produce_var_in_reg(in, out, out, in, out) is det.

%	code_exprn__produce_var_in_reg_or_stack(Var, Rval, Code,
%			ExprnInfo0, ExprnInfo)
%		Produces a code fragment Code to evaluate Var and
%		provide it as an Rval of the form lval(reg(_)),
%		lval(stackvar(_)), or lval(framevar(_)).

:- pred code_exprn__produce_var_in_reg_or_stack(var, rval, code_tree,
						exprn_info, exprn_info).
:- mode code_exprn__produce_var_in_reg_or_stack(in, out, out, in, out) is det.

%	code_exprn__materialize_vars_in_rval(Rval0, Rval, Code, ExprnInfo0,
%		ExprnInfo)
%		Produces code to materialize any vars that occur in `Rval0'
%		and substitutes their value to produce `Rval'.
%		`Rval' is only valid in code between the call to
%		code_exprn__materialize_vars_in_rval and the next goal which
%		may modify the ExprnInfo structure.

:- pred code_exprn__materialize_vars_in_rval(rval, rval, code_tree,
		exprn_info, exprn_info).
:- mode code_exprn__materialize_vars_in_rval(in, out, out, in, out) is det.

%	code_exprn__acquire_reg(Reg, ExprnInfo0, ExprnInfo)
%		Finds an unused register and marks it as 'in use'.
%
%	code_exprn__release_reg(Reg, ExprnInfo, ExprnInfo)
%		Marks a previously acquired reg and releases it so
%		that it can be reused.

:- pred code_exprn__acquire_reg(reg, exprn_info, exprn_info).
:- mode code_exprn__acquire_reg(out, in, out) is det.

:- pred code_exprn__release_reg(reg, exprn_info, exprn_info).
:- mode code_exprn__release_reg(in, in, out) is det.

%	XXX These should be local, or their function should be folded into
%	acquire/release.
%	??? Why?  -fjh.
%
%	code_exprn__lock_reg(Reg, ExprnInfo, ExprnInfo)
%		Prevents a register from being reused, even if
%		there are no variables refering to it.
%
%	code_exprn__unlock_reg(Reg, ExprnInfo0, ExprnInfo)
%		Undoes a lock operation.

:- pred code_exprn__lock_reg(reg, exprn_info, exprn_info).
:- mode code_exprn__lock_reg(in, in, out) is det.

:- pred code_exprn__unlock_reg(reg, exprn_info, exprn_info).
:- mode code_exprn__unlock_reg(in, in, out) is det.

%	code_exprn__clear_r1(Code)
%		Produces a code fragment Code to move whatever is in r1
%		to some other register, if r1 is live.  This is used
%		prior to semidet pragma c_codes.

:- pred code_exprn__clear_r1(code_tree, exprn_info, exprn_info).
:- mode code_exprn__clear_r1(out, in, out) is det.

%	code_exprn__get_varlocs(ExprnInfo, Locations)
%		Returns a map from each variable that occurs in ExprnInfo to
%		the set of locations (really rvals) in which it may be found.

:- pred code_exprn__get_varlocs(exprn_info, map(var, set(rval))).
:- mode code_exprn__get_varlocs(in, out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module exprn_aux.
:- import_module bool, map, bag, set, require, int, term, string.

:- type var_stat	--->	evaled(set(rval))
			;	cached(rval).

:- type var_map	==	map(var, var_stat).

:- type exprn_info	--->
		exprn_info(
			varset,		% all the variables and their names
			var_map,	% what each variable stands for
			bag(reg),	% the 'in use' markers for regs
			set(reg),	% extra markers for acquired regs
			exprn_opts	% options needed for constant checks
		).

%------------------------------------------------------------------------------%

code_exprn__init_state(Initializations, Varset, Options, ExprnInfo) :-
	map__init(Vars0),
	bag__init(Regs0),
	code_exprn__init_state_2(Initializations, Vars0, Vars, Regs0, Regs),
	set__init(Acqu),
	exprn_aux__init_exprn_opts(Options, ExprnOpts),
	ExprnInfo = exprn_info(Varset, Vars, Regs, Acqu, ExprnOpts).

:- pred code_exprn__init_state_2(assoc_list(var, rval), var_map, var_map,
							bag(reg), bag(reg)).
:- mode code_exprn__init_state_2(in, in, out, in, out) is det.

code_exprn__init_state_2([], Vars, Vars, Regs, Regs).
code_exprn__init_state_2([V - L | Rest], Vars0, Vars, Regs0, Regs) :-
	(
		map__search(Vars0, V, evaled(Vals0))
	->
		set__insert(Vals0, L, Vals)
	;
		set__singleton_set(Vals, L)
	),
	map__set(Vars0, V, evaled(Vals), Vars1),
	(
		L = lval(reg(R))
	->
		bag__insert(Regs0, R, Regs1)
	;
		Regs1 = Regs0
	),
	code_exprn__init_state_2(Rest, Vars1, Vars, Regs1, Regs).

%------------------------------------------------------------------------------%

code_exprn__get_varlocs(ExprnInfo, Locations) :-
	code_exprn__get_vars(Vars, ExprnInfo, _),
	map__to_assoc_list(Vars, VarList),
	map__init(Locations0),
	code_exprn__repackage_locations(VarList, Locations0, Locations).

:- pred code_exprn__repackage_locations(assoc_list(var, var_stat),
			map(var, set(rval)), map(var, set(rval))).
:- mode code_exprn__repackage_locations(in, in, out) is det.

code_exprn__repackage_locations([], Loc, Loc).
code_exprn__repackage_locations([V - Locs | Rest], Loc0, Loc) :-
	(
		Locs = cached(Rval),
		set__singleton_set(Rvals, Rval)
	;
		Locs = evaled(Rvals)
	),
	map__set(Loc0, V, Rvals, Loc1),
	code_exprn__repackage_locations(Rest, Loc1, Loc).

%------------------------------------------------------------------------------%

code_exprn__clobber_regs(CriticalVars) -->
	code_exprn__get_vars(Vars0),
	{ map__to_assoc_list(Vars0, VarsList) },
	{ map__init(Vars1) },
	{ code_exprn__clobber_regs_2(VarsList, CriticalVars, Vars0,
							Vars1, Vars) },
	code_exprn__set_vars(Vars),
	{ bag__init(Regs) },
	code_exprn__set_regs(Regs),
	{ set__init(Acqu) },
	code_exprn__set_acquired(Acqu).

:- pred code_exprn__clobber_regs_2(assoc_list(var, var_stat), list(var),
						var_map, var_map, var_map).
:- mode code_exprn__clobber_regs_2(in, in, in, in, out) is det.

code_exprn__clobber_regs_2([], _Critical, _OldVars, Vars, Vars).
code_exprn__clobber_regs_2([V - Stat | Rest], Critical, OldVars, Vars0, Vars) :-
	(
		Stat = cached(Exprn),
		(
			code_exprn__rval_depends_on_reg(Exprn, OldVars)
		->
			(
				list__member(V, Critical)
			->
				error("code_exprn__clobber_regs: attempt to clobber critical register")
			;
				Vars1 = Vars0
			)
		;
			map__set(Vars0, V, Stat, Vars1)
		)
	;
		Stat = evaled(Rvals0),
		code_exprn__filter_out_reg_depending(Rvals0, OldVars, Rvals),
		(
			set__empty(Rvals)
		->
			(
				list__member(V, Critical)
			->
				error("code_exprn__clobber_regs: attempt to clobber critical register")
			;
				Vars1 = Vars0
			)
		;
			map__set(Vars0, V, evaled(Rvals), Vars1)
		)
	),
	code_exprn__clobber_regs_2(Rest, Critical, OldVars, Vars1, Vars).

%------------------------------------------------------------------------------%

:- pred code_exprn__rval_depends_on_reg(rval, var_map).
:- mode code_exprn__rval_depends_on_reg(in, in) is semidet.

code_exprn__rval_depends_on_reg(lval(Lval), Vars) :-
	code_exprn__lval_depends_on_reg(Lval, Vars).
code_exprn__rval_depends_on_reg(var(Var), Vars) :-
	map__lookup(Vars, Var, Stat),
	(
		Stat = cached(Rval),
		code_exprn__rval_depends_on_reg(Rval, Vars)
	;
		Stat = evaled(Rvals0),
		code_exprn__filter_out_reg_depending(Rvals0, Vars, Rvals),
		set__empty(Rvals)
	).
code_exprn__rval_depends_on_reg(create(_Tag, Rvals, _Unique, _LabNum), Vars) :-
	code_exprn__args_depend_on_reg(Rvals, Vars).
code_exprn__rval_depends_on_reg(mkword(_Tag, Rval), Vars) :-
	code_exprn__rval_depends_on_reg(Rval, Vars).
code_exprn__rval_depends_on_reg(const(_Const), _Vars) :-
	fail.
code_exprn__rval_depends_on_reg(unop(_Op, Rval), Vars) :-
	code_exprn__rval_depends_on_reg(Rval, Vars).
code_exprn__rval_depends_on_reg(binop(_Op, Rval0, Rval1), Vars) :-
	(
		code_exprn__rval_depends_on_reg(Rval0, Vars)
	;
		code_exprn__rval_depends_on_reg(Rval1, Vars)
	).

:- pred code_exprn__lval_depends_on_reg(lval, var_map).
:- mode code_exprn__lval_depends_on_reg(in, in) is semidet.

code_exprn__lval_depends_on_reg(reg(_), _Vars) :-
	true.
code_exprn__lval_depends_on_reg(lvar(Var), Vars) :-
	map__lookup(Vars, Var, Stat),
	(
		Stat = cached(Rval),
		code_exprn__rval_depends_on_reg(Rval, Vars)
	;
		Stat = evaled(Rvals0),
		code_exprn__filter_out_reg_depending(Rvals0, Vars, Rvals),
		set__empty(Rvals)
	).
code_exprn__lval_depends_on_reg(field(_Tag, Rval0, Rval1), Vars) :-
	(
		code_exprn__rval_depends_on_reg(Rval0, Vars)
	;
		code_exprn__rval_depends_on_reg(Rval1, Vars)
	).

:- pred code_exprn__args_depend_on_reg(list(maybe(rval)), var_map).
:- mode code_exprn__args_depend_on_reg(in, in) is semidet.

code_exprn__args_depend_on_reg([], _Vars) :-
	fail.
code_exprn__args_depend_on_reg([Arg | Args], Vars) :-
	(
		Arg = yes(Rval),
		code_exprn__rval_depends_on_reg(Rval, Vars)
	->
		true
	;
		code_exprn__args_depend_on_reg(Args, Vars)
	).

:- pred code_exprn__filter_out_reg_depending(set(rval), var_map, set(rval)).
:- mode code_exprn__filter_out_reg_depending(in, in, out) is det.

code_exprn__filter_out_reg_depending(Rvals0, Vars, Rvals) :-
	set__to_sorted_list(Rvals0, RvalList0),
	code_exprn__filter_out_reg_depending_2(RvalList0, Vars, RvalList),
	set__sorted_list_to_set(RvalList, Rvals).

:- pred code_exprn__filter_out_reg_depending_2(list(rval), var_map, list(rval)).
:- mode code_exprn__filter_out_reg_depending_2(in, in, out) is det.

code_exprn__filter_out_reg_depending_2([], _Vars, []).
code_exprn__filter_out_reg_depending_2([Rval0 | Rvals0], Vars, Rvals) :-
	code_exprn__filter_out_reg_depending_2(Rvals0, Vars, Rvals1),
	(
		code_exprn__rval_depends_on_reg(Rval0, Vars)
	->
		Rvals = Rvals1
	;
		Rvals = [Rval0 | Rvals1]
	).

%------------------------------------------------------------------------------%

code_exprn__set_var_location(Var, Lval) -->
	(
		code_exprn__lval_in_use(Lval)
	->
		{ error("code_exprn__set_var_location: location already in use") }
	;
		code_exprn__maybe_set_var_location(Var, Lval)
	).

%------------------------------------------------------------------------------%

code_exprn__maybe_set_var_location(Var, Lval) -->
	code_exprn__get_vars(Vars0),
	{ set__singleton_set(Locs, lval(Lval)) },
	{ map__set(Vars0, Var, evaled(Locs), Vars) },
	code_exprn__set_vars(Vars),
	code_exprn__add_lval_reg_dependencies(Lval).

%------------------------------------------------------------------------------%

code_exprn__lval_in_use(Lval) -->
	code_exprn__lval_in_use_except(Lval, no).

:- pred code_exprn__lval_in_use_except(lval, maybe(var),
	exprn_info, exprn_info).
:- mode code_exprn__lval_in_use_except(in, in, in, out) is semidet.

code_exprn__lval_in_use_except(Lval, MaybeVar) -->
	code_exprn__get_vars(Vars),
	{ map__to_assoc_list(Vars, VarStatList) },
	{ code_exprn__lval_in_use_2(Lval, MaybeVar, VarStatList) }.

	% The auxiliary predicate is needed to allow Mercury to recognize
	% that the subcomputation has no outputs without using existential
	% quantification, whose syntax SICStus doesn't like.

:- pred code_exprn__lval_in_use_2(lval, maybe(var), assoc_list(var, var_stat)).
:- mode code_exprn__lval_in_use_2(in, in, in) is semidet.

code_exprn__lval_in_use_2(Lval, MaybeVar, VarStatList) :-
	list__member(VarStat, VarStatList),
	VarStat = Var - Stat,
	(
		MaybeVar = no
	;
		MaybeVar = yes(ExceptionVar),
		\+ Var = ExceptionVar
	),
	(
		Stat = cached(Rval),
		exprn_aux__rval_contains_lval(Rval, Lval)
	;
		Stat = evaled(Rvals),
		set__member(Rval, Rvals),
		exprn_aux__rval_contains_lval(Rval, Lval)
	).

%------------------------------------------------------------------------------%

:- pred code_exprn__clear_lval_of_synonyms(lval, exprn_info, exprn_info).
:- mode code_exprn__clear_lval_of_synonyms(in, in, out) is det.

code_exprn__clear_lval_of_synonyms(Lval) -->
	code_exprn__get_vars(Vars),
	{ map__to_assoc_list(Vars, VarStatList) },
	code_exprn__clear_lval_of_synonyms_1(VarStatList, Lval).

:- pred code_exprn__clear_lval_of_synonyms_1(assoc_list(var, var_stat), lval,
	exprn_info, exprn_info).
:- mode code_exprn__clear_lval_of_synonyms_1(in, in, in, out) is det.

code_exprn__clear_lval_of_synonyms_1([], _) --> [].
code_exprn__clear_lval_of_synonyms_1([Var - Stat | VarStatList], Lval) -->
	(
		{ Stat = cached(_) }
	;
		{ Stat = evaled(Rvals0) },
		{ set__to_sorted_list(Rvals0, RvalsList0) },
		{ code_exprn__find_rvals_without_lval(RvalsList0, Lval,
			RvalsList) },
		( { RvalsList = [] } ->
			[]
		;
			code_exprn__get_vars(Vars0),
			{ set__sorted_list_to_set(RvalsList, Rvals) },
			{ map__set(Vars0, Var, evaled(Rvals), Vars) },
			code_exprn__set_vars(Vars)
		)
	),
	code_exprn__clear_lval_of_synonyms_1(VarStatList, Lval).

:- pred code_exprn__find_rvals_without_lval(list(rval), lval, list(rval)).
:- mode code_exprn__find_rvals_without_lval(in, in, out) is det.

code_exprn__find_rvals_without_lval([], _, []).
code_exprn__find_rvals_without_lval([Rval0 | Rvals0], Lval, Rvals) :-
	code_exprn__find_rvals_without_lval(Rvals0, Lval, Rvals1),
	( exprn_aux__rval_contains_lval(Rval0, Lval) ->
		Rvals = Rvals1
	;
		Rvals = [Rval0 | Rvals1]
	).

%------------------------------------------------------------------------------%

:- pred code_exprn__add_lval_reg_dependencies(lval, exprn_info, exprn_info).
:- mode code_exprn__add_lval_reg_dependencies(in, in, out) is det.

code_exprn__add_lval_reg_dependencies(Lval) -->
	(
		{ Lval = reg(Reg) }
	->
		code_exprn__get_regs(Regs0),
		{ bag__insert(Regs0, Reg, Regs) },
		code_exprn__set_regs(Regs)
	;
		{ Lval = field(_Tag, Rval0, Rval1) }
	->
		code_exprn__add_rval_reg_dependencies(Rval0),
		code_exprn__add_rval_reg_dependencies(Rval1)
	;
		[]
	).

:- pred code_exprn__add_rval_list_reg_dependencies(list(rval),
						exprn_info, exprn_info).
:- mode code_exprn__add_rval_list_reg_dependencies(in, in, out) is det.

code_exprn__add_rval_list_reg_dependencies([]) --> [].
code_exprn__add_rval_list_reg_dependencies([R | Rs]) -->
	code_exprn__add_rval_reg_dependencies(R),
	code_exprn__add_rval_list_reg_dependencies(Rs).

:- pred code_exprn__add_rval_reg_dependencies(rval, exprn_info, exprn_info).
:- mode code_exprn__add_rval_reg_dependencies(in, in, out) is det.

code_exprn__add_rval_reg_dependencies(lval(Lval)) -->
	code_exprn__add_lval_reg_dependencies(Lval).
code_exprn__add_rval_reg_dependencies(var(_Var)) --> [].
code_exprn__add_rval_reg_dependencies(create(_, Rvals, _, _)) -->
	code_exprn__add_arg_reg_dependencies(Rvals).
code_exprn__add_rval_reg_dependencies(mkword(_Tag, Rval)) -->
	code_exprn__add_rval_reg_dependencies(Rval).
code_exprn__add_rval_reg_dependencies(const(_Const)) --> [].
code_exprn__add_rval_reg_dependencies(unop(_Op, Rval)) -->
	code_exprn__add_rval_reg_dependencies(Rval).
code_exprn__add_rval_reg_dependencies(binop(_Op, Rval0, Rval1)) -->
	code_exprn__add_rval_reg_dependencies(Rval0),
	code_exprn__add_rval_reg_dependencies(Rval1).

:- pred code_exprn__add_arg_reg_dependencies(list(maybe(rval)),
						exprn_info, exprn_info).
:- mode code_exprn__add_arg_reg_dependencies(in, in, out) is det.

code_exprn__add_arg_reg_dependencies([]) --> [].
code_exprn__add_arg_reg_dependencies([M | Ms]) -->
	(
		{ M = yes(Rval) }
	->
		code_exprn__add_rval_reg_dependencies(Rval)
	;
		[]
	),
	code_exprn__add_arg_reg_dependencies(Ms).

%------------------------------------------------------------------------------%

:- pred code_exprn__rem_lval_reg_dependencies(lval, exprn_info, exprn_info).
:- mode code_exprn__rem_lval_reg_dependencies(in, in, out) is det.

code_exprn__rem_lval_reg_dependencies(Lval) -->
	(
		{ Lval = reg(Reg) }
	->
		code_exprn__get_regs(Regs0),
		{ bag__remove(Regs0, Reg, Regs) },
		code_exprn__set_regs(Regs)
	;
		{ Lval = field(_Tag, Rval0, Rval1) }
	->
		code_exprn__rem_rval_reg_dependencies(Rval0),
		code_exprn__rem_rval_reg_dependencies(Rval1)
	;
		[]
	).

:- pred code_exprn__rem_rval_list_reg_dependencies(list(rval),
						exprn_info, exprn_info).
:- mode code_exprn__rem_rval_list_reg_dependencies(in, in, out) is det.

code_exprn__rem_rval_list_reg_dependencies([]) --> [].
code_exprn__rem_rval_list_reg_dependencies([R | Rs]) -->
	code_exprn__rem_rval_reg_dependencies(R),
	code_exprn__rem_rval_list_reg_dependencies(Rs).

:- pred code_exprn__rem_rval_reg_dependencies(rval, exprn_info, exprn_info).
:- mode code_exprn__rem_rval_reg_dependencies(in, in, out) is det.

code_exprn__rem_rval_reg_dependencies(lval(Lval)) -->
	code_exprn__rem_lval_reg_dependencies(Lval).
code_exprn__rem_rval_reg_dependencies(var(_Var)) --> [].
code_exprn__rem_rval_reg_dependencies(create(_, Rvals, _, _)) -->
	code_exprn__rem_arg_reg_dependencies(Rvals).
code_exprn__rem_rval_reg_dependencies(mkword(_Tag, Rval)) -->
	code_exprn__rem_rval_reg_dependencies(Rval).
code_exprn__rem_rval_reg_dependencies(const(_Const)) --> [].
code_exprn__rem_rval_reg_dependencies(unop(_Op, Rval)) -->
	code_exprn__rem_rval_reg_dependencies(Rval).
code_exprn__rem_rval_reg_dependencies(binop(_Op, Rval0, Rval1)) -->
	code_exprn__rem_rval_reg_dependencies(Rval0),
	code_exprn__rem_rval_reg_dependencies(Rval1).

:- pred code_exprn__rem_arg_reg_dependencies(list(maybe(rval)),
						exprn_info, exprn_info).
:- mode code_exprn__rem_arg_reg_dependencies(in, in, out) is det.

code_exprn__rem_arg_reg_dependencies([]) --> [].
code_exprn__rem_arg_reg_dependencies([M | Ms]) -->
	(
		{ M = yes(Rval) }
	->
		code_exprn__rem_rval_reg_dependencies(Rval)
	;
		[]
	),
	code_exprn__rem_arg_reg_dependencies(Ms).

%------------------------------------------------------------------------------%

code_exprn__var_becomes_dead(Var) -->
	code_exprn__get_vars(Vars0),
	(
		{ map__search(Vars0, Var, Stat) }
	->
		(
			{ Stat = cached(Rval0) },
			code_exprn__rem_rval_reg_dependencies(Rval0)
		;
			{ Stat = evaled(Rvals0) },
			{ set__to_sorted_list(Rvals0, RvalList0) },
			code_exprn__rem_rval_list_reg_dependencies(RvalList0),
			code_exprn__get_options(ExprnOpts),
			(
				{ code_exprn__member_expr_is_constant(RvalList0,
						Vars0, ExprnOpts, Rval7) }
			->
				{ Rval0 = Rval7 }
			;
				{ code_exprn__select_rval(RvalList0, Rval0) }
			)
		),
		{ map__delete(Vars0, Var, Vars1) },
		code_exprn__set_vars(Vars1),
		code_exprn__update_dependent_vars(Var, Rval0)
	;
		% XXX When we make the code generator tighter,
		% we can reinstate this sanity check. In particular,
		% code_info needs to know which args (etc) have
		% been explicitly killed off during the generation
		% of the goal.
		% code_exprn__get_var_name(Var, Name),
		% { string__append_list(["code_exprn__var_becomes_dead: var ",
		% 	Name, " not found!"], Msg) },
		% { error(Msg) }
		[]
	).

%------------------------------------------------------------------------------%

:- pred code_exprn__update_dependent_vars(var, rval, exprn_info, exprn_info).
:- mode code_exprn__update_dependent_vars(in, in, in, out) is det.

code_exprn__update_dependent_vars(Var, Rval) -->
	code_exprn__get_vars(Vars0),
	{ map__to_assoc_list(Vars0, VarList0) },
	code_exprn__update_dependent_vars_2(VarList0, Var, Rval, VarList),
	{ map__from_assoc_list(VarList, Vars) },
	code_exprn__set_vars(Vars).

:- pred code_exprn__update_dependent_vars_2(assoc_list(var, var_stat),
		var, rval, assoc_list(var, var_stat), exprn_info, exprn_info).
:- mode code_exprn__update_dependent_vars_2(in, in, in, out, in, out) is det.

code_exprn__update_dependent_vars_2([], _Var, _Rval, []) --> [].
code_exprn__update_dependent_vars_2([V - Stat0 | Rest0], Var, Rval,
							[V - Stat | Rest]) -->
	(
		{ Stat0 = cached(Exprn0) },
		{ exprn_aux__rval_contains_rval(Exprn0, var(Var)) }
	->
		code_exprn__add_rval_reg_dependencies(Rval),
		{ exprn_aux__substitute_rval_in_rval(var(Var), Rval,
							Exprn0, Exprn1) },
		{ exprn_aux__simplify_rval(Exprn1, Exprn) },
		(
			{ exprn_aux__vars_in_rval(Exprn, []) }
		->
			{ set__singleton_set(Rvals, Exprn) },
			{ Stat = evaled(Rvals) }
		;
			{ Stat = cached(Exprn) }
		)
	;
		{ Stat0 = evaled(Rvals0) }
	->
		{ set__to_sorted_list(Rvals0, RvalList0) },
		code_exprn__update_dependent_vars_3(RvalList0, Var, Rval,
					RvalList),
		{ set__sorted_list_to_set(RvalList, Rvals) },
		{ Stat = evaled(Rvals) }
	;
		% Stat0 = cached(Exprn), \+ contains
		{ Stat = Stat0 }
	),
	code_exprn__update_dependent_vars_2(Rest0, Var, Rval, Rest).

:- pred code_exprn__update_dependent_vars_3(list(rval), var, rval, list(rval),
					exprn_info, exprn_info).
:- mode code_exprn__update_dependent_vars_3(in, in, in, out, in, out) is det.

code_exprn__update_dependent_vars_3([], _Var, _Rval, []) --> [].
code_exprn__update_dependent_vars_3([R0 | Rs0], Var, Rval, [R | Rs]) -->
	(
		{ exprn_aux__rval_contains_rval(R0, var(Var)) }
	->
		{ exprn_aux__substitute_rval_in_rval(var(Var), Rval, R0, R1) },
		{ exprn_aux__simplify_rval(R1, R) },
		code_exprn__rem_rval_reg_dependencies(R0),
		code_exprn__add_rval_reg_dependencies(R)
	;
		{ R = R0 }
	),
	code_exprn__update_dependent_vars_3(Rs0, Var, Rval, Rs).

%------------------------------------------------------------------------------%

:- pred code_exprn__select_rval(list(rval), rval).
:- mode code_exprn__select_rval(in, out) is det.

code_exprn__select_rval(Rvals, Rval) :-
	(
		Rvals = []
	->
		error("code_exprn__select_rval: no rvals")
	;
		code_exprn__select_reg(Rvals, Rval0)
	->
		Rval = Rval0
	;
		code_exprn__select_simple_const(Rvals, Rval1)
	->
		Rval = Rval1
	;
		code_exprn__select_stackvar(Rvals, Rval2)
	->
		Rval = Rval2
	;
		Rvals = [Rval3 | _]
	->
		Rval = Rval3
	;
		error("code_exprn__select_rval: cosmic rays strike again")
	).

:- pred code_exprn__select_reg(list(rval), rval).
:- mode code_exprn__select_reg(in, out) is semidet.

code_exprn__select_reg([R | Rs], Rval) :-
	(
		R = lval(reg(_))
	->
		Rval = R
	;
		code_exprn__select_reg(Rs, Rval)
	).

:- pred code_exprn__select_simple_const(list(rval), rval).
:- mode code_exprn__select_simple_const(in, out) is semidet.

code_exprn__select_simple_const([R | Rs], Rval) :-
	(
		R = const(_)
	->
		Rval = R
	;
		code_exprn__select_simple_const(Rs, Rval)
	).

:- pred code_exprn__select_stackvar(list(rval), rval).
:- mode code_exprn__select_stackvar(in, out) is semidet.

code_exprn__select_stackvar([R | Rs], Rval) :-
	(
		R = lval(C),
		(
			C = stackvar(_)
		;
			C = framevar(_)
		)
	->
		Rval = R
	;
		code_exprn__select_stackvar(Rs, Rval)
	).

%------------------------------------------------------------------------------%

:- pred code_exprn__expr_is_constant(rval, var_map, exprn_opts, rval).
:- mode code_exprn__expr_is_constant(in, in, in, out) is semidet.

code_exprn__expr_is_constant(const(Const), _Vars, ExprnOpts, const(Const)) :-
	exprn_aux__const_is_constant(Const, ExprnOpts, yes).

code_exprn__expr_is_constant(unop(Op, Expr0), Vars, ExprnOpts,
		unop(Op, Expr)) :-
	code_exprn__expr_is_constant(Expr0, Vars, ExprnOpts, Expr).

code_exprn__expr_is_constant(binop(Op, Expr1, Expr2), Vars, ExprnOpts,
		binop(Op, Expr3, Expr4)) :-
	code_exprn__expr_is_constant(Expr1, Vars, ExprnOpts, Expr3),
	code_exprn__expr_is_constant(Expr2, Vars, ExprnOpts, Expr4).

code_exprn__expr_is_constant(mkword(Tag, Expr0), Vars, ExprnOpts,
		mkword(Tag, Expr)) :-
	code_exprn__expr_is_constant(Expr0, Vars, ExprnOpts, Expr).

code_exprn__expr_is_constant(create(Tag, Args0, Unique, Label), Vars, ExprnOpts,
		create(Tag, Args, Unique, Label)) :-
	ExprnOpts = nlg_asm_sgt_ubf(_, _, StaticGroundTerms, _),
	StaticGroundTerms = yes,
	code_exprn__args_are_constant(Args0, Vars, ExprnOpts, Args).

code_exprn__expr_is_constant(var(Var), Vars, ExprnOpts, Rval) :-
	map__search(Vars, Var, Stat),
	(
		Stat = cached(Rval0),
		code_exprn__expr_is_constant(Rval0, Vars, ExprnOpts, Rval)
	;
		Stat = evaled(Rvals),
		set__to_sorted_list(Rvals, RvalList),
		code_exprn__member_expr_is_constant(RvalList, Vars, ExprnOpts,
			Rval)
	).

:- pred code_exprn__args_are_constant(list(maybe(rval)), var_map,
					exprn_opts, list(maybe(rval))).
:- mode code_exprn__args_are_constant(in, in, in, out) is semidet.

code_exprn__args_are_constant([], _Vars, _ExprnOpts, []).
code_exprn__args_are_constant([Arg0 | Args0], Vars, ExprnOpts, [Arg | Args]) :-
	% if any of the fields are 'no' then we cannot treat the
	% term as a constant.
	Arg0 = yes(Rval0),
	code_exprn__expr_is_constant(Rval0, Vars, ExprnOpts, Rval),
	Arg = yes(Rval),
	code_exprn__args_are_constant(Args0, Vars, ExprnOpts, Args).

:- pred code_exprn__member_expr_is_constant(list(rval), var_map,
							exprn_opts, rval).
:- mode code_exprn__member_expr_is_constant(in, in, in, out) is semidet.

code_exprn__member_expr_is_constant([Rval0 | Rvals0], Vars, ExprnOpts, Rval) :-
	(
		code_exprn__expr_is_constant(Rval0, Vars, ExprnOpts, Rval1)
	->
		Rval = Rval1
	;
		code_exprn__member_expr_is_constant(Rvals0, Vars, ExprnOpts,
			Rval)
	).

%------------------------------------------------------------------------------%

code_exprn__cache_exprn(Var, Rval) -->
	code_exprn__get_vars(Vars0),
	(
		{ map__search(Vars0, Var, _) }
	->
		code_exprn__get_var_name(Var, Name),
		{ string__append("code_exprn__cache_exprn: existing definition of variable ", Name, Msg) },
		{ error(Msg) }
	;
		[]
	),
	code_exprn__add_rval_reg_dependencies(Rval),
	(
		{ exprn_aux__vars_in_rval(Rval, []) }
	->
		{ set__singleton_set(Rvals, Rval) },
		{ map__set(Vars0, Var, evaled(Rvals), Vars) }
	;
		{ map__set(Vars0, Var, cached(Rval), Vars) }
	),
	code_exprn__set_vars(Vars).

%------------------------------------------------------------------------------%

code_exprn__place_vars([], empty) --> [].
code_exprn__place_vars([Var - Lval | StoreMap], Code) -->
	code_exprn__place_var(Var, Lval, FirstCode),
	code_exprn__place_vars(StoreMap, RestCode),
	{ Code = tree(FirstCode, RestCode) }.

code_exprn__place_var(Var, Lval, Code) -->
	code_exprn__get_var_status(Var, Stat),
	(
		{ Stat = cached(Rval) },
		code_exprn__place_cached(Rval, Var, Lval, Code)
	;
		{ Stat = evaled(Rvals) },
		code_exprn__place_evaled(Rvals, Var, Lval, Code)
	).

:- pred code_exprn__place_cached(rval, var, lval, code_tree,
						exprn_info, exprn_info).
:- mode code_exprn__place_cached(in, in, in, out, in, out) is det.

code_exprn__place_cached(Rval0, Var, Lval, Code) -->
	code_exprn__get_vars(Vars0),
	(
		{ exprn_aux__vars_in_rval(Rval0, []) }
	->
		{ error("code_exprn__place_var: cached exprn with no vars!") }
	;
			% if the variable already has its value stored in the
			% right place, we don't need to generate any code
		{ Rval0 = var(Var1) },
		{ map__search(Vars0, Var1, Stat0) },
		{ Stat0 = evaled(VarRvals) },
		{ set__member(lval(Lval), VarRvals) }
	->
			% but we do need to reserve the registers
			% needed to access Lval
		code_exprn__add_lval_reg_dependencies(Lval),
		{ map__set(Vars0, Var, Stat0, Vars) },
		code_exprn__set_vars(Vars),
		{ Code = empty }
	;
			% If the value of the variable is a constant or
			% is built up by operations involving only constants,
			% get the constant form and assign that
		code_exprn__get_options(ExprnOpts),
		{ code_exprn__expr_is_constant(Rval0, Vars0, ExprnOpts, Rval) }
	->
		code_exprn__place_exprn(yes(Lval), yes(Var), Rval, yes, yes,
			_, Code)
	;
		code_exprn__place_exprn(yes(Lval), yes(Var), Rval0, no, no,
			_, Code)
	).

:- pred code_exprn__place_evaled(set(rval), var, lval, code_tree,
	exprn_info, exprn_info).
:- mode code_exprn__place_evaled(in, in, in, out, in, out) is det.

code_exprn__place_evaled(Rvals0, Var, Lval, Code) -->
	code_exprn__get_vars(Vars0),
	(
		{ set__member(lval(Lval), Rvals0) }
	->
		code_exprn__get_vars(Vars1),
		{ Stat = evaled(Rvals0) },
		{ map__set(Vars1, Var, Stat, Vars) },
		code_exprn__set_vars(Vars),
		{ Code = empty }
	;
		{ set__to_sorted_list(Rvals0, RvalList) },
		{ code_exprn__select_rval(RvalList, Rval0) },
		{
			Rval0 = lval(reg(_))
		;
			Rval0 = lval(stackvar(_))
		;
			Rval0 = lval(framevar(_))
		}
	->
		code_exprn__place_exprn(yes(Lval), yes(Var), Rval0, yes, no,
			_, Code)
	;
		{ set__to_sorted_list(Rvals0, RvalList0) },
		code_exprn__get_options(ExprnOpts),
		{ code_exprn__member_expr_is_constant(RvalList0,
				Vars0, ExprnOpts, Rval0) }
	->
		code_exprn__place_exprn(yes(Lval), yes(Var), Rval0, yes, yes,
			_, Code)
	;
		{ set__to_sorted_list(Rvals0, RvalList) },
		{ code_exprn__select_rval(RvalList, Rval) },
		code_exprn__place_exprn(yes(Lval), yes(Var), Rval, no, no,
			_, Code)
	).

	% code_exprn__place_arg(Rval, MaybeLval, Lval, Code):
	% generate the code required (if any) to materialize the value Rval
	% in some location, and return that location as Lval. If MaybeLval
	% is yes(Lval0), we must put the value into the location Lval0.

:- pred code_exprn__place_arg(rval, maybe(lval), lval, code_tree,
	exprn_info, exprn_info).
:- mode code_exprn__place_arg(in, in, out, out, in, out) is det.

code_exprn__place_arg(Rval0, MaybeLval, Lval, Code) -->
	code_exprn__get_vars(Vars0),
	(
			% If the variable already has its value stored in an
			% acceptable place, we don't need to generate any code.
		{ Rval0 = var(Var1) },
		{ map__search(Vars0, Var1, Stat0) },
		{ Stat0 = evaled(VarRvals) },
		{ set__to_sorted_list(VarRvals, RvalList) },
		{
			MaybeLval = no,
			code_exprn__select_rval(RvalList, BestVarRval),
			BestVarRval = lval(Lval0)
		;
			MaybeLval = yes(Lval0),
			list__member(lval(Lval0), RvalList)
		}
	->
		{ Lval = Lval0 },
		code_exprn__add_lval_reg_dependencies(Lval),
		{ Code = empty }
	;
			% If the value is a constant or is built up
			% by operations involving only constants,
			% get the constant form and assign that.
		code_exprn__get_options(ExprnOpts),
		{ code_exprn__expr_is_constant(Rval0, Vars0, ExprnOpts, Rval) }
	->
		code_exprn__place_exprn(MaybeLval, no, Rval, yes, yes, Lval,
			Code)
	;
		code_exprn__place_exprn(MaybeLval, no, Rval0, no, no, Lval,
			Code)
	).

:- pred code_exprn__place_exprn(maybe(lval), maybe(var), rval, bool, bool,
	lval, code_tree, exprn_info, exprn_info).
:- mode code_exprn__place_exprn(in, in, in, in, in, out, out, in, out) is det.

code_exprn__place_exprn(MaybeLval, MaybeVar, Rval0, StandAlone, IsConst,
		Lval, Code) -->
%	(
%		{ IsConst = no },
%		{ Rval0 = create(_, MaybeSubRvals, _) },
%		code_exprn__find_real_creates(MaybeSubRvals, CreateArgs)
%	->
%		% Look for SubRvals that are themselves creates.
%		% We prefer to materialize these variables *before* we lock
%		% Lval, as this allows the materialization code to use
%		% the register(s) needed by Lval (if it is given).
%
%		code_exprn__produce_args(CreateArgs, CreateArgLocs, ArgCode),
%		code_exprn__rem_rval_reg_dependencies(Rval0),
%		{ exprn_aux__substitute_rvals_in_rval(CreateArgLocs,
%			Rval0, Rval1) },
%		code_exprn__add_rval_reg_dependencies(Rval1)
%	;
		{ Rval1 = Rval0 },
		{ ArgCode = empty },
		{ CreateArgLocs = [] },
%	),
		% If we are not required to put the result
		% in any particular place, just pick a register.
	( { MaybeLval = yes(Lval0) } ->
		{ Lval = Lval0 },
		% move stuff out of the way, and heed any changes
		% this produces in the form of the expression
		code_exprn__clear_lval(Lval, Rval1, Rval2, ClearCode)
	;
		code_exprn__get_spare_reg(Reg),
		{ Lval = reg(Reg) },
		{ Rval2 = Rval1 },
		{ ClearCode = empty }
	),
		% reserve the target lval
	code_exprn__add_lval_reg_dependencies(Lval),
	code_exprn__maybe_get_var_name(MaybeVar, VarName),
	( { IsConst = yes } ->
		{ string__append("Assigning from ", VarName, Comment) },
		{ ExprnCode = node([assign(Lval, Rval2) - Comment]) }
	;
		( { StandAlone = yes } ->
			{ VarCode = empty },
			{ Rval = Rval2 }
		;
			{ exprn_aux__vars_in_rval(Rval2, VarList) },
			code_exprn__produce_vars(VarList, VarLocList, VarCode),
			code_exprn__rem_rval_reg_dependencies(Rval2),
			{ exprn_aux__substitute_vars_in_rval(VarLocList,
				Rval2, Rval) },
			code_exprn__add_rval_reg_dependencies(Rval),
			code_exprn__maybe_set_evaled(MaybeVar, [Rval])
		),
		code_exprn__construct_code(Lval, VarName, Rval, RealCode),
		{ ExprnCode = tree(VarCode, RealCode) }
	),
	code_exprn__release_arglocs(CreateArgLocs),
	code_exprn__maybe_add_evaled(MaybeVar, lval(Lval)),
	code_exprn__maybe_fix_clearcode(ClearCode, ExprnCode, ClearExprnCode),
	{ Code = tree(ArgCode, ClearExprnCode) }.

:- pred code_exprn__maybe_fix_clearcode(code_tree, code_tree, code_tree,
	exprn_info, exprn_info).
:- mode code_exprn__maybe_fix_clearcode(in, in, out, in, out) is det.

code_exprn__maybe_fix_clearcode(ClearCode, ExprnCode, Code) -->
	(
		{ ClearCode = node([assign(reg(Reg), lval(Lval)) - _]) },
		{ tree__flatten(ExprnCode, ExprnListList) },
		{ list__condense(ExprnListList, ExprnList) },
		{ ExprnList = [assign(Target, Rval0) - Comment] },
		code_exprn__clear_lval_of_synonyms(reg(Reg)),
		\+ code_exprn__lval_in_use(reg(Reg))
	->
		{ exprn_aux__substitute_rval_in_rval(lval(reg(Reg)), lval(Lval),
			Rval0, Rval) },
		{ Code = node([assign(Target, Rval) - Comment]) }
	;
		{ Code = tree(ClearCode, ExprnCode) }
	).

code_exprn__materialize_vars_in_rval(Rval0, Rval, Code) -->
	{ exprn_aux__vars_in_rval(Rval0, VarList) },
	code_exprn__produce_vars(VarList, VarLocList, Code),
	{ exprn_aux__substitute_vars_in_rval(VarLocList, Rval0, Rval) }.

%------------------------------------------------------------------------------%

:- pred code_exprn__find_real_creates(list(maybe(rval)), list(rval),
	exprn_info, exprn_info).
:- mode code_exprn__find_real_creates(in, out, in, out) is det.

code_exprn__find_real_creates([], []) --> [].
code_exprn__find_real_creates([MaybeRval | MaybeRvals], SubCreates) -->
	code_exprn__find_real_creates(MaybeRvals, SubCreates1),
	(
		{ MaybeRval = yes(Rval) },
		code_exprn__rval_is_real_create(Rval)
	->
		{ SubCreates = [Rval | SubCreates1] }
	;
		{ SubCreates = SubCreates1 }
	).

:- pred code_exprn__rval_is_real_create(rval, exprn_info, exprn_info).
:- mode code_exprn__rval_is_real_create(in, in, out) is semidet.

code_exprn__rval_is_real_create(Rval) -->
	(
		{ Rval = create(_, _, _, _) },
		code_exprn__get_vars(Vars0),
		code_exprn__get_options(ExprnOpts),
		{ \+ code_exprn__expr_is_constant(Rval, Vars0, ExprnOpts, _) }
	;
		{ Rval = var(Var) },
		code_exprn__get_var_status(Var, Stat),
		(
			{ Stat = cached(RealRval) }
		;
			{ Stat = evaled(Rvals) },
			{ set__to_sorted_list(Rvals, RvalList) },
			{ code_exprn__select_rval(RvalList, RealRval) }
		),
		code_exprn__rval_is_real_create(RealRval)
	).

%------------------------------------------------------------------------------%

:- pred code_exprn__construct_code(lval, string, rval, code_tree,
	exprn_info, exprn_info).
:- mode code_exprn__construct_code(in, in, in, out, in, out) is det.

code_exprn__construct_code(Lval, VarName, Rval0, Code) -->
	{ exprn_aux__simplify_rval(Rval0, Rval) },
	(
		{ Rval = create(Tag, Rvals, _Unique, _Label) }
	->
		{ list__length(Rvals, Arity) },
		(
			{ Arity = 0 }
		->
			{ Code = node([
				assign(Lval, mkword(Tag, const(int_const(0)))) -
					"Construct constant"
			]) }
		;
			( { Lval = field(_, _, _) } ->
				code_exprn__acquire_reg(Reg),
				code_exprn__construct_cell(reg(Reg),
					VarName, Tag, Arity, Rvals, Code0),
				{ string__append(VarName, " placement",
					Comment) },
				{ Code1 = node([
					assign(Lval, lval(reg(Reg))) - Comment
				]) },
				{ Code = tree(Code0, Code1) },
				code_exprn__release_reg(Reg)
			;
				code_exprn__construct_cell(Lval,
					VarName, Tag, Arity, Rvals, Code)
			)
		)
	;
		{ string__append("Assigning from ", VarName, Comment) },
		{ Code = node([assign(Lval, Rval) - Comment]) }
	).

:- pred code_exprn__construct_cell(lval, string, tag, int, list(maybe(rval)),
	code_tree, exprn_info, exprn_info).
:- mode code_exprn__construct_cell(in, in, in, in, in, out, in, out) is det.

code_exprn__construct_cell(Lval, VarName, Tag, Arity, Rvals, Code) -->
	{ string__append("Allocating heap for ", VarName, Comment) },
	{ Code0 = node([
		incr_hp(Lval, yes(Tag), const(int_const(Arity))) - Comment
	]) },
	code_exprn__construct_args(Rvals, Tag, Lval, 0, Targets, Code1),
	code_exprn__free_arg_dependenciess(Targets),
	{ Code = tree(Code0, Code1) }.

:- pred code_exprn__construct_args(list(maybe(rval)), int, lval, int,
	list(lval), code_tree, exprn_info, exprn_info).
:- mode code_exprn__construct_args(in, in, in, in, out, out, in, out) is det.

code_exprn__construct_args([], _, _, _, [], empty) --> [].
code_exprn__construct_args([R | Rs], Tag, Lval, N0, Targets, Code) -->
	(
		{ R = yes(Rval) }
	->
		{ Target0 = field(Tag, lval(Lval), const(int_const(N0))) },
		{ MaybeTarget = yes(Target0) },
		code_exprn__place_arg(Rval, yes(Target0), _, Code0)
	;
		{ Code0 = empty },
		{ MaybeTarget = no }
	),
	{ N1 is N0 + 1 },
	code_exprn__construct_args(Rs, Tag, Lval, N1, Targets1, Code1),
	{ Code = tree(Code0, Code1) },
	{
		MaybeTarget = yes(Target),
		Targets = [Target | Targets1]
	;
		MaybeTarget = no,
		Targets = Targets1
	}.

:- pred code_exprn__free_arg_dependenciess(list(lval), exprn_info, exprn_info).
:- mode code_exprn__free_arg_dependenciess(in, in, out) is det.

code_exprn__free_arg_dependenciess([]) --> [].
code_exprn__free_arg_dependenciess([Target | Targets]) -->
	code_exprn__rem_lval_reg_dependencies(Target),
	code_exprn__free_arg_dependenciess(Targets).

%------------------------------------------------------------------------------%

:- pred code_exprn__produce_vars(list(var), assoc_list(var, rval), code_tree,
						exprn_info, exprn_info).
:- mode code_exprn__produce_vars(in, out, out, in, out) is det.

code_exprn__produce_vars([], [], empty) --> [].
code_exprn__produce_vars([V | Vs], [V - R | Rest], Code) -->
	code_exprn__produce_var(V, R, Code0),
	code_exprn__produce_vars(Vs, Rest, Code1),
	{ Code = tree(Code0, Code1) }.

%------------------------------------------------------------------------------%

code_exprn__produce_var(Var, Rval, Code) -->
	code_exprn__get_var_status(Var, Stat),
	(
		{ Stat = evaled(Rvals) },
		\+ (
			{ set__member(RvalX, Rvals) },
			{
				RvalX = binop(_, _, _)
			;
				RvalX = unop(_, _)
			;
				RvalX = create(_, _, _, _)
			;
				RvalX = mkword(_, _)
			}
		)
	->
		{ set__to_sorted_list(Rvals, RvalList) },
		{ code_exprn__select_rval(RvalList, Rval) },
		{ Code = empty }
	;
		code_exprn__get_spare_reg(Reg),
		{ Lval = reg(Reg) },
		{ Rval = lval(Lval) },
		code_exprn__place_var(Var, Lval, Code)
	).

%------------------------------------------------------------------------------%

code_exprn__produce_var_in_reg(Var, Rval, Code) -->
	code_exprn__produce_var(Var, Rval0, Code0),
	(
		{ Rval0 = lval(reg(_)) }
	->
		{ Code = Code0 },
		{ Rval = Rval0 }
	;
		code_exprn__get_spare_reg(Reg),
		{ Lval = reg(Reg) },
		code_exprn__place_var(Var, Lval, Code1),
		{ Rval = lval(Lval) },
		{ Code = tree(Code0, Code1) }
	).

code_exprn__produce_var_in_reg_or_stack(Var, Rval, Code) -->
	code_exprn__produce_var(Var, Rval0, Code0),
	(
		{ Rval0 = lval(Loc) },
		{ Loc = reg(_) ; Loc = stackvar(_) ; Loc = framevar(_) }
	->
		{ Code = Code0 },
		{ Rval = Rval0 }
	;
		code_exprn__get_spare_reg(Reg),
		{ Lval = reg(Reg) },
		code_exprn__place_var(Var, Lval, Code1),
		{ Rval = lval(Lval) },
		{ Code = tree(Code0, Code1) }
	).

%------------------------------------------------------------------------------%

:- pred code_exprn__produce_args(list(rval), assoc_list(rval, rval),
	code_tree, exprn_info, exprn_info).
:- mode code_exprn__produce_args(in, out, out, in, out) is det.

code_exprn__produce_args([], [], empty) --> [].
code_exprn__produce_args([Arg | Args], [Arg - Loc | ArgLocs], Code) -->
	code_exprn__produce_arg(Arg, Loc, Code0),
	code_exprn__produce_args(Args, ArgLocs, Code1),
	{ Code = tree(Code0, Code1) }.

:- pred code_exprn__produce_arg(rval, rval, code_tree, exprn_info, exprn_info).
:- mode code_exprn__produce_arg(in, out, out, in, out) is det.

code_exprn__produce_arg(Rval0, Rval, Code) -->
	code_exprn__place_arg(Rval0, no, Lval, Code),
	{ Rval = lval(Lval) }.

:- pred code_exprn__release_arglocs(assoc_list(rval, rval),
	exprn_info, exprn_info).
:- mode code_exprn__release_arglocs(in, in, out) is det.

code_exprn__release_arglocs([]) --> [].
code_exprn__release_arglocs([_ - ArgLoc | ArgLocs]) -->
	( { ArgLoc = lval(reg(Reg)) } ->
		code_exprn__unlock_reg(Reg)
	;
		{ error("non-register subargument location in code_exprn") }
	),
	code_exprn__release_arglocs(ArgLocs).

%------------------------------------------------------------------------------%

	% Move whatever is in r1 out of the way.

code_exprn__clear_r1(Code) -->
	code_exprn__clear_lval_return_shuffle(reg(r(1)), _, Code).

	% Move whatever is in Lval out of the way.
	% It is possible that the value we want to put into Lval
	% may need to refer to the value being moved out of Lval,
	% e.g. as the base register of a field reference.
	% To handle these correctly, we allow the caller to tell us
	% the Rval0 that will go into Lval, and we adjust it as necessary
	% before returning it as Rval.

:- pred code_exprn__clear_lval(lval, rval, rval, code_tree,
	exprn_info, exprn_info).
:- mode code_exprn__clear_lval(in, in, out, out, in, out) is det.

code_exprn__clear_lval(Lval, Rval0, Rval, Code) -->
	code_exprn__clear_lval_return_shuffle(Lval, MaybeShuffle, Code),
	(
		{ MaybeShuffle = yes(NewLval) }
	->
		{ exprn_aux__substitute_lval_in_rval(Lval, NewLval,
			Rval0, Rval) }
	;
		{ Rval = Rval0 }
	).

:- pred code_exprn__clear_lval_return_shuffle(lval, maybe(lval), code_tree,
	exprn_info, exprn_info).
:- mode code_exprn__clear_lval_return_shuffle(in, out, out, in, out) is det.

code_exprn__clear_lval_return_shuffle(Lval, MaybeShuffle, Code) -->
	(
		code_exprn__lval_in_use(Lval)
	->
		code_exprn__get_spare_reg(Reg),
		code_exprn__get_vars(Vars0),
		{ map__to_assoc_list(Vars0, VarsList0) },
		code_exprn__relocate_lval(VarsList0, Lval, reg(Reg), VarsList),
		{ map__from_assoc_list(VarsList, Vars) },
		code_exprn__set_vars(Vars),
		{ MaybeShuffle = yes(reg(Reg)) },
		{ Code = node([assign(reg(Reg), lval(Lval)) - "shuffle lval"]) }
	;
		{ MaybeShuffle = no },
		{ Code = empty }
	).

:- pred code_exprn__relocate_lval(assoc_list(var, var_stat), lval, lval,
			assoc_list(var, var_stat), exprn_info, exprn_info).
:- mode code_exprn__relocate_lval(in, in, in, out, in, out) is det.

code_exprn__relocate_lval([], _OldVal, _NewVal, []) --> [].
code_exprn__relocate_lval([V - Stat0 | Rest0], OldVal,
				NewVal, [V - Stat | Rest]) -->
	(
		{ Stat0 = cached(Exprn0) },
		(
			{ exprn_aux__rval_contains_lval(Exprn0, OldVal) }
		->
			code_exprn__rem_rval_reg_dependencies(Exprn0),
			{ exprn_aux__substitute_lval_in_rval(OldVal, NewVal,
							Exprn0, Exprn) },
			code_exprn__add_rval_reg_dependencies(Exprn),
			{ Stat = cached(Exprn) }
		;
			{ Stat = Stat0 }
		)
	;
		{ Stat0 = evaled(Rvals0) },
		{ set__to_sorted_list(Rvals0, RvalsList0) },
		code_exprn__relocate_lval_2(RvalsList0, OldVal,
							NewVal, RvalsList),
		{ set__sorted_list_to_set(RvalsList, Rvals) },
		{ Stat = evaled(Rvals) }
	),
	code_exprn__relocate_lval(Rest0, OldVal, NewVal, Rest).

:- pred code_exprn__relocate_lval_2(list(rval), lval, lval, list(rval),
							exprn_info, exprn_info).
:- mode code_exprn__relocate_lval_2(in, in, in, out, in, out) is det.

code_exprn__relocate_lval_2([], _OldVal, _NewVal, []) --> [].
code_exprn__relocate_lval_2([R0 | Rs0], OldVal, NewVal, [R | Rs]) -->
	(
		{ exprn_aux__rval_contains_lval(R0, OldVal) }
	->
		code_exprn__rem_rval_reg_dependencies(R0),
		{ exprn_aux__substitute_lval_in_rval(OldVal, NewVal, R0, R) },
		code_exprn__add_rval_reg_dependencies(R)
	;
		{ R = R0 }
	),
	code_exprn__relocate_lval_2(Rs0, OldVal, NewVal, Rs).

%------------------------------------------------------------------------------%

:- pred code_exprn__get_var_status(var, var_stat, exprn_info, exprn_info).
:- mode code_exprn__get_var_status(in, out, in, out) is det.

code_exprn__get_var_status(Var, Stat) -->
	code_exprn__get_vars(Vars0),
	(
		{ map__search(Vars0, Var, Stat0) }
	->
		{ Stat = Stat0 }
	;
		code_exprn__get_var_name(Var, Name),
		{ term__var_to_int(Var, Num) },
		{ string__int_to_string(Num, NumStr) },
		{ string__append_list(["variable ", Name, " (", NumStr,
			") not found"], Msg) },
		{ error(Msg) }
	).

:- pred code_exprn__maybe_set_evaled(maybe(var), list(rval),
	exprn_info, exprn_info).
:- mode code_exprn__maybe_set_evaled(in, in, in, out) is det.

code_exprn__maybe_set_evaled(no, _) --> [].
code_exprn__maybe_set_evaled(yes(Var), RvalList) -->
	code_exprn__get_vars(Vars0),
	{ set__list_to_set(RvalList, Rvals) },
	{ Stat = evaled(Rvals) },
	{ map__set(Vars0, Var, Stat, Vars) },
	code_exprn__set_vars(Vars).

:- pred code_exprn__maybe_add_evaled(maybe(var), rval, exprn_info, exprn_info).
:- mode code_exprn__maybe_add_evaled(in, in, in, out) is det.

code_exprn__maybe_add_evaled(no, _) --> [].
code_exprn__maybe_add_evaled(yes(Var), NewRval) -->
	code_exprn__get_vars(Vars0),
	{ map__lookup(Vars0, Var, Stat0) },
	{
		Stat0 = evaled(Rvals0),
		set__insert(Rvals0, NewRval, Rvals)
	;
		Stat0 = cached(_),
		set__singleton_set(Rvals, NewRval)
	},
	{ Stat = evaled(Rvals) },
	{ map__set(Vars0, Var, Stat, Vars) },
	code_exprn__set_vars(Vars).

%------------------------------------------------------------------------------%

	% Warning: if you get a reg, you must mark it as in use yourself.

:- pred code_exprn__get_spare_reg(reg, exprn_info, exprn_info).
:- mode code_exprn__get_spare_reg(out, in, out) is det.

code_exprn__get_spare_reg(Reg) -->
	code_exprn__get_regs(Regs),
	{ code_exprn__get_spare_reg_2(1, Regs, Reg) }.

:- pred code_exprn__get_spare_reg_2(int, bag(reg), reg).
:- mode code_exprn__get_spare_reg_2(in, in, out) is det.

code_exprn__get_spare_reg_2(N0, Regs, Reg) :-
	Reg0 = r(N0),
	(
		bag__contains(Reg0, Regs)
	->
		N1 is N0 + 1,
		code_exprn__get_spare_reg_2(N1, Regs, Reg)
	;
		Reg = Reg0
	).

%------------------------------------------------------------------------------%

code_exprn__acquire_reg(Reg) -->
	code_exprn__get_spare_reg(Reg),
	code_exprn__get_regs(Regs0),
	{ bag__insert(Regs0, Reg, Regs) },
	code_exprn__set_regs(Regs),
	code_exprn__get_acquired(Acqu0),
	{ set__insert(Acqu0, Reg, Acqu) },
	code_exprn__set_acquired(Acqu).

code_exprn__release_reg(Reg) -->
	code_exprn__get_acquired(Acqu0),
	(
		{ set__member(Reg, Acqu0) }
	->
		{ set__delete(Acqu0, Reg, Acqu) },
		code_exprn__set_acquired(Acqu),
		code_exprn__get_regs(Regs0),
		{ bag__remove(Regs0, Reg, Regs) },
		(
			{ bag__contains(Reg, Regs) }
		->
			{ error("code_exprn__release_reg: reg still has references") }
		;
			[]
		),
		code_exprn__set_regs(Regs)
	;
		{ error("code_exprn__release_reg: attempt to release an unacquired reg") }
	).

%------------------------------------------------------------------------------%

code_exprn__lock_reg(Reg) -->
	code_exprn__get_regs(Regs0),
	{ bag__insert(Regs0, Reg, Regs) },
	code_exprn__set_regs(Regs).

code_exprn__unlock_reg(Reg) -->
	code_exprn__get_regs(Regs0),
	{ bag__remove(Regs0, Reg, Regs) },
	code_exprn__set_regs(Regs).

%------------------------------------------------------------------------------%

:- pred code_exprn__maybe_get_var_name(maybe(var), string,
	exprn_info, exprn_info).
:- mode code_exprn__maybe_get_var_name(in, out, in, out) is det.

code_exprn__maybe_get_var_name(no, "unknown variable") --> [].
code_exprn__maybe_get_var_name(yes(Var), Name) -->
	code_exprn__get_var_name(Var, Name).

:- pred code_exprn__get_var_name(var, string, exprn_info, exprn_info).
:- mode code_exprn__get_var_name(in, out, in, out) is det.

code_exprn__get_var_name(Var, Name) -->
	code_exprn__get_varset(Varset),
	{ varset__lookup_name(Varset, Var, Name) }.

%------------------------------------------------------------------------------%

:- pred code_exprn__get_varset(varset, exprn_info, exprn_info).
:- mode code_exprn__get_varset(out, in, out) is det.

:- pred code_exprn__set_varset(varset, exprn_info, exprn_info).
:- mode code_exprn__set_varset(in, in, out) is det.

:- pred code_exprn__get_vars(var_map, exprn_info, exprn_info).
:- mode code_exprn__get_vars(out, in, out) is det.

:- pred code_exprn__set_vars(var_map, exprn_info, exprn_info).
:- mode code_exprn__set_vars(in, in, out) is det.

:- pred code_exprn__get_regs(bag(reg), exprn_info, exprn_info).
:- mode code_exprn__get_regs(out, in, out) is det.

:- pred code_exprn__set_regs(bag(reg), exprn_info, exprn_info).
:- mode code_exprn__set_regs(in, in, out) is det.

:- pred code_exprn__get_acquired(set(reg), exprn_info, exprn_info).
:- mode code_exprn__get_acquired(out, in, out) is det.

:- pred code_exprn__set_acquired(set(reg), exprn_info, exprn_info).
:- mode code_exprn__set_acquired(in, in, out) is det.

:- pred code_exprn__get_options(exprn_opts, exprn_info, exprn_info).
:- mode code_exprn__get_options(out, in, out) is det.

code_exprn__get_varset(Varset, ExprnInfo, ExprnInfo) :-
	ExprnInfo = exprn_info(Varset, _Vars, _Regs, _Acqu, _Opt).

code_exprn__set_varset(Varset, ExprnInfo0, ExprnInfo) :-
	ExprnInfo0 = exprn_info(_Varset, Vars, Regs, Acqu, Opt),
	ExprnInfo = exprn_info(Varset, Vars, Regs, Acqu, Opt).

code_exprn__get_vars(Vars, ExprnInfo, ExprnInfo) :-
	ExprnInfo = exprn_info(_Varset, Vars, _Regs, _Acqu, _Opt).

code_exprn__set_vars(Vars, ExprnInfo0, ExprnInfo) :-
	ExprnInfo0 = exprn_info(Varset, _Vars, Regs, Acqu, Opt),
	ExprnInfo = exprn_info(Varset, Vars, Regs, Acqu, Opt).

code_exprn__get_regs(Regs, ExprnInfo, ExprnInfo) :-
	ExprnInfo = exprn_info(_Varset, _Vars, Regs, _Acqu, _Opt).

code_exprn__set_regs(Regs, ExprnInfo0, ExprnInfo) :-
	ExprnInfo0 = exprn_info(Varset, Vars, _Regs, Acqu, Opt),
	ExprnInfo = exprn_info(Varset, Vars, Regs, Acqu, Opt).

code_exprn__get_acquired(Acqu, ExprnInfo, ExprnInfo) :-
	ExprnInfo = exprn_info(_Varset, _Vars, _Regs, Acqu, _Opt).

code_exprn__set_acquired(Acqu, ExprnInfo0, ExprnInfo) :-
	ExprnInfo0 = exprn_info(Varset, Vars, Regs, _Acqu, Opt),
	ExprnInfo = exprn_info(Varset, Vars, Regs, Acqu, Opt).

code_exprn__get_options(Opt, ExprnInfo, ExprnInfo) :-
	ExprnInfo = exprn_info(_Varset, _Vars, _Regs, _Acqu, Opt).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

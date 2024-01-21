%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: make_hlds.m.
% Main author: fjh.

% This module converts from the parse tree structure which is read in by
% prog_io.m, into the simplified high level data structure defined in
% hlds.m.  In the parse tree, the program is represented as a list of
% items; we insert each item into the appropriate symbol table, and report
% any duplicate definition errors.  We also transform clause bodies from
% (A,B,C) into conj([A,B,C]) form, convert all unifications into
% super-homogenous form, and introduce implicit quantification.
% 
% XXX we should record each error using module_info_incr_errors.

% WISHLIST - we should handle explicit module quantification

:- module make_hlds.
:- interface.

:- import_module prog_data, hlds_module, hlds_pred, hlds_goal, hlds_data.
:- import_module equiv_type.

:- import_module io, std_util.

% parse_tree_to_hlds(ParseTree, EqvMap, HLDS, UndefTypes, UndefModes):
%	Given EqvMap, converts ParseTree to HLDS.
%	Any errors found are recorded in the HLDS num_errors field.
%	Returns UndefTypes = yes if undefined types found.
%	Returns UndefModes = yes if undefined modes found.
:- pred parse_tree_to_hlds(program, eqv_map, module_info, bool, bool,
			io__state, io__state).
:- mode parse_tree_to_hlds(in, in, out, out, out, di, uo) is det.

:- pred create_atomic_unification(var, unify_rhs, term__context,
			unify_main_context, unify_sub_contexts, hlds__goal).
:- mode create_atomic_unification(in, in, in, in, in, out) is det.

:- pred add_new_proc(pred_info, arity, list(mode), maybe(list(is_live)),
		maybe(determinism), term__context, pred_info, proc_id).
:- mode add_new_proc(in, in, in, in, in, in, out, out) is det.

:- pred clauses_info_init(int::in, clauses_info::out) is det.

:- pred next_mode_id(proc_table, maybe(determinism), proc_id).
:- mode next_mode_id(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module string, char, int, set, bintree, list, map, require.
:- import_module bool, getopt, assoc_list, term, term_io, varset.

:- import_module module_qual, prog_util, prog_io, prog_out, hlds_out.
:- import_module globals, options.
:- import_module make_tags, quantification, shapes.
:- import_module code_util, unify_proc, special_pred, type_util, mode_util.
:- import_module mercury_to_mercury, passes_aux, clause_to_proc, inst_match.
:- import_module fact_table.

parse_tree_to_hlds(module(Name, Items), EqvMap, Module, UndefTypes, UndefModes)
		-->
	globals__io_get_globals(Globals),
	{ module_info_init(Name, Globals, Module0) },
	add_item_list_decls_pass_1(Items, local, Module0, Module1),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
	add_item_list_decls_pass_2(Items, local, Module1, Module2),
	maybe_report_stats(Statistics),
		% balance the binary trees
	{ module_info_optimize(Module2, Module3) },
	maybe_report_stats(Statistics),
	{ init_mq_info_module(Module3, MQInfo0) },
	{ init_qual_info(MQInfo0, EqvMap, Info0) },
	add_item_list_clauses(Items, local, Module3, Module4,
				Info0, Info),
	{ qual_info_get_mq_info(Info, MQInfo) },
	{ mq_info_get_type_error_flag(MQInfo, UndefTypes) },
	{ mq_info_get_mode_error_flag(MQInfo, UndefModes) },
	{ mq_info_get_num_errors(MQInfo, MQ_NumErrors) },
	{ module_info_num_errors(Module4, NumErrors0) },
	{ NumErrors is NumErrors0 + MQ_NumErrors },
	{ module_info_set_num_errors(Module4, NumErrors, Module5) },
		% the predid list is constructed in reverse order, for
		% efficiency, so we return it to the correct order here.
	{ module_info_reverse_predids(Module5, Module) }.

%-----------------------------------------------------------------------------%

	% pass 1:
	% Add the declarations one by one to the module,
	% except for type definitions and pragmas.

:- pred add_item_list_decls_pass_1(item_list, import_status,
				module_info, module_info,
				io__state, io__state).
:- mode add_item_list_decls_pass_1(in, in, in, out, di, uo) is det.

add_item_list_decls_pass_1([], _, Module, Module) --> [].
add_item_list_decls_pass_1([Item - Context | Items], Status0, Module0, Module)
		-->
	add_item_decl_pass_1(Item, Context, Status0, Module0, Status1, Module1),
	add_item_list_decls_pass_1(Items, Status1, Module1, Module).

	% pass 2:
	% Add the type definitions and pragmas one by one to the module,
	% and add default modes for functions with no mode declaration.
	%
	% Adding type definitions needs to come after we have added the
	% pred declarations,
	% since we need to have the pred_id for `index/2' and `compare/3'
	% when we add compiler-generated clauses for `compare/3'.
	% (And similarly for other compiler-generated predicates like that.)
	%
	% Adding pragmas needs to come after we have added the
	% pred declarations, in order to allow the pragma declarations 
	% for a predicate to syntactically precede the pred declaration.
	%
	% Adding default modes for functions needs to come after we have
	% have processed all the mode declarations, since otherwise we
	% can't be sure that there isn't a mode declaration for the function.

:- pred add_item_list_decls_pass_2(item_list, import_status,
		module_info, module_info, io__state, io__state).
:- mode add_item_list_decls_pass_2(in, in, in, out, di, uo) is det.

add_item_list_decls_pass_2([], _, Module, Module) --> [].
add_item_list_decls_pass_2([Item - Context | Items], Status0, Module0, Module)
		-->
	add_item_decl_pass_2(Item, Context, Status0, Module0, Status1, Module1),
	add_item_list_decls_pass_2(Items, Status1, Module1, Module).

	% pass 3:
	% add the clauses one by one to the module
	% (I supposed this could conceivably be folded into pass 2?)

:- pred add_item_list_clauses(item_list, import_status, module_info,
		module_info, qual_info, qual_info, io__state, io__state).
:- mode add_item_list_clauses(in, in, in, out, in, out, di, uo) is det.

add_item_list_clauses([], _Status, Module, Module, Info, Info) --> [].
add_item_list_clauses([Item - Context | Items], Status0,
		Module0, Module, Info0, Info) -->
	add_item_clause(Item, Status0, Status1, Context,
			Module0, Module1, Info0, Info1),
	add_item_list_clauses(Items, Status1, Module1, Module, Info1, Info).

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_decl_pass_1(item, term__context, import_status, module_info,
			import_status, module_info, io__state, io__state).
:- mode add_item_decl_pass_1(in, in, in, in, out, out, di, uo) is det.

	% skip clauses
add_item_decl_pass_1(pred_clause(_, _, _, _), _, Status, Module, Status, Module)
		--> [].
add_item_decl_pass_1(func_clause(_, _, _, _, _), _, Status, Module, Status,
		Module) --> [].

add_item_decl_pass_1(type_defn(_, _, _), _, Status, Module, Status, Module)
		--> [].

add_item_decl_pass_1(inst_defn(VarSet, InstDefn, Cond), Context,
		Status, Module0, Status, Module) -->
	module_add_inst_defn(Module0, VarSet, InstDefn, Cond, Context,
			Status, Module).

add_item_decl_pass_1(mode_defn(VarSet, ModeDefn, Cond), Context,
		Status, Module0, Status, Module) -->
	module_add_mode_defn(Module0, VarSet, ModeDefn, Cond, Context,
			Status, Module).

add_item_decl_pass_1(pred(VarSet, PredName, TypesAndModes, MaybeDet, Cond),
		Context, Status, Module0, Status, Module) -->
	module_add_pred(Module0, VarSet, PredName, TypesAndModes, MaybeDet,
		Cond, Context, Status, Module).

add_item_decl_pass_1(func(VarSet, FuncName, TypesAndModes, RetTypeAndMode,
		MaybeDet, Cond), Context, Status, Module0, Status, Module) -->
	module_add_func(Module0, VarSet, FuncName, TypesAndModes,
		RetTypeAndMode, MaybeDet, Cond, Context, Status, Module).

add_item_decl_pass_1(pred_mode(VarSet, PredName, Modes, MaybeDet, Cond),
		Context, Status, Module0, Status, Module) -->
	module_add_mode(Module0, VarSet, PredName, Modes, MaybeDet, Cond,
		Context, predicate, Module).

add_item_decl_pass_1(func_mode(VarSet, FuncName, Modes, RetMode, MaybeDet,
		Cond), Context, Status, Module0, Status, Module) -->
	{ list__append(Modes, [RetMode], Modes1) },
	module_add_mode(Module0, VarSet, FuncName, Modes1,
		MaybeDet, Cond, Context, function, Module).

add_item_decl_pass_1(pragma(_), _, Status, Module, Status, Module) --> [].

add_item_decl_pass_1(module_defn(_VarSet, ModuleDefn), Context,
		Status0, Module0, Status, Module) -->
	( { module_defn_update_import_status(ModuleDefn, Status1) } ->
		{ Status = Status1 },
		{ Module = Module0 }
	; { ModuleDefn = import(module(_)) } ->
		{ Status = Status0 },
		{ Module = Module0 }
	; { ModuleDefn = external(name_arity(Name, Arity)) } ->
		{ Status = Status0 },
		module_mark_as_external(Name, Arity, Context, Module0, Module)
	;
		{ Status = Status0 },
		{ Module = Module0 },
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		prog_out__write_context(Context),
		report_warning("Warning: declaration not yet implemented.\n"),
		io__set_output_stream(OldStream, _)
	).

add_item_decl_pass_1(nothing, _, Status, Module, Status, Module) --> [].

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_decl_pass_2(item, term__context, import_status, module_info,
			import_status, module_info,
			io__state, io__state).
:- mode add_item_decl_pass_2(in, in, in, in, out, out, di, uo) is det.

add_item_decl_pass_2(module_defn(_VarSet, ModuleDefn), _Context,
		Status0, Module, Status, Module) -->
	{ module_defn_update_import_status(ModuleDefn, Status1) ->
		Status = Status1
	;
		Status = Status0
	}.

add_item_decl_pass_2(type_defn(VarSet, TypeDefn, Cond), Context,
		Status, Module0, Status, Module) -->
	module_add_type_defn(Module0, VarSet, TypeDefn, Cond, Context, Status,
		Module).

add_item_decl_pass_2(pragma(Pragma), Context, Status, Module0, Status, Module)
		-->
	(
		% ignore `pragma source_file' declarations - they're dealt
		% with elsewhere
		{ Pragma = source_file(_) },
		{ Module = Module0 }
	;
		{ Pragma = c_code(C_Body_Code) },
		{ module_add_c_body_code(C_Body_Code, Context,
			Module0, Module) }
	;
		{ Pragma  = c_header_code(C_Header) },
		{ module_add_c_header(C_Header, Context, Module0, Module) }
	;
		% Handle pragma c_code decls later on (when we process
		% clauses).
		{ Pragma = c_code(_, _, _, _, _, _) },
		{ Module = Module0 }
	;
		{ Pragma = memo(Name, Arity) },
		add_pred_marker(Module0, "memo", Name, Arity, Context,
			[request(memo)], Module1),
		add_stratified_pred(Module1, "memo", Name, Arity, Context, 
			Module)
	;
		{ Pragma = inline(Name, Arity) },
		add_pred_marker(Module0, "inline", Name, Arity, Context,
			[request(inline)], Module)
	;
		{ Pragma = obsolete(Name, Arity) },
		add_pred_marker(Module0, "obsolete", Name, Arity, Context,
			[request(obsolete)], Module)
	;
		% XXX should handle pragma(export) for functions better
		{ Pragma = export(Name, Modes, C_Function) },
		{ module_info_get_predicate_table(Module0, PredTable) },
		{ list__length(Modes, Arity) },
		(
			{ predicate_table_search_sym_arity(PredTable,
				Name, Arity, [PredId]) }
		->
			{ predicate_table_get_preds(PredTable, Preds) },
			{ map__lookup(Preds, PredId, PredInfo) },
			{ pred_info_procedures(PredInfo, Procs) },
			{ map__to_assoc_list(Procs, ExistingProcs) },
			(
				{ get_matching_procedure(ExistingProcs, Modes, 
					Module0, ProcId) }
			->
				{ module_info_get_pragma_exported_procs(Module0,
					PragmaExportedProcs0) },
				{ NewExportedProc = pragma_exported_proc(PredId,
					ProcId, C_Function) },
				{ PragmaExportedProcs = 
					[NewExportedProc|PragmaExportedProcs0]},
				{ module_info_set_pragma_exported_procs(Module0,
					PragmaExportedProcs, Module) }
			;
				undefined_mode_error(Name, Arity, Context,
					"`:- pragma export' declaration"),
				{ module_info_incr_errors(Module0, Module) }
			)
		;
			undefined_pred_or_func_error(Name, Arity, Context,
				"`:- pragma export' declaration"),
			{ module_info_incr_errors(Module0, Module) }
		)
	;
		% Handle pragma fact_table decls later on (when we process
		% clauses).
		{ Pragma = fact_table(_, _, _) },
		{ Module = Module0 }
	).

add_item_decl_pass_2(func(_VarSet, FuncName, TypesAndModes, _RetTypeAndMode,
		_MaybeDet, _Cond), _Context, Status, Module0, Status, Module)
		-->
	%
	% add default modes for function declarations, if necessary
	%
	{ list__length(TypesAndModes, Arity) },
	{ module_info_get_predicate_table(Module0, PredTable0) },
	(
		{ predicate_table_search_func_sym_arity(PredTable0,
			FuncName, Arity, PredIds) }
	->
		{ predicate_table_get_preds(PredTable0, Preds0) },
		{ maybe_add_default_modes(PredIds, Preds0, Preds) },
		{ predicate_table_set_preds(PredTable0, Preds, PredTable) },
		{ module_info_set_predicate_table(Module0, PredTable, Module) }
	;
		{ error("make_hlds.m: can't find func declaration") }
	).

add_item_decl_pass_2(func_clause(_, _, _, _, _), _, Status, Module, Status,
		Module) --> [].
add_item_decl_pass_2(pred_clause(_, _, _, _), _, Status, Module, Status, Module)
		--> [].
add_item_decl_pass_2(inst_defn(_, _, _), _, Status, Module, Status, Module)
		--> [].
add_item_decl_pass_2(mode_defn(_, _, _), _, Status, Module, Status, Module)
		--> [].
add_item_decl_pass_2(pred(_, _, _, _, _), _, Status, Module, Status, Module)
		--> [].
add_item_decl_pass_2(pred_mode(_, _, _, _, _), _, Status, Module, Status,
		Module) --> [].
add_item_decl_pass_2(func_mode(_, _, _, _, _, _), _, Status, Module, Status,
		Module) --> [].
add_item_decl_pass_2(nothing, _, Status, Module, Status, Module) --> [].

%------------------------------------------------------------------------------

	% If a module_defn updates the import_status, return the new
	% status, otherwise fail.
:- pred module_defn_update_import_status(module_defn::in,
				import_status::out) is semidet.

module_defn_update_import_status(interface, exported).
module_defn_update_import_status(implementation, local).
module_defn_update_import_status(imported, imported).
module_defn_update_import_status(opt_imported, opt_imported).

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_clause(item, import_status, import_status, term__context,
	module_info, module_info, qual_info, qual_info, io__state, io__state).
:- mode add_item_clause(in, in, out, in, in, out, in, out, di, uo) is det.

add_item_clause(func_clause(VarSet, PredName, Args, Result, Body), Status,
		Status, Context, Module0, Module, Info0, Info) -->
	module_add_func_clause(Module0, VarSet, PredName, Args, Result, Body,
		Status, Context, Module, Info0, Info).
add_item_clause(pred_clause(VarSet, PredName, Args, Body), Status, Status,
		Context, Module0, Module, Info0, Info) -->
	module_add_pred_clause(Module0, VarSet, PredName, Args, Body, Status,
		Context, Module, Info0, Info).
add_item_clause(type_defn(_, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(inst_defn(_, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(mode_defn(_, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(pred(_, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(func(_, _, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(pred_mode(_, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(func_mode(_, _, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(module_defn(_, Defn), Status0, Status, _,
		Module, Module, Info, Info) --> 
	{ module_defn_update_import_status(Defn, Status1) ->
		Status = Status1
	;
		Status = Status0
	}.
add_item_clause(pragma(Pragma), Status, Status, Context,
		Module0, Module, Info0, Info) -->
	(
		{ Pragma = c_code(IsRecursive, Pred, PredOrFunc, Vars, 
			VarSet, C_Code) }
	->
		module_add_pragma_c_code(IsRecursive, Pred, PredOrFunc, Vars, 
			VarSet, C_Code, Status, Context, 
			Module0, Module, Info0, Info)
	;
		{ Pragma = fact_table(Pred, Arity, File) }
	->
		module_add_pragma_fact_table(Pred, Arity, File, 
			Status, Context, Module0, Module, Info0, Info)
	;
		% don't worry about any pragma decs but c_code and fact_table
		% here
		{ Module = Module0 },
		{ Info = Info0 }	
	).
add_item_clause(nothing, Status, Status, _, Module, Module, Info, Info) --> [].

%-----------------------------------------------------------------------------%

:- pred add_stratified_pred(module_info, string, sym_name, arity,
	term__context, module_info, io__state, io__state).
:- mode add_stratified_pred(in, in, in, in, in, out, di, uo) is det.

add_stratified_pred(Module0, PragmaName, Name, Arity, Context, Module) -->
	{ module_info_get_predicate_table(Module0, PredTable0) },
	(
		{ predicate_table_search_sym_arity(PredTable0, Name, 
			Arity, PredIds) }
	->
		{ module_info_stratified_preds(Module0, StratPredIds0) },
		{ set__insert_list(StratPredIds0, PredIds, StratPredIds) },
		{ module_info_set_stratified_preds(Module0, StratPredIds, 
			Module) }
	;
		{ string__append_list(
			["`:- pragma ", PragmaName, "' declaration"],
			Description) },
		undefined_pred_or_func_error(Name, Arity, Context,
			Description),
		{ module_info_incr_errors(Module0, Module) }
	).

%-----------------------------------------------------------------------------%

:- pred add_pred_marker(module_info, string, sym_name, arity,
	term__context, list(marker_status), module_info, io__state, io__state).
:- mode add_pred_marker(in, in, in, in, in, in, out, di, uo) is det.

add_pred_marker(Module0, PragmaName, Name, Arity, Context, Markers, Module) -->
	{ module_info_get_predicate_table(Module0, PredTable0) },
	(
		{ predicate_table_search_sym_arity(PredTable0, Name, 
			Arity, PredIds) }
	->
		{ predicate_table_get_preds(PredTable0, Preds0) },
		{ pragma_set_markers(Preds0, PredIds, Markers, Preds) },
		{ predicate_table_set_preds(PredTable0, Preds, 
			PredTable) },
		{ module_info_set_predicate_table(Module0, PredTable, 
			Module) }
	;
		{ string__append_list(
			["`:- pragma ", PragmaName, "' declaration"],
			Description) },
		undefined_pred_or_func_error(Name, Arity, Context,
			Description),
		{ module_info_incr_errors(Module0, Module) }
	).

%-----------------------------------------------------------------------------%

:- pred module_mark_as_external(sym_name, int, term__context,
			module_info, module_info, io__state, io__state).
:- mode module_mark_as_external(in, in, in, in, out, di, uo) is det.

module_mark_as_external(PredName, Arity, Context, Module0, Module) -->
	% `external' declarations can only apply to things defined
	% in this module, since everything else is already external.
	{ module_info_get_predicate_table(Module0, PredicateTable0) },
	(
		{ predicate_table_search_sym_arity(PredicateTable0,
			PredName, Arity, PredIdList) }
	->
		{ module_mark_preds_as_external(PredIdList, Module0, Module) }
	;
		undefined_pred_or_func_error(PredName, Arity,
			Context, "`external' declaration"),
		{ module_info_incr_errors(Module0, Module) }
	).

:- pred module_mark_preds_as_external(list(pred_id), module_info, module_info).
:- mode module_mark_preds_as_external(in, in, out) is det.

module_mark_preds_as_external([], Module, Module).
module_mark_preds_as_external([PredId | PredIds], Module0, Module) :-
	module_info_preds(Module0, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	pred_info_mark_as_external(PredInfo0, PredInfo),
	map__set(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(Module0, Preds, Module1),
	module_mark_preds_as_external(PredIds, Module1, Module).

%-----------------------------------------------------------------------------%

:- pred module_add_inst_defn(module_info, varset, inst_defn, condition,
		term__context, import_status, module_info, io__state, io__state).
:- mode module_add_inst_defn(in, in, in, in, in, in, out, di, uo) is det.

module_add_inst_defn(Module0, VarSet, InstDefn, Cond,
			Context, Status, Module) -->
	{ module_info_insts(Module0, InstTable0) },
	{ inst_table_get_user_insts(InstTable0, Insts0) },
	insts_add(Insts0, VarSet, InstDefn, Cond, Context, Status, Insts),
	{ inst_table_set_user_insts(InstTable0, Insts, InstTable) },
	{ module_info_set_insts(Module0, InstTable, Module) }.

:- pred insts_add(user_inst_table, varset, inst_defn, condition, term__context,
			import_status, user_inst_table, io__state, io__state).
:- mode insts_add(in, in, in, in, in, in, out, di, uo) is det.

	% XXX handle abstract insts
insts_add(_, _, abstract_inst(_, _), _, _, _, _) -->
	{ error("sorry, abstract insts not implemented") }.
insts_add(Insts0, VarSet, eqv_inst(Name, Args, Body),
			Cond, Context, Status, Insts) -->
	{ list__length(Args, Arity) },
	(
		{ I = hlds__inst_defn(VarSet, Args, eqv_inst(Body), Cond,
					Context, Status) },
		{ user_inst_table_insert(Insts0, Name - Arity, I, Insts1) }
	->
		{ Insts = Insts1 }
	;
		{ Insts = Insts0 },
		% If abstract insts are implemented, this will need to change
		% to update the hlds__inst_defn to the non-abstract inst.
		( { Status = opt_imported } ->
			[]
		;
			% XXX we should record each error using 
			%	 module_info_incr_errors
			{ user_inst_table_get_inst_defns(Insts, InstDefns) },
			{ map__lookup(InstDefns, Name - Arity, OrigI) },
			{ OrigI = hlds__inst_defn(_, _, _, _,
						OrigContext, _) },
			multiple_def_error(Name, Arity, "inst",
					Context, OrigContext)
		)
	).

%-----------------------------------------------------------------------------%

:- pred module_add_mode_defn(module_info, varset, mode_defn, condition,
	term__context, import_status, module_info, io__state, io__state).
:- mode module_add_mode_defn(in, in, in, in, in, in, out, di, uo) is det.

module_add_mode_defn(Module0, VarSet, ModeDefn, Cond,
		Context, Status, Module) -->
	{ module_info_modes(Module0, Modes0) },
	modes_add(Modes0, VarSet, ModeDefn, Cond, Context, Status, Modes),
	{ module_info_set_modes(Module0, Modes, Module) }.

:- pred modes_add(mode_table, varset, mode_defn, condition, term__context,
			import_status, mode_table, io__state, io__state).
:- mode modes_add(in, in, in, in, in, in, out, di, uo) is det.

modes_add(Modes0, VarSet, eqv_mode(Name, Args, Body),
			Cond, Context, Status, Modes) -->
	{ list__length(Args, Arity) },
	(
		{ I = hlds__mode_defn(VarSet, Args, eqv_mode(Body), Cond,
			Context, Status) },
		{ mode_table_insert(Modes0, Name - Arity, I, Modes1) }
	->
		{ Modes = Modes1 }
	;
		{ Modes = Modes0 },
		( { Status = opt_imported } ->
			[]
		;
			{ mode_table_get_mode_defns(Modes, ModeDefns) },
			{ map__lookup(ModeDefns, Name - Arity, OrigI) },
			{ OrigI = hlds__mode_defn(_, _, _, _,
						OrigContext, _) },
			% XXX we should record each error using
			% 	module_info_incr_errors
			multiple_def_error(Name, Arity, "mode",
					Context, OrigContext)
		)
	).

:- pred mode_name_args(mode_defn, sym_name, list(inst_param), hlds__mode_body).
:- mode mode_name_args(in, out, out, out) is det.

mode_name_args(eqv_mode(Name, Args, Body), Name, Args, eqv_mode(Body)).

%-----------------------------------------------------------------------------%

	% We allow more than one "definition" for a given type so
	% long all of them except one are actually just declarations,
	% e.g. `:- type t.', which is parsed as an type definition for
	% t which defines t as an abstract_type.

:- pred module_add_type_defn(module_info, tvarset, type_defn, condition,
	term__context, import_status, module_info, io__state, io__state).
:- mode module_add_type_defn(in, in, in, in, in, in, out, di, uo) is det.

module_add_type_defn(Module0, TVarSet, TypeDefn, _Cond, Context, Status0,
		Module) -->
	{ module_info_types(Module0, Types0) },
	globals__io_get_globals(Globals),
	{ convert_type_defn(TypeDefn, Globals, Name, Args, Body) },
	{ list__length(Args, Arity) },
	{ Body = abstract_type ->
		( Status0 = exported ->
			Status1 = abstract_exported
		; Status0 = imported ->
			Status1 = abstract_imported
		;
			Status1 = Status0
		)
	;
		Status1 = Status0
	},
	{ 
		% the type is exported if *any* occurrence is exported,
		% even a previous abstract occurrence
		map__search(Types0, Name - Arity, OldDefn)
	->
		hlds_data__get_type_defn_status(OldDefn, OldStatus),
		combine_status(Status1, OldStatus, Status)
	;
		Status = Status1 
	},
	{ hlds_data__set_type_defn(TVarSet, Args, Body, Status, Context, T) },
	(
		% if there was an existing non-abstract definition for the type
		{ map__search(Types0, Name - Arity, T2) },
		{ hlds_data__get_type_defn_tparams(T2, Params) },
		{ hlds_data__get_type_defn_body(T2, Body_2) },
		{ hlds_data__get_type_defn_context(T2, OrigContext) },
		{ hlds_data__get_type_defn_status(T2, OrigStatus) },
		{ Body_2 \= abstract_type }
	->
	  	(
			% then if this definition was abstract, ignore it
			% (but update the status of the old defn if necessary)
			{ Body = abstract_type }
		->
			{
				Status = OrigStatus
			->
				Module = Module0
			;
				hlds_data__set_type_defn(TVarSet, Params,
					Body_2, OrigStatus, OrigContext, T3),
				TypeId = Name - Arity,
				map__set(Types0, TypeId, T3, Types),
				module_info_set_types(Module0, Types, Module)
			}
		;

			% otherwise issue an error message if the second
			% definition wasn't read while reading .opt files. 
			{ Status = opt_imported }
		->
			{ Module = Module0 }
		;
			{ module_info_incr_errors(Module0, Module) },
			multiple_def_error(Name, Arity, "type", Context,
				OrigContext)
		)
	;
		{ TypeId = Name - Arity },
		{ map__set(Types0, TypeId, T, Types) },
		(
			{ Body = du_type(ConsList, _, _) }
		->
			{ module_info_ctors(Module0, Ctors0) },
			ctors_add(ConsList, TypeId, Context, Ctors0, Ctors),
			{ module_info_set_ctors(Module0, Ctors, Module1) }
		;
			{ Module1 = Module0 }
		),
		{ construct_qualified_term(Name, Args, Type) },
		(
			{ Body = abstract_type }
		->
			{ special_pred_list(SpecialPredIds) },
			{ add_special_pred_decl_list(SpecialPredIds,
					Module1, TVarSet, Type, TypeId,
					Context, Status, Module2) }
		;
			{ special_pred_list(SpecialPredIds) },
			{ add_special_pred_list(SpecialPredIds,
					Module1, TVarSet, Type, TypeId,
					Body, Context, Status, Module2a) },
			( 
				{ Status \= imported },
				{ Status \= opt_imported },
				{ Status \= abstract_imported }
				% Only want to handle exports for types that 
				% are defined locally (cuts down on 
				% duplicates). 
			->
				{ add_abstract_export(Module2a, Type, 
					TypeId, Module2) }
			;
				{ Module2 = Module2a }
			)
		),
		{ module_info_set_types(Module2, Types, Module) },
		( { Body = uu_type(_) } ->
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			report_warning(StdErr, 
	"Warning: undiscriminated union types (`+') not implemented.\n"),
			io__set_output_stream(OldStream, _)
		;
			[]
		)
	).

:- pred combine_status(import_status, import_status, import_status).
:- mode combine_status(in, in, out) is det.

combine_status(StatusA, StatusB, Status) :-
        ( combine_status_2(StatusA, StatusB, CombinedStatus) ->
                Status = CombinedStatus
        ;
                error("pseudo_imported or pseudo_exported type definition")
        ).

:- pred combine_status_2(import_status, import_status, import_status).
:- mode combine_status_2(in, in, out) is semidet.

combine_status_2(imported, Status2, Status) :-
	 combine_status_imported(Status2, Status).
combine_status_2(local, Status2, Status) :-
	combine_status_local(Status2, Status).
combine_status_2(exported, _Status2, exported).
combine_status_2(opt_imported, _Status2, opt_imported).
combine_status_2(abstract_imported, Status2, Status) :-
	combine_status_abstract_imported(Status2, Status).
combine_status_2(abstract_exported, Status2, Status) :-
	combine_status_abstract_exported(Status2, Status).

:- pred combine_status_imported(import_status, import_status).
:- mode combine_status_imported(in, out) is semidet.

combine_status_imported(imported,	imported).
combine_status_imported(local,		imported).
combine_status_imported(exported,	exported).
combine_status_imported(opt_imported,	opt_imported).
combine_status_imported(abstract_imported, imported).
combine_status_imported(abstract_exported, abstract_exported).


:- pred combine_status_local(import_status, import_status).
:- mode combine_status_local(in, out) is semidet.

combine_status_local(imported,		local).
combine_status_local(local,		local).
combine_status_local(exported,		exported).
combine_status_local(opt_imported,	local).
combine_status_local(abstract_imported, local).
combine_status_local(abstract_exported, abstract_exported).

:- pred combine_status_abstract_exported(import_status, import_status).
:- mode combine_status_abstract_exported(in, out) is det.

combine_status_abstract_exported(Status2, Status) :-
	( Status2 = exported ->
		Status = exported
	;
		Status = abstract_exported
	).

:- pred combine_status_abstract_imported(import_status, import_status).
:- mode combine_status_abstract_imported(in, out) is det.

combine_status_abstract_imported(Status2, Status) :-
	( Status2 = imported ->
		Status = imported
	;
		Status = abstract_imported
	).

:- pred add_abstract_export(module_info, type, type_id, module_info).
:- mode add_abstract_export(in, in, in, out) is det.
add_abstract_export(Module0, Type, TypeId, Module) :-
	module_info_shape_info(Module0, Shape_Info0),
	Shape_Info0 = shape_info(Shapes, Abs_Exports0, SpecialPredShapes),
	S_Num = no(Type),
	map__set(Abs_Exports0, TypeId, S_Num, Abs_Exports1),
	Shape_Info = shape_info(Shapes, Abs_Exports1, SpecialPredShapes),
	module_info_set_shape_info(Module0, Shape_Info, Module).

:- pred add_special_preds(module_info, tvarset, type, type_id, 
		hlds__type_body, term__context, import_status, module_info).
:- mode add_special_preds(in, in, in, in, in, in, in, out) is det.

add_special_preds(Module0, TVarSet, Type, TypeId,
			Body, Context, Status, Module) :-
	special_pred_list(SpecialPredIds),
	( Body = abstract_type ->
		add_special_pred_decl_list(SpecialPredIds, Module0, TVarSet,
				Type, TypeId, Context, Status, Module)
	;
		add_special_pred_list(SpecialPredIds, Module0, TVarSet, Type,
				TypeId, Body, Context, Status, Module)
	).
	
:- pred convert_type_defn(type_defn, globals,
			sym_name, list(type_param), hlds__type_body).
:- mode convert_type_defn(in, in, out, out, out) is det.

convert_type_defn(du_type(Name, Args, Body), Globals, Name, Args,
		du_type(Body, CtorTags, IsEnum)) :-
	assign_constructor_tags(Body, Globals, CtorTags, IsEnum).
convert_type_defn(uu_type(Name, Args, Body), _, Name, Args, uu_type(Body)).
convert_type_defn(eqv_type(Name, Args, Body), _, Name, Args, eqv_type(Body)).
convert_type_defn(abstract_type(Name, Args), _, Name, Args, abstract_type).

:- pred ctors_add(list(constructor), type_id, term__context, cons_table,
			cons_table, io__state, io__state).
:- mode ctors_add(in, in, in, in, out, di, uo) is det.

ctors_add([], _TypeId, _Context, Ctors, Ctors) --> [].
ctors_add([Name - Args | Rest], TypeId, Context, Ctors0, Ctors) -->
	{ make_cons_id(Name, Args, TypeId, ConsId) },
	{ assoc_list__values(Args, Types) },
	{ ConsDefn = hlds__cons_defn(Types, TypeId, Context) },
	( %%% some [ConsDefns0]
		{ map__search(Ctors0, ConsId, ConsDefns0) }
	->
		{ ConsDefns1 = ConsDefns0 }
	;
		{ ConsDefns1 = [] }
	),
	(
		{ list__member(OtherConsDefn, ConsDefns1) },
		{ OtherConsDefn = hlds__cons_defn(_, TypeId, _) }
	->
		% XXX we should record each error using module_info_incr_errors
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		prog_out__write_context(Context),
		io__write_string("Error: constructor `"),
		hlds_out__write_cons_id(ConsId),
		io__write_string("' for type `"),
		hlds_out__write_type_id(TypeId),
		io__write_string("' multiply defined.\n"),
		io__set_exit_status(1),
		io__set_output_stream(OldStream, _),
		{ ConsDefns2 = ConsDefns1 }
	;
		{ ConsDefns2 = [ConsDefn | ConsDefns1] }
	),
	{ map__set(Ctors0, ConsId, ConsDefns2, Ctors1) },
	ctors_add(Rest, TypeId, Context, Ctors1, Ctors).

%---------------------------------------------------------------------------%

:- pred module_add_pred(module_info, varset, sym_name, list(type_and_mode),
		maybe(determinism), condition, term__context, import_status,
		module_info,
		io__state, io__state).
:- mode module_add_pred(in, in, in, in, in, in, in, in, out, di, uo) is det.

module_add_pred(Module0, VarSet, PredName, TypesAndModes, MaybeDet, Cond,
		Context, Status, Module) -->
	% Only preds with opt_imported clauses are tagged as opt_imported, so
	% that the compiler doesn't look for clauses for other preds read in
	% from optimization interfaces.
	{ Status = opt_imported ->
		DeclStatus = opt_decl 
	;
		DeclStatus = Status
	},
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	add_new_pred(Module0, VarSet, PredName, Types, Cond, Context,
		DeclStatus, predicate, Module1),
	(
		{ MaybeModes = yes(Modes) }
	->
		module_add_mode(Module1, VarSet, PredName, Modes, MaybeDet,
			Cond, Context, predicate, Module)
	;
		{ Module = Module1 }
	).

:- pred module_add_func(module_info, varset, sym_name, list(type_and_mode),
		type_and_mode, maybe(determinism), condition, term__context,
		import_status, module_info,
		io__state, io__state).
:- mode module_add_func(in, in, in, in, in, in, in, in, in, out, di, uo) is det.

module_add_func(Module0, VarSet, FuncName, TypesAndModes, RetTypeAndMode,
		MaybeDet, Cond, Context, Status, Module) -->
	% Only funcs with opt_imported clauses are tagged as opt_imported, so
	% that the compiler doesn't look for clauses for other preds.
	{ Status = opt_imported ->
		DeclStatus = imported
	;
		DeclStatus = Status
	},
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	{ split_type_and_mode(RetTypeAndMode, RetType, MaybeRetMode) },
	{ list__append(Types, [RetType], Types1) },
	add_new_pred(Module0, VarSet, FuncName, Types1, Cond, Context,
		DeclStatus, function, Module1),
	(
		{ MaybeModes = yes(Modes) },
		{ MaybeRetMode = yes(RetMode) }
	->
		{ list__append(Modes, [RetMode], Modes1) },
		module_add_mode(Module1, VarSet, FuncName, Modes1,
			MaybeDet, Cond, Context, function, Module)
	;
		{ Module = Module1 }
	).

:- pred add_new_pred(module_info, tvarset, sym_name, list(type),
		condition, term__context, import_status, pred_or_func,
		module_info, io__state, io__state).
:- mode add_new_pred(in, in, in, in, in, in, in, in, out, di, uo) is det.

% NB.  Predicates are also added in polymorphism.m, which converts
% lambda expressions into separate predicates, so any changes may need
% to be reflected there too.

add_new_pred(Module0, TVarSet, PredName, Types, Cond, Context, Status,
		PredOrFunc, Module) -->
	{ module_info_name(Module0, ModuleName) },
	{ list__length(Types, Arity) },
	(
		{ PredName = unqualified(_PName) },
		{ module_info_incr_errors(Module0, Module) },
		unqualified_pred_error(PredName, Arity, Context)
		% All predicate names passed into this predicate should have 
		% been qualified by prog_io.m, when they were first read.
	;
		{ PredName = qualified(MNameOfPred, PName) },
		{ Module1 = Module0 },
		{ module_info_get_predicate_table(Module1, PredicateTable0) },
		{ clauses_info_init(Arity, ClausesInfo) },
		{ pred_info_init(ModuleName, PredName, Arity, TVarSet, Types,
				Cond, Context, ClausesInfo, Status, no, none,	
				PredOrFunc, PredInfo0) },
		(
			{ predicate_table_search_pf_m_n_a(PredicateTable0,
				PredOrFunc, MNameOfPred, PName, Arity,
				[OrigPred|_]) }
		->
			( { Status \= opt_decl } ->
				{ module_info_incr_errors(Module1, Module) },
				{ module_info_pred_info(Module, OrigPred,
					OrigPredInfo) },
				{ pred_info_context(OrigPredInfo,
					OrigContext) },
				{ hlds_out__pred_or_func_to_str(PredOrFunc,
					DeclString) },
				multiple_def_error(PredName, Arity, DeclString,
					Context, OrigContext)
			;
				% This can happen for exported external preds.
				{ Module = Module0 }
			)
		;
			{ predicate_table_insert(PredicateTable0, PredInfo0, 
					PredId, PredicateTable1) },
			( 
				{ code_util__predinfo_is_builtin(Module1, 
						PredInfo0) }
			->
				{ add_builtin(PredId, Types,
					PredInfo0, PredInfo) },
				{ predicate_table_get_preds(PredicateTable1,
					Preds1) },
				{ map__set(Preds1, PredId, PredInfo, Preds) },
				{ predicate_table_set_preds(PredicateTable1,
					Preds, PredicateTable) }
			;
				{ PredicateTable = PredicateTable1 }
			),
			{ module_info_set_predicate_table(Module1, 
					PredicateTable, Module) }
		)
	).

%-----------------------------------------------------------------------------%

:- pred add_builtin(pred_id, list(type), pred_info, pred_info).
:- mode add_builtin(in, in, in, out) is det.

	% For a builtin predicate, say foo/2, we add a clause
	%
	%	foo(H1, H2) :- foo(H1, H2).
	%
	% This does not generate an infinite loop!
	% Instead, the compiler will generate the usual builtin inline code
	% for foo/2 in the body.  The reason for generating this
	% forwarding code stub is so that things work correctly if
	% you take the address of the predicate.

add_builtin(PredId, Types, PredInfo0, PredInfo) :-
		%
		% lookup some useful info: Module, Name, Context, HeadVars
		%
	pred_info_module(PredInfo0, Module),
	pred_info_name(PredInfo0, Name),
	pred_info_context(PredInfo0, Context),
	pred_info_clauses_info(PredInfo0, ClausesInfo0),
	ClausesInfo0 = clauses_info(VarSet, _VarTypes0, _VarTypes1,
					HeadVars, _ClauseList0),

		%
		% construct the pseudo-recursive call to Module:Name(HeadVars)
		%
	SymName = qualified(Module, Name),
	ModeId = 0, % mode checking will figure it out
	hlds__is_builtin_make_builtin(yes, yes, IsBuiltin),
	MaybeUnifyContext = no,
	Call = call(PredId, ModeId, HeadVars, IsBuiltin, MaybeUnifyContext,
			SymName),

		%
		% construct a clause containing that pseudo-recursive call
		%
	goal_info_init(GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo),
	Goal = Call - GoalInfo,
	Clause = clause([], Goal, Context),

		%
		% put the clause we just built into the pred_info,
		% annotateed with the appropriate types
		%
	ClauseList = [Clause],
	map__from_corresponding_lists(HeadVars, Types, VarTypes),
	ClausesInfo = clauses_info(VarSet, VarTypes, VarTypes,
					HeadVars, ClauseList),
	pred_info_set_clauses_info(PredInfo0, ClausesInfo, PredInfo).

%-----------------------------------------------------------------------------%

:- pred add_special_pred_list(list(special_pred_id),
			module_info, tvarset, type, type_id, hlds__type_body,
			term__context, import_status,
			module_info).
:- mode add_special_pred_list(in, in, in, in, in, in, in, in, out) is det.

add_special_pred_list([], Module, _, _, _, _, _, _, Module).
add_special_pred_list([SpecialPredId | SpecialPredIds], Module0,
		TVarSet, Type, TypeId, Body, Context, Status, Module) :-
	add_special_pred(SpecialPredId, Module0,
		TVarSet, Type, TypeId, Body, Context, Status, Module1),
	add_special_pred_list(SpecialPredIds, Module1,
		TVarSet, Type, TypeId, Body, Context, Status, Module).

:- pred add_special_pred(special_pred_id,
			module_info, tvarset, type, type_id, hlds__type_body,
			term__context, import_status,
			module_info).
:- mode add_special_pred(in, in, in, in, in, in, in, in, out) is det.

add_special_pred(SpecialPredId,
		Module0, TVarSet, Type, TypeId, TypeBody, Context, Status0,
		Module) :-
	adjust_special_pred_status(Status0, SpecialPredId, Status),
	module_info_get_special_pred_map(Module0, SpecialPredMap0),
	( map__contains(SpecialPredMap0, SpecialPredId - TypeId) ->
		Module1 = Module0
	;
		add_special_pred_decl(SpecialPredId,
			Module0, TVarSet, Type, TypeId, Context, Status,
			Module1)
	),
	module_info_get_special_pred_map(Module1, SpecialPredMap1),
	map__lookup(SpecialPredMap1, SpecialPredId - TypeId, PredId),
	module_info_preds(Module1, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	% if the type was imported, then the special preds for that
	% type should be imported too
	( (Status = imported ; Status = pseudo_imported) ->
		pred_info_set_import_status(PredInfo0, Status, PredInfo1)
	;
		PredInfo1 = PredInfo0
	),
	unify_proc__generate_clause_info(SpecialPredId, Type, TypeBody,
				Module1, ClausesInfo),
	pred_info_set_clauses_info(PredInfo1, ClausesInfo, PredInfo),
	map__set(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(Module1, Preds, Module).

:- pred add_special_pred_decl_list(list(special_pred_id),
			module_info, tvarset, type, type_id, 
			term__context, import_status,
			module_info).
:- mode add_special_pred_decl_list(in, in, in, in, in, in, in, out) is det.

add_special_pred_decl_list([], Module, _, _, _, _, _, Module).
add_special_pred_decl_list([SpecialPredId | SpecialPredIds], Module0,
		TVarSet, Type, TypeId, Context, Status, Module) :-
	add_special_pred_decl(SpecialPredId, Module0,
		TVarSet, Type, TypeId, Context, Status, Module1),
	add_special_pred_decl_list(SpecialPredIds, Module1,
		TVarSet, Type, TypeId, Context, Status, Module).

:- pred add_special_pred_decl(special_pred_id,
				module_info, tvarset, type, type_id,
				term__context, import_status,
				module_info).
:- mode add_special_pred_decl(in, in, in, in, in, in, in, out) is det.

add_special_pred_decl(SpecialPredId,
			Module0, TVarSet, Type, TypeId, Context, Status0,
			Module) :-
	module_info_name(Module0, ModuleName),
	PredName = unqualified(Name),
	special_pred_info(SpecialPredId, Type, Name, ArgTypes, ArgModes, Det),
	special_pred_name_arity(SpecialPredId, _, _, Arity),
	Cond = true,
	clauses_info_init(Arity, ClausesInfo0),
	adjust_special_pred_status(Status0, SpecialPredId, Status),
	pred_info_init(ModuleName, PredName, Arity, TVarSet, ArgTypes, Cond,
		Context, ClausesInfo0, Status, no, none, predicate, PredInfo0),
	ArgLives = no,
	add_new_proc(PredInfo0, Arity, ArgModes, ArgLives, yes(Det), Context,
			PredInfo, _),

	module_info_get_predicate_table(Module0, PredicateTable0),
	predicate_table_insert(PredicateTable0, PredInfo, PredId,
		PredicateTable),
	module_info_set_predicate_table(Module0, PredicateTable,
		Module1),
	module_info_get_special_pred_map(Module1, SpecialPredMap0),
	map__set(SpecialPredMap0, SpecialPredId - TypeId, PredId,
		SpecialPredMap),
	module_info_set_special_pred_map(Module1, SpecialPredMap, Module).


:- pred adjust_special_pred_status(import_status, special_pred_id,
				import_status).
:- mode adjust_special_pred_status(in, in, out) is det.

adjust_special_pred_status(Status0, SpecialPredId, Status) :-
	( ( Status0 = opt_imported ; Status0 = abstract_imported ) ->
		Status1 = imported
	; Status0 = abstract_exported ->
		Status1 = exported
	;
		Status1 = Status0
	),

	% unification predicates are special - they are 
	% "pseudo"-imported/exported (only mode 0 is imported/exported).
	( SpecialPredId = unify ->
		( Status1 = imported ->
			Status = pseudo_imported
		; Status1 = exported ->
			Status = pseudo_exported
		;
			Status = Status1
		)
	;
		Status = Status1
	).

add_new_proc(PredInfo0, Arity, ArgModes, MaybeArgLives, MaybeDet, Context,
		PredInfo, ModeId) :-
	pred_info_procedures(PredInfo0, Procs0),
	next_mode_id(Procs0, MaybeDet, ModeId),
	proc_info_init(Arity, ArgModes, MaybeArgLives, MaybeDet, Context,
			NewProc),
	map__set(Procs0, ModeId, NewProc, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo).

%-----------------------------------------------------------------------------%

	% Add a mode declaration for a predicate.

:- pred module_add_mode(module_info, varset, sym_name, list(mode),
		maybe(determinism), condition, term__context, pred_or_func,
		module_info, io__state, io__state).
:- mode module_add_mode(in, in, in, in, in, in, in, in, out, di, uo) is det.

	% We should store the mode varset and the mode condition
	% in the hlds - at the moment we just ignore those two arguments.

module_add_mode(ModuleInfo0, _VarSet, PredName, Modes, MaybeDet, _Cond,
			MContext, PredOrFunc, ModuleInfo) -->

		% Lookup the pred or func declaration in the predicate table.
		% If it's not there (or if it is ambiguous), optionally print a
		% warning message and insert an implicit definition for the
		% predicate; it is presumed to be local, and its type
		% will be inferred automatically.

	{ module_info_name(ModuleInfo0, ModuleName0) },
	{ sym_name_get_module_name(PredName, ModuleName0, ModuleName) },
	{ list__length(Modes, Arity) },
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) },
	(
		{ predicate_table_search_pf_sym_arity(PredicateTable0,
			PredOrFunc, PredName, Arity,
			[PredId0]) }
	->
		{ PredicateTable1 = PredicateTable0 },
		{ PredId = PredId0 }
	;
		maybe_undefined_pred_error(PredName, Arity, PredOrFunc,
			MContext, "mode declaration"),
		{ preds_add_implicit(PredicateTable0,
				ModuleName, PredName, Arity, MContext,
				PredOrFunc,
				PredId, PredicateTable1) }
	),

		% Lookup the pred_info for this predicate
	{ predicate_table_get_preds(PredicateTable1, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },

		% check that the determinism was specified
	(
		{ MaybeDet = no }
	->
		( { pred_info_is_exported(PredInfo0) } ->
			unspecified_det_for_exported(PredName, Arity,
				PredOrFunc, MContext)
		;
			globals__io_lookup_bool_option(infer_det, InferDet),
			(
				{ InferDet = no }
			->
				unspecified_det_for_local(PredName, Arity,
					PredOrFunc, MContext)
			;
				[]
			)
		)
	;
		[]
	),
		% add the mode declaration to the proc_info for this procedure.
		% XXX we should check that this mode declaration
		% isn't the same as an existing one
	{ ArgLives = no },
	{ add_new_proc(PredInfo0, Arity, Modes, ArgLives, MaybeDet, MContext,
			PredInfo, _) },
	{ map__set(Preds0, PredId, PredInfo, Preds) },
	{ predicate_table_set_preds(PredicateTable1, Preds, PredicateTable) },
	{ module_info_set_predicate_table(ModuleInfo0, PredicateTable,
		ModuleInfo) }.

	% Whenever there is a clause or mode declaration for an undeclared
	% predicate, we add an implicit declaration
	%	:- pred p(T1, T2, ..., Tn).
	% for that predicate; the real types will be inferred by
	% type inference.

:- pred preds_add_implicit(predicate_table, module_name, sym_name, arity,
				term__context, pred_or_func,
				pred_id, predicate_table).
:- mode preds_add_implicit(in, in, in, in, in, in, out, out) is det.

preds_add_implicit(PredicateTable0,
			ModuleName, PredName, Arity, Context, PredOrFunc,
			PredId, PredicateTable) :-
	varset__init(TVarSet0),
	make_n_fresh_vars("T", Arity, TVarSet0, TypeVars, TVarSet),
	term__var_list_to_term_list(TypeVars, Types),
	Cond = true,
	clauses_info_init(Arity, ClausesInfo),
	pred_info_init(ModuleName, PredName, Arity, TVarSet, Types, Cond,
		Context, ClausesInfo, local, no, none, PredOrFunc, PredInfo0),
	pred_info_set_marker_list(PredInfo0, [request(infer_type)], PredInfo),
	(
		\+ predicate_table_search_pf_sym_arity(PredicateTable0,
			PredOrFunc, PredName, Arity, _)
	->
		predicate_table_insert(PredicateTable0, PredInfo, PredId,
			PredicateTable)
	;	
		error("preds_add_implicit")
	).

	% This is a quick hack, especially the trick with
	% determinism_priority.  Efficiency could be improved -
	% we should probably store the next available ModeId rather
	% than recomputing it all the time.

next_mode_id(Procs, MaybeDet, ModeId) :-
	map__to_assoc_list(Procs, List),
	list__length(List, ModeId0),
	(
		MaybeDet = no,
		determinism_priority_unspecified(Priority)
	;
		MaybeDet = yes(Det),
		determinism_priority(Det, Priority)
	),
	determinism_priority_step(Step),
	( ModeId0 >= Step ->
		error("too many modes per predicate")
	;
		true
	),
	ModeId is ModeId0 + Priority.

	% If we can call a predicate in either of two different modes,
	% we should prefer to call it in a deterministic mode
	% rather than a non-deterministic one, and we should prefer
	% to call it in a semideterministic mode rather than a deterministic
	% one.  Also we should prefer a nondet mode rather than a multidet mode.
	% Higher numbers mean lower priority.
	% This works because mode analysis tries each mode in turn,
	% starting with the lowest-numbered modes.

:- pred determinism_priority(determinism, int).
:- mode determinism_priority(in, out) is det.

determinism_priority(semidet, 0).
determinism_priority(failure, 0).
determinism_priority(det, 10000).
determinism_priority(erroneous, 10000).
determinism_priority(cc_nondet, 30000).
determinism_priority(nondet, 40000).
determinism_priority(cc_multidet, 50000).
determinism_priority(multidet, 60000).

:- pred determinism_priority_unspecified(int).
:- mode determinism_priority_unspecified(out) is det.

determinism_priority_unspecified(20000).

:- pred determinism_priority_step(int).
:- mode determinism_priority_step(out) is det.

determinism_priority_step(10000).

%-----------------------------------------------------------------------------%

:- pred module_add_pred_clause(module_info, varset, sym_name, list(term), goal,
		import_status, term__context, module_info,
		qual_info, qual_info, io__state, io__state).
:- mode module_add_pred_clause(in, in, in, in, in, in, in, out,
		in, out, di, uo) is det.

module_add_pred_clause(ModuleInfo0, ClauseVarSet, PredName, Args, Body,
			Status, Context, ModuleInfo, Info0, Info) -->
		% print out a progress message
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		{ list__length(Args, Arity) },
		io__write_string("% Processing clause for predicate `"),
		hlds_out__write_pred_call_id(PredName/Arity),
		io__write_string("'...\n")
	;
		[]
	),
	module_add_clause(ModuleInfo0, ClauseVarSet, PredName, Args, Body,
		Status, Context, predicate, ModuleInfo, Info0, Info).

:- pred module_add_func_clause(module_info, varset, sym_name, list(term), term,
		goal, import_status, term__context, module_info,
		qual_info, qual_info, io__state, io__state).
:- mode module_add_func_clause(in, in, in, in, in,
	in, in, in, out, in, out, di, uo) is det.

module_add_func_clause(ModuleInfo0, ClauseVarSet, FuncName, Args0, Result, Body,
			Status, Context, ModuleInfo, Info0, Info) -->
		% print out a progress message
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		io__write_string("% Processing clause for function `"),
		{ list__length(Args0, Arity) },
		hlds_out__write_pred_call_id(FuncName/Arity),
		io__write_string("'...\n")
	;
		[]
	),
	{ list__append(Args0, [Result], Args) },
	module_add_clause(ModuleInfo0, ClauseVarSet, FuncName, Args, Body,
		Status, Context, function, ModuleInfo, Info0, Info).

:- pred module_add_clause(module_info, varset, sym_name, list(term), goal,
		import_status, term__context, pred_or_func,
		module_info, qual_info, qual_info, io__state, io__state).
:- mode module_add_clause(in, in, in, in, in, in, in, in,
		out, in, out, di, uo) is det.

module_add_clause(ModuleInfo0, ClauseVarSet, PredName, Args, Body, Status,
			Context, PredOrFunc, ModuleInfo, Info0, Info) -->
		% Lookup the pred declaration in the predicate table.
		% (if it's not there, call maybe_undefined_pred_error
		% and insert an implicit declaration for the predicate.)
	{ module_info_name(ModuleInfo0, ModuleName) },
	{ list__length(Args, Arity) },
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) },
	(
		{ predicate_table_search_pf_sym_arity(PredicateTable0,
				PredOrFunc, PredName, Arity, [PredId0]) }
	->
		{ PredId = PredId0 },
		{ PredicateTable1 = PredicateTable0 }
	;

		maybe_undefined_pred_error(PredName, Arity, PredOrFunc,
			Context, "clause"),
		{ preds_add_implicit(PredicateTable0,
				ModuleName, PredName, Arity, Context,
				PredOrFunc,
				PredId, PredicateTable1) }
	),
		% Lookup the pred_info for this pred,
		% add the clause to the clauses_info in the pred_info,
		% if there are no modes add an `infer_modes' marker,
		% and then save the pred_info.
	{ predicate_table_get_preds(PredicateTable1, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	{ Status = opt_imported ->
		pred_info_set_import_status(PredInfo0, opt_imported, PredInfo1)
	;
		PredInfo1 = PredInfo0
	},
	(
		{ pred_info_get_goal_type(PredInfo1, pragmas) }
	->
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: clause for predicate `"),
		hlds_out__write_pred_call_id(PredName/Arity),
		io__write_string("'\n"),
		prog_out__write_context(Context),
		io__write_string("  with `:- pragma c_code' declaration preceding.\n"),
		{ Info = Info0 }
	;
		{
		pred_info_clauses_info(PredInfo1, Clauses0),
		pred_info_procedures(PredInfo1, Procs),
		pred_info_typevarset(PredInfo1, TVarSet0),
		map__keys(Procs, ModeIds)
		},
		clauses_info_add_clause(Clauses0, PredId, ModeIds,
			ClauseVarSet, TVarSet0, Args, Body, Context, Goal,
			VarSet, TVarSet, Clauses, Warnings, Info0, Info),
		{
		pred_info_set_clauses_info(PredInfo1, Clauses, PredInfo2),
		pred_info_set_goal_type(PredInfo2, clauses, PredInfo3),
		pred_info_set_typevarset(PredInfo3, TVarSet, PredInfo4),
		pred_info_arg_types(PredInfo4, _ArgTVarSet, ArgTypes),
		pred_info_set_arg_types(PredInfo4, TVarSet,
					ArgTypes, PredInfo5),

		%
		% check if there are no modes for the predicate,
		% and if so, set the `infer_modes' flag for that predicate
		%
		( ModeIds = [] ->
			pred_info_get_marker_list(PredInfo5, Markers0),
			Markers = [request(infer_modes) | Markers0],
			pred_info_set_marker_list(PredInfo5, Markers,
				PredInfo)
		;
			PredInfo = PredInfo5
		),
		map__set(Preds0, PredId, PredInfo, Preds),
		predicate_table_set_preds(PredicateTable1, Preds,
			PredicateTable),
		module_info_set_predicate_table(ModuleInfo0, PredicateTable,
			ModuleInfo)
		},
		( { Status \= opt_imported } ->
			% warn about singleton variables 
			maybe_warn_singletons(VarSet, PredName/Arity, Goal),
			% warn about variables with overlapping scopes
			maybe_warn_overlap(Warnings, VarSet, PredOrFunc,
						PredName/Arity)
		;
			[]
		)
	).

%-----------------------------------------------------------------------------%

:- pred module_add_c_header(string, term__context, module_info, module_info).
:- mode module_add_c_header(in, in, in, out) is det.

module_add_c_header(C_Header, Context, Module0, Module) :-
	module_info_get_c_header(Module0, C_HeaderIndex0),
		% store the c headers in reverse order and reverse them later
		% for efficiency
	C_HeaderIndex1 = [C_Header - Context|C_HeaderIndex0],
	module_info_set_c_header(Module0, C_HeaderIndex1, Module).
	
:- pred module_add_c_body_code(string, term__context, module_info, module_info).
:- mode module_add_c_body_code(in, in, in, out) is det.

module_add_c_body_code(C_Body_Code, Context, Module0, Module) :-
	module_info_get_c_body_code(Module0, C_Body_List0),
		% store the c headers in reverse order and reverse them later
		% for efficiency
	C_Body_List = [C_Body_Code - Context | C_Body_List0],
	module_info_set_c_body_code(Module0, C_Body_List, Module).
	
%-----------------------------------------------------------------------------%

:- pred module_add_pragma_c_code(c_is_recursive, sym_name, pred_or_func, 
		list(pragma_var), varset, string, import_status, term__context, 
		module_info, module_info, qual_info, qual_info, 
		io__state, io__state).
:- mode module_add_pragma_c_code(in, in, in, in, in, in, in, in, in, out,
		in, out, di, uo) is det.  
module_add_pragma_c_code(IsRecursive, PredName, PredOrFunc, PVars, VarSet, 
			C_Code, Status, Context, ModuleInfo0, ModuleInfo, 
			Info0, Info) --> 
	{ module_info_name(ModuleInfo0, ModuleName) },
	{ list__length(PVars, Arity) },
		% print out a progress message
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( 
		{ VeryVerbose = yes }
	->
		io__write_string("% Processing `:- pragma c_code' for "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string("...\n")
	;
		[]
	),

		% Lookup the pred declaration in the predicate table.
		% (if it's not there, print an error message and insert
		% a dummy declaration for the predicate.) 
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) }, 
	( 
		{ predicate_table_search_pf_sym_arity(PredicateTable0,
			PredOrFunc, PredName, Arity, [PredId0]) }
	->
		{ PredId = PredId0 },
		{ PredicateTable1 = PredicateTable0 }
	;
		maybe_undefined_pred_error(PredName, Arity, PredOrFunc,
			Context, "pragma (c_code)"),
		{ preds_add_implicit(PredicateTable0,
				ModuleName, PredName, Arity, Context,
				PredOrFunc, PredId, PredicateTable1) }
	),
		% Lookup the pred_info for this pred,
		% add the pragma to the proc_info in the proc_table in the
		% pred_info, and save the pred_info.
	{ predicate_table_get_preds(PredicateTable1, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	{ Status = opt_imported ->
		pred_info_set_import_status(PredInfo0, opt_imported, PredInfo1)
	;
		PredInfo1 = PredInfo0
	},
	( 
		{ pred_info_is_imported(PredInfo1) }
	->
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: `:- pragma c_code' "),
		io__write_string("declaration for imported "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string(".\n"),
		{ Info = Info0 }
	;	
		{ pred_info_get_goal_type(PredInfo1, clauses) }
	->
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: `:- pragma c_code' declaration "),
		io__write_string("for "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("  with clauses preceding.\n"),
		{ Info = Info0 }
	;
		% add the pragma declaration to the proc_info for this procedure
		{ pred_info_procedures(PredInfo1, Procs) },
		{ map__to_assoc_list(Procs, ExistingProcs) },
		{ pragma_get_modes(PVars, Modes) },
		(
			{ get_matching_procedure(ExistingProcs, Modes,
						ModuleInfo0, ProcId) }
		->
			{ pred_info_clauses_info(PredInfo1, Clauses0) },
			clauses_info_add_pragma_c_code(Clauses0,
					IsRecursive, PredId, ProcId, VarSet,
					PVars, C_Code, Context,
					Clauses, Goal, Info0, Info),
			{ pred_info_set_clauses_info(PredInfo1, Clauses, 
					PredInfo2) },
			{ pred_info_set_goal_type(PredInfo2, pragmas, 
					PredInfo) },
			{ map__set(Preds0, PredId, PredInfo, Preds) },
			{ predicate_table_set_preds(PredicateTable1, Preds, 
				PredicateTable) },
			{ module_info_set_predicate_table(ModuleInfo0, 
				PredicateTable, ModuleInfo) },
			maybe_warn_singletons(VarSet, PredName/Arity, Goal)
		;
			{ module_info_incr_errors(ModuleInfo0, ModuleInfo) }, 
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			io__write_string("Error: `:- pragma c_code' "),
			io__write_string("declaration for undeclared mode "),
			io__write_string("of "),
			hlds_out__write_call_id(PredOrFunc, PredName/Arity),
			io__write_string(".\n"),
			io__set_output_stream(OldStream, _),
			{ Info = Info0 }
		)
	).

%-----------------------------------------------------------------------------%

	% from the list of pragma_vars extract the modes.
:- pred pragma_get_modes(list(pragma_var), list(mode)).
:- mode pragma_get_modes(in, out) is det.
pragma_get_modes([], []).
pragma_get_modes([V|Vars], [M|Modes]) :-
	V = pragma_var(_Variable, _Name, M),
	pragma_get_modes(Vars, Modes).

%-----------------------------------------------------------------------------%

	% from the list of pragma_vars , extract the vars.
:- pred pragma_get_vars(list(pragma_var), list(var)).
:- mode pragma_get_vars(in, out) is det.
pragma_get_vars([], []).
pragma_get_vars([P|PragmaVars], [V|Vars]) :-
	P = pragma_var(V, _Name, _Mode),
	pragma_get_vars(PragmaVars, Vars).

%---------------------------------------------------------------------------%

	% from the list of pragma_vars, extract the names.

:- pred pragma_get_var_names(list(pragma_var), list(maybe(string))).
:- mode pragma_get_var_names(in, out) is det.

pragma_get_var_names([], []).
pragma_get_var_names([P|PragmaVars], [yes(N)|Names]) :-
        P = pragma_var(_Var, N, _Mode),
        pragma_get_var_names(PragmaVars, Names).

%---------------------------------------------------------------------------%

	% For each pred_id in the list, set the given markers
	% in the corresponding pred_info.

:- pred pragma_set_markers(pred_table, list(pred_id), list(marker_status),
	pred_table).
:- mode pragma_set_markers(in, in, in, out) is det.

pragma_set_markers(PredTable, [], _, PredTable).
pragma_set_markers(PredTable0, [PredId | PredIds], Markers, PredTable) :-
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_get_marker_list(PredInfo0, MarkerList0),
	pragma_set_markers_2(Markers, MarkerList0, MarkerList),
	pred_info_set_marker_list(PredInfo0, MarkerList, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable1),
	pragma_set_markers(PredTable1, PredIds, Markers, PredTable).

:- pred pragma_set_markers_2(list(marker_status), list(marker_status),
	list(marker_status)).
:- mode pragma_set_markers_2(in, in, out) is det.

pragma_set_markers_2([], MarkerList, MarkerList).
pragma_set_markers_2([Marker | Markers], MarkerList0, MarkerList) :-
	( list__member(Marker, MarkerList0) ->
		MarkerList1 = MarkerList0
	;
		MarkerList1 = [Marker | MarkerList0]
	),
	pragma_set_markers_2(Markers, MarkerList1, MarkerList).

%---------------------------------------------------------------------------%

	% Find the procedure with modes which match the ones we want.

:- pred get_matching_procedure(assoc_list(proc_id, proc_info), list(mode), 
		module_info, proc_id).
:- mode get_matching_procedure(in, in, in, out) is semidet.

get_matching_procedure([P|Procs], Modes, ModuleInfo, OurProcId) :-
	P = ProcId - ProcInfo,
	proc_info_argmodes(ProcInfo, ArgModes),
	( mode_list_matches(Modes, ArgModes, ModuleInfo) ->
		OurProcId = ProcId
	;
		get_matching_procedure(Procs, Modes, ModuleInfo, OurProcId)
	).

:- pred mode_list_matches(list(mode)::in, list(mode)::in,
				module_info::in) is semidet.

mode_list_matches([], [], _).
mode_list_matches([Mode1 | Modes1], [Mode2 | Modes2], ModuleInfo) :-
	mode_get_insts(ModuleInfo, Mode1, Inst1, Inst2),
	mode_get_insts(ModuleInfo, Mode2, Inst1, Inst2),
	mode_list_matches(Modes1, Modes2, ModuleInfo).


%-----------------------------------------------------------------------------%

	% Warn about variables which occur only once but don't start with
	% an underscore, or about variables which do start with an underscore
	% but occur more than once.
	%
:- pred maybe_warn_overlap(list(quant_warning), varset,
				pred_or_func, pred_call_id,
				io__state, io__state).
:- mode maybe_warn_overlap(in, in, in, in, di, uo) is det.

maybe_warn_overlap(Warnings, VarSet, PredOrFunc, PredCallId) -->
	globals__io_lookup_bool_option(warn_overlapping_scopes,
			WarnOverlappingScopes),
	( { WarnOverlappingScopes = yes } ->
		warn_overlap(Warnings, VarSet, PredOrFunc, PredCallId)
	;	
		[]
	).


:- pred warn_overlap(list(quant_warning), varset, pred_or_func, pred_call_id,
				io__state, io__state).
:- mode warn_overlap(in, in, in, in, di, uo) is det.

warn_overlap([], _, _, _) --> [].
warn_overlap([Warn|Warns], VarSet, PredOrFunc, PredCallId) -->
	{ Warn = warn_overlap(Vars, Context) },
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	prog_out__write_context(Context),
	io__write_string(StdErr, "In clause for "),
	hlds_out__write_call_id(PredOrFunc, PredCallId),
	io__write_string(StdErr, ":\n"),
	prog_out__write_context(Context),
	( { Vars = [Var] } ->
		io__write_string(StdErr, "  Warning: variable `"),
		mercury_output_var(Var, VarSet),
		report_warning(StdErr, "' has overlapping scopes.\n")
	;
		io__write_string(StdErr, "  Warning: variables `"),
		mercury_output_vars(Vars, VarSet),
		report_warning(StdErr, "'\n"),
		prog_out__write_context(Context),
		report_warning(StdErr, "  each have overlapping scopes.\n")
	),
	io__set_output_stream(OldStream, _),
	warn_overlap(Warns, VarSet, PredOrFunc, PredCallId).

%-----------------------------------------------------------------------------%

	% Warn about variables which occur only once but don't start with
	% an underscore, or about variables which do start with an underscore
	% but occur more than once.
	%
:- pred maybe_warn_singletons(varset, pred_call_id, hlds__goal,
				io__state, io__state).
:- mode maybe_warn_singletons(in, in, in, di, uo) is det.

maybe_warn_singletons(VarSet, PredCallId, Body) -->
	globals__io_lookup_bool_option(warn_singleton_vars, WarnSingletonVars),
	( { WarnSingletonVars = yes } ->
		{ set__init(QuantVars) },
		warn_singletons_in_goal(Body, QuantVars, VarSet, PredCallId)
	;	
		[]
	).

:- pred warn_singletons_in_goal(hlds__goal, set(var), varset, pred_call_id,
				io__state, io__state).
:- mode warn_singletons_in_goal(in, in, in, in, di, uo) is det.

warn_singletons_in_goal(Goal - GoalInfo, QuantVars, VarSet, PredCallId) -->
	warn_singletons_in_goal_2(Goal, GoalInfo, QuantVars, VarSet,
		PredCallId).

:- pred warn_singletons_in_goal_2(hlds__goal_expr, hlds__goal_info, set(var),
				varset, pred_call_id, io__state, io__state).
:- mode warn_singletons_in_goal_2(in, in, in, in, in, di, uo) is det.

warn_singletons_in_goal_2(conj(Goals), _GoalInfo, QuantVars, VarSet,
		PredCallId) -->
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, PredCallId).

warn_singletons_in_goal_2(disj(Goals, _), _GoalInfo, QuantVars, VarSet,
		PredCallId) -->
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, PredCallId).

warn_singletons_in_goal_2(switch(_Var, _CanFail, Cases, _),
			_GoalInfo, QuantVars, VarSet, PredCallId) -->
	warn_singletons_in_cases(Cases, QuantVars, VarSet, PredCallId).

warn_singletons_in_goal_2(not(Goal), _GoalInfo, QuantVars, VarSet, PredCallId)
		-->
	warn_singletons_in_goal(Goal, QuantVars, VarSet, PredCallId).

warn_singletons_in_goal_2(some(Vars, SubGoal), GoalInfo, QuantVars, VarSet,
		PredCallId) -->
	%
	% warn if any quantified variables occur only in the quantifier
	%
	( { Vars \= [] } ->
		{ goal_vars(SubGoal, SubGoalVars) },
		{ goal_info_get_context(GoalInfo, Context) },
		{ set__init(EmptySet) },
		warn_singletons(Vars, EmptySet, SubGoalVars, VarSet, Context,
			PredCallId)
	;
		[]
	),
	{ set__insert_list(QuantVars, Vars, QuantVars1) },
	warn_singletons_in_goal(SubGoal, QuantVars1, VarSet, PredCallId).

warn_singletons_in_goal_2(if_then_else(Vars, Cond, Then, Else, _), GoalInfo,
				QuantVars, VarSet, PredCallId) -->
	%
	% warn if any quantified variables do not occur in the condition
	% or the "then" part of the if-then-else
	%
	( { Vars \= [] } ->
		{ goal_vars(Cond, CondVars) },
		{ goal_vars(Then, ThenVars) },
		{ set__union(CondVars, ThenVars, CondThenVars) },
		{ goal_info_get_context(GoalInfo, Context) },
		{ set__init(EmptySet) },
		warn_singletons(Vars, EmptySet, CondThenVars, VarSet,
			Context, PredCallId)
	;
		[]
	),

	{ set__insert_list(QuantVars, Vars, QuantVars1) },
	warn_singletons_in_goal(Cond, QuantVars1, VarSet, PredCallId),
	warn_singletons_in_goal(Then, QuantVars1, VarSet, PredCallId),
	warn_singletons_in_goal(Else, QuantVars, VarSet, PredCallId).

warn_singletons_in_goal_2(call(_, _, Args, _, _, _),
			GoalInfo, QuantVars, VarSet, PredCallId) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons(Args, NonLocals, QuantVars, VarSet, Context,
		PredCallId).

warn_singletons_in_goal_2(higher_order_call(_, Args, _, _, _),
			GoalInfo, QuantVars, VarSet, PredCallId) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons(Args, NonLocals, QuantVars, VarSet, Context,
		PredCallId).

warn_singletons_in_goal_2(unify(Var, RHS, _, _, _),
			GoalInfo, QuantVars, VarSet, PredCallId) -->
	warn_singletons_in_unify(Var, RHS, GoalInfo, QuantVars, VarSet,
		PredCallId).

warn_singletons_in_goal_2(pragma_c_code(C_Code, _, _, _, _, ArgNames), 
		GoalInfo, _QuantVars, _VarSet, PredCallId) --> 
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons_in_pragma_c_code(C_Code, ArgNames, Context, 
		PredCallId).

:- pred warn_singletons_in_goal_list(list(hlds__goal), set(var), varset,
				pred_call_id, io__state, io__state).
:- mode warn_singletons_in_goal_list(in, in, in, in, di, uo) is det.

warn_singletons_in_goal_list([], _, _, _) --> [].
warn_singletons_in_goal_list([Goal|Goals], QuantVars, VarSet, CallPredId) -->
	warn_singletons_in_goal(Goal, QuantVars, VarSet, CallPredId),
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, CallPredId).

:- pred warn_singletons_in_cases(list(case), set(var), varset, pred_call_id,
					io__state, io__state).
:- mode warn_singletons_in_cases(in, in, in, in, di, uo) is det.

warn_singletons_in_cases([], _, _, _) --> [].
warn_singletons_in_cases([Case|Cases], QuantVars, VarSet, CallPredId) -->
	{ Case = case(_ConsId, Goal) },
	warn_singletons_in_goal(Goal, QuantVars, VarSet, CallPredId),
	warn_singletons_in_cases(Cases, QuantVars, VarSet, CallPredId).

:- pred warn_singletons_in_unify(var, unify_rhs, hlds__goal_info, set(var),
			varset, pred_call_id, io__state, io__state).
:- mode warn_singletons_in_unify(in, in, in, in, in, in, di, uo) is det.

warn_singletons_in_unify(X, var(Y), GoalInfo, QuantVars, VarSet, CallPredId) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons([X, Y], NonLocals, QuantVars, VarSet,
			Context, CallPredId).

warn_singletons_in_unify(X, functor(_ConsId, Vars), GoalInfo, QuantVars, VarSet,
				CallPredId) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons([X | Vars], NonLocals, QuantVars, VarSet,
			Context, CallPredId).

warn_singletons_in_unify(X, lambda_goal(_PredOrFunc, LambdaVars, _Modes, _Det,
				LambdaGoal),
				GoalInfo, QuantVars, VarSet, CallPredId) -->
	%
	% warn if any lambda-quantified variables occur only in the quantifier
	%
	{ LambdaGoal = _ - LambdaGoalInfo },
	{ goal_info_get_nonlocals(LambdaGoalInfo, LambdaNonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons(LambdaVars, LambdaNonLocals, QuantVars, VarSet,
			Context, CallPredId),

	%
	% warn if X (the variable we're unifying the lambda expression with)
	% is singleton
	%
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	warn_singletons([X], NonLocals, QuantVars, VarSet, Context, CallPredId),

	%
	% warn if the lambda-goal contains singletons
	%
	warn_singletons_in_goal(LambdaGoal, QuantVars, VarSet, CallPredId).

%-----------------------------------------------------------------------------%

	% warn_singletons_in_pragma_c_code checks to see if each variable is
	% a substring of the given c code. If not, it gives a warning
:- pred warn_singletons_in_pragma_c_code(string, list(maybe(string)),
	term__context, pred_call_id, io__state, io__state).
:- mode warn_singletons_in_pragma_c_code(in, in, in, in, di, uo) is det.

warn_singletons_in_pragma_c_code(C_Code, ArgNames, 
		Context, PredCallId) -->
	{ c_code_to_name_list(C_Code, C_CodeList) },
	{ warn_singletons_in_pragma_c_code_2(C_CodeList, ArgNames,
		Context, SingletonVars) },
	( { SingletonVars = [] } ->
		[]
	;
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		prog_out__write_context(Context),
		( { SingletonVars = [_] } ->
			io__write_string("Warning: variable `"),
			write_string_list(SingletonVars),
			io__write_string("' does not occur in C code\n")
		;
			io__write_string("Warning: variables `"),
			write_string_list(SingletonVars),
			io__write_string("' do not occur in C code\n")
		),
		prog_out__write_context(Context),
		io__write_string("  in `:- pragma c_code' for "),
		hlds_out__write_call_id(predicate, PredCallId),
		io__write_string(".\n"),
		io__set_output_stream(OldStream, _)
	).


%-----------------------------------------------------------------------------%

:- pred warn_singletons_in_pragma_c_code_2(list(string), list(maybe(string)), 
				term__context, list(string)).
:- mode warn_singletons_in_pragma_c_code_2(in, in, in, out) is det.

warn_singletons_in_pragma_c_code_2(_, [], _, []).
warn_singletons_in_pragma_c_code_2(C_CodeList, [Arg|Args],
		Context, SingletonVars) :-
	warn_singletons_in_pragma_c_code_2(C_CodeList, Args,
		 Context, SingletonVars0),
	( Arg = yes(Name) ->
		(
			( string__prefix(Name, "_")
			; list__member(Name, C_CodeList)
			)
		->
			SingletonVars = SingletonVars0
		;
			SingletonVars = [Name|SingletonVars0]
		)
	;
		SingletonVars = SingletonVars0
	).
%-----------------------------------------------------------------------------%

	% c_code_to_name_list(Code, List) is true iff List is a list of the 
	% identifiers used in the C code in Code.
:- pred c_code_to_name_list(string, list(string)).
:- mode c_code_to_name_list(in, out) is det.

c_code_to_name_list(Code, List) :-
	string__to_char_list(Code, CharList),
	c_code_to_name_list_2(CharList, List).

:- pred c_code_to_name_list_2(list(char), list(string)).
:- mode c_code_to_name_list_2(in, out) is det.

c_code_to_name_list_2(C_Code, List) :-
	get_first_c_name(C_Code, NameCharList, TheRest),
	(
		NameCharList = []
	->
		% no names left
		List = []
	;
		c_code_to_name_list_2(TheRest, Names),
		string__from_char_list(NameCharList, Name),
		List = [Name|Names]
	).

:- pred get_first_c_name(list(char), list(char), list(char)).
:- mode get_first_c_name(in, out, out) is det.
	
get_first_c_name([], [], []).
get_first_c_name([C|CodeChars], NameCharList, TheRest) :-
	(
		char__is_alnum_or_underscore(C)
	->
		get_first_c_name_in_word(CodeChars, NameCharList0, TheRest),
		NameCharList = [C|NameCharList0]
	;
			% strip off any characters in the C code which 
			% don't form part of an identifier.
		get_first_c_name(CodeChars, NameCharList, TheRest)
	).
	
:- pred get_first_c_name_in_word(list(char), list(char), list(char)).
:- mode get_first_c_name_in_word(in, out, out) is det.

get_first_c_name_in_word([], [], []).
get_first_c_name_in_word([C|CodeChars], NameCharList, TheRest) :-
	(
		char__is_alnum_or_underscore(C)
	->
			% There are more characters in the word
		get_first_c_name_in_word(CodeChars, NameCharList0, TheRest),
		NameCharList = [C|NameCharList0]
	;
			% The word is finished
		NameCharList = [],
		TheRest = CodeChars
	).

%-----------------------------------------------------------------------------%

:- pred write_string_list(list(string), io__state, io__state).
:- mode write_string_list(in, di, uo) is det.

write_string_list([]) --> [].
write_string_list([X|Xs]) -->
	io__write_string(X),
	(
		{ Xs = [] }
	->
		[]
	;
		io__write_string(", "),
		write_string_list(Xs)
	).

%-----------------------------------------------------------------------------%

	% warn_singletons(Vars, NonLocals, QuantVars, ...):
	% 	Warn if any of the non-underscore variables in Vars don't
	%	occur in NonLocals and don't have the same name as any variable
	%	in QuantVars, or if any of the underscore variables
	%	in Vars do occur in NonLocals.

:- pred warn_singletons(list(var), set(var), set(var), varset, term__context,
			pred_call_id, io__state, io__state).
:- mode warn_singletons(in, in, in, in, in, in, di, uo) is det.

warn_singletons(GoalVars, NonLocals, QuantVars, VarSet, Context, PredCallId) -->
	io__stderr_stream(StdErr),

	% find all the variables in the goal that don't occur outside the
	% goal (i.e. are singleton), have a variable name that doesn't
	% start with "_" or "DCG_", and don't have the same name as any
	% variable in QuantVars (i.e. weren't explicitly quantified).
	
	{ solutions(lambda([Var::out] is nondet, (
		  	list__member(Var, GoalVars),
			\+ set__member(Var, NonLocals),
			varset__search_name(VarSet, Var, Name),
			\+ string__prefix(Name, "_"),
			\+ string__prefix(Name, "DCG_"),
			\+ (	
				set__member(QuantVar, QuantVars),
				varset__search_name(VarSet, QuantVar, Name)
			)
		)), SingletonVars) },

	% if there were any such variables, issue a warning

	( { SingletonVars = [] } ->
		[]
	;
		prog_out__write_context(Context),
		io__write_string(StdErr, "In clause for predicate `"),
		hlds_out__write_pred_call_id(PredCallId),
		io__write_string(StdErr, "':\n"),
		prog_out__write_context(Context),
		( { SingletonVars = [_] } ->
			io__write_string(StdErr, "  Warning: variable `"),
			mercury_output_vars(SingletonVars, VarSet),
			report_warning(StdErr, "' occurs only once in this scope.\n")
		;
			io__write_string(StdErr, "  Warning: variables `"),
			mercury_output_vars(SingletonVars, VarSet),
			report_warning(StdErr, "' occur only once in this scope.\n")
		)
	),

	% find all the variables in the goal that do occur outside the
	% goal (i.e. are not singleton) and have a variable name that starts
	% with "_".
	
	{ solutions(lambda([Var2::out] is nondet, (
		  	list__member(Var2, GoalVars),
			set__member(Var2, NonLocals),
			varset__search_name(VarSet, Var2, Name2),
			string__prefix(Name2, "_")
		)), MultiVars) },

	% if there were any such variables, issue a warning

	( { MultiVars = [] } ->
		[]
	;
		prog_out__write_context(Context),
		io__write_string(StdErr, "In clause for predicate `"),
		hlds_out__write_pred_call_id(PredCallId),
		io__write_string(StdErr, "':\n"),
		prog_out__write_context(Context),
		( { MultiVars = [_] } ->
			io__write_string(StdErr, "  Warning: variable `"),
			mercury_output_vars(MultiVars, VarSet),
			report_warning(StdErr, "' occurs more than once in this scope.\n")
		;
			io__write_string(StdErr, "  Warning: variables `"),
			mercury_output_vars(MultiVars, VarSet),
			report_warning(StdErr, "' occur more than once in this scope.\n")
		)
	).

%-----------------------------------------------------------------------------

clauses_info_init(Arity, ClausesInfo) :-
	map__init(VarTypes),
	varset__init(VarSet0),
	make_n_fresh_vars("HeadVar__", Arity, VarSet0, HeadVars, VarSet),
	ClausesInfo = clauses_info(VarSet, VarTypes, VarTypes, HeadVars, []).

:- pred clauses_info_add_clause(clauses_info::in, pred_id::in, 
		list(proc_id)::in, varset::in, tvarset::in, list(term)::in,
		goal::in, term__context::in, hlds__goal::out, varset::out,
		tvarset::out, clauses_info::out, list(quant_warning)::out,
		qual_info::in, qual_info::out,
		io__state::di, io__state::uo) is det.

clauses_info_add_clause(ClausesInfo0, PredId, ModeIds, CVarSet, TVarSet0,
		Args, Body, Context, Goal, VarSet, TVarSet0,
		ClausesInfo, Warnings, Info0, Info) -->
	{ ClausesInfo0 = clauses_info(VarSet0, VarTypes0, VarTypes1,
					HeadVars, ClauseList0) },
	{ update_qual_info(Info0, TVarSet0, VarTypes0, PredId, Info1) },
	{ varset__merge_subst(VarSet0, CVarSet, VarSet1, Subst) },
	transform(Subst, HeadVars, Args, Body, VarSet1, Context,
				Goal, VarSet, Warnings, Info1, Info),
		% XXX we should avoid append - this gives O(N*N)
	{ list__append(ClauseList0, [clause(ModeIds, Goal, Context)],
							ClauseList) },
	{ qual_info_get_var_types(Info, VarTypes) },
	{ ClausesInfo = clauses_info(VarSet, VarTypes, VarTypes1,
					HeadVars, ClauseList) }.

%-----------------------------------------------------------------------------

% Add the pragma_c_code goal to the clauses_info for this procedure.
% To do so, we must also insert unifications between the variables in the
% pragma c_code declaration and the head vars of the pred. Also return the
% hlds__goal.

:- pred clauses_info_add_pragma_c_code(clauses_info, c_is_recursive,
	pred_id, proc_id, varset, list(pragma_var), string, term__context,
	clauses_info, hlds__goal, qual_info, qual_info,
	io__state, io__state) is det.
:- mode clauses_info_add_pragma_c_code(in, in, in, in, in, in, in, in, out, 
	out, in, out, di, uo) is det.

clauses_info_add_pragma_c_code(ClausesInfo0, IsRecursive, PredId, ModeId,
	PVarSet, PVars, C_Code, Context, ClausesInfo, HldsGoal, Info0, Info)
		-->
	{
	ClausesInfo0 = clauses_info(VarSet0, VarTypes, VarTypes1,
				 HeadVars, ClauseList),
	pragma_get_vars(PVars, Args0),
	pragma_get_var_names(PVars, Names),

		% merge the varsets of the proc and the new pragma_c_code
        varset__merge_subst(VarSet0, PVarSet, VarSet1, Subst),
        map__apply_to_list(Args0, Subst, TermArgs),
	term__term_list_to_var_list(TermArgs, Args),

		% build the pragma_c_code
	goal_info_init(GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo),
	HldsGoal0 = pragma_c_code(C_Code, IsRecursive, PredId, ModeId, Args,
				Names) - GoalInfo
	}, 
		% Insert unifications with the head args.
	insert_arg_unifications(HeadVars, TermArgs, Context, head, HldsGoal0,
		VarSet1, HldsGoal1, VarSet2, Info0, Info),
	{
	map__init(Empty),
	implicitly_quantify_clause_body(HeadVars, HldsGoal1, VarSet2, Empty,
		HldsGoal, VarSet, _, _Warnings),
	NewClause = clause([ModeId], HldsGoal, Context),
	ClausesInfo =  clauses_info(VarSet, VarTypes, VarTypes1, HeadVars, 
		[NewClause|ClauseList])
	}.

%-----------------------------------------------------------------------------

:- pred transform(substitution, list(var), list(term), goal, varset,
			term__context, hlds__goal, varset, list(quant_warning),
			qual_info, qual_info, io__state, io__state).
:- mode transform(in, in, in, in, in, in, out, out, out,
			in, out, di, uo) is det.

transform(Subst, HeadVars, Args0, Body, VarSet0, Context,
		Goal, VarSet, Warnings, Info0, Info) -->
	transform_goal(Body, VarSet0, Subst, Goal1, VarSet1, Info0, Info1),
	{ term__apply_substitution_to_list(Args0, Subst, Args) },
	insert_arg_unifications(HeadVars, Args, Context, head, Goal1, VarSet1,
		Goal2, VarSet2, Info1, Info),
	{ map__init(Empty) },
	{ implicitly_quantify_clause_body(HeadVars, Goal2, VarSet2, Empty,
				Goal, VarSet, _, Warnings) }.

%-----------------------------------------------------------------------------%

	% Convert goals from the prog_data `goal' structure into the
	% hlds `hlds__goal' structure.  At the same time, convert
	% it to super-homogeneous form by unravelling all the complex
	% unifications, and annotate those unifications with a unify_context
	% so that we can still give good error messages.
	% And also at the same time, apply the given substitution to
	% the goal, to rename it apart from the other clauses.

:- pred transform_goal(goal, varset, substitution, hlds__goal, varset,
			qual_info, qual_info, io__state, io__state).
:- mode transform_goal(in, in, in, out, out, in, out, di, uo) is det.

transform_goal(Goal0 - Context, VarSet0, Subst, Goal1 - GoalInfo1, VarSet,
		Info0, Info) -->
	transform_goal_2(Goal0, Context, VarSet0, Subst, Goal1 - GoalInfo0,
					VarSet, Info0, Info),
	{ goal_info_set_context(GoalInfo0, Context, GoalInfo1) }.

:- pred transform_goal_2(goal_expr, term__context, varset, substitution,
		hlds__goal, varset, qual_info, qual_info, io__state, io__state).
:- mode transform_goal_2(in, in, in, in, out, out, in, out, di, uo) is det.

transform_goal_2(fail, _, VarSet, _, disj([], Empty) - GoalInfo, VarSet,
		Info, Info) -->
	{ map__init(Empty) },
	{ goal_info_init(GoalInfo) }.

transform_goal_2(true, _, VarSet, _, conj([]) - GoalInfo, VarSet,
		Info, Info) -->
	{ goal_info_init(GoalInfo) }.

	% Convert `all [Vars] Goal' into `not some [Vars] not Goal'.
transform_goal_2(all(Vars0, Goal0), Context, VarSet0, Subst, Goal, VarSet,
		Info0, Info) -->
	{ TransformedGoal = not(some(Vars0, not(Goal0) - Context) - Context) },
	transform_goal_2(TransformedGoal, Context, VarSet0, Subst,
						Goal, VarSet, Info0, Info).

transform_goal_2(some(Vars0, Goal0), _, VarSet0, Subst,
		some(Vars, Goal) - GoalInfo, VarSet, Info0, Info) -->
	{ substitute_vars(Vars0, Subst, Vars) },
	transform_goal(Goal0, VarSet0, Subst, Goal, VarSet, Info0, Info),
	{ goal_info_init(GoalInfo) }.

transform_goal_2(if_then_else(Vars0, A0, B0, C0), _, VarSet0, Subst,
	if_then_else(Vars, A, B, C, Empty) - GoalInfo, VarSet, Info0, Info)
		-->
	{ substitute_vars(Vars0, Subst, Vars) },
	transform_goal(A0, VarSet0, Subst, A, VarSet1, Info0, Info1),
	transform_goal(B0, VarSet1, Subst, B, VarSet2, Info1, Info2),
	transform_goal(C0, VarSet2, Subst, C, VarSet, Info2, Info),
	{ map__init(Empty) },
	{ goal_info_init(GoalInfo) }.

transform_goal_2(if_then(Vars0, A0, B0), Context, Subst, VarSet0,
		Goal, VarSet, Info0, Info) -->
	transform_goal_2(if_then_else(Vars0, A0, B0, true - Context),
			Context, Subst, VarSet0, Goal, VarSet, Info0, Info).

transform_goal_2(not(A0), _, VarSet0, Subst, Goal, VarSet, Info0, Info) -->
	transform_goal(A0, VarSet0, Subst, A, VarSet, Info0, Info),
	% eliminate double negations
	{ A = not(Goal1) - _ ->
		Goal = Goal1
	;
		goal_info_init(GoalInfo),
		Goal = not(A) - GoalInfo
	}.

transform_goal_2((A0,B0), _, VarSet0, Subst, Goal, VarSet, Info0, Info) -->
	get_conj(B0, Subst, [], VarSet0, L0, VarSet1, Info0, Info1),
	get_conj(A0, Subst, L0, VarSet1, L, VarSet, Info1, Info),
	{ goal_info_init(GoalInfo) },
	{ conj_list_to_goal(L, GoalInfo, Goal) }.

transform_goal_2((A0;B0), _, VarSet0, Subst, Goal, VarSet, Info0, Info) -->
	get_disj(B0, Subst, [], VarSet0, L0, VarSet1, Info0, Info1),
	get_disj(A0, Subst, L0, VarSet1, L, VarSet, Info1, Info),
	{ goal_info_init(GoalInfo) },
	{ disj_list_to_goal(L, GoalInfo, Goal) }.

transform_goal_2(implies(P, Q), Context, VarSet0, Subst, Goal, VarSet,
		Info0, Info) -->
		% `P => Q' is defined as `not (P, not Q)'
	{ TransformedGoal = not( (P, not(Q) - Context) - Context ) },
	transform_goal_2(TransformedGoal, Context, VarSet0, Subst,
		Goal, VarSet, Info0, Info).

transform_goal_2(equivalent(P, Q), Context, VarSet0, Subst, Goal, VarSet,
		Info0, Info) -->
		% `P <=> Q' is defined as `(P => Q), (Q => P)'
	{ TransformedGoal = (implies(P, Q) - Context,
				implies(Q, P) - Context) },
	transform_goal_2(TransformedGoal, Context, VarSet0, Subst,
		Goal, VarSet, Info0, Info).

transform_goal_2(call(Name, Args0), Context, VarSet0, Subst, Goal, VarSet,
		Info0, Info) -->
	( 
		{ Name = unqualified("\\=") },
		{ Args0 = [LHS, RHS] }
	->
			% `LHS \= RHS' is defined as `not (RHS = RHS)'
		transform_goal_2(not(unify(LHS, RHS) - Context), Context,
				VarSet0, Subst, Goal, VarSet, Info0, Info)
	;
		{ term__apply_substitution_to_list(Args0, Subst, Args) },
		{ make_fresh_arg_vars(Args, VarSet0, HeadVars, VarSet1) },
		{
			% check for a higher-order call,
			% i.e. a call to either call/N or ''/N.
			( Name = unqualified("call")
			; Name = unqualified("")
			),
			HeadVars = [PredVar | RealHeadVars]
		->
			% initialize some fields to junk
			Types = [],
			Modes = [],
			Det = erroneous,
			Call = higher_order_call(PredVar, RealHeadVars,
					Types, Modes, Det)
		;
			% initialize some fields to junk
			invalid_pred_id(PredId),
			ModeId = 0,
			hlds__is_builtin_make_builtin(no, no, Builtin),
			MaybeUnifyContext = no,
			Call = call(PredId, ModeId, HeadVars, Builtin,
					MaybeUnifyContext, Name)
		},
		{ goal_info_init(GoalInfo0) },
		{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
		{ Goal0 = Call - GoalInfo },

		{ list__length(Args, Arity) },
		{ PredCallId = Name/Arity },
		insert_arg_unifications(HeadVars, Args,
			Context, call(PredCallId),
			Goal0, VarSet1, Goal, VarSet, Info0, Info)
	).

transform_goal_2(unify(A0, B0), Context, VarSet0, Subst, Goal, VarSet,
		Info0, Info) -->
	{ term__apply_substitution(A0, Subst, A) },
	{ term__apply_substitution(B0, Subst, B) },
	unravel_unification(A, B, Context, explicit, [],
			VarSet0, Goal, VarSet, Info0, Info).

%-----------------------------------------------------------------------------

	% `insert_arg_unifications' takes a list of variables,
	% a list of terms to unify them with, and a goal, and
	% inserts the appropriate unifications onto the front of
	% the goal.  It calls `unravel_unification' to ensure
	% that each unification gets reduced to superhomogeneous form.
	% It also gets passed a `arg_context', which indicates
	% where the terms came from.

:- type arg_context
	--->	head		% the arguments in the head of the clause
	;	call(pred_call_id) % the arguments in a call to a predicate
	;	functor(	% the arguments in a functor
			cons_id,
			unify_main_context,
			unify_sub_contexts
		).

:- pred insert_arg_unifications(list(var), list(term),
		term__context, arg_context, hlds__goal, varset, hlds__goal,
		varset, qual_info, qual_info, io__state, io__state).
:- mode insert_arg_unifications(in, in, in, in, in, in, out, out,
		in, out, di, uo) is det.

insert_arg_unifications(HeadVars, Args, Context, ArgContext, Goal0, VarSet0,
			Goal, VarSet, Info0, Info) -->
	( { HeadVars = [] } ->
		{ Goal = Goal0 },
		{ VarSet = VarSet0 },
		{ Info = Info0 }
	;
		{ Goal0 = _ - GoalInfo },
		{ goal_to_conj_list(Goal0, List0) },
		insert_arg_unifications_2(HeadVars, Args, Context, ArgContext,
			0, List0, VarSet0, List, VarSet, Info0, Info),
		{ conj_list_to_goal(List, GoalInfo, Goal) }
	).

:- pred insert_arg_unifications_2(list(var), list(term),
		term__context, arg_context, int, list(hlds__goal), varset,
		list(hlds__goal), varset, qual_info, qual_info,
		io__state, io__state).
:- mode insert_arg_unifications_2(in, in, in, in, in, in, in, out,
		out, in, out, di, uo) is det.

:- insert_arg_unifications_2(A, B, _,_,_,_, _, _, _, _, _, _, _) when A and B.

insert_arg_unifications_2([], [_|_], _, _, _, _, _, _, _, _, _) -->
	{ error("insert_arg_unifications_2: length mismatch") }.
insert_arg_unifications_2([_|_], [], _, _, _, _, _, _, _, _, _) -->
	{ error("insert_arg_unifications_2: length mismatch") }.
insert_arg_unifications_2([], [], _, _, _, List, VarSet, List, VarSet,
			Info, Info) --> [].
insert_arg_unifications_2([Var|Vars], [Arg|Args], Context, ArgContext, N0,
			List0, VarSet0, List, VarSet, Info0, Info) -->
	{ N1 is N0 + 1 },
		% skip unifications of the form `X = X'
	( { Arg = term__variable(Var) } ->
		insert_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
				List0, VarSet0, List, VarSet, Info0, Info)
	;
		{ arg_context_to_unify_context(ArgContext, N1,
			UnifyMainContext, UnifySubContext) },
		unravel_unification(term__variable(Var), Arg,
			Context, UnifyMainContext, UnifySubContext,
			VarSet0, Goal, VarSet1, Info0, Info1),
		{ goal_to_conj_list(Goal, ConjList) },
		{ list__append(ConjList, List1, List) },
		insert_arg_unifications_2(Vars, Args, Context, ArgContext,
			N1, List0, VarSet1, List1, VarSet, Info1, Info)
	).

	% append_arg_unifications is the same as insert_arg_unifications,
	% except that the unifications are added after the goal rather
	% than before the goal.

:- pred append_arg_unifications(list(var), list(term),
		term__context, arg_context, hlds__goal, varset, hlds__goal,
		varset, qual_info, qual_info, io__state, io__state).
:- mode append_arg_unifications(in, in, in, in, in, in,
		out, out, in, out, di, uo) is det.

append_arg_unifications(HeadVars, Args, Context, ArgContext, Goal0, VarSet0,
			Goal, VarSet, Info0, Info) -->
	( { HeadVars = [] } ->
		{ Goal = Goal0 },
		{ VarSet = VarSet0 },
		{ Info = Info0 }
	;
		{ Goal0 = _ - GoalInfo },
		{ goal_to_conj_list(Goal0, List0) },
		append_arg_unifications_2(HeadVars, Args, Context, ArgContext,
			0, List0, VarSet0, List, VarSet, Info0, Info),
		{ conj_list_to_goal(List, GoalInfo, Goal) }
	).

:- pred append_arg_unifications_2(list(var), list(term),
	term__context, arg_context, int, list(hlds__goal), varset,
	list(hlds__goal), varset, qual_info, qual_info, io__state, io__state).
:- mode append_arg_unifications_2(in, in, in, in, in, in, in,
	out, out, in, out, di, uo) is det.

:- append_arg_unifications_2(A, B, _,_,_, _, _, _, _, _, _, _, _) when A and B.

append_arg_unifications_2([], [_|_], _, _, _, _, _, _, _, _, _) -->
	{ error("append_arg_unifications_2: length mismatch") }.
append_arg_unifications_2([_|_], [], _, _, _, _, _, _, _, _, _) -->
	{ error("append_arg_unifications_2: length mismatch") }.
append_arg_unifications_2([], [], _, _, _, List, VarSet, List, VarSet,
			Info, Info) --> [].
append_arg_unifications_2([Var|Vars], [Arg|Args], Context, ArgContext, N0,
			List0, VarSet0, List, VarSet, Info0, Info) -->
	{ N1 is N0 + 1 },
		% skip unifications of the form `X = X'
	( { Arg = term__variable(Var) } ->
		append_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
				List0, VarSet0, List, VarSet, Info0, Info)
	;
		{ arg_context_to_unify_context(ArgContext, N1,
				UnifyMainContext, UnifySubContext) },
		unravel_unification(term__variable(Var), Arg,
			Context, UnifyMainContext, UnifySubContext,
			VarSet0, Goal, VarSet1, Info0, Info1),
		{ goal_to_conj_list(Goal, ConjList) },
		{ list__append(List0, ConjList, List1) },
		append_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
				List1, VarSet1, List, VarSet, Info1, Info)
	).

:- pred arg_context_to_unify_context(arg_context, int,
				unify_main_context, unify_sub_contexts).
:- mode arg_context_to_unify_context(in, in, out, out) is det.

arg_context_to_unify_context(head, N, head(N), []).
arg_context_to_unify_context(call(PredId), N, call(PredId, N), []).
arg_context_to_unify_context(functor(ConsId, MainContext, SubContexts), N,
			MainContext, [ConsId - N | SubContexts]).

%-----------------------------------------------------------------------------%

	% make_fresh_arg_vars(Args, VarSet0, Vars, VarSet):
	%	`Vars' is a list of distinct variables corresponding to
	%	the terms in `Args'.  For each term in `Args', if
	%	the term is a variable V which is distinct from the
	%	variables already produced, then the corresponding
	%	variable in `Vars' is just V, otherwise a fresh variable
	%	is allocated from `VarSet0'.   `VarSet' is the resulting
	%	varset after all the necessary variables have been allocated.
	%
	%	For efficiency, the list `Vars' is constructed backwards
	%	and then reversed to get the correct order.

:- pred make_fresh_arg_vars(list(term), varset, list(var), varset).
:- mode make_fresh_arg_vars(in, in, out, out) is det.

make_fresh_arg_vars(Args, VarSet0, Vars, VarSet) :-
	make_fresh_arg_vars_2(Args, [], VarSet0, Vars1, VarSet),
	list__reverse(Vars1, Vars).

:- pred make_fresh_arg_vars_2(list(term), list(var), varset,
				list(var), varset).
:- mode make_fresh_arg_vars_2(in, in, in, out, out) is det.

make_fresh_arg_vars_2([], Vars, VarSet, Vars, VarSet).
make_fresh_arg_vars_2([Arg | Args], Vars0, VarSet0, Vars, VarSet) :-
	( Arg = term__variable(ArgVar), \+ list__member(ArgVar, Vars0) ->
		Var = ArgVar,
		VarSet1 = VarSet0
	;
		varset__new_var(VarSet0, Var, VarSet1)
	),
	make_fresh_arg_vars_2(Args, [Var | Vars0], VarSet1, Vars, VarSet).

%-----------------------------------------------------------------------------%

:- pred unravel_unification(term, term, term__context,
		unify_main_context, unify_sub_contexts, varset, hlds__goal,
		varset, qual_info, qual_info, io__state, io__state).
:- mode unravel_unification(in, in, in, in, in, in, out, out,
		in, out, di, uo) is det.

	% `X = Y' needs no unravelling.

unravel_unification(term__variable(X), term__variable(Y), Context,
	MainContext, SubContext, VarSet0, Goal, VarSet, Info, Info)
		-->
	{ create_atomic_unification(X, var(Y), Context, MainContext,
		SubContext, Goal) },
	{ VarSet0 = VarSet }.

	% If we find a unification of the form
	%	X = f(A1, A2, A3)
	% we replace it with
	%	X = f(NewVar1, NewVar2, NewVar3),
	%	NewVar1 = A1,
	%	NewVar2 = A2,
	%	NewVar3 = A3.
	% In the trivial case `X = c', no unravelling occurs.

unravel_unification(term__variable(X), Rhs,
			Context, MainContext, SubContext, VarSet0,
			Goal, VarSet, Info0, Info) -->
	{ Rhs = term__functor(F, Args, FunctorContext) },
	(
		% Handle explicit type qualification.
		{ semidet_fail },
		{ F = term__atom("TYPE_QUAL_OP") },
		{ Args = [RVal, DeclType] }
	->
		process_type_qualification(X, DeclType, VarSet0,
			Context, Info0, Info1),
		unravel_unification(term__variable(X), RVal,
			Context, MainContext, SubContext, VarSet0,
			Goal, VarSet, Info1, Info)
	;	
	    {
		% handle lambda expressions
		F = term__atom("lambda"),
		Args = [LambdaExpressionTerm, GoalTerm0],
		parse_lambda_expression(LambdaExpressionTerm,
			Vars0, Modes0, Det0)
	    ->
		Vars1 = Vars0, Modes1 = Modes0, Det1 = Det0,
		GoalTerm = GoalTerm0
	    ;
		% handle higher-order pred expressions -
		% same semantics as lambda expressions, different syntax
		% (the original lambda expression syntax is now deprecated)
		F = term__atom(":-"),
		Args = [PredTerm, GoalTerm0],
		parse_pred_expression(PredTerm, Vars0, Modes0, Det0)
	    ->
		Vars1 = Vars0, Modes1 = Modes0, Det1 = Det0,
		GoalTerm = GoalTerm0
	    ;
		parse_pred_expression(term__functor(F, Args, FunctorContext),
			Vars1, Modes1, Det1),
		GoalTerm = term__functor(term__atom("true"), [], Context)
	    }
	->
		{ qual_info_get_mq_info(Info0, MQInfo0) },
		module_qual__qualify_mode_list(Modes1, Modes, Context,
						MQInfo0, MQInfo1),
		{ qual_info_set_mq_info(Info0, MQInfo1, Info1) },
		{ Det = Det1 },
		{ make_fresh_arg_vars(Vars1, VarSet0, Vars, VarSet1) },
		{ parse_goal(GoalTerm, VarSet1, ParsedGoal, VarSet2) },
		{ map__init(Substitution) },
		transform_goal(ParsedGoal, VarSet2, Substitution,
				HLDS_Goal0, VarSet3, Info1, Info2),
		insert_arg_unifications(Vars, Vars1, Context, head,
			HLDS_Goal0, VarSet3, HLDS_Goal, VarSet, Info2, Info),
		{ create_atomic_unification(X,
			lambda_goal(predicate, Vars, Modes, Det, HLDS_Goal),
			Context, MainContext, SubContext, Goal) }
	;
	    {
		% handle higher-order func expressions -
		% like higher-order pred expressions, but for functions
		F = term__atom(":-"),
		Args = [FuncTerm, GoalTerm0],
		parse_func_expression(FuncTerm, Vars0, Modes0, Det0)
	    ->
		Vars1 = Vars0, Modes1 = Modes0, Det1 = Det0,
		GoalTerm = GoalTerm0
	    ;
		parse_func_expression(term__functor(F, Args, FunctorContext),
			Vars1, Modes1, Det1),
		GoalTerm = term__functor(term__atom("true"), [], Context)
	    }
	->
		{ qual_info_get_mq_info(Info0, MQInfo0) },
		module_qual__qualify_mode_list(Modes1, Modes, Context,
						MQInfo0, MQInfo1),
		{ qual_info_set_mq_info(Info0, MQInfo1, Info1) },
		{ Det = Det1 },
		{ make_fresh_arg_vars(Vars1, VarSet0, Vars, VarSet1) },
		{ parse_goal(GoalTerm, VarSet1, ParsedGoal, VarSet2) },
		{ map__init(Substitution) },
		transform_goal(ParsedGoal, VarSet2, Substitution,
				HLDS_Goal0, VarSet3, Info1, Info2),
		insert_arg_unifications(Vars, Vars1, Context, head,
			HLDS_Goal0, VarSet3, HLDS_Goal, VarSet, Info2, Info),
		{ create_atomic_unification(X,
			lambda_goal(function, Vars, Modes, Det, HLDS_Goal),
			Context, MainContext, SubContext, Goal) }
	;
	        % handle if-then-else expressions
		{   F = term__atom("else"),
		    Args = [term__functor(term__atom("if"), [
				term__functor(term__atom("then"),
					[IfTerm, ThenTerm], _)
				], _),
			    ElseTerm]
		;   F = term__atom(";"),
		    Args = [term__functor(term__atom("->"),
				[IfTerm, ThenTerm], _),
			    ElseTerm]
		},
		{ parse_some_vars_goal(IfTerm, VarSet0, Vars, IfParseTree,
			VarSet11) }
	->
		{ map__init(Subst) },
		transform_goal(IfParseTree, VarSet11, Subst, IfGoal, VarSet22,
			Info0, Info1),
		unravel_unification(term__variable(X), ThenTerm,
			Context, MainContext, SubContext, VarSet22, ThenGoal,
			VarSet33, Info1, Info2),
		unravel_unification(term__variable(X), ElseTerm,
			Context, MainContext, SubContext, VarSet33, ElseGoal,
			VarSet, Info2, Info),
		{ map__init(Empty) },
		{ IfThenElse = if_then_else(Vars, IfGoal, ThenGoal, ElseGoal,
			Empty) },
		{ goal_info_init(GoalInfo0) },
		{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
		{ Goal = IfThenElse - GoalInfo }
	;
		{ parse_qualified_term(Rhs, "", MaybeFunctor) },
		(
			{ MaybeFunctor = ok(FunctorName, FunctorArgs) },
			{ list__length(FunctorArgs, Arity) },
			{ ConsId = cons(FunctorName, Arity) }
		;
			% float, int or string constant
			% 	- any errors will be caught by typechecking
			{ MaybeFunctor = error(_, _) },
			{ list__length(Args, Arity) },
			{ make_functor_cons_id(F, Arity, ConsId) },
			{ FunctorArgs = Args }
		),
		( { FunctorArgs = [] } ->
			{ create_atomic_unification(X, functor(ConsId, []),
				Context, MainContext, SubContext, Goal) },
			{ VarSet = VarSet0 },
			{ Info = Info0 }
		;
			{ make_fresh_arg_vars(FunctorArgs, VarSet0,
				HeadVars, VarSet1) },
			{ create_atomic_unification(X,
				functor(ConsId, HeadVars), Context,
				MainContext, SubContext, Goal0) },
			{ ArgContext = functor(ConsId,
				MainContext, SubContext) },
			append_arg_unifications(HeadVars, FunctorArgs,
				FunctorContext, ArgContext, Goal0,
				VarSet1, Goal, VarSet, Info0, Info)
		)
	).

	% Handle `f(...) = X' in the same way as `X = f(...)'.

unravel_unification(term__functor(F, As, FC), term__variable(Y),
		C, MC, SC, VarSet0, Goal, VarSet, Info0, Info) -->
	unravel_unification(term__variable(Y), term__functor(F, As, FC),
		C, MC, SC, VarSet0, Goal, VarSet, Info0, Info).

	% If we find a unification of the form `f1(...) = f2(...)',
	% then we replace it with `Tmp = f1(...), Tmp = f2(...)',
	% and then process it according to the rule above.
	% Note that we can't simplify it yet, because we might simplify
	% away type errors.

unravel_unification(term__functor(LeftF, LeftAs, LeftC),
			term__functor(RightF, RightAs, RightC),
			Context, MainContext, SubContext, VarSet0,
			Goal, VarSet, Info0, Info) -->
	{ varset__new_var(VarSet0, TmpVar, VarSet1) },
	unravel_unification(
		term__variable(TmpVar), term__functor(LeftF, LeftAs, LeftC),
		Context, MainContext, SubContext,
		VarSet1, Goal0, VarSet2, Info0, Info1),
	unravel_unification(
		term__variable(TmpVar), term__functor(RightF, RightAs, RightC),
		Context, MainContext, SubContext,
		VarSet2, Goal1, VarSet, Info1, Info),
	{ goal_info_init(GoalInfo) },
	{ goal_to_conj_list(Goal0, ConjList0) },
	{ goal_to_conj_list(Goal1, ConjList1) },
	{ list__append(ConjList0, ConjList1, ConjList) },
	{ conj_list_to_goal(ConjList, GoalInfo, Goal) }.

	% create the hlds__goal for a unification which cannot be
	% further simplified, filling in all the as yet
	% unknown slots with dummy values

create_atomic_unification(A, B, Context, UnifyMainContext, UnifySubContext,
                Goal) :-
	UMode = ((free - free) -> (free - free)),
	Mode = ((free -> free) - (free -> free)),
	UnifyInfo = complicated_unify(UMode, can_fail),
	UnifyC = unify_context(UnifyMainContext, UnifySubContext),
	goal_info_init(GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo),
	Goal = unify(A, B, Mode, UnifyInfo, UnifyC) - GoalInfo.

%-----------------------------------------------------------------------------%

	% Process an explicit type qualification.
:- pred process_type_qualification(var, type, varset, term__context,
		qual_info, qual_info, io__state, io__state).
:- mode process_type_qualification(in, in, in, in, in, out, di, uo) is det.

process_type_qualification(Var, Type0, VarSet, Context, Info0, Info) -->
	{ Info0 = qual_info(EqvMap, TVarSet0, TVarRenaming0, Index0,
				VarTypes0, PredId, MQInfo0) },

	module_qual__qualify_type(Type0, Type1, Context, MQInfo0, MQInfo),
	{
	% Find any new type variables introduced by this type, and
	% add them to the var-name index and the variable renaming.
	term__vars(Type1, TVars),
	get_new_tvars(TVars, VarSet, TVarSet0, TVarSet1,
		Index0, Index, TVarRenaming0, TVarRenaming),
			
	% Apply the updated renaming to convert type variables in
	% the clause to type variables in the tvarset.
	term__apply_variable_renaming(Type1, TVarRenaming, Type2),

	% Expand equivalence types.
	equiv_type__replace_in_type(Type2, TVarSet1, EqvMap, Type, TVarSet)
	},
	update_var_types(VarTypes0, Var, Type, Context, VarTypes),	
	{ Info = qual_info(EqvMap, TVarSet, TVarRenaming,
			Index, VarTypes, PredId, MQInfo) }.

:- pred update_var_types(map(var, type), var, type, term__context,
			map(var, type), io__state, io__state).
:- mode update_var_types(in, in, in, in, out, di, uo) is det.

update_var_types(VarTypes0, Var, Type, Context, VarTypes) -->
	( { map__search(VarTypes0, Var, Type0) } ->
		( { Type = Type0 } ->
			{ VarTypes = VarTypes0 }
		;
			prog_out__write_context(Context),
			io__write_string("Error: explicit type qualification does\n"),
			prog_out__write_context(Context),
			io__write_string("  not match prior qualification.\n"),
			io__set_exit_status(1),
			{ VarTypes = VarTypes0 }
		)
	;
		{ map__set(VarTypes0, Var, Type, VarTypes) }
	).

	% Add new type variables for those introduced by a type qualification.
:- pred get_new_tvars(list(var), varset, tvarset, tvarset,
	map(string, var), map(string, var), map(var, var), map(var, var)).
:- mode get_new_tvars(in, in, in, out, in, out, in, out) is det.

get_new_tvars([], _, T, T, I, I, R, R).
get_new_tvars([TVar | TVars], VarSet, TVarSet0, TVarSet,
		Index0, Index, TVarRenaming0, TVarRenaming) :-
	( map__contains(TVarRenaming0, TVar) ->
		TVarRenaming1 = TVarRenaming0,
		TVarSet2 = TVarSet0,
		Index1 = Index0
	;
		varset__lookup_name(VarSet, TVar, TVarName),
		( map__search(Index0, TVarName, TVarSetVar) ->
			map__det_insert(TVarRenaming0, TVar, TVarSetVar,
						TVarRenaming1),
			TVarSet2 = TVarSet0,
			Index1 = Index0
		;
			varset__new_var(TVarSet0, NewTVar, TVarSet1),
			varset__name_var(TVarSet1, NewTVar,
					TVarName, TVarSet2),
			map__det_insert(Index0, TVarName, NewTVar, Index1),
			map__det_insert(TVarRenaming0, TVar, NewTVar,
					TVarRenaming1)
		)
	),
	get_new_tvars(TVars, VarSet, TVarSet2, TVarSet,
		 Index1, Index, TVarRenaming1, TVarRenaming).
			
%-----------------------------------------------------------------------------%

% substitute_vars(Vars0, Subst, Vars)
%	apply substitiution `Subst' (which must only rename vars) to `Vars0',
%	and return the result in `Vars'.

:- pred substitute_vars(list(var), substitution, list(var)).
:- mode substitute_vars(in, in, out) is det.

substitute_vars([], _, []).
substitute_vars([Var0 | Vars0], Subst, [Var | Vars]) :-
	term__apply_substitution(term__variable(Var0), Subst, Term),
	( Term = term__variable(Var1) ->
		Var = Var1
	;
		error("substitute_vars: invalid substitution")
	),
	substitute_vars(Vars0, Subst, Vars).

%-----------------------------------------------------------------------------%

% get_conj(Goal, Conj0, Subst, Conj) :
% 	Goal is a tree of conjuncts.  Flatten it into a list (applying Subst),
%	append Conj0, and return the result in Conj.

:- pred get_conj(goal, substitution, list(hlds__goal), varset,
	list(hlds__goal), varset, qual_info, qual_info, io__state, io__state).
:- mode get_conj(in, in, in, in, out, out, in, out, di, uo) is det.

get_conj(Goal, Subst, Conj0, VarSet0, Conj, VarSet, Info0, Info) -->
	(
		{ Goal = (A,B) - _Context }
	->
		get_conj(B, Subst, Conj0, VarSet0, Conj1, VarSet1,
						Info0, Info1),
		get_conj(A, Subst, Conj1, VarSet1, Conj, VarSet, Info1, Info)
	;
		transform_goal(Goal, VarSet0, Subst, Goal1, VarSet,
						Info0, Info),
		{ goal_to_conj_list(Goal1, ConjList) },
		{ list__append(ConjList, Conj0, Conj) }
	).

% get_disj(Goal, Subst, Disj0, Disj) :
% 	Goal is a tree of disjuncts.  Flatten it into a list (applying Subst)
%	append Disj0, and return the result in Disj.

:- pred get_disj(goal, substitution, list(hlds__goal), varset,
	list(hlds__goal), varset, qual_info, qual_info, io__state, io__state).
:- mode get_disj(in, in, in, in, out, out, in, out, di, uo) is det.

get_disj(Goal, Subst, Disj0, VarSet0, Disj, VarSet, Info0, Info) -->
	(
		{ Goal = (A;B) - _Context }
	->
		get_disj(B, Subst, Disj0, VarSet0, Disj1, VarSet1,
							Info0, Info1),
		get_disj(A, Subst, Disj1, VarSet1, Disj, VarSet, Info1, Info)
	;
		transform_goal(Goal, VarSet0, Subst, Goal1, VarSet,
							Info0, Info),
		{ Disj = [Goal1 | Disj0] }
	).

%-----------------------------------------------------------------------------%

	% Information used to process explicit type qualifications.
:- type qual_info
	--->	qual_info(
			eqv_map,	% Used to expand equivalence types. 
			tvarset,	% All type variables for predicate.
			map(var, var),	% Map from clause type variable to
					% actual type variable in tvarset.
			map(string, var),
				% Type variables in tvarset indexed by name.
			map(var, type), % Var types
			pred_id,	% Last pred processed.
			mq_info		% Module qualification info.
		).

:- pred init_qual_info(mq_info, eqv_map, qual_info).
:- mode init_qual_info(in, in, out) is det.

init_qual_info(MQInfo, EqvMap, QualInfo) :-
	varset__init(TVarSet),
	map__init(Renaming),
	map__init(Index),
	map__init(VarTypes),
	invalid_pred_id(PredId),
	QualInfo = qual_info(EqvMap, TVarSet, Renaming,
			Index, VarTypes, PredId, MQInfo).

	% Update the qual_info when processing a new clause.
:- pred update_qual_info(qual_info, tvarset, map(var, type),
				pred_id, qual_info).
:- mode update_qual_info(in, in, in, in, out) is det.

update_qual_info(QualInfo0, TVarSet, VarTypes, PredId, QualInfo) :-
	QualInfo0 = qual_info(EqvMap, TVarSet0, _Renaming0, Index0,
					VarTypes0, PredId0, MQInfo),
	( PredId = PredId0 ->
		% The renaming for one clause is useless in the others.
		map__init(Renaming),
		QualInfo = qual_info(EqvMap, TVarSet0, Renaming,
				Index0, VarTypes0, PredId0, MQInfo)
	;
		varset__create_name_var_map(TVarSet, Index),
		map__init(Renaming),
		QualInfo = qual_info(EqvMap, TVarSet, Renaming,
				Index, VarTypes, PredId, MQInfo)
	).


	% All the other items are needed all at once in one or two places,
	% so access predicates for them would be a waste of time.

:- pred qual_info_get_mq_info(qual_info, mq_info).
:- mode qual_info_get_mq_info(in, out) is det.

qual_info_get_mq_info(qual_info(_,_,_,_,_,_,MQInfo), MQInfo).

:- pred qual_info_set_mq_info(qual_info, mq_info, qual_info).
:- mode qual_info_set_mq_info(in, in, out) is det.

qual_info_set_mq_info(qual_info(A,B,C,D,E,F,_), MQInfo,
			qual_info(A,B,C,D,E,F, MQInfo)).

:- pred qual_info_get_var_types(qual_info, map(var, type)).
:- mode qual_info_get_var_types(in, out) is det.

qual_info_get_var_types(qual_info(_,_,_,_,VarTypes,_,_), VarTypes).


%-----------------------------------------------------------------------------%

	% Predicates to write out the different warning and error messages.

:- pred multiple_def_error(sym_name, int, string, term__context, term__context,
				io__state, io__state).
:- mode multiple_def_error(in, in, in, in, in, di, uo) is det.

multiple_def_error(Name, Arity, DefType, Context, OrigContext) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("' multiply defined.\n"),
	prog_out__write_context(OrigContext),
	io__write_string(
		"  Here is the previous definition of "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'.\n").

:- pred undefined_pred_or_func_error(sym_name, int, term__context, string,
				io__state, io__state).
:- mode undefined_pred_or_func_error(in, in, in, in, di, uo) is det.

undefined_pred_or_func_error(Name, Arity, Context, Description) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: "),
	io__write_string(Description),
	io__write_string(" for "),
	hlds_out__write_pred_call_id(Name/Arity),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  without preceding `pred' or `func' declaration\n").

:- pred undefined_mode_error(sym_name, int, term__context, string,
				io__state, io__state).
:- mode undefined_mode_error(in, in, in, in, di, uo) is det.

undefined_mode_error(Name, Arity, Context, Description) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: "),
	io__write_string(Description),
	io__write_string(" for `"),
	prog_out__write_context(Context),
	io__write_string("`"),
	hlds_out__write_pred_call_id(Name/Arity),
	io__write_string("' specifies non-existent mode.\n").

:- pred maybe_undefined_pred_error(sym_name, int, pred_or_func, term__context,
				string, io__state, io__state).
:- mode maybe_undefined_pred_error(in, in, in, in, in, di, uo) is det.

% This is not considered an unconditional error anymore:
% if there is no :- pred declaration, we just infer one,
% unless the `--no-infer-types' option was specified.

maybe_undefined_pred_error(Name, Arity, PredOrFunc, Context, Description) -->
	globals__io_lookup_bool_option(infer_types, InferTypes),
	( { InferTypes = yes } ->
		[]
	;
		io__set_exit_status(1),
		prog_out__write_context(Context),
		io__write_string("Error: "),
		io__write_string(Description),
		io__write_string(" for "),
		hlds_out__write_call_id(PredOrFunc, Name/Arity),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("  without preceding `"),
		{ hlds_out__pred_or_func_to_str(PredOrFunc, DeclString) },
		io__write_string(DeclString),
		io__write_string("' declaration.\n")
	).

:- pred unspecified_det_for_local(sym_name, arity, pred_or_func, term__context, 
				io__state, io__state).
:- mode unspecified_det_for_local(in, in, in, in, di, uo) is det.

unspecified_det_for_local(Name, Arity, PredOrFunc, Context) -->
	prog_out__write_context(Context),
	report_warning("Error: no determinism declaration for local\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_call_id(PredOrFunc, Name/Arity),
	io__write_string(".\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		prog_out__write_context(Context),
		io__write_string("  (This is an error because you specified the `--no-infer-det'"),
		prog_out__write_context(Context),
		io__write_string("  option.  Use the `--infer-det' option if you want the"),
		prog_out__write_context(Context),
		io__write_string("  compiler to automatically infer the determinism of"),
		prog_out__write_context(Context),
		io__write_string("  local predicates.)")
	;
		[]
	).

:- pred unspecified_det_for_exported(sym_name, arity, pred_or_func,
			term__context, io__state, io__state).
:- mode unspecified_det_for_exported(in, in, in, in, di, uo) is det.

unspecified_det_for_exported(Name, Arity, PredOrFunc, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: no determinism declaration for exported\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_call_id(PredOrFunc, Name/Arity),
	io__write_string(".\n").

:- pred clause_for_imported_pred_error(sym_name, arity, pred_or_func,
				term__context, io__state, io__state).
:- mode clause_for_imported_pred_error(in, in, in, in, di, uo) is det.

clause_for_imported_pred_error(Name, Arity, PredOrFunc, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: clause for imported "),
	hlds_out__write_call_id(PredOrFunc, Name/Arity),
	io__write_string(".\n").

:- pred unqualified_pred_error(sym_name, int, term__context,
				io__state, io__state).
:- mode unqualified_pred_error(in, in, in, di, uo) is det.

unqualified_pred_error(PredName, Arity, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Internal error: an unqualified predicate name `"),
	prog_out__write_sym_name(PredName),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'.\n"),
	prog_out__write_context(Context),
	io__write_string("  should have been qualified by prog_io.m.\n").

%-----------------------------------------------------------------------------%
%	module_add_pragma_fact_table(PredName, Arity, FileName, 
%		Status, Context, Module0, Module, Info0, Info)
% Add a `pragma fact_table' declaration to the HLDS.  This predicate calls the 
% fact table compiler (fact_table_compile_facts) to create a separate `.o' file
% for the fact_table and then creates separate pieces of `pragma c_code' to 
% access the table in each mode of the fact table predicate.

:- pred module_add_pragma_fact_table(sym_name, arity, string, 
		import_status, term__context, module_info, module_info,
		qual_info, qual_info, io__state, io__state).
:- mode module_add_pragma_fact_table(in, in, in, in, in, in, out, in, out,
		di, uo) is det.

module_add_pragma_fact_table(Pred, Arity, FileName, Status, Context,
		Module0, Module, Info0, Info) -->
	{ module_info_get_predicate_table(Module0, PredicateTable) },
	(
	    { predicate_table_search_sym_arity(PredicateTable, Pred, 
		    Arity, PredIDs0) },
	    { PredIDs0 = [PredID | PredIDs1] }
	->
	    (
		{ PredIDs1 = [] }, 		% only one predicate found
		{ module_info_pred_info(Module0, PredID, PredInfo0) },

		    % compile the fact table into a separate .o file
		fact_table_compile_facts(Pred, Arity, FileName, 
			PredInfo0, PredInfo, Context),

		{module_info_set_pred_info(Module0, PredID, PredInfo, Module1)},
		{ pred_info_procedures(PredInfo, ProcTable) },
		{ pred_info_procids(PredInfo, ProcIDs) },
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		{
		    PredOrFunc = predicate,
		    NumArgs = Arity
		;
		    PredOrFunc =  function,
		    NumArgs is Arity + 1
		},

		    % create some pragma c_code to access table in each mode
		module_add_fact_table_procedures(ProcIDs, ProcTable, Pred,
		    PredOrFunc, NumArgs, Status, Context, 
		    Module1, Module, Info0, Info)
	    ;
	    	{ PredIDs1 = [_ | _] },		% >1 predicate found
	    	io__set_exit_status(1),
	    	prog_out__write_context(Context),
		io__write_string("In pragma fact_table for `"),
		hlds_out__write_pred_call_id(Pred/Arity),
		io__write_string("':\n"),
		prog_out__write_context(Context),
		io__write_string(
			"  error: ambiguous predicate/function name.\n"),
		{ Module = Module0 },
		{ Info = Info0 }
	    )
	;
	    undefined_pred_or_func_error(Pred, Arity, Context, 
	    	"pragma fact_table"),
	    { Module = Module0 },
	    { Info = Info0 }
	).


	% Add a `pragma c_code' for each mode of the fact table lookup to the
	% HLDS.
	% `pragma fact_table's are represented in the HLDS by a 
	% `pragma c_code' for each mode of the predicate.

:- pred module_add_fact_table_procedures(list(proc_id), proc_table, sym_name, 
		pred_or_func, arity, import_status, term__context, module_info, 
		module_info, qual_info, qual_info, io__state, io__state).
:- mode module_add_fact_table_procedures(in, in, in, in, in, in, in, in, out,
		in, out, di, uo) is det.

module_add_fact_table_procedures([],_,_,_,_,_,_, Mod, Mod, Inf, Inf) --> [].
module_add_fact_table_procedures([ProcID | ProcIDs], ProcTable, SymName, 
		PredOrFunc, Arity, Status, Context, 
		Module0, Module, Info0, Info) -->
	module_add_fact_table_proc(ProcID, ProcTable, SymName, PredOrFunc, 
			Arity, Status, Context, Module0, Module1, Info0, Info1),
	module_add_fact_table_procedures(ProcIDs, ProcTable, SymName, 
		PredOrFunc, Arity, Status, Context, 
		Module1, Module, Info1, Info).

:- pred module_add_fact_table_proc(proc_id, proc_table, sym_name, pred_or_func, 
		arity, import_status, term__context, module_info, module_info, 
		qual_info, qual_info, io__state, io__state).
:- mode module_add_fact_table_proc(in, in, in, in, in, in, in, in, out,
		in, out, di, uo) is det.

module_add_fact_table_proc(ProcID, ProcTable, SymName, PredOrFunc, Arity, 
		Status, Context, Module0, Module, Info0, Info) -->
	{ map__lookup(ProcTable, ProcID, ProcInfo) },
	{ varset__init(VarSet0) },
	{ varset__new_vars(VarSet0, Arity, Vars, VarSet) },
	{ proc_info_argmodes(ProcInfo, Modes) },
	{ fact_table_pragma_vars(Vars, Modes, VarSet, PragmaVars) },
	{ fact_table_generate_c_code(SymName, PragmaVars, C_Code) },
	module_add_pragma_c_code(non_recursive, SymName, PredOrFunc, PragmaVars,
		VarSet, C_Code, Status, Context, Module0, Module, Info0, Info).

	% Create a list(pragma_var) that looks like the ones that are created
	% for pragma c_code in prog_io.m.
	% This is required by module_add_pragma_c_code to add the C code for
	% the procedure to the HLDS.

:- pred fact_table_pragma_vars(list(var), list(mode), varset, list(pragma_var)).
:- mode fact_table_pragma_vars(in, in, in, out) is det.

fact_table_pragma_vars(Vars0, Modes0, VarSet, PragmaVars0) :-
	(
		Vars0 = [Var | Vars1],
		Modes0 = [Mode | Modes1]
	->
		varset__lookup_name(VarSet, Var, Name),
		PragmaVar = pragma_var(Var, Name, Mode),
		fact_table_pragma_vars(Vars1, Modes1, VarSet, PragmaVars1),
		PragmaVars0 = [PragmaVar | PragmaVars1]
	;
		PragmaVars0 = []
	).

%-----------------------------------------------------------------------------%

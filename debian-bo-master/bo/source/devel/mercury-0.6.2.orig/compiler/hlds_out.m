%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% hlds_out.m

% Main authors: conway, fjh.

% There is quite a bit of overlap between the following modules:
%
%	hlds_out.m
%	mercury_to_mercury.m
%	term_io.m
%
% mercury_to_mercury.m prints the parse tree data structure defined
% in prog_data.m.  hlds_out.m does a similar task, but for the data
% structure defined in hlds.m.  term_io.m prints terms.

% There are two different ways of printing variables.
% One way uses the names Var', Var'', etc. which are generated
% by the compiler.  The other way converts all names back into
% a format allowed as source code.  Currently this module calls
% mercury_to_mercury.m, which uses the second method, rather
% than term_io.m, which uses the first method.  We should
% think about using an option to specify which method is used.

%-----------------------------------------------------------------------------%

:- module hlds_out.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal, hlds_data.
:- import_module prog_data, llds.
:- import_module io.

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_type_id(type_id, io__state, io__state).
:- mode hlds_out__write_type_id(in, di, uo) is det.

:- pred hlds_out__write_cons_id(cons_id, io__state, io__state).
:- mode hlds_out__write_cons_id(in, di, uo) is det.

:- pred hlds_out__cons_id_to_string(cons_id, string).
:- mode hlds_out__cons_id_to_string(in, out) is det.

:- pred hlds_out__write_pred_id(module_info, pred_id, io__state, io__state).
:- mode hlds_out__write_pred_id(in, in, di, uo) is det.

:- pred hlds_out__write_pred_proc_id(module_info, pred_id, proc_id,
				io__state, io__state).
:- mode hlds_out__write_pred_proc_id(in, in, in, di, uo) is det.

:- pred hlds_out__write_call_id(pred_or_func, pred_call_id,
				io__state, io__state).
:- mode hlds_out__write_call_id(in, in, di, uo) is det.

:- pred hlds_out__write_pred_call_id(pred_call_id, io__state, io__state).
:- mode hlds_out__write_pred_call_id(in, di, uo) is det.

:- pred hlds_out__write_pred_or_func(pred_or_func, io__state, io__state).
:- mode hlds_out__write_pred_or_func(in, di, uo) is det.

:- pred hlds_out__pred_or_func_to_str(pred_or_func, string).
:- mode hlds_out__pred_or_func_to_str(in, out) is det.

	% hlds_out__write_unify_context/5 writes out a message such as
	%	foo.m:123:   in argument 3 of functor `foo/5':
	% 	foo.m:123:   in unification of `X' and `blah':
	% based on the unify_context and term__context.
	%
:- pred hlds_out__write_unify_context(unify_context, term__context,
				io__state, io__state).
:- mode hlds_out__write_unify_context(in, in, di, uo) is det.

	% hlds_out__write_unify_context_first/6 is the
	% same as above, except that it also takes and returns a bool
	% which specifies whether this is the start of a sentence.
	% If the first argument is `yes', then
	% it means this is the first line of an error message, so
	% the message starts with a capital letter, e.g.
	%	foo.m:123:   In argument 3 of functor `foo/5':
	% 	foo.m:123:   in unification of `X' and `blah':
	% The bool returned as the fourth argument will be `no' unless nothing
	% was printed out, in which case it will be the same as the first arg.
	%
:- pred hlds_out__write_unify_context(bool, unify_context, term__context,
						bool, io__state, io__state).
:- mode hlds_out__write_unify_context(in, in, in, out, di, uo) is det.

:- pred hlds_out__write_determinism(determinism, io__state, io__state).
:- mode hlds_out__write_determinism(in, di, uo) is det.

:- pred hlds_out__write_can_fail(can_fail, io__state, io__state).
:- mode hlds_out__write_can_fail(in, di, uo) is det.

:- pred hlds_out__write_code_model(code_model, io__state, io__state).
:- mode hlds_out__write_code_model(in, di, uo) is det.

:- pred hlds_out__write_import_status(import_status, io__state, io__state).
:- mode hlds_out__write_import_status(in, di, uo) is det.

%-----------------------------------------------------------------------------%

	% print out an entire hlds structure.

:- pred hlds_out__write_hlds(int, module_info, io__state, io__state).
:- mode hlds_out__write_hlds(in, in, di, uo) is det.

:- pred hlds_out__write_clauses(int, module_info, pred_id, varset, list(var),
		pred_or_func, list(clause), vartypes, io__state, io__state).
:- mode hlds_out__write_clauses(in, in, in, in, in, in, in, in, di, uo) is det.

	% print out an hlds goal.

:- pred hlds_out__write_goal(hlds__goal, module_info, varset, int, string,
				io__state, io__state).
:- mode hlds_out__write_goal(in, in, in, in, in, di, uo) is det.

	% print out a functor and its arguments

:- pred hlds_out__write_functor(const, list(var), varset, io__state, io__state).
:- mode hlds_out__write_functor(in, in, in, di, uo) is det.

	% print out a cons_id and arguments

:- pred hlds_out__write_functor_cons_id(cons_id, list(var), varset,
				io__state, io__state).
:- mode hlds_out__write_functor_cons_id(in, in, in, di, uo) is det.

	% print out the right-hand-side of a unification

:- pred hlds_out__write_unify_rhs(unify_rhs, module_info, varset, int,
			io__state, io__state).
:- mode hlds_out__write_unify_rhs(in, in, in, in, di, uo) is det.

	% print out a list of variables their corresponding modes
	% (e.g. for a lambda expressions)

:- pred hlds_out__write_var_modes(list(var), list(mode), varset,
					io__state, io__state).
:- mode hlds_out__write_var_modes(in, in, in, di, uo) is det.

	% print out the name of a marker

:- pred hlds_out__write_marker(marker, io__state, io__state).
:- mode hlds_out__write_marker(in, di, uo) is det.

:- type vartypes --->
		yes(tvarset, map(var, type))
	;	no.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_to_mercury, globals, options.
:- import_module llds_out, prog_out, prog_util, instmap.

:- import_module bool, int, string, list, set, map, std_util, assoc_list.
:- import_module term, term_io, varset, require, getopt.

hlds_out__write_type_id(Name - Arity) -->
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity).

hlds_out__cons_id_to_string(cons(SymName, Arity), String) :-
	string__int_to_string(Arity, ArityString),
	(
		SymName = unqualified(Name),	
		string__append_list(["'", Name, "'/", ArityString], String)
	;
		SymName = qualified(Module, Name),
		string__append_list(["'", Module, ":",
			Name, "'/", ArityString], String)
	).
hlds_out__cons_id_to_string(int_const(Int), String) :-
	string__int_to_string(Int, String).
hlds_out__cons_id_to_string(string_const(String), S) :-
	string__append_list(["""", String, """"], S).
hlds_out__cons_id_to_string(float_const(_), "<float>").
hlds_out__cons_id_to_string(pred_const(_, _), "<pred>").
hlds_out__cons_id_to_string(code_addr_const(_, _), "<code_addr>").
hlds_out__cons_id_to_string(base_type_info_const(_, _, _), "<base_type_info>").

hlds_out__write_cons_id(cons(SymName, Arity)) -->
	(
		{ SymName = qualified(Module, Name) },
		io__write_string(Module),
		io__write_string(":")
	;
		{ SymName = unqualified(Name) }
	),
	io__write_string(Name),
	io__write_string("/"),
	io__write_int(Arity).
hlds_out__write_cons_id(int_const(Int)) -->
	io__write_int(Int).
hlds_out__write_cons_id(string_const(String)) -->
	io__write_char('"'),
	io__write_string(String),
	io__write_char('"').
hlds_out__write_cons_id(float_const(Float)) -->
	io__write_float(Float).
hlds_out__write_cons_id(pred_const(_PredId, _ProcId)) -->
	io__write_string("<pred>").
hlds_out__write_cons_id(code_addr_const(_PredId, _ProcId)) -->
	io__write_string("<code_addr>").
hlds_out__write_cons_id(base_type_info_const(_, _, _)) -->
	io__write_string("<base_type_info>").

hlds_out__write_pred_id(ModuleInfo, PredId) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_module(PredInfo, Module) },
	{ pred_info_name(PredInfo, Name) },
	{ pred_info_arity(PredInfo, Arity) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" `"),
	io__write_string(Module),
	io__write_string(":"),
	( { string__append("__", _, Name) } ->
		{ pred_info_arg_types(PredInfo, TVarSet, ArgTypes) },
		{ term__context_init(Context) },
		mercury_output_term(term__functor(term__atom(Name),
				ArgTypes, Context), TVarSet)
	;
		{ PredOrFunc = function ->
			OrigArity is Arity - 1
		;
			OrigArity = Arity
		},
		io__write_string(Name),
		io__write_string("/"),
		io__write_int(OrigArity)
	),
	io__write_string("'").

hlds_out__write_pred_proc_id(ModuleInfo, PredId, ProcId) -->
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(" mode "),
	{ ModeNum is ProcId mod 10000 },
	io__write_int(ModeNum).

hlds_out__write_call_id(PredOrFunc, Name/Arity) -->
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" `"),
	{ PredOrFunc = function ->
		OrigArity is Arity - 1
	;
		OrigArity = Arity
	},
	hlds_out__write_pred_call_id(Name/OrigArity),
	io__write_string("'").

hlds_out__write_pred_call_id(Name / Arity) -->
	( { Name = qualified(ModuleName, _) } ->
		io__write_string(ModuleName),
		io__write_char(':')
	;
		[]
	),
	{ unqualify_name(Name, PredName) },
	io__write_string(PredName),
	io__write_char('/'),
	io__write_int(Arity).

hlds_out__write_pred_or_func(predicate) -->
	io__write_string("predicate").
hlds_out__write_pred_or_func(function) -->
	io__write_string("function").

hlds_out__pred_or_func_to_str(predicate, "pred").
hlds_out__pred_or_func_to_str(function, "func").

%-----------------------------------------------------------------------------%

hlds_out__write_unify_context(UnifyContext, Context) -->
	hlds_out__write_unify_context(no, UnifyContext, Context, _).

hlds_out__write_unify_context(First0,
		unify_context(MainContext, RevSubContexts), Context, First) -->
	hlds_out__write_unify_main_context(First0, MainContext, Context,
			First1),
	{ list__reverse(RevSubContexts, SubContexts) },
	hlds_out__write_unify_sub_contexts(First1, SubContexts, Context, First).

:- pred hlds_out__write_unify_main_context(bool, unify_main_context,
		term__context, bool, io__state, io__state).
:- mode hlds_out__write_unify_main_context(in, in, in, out, di, uo) is det.

hlds_out__write_unify_main_context(First, explicit, _, First) -->
	[].
hlds_out__write_unify_main_context(First, head(ArgNum), Context, no) -->
	hlds_out__write_in_argument(First, ArgNum, Context),
	io__write_string(" of clause head:\n").
hlds_out__write_unify_main_context(First, call(PredId, ArgNum), Context, no) -->
	hlds_out__write_in_argument(First, ArgNum, Context),
	io__write_string(" of call to predicate `"),
	hlds_out__write_pred_call_id(PredId),
	io__write_string("':\n").

:- pred hlds_out__write_unify_sub_contexts(bool, unify_sub_contexts,
				term__context, bool, io__state, io__state).
:- mode hlds_out__write_unify_sub_contexts(in, in, in, out, di, uo) is det.

hlds_out__write_unify_sub_contexts(First, [], _, First) --> [].
hlds_out__write_unify_sub_contexts(First0, [ConsId - ArgNum | SubContexts],
		Context, First) -->
	hlds_out__write_in_argument(First0, ArgNum, Context),
	io__write_string(" of functor `"),
	hlds_out__write_cons_id(ConsId),
	io__write_string("':\n"),
	hlds_out__write_unify_sub_contexts(no, SubContexts, Context, First).

:- pred hlds_out__write_in_argument(bool, int, term__context,
					io__state, io__state).
:- mode hlds_out__write_in_argument(in, in, in, di, uo) is det.

hlds_out__write_in_argument(First, ArgNum, Context) -->
	prog_out__write_context(Context),
	( { First = yes } ->
		io__write_string("  In argument ")
	;
		io__write_string("  in argument ")
	),
	io__write_int(ArgNum).

%-----------------------------------------------------------------------------%

hlds_out__write_hlds(Indent, Module) -->
	{
		module_info_preds(Module, PredTable),
		module_info_types(Module, TypeTable),
		module_info_insts(Module, InstTable),
		module_info_modes(Module, ModeTable)
	},

	hlds_out__write_header(Indent, Module),
	io__write_string("\n"),
	hlds_out__write_types(Indent, TypeTable),
	io__write_string("\n"),
	hlds_out__write_insts(Indent, InstTable),
	io__write_string("\n"),
	hlds_out__write_modes(Indent, ModeTable),
	io__write_string("\n"),
	hlds_out__write_preds(Indent, Module, PredTable),
	io__write_string("\n"),
	hlds_out__write_footer(Indent, Module).

:- pred hlds_out__write_header(int, module_info, io__state, io__state).
:- mode hlds_out__write_header(in, in, di, uo) is det.

hlds_out__write_header(Indent, Module) -->
	{ module_info_name(Module, Name) },
	hlds_out__write_indent(Indent),
	io__write_string(":- module "),
	io__write_string(Name),
	io__write_string(".\n").

:- pred hlds_out__write_footer(int, module_info, io__state, io__state).
:- mode hlds_out__write_footer(in, in, di, uo) is det.

hlds_out__write_footer(Indent, Module) -->
	{ module_info_name(Module, Name) },
	hlds_out__write_indent(Indent),
	io__write_string(":- end_module "),
	io__write_string(Name),
	io__write_string(".\n").

:- pred hlds_out__write_preds(int, module_info, pred_table,
				io__state, io__state).
:- mode hlds_out__write_preds(in, in, in, di, uo) is det.

hlds_out__write_preds(Indent, ModuleInfo, PredTable) -->
	io__write_string("%-------- Predicates --------\n\n"),
	hlds_out__write_indent(Indent),
	{ map__keys(PredTable, PredIds) },
	hlds_out__write_preds_2(Indent, ModuleInfo, PredIds, PredTable).

:- pred hlds_out__write_preds_2(int, module_info, list(pred_id), pred_table,
			io__state, io__state).
:- mode hlds_out__write_preds_2(in, in, in, in, di, uo) is det.

hlds_out__write_preds_2(Indent, ModuleInfo, PredIds0, PredTable) -->
	(
		{ PredIds0 = [PredId|PredIds] }
	->
		{ map__lookup(PredTable, PredId, PredInfo) },
		( { pred_info_is_imported(PredInfo) } ->
			[]
		;
			% for pseudo-imported predicates (i.e. unification
			% preds), only print them if we are using a local
			% mode for them
			{ pred_info_is_pseudo_imported(PredInfo) },
			{ pred_info_procids(PredInfo, ProcIds) },
			{ ProcIds \= [0] }
		->
			[]
		;
			hlds_out__write_pred(Indent, ModuleInfo, PredId,
				PredInfo)
		),
		hlds_out__write_preds_2(Indent, ModuleInfo, PredIds, PredTable)
	;
		[]
	).

:- pred hlds_out__write_pred(int, module_info, pred_id, pred_info,
				io__state, io__state).
:- mode hlds_out__write_pred(in, in, in, in, di, uo) is det.

hlds_out__write_pred(Indent, ModuleInfo, PredId, PredInfo) -->
	{ pred_info_arg_types(PredInfo, TVarSet, ArgTypes) },
	{ pred_info_clauses_info(PredInfo, ClausesInfo) },
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ pred_info_context(PredInfo, Context) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_import_status(PredInfo, ImportStatus) },
	{ pred_info_get_marker_list(PredInfo, Markers) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	mercury_output_pred_type(TVarSet, unqualified(PredName), ArgTypes,
		no, Context),
	{ ClausesInfo = clauses_info(VarSet, _, VarTypes, HeadVars, Clauses) },
	hlds_out__write_indent(Indent),
	io__write_string("% pred id: "),
	io__write_int(PredId),
	io__write_string(", category: "),
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(", status: "),
	hlds_out__write_import_status(ImportStatus),
	io__write_string("\n"),
	( { Markers = [] } ->
		[]
	;
		io__write_string("% markers:"),
		hlds_out__write_marker_list(Markers),
		io__write_string("\n")
	),
	hlds_out__write_var_types(Indent, VarSet, VarTypes, TVarSet),

		% Never write the clauses out verbosely -
		% Temporarily disable the verbose_dump_hlds option
	globals__io_lookup_bool_option(verbose_dump_hlds, Verbose),
	globals__io_set_option(verbose_dump_hlds, bool(no)),
	hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet,
			HeadVars, PredOrFunc, Clauses, no),
		% Re-enable it
	globals__io_set_option(verbose_dump_hlds, bool(Verbose)),

	hlds_out__write_procs(Indent, ModuleInfo, PredId, ImportStatus,
		ProcTable),
	io__write_string("\n").

:- pred hlds_out__write_marker_list(list(marker_status), io__state, io__state).
:- mode hlds_out__write_marker_list(in, di, uo) is det.

hlds_out__write_marker_list([]) --> [].
hlds_out__write_marker_list([Marker | Markers]) -->
	hlds_out__write_marker_status(Marker),
	hlds_out__write_marker_list(Markers).

:- pred hlds_out__write_marker_status(marker_status, io__state, io__state).
:- mode hlds_out__write_marker_status(in, di, uo) is det.

hlds_out__write_marker_status(request(Marker)) -->
	io__write_string(" request("),
	hlds_out__write_marker(Marker),
	io__write_string(")").
hlds_out__write_marker_status(done(Marker)) -->
	io__write_string(" done("),
	hlds_out__write_marker(Marker),
	io__write_string(")").

hlds_out__write_marker(infer_type) -->
	io__write_string("infer_type").
hlds_out__write_marker(infer_modes) -->
	io__write_string("infer_modes").
hlds_out__write_marker(inline) -->
	io__write_string("inline").
hlds_out__write_marker(dnf) -->
	io__write_string("dnf").
hlds_out__write_marker(magic) -->
	io__write_string("magic").
hlds_out__write_marker(obsolete) -->
	io__write_string("obsolete").
hlds_out__write_marker(memo) -->
	io__write_string("memo").

hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet, HeadVars,
		PredOrFunc, Clauses0, TypeQual) -->
	(
		{ Clauses0 = [Clause|Clauses] }
	->
		hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet,
			HeadVars, PredOrFunc, Clause, TypeQual),
		hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet,
			HeadVars, PredOrFunc, Clauses, TypeQual)
	;
		[]
	).

:- pred hlds_out__write_clause(int, module_info, pred_id, varset, list(var),
			pred_or_func, clause, vartypes, io__state, io__state).
:- mode hlds_out__write_clause(in, in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet,
		HeadVars, PredOrFunc, Clause, TypeQual) -->
	{
		Clause = clause(
			Modes,
			Goal,
			_Context
		),
		Indent1 is Indent + 1
	},
	globals__io_lookup_bool_option(verbose_dump_hlds, Verbose),
	( { Verbose = yes } ->
		hlds_out__write_indent(Indent),
		io__write_string("% Modes for which this clause applies: "),
		hlds_out__write_intlist(Modes),
		io__write_string("\n")
	;
		[]
	),
	hlds_out__write_clause_head(ModuleInfo, PredId, VarSet,
					HeadVars, PredOrFunc),
	( { Goal = conj([]) - _GoalInfo } ->
		io__write_string(".\n")
	;
		io__write_string(" :-\n"),
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet,
					Indent1, ".", TypeQual)
	).

:- pred hlds_out__write_intlist(list(int), io__state, io__state).
:- mode hlds_out__write_intlist(in, di, uo) is det.

hlds_out__write_intlist(IntList) -->
	(
		{ IntList = [] }
	->
		io__write_string("[]")
	;
		io__write_string("[ "),
		hlds_out__write_intlist_2(IntList),
		io__write_string("]")
	).

:- pred hlds_out__write_intlist_2(list(int), io__state, io__state).
:- mode hlds_out__write_intlist_2(in, di, uo) is det.

hlds_out__write_intlist_2(Ns0) -->
	(
		{ Ns0 = [N] }
	->
		io__write_int(N)
	;
		{ Ns0 = [N|Ns] }
	->
		io__write_int(N),
		io__write_string(", "),
		hlds_out__write_intlist_2(Ns)
	;
		{ error("This should be unreachable.") }
	).

:- pred hlds_out__write_clause_head(module_info, pred_id, varset, list(var),
					pred_or_func, io__state, io__state).
:- mode hlds_out__write_clause_head(in, in, in, in, in, di, uo) is det.

hlds_out__write_clause_head(ModuleInfo, PredId, VarSet,
			HeadVars, PredOrFunc) -->
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	(
		{ PredOrFunc = function },
		{ pred_args_to_func_args(HeadVars, FuncArgs, RetVal) },
		hlds_out__write_qualified_functor(ModuleName,
			term__atom(PredName), FuncArgs, VarSet),
		io__write_string(" = "),
		mercury_output_term(term__variable(RetVal), VarSet)
	;
		{ PredOrFunc = predicate },
		hlds_out__write_qualified_functor(ModuleName,
			term__atom(PredName), HeadVars, VarSet)
	).

hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent, Follow) -->
		% don't type qualify everything
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, Indent, Follow, no).

	% TypeQual is yes(TVarset, VarTypes) if all constructors should
	% be module qualified.
:- pred hlds_out__write_goal_a(hlds__goal, module_info, varset, int, string,
				vartypes, io__state, io__state).
:- mode hlds_out__write_goal_a(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_goal_a(Goal - GoalInfo, ModuleInfo,
			VarSet, Indent, Follow, TypeQual) -->
	globals__io_lookup_bool_option(verbose_dump_hlds, Verbose),
	( { Verbose = yes } ->
%		{ goal_info_get_context(GoalInfo, Context) },
%		{ term__context_file(Context, FileName) },
%		{ term__context_line(Context, LineNumber) },
%		( { FileName \= "" } ->
%			hlds_out__write_indent(Indent),
%			io__write_string("% context: file `"),
%			io__write_string(FileName),
%			io__write_string("', line "),
%			io__write_int(LineNumber),
%			io__write_string("\n")
%		;
%			[]
%		),
		{ goal_info_get_nonlocals(GoalInfo, NonLocalsSet) },
		{ set__to_sorted_list(NonLocalsSet, NonLocalsList) },
		( { NonLocalsList \= [] } ->
			hlds_out__write_indent(Indent),
			io__write_string("% nonlocals: "),
			mercury_output_vars(NonLocalsList, VarSet),
			io__write_string("\n")
		;
			[]
		),
		{ goal_info_get_pre_births(GoalInfo, PreBirths) },
		{ set__to_sorted_list(PreBirths, PreBirthList) },
		( { PreBirthList \= [] } ->
			hlds_out__write_indent(Indent),
			io__write_string("% pre-births: "),
			mercury_output_vars(PreBirthList, VarSet),
			io__write_string("\n")
		;
			[]
		),
		{ goal_info_get_pre_deaths(GoalInfo, PreDeaths) },
		{ set__to_sorted_list(PreDeaths, PreDeathList) },
		( { PreDeathList \= [] } ->
			hlds_out__write_indent(Indent),
			io__write_string("% pre-deaths: "),
			mercury_output_vars(PreDeathList, VarSet),
			io__write_string("\n")
		;
			[]
		),
		{ goal_info_get_follow_vars(GoalInfo, MaybeFollowVars) },
		(
			{ MaybeFollowVars = yes(FollowVars) }
		->
			{ map__to_assoc_list(FollowVars, FVlist) },
			hlds_out__write_indent(Indent),
			io__write_string("% follow vars:\n"),
			hlds_out__write_var_to_lvals(FVlist,
				VarSet, Indent)
		;
			[]
		),
		hlds_out__write_indent(Indent),
		io__write_string("% determinism: "),
		{ goal_info_get_determinism(GoalInfo, Determinism) },
		hlds_out__write_determinism(Determinism),
		io__write_string("\n")
	;
		[]
	),
	hlds_out__write_goal_2(Goal, ModuleInfo, VarSet, Indent, Follow,
		TypeQual),
	( { Verbose = yes } ->
		{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
		( { instmap_delta_changed_vars(InstMapDelta, Vars),
				set__empty(Vars) } ->
			[]
		;
			hlds_out__write_indent(Indent),
			io__write_string("% new insts: "),
			hlds_out__write_instmap_delta(InstMapDelta, VarSet,
				Indent),
			io__write_string("\n")
		),
		{ goal_info_get_post_births(GoalInfo, PostBirths) },
		{ set__to_sorted_list(PostBirths, PostBirthList) },
		( { PostBirthList \= [] } ->
			hlds_out__write_indent(Indent),
			io__write_string("% post-births: "),
			mercury_output_vars(PostBirthList, VarSet),
			io__write_string("\n")
		;
			[]
		),
		{ goal_info_get_post_deaths(GoalInfo, PostDeaths) },
		{ set__to_sorted_list(PostDeaths, PostDeathList) },
		( { PostDeathList \= [] } ->
			hlds_out__write_indent(Indent),
			io__write_string("% post-deaths: "),
			mercury_output_vars(PostDeathList, VarSet),
			io__write_string("\n")
		;
			[]
		),
		{ goal_info_get_resume_point(GoalInfo, Resume) },
		(
			{ Resume = no_resume_point }
		;
			{ Resume = resume_point(ResumeVars, Locs) },
			{ set__to_sorted_list(ResumeVars, ResumeVarList) },
			hlds_out__write_indent(Indent),
			io__write_string("% resume point "),
			(
				{ Locs = orig_only },
				io__write_string("orig only ")
			;
				{ Locs = stack_only },
				io__write_string("stack only ")
			;
				{ Locs = orig_and_stack },
				io__write_string("orig and stack ")
			;
				{ Locs = stack_and_orig },
				io__write_string("stack and orig ")
			),
			mercury_output_vars(ResumeVarList, VarSet),
			io__write_string("\n")
		),
		(
			( { Goal = disj(_, SM) }
			; { Goal = switch(_, _, _, SM) }
			; { Goal = if_then_else(_, _, _, _, SM) }
			)
		->
			{ map__to_assoc_list(SM, SMlist) },
			hlds_out__write_indent(Indent),
			io__write_string("% store map:\n"),
			hlds_out__write_var_to_lvals(SMlist,
				VarSet, Indent)
		;
			[]
		)
	;
		[]
	).

:- pred hlds_out__write_goal_2(hlds__goal_expr, module_info, varset,
			int, string, vartypes, io__state, io__state).
:- mode hlds_out__write_goal_2(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_goal_2(switch(Var, CanFail, CasesList, _), ModuleInfo, VarSet,
		Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("( % "),
	hlds_out__write_can_fail(CanFail),
	io__write_string(" switch on `"),
	mercury_output_var(Var, VarSet),
	io__write_string("'\n"),
	{ Indent1 is Indent + 1 },
	( { CasesList = [Case | Cases] } ->
		hlds_out__write_case(Case, Var, ModuleInfo,
					VarSet, Indent1, TypeQual),
		hlds_out__write_cases(Cases, Var, ModuleInfo,
					VarSet, Indent, TypeQual)
	;
		hlds_out__write_indent(Indent1),
		io__write_string("fail\n")
	),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(some(Vars, Goal), ModuleInfo,
		VarSet, Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("some ["),
	mercury_output_vars(Vars, VarSet),
	io__write_string("] (\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, Indent1, "", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(if_then_else(Vars, Cond, Then, Else, _), ModuleInfo,
		VarSet, Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("(if"),
	hlds_out__write_some(Vars, VarSet),
	io__write_string("\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_goal_a(Cond, ModuleInfo, VarSet, Indent1, "", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string("then\n"),
	hlds_out__write_goal_a(Then, ModuleInfo, VarSet, Indent1, "", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string("else\n"),
	globals__io_lookup_bool_option(verbose_dump_hlds, Verbose),
	(
		{ Verbose = no },
		{ Else = if_then_else(_, _, _, _, _) - _ }
	->
		hlds_out__write_goal_a(Else, ModuleInfo, VarSet, Indent, "",
			TypeQual)
	;
		hlds_out__write_goal_a(Else, ModuleInfo, VarSet, Indent1, "",
			TypeQual)
	),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(not(Goal), ModuleInfo, VarSet, Indent, Follow,
		TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("\\+ (\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, Indent1, "", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(conj(List), ModuleInfo, VarSet, Indent, Follow,
		TypeQual) -->
	( { List = [Goal | Goals] } ->
		globals__io_lookup_bool_option(verbose_dump_hlds, Verbose),
		( { Verbose = yes } ->
			{ Indent1 is Indent + 1 },
			hlds_out__write_indent(Indent),
			io__write_string("( % conjunction\n"),
			hlds_out__write_conj(Goal, Goals, ModuleInfo, VarSet,
				Indent1, "", Verbose, TypeQual),
			hlds_out__write_indent(Indent),
			io__write_string(")"),
			io__write_string(Follow),
			io__write_string("\n")
		;
			hlds_out__write_conj(Goal, Goals, ModuleInfo, VarSet,
				Indent, Follow, Verbose, TypeQual)
		)
	;
		hlds_out__write_indent(Indent),
		io__write_string("true"),
		io__write_string(Follow),
		io__write_string("\n")
	).

hlds_out__write_goal_2(disj(List, _), ModuleInfo, VarSet, Indent, Follow,
		TypeQual) -->
	hlds_out__write_indent(Indent),
	( { List = [Goal | Goals] } ->
		io__write_string("( % disjunction\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet,
			Indent1, "", TypeQual),
		hlds_out__write_disj(Goals, ModuleInfo, VarSet,
			Indent, TypeQual),
		hlds_out__write_indent(Indent),
		io__write_string(")"),
		io__write_string(Follow),
		io__write_string("\n")
	;
		io__write_string("fail"),
		io__write_string(Follow),
		io__write_string("\n")
	).

hlds_out__write_goal_2(higher_order_call(PredVar, ArgVars, _, _, _),
			_ModuleInfo, VarSet, Indent, Follow, _TypeQual) -->
		% XXX we should print more info here
	hlds_out__write_indent(Indent),
	hlds_out__write_functor(term__atom("call"), [PredVar|ArgVars], VarSet),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(call(_PredId, _ProcId, ArgVars, Builtin, _, PredName),
			_ModuleInfo, VarSet, Indent, Follow, _TypeQual) -->
		% XXX we should print more info here
	globals__io_lookup_bool_option(verbose_dump_hlds, Verbose),
	(
		{ Verbose = yes },
		{ hlds__is_builtin_is_internal(Builtin) }
	->
		hlds_out__write_indent(Indent),
		io__write_string("% internal\n")
	;
		[]
	),
	(
		{ Verbose = yes },
		{ hlds__is_builtin_is_inline(Builtin) }
	->
		hlds_out__write_indent(Indent),
		io__write_string("% inline\n")
	;
		[]
	),
	hlds_out__write_indent(Indent),
	(
		{ PredName = qualified(ModuleName, Name) },
		hlds_out__write_qualified_functor(ModuleName, term__atom(Name),
						ArgVars, VarSet)
	;
		{ PredName = unqualified(Name) },
		hlds_out__write_functor(term__atom(Name), ArgVars, VarSet)
	),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(unify(A, B, _, Unification, _), ModuleInfo, VarSet,
		Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	mercury_output_var(A, VarSet),
	io__write_string(" = "),
	{ TypeQual = yes(_, VarTypes) ->
		map__lookup(VarTypes, A, UniType),
		VarType = yes(UniType)
	;
		VarType = no
	},
	hlds_out__write_unify_rhs_2(B, ModuleInfo, VarSet,
				Indent, Follow, VarType, TypeQual),
	globals__io_lookup_bool_option(verbose_dump_hlds, Verbose),
	(
		{ Verbose = no }
	->
		[]
	;
		% don't output bogus info if we haven't been through
		% mode analysis yet
		{ Unification = complicated_unify(Mode, CanFail) },
		{ CanFail = can_fail },
		{ Mode = (free - free -> free - free) }
	->
		[]
	;
		hlds_out__write_unification(Unification, ModuleInfo, VarSet,
			Indent)
	).

hlds_out__write_goal_2(pragma_c_code(C_Code, _, _, _, _, ArgNames), _, _,
		Indent, Follow, _) -->
	hlds_out__write_indent(Indent),
	io__write_string("$pragma(c_code, ["),
	{ get_pragma_c_var_names(ArgNames, Names) },
	hlds_out__write_string_list(Names),
	io__write_string("], """),
	io__write_string(C_Code),
	io__write_string(""" )"),
	io__write_string(Follow),
	io__write_string("\n").

:- pred hlds_out__write_string_list(list(string), io__state, io__state).
:- mode hlds_out__write_string_list(in, di, uo) is det.

hlds_out__write_string_list([]) --> [].
hlds_out__write_string_list([Name]) -->
	io__write_string(Name).
hlds_out__write_string_list([Name1, Name2 | Names]) -->
	io__write_string(Name1),
	io__write_string(", "),
	hlds_out__write_string_list([Name2 | Names]).

:- pred hlds_out__write_unification(unification, module_info, varset, int,
					io__state, io__state).
:- mode hlds_out__write_unification(in, in, in, in, di, uo) is det.

hlds_out__write_unification(assign(X, Y), _ModuleInfo, VarSet, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(X, VarSet),
	io__write_string(" := "),
	mercury_output_var(Y, VarSet),
	io__write_string("\n").
hlds_out__write_unification(simple_test(X, Y), _ModuleInfo, VarSet, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(X, VarSet),
	io__write_string(" == "),
	mercury_output_var(Y, VarSet),
	io__write_string("\n").
hlds_out__write_unification(construct(Var, ConsId, ArgVars, ArgModes),
		ModuleInfo, VarSet, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, VarSet),
	io__write_string(" := "),
	mercury_output_functor(ConsId, ArgVars, ArgModes, ModuleInfo, VarSet,
			Indent).
hlds_out__write_unification(deconstruct(Var, ConsId, ArgVars, ArgModes,
		CanFail), ModuleInfo, VarSet, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, VarSet),
	( { CanFail = can_fail },
		io__write_string(" ?= ")
	; { CanFail = cannot_fail },
		io__write_string(" => ")
	),
	!,
	mercury_output_functor(ConsId, ArgVars, ArgModes, ModuleInfo, VarSet,
			Indent).
hlds_out__write_unification(complicated_unify(Mode, CanFail),
		_ModuleInfo, VarSet, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	( { CanFail = can_fail },
		io__write_string("can_fail, ")
	; { CanFail = cannot_fail },
		io__write_string("cannot_fail, ")
	),
	!,
	io__write_string("mode: "),
	mercury_output_uni_mode(Mode, VarSet),
	io__write_string("\n").

:- pred mercury_output_functor(cons_id, list(var), list(uni_mode),
			module_info, varset, int, io__state, io__state).
:- mode mercury_output_functor(in, in, in, in, in, in, di, uo) is det.

mercury_output_functor(ConsId, ArgVars, ArgModes, _ModuleInfo, VarSet,
		Indent) -->
	hlds_out__write_cons_id(ConsId),
	( { ArgVars = [] } ->
		[]
	;
		io__write_string(" ("),
		mercury_output_vars(ArgVars, VarSet),
		io__write_string(")\n"),
		hlds_out__write_indent(Indent),
		io__write_string("% arg-modes "),
		mercury_output_uni_mode_list(ArgModes, VarSet)
	),
	io__write_string("\n").

hlds_out__write_unify_rhs(Rhs, ModuleInfo, VarSet, Indent) -->
	hlds_out__write_unify_rhs_3(Rhs, ModuleInfo, VarSet, Indent, no, no).

:- pred hlds_out__write_unify_rhs_2(unify_rhs, module_info, varset,
		int, string, maybe(type), vartypes, io__state, io__state).
:- mode hlds_out__write_unify_rhs_2(in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_unify_rhs_2(Rhs, ModuleInfo, VarSet, Indent, Follow,
		MaybeType, TypeQual) -->
	hlds_out__write_unify_rhs_3(Rhs, ModuleInfo, VarSet, Indent,
		MaybeType, TypeQual),
	io__write_string(Follow),
	io__write_string("\n").

:- pred hlds_out__write_unify_rhs_3(unify_rhs, module_info, varset,
		int, maybe(type), vartypes, io__state, io__state).
:- mode hlds_out__write_unify_rhs_3(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_unify_rhs_3(var(Var), _, VarSet, _, _, _) -->
	mercury_output_var(Var, VarSet).
hlds_out__write_unify_rhs_3(functor(ConsId, ArgVars), _,
			VarSet, _, MaybeType, TypeQual) -->
	hlds_out__write_functor_cons_id(ConsId, ArgVars, VarSet),
	( { MaybeType = yes(Type), TypeQual = yes(TVarSet, _) } ->
		io__write_string(" TYPE_QUAL_OP "),
		mercury_output_term(Type, TVarSet)
	;
		[]
	).
hlds_out__write_unify_rhs_3(lambda_goal(PredOrFunc, Vars, Modes, Det, Goal),
		ModuleInfo, VarSet, Indent, MaybeType, TypeQual) -->
	(
		{ PredOrFunc = predicate },
		io__write_string("(pred("),
		hlds_out__write_var_modes(Vars, Modes, VarSet),
		io__write_string(") is "),
		mercury_output_det(Det),
		io__write_string(" :-\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet,
					Indent1, "", TypeQual),
		hlds_out__write_indent(Indent),
		io__write_string(")")
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(Modes, ArgModes, RetMode) },
		{ pred_args_to_func_args(Vars, ArgVars, RetVar) },
		io__write_string("(func("),
		hlds_out__write_var_modes(ArgVars, ArgModes, VarSet),
		io__write_string(") = ("),
		hlds_out__write_var_mode(RetVar, RetMode, VarSet),
		io__write_string(") is "),
		mercury_output_det(Det),
		io__write_string(" :-\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet,
					Indent1, "", TypeQual),
		hlds_out__write_indent(Indent),
		io__write_string(")")
	),
	( { MaybeType = yes(Type), TypeQual = yes(TVarSet, _) } ->
		io__write_string(" TYPE_QUAL_OP "),
		mercury_output_term(Type, TVarSet)
	;
		[]
	).

hlds_out__write_functor(Functor, ArgVars, VarSet) -->
	{ term__context_init(Context) },
	{ term__var_list_to_term_list(ArgVars, ArgTerms) },
	{ Term = term__functor(Functor, ArgTerms, Context) },
	mercury_output_term(Term, VarSet).

:- pred hlds_out__write_qualified_functor(string, const, list(var), varset,
				io__state, io__state).
:- mode hlds_out__write_qualified_functor(in, in, in, in, di, uo) is det.

hlds_out__write_qualified_functor(ModuleName, Functor, ArgVars, VarSet) -->
	io__write_string(ModuleName),
	io__write_string(":"),
	hlds_out__write_functor(Functor, ArgVars, VarSet).

hlds_out__write_functor_cons_id(ConsId, ArgVars, VarSet) -->
	(
		{ ConsId = cons(SymName, _) },
		(
			{ SymName = qualified(Module, Name) },
			hlds_out__write_qualified_functor(Module,
				term__atom(Name), ArgVars, VarSet)
		;
			{ SymName = unqualified(Name) },
			hlds_out__write_functor(term__atom(Name),
				ArgVars, VarSet)
		)
	;
		{ ConsId = int_const(Int) },
		hlds_out__write_functor(term__integer(Int), ArgVars, VarSet)
	;
		{ ConsId = float_const(Float) },
		hlds_out__write_functor(term__float(Float), ArgVars, VarSet)
	;
		{ ConsId = string_const(Str) },
		hlds_out__write_functor(term__string(Str), ArgVars, VarSet)
	;
		{ ConsId = pred_const(_, _) },
		{ error("hlds_out__write_functor_cons_id: pred_const") }
	;
		{ ConsId = code_addr_const(_, _) },
		{ error("hlds_out__write_functor_cons_id: code_addr_const") }
	;
		{ ConsId = base_type_info_const(Module, Name, Arity) },
		io__write_string("base_type_info("""),
		io__write_string(Module),
		io__write_string(""", """),
		io__write_string(Name),
		io__write_string(""", "),
		io__write_int(Arity),
		io__write_string(")")
	).

hlds_out__write_var_modes([], [], _) --> [].
hlds_out__write_var_modes([Var|Vars], [Mode|Modes], VarSet) -->
	hlds_out__write_var_mode(Var, Mode, VarSet),
	( { Vars \= [] } ->
		io__write_string(", ")
	;
		[]
	),
	hlds_out__write_var_modes(Vars, Modes, VarSet).
hlds_out__write_var_modes([], [_|_], _) -->
	{ error("hlds_out__write_var_modes: length mis-match") }.
hlds_out__write_var_modes([_|_], [], _) -->
	{ error("hlds_out__write_var_modes: length mis-match") }.

:- pred hlds_out__write_var_mode(var, mode, varset, io__state, io__state).
:- mode hlds_out__write_var_mode(in, in, in, di, uo) is det.

hlds_out__write_var_mode(Var, Mode, VarSet) -->
	mercury_output_var(Var, VarSet),
	io__write_string("::"),
	mercury_output_mode(Mode, VarSet).

:- pred hlds_out__write_conj(hlds__goal, list(hlds__goal), module_info, varset,
		int, string, bool, vartypes, io__state, io__state).
:- mode hlds_out__write_conj(in, in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_conj(Goal1, Goals1, ModuleInfo, VarSet, Indent, Follow,
		Verbose, TypeQual) -->
	(
		{ Goals1 = [Goal2 | Goals2] }
	->
		( { Verbose = yes } ->
			% when generating verbose dumps,
			% we want the comma on its own line,
			% since that way it visually separates
			% the lines after one goal
			% and the lines before the next
			hlds_out__write_goal_a(Goal1, ModuleInfo, VarSet,
						Indent, "", TypeQual),
			hlds_out__write_indent(Indent),
			io__write_string(",\n")
		;
			hlds_out__write_goal_a(Goal1, ModuleInfo, VarSet,
						Indent, ",", TypeQual)
		),
		hlds_out__write_conj(Goal2, Goals2, ModuleInfo, VarSet,
					Indent, Follow, Verbose, TypeQual)
	;
		hlds_out__write_goal_a(Goal1, ModuleInfo, VarSet,
					Indent, Follow, TypeQual)
	).

:- pred hlds_out__write_disj(list(hlds__goal), module_info, varset, int,
				vartypes, io__state, io__state).
:- mode hlds_out__write_disj(in, in, in, in, in, di, uo) is det.

hlds_out__write_disj(GoalList, ModuleInfo, VarSet, Indent, TypeQual) -->
	(
		{ GoalList = [Goal | Goals] }
	->
		hlds_out__write_indent(Indent),
		io__write_string(";\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet,
					Indent1, "", TypeQual),
		hlds_out__write_disj(Goals, ModuleInfo, VarSet,
					Indent, TypeQual)
	;
		[]
	).

:- pred hlds_out__write_case(case, var, module_info, varset, int,
				vartypes, io__state, io__state).
:- mode hlds_out__write_case(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_case(case(ConsId, Goal), Var, ModuleInfo,
			VarSet, Indent, VarTypes) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, VarSet),
	io__write_string(" has functor "),
	hlds_out__write_cons_id(ConsId),
	io__write_string("\n"),
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, Indent, "", VarTypes).

:- pred hlds_out__write_cases(list(case), var, module_info, varset, int,
				vartypes, io__state, io__state).
:- mode hlds_out__write_cases(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_cases(CasesList, Var, ModuleInfo, VarSet, Indent, VarTypes) -->
	(
		{ CasesList = [Case | Cases] }
	->
		hlds_out__write_indent(Indent),
		io__write_string(";\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_case(Case, Var, ModuleInfo,
				VarSet, Indent1, VarTypes),
		hlds_out__write_cases(Cases, Var, ModuleInfo,
				VarSet, Indent, VarTypes)
	;
		[]
	).

:- pred hlds_out__write_some(list(var), varset, io__state, io__state).
:- mode hlds_out__write_some(in, in, di, uo) is det.

	% quantification is all implicit by the time we get to the hlds.

hlds_out__write_some(_Vars, _VarSet) --> [].

:- pred hlds_out__write_builtin(is_builtin, io__state, io__state).
:- mode hlds_out__write_builtin(in, di, uo) is det.

hlds_out__write_builtin(Builtin) -->
	(
		{ hlds__is_builtin_is_inline(Builtin) }
	->
		io__write_string("is inline")
	;
		io__write_string("is not inline")
	).

:- pred hlds_out__write_instmap(instmap, varset, int, io__state, io__state).
:- mode hlds_out__write_instmap(in, in, in, di, uo) is det.

hlds_out__write_instmap(InstMap, VarSet, Indent) -->
	( { instmap__is_unreachable(InstMap) } ->
		io__write_string("unreachable")
	;
		{ instmap__to_assoc_list(InstMap, AssocList) },
		hlds_out__write_instmap_2(AssocList, VarSet, Indent)
	).

:- pred hlds_out__write_instmap_2(assoc_list(var, inst), varset, int,
					io__state, io__state).
:- mode hlds_out__write_instmap_2(in, in, in, di, uo) is det.

hlds_out__write_instmap_2([], _, _) --> [].
hlds_out__write_instmap_2([Var - Inst | Rest], VarSet, Indent) -->
	mercury_output_var(Var, VarSet),
	io__write_string(" -> "),
	{ varset__init(InstVarSet) },
	mercury_output_inst(Inst, InstVarSet),
	( { Rest = [] } ->
		[]
	;
		mercury_output_newline(Indent),
		io__write_string("%            "),
		hlds_out__write_instmap_2(Rest, VarSet, Indent)
	).

:- pred hlds_out__write_instmap_delta(instmap_delta, varset, int,
			io__state, io__state).
:- mode hlds_out__write_instmap_delta(in, in, in, di, uo) is det.

hlds_out__write_instmap_delta(InstMapDelta, VarSet, Indent) -->
	( { instmap_delta_is_unreachable(InstMapDelta) } ->
		io__write_string("unreachable")
	;
		{ instmap_delta_to_assoc_list(InstMapDelta, AssocList) },
		hlds_out__write_instmap_2(AssocList, VarSet, Indent)
	).

hlds_out__write_import_status(local) -->
	io__write_string("local").
hlds_out__write_import_status(exported) -->
	io__write_string("exported").
hlds_out__write_import_status(abstract_exported) -->
	io__write_string("abstract_exported").
hlds_out__write_import_status(pseudo_exported) -->
	io__write_string("pseudo_exported").
hlds_out__write_import_status(imported) -->
	io__write_string("imported").
hlds_out__write_import_status(abstract_imported) -->
	io__write_string("abstract_imported").
hlds_out__write_import_status(opt_imported) -->
	io__write_string("opt_imported").
hlds_out__write_import_status(opt_decl) -->
	io__write_string("opt_decl").
hlds_out__write_import_status(pseudo_imported) -->
	io__write_string("pseudo_imported").

:- pred hlds_out__write_var_types(int, varset, map(var, type), varset,
					io__state, io__state).
:- mode hlds_out__write_var_types(in, in, in, in, di, uo) is det.

hlds_out__write_var_types(Indent, VarSet, VarTypes, TVarSet) -->
	{ map__keys(VarTypes, Vars) },
	hlds_out__write_var_types_2(Vars, Indent, VarSet, VarTypes, TVarSet).

:- pred hlds_out__write_var_types_2(list(var), int, varset, map(var, type),
					varset, io__state, io__state).
:- mode hlds_out__write_var_types_2(in, in, in, in, in, di, uo) is det.

hlds_out__write_var_types_2([], _, _, _, _) --> [].
hlds_out__write_var_types_2([Var | Vars], Indent, VarSet, VarTypes, TypeVarSet)
		-->
	{ map__lookup(VarTypes, Var, Type) },
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, VarSet),
	io__write_string(" (number "),
	{ term__var_to_int(Var, VarNum) },
	io__write_int(VarNum),
	io__write_string(")"),
	io__write_string(" :: "),
	mercury_output_term(Type, TypeVarSet),
	io__write_string("\n"),
	hlds_out__write_var_types_2(Vars, Indent, VarSet, VarTypes, TypeVarSet).

:- pred hlds_out__write_stack_slots(int, stack_slots, varset,
					io__state, io__state).
:- mode hlds_out__write_stack_slots(in, in, in, di, uo) is det.

hlds_out__write_stack_slots(Indent, StackSlots, VarSet) -->
	{ map__to_assoc_list(StackSlots, VarSlotList) },
	hlds_out__write_var_to_lvals(VarSlotList, VarSet, Indent).

:- pred hlds_out__write_var_to_lvals(assoc_list(var, lval), varset, int,
						io__state, io__state).
:- mode hlds_out__write_var_to_lvals(in, in, in, di, uo) is det.

hlds_out__write_var_to_lvals([], _, _) --> [].
hlds_out__write_var_to_lvals([Var - Loc | VarLocs], VarSet, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("%\t"),
	mercury_output_var(Var, VarSet),
	io__write_string("\t-> "),
	{ llds_out__lval_to_string(Loc, LocStrPrime) ->
		LocStr = LocStrPrime
	;
		LocStr = "unknown location"
	},
	io__write_string(LocStr),
	io__write_string("\n"),
	hlds_out__write_var_to_lvals(VarLocs, VarSet, Indent).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_types(int, type_table, io__state, io__state).
:- mode hlds_out__write_types(in, in, di, uo) is det.

hlds_out__write_types(Indent, TypeTable) -->
	hlds_out__write_indent(Indent),
	io__write_string("%-------- Types --------\n"),
	{ map__to_assoc_list(TypeTable, TypeAL) },
	hlds_out__write_types_2(Indent, TypeAL).

:- pred hlds_out__write_types_2(int, assoc_list(type_id, hlds__type_defn),
			io__state, io__state).
:- mode hlds_out__write_types_2(in, in, di, uo) is det.

hlds_out__write_types_2(_Indent, []) --> [].
hlds_out__write_types_2(Indent, [TypeId - TypeDefn | Types]) -->
	{ hlds_data__get_type_defn_tvarset(TypeDefn, TVarSet) },
	{ hlds_data__get_type_defn_tparams(TypeDefn, TypeParams) },
	{ hlds_data__get_type_defn_body(TypeDefn, TypeBody) },
	{ hlds_data__get_type_defn_status(TypeDefn, Status) },
	{ hlds_data__get_type_defn_context(TypeDefn, Context) },

	% Write the context

	io__write_char('\n'),
	globals__io_lookup_bool_option(verbose_dump_hlds, Verbose),
	( { Verbose = yes } ->
		{ term__context_file(Context, FileName) },
		{ term__context_line(Context, LineNumber) },
		( { FileName \= "" } ->
			hlds_out__write_indent(Indent),
			io__write_string("% context: file `"),
			io__write_string(FileName),
			io__write_string("', line "),
			io__write_int(LineNumber),
			io__write_string(", status "),
			hlds_out__write_import_status(Status),
			io__write_char('\n')
		;
			[]
		)
	;
		[]
	),

	hlds_out__write_indent(Indent),
	io__write_string(":- type "),
	hlds_out__write_type_name(TypeId),
	hlds_out__write_type_params(TVarSet, TypeParams),
	{ Indent1 is Indent + 1 },
	hlds_out__write_type_body(Indent1, TVarSet, TypeBody),
	hlds_out__write_types_2(Indent, Types).

:- pred hlds_out__write_type_name(type_id, io__state, io__state).
:- mode hlds_out__write_type_name(in, di, uo) is det.

hlds_out__write_type_name(Name - _Arity) -->
	prog_out__write_sym_name(Name).

:- pred hlds_out__write_type_params(tvarset, list(type_param),
			io__state, io__state).
:- mode hlds_out__write_type_params(in, in, di, uo) is det.

hlds_out__write_type_params(_Tvarset, []) --> [].
hlds_out__write_type_params(Tvarset, [P]) -->
	io__write_string("("),
	term_io__write_term(Tvarset, P),
	io__write_string(")").
hlds_out__write_type_params(Tvarset, [P | Ps]) -->
	{ Ps = [_ | _] },
	io__write_string("("),
	term_io__write_term(Tvarset, P),
	hlds_out__write_type_params_2(Tvarset, Ps).

:- pred hlds_out__write_type_params_2(tvarset, list(type_param),
			io__state, io__state).
:- mode hlds_out__write_type_params_2(in, in, di, uo) is det.

hlds_out__write_type_params_2(_Tvarset, []) -->
	io__write_string(")").
hlds_out__write_type_params_2(Tvarset, [P | Ps]) -->
	io__write_string(", "),
	term_io__write_term(Tvarset, P),
	hlds_out__write_type_params_2(Tvarset, Ps).

:- pred hlds_out__write_type_body(int, tvarset, hlds__type_body,
				io__state, io__state).
:- mode hlds_out__write_type_body(in, in, in, di, uo) is det.

hlds_out__write_type_body(Indent, Tvarset, du_type(Ctors, _Tags, _Enum)) -->
	io__write_string(" --->\n"),
	hlds_out__write_constructors(Indent, Tvarset, Ctors).
hlds_out__write_type_body(_Indent, _Tvarset, uu_type(_)) -->
	{ error("hlds_out__write_type_body: undiscriminated union found") }.
hlds_out__write_type_body(_Indent, Tvarset, eqv_type(Type)) -->
	io__write_string(" == "),
	term_io__write_term(Tvarset, Type),
	io__write_string(".\n").
hlds_out__write_type_body(_Indent, _Tvarset, abstract_type) -->
	io__write_string(".\n").

:- pred hlds_out__write_constructors(int, tvarset, list(constructor),
				io__state, io__state).
:- mode hlds_out__write_constructors(in, in, in, di, uo) is det.

hlds_out__write_constructors(_Indent, _Tvarset, []) -->
	{ error("hlds_out__write_constructors: empty constructor list?") }.
hlds_out__write_constructors(Indent, Tvarset, [C]) -->
	hlds_out__write_indent(Indent),
	io__write_char('\t'),
	hlds_out__write_constructor(Tvarset, C),
	io__write_string(".\n").
hlds_out__write_constructors(Indent, Tvarset, [C | Cs]) -->
	{ Cs = [_ | _] },
	hlds_out__write_indent(Indent),
	io__write_char('\t'),
	hlds_out__write_constructor(Tvarset, C),
	io__write_string("\n"),
	hlds_out__write_constructors_2(Indent, Tvarset, Cs).

:- pred hlds_out__write_constructors_2(int, tvarset, list(constructor),
				io__state, io__state).
:- mode hlds_out__write_constructors_2(in, in, in, di, uo) is det.

hlds_out__write_constructors_2(_Indent, _Tvarset, []) --> [].
hlds_out__write_constructors_2(Indent, Tvarset, [C | Cs]) -->
	hlds_out__write_indent(Indent),
	io__write_string(";\t"),
	hlds_out__write_constructor(Tvarset, C),
	( { Cs = [] } ->
		io__write_string(".\n")
	;
		io__write_string("\n"),
		hlds_out__write_constructors_2(Indent, Tvarset, Cs)
	).

:- pred hlds_out__write_constructor(tvarset, constructor, io__state, io__state).
:- mode hlds_out__write_constructor(in, in, di, uo) is det.

hlds_out__write_constructor(Tvarset, Name - Args) -->
	prog_out__write_sym_name(Name),
	( { Args = [Arg | Rest] } ->
		io__write_char('('),
		mercury_output_ctor_arg(Tvarset, Arg),
		mercury_output_remaining_ctor_args(Tvarset, Rest),
		io__write_char(')')
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_insts(int, inst_table, io__state, io__state).
:- mode hlds_out__write_insts(in, in, di, uo) is det.

hlds_out__write_insts(Indent, _X) -->
	hlds_out__write_indent(Indent),
	io__write_string("%-------- Insts --------\n"),
	hlds_out__write_indent(Indent),
	io__write_string("%%% Not yet implemented, sorry\n").

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_modes(int, mode_table, io__state, io__state).
:- mode hlds_out__write_modes(in, in, di, uo) is det.

hlds_out__write_modes(Indent, _X) -->
	hlds_out__write_indent(Indent),
	io__write_string("%-------- Modes --------\n"),
	hlds_out__write_indent(Indent),
	io__write_string("%%% Not yet implemented, sorry\n").

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_mode_list(int, list(mode), io__state, io__state).
:- mode hlds_out__write_mode_list(in, in, di, uo) is det.

hlds_out__write_mode_list(Indent, _X) -->
	hlds_out__write_indent(Indent),
	% XXX
	io__write_string("\n").

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_procs(int, module_info, pred_id, import_status,
		proc_table, io__state, io__state).
:- mode hlds_out__write_procs(in, in, in, in, in, di, uo) is det.

hlds_out__write_procs(Indent, ModuleInfo, PredId, ImportStatus, ProcTable) -->
	{ map__keys(ProcTable, ProcIds) },
	hlds_out__write_procs_2(ProcIds, ModuleInfo, Indent, PredId,
		ImportStatus, ProcTable).

:- pred hlds_out__write_procs_2(list(proc_id), module_info, int, pred_id,
			import_status, proc_table, io__state, io__state).
:- mode hlds_out__write_procs_2(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_procs_2([], _ModuleInfo, _Indent, _PredId, _, _ProcTable) -->
	[].
hlds_out__write_procs_2([ProcId | ProcIds], ModuleInfo, Indent, PredId,
		ImportStatus, ProcTable) -->
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	hlds_out__write_proc(Indent, ModuleInfo, PredId, ProcId, ImportStatus,
		ProcInfo),
	hlds_out__write_procs_2(ProcIds, ModuleInfo, Indent, PredId,
		ImportStatus, ProcTable).

:- pred hlds_out__write_proc(int, module_info, pred_id, proc_id, import_status,
				proc_info, io__state, io__state).
:- mode hlds_out__write_proc(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_proc(Indent, ModuleInfo, PredId, ProcId, ImportStatus, Proc) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_typevarset(PredInfo, TVarSet) },
	{ proc_info_vartypes(Proc, VarTypes) },
	{ proc_info_declared_determinism(Proc, DeclaredDeterminism) },
	{ proc_info_inferred_determinism(Proc, InferredDeterminism) },
	{ proc_info_variables(Proc, VarSet) },
	{ proc_info_headvars(Proc, HeadVars) },
	{ proc_info_argmodes(Proc, HeadModes) },
	{ proc_info_goal(Proc, Goal) },
	{ proc_info_context(Proc, ModeContext) },
	{ Indent1 is Indent + 1 },

	hlds_out__write_indent(Indent1),
	io__write_string("% mode number "),
	io__write_int(ProcId),
	io__write_string(" of "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(" ("),
	hlds_out__write_determinism(InferredDeterminism),
	io__write_string("):\n"),

	hlds_out__write_var_types(Indent, VarSet, VarTypes, TVarSet),

	hlds_out__write_indent(Indent),
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ varset__init(ModeVarSet) },
	mercury_output_pred_mode_decl(ModeVarSet, unqualified(PredName),
			HeadModes, DeclaredDeterminism, ModeContext),

	( { ImportStatus = pseudo_imported, ProcId = 0 } ->
		[]
	;
		{ proc_info_stack_slots(Proc, StackSlots) },
		hlds_out__write_indent(Indent),
		hlds_out__write_stack_slots(Indent, StackSlots, VarSet),
		hlds_out__write_indent(Indent),
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		hlds_out__write_clause_head(ModuleInfo, PredId, VarSet,
						HeadVars, PredOrFunc),
		io__write_string(" :-\n"),
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent1, ".")
	).

% :- pred hlds_out__write_varnames(int, map(var, string), io__state, io__state).
% :- mode hlds_out__write_varnames(in, in, di, uo) is det.
% 
% hlds_out__write_varnames(Indent, VarNames) -->
% 	{ map__to_assoc_list(VarNames, VarNameList) },
% 	(
% 		{ VarNameList = [] }
% 	->
% 		hlds_out__write_indent(Indent),
% 		io__write_string("[]\n")
% 	;
% 		hlds_out__write_indent(Indent),
% 		io__write_string("[\n"),
% 		{Indent1 is Indent + 1},
% 		hlds_out__write_varnames_2(Indent1, VarNameList),
% 		hlds_out__write_indent(Indent),
% 		io__write_string("]\n")
% 	).
% 
% :- pred hlds_out__write_varnames_2(int, list(pair(var, string)),
% 							io__state, io__state).
% :- mode hlds_out__write_varnames_2(in, in, di, uo) is det.
% 
% hlds_out__write_varnames_2(Indent, VarNameList0) -->
% 	(
% 		{ VarNameList0 = [VarId - Name|VarNameList] }
% 	->
% 		{ Indent1 is Indent + 1 },
% 		hlds_out__write_indent(Indent1),
% 		{ term__var_to_int(VarId, VarNum) },
% 		io__write_int(VarNum),
% 		io__write_string(" - "),
% 		io__write_string(Name),
% 		io__write_string("\n"),
% 		( { VarNameList = [] } ->
% 			[]
% 		;
% 			io__write_string(",\n"),
% 			hlds_out__write_varnames_2(Indent, VarNameList)
% 		)
% 	;
% 		{ error("This cannot happen") }
% 	).

:- pred hlds_out__write_vartypes(int, map(var, type), io__state, io__state).
:- mode hlds_out__write_vartypes(in, in, di, uo) is det.

hlds_out__write_vartypes(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write(X),
	io__write_string("\n").

hlds_out__write_determinism(det) -->
	io__write_string("det").
hlds_out__write_determinism(semidet) -->
	io__write_string("semidet").
hlds_out__write_determinism(nondet) -->
	io__write_string("nondet").
hlds_out__write_determinism(multidet) -->
	io__write_string("multi").
hlds_out__write_determinism(cc_nondet) -->
	io__write_string("cc_nondet").
hlds_out__write_determinism(cc_multidet) -->
	io__write_string("cc_multi").
hlds_out__write_determinism(erroneous) -->
	io__write_string("erroneous").
hlds_out__write_determinism(failure) -->
	io__write_string("failure").

hlds_out__write_can_fail(can_fail) -->
	io__write_string("can_fail").
hlds_out__write_can_fail(cannot_fail) -->
	io__write_string("cannot_fail").

hlds_out__write_code_model(model_det) -->
	io__write_string("model_det").
hlds_out__write_code_model(model_semi) -->
	io__write_string("model_semi").
hlds_out__write_code_model(model_non) -->
	io__write_string("model_non").

:- pred hlds_out__write_indent(int, io__state, io__state).
:- mode hlds_out__write_indent(in, di, uo) is det.

hlds_out__write_indent(Indent) -->
	(
		{ Indent = 0 }
	->
		[]
	;
		io__write_char('\t'),
		{ Indent1 is Indent - 1 },
		hlds_out__write_indent(Indent1)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

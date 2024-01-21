%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mercury_compile.m.
% Main authors: fjh, zs.

% This is the top-level of the Mercury compiler.

% This module invokes the different passes of the compiler as appropriate.

%-----------------------------------------------------------------------------%

:- module mercury_compile.
:- interface.
:- import_module bool, string, io.

:- pred main(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

	% library modules
:- import_module int, list, map, set, std_util, dir, require.
:- import_module library, getopt, term, varset.

	% the main compiler passes (in order of execution)
:- import_module handle_options, prog_io, modules, module_qual, equiv_type.
:- import_module make_hlds, typecheck, modes.
:- import_module switch_detection, cse_detection, det_analysis, unique_modes.
:- import_module simplify, intermod, bytecode_gen, bytecode, (lambda).
:- import_module polymorphism, intermod, higher_order, inlining, common, dnf.
:- import_module constraint, unused_args, dead_proc_elim, excess, saved_vars.
:- import_module lco, liveness, stratify.
:- import_module follow_code, live_vars, arg_info, store_alloc.
:- import_module code_gen, optimize, export, base_type_info, base_type_layout.
:- import_module llds_common, llds_out.

	% miscellaneous compiler modules
:- import_module prog_data, hlds_module, hlds_pred, hlds_out, llds.
:- import_module mercury_to_c, mercury_to_mercury, mercury_to_goedel.
:- import_module dependency_graph, garbage_out, shapes.
:- import_module options, globals, passes_aux.


%-----------------------------------------------------------------------------%

main -->
	handle_options(MaybeError, Args, Link),
	main_2(MaybeError, Args, Link).

%-----------------------------------------------------------------------------%

:- pred main_2(maybe(string), list(string), bool, io__state, io__state).
:- mode main_2(in, in, in, di, uo) is det.

main_2(yes(ErrorMessage), _, _) -->
	usage_error(ErrorMessage).
main_2(no, Args, Link) -->
	globals__io_lookup_bool_option(help, Help),
	( { Help = yes } ->
		long_usage
	; { Args = [] } ->
		usage
	;
		{ strip_module_suffixes(Args, ModuleNames) },
		process_module_list(ModuleNames),
		io__get_exit_status(ExitStatus),
		( { ExitStatus = 0 } ->
			( { Link = yes } ->
				mercury_compile__link_module_list(ModuleNames)
			;
				[]
			)
		;
			% If we found some errors, but the user didn't enable
			% the `-E' (`--verbose-errors') option, give them a
			% hint about it.

			globals__io_lookup_bool_option(verbose_errors,
				VerboseErrors),
			( { VerboseErrors = no } ->
				io__write_string("For more information, try recompiling with `-E'.\n")
			;
				[]
			)
		)
	).

	% Process a list of module names.
	% Remove any `.m' extension before processing
	% the module name.

:- pred strip_module_suffixes(list(string), list(string)).
:- mode strip_module_suffixes(in, out) is det.

strip_module_suffixes([], []).
strip_module_suffixes([Module0 | Modules0], [Module | Modules]) :-
	(
		string__remove_suffix(Module0, ".m", Module1)
	->
		Module = Module1
	;
		Module = Module0
	),
	strip_module_suffixes(Modules0, Modules).

:- pred process_module_list(list(string), io__state, io__state).
:- mode process_module_list(in, di, uo) is det.

process_module_list([]) --> [].
process_module_list([Module | Modules]) -->
	process_module(Module),
	process_module_list(Modules).

	% Open the file and process it.

:- pred process_module(string, io__state, io__state).
:- mode process_module(in, di, uo) is det.

process_module(Module) -->
	 	% All messages go to stderr
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, _),

	globals__io_lookup_bool_option(generate_dependencies, GenerateDeps),
	( { GenerateDeps = yes } ->
		generate_dependencies(Module)
	;
		process_module_2(Module)
	).

:- pred process_module_2(string, io__state, io__state).
:- mode process_module_2(in, di, uo) is det.

process_module_2(ModuleName) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Parsing `"),
	maybe_write_string(Verbose, ModuleName),
	maybe_write_string(Verbose, ".m' and imported interfaces...\n"),
	io__gc_call(read_mod(ModuleName, ".m", "Reading module", yes,
			Items0, Error)),
	globals__io_lookup_bool_option(statistics, Stats),
	maybe_report_stats(Stats),

	globals__io_lookup_bool_option(halt_at_syntax_errors, HaltSyntax),
	globals__io_lookup_bool_option(make_interface, MakeInterface),
	globals__io_lookup_bool_option(make_short_interface,
						MakeShortInterface),
	globals__io_lookup_bool_option(convert_to_mercury, ConvertToMercury),
	globals__io_lookup_bool_option(convert_to_goedel, ConvertToGoedel),
	( { Error = fatal } ->
		[]
	; { Error = yes, HaltSyntax = yes } ->
		[]
	; { MakeInterface = yes } ->
		make_interface(ModuleName, Items0)
	; { MakeShortInterface = yes } ->
		make_short_interface(ModuleName, Items0)
	; { ConvertToMercury = yes } ->
		{ string__append(ModuleName, ".ugly", OutputFileName) },
		convert_to_mercury(ModuleName, OutputFileName, Items0)
	; { ConvertToGoedel = yes } ->
		convert_to_goedel(ModuleName, Items0)
	;
		grab_imported_modules(ModuleName, Items0, Module, FactDeps,
			Error2),
		( { Error2 \= fatal } ->
			mercury_compile(Module, FactDeps)
		;
			[]
		)
	).

%-----------------------------------------------------------------------------%

	% Given a fully expanded module (i.e. a module name and a list
	% of all the items in the module and any of its imports),
	% compile it.

	% Stage number assignments:
	%
	%	 1 to 25	front end pass
	%	26 to 50	middle pass
	%	51 to 99	back end pass
	%
	% The initial arrangement has the stage numbers increasing by three
	% so that new stages can be slotted in without too much trouble.

:- pred mercury_compile(module_imports, list(string), io__state, io__state).
:- mode mercury_compile(in, in, di, uo) is det.

mercury_compile(Module, FactDeps) -->
	{ Module = module_imports(ModuleName, _, _, _, _) },
	mercury_compile__pre_hlds_pass(Module, FactDeps, HLDS1, UndefTypes,
						UndefModes, Errors1),
	mercury_compile__frontend_pass(HLDS1, HLDS20, UndefTypes,
						UndefModes, Errors2),
	( { Errors1 = no }, { Errors2 = no } ->
	    globals__io_lookup_bool_option(verbose, Verbose),
	    globals__io_lookup_bool_option(statistics, Stats),
	    mercury_compile__maybe_write_dependency_graph(HLDS20,
		Verbose, Stats, HLDS21),
	    globals__io_lookup_bool_option(typecheck_only, TypeCheckOnly),
	    globals__io_lookup_bool_option(errorcheck_only, ErrorCheckOnly),
	    globals__io_lookup_bool_option(make_optimization_interface,
	    	MakeOptInt),
	    ( { TypeCheckOnly = yes } ->
		[]
	    ; { ErrorCheckOnly = yes } ->
		% we may still want to run `unused_args' so that we get
		% the appropriate warnings
		globals__io_set_option(optimize_unused_args, bool(no)),
		mercury_compile__maybe_unused_args(HLDS21, Verbose, Stats, _)
	    ; { MakeOptInt = yes } ->
		% only run up to typechecking when making the .opt file
	    	[]
	    ;
		mercury_compile__maybe_output_prof_call_graph(HLDS21,
			Verbose, Stats, HLDS25),
		mercury_compile__middle_pass(ModuleName, HLDS25, HLDS50),
		globals__io_lookup_bool_option(highlevel_c, HighLevelC),
		( { HighLevelC = yes } ->
			{ string__append(ModuleName, ".c", C_File) },
			mercury_compile__gen_hlds(C_File, HLDS50),
			globals__io_lookup_bool_option(compile_to_c,
				CompileToC),
			( { CompileToC = no } ->
				mercury_compile__single_c_to_obj(ModuleName,
					_CompileOK)
			;
				[]
			)
		;
			mercury_compile__backend_pass(HLDS50, HLDS70, LLDS),
			mercury_compile__output_pass(HLDS70, LLDS,
				ModuleName, _CompileErrors)
		)
	    )
	;
	    []
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__pre_hlds_pass(module_imports, list(string), 
		module_info, bool, bool, bool, io__state, io__state).
:- mode mercury_compile__pre_hlds_pass(in, in, out, out, out, out, 
		di, uo) is det.

mercury_compile__pre_hlds_pass(ModuleImports0, FactDeps, HLDS1, UndefTypes, 
		UndefModes, FoundError) -->
	globals__io_lookup_bool_option(statistics, Stats),
	globals__io_lookup_bool_option(verbose, Verbose),

	{ ModuleImports0 = module_imports(Module, LongDeps,
					ShortDeps, Items0, _) },
	write_dependency_file(Module, LongDeps, ShortDeps, FactDeps),
	mercury_compile__module_qualify_items(Items0, Items1, Module, Verbose,
					Stats, _, UndefTypes0, UndefModes0),
		% Items from optimization interfaces are needed before
		% equivalence types are expanded, but after module
		% qualification.
	{ ModuleImports1 = module_imports(Module, LongDeps, ShortDeps,
							Items1, no) },
		% Errors in .opt files result in software errors.
	mercury_compile__maybe_grab_optfiles(ModuleImports1, Verbose,
					 ModuleImports2, IntermodError),
	{ ModuleImports2 = module_imports(_, _, _, Items2, _) },
	mercury_compile__expand_equiv_types(Items2, Verbose, Stats,
					Items, CircularTypes, EqvMap),
	{ bool__or(UndefTypes0, CircularTypes, UndefTypes1) },
	mercury_compile__make_hlds(Module, Items, EqvMap, Verbose, Stats,
				HLDS0, UndefTypes2, UndefModes2, FoundError),
	{ bool__or(UndefTypes1, UndefTypes2, UndefTypes) },
	{ bool__or(UndefModes0, UndefModes2, UndefModes) },
	mercury_compile__maybe_dump_hlds(HLDS0, "1", "initial"),

	% Only stop on syntax errors in .opt files.
	( { FoundError = yes ; IntermodError = yes } ->
		{ module_info_incr_errors(HLDS0, HLDS1) }
	;	
		{ HLDS1 = HLDS0 }
	).

:- pred mercury_compile__module_qualify_items(item_list, item_list, string,
		bool, bool, int, bool, bool, io__state, io__state).
:- mode mercury_compile__module_qualify_items(in, out, in, in, in, out, out,
			out, di, uo) is det. 

mercury_compile__module_qualify_items(Items0, Items, ModuleName, Verbose, Stats,
			NumErrors, UndefTypes, UndefModes) -->
	maybe_write_string(Verbose, "% Module qualifying items...\n"),
	maybe_flush_output(Verbose),
	module_qual__module_qualify_items(Items0, Items, ModuleName, yes,
				NumErrors, UndefTypes, UndefModes),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_grab_optfiles(module_imports, bool,
	 module_imports, bool, io__state, io__state).
:- mode mercury_compile__maybe_grab_optfiles(in, in, out, out, di, uo) is det.

mercury_compile__maybe_grab_optfiles(Imports0, Verbose, Imports, Error) -->
	globals__io_lookup_bool_option(intermodule_optimization, UseOptInt),
	globals__io_lookup_bool_option(make_optimization_interface,
						MakeOptInt),
	( { UseOptInt = yes, MakeOptInt = no } ->
		maybe_write_string(Verbose, "% Reading .opt files...\n"),
		maybe_flush_output(Verbose),
		intermod__grab_optfiles(Imports0, Imports, Error),
		maybe_write_string(Verbose, "% Done.\n")
	;
		{ Imports = Imports0 },
		{ Error = no }
	).

:- pred mercury_compile__expand_equiv_types(item_list, bool, bool, item_list,
	bool, eqv_map, io__state, io__state).
:- mode mercury_compile__expand_equiv_types(in, in, in, out,
	out, out, di, uo) is det.

mercury_compile__expand_equiv_types(Items0, Verbose, Stats,
		Items, CircularTypes, EqvMap) -->
	maybe_write_string(Verbose, "% Expanding equivalence types..."),
	maybe_flush_output(Verbose),
	equiv_type__expand_eqv_types(Items0, Items, CircularTypes, EqvMap),
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__make_hlds(module_name, item_list, eqv_map, bool, bool,
	module_info, bool, bool, bool, io__state, io__state).
:- mode mercury_compile__make_hlds(in, in, in, in, in,
	out, out, out, out, di, uo) is det.

mercury_compile__make_hlds(Module, Items, EqvMap, Verbose, Stats,
		HLDS, UndefTypes, UndefModes, FoundSemanticError) -->
	maybe_write_string(Verbose, "% Converting parse tree to hlds...\n"),
	{ Prog = module(Module, Items) },
	parse_tree_to_hlds(Prog, EqvMap, HLDS, UndefTypes, UndefModes),
	{ module_info_num_errors(HLDS, NumErrors) },
	( { NumErrors > 0 } ->
		{ FoundSemanticError = yes },
		io__set_exit_status(1)
	;
		{ FoundSemanticError = no }
	),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__frontend_pass(module_info, module_info, bool,
					bool, bool, io__state, io__state).
% :- mode mercury_compile__frontend_pass(di, uo, in, in, out, di, uo) is det.
:- mode mercury_compile__frontend_pass(in, out, in, in, out, di, uo) is det.

mercury_compile__frontend_pass(HLDS1, HLDS, FoundUndefTypeError,
		FoundUndefModeError, FoundError) -->
	%
	% We can't continue after an undefined type error, since
	% typecheck would get internal errors
	%
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Type-checking...\n"),
	( { FoundUndefTypeError = yes } ->
	    { HLDS = HLDS1 },
	    { FoundError = yes },
	    maybe_write_string(Verbose,
		    "% Program contains undefined type error(s).\n"),
	    io__set_exit_status(1)
	;

	    %
	    % Next typecheck the clauses.
	    %
	    typecheck(HLDS1, HLDS3, FoundTypeError),
	    ( { FoundTypeError = yes } ->
		maybe_write_string(Verbose,
			"% Program contains type error(s).\n"),
		io__set_exit_status(1)
	    ;
		maybe_write_string(Verbose, "% Program is type-correct.\n")
	    ),
	    mercury_compile__maybe_dump_hlds(HLDS3, "3", "typecheck"),

	    %
	    % Now continue, even if we got a type error,
	    % unless `--typecheck-only' was specified.
	    %
	    globals__io_lookup_bool_option(typecheck_only, TypecheckOnly),
	    ( { TypecheckOnly = yes } ->
		{ HLDS = HLDS3 },
		{ FoundError = FoundTypeError }
	    ;
		globals__io_lookup_bool_option(make_optimization_interface,
							MakeOptInt),
		( { MakeOptInt = yes }, { FoundTypeError = no } ->
			intermod__write_optfile(HLDS3),
			{ FoundError = no },
			{ HLDS = HLDS3 }
		;
			%
			% We can't continue after an undefined insts/mode
			% error, since mode analysis would get internal errors
			%
			( { FoundUndefModeError = yes } ->
			    { FoundError = yes },
			    { HLDS = HLDS3 },
			    maybe_write_string(Verbose,
		"% Program contains undefined inst or undefined mode error(s).\n"),
			    io__set_exit_status(1)
			;
			    %
			    % Now go ahead and do the rest of mode checking and
			    % determinism analysis
			    %
			    mercury_compile__frontend_pass_2_by_phases(HLDS3,
			    		HLDS, FoundModeOrDetError),
			    { bool__or(FoundTypeError, FoundModeOrDetError,
					FoundError) }
			)
		)
	    )
	).

:- pred mercury_compile__frontend_pass_2(module_info, module_info,
	bool, io__state, io__state).
% :- mode mercury_compile__frontend_pass_2(di, uo, out, di, uo) is det.
:- mode mercury_compile__frontend_pass_2(in, out, out, di, uo) is det.

mercury_compile__frontend_pass_2(HLDS0, HLDS, FoundError) -->
	globals__io_lookup_bool_option(trad_passes, TradPasses),
	(
		{ TradPasses = no },
		mercury_compile__frontend_pass_2_by_phases(HLDS0, HLDS,
			FoundError)
	;
		{ TradPasses = yes },
		mercury_compile__frontend_pass_2_by_preds(HLDS0, HLDS,
			FoundError)
	).

:- pred mercury_compile__frontend_pass_2_by_phases(module_info, module_info,
	bool, io__state, io__state).
% :- mode mercury_compile__frontend_pass_2_by_phases(di, uo, out, di, uo)
% is det.
:- mode mercury_compile__frontend_pass_2_by_phases(in, out, out, di, uo) is det.

mercury_compile__frontend_pass_2_by_phases(HLDS4, HLDS20, FoundError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__modecheck(HLDS4, Verbose, Stats, HLDS5,
		FoundModeError, UnsafeToContinue),
	mercury_compile__maybe_dump_hlds(HLDS5, "5", "modecheck"),

	( { UnsafeToContinue = yes } ->
		{ FoundError = yes },
		{ HLDS12 = HLDS5 }
	;
		mercury_compile__detect_switches(HLDS5, Verbose, Stats, HLDS6),
		mercury_compile__maybe_dump_hlds(HLDS6, "6", "switch_detect"),

		mercury_compile__detect_cse(HLDS6, Verbose, Stats, HLDS7),
		mercury_compile__maybe_dump_hlds(HLDS7, "7", "cse"),

		mercury_compile__check_determinism(HLDS7, Verbose, Stats, HLDS8,
			FoundDetError),
		mercury_compile__maybe_dump_hlds(HLDS8, "8", "determinism"),

		mercury_compile__check_unique_modes(HLDS8, Verbose, Stats,
			HLDS9, FoundUniqError),
		mercury_compile__maybe_dump_hlds(HLDS9, "9", "unique_modes"),
		
		mercury_compile__check_stratification(HLDS9, Verbose, Stats, 
			HLDS10, FoundStratError),
		mercury_compile__maybe_dump_hlds(HLDS10, "10", "stratification"),

		mercury_compile__simplify(HLDS10, Verbose, Stats, HLDS11),
		mercury_compile__maybe_dump_hlds(HLDS11, "11", "simplify"),

		%
		% work out whether we encountered any errors
		%
		(
			{ FoundModeError = no },
			{ FoundDetError = no },
			{ FoundUniqError = no },
			{ FoundStratError = no }
		->
			{ FoundError = no },
			globals__io_lookup_bool_option(intermodule_optimization,
								Intermod),
			{ Intermod = yes ->
				intermod__adjust_pred_import_status(HLDS11,
					HLDS12)
			;
				HLDS12 = HLDS11
			}
		;
			{ FoundError = yes },
			{ HLDS12 = HLDS11 }
		)
	),

	{ HLDS20 = HLDS12 },
	mercury_compile__maybe_dump_hlds(HLDS20, "20", "front_end").

:- pred mercury_compile__frontend_pass_2_by_preds(module_info, module_info,
	bool, io__state, io__state).
% :- mode mercury_compile__frontend_pass_2_by_preds(di, uo, out, di, uo)
%	is det.
:- mode mercury_compile__frontend_pass_2_by_preds(in, out, out, di, uo)
	is det.

mercury_compile__frontend_pass_2_by_preds(HLDS0, HLDS, FoundError) -->
	mercury_compile__frontend_pass_2_by_phases(HLDS0, HLDS, FoundError).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__middle_pass(string, module_info, module_info,
					io__state, io__state).
% :- mode mercury_compile__middle_pass(in, di, uo, di, uo) is det.
:- mode mercury_compile__middle_pass(in, in, out, di, uo) is det.

mercury_compile__middle_pass(ModuleName, HLDS25, HLDS50) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__maybe_excess_assigns(HLDS25, Verbose, Stats, HLDS26),
	mercury_compile__maybe_dump_hlds(HLDS26, "26", "excessassign"),

	mercury_compile__maybe_polymorphism(HLDS26, Verbose, Stats, HLDS28),
	mercury_compile__maybe_dump_hlds(HLDS28, "28", "polymorphism"),

	mercury_compile__maybe_base_type_infos(HLDS28, Verbose, Stats, HLDS29),
	mercury_compile__maybe_dump_hlds(HLDS29, "29", "base_type_infos"),

	mercury_compile__maybe_base_type_layouts(HLDS29, Verbose, Stats,HLDS30),
	mercury_compile__maybe_dump_hlds(HLDS30, "30", "base_type_layouts"),

	mercury_compile__maybe_bytecodes(HLDS30, ModuleName, Verbose, Stats),

	mercury_compile__maybe_higher_order(HLDS30, Verbose, Stats, HLDS31),
	mercury_compile__maybe_dump_hlds(HLDS31, "31", "higher_order"),

	mercury_compile__maybe_do_inlining(HLDS31, Verbose, Stats, HLDS34),
	mercury_compile__maybe_dump_hlds(HLDS34, "34", "inlining"),

	mercury_compile__maybe_common_struct(HLDS34, Verbose, Stats, HLDS37),
	mercury_compile__maybe_dump_hlds(HLDS37, "37", "common"),

	% dnf transformations should be after inlining
	% magic sets transformations should be before constraints
	mercury_compile__maybe_transform_dnf(HLDS37, Verbose, Stats, HLDS38),
	mercury_compile__maybe_dump_hlds(HLDS38, "38", "dnf"),

	mercury_compile__maybe_constraints(HLDS38, Verbose, Stats, HLDS40),
	mercury_compile__maybe_dump_hlds(HLDS40, "40", "constraint"),

	mercury_compile__maybe_unused_args(HLDS40, Verbose, Stats, HLDS43),
	mercury_compile__maybe_dump_hlds(HLDS43, "43", "unused_args"),

	mercury_compile__maybe_dead_procs(HLDS43, Verbose, Stats, HLDS46),
	mercury_compile__maybe_dump_hlds(HLDS46, "46", "dead_procs"),

	mercury_compile__maybe_lco(HLDS46, Verbose, Stats, HLDS47),
	mercury_compile__maybe_dump_hlds(HLDS47, "47", "lco"),

	% map_args_to_regs affects the interface to a predicate,
	% so it must be done in one phase immediately before code generation

	mercury_compile__map_args_to_regs(HLDS47, Verbose, Stats, HLDS49),
	mercury_compile__maybe_dump_hlds(HLDS49, "49", "args_to_regs"),

	{ HLDS50 = HLDS49 },
	mercury_compile__maybe_dump_hlds(HLDS49, "50", "middle_pass").

%-----------------------------------------------------------------------------%

:- pred mercury_compile__backend_pass(module_info, module_info,
	list(c_procedure), io__state, io__state).
% :- mode mercury_compile__backend_pass(di, uo, out, di, uo) is det.
:- mode mercury_compile__backend_pass(in, out, out, di, uo) is det.

mercury_compile__backend_pass(HLDS0, HLDS, LLDS) -->
	globals__io_lookup_bool_option(trad_passes, TradPasses),
	(
		{ TradPasses = no },
		mercury_compile__backend_pass_by_phases(HLDS0, HLDS, LLDS)
	;
		{ TradPasses = yes },
		mercury_compile__backend_pass_by_preds(HLDS0, HLDS, LLDS)
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__backend_pass_by_phases(module_info, module_info,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__backend_pass_by_phases(in, out, out, di, uo) is det.

mercury_compile__backend_pass_by_phases(HLDS50, HLDS99, LLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__maybe_followcode(HLDS50, Verbose, Stats, HLDS52),
	mercury_compile__maybe_dump_hlds(HLDS52, "52", "followcode"),

	mercury_compile__maybe_excess_assigns(HLDS52, Verbose, Stats, HLDS53),
	mercury_compile__maybe_dump_hlds(HLDS53, "53", "excessassign"),

	mercury_compile__maybe_saved_vars(HLDS53, Verbose, Stats, HLDS56),
	mercury_compile__maybe_dump_hlds(HLDS56, "56", "savedvars"),

	mercury_compile__compute_liveness(HLDS56, Verbose, Stats, HLDS59),
	mercury_compile__maybe_dump_hlds(HLDS59, "59", "liveness"),

	mercury_compile__compute_stack_vars(HLDS59, Verbose, Stats, HLDS65),
	mercury_compile__maybe_dump_hlds(HLDS65, "65", "stackvars"),

	mercury_compile__allocate_store_map(HLDS65, Verbose, Stats, HLDS68),
	mercury_compile__maybe_dump_hlds(HLDS68, "68", "store_map"),

	maybe_report_sizes(HLDS68),

	{ HLDS90 = HLDS68 },
	mercury_compile__maybe_dump_hlds(HLDS90, "90", "precodegen"),

	mercury_compile__generate_code(HLDS90, Verbose, Stats, HLDS95, LLDS1),
	mercury_compile__maybe_dump_hlds(HLDS95, "95", "codegen"),

	{ HLDS99 = HLDS95 },
	mercury_compile__maybe_dump_hlds(HLDS99, "99", "final"),

	mercury_compile__maybe_do_optimize(LLDS1, Verbose, Stats, LLDS).

:- pred mercury_compile__backend_pass_by_preds(module_info, module_info,
	list(c_procedure), io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds(di, uo, out, di, uo) is det.
:- mode mercury_compile__backend_pass_by_preds(in, out, out, di, uo) is det.

mercury_compile__backend_pass_by_preds(HLDS0, HLDS, LLDS) -->
	{ module_info_predids(HLDS0, PredIds) },
	mercury_compile__backend_pass_by_preds_2(PredIds, HLDS0, HLDS, LLDS).

:- pred mercury_compile__backend_pass_by_preds_2(list(pred_id),
	module_info, module_info, list(c_procedure), io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds_2(in, di, uo, out, di, uo)
% 	is det.
:- mode mercury_compile__backend_pass_by_preds_2(in, in, out, out, di, uo)
	is det.

mercury_compile__backend_pass_by_preds_2([], ModuleInfo, ModuleInfo, [])
	--> [].
mercury_compile__backend_pass_by_preds_2([PredId | PredIds], ModuleInfo0,
		ModuleInfo, Code) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	( { ProcIds = [] } ->
		{ ModuleInfo1 = ModuleInfo0 },
		{ Code1 = [] }
	;
		globals__io_lookup_bool_option(verbose, Verbose),
		( { Verbose = yes } ->
			io__write_string("% Generating code for "),
			hlds_out__write_pred_id(ModuleInfo0, PredId),
			io__write_string("\n")
		;
			[]
		),
		mercury_compile__backend_pass_by_preds_3(ProcIds, PredId,
			PredInfo, ModuleInfo0, ModuleInfo1, Code1)
	),
	mercury_compile__backend_pass_by_preds_2(PredIds,
		ModuleInfo1, ModuleInfo, Code2),
	{ list__append(Code1, Code2, Code) }.

:- pred mercury_compile__backend_pass_by_preds_3(list(proc_id), pred_id,
	pred_info, module_info, module_info, list(c_procedure),
	io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds_3(in, in, in, di, uo, out,
% 	di, uo) is det.
:- mode mercury_compile__backend_pass_by_preds_3(in, in, in, in, out, out,
	di, uo) is det.

mercury_compile__backend_pass_by_preds_3([], _, _, ModuleInfo, ModuleInfo, [])
		--> [].
mercury_compile__backend_pass_by_preds_3([ProcId | ProcIds], PredId, PredInfo,
		ModuleInfo0, ModuleInfo, [Proc | Procs]) -->
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	mercury_compile__backend_pass_by_preds_4(ProcInfo, ProcId, PredId,
		ModuleInfo0, ModuleInfo1, Proc),
	mercury_compile__backend_pass_by_preds_3(ProcIds, PredId, PredInfo,
		ModuleInfo1, ModuleInfo, Procs).

:- pred mercury_compile__backend_pass_by_preds_4(proc_info, proc_id, pred_id,
	module_info, module_info, c_procedure, io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds_4(in, in, in, di, uo,
% 	out, di, uo) is det.
:- mode mercury_compile__backend_pass_by_preds_4(in, in, in, in, out,
	out, di, uo) is det.

mercury_compile__backend_pass_by_preds_4(ProcInfo0, ProcId, PredId,
		ModuleInfo0, ModuleInfo, Proc) -->
	globals__io_lookup_bool_option(follow_code, FollowCode),
	globals__io_lookup_bool_option(prev_code, PrevCode),
	( { FollowCode = yes ; PrevCode = yes } ->
		{ move_follow_code_in_proc(ProcInfo0, ProcInfo1,
			ModuleInfo0, ModuleInfo1) }
	;
		{ ProcInfo1 = ProcInfo0 },
		{ ModuleInfo1 = ModuleInfo0 }
	),
	globals__io_lookup_bool_option(excess_assign, ExcessAssign),
	( { ExcessAssign = yes } ->
		{ excess_assignments_proc(ProcInfo1, ModuleInfo0, ProcInfo2) }
	;
		{ ProcInfo2 = ProcInfo1 }
	),
	globals__io_lookup_bool_option(optimize_saved_vars, SavedVars),
	( { SavedVars = yes } ->
		{ saved_vars_proc(ProcInfo2, ModuleInfo0, ProcInfo3) }
	;
		{ ProcInfo3 = ProcInfo2 }
	),
	detect_liveness_proc(PredId, ProcId, ModuleInfo1, ProcInfo3, ProcInfo4),
	{ allocate_stack_slots_in_proc(ProcInfo4, ModuleInfo1, ProcInfo5) },
	{ store_alloc_in_proc(ProcInfo5, ModuleInfo1, ProcInfo6) },
	{ module_info_get_shapes(ModuleInfo1, Shapes0) },
	generate_proc_code(ProcInfo6, ProcId, PredId, ModuleInfo1,
		Shapes0, Shapes, Proc0),
	{ module_info_set_shapes(ModuleInfo1, Shapes, ModuleInfo) },
	globals__io_lookup_bool_option(optimize, Optimize),
	( { Optimize = yes } ->
		optimize__proc(Proc0, Proc)
	;
		{ Proc = Proc0 }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__modecheck(module_info, bool, bool,
				module_info, bool, bool, io__state, io__state).
:- mode mercury_compile__modecheck(in, in, in, out, out, out, di, uo) is det.

mercury_compile__modecheck(HLDS0, Verbose, Stats, HLDS, FoundModeError,
		UnsafeToContinue) -->
	{ module_info_num_errors(HLDS0, NumErrors0) },
	modecheck(HLDS0, HLDS, UnsafeToContinue),
	{ module_info_num_errors(HLDS, NumErrors) },
	( { NumErrors \= NumErrors0 } ->
		{ FoundModeError = yes },
		maybe_write_string(Verbose,
			"% Program contains mode error(s).\n"),
		io__set_exit_status(1)
	;
		{ FoundModeError = no },
		maybe_write_string(Verbose,
			"% Program is mode-correct.\n")
	),
	maybe_report_stats(Stats).

:- pred mercury_compile__detect_switches(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__detect_switches(in, in, in, out, di, uo) is det.

mercury_compile__detect_switches(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Detecting switches..."),
	maybe_flush_output(Verbose),
	detect_switches(HLDS0, HLDS),
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__detect_cse(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__detect_cse(in, in, in, out, di, uo) is det.

mercury_compile__detect_cse(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(common_goal, CommonGoal),
	( { CommonGoal = yes } ->
		maybe_write_string(Verbose, "% Detecting common deconstructions...\n"),
		detect_cse(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__check_determinism(module_info, bool, bool,
	module_info, bool, io__state, io__state).
:- mode mercury_compile__check_determinism(in, in, in, out, out, di, uo) is det.

mercury_compile__check_determinism(HLDS0, Verbose, Stats, HLDS, FoundError) -->
	{ module_info_num_errors(HLDS0, NumErrors0) },
	determinism_pass(HLDS0, HLDS),
	{ module_info_num_errors(HLDS, NumErrors) },
	( { NumErrors \= NumErrors0 } ->
		{ FoundError = yes },
		maybe_write_string(Verbose,
			"% Program contains determinism error(s).\n"),
		io__set_exit_status(1)
	;
		{ FoundError = no },
		maybe_write_string(Verbose,
			"% Program is determinism-correct.\n")
	),
	maybe_report_stats(Stats).

:- pred mercury_compile__check_unique_modes(module_info, bool, bool,
	module_info, bool, io__state, io__state).
:- mode mercury_compile__check_unique_modes(in, in, in, out, out, di, uo)
	is det.

mercury_compile__check_unique_modes(HLDS0, Verbose, Stats, HLDS, FoundError) -->
	maybe_write_string(Verbose,
		"% Checking for backtracking over unique modes...\n"),
	io__get_exit_status(OldStatus),
	io__set_exit_status(0),
	unique_modes__check_module(HLDS0, HLDS),
	io__get_exit_status(NewStatus),
	( { NewStatus \= 0 } ->
		{ FoundError = yes },
		maybe_write_string(Verbose,
			"% Program contains unique mode error(s).\n")
	;
		{ FoundError = no },
		maybe_write_string(Verbose,
			"% Program is unique-mode-correct.\n"),
		io__set_exit_status(OldStatus)
	),
	maybe_report_stats(Stats).

:- pred mercury_compile__check_stratification(module_info, bool, bool,
	module_info, bool, io__state, io__state).
:- mode mercury_compile__check_stratification(in, in, in, out, out, di, uo)
	is det.

mercury_compile__check_stratification(HLDS0, Verbose, Stats, HLDS, 
		FoundError) -->
	{ module_info_stratified_preds(HLDS0, StratifiedPreds) },
	globals__io_lookup_bool_option(warn_non_stratification, Warn),
	(
		  { \+ set__empty(StratifiedPreds)
		  ; Warn = yes }
	->
		maybe_write_string(Verbose,
			"% Checking stratification...\n"),
		io__get_exit_status(OldStatus),
		io__set_exit_status(0),
		stratify__check_stratification(HLDS0, HLDS),
		io__get_exit_status(NewStatus),
		( { NewStatus \= 0 } ->
			{ FoundError = yes },
			maybe_write_string(Verbose,
				"% Program contains stratification error(s).\n")
		;
			{ FoundError = no },
			maybe_write_string(Verbose,
				"% done.\n"),
			io__set_exit_status(OldStatus)
		),
		maybe_report_stats(Stats)
	;
		{ FoundError = no },
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__simplify(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__simplify(in, in, in, out, di, uo) is det.

mercury_compile__simplify(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Simplifying goals...\n"),
	maybe_flush_output(Verbose),
	process_all_nonimported_procs(
		update_proc_error(simplify__proc),
		HLDS0, HLDS),
	maybe_write_string(Verbose, "% done\n"),
	maybe_report_stats(Stats).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_write_dependency_graph(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_write_dependency_graph(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_write_dependency_graph(ModuleInfo0, Verbose, Stats,
		ModuleInfo) -->
	globals__io_lookup_bool_option(show_dependency_graph, ShowDepGraph),
	( { ShowDepGraph = yes } ->
		maybe_write_string(Verbose, "% Writing dependency graph..."),
		{ module_info_name(ModuleInfo0, Name) },
		{ string__append(Name, ".dependency_graph", WholeName) },
		io__tell(WholeName, Res),
		( { Res = ok } ->
			dependency_graph__write_dependency_graph(ModuleInfo0,
							ModuleInfo),
			io__told,
			maybe_write_string(Verbose, " done.\n")
		;
			report_error("unable to write dependency graph."),
			{ ModuleInfo0 = ModuleInfo }
		),
		maybe_report_stats(Stats)
	;
		{ ModuleInfo0 = ModuleInfo }
	).

	% Outputs the file <module_name>.prof, which contains the static
	% call graph in terms of label names, if the profiling flag is enabled.

:- pred mercury_compile__maybe_output_prof_call_graph(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_output_prof_call_graph(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_output_prof_call_graph(ModuleInfo0, Verbose, Stats,
		ModuleInfo) -->
	globals__io_lookup_bool_option(profiling, Profiling),
	(
		{ Profiling = yes }
	->
		maybe_write_string(Verbose, "% Outputing profiling call graph..."),
		maybe_flush_output(Verbose),
		{ module_info_name(ModuleInfo0, Name) },
		{ string__append(Name, ".prof", WholeName) },
		io__tell(WholeName, Res),
		(
			{ Res = ok }
		->
			dependency_graph__write_prof_dependency_graph(
						ModuleInfo0, ModuleInfo),
			io__told
		;
			report_error("unable to write profiling static call graph."),
			{ ModuleInfo = ModuleInfo0 }
		),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ ModuleInfo = ModuleInfo0 }
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_polymorphism(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_polymorphism(in, in, in, out, di, uo) is det.

mercury_compile__maybe_polymorphism(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(polymorphism, Polymorphism),
	( { Polymorphism = yes } ->
		maybe_write_string(Verbose,
			"% Transforming polymorphic unifications..."),
		maybe_flush_output(Verbose),
		{ polymorphism__process_module(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__maybe_base_type_infos(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_base_type_infos(in, in, in, out, di, uo) is det.

mercury_compile__maybe_base_type_infos(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_get_type_info_method(TypeInfoMethod),
	( { TypeInfoMethod = shared_one_or_two_cell } ->
		maybe_write_string(Verbose,
			"% Generating base_type_info structures..."),
		maybe_flush_output(Verbose),
		{ base_type_info__generate_hlds(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

	% We only add base_type_layouts if shared-one-or-two-cell
	% type_infos are being used (the layouts refer to the
	% base_type_infos, so will fail to link without them).

:- pred mercury_compile__maybe_base_type_layouts(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_base_type_layouts(in, in, in, out, di, uo) is det.

mercury_compile__maybe_base_type_layouts(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_get_type_info_method(TypeInfoMethod),
	globals__io_lookup_bool_option(type_layout, TypeLayoutOption),
	( 
		{ TypeInfoMethod = shared_one_or_two_cell, 
		  TypeLayoutOption = yes } 
	->
		maybe_write_string(Verbose,
			"% Generating base_type_layout structures..."),
		maybe_flush_output(Verbose),
		{ base_type_layout__generate_hlds(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__maybe_bytecodes(module_info, string, bool, bool,
	io__state, io__state).
:- mode mercury_compile__maybe_bytecodes(in, in, in, in, di, uo) is det.

mercury_compile__maybe_bytecodes(HLDS0, ModuleName, Verbose, Stats) -->
	globals__io_lookup_bool_option(generate_bytecode, GenBytecode),
	( { GenBytecode = yes } ->
		globals__io_get_args_method(Args),
		{ generate_arg_info(HLDS0, Args, HLDS1) },
		maybe_write_string(Verbose,
			"% Generating bytecodes...\n"),
		maybe_flush_output(Verbose),
		bytecode_gen__module(HLDS1, Bytecode),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats),
		{ string__append(ModuleName, ".bytecode", BytecodeFile) },
		maybe_write_string(Verbose,
			"% Writing byecodes to `"),
		maybe_write_string(Verbose, BytecodeFile),
		maybe_write_string(Verbose, "'..."),
		maybe_flush_output(Verbose),
		output_bytecode_file(BytecodeFile, Bytecode),
		maybe_write_string(Verbose, " done.\n"),
		{ string__append(ModuleName, ".bytedebug", BytedebugFile) },
		maybe_write_string(Verbose,
			"% Writing byecodes to `"),
		maybe_write_string(Verbose, BytedebugFile),
		maybe_write_string(Verbose, "'..."),
		maybe_flush_output(Verbose),
		debug_bytecode_file(BytedebugFile, Bytecode),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		[]
	).

:- pred mercury_compile__maybe_higher_order(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_higher_order(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_higher_order(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(optimize_higher_order, Optimize),
	( { Optimize = yes } ->
		maybe_write_string(Verbose,
				"% Specializing higher-order predicates...\n"),
		maybe_flush_output(Verbose),
		specialize_higher_order(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_do_inlining(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_do_inlining(in, in, in, out, di, uo) is det.

mercury_compile__maybe_do_inlining(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(errorcheck_only, ErrorCheckOnly),
	globals__io_lookup_bool_option(inline_simple, Simple),
	globals__io_lookup_bool_option(inline_single_use, SingleUse),
	globals__io_lookup_int_option(inline_compound_threshold, Threshold),
	(
		{ ErrorCheckOnly = no },
		{ Simple = yes ; SingleUse = yes ; Threshold > 0 }
	->
		maybe_write_string(Verbose, "% Inlining...\n"),
		maybe_flush_output(Verbose),
		inlining(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__maybe_common_struct(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_common_struct(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_common_struct(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(common_struct, CommonStruct),
	( { CommonStruct = yes } ->
		maybe_write_string(Verbose, "% Detecting common structures..."),
		maybe_flush_output(Verbose),
		{ common__optimise_common_subexpressions(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_transform_dnf(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_transform_dnf(in, in, in, out, di, uo) is det.

mercury_compile__maybe_transform_dnf(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(aditi, Aditi),
	( { Aditi = yes } ->
		maybe_write_string(Verbose, "% Disjunctive normal form transformation..."),
		maybe_flush_output(Verbose),
		{ dnf__transform_module(HLDS0, no, no, HLDS) },
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_constraints(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_constraints(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_constraints(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(constraint_propagation, ConstraintProp),
	( { ConstraintProp = yes } ->
		maybe_write_string(Verbose, "% Propagating constraints..."),
		maybe_flush_output(Verbose),
		constraint_propagation(HLDS0, HLDS),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_unused_args(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_unused_args(in, in, in, out, di, uo) is det.

mercury_compile__maybe_unused_args(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(optimize_unused_args, Optimize),
	globals__io_lookup_bool_option(warn_unused_args, Warn),
	( { Optimize = yes; Warn = yes } ->
		maybe_write_string(Verbose, "% Finding unused arguments ...\n"),
		maybe_flush_output(Verbose),
		unused_args__process_module(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_dead_procs(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_dead_procs(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_dead_procs(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(optimize_dead_procs, Dead),
	( { Dead = yes } ->
		maybe_write_string(Verbose, "% Eliminating dead procedures...\n"),
		maybe_flush_output(Verbose),
		dead_proc_elim(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_lco(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_lco(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_lco(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(optimize_constructor_last_call, LCO),
	( { LCO = yes } ->
		maybe_write_string(Verbose, "% Looking for LCO modulo constructor application ...\n"),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(
			update_proc_io(lco_modulo_constructors), HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

%-----------------------------------------------------------------------------%

% The backend passes

:- pred mercury_compile__map_args_to_regs(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__map_args_to_regs(in, in, in, out, di, uo) is det.

mercury_compile__map_args_to_regs(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Mapping args to regs..."),
	maybe_flush_output(Verbose),
	globals__io_get_args_method(Args),
	{ generate_arg_info(HLDS0, Args, HLDS) },
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_excess_assigns(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_excess_assigns(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_excess_assigns(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(excess_assign, ExcessAssign),
	( { ExcessAssign = yes } ->
		maybe_write_string(Verbose, "% Removing excess assignments..."),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(update_proc(
			excess_assignments_proc), HLDS0, HLDS),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_saved_vars(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_saved_vars(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_saved_vars(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(optimize_saved_vars, SavedVars),
	( { SavedVars = yes } ->
		maybe_write_string(Verbose, "% Reordering to minimize variable saves..."),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(update_proc(
			saved_vars_proc), HLDS0, HLDS),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_followcode(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_followcode(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_followcode(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(follow_code, FollowCode),
	globals__io_lookup_bool_option(prev_code, PrevCode),
	( { FollowCode = yes ; PrevCode = yes } ->
		maybe_write_string(Verbose, "% Migrating branch code..."),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(update_module(
			move_follow_code_in_proc), HLDS0, HLDS),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__compute_liveness(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__compute_liveness(in, in, in, out, di, uo) is det.

mercury_compile__compute_liveness(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Computing liveness...\n"),
	maybe_flush_output(Verbose),
	process_all_nonimported_procs(update_proc_io(detect_liveness_proc),
		HLDS0, HLDS),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__compute_stack_vars(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__compute_stack_vars(in, in, in, out, di, uo) is det.

mercury_compile__compute_stack_vars(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Computing stack vars..."),
	maybe_flush_output(Verbose),
	process_all_nonimported_procs(update_proc(allocate_stack_slots_in_proc),
		HLDS0, HLDS),
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__allocate_store_map(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__allocate_store_map(in, in, in, out, di, uo) is det.

mercury_compile__allocate_store_map(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Allocating store map..."),
	maybe_flush_output(Verbose),
	process_all_nonimported_procs(update_proc(store_alloc_in_proc),
		HLDS0, HLDS),
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__generate_code(module_info, bool, bool, module_info,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__generate_code(in, in, in, out, out, di, uo) is det.

mercury_compile__generate_code(HLDS0, Verbose, Stats, HLDS, LLDS) -->
	maybe_write_string(Verbose, "% Generating code...\n"),
	maybe_flush_output(Verbose),
	generate_code(HLDS0, HLDS, LLDS),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_do_optimize(list(c_procedure), bool, bool,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__maybe_do_optimize(in, in, in, out, di, uo) is det.

mercury_compile__maybe_do_optimize(LLDS0, Verbose, Stats, LLDS) -->
	globals__io_lookup_bool_option(optimize, Optimize),
	( { Optimize = yes } ->
		maybe_write_string(Verbose,
			"% Doing optimizations...\n"),
		maybe_flush_output(Verbose),
		optimize__main(LLDS0, LLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ LLDS = LLDS0 }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The LLDS output pass

:- pred mercury_compile__output_pass(module_info, list(c_procedure), string,
	bool, io__state, io__state).
:- mode mercury_compile__output_pass(in, in, in, out, di, uo) is det.

mercury_compile__output_pass(HLDS0, LLDS0, ModuleName, CompileErrors) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	{ base_type_info__generate_llds(HLDS0, BaseTypeInfos) },
	{ base_type_layout__generate_llds(HLDS0, BaseTypeLayouts0) },

	{ llds_common(LLDS0, BaseTypeLayouts0, ModuleName, LLDS1, 
		BaseTypeLayouts, CommonData) },
	{ list__append(BaseTypeInfos, BaseTypeLayouts, BaseTypeData) },
	mercury_compile__chunk_llds(HLDS0, LLDS1, BaseTypeData, CommonData,
		LLDS2, NumChunks),
	mercury_compile__output_llds(ModuleName, LLDS2, Verbose, Stats),

	mercury_compile__maybe_find_abstr_exports(HLDS0, Verbose, Stats,
		HLDS1),

	{ module_info_shape_info(HLDS1, Shape_Info) },
	mercury_compile__maybe_write_gc(ModuleName, Shape_Info, LLDS2,
		Verbose, Stats),

	globals__io_lookup_bool_option(compile_to_c, CompileToC),
	( { CompileToC = no } ->
		mercury_compile__c_to_obj(ModuleName, NumChunks, CompileOK),
		{ bool__not(CompileOK, CompileErrors) }
	;
		{ CompileErrors = no }
	),
	export__produce_header_file(HLDS1, ModuleName).

	% Split the code up into bite-size chunks for the C compiler.

:- pred mercury_compile__chunk_llds(module_info, list(c_procedure),
	list(c_module), list(c_module), c_file, int, io__state, io__state).
% :- mode mercury_compile__chunk_llds(in, di, di, uo, out, di, uo) is det.
:- mode mercury_compile__chunk_llds(in, in, in, in, out, out, di, uo) is det.

mercury_compile__chunk_llds(HLDS, Procedures, BaseTypeData, CommonDataModules,
		c_file(Name, C_HeaderCode, ModuleList), NumChunks) -->
	{ module_info_name(HLDS, Name) },
	{ string__append(Name, "_module", ModName) },
	globals__io_lookup_int_option(procs_per_c_function, ProcsPerFunc),
	{ module_info_get_c_header(HLDS, C_HeaderCode) },
	{ module_info_get_c_body_code(HLDS, C_BodyCode0) },
	{ get_c_body_code(C_BodyCode0, C_BodyCode) },
	( { ProcsPerFunc = 0 } ->
		% ProcsPerFunc = 0 really means infinity -
		% we store all the procs in a single function.
		{ ProcModules = [c_module(ModName, Procedures)] }
	;
		{ list__chunk(Procedures, ProcsPerFunc, ChunkList) },
		{ mercury_compile__combine_chunks(ChunkList, ModName,
			ProcModules) }
	),
	{ export__get_pragma_exported_procs(HLDS, PragmaExports) },
	{ list__condense([C_BodyCode, BaseTypeData, CommonDataModules,
		ProcModules, [c_export(PragmaExports)]], ModuleList) },
	{ list__length(ModuleList, NumChunks) }.

:- pred get_c_body_code(c_body_info, list(c_module)).
:- mode get_c_body_code(in, out) is det.

get_c_body_code([], []).
get_c_body_code([Code - Context | CodesAndContexts],
			[c_code(Code, Context) | C_Modules]) :-
	get_c_body_code(CodesAndContexts, C_Modules).

:- pred mercury_compile__combine_chunks(list(list(c_procedure)), string,
	list(c_module)).
:- mode mercury_compile__combine_chunks(in, in, out) is det.

mercury_compile__combine_chunks(ChunkList, ModName, Modules) :-
	mercury_compile__combine_chunks_2(ChunkList, ModName, 0, Modules).

:- pred mercury_compile__combine_chunks_2(list(list(c_procedure)), string, int,
	list(c_module)).
:- mode mercury_compile__combine_chunks_2(in, in, in, out) is det.

mercury_compile__combine_chunks_2([], _ModName, _N, []).
mercury_compile__combine_chunks_2([Chunk|Chunks], ModName, Num,
		[Module | Modules]) :-
	string__int_to_string(Num, NumString),
	string__append(ModName, NumString, ThisModuleName),
	Module = c_module(ThisModuleName, Chunk),
	Num1 is Num + 1,
	mercury_compile__combine_chunks_2(Chunks, ModName, Num1, Modules).

:- pred mercury_compile__output_llds(module_name, c_file, bool, bool,
	io__state, io__state).
:- mode mercury_compile__output_llds(in, in, in, in, di, uo) is det.

mercury_compile__output_llds(ModuleName, LLDS, Verbose, Stats) -->
	maybe_write_string(Verbose,
		"% Writing output to `"),
	maybe_write_string(Verbose, ModuleName),
	maybe_write_string(Verbose, ".c'..."),
	maybe_flush_output(Verbose),
	output_c_file(LLDS),
	maybe_write_string(Verbose, " done.\n"),
	maybe_flush_output(Verbose),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_write_gc(module_name, shape_info, c_file,
	bool, bool, io__state, io__state).
:- mode mercury_compile__maybe_write_gc(in, in, in, in, in, di, uo) is det.

mercury_compile__maybe_write_gc(ModuleName, ShapeInfo, LLDS, Verbose, Stats) -->
	globals__io_get_gc_method(GarbageCollectionMethod),
	( { GarbageCollectionMethod = accurate } ->
		maybe_write_string(Verbose, "% Writing gc info to `"),
		maybe_write_string(Verbose, ModuleName),
		maybe_write_string(Verbose, ".garb'..."),
		maybe_flush_output(Verbose),
		garbage_out__do_garbage_out(ShapeInfo, LLDS),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		[]
	).

:- pred mercury_compile__maybe_find_abstr_exports(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_find_abstr_exports(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_find_abstr_exports(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_get_gc_method(GarbageCollectionMethod),
	(
		{ GarbageCollectionMethod = accurate }
	->
		maybe_write_string(Verbose, "% Looking up abstract type "),
		maybe_write_string(Verbose, "exports..."),
		maybe_flush_output(Verbose),
		{ shapes__do_abstract_exports(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The `--high-level-C' alternative backend

:- pred mercury_compile__gen_hlds(string, module_info, io__state, io__state).
:- mode mercury_compile__gen_hlds(in, in, di, uo) is det.

mercury_compile__gen_hlds(DumpFile, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	maybe_write_string(Verbose, "% Dumping out HLDS to `"),
	maybe_write_string(Verbose, DumpFile),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__tell(DumpFile, Res),
	( { Res = ok } ->
		io__gc_call(mercury_to_c__gen_hlds(0, HLDS)),
		io__told,
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		maybe_write_string(Verbose, "\n"),
		{ string__append_list(["can't open file `",
			DumpFile, "' for output."], ErrorMessage) },
		report_error(ErrorMessage)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type compiler_type ---> gcc ; lcc ; unknown.

:- pred mercury_compile__c_to_obj(string, int, bool, io__state, io__state).
:- mode mercury_compile__c_to_obj(in, in, out, di, uo) is det.

mercury_compile__c_to_obj(ModuleName, NumChunks, Succeeded) -->
	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { SplitFiles = yes } ->
		mercury_compile__c_to_obj_list(ModuleName, 0, NumChunks,
			Succeeded)
	;
		mercury_compile__single_c_to_obj(ModuleName, Succeeded)
	).

:- pred mercury_compile__c_to_obj_list(string, int, int, bool,
					io__state, io__state).
:- mode mercury_compile__c_to_obj_list(in, in, in, out, di, uo) is det.

	% compile each of the C files in `<module>.dir'

mercury_compile__c_to_obj_list(ModuleName, Chunk, NumChunks, Succeeded) -->
	( { Chunk > NumChunks } ->
		{ Succeeded = yes }
	;
		{ dir__basename(ModuleName, BaseName) },
		{ string__format("%s.dir/%s_%03d",
			[s(BaseName), s(BaseName), i(Chunk)], NewName) },
		mercury_compile__single_c_to_obj(NewName, Succeeded0),
		( { Succeeded0 = no } ->
			{ Succeeded = no }
		;
			{ Chunk1 is Chunk + 1 },
			mercury_compile__c_to_obj_list(ModuleName,
				Chunk1, NumChunks, Succeeded)
		)
	).

:- pred mercury_compile__single_c_to_obj(string, bool, io__state, io__state).
:- mode mercury_compile__single_c_to_obj(in, out, di, uo) is det.

mercury_compile__single_c_to_obj(ModuleName, Succeeded) -->
	{ string__append(ModuleName, ".c", C_File) },
	{ string__append(ModuleName, ".o", O_File) },
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Compiling `"),
	maybe_write_string(Verbose, C_File),
	maybe_write_string(Verbose, "':\n"),
	globals__io_lookup_string_option(cc, CC),
	globals__io_lookup_accumulating_option(cflags, C_Flags_List),
	{ join_string_list(C_Flags_List, "", "", " ", CFLAGS) },

	globals__io_lookup_string_option(c_include_directory, C_INCL),
	{ C_INCL = "" ->
		InclOpt = ""
	;
		string__append_list(["-I", C_INCL, " "], InclOpt)
	},
	globals__io_lookup_bool_option(split_c_files, Split_C_Files),
	{ Split_C_Files = yes ->
		SplitOpt = "-DSPLIT_C_FILES "
	;
		SplitOpt = ""
	},
	globals__io_lookup_bool_option(gcc_global_registers, GCC_Regs),
	( { GCC_Regs = yes } ->
		globals__io_lookup_string_option(cflags_for_regs,
			CFLAGS_FOR_REGS),
		{ RegOpt = "-DUSE_GCC_GLOBAL_REGISTERS " }
	;
		{ CFLAGS_FOR_REGS = "" },
		{ RegOpt = "" }
	),
	globals__io_lookup_bool_option(gcc_non_local_gotos, GCC_Gotos),
	( { GCC_Gotos = yes } ->
		{ GotoOpt = "-DUSE_GCC_NONLOCAL_GOTOS " },
		globals__io_lookup_string_option(cflags_for_gotos,
			CFLAGS_FOR_GOTOS)
	;
		{ GotoOpt = "" },
		{ CFLAGS_FOR_GOTOS = "" }
	),
	globals__io_lookup_bool_option(asm_labels, ASM_Labels),
	{ ASM_Labels = yes ->
		AsmOpt = "-DUSE_ASM_LABELS "
	;
		AsmOpt = ""
	},
	globals__io_get_gc_method(GC_Method),
	{ GC_Method = conservative ->
		GC_Opt = "-DCONSERVATIVE_GC "
	; GC_Method = accurate ->
		GC_Opt = "-DNATIVE_GC "
	;
		GC_Opt = ""
	},
	globals__io_lookup_bool_option(profiling, Profiling),
	{ Profiling = yes ->
		ProfileOpt = "-DPROFILE_CALLS -DPROFILE_TIME "
	;
		ProfileOpt = ""
	},
	globals__io_get_tags_method(Tags_Method),
	{ Tags_Method = high ->
		TagsOpt = "-DHIGHTAGS "
	;
		TagsOpt = ""
	},
	globals__io_lookup_int_option(num_tag_bits, NumTagBits),
	{ string__int_to_string(NumTagBits, NumTagBitsString) },
	{ string__append_list(
		["-DTAGBITS=", NumTagBitsString, " "], NumTagBitsOpt) },
	globals__io_lookup_bool_option(debug, Debug),
	{ Debug = yes ->
		DebugOpt = "-g "
	;
		DebugOpt = "-DSPEED "
	},
	{ string__sub_string_search(CC, "gcc", _) ->
		CompilerType = gcc
	; string__sub_string_search(CC, "lcc", _) ->
		CompilerType = lcc
	;
		CompilerType = unknown
	},
	globals__io_lookup_bool_option(constraints, Constraints),
	{ Constraints = yes ->
		ConstraintsOpt = "-DCONSTRAINTS "
	;
		ConstraintsOpt = ""
	},
	globals__io_get_args_method(ArgsMethod),
	{ ArgsMethod = compact ->
		ArgsOpt = "-DCOMPACT_ARGS "
	;
		ArgsOpt = ""
	},
	globals__io_get_type_info_method(TypeInfoMethod),
	{ TypeInfoMethod = one_cell,
		TypeInfoOpt = "-DONE_CELL_TYPE_INFO "
	; TypeInfoMethod = shared_one_or_two_cell,
		TypeInfoOpt = "-DSHARED_ONE_OR_TWO_CELL_TYPEINFO "
	; TypeInfoMethod = one_or_two_cell,
		TypeInfoOpt = "-DONE_OR_TWO_CELL_TYPEINFO "
	},
	globals__io_lookup_bool_option(type_layout, TypeLayoutOption),
	{ TypeLayoutOption = no ->
		TypeLayoutOpt = "-DNO_TYPE_LAYOUT"
	;
		TypeLayoutOpt = ""
	},
	globals__io_lookup_bool_option(c_optimize, C_optimize),
	{ C_optimize = yes, Debug = no ->
		( CompilerType = gcc ->
			OptimizeOpt = "-O2 -fomit-frame-pointer "
		; CompilerType = lcc ->
			OptimizeOpt = ""
		;
			OptimizeOpt = "-O "
		)
	;
		OptimizeOpt = ""
	},
	globals__io_lookup_bool_option(inline_alloc, InlineAlloc),
	{ InlineAlloc = yes ->
		InlineAllocOpt = "-DINLINE_ALLOC -DSILENT "
	;
		InlineAllocOpt = ""
	},
	{ CompilerType = gcc ->
		% if --inline-alloc is enabled, don't enable missing-prototype
		% warnings, since gc_inline.h is missing lots of prototypes
		( InlineAlloc = yes ->
			WarningOpt = "-Wall -Wwrite-strings -Wpointer-arith -Wcast-qual -Wtraditional -Wshadow -Wmissing-prototypes -Wno-unused "
		;
			WarningOpt = "-Wall -Wwrite-strings -Wpointer-arith -Wcast-qual -Wtraditional -Wshadow -Wmissing-prototypes -Wno-unused -Wstrict-prototypes "
		)
	; CompilerType = lcc ->
		WarningOpt = "-w "
	;
		WarningOpt = ""
	},
	% Be careful with the order here!  Some options override others,
	% e.g. CFLAGS_FOR_REGS must come after OptimizeOpt so that
	% it can override -fomit-frame-pointer with -fno-omit-frame-pointer.
	% Also be careful that each option is separated by spaces.
	{ string__append_list([CC, " ", InclOpt, SplitOpt, OptimizeOpt,
		RegOpt, GotoOpt, AsmOpt,
		CFLAGS_FOR_REGS, " ", CFLAGS_FOR_GOTOS, " ",
		GC_Opt, ProfileOpt, TagsOpt, NumTagBitsOpt, DebugOpt,
		ConstraintsOpt, ArgsOpt, TypeInfoOpt, TypeLayoutOpt,
		InlineAllocOpt, WarningOpt, CFLAGS,
		" -c ", C_File, " -o ", O_File], Command) },
	invoke_system_command(Command, Succeeded),
	( { Succeeded = no } ->
		report_error("problem compiling C file.")
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__link_module_list(list(string), io__state, io__state).
:- mode mercury_compile__link_module_list(in, di, uo) is det.

mercury_compile__link_module_list(Modules) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	globals__io_lookup_string_option(output_file_name, OutputFile0),
	( { OutputFile0 = "" } ->
	    ( { Modules = [Module | _] } ->
		{ dir__basename(Module, OutputFile) }
	    ;
		{ error("link_module_list: no modules") }
	    )
	;
	    { OutputFile = OutputFile0 }
	),
	{ dir__basename(OutputFile, OutputFileBase) },

	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { SplitFiles = yes } ->
	    { join_module_list(Modules, ".dir/*.o ", [], ObjectList) },
	    { list__append(
		["ar cr ", OutputFileBase, ".a " | ObjectList],
		[" && ranlib ", OutputFileBase, ".a"],
		MakeLibCmdList) },
	    { string__append_list(MakeLibCmdList, MakeLibCmd) },
	    invoke_system_command(MakeLibCmd, MakeLibCmdOK),
	    { string__append(OutputFileBase, ".a", Objects) }
	;
	    { MakeLibCmdOK = yes },
	    { join_module_list(Modules, ".o ", [], ObjectsList) },
	    { string__append_list(ObjectsList, Objects) }
	),
	( { MakeLibCmdOK = no } ->
	    report_error("creation of object file library failed.")
	;
	    % create the initialization C file
	    maybe_write_string(Verbose, "% Creating initialization file...\n"),
	    { string__append(OutputFileBase, "_init", C_Init_Base) },
	    { join_module_list(Modules, ".m ", ["> ", C_Init_Base, ".c"],
				MkInitCmd0) },
	    { string__append_list(["c2init " | MkInitCmd0], MkInitCmd) },
	    invoke_system_command(MkInitCmd, MkInitOK),
	    maybe_report_stats(Stats),
	    ( { MkInitOK = no } ->
		report_error("creation of init file failed.")
	    ;
		% compile it
		maybe_write_string(Verbose,
			"% Compiling initialization file...\n"),
		mercury_compile__single_c_to_obj(C_Init_Base, CompileOK),
		maybe_report_stats(Stats),
		( { CompileOK = no } ->
		    report_error("compilation of init file failed.")
		;
		    maybe_write_string(Verbose, "% Linking...\n"),
		    globals__io_lookup_string_option(grade, Grade),
		    globals__io_lookup_accumulating_option(link_flags,
				LinkFlagsList),
		    { join_string_list(LinkFlagsList, "", "", " ", LinkFlags) },
		    globals__io_lookup_accumulating_option(
				link_library_directories,
				LinkLibraryDirectoriesList),
		    { join_string_list(LinkLibraryDirectoriesList, "-L", "",
				" ", LinkLibraryDirectories) },
		    globals__io_lookup_accumulating_option(link_libraries,
				LinkLibrariesList),
		    { join_string_list(LinkLibrariesList, "-l", "", " ",
				LinkLibraries) },
		    globals__io_lookup_accumulating_option(link_objects,
				LinkObjectsList),
		    { join_string_list(LinkObjectsList, "", "", " ",
				LinkObjects) },
		    { string__append_list(
			["ml --grade ", Grade, " ", LinkFlags,
			" -o ", OutputFile, " ",
			OutputFileBase, "_init.o ", Objects, " ",
			LinkObjects, " ",
			LinkLibraryDirectories, " ", LinkLibraries],
			LinkCmd) },
		    invoke_system_command(LinkCmd, LinkCmdOK),
		    maybe_report_stats(Stats),
		    ( { LinkCmdOK = no } ->
			report_error("link failed.")
		    ;
			[]
		    )
		)
	    )
	).

:- pred join_string_list(list(string), string, string, string, string).
:- mode join_string_list(in, in, in, in, out) is det.

join_string_list([], _Prefix, _Suffix, _Separator, "").
join_string_list([String | Strings], Prefix, Suffix, Separator, Result) :-
	( Strings = [] ->
		string__append_list([Prefix, String, Suffix], Result)
	;
		join_string_list(Strings, Prefix, Suffix, Separator, Result0),
		string__append_list([Prefix, String, Suffix, Separator,
			Result0], Result)
	).

:- pred join_module_list(list(string), string, list(string), list(string)).
:- mode join_module_list(in, in, in, out) is det.

join_module_list([], _Separator, Terminator, Terminator).
join_module_list([Module0 | Modules0], Separator, Terminator,
			[Basename0, Separator | Rest]) :-
	dir__basename(Module0, Basename0),
	join_module_list(Modules0, Separator, Terminator, Rest).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_dump_hlds(module_info, string, string,
	io__state, io__state).
:- mode mercury_compile__maybe_dump_hlds(in, in, in, di, uo) is det.

mercury_compile__maybe_dump_hlds(HLDS, StageNum, StageName) -->
	globals__io_lookup_accumulating_option(dump_hlds, DumpStages),
	(
		{ list__member(StageNum, DumpStages)
		; list__member(StageName, DumpStages)
		; list__member("all", DumpStages)
		}
	->
		{ module_info_name(HLDS, ModuleName) },
		{ string__append_list(
			[ModuleName, ".hlds_dump.", StageNum, "-", StageName],
			DumpFile) },
		mercury_compile__dump_hlds(DumpFile, HLDS)
	;
		[]
	).

:- pred mercury_compile__dump_hlds(string, module_info, io__state, io__state).
:- mode mercury_compile__dump_hlds(in, in, di, uo) is det.

mercury_compile__dump_hlds(DumpFile, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	maybe_write_string(Verbose, "% Dumping out HLDS to `"),
	maybe_write_string(Verbose, DumpFile),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__tell(DumpFile, Res),
	( { Res = ok } ->
		io__gc_call(hlds_out__write_hlds(0, HLDS)),
		io__told,
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		maybe_write_string(Verbose, "\n"),
		{ string__append_list(["can't open file `",
			DumpFile, "' for output."], ErrorMessage) },
		report_error(ErrorMessage)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

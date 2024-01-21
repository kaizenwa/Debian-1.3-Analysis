%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: modules.m
% main author: fjh

% This module contains all the code for handling module imports and exports,
% for computing module dependencies, and for generate makefile fragments to
% record those dependencies.
%
%
% The interface system works as follows:
%
% 1. a .int3 file is written, which contains all the types, insts
% and modes defined in the interface. Equivalence types, insts and
% modes are written in full, others are written in abstract form.
% These are module qualified as far as possible given the information
% present in the current module. The datestamp on the .date3 file
% gives the last time the .int3 file was checked for consistency.
%
% 2. The .int and .int2 files are created, using the .int3 files
% of imported modules to fully module qualify all items. Therefore
% the .int2 file is just a fully qualified version of the .int3 file.
% The .int3 file must be kept for datestamping purposes. The datestamp
% on the .date file gives the last time the .int and .int2 files
% were checked.
%
%-----------------------------------------------------------------------------%

:- module modules.

:- interface.

:- import_module prog_data, prog_io.
:- import_module list, io.

	% read_mod(ModuleName, Extension, Descr, Search, Items, Error):
	%	Given a module name and a file extension (e.g. `.m',
	%	`.int', or `int2'), read in the list of items in that file.
	%	If Search is yes, search all directories given by the option
	%	search_directories for the module.
	%
:- pred read_mod(string, string, string, bool, item_list, module_error,
		io__state, io__state).
:- mode read_mod(in, in, in, in, out, out, di, uo) is det.

	% Same as above, but doesn't return error messages.
:- pred read_mod_ignore_errors(string, string, string, bool, item_list, 
		module_error, io__state, io__state).
:- mode read_mod_ignore_errors(in, in, in, in, out, out, di, uo) is det.


	% make_interface(ModuleName, Items):
	%	Given a module name and the list of items in that module,
	%	output the long (`.int') and short (`.int2') interface files
	%	for the module.
	%
:- pred make_interface(string, item_list, io__state, io__state).
:- mode make_interface(in, in, di, uo) is det.

	% 	Output the unqualified short interface file to <module>.int3.
	%
:- pred make_short_interface(string, item_list, io__state, io__state).
:- mode make_short_interface(in, in, di, uo) is det.

	% grab_imported_modules(ModuleName, Items, Module, FactDeps, Error)
	%	Given a module name and the list of items in that module,
	%	read in the full interface files for all the imported modules,
	%	and the short interface files for all the indirectly imported
	%	modules, and return a `module_imports' structure containing the
	%	relevant information.
	%	Also returns FactDeps list of filenames for fact tables in this
	%	module.
	%
:- type module_imports --->
	module_imports(
		string,		% The primary module name
		list(string),	% The list of modules it directly imports
		list(string),	% The list of modules it indirectly imports
		item_list,	% The contents of the module and its imports
		module_error	% Whether an error has been encountered
	).

:- pred grab_imported_modules(string, item_list, module_imports, list(string), 
			module_error, io__state, io__state).
:- mode grab_imported_modules(in, in, out, out, out, di, uo) is det.

	% write_dependency_file(ModuleName, LongDeps, ShortDeps, FactDeps):
	%	Write out the per-module makefile dependencies (`.d') file
	%	for a module `ModuleName' which depends directly on the
	% 	modules `LongDeps' and indirectly on the modules `ShortDeps'.
	%	FactDeps is the list of filenames of fact tables in the module.
	%
:- pred write_dependency_file(string, list(string), list(string), list(string),
				io__state, io__state).
:- mode write_dependency_file(in, in, in, in, di, uo) is det.

	% generate_dependencies(ModuleName):
	%	Generate the per-program makefile dependencies (`.dep') file
	%	for a program whose top-level module is `ModuleName'.
	%	This involes first transitively reading in all imported
	%	modules.  While we're at it, we also save the per-module
	%	makefile dependency (`.d') files for all those modules.
	%
:- pred generate_dependencies(string, io__state, io__state).
:- mode generate_dependencies(in, di, uo) is det.

	% Given a module (well, a list of items),
	% determine all the modules that it depends upon
	% (both interface dependencies and also implementation dependencies).

:- pred get_dependencies(item_list, list(string)).
:- mode get_dependencies(in, out) is det.

	% Do the importing of the interface files of imported modules.

:- pred process_module_interfaces(list(string), list(string),
				module_imports, module_imports,
				io__state, io__state).
:- mode process_module_interfaces(in, in, in, out, di, uo) is det.

	% touch_interface_datestamp(ModuleName, Ext).
	%
	% Touch the datestamp file `ModuleName.Ext'. Datestamp files
	% are used to record when each of the interface files was last
	% updated.

:- pred touch_interface_datestamp(string, string, io__state, io__state).
:- mode touch_interface_datestamp(in, in, di, uo) is det.

	% update_interface(FileName)
	%
	% Call the shell script mercury_update_interface to update the
	% interface file FileName if it has changed.

:- pred update_interface(string, io__state, io__state).
:- mode update_interface(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module passes_aux, prog_out, mercury_to_mercury.
:- import_module globals, options, intermod, module_qual.
:- import_module bool, string, set, map, term, varset, dir, std_util, library.


	% Read in the .int3 files that the current module depends on,
	% and use these to qualify all items in the interface as much as
	% possible. Then write out the .int and .int2 files.
make_interface(ModuleName, Items0) -->
		% Get interface, including imports.
	{ get_interface(Items0, yes, InterfaceItems0) },
	{ term__context_init(Context) },
	{ varset__init(Varset) },
	{ get_dependencies(InterfaceItems0, InterfaceDeps) },
	{ list__append(InterfaceItems0,
			[module_defn(Varset, imported) - Context],
			InterfaceItems0a) },
	{ Module0 = module_imports(ModuleName, [], [], InterfaceItems0a, no) },
		% Get the .int3s that the current .int depends on.
	process_module_short_interfaces(["mercury_builtin" | InterfaceDeps],
					".int3", Module0, Module),
	{ Module = module_imports(_, _, _, InterfaceItems1, Error) },
	( { Error = yes } ->
		io__write_strings(["Error reading short interface files.\n",
				ModuleName, ".int and ",
				ModuleName, ".int2 not written.\n"])
	;
			% Qualify all items.
		module_qual__module_qualify_items(InterfaceItems1,
				InterfaceItems2, ModuleName, yes, _, _, _),
		io__get_exit_status(Status),
		( { Status \= 0 } ->
			io__write_strings([ModuleName, ".int not written.\n"])
		;
			{ strip_imported_items(InterfaceItems2, [],
							InterfaceItems3) },
			check_for_clauses_in_interface(InterfaceItems3,
							InterfaceItems),
			check_for_no_exports(InterfaceItems, ModuleName),
			write_interface_file(ModuleName, ".int",
							InterfaceItems),
			{ get_short_interface(InterfaceItems,
						ShortInterfaceItems) },
			write_interface_file(ModuleName, ".int2",
						ShortInterfaceItems),
			touch_interface_datestamp(ModuleName, ".date")
		)
	).

	% This qualifies everything as much as it can given the
	% information in the current module and writes out the .int3 file.
make_short_interface(ModuleName, Items0) -->
	{ get_interface(Items0, no, InterfaceItems0) },
	check_for_clauses_in_interface(InterfaceItems0, InterfaceItems),
	{ get_short_interface(InterfaceItems, ShortInterfaceItems0) },
	module_qual__module_qualify_items(ShortInterfaceItems0,
			ShortInterfaceItems, ModuleName, no, _, _, _),
	write_interface_file(ModuleName, ".int3", ShortInterfaceItems),
	touch_interface_datestamp(ModuleName, ".date3").


:- pred strip_imported_items(item_list::in, item_list::in,
						item_list::out) is det.

strip_imported_items([], Items0, Items) :-
	list__reverse(Items0, Items). 
strip_imported_items([Item - Context | Rest], Items0, Items) :-
	(
		Item = module_defn(_, imported)
	->
		list__reverse(Items0, Items)
	;
		strip_imported_items(Rest, [Item - Context | Items0], Items)
	).
%-----------------------------------------------------------------------------%

:- pred check_for_clauses_in_interface(item_list, item_list,
					io__state, io__state).
:- mode check_for_clauses_in_interface(in, out, di, uo) is det.

check_for_clauses_in_interface([], []) --> [].
check_for_clauses_in_interface([Item0 | Items0], Items) -->
	(
		( { Item0 = pred_clause(_,_,_,_) - Context }
		; { Item0 = func_clause(_,_,_,_,_) - Context }
		)
	->
		prog_out__write_context(Context),
		io__write_string("Warning: clause in module interface.\n"),
		check_for_clauses_in_interface(Items0, Items)
	;
		{ Items = [Item0 | Items1] },
		check_for_clauses_in_interface(Items0, Items1)
	).

:- pred check_for_no_exports(item_list, string, io__state, io__state).
:- mode check_for_no_exports(in, in, di, uo) is det.

check_for_no_exports([], ModuleName) -->
	warn_no_exports(ModuleName).
check_for_no_exports([Item - _Context | Items], ModuleName) -->
	(
		{ Item = nothing
		; Item = module_defn(_,_)
		}
	->
		% nothing useful - keep searching
		check_for_no_exports(Items, ModuleName)
	;
		% we found something useful - don't issue the warning
		[]
	).

:- pred warn_no_exports(string, io__state, io__state).
:- mode warn_no_exports(in, di, uo) is det.

warn_no_exports(ModuleName) -->
	globals__io_lookup_bool_option(warn_nothing_exported, ExportWarning),
	( 	
		{ ExportWarning = yes }
	->
		report_warning(ModuleName, 1,
			"interface does not export anything."),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		(
			{ VerboseErrors = yes }
		->
			io__stderr_stream(StdErr),
			io__write_strings(StdErr, [ "\t\t",
	"To be useful, a module should export something.\n\t\t",
	"A file should contain at least one declaration other than\n\t\t",
	"`:- import_module' in its interface section(s).\n\t\t",
	"This would normally be a `:- pred', `:- func', `:- type',\n\t\t",
	"`:- inst' or `:- mode' declaration.\n"
				])
		;
			[]
		)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred write_interface_file(string, string, item_list, io__state, io__state).
:- mode write_interface_file(in, in, in, di, uo) is det.

write_interface_file(ModuleName, Suffix, InterfaceItems) -->

		% create <Module>.int.tmp

	{ string__append(ModuleName, Suffix, OutputFileName) },
	{ string__append(OutputFileName, ".tmp", TmpOutputFileName) },
	{ dir__basename(ModuleName, BaseModuleName) },

		% we need to add a `:- interface' declaration at the start
		% of the item list
	{ varset__init(VarSet) },
	{ term__context_init(ModuleName, 0, Context) },
	{ InterfaceDeclaration = module_defn(VarSet, interface) - Context },
	{ InterfaceItems1 = [InterfaceDeclaration | InterfaceItems] },

	convert_to_mercury(BaseModuleName, TmpOutputFileName, InterfaceItems1),
	update_interface(OutputFileName).

		% invoke the shell script `mercury_update_interface'
		% to update <Module>.int from <Module>.int.tmp if
		% necessary

update_interface(OutputFileName) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Updating interface:\n"),
	( { Verbose = yes } ->
		{ Command = "mercury_update_interface -v " }
	;
		{ Command = "mercury_update_interface " }
	),
	{ string__append(Command, OutputFileName, ShellCommand) },
	invoke_system_command(ShellCommand, Succeeded),
	( { Succeeded = no } ->
		report_error("problem updating interface files.")
	;
		[]
	).

%-----------------------------------------------------------------------------%

touch_interface_datestamp(ModuleName, Ext) -->
	{ string__append(ModuleName, Ext, OutputFileName) },

	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Touching `"),
	maybe_write_string(Verbose, OutputFileName),
	maybe_write_string(Verbose, "'... "),
	maybe_flush_output(Verbose),
	io__open_output(OutputFileName, Result),
	( { Result = ok(OutputStream) } ->
		io__write_string(OutputStream, "\n"),
		io__close_output(OutputStream),
		maybe_write_string(Verbose, " done.\n")
	;
		io__write_string("\nError opening `"),
		io__write_string(OutputFileName),
		io__write_string("' for output\n")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

grab_imported_modules(ModuleName, Items0, Module, FactDeps, Error) -->
	{ get_dependencies(Items0, ImportedModules) },
	{ get_fact_table_dependencies(Items0, FactDeps) },

		% we add a pseudo-declaration `:- imported' at the end
		% of the item list, so that make_hlds knows which items
		% are imported and which are defined in the main module
	{ varset__init(VarSet) },
	{ term__context_init(ModuleName, 0, Context) },
	{ list__append(Items0,
		[module_defn(VarSet, imported) - Context], Items1) },
	{ dir__basename(ModuleName, BaseModuleName) },
	{ Module0 = module_imports(BaseModuleName, [], [], Items1, no) },
	process_module_interfaces(["mercury_builtin" | ImportedModules], 
		[], Module0, Module),
	{ Module = module_imports(_, _, _, _, Error) }.

%-----------------------------------------------------------------------------%

write_dependency_file(ModuleName, LongDeps0, ShortDeps0, FactDeps0) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	{ string__append(ModuleName, ".d", DependencyFileName) },
	maybe_write_string(Verbose, "% Writing auto-dependency file `"),
	maybe_write_string(Verbose, DependencyFileName),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__open_output(DependencyFileName, Result),
	( { Result = ok(DepStream) } ->
		{ list__sort_and_remove_dups(LongDeps0, LongDeps1) },
		{ list__delete_all(LongDeps1, ModuleName, LongDeps) },
		{ list__sort_and_remove_dups(ShortDeps0, ShortDeps1) },
		{ list__delete_elems(ShortDeps1, LongDeps, ShortDeps2) },
		{ list__delete_all(ShortDeps2, ModuleName, ShortDeps) },
		{ list__sort_and_remove_dups(FactDeps0, FactDeps) },

		io__write_strings(DepStream, [
			ModuleName, ".optdate ",
			ModuleName, ".c ",
			ModuleName, ".err ",
			ModuleName, ".o : ",
			ModuleName, ".m"
		] ),
		write_dependencies_list(LongDeps, ".int", DepStream),
		write_dependencies_list(ShortDeps, ".int2", DepStream),
		write_dependencies_list(FactDeps, "", DepStream),

		globals__io_lookup_bool_option(intermodule_optimization,
							Intermod),
		( { Intermod = yes } ->
			io__write_strings(DepStream, [
				"\n\n", ModuleName, ".c ",
				ModuleName, ".err ", ModuleName, ".o :"
			]),
			% The .c file only depends on the .opt files from 
			% the current directory, so that inter-module
			% optimization works when the .opt files for the 
			% library are unavailable. This is only necessary 
			% because make doesn't allow conditional dependencies.
			% The dependency on the current module's .opt file
			% is to make sure the module gets type-checked without
			% having the definitions of abstract types from other
			% modules.
			get_curr_dir_deps(LongDeps, [], OptDeps),
			write_dependencies_list([ModuleName | OptDeps],
				".opt", DepStream)
		;
			[]
		),

		io__write_strings(DepStream, [
				"\n\n", ModuleName, ".date : ",
				ModuleName, ".m"
		]),
		write_dependencies_list(LongDeps, ".int3", DepStream),
		write_dependencies_list(ShortDeps, ".int3", DepStream),

		io__write_strings(DepStream, [
			"\n\n",
			ModuleName, ".dir/", ModuleName, "_000.o: ",
				ModuleName, ".m\n",
			"\trm -rf ", ModuleName, ".dir\n",
			"\t$(MCS) -s$(GRADE) $(MCSFLAGS) ", ModuleName, ".m\n"
		]),

		io__close_output(DepStream),
		maybe_write_string(Verbose, " done.\n")
	;
		{ string__append_list(["can't open file `", DependencyFileName,
				"' for output."], Message) },
		report_error(Message)
	).

	% Filter out the dependencies that are in the current directory.
:- pred get_curr_dir_deps(list(string)::in, list(string)::in, 
		list(string)::out, io__state::di, io__state::uo) is det.

get_curr_dir_deps([], CurrDirDeps, CurrDirDeps) --> [].
get_curr_dir_deps([Dep | Deps], CurrDirDeps0, CurrDirDeps) -->
	{ string__append(Dep, ".m", DepName) },
	io__see(DepName, Res),
	( { Res = ok } ->
		{ CurrDirDeps1 = [Dep | CurrDirDeps0] },
		io__seen
	;
		{ CurrDirDeps1 = CurrDirDeps0 }
	),
	get_curr_dir_deps(Deps, CurrDirDeps1, CurrDirDeps).

%-----------------------------------------------------------------------------%

generate_dependencies(Module) -->
	%
	% first, build up a map of the dependencies (writing `.d' files as
	% we go)
	%
	{ map__init(DepsMap0) },
	generate_deps_map([Module], DepsMap0, DepsMap),
	%
	% check whether we couldn't read the main `.m' file
	%
	{ map__lookup(DepsMap, Module, deps(_, Error, _, _, _)) },
	( { Error = fatal } ->
	    { string__append_list(["fatal error reading module `",
				Module, "'."], Message) },
	    report_error(Message)
	;
	    %
	    % now, write the `.dep' file
	    %
	    { string__append(Module, ".dep", DepFileName) },
	    globals__io_lookup_bool_option(verbose, Verbose),
	    maybe_write_string(Verbose, "% Creating auto-dependency file `"),
	    maybe_write_string(Verbose, DepFileName),
	    maybe_write_string(Verbose, "'...\n"),
	    io__open_output(DepFileName, Result),
	    ( { Result = ok(DepStream) } ->
		generate_dep_file(Module, DepsMap, DepStream),
		io__close_output(DepStream),
		maybe_write_string(Verbose, "% done\n")
	    ;
		{ string__append_list(["can't open file `", DepFileName,
				"' for output."], Message) },
		report_error(Message)
	    )
	).

% This is the data structure we use to record the dependencies.
% We keep a map from module name to information about the module.

:- type deps_map == map(string, deps).
:- type deps
	---> deps(
		bool,		% have we processed this module yet?
		module_error,	% if we did, where there any errors?
		list(string),	% interface dependencies
		list(string),	% implementation dependencies
		list(string)	% fact table dependencies
	).

% This is the predicate which creates the above data structure.

:- pred generate_deps_map(list(string), deps_map, deps_map,
			io__state, io__state).
:- mode generate_deps_map(in, in, out, di, uo) is det.

generate_deps_map([], DepsMap, DepsMap) --> [].
generate_deps_map([Module | Modules], DepsMap0, DepsMap) -->
		% Look up the module's dependencies, and determine whether
		% it has been processed yet.
	lookup_dependencies(Module, DepsMap0, no, Done, Error, ImplDeps, 
				IntDeps, FactDeps, DepsMap1),
		% If the module hadn't been processed yet, compute its
		% transitive dependencies (we already know its primary ones),
		% (1) output this module's dependencies to its `.d' file
		% (if the `.m' file exists), (2) add its imports to the list of
		% dependencies we need to generate, and (3) mark it as having
		% been processed.
	( { Done = no } ->
		{ map__set(DepsMap1, Module,
			deps(yes, Error, IntDeps, ImplDeps, FactDeps), 
			DepsMap2) },
		transitive_dependencies(ImplDeps, DepsMap2, no, SecondaryDeps,
			DepsMap3),
		( { Error \= fatal } ->
			write_dependency_file(Module, ImplDeps, SecondaryDeps,
			FactDeps)
		;
			[]
		),
		{ list__append(ImplDeps, Modules, Modules2) }
	;
		{ DepsMap3 = DepsMap1 },
		{ Modules2 = Modules }
	),
		% Recursively process the remaining modules
	generate_deps_map(Modules2, DepsMap3, DepsMap).


% Write out the `.dep' file, using the information collected in the
% deps_map data structure.

:- pred generate_dep_file(string, deps_map, io__output_stream,
			io__state, io__state).
:- mode generate_dep_file(in, in, in, di, uo) is det.

generate_dep_file(ModuleName, DepsMap, DepStream) -->
	io__write_string(DepStream,
		"# Automatically generated dependencies for module `"),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, "'.\n"),
	{ library__version(Version) },
	io__write_string(DepStream,
		"# Generated by the Mercury compiler, version "),
	io__write_string(DepStream, Version),
	io__write_string(DepStream, ".\n\n"),

	{ map__keys(DepsMap, Modules0) },
	{ select_ok_modules(Modules0, DepsMap, Modules) },

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".ms ="),
	write_dependencies_list(Modules, ".m", DepStream),
	io__write_string(DepStream, "\n\n"),

	globals__io_lookup_bool_option(assume_gmake, Gmake),
	(
		{ Gmake = yes },
		{ string__append(ModuleName, ".ms", VarName) },
		{ Basis = yes(VarName - ".m") }
	;
		{ Gmake = no },
		{ Basis = no }
	),

	{ get_extra_link_objects(Modules, DepsMap, ExtraLinkObjs) },

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nos = "),
	write_compact_dependencies_list(Modules, ".no", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".qls = "),
	write_compact_dependencies_list(Modules, ".ql", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".cs = "),
	write_compact_dependencies_list(Modules, ".c", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".os = "),
	write_compact_dependencies_list(Modules, ".o", Basis, DepStream),
	write_dependencies_list(ExtraLinkObjs, ".o", DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".pic_os = "),
	write_compact_dependencies_list(Modules, ".$(EXT_FOR_PIC_OBJECTS)",
		Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".dirs = "),
	write_compact_dependencies_list(Modules, ".dir", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".dir_os = "),
	write_compact_dependencies_list(Modules, ".dir/*.o", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".ss = "),
	write_compact_dependencies_list(Modules, ".s", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".errs = "),
	write_compact_dependencies_list(Modules, ".err", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".dates = "),
	write_compact_dependencies_list(Modules, ".date", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".date3s = "),
	write_compact_dependencies_list(Modules, ".date3", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".optdates = "),
	write_compact_dependencies_list(Modules, ".optdate", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".ds = "),
	write_compact_dependencies_list(Modules, ".d", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".hs = "),
	write_compact_dependencies_list(Modules, ".h", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".ints = "),
	write_compact_dependencies_list(Modules, ".int", Basis, DepStream),
	write_compact_dependencies_separator(Basis, DepStream),
	write_compact_dependencies_list(Modules, ".int2", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".int3s = "),
	write_compact_dependencies_list(Modules, ".int3", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".opts = "),
	write_compact_dependencies_list(Modules, ".opt", Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	globals__io_lookup_string_option(gc, GC_Opt),
	( { GC_Opt = "accurate" } ->
		{ map__init(AllMap) },
		trans_impl_dependencies([ModuleName], AllMap, yes,
			AllDeps, _),
		io__write_string(DepStream, ModuleName),
		io__write_string(DepStream, ".garbs = "),
		write_compact_dependencies_list(AllDeps, ".garb",
			Basis, DepStream),
		io__write_string(DepStream, "\n\n")
	;
		[]
	),

	( { GC_Opt = "accurate" } ->
		io__write_strings(DepStream, [
			ModuleName, "_garb.c : $(",
			ModuleName, ".garbs)\n",
                        "\t$(MLINK) $(MLINKFLAGS) -o ", ModuleName,
                        "_garb.c $(", ModuleName, ".garbs)\n\n"
		])
	;
		[]
	),

	( { GC_Opt = "accurate" } ->
		io__write_strings(DepStream, [
			ModuleName, " : $(", ModuleName, ".os) ",
			ModuleName, "_init.o ",
			ModuleName, "_garb.o\n",
		"\t$(ML) -s $(GRADE) $(MLFLAGS) -o ", ModuleName, " ",
			ModuleName, "_garb.o ",
			ModuleName, "_init.o \\\n",
			"\t$(", ModuleName, ".os) $(MLLIBS)\n\n"
		])
	;
		io__write_strings(DepStream, [
			ModuleName, " : $(", ModuleName, ".os) ",
			ModuleName, "_init.o\n",
			"\t$(ML) -s $(GRADE) $(MLFLAGS) -o ", ModuleName, " ",
			ModuleName, "_init.o \\\n",
			"\t$(", ModuleName, ".os) $(MLLIBS)\n\n"
		])
	),

	io__write_strings(DepStream, [
		ModuleName, ".split : ", ModuleName, ".split.a ",
				ModuleName, "_init.o\n",
		"\t$(ML) -s $(GRADE) $(MLFLAGS) -o ", ModuleName, ".split ",
			ModuleName, "_init.o \\\n",
			"\t", ModuleName, ".split.a $(MLLIBS)\n\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".split.a : $(", ModuleName, ".dir_os)\n",
		"\trm -f ", ModuleName, ".split.a\n",
		"\tar cr ", ModuleName, ".split.a\n",
		"\tfor dir in $(", ModuleName, ".dirs); do \\\n",
		"\t	ar q ", ModuleName, ".split.a $$dir/*.o; \\\n",
		"\tdone\n",
		"\tranlib ", ModuleName, ".split.a\n\n"
	]),

/************
% I decided to leave the rules for `foo.so' and `foo.a' out,
% mainly because the rule for `foo.so' is hard to make portable.
% The rules here would conflict with the one in library/Mmake.

	io__write_strings(DepStream, [
		ModuleName, ".so : $(", ModuleName, ".pic_os)\n",
		"\t$(LINK_SHARED_LIB) -o ", ModuleName, ".so ",
			"$(", ModuleName, ".pic_os)\n\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".a : $(", ModuleName, ".os)\n",
		"\trm -f ", ModuleName, ".a\n",
		"\tar cr ", ModuleName, ".a ",
			"$(", ModuleName, ".os)\n",
		"\tranlib ", ModuleName, ".a\n\n"
	]),
************/

	io__write_strings(DepStream, [
		ModuleName, "_init.c :\n",
		"\t$(C2INIT) $(C2INITFLAGS) $(", ModuleName, ".ms) > ",
			ModuleName, "_init.c\n\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".nu : $(", ModuleName, ".nos)\n",
		"\t$(MNL) $(MNLFLAGS) -o ", ModuleName, ".nu ",
			"$(", ModuleName, ".nos)\n\n",

		ModuleName, ".nu.debug : $(", ModuleName, ".nos)\n",
		"\t$(MNL) --debug $(MNLFLAGS) -o ", ModuleName, ".nu.debug ",
			"$(", ModuleName, ".nos)\n\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".sicstus : $(", ModuleName, ".qls)\n",
		"\t$(MSL) $(MSLFLAGS) -o ", ModuleName, ".sicstus ",
			"$(", ModuleName, ".qls)\n\n",

		ModuleName, ".sicstus.debug : $(", ModuleName, ".qls)\n",
			"\t$(MSL) --debug $(MSLFLAGS) -o ", ModuleName,
			".sicstus.debug $(", ModuleName, ".qls)\n\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".check : $(", ModuleName, ".errs)\n\n",

		ModuleName, ".ints : $(", ModuleName, ".dates)\n\n",
		ModuleName, ".int3s : $(", ModuleName, ".date3s)\n\n",
		ModuleName, ".opts : $(", ModuleName, ".optdates)\n\n"
	]),

	io__write_strings(DepStream, [
		"clean: ", ModuleName, ".clean\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".clean :\n",
		"\t-rm -rf ", ModuleName, ".dir\n",
		"\t-rm -f $(", ModuleName, ".cs) ", ModuleName, "_init.c\n",
		"\t-rm -f $(", ModuleName, ".ss) ", ModuleName, "_init.s\n",
		"\t-rm -f $(", ModuleName, ".os) ", ModuleName, "_init.o\n",
		"\t-rm -f $(", ModuleName, ".nos)\n",
		"\t-rm -f $(", ModuleName, ".qls)\n",
		"\t-rm -f $(", ModuleName, ".errs)\n"
	]),

	( { GC_Opt = "accurate" } ->
		io__write_strings(DepStream, [
			"\t-rm -f $(", ModuleName, ".garbs)\n",
			"\t-rm -f ", ModuleName, "_garb.o ", ModuleName, 
				"_garb.c ", ModuleName, "_garb.s\n\n"
		])
	;
		io__write_string(DepStream, "\n")
	),

	io__write_strings(DepStream, [
		ModuleName, ".change_clean :\n",
		"\t-rm -f $(", ModuleName, ".cs) ", ModuleName, "_init.c\n",
		"\t-rm -f $(", ModuleName, ".ss) ", ModuleName, "_init.s\n",
		"\t-rm -f $(", ModuleName, ".os) ", ModuleName, "_init.o\n",
		"\t-rm -f $(", ModuleName, ".hs)\n",
		"\t-rm -f $(", ModuleName, ".ds)\n",
		"\t-rm -f ", ModuleName, ".dep ", ModuleName, "\n",
		"\t-rm -f ", ModuleName, ".split ", ModuleName,".split.a\n\n"
	]),

	io__write_strings(DepStream, [
		"realclean: ", ModuleName, ".realclean\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".realclean : ", ModuleName, ".clean\n",
		"\t-rm -f $(", ModuleName, ".dates)\n",
		"\t-rm -f $(", ModuleName, ".date3s)\n",
		"\t-rm -f $(", ModuleName, ".optdates)\n",
		"\t-rm -f $(", ModuleName, ".ints)\n",
		"\t-rm -f $(", ModuleName, ".int3s)\n",
		"\t-rm -f $(", ModuleName, ".opts)\n",
		"\t-rm -f $(", ModuleName, ".ds)\n",
		"\t-rm -f $(", ModuleName, ".hs)\n"
	]),
	io__write_strings(DepStream, [
		"\t-rm -f ",
			ModuleName, " ",
			ModuleName, ".split ",
			ModuleName, ".split.a ",
			ModuleName, ".nu ",
			ModuleName, ".nu.save ",
			ModuleName, ".nu.debug.save ",
			ModuleName, ".nu.debug ",
			ModuleName, ".sicstus ",
			ModuleName, ".sicstus.debug ",
			ModuleName, ".dep\n\n"
	]),
	io__write_strings(DepStream, [
		"clean_nu: ", ModuleName, ".clean_nu\n",
		ModuleName, ".clean_nu :\n",
		"\t-rm -f $(", ModuleName, ".nos)\n\n",

		"clean_sicstus: ", ModuleName, ".clean_sicstus\n",
		ModuleName, ".clean_sicstus :\n",
		"\t-rm -f $(", ModuleName, ".qls)\n\n"
	]).

%-----------------------------------------------------------------------------%
	% get_extra_link_objects(Modules, DepsMap, ExtraLinkObjs) },
	% Find any extra .o files that should be linked into the executable.
	% Currently only looks for fact table object files.
:- pred get_extra_link_objects(list(string), deps_map, list(string)).
:- mode get_extra_link_objects(in, in, out) is det.

get_extra_link_objects(Modules, DepsMap, ExtraLinkObjs) :-
	get_extra_link_objects_2(Modules, DepsMap, [], ExtraLinkObjs).

:- pred get_extra_link_objects_2(list(string), deps_map, 
		list(string), list(string)).
:- mode get_extra_link_objects_2(in, in, in, out) is det.

get_extra_link_objects_2([], _DepsMap, ExtraLinkObjs, ExtraLinkObjs).
get_extra_link_objects_2([Module | Modules], DepsMap, 
		ExtraLinkObjs0, ExtraLinkObjs) :-
	map__lookup(DepsMap, Module, deps(_, _, _, _, ObjList)),
	list__append(ObjList, ExtraLinkObjs0, ExtraLinkObjs1),
	get_extra_link_objects_2(Modules, DepsMap, ExtraLinkObjs1, 
		ExtraLinkObjs).

%-----------------------------------------------------------------------------%

:- pred select_ok_modules(list(string), deps_map, list(string)).
:- mode select_ok_modules(in, in, out) is det.

select_ok_modules([], _, []).
select_ok_modules([Module | Modules0], DepsMap, Modules) :-
	map__lookup(DepsMap, Module, deps(_, Error, _, _, _)),
	( Error = fatal ->
		Modules = Modules1
	;
		Modules = [Module | Modules1]
	),
	select_ok_modules(Modules0, DepsMap, Modules1).

%-----------------------------------------------------------------------------%

:- pred write_dependencies_list(list(string), string, io__output_stream,
				io__state, io__state).
:- mode write_dependencies_list(in, in, in, di, uo) is det.

write_dependencies_list([], _, _) --> [].
write_dependencies_list([Module | Modules], Suffix, DepStream) -->
	io__write_string(DepStream, " \\\n\t"),
	io__write_string(DepStream, Module),
	io__write_string(DepStream, Suffix),
	write_dependencies_list(Modules, Suffix, DepStream).

%-----------------------------------------------------------------------------%

:- pred write_compact_dependencies_list(list(string), string,
	maybe(pair(string)), io__output_stream, io__state, io__state).
:- mode write_compact_dependencies_list(in, in, in, in, di, uo) is det.

write_compact_dependencies_list(Modules, Suffix, no, DepStream) -->
	write_dependencies_list(Modules, Suffix, DepStream).
write_compact_dependencies_list(_Modules, Suffix, yes(VarName - OldSuffix),
		DepStream) -->
	io__write_string(DepStream, "$("),
	io__write_string(DepStream, VarName),
	io__write_string(DepStream, ":"),
	io__write_string(DepStream, OldSuffix),
	io__write_string(DepStream, "="),
	io__write_string(DepStream, Suffix),
	io__write_string(DepStream, ")").

:- pred write_compact_dependencies_separator(maybe(pair(string)),
	io__output_stream, io__state, io__state).
:- mode write_compact_dependencies_separator(in, in, di, uo) is det.

write_compact_dependencies_separator(no, _DepStream) --> [].
write_compact_dependencies_separator(yes(_), DepStream) -->
	io__write_string(DepStream, " ").

%-----------------------------------------------------------------------------%

	% Given a list of modules, return a list of those modules
	% and all their transitive interface dependencies.

:- pred transitive_dependencies(list(string), deps_map, bool, list(string), 
				deps_map, io__state, io__state).
:- mode transitive_dependencies(in, in, in, out, out, di, uo) is det.

transitive_dependencies(Modules, DepsMap0, Search, Dependencies, DepsMap) -->
	{ set__init(Dependencies0) },
	transitive_dependencies_2(Modules, Dependencies0, DepsMap0, Search,
		Dependencies1, DepsMap),
	{ set__to_sorted_list(Dependencies1, Dependencies) }.

:- pred transitive_dependencies_2(list(string), set(string), deps_map, bool,
				set(string), deps_map,
				io__state, io__state).
:- mode transitive_dependencies_2(in, in, in, in, out, out, di, uo) is det.

transitive_dependencies_2([], Deps, DepsMap, _Search, Deps, DepsMap) --> [].
transitive_dependencies_2([Module | Modules0], Deps0, DepsMap0, Search, Deps, 
		DepsMap)
		-->
	( { set__member(Module, Deps0) } ->
		{ Deps1 = Deps0 },
		{ DepsMap1 = DepsMap0 },
		{ Modules1 = Modules0 }
	;
		{ set__insert(Deps0, Module, Deps1) },
		lookup_dependencies(Module, DepsMap0, Search, _, _, 
			IntDeps, _ImplDeps, _FactDeps, DepsMap1),
		{ list__append(IntDeps, Modules0, Modules1) }
	),
	transitive_dependencies_2(Modules1, Deps1, DepsMap1, Search, Deps, 
		DepsMap).

%-----------------------------------------------------------------------------%

        % Given a list of modules, return a list of those modules
        % and all their transitive implementation dependencies.

:- pred trans_impl_dependencies(list(string), deps_map, bool,
                        list(string), deps_map, io__state, io__state).
:- mode trans_impl_dependencies(in, in, in, out, out, di, uo) is det.

trans_impl_dependencies(Modules, DepsMap0, Search, Dependencies, DepsMap) -->
        { set__init(Dependencies0) },
        trans_impl_dependencies_2(Modules, Dependencies0, DepsMap0, Search,
                Dependencies1, DepsMap),
        { set__to_sorted_list(Dependencies1, Dependencies) }.

:- pred trans_impl_dependencies_2(list(string), set(string), deps_map, bool,
                                set(string), deps_map,
                                io__state, io__state).
:- mode trans_impl_dependencies_2(in, in, in, in, out, out, di, uo) is det.

trans_impl_dependencies_2([], Deps, DepsMap, _Search, Deps, DepsMap) --> [].
trans_impl_dependencies_2([Module | Modules0], Deps0, DepsMap0, Search, Deps,
                DepsMap)
                -->
        ( { set__member(Module, Deps0) } ->
                { Deps1 = Deps0 },
                { DepsMap1 = DepsMap0 },
                { Modules2 = Modules0 }
        ;
                { set__insert(Deps0, Module, Deps1) },
                lookup_dependencies(Module, DepsMap0, Search, _, _, IntDeps, 
                	ImplDeps, _FactDeps, DepsMap1),
                { list__append(IntDeps, Modules0, Modules1) },
                { list__append(ImplDeps, Modules1, Modules2) }
        ),
        trans_impl_dependencies_2(Modules2, Deps1, DepsMap1, Search, Deps,
                DepsMap).

%-----------------------------------------------------------------------------%

	% Look up a module in the dependency map
	% If we don't know its dependencies, read the
	% module and save the dependencies in the dependency map.

:- pred lookup_dependencies(string, deps_map, bool,
		bool, module_error, list(string), list(string), list(string),
		deps_map, io__state, io__state).
:- mode lookup_dependencies(in, in, in, out, out, out, out, out, out, 
		di, uo) is det.

lookup_dependencies(Module, DepsMap0, Search, Done, Error, IntDeps, 
		ImplDeps, FactDeps, DepsMap)
		-->
	(
		{ map__search(DepsMap0, Module,
			deps(Done0, Error0, IntDeps0, ImplDeps0, FactDeps0)) }
	->
		{ Done = Done0 },
		{ Error = Error0 },
		{ IntDeps = IntDeps0 },
		{ ImplDeps = ImplDeps0 },
		{ FactDeps = FactDeps0 },
		{ DepsMap = DepsMap0 }
	;
		read_dependencies(Module, Search, ImplDeps, IntDeps, FactDeps, 
				Error),
		{ map__set(DepsMap0, Module, 
		    deps(no, Error, IntDeps, ImplDeps, FactDeps), DepsMap) },
		{ Done = no }
	).

	% Read a module to determine its dependencies.

:- pred read_dependencies(string, bool, list(string), list(string),
			list(string), module_error, io__state, io__state).
:- mode read_dependencies(in, in, out, out, out, out, di, uo) is det.

read_dependencies(Module, Search, InterfaceDeps, ImplementationDeps, 
		FactTableDeps, Error) -->
	io__gc_call(read_mod_ignore_errors(Module, ".m",
			"Getting dependencies for module", Search, Items0, Error)),
	( { Items0 = [], Error = fatal } ->
		io__gc_call(read_mod_ignore_errors(Module, ".int", 
		    "Getting dependencies for module interface", Search, 
		    Items, _Error))
	;
		{ Items = Items0 }
	),
	{ get_dependencies(Items, ImplementationDeps0) },
	{ get_interface(Items, no, InterfaceItems) },
	{ get_dependencies(InterfaceItems, InterfaceDeps) },
	{ ImplementationDeps = ["mercury_builtin" | ImplementationDeps0] },
	{ get_fact_table_dependencies(Items, FactTableDeps) }.

%-----------------------------------------------------------------------------%

read_mod(ModuleName, Extension, Descr, Search, Items, Error) -->
	{ dir__basename(ModuleName, Module) },
	{ string__append(ModuleName, Extension, FileName) },
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	maybe_write_string(VeryVerbose, "% "),
	maybe_write_string(VeryVerbose, Descr),
	maybe_write_string(VeryVerbose, " `"),
	maybe_write_string(VeryVerbose, FileName),
	maybe_write_string(VeryVerbose, "'... "),
	maybe_flush_output(VeryVerbose),
	prog_io__read_module(FileName, Module, Search, Error, Messages, Items),
	( { Error = fatal } ->
		maybe_write_string(VeryVerbose, "fatal error(s).\n"),
		io__set_exit_status(1)
	; { Error = yes } ->
		maybe_write_string(VeryVerbose, "parse error(s).\n"),
		io__set_exit_status(1)
	;
		maybe_write_string(VeryVerbose, "successful parse.\n")
	),
	prog_out__write_messages(Messages).

/*
:- pred combine_module_errors(module_error, module_error, module_error).
:- mode combine_module_errors(in, in, out) is det.

combine_module_errors(fatal, _, fatal).
combine_module_errors(yes, fatal, fatal).
combine_module_errors(yes, yes, yes).
combine_module_errors(yes, no, yes).
combine_module_errors(no, Error, Error).
*/

read_mod_ignore_errors(ModuleName, Extension, Descr, Search, Items, Error) -->
	{ dir__basename(ModuleName, Module) },
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	maybe_write_string(VeryVerbose, "% "),
	maybe_write_string(VeryVerbose, Descr),
	maybe_write_string(VeryVerbose, " `"),
	maybe_write_string(VeryVerbose, Module),
	maybe_write_string(VeryVerbose, "'... "),
	maybe_flush_output(VeryVerbose),
	{ string__append(ModuleName, Extension, FileName) },
	prog_io__read_module(FileName, Module, Search, Error, _Messages, Items),
	maybe_write_string(VeryVerbose, "done.\n").

:- pred read_mod_short_interface(string, string, string, bool, item_list, 
		module_error, io__state, io__state).
:- mode read_mod_short_interface(in, in, in, in, out, out, di, uo) is det.
read_mod_short_interface(Module, Ext, Descr, Search, Items, Error) -->
	read_mod(Module, Ext, Descr, Search, Items, Error).

:- pred read_mod_interface(string, string, bool, item_list, module_error,
				io__state, io__state).
:- mode read_mod_interface(in, in, in, out, out, di, uo) is det.

read_mod_interface(Module, Descr, Search, Items, Error) -->
	read_mod(Module, ".int", Descr, Search, Items, Error).

%-----------------------------------------------------------------------------%

process_module_interfaces([], IndirectImports, Module0, Module) -->
	process_module_short_interfaces(IndirectImports, Module0, Module).

process_module_interfaces([Import | Imports], IndirectImports0, Module0, Module)
		-->
	{ Module0 = module_imports(ModuleName, DirectImports0,
				OldIndirectImports, Items0, Error0) },
	(
		{ Import = ModuleName }
	->
		( { ModuleName = "mercury_builtin" } ->
			[]
		;
			globals__io_lookup_bool_option(inhibit_warnings,
							NoWarn),
			( { NoWarn = no } ->
				{ term__context_init(ModuleName, 1, Context) },
				prog_out__write_context(Context),
				io__write_string(
					"Warning: module imports itself!\n")
			;
				[]
			)
		),
		process_module_interfaces(Imports, IndirectImports0,
					Module0, Module)
	;
		{ list__member(Import, DirectImports0) }
	->
		process_module_interfaces(Imports, IndirectImports0,
					Module0, Module)
	;
		io__gc_call(
			read_mod_interface(Import,
				"Reading interface for module", yes, 
				LongIntItems1, Error1)
		),
		% strip off the `:- interface' declaration at the start, if any
		{
			LongIntItems1 = [ FirstItem | LongIntItems2 ],
			FirstItem = module_defn(_, interface) - _
		->
			Items1 = LongIntItems2
		;
			Items1 = LongIntItems1
		},
		{ ( Error1 \= no ->
			Error2 = yes
		;
			Error2 = Error0
		) },

		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),

		{ get_dependencies(Items1, IndirectImports1) },
		( { Error1 = fatal } ->
			{ DirectImports1 = DirectImports0 }
		;
			{ DirectImports1 = [Import | DirectImports0] }
		),
		{ list__append(IndirectImports0, IndirectImports1,
			IndirectImports2) },
		{ list__append(Items0, Items1, Items2) },
		{ Module1 = module_imports(ModuleName, DirectImports1, 
					OldIndirectImports, Items2, Error2) },
		process_module_interfaces(Imports, IndirectImports2,
				Module1, Module)
	).

%-----------------------------------------------------------------------------%

:- pred process_module_short_interfaces(list(string),
		module_imports, module_imports, io__state, io__state).
:- mode process_module_short_interfaces(in, in, out, di, uo) is det.

process_module_short_interfaces(Imports, Module0, Module) -->
	process_module_short_interfaces(Imports, ".int2", Module0, Module).


:- pred process_module_short_interfaces(list(string), string, 
		module_imports, module_imports, io__state, io__state).
:- mode process_module_short_interfaces(in, in, in, out, di, uo) is det.

process_module_short_interfaces([], _, Module, Module) --> [].
process_module_short_interfaces([Import | Imports], Ext, Module0, Module) -->
	{ Module0 = module_imports(ModuleName, DirectImports, IndirectImports0,
			Items0, Error0) },
	(
		% check if the imported module has already been imported
		{ Import = ModuleName
		; list__member(Import, DirectImports)
		; list__member(Import, IndirectImports0)
		}
	->
		process_module_short_interfaces(Imports, Ext, Module0, Module)
	;
		io__gc_call(
			read_mod_short_interface(Import, Ext,
				"Reading short interface for module", yes,
					ShortIntItems1, Error1)
		),
		% strip off the `:- interface' declaration at the start, if any
		{
			ShortIntItems1 = [ FirstItem | ShortIntItems2 ],
			FirstItem = module_defn(_, interface) - _
		->
			Items1 = ShortIntItems2
		;
			Items1 = ShortIntItems1
		},
		{ Error1 \= no ->
			Error2 = yes
		;
			Error2 = Error0
		},

		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),

		{ get_dependencies(Items1, Imports1) },
		{ list__append(Imports, Imports1, Imports2) },
		{ list__append(Items0, Items1, Items2) },
		{ IndirectImports1 = [Import | IndirectImports0] },
		{ Module1 = module_imports(ModuleName, DirectImports,
			IndirectImports1, Items2, Error2) },
		process_module_short_interfaces(Imports2, Ext, Module1, Module)
	).

%-----------------------------------------------------------------------------%

get_dependencies(Items, Deps) :-
	get_dependencies_2(Items, [], Deps).

:- pred get_dependencies_2(item_list, list(string), list(string)).
:- mode get_dependencies_2(in, in, out) is det.

get_dependencies_2([], Deps, Deps).
get_dependencies_2([Item - _Context | Items], Deps0, Deps) :-
	( 
		Item = module_defn(_VarSet, import(module(Modules)))
	->
		list__append(Deps0, Modules, Deps1)
	;
		Deps1 = Deps0
	),
	get_dependencies_2(Items, Deps1, Deps).

%-----------------------------------------------------------------------------%
	% get the fact table dependencies for a module
:- pred get_fact_table_dependencies(item_list, list(string)).
:- mode get_fact_table_dependencies(in, out) is det.

get_fact_table_dependencies(Items, Deps) :-
	get_fact_table_dependencies_2(Items, [], Deps).


:- pred get_fact_table_dependencies_2(item_list, list(string), list(string)).
:- mode get_fact_table_dependencies_2(in, in, out) is det.

get_fact_table_dependencies_2([], Deps, Deps).
get_fact_table_dependencies_2([Item - _Context | Items], Deps0, Deps) :-
	(
		Item = pragma(fact_table(_SymName, _Arity, FileName))
	->
		Deps1 = [FileName | Deps0]
	;
		Deps1 = Deps0
	),
	get_fact_table_dependencies_2(Items, Deps1, Deps).

%-----------------------------------------------------------------------------%

	% Given a module (well, a list of items), extract the interface
	% part of that module, i.e. all the items between `:- interface'
	% and `:- implementation'. If IncludeImported is yes, also
	% include all items after a `:- imported'. This is useful for
	% making the .int file.

:- pred get_interface(item_list, bool, item_list).
:- mode get_interface(in, in, out) is det.
get_interface(Items0, IncludeImported, Items) :-
	get_interface_2(Items0, no, IncludeImported, [], RevItems),
	list__reverse(RevItems, Items).

:- pred get_interface_2(item_list, bool, bool, item_list, item_list).
:- mode get_interface_2(in, in, in, in, out) is det.

get_interface_2([], _, _, Items, Items).
get_interface_2([Item - Context | Rest], InInterface0,
				IncludeImported, Items0, Items) :-
	( Item = module_defn(_, interface) ->
		Items1 = Items0,
		InInterface1 = yes
	; Item = module_defn(_, imported) ->
		% module_qual.m needs the :- imported declaration.
		( IncludeImported = yes ->
			InInterface1 = yes,
			Items1 = [Item - Context | Items0]
		;
			InInterface1 = no,
			Items1 = Items0
		)
	; Item = module_defn(_, implementation) ->
		Items1 = Items0,
		InInterface1 = no
	;
		( InInterface0 = yes ->
			Items1 = [Item - Context | Items0]
		;
			Items1 = Items0
		),
		InInterface1 = InInterface0
	),
	get_interface_2(Rest, InInterface1, IncludeImported, Items1, Items).

	% Given a module interface (well, a list of items), extract the
	% short interface part of that module, i.e. the exported
	% type/inst/mode declarations, but not the exported pred or
	% constructor declarations.  If the module interface imports
	% other modules, then the short interface only needs to include
	% those import_module declarations only if the short interface
	% contains some equivalence types or some mode or inst definitions
	% that might use declarations in the imported modules.
	% If the short interface is empty, or only contains abstract
	% type declarations, then it doesn't need any import_module
	% declarations.

:- pred get_short_interface(item_list, item_list).
:- mode get_short_interface(in, out) is det.

get_short_interface(Items0, Items) :-
	get_short_interface_2(Items0, [], [], no,
			RevItems, RevImports, NeedsImports),
	list__reverse(RevItems, Items1),
	( NeedsImports = yes ->
		list__reverse(RevImports, Imports1),
		list__append(Imports1, Items1, Items)
	;
		Items = Items1
	).

:- pred get_short_interface_2(item_list, item_list, item_list, bool,
				item_list, item_list, bool).
:- mode get_short_interface_2(in, in, in, in, out, out, out) is det.

get_short_interface_2([], Items, Imports, NeedsImports,
			Items, Imports, NeedsImports).
get_short_interface_2([ItemAndContext | Rest], Items0, Imports0, NeedsImports0,
			Items, Imports, NeedsImports) :-
	ItemAndContext = Item0 - Context,
	( Item0 = module_defn(_, import(_)) ->
		Items1 = Items0,
		Imports1 = [ItemAndContext | Imports0],
		NeedsImports1 = NeedsImports0
	; make_abstract_type_defn(Item0, Item1) ->
		Imports1 = Imports0,
		Items1 = [Item1 - Context | Items0],
		NeedsImports1 = NeedsImports0
	; include_in_short_interface(Item0) ->
		Imports1 = Imports0,
		Items1 = [ItemAndContext | Items0],
		NeedsImports1 = yes
	;
		Items1 = Items0,
		Imports1 = Imports0,
		NeedsImports1 = NeedsImports0
	),
	get_short_interface_2(Rest, Items1, Imports1, NeedsImports1,
				Items, Imports, NeedsImports).

:- pred include_in_short_interface(item).
:- mode include_in_short_interface(in) is semidet.

include_in_short_interface(type_defn(_, _, _)).
include_in_short_interface(inst_defn(_, _, _)).
include_in_short_interface(mode_defn(_, _, _)).
include_in_short_interface(module_defn(_, _)).

:- pred make_abstract_type_defn(item, item).
:- mode make_abstract_type_defn(in, out) is semidet.

make_abstract_type_defn(type_defn(VarSet, du_type(Name, Args, _Ctors), Cond),
			type_defn(VarSet, abstract_type(Name, Args), Cond)).
make_abstract_type_defn(type_defn(VarSet, abstract_type(Name, Args), Cond),
			type_defn(VarSet, abstract_type(Name, Args), Cond)).

%-----------------------------------------------------------------------------%

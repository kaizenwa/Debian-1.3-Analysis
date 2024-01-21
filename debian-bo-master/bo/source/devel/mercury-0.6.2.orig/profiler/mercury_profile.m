%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Mercury profiler
% Main author: petdr.
%
% Notes:
%	Processes the Prof.* and the *.prof files to produce an output very
%	similar to 'gprof'
%
%	Based on the profiling scheme described in XXX
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mercury_profile.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module process_file, call_graph, generate_output, propagate, output.
:- import_module prof_info.
:- import_module bool, list, std_util, string, library.
:- import_module options, getopt, globals.
:- import_module relation.
:- import_module prof_debug.

%-----------------------------------------------------------------------------%


main -->
	io__command_line_arguments(Args0),
	{ OptionOps = option_ops(short_option, long_option, option_defaults) },
	{ getopt__process_options(OptionOps, Args0, Args, Result0) },
	postprocess_options(Result0, Args, Result),
	main_2(Result, Args).


:- pred postprocess_options(maybe_option_table(option), list(string),
	maybe(string), io__state, io__state).
:- mode postprocess_options(in, in, out, di, uo) is det.

postprocess_options(error(ErrorMessage), _Args, yes(ErrorMessage)) --> [].
postprocess_options(ok(OptionTable), Args, no) --> 
	globals__io_init(OptionTable),

	% --very-verbose implies --verbose
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	(
		{ VeryVerbose = yes }
	->
		 globals__io_set_option(verbose, bool(yes))
	;
		[]
	),


	% Any empty list of arguments implies that we must build the call
	% graph from the dynamic information.
	(
		{ Args = [] }
	->
		globals__io_set_option(dynamic_cg, bool(yes))
	;
		[]
	).


        % Display error message and then usage message
:- pred usage_error(string::in, io__state::di, io__state::uo) is det.
usage_error(ErrorMessage) -->
        io__progname_base("mercury_profile", ProgName),
        io__stderr_stream(StdErr),
        io__write_string(StdErr, ProgName),
        io__write_string(StdErr, ": "),
        io__write_string(StdErr, ErrorMessage),
        io__write_string(StdErr, "\n"),
        io__set_exit_status(1),
        usage.


        % Display usage message
:- pred usage(io__state::di, io__state::uo) is det.
usage -->
        io__progname_base("mprof", ProgName),
        io__stderr_stream(StdErr),
	{ library__version(Version) },
        io__write_strings(StdErr,
		["Mercury Profiler, version ", Version, "\n"]),
        io__write_string(StdErr, "Copyright (C) 1995 University of Melbourne\n"),
        io__write_string(StdErr, "Usage: "),
        io__write_string(StdErr, ProgName),
        io__write_string(StdErr, " [<options>] [<files ...>]\n"),
        io__write_string(StdErr, "Use `"),
        io__write_string(StdErr, ProgName),
        io__write_string(StdErr, " --help' for more information.\n").

:- pred long_usage(io__state::di, io__state::uo) is det.
long_usage -->
        io__progname_base("mprof", ProgName),
	{ library__version(Version) },
        io__write_strings(["Mercury Profiler, version ", Version, "\n"]),
        io__write_string("Copyright (C) 1995 University of Melbourne\n"),
        io__write_string("Usage: "),
        io__write_string(ProgName),
        io__write_string(" [<options>] [<files ...>]\n"),

	io__write_string("Files:\n"),
	io__write_string("\tFiles that contain the static call graph, need one for every module \n"),
	io__write_string("\tin the program\n\n"),

        io__write_string("Options:\n"),
        options_help.


%-----------------------------------------------------------------------------%


:- pred main_2(maybe(string), list(string), io__state, io__state).
:- mode main_2(in, in, di, uo) is det.

main_2(yes(ErrorMessage), _) -->
        usage_error(ErrorMessage).
main_2(no, Args) -->
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, StdOut),
	globals__io_lookup_bool_option(call_graph, CallGraphOpt),
        globals__io_lookup_bool_option(help, Help),
        (
                { Help = yes }
        ->
                long_usage
        ;
		globals__io_lookup_bool_option(verbose, Verbose),

		maybe_write_string(Verbose, "% Processing input files..."),
		process_file__main(Prof0, CallGraph0),
		maybe_write_string(Verbose, " done\n"),
		
		(
			{ CallGraphOpt = yes }
		->
			maybe_write_string(Verbose, "% Building call graph..."),
			call_graph__main(Args, CallGraph0, CallGraph),
			maybe_write_string(Verbose, " done\n"),

			maybe_write_string(Verbose, "% Propagating counts..."),
			propagate__counts(CallGraph, Prof0, Prof),
			maybe_write_string(Verbose, " done\n")
		;
			{ Prof = Prof0 }
		),
		
		maybe_write_string(Verbose, "% Generating output..."),
		generate_output__main(Prof, IndexMap, OutputProf),
		maybe_write_string(Verbose, " done\n"),

		io__set_output_stream(StdOut, _),
		output__main(OutputProf, IndexMap),
		io__write_char('\n'),
		io__told
        ).


%-----------------------------------------------------------------------------%

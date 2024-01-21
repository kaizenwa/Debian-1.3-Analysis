%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: getopt.m
% Authors: fjh, zs
% Stability: medium

% This module exports the predicate getopt__process_options/4,
% which can be used to parse command-line options.
%
% This version allows both short (single-character) options
% and GNU-style long options. It also has the GNU extension
% of recognizing options anywhere in the command-line, not
% just at the start.
%
% To use this module, you must provide an `option' type which
% is an enumeration of all your different options.
% You must provide predicates `short_option(Char, Option)'
% and `long_option(String, Option)' which convert the short
% and/or long names for the option to this enumeration type.
% (An option can have as many names as you like, long or short.)
% You must provide a predicate `option_default(Option, OptionData)'
% which specifies both the type and the default value for every option.
% We support four different option types: bool, int, string, and
% "accumulating" (which accumulates a list of strings).
% For the first three option types, if there are multiple occurrences
% of the same option on the command-line, then the last (right-most)
% occurrence will take precedence.  For "accumulating" options,
% multiple occurrences will be appended together into a list.
% Single-character boolean options can be negated by following them
% with another `-', e.g. `-x-' will negate the `-x' option.
% Long boolean options can be negated by preceding them with `--no-',
% e.g. `--no-foo' will negate the `--foo' option.

:- module getopt.
:- interface.
:- import_module int, string, bool, list, map, std_util.

% getopt__process_options(OptionOps, Args, NonOptionArgs, Result)
%
%	Scans through 'Args' looking for options, places all the
%	non-option arguments in 'NonOptionArgs', and records the
%	options in the OptionTable.  OptionTable is a map from 
%	a user-defined option type to option_data.
%	If an invalid option is encountered, we return error(Message)
%	otherwise we return ok(OptionTable) in 'Result'.
% 
%	The argument `OptionOps' is a structure holding three or four
%	predicates used to categorize a set of options. Their
%	interfaces should be like these:
%
% :- pred short_option(character::in, option::out) is semidet.
% 	True if the character names a valid single-character option.
%
% :- pred long_option(string::in, option::out) is semidet.
%	True if the character names a valid long option.
%
% :- pred option_default(option::out, option_data::out) is nondet.
%	Nondeterministically returns all the options with their
%	corresponding types and default values.
%
% :- pred special_handler(option::in, special_data::in,
%	option_table::in, maybe_option_table::out) is semidet.
%	This predicate is invoked whenever getopt finds an option
%	(long or short) designated as special, with special_data holding
%	the argument of the option (if any). The predicate can change the
%	option table in arbitrary ways in the course of handling the option,
%	or it can return an error message.
%	The canonical examples of special options are -O options in compilers,
%	which set many other options at once.

:- pred getopt__process_options(
		option_ops(OptionType)::in(option_ops),
		list(string)::in,
		list(string)::out,
		maybe_option_table(OptionType)::out
	) is det.

:- type option_ops(OptionType)
	--->	option_ops(
			pred(character, OptionType),	% short_option
			pred(string, OptionType),	% long_option
			pred(OptionType, option_data)	% option_default
		)
	;	option_ops(
			pred(character, OptionType),	% short_option
			pred(string, OptionType),	% long_option
			pred(OptionType, option_data),	% option_default
			pred(OptionType, special_data,	% special option handler
				option_table(OptionType),
				maybe_option_table(OptionType))
		).

:- inst option_ops =
	bound((
		option_ops(
			pred(in, out) is semidet,	% short_option
			pred(in, out) is semidet,	% long_option
			pred(out, out) is nondet	% option_default
		)
	;	option_ops(
			pred(in, out) is semidet,	% short_option
			pred(in, out) is semidet,	% long_option
			pred(out, out) is nondet,	% option_default
			pred(in, in, in, out) is semidet% special handler
		)
	)).

:- type option_data
	--->	bool(bool)
	;	int(int)
	;	string(string)
	;	accumulating(list(string))
	;	special
	;	bool_special
	;	int_special
	;	string_special.

:- type special_data
	--->	none
	;	bool(bool)
	;	int(int)
	;	string(string).

:- type option_table(OptionType)
	==	map(OptionType, option_data).

:- type maybe_option_table(OptionType)
	--->	ok(option_table(OptionType))
	;	error(string).

	% The following three predicates search the option table for
	% an option of the specified type; if it is not found, they
	% report an error by calling error/1.

:- pred getopt__lookup_bool_option(option_table(Option), Option, bool).
:- mode getopt__lookup_bool_option(in, in, out) is det.

:- pred getopt__lookup_int_option(option_table(Option), Option, int).
:- mode getopt__lookup_int_option(in, in, out) is det.

:- pred getopt__lookup_string_option(option_table(Option), Option, string).
:- mode getopt__lookup_string_option(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require.

getopt__process_options(OptionOps, Args0, Args, Result) :-
	getopt__get_option_defaults(OptionOps, OptionDefaultsPred),
	solutions(lambda([OptionDataPair::out] is nondet, (
			OptionDataPair = Option - OptionData,
			call(OptionDefaultsPred, Option, OptionData)
		)), OptionDefaultsList),
	map__from_assoc_list(OptionDefaultsList, OptionTable0),
	getopt__process_arguments(Args0, Args, OptionOps, OptionTable0, Result).

:- pred getopt__process_arguments(list(string)::in, list(string)::out,
	option_ops(OptionType)::in(option_ops), option_table(OptionType)::in,
	maybe_option_table(OptionType)::out) is det.

getopt__process_arguments([], [], _, OptionTable, ok(OptionTable)).
getopt__process_arguments([Option | Args0], Args, OptionOps,
		OptionTable0, Result) :-
	( Option = "--" ->	% "--" terminates option processing
		Args = Args0,
		Result = ok(OptionTable0)
	; string__append("--no-", LongOption, Option) ->
		getopt__get_long_options(OptionOps, LongOptionPred),
		( call(LongOptionPred, LongOption, Flag) ->
			string__append("--", LongOption, OptName),
			process_negated_bool_option(OptName, Flag,
				OptionOps, OptionTable0, Result1),
			( Result1 = ok(OptionTable1) ->
				getopt__process_arguments(Args0, Args,
					OptionOps, OptionTable1, Result)
			;
				Result = Result1,
				Args = Args0
			)
		;
			string__append_list(["unrecognized option `",
				Option, "'"], ErrorMsg),
			Result = error(ErrorMsg),
			Args = Args0
		)
	; string__append("--", LongOptionStr, Option) ->
		getopt__get_long_options(OptionOps, LongOptionPred),
		( string__sub_string_search(LongOptionStr, "=", OptionLen) ->
			string__split(LongOptionStr, OptionLen,
				LongOption, EqualOptionArg),
			( string__first_char(EqualOptionArg, '=', OptionArg) ->
				MaybeArg = yes(OptionArg)
			;
				error("bad split of --longoption=arg")
			)
		;
			LongOption = LongOptionStr,
			MaybeArg = no
		),
	  	( call(LongOptionPred, LongOption, Flag) ->
			( map__search(OptionTable0, Flag, OptionData) ->
				getopt__handle_long_option(Option, Flag,
					OptionData, MaybeArg, Args0, Args,
					OptionOps, OptionTable0, Result)
			;
				string__append_list(["unknown type for option `",
					Option, "'"], ErrorMsg),
				Result = error(ErrorMsg),
				Args = Args0
			)
		;
			string__append("unrecognized option `", Option, Tmp),
			string__append(Tmp, "'", ErrorMsg),
			Result = error(ErrorMsg),
			Args = Args0
		)
	; string__first_char(Option, '-', ShortOptions) ->
		string__to_char_list(ShortOptions, ShortOptionsList),
		% Process a single negated option `-x-'.
		( ShortOptionsList = [SingleShortOpt, '-'] ->
			getopt__get_short_options(OptionOps, ShortOptionPred),
			( call(ShortOptionPred, SingleShortOpt, Flag) ->
				string__from_char_list(['-', SingleShortOpt],
					OptName),
				process_negated_bool_option(OptName, Flag,
					OptionOps, OptionTable0, Result1),
				( Result1 = ok(OptionTable1) ->
					getopt__process_arguments(Args0, Args,
						OptionOps, OptionTable1, Result)
				;
					Result = Result1,
					Args = Args0
				)
			;
				string__append_list(["unrecognized option `-",
					ShortOptions, "'"], ErrorMsg),
				Result = error(ErrorMsg),
				Args = Args0
			)
		;
			% Process a list of options `-xyz'.
			% -xyz may be several boolean options
			% or part of it may be the argument of an option.
			% The first element of Args0 may also be an argument
			% of an option.
			getopt__handle_short_options(ShortOptionsList,
				OptionOps, Args0, Args1, OptionTable0, Result1),
			( Result1 = ok(OptionTable1) ->
				getopt__process_arguments(Args1, Args,
					OptionOps, OptionTable1, Result)
			;
				Result = Result1,
				Args = Args0
			)
		)
	;
		% It's a normal non-option argument.
		% As a GNU extension, keep searching for options
		% in the remaining arguments.
		getopt__process_arguments(Args0, Args1, OptionOps,
			OptionTable0, Result),
		Args = [Option | Args1]
	).

:- pred getopt__handle_long_option(string::in, OptionType::in, option_data::in,
	maybe(string)::in, list(string)::in, list(string)::out,
	option_ops(OptionType)::in(option_ops), option_table(OptionType)::in,
	maybe_option_table(OptionType)::out) is det.

getopt__handle_long_option(Option, Flag, OptionData, MaybeOptionArg0,
		Args0, Args, OptionOps, OptionTable0, Result) :-
	(
		getopt__need_arg(OptionData, yes),
		MaybeOptionArg0 = no
	->
		( Args0 = [Arg | ArgsTail] ->
			MaybeOptionArg = yes(Arg),
			Args1 = ArgsTail,
			MissingArg = no
		;
			MaybeOptionArg = no,
			Args1 = Args0,
			MissingArg = yes
		)
	;
		MaybeOptionArg = MaybeOptionArg0,
		Args1 = Args0,
		MissingArg = no
	),
	( MissingArg = yes ->
		Args = Args0,
		string__append_list(["option `", Option,
			"' needs an argument"],
			ErrorMsg),
		Result = error(ErrorMsg)
	;
		getopt__process_option(OptionData, Option, Flag,
			MaybeOptionArg, OptionOps, OptionTable0, Result1),
		( Result1 = ok(OptionTable1) ->
			getopt__process_arguments(Args1, Args,
				OptionOps, OptionTable1, Result)
		;
			Result = Result1,
			Args = Args1
		)
	).

:- pred getopt__handle_short_options(list(character)::in,
	option_ops(OptionType)::in(option_ops), list(string)::in,
	list(string)::out, option_table(OptionType)::in,
	maybe_option_table(OptionType)::out) is det.

getopt__handle_short_options([], _, Args, Args, OptionTable, ok(OptionTable)).
getopt__handle_short_options([Opt | Opts0], OptionOps, Args0, Args,
		OptionTable0, Result) :-
	getopt__get_short_options(OptionOps, ShortOptionPred),
	( call(ShortOptionPred, Opt, Flag) ->
		( map__search(OptionTable0, Flag, OptionData) ->
			( getopt__need_arg(OptionData, yes) ->
				getopt__get_short_option_arg(Opts0, Arg,
					Args0, Args1),
				MaybeOptionArg = yes(Arg),
				Opts1 = []
			;
				MaybeOptionArg = no,
				Opts1 = Opts0,
				Args1 = Args0
			),
			string__from_char_list(['-', Opt], Option),
			getopt__process_option(OptionData, Option, Flag,
				MaybeOptionArg, OptionOps, 
				OptionTable0, Result1),
			( Result1 = ok(OptionTable1) ->
				getopt__handle_short_options(Opts1, OptionOps,
					Args1, Args, OptionTable1, Result)
			;
				Result = Result1,
				Args = Args1
			)
		;
			string__char_to_string(Opt, OptString),
			string__append_list(["unknown type for option `-",
				OptString, "'"], ErrorMsg),
			Result = error(ErrorMsg),
			Args = Args0
		)
	;
		string__char_to_string(Opt, OptString),
		string__append_list(["unrecognized option `-", OptString, "'"],
			ErrorMsg),
		Result = error(ErrorMsg),
		Args = Args0
	).

:- pred getopt__get_short_option_arg(list(character), string,
	list(string), list(string)).
:- mode getopt__get_short_option_arg(in, out, in, out) is det.

getopt__get_short_option_arg(Opts, Arg, Args0, Args) :-
	(
		Opts = [],
		Args0 = [ArgPrime | ArgsPrime]
	->
		Arg = ArgPrime,
		Args = ArgsPrime
	;
		string__from_char_list(Opts, Arg),
		Args = Args0
	).

:- pred getopt__process_option(option_data::in, string::in, OptionType::in,
	maybe(string)::in, option_ops(OptionType)::in(option_ops),
	option_table(OptionType)::in,
	maybe_option_table(OptionType)::out) is det.

getopt__process_option(bool(_), _Option, Flag, MaybeArg, _OptionOps,
		OptionTable0, Result) :-
	( MaybeArg = yes(_Arg) ->
		map__set(OptionTable0, Flag, bool(no), OptionTable),
		Result = ok(OptionTable)
	;
		map__set(OptionTable0, Flag, bool(yes), OptionTable),
		Result = ok(OptionTable)
	).
getopt__process_option(int(_), Option, Flag, MaybeArg, _OptionOps, 
		OptionTable0, Result) :-
	( MaybeArg = yes(Arg) ->
		( string__to_int(Arg, IntArg) ->
			map__set(OptionTable0, Flag, int(IntArg), OptionTable),
			Result = ok(OptionTable)
		;
			getopt__numeric_argument(Option, Arg, Result)
		)
	;
		error("integer argument expected in getopt__process_option")
	).
getopt__process_option(string(_), _Option, Flag, MaybeArg, _OptionOps,
		OptionTable0, Result) :-
	( MaybeArg = yes(Arg) ->
		map__set(OptionTable0, Flag, string(Arg), OptionTable),
		Result = ok(OptionTable)
	;
		error("string argument expected in getopt__process_option")
	).
getopt__process_option(accumulating(List0), _Option, Flag, MaybeArg, _OptionOps,
		OptionTable0, Result) :-
	( MaybeArg = yes(Arg) ->
		list__append(List0, [Arg], List),
		map__set(OptionTable0, Flag, accumulating(List), OptionTable),
		Result = ok(OptionTable)
	;
		error("acumulating argument expected in getopt__process_option")
	).
getopt__process_option(special, Option, Flag, MaybeArg, OptionOps,
		OptionTable0, Result) :-
	( MaybeArg = yes(_Arg) ->
		error("no special argument expected in getopt__process_option")
	;
		getopt__process_special(Option, Flag, none,
			OptionOps, OptionTable0, Result)
	).
getopt__process_option(bool_special, Option, Flag, MaybeArg, OptionOps,
		OptionTable0, Result) :-
	( MaybeArg = yes(_Arg) ->
		getopt__process_special(Option, Flag, bool(no),
			OptionOps, OptionTable0, Result)
	;
		getopt__process_special(Option, Flag, bool(yes),
			OptionOps, OptionTable0, Result)
	).
getopt__process_option(int_special, Option, Flag, MaybeArg, OptionOps,
		OptionTable0, Result) :-
	( MaybeArg = yes(Arg) ->
		( string__to_int(Arg, IntArg) ->
			getopt__process_special(Option, Flag, int(IntArg),
				OptionOps, OptionTable0, Result)
		;
			getopt__numeric_argument(Option, Arg, Result)
		)
	;
		error("int_special argument expected in getopt__process_option")
	).
getopt__process_option(string_special, Option, Flag, MaybeArg, OptionOps,
		OptionTable0, Result) :-
	( MaybeArg = yes(Arg) ->
		getopt__process_special(Option, Flag, string(Arg),
			OptionOps, OptionTable0, Result)
	;
		error("string_special argument expected in getopt__process_option")
	).

:- pred process_negated_bool_option(string, OptionType, option_ops(OptionType),
	option_table(OptionType), maybe_option_table(OptionType)).
:- mode process_negated_bool_option(in, in, in(option_ops), in, out) is det.

process_negated_bool_option(Option, Flag, OptionOps, OptionTable0, Result) :-
	( map__search(OptionTable0, Flag, OptionData) ->
		( OptionData = bool(_) ->
			map__set(OptionTable0, Flag, bool(no), OptionTable),
			Result = ok(OptionTable)
		; OptionData = bool_special ->
			getopt__process_special(Option, Flag, bool(no),
				OptionOps, OptionTable0, Result)
		;
			string__append_list(["cannot negate option `", Option,
				"' -- only boolean options can be negated"],
				ErrorMsg),
			Result = error(ErrorMsg)
		)
	;
		string__append_list(["unknown type for option `",
			Option, "'"], ErrorMsg),
		Result = error(ErrorMsg)
	).

:- pred getopt__process_special(string::in, OptionType::in, special_data::in,
	option_ops(OptionType)::in(option_ops), option_table(OptionType)::in,
	maybe_option_table(OptionType)::out) is det.

getopt__process_special(Option, Flag, OptionData, OptionOps,
		OptionTable0, Result) :-
	(
		getopt__get_special_handler(OptionOps, Handler)
	->
		(
			call(Handler, Flag, OptionData, OptionTable0, Result0)
		->
			Result = Result0
		;
			string__append_list(["the handler of option `",
				Option, "' failed"], ErrorMsg),
			Result = error(ErrorMsg)
		)
	;
		string__append_list(["option `", Option, "' has no handler"],
			ErrorMsg),
		Result = error(ErrorMsg)
	).

%-----------------------------------------------------------------------------%

:- pred getopt__need_arg(option_data::in, bool::out) is det.

getopt__need_arg(bool(_), no).
getopt__need_arg(int(_), yes).
getopt__need_arg(string(_), yes).
getopt__need_arg(accumulating(_), yes).
getopt__need_arg(special, no).
getopt__need_arg(bool_special, no).
getopt__need_arg(int_special, yes).
getopt__need_arg(string_special, yes).

:- pred getopt__numeric_argument(string::in, string::in,
	maybe_option_table(OptionType)::out) is det.

getopt__numeric_argument(Option, Arg, Result) :-
	string__append_list(["option `", Option,
		"' requires a numeric argument; `", Arg,
		"' is not numeric"], ErrorMsg),
	Result = error(ErrorMsg).

%-----------------------------------------------------------------------------%

:- pred getopt__get_short_options(option_ops(OptionType)::in(option_ops),
	pred(character, OptionType)::out(pred(in, out) is semidet)) is det.

getopt__get_short_options(option_ops(ShortOpt, _, _), ShortOpt).
getopt__get_short_options(option_ops(ShortOpt, _, _, _), ShortOpt).

:- pred getopt__get_long_options(option_ops(OptionType)::in(option_ops),
	pred(string, OptionType)::out(pred(in, out) is semidet)) is det.

getopt__get_long_options(option_ops(_, LongOpt, _), LongOpt).
getopt__get_long_options(option_ops(_, LongOpt, _, _), LongOpt).

:- pred getopt__get_option_defaults(option_ops(OptionType)::in(option_ops),
	pred(OptionType, option_data)::out(pred(out, out) is nondet)) is det.

getopt__get_option_defaults(option_ops(_, _, OptionDefs), OptionDefs).
getopt__get_option_defaults(option_ops(_, _, OptionDefs, _), OptionDefs).

:- pred getopt__get_special_handler(option_ops(OptionType)::in(option_ops),
	pred(OptionType, special_data,
		option_table(OptionType), maybe_option_table(OptionType))::
		out(pred(in, in, in, out) is semidet)) is semidet.

getopt__get_special_handler(option_ops(_, _, _, SpecHandler), SpecHandler).

%-----------------------------------------------------------------------------%

getopt__lookup_bool_option(OptionTable, Opt, Val) :-
	( map__lookup(OptionTable, Opt, bool(Val0)) ->
		Val = Val0
	;
		error("Expected bool option and didn't get one.")
	).

getopt__lookup_int_option(OptionTable, Opt, Val) :-
	( map__lookup(OptionTable, Opt, int(Val0)) ->
		Val = Val0
	;
		error("Expected int option and didn't get one.")
	).

getopt__lookup_string_option(OptionTable, Opt, Val) :-
	( map__lookup(OptionTable, Opt, string(Val0)) ->
		Val = Val0
	;
		error("Expected string option and didn't get one.")
	).

%-----------------------------------------------------------------------------%

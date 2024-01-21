%-----------------------------------------------------------------------------%

:- module cat.

% Simple implementation of the standard unix `cat' filter:
% copy input files (or stdin, if no input files) to stdout.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module string, list, char.

main -->
	io__command_line_arguments(Args),
	( { Args = [] } ->
		cat
	;
		cat_file_list(Args)
	).
		

:- pred cat_file_list(list(string)::in, io__state::di, io__state::uo) is det.

cat_file_list([]) --> [].
cat_file_list([File | Files]) -->
	cat_file(File),
	cat_file_list(Files).

:- pred cat_file(string::in, io__state::di, io__state::uo) is det.

cat_file(File) -->
	io__open_input(File, Result),
	( { Result = ok(Stream) },
		cat_stream(Stream)
	; { Result = error(Error) },
		io__progname("cat", Progname),
		{ io__error_message(Error, Message) },
		io__write_strings([
			Progname, ": ",
			"error opening file `", File, "' for input:\n\t",
			Message, "\n"
		])
	).

:- pred cat_stream(io__input_stream::in, io__state::di, io__state::uo) is det.

cat_stream(Stream) -->
	io__set_input_stream(Stream, _OldStream),
	cat.

:- pred cat(io__state::di, io__state::uo) is det.

cat -->
	io__read_line(Result),
	cat_2(Result).

:- pred cat_2(io__result(list(char))::in, io__state::di, io__state::uo) is det.

cat_2(Result) -->
	( { Result = ok(CharList) },
		{ string__from_char_list(CharList, String) },
		io__write_string(String),
		io__read_line(NextResult),
		cat_2(NextResult)
	; { Result = eof }
	; { Result = error(Error) },
		{ io__error_message(Error, Message) },
		io__input_stream_name(StreamName),
		io__progname("cat", ProgName),
		io__write_strings([
			ProgName, ": ",
			"error reading input file `", StreamName, "': \n\t",
			Message, "\n"
		])
	).

%-----------------------------------------------------------------------------%

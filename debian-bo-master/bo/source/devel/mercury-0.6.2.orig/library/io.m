%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: io.m.
% Main author: fjh.
% Stability: medium to high.
%
% This file encapsulates all the file I/O.
% We implement a purely logical I/O system using non-logical I/O primitives
% of the underlying system (C or Prolog).
% The logicalness is ensured by passing around a ``state-of-the-world''
% argument using unique modes.  The compiler will check that the state
% of the world argument is properly single-threaded, and will also check
% to ensure that you don't attempt to backtrack over any I/O.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module io.
:- interface.
:- import_module char, int, float, string, std_util, list.

%-----------------------------------------------------------------------------%

% External interface: imported predicate

% :- pred main(io__state, io__state).
% :- mode main(di, uo) is det.
%	main(IOState0, IOState1).
%		This module provides startup code which calls main/2.

%-----------------------------------------------------------------------------%

% Exported types

	% The state of the universe.

:- type io__state.

	% Opaque handles for text I/O streams.

:- type io__input_stream.

:- type io__output_stream.

	% Opaque handles for binary I/O streams.

:- type io__binary_input_stream.

:- type io__binary_output_stream.

	% Various types used for the result from the access predicates

:- type io__res		--->	ok
			;	error(io__error).

:- type io__res(T)	--->	ok(T)
			;	error(io__error).

:- type io__result	--->	ok
			;	eof
			;	error(io__error).

:- type io__result(T)	--->	ok(T)
			;	eof
			;	error(io__error).

:- type io__read_result(T)	--->	ok(T)
				;	eof
				;	error(string, int).

:- type io__error.	% Use io__error_message to decode it.

	% Poly-type is used for io__write_many, which does
	% some vaguely printf-like formatting.

:- type io__poly_type == string__poly_type.
%			--->
%		c(char)
%	;	s(string)
%	;	i(int)
%	;	f(float).
%

%-----------------------------------------------------------------------------%

% Text input predicates.

:- pred io__read_char(io__result(char), io__state, io__state).
:- mode io__read_char(out, di, uo) is det.
%		Reads a character from the current input stream.

:- pred io__read_word(io__result(list(char)), io__state, io__state).
:- mode io__read_word(out, di, uo) is det.
%		Reads a whitespace delimited word from the current input stream.

:- pred io__read_line(io__result(list(char)), io__state, io__state).
:- mode io__read_line(out, di, uo) is det.
%		Reads a line from the current input stream.

:- pred io__putback_char(char, io__state, io__state).
:- mode io__putback_char(in, di, uo) is det.
%		Un-reads a character from the current input stream.
%		You can put back as many characters as you like.
%		You can even put back something that you didn't actually read.

:- pred io__read_char(io__input_stream, io__result(char),
				io__state, io__state).
:- mode io__read_char(in, out, di, uo) is det.
%		Reads a character from specified stream.

:- pred io__read_word(io__input_stream, io__result(list(char)),
							io__state, io__state).
:- mode io__read_word(in, out, di, uo) is det.
%		Reads a whitespace delimited word from specified stream.

:- pred io__read_line(io__input_stream, io__result(list(char)),
							io__state, io__state).
:- mode io__read_line(in, out, di, uo) is det.
%		Reads a line from specified stream.

:- pred io__putback_char(io__input_stream, char, io__state, io__state).
:- mode io__putback_char(in, in, di, uo) is det.
%		Un-reads a character from specified stream.
%		You can put back as many characters as you like.
%		You can even put back something that you didn't actually read.

:- pred io__read_anything(io__read_result(T), io__state, io__state).
:- mode io__read_anything(out, di, uo) is det.
%		Reads its argument from the current input stream.
%		The argument may be of (almost) any type. 
%		The term read had better be of the right type!
%		XXX io__read_anything is NOT YET IMPLEMENTED.
%		It will also probably be renamed io__read.

:- pred io__read_anything(io__input_stream, io__read_result(T),
							io__state, io__state).
:- mode io__read_anything(in, out, di, uo) is det.
%		Reads its argument to the specified stream.
%		The argument may be of (almost) any type.
%		The term read had better be of the right type!
%		XXX io__read_anything is NOT YET IMPLEMENTED.
%		It will also probably be renamed io__read.

:- pred io__ignore_whitespace(io__result, io__state, io__state).
:- mode io__ignore_whitespace(out, di, uo) is det.
%		Discards all the whitespace from the current stream.

:- pred io__ignore_whitespace(io__input_stream, io__result,
				io__state, io__state).
:- mode io__ignore_whitespace(in, out, di, uo) is det.
%		Discards all the whitespace from the specified stream.



%-----------------------------------------------------------------------------%

% Text output predicates.

:- pred io__write_string(string, io__state, io__state).
:- mode io__write_string(in, di, uo) is det.
%		Writes a string to the current output stream.

:- pred io__write_string(io__output_stream, string, io__state, io__state).
:- mode io__write_string(in, in, di, uo) is det.
%		Writes a string to the specified stream.

:- pred io__write_strings(list(string), io__state, io__state).
:- mode io__write_strings(in, di, uo) is det.
%		Writes a list of strings to the current output stream.

:- pred io__write_strings(io__output_stream, list(string),
				io__state, io__state).
:- mode io__write_strings(in, in, di, uo) is det.
%		Writes a string to the specified stream.

:- pred io__write_char(char, io__state, io__state).
:- mode io__write_char(in, di, uo) is det.
%		Writes a character to the current output stream.

:- pred io__write_char(io__output_stream, char, io__state, io__state).
:- mode io__write_char(in, in, di, uo) is det.
%		Writes a character to the specified stream.

:- pred io__write_int(int, io__state, io__state).
:- mode io__write_int(in, di, uo) is det.
%		Writes an integer to the current output stream.

:- pred io__write_int(io__output_stream, int, io__state, io__state).
:- mode io__write_int(in, in, di, uo) is det.
%		Writes an integer to the specified stream.

:- pred io__write_float(float, io__state, io__state).
:- mode io__write_float(in, di, uo) is det.
%	io__write_float(Float, IO0, IO1).
%		Writes a floating point number to the current output stream.

:- pred io__write_float(io__output_stream, float, io__state, io__state).
:- mode io__write_float(in, in, di, uo) is det.
%	io__write_float(Float, IO0, IO1).
%		Writes a floating point number to the specified stream.

:- pred io__write_many(list(io__poly_type), io__state, io__state).
:- mode io__write_many(in, di, uo) is det.
%	writes a polyglot to output.

:- pred io__write_many(io__output_stream, list(io__poly_type), io__state, io__state).
:- mode io__write_many(in, in, di, uo) is det.
%	writes a polyglot to a specified stream.

:- pred io__write(T, io__state, io__state).
:- mode io__write(in, di, uo) is det.
%		Writes its argument to the current output stream.
%		The argument may be of (almost) any type.
%		(Any type except a higher-order predicate type,
%		or some of the builtin types such as io__state itself.)
%		XXX Not all quoting of atoms is done correctly.

:- pred io__write(io__output_stream, T, io__state, io__state).
:- mode io__write(in, in, di, uo) is det.
%		Writes its argument to the specified stream.
%		The argument may be of (almost) any type.
%		(Any type except a higher-order predicate type,
%		or some of the builtin types such as io__state itself.)

:- pred io__flush_output(io__state, io__state).
:- mode io__flush_output(di, uo) is det.
%	Flush the output buffer of the current output stream.

:- pred io__flush_output(io__output_stream, io__state, io__state).
:- mode io__flush_output(in, di, uo) is det.
%	Flush the output buffer of the specified output stream.

%-----------------------------------------------------------------------------%

% Input text stream predicates.

:- pred io__see(string, io__res, io__state, io__state).
:- mode io__see(in, out, di, uo) is det.
%	io__see(File, Result, IO0, IO1).
%		Attempts to open a file for input, and if successful
%		sets the current input stream to the newly opened stream.
%		Result is either 'ok' or 'error'.

:- pred io__seen(io__state, io__state).
:- mode io__seen(di, uo) is det.
%		Closes the current input stream.
%		The current input stream reverts to standard input.

:- pred io__open_input(string, io__res(io__input_stream), io__state, io__state).
:- mode io__open_input(in, out, di, uo) is det.
%	io__open_input(File, Result, IO0, IO1).
%		Attempts to open a file for input.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__close_input(io__input_stream, io__state, io__state).
:- mode io__close_input(in, di, uo) is det.
%	io__close_input(File, IO0, IO1).
%		Closes an open input stream.

:- pred io__input_stream(io__input_stream, io__state, io__state).
:- mode io__input_stream(out, di, uo) is det.
%		Retrieves the current input stream.
%		Does not modify the IO state.

:- pred io__set_input_stream(io__input_stream, io__input_stream,
				io__state, io__state).
:- mode io__set_input_stream(in, out, di, uo) is det.
%       io__set_input_stream(NewStream, OldStream, IO0, IO1)
%		Changes the current input stream to the stream specified.
%		Returns the previous stream.

:- pred io__stdin_stream(io__input_stream, io__state, io__state).
:- mode io__stdin_stream(out, di, uo) is det.
%		Retrieves the standard input stream.
%		Does not modify the IO state.

:- pred io__input_stream_name(string, io__state, io__state).
:- mode io__input_stream_name(out, di, uo) is det.
%	Retrieves the human-readable name associated with the current input
%	stream.
%	For file streams, this is the filename.
%	For stdin this is the string "<standard input>".

:- pred io__input_stream_name(io__input_stream, string, io__state, io__state).
:- mode io__input_stream_name(in, out, di, uo) is det.
%	Retrieves the human-readable name associated with the specified input
%	stream.
%	For file streams, this is the filename.
%	For stdin this is the string "<standard input>".

:- pred io__get_line_number(int, io__state, io__state).
:- mode io__get_line_number(out, di, uo) is det.

:- pred io__get_line_number(io__input_stream, int, io__state, io__state).
:- mode io__get_line_number(in, out, di, uo) is det.

%	Return the line number of the current input stream.
%	Lines are numbered starting at 1.

:- pred io__set_line_number(int, io__state, io__state).
:- mode io__set_line_number(in, di, uo) is det.

:- pred io__set_line_number(io__input_stream, int, io__state, io__state).
:- mode io__set_line_number(in, in, di, uo) is det.

%	Return the line number of the current input stream.
%	Lines are numbered starting at 1.

%-----------------------------------------------------------------------------%

% Output text stream predicates.

:- pred io__tell(string, io__res, io__state, io__state).
:- mode io__tell(in, out, di, uo) is det.
%	io__tell(File, Result, IO0, IO1).
%		Attempts to open a file for output, and if successful
%		sets the current output stream to the newly opened stream.
%		As per Prolog tell/1. Result is either 'ok' or 'error(ErrCode)'.

:- pred io__told(io__state, io__state).
:- mode io__told(di, uo) is det.
%	io__told(IO0, IO1).
%		Closes the current output stream.
%		The default output stream reverts to standard output.
%		As per Prolog told/0.

:- pred io__open_output(string, io__res(io__output_stream),
				io__state, io__state).
:- mode io__open_output(in, out, di, uo) is det.
%	io__open_output(File, Result, IO0, IO1).
%		Attempts to open a file for output.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__open_append(string, io__res(io__output_stream),
				io__state, io__state).
:- mode io__open_append(in, out, di, uo) is det.
%	io__open_append(File, Result, IO0, IO1).
%		Attempts to open a file for appending.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__close_output(io__output_stream, io__state, io__state).
:- mode io__close_output(in, di, uo) is det.
%	io__close_output(File, IO0, IO1).
%		Closes an open output stream.

:- pred io__output_stream(io__output_stream, io__state, io__state).
:- mode io__output_stream(out, di, uo) is det.
%		Retrieves the current output stream.
%		Does not modify the IO state.

:- pred io__set_output_stream(io__output_stream, io__output_stream,
				io__state, io__state).
:- mode io__set_output_stream(in, out, di, uo) is det.
%	io__set_output_stream(NewStream, OldStream, IO0, IO)
%		Changes the current output stream to the stream specified.
%		Returns the previous stream.

:- pred io__stdout_stream(io__output_stream, io__state, io__state).
:- mode io__stdout_stream(out, di, uo) is det.
%		Retrieves the standard output stream.
%		Does not modify the IO state.

:- pred io__stderr_stream(io__output_stream, io__state, io__state).
:- mode io__stderr_stream(out, di, uo) is det.
%		Retrieves the standard error stream.
%		Does not modify the IO state.

:- pred io__output_stream_name(string, io__state, io__state).
:- mode io__output_stream_name(out, di, uo) is det.
%	Retrieves the human-readable name associated with the current
%	output stream.
%	For file streams, this is the filename.
%	For stdout this is the string "<standard output>".
%	For stderr this is the string "<standard error>".

:- pred io__output_stream_name(io__output_stream, string, io__state, io__state).
:- mode io__output_stream_name(in, out, di, uo) is det.
%	Retrieves the human-readable name associated with the specified stream.
%	For file streams, this is the filename.
%	For stdout this is the string "<standard output>".
%	For stderr this is the string "<standard error>".

%-----------------------------------------------------------------------------%

% Binary input predicates.

:- pred io__read_byte(io__result(int), io__state, io__state).
:- mode io__read_byte(out, di, uo) is det.
%		Reads a single byte from the current binary input
%		stream and returns it in the bottom 8 bits of an integer.

:- pred io__read_byte(io__binary_input_stream, io__result(int),
				io__state, io__state).
:- mode io__read_byte(in, out, di, uo) is det.
%		Reads a single byte from the specified binary input
%		stream and returns it in the bottom 8 bits of an integer.

:- pred io__putback_byte(int, io__state, io__state).
:- mode io__putback_byte(in, di, uo) is det.
%		Un-reads a byte from the current binary input stream.
%		You can put back as many bytes as you like.
%		You can even put back something that you didn't actually read.
%		The byte is taken from the bottom 8 bits of an integer.

:- pred io__putback_byte(io__binary_input_stream, int, io__state, io__state).
:- mode io__putback_byte(in, in, di, uo) is det.
%		Un-reads a byte from specified binary input stream.
%		You can put back as many bytes as you like.
%		You can even put back something that you didn't actually read.
%		The byte is returned in the bottom 8 bits of an integer.

%-----------------------------------------------------------------------------%

% Binary output predicates.

% XXX what about wide characters?

:- pred io__write_byte(int, io__state, io__state).
:- mode io__write_byte(in, di, uo) is det.
%		Writes a single byte to the current binary output stream.
%		The byte is taken from the bottom 8 bits of an int.

:- pred io__write_byte(io__binary_output_stream, int, io__state, io__state).
:- mode io__write_byte(in, in, di, uo) is det.
%		Writes a single byte to the specified binary output stream.
%		The byte is taken from the bottom 8 bits of an int.

:- pred io__write_bytes(string, io__state, io__state).
:- mode io__write_bytes(in, di, uo) is det.
%		Writes several bytes to the current binary output stream.
%		The bytes are taken from a string.

:- pred io__write_bytes(io__binary_output_stream, string, io__state, io__state).
:- mode io__write_bytes(in, in, di, uo) is det.
%		Writes several bytes to the specified binary output stream.
%		The bytes are taken from a string.

:- pred io__flush_binary_output(io__state, io__state).
:- mode io__flush_binary_output(di, uo) is det.
%	Flush the output buffer of the current binary output stream.

:- pred io__flush_binary_output(io__binary_output_stream, io__state, io__state).
:- mode io__flush_binary_output(in, di, uo) is det.
%	Flush the output buffer of the specified binary output stream.

%-----------------------------------------------------------------------------%

% Binary input stream predicates.

:- pred io__see_binary(string, io__res, io__state, io__state).
:- mode io__see_binary(in, out, di, uo) is det.
%	io__see_binary(File, Result, IO0, IO1).
%		Attempts to open a file for binary input, and if successful
%		sets the current binary input stream to the newly opened stream.
%		Result is either 'ok' or 'error'.

:- pred io__seen_binary(io__state, io__state).
:- mode io__seen_binary(di, uo) is det.
%		Closes the current input stream.
%		The current input stream reverts to standard input.

:- pred io__open_binary_input(string, io__res(io__binary_input_stream),
			io__state, io__state).
:- mode io__open_binary_input(in, out, di, uo) is det.
%	io__open_binary_input(File, Result, IO0, IO1).
%		Attempts to open a binary file for input.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__close_binary_input(io__binary_input_stream, io__state, io__state).
:- mode io__close_binary_input(in, di, uo) is det.
%	io__close_binary_input(File, IO0, IO1).
%		Closes an open binary input stream.

:- pred io__binary_input_stream(io__binary_input_stream, io__state, io__state).
:- mode io__binary_input_stream(out, di, uo) is det.
%		Retrieves the current binary input stream.
%		Does not modify the IO state.

:- pred io__set_binary_input_stream(io__binary_input_stream,
			io__binary_input_stream, io__state, io__state).
:- mode io__set_binary_input_stream(in, out, di, uo) is det.
%       io__set_binary_input_stream(NewStream, OldStream, IO0, IO1)
%		Changes the current input stream to the stream specified.
%		Returns the previous stream.

:- pred io__stdin_binary_stream(io__binary_input_stream, io__state, io__state).
:- mode io__stdin_binary_stream(out, di, uo) is det.
%		Retrieves the standard binary input stream.
%		Does not modify the IO state.

:- pred io__binary_input_stream_name(string, io__state, io__state).
:- mode io__binary_input_stream_name(out, di, uo) is det.
%	Retrieves the human-readable name associated with the current binary
%	input stream.
%	For file streams, this is the filename.

:- pred io__binary_input_stream_name(io__binary_input_stream, string,
		io__state, io__state).
:- mode io__binary_input_stream_name(in, out, di, uo) is det.
%	Retrieves the human-readable name associated with the specified binary
%	input stream.
%	For file streams, this is the filename.

%-----------------------------------------------------------------------------%

% Binary output stream predicates.

:- pred io__tell_binary(string, io__res, io__state, io__state).
:- mode io__tell_binary(in, out, di, uo) is det.
%	io__tell_binary(File, Result, IO0, IO1).
%		Attempts to open a file for binary output, and if successful
%		sets the current binary output stream to the newly opened
%		stream. As per Prolog tell/1. Result is either 'ok' or
%		'error(ErrCode)'.

:- pred io__told_binary(io__state, io__state).
:- mode io__told_binary(di, uo) is det.
%	io__told_binary(IO0, IO1).
%		Closes the current binary output stream.
%		The default binary output stream reverts to standard output.
%		As per Prolog told/0.

:- pred io__open_binary_output(string, io__res(io__binary_output_stream),
				io__state, io__state).
:- mode io__open_binary_output(in, out, di, uo) is det.
%	io__open_binary_output(File, Result, IO0, IO1).
%		Attempts to open a file for binary output.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__open_binary_append(string, io__res(io__binary_output_stream),
				io__state, io__state).
:- mode io__open_binary_append(in, out, di, uo) is det.
%	io__open_binary_append(File, Result, IO0, IO1).
%		Attempts to open a file for binary appending.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__close_binary_output(io__binary_output_stream, io__state, io__state).
:- mode io__close_binary_output(in, di, uo) is det.
%	io__close_binary_output(File, IO0, IO1).
%		Closes an open binary output stream.

:- pred io__binary_output_stream(io__binary_output_stream,
			io__state, io__state).
:- mode io__binary_output_stream(out, di, uo) is det.
%		Retrieves the current binary output stream.
%		Does not modify the IO state.

:- pred io__stdout_binary_stream(io__binary_output_stream, io__state,
				io__state).
:- mode io__stdout_binary_stream(out, di, uo) is det.
%		Retrieves the standard binary output stream.
%		Does not modify the IO state.

:- pred io__set_binary_output_stream(io__binary_output_stream,
			io__binary_output_stream, io__state, io__state).
:- mode io__set_binary_output_stream(in, out, di, uo) is det.
%	io__set_binary_output_stream(NewStream, OldStream, IO0, IO)
%		Changes the current binary output stream to the stream
%		specified. Returns the previous stream.

:- pred io__binary_output_stream_name(string, io__state, io__state).
:- mode io__binary_output_stream_name(out, di, uo) is det.
%	Retrieves the human-readable name associated with the current
%	binary output stream.
%	For file streams, this is the filename.

:- pred io__binary_output_stream_name(io__binary_output_stream, string,
			io__state, io__state).
:- mode io__binary_output_stream_name(in, out, di, uo) is det.
%	Retrieves the human-readable name associated with the specified 
%	output stream.
%	For file streams, this is the filename.

%-----------------------------------------------------------------------------%

% Global state predicates.

:- pred io__progname(string, string, io__state, io__state).
:- mode io__progname(in, out, di, uo) is det.
% 	io__progname(DefaultProgname, Progname)
%		Returns the name that the program was invoked with,
%		if available, or DefaultProgname if the name is not
%		available.
%		
%		Does not modify the IO state.

:- pred io__progname_base(string, string, io__state, io__state).
:- mode io__progname_base(in, out, di, uo) is det.
% 	io__progname_base(DefaultProgname, Progname)
%		Like `io__progname', except that it strips off any path name
%		preceding the program name.  Useful for error messages.

:- pred io__command_line_arguments(list(string), io__state, io__state).
:- mode io__command_line_arguments(out, di, uo) is det.
% 	io__command_line_arguments(Args)
%		Returns the arguments that the program was invoked with,
%		if available, otherwise an empty list.
%		
%		Does not modify the IO state.

% The io__state contains an integer used to record the program's exit status.
% When the program finishes, it will return this exit status to the operating
% system.  The following predicates can be used to get and set the exit status.

:- pred io__get_exit_status(int, io__state, io__state).
:- mode io__get_exit_status(out, di, uo) is det.

:- pred io__set_exit_status(int, io__state, io__state).
:- mode io__set_exit_status(in, di, uo) is det.

% The io__state includes a `globals' field which is not used by the I/O
% library, but can be used by the application.  The globals field is
% of type `univ' so that the application can store any data it wants there.
% The following predicates can be used to access this global state.

:- pred io__get_globals(univ, io__state, io__state).
:- mode io__get_globals(uo, di, uo) is det.
	% Doesn't modify the io__state.

:- pred io__set_globals(univ, io__state, io__state).
:- mode io__set_globals(di, di, uo) is det.

% The following predicates provide an interface to the environment list.
% Do not attempt to put spaces or '=' signs in the names of environment
% variables, or bad things may result!

:- pred io__get_environment_var(string, maybe(string), io__state, io__state).
:- mode io__get_environment_var(in, out, di, uo) is det.
	% First argument is the name of the environment variable.
	% Returns yes(Value) if the variable was set (Value will
	% be set to the value of the variable) and no if the
	% variable was not set.

:- pred io__set_environment_var(string, string, io__state, io__state).
:- mode io__set_environment_var(in, in, di, uo) is det.
	% First argument is the name of the environment variable,
	% second argument is the value to be assigned to that
	% variable.  Will abort if the system runs out of environment
	% space.

%-----------------------------------------------------------------------------%

% Memory management predicates.

	% Write some memory/time usage statistics to stdout.

:- pred io__report_stats(io__state, io__state).
:- mode io__report_stats(di, uo) is det.

	% Preallocate heap space (to avoid NU-Prolog panic).

:- pred io__preallocate_heap_space(int, io__state, io__state).
:- mode io__preallocate_heap_space(in, di, uo) is det.

/*** no longer supported, sorry
:- pred io__gc_call(pred(io__state, io__state), io__state, io__state).
:- mode io__gc_call(pred(di, uo) is det, di, uo) is det.
%	io__gc_call(Goal, IO0, IO1).
%		Execute Goal, passing IO0, and IO1, and
%		collect any garbage created during it's execution.
***/

%-----------------------------------------------------------------------------%

% Miscellaneous predicates

:- pred io__call_system(string, io__res(int), io__state, io__state).
:- mode io__call_system(in, out, di, uo) is det.
%	io__call_system(Command, Result, IO0, IO1).
%		Invokes the operating system shell with the specified
%		Command.  Result is either `ok(ExitStatus)', if it was
%		possible to invoke the command, or `error(ErrorCode)' if not.

:- pred io__error_message(io__error, string).
:- mode io__error_message(in, out) is det.
%	io__error_message(ErrorCode, ErrorMessage).
%		Look up the error message corresponding to a particular error
%		code.

%-----------------------------------------------------------------------------%

% For use by term_io.m:

:- import_module ops.

:- pred io__get_op_table(ops__table, io__state, io__state).
:- mode io__get_op_table(out, di, uo) is det.

:- pred io__set_op_table(ops__table, io__state, io__state).
:- mode io__set_op_table(di, di, uo) is det.

% For use by the Mercury runtime:

:- type io__external_state.

:- pred io__init_state(io__external_state, io__state).
:- mode io__init_state(di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, dir, term_io, varset, require, time.

:- type io__state
	---> 	io__state(
			io__stream_names,	% map from stream to stream name
			io__stream_putback,	% map from input stream to
						% list of putback characters
						% Note: only used for the Prolog
						% implementation.
			ops__table, 		% current operators
			univ,			% for use by the application
			io__external_state
		).

:- type io__stream_names ==	map(io__stream, string).
:- type io__stream_putback ==	map(io__stream, list(char)).

:- type io__input_stream ==	io__stream.
:- type io__output_stream ==	io__stream.

:- type io__binary_input_stream ==	io__stream.
:- type io__binary_output_stream ==	io__stream.

:- type io__stream.
/*
 * In NU-Prolog: 
 *	io__stream	--->	stream(int, int)
 *			;	user_input
 *			;	user_output
 *			;	user_error.
 * In C:
 *	io__stream	==	pointer to MercuryFile
 */

	% This inter-language stuff is tricky.
	% We communicate via ints rather than via io__result_codes because
	% we don't want the C code to depend on how Mercury stores its
	% discriminated union data types.

:- pred io__read_char_code(io__input_stream, int, io__state, io__state).
:- mode io__read_char_code(in, out, di, uo) is det.
%		Reads a character code from specified stream.
%		Returns -1 if at EOF, -2 if an error occurs.

:- pred io__call_system_code(string, int, io__state, io__state).
:- mode io__call_system_code(in, out, di, uo) is det.
%	io__call_system(Command, Status, IO0, IO1).
%		Invokes the operating system shell with the specified
%		Command.  Returns Status = -1 on failure.

:- pred io__do_open(string, string, int, io__input_stream,
			io__state, io__state).
:- mode io__do_open(in, in, out, out, di, uo) is det.
%	io__do_open(File, Mode, ResultCode, Stream, IO0, IO1).
%		Attempts to open a file in the specified mode.
%		Result is 0 for success, -1 for failure.

:- pred io__getenv(string, string).
:- mode io__getenv(in, out) is semidet.
%	io__getenv(Var, Value).
%		Gets the value Value associated with the environment
%		variable Var.  Fails if the variable was not set.

:- pred io__putenv(string).
:- mode io__putenv(in) is semidet.
%	io__putenv(VarString).
%		If VarString is a string of the form "name=value",
%		sets the environment variable name to the specified
%		value.  Fails if the operation does not work.

%-----------------------------------------------------------------------------%

% input predicates

io__read_char(Result) -->
	io__input_stream(Stream),
	io__read_char(Stream, Result).

io__read_char(Stream, Result, IO_0, IO) :-
	io__read_char_code(Stream, Code, IO_0, IO),
	(
		Code = -1
	->
		Result = eof
	;
		char__to_int(Char, Code)
	->
		Result = ok(Char)
	;
		% XXX improve error message
		Result = error("read error")
	).

io__read_byte(Result) -->
	io__binary_input_stream(Stream),
	io__read_byte(Stream, Result).

io__read_byte(Stream, Result, IO_0, IO) :-
	io__read_char_code(Stream, Code, IO_0, IO),
	(
		Code = -1
	->
		Result = eof
	;
		Code = -2
	->
		% XXX improve error message
		Result = error("read error")
	;
		Result = ok(Code)
	).

io__read_word(Result) -->
	io__input_stream(Stream),
	io__read_word(Stream, Result).
	
io__read_word(Stream, Result) -->
	io__ignore_whitespace(Stream, WSResult),
	(
		{ WSResult = error(Error) },
		{ Result = error(Error) }
	;
		{ WSResult = eof },
		{ Result = eof }
	;
		{ WSResult = ok },
		io__read_word_2(Stream, Result)
	).

:- pred io__read_word_2(io__input_stream, io__result(list(char)),
				io__state, io__state).
:- mode	io__read_word_2(in, out, di, uo) is det.

io__read_word_2(Stream, Result) -->
	io__read_char(Stream, CharResult),
	(
		{ CharResult = error(Error) },
		{ Result = error(Error) }
	;
		{ CharResult = eof },
		{ Result = eof }
	;
		{ CharResult = ok(Char) },
		(
			{ char__is_whitespace(Char) }
		->
			io__putback_char(Stream, Char),
			{ Result = ok([]) }
		;
			io__read_word_2(Stream, Result0),
			(
				{ Result0 = ok(Chars) },
				{ Result = ok([Char | Chars]) }
			;
				{ Result0 = error(_) },
				{ Result = Result0 }
			;
				{ Result0 = eof },
				{ Result = ok([Char]) }
			)
		)	
	).

io__read_line(Result) -->
	io__input_stream(Stream),
	io__read_line(Stream, Result).

io__read_line(Stream, Result) -->
	io__read_char(Stream, CharResult),
	(
		{ CharResult = error(Error) },
		{ Result = error(Error) }
	;
		{ CharResult = eof },
		{ Result = eof }
	;
		{ CharResult = ok(Char) },
		( { Char = '\n' } ->
			{ Result = ok([Char]) }
		;
			io__read_line(Stream, Result0),
			(
				{ Result0 = ok(Chars) },
				{ Result = ok([Char | Chars]) }
			;
				{ Result0 = error(_) },
				{ Result = Result0 }
			;
				{ Result0 = eof },
				{ Result = ok([Char]) }
			)
		)
	).

io__putback_char(Char) -->
	io__input_stream(Stream),
	io__putback_char(Stream, Char).

io__putback_byte(Char) -->
	io__binary_input_stream(Stream),
	io__putback_byte(Stream, Char).

io__read_anything(X) -->
	term_io__read_term(ReadResult),
	(	{ ReadResult = term(_VarSet, Term) },
		( { term_to_type(Term, Type) } ->
			{ X = ok(Type) }
		;
			{ X = error("io__read_anything : the term read was not a valid type", 0) }
		)
	;
		{ ReadResult = eof },
		{ X = eof }
	;
		{ ReadResult = error(String, Int) },
		{ X = error(String, Int) }
	).

io__read_anything(Stream, X) -->
	io__set_input_stream(Stream, OrigStream),
	io__read_anything(X),
	io__set_input_stream(OrigStream, _Stream).

io__ignore_whitespace(Result) -->
	io__input_stream(Stream),
	io__ignore_whitespace(Stream, Result).

io__ignore_whitespace(Stream, Result) -->
	io__read_char(Stream, CharResult),
	(
		{ CharResult = error(Error) },
		{ Result = error(Error) }
	;
		{ CharResult = eof },
		{ Result = eof }
	;
		{ CharResult = ok(Char) },
		(
			{ char__is_whitespace(Char) }
		->
			io__ignore_whitespace(Stream, Result)
		;
			io__putback_char(Stream, Char),
			{ Result = ok }
		)	
	).

%-----------------------------------------------------------------------------%

% output predicates

io__write_strings(Strings) -->
	io__output_stream(Stream),
	io__write_strings(Stream, Strings).

io__write_strings(_Stream, []) --> [].
io__write_strings(Stream, [S|Ss]) -->
	io__write_string(Stream, S),
	io__write_strings(Stream, Ss).

io__write_many(Poly_list) -->
	io__output_stream(Stream),
	io__write_many(Stream, Poly_list).

io__write_many( _Stream, [], IO, IO ).
io__write_many( Stream, [ c(C) | Rest ] ) -->
	io__write_char(Stream, C),
	io__write_many(Stream, Rest).
io__write_many( Stream, [ i(I) | Rest ] ) -->
	io__write_int(Stream, I),
	io__write_many(Stream, Rest).
io__write_many( Stream, [ s(S) | Rest ]) -->
	io__write_string(Stream, S),
	io__write_many(Stream, Rest).
io__write_many( Stream, [ f(F) | Rest ]) -->
	io__write_float(Stream, F),
	io__write_many(Stream, Rest).

io__write(Stream, X) -->
	io__set_output_stream(Stream, OrigStream),
	io__write(X),
	io__set_output_stream(OrigStream, _Stream).

%-----------------------------------------------------------------------------%

% stream predicates

io__open_input(FileName, Result) -->
	io__do_open(FileName, "r", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		% XXX improve error message
		{ Result = error("can't open input file") }
	).

io__open_output(FileName, Result) -->
	io__do_open(FileName, "w", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		% XXX improve error message
		{ Result = error("can't open output file") }
	).

io__open_append(FileName, Result) -->
	io__do_open(FileName, "a", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		% XXX improve error message
		{ Result = error("can't append to file") }
	).

io__open_binary_input(FileName, Result) -->
	io__do_open(FileName, "rb", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		% XXX improve error message
		{ Result = error("can't open input file") }
	).

io__open_binary_output(FileName, Result) -->
	io__do_open(FileName, "wb", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		% XXX improve error message
		{ Result = error("can't open output file") }
	).

io__open_binary_append(FileName, Result) -->
	io__do_open(FileName, "ab", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		% XXX improve error message
		{ Result = error("can't append to file") }
	).

%-----------------------------------------------------------------------------%

	% Declarative versions of Prolog's see/1 and seen/0.

io__see(File, Result) -->
	io__open_input(File, Result0),
	(
		{ Result0 = ok(Stream) },
		io__set_input_stream(Stream, _),
		{ Result = ok }
	;
		{ Result0 = error(Error) },
		{ Result = error(Error) }
	).

io__seen -->
	io__stdin_stream(Stdin),
	io__set_input_stream(Stdin, OldStream),
	io__close_input(OldStream).

	% Plus binary IO versions.

io__see_binary(File, Result) -->
	io__open_binary_input(File, Result0),
	(
		{ Result0 = ok(Stream) },
		io__set_binary_input_stream(Stream, _),
		{ Result = ok }
	;
		{ Result0 = error(Error) },
		{ Result = error(Error) }
	).

io__seen_binary -->
	io__stdin_binary_stream(Stdin),
	io__set_binary_input_stream(Stdin, OldStream),
	io__close_binary_input(OldStream).

%-----------------------------------------------------------------------------%

	% Declarative versions of Prolog's tell/1 and told/0.

io__told -->
	io__stdout_stream(Stdout),
	io__set_output_stream(Stdout, OldStream),
	io__close_output(OldStream).

io__tell(File, Result) -->
	io__open_output(File, Result0),
	( { Result0 = ok(Stream) } ->
		io__set_output_stream(Stream, _),
		{ Result = ok }
	;
		% XXX improve error message
		{ Result = error("can't open output file") }
	).

io__told_binary -->
	io__stdout_binary_stream(Stdout),
	io__set_binary_output_stream(Stdout, OldStream),
	io__close_binary_output(OldStream).

io__tell_binary(File, Result) -->
	io__open_binary_output(File, Result0),
	( { Result0 = ok(Stream) } ->
		io__set_binary_output_stream(Stream, _),
		{ Result = ok }
	;
		% XXX improve error message
		{ Result = error("can't open output file") }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% stream name predicates

io__input_stream_name(Name) -->
	io__input_stream(Stream),
	io__stream_name(Stream, Name).

io__input_stream_name(Stream, Name) -->
	io__stream_name(Stream, Name).

io__output_stream_name(Name) -->
	io__output_stream(Stream),
	io__stream_name(Stream, Name).

io__output_stream_name(Stream, Name) -->
	io__stream_name(Stream, Name).

io__binary_input_stream_name(Name) -->
	io__binary_input_stream(Stream),
	io__stream_name(Stream, Name).

io__binary_input_stream_name(Stream, Name) -->
	io__stream_name(Stream, Name).

io__binary_output_stream_name(Name) -->
	io__binary_output_stream(Stream),
	io__stream_name(Stream, Name).

io__binary_output_stream_name(Stream, Name) -->
	io__stream_name(Stream, Name).

:- pred io__stream_name(io__stream, string, io__state, io__state).
:- mode io__stream_name(in, out, di, uo) is det.

	% XXX major design flaw with regard to unique modes
	% means that this is very inefficient.
io__stream_name(Stream, Name, IOState0, IOState) :-
	IOState0 = io__state(StreamNames0, B, C, D, E),
	copy(StreamNames0, StreamNames),
	IOState = io__state(StreamNames, B, C, D, E),
	( map__search(StreamNames0, Stream, Name1) ->
		Name = Name1
	;
		Name = "<stream name unavailable>"
	).

:- pred io__delete_stream_name(io__stream, io__state, io__state).
:- mode io__delete_stream_name(in, di, uo) is det.

io__delete_stream_name(Stream, io__state(StreamNames0, B, C, D, E),
		io__state(StreamNames, B, C, D, E)) :-
	map__delete(StreamNames0, Stream, StreamNames).

:- pred io__insert_stream_name(io__stream, string, io__state, io__state).
:- mode io__insert_stream_name(in, in, di, uo) is det.

io__insert_stream_name(Stream, Name,
		io__state(StreamNames0, B, C, D, E),
		io__state(StreamNames, B, C, D, E)) :-
	copy(Stream, Stream1),
	copy(Name, Name1),
	map__set(StreamNames0, Stream1, Name1, StreamNames).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% global state predicates

	% XXX major design flaw with regard to unique modes
	% and io__get_globals/3

/* old definition
io__get_globals(Globals, IOState, IOState) :-
	IOState = io__state(_, _, _, Globals, _).
*/
/* new definition - horrendously inefficient! */
io__get_globals(Globals, IOState0, IOState) :-
	IOState0 = io__state(A, B, C, Globals0, E),
	copy(Globals0, Globals1),
	IOState = io__state(A, B, C, Globals1, E),
	Globals = Globals0.

io__set_globals(Globals, io__state(A, B, C, _, E),
		io__state(A, B, C, Globals, E)).

io__progname_base(DefaultName, PrognameBase) -->
	io__progname(DefaultName, Progname),
	{ dir__basename(Progname, PrognameBase) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% environment interface predicates

io__get_environment_var(Var, OptValue) -->
	( { io__getenv(Var, Value) } ->
	    { OptValue0 = yes(Value) }
	;
	    { OptValue0 = no }
	),
	{ OptValue = OptValue0 }.

io__set_environment_var(Var, Value) -->
	{ string__format("%s=%s", [s(Var), s(Value)], EnvString) },
	( { io__putenv(EnvString) } ->
	    []
	;
	    % XXX What is good behaviour here?

	    { string__format("Could not set environment variable %s",
				[s(Var)], Message) },
	    { error(Message) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% memory management predicates

io__report_stats -->
	{ report_stats }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% miscellaneous predicates

io__init_state(ExternalState, IOState) :-
	map__init(Names0),
	map__init(PutBack),
	ops__init_op_table(OpTable),
	type_to_univ("<globals>", Globals),
	IOState0 = io__state(Names0, PutBack, OpTable, Globals, ExternalState),
	io__insert_std_stream_names(IOState0, IOState).

:- pred io__insert_std_stream_names(io__state, io__state).
:- mode io__insert_std_stream_names(di, uo) is det.

io__insert_std_stream_names -->
	io__stdin_stream(Stdin),
	io__insert_stream_name(Stdin, "<standard input>"),
	io__stdout_stream(Stdout),
	io__insert_stream_name(Stdout, "<standard output>"),
	io__stderr_stream(Stderr),
	io__insert_stream_name(Stderr, "<standard error>").

io__call_system(Command, Result) -->
	io__call_system_code(Command, Status),
	{ Status = -1 ->
		% XXX improve error message
		Result = error("can't invoke system command")
	;
		Result = ok(Status)
	}.

:- type io__error	==	string.		% This is subject to change.

io__error_message(Error, Error).

%-----------------------------------------------------------------------------%

	% XXX major design flaw with regard to unique modes and
	% io__get_op_table
/* old definition
io__get_op_table(OpTable) -->
	=(io__state(_, _, OpTable, _, _)).
*/
/* new definition - awfully inefficient! */
io__get_op_table(OpTable, IOState0, IOState) :-
	IOState0 = io__state(A, B, OpTable, D, E),
	copy(OpTable, OpTable1),
	IOState = io__state(A, B, OpTable1, D, E).

io__set_op_table(OpTable,	io__state(A, B, _, D, E),
				io__state(A, B, OpTable, D, E)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

/*
** The remaining predicates are implemented using the C interface.
** They are also implemented for NU-Prolog in `io.nu.nl'.
*/

:- pragma(c_header_code, "

#include ""init.h""
#include ""wrapper.h""
#include ""type_info.h""

#include <stdlib.h>
#include <string.h>
#include <errno.h>

/*
** Mercury files are not quite the same as C stdio FILEs,
** because we keep track of a little bit more information.
*/

typedef struct mercury_file {
	FILE *file;
	int line_number;
} MercuryFile;

extern MercuryFile mercury_stdin;
extern MercuryFile mercury_stdout;
extern MercuryFile mercury_stderr;
extern MercuryFile *mercury_current_text_input;
extern MercuryFile *mercury_current_text_output;
extern MercuryFile *mercury_current_binary_input;
extern MercuryFile *mercury_current_binary_output;

#define initial_external_state()	0	/* some random number */
#define update_io(r_src, r_dest)	((r_dest) = (r_src))
#define final_io_state(r)		((void)0)

void 		mercury_init_io(void);
MercuryFile*	mercury_open(const char *filename, const char *type);
int		mercury_output_error(MercuryFile* mf);
void		mercury_print_string(MercuryFile* mf, const char *s);
void		mercury_print_binary_string(MercuryFile* mf, const char *s);
int		mercury_getc(MercuryFile* mf);
void		mercury_close(MercuryFile* mf);
").

:- pragma(c_code, "

MercuryFile mercury_stdin = { NULL, 0 };
MercuryFile mercury_stdout = { NULL, 0 };
MercuryFile mercury_stderr = { NULL, 0 };
MercuryFile *mercury_current_text_input = &mercury_stdin;
MercuryFile *mercury_current_text_output = &mercury_stdout;
MercuryFile *mercury_current_binary_input = &mercury_stdin;
MercuryFile *mercury_current_binary_output = &mercury_stdout;

void
mercury_init_io(void)
{
	mercury_stdin.file = stdin;
	mercury_stdout.file = stdout;
	mercury_stderr.file = stderr;
}

").

:- pragma(c_code, "

MercuryFile*
mercury_open(const char *filename, const char *type)
{
	MercuryFile *mf;
	FILE *f;
	
	f = fopen(filename, type);
	if (!f) return NULL;
	mf = make(MercuryFile);
	mf->file = f;
	mf->line_number = 1;
	return mf;
}

").

:- pragma(c_code, "

int
mercury_output_error(MercuryFile* mf)
{
	fprintf(stderr,
		""Mercury runtime: error writing to output file: %s\\n"",
		strerror(errno));
	exit(1);
}

").

:- pragma(c_code, "

void
mercury_print_string(MercuryFile* mf, const char *s)
{
	if (fprintf(mf->file, ""%s"", s) < 0) {
		mercury_output_error(mf);
	}
	while (*s) {
		if (*s++ == '\\n') {
			mf->line_number++;
		}
	}
}

").

:- pragma(c_code, "

void
mercury_print_binary_string(MercuryFile* mf, const char *s)
{
	if (fprintf(mf->file, ""%s"", s) < 0) {
		mercury_output_error(mf);
	}
}

").

:- pragma(c_code, "

int
mercury_getc(MercuryFile* mf)
{
	int c = getc(mf->file);
	if (c == '\\n') {
		mf->line_number++;
	}
	return c;
}

").

:- pragma(c_code, "

void
mercury_close(MercuryFile* mf)
{
	if (mf != &mercury_stdin &&
	    mf != &mercury_stdout &&
	    mf != &mercury_stderr)
	{
		if (fclose(mf->file) < 0) {
			fprintf(stderr,
				""Mercury runtime: error closing file: %s\\n"",
				strerror(errno));
			exit(1);
		}
		oldmem(mf);
	}
}

").

:- pragma(c_header_code, "#include ""init.h""").
:- pragma(c_header_code, "#include ""prof.h""").
:- pragma(c_code, "

Declare_entry(mercury__io__init_state_2_0);

/* This code is the program startup point -- it is called by the Mercury
   runtime.

   The handwritten code below is almost equivalent to

	io__run :-
		initial_external_state(IO0),
		program_entry_point(IO0, IO),
		final_io_state(IO).

   except that program_entry_point is a variable, which is by default
   set to the address of main/2.
*/

Define_extern_entry(mercury__io__run_0_0);
Declare_label(mercury__io__run_0_0_i1);
Declare_label(mercury__io__run_0_0_i2);

BEGIN_MODULE(io_run_module)
	init_entry(mercury__io__run_0_0);
	init_label(mercury__io__run_0_0_i1);
	init_label(mercury__io__run_0_0_i2);
BEGIN_CODE
Define_entry(mercury__io__run_0_0);
        mkframe(""mercury__io__run_0_0"", 0, ENTRY(do_fail));
	r1 = initial_external_state();
	noprof_call(ENTRY(mercury__io__init_state_2_0),
		LABEL(mercury__io__run_0_0_i1));
Define_label(mercury__io__run_0_0_i1);
#ifdef	COMPACT_ARGS
#else
	r1 = r2;
#endif
	if (program_entry_point == NULL) {
		fatal_error(""no program entry point supplied"");
	}

#ifdef  PROFILE_TIME
	prof_init_time_profile();
#endif

	noprof_call(program_entry_point,
		LABEL(mercury__io__run_0_0_i2));

Define_label(mercury__io__run_0_0_i2);

#ifdef  PROFILE_TIME
	prof_turn_off_time_profiling();
	prof_output_addr_table();
#endif
#ifdef  PROFILE_CALLS
	prof_output_addr_pair_table();
#endif

	final_io_state(r2);
	succeed();
END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_io_run_module
*/
void sys_init_io_run_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_io_run_module(void) {
	extern ModuleFunc io_run_module;
	io_run_module();
}

").

/* input predicates */

:- pragma(c_code, io__read_char_code(File::in, CharCode::out, IO0::di, IO::uo),
"
	CharCode = mercury_getc((MercuryFile*)File);
	update_io(IO0, IO);
").

:- pragma(c_code, io__putback_char(File::in, Character::in, IO0::di, IO::uo),
"{
	MercuryFile* mf = (MercuryFile *)File;
	if (Character == '\\n') {
		mf->line_number--;
	}
	/* XXX should work even if ungetc() fails */
	if (ungetc(Character, mf->file) == EOF) {
		fatal_error(""io__putback_char: ungetc failed"");
	}
	update_io(IO0, IO);
}").

:- pragma(c_code, io__putback_byte(File::in, Character::in, IO0::di, IO::uo),
"{
	MercuryFile* mf = (MercuryFile *)File;
	/* XXX should work even if ungetc() fails */
	if (ungetc(Character, mf->file) == EOF) {
		fatal_error(""io__putback_byte: ungetc failed"");
	}
	update_io(IO0, IO);
}").

/* output predicates - with output to mercury_current_text_output */

:- pragma(c_code, io__write_string(Message::in, IO0::di, IO::uo), "
	mercury_print_string(mercury_current_text_output, Message);
	update_io(IO0, IO);
").

:- pragma(c_code, io__write_char(Character::in, IO0::di, IO::uo), "
	if (putc(Character, mercury_current_text_output->file) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	if (Character == '\\n') {
		mercury_current_text_output->line_number++;
	}
	update_io(IO0, IO);
").

:- pragma(c_code, io__write_int(Val::in, IO0::di, IO::uo), "
	if (fprintf(mercury_current_text_output->file, ""%ld"", (long) Val) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma(c_code, io__write_float(Val::in, IO0::di, IO::uo), "
	if (fprintf(mercury_current_text_output->file, ""%#.15g"", Val) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma(c_code, io__write_byte(Byte::in, IO0::di, IO::uo), "
	if (putc(Byte, mercury_current_binary_output->file) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma(c_code, io__write_bytes(Message::in, IO0::di, IO::uo), "{
	mercury_print_binary_string(mercury_current_binary_output, Message);
	update_io(IO0, IO);
}").

:- pragma(c_code, io__write(Anything::in, IO0::di, IO::uo), "{
	mercury_print_type((Word *) TypeInfo_for_T, Anything);
	update_io(IO0, IO);
}").

:- pragma(c_code, io__flush_output(IO0::di, IO::uo), "
	if (fflush(mercury_current_text_output->file) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma(c_code, io__flush_binary_output(IO0::di, IO::uo), "
	if (fflush(mercury_current_binary_output->file) < 0) {
		mercury_output_error(mercury_current_binary_output);
	}
	update_io(IO0, IO);
").

/* output predicates - with output to the specified stream */

:- pragma(c_code, io__write_string(Stream::in, Message::in, IO0::di, IO::uo),
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	mercury_print_string(stream, Message);
	update_io(IO0, IO);
}").

:- pragma(c_code, io__write_char(Stream::in, Character::in, IO0::di, IO::uo),
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (putc(Character, stream->file) < 0) {
		mercury_output_error(stream);
	}
	if (Character == '\\n') {
		stream->line_number++;
	}
	update_io(IO0, IO);
}").

:- pragma(c_code, io__write_int(Stream::in, Val::in, IO0::di, IO::uo), "{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fprintf(stream->file, ""%ld"", (long) Val) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma(c_code, io__write_float(Stream::in, Val::in, IO0::di, IO::uo), "{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fprintf(stream->file, ""%.15g"", Val) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma(c_code, io__write_byte(Stream::in, Byte::in, IO0::di, IO::uo), "{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (putc(Byte, stream->file) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma(c_code, io__write_bytes(Stream::in, Message::in, IO0::di, IO::uo), "{
	MercuryFile *stream = (MercuryFile *) Stream;
	mercury_print_binary_string(stream, Message);
	update_io(IO0, IO);
}").

:- pragma(c_code, io__flush_output(Stream::in, IO0::di, IO::uo), "{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fflush(stream->file) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma(c_code, io__flush_binary_output(Stream::in, IO0::di, IO::uo), "{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fflush(stream->file) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

/* stream predicates */

:- pragma(c_code, "

#ifdef  USE_TYPE_LAYOUT

	/* Rest of word is pointer to 2 cell struct */

extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_io__base_type_layout_io__stream_0b[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_io__base_type_layout_io__stream_0a[] = {
	(Word *) ((Integer) 2),
	(Word *) ((Integer) mercury_data_io__base_type_layout_io__stream_0b),
	(Word *) ((Integer) mercury_data_io__base_type_layout_io__stream_0b),
	(Word *) string_const(""io__stream"", 10)
};

Word * mercury_data_io__base_type_layout_io__stream_0[] = {
	make_typelayout_for_all_tags(TYPELAYOUT_SIMPLE_TAG, 
		((Integer) mercury_data_io__base_type_layout_io__stream_0a))
};

#endif

Define_extern_entry(mercury____Unify___io__stream_0_0);
Define_extern_entry(mercury____Index___io__stream_0_0);
Define_extern_entry(mercury____Compare___io__stream_0_0);
Define_extern_entry(mercury____Term_To_Type___io__stream_0_0);
Define_extern_entry(mercury____Type_To_Term___io__stream_0_0);

BEGIN_MODULE(io_stream_module)
	init_entry(mercury____Unify___io__stream_0_0);
	init_entry(mercury____Index___io__stream_0_0);
	init_entry(mercury____Compare___io__stream_0_0);
	init_entry(mercury____Term_To_Type___io__stream_0_0);
	init_entry(mercury____Type_To_Term___io__stream_0_0);
BEGIN_CODE

Define_entry(mercury____Unify___io__stream_0_0);
	unify_output =
		((MercuryFile*) unify_input1 == (MercuryFile *) unify_input2);
	proceed();

Define_entry(mercury____Index___io__stream_0_0);
	index_output = -1;
	proceed();

Define_entry(mercury____Compare___io__stream_0_0);
	compare_output = ((compare_input1 < compare_input2) ? COMPARE_LESS :
		          (compare_input1 > compare_input2) ? COMPARE_GREATER :
			  				      COMPARE_EQUAL);
	proceed();

Define_entry(mercury____Term_To_Type___io__stream_0_0);
	/* don't know what to put here. */
	fatal_error(""cannot convert term to type io__stream"");

Define_entry(mercury____Type_To_Term___io__stream_0_0);
	/* don't know what to put here. */
	fatal_error(""cannot covert type io__stream to term"");

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_io_stream_module
*/
void sys_init_io_stream_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_io_stream_module(void) {
	extern ModuleFunc io_stream_module;
	io_stream_module();
}

").

:- pragma(c_code, io__stdin_stream(Stream::out, IO0::di, IO::uo), "
	Stream = (Word) &mercury_stdin;
	update_io(IO0, IO);
").

:- pragma(c_code, io__stdout_stream(Stream::out, IO0::di, IO::uo), "
	Stream = (Word) &mercury_stdout;
	update_io(IO0, IO);
").

:- pragma(c_code, io__stderr_stream(Stream::out, IO0::di, IO::uo), "
	Stream = (Word) &mercury_stderr;
	update_io(IO0, IO);
").

:- pragma(c_code, io__stdin_binary_stream(Stream::out, IO0::di, IO::uo), "
	Stream = (Word) &mercury_stdin;
	update_io(IO0, IO);
").

:- pragma(c_code, io__stdout_binary_stream(Stream::out, IO0::di, IO::uo), "
	Stream = (Word) &mercury_stdout;
	update_io(IO0, IO);
").

:- pragma(c_code, io__input_stream(Stream::out, IO0::di, IO::uo), "
	Stream = (Word) mercury_current_text_input;
	update_io(IO0, IO);
").

:- pragma(c_code, io__output_stream(Stream::out, IO0::di, IO::uo), "
	Stream = (Word) mercury_current_text_output;
	update_io(IO0, IO);
").

:- pragma(c_code, io__binary_input_stream(Stream::out, IO0::di, IO::uo), "
	Stream = (Word) mercury_current_binary_input;
	update_io(IO0, IO);
").

:- pragma(c_code, io__binary_output_stream(Stream::out, IO0::di, IO::uo), "
	Stream = (Word) mercury_current_binary_output;
	update_io(IO0, IO);
").

:- pragma(c_code, io__get_line_number(LineNum::out, IO0::di, IO::uo), "
	LineNum = mercury_current_text_input->line_number;
	update_io(IO0, IO);
").
	
:- pragma(c_code,
	io__get_line_number(Stream::in, LineNum::out, IO0::di, IO::uo),
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	LineNum = stream->line_number;
	update_io(IO0, IO);
}").
	
:- pragma(c_code, io__set_line_number(LineNum::in, IO0::di, IO::uo), "
	mercury_current_text_input->line_number = LineNum;
	update_io(IO0, IO);
").
	
:- pragma(c_code,
	io__set_line_number(Stream::in, LineNum::in, IO0::di, IO::uo),
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	stream->line_number = LineNum;
	update_io(IO0, IO);
}").
	
% io__set_input_stream(NewStream, OldStream, IO0, IO1)
%	Changes the current input stream to the stream specified.
%	Returns the previous stream.
:- pragma(c_code,
	io__set_input_stream(NewStream::in, OutStream::out, IO0::di, IO::uo),
"
	OutStream = (Word) mercury_current_text_input;
	mercury_current_text_input = (MercuryFile*) NewStream;
	update_io(IO0, IO);
").

:- pragma(c_code,
	io__set_output_stream(NewStream::in, OutStream::out, IO0::di, IO::uo),
"
	OutStream = (Word) mercury_current_text_output;
	mercury_current_text_output = (MercuryFile*) NewStream;
	update_io(IO0, IO);
").

:- pragma(c_code,
	io__set_binary_input_stream(NewStream::in, OutStream::out,
					IO0::di, IO::uo),
"
	OutStream = (Word) mercury_current_binary_input;
	mercury_current_binary_input = (MercuryFile*) NewStream;
	update_io(IO0, IO);
").

:- pragma(c_code,
	io__set_binary_output_stream(NewStream::in, OutStream::out,
					IO0::di, IO::uo),
"
	OutStream = (Word) mercury_current_binary_output;
	mercury_current_binary_output = (MercuryFile*) NewStream;
	update_io(IO0, IO);
").

/* stream open/close predicates */

% io__do_open(File, Mode, ResultCode, Stream, IO0, IO1).
%	Attempts to open a file in the specified mode.
%	ResultCode is 0 for success, -1 for failure.
:- pragma(c_code,
	io__do_open(FileName::in, Mode::in, ResultCode::out,
			Stream::out, IO0::di, IO::uo),
"
	Stream = (Word) mercury_open(FileName, Mode);
	ResultCode = (Stream ? 0 : -1);
	update_io(IO0, IO);
").

:- pragma(c_code, io__close_input(Stream::in, IO0::di, IO::uo), "
	mercury_close((MercuryFile*)Stream);
	update_io(IO0, IO);
").

:- pragma(c_code, io__close_output(Stream::in, IO0::di, IO::uo), "
	mercury_close((MercuryFile*)Stream);
	update_io(IO0, IO);
").

:- pragma(c_code, io__close_binary_input(Stream::in, IO0::di, IO::uo), "
	mercury_close((MercuryFile*)Stream);
	update_io(IO0, IO);
").

:- pragma(c_code, io__close_binary_output(Stream::in, IO0::di, IO::uo), "
	mercury_close((MercuryFile*)Stream);
	update_io(IO0, IO);
").

/* miscellaneous predicates */

:- pragma(c_code,
	io__progname(DefaultProgname::in, Progname::out, IO0::di, IO::uo),
"
	/*
	** XXX need to guarantee alignment of strings
	** (in this case, the string `progname')
	*/
	Progname = (progname ? progname : DefaultProgname);
	update_io(IO0, IO);
").

:- pragma(c_code, io__command_line_arguments(Args::out, IO0::di, IO::uo), "
	/* convert mercury_argv from a vector to a list */
	{ int i = mercury_argc;
	  Args = list_empty();
	  while (--i >= 0) {
		Args = list_cons((Word) mercury_argv[i], Args);
	  }
	}
	update_io(IO0, IO);
").

:- pragma(c_code, io__get_exit_status(ExitStatus::out, IO0::di, IO::uo), "
	ExitStatus = mercury_exit_status;
	update_io(IO0, IO);
").

:- pragma(c_code, io__set_exit_status(ExitStatus::in, IO0::di, IO::uo), "
	mercury_exit_status = ExitStatus;
	update_io(IO0, IO);
").

:- pragma(c_code, io__preallocate_heap_space(HeapSpace::in, IO0::di, IO::uo),
"
	/* HeapSpace not used */
	/* don't do anything - preallocate_heap_space was just a
	   hack for NU-Prolog */
	update_io(IO0, IO);
").

:- pragma(c_code,
	io__call_system_code(Command::in, Status::out, IO0::di, IO::uo),
"
	Status = system(Command);
	update_io(IO0, IO);
").

/*---------------------------------------------------------------------------*/

/* io__getenv and io__putenv, from io.m */

:- pragma(c_code, io__getenv(Var::in, Value::out), "{
	Value = getenv(Var);
	SUCCESS_INDICATOR = (Value != 0);
}").

:- pragma(c_code, io__putenv(VarAndValue::in), "
	SUCCESS_INDICATOR = (putenv(VarAndValue) == 0);
").

/*---------------------------------------------------------------------------*/

:- pragma(c_code, "

#ifdef  USE_TYPE_LAYOUT

	/* 
	 * We'll just pretend the io__external_state is an
	 * integer. For memory copying application, this is
	 * close enough.
	 */

Word * mercury_data_io__base_type_layout_io__external_state_0[] = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_INT_VALUE))
};

#endif

Define_extern_entry(mercury____Unify___io__external_state_0_0);
Define_extern_entry(mercury____Compare___io__external_state_0_0);
Declare_label(mercury____Compare___io__external_state_0_0_i1);
Define_extern_entry(mercury____Index___io__external_state_0_0);
Define_extern_entry(mercury____Type_To_Term___io__external_state_0_0);
Define_extern_entry(mercury____Term_To_Type___io__external_state_0_0);

BEGIN_MODULE(unify_external_state_module)
	init_entry(mercury____Unify___io__external_state_0_0);
	init_entry(mercury____Compare___io__external_state_0_0);
	init_entry(mercury____Index___io__external_state_0_0);
	init_entry(mercury____Type_To_Term___io__external_state_0_0);
	init_entry(mercury____Term_To_Type___io__external_state_0_0);
BEGIN_CODE

Define_entry(mercury____Unify___io__external_state_0_0);
Define_entry(mercury____Compare___io__external_state_0_0);
Define_entry(mercury____Index___io__external_state_0_0);
Define_entry(mercury____Term_To_Type___io__external_state_0_0);
Define_entry(mercury____Type_To_Term___io__external_state_0_0);
	/* the unique mode system should prevent these */
	fatal_error(""cannot unify/compare/index/term_to_type/type_to_term io__external_state"");

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_unify_external_state_module
*/
	/* suppress gcc -Wmissing-decl warning */
void sys_init_unify_external_state_module(void);
void sys_init_unify_external_state_module(void) {
	extern ModuleFunc unify_external_state_module;
	unify_external_state_module();
}

").

/*---------------------------------------------------------------------------*/

% Code for io__write.

:- pragma(c_code, "

	/* Prototypes */

void mercury_print_const(Word data_value, Word entry_value);
void mercury_print_enum(Word data_value, Word entry_value);
void mercury_print_simple(Word data_value, Word entry_value, Word * type_info);
void mercury_print_builtin(Word data_value, Word entry_value);
void mercury_print_complicated(Word data_value, Word entry_value, 
		Word * type_info);

void mercury_print_type(Word *type_info, Word data_value);
Word * make_type_info(Word *term_type_info, Word *arg_pseudo_type_info,
	int *allocated);


	/* 
	 * Given a type_info (term_type_info) which contains a
	 * base_type_info pointer and possibly other type_infos
	 * giving the values of the type parameters of this type,
	 * and a pseudo-type_info (arg_pseudo_type_info), which contains a
	 * base_type_info pointer and possibly other type_infos
	 * giving EITHER
	 * 	- the values of the type parameters of this type,
	 * or	- an indication of the type parameter of the
	 * 	  term_type_info that should be substituted here
	 *
	 * This returns a fully instantiated type_info, a version of the
	 * arg_pseudo_type_info with all the type variables filled in.
	 * If there are no type variables to fill in, we return the
	 * arg_pseudo_type_info, unchanged. Otherwise, we allocate
	 * memory using malloc().  If memory is allocated, the integer
	 * argument (passed by reference) is set to 1, otherwise it is
	 * set to 0.  It is the caller's responsibility to check whether 
	 * the call to make_type_info allocated memory, and if so, free
	 * it.
	 *
	 * This code could be tighter. In general, we want to
	 * handle our own allocations rather than using malloc().
	 * Also, we might be able to do only one traversal.
	 */

Word * make_type_info(Word *term_type_info, Word *arg_pseudo_type_info,
	int *allocated) 
{
	int arity, i;
	Word base_type_info;
	Word *type_info;

	*allocated = 0;

		/* The arg_pseudo_type_info might be a polymorphic variable */

	if ((Word) arg_pseudo_type_info < TYPELAYOUT_MAX_VARINT) {
		return (Word *) term_type_info[(Word) arg_pseudo_type_info];
	}


	base_type_info = arg_pseudo_type_info[0];

		/* no arguments - optimise common case */
	if (base_type_info == 0) {
		return arg_pseudo_type_info;
	} else {
		arity = ((Word *) base_type_info)[0];
	}

	for (i = arity; i > 0; i--) {
		if (arg_pseudo_type_info[i] < TYPELAYOUT_MAX_VARINT) {
			break;
		}
	}

		/* 
		 * See if any of the arguments were polymorphic.
		 * If so, substitute.
		 */
	if (i > 0) {
		type_info = checked_malloc(arity * sizeof(Word));
		*allocated = 1;
		for (i = 0; i <= arity; i++) {
			if (arg_pseudo_type_info[i] < TYPELAYOUT_MAX_VARINT) {
				type_info[i] = term_type_info[arg_pseudo_type_info[i]];
				if (type_info[i] < TYPELAYOUT_MAX_VARINT) {
					fatal_error(""Error! Can't instantiate type variable."");
				}
			} else {
				type_info[i] = arg_pseudo_type_info[i];
			}
		}
		return type_info;
	} else {
		return arg_pseudo_type_info;
	}

}

/*
 * Print a constant value
 */

void
mercury_print_const(Word data_value, Word entry_value) 
{

#ifdef DEBUG_IO__WRITE
	printf(""This is a constant functor, %ld of %ld with this tag\n"",
            data_value + 1, ((Word *) entry_value)[1]); 
#endif

	/* the functors are stored after the enum_indicator and
	 * the number of functors
	 */
	printf(""%s"", (char *) ((Word *) entry_value)[data_value + 2]);	
}

void
mercury_print_enum(Word data_value, Word entry_value) 
{

#ifdef DEBUG_IO__WRITE
	printf(""This is a constant functor, %ld of %ld in this enum\n"",
            data_value + 1, entry_value[1]); 
#endif

	/* the functors are stored after the enum_indicator and
	 * the number of functors
	 */

	printf(""%s"", (char *) ((Word *) entry_value)[data_value + 2]);	
}


/* 
 * Simple tags - type_layout points to an array containing
 * the arity, then a pseudo-typeinfo for each argument.
 *
 * Data word points to an array of argument data.
 */
void
mercury_print_simple(Word data_value, Word entry_value, Word * type_info) 
{
	int num_args, i;
	int allocated = 0;

	num_args = field(0, (Word *) entry_value, 0);

#ifdef DEBUG_IO__WRITE
	printf(""This functor has %d arguments.\n"", num_args); 
#endif
	
	printf(""%s("", (char *) ((Word *) entry_value)[num_args + 1]);

	for (i = 0; i < num_args ; i++) {
		Word * arg_pseudo_type_info;
		Word * arg_type_info;

		if (i != 0) {
			printf("", "");
		}

#ifdef DEBUG_IO__WRITE
		printf(""Argument %d of %d is:\n"", i+1, num_args);
#endif

		arg_pseudo_type_info = (Word *) ((Word *) entry_value)[i + 1];

#ifdef DEBUG_IO__WRITE
		printf(""Entry %ld Data %ld "", (Word) entry_type_info,
			((Word *) data_value)[i]); 
#endif
		arg_type_info = make_type_info(type_info, (Word *) 
			arg_pseudo_type_info, &allocated);

#ifdef DEBUG_IO__WRITE
		printf(""Typeinfo %ld\n"", (Word) entry_type_info);
#endif
		mercury_print_type(arg_type_info, ((Word *) data_value)[i]);

		if (allocated) {
			free(arg_type_info);
		}

	}

	printf("")"");
}

/*
 * Complicated tags - entry_value points to a vector containing: 
 *	The number of sharers of this tag
 *	A pointer to a simple tag structure (see mercury_print_simple)
 *	for each sharer.
 *
 *	The data_value points to the actual sharer of this tag, 
 *	which should be used as an index into the vector of pointers
 *	into simple tag structures. The next n words the data_value
 *	points to are the arguments of the functor.
 */

void
mercury_print_complicated(Word data_value, Word entry_value, Word * type_info) 
{
	Word new_data_value, new_entry_value, new_entry_body,
		new_entry_tag, secondary_tag;

	secondary_tag = ((Word *) data_value)[0];

#ifdef DEBUG_IO__WRITE
	printf(""This is %ld of %ld functors sharing this tag\n"",
		secondary_tag + 1, ((Word *) entry_value)[0]); 
#endif

	new_entry_value = ((Word *) entry_value)[secondary_tag + 1];
	new_entry_tag = tag(new_entry_value);
	new_entry_body = body(new_entry_value, new_entry_tag);
	new_data_value = (Word) ((Word *) data_value + 1);

	mercury_print_simple(new_data_value, new_entry_body, type_info);
}

void
mercury_print_builtin(Word data_value, Word entry_value) 
{

	switch ((int) entry_value) {
	
	case TYPELAYOUT_UNASSIGNED_VALUE:
		fatal_error(""Attempt to use an UNASSIGNED tag in io__write."");
		break;

	case TYPELAYOUT_UNUSED_VALUE:
		fatal_error(""Attempt to use an UNUSED tag in io__write."");
		break;

	case TYPELAYOUT_STRING_VALUE:
		if (fprintf(mercury_current_text_output->file, 
			""%c%s%c"", '""', (char *) data_value, '""') < 0) {
			mercury_output_error(mercury_current_text_output);
		}
		break;

	case TYPELAYOUT_FLOAT_VALUE:
	{
		Float f;
		f = word_to_float(data_value);
		if (fprintf(mercury_current_text_output->file, 
			""%#.15g"", f) < 0) {
			mercury_output_error(mercury_current_text_output);
		}
	}
	break;

	case TYPELAYOUT_INT_VALUE:
		if (fprintf(mercury_current_text_output->file, ""%ld"", 
			(long) data_value) < 0) {
			mercury_output_error(mercury_current_text_output);
		}
	break;

	case TYPELAYOUT_CHARACTER_VALUE:
		if (fprintf(mercury_current_text_output->file, ""\'%c\'"",
			(char) data_value) < 0) {
			mercury_output_error(mercury_current_text_output);
		}
		if (data_value == '\\n') {
			mercury_current_text_output->line_number++;
		}
		break;

	case TYPELAYOUT_UNIV_VALUE:

#ifdef DEBUG_IO__WRITE
	printf(""This is a univ, it is really a:\n"");
#endif
		/* Univ is a two word structure, containing
		 * type_info and data.
		 */
		mercury_print_type((Word *) ((Word *) data_value)[0], 
			((Word *) data_value)[1]);
		break;

	case TYPELAYOUT_PREDICATE_VALUE:
		if (fprintf(mercury_current_text_output->file, 
			""<<predicate>>"") < 0) { 
			mercury_output_error(mercury_current_text_output);
		}
		break;

	default:
		fatal_error(""Invalid tag value in io__write"");
		break;
	}

}

	/*
	 * Print out Mercury data, given its type_info, and the data 
	 * itself.
	 *
	 * Note: The variable entry_value and data_value are used
	 * for a number of purposes, as depending on the type of the
	 * data, they can be represent many things.
	 *
	 *
	 */

void
mercury_print_type(Word *type_info, Word data_word) 
{
	Word *base_type_info, *arg_type_info, *base_type_layout;
	Word data_value, entry_value, base_type_layout_entry;
	int entry_tag, data_tag; 

	base_type_info = (Word *) type_info[0];

		/* 
		 * Find the base_type_info - type_infos for types with no args 
		 * are themselves base_type_infos
		 */

	if(base_type_info == 0) {
		base_type_info = type_info;
	}

		/* Retrieve base_type_layout */
	base_type_layout = (Word *) base_type_info[OFFSET_FOR_BASE_TYPE_LAYOUT];

	data_tag = tag(data_word);
	data_value = body(data_word, data_tag);
	
	base_type_layout_entry = base_type_layout[data_tag];

	entry_tag = tag(base_type_layout_entry);
	entry_value = body(base_type_layout_entry, entry_tag);
	
	switch(entry_tag) {

	case TYPELAYOUT_CONST_TAG: /* case TYPELAYOUT_COMP_CONST_TAG: */

		/* Is it a builtin or a constant/enum? */ 

		if (entry_value > TYPELAYOUT_MAX_VARINT) {

			/* Check enum indicator */

			if (((Word *) entry_value)[0]) {
				mercury_print_enum(data_word, entry_value);
			} else {
				data_value = unmkbody(data_value);
				mercury_print_const(data_value, entry_value);
			}
		} else {
			entry_value = unmkbody(entry_value);
			mercury_print_builtin(data_word, entry_value);
		}
		break;

	case TYPELAYOUT_SIMPLE_TAG:
		mercury_print_simple(data_value, entry_value, type_info);
		break;

	case TYPELAYOUT_COMPLICATED_TAG:
		mercury_print_complicated(data_value, entry_value, type_info);
		break;

	case TYPELAYOUT_EQUIV_TAG: /* case TYPELAYOUT_NO_TAG: */
	{
		int allocated = 0; 

#ifdef DEBUG_IO__WRITE
		printf(""Equivalent to:\n""); 
#endif

		/* is it equivalent to a type variable? */

		if (entry_value < TYPELAYOUT_MAX_VARINT) {
			arg_type_info = make_type_info(type_info, 
				(Word *) entry_value, &allocated);
			mercury_print_type(arg_type_info, data_word);
			if (allocated) {
				free(arg_type_info);
			}
		}
			/* is it a no_tag type? */
		else if (((Word *) entry_value)[0]) {
			mercury_print_simple((Word) &data_word, entry_value, 
				type_info);
		}
			/* is it an equivalent type */
		else {
			arg_type_info = make_type_info(type_info, 
				(Word *) ((Word *) entry_value)[1], &allocated);
			mercury_print_type(arg_type_info, data_word);
			if (allocated) {
				free(arg_type_info);
			}
		}

	}
	break;

	default:
		/* If this happens, the layout data is corrupt */

		fatal_error(""Found unused tag value in io__write"");
	}
}


").


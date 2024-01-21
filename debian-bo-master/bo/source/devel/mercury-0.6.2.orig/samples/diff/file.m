%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: bromage
% Simplified by Marnix Klooster <marnix@worldonline.nl>
% Last changed 22 October 1996

% This module provides file input.  One can read a file entirely,
% select a single line from a read file, get the number of lines
% in a read file, and convert a read file to a list of strings.

%-----------------------------------------------------------------------------%

:- module file.
:- interface.

:- import_module io, list, string.

:- type file.

	% file__read_file reads a file from a filename.
:- pred file__read_file(string, io__res(file), io__state, io__state).
:- mode file__read_file(in, out, di, uo) is det.

	% file__read_input reads a file from the input
	% stream.
:- pred file__read_input(io__res(file), io__state, io__state).
:- mode file__read_input(out, di, uo) is det.

	% file__get_line retrieves a line from a file.
	% (Lines are numbered from 0.)
	% Fails if the line is out of bounds.
:- pred file__get_line(file, int, string).
:- mode file__get_line(in, in, out) is semidet.

	% file__get_numlines returns the number of lines
	% in a file.
:- pred file__get_numlines(file, int).
:- mode file__get_numlines(in, out) is det.

	% file__to_list converts a file into a list of
	% lines.
:- pred file__to_list(file, list(string)).
:- mode file__to_list(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module array, require, int.

%-----------------------------------------------------------------------------%

:- type file == array(string).

	% Open the stream, read from the stream, then close
	% the stream.
file__read_file(FileName, Contents) -->
	io__open_input(FileName, Res),
	( { Res = ok(InputStream) },
	    file__read_stream(InputStream, Contents0),
	    io__close_input(InputStream),
	    { Contents = ok(Contents0) }
	; { Res = error(Error) },
	    { Contents = error(Error) }
	).

	% Get the input stream, then read from it.
file__read_input(ok(Contents)) -->
	io__input_stream(InputStream),
	file__read_stream(InputStream, Contents).

	% file__read_stream is the "real" file reader.
:- pred file__read_stream(io__input_stream, file, io__state, io__state).
:- mode file__read_stream(in, out, di, uo) is det.
file__read_stream(Stream, File) -->
	file__read_stream2(Stream, 0, _, File).

	% Given a Stream from which LinesIn lines have already been
	% read, fill File[LinesIn] to File[LinesOut-1] with the rest
	% of the lines.  LinesOut is the number of lines in the file.
	% (Note that line numbering starts at zero.)
:- pred file__read_stream2(io__input_stream, int, int, file,
		io__state, io__state).
:- mode file__read_stream2(in, in, out, out, di, uo) is det.
file__read_stream2(Stream, LinesIn, LinesOut, File) -->
	io__read_line(Stream, Res),
	( { Res = eof },
            { LinesOut = LinesIn },
	    { LinesOut1 is LinesOut - 1 },
	    { array__init(0, LinesOut1, "", File) }
	; { Res = ok(Line) },
	    { string__from_char_list(Line, Line1) },
            { LinesIn1 is LinesIn + 1 },
	    file__read_stream2(Stream, LinesIn1, LinesOut, File1),
	    { array__set(File1, LinesIn, Line1, File) }
	; { Res = error(Error) },
	    { io__error_message(Error, Msg) },
	    { error(Msg) }
	).

%-----------------------------------------------------------------------------%

file__get_line(File, LineNo, Line) :-
	array__semidet_lookup(File, LineNo, Line).

file__get_numlines(File, NumLines) :-
	array__bounds(File, _, NumLines1),
	NumLines is NumLines1 + 1.

file__to_list(File, List) :-
	array__to_list(File, List).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

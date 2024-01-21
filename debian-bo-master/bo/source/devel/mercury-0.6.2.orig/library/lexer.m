%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: lexer.m.
% main author: fjh.
% stability: high.
%
% Lexical analysis.  This module defines the representation of tokens
% and exports predicates for reading in tokens from an input stream.
%
% See ISO Prolog 6.4.  Also see the comments at the top of parser.m.
%
%-----------------------------------------------------------------------------%

:- module lexer.
:- interface.
:- import_module char, string, int, float, list, std_util, io.

:- type	token
	--->	name(string)
	;	variable(string)
	;	integer(int)
	;	float(float)
	;	string(string)		% "...."
	;	open			% '('
	;	open_ct			% '(' without any preceding whitespace
	;	close			% ')'
	;	open_list		% '['
	;	close_list		% ']'
	;	open_curly		% '}'
	;	close_curly		% '{'
	;	ht_sep			% '|'
	;	comma			% ','
	;	end			% '.'
	;	junk(character)		% junk character in the input stream
	;	error(string)		% some other invalid token
	;	io_error(io__error)	% error reading from the input stream
	;	eof.			% end-of-file

% For every token, we record the line number of the line on
% which the token occurred.

:- type token_context == int.	% line number

:- type token_list == list(pair(token, token_context)).

:- pred lexer__get_token_list(token_list, io__state, io__state).
:- mode lexer__get_token_list(out, di, uo) is det.
%	Read a list of tokens from the current input stream.
%	Keep reading until either we encounter either an `end' token
%	(i.e. a full stop followed by whitespace) or the end-of-file.

:- pred lexer__token_to_string(token, string).
:- mode lexer__token_to_string(in, out) is det.
%	Convert a token to a human-readable string describing the token.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require.

lexer__token_to_string(name(Name), String) :-
	string__append_list(["token '", Name, "'"], String).
lexer__token_to_string(variable(Var), String) :-
	string__append_list(["variable `", Var, "'"], String).
lexer__token_to_string(integer(Int), String) :-
	string__int_to_string(Int, IntString),
	string__append_list(["integer `", IntString, "'"], String).
lexer__token_to_string(float(Float), String) :-
	string__float_to_string(Float, FloatString),
	string__append_list(["float `", FloatString, "'"], String).
lexer__token_to_string(string(Token), String) :-
	string__append_list(["string """, Token, """"], String).
lexer__token_to_string(open, "token ` ('").
lexer__token_to_string(open_ct, "token `('").
lexer__token_to_string(close, "token `)'").
lexer__token_to_string(open_list, "token `['").
lexer__token_to_string(close_list, "token `]'").
lexer__token_to_string(open_curly, "token `{'").
lexer__token_to_string(close_curly, "token `}'").
lexer__token_to_string(ht_sep, "token `|'").
lexer__token_to_string(comma, "token `,'").
lexer__token_to_string(end, "token `. '").
lexer__token_to_string(eof, "end-of-file").
lexer__token_to_string(junk(JunkChar), String) :-
	char__to_int(JunkChar, Code),
	string__int_to_base_string(Code, 16, Hex),
	string__append_list(["illegal character <<0x", Hex, ">>"], String).
lexer__token_to_string(io_error(IO_Error), String) :-
	io__error_message(IO_Error, IO_ErrorMessage),
	string__append("I/O error: ", IO_ErrorMessage, String).
lexer__token_to_string(error(Message), String) :-
	string__append_list(["illegal token (", Message, ")"], String).

	% We build the tokens up as lists of characters in reverse order.
	% When we get to the end of each token, we call
	% `lexer__rev_char_list_to_string/2' to convert that representation
	% into a string.

	% Comments of the form
	%	foo --> bar . baz
	% mean that we are parsing a `foo', and we've already scanned
	% past the `bar', so now we need to match with a `baz'.

lexer__get_token_list(Tokens) -->
	lexer__get_token(Token, Context),
	( { Token = eof } ->
		{ Tokens = [] }
	; { Token = end ; Token = error(_) ; Token = io_error(_) } ->
		{ Tokens = [Token - Context] }
	;
		{ Tokens = [Token - Context | Tokens1] },
		lexer__get_token_list(Tokens1)
	).

:- pred lexer__get_token(token, token_context, io__state, io__state).
:- mode lexer__get_token(out, out, di, uo) is det.

lexer__get_token(Token, Context) -->
	lexer__get_token_1(Token),
	lexer__get_context(Context).

:- pred lexer__get_context(token_context, io__state, io__state).
:- mode lexer__get_context(out, di, uo) is det.

lexer__get_context(Context) -->
	io__get_line_number(Context).

%-----------------------------------------------------------------------------%

:- pred lexer__get_token_1(token, io__state, io__state).
:- mode lexer__get_token_1(out, di, uo) is det.

	% We use a big switch on Char here for efficiency - obviously not
	% for conciseness.
lexer__get_token_1(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = eof }
	; { Result = ok(Char) },
		( { Char = '\001', Token = junk(Char) }
		; { Char = '\002', Token = junk(Char) }
		; { Char = '\003', Token = junk(Char) }
		; { Char = '\004', Token = junk(Char) }
		; { Char = '\005', Token = junk(Char) }
		; { Char = '\006', Token = junk(Char) }
		; { Char = '\007', Token = junk(Char) }
		; { Char = '\010', Token = junk(Char) }
% \t		; { Char = '\011', Token = junk(Char) }
		; { Char = '\t' }, lexer__get_token_2(Token)
% \n		; { Char = '\012', Token = junk(Char) }
		; { Char = '\n' }, lexer__get_token_2(Token)
		; { Char = '\013', Token = junk(Char) }
% \f		; { Char = '\014', Token = junk(Char) }
		; { Char = '\f' }, lexer__get_token_2(Token)
		; { Char = '\015', Token = junk(Char) }
		; { Char = '\016', Token = junk(Char) }
		; { Char = '\017', Token = junk(Char) }
		; { Char = '\020', Token = junk(Char) }
		; { Char = '\021', Token = junk(Char) }
		; { Char = '\022', Token = junk(Char) }
		; { Char = '\023', Token = junk(Char) }
		; { Char = '\024', Token = junk(Char) }
		; { Char = '\025', Token = junk(Char) }
		; { Char = '\026', Token = junk(Char) }
		; { Char = '\027', Token = junk(Char) }
		; { Char = '\030', Token = junk(Char) }
		; { Char = '\031', Token = junk(Char) }
		; { Char = '\032', Token = junk(Char) }
		; { Char = '\033', Token = junk(Char) }
		; { Char = '\034', Token = junk(Char) }
		; { Char = '\035', Token = junk(Char) }
		; { Char = '\036', Token = junk(Char) }
		; { Char = '\037', Token = junk(Char) }
		; { Char = ' ' }, lexer__get_token_2(Token)
		; { Char = ('!') }, { Token = name("!") }
		; { Char = '"' }, lexer__get_quoted_name(Char, [], Token)
		; { Char = ('#') }, lexer__get_source_line_number([], Token)
		; { Char = ('$') }, lexer__get_graphic([Char], Token)
		; { Char = ('%') }, lexer__skip_to_eol(Token)
		; { Char = ('&') }, lexer__get_graphic([Char], Token)
		; { Char = '''' }, lexer__get_quoted_name(Char, [], Token)
		; { Char = ('(') }, { Token = open_ct }
		; { Char = (')') }, { Token = close }
		; { Char = ('*') }, lexer__get_graphic([Char], Token)
		; { Char = ('+') }, lexer__get_graphic([Char], Token)
		; { Char = (',') }, { Token = comma }
		; { Char = ('-') }, lexer__get_graphic([Char], Token)
		; { Char = ('.') }, lexer__get_dot(Token)
		; { Char = ('/') }, lexer__get_slash(Token)
		; { Char = '0' }, lexer__get_zero(Token)
		; { Char = '1' }, lexer__get_number([Char], Token)
		; { Char = '2' }, lexer__get_number([Char], Token)
		; { Char = '3' }, lexer__get_number([Char], Token)
		; { Char = '4' }, lexer__get_number([Char], Token)
		; { Char = '5' }, lexer__get_number([Char], Token)
		; { Char = '6' }, lexer__get_number([Char], Token)
		; { Char = '7' }, lexer__get_number([Char], Token)
		; { Char = '8' }, lexer__get_number([Char], Token)
		; { Char = '9' }, lexer__get_number([Char], Token)
		; { Char = (':') }, lexer__get_graphic([Char], Token)
		; { Char = (';') }, { Token = name(";") }
		; { Char = ('<') }, lexer__get_graphic([Char], Token)
		; { Char = ('=') }, lexer__get_graphic([Char], Token)
		; { Char = ('>') }, lexer__get_graphic([Char], Token)
		; { Char = ('?') }, lexer__get_graphic([Char], Token)
		; { Char = ('@') }, lexer__get_graphic([Char], Token)
		; { Char = 'A' }, lexer__get_variable([Char], Token)
		; { Char = 'B' }, lexer__get_variable([Char], Token)
		; { Char = 'C' }, lexer__get_variable([Char], Token)
		; { Char = 'D' }, lexer__get_variable([Char], Token)
		; { Char = 'E' }, lexer__get_variable([Char], Token)
		; { Char = 'F' }, lexer__get_variable([Char], Token)
		; { Char = 'G' }, lexer__get_variable([Char], Token)
		; { Char = 'H' }, lexer__get_variable([Char], Token)
		; { Char = 'I' }, lexer__get_variable([Char], Token)
		; { Char = 'J' }, lexer__get_variable([Char], Token)
		; { Char = 'K' }, lexer__get_variable([Char], Token)
		; { Char = 'L' }, lexer__get_variable([Char], Token)
		; { Char = 'M' }, lexer__get_variable([Char], Token)
		; { Char = 'N' }, lexer__get_variable([Char], Token)
		; { Char = 'O' }, lexer__get_variable([Char], Token)
		; { Char = 'P' }, lexer__get_variable([Char], Token)
		; { Char = 'Q' }, lexer__get_variable([Char], Token)
		; { Char = 'R' }, lexer__get_variable([Char], Token)
		; { Char = 'S' }, lexer__get_variable([Char], Token)
		; { Char = 'T' }, lexer__get_variable([Char], Token)
		; { Char = 'U' }, lexer__get_variable([Char], Token)
		; { Char = 'V' }, lexer__get_variable([Char], Token)
		; { Char = 'W' }, lexer__get_variable([Char], Token)
		; { Char = 'X' }, lexer__get_variable([Char], Token)
		; { Char = 'Y' }, lexer__get_variable([Char], Token)
		; { Char = 'Z' }, lexer__get_variable([Char], Token)
		; { Char = ('[') }, { Token = open_list }
		; { Char = ('\\') }, lexer__get_graphic([Char], Token)
		; { Char = (']') }, { Token = close_list }
		; { Char = ('^') }, lexer__get_graphic([Char], Token)
		; { Char = '_' }, lexer__get_variable([Char], Token)
		; { Char = 'a' }, lexer__get_name([Char], Token)
		; { Char = 'b' }, lexer__get_name([Char], Token)
		; { Char = 'c' }, lexer__get_name([Char], Token)
		; { Char = 'd' }, lexer__get_name([Char], Token)
		; { Char = 'e' }, lexer__get_name([Char], Token)
		; { Char = 'f' }, lexer__get_name([Char], Token)
		; { Char = 'g' }, lexer__get_name([Char], Token)
		; { Char = 'h' }, lexer__get_name([Char], Token)
		; { Char = 'i' }, lexer__get_name([Char], Token)
		; { Char = 'j' }, lexer__get_name([Char], Token)
		; { Char = 'k' }, lexer__get_name([Char], Token)
		; { Char = 'l' }, lexer__get_name([Char], Token)
		; { Char = 'm' }, lexer__get_name([Char], Token)
		; { Char = 'n' }, lexer__get_name([Char], Token)
		; { Char = 'o' }, lexer__get_name([Char], Token)
		; { Char = 'p' }, lexer__get_name([Char], Token)
		; { Char = 'q' }, lexer__get_name([Char], Token)
		; { Char = 'r' }, lexer__get_name([Char], Token)
		; { Char = 's' }, lexer__get_name([Char], Token)
		; { Char = 't' }, lexer__get_name([Char], Token)
		; { Char = 'u' }, lexer__get_name([Char], Token)
		; { Char = 'v' }, lexer__get_name([Char], Token)
		; { Char = 'w' }, lexer__get_name([Char], Token)
		; { Char = 'x' }, lexer__get_name([Char], Token)
		; { Char = 'y' }, lexer__get_name([Char], Token)
		; { Char = 'z' }, lexer__get_name([Char], Token)
		; { Char = ('{') }, { Token = open_curly }
		; { Char = ('}') }, { Token = close_curly }
		; { Char = ('|') }, { Token = ht_sep }
		; { Char = ('~') }, lexer__get_graphic([Char], Token)
		; { Char = '\140', Token = junk(Char) } % `
		; { Char = '\177', Token = junk(Char) }
		)
	).

:- pred lexer__get_token_2(token, io__state, io__state).
:- mode lexer__get_token_2(out, di, uo) is det.

	% This is just like get_token_1, except that we have already
	% scanned past some whitespace, so '(' gets scanned as `open'
	% rather than `open_ct'.
	% We use a big switch on Char here for efficiency - obviously not
	% for consiseness.

lexer__get_token_2(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = eof }
	; { Result = ok(Char) },
		( { Char = '\001', Token = junk(Char) }
		; { Char = '\002', Token = junk(Char) }
		; { Char = '\003', Token = junk(Char) }
		; { Char = '\004', Token = junk(Char) }
		; { Char = '\005', Token = junk(Char) }
		; { Char = '\006', Token = junk(Char) }
		; { Char = '\007', Token = junk(Char) }
		; { Char = '\010', Token = junk(Char) }
% \t		; { Char = '\011', Token = junk(Char) }
		; { Char = '\t' }, lexer__get_token_2(Token)
% \n		; { Char = '\012', Token = junk(Char) }
		; { Char = '\n' }, lexer__get_token_2(Token)
		; { Char = '\013', Token = junk(Char) }
% \f		; { Char = '\014', Token = junk(Char) }
		; { Char = '\f' }, lexer__get_token_2(Token)
		; { Char = '\015', Token = junk(Char) }
		; { Char = '\016', Token = junk(Char) }
		; { Char = '\017', Token = junk(Char) }
		; { Char = '\020', Token = junk(Char) }
		; { Char = '\021', Token = junk(Char) }
		; { Char = '\022', Token = junk(Char) }
		; { Char = '\023', Token = junk(Char) }
		; { Char = '\024', Token = junk(Char) }
		; { Char = '\025', Token = junk(Char) }
		; { Char = '\026', Token = junk(Char) }
		; { Char = '\027', Token = junk(Char) }
		; { Char = '\030', Token = junk(Char) }
		; { Char = '\031', Token = junk(Char) }
		; { Char = '\032', Token = junk(Char) }
		; { Char = '\033', Token = junk(Char) }
		; { Char = '\034', Token = junk(Char) }
		; { Char = '\035', Token = junk(Char) }
		; { Char = '\036', Token = junk(Char) }
		; { Char = '\037', Token = junk(Char) }
		; { Char = ' ' }, lexer__get_token_2(Token)
		; { Char = ('!'),  Token = name("!") }
		; { Char = '"' },
			lexer__get_quoted_name(Char, [], Token)
		; { Char = ('#') }, lexer__get_source_line_number([], Token)
		; { Char = ('$') }, lexer__get_graphic([Char], Token)
		; { Char = ('%') }, lexer__skip_to_eol(Token)
		; { Char = ('&') }, lexer__get_graphic([Char], Token)
		; { Char = '''' },
			lexer__get_quoted_name(Char, [], Token)
		; { Char = '(',  Token = open }
		; { Char = ')',  Token = close }
		; { Char = ('*') }, lexer__get_graphic([Char], Token)
		; { Char = ('+') }, lexer__get_graphic([Char], Token)
		; { Char = (','),  Token = comma }
		; { Char = ('-') }, lexer__get_graphic([Char], Token)
		; { Char = ('.') }, lexer__get_dot(Token)
		; { Char = ('/') }, lexer__get_slash(Token)
		; { Char = '0' }, lexer__get_zero(Token)
		; { Char = '1' }, lexer__get_number([Char], Token)
		; { Char = '2' }, lexer__get_number([Char], Token)
		; { Char = '3' }, lexer__get_number([Char], Token)
		; { Char = '4' }, lexer__get_number([Char], Token)
		; { Char = '5' }, lexer__get_number([Char], Token)
		; { Char = '6' }, lexer__get_number([Char], Token)
		; { Char = '7' }, lexer__get_number([Char], Token)
		; { Char = '8' }, lexer__get_number([Char], Token)
		; { Char = '9' }, lexer__get_number([Char], Token)
		; { Char = (':') }, lexer__get_graphic([Char], Token)
		; { Char = (';'),  Token = name(";") }
		; { Char = ('<') }, lexer__get_graphic([Char], Token)
		; { Char = ('=') }, lexer__get_graphic([Char], Token)
		; { Char = ('>') }, lexer__get_graphic([Char], Token)
		; { Char = ('?') }, lexer__get_graphic([Char], Token)
		; { Char = ('@') }, lexer__get_graphic([Char], Token)
		; { Char = 'A' }, lexer__get_variable([Char], Token)
		; { Char = 'B' }, lexer__get_variable([Char], Token)
		; { Char = 'C' }, lexer__get_variable([Char], Token)
		; { Char = 'D' }, lexer__get_variable([Char], Token)
		; { Char = 'E' }, lexer__get_variable([Char], Token)
		; { Char = 'F' }, lexer__get_variable([Char], Token)
		; { Char = 'G' }, lexer__get_variable([Char], Token)
		; { Char = 'H' }, lexer__get_variable([Char], Token)
		; { Char = 'I' }, lexer__get_variable([Char], Token)
		; { Char = 'J' }, lexer__get_variable([Char], Token)
		; { Char = 'K' }, lexer__get_variable([Char], Token)
		; { Char = 'L' }, lexer__get_variable([Char], Token)
		; { Char = 'M' }, lexer__get_variable([Char], Token)
		; { Char = 'N' }, lexer__get_variable([Char], Token)
		; { Char = 'O' }, lexer__get_variable([Char], Token)
		; { Char = 'P' }, lexer__get_variable([Char], Token)
		; { Char = 'Q' }, lexer__get_variable([Char], Token)
		; { Char = 'R' }, lexer__get_variable([Char], Token)
		; { Char = 'S' }, lexer__get_variable([Char], Token)
		; { Char = 'T' }, lexer__get_variable([Char], Token)
		; { Char = 'U' }, lexer__get_variable([Char], Token)
		; { Char = 'V' }, lexer__get_variable([Char], Token)
		; { Char = 'W' }, lexer__get_variable([Char], Token)
		; { Char = 'X' }, lexer__get_variable([Char], Token)
		; { Char = 'Y' }, lexer__get_variable([Char], Token)
		; { Char = 'Z' }, lexer__get_variable([Char], Token)
		; { Char = ('['),  Token = open_list }
		; { Char = ('\\') }, lexer__get_graphic([Char], Token)
		; { Char = (']'),  Token = close_list }
		; { Char = ('^') }, lexer__get_graphic([Char], Token)
		; { Char = '_' }, lexer__get_variable([Char], Token)
		; { Char = '\140', Token = junk(Char) } % `
		; { Char = 'a' }, lexer__get_name([Char], Token)
		; { Char = 'b' }, lexer__get_name([Char], Token)
		; { Char = 'c' }, lexer__get_name([Char], Token)
		; { Char = 'd' }, lexer__get_name([Char], Token)
		; { Char = 'e' }, lexer__get_name([Char], Token)
		; { Char = 'f' }, lexer__get_name([Char], Token)
		; { Char = 'g' }, lexer__get_name([Char], Token)
		; { Char = 'h' }, lexer__get_name([Char], Token)
		; { Char = 'i' }, lexer__get_name([Char], Token)
		; { Char = 'j' }, lexer__get_name([Char], Token)
		; { Char = 'k' }, lexer__get_name([Char], Token)
		; { Char = 'l' }, lexer__get_name([Char], Token)
		; { Char = 'm' }, lexer__get_name([Char], Token)
		; { Char = 'n' }, lexer__get_name([Char], Token)
		; { Char = 'o' }, lexer__get_name([Char], Token)
		; { Char = 'p' }, lexer__get_name([Char], Token)
		; { Char = 'q' }, lexer__get_name([Char], Token)
		; { Char = 'r' }, lexer__get_name([Char], Token)
		; { Char = 's' }, lexer__get_name([Char], Token)
		; { Char = 't' }, lexer__get_name([Char], Token)
		; { Char = 'u' }, lexer__get_name([Char], Token)
		; { Char = 'v' }, lexer__get_name([Char], Token)
		; { Char = 'w' }, lexer__get_name([Char], Token)
		; { Char = 'x' }, lexer__get_name([Char], Token)
		; { Char = 'y' }, lexer__get_name([Char], Token)
		; { Char = 'z' }, lexer__get_name([Char], Token)
		; { Char = ('{'),  Token = open_curly }
		; { Char = ('|'),  Token = ht_sep }
		; { Char = ('}'),  Token = close_curly }
		; { Char = ('~') }, lexer__get_graphic([Char], Token)
		; { Char = '\177', Token = junk(Char) }
		)
	).

%-----------------------------------------------------------------------------%

:- pred lexer__special_token(character, token).
:- mode lexer__special_token(in, out) is semidet.

lexer__special_token('(', open).	% May get converted to open_ct
lexer__special_token(')', close).
lexer__special_token('[', open_list).
lexer__special_token(']', close_list).
lexer__special_token('{', open_curly).
lexer__special_token('}', close_curly).
lexer__special_token('|', ht_sep).
lexer__special_token(',', comma).
lexer__special_token(';', name(";")).
lexer__special_token('!', name("!")).

:- pred lexer__graphic_token_char(character).
:- mode lexer__graphic_token_char(in) is semidet.

lexer__graphic_token_char('#').
lexer__graphic_token_char('$').
lexer__graphic_token_char('&').
lexer__graphic_token_char('*').
lexer__graphic_token_char('+').
lexer__graphic_token_char('-').
lexer__graphic_token_char('.').
lexer__graphic_token_char('/').
lexer__graphic_token_char(':').
lexer__graphic_token_char('<').
lexer__graphic_token_char('=').
lexer__graphic_token_char('>').
lexer__graphic_token_char('?').
lexer__graphic_token_char('@').
lexer__graphic_token_char('^').
lexer__graphic_token_char('~').
lexer__graphic_token_char('\\').

%-----------------------------------------------------------------------------%

:- pred lexer__get_dot(token, io__state, io__state).
:- mode lexer__get_dot(out, di, uo) is det.

lexer__get_dot(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = end }
	; { Result = ok(Char) },
		( { lexer__whitespace_after_dot(Char) } ->
			io__putback_char(Char),
			{ Token = end }
		; { lexer__graphic_token_char(Char) } ->
			lexer__get_graphic([Char, '.'], Token)
		;
			io__putback_char(Char),
			{ Token = name(".") }
		)
	).

:- pred lexer__whitespace_after_dot(character).
:- mode lexer__whitespace_after_dot(in) is semidet.

lexer__whitespace_after_dot(' ').
lexer__whitespace_after_dot('\t').
lexer__whitespace_after_dot('\n').
lexer__whitespace_after_dot('%').

%-----------------------------------------------------------------------------%

	% comments

:- pred lexer__skip_to_eol(token, io__state, io__state).
:- mode lexer__skip_to_eol(out, di, uo) is det.

lexer__skip_to_eol(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = eof }
	; { Result = ok(Char) },
		( { Char = '\n' } ->
			lexer__get_token_2(Token)
		;
			lexer__skip_to_eol(Token)
		)
	).

:- pred lexer__get_slash(token, io__state, io__state).
:- mode lexer__get_slash(out, di, uo) is det.

lexer__get_slash(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = name("/") }
	; { Result = ok(Char) },
		( { Char = ('*') } ->
			lexer__get_comment(Token)
		; { lexer__graphic_token_char(Char) } ->
			lexer__get_graphic([Char, '/'], Token)
		;
			io__putback_char(Char),
			{ Token = name("/") }
		)
	).

:- pred lexer__get_comment(token, io__state, io__state).
:- mode lexer__get_comment(out, di, uo) is det.

lexer__get_comment(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error("unterminated '/*' comment") }
	; { Result = ok(Char) },
		( { Char = ('*') } ->
			lexer__get_comment_2(Token)
		;
			lexer__get_comment(Token)
		)
	).

:- pred lexer__get_comment_2(token, io__state, io__state).
:- mode lexer__get_comment_2(out, di, uo) is det.

lexer__get_comment_2(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error("unterminated '/*' comment") }
	; { Result = ok(Char) },
		( { Char = ('/') } ->
			lexer__get_token_2(Token)
		; { Char = ('*') } ->
			lexer__get_comment_2(Token)
		;
			lexer__get_comment(Token)
		)
	).

%-----------------------------------------------------------------------------%

	% quoted names and quoted strings

:- pred lexer__get_quoted_name(character, list(character), token,
				io__state, io__state).
:- mode lexer__get_quoted_name(in, in, out, di, uo) is det.

lexer__get_quoted_name(QuoteChar, Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error("unterminated quote") }
	; { Result = ok(Char) },
		( { Char = QuoteChar } ->
			lexer__get_quoted_name_quote(QuoteChar, Chars, Token)
		; { Char = ('\\') } ->
			lexer__get_quoted_name_escape(QuoteChar, Chars, Token)
		;
			lexer__get_quoted_name(QuoteChar, [Char | Chars], Token)
		)
	).

:- pred lexer__get_quoted_name_quote(character, list(character), token,
				io__state, io__state).
:- mode lexer__get_quoted_name_quote(in, in, out, di, uo) is det.

lexer__get_quoted_name_quote(QuoteChar, Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ lexer__finish_quoted_name(QuoteChar, Chars, Token) }
	; { Result = ok(Char) },
		( { Char = QuoteChar } ->
			lexer__get_quoted_name(QuoteChar, [Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__finish_quoted_name(QuoteChar, Chars, Token) }
		)
	).

:- pred lexer__finish_quoted_name(character, list(character), token).
:- mode lexer__finish_quoted_name(in, in, out) is det.

lexer__finish_quoted_name(QuoteChar, Chars, Token) :-
	lexer__rev_char_list_to_string(Chars, String),
	( QuoteChar = '''' ->
		Token = name(String)
	; QuoteChar = '"' ->
		Token = string(String)
	;
		error("lexer.m: unknown quote character")
	).

:- pred lexer__get_quoted_name_escape(character, list(character), token,
					io__state, io__state).
:- mode lexer__get_quoted_name_escape(in, in, out, di, uo) is det.

lexer__get_quoted_name_escape(QuoteChar, Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error("unterminated quoted name") }
	; { Result = ok(Char) }, !,
		( { Char = '\n' } ->
			lexer__get_quoted_name(QuoteChar, Chars, Token)
		; { lexer__escape_char(Char, EscapedChar) } ->
			{ Chars1 = [EscapedChar | Chars] },
			lexer__get_quoted_name(QuoteChar, Chars1, Token)
		; { Char = 'x' } ->
			lexer__get_hex_escape(QuoteChar, Chars, [], Token)
		; { char__is_octal_digit(Char) } ->
			lexer__get_octal_escape(QuoteChar, Chars, [Char],
				Token)
		;
			{ Token = error("invalid escape character") }
		)
	).

:- pred lexer__escape_char(character, character).
:- mode lexer__escape_char(in, out) is semidet.

lexer__escape_char('a', '\a').
lexer__escape_char('b', '\b').
lexer__escape_char('r', '\r').
lexer__escape_char('f', '\f').
lexer__escape_char('t', '\t').
lexer__escape_char('n', '\n').
lexer__escape_char('v', '\v').
lexer__escape_char('\\', '\\').
lexer__escape_char('''', '''').
lexer__escape_char('"', '"').
lexer__escape_char('`', '`').

:- pred lexer__get_hex_escape(character, list(character), list(character),
				token, io__state, io__state).
:- mode lexer__get_hex_escape(in, in, in, out, di, uo) is det.

lexer__get_hex_escape(QuoteChar, Chars, HexChars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error("unterminated quote") }
	; { Result = ok(Char) }, !,
		( { char__is_hex_digit(Char) } ->
			lexer__get_hex_escape(QuoteChar, Chars,
						[Char | HexChars], Token)
		; { Char = ('\\') } ->
			lexer__finish_hex_escape(QuoteChar, Chars, HexChars,
				Token)
		;
			{ Token = error("unterminated hex escape") }
		)
	).

:- pred lexer__finish_hex_escape(character, list(character), list(character),
				token, io__state, io__state).
:- mode lexer__finish_hex_escape(in, in, in, out, di, uo) is det.

lexer__finish_hex_escape(QuoteChar, Chars, HexChars, Token) -->
	( { HexChars = [] } ->
		{ Token = error("empty hex escape") }
	;
		{ lexer__rev_char_list_to_string(HexChars, HexString) },
		(
			{ string__base_string_to_int(16, HexString, Int) },
			{ char__to_int(Char, Int) }
		->
			lexer__get_quoted_name(QuoteChar, [Char|Chars], Token) 
		;
			{ Token = error("invalid hex escape") }
		)
	).

:- pred lexer__get_octal_escape(character, list(character), list(character),
				token, io__state, io__state).
:- mode lexer__get_octal_escape(in, in, in, out, di, uo) is det.

lexer__get_octal_escape(QuoteChar, Chars, OctalChars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error("unterminated quote") }
	; { Result = ok(Char) }, !,
		( { char__is_octal_digit(Char) } ->
			lexer__get_octal_escape(QuoteChar, Chars,
						[Char | OctalChars], Token)
		; { Char = ('\\') } ->
			lexer__finish_octal_escape(QuoteChar, Chars, OctalChars,
				Token)
		;
			/****** 
				% We don't report this as an error since
				% we need bug-for-bug compatibility with
				% NU-Prolog
			{ Token = error("unterminated octal escape") }
			******/
			io__putback_char(Char),
			lexer__finish_octal_escape(QuoteChar, Chars, OctalChars,
				Token)
		)
	).

:- pred lexer__finish_octal_escape(character, list(character), list(character),
				token, io__state, io__state).
:- mode lexer__finish_octal_escape(in, in, in, out, di, uo) is det.

lexer__finish_octal_escape(QuoteChar, Chars, OctalChars, Token) -->
	( { OctalChars = [] } ->
		{ Token = error("empty octal escape") }
	;
		{ lexer__rev_char_list_to_string(OctalChars, OctalString) },
		(
			{ string__base_string_to_int(8, OctalString, Int) },
			{ char__to_int(Char, Int) }
		->
			lexer__get_quoted_name(QuoteChar, [Char|Chars], Token) 
		;
			{ Token = error("invalid octal escape") }
		)
	).

%-----------------------------------------------------------------------------%

	% names and variables

:- pred lexer__get_name(list(character), token, io__state, io__state).
:- mode lexer__get_name(in, out, di, uo) is det.

lexer__get_name(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ lexer__rev_char_list_to_string(Chars, Name) },
		{ Token = name(Name) }
	; { Result = ok(Char) },
		( { char__is_alnum_or_underscore(Char) } ->
			lexer__get_name([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_string(Chars, Name) },
			{ Token = name(Name) }
		)
	).

	%
	% A line number directive token is `#' followed by an integer
	% (specifying the line number) followed by a newline.
	% Such a token sets the source line number for the next
	% line, but it is otherwise ignored.  This means that line number
	% directives may appear anywhere that a token may appear, including
	% in the middle of terms.
	% (The source file name can be set with a `:- pragma source_file' 
	% declaration.)
	%

:- pred lexer__get_source_line_number(list(char), token, io__state, io__state).
:- mode lexer__get_source_line_number(in, out, di, uo) is det.

lexer__get_source_line_number(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error(
			"unexpected end-of-file in `#' line number directive") }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_source_line_number([Char | Chars], Token)
		; { Char = '\n' } ->
			{ lexer__rev_char_list_to_string(Chars, String) },
			(
				{ string__base_string_to_int(10, String, Int) },
				{ Int > 0 }
			->
				io__set_line_number(Int),
				lexer__get_token_1(Token)
			;
				{ string__append_list([
					"invalid line number `", String,
					"' in `#' line number directive"],
					Message) },
				{ Token = error(Message) }
			)
		;
			{ string__from_char_list([Char], String) },
			{ string__append_list([
				"invalid character `", String,
				"' in `#' line number directive"],
				Message),
			Token = error(Message) }
		)
	).

:- pred lexer__get_graphic(list(character), token, io__state, io__state).
:- mode lexer__get_graphic(in, out, di, uo) is det.

lexer__get_graphic(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ lexer__rev_char_list_to_string(Chars, Name) },
		{ Token = name(Name) }
	; { Result = ok(Char) },
		( { lexer__graphic_token_char(Char) } ->
			lexer__get_graphic([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_string(Chars, Name) },
			{ Token = name(Name) }
		)
	).

:- pred lexer__get_variable(list(character), token, io__state, io__state).
:- mode lexer__get_variable(in, out, di, uo) is det.

lexer__get_variable(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ lexer__rev_char_list_to_string(Chars, VariableName) },
		{ Token = variable(VariableName) }
	; { Result = ok(Char) },
		( { char__is_alnum_or_underscore(Char) } ->
			lexer__get_variable([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_string(Chars, VariableName) },
			{ Token = variable(VariableName) }
		)
	).

%-----------------------------------------------------------------------------%

	% integer and float literals

:- pred lexer__get_zero(token, io__state, io__state).
:- mode lexer__get_zero(out, di, uo) is det.

lexer__get_zero(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = integer(0) }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_number([Char], Token)
		; { Char = '''' } ->
			lexer__get_char_code(Token)
		; { Char = 'b' } ->
			lexer__get_binary(Token)
		; { Char = 'o' } ->
			lexer__get_octal(Token)
		; { Char = 'x' } ->
			lexer__get_hex(Token)
		; { Char = ('.') } ->
			lexer__get_int_dot(['0'], Token)
		; { Char = 'e' ; Char = 'E' } ->
			lexer__get_float_exponent([Char, '0'], Token)
		;
			io__putback_char(Char),
			{ Token = integer(0) }
		)
	).

:- pred lexer__get_char_code(token, io__state, io__state).
:- mode lexer__get_char_code(out, di, uo) is det.

lexer__get_char_code(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error("unterminated char code constant") }
	; { Result = ok(Char) },
		{ char__to_int(Char, CharCode) },
		{ Token = integer(CharCode) }
	).

:- pred lexer__get_binary(token, io__state, io__state).
:- mode lexer__get_binary(out, di, uo) is det.

lexer__get_binary(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error("unterminated binary constant") }
	; { Result = ok(Char) },
		( { char__is_binary_digit(Char) } ->
			lexer__get_binary_2([Char], Token)
		;
			io__putback_char(Char),
			{ Token = error("unterminated binary constant") }
		)
	).

:- pred lexer__get_binary_2(list(character), token, io__state, io__state).
:- mode lexer__get_binary_2(in, out, di, uo) is det.

lexer__get_binary_2(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ lexer__rev_char_list_to_int(Chars, 2, Token) }
	; { Result = ok(Char) },
		( { char__is_binary_digit(Char) } ->
			lexer__get_binary_2([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_int(Chars, 2, Token) }
		)
	).

:- pred lexer__get_octal(token, io__state, io__state).
:- mode lexer__get_octal(out, di, uo) is det.

lexer__get_octal(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error("unterminated octal constant") }
	; { Result = ok(Char) },
		( { char__is_octal_digit(Char) } ->
			lexer__get_octal_2([Char], Token)
		;
			io__putback_char(Char),
			{ Token = error("unterminated octal constant") }
		)
	).

:- pred lexer__get_octal_2(list(character), token, io__state, io__state).
:- mode lexer__get_octal_2(in, out, di, uo) is det.

lexer__get_octal_2(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ lexer__rev_char_list_to_int(Chars, 8, Token) }
	; { Result = ok(Char) },
		( { char__is_octal_digit(Char) } ->
			lexer__get_octal_2([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_int(Chars, 8, Token) }
		)
	).

:- pred lexer__get_hex(token, io__state, io__state).
:- mode lexer__get_hex(out, di, uo) is det.

lexer__get_hex(Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error("unterminated hex constant") }
	; { Result = ok(Char) },
		( { char__is_hex_digit(Char) } ->
			lexer__get_hex_2([Char], Token)
		;
			io__putback_char(Char),
			{ Token = error("unterminated hex constant") }
		)
	).

:- pred lexer__get_hex_2(list(character), token, io__state, io__state).
:- mode lexer__get_hex_2(in, out, di, uo) is det.

lexer__get_hex_2(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ lexer__rev_char_list_to_int(Chars, 16, Token) }
	; { Result = ok(Char) },
		( { char__is_hex_digit(Char) } ->
			lexer__get_hex_2([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_int(Chars, 16, Token) }
		)
	).

:- pred lexer__get_number(list(character), token, io__state, io__state).
:- mode lexer__get_number(in, out, di, uo) is det.

lexer__get_number(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ lexer__rev_char_list_to_int(Chars, 10, Token) }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_number([Char | Chars], Token)
		; { Char = ('.') } ->
			lexer__get_int_dot(Chars, Token)
		; { Char = 'e' ; Char = 'E' } ->
			lexer__get_float_exponent([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_int(Chars, 10, Token) }
		)
	).

	% XXX the float literal syntax doesn't match ISO Prolog

:- pred lexer__get_int_dot(list(character), token, io__state, io__state).
:- mode lexer__get_int_dot(in, out, di, uo) is det.

lexer__get_int_dot(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		io__putback_char('.'),
		{ lexer__rev_char_list_to_int(Chars, 10, Token) }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_float_decimals([Char, '.' | Chars], Token)
		;
			io__putback_char(Char),
			io__putback_char('.'),
			{ lexer__rev_char_list_to_int(Chars, 10, Token) }
		)
	).

:- pred lexer__get_float_decimals(list(character), token, io__state, io__state).
:- mode lexer__get_float_decimals(in, out, di, uo) is det.

	% we've read past the decimal point, so now get the decimals

lexer__get_float_decimals(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ lexer__rev_char_list_to_float(Chars, Token) }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_float_decimals([Char | Chars], Token)
		; { Char = 'e' ; Char = 'E' } ->
			lexer__get_float_exponent([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_float(Chars, Token) }
		)
	).

:- pred lexer__get_float_exponent(list(character), token, io__state, io__state).
:- mode lexer__get_float_exponent(in, out, di, uo) is det.

lexer__get_float_exponent(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ lexer__rev_char_list_to_float(Chars, Token) }
	; { Result = ok(Char) },
		( { Char = ('+') ; Char = ('-') } ->
			lexer__get_float_exponent_2([Char | Chars], Token)
		; { char__is_digit(Char) } ->
			lexer__get_float_exponent_3([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ Token =
			  error("unterminated exponent in float token") }
		)
	).

:- pred lexer__get_float_exponent_2(list(character), token,
				io__state, io__state).
:- mode lexer__get_float_exponent_2(in, out, di, uo) is det.

	% we've read past the E signalling the start of the exponent -
	% make sure that there's at least one digit following,
	% and then get the remaining digits

lexer__get_float_exponent_2(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ Token = error("unterminated exponent in float token") }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_float_exponent_3([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ Token =
			  error("unterminated exponent in float token") }
		)
	).

:- pred lexer__get_float_exponent_3(list(character), token,
					io__state, io__state).
:- mode lexer__get_float_exponent_3(in, out, di, uo) is det.

	% we've read past the first digit of the exponent -
	% now get the remaining digits

lexer__get_float_exponent_3(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) }, !,
		{ Token = io_error(Error) }
	; { Result = eof }, !,
		{ lexer__rev_char_list_to_float(Chars, Token) }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_float_exponent_3([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_float(Chars, Token) }
		)
	).

%-----------------------------------------------------------------------------%

	% Utility routines

:- pred lexer__rev_char_list_to_int(list(character), int, token).
:- mode lexer__rev_char_list_to_int(in, in, out) is det.

lexer__rev_char_list_to_int(RevChars, Base, Token) :-
	lexer__rev_char_list_to_string(RevChars, String),
	( string__base_string_to_int(Base, String, Int) ->
		Token = integer(Int)
	;
		Token = error("invalid integer token")
	).

:- pred lexer__rev_char_list_to_float(list(character), token).
:- mode lexer__rev_char_list_to_float(in, out) is det.

lexer__rev_char_list_to_float(RevChars, Token) :-
	lexer__rev_char_list_to_string(RevChars, String),
	( string__to_float(String, Float) ->
		Token = float(Float)
	;
		Token = error("invalid float token")
	).

:- pred lexer__rev_char_list_to_string(list(character), string).
:- mode lexer__rev_char_list_to_string(in, out) is det.

% lexer__rev_char_list_to_string was originally implemented like this:
%
%lexer__rev_char_list_to_string(RevChars, String) :-
%       list__reverse(RevChars, Chars),
%       string__from_char_list(Chars, String).
%
% The optimized implementation in C below is there for efficiency since
% it improves the overall speed of parsing by about 7%.

:- pragma(c_code, lexer__rev_char_list_to_string(Chars::in, Str::out), "
{
	Word list_ptr;
	Word size, len;
	Word str_ptr;
/*
** loop to calculate list length + sizeof(Word) in `size' using list in
** `list_ptr' and separately count the length of the string
*/
	size = sizeof(Word);
	len = 1;
	list_ptr = Chars;
	while (!list_is_empty(list_ptr)) {
		size++;
		len++;
		list_ptr = list_tail(list_ptr);
	}
/*
** allocate (length + 1) bytes of heap space for string
** i.e. (length + 1 + sizeof(Word) - 1) / sizeof(Word) words
*/
	incr_hp_atomic(str_ptr, size / sizeof(Word));
	Str = (char *) str_ptr;
/*
** set size to be the offset of the end of the string
** (ie the \\0) and null terminate the string.
*/
	Str[--len] = '\\0';
/*
** loop to copy the characters from the list_ptr to the string
** in reverse order.
*/
	list_ptr = Chars;
	while (!list_is_empty(list_ptr)) {
		Str[--len] = (char) list_head(list_ptr);
		list_ptr = list_tail(list_ptr);
	}
}").

%-----------------------------------------------------------------------------%

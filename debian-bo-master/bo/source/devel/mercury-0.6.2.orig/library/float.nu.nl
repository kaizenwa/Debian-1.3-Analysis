%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: float.nu.nl.
% Main author: fjh.
%
% This file provides a Prolog implementation of `float__min', `float__max',
% and `float__epsilon'.
%
%-----------------------------------------------------------------------------%

% XXX These definitions are highly platform-dependent.
%     Currently we just hard-wire the definitions for IEEE double precision.

% This code is written very carefully to avoid bugs and incompatibilities
% in NU-Prolog and SICStus Prolog:
%
% 	We mustn't write `X = 1.7976931348623157E308', because NU-Prolog
% 	has a bug which means that it fails to compile that.
%
% 	We mustn't write `X is 1.7976931348623157 * 10 ** 308', as NU-Prolog
%	wants, because SICStus reports a syntax error.
%
% 	We mustn't write `X is 1.7976931348623157 * exp(10, 308)', as SICStus
%	Prolog wants, because NU-Prolog has a different bug which means that
%	it fails to compile that too.
% 
% Writing this code was like walking a mine-field... if you change it,
% be sure to test it on both NU-Prolog and SICStus.

float__max(X) :-
	% 1.7976931348623157E+308
	( nuprolog ->
		X is 1.79769313486231 * '**'(10, 308)
	;
		Exp = exp(10, 308),
		X is 1.79769313486231 * Exp
	).

	% really float__min(2.2250738585072014e-308).
	% but if we put in the last two digits, then
	% NU-Prolog (1.6.12) will underflow to zero.
	% (This happens with compiled code (nc), but not
	% with consulted code (np).)
float__min(2.22507385850721e-308).

float__epsilon(2.2204460492503131e-16).

%-----------------------------------------------------------------------------%

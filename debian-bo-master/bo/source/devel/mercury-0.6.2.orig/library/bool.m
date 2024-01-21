%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: bool.m.
% Main authors: fjh, zs.
% Stability: medium to high.

% This module exports the boolean type `bool' and some operations on bools.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module bool.

:- interface.

:- import_module list.

%-----------------------------------------------------------------------------%

% The boolean type.
% Unlike most languages, we use `yes' and `no' as boolean constants
% rather than `true' and `false'.  This is to avoid confusion
% with the predicates `true' and `fail'.

:- type bool ---> yes ; no.

:- pred bool__or(bool, bool, bool).
:- mode bool__or(in, in, out) is det.

:- pred bool__or_list(list(bool), bool).
:- mode bool__or_list(in, out) is det.

:- pred bool__and(bool, bool, bool).
:- mode bool__and(in, in, out) is det.

:- pred bool__and_list(list(bool), bool).
:- mode bool__and_list(in, out) is det.

:- pred bool__not(bool, bool).
:- mode bool__not(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

bool__or(yes, _, yes).
bool__or(no, Bool, Bool).

bool__or_list([], no).
bool__or_list([Bool | Bools], Result) :-
	( Bool = yes ->
		Result = yes
	;
		bool__or_list(Bools, Result)
	).

bool__and(no, _, no).
bool__and(yes, Bool, Bool).

bool__and_list([], yes).
bool__and_list([Bool | Bools], Result) :-
	( Bool = no ->
		Result = no
	;
		bool__and_list(Bools, Result)
	).

bool__not(no, yes).
bool__not(yes, no).

%-----------------------------------------------------------------------------%

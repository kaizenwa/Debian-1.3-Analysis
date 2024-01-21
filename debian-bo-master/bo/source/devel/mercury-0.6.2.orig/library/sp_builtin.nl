%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: sp_builtin.nl:
% Main author: fjh.
%
% This file is for Sicstus Prolog compatibility.
%
%-----------------------------------------------------------------------------%

:- nofileerrors.
:- prolog_flag(redefine_warnings, _, off).

% Declare the appropriate operators.

:- op(1199, fx, (module)).
:- op(1199, fx, (end_module)).

:- op(1199, fx, (export_module)).
:- op(1199, fx, (export_sym)).
:- op(1199, fx, (export_pred)).
:- op(1199, fx, (export_cons)).
:- op(1199, fx, (export_type)).
:- op(1199, fx, (export_adt)).
:- op(1199, fx, (export_op)).

:- op(1199, fx, (import_module)).
:- op(1199, fx, (import_sym)).
:- op(1199, fx, (import_pred)).
:- op(1199, fx, (import_cons)).
:- op(1199, fx, (import_type)).
:- op(1199, fx, (import_adt)).
:- op(1199, fx, (import_op)).

:- op(1199, fx, (use_module)).
:- op(1199, fx, (use_sym)).
:- op(1199, fx, (use_pred)).
:- op(1199, fx, (use_cons)).
:- op(1199, fx, (use_type)).
:- op(1199, fx, (use_adt)).
:- op(1199, fx, (use_op)).

:- op(1199, fx, (rule)).

:- op(1199, fx, (pragma)).

:- op(1199, fx, (type)).
:- op(1199, fx, (pred)).
:- op(1199, fx, (func)).
:- op(1199, fx, (mode)).
:- op(1199, fx, (inst)).
:- op(1179, xfy, (--->)).
:- op(975, xfx, ('::')).
:- op(700, xfx, ( \= ) ).
:- op(500, fx, ( \ ) ).

:- op(920, xfy, (=>)).
:- op(920, xfy, (<=)).
:- op(920, xfy, (<=>)).
:- op(900, fy, (not)).

:- op(900, xfx, (when)).
:- op(740, xfy, (or)).
:- op(720, xfy, (and)).

:- op(701, xfx, (is)).

% Use term_expansion/2 to prevent warnings about undefined predicates
% when the interpreter tries to execute the new declarations.

mercury_declaration(rule(_)).

mercury_declaration(type(_)).
mercury_declaration(pred(_)).
mercury_declaration(func(_)).
mercury_declaration(mode(_)).
mercury_declaration(inst(_)).

mercury_declaration(module(_)).
mercury_declaration(end_module(_)).
mercury_declaration(interface).
mercury_declaration(implementation).

mercury_declaration(import_module(_)).
mercury_declaration(import_sym(_)).
mercury_declaration(import_pred(_)).
mercury_declaration(import_cons(_)).
mercury_declaration(import_type(_)).
mercury_declaration(import_adt(_)).
mercury_declaration(import_op(_)).

mercury_declaration(export_module(_)).
mercury_declaration(export_sym(_)).
mercury_declaration(export_pred(_)).
mercury_declaration(export_cons(_)).
mercury_declaration(export_type(_)).
mercury_declaration(export_adt(_)).
mercury_declaration(export_op(_)).

mercury_declaration(use_module(_)).
mercury_declaration(use_sym(_)).
mercury_declaration(use_pred(_)).
mercury_declaration(use_cons(_)).
mercury_declaration(use_type(_)).
mercury_declaration(use_adt(_)).
mercury_declaration(use_op(_)).

mercury_declaration(external(_)).

mercury_declaration(when(_,_)).

mercury_declaration(pragma(_,_)).
mercury_declaration(pragma(_,_,_)).

term_expansion((:- Term), Clauses) :-
	mercury_declaration(Term),
	Clauses = [].

%-----------------------------------------------------------------------------%

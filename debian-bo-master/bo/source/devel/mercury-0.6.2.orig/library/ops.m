%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: ops.m.
% main author: fjh.
% stability: low.
%
% Here's where we maintain the table of current operators.
%
% XXX In the current implementation the table is fixed and cannot be
% modified at run-time.
%
%-----------------------------------------------------------------------------%

:- module ops.
:- interface.

:- type ops__specifier
	--->	fx ; fy ; xf ; yf ; xfx ; yfx ; xfy ; fxx ; fxy ; fyx.

:- type ops__assoc
	--->	x ; y.

:- type ops__class
	--->	infix(ops__assoc, ops__assoc)
	;	prefix(ops__assoc)
	;	binary_prefix(ops__assoc, ops__assoc)
	;	postfix(ops__assoc).

:- type ops__table.

:- type ops__priority == int.

	% create an ops_table with the standard Mercury operators.
:- pred ops__init_op_table(ops__table).
:- mode ops__init_op_table(uo) is det.

	% check whether a string is the name of an infix operator,
	% and if it is, return its precedence and associativity.
:- pred ops__lookup_infix_op(ops__table, string, int, ops__assoc, ops__assoc).
:- mode ops__lookup_infix_op(in, in, out, out, out) is semidet.

	% check whether a string is the name of a prefix operator,
	% and if it is, return its precedence and associativity.
:- pred ops__lookup_prefix_op(ops__table, string, int, ops__assoc).
:- mode ops__lookup_prefix_op(in, in, out, out) is semidet.

	% check whether a string is the name of a binary prefix operator,
	% and if it is, return its precedence and associativity.
:- pred ops__lookup_binary_prefix_op(ops__table, string,
					int, ops__assoc, ops__assoc).
:- mode ops__lookup_binary_prefix_op(in, in, out, out, out) is semidet.
		
	% check whether a string is the name of a postfix operator,
	% and if it is, return its precedence and associativity.
:- pred ops__lookup_postfix_op(ops__table, string, int, ops__assoc).
:- mode ops__lookup_postfix_op(in, in, out, out) is semidet.

	% check whether a string is the name of an operator
:- pred ops__lookup_op(ops__table, string).
:- mode ops__lookup_op(in, in) is semidet.

	% convert an ops__specifer (e.g. `xfy') to an ops__class
	% (e.g. `infix(x, y)').
:- pred ops__op_specifier_to_class(ops__specifier, ops__class).
:- mode ops__op_specifier_to_class(in, out) is det.
% :- mode ops__op_specifier_to_class(out, in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- type ops__table ---> ops__table.	% XXX

:- type ops__category ---> before ; after.

ops__op_specifier_to_class(fx, prefix(x)).
ops__op_specifier_to_class(fy, prefix(y)).
ops__op_specifier_to_class(xf, postfix(x)).
ops__op_specifier_to_class(yf, postfix(y)).
ops__op_specifier_to_class(xfx, infix(x,x)).
ops__op_specifier_to_class(yfx, infix(y,x)).
ops__op_specifier_to_class(xfy, infix(x,y)).
ops__op_specifier_to_class(fxx, binary_prefix(x,x)).
ops__op_specifier_to_class(fyx, binary_prefix(y,x)).
ops__op_specifier_to_class(fxy, binary_prefix(x,y)).

ops__lookup_infix_op(_OpTable, Name, Priority, LeftAssoc, RightAssoc) :-
	ops__op_table(Name, after, Specifier, Priority), !,
	ops__op_specifier_to_class(Specifier,
		infix(LeftAssoc, RightAssoc)).

ops__lookup_prefix_op(_OpTable, Name, Priority, LeftAssoc) :-
	ops__op_table(Name, before, Specifier, Priority), !,
	ops__op_specifier_to_class(Specifier, prefix(LeftAssoc)).

ops__lookup_binary_prefix_op(_OpTable, Name, Priority, LeftAssoc, RightAssoc) :-
	ops__op_table(Name, before, Specifier, Priority), !,
	ops__op_specifier_to_class(Specifier,
		binary_prefix(LeftAssoc, RightAssoc)).

ops__lookup_postfix_op(_OpTable, Name, Priority, LeftAssoc) :-
	ops__op_table(Name, after, Specifier, Priority), !,
	ops__op_specifier_to_class(Specifier, postfix(LeftAssoc)).

ops__lookup_op(_OpTable, Name) :-
	ops__op_table(Name, _, _, _).

:- pred ops__op_table(string, ops__category, ops__specifier, ops__priority).
:- mode ops__op_table(in, in, out, out) is semidet.
:- mode ops__op_table(in, out, out, out) is nondet.

ops__op_table("*", after, yfx, 400).
ops__op_table("**", after, xfy, 300).
ops__op_table("+", after, yfx, 500).
ops__op_table("+", before, fx, 500).
ops__op_table(",", after, xfy, 1000).
ops__op_table("-", after, yfx, 500).
ops__op_table("-", before, fx, 500).
ops__op_table("--->", after, xfy, 1179).
ops__op_table("-->", after, xfx, 1200).
ops__op_table("->", after, xfy, 1050).
ops__op_table(".", after, xfy, 600).
ops__op_table("/", after, yfx, 400).
ops__op_table("//", after, yfx, 400).
ops__op_table("/\\", after, yfx, 500).
ops__op_table(":", after, xfy, 600).
% ops__op_table(":", before, fx, 1175).
ops__op_table(":-", after, xfx, 1200).
ops__op_table(":-", before, fx, 1200).
ops__op_table("::", after, xfx, 1175).
ops__op_table(";", after, xfy, 1100).
ops__op_table("<", after, xfx, 700).
ops__op_table("<<", after, yfx, 400).
ops__op_table("<=", after, xfy, 920).
ops__op_table("<=>", after, xfy, 920).
ops__op_table("=", after, xfx, 700).
% ops__op_table("=..", after, xfx, 700).
% ops__op_table("=:=", after, xfx, 700).
ops__op_table("=<", after, xfx, 700).
ops__op_table("==", after, xfx, 700).
ops__op_table("=>", after, xfy, 920).
% ops__op_table("=\\=", after, xfx, 700).
ops__op_table(">", after, xfx, 700).
ops__op_table(">=", after, xfx, 700).
ops__op_table(">>", after, yfx, 400).
% ops__op_table("?-", before, fx, 1200).
% ops__op_table("@<", after, xfx, 700).
% ops__op_table("@=<", after, xfx, 700).
% ops__op_table("@>", after, xfx, 700).
% ops__op_table("@>=", after, xfx, 700).
ops__op_table("\\", before, fx, 500).
ops__op_table("\\+", before, fy, 900).
ops__op_table("\\/", after, yfx, 500).
ops__op_table("\\=", after, xfx, 700).
% ops__op_table("\\==", after, xfx, 700).
ops__op_table("^", after, xfy, 200).
ops__op_table("all", before, fxy, 950).
ops__op_table("and", after, xfy, 720).
% ops__op_table("delete", before, fy, 1175).
% ops__op_table("dynamic", before, fy, 1150).
ops__op_table("else", after, xfy, 1170).
ops__op_table("end_module", before, fx, 1199).
ops__op_table("export_adt", before, fx, 1199).
ops__op_table("export_cons", before, fx, 1199).
ops__op_table("export_module", before, fx, 1199).
ops__op_table("export_op", before, fx, 1199).
ops__op_table("export_pred", before, fx, 1199).
ops__op_table("export_sym", before, fx, 1199).
ops__op_table("export_type", before, fx, 1199).
ops__op_table("func", before, fx, 1180).
% ops__op_table("gAll", before, fxy, 950).
% ops__op_table("gSome", before, fxy, 950).
ops__op_table("if", before, fx, 1160).
ops__op_table("import_adt", before, fx, 1199).
ops__op_table("import_cons", before, fx, 1199).
ops__op_table("import_module", before, fx, 1199).
ops__op_table("import_op", before, fx, 1199).
ops__op_table("import_pred", before, fx, 1199).
ops__op_table("import_sym", before, fx, 1199).
ops__op_table("import_type", before, fx, 1199).
% ops__op_table("in", after, xfx, 1172).
% ops__op_table("insert", before, fy, 1175).
ops__op_table("inst", before, fx, 1199).
ops__op_table("is", after, xfx, 701).
ops__op_table("lambda", before, fxy, 950).
% ops__op_table("lib", before, fy, 900).
% ops__op_table("listing", before, fy, 900).
% ops__op_table("man", before, fy, 900).
ops__op_table("mod", after, xfx, 300).
ops__op_table("mode", before, fx, 1199).
ops__op_table("module", before, fx, 1199).
% ops__op_table("nospy", before, fy, 900).
ops__op_table("not", before, fy, 900).
% ops__op_table("once", before, fy, 900).
ops__op_table("or", after, xfy, 740).
ops__op_table("pragma", before, fx, 1199).
ops__op_table("pred", before, fx, 1180).
% ops__op_table("pure", before, fy, 1150).
ops__op_table("rule", before, fx, 1199).
ops__op_table("some", before, fxy, 950).
% ops__op_table("sorted", after, xf, 1171).	% can't support this
% ops__op_table("sorted", after, xfx, 1171).
% ops__op_table("spy", before, fy, 900).
ops__op_table("then", after, xfx, 1150).
% ops__op_table("to", after, xfx, 980).
ops__op_table("type", before, fx, 1180).
% ops__op_table("update", before, fy, 1175).
% ops__op_table("useIf", before, fx, 1180).
ops__op_table("use_adt", before, fx, 1199).
ops__op_table("use_cons", before, fx, 1199).
ops__op_table("use_module", before, fx, 1199).
ops__op_table("use_op", before, fx, 1199).
ops__op_table("use_pred", before, fx, 1199).
ops__op_table("use_sym", before, fx, 1199).
ops__op_table("use_type", before, fx, 1199).
% ops__op_table("wait", before, fy, 900).
ops__op_table("when", after, xfx, 900).
ops__op_table("where", after, xfx, 1175).
ops__op_table("~", before, fy, 900).
% ops__op_table("~=", after, xfx, 700).

ops__init_op_table(ops__table).

%-----------------------------------------------------------------------------%

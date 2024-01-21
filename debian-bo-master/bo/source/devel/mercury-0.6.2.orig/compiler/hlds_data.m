%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module defines the part of the HLDS that deals with issues related
% to data and its representation: function symbols, types, insts, modes.

% Main authors: fjh, conway.

:- module hlds_data.

:- interface.

:- import_module hlds_pred, llds, prog_data.
:- import_module bool, list, map, varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The symbol table for constructors.
	% This table is used by the type-checker to look
	% up the type of functors/constants.

:- type cons_table	==	map(cons_id, list(hlds__cons_defn)).

:- type cons_id		--->	cons(sym_name, arity)	% name, arity
			;	int_const(int)
			;	string_const(string)
			;	float_const(float)
			;	pred_const(pred_id, proc_id)
			;	code_addr_const(pred_id, proc_id)
				% Used for constructing type_infos.
				% Note that a pred_const is for a closure
				% whereas a code_addr_const is just an address.
			;	base_type_info_const(string, string, int).
				% module name, type name, type arity

	% A cons_defn is the definition of a constructor (i.e. a constant
	% or a functor) for a particular type.

:- type hlds__cons_defn
	--->	hlds__cons_defn(
			% maybe add tvarset?
			list(type),		% The types of the arguments
						% of this functor (if any)
			type_id,		% The result type, i.e. the
						% type to which this
						% cons_defn belongs.
			term__context		% The location of this
						% ctor definition in the
						% original source code
		).

%-----------------------------------------------------------------------------%

	% Various predicates for accessing the cons_id type.

	% Given a cons_id, convert it into a const (from term.m) and
	% an integer arity.  Fails if the cons_id is not representable
	% as a const (for example, if it is a higher-order pred constant
	% or an address constant or has a module qualifier).

:- pred cons_id_to_const(cons_id, const, arity).
:- mode cons_id_to_const(in, out, out) is semidet.

	% The reverse conversion - make a cons_id for a functor.
	% Given a const and an arity for the functor, create a cons_id.

:- pred make_functor_cons_id(const, arity, cons_id).
:- mode make_functor_cons_id(in, in, out) is det.

	% Another way of making a cons_id from a functor.
	% Given the name, argument types, and type_id of a functor,
	% create a cons_id for that functor.

:- pred make_cons_id(sym_name, list(constructor_arg), type_id, cons_id).
:- mode make_cons_id(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

	% Module qualified cons_ids can't be converted to consts.
cons_id_to_const(cons(unqualified(Name), Arity), term__atom(Name), Arity).
cons_id_to_const(int_const(Int), term__integer(Int), 0).
cons_id_to_const(string_const(String), term__string(String), 0).
cons_id_to_const(float_const(Float), term__float(Float), 0).

make_functor_cons_id(term__atom(Name), Arity, cons(unqualified(Name), Arity)).
make_functor_cons_id(term__integer(Int), _, int_const(Int)).
make_functor_cons_id(term__string(String), _, string_const(String)).
make_functor_cons_id(term__float(Float), _, float_const(Float)).

make_cons_id(SymName, Args, _TypeId, cons(SymName, Arity)) :-
	list__length(Args, Arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% The symbol table for types.

:- type type_id		== 	pair(sym_name, arity).
				% name, arity

:- type type_table	==	map(type_id, hlds__type_defn).

	% This is how type, modes and constructors are represented.
	% The parts that are not defined here (i.e. type_param, constructor,
	% type, inst, mode, condition) are represented in the same way as
	% in prog_io.m, and are defined there.

	% An hlds__type_defn holds the information about a type definition.

:- type hlds__type_defn.

:- pred hlds_data__set_type_defn(tvarset, list(type_param),
	hlds__type_body, import_status, term__context, hlds__type_defn).
:- mode hlds_data__set_type_defn(in, in, in, in, in, out) is det.

:- pred hlds_data__get_type_defn_tvarset(hlds__type_defn, tvarset).
:- mode hlds_data__get_type_defn_tvarset(in, out) is det.

:- pred hlds_data__get_type_defn_tparams(hlds__type_defn, list(type_param)).
:- mode hlds_data__get_type_defn_tparams(in, out) is det.

:- pred hlds_data__get_type_defn_body(hlds__type_defn, hlds__type_body).
:- mode hlds_data__get_type_defn_body(in, out) is det.

:- pred hlds_data__get_type_defn_status(hlds__type_defn, import_status).
:- mode hlds_data__get_type_defn_status(in, out) is det.

:- pred hlds_data__get_type_defn_context(hlds__type_defn, term__context).
:- mode hlds_data__get_type_defn_context(in, out) is det.

:- pred hlds_data__set_type_defn_status(hlds__type_defn, import_status,
			hlds__type_defn).
:- mode hlds_data__set_type_defn_status(in, in, out) is det.

	% An `hlds__type_body' holds the body of a type definition:
	% du = discriminated union, uu = undiscriminated union,
	% eqv_type = equivalence type (a type defined to be equivalen
	% to some other type)

:- type hlds__type_body
	--->	du_type(
			list(constructor), 	% the ctors for this type
			cons_tag_values,	% their tag values
			bool		% is this type an enumeration?
		)
	;	uu_type(list(type))	% not yet implemented!
	;	eqv_type(type)
	;	abstract_type.

	% The `cons_tag_values' type stores the information on how
	% a discriminated union type is represented.
	% For each functor in the d.u. type, it gives a cons_tag
	% which specifies how that functor and its arguments are represented.

:- type cons_tag_values	== map(cons_id, cons_tag).

	% A `cons_tag' specifies how a functor and its arguments (if any)
	% are represented.  Currently all values are represented as
	% a single word; values which do not fit into a word are represented
	% by a (possibly tagged) pointer to memory on the heap.

:- type cons_tag
	--->	string_constant(string)
			% Strings are represented using the string_const()
			% macro; in the current implementation, Mercury
			% strings are represented just as C null-terminated
			% strings.
	;	float_constant(float)
			% Floats are represented using the float_to_word(),
			% word_to_float(), and float_const() macros.
			% The default implementation of these is to
			% use boxed double-precision floats.
	;	int_constant(int)
			% This means the constant is represented just as
			% a word containing the specified integer value.
			% This is used for enumerations and character
			% constants as well as for int constants.
	;	pred_closure_tag(pred_id, proc_id)
			% Higher-order pred closures tags.
			% These are represented as a pointer to
			% an argument vector.
			% The first two words of the argument vector
			% hold the number of args and the address of
			% the procedure respectively.
			% The remaining words hold the arguments.
	;	code_addr_constant(pred_id, proc_id)
			% Procedure address constants
			% (used for constructing type_infos).
			% The word just contains the address of the
			% specified procedure.
	;	base_type_info_constant(string, string, int)
			% This is how we refer to base_type_info structures
			% represented as global data. The two strings are
			% the name of the module the type is defined in
			% and the name of the type, while the integer is
			% the arity.
	;	simple_tag(tag_bits)
			% This is for constants or functors which only
			% require a simple tag.  (A "simple" tag is one
			% which fits on the bottom of a pointer - i.e.
			% two bits for 32-bit architectures, or three bits
			% for 64-bit architectures).
			% For constants we store a tagged zero, for functors
			% we store a tagged pointer to the argument vector.
	;	complicated_tag(tag_bits, int)
			% This is for functors or constants which
			% require more than just a two-bit tag. In this case,
			% we use both a primary and a secondary tag.
			% The secondary tag is stored as the first word of
			% the argument vector. (If it is a constant, then
			% in this case there is an argument vector of size 1
			% which just holds the secondary tag.)
	;	complicated_constant_tag(tag_bits, int)
			% This is for constants which require more than a
			% two-bit tag. In this case, we use both a primary
			% and a secondary tag, but this time the secondary
			% tag is stored in the rest of the main word rather
			% than in the first word of the argument vector.
	;	no_tag.
			% This is for types with a single functor of arity one.
			% In this case, we don't need to store the functor,
			% and instead we store the argument directly.

	% The type `tag_bits' holds a simple tag value.

:- type tag_bits	==	int.	% actually only 2 (or maybe 3) bits

:- implementation.

:- type hlds__type_defn
	--->	hlds__type_defn(
			tvarset,		% Names of type vars (empty
						% except for polymorphic types)
			list(type_param),	% Formal type parameters
			hlds__type_body,	% The definition of the type

			import_status,		% Is the type defined in this
						% module, and if yes, is it
						% exported

%			condition,		% UNUSED
%				% Reserved for holding a user-defined invariant
%				% for the type, as in the NU-Prolog's type
%				% checker, which allows `where' conditions on
%				% type definitions.  For example:
%				% :- type sorted_list(T) == list(T)
%				%	where sorted.

			term__context		% The location of this type
						% definition in the original
						% source code
		).

hlds_data__set_type_defn(Tvarset, Params, Body, Status, Context, Defn) :-
	Defn = hlds__type_defn(Tvarset, Params, Body, Status, Context).

hlds_data__get_type_defn_tvarset(hlds__type_defn(Tvarset, _, _, _, _), Tvarset).
hlds_data__get_type_defn_tparams(hlds__type_defn(_, Params, _, _, _), Params).
hlds_data__get_type_defn_body(hlds__type_defn(_, _, Body, _, _), Body).
hlds_data__get_type_defn_status(hlds__type_defn(_, _, _, Status, _), Status).
hlds_data__get_type_defn_context(hlds__type_defn(_, _, _, _, Context), Context).

hlds_data__set_type_defn_status(hlds__type_defn(A, B, C, _, E), Status, 
				hlds__type_defn(A, B, C, Status, E)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% The symbol table for insts.

:- type inst_id		==	pair(sym_name, arity).
				% name, arity.

:- type inst_table.

:- type user_inst_table.
:- type user_inst_defns ==	map(inst_id, hlds__inst_defn).

:- type unify_inst_table ==	map(inst_name, maybe_inst_det).

:- type unify_inst_pair	--->	unify_inst_pair(is_live, inst, inst,
					unify_is_real).

:- type merge_inst_table ==	map(pair(inst), maybe_inst).

:- type ground_inst_table == 	map(inst_name, maybe_inst).

:- type shared_inst_table == 	map(inst_name, maybe_inst).

:- type mostly_uniq_inst_table == map(inst_name, maybe_inst).

:- type maybe_inst	--->	unknown
			;	known(inst).

:- type maybe_inst_det	--->	unknown
			;	known(inst, determinism).

	% An `hlds__inst_defn' holds the information we need to store
	% about inst definitions such as
	%	:- inst list_skel(I) = bound([] ; [I | list_skel(I)].

:- type hlds__inst_defn
	--->	hlds__inst_defn(
			varset,			% The names of the inst
						% parameters (if any).
			list(inst_param),	% The inst parameters (if any).
						% ([I] in the above example.)
			hlds__inst_body,	% The definition of this inst.
			condition,		% Unused (reserved for
						% holding a user-defined 
						% invariant).
			term__context,		% The location in the source
						% code of this inst definition.

			import_status		% So intermod.m can tell 
						% whether to output this inst.
		).

:- type hlds__inst_body
	--->	eqv_inst(inst)			% This inst is equivalent to
						% some other inst.
	;	abstract_inst.			% This inst is just a forward
						% declaration; the real
						% definition will be filled in
						% later.  (XXX Abstract insts
						% are not really supported.)

%-----------------------------------------------------------------------------%

:- pred inst_table_init(inst_table).
:- mode inst_table_init(out) is det.

:- pred inst_table_get_user_insts(inst_table, user_inst_table).
:- mode inst_table_get_user_insts(in, out) is det.

:- pred inst_table_get_unify_insts(inst_table, unify_inst_table).
:- mode inst_table_get_unify_insts(in, out) is det.

:- pred inst_table_get_merge_insts(inst_table, merge_inst_table).
:- mode inst_table_get_merge_insts(in, out) is det.

:- pred inst_table_get_ground_insts(inst_table, ground_inst_table).
:- mode inst_table_get_ground_insts(in, out) is det.

:- pred inst_table_get_shared_insts(inst_table, shared_inst_table).
:- mode inst_table_get_shared_insts(in, out) is det.

:- pred inst_table_get_mostly_uniq_insts(inst_table, mostly_uniq_inst_table).
:- mode inst_table_get_mostly_uniq_insts(in, out) is det.

:- pred inst_table_set_user_insts(inst_table, user_inst_table, inst_table).
:- mode inst_table_set_user_insts(in, in, out) is det.

:- pred inst_table_set_unify_insts(inst_table, unify_inst_table, inst_table).
:- mode inst_table_set_unify_insts(in, in, out) is det.

:- pred inst_table_set_merge_insts(inst_table, merge_inst_table, inst_table).
:- mode inst_table_set_merge_insts(in, in, out) is det.

:- pred inst_table_set_ground_insts(inst_table, ground_inst_table, inst_table).
:- mode inst_table_set_ground_insts(in, in, out) is det.

:- pred inst_table_set_shared_insts(inst_table, shared_inst_table, inst_table).
:- mode inst_table_set_shared_insts(in, in, out) is det.

:- pred inst_table_set_mostly_uniq_insts(inst_table, mostly_uniq_inst_table,
					inst_table).
:- mode inst_table_set_mostly_uniq_insts(in, in, out) is det.

:- pred user_inst_table_get_inst_defns(user_inst_table, user_inst_defns).
:- mode user_inst_table_get_inst_defns(in, out) is det.

:- pred user_inst_table_get_inst_ids(user_inst_table, list(inst_id)).
:- mode user_inst_table_get_inst_ids(in, out) is det.

:- pred user_inst_table_insert(user_inst_table, inst_id, hlds__inst_defn,
					user_inst_table).
:- mode user_inst_table_insert(in, in, in, out) is semidet.

	% Optimize the user_inst_table for lookups. This just sorts
	% the cached list of inst_ids.
:- pred user_inst_table_optimize(user_inst_table, user_inst_table).
:- mode user_inst_table_optimize(in, out) is det.

:- implementation.

:- type inst_table
	--->	inst_table(
			user_inst_table,
			unify_inst_table,
			merge_inst_table,
			ground_inst_table,
			shared_inst_table,
			mostly_uniq_inst_table
		).

:- type user_inst_defns.

:- type user_inst_table
	--->	user_inst_table(
			user_inst_defns,
			list(inst_id)	% Cached for efficiency when module
				% qualifying the modes of lambda expressions.
		).

inst_table_init(inst_table(UserInsts, UnifyInsts, MergeInsts, GroundInsts,
				SharedInsts, NondetLiveInsts)) :-
	map__init(UserInstDefns),
	UserInsts = user_inst_table(UserInstDefns, []),
	map__init(UnifyInsts),
	map__init(MergeInsts),
	map__init(GroundInsts),
	map__init(SharedInsts),
	map__init(NondetLiveInsts).

inst_table_get_user_insts(inst_table(UserInsts, _, _, _, _, _), UserInsts).

inst_table_get_unify_insts(inst_table(_, UnifyInsts, _, _, _, _), UnifyInsts).

inst_table_get_merge_insts(inst_table(_, _, MergeInsts, _, _, _), MergeInsts).

inst_table_get_ground_insts(inst_table(_, _, _, GroundInsts, _, _),
			GroundInsts).

inst_table_get_shared_insts(inst_table(_, _, _, _, SharedInsts, _),
			SharedInsts).

inst_table_get_mostly_uniq_insts(inst_table(_, _, _, _, _, NondetLiveInsts),
			NondetLiveInsts).

inst_table_set_user_insts(inst_table(_, B, C, D, E, F), UserInsts,
			inst_table(UserInsts, B, C, D, E, F)).

inst_table_set_unify_insts(inst_table(A, _, C, D, E, F), UnifyInsts,
			inst_table(A, UnifyInsts, C, D, E, F)).

inst_table_set_merge_insts(inst_table(A, B, _, D, E, F), MergeInsts,
			inst_table(A, B, MergeInsts, D, E, F)).

inst_table_set_ground_insts(inst_table(A, B, C, _, E, F), GroundInsts,
			inst_table(A, B, C, GroundInsts, E, F)).

inst_table_set_shared_insts(inst_table(A, B, C, D, _, F), SharedInsts,
			inst_table(A, B, C, D, SharedInsts, F)).

inst_table_set_mostly_uniq_insts(inst_table(A, B, C, D, E, _), NondetLiveInsts,
			inst_table(A, B, C, D, E, NondetLiveInsts)).

user_inst_table_get_inst_defns(user_inst_table(InstDefns, _), InstDefns).

user_inst_table_get_inst_ids(user_inst_table(_, InstIds), InstIds).

user_inst_table_insert(user_inst_table(InstDefns0, InstIds0), InstId,
			InstDefn, user_inst_table(InstDefns, InstIds)) :-
	map__insert(InstDefns0, InstId, InstDefn, InstDefns),
	InstIds = [InstId | InstIds0].

user_inst_table_optimize(user_inst_table(InstDefns0, InstIds0), 
			user_inst_table(InstDefns, InstIds)) :-
	map__optimize(InstDefns0, InstDefns),
	list__sort(InstIds0, InstIds).
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% The symbol table for modes.

:- type mode_id		==	pair(sym_name, arity).
				% name, arity

:- type mode_table.
:- type mode_defns	 ==	map(mode_id, hlds__mode_defn).

	% A hlds__mode_defn stores the information about a mode
	% definition such as
	%	:- mode out :: free -> ground.
	% or
	%	:- mode in(I) :: I -> I.
	% or
	%	:- mode in_list_skel :: in(list_skel).

:- type hlds__mode_defn
	--->	hlds__mode_defn(
			varset,			% The names of the inst
						% parameters (if any).
			list(inst_param),	% The list of the inst
						% parameters (if any).
						% (e.g. [I] for the second
						% example above.)
			hlds__mode_body,	% The definition of this mode.
			condition,		% Unused (reserved for
						% holding a user-defined
						% invariant).
			term__context,		% The location of this mode
						% definition in the original
						% source code.
			import_status		% So intermod.m can tell 
						% whether to output this mode.
					
		).

	% The only sort of mode definitions allowed are equivalence modes.

:- type hlds__mode_body
	--->	eqv_mode(mode).		% This mode is equivalent to some
					% other mode.

	% Given a mode table get the mode_id - hlds__mode_defn map.
:- pred mode_table_get_mode_defns(mode_table, mode_defns).
:- mode mode_table_get_mode_defns(in, out) is det.

	% Get the list of defined mode_ids from the mode_table.
:- pred mode_table_get_mode_ids(mode_table, list(mode_id)).
:- mode mode_table_get_mode_ids(in, out) is det.

	% Insert a mode_id and corresponding hlds__mode_defn into the
	% mode_table. Fail if the mode_id is already present in the table.
:- pred mode_table_insert(mode_table, mode_id, hlds__mode_defn, mode_table).
:- mode mode_table_insert(in, in, in, out) is semidet.

:- pred mode_table_init(mode_table).
:- mode mode_table_init(out) is det.

	% Optimize the mode table for lookups.
:- pred mode_table_optimize(mode_table, mode_table).
:- mode mode_table_optimize(in, out) is det.


:- implementation.

:- type mode_table
	--->	mode_table(
			mode_defns,
			list(mode_id)	% Cached for efficiency
		).

mode_table_get_mode_defns(mode_table(ModeDefns, _), ModeDefns).

mode_table_get_mode_ids(mode_table(_, ModeIds), ModeIds).

mode_table_insert(mode_table(ModeDefns0, ModeIds0), ModeId, ModeDefn,
			mode_table(ModeDefns, ModeIds)) :-
	map__insert(ModeDefns0, ModeId, ModeDefn, ModeDefns),
	ModeIds = [ModeId | ModeIds0].

mode_table_init(mode_table(ModeDefns, [])) :-
	map__init(ModeDefns).

mode_table_optimize(mode_table(ModeDefns0, ModeIds0),
			mode_table(ModeDefns, ModeIds)) :-
	map__optimize(ModeDefns0, ModeDefns), 	% NOP	
	list__sort(ModeIds0, ModeIds).		% Sort the list of mode_ids
			% for quick conversion to a set by module_qual
			% when qualifying the modes of lambda expressions.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- interface.

:- type determinism	--->	det
			;	semidet
			;	nondet
			;	multidet
			;	cc_nondet
			;	cc_multidet
			;	erroneous
			;	failure.

:- type can_fail	--->	can_fail
			;	cannot_fail.

:- type soln_count
			--->	at_most_zero
			;	at_most_one
			;	at_most_many_cc
				% "_cc" means "committed-choice": there is
				% more than one logical solution, but
				% the pred or goal is being used in a context
				% where we are only looking for the first
				% solution.
			;	at_most_many.

:- pred determinism_components(determinism, can_fail, soln_count).
:- mode determinism_components(in, out, out) is det.
:- mode determinism_components(out, in, in) is det.

:- pred determinism_to_code_model(determinism, code_model).
:- mode determinism_to_code_model(in, out) is det.

:- implementation.

determinism_components(det,         cannot_fail, at_most_one).
determinism_components(semidet,     can_fail,    at_most_one).
determinism_components(multidet,    cannot_fail, at_most_many).
determinism_components(nondet,      can_fail,    at_most_many).
determinism_components(cc_multidet, cannot_fail, at_most_many_cc).
determinism_components(cc_nondet,   can_fail,    at_most_many_cc).
determinism_components(erroneous,   cannot_fail, at_most_zero).
determinism_components(failure,     can_fail,    at_most_zero).

determinism_to_code_model(det,         model_det).
determinism_to_code_model(semidet,     model_semi).
determinism_to_code_model(nondet,      model_non).
determinism_to_code_model(multidet,    model_non).
determinism_to_code_model(cc_nondet,   model_semi).
determinism_to_code_model(cc_multidet, model_det).
determinism_to_code_model(erroneous,   model_det).
determinism_to_code_model(failure,     model_semi).

%-----------------------------------------------------------------------------%

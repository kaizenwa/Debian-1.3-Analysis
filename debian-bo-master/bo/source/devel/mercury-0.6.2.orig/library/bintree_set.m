%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%--------------------------------------------------------------------------%

:- module bintree_set.

% Main authors: fjh.
% Stability: medium.

% This file provides an alternate implementation of the `set' ADT
% defined in module `set'.  See that file for comments about the semantics
% of the predicates.  This file implements sets as binary sorted trees,
% using module `bintree', and so provides different performance
% characteristics.

% bintree_set__is_member is a version of bintree_set__member
% with a more restricted mode, which is implemented much
% more efficiently using bintree__search.

%--------------------------------------------------------------------------%

:- interface.
:- import_module list.

:- type bintree_set(_T).

	% `bintree_set__list_to_set(List, Set)' is true iff `Set' is the set 
	% containing only the members of `List'.

:- pred bintree_set__list_to_set(list(T), bintree_set(T)).
:- mode bintree_set__list_to_set(in, out) is det.

	% `bintree_set__sorted_list_to_set(List, Set)' is true iff
	% `Set' is the set containing only the members of `List'.
	% `List' must be sorted.

:- pred bintree_set__sorted_list_to_set(list(T), bintree_set(T)).
:- mode bintree_set__sorted_list_to_set(in, out) is det.

	% `bintree_set__list_to_bintree_set(Set, List)' is true iff
	% `List' is the list of all the members of `Set', in sorted
	% order.

:- pred bintree_set__to_sorted_list(bintree_set(T), list(T)).
:- mode bintree_set__to_sorted_list(in, out) is det.

	% `bintree_set__init(Set)' is true iff `Set' is an empty set.

:- pred bintree_set__init(bintree_set(_T)).
:- mode bintree_set__init(uo) is det.

:- pred bintree_set__singleton_set(bintree_set(T), T).
:- mode bintree_set__singleton_set(out, in) is det.

	% `bintree_set__equal(SetA, SetB)' is true iff
	% `SetA' and `SetB' contain the same elements.

:- pred bintree_set__equal(bintree_set(T), bintree_set(T)).
:- mode bintree_set__equal(in, in) is semidet.

	% `bintree_set__subset(SetA, SetB)' is true iff `SetA' is a
	% subset of `SetB'.

:- pred bintree_set__subset(bintree_set(T), bintree_set(T)).
:- mode bintree_set__subset(in, in) is semidet.

	% `bintree_set__superset(SetA, SetB)' is true iff `SetA' is a
	% superset of `SetB'.

:- pred bintree_set__superset(bintree_set(T), bintree_set(T)).
:- mode bintree_set__superset(in, in) is semidet.

	% `bintree_set_member(X, Set)' is true iff `X' is a member of `Set'.

:- pred bintree_set__member(T, bintree_set(T)).
:- mode bintree_set__member(in, in) is semidet.
:- mode bintree_set__member(out, in) is nondet.

	% `bintree_set_member(X, Set)' is true iff `X' is a member of `Set'.

:- pred bintree_set__is_member(T, bintree_set(T)).
:- mode bintree_set__is_member(in, in) is semidet.

	% `bintree_set__insert(Set0, X, Set)' is true iff `Set' is the union of
	% `Set0' and the set containing only `X'.

:- pred bintree_set__insert(bintree_set(T), T, bintree_set(T)).
:- mode bintree_set__insert(di, di, uo) is det.
:- mode bintree_set__insert(in, in, out) is det.

	% `bintree_set__insert_list(Set0, Xs, Set)' is true iff `Set'
	% is the union of `Set0' and the set containing only the
	% members of `Xs'.

:- pred bintree_set__insert_list(bintree_set(T), list(T), bintree_set(T)).
:- mode bintree_set__insert_list(di, di, uo) is det.
:- mode bintree_set__insert_list(in, in, out) is det.

	% `bintree_set__remove(Set0, X, Set)' is true iff `Set0' contains `X',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only `X', i.e.  if `Set' is the set which contains
	% all the elements of `Set0' except `X'.

:- pred bintree_set__remove(bintree_set(T), T, bintree_set(T)).
:- mode bintree_set__remove(in, in, out) is semidet.
% The following mode could be implemented, but hasn't been:
% :- mode bintree_set__remove(in, out, out) is nondet. 

	% `bintree_set__remove_list(Set0, Xs, Set)' is true iff Xs does
	% not contain any duplicates, `Set0' contains every member of
	% `Xs', and `Set' is the relative complement of `Set0' and the
	% set containing only the members of `Xs'.

:- pred bintree_set__remove_list(bintree_set(T), list(T), bintree_set(T)).
:- mode bintree_set__remove_list(in, in, out) is semidet.

	% `bintree_set__delete(Set0, X, Set)' is true iff `Set' is the relative
	% complement of `Set0' and the set containing only `X', i.e.
	% if `Set' is the set which contains all the elements of `Set0'
	% except `X'.

:- pred bintree_set__delete(bintree_set(T), T, bintree_set(T)).
:- mode bintree_set__delete(in, in, out) is det.

	% `bintree_set__delete_list(Set0, Xs, Set)' is true iff `Set'
	% is the relative complement of `Set0' and the set containing
	% only the members of `Xs'.

:- pred bintree_set__delete_list(bintree_set(T), list(T), bintree_set(T)).
:- mode bintree_set__delete_list(in, in, out) is det.

	% `set_union(SetA, SetB, Set)' is true iff `Set' is the union of
	% `SetA' and `SetB'.  If the sets are known to be of different
	% sizes, then for efficiency make `SetA' the larger of the two.

:- pred bintree_set__union(bintree_set(T), bintree_set(T), bintree_set(T)).
:- mode bintree_set__union(in, in, out) is det.

	% `set_intersect(SetA, SetB, Set)' is true iff `Set' is the
	% intersection of `SetA' and `SetB'.

:- pred bintree_set__intersect(bintree_set(T), bintree_set(T), bintree_set(T)).
:- mode bintree_set__intersect(in, in, out) is det.

%--------------------------------------------------------------------------%

:- implementation.
:- import_module bintree, assoc_list.
:- import_module std_util.

:- type bintree_set(T)          ==      bintree(T, unit).

%--------------------------------------------------------------------------%

bintree_set__list_to_set(List, Set) :-
	list__sort_and_remove_dups(List, SortedList),
	bintree_set__sorted_list_to_set(SortedList, Set).

bintree_set__sorted_list_to_set(List, Set) :-
	assoc_unit(List, AssocList),
	bintree__from_sorted_list(AssocList, Set).

bintree_set__to_sorted_list(Set, List) :-
	bintree__keys(Set, List).

:- pred assoc_unit(list(T), assoc_list(T, unit)).
:- mode assoc_unit(in, out) is det.

assoc_unit([], []).
assoc_unit([X | Xs], [X - unit | Ys]) :-
	assoc_unit(Xs, Ys).

%--------------------------------------------------------------------------%

bintree_set__init(Set) :-
	bintree__init(Set).

bintree_set__singleton_set(Set, Elem) :-
	bintree__init(Set0),
	bintree__set(Set0, Elem, unit, Set).

bintree_set__equal(SetA, SetB) :-
	bintree__keys(SetA, SortedElements),
	bintree__keys(SetB, SortedElements).

%--------------------------------------------------------------------------%

bintree_set__subset(S0, S1) :-
	bintree__keys(S0, SortedElements0),
	bintree_set__contains_list(SortedElements0, S1).

:- pred bintree_set__contains_list(list(T), bintree_set(T)).
:- mode bintree_set__contains_list(in, in) is semidet.

bintree_set__contains_list([E|Es], S) :-
	bintree__search(S, E, _),
	bintree_set__contains_list(Es, S).

bintree_set__superset(S0, S1) :-
	bintree_set__subset(S1, S0).

%--------------------------------------------------------------------------%

bintree_set__member(E, S) :-
	bintree__keys(S, Elements),
	list__member(E, Elements).

bintree_set__is_member(E, S) :-
	bintree__search(S, E, _).

%--------------------------------------------------------------------------%

:- bintree_set__insert_list(_, Xs, _) when Xs.

bintree_set__insert_list(S, [], S).
bintree_set__insert_list(S0, [E|Es], S) :-
	bintree_set__insert(S0, E, S1),
	bintree_set__insert_list(S1, Es, S).

bintree_set__insert(S0, E, S) :-
	bintree__set(S0, E, unit, S).

%--------------------------------------------------------------------------%

:- bintree_set__remove_list(_, Xs, _) when Xs.

bintree_set__remove_list(S, [], S).
bintree_set__remove_list(S0, [X | Xs], S) :-
	bintree_set__member(X, S0),
	bintree_set__remove(S0, X, S1),
	bintree_set__remove_list(S1, Xs, S).

bintree_set__remove(S0, E, S) :-
	bintree__remove(S0, E, _, S).

%--------------------------------------------------------------------------%

:- bintree_set__delete_list(_, Xs, _) when Xs.

bintree_set__delete_list(S, [], S).
bintree_set__delete_list(S0, [X | Xs], S) :-
	bintree_set__delete(S0, X, S1),
	bintree_set__delete_list(S1, Xs, S).

bintree_set__delete(S0, E, S) :-
	bintree__delete(S0, E, S).

%--------------------------------------------------------------------------%

bintree_set__union(S0, S1, S) :-
	bintree__to_list(S0, L0),
	bintree__to_list(S1, L1),
	list__merge(L0, L1, L),
	bintree__from_sorted_list(L, S).

bintree_set__intersect(S0, S1, S) :-
	bintree__keys(S1, L1),
	bintree_set__delete_list(S0, L1, S).

%--------------------------------------------------------------------------%

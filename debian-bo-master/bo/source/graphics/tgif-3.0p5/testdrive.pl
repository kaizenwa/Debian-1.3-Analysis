%
% Author:      William Chia-Wei Cheng (william@cs.ucla.edu)
%
% Copyright (C) 1992-1993, William Cheng.
%
% Permission limited to the use, copy, modify, and distribute this software
% and its documentation for any purpose is hereby granted by the Author without
% fee, provided that the above copyright notice appear in all copies and
% that both the copyright notice and this permission notice appear in
% supporting documentation, and that the name of the Author not be used
% in advertising or publicity pertaining to distribution of the software
% without specific, written prior permission.  The Author makes no
% representations about the suitability of this software for any purpose.
% It is provided "as is" without express or implied warranty.  All other
% rights (including the right to sell "tgif" and the right to sell derivative
% works of tgif) are reserved by the Author.
%
% THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
% INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
% EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT OR
% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
% USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
% PERFORMANCE OF THIS SOFTWARE.
%
% @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/testdrive.pl,v 3.0 1996/05/06 16:12:12 william Exp $
%

% ?- compile(testdrive).
% ?- tgif.
%
% This is an example of a Prolog driver, which list the IDs of the
% objects in the current drawing (attributes are skipped).  The code
% might be buggy, it is there to give some hints of how 'tgif.pl' can
% be used.
%
% In order to use it, don't forget to uncomment the stuff related to
% 'frontend11.o' in either the 'Imakefile' or the 'Makefile.noimake'.
% It also expects 'tgif.pl' and 'frontend.pl' to be in the same directory
% as itself.

:- ensure_loaded(frontend).
:- ensure_loaded(tgif).

tgif :- interface(init,'',Cmd,Domain,File,_,_,_,_), tgif(Cmd,Domain,File).

tgif(InitFile) :-
	interface(init,InitFile,Cmd,Domain,File,_,_,_,_), tgif(Cmd,Domain,File).

tgif('Solve',_Domain,File) :-
	process_file(File),
	interface('','',NewCmd,NewDomain,NewFile,_,_,_,_),
	!, tgif(NewCmd,NewDomain,NewFile).
tgif('Quit',_Domain,_File) :- interface(quit,'',_,_,_,_,_,_,_), !.

% --------------------------------------------------------------------- %

process_file(File) :-
	tgif_real_clean,
	my_consult(File),
	tgif_state(_,_),
	write('=============='), nl,
	write('Listing IDs...'), nl,
	write('=============='), nl,
	( tgif_obj(Obj), print_id(Obj,0), fail ; ! ).

% --------------------------------------------------------------------- %

print_id(Obj,Level) :-
	print_level(Level), NextLevel is Level+1,
	get_id(Obj,Id),
	functor(Obj,Name,_Arity),
	write(Name), write(': '), write(Id), nl, !,
	(	( Name==group | Name==sym | Name==icon ) ->
		atom_chars(Name,NameStr), catlist(["tgif_",NameStr],FunctorStr),
		atom_chars(Functor,FunctorStr), Goal =.. [Functor,Obj,Parms],
		call(Goal), memberchk(=(objs,Objs),Parms),
		( member(SubObj,Objs), print_id(SubObj,NextLevel), fail ; true )
	;	true
	),
	!, get_attrs(Obj,Attrs),
	(	Attrs \== [] ->
		print_level(NextLevel), write('attrs:'), nl,
		AttrLevel is NextLevel+1,
		!, print_attr_id(Attrs,AttrLevel)
	;	true
	).

print_attr_id([],_AttrLevel) :- !.
print_attr_id([Attr|Attrs],AttrLevel) :-
	tgif_attr(Attr,AttrParms),
	memberchk(=(text_obj,TextObj),AttrParms),
	print_id(TextObj,AttrLevel),
	!, print_attr_id(Attrs,AttrLevel).

print_level(0) :- !.
print_level(N) :- write('   '), N1 is N-1, !, print_level(N1).

get_id(Obj,Id) :-
	functor(Obj,Name,_Arity),
	atom_chars(Name,NameStr), catlist(["tgif_",NameStr],FunctorStr),
	atom_chars(Functor,FunctorStr), Goal =.. [Functor,Obj,Parms],
	call(Goal),
	memberchk(=(id,Id),Parms), !.
get_id(_Obj,none).

get_attrs(Obj,Attrs) :-
	functor(Obj,Name,_Arity),
	atom_chars(Name,NameStr), catlist(["tgif_",NameStr],FunctorStr),
	atom_chars(Functor,FunctorStr), Goal =.. [Functor,Obj,Parms],
	call(Goal),
	memberchk(=(attrs,Attrs),Parms), !.
get_attrs(_Obj,[]).

% --------------------------------------------------------------------- %

my_consult(File) :- seeing(X), see(File), my_consult, seen, see(X).

my_consult :-
	repeat,
	read(Term),
	(	Term == end_of_file ->
		!
	;	Term = :-(_) ->			% ignore directives
		fail
	;	assertz(Term),
		fail
	).

member(Element, [Element|_]).
member(Element, [_|Rest]) :- member(Element, Rest).

memberchk(Element, [Element|_]) :- !.
memberchk(Element, [_|Rest]) :- memberchk(Element, Rest).

catlist([X|[]], X) :- !.
catlist([[]|X], Y) :- !, catlist(X, Y).
catlist([[H|T]|X], [H|Y]) :- catlist([T|X], Y).

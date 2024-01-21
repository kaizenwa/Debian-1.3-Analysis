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
% @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/frontend.pl,v 3.0 1996/05/06 16:05:15 william Exp $
%

% The main predicate exported is:  interface/9.

:- abolish(foreign_file/2).
:- abolish(foreign/3).

foreign_file('frontend11.o', ['MainLoop','Animate','UpdAttrVal']).

foreign('MainLoop', c,
	interface(+string, +string, -string, -string, -string, -string,
	-string, -string, -string)).
foreign('Animate', c,
	interface_animate(+string, +string, +string, +string, -string)).
foreign('UpdAttrVal', c,
	interface_updattr(+string, +string, +string, +string, -string)).

:- load_foreign_files(['frontend11.o'], ['-L../../lib','-lX11','-lm']),
	abolish(foreign_file/2),
	abolish(foreign/3).

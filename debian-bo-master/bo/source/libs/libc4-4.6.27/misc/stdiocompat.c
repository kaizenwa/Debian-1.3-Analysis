/* Copyright (C) 1991 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>

#include <gnu-stabs.h>

#ifdef	HAVE_GNU_LD

symbol_alias(_IO_stdin_, __std_filebuf_0);
symbol_alias(_IO_stdout_, __std_filebuf_1);
symbol_alias(_IO_stderr_, __std_filebuf_2);

symbol_alias(_IO_stdin_, _cin_sbuf);
symbol_alias(_IO_stdout_, _cout_sbuf);
symbol_alias(_IO_stderr_, _cerr_sbuf);

#endif

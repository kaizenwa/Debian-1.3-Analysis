/*
   error.h

   This file is part of LuxMan.
   
   Copyright (C) 1994,1995 Frank McIngvale (frankm@nuance.com)
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
   Note to myself - common errors

   Bad arg
   Out of memory
   Mem clobbered
   Can't get here
*/
#ifndef _error_h_
#define _error_h_

void do_fatal_error( char *file, int line, char *fmt, ... );

#define fatal( fmt, arg... ) do_fatal_error( __FILE__, __LINE__, fmt, ## arg )

#define ERROR_NOMEM() fatal( "Out of memory" );

#endif

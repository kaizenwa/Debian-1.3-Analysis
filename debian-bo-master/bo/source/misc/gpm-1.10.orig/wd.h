/*
 * wd.h - some stupid things to enable messages
 *
 * Copyright 1994   DIS - Universita` di Pavia - Italy
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ********/

#ifndef _WD_H_
#define _WD_H_

#include <stdio.h>

/*========== the WD macro prints on stderr to help tracing program flow =*/

#if defined DEBUG
#  ifdef TCL_MEM_DEBUG
#  define WD (fprintf(stderr,"  File " __FILE__ "(%i)\n", __LINE__),\
               Tcl_ValidateAllMemory(__FILE__,__LINE__))
#  else
#  define WD fprintf(stderr,"File " __FILE__ "(%i)\n", __LINE__)
#  endif
#else
#define WD 
#endif


/*========== the PDEBUG symbol masks fprintf =======*/

#ifdef DEBUG
#define PDEBUG(data) fprintf data
#else
#define PDEBUG(data)
#endif

/*========== the PDEBUGG symbol hides everything =======*/

#define PDEBUGG(data)

#endif /* _WD_H_ */

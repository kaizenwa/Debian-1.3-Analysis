/* chkrlib.h prototypes of functions for chkrlib.
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written September 1993 Tristan Gingold

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#ifndef _CHKRLIB_H_
#define _CHKRLIB_H_

#include "define.h"
#include <sys/types.h>

/* The stderr for Checker.  */
extern int chkr_out;

#include "chkr-string.h"

#include "chkrsyscall.h"

extern char *chkr_getenv (const char *name);
extern char* chkr_mktemp (char *_template);
extern char *chkr_getcwd (char *buf, size_t size);
extern void chkr_qsort (PTR base, size_t nmemb, size_t size,
		int (*compar)(const PTR, const PTR));
		
#ifdef VPRINTF_FLOAT
extern char* ecvt (double arg, int ndigits, int *decpt, int *sign);
#endif

#include <alloca.h>
#include <errno.h>

void chkr_vprintf (const char *message, PTR *param) ATTRIBUTE ((format (printf, 1, 0)));
void chkr_printf (const char *message, ...) ATTRIBUTE ((format (printf, 1, 2)));
void chkr_header (const char *message, ...) ATTRIBUTE ((format (printf, 1, 2)));
void chkr_puts (const char *str);
extern void (*___chkr_trap) (const char *message);

#endif /* _CHKRLIB_H_ */

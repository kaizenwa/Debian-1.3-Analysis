/* Checker stubs for functions defined in fnmatch.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "available-stubs.h"

#ifdef HAVE_FNMATCH_H
#include <fnmatch.h>
#include "checker_api.h"

#if 0
#define HAVE_fnmatch
#endif

/* compiled from: . */
#ifdef HAVE_fnmatch
/* From `/usr/include/fnmatch.h:43'.  */
int
chkr$fnmatch (const char *pattern, const char *string, int flags)
{
  stubs_chkr_check_str (pattern, CHKR_RO, "pattern");
  stubs_chkr_check_str (string, CHKR_RO, "string");
#if USE_BI_JUMP
  __builtin_jump (fnmatch);
#else
  return fnmatch (pattern, string, flags);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fnmatch */

#endif /* HAVE_FNMATCH_H */

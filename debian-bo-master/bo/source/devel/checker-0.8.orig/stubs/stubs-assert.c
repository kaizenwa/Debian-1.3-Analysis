/* Checker stubs for functions defined in assert.h
   Copyright 1996 Tristan Gingold
		  Written July 1996 by Tristan Gingold

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

#ifdef HAVE_ASSERT_H
#include <assert.h>
#include "checker_api.h"

/* compiled from: . */
#ifdef HAVE___eprintf
/* From `/usr/i486-unknown-linux/include/assert.h:31'.  */
void
chkr$__eprintf (const char *str, const char *exp, unsigned int line, const char *filename)
{
  stubs_chkr_check_str (str, CHKR_RO, "str");
  stubs_chkr_check_str (exp, CHKR_RO, "exp");
  stubs_chkr_check_str (filename, CHKR_RO, "filename");
#if USE_BI_JUMP
  __builtin_jump (__eprintf);
#else
  __eprintf (str, exp, line, filename);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE___eprintf */


#endif /* HAVE_ASSERT_H */

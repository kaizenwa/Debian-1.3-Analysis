/* Checker stubs for functions defined in utime.h
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

#ifdef HAVE_UTIME_H
#include <utime.h>
#include "checker_api.h"

#if 0
#define HAVE_utime
#endif

/* compiled from: . */
#ifdef HAVE_utime
/* From `/usr/include/utime.h:33'.  */
int
chkr$utime (const char *path, struct utimbuf *times)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
  if (times)
    {
      stubs_chkr_check_addr (&(times->actime), sizeof (time_t), CHKR_RO, "times->actime");
      stubs_chkr_check_addr (&(times->modtime), sizeof (time_t), CHKR_RO, "times->modtime");
    }
#if USE_BI_JUMP
  __builtin_jump (utime);
#else
  return utime (path, times);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_utime */

#endif /* HAVE_UTIME_H */

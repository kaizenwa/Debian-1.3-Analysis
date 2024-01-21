/* Checker stubs for functions defined in sys/timeb.h
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

#ifdef HAVE_SYS_TIMEB_H
#include <sys/types.h>
#include <sys/timeb.h>
#include "checker_api.h"

#if 0
#define HAVE_ftime
#endif

/* compiled from: . */
#ifdef HAVE_ftime
int
chkr$ftime (struct timeb *tp)
{
  chkr_check_addr (&(tp->time), sizeof (time_t), CHKR_WO);
  chkr_check_addr (&(tp->millitm), sizeof (unsigned short), CHKR_WO);
  chkr_check_addr (&(tp->timezone), sizeof (short), CHKR_WO);
  chkr_check_addr (&(tp->dstflag), sizeof (short), CHKR_WO);
#if USE_BI_JUMP
  __builtin_jump (ftime);
#else
  return ftime (tp);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ftime */

#endif /* HAVE_SYS_TIMEB_H */

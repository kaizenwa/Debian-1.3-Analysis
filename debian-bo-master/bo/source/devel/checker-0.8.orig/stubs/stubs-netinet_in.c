/* Checker stubs for functions defined in netinet/in.h
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

#ifdef HAVE_NETINET_IN_H
#include <sys/types.h>
#include <netinet/in.h>
#include "checker_api.h"

#undef HAVE_bindresvport

#if 0
#define HAVE_ntohl
#define HAVE_ntohs
#define HAVE_htonl
#define HAVE_htons
#endif

/* compiled from: . */
#ifdef HAVE_ntohl
/* From `/usr/include/asm/byteorder.h:17'.  */
long unsigned int
chkr$ntohl (long unsigned int n)
{
#if USE_BI_JUMP
  __builtin_jump (ntohl);
#else
  return ntohl (n);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ntohl */

#ifdef HAVE_ntohs
/* From `/usr/include/asm/byteorder.h:18'.  */
short unsigned int
chkr$ntohs (short unsigned int n)
{
#if USE_BI_JUMP
  __builtin_jump (ntohs);
#else
  return ntohs (n);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ntohs */

#ifdef HAVE_htonl
/* From `/usr/include/asm/byteorder.h:19'.  */
long unsigned int
chkr$htonl (long unsigned int h)
{
#if USE_BI_JUMP
  __builtin_jump (htonl);
#else
  return htonl (h);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_htonl */

#ifdef HAVE_htons
/* From `/usr/include/asm/byteorder.h:20'.  */
short unsigned int
chkr$htons (short unsigned int h)
{
#if USE_BI_JUMP
  __builtin_jump (htons);
#else
  return htons (h);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_htons */

#ifdef HAVE_bindresvport
/* From `/usr/include/netinet/in.h:84'.  */
int
chkr$bindresvport (int arg0, struct sockaddr_in * arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg1, sizeof (struct sockaddr_in), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (bindresvport);
#else
  {
    int res;
    res = bindresvport (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_bindresvport */

#endif /* HAVE_NETINET_IN_H */

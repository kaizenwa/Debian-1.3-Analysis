/* Checker stubs for functions defined in arpa/inet.h
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

#ifdef HAVE_ARPA_INET_H
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "checker_api.h"

#if 0
#define HAVE_inet_addr
#define HAVE_inet_ntoa
#define HAVE_inet_aton
#define HAVE_inet_lnaof
#define HAVE_inet_netof
#define HAVE_inet_makeaddr
#define HAVE_inet_network
#endif

/* compiled from: . */
#ifdef HAVE_inet_addr
/* From `/usr/include/arpa/inet.h:76'.  */
long unsigned int
chkr$inet_addr (const char *cp)
{
  stubs_chkr_check_str (cp, CHKR_RO, "cp");
#if USE_BI_JUMP
  __builtin_jump (inet_addr);
#else
  return inet_addr (cp);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_inet_addr */

#ifdef HAVE_inet_aton
/* From `/usr/include/arpa/inet.h:77'.  */
int
chkr$inet_aton (const char *name, struct in_addr *addr)
{
  int res;
  
  stubs_chkr_check_str (name, CHKR_RO, "name");
  stubs_chkr_check_addr (addr, sizeof (struct in_addr), CHKR_MW, "addr");
  
  res = inet_aton (name, addr);
  if (res)
    stubs_chkr_set_right (&(addr->s_addr), sizeof (unsigned long int), CHKR_RW);
  return res;
}
#endif /* HAVE_inet_aton */

#ifdef HAVE_inet_lnaof
/* From `/usr/include/arpa/inet.h:78'.  */
long unsigned int
chkr$inet_lnaof (struct in_addr addr)
{
#if USE_BI_JUMP
  __builtin_jump (inet_lnaof);
#else
  return inet_lnaof (addr);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_inet_lnaof */

#ifdef HAVE_inet_makeaddr
/* From `/usr/include/arpa/inet.h:79'.  */
struct in_addr
chkr$inet_makeaddr (int net, int lna)
{
  struct in_addr *res;
  res = __builtin_aggregate_incoming_address ();
  if (res)
    stubs_chkr_set_right (&(res->s_addr), sizeof (unsigned long int), CHKR_RW);
  return inet_makeaddr (net, lna);
}
#endif /* HAVE_inet_makeaddr */

#ifdef HAVE_inet_netof
/* From `/usr/include/arpa/inet.h:80'.  */
int
chkr$inet_netof (struct in_addr in)
{
#if USE_BI_JUMP
  __builtin_jump (inet_netof);
#else
  return inet_netof (in);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_inet_netof */

#ifdef HAVE_inet_network
/* From `/usr/include/arpa/inet.h:81'.  */
unsigned int
chkr$inet_network (const char *cp)
{
  stubs_chkr_check_str (cp, CHKR_RO, "cp");
#if USE_BI_JUMP
  __builtin_jump (inet_network);
#else
  return inet_network (cp);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_inet_network */

#ifdef HAVE_inet_ntoa
/* From `/usr/include/arpa/inet.h:82'.  */
char *
chkr$inet_ntoa (struct in_addr in)
{
  char * res;
  int len;
  res = inet_ntoa (in);
  if (res)
    {
      len = strlen (res);
      if (len)
        stubs_chkr_set_right (res, len + 1, CHKR_RW);
    }
  return res;
}
#endif /* HAVE_inet_ntoa */

#endif /* HAVE_ARPA_INET_H */

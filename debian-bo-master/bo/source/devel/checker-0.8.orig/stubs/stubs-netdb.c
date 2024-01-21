/* Checker stubs for functions defined in netdb.h
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

#ifdef HAVE_NETDB_H
#include <sys/types.h>
#include <netdb.h>
#ifdef HAVE_RPC_RPCENT_H
#include <rpc/rpcent.h>
#endif
#include "checker_api.h"

#undef HAVE_getnetbyname
#undef HAVE_getprotobyname
#undef HAVE_getservbyport
#undef HAVE_getrpcbyname

#if 0
#define HAVE_gethostbyname
#define HAVE_gethostbyaddr
#define HAVE_getservbyname
#define HAVE_herror
#define HAVE_endhostent
#define HAVE_endnetent
#define HAVE_endprotoent
#define HAVE_endservent
#define HAVE_endrpcent
#define HAVE_gethostent
#define HAVE_getnetbyaddr
#define HAVE_getnetbyname
#define HAVE_getnetent
#define HAVE_getprotobyname
#define HAVE_getprotobynumber
#define HAVE_getprotoent
#define HAVE_getservbyport
#define HAVE_getservent
#define HAVE_getrpcent
#define HAVE_getrpcbyname
#define HAVE_getrpcbynumber
#define HAVE_sethostent
#define HAVE_setnetent
#define HAVE_setprotoent
#define HAVE_setservent
#define HAVE_setrpcent
#endif

#ifdef HAVE_chkr_func
void
stubs_chkr_set_right_hostent (struct hostent *he)
{
  int len;
  char **arg;
  stubs_chkr_set_right (&(he->h_name), sizeof (char *), CHKR_RW);
  stubs_chkr_set_right (&(he->h_aliases), sizeof (char **), CHKR_RW);
  stubs_chkr_set_right (&(he->h_addrtype), sizeof (int), CHKR_RW);
  stubs_chkr_set_right (&(he->h_length), sizeof (int), CHKR_RW);
  stubs_chkr_set_right (&(he->h_addr_list), sizeof (char **), CHKR_RW);
  len = strlen (he->h_name);
  stubs_chkr_set_right (he->h_name, len + 1, CHKR_RW);
  for (arg = he->h_aliases; *arg; arg++)
    {
      len = strlen (*arg);
      stubs_chkr_set_right (*arg, len + 1, CHKR_RW);
    }
  stubs_chkr_set_right (he->h_aliases, (arg + 1) - he->h_aliases, CHKR_RW);
  for (arg = he->h_addr_list; *arg; arg++)
    {
      len = strlen (*arg);
      stubs_chkr_set_right (*arg, len + 1, CHKR_RW);
    }
  stubs_chkr_set_right (he->h_addr_list, (arg + 1) - he->h_addr_list, CHKR_RW);
}

void
stubs_chkr_set_right_servent (struct servent *se)
{
  int len;
  char **arg;
  stubs_chkr_set_right (&(se->s_name), sizeof (char *), CHKR_RW);
  stubs_chkr_set_right (&(se->s_aliases), sizeof (char **), CHKR_RW);
  stubs_chkr_set_right (&(se->s_port), sizeof (int), CHKR_RW);
  stubs_chkr_set_right (&(se->s_proto), sizeof (int), CHKR_RW);
  len = strlen (se->s_name);
  stubs_chkr_set_right (se->s_name, len + 1, CHKR_RW);
  len = strlen (se->s_proto);
  stubs_chkr_set_right (se->s_proto, len + 1, CHKR_RW);
  for (arg = se->s_aliases; *arg; arg++)
    {
      len = strlen (*arg);
      stubs_chkr_set_right (*arg, len + 1, CHKR_RW);
    }
  stubs_chkr_set_right (se->s_aliases, (arg + 1) - se->s_aliases, CHKR_RW);
}

void
stubs_chkr_set_right_netent (struct netent *ne)
{
  int len;
  char **arg;
  stubs_chkr_set_right (&(ne->n_name), sizeof (char *), CHKR_RW);
  stubs_chkr_set_right (&(ne->n_aliases), sizeof (char **), CHKR_RW);
  stubs_chkr_set_right (&(ne->n_addrtype), sizeof (int), CHKR_RW);
  stubs_chkr_set_right (&(ne->n_net), sizeof (unsigned long), CHKR_RW);
  len = strlen (ne->n_name);
  stubs_chkr_set_right (ne->n_name, len + 1, CHKR_RW);
  for (arg = ne->n_aliases; *arg; arg++)
    {
      len = strlen (*arg);
      stubs_chkr_set_right (*arg, len + 1, CHKR_RW);
    }
  stubs_chkr_set_right (ne->n_aliases, (arg + 1) - ne->n_aliases, CHKR_RW);
}

void
stubs_chkr_set_right_protoent (struct protoent *pe)
{
  int len;
  char **arg;
  stubs_chkr_set_right (&(pe->p_name), sizeof (char *), CHKR_RW);
  stubs_chkr_set_right (&(pe->p_aliases), sizeof (char **), CHKR_RW);
  stubs_chkr_set_right (&(pe->p_proto), sizeof (int), CHKR_RW);
  len = strlen (pe->p_name);
  stubs_chkr_set_right (pe->p_name, len + 1, CHKR_RW);
  for (arg = pe->p_aliases; *arg; arg++)
    {
      len = strlen (*arg);
      stubs_chkr_set_right (*arg, len + 1, CHKR_RW);
    }
  stubs_chkr_set_right (pe->p_aliases, (arg + 1) - pe->p_aliases, CHKR_RW);
}

void
stubs_chkr_set_right_rpcent (struct rpcent *re)
{
  int len;
  char **arg;
  stubs_chkr_set_right (&(re->r_name), sizeof (char *), CHKR_RW);
  stubs_chkr_set_right (&(re->r_aliases), sizeof (char **), CHKR_RW);
  stubs_chkr_set_right (&(re->r_number), sizeof (int), CHKR_RW);
  len = strlen (re->r_name);
  stubs_chkr_set_right (re->r_name, len + 1, CHKR_RW);
  for (arg = re->r_aliases; *arg; arg++)
    {
      len = strlen (*arg);
      stubs_chkr_set_right (*arg, len + 1, CHKR_RW);
    }
  stubs_chkr_set_right (re->r_aliases, (arg + 1) - re->r_aliases, CHKR_RW);
}
#else
void stubs_chkr_set_right_hostent (struct hostent *he);
void stubs_chkr_set_right_servent (struct servent *se);
void stubs_chkr_set_right_netent (struct netent *ne);
void stubs_chkr_set_right_protoent (struct protoent *pe);
void stubs_chkr_set_right_rpcent (struct rpcent *re);
#endif

/* compiled from: . */
#ifdef HAVE_endhostent
/* From `/usr/include/netdb.h:135'.  */
void
chkr$endhostent (void)
{
#if USE_BI_JUMP
  __builtin_jump (endhostent);
#else
  endhostent ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_endhostent */

#ifdef HAVE_endnetent
/* From `/usr/include/netdb.h:136'.  */
void
chkr$endnetent (void)
{
#if USE_BI_JUMP
  __builtin_jump (endnetent);
#else
  endnetent ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_endnetent */

#ifdef HAVE_endprotoent
/* From `/usr/include/netdb.h:137'.  */
void
chkr$endprotoent (void)
{
#if USE_BI_JUMP
  __builtin_jump (endprotoent);
#else
  endprotoent ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_endprotoent */

#ifdef HAVE_endservent
/* From `/usr/include/netdb.h:138'.  */
void
chkr$endservent (void)
{
#if USE_BI_JUMP
  __builtin_jump (endservent);
#else
  endservent ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_endservent */

#ifdef HAVE_endrpcent
/* From `/usr/include/netdb.h:139'.  */
void
chkr$endrpcent (void)
{
#if USE_BI_JUMP
  __builtin_jump (endrpcent);
#else
  endrpcent ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_endrpcent */

#ifdef HAVE_gethostbyaddr
/* From `/usr/include/netdb.h:140'.  */
struct hostent *
chkr$gethostbyaddr (const char *addr, int len, int type)
{
  struct hostent *res;
  if (len > 0)
    stubs_chkr_check_addr (addr, len, CHKR_RO, "addr");
  res = gethostbyaddr (addr, len, type);
  if (res)
    stubs_chkr_set_right_hostent (res);
  return res;
}
#endif /* HAVE_gethostbyaddr */

#ifdef HAVE_gethostbyname
/* From `/usr/include/netdb.h:141'.  */
struct hostent *
chkr$gethostbyname (const char *name)
{
  struct hostent *res;
  stubs_chkr_check_str (name, CHKR_RO, "name");
  res = gethostbyname (name);
  if (res)
    stubs_chkr_set_right_hostent (res);
  return res;
}
#endif /* HAVE_gethostbyname */

#ifdef HAVE_gethostent
/* From `/usr/include/netdb.h:142'.  */
struct hostent *
chkr$gethostent (void)
{
  struct hostent * res;
  res = gethostent ();
  if (res)
    stubs_chkr_set_right_hostent (res);
  return res;
}
#endif /* HAVE_gethostent */

#ifdef HAVE_getnetbyaddr
/* From `/usr/include/netdb.h:143'.  */
struct netent *
chkr$getnetbyaddr (long int net, int type)
{
  struct netent * res;
  res = getnetbyaddr (net, type);
  if (res)
    stubs_chkr_set_right_netent (res);
  return res;
}
#endif /* HAVE_getnetbyaddr */

#ifdef HAVE_getnetbyname
/* From `/usr/include/netdb.h:144'.  */
struct netent *
chkr$getnetbyname (const char *name)
{
  struct netent * res;
  
  stubs_chkr_check_str (name, CHKR_RO, "name");
  res = getnetbyname (name);
  if (res)
    stubs_chkr_set_right_netent (res);
  return res;
}
#endif /* HAVE_getnetbyname */

#ifdef HAVE_getnetent
/* From `/usr/include/netdb.h:145'.  */
struct netent *
chkr$getnetent (void)
{
  struct netent * res;
  
  res = getnetent ();
  if (res)
    stubs_chkr_set_right_netent (res);
  return res;
}
#endif /* HAVE_getnetent */

#ifdef HAVE_getprotobyname
/* From `/usr/include/netdb.h:146'.  */
struct protoent *
chkr$getprotobyname (const char *name)
{
  struct protoent * res;

  stubs_chkr_check_str (name, CHKR_RO, "name");
  res = getprotobyname (name);
  if (res)
    stubs_chkr_set_right_protoent (res);
  return res;
}
#endif /* HAVE_getprotobyname */

#ifdef HAVE_getprotobynumber
/* From `/usr/include/netdb.h:147'.  */
struct protoent *
chkr$getprotobynumber (int proto)
{
  struct protoent * res;

  res = getprotobynumber (proto);
  if (res)
    stubs_chkr_set_right_protoent (res);
  return res;
}
#endif /* HAVE_getprotobynumber */

#ifdef HAVE_getprotoent
/* From `/usr/include/netdb.h:148'.  */
struct protoent *
chkr$getprotoent (void)
{
  struct protoent * res;

  res = getprotoent ();
  if (res)
    stubs_chkr_set_right_protoent (res);
  return res;
}
#endif /* HAVE_getprotoent */

#ifdef HAVE_getservbyname
/* From `/usr/include/netdb.h:149'.  */
struct servent *
chkr$getservbyname (const char *name, const char *proto)
{
  struct servent *res;

  stubs_chkr_check_str (name, CHKR_RO, "name");
  stubs_chkr_check_str (proto, CHKR_RO, "proto");
  res = getservbyname (name, proto);
  if (res)
    stubs_chkr_set_right_servent (res);
  return res;
}
#endif /* HAVE_getservbyname */

#ifdef HAVE_getservbyport
/* From `/usr/include/netdb.h:150'.  */
struct servent *
chkr$getservbyport (int port, const char *proto)
{
  struct servent *res;

  stubs_chkr_check_str (proto, CHKR_RO, "proto");
  res = getservbyport (port, proto);
  if (res)
    stubs_chkr_set_right_servent (res);
  return res;
}
#endif /* HAVE_getservbyport */

#ifdef HAVE_getservent
/* From `/usr/include/netdb.h:151'.  */
struct servent *
chkr$getservent (void)
{
  struct servent * res;

  res = getservent ();
  if (res)
    stubs_chkr_set_right_servent (res);
  return res;
}
#endif /* HAVE_getservent */

#ifdef HAVE_getrpcent
/* From `/usr/include/netdb.h:152'.  */
struct rpcent *
chkr$getrpcent (void)
{
  struct rpcent * res;

  res = getrpcent ();
  if (res)
    stubs_chkr_set_right_rpcent (res);
  return res;
}
#endif /* HAVE_getrpcent */

#ifdef HAVE_getrpcbyname
/* From `/usr/include/netdb.h:153'.  */
struct rpcent *
chkr$getrpcbyname (const char *name)
{
  struct rpcent *res;

  stubs_chkr_check_str (name, CHKR_RO, "name");
  res = getrpcbyname (name);
  if (res)
    stubs_chkr_set_right_rpcent (res);
  return res;
}
#endif /* HAVE_getrpcbyname */

#ifdef HAVE_getrpcbynumber
/* From `/usr/include/netdb.h:154'.  */
struct rpcent *
chkr$getrpcbynumber (int num)
{
  struct rpcent * res;

  res = getrpcbynumber (num);
  if (res)
    stubs_chkr_set_right_rpcent (res);
  return res;
}
#endif /* HAVE_getrpcbynumber */

#ifdef HAVE_herror
/* From `/usr/include/netdb.h:155'.  */
void
chkr$herror (const char *s)
{
  stubs_chkr_check_str (s, CHKR_RO, "s");
#if USE_BI_JUMP
  __builtin_jump (herror);
#else
  herror (s);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_herror */

#ifdef HAVE_sethostent
/* From `/usr/include/netdb.h:156'.  */
void
chkr$sethostent (int num)
{
#if USE_BI_JUMP
  __builtin_jump (sethostent);
#else
  sethostent (num);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sethostent */

#ifdef HAVE_setnetent
/* From `/usr/include/netdb.h:158'.  */
void
chkr$setnetent (int num)
{
#if USE_BI_JUMP
  __builtin_jump (setnetent);
#else
  setnetent (num);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setnetent */

#ifdef HAVE_setprotoent
/* From `/usr/include/netdb.h:159'.  */
void
chkr$setprotoent (int num)
{
#if USE_BI_JUMP
  __builtin_jump (setprotoent);
#else
  setprotoent (num);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setprotoent */

#ifdef HAVE_setservent
/* From `/usr/include/netdb.h:160'.  */
void
chkr$setservent (int num)
{
#if USE_BI_JUMP
  __builtin_jump (setservent);
#else
  setservent (num);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setservent */

#ifdef HAVE_setrpcent
/* From `/usr/include/netdb.h:161'.  */
void
chkr$setrpcent (int num)
{
#if USE_BI_JUMP
  __builtin_jump (setrpcent);
#else
  setrpcent (num);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setrpcent */

#endif /* HAVE_NETDB_H */

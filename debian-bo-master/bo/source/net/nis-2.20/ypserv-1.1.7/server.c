/*
** server.c - YP server routines.
**
** Copyright (c) 1996, 1997 Thorsten Kukuk
** Copyright (c) 1993 Signum Support AB, Sweden
**
** This file is part of the NYS YP Server.
**
** The NYS YP Server is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS YP Server is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** General Public License for more details.
**
** You should have received a copy of the GNU General Public
** License along with the NYS YP Server; see the file COPYING.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Peter Eriksson <pen@signum.se>
**         Thorsten Kukuk <kukuk@uni-paderborn.de>
*/

#ifndef LINT
static const char rcsid[] = "$Id: server.c,v 1.14 1997/04/12 16:16:42 kukuk Exp $";
#endif

#if defined(HAVE_CONFIG_H)
#include "config.h"
#endif

#include "system.h"

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <dirent.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include "yp.h"
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <syslog.h>
#include <sys/param.h>
#include <errno.h>
#include <stdarg.h>

#include "ypserv.h"
#include "yp_msg.h"
#include "yp_db.h"

int children = 0;
int forked = 0;
int xfr_check_port = 1;

#if !defined(HAVE_STRDUP)
#include <compat/strdup.c>
#endif

#if !defined(HAVE_GETHOSTNAME)
#include <sys/utsname.h>

int
gethostname (char *buf, int buflen)
{
  struct utsname nb;

  if (uname (&nb) < 0)
    return -1;

  if (strlen (nb.nodename) > buflen - 1)
    {
      strncpy (buf, nb.nodename, buflen);
      buf[buflen - 1] = '\0';
    }
  else
    strcpy (buf, nb.nodename);

  return 0;
}

#endif

void *
ypproc_null_2_svc (void *dummy, struct svc_req *rqstp)
{
  static char *result;
  static char rval = 0;
  struct sockaddr_in *rqhost;

  if (debug_flag)
    {
      rqhost = svc_getcaller (rqstp->rq_xprt);
      yp_msg ("ypproc_null() [From: %s:%d]\n",
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));
    }

  if (!is_valid_host (rqstp, NULL, NULL))
    return NULL;

  result = &rval;
  return (void *) &result;
}

bool_t *
ypproc_domain_2_svc (domainname * name, struct svc_req *rqstp)
{
  static bool_t result;
  struct sockaddr_in *rqhost;

  if (debug_flag)
    {
      rqhost = svc_getcaller (rqstp->rq_xprt);
      yp_msg ("ypproc_domain(\"%s\") [From: %s:%d]\n",
	      *name,
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));
    }

  if (!is_valid_host (rqstp, NULL, NULL))
    {
      if (debug_flag)
	yp_msg ("\t-> Ignored (not a valid source host)\n");
      result = FALSE;
    }
  else
    {
      if (is_valid_domain (*name))
	result = TRUE;
      else
	result = FALSE;
    }

  if (debug_flag)
    yp_msg ("\t-> %s.\n",
	    (result == TRUE ? "Ok" : "Not served by us"));

  return &result;
}

bool_t *
ypproc_domain_nonack_2_svc (domainname * name,
			    struct svc_req * rqstp)
{
  static bool_t result;
  struct sockaddr_in *rqhost;

  if (debug_flag)
    {
      rqhost = svc_getcaller (rqstp->rq_xprt);
      yp_msg ("ypproc_domain_nonack(\"%s\") [From: %s:%d]\n",
	      *name,
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));
    }

  if (!is_valid_host (rqstp, NULL, NULL))
    {
      if (debug_flag)
	yp_msg ("\t-> Ignored (not a valid source host)\n");
      return NULL;
    }
  else if (!is_valid_domain (*name))
    {
      if (debug_flag)
	yp_msg ("\t-> Ignored (not a valid domain)\n");

      /* Bail out and don't return any RPC value */
      return NULL;
    }

  if (debug_flag)
    yp_msg ("\t-> OK.\n");

  result = TRUE;
  return &result;
}

/*
** Get the DateTimeModified value for a certain map database
*/
static unsigned long
get_dtm (const char *domain, const char *map)
{
  struct stat sbuf;
  char buf[strlen (domain) + strlen (map) + 3];

  sprintf (buf, "%s/%s", domain, map);

  if (stat (buf, &sbuf) < 0)
    return 0;
  else
    return (unsigned long) sbuf.st_mtime;
}

/*
** YP function "MATCH" implementation
*/
ypresp_val *
ypproc_match_2_svc (ypreq_key * key, struct svc_req *rqstp)
{
  static ypresp_val result;
  struct sockaddr_in *rqhost;
  int valid;
  int mangle_field = 0;

  if (debug_flag)
    {
      rqhost = svc_getcaller (rqstp->rq_xprt);

      yp_msg ("ypproc_match(): [From: %s:%d]\n",
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));

      yp_msg ("\t\tdomainname = \"%s\"\n",
	      key->domain);
      yp_msg ("\t\tmapname = \"%s\"\n",
	      key->map);
      yp_msg ("\t\tkeydat = \"%.*s\"\n",
	      (int) key->key.keydat_len,
	      key->key.keydat_val);
    }

  if ((valid = is_valid_host (rqstp, key->map, key->domain)) < 1)
    {
      if (valid == 0)
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (not a valid source host)\n");

	  result.stat = YP_YPERR;
	}
      else
	{
	  if (debug_flag)
	    yp_msg ("\t-> Error: not a valid domain\n");
	  result.stat = YP_NODOM;
	}
      return &result;
    }
  else if (valid > 200)
    mangle_field = valid - 200;

  result.val.valdat_len = 0;
  if (result.val.valdat_val)
    {
      free (result.val.valdat_val);
      result.val.valdat_val = NULL;
    }

  if (key->domain[0] == '\0' || key->map[0] == '\0' ||
      key->key.keydat_len == 0 || key->key.keydat_val[0] == '\0')
    result.stat = YP_BADARGS;
  else
    {
      datum rdat, qdat;

      DB_FILE dbp = ypdb_open (key->domain, key->map);
      if (dbp == NULL)
	result.stat = YP_NOMAP;
      else
	{
	  qdat.dsize = key->key.keydat_len;
	  qdat.dptr = key->key.keydat_val;

	  result.stat = ypdb_read (dbp, &qdat, NULL, &rdat,
				 F_ALL, mangle_field);

	  if (result.stat == YP_TRUE)
	    {
	      result.val.valdat_len = rdat.dsize;
	      result.val.valdat_val = rdat.dptr;
	    }

	  ypdb_close (dbp);
	}
    }

  if (debug_flag)
    {
      if (result.stat == YP_TRUE)
	yp_msg ("\t-> Value = \"%.*s\"\n",
		(int) result.val.valdat_len,
		result.val.valdat_val);
      else
	yp_msg ("\t-> Error #%d\n", result.stat);
    }


  /*
  ** Do the jive thing if we didn't find the host in the YP map
  ** and we have enabled the magic DNS lookup stuff.
  **
  ** DNS lookups are handled in a subprocess so that the server
  ** doesn't block while waiting for requests to complete.
  */
  if (result.stat != YP_TRUE && strstr (key->map, "hosts") && dns_flag)
    {
      struct hostent *he;
      if (children < MAX_CHILDREN && fork ())
	{
	  children++;
	  return NULL;
	}
      else
	forked++;

      key->key.keydat_val[key->key.keydat_len] = '\0';


      if (debug_flag)
	yp_msg ("Doing DNS lookup of %s\n", key->key.keydat_val);

      if (strcmp (key->map, "hosts.byname") == 0)
	he = res_gethostbyname (key->key.keydat_val);
      else if (strcmp (key->map, "hosts.byaddr") == 0)
	{
	  long a;

	  a = inet_addr (key->key.keydat_val);
	  he = res_gethostbyaddr ((const char *) &a, sizeof (long), AF_INET);
	}
      else
	he = NULL;

      if (he)
	{
	  int len, i, j;
	  char *cp;

	  if (debug_flag)
	    yp_msg ("\t-> OK (%s/%s)\n",
		    he->h_name,
		    inet_ntoa (*(struct in_addr *) he->h_addr));

	  for (j = 0; he->h_addr_list[j]; j++)
	    ;

	  len = 16;		/* Numerical IP address */
	  len += strlen (he->h_name) + 1;
	  for (i = 0; he->h_aliases[i]; i++)
	    len += strlen (he->h_aliases[i]) + 1;

	  len *= j;
	  if ((cp = malloc (len + 1)) == NULL)
	    {
	      yp_msg ("ERROR: could not allocate enough memory! [%s|%d]\n", __FILE__, __LINE__);
	      result.stat = YP_YPERR;
	      return &result;
	    }

	  strcpy (cp, inet_ntoa (*(struct in_addr *) he->h_addr_list[0]));
	  strcat (cp, "	");
	  strcat (cp, he->h_name);

	  for (i = 0; he->h_aliases[i]; i++)
	    {
	      strcat (cp, " ");
	      strcat (cp, he->h_aliases[i]);
	    }

	  for (j = 1; he->h_addr_list[j]; j++)
	    {
	      strcat (cp, "\n");
	      strcat (cp, inet_ntoa (*(struct in_addr *) he->h_addr_list[j]));
	      strcat (cp, "	");
	      strcat (cp, he->h_name);

	      for (i = 0; he->h_aliases[i]; i++)
		{
		  strcat (cp, " ");
		  strcat (cp, he->h_aliases[i]);
		}
	    }
	  strcat (cp, "\n");
	  len = strlen (cp);

	  result.val.valdat_len = len;
	  result.val.valdat_val = cp;
	  result.stat = YP_TRUE;
	}
      else
	{
	  if (debug_flag)
	    yp_msg ("\t-> Not Found\n");

	  result.stat = YP_NOKEY;
	}
    }

  return &result;
}

ypresp_key_val *
ypproc_first_2_svc (ypreq_nokey * key, struct svc_req * rqstp)
{
  static ypresp_key_val result;
  struct sockaddr_in *rqhost;
  int valid;
  int mangle_field = 0;

  if (debug_flag)
    {
      rqhost = svc_getcaller (rqstp->rq_xprt);
      yp_msg ("ypproc_first(): [From: %s:%d]\n",
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));

      yp_msg ("\tdomainname = \"%s\"\n", key->domain);
      yp_msg ("\tmapname = \"%s\"\n", key->map);
#if 0
      yp_msg ("\tkeydat = \"%.*s\"\n",
	      (int) key->key.keydat_len,
	      key->key.keydat_val);
#endif
    }

  if ((valid = is_valid_host (rqstp, key->map, key->domain)) < 1)
    {
      if (valid == 0)
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (not a valid source host)\n");
	  result.stat = YP_YPERR;
	}
      else
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (not a valid domain)\n");
	  result.stat = YP_NODOM;
	}
      return &result;
    }
  else if (valid > 200)
    mangle_field = valid - 200;

  result.key.keydat_len = 0;
  if (result.key.keydat_val)
    {
      free (result.key.keydat_val);
      result.key.keydat_val = NULL;
    }

  result.val.valdat_len = 0;
  if (result.val.valdat_val)
    {
      free (result.val.valdat_val);
      result.val.valdat_val = NULL;
    }

  if (key->map[0] == '\0' || key->domain[0] == '\0')
    result.stat = YP_BADARGS;
  else
    {
      datum dkey, dval;

      DB_FILE dbp = ypdb_open (key->domain, key->map);
      if (dbp == NULL)
	result.stat = YP_NOMAP;
      else
	{
	  result.stat = ypdb_read (dbp, NULL, &dkey, &dval, 0, mangle_field);

	  if (result.stat == YP_TRUE)
	    {
	      result.key.keydat_len = dkey.dsize;
	      result.key.keydat_val = dkey.dptr;

	      result.val.valdat_len = dval.dsize;
	      result.val.valdat_val = dval.dptr;
	    }

	  ypdb_close (dbp);
	}
    }

  if (debug_flag)
    {
      if (result.stat == YP_TRUE)
	yp_msg ("\t-> Key = \"%.*s\", Value = \"%.*s\"\n",
		(int) result.key.keydat_len,
		result.key.keydat_val,
		(int) result.val.valdat_len,
		result.val.valdat_val);

      else if (result.stat == YP_NOMORE)
	yp_msg ("\t-> No more entry's\n");
      else
	yp_msg ("\t-> Error #%d\n", result.stat);
    }

  return &result;
}

ypresp_key_val *
ypproc_next_2_svc (ypreq_key * key, struct svc_req * rqstp)
{
  static ypresp_key_val result;
  struct sockaddr_in *rqhost;
  int valid;
  int mangle_field = 0;

  if (debug_flag)
    {
      rqhost = svc_getcaller (rqstp->rq_xprt);

      yp_msg ("ypproc_next(): [From: %s:%d]\n",
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));

      yp_msg ("\tdomainname = \"%s\"\n", key->domain);
      yp_msg ("\tmapname = \"%s\"\n", key->map);
      yp_msg ("\tkeydat = \"%.*s\"\n",
	      (int) key->key.keydat_len,
	      key->key.keydat_val);
    }

  if ((valid = is_valid_host (rqstp, key->map, key->domain)) < 1)
    {
      if (valid == 0)
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (not a valid source host)\n");
	  result.stat = YP_YPERR;
	}
      else
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (not a valid domain)\n");
	  result.stat = YP_NODOM;
	}
      return &result;
    }
  else if (valid > 200)
    mangle_field = valid - 200;

  result.key.keydat_len = 0;
  if (result.key.keydat_val)
    {
      free (result.key.keydat_val);
      result.key.keydat_val = NULL;
    }

  result.val.valdat_len = 0;
  if (result.val.valdat_val)
    {
      free (result.val.valdat_val);
      result.val.valdat_val = NULL;
    }

  if (key->map[0] == '\0' || key->domain[0] == '\0')
    result.stat = YP_BADARGS;
  else if (!is_valid_domain (key->domain))
    result.stat = YP_NODOM;
  else
    {
      datum dkey, dval, okey;

      DB_FILE dbp = ypdb_open (key->domain, key->map);
      if (dbp == NULL)
	result.stat = YP_NOMAP;
      else
	{
	  dkey.dsize = key->key.keydat_len;
	  dkey.dptr = key->key.keydat_val;

	  result.stat = ypdb_read (dbp, &dkey, &okey, &dval,
				 F_NEXT, mangle_field);
	  /*
	  ** If the key is empty, get the next one, until we
	  ** get a key which is not empty
	  */
	  while ((okey.dsize == 0) && (result.stat == YP_TRUE))
	    {
	      dkey.dsize = okey.dsize;
	      dkey.dptr = okey.dptr;

	      result.stat = ypdb_read (dbp, &dkey, &okey, &dval,
				     F_NEXT, mangle_field);
	    }

	  if (result.stat == YP_TRUE)
	    {
	      result.key.keydat_len = okey.dsize;
	      result.key.keydat_val = okey.dptr;

	      result.val.valdat_len = dval.dsize;
	      result.val.valdat_val = dval.dptr;
	    }
	  ypdb_close (dbp);
	}
    }

  if (debug_flag)
    {
      if (result.stat == YP_TRUE)
	yp_msg ("\t-> Key = \"%.*s\", Value = \"%.*s\"\n",
		(int) result.key.keydat_len,
		result.key.keydat_val,
		(int) result.val.valdat_len,
		result.val.valdat_val);
      else if (result.stat == YP_NOMORE)
	yp_msg ("\t-> No more entry's\n");
      else
	yp_msg ("\t-> Error #%d\n", result.stat);
    }

  return &result;
}

static inline void
print_ypmap_parms (const struct ypmap_parms *pp)
{
  yp_msg ("\t\tdomain   = \"%s\"\n", pp->domain);
  yp_msg ("\t\tmap      = \"%s\"\n", pp->map);
  yp_msg ("\t\tordernum = %u\n", pp->ordernum);
  yp_msg ("\t\tpeer     = \"%s\"\n", pp->peer);
}

ypresp_xfr *
ypproc_xfr_2_svc (ypreq_xfr *xfr, struct svc_req *rqstp)
{
  static ypresp_xfr result;
  struct sockaddr_in *rqhost;
  DB_FILE dbp;

  rqhost = svc_getcaller (rqstp->rq_xprt);

  if (debug_flag)
    {
      yp_msg ("ypproc_xfr_2_svc(): [From: %s:%d]\n\tmap_parms:\n",
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));

      print_ypmap_parms (&xfr->map_parms);
      yp_msg ("\t\ttransid = %u\n", xfr->transid);
      yp_msg ("\t\tprog = %u\n", xfr->prog);
      yp_msg ("\t\tport = %u\n", xfr->port);
    }

  result.transid = xfr->transid;

  if (!is_valid_host (rqstp, NULL, NULL))
    {
      if (debug_flag)
	yp_msg ("\t-> Ignored (not a valid source host)\n");

      result.xfrstat = YPXFR_REFUSED;
      return &result;
    }

  if (!is_valid_domain (xfr->map_parms.domain))
    {
      if (debug_flag)
	yp_msg ("\t-> Ignored (not a valid domain)\n");

      result.xfrstat = YPXFR_NODOM;
      return &result;
    }

  if (xfr->map_parms.map == NULL || strchr (xfr->map_parms.map ,'/'))
    {
      if (debug_flag)
	yp_msg ("\t-> Ignored (map contains \"/\"!)\n");

      result.xfrstat = YPXFR_REFUSED;
      return &result;
    }

  if (xfr_check_port)
    {
      if(ntohs(rqhost->sin_port) >= IPPORT_RESERVED)
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (no reserved port!)\n");

	  result.xfrstat = YPXFR_REFUSED;
	  return &result;
	}
    }

  /* If we have the map, check, if the master name is the same as in
     the ypreq_xfr struct If we doesn't have the map, trust the host */
  dbp = ypdb_open(xfr->map_parms.domain, xfr->map_parms.map);
  if (dbp != NULL)
    {
      datum key;

      key.dsize = sizeof("YP_MASTER_NAME")-1;
      key.dptr = "YP_MASTER_NAME";
      if(ypdb_exists(dbp, key))
	{
	  datum val = ypdb_fetch (dbp, key);

	  if (val.dsize != strlen (xfr->map_parms.peer) ||
	      strncmp (val.dptr, xfr->map_parms.peer, val.dsize) != 0)
	    {
	      if (debug_flag)
		yp_msg ("\t->Ignored (%s is not the master!)\n",
			xfr->map_parms.peer);

	      result.xfrstat = YPXFR_NODOM;
	      return &result;
	    }
	}
      else
	{
	  /* If we do not have a YP_MASTER_NAME key, we don't have a
	     master/slave NIS system */
	  if (debug_flag)
	    yp_msg ("\t->Ignored (no YP_MASTER_NAME key in local map)\n");

	  result.xfrstat = YPXFR_REFUSED;
	  return &result;
	}
      ypdb_close(dbp);
    }

  switch (fork ())
    {
    case 0:
      {
	char ypxfr_command[strlen (YPBINDIR) + 8];
	char g[30], t[30], p[30];

	sprintf (ypxfr_command, "%s/ypxfr", YPBINDIR);
	sprintf (t, "%u", xfr->transid);
	sprintf (g, "%u", xfr->prog);
	sprintf (p, "%u", xfr->port);
	if (debug_flag)
	  execl (ypxfr_command, "ypxfr", "--debug", "-d", xfr->map_parms.domain,
		 "-h", xfr->map_parms.peer, "-C", t, g,
		 inet_ntoa (rqhost->sin_addr), p, xfr->map_parms.map, NULL);
	else
	  execl (ypxfr_command, "ypxfr", "-d", xfr->map_parms.domain, "-h",
		 xfr->map_parms.peer, "-C", t, g,
		 inet_ntoa (rqhost->sin_addr), p, xfr->map_parms.map, NULL);

	yp_msg ("ypxfr execl(): %s", strerror (errno));
	exit (0);
      }
    case -1:
      yp_msg ("fork(): %s", strerror (errno));
      result.xfrstat = YPXFR_XFRERR;
    default:
      result.xfrstat = YPXFR_SUCC;
      break;
    }

  return &result;
}


/*
** We don't need to do anything, since we doesn't cache anything.
** Maybe we should ?
*/
void *
ypproc_clear_2_svc (void *dummy, struct svc_req *rqstp)
{
  static char *result;
  static char rval = 0;

  if (debug_flag)
    {
      struct sockaddr_in *rqhost;

      rqhost = svc_getcaller (rqstp->rq_xprt);
      yp_msg ("ypproc_clear_2_svc() [From: %s:%d]\n",
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));
    }

  if (!is_valid_host (rqstp, NULL, NULL))
    {
      if (debug_flag)
	yp_msg ("\t-> Ignored (not a valid source host)\n");

      return NULL;
    }

  result = &rval;
  return (void *) &result;
}

/*
** We need the struct for giving ypall_encode the DB_FILE
** handle and the mangle field.
*/

typedef struct ypall_data
{
  DB_FILE dbm;
  int field;
}
 *ypall_data_t;

static int
ypall_close (void *data)
{
  if (debug_flag && data == NULL)
    {
      yp_msg ("ypall_close() called with NULL pointer.\n");
      return 0;
    }

  ypdb_close (((ypall_data_t) data)->dbm);
  free (data);
  return 0;
}

static int
ypall_encode (ypresp_key_val * val, void *data)
{
  datum dkey, dval, okey;

  dkey.dptr = val->key.keydat_val;
  dkey.dsize = val->key.keydat_len;

  val->stat = ypdb_read (((ypall_data_t) data)->dbm, &dkey, &okey, &dval,
		       F_NEXT, ((ypall_data_t) data)->field);

  /*
  ** If key is empty, search the next one which is not empty
  */
  while ((okey.dsize == 0) && (val->stat == YP_TRUE))
    {
      dkey.dptr = okey.dptr;
      dkey.dsize = okey.dsize;

      val->stat = ypdb_read (((ypall_data_t) data)->dbm, &dkey, &okey, &dval,
			   F_NEXT, ((ypall_data_t) data)->field);
    }

  if (val->stat == YP_TRUE)
    {
      val->key.keydat_val = okey.dptr;
      val->key.keydat_len = okey.dsize;

      val->val.valdat_val = dval.dptr;
      val->val.valdat_len = dval.dsize;
    }

  return val->stat;
}

ypresp_all *
ypproc_all_2_svc (ypreq_nokey * nokey, struct svc_req * rqstp)
{
  static ypresp_all result;
  extern xdr_ypall_cb_t xdr_ypall_cb;
  int valid;
  int mangle_field = 0;

  if (debug_flag)
    {
      struct sockaddr_in *rqhost;

      rqhost = svc_getcaller (rqstp->rq_xprt);
      yp_msg ("ypproc_all_2_svc(): [From: %s:%d]\n",
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));

      yp_msg ("\t\tdomain = \"%s\"\n", nokey->domain);
      yp_msg ("\t\tmap = \"%s\"\n", nokey->map);
    }


  if ((valid = is_valid_host (rqstp, nokey->map, nokey->domain)) < 1)
    {
      if (valid == 0)
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (not a valid source host)\n");
	  result.ypresp_all_u.val.stat = YP_YPERR;
	}
      else
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (not a valid domain)\n");
	  result.ypresp_all_u.val.stat = YP_NODOM;
	}
      return &result;
    }
  if (valid > 200)
    mangle_field = valid - 200;

  if (children < MAX_CHILDREN && fork ())
    {
      children++;
      return NULL;
    }
  else
    forked++;

  xdr_ypall_cb.u.encode = NULL;
  xdr_ypall_cb.u.close = NULL;
  xdr_ypall_cb.data = NULL;

  result.more = TRUE;

  if (nokey->map[0] == '\0' || nokey->domain[0] == '\0')
    result.ypresp_all_u.val.stat = YP_BADARGS;
  else
    {
      datum dkey, dval;
      ypall_data_t data;

      if ((data = malloc (sizeof (struct ypall_data))) == NULL)
	{
	  yp_msg ("ERROR: could not allocate enough memory! [%s|%d]\n", __FILE__, __LINE__);
	  result.ypresp_all_u.val.stat = YP_YPERR;
	  return &result;
	}
      data->dbm = ypdb_open (nokey->domain, nokey->map);

      if (data->dbm == NULL)
	result.ypresp_all_u.val.stat = YP_NOMAP;
      else
	{
	  result.ypresp_all_u.val.stat =
	    ypdb_read (data->dbm, NULL, &dkey, &dval, 0, mangle_field);

	  if (result.ypresp_all_u.val.stat == YP_TRUE)
	    {
	      result.ypresp_all_u.val.key.keydat_len = dkey.dsize;
	      result.ypresp_all_u.val.key.keydat_val = dkey.dptr;

	      result.ypresp_all_u.val.val.valdat_len = dval.dsize;
	      result.ypresp_all_u.val.val.valdat_val = dval.dptr;

	      data->field = mangle_field;

	      xdr_ypall_cb.u.encode = ypall_encode;
	      xdr_ypall_cb.u.close = ypall_close;
	      xdr_ypall_cb.data = (void *) data;

	      if (debug_flag)
		yp_msg ("\t -> First value returned.\n");

	      return &result;
	    }

	  ypdb_close (data->dbm);
	}
      free (data);
    }

  if (debug_flag)
    yp_msg ("\t -> Error should have been returned.\n");

  return &result;
}

ypresp_master *
ypproc_master_2_svc (ypreq_nokey * nokey, struct svc_req * rqstp)
{
  static ypresp_master result;
  char hostbuf[MAXHOSTNAMELEN + 2];
  int valid;

  if (debug_flag)
    {
      struct sockaddr_in *rqhost;

      rqhost = svc_getcaller (rqstp->rq_xprt);
      yp_msg ("ypproc_master_2_svc(): [From: %s:%d]\n",
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));

      yp_msg ("\t\tdomain = \"%s\"\n", nokey->domain);
      yp_msg ("\t\tmap = \"%s\"\n", nokey->map);
    }

  if ((valid = is_valid_host (rqstp, nokey->map, nokey->domain)) < 1)
    {
      if (valid == 0)
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (not a valid source host)\n");
	  result.stat = YP_YPERR;
	}
      else
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (not a domain)\n");
	  result.stat = YP_NODOM;
	}
      result.peer = strdup ("");
      return &result;
    }

  if (result.peer)
    {
      free (result.peer);
      result.peer = NULL;
    }

  if (nokey->map[0] == '\0')
    result.stat = YP_BADARGS;
  else
    {
      DB_FILE dbp = ypdb_open (nokey->domain, nokey->map);
      if (dbp == NULL)
	result.stat = YP_NOMAP;
      else
	{
	  datum key, val;

	  key.dsize = sizeof ("YP_MASTER_NAME") - 1;
	  key.dptr = "YP_MASTER_NAME";

	  val = ypdb_fetch (dbp, key);
	  if (val.dptr == NULL)
	    {
	      /* No YP_MASTER_NAME record in map? Assume we are Master */
	      gethostname (hostbuf, sizeof (hostbuf) - 1);
	      if ((result.peer = strdup (hostbuf)) == NULL)
		result.stat = YP_YPERR;
	      else
		result.stat = YP_TRUE;
	    }
	  else
	    {
	      int i;

	      /* put the eof string mark at the end of the string */
	      for (i = 0; i < val.dsize; i++)
		hostbuf[i] = val.dptr[i];
	      hostbuf[val.dsize] = '\0';
	      free (val.dptr);

	      if ((result.peer = strdup (hostbuf)) == NULL)
		result.stat = YP_YPERR;
	      else
		result.stat = YP_TRUE;
	    }

	  ypdb_close (dbp);
	}
    }

  if (result.peer == NULL)
    result.peer = strdup ("");

  if (debug_flag)
    yp_msg ("\t-> Peer = \"%s\"\n", result.peer);

  return &result;
}

ypresp_order *
ypproc_order_2_svc (ypreq_nokey * nokey, struct svc_req * rqstp)
{
  static ypresp_order result;
  int valid;

  if (debug_flag)
    {
      struct sockaddr_in *rqhost;

      rqhost = svc_getcaller (rqstp->rq_xprt);

      yp_msg ("ypproc_order_2_svc(): [From: %s:%d]\n",
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));

      yp_msg ("\t\tdomain = \"%s\"\n", nokey->domain);
      yp_msg ("\t\tmap = \"%s\"\n", nokey->map);
    }

  if ((valid = is_valid_host (rqstp, nokey->map, nokey->domain)) < 1)
    {
      if (valid == 0)
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (not a valid source host)\n");
	  result.stat = YP_YPERR;
	}
      else
	{
	  if (debug_flag)
	    yp_msg ("\t-> Ignored (not a valid domain)\n");
	  result.stat = YP_NODOM;
	}
      return &result;
    }

  result.ordernum = 0;

  if (nokey->map[0] == '\0')
    result.stat = YP_BADARGS;
  else
    {
      DB_FILE dbp = ypdb_open (nokey->domain, nokey->map);
      if (dbp == NULL)
	result.stat = YP_NOMAP;
      else
	{
	  datum key, val;

	  key.dsize = sizeof ("YP_LAST_MODIFIED") - 1;
	  key.dptr = "YP_LAST_MODIFIED";

	  val = ypdb_fetch (dbp, key);
	  if (val.dptr == NULL)
	    {
	      /* No YP_LAST_MODIFIED record in map? Use DTM timestamp.. */
	      result.ordernum = get_dtm (nokey->domain, nokey->map);
	    }
	  else
	    {
	      char buf[val.dsize + 2];

	      memcpy (buf, val.dptr, val.dsize);
	      buf[val.dsize + 1] = '\0';
	      result.ordernum = atoi (buf);
	      free (val.dptr);
	    }

	  result.stat = YP_TRUE;
	  ypdb_close (dbp);
	}
    }

  if (debug_flag)
    yp_msg ("-> Order # %u\n", result.ordernum);

  return &result;
}

static void
free_maplist (ypmaplist * mlp)
{
  ypmaplist *next;

  while (mlp != NULL)
    {
      next = mlp->next;
      free (mlp->map);
      free (mlp);
      mlp = next;
    }
}

static int
add_maplist (ypmaplist ** mlhp, char *map)
{
  ypmaplist *mlp;

  if ((mlp = malloc (sizeof (*mlp))) == NULL)
    return -1;

  if ((mlp->map = strdup (map)) == NULL)
    {
      free (mlp);
      return -1;
    }

  mlp->next = *mlhp;
  *mlhp = mlp;

  return 0;
}


ypresp_maplist *
ypproc_maplist_2_svc (domainname * name, struct svc_req * rqstp)
{
  static ypresp_maplist result;

  if (debug_flag)
    {
      struct sockaddr_in *rqhost;

      rqhost = svc_getcaller (rqstp->rq_xprt);

      yp_msg ("ypproc_maplist_2_svc(): [From: %s:%d]\n",
	      inet_ntoa (rqhost->sin_addr),
	      ntohs (rqhost->sin_port));

      yp_msg ("\t\tdomain = \"%s\"\n", *name);
    }

  if (!is_valid_host (rqstp, NULL, NULL))
    {
      if (debug_flag)
	yp_msg ("\t-> Ignored (not a valid source host)\n");

      return NULL;
    }

  if (result.maps)
    free_maplist (result.maps);

  result.maps = NULL;

  if ((*name)[0] == '\0')
    result.stat = YP_BADARGS;
  else if (!is_valid_domain (*name))
    result.stat = YP_NODOM;
  else
    {
      DIR *dp;

      /* open domain directory */
      dp = opendir (*name);
      if (dp == NULL)
	{
	  if (debug_flag)
	    {
	      yp_msg ("%s: opendir: %s", progname, strerror (errno));
	    }

	  result.stat = YP_BADDB;
	}
      else
	{
	  struct dirent *dep;

	  while ((dep = readdir (dp)) != NULL)
	    {
	      /* ignore files . and .. */
	      if (strcmp (".", dep->d_name) == 0 ||
		  strcmp ("..", dep->d_name) == 0)
		continue;
	      if (add_maplist (&result.maps, dep->d_name) < 0)
		{
		  result.stat = YP_YPERR;
		  break;
		}
	    }
	  closedir (dp);
	  result.stat = YP_TRUE;
	}
    }

  if (debug_flag)
    {
      if (result.stat == YP_TRUE)
	{
	  ypmaplist *p;

	  p = result.maps;
	  yp_msg ("-> ");
	  while (p)
	    {
	      yp_msg ("%s", p->map);
	      if (p->next)
		{
		  yp_msg (", ");
		}
	      p = p->next;
	    }
	  putc ('\n', stderr);
	}
      else
	yp_msg ("\t-> Error #%d\n", result.stat);
    }

  return &result;
}

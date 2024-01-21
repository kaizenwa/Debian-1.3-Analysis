/* @(#)getrpcent.c	2.2 88/07/29 4.0 RPCSRC */
#if !defined(lint) && defined(SCCSIDS)
static  char sccsid[] = "@(#)getrpcent.c 1.9 87/08/11  Copyr 1984 Sun Micro";
#endif

/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <rpc/rpc.h>
#include <netdb.h>
#include <sys/socket.h>

#ifdef YP
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
#endif

#ifdef __STDC__
#include <stdlib.h>
#endif

/*
 * Internet version.
 */
static
struct rpcdata {
	FILE	*rpcf;
	char	*current;
	int	currentlen;
	int	stayopen;
#define	MAXALIASES	35
	char	*rpc_aliases[MAXALIASES];
	struct	rpcent rpc;
	char	line[BUFSIZ+1];
	char	*domain;
} *rpcdata;

#ifdef __linux__
#include <string.h>
#include <arpa/inet.h>
static	struct rpcent *interpret(char *, int);
static  struct rpcent *_fgetrpcent(void);
#else
static	struct rpcent *interpret();
static  struct rpcent *_fgetrpcent();
char	*inet_ntoa();
struct	hostent *gethostent();
static	char *index();
static char RPCDB[] = _PATH_RPC; /* Changed from "/etc/rpc" by roland@gnu */
#endif

#ifdef YP
static struct rpcent * _nis_getrpcent(int);
static struct rpcent * _nis_getrpcbynumber(int);
static int ypmode = 0;
static int inkeylen;
static char *inkey = NULL;
#endif

static struct rpcdata *
#ifdef __linux__
_rpcdata(void)
#else
_rpcdata()
#endif
{
	register struct rpcdata *d = rpcdata;

	if (d == 0) {
		d = (struct rpcdata *)calloc(1, sizeof (struct rpcdata));
		rpcdata = d;
	}
	return (d);
}

struct rpcent *
#ifdef __linux__
getrpcbynumber(int number)
#else
getrpcbynumber(number)
	register int number;
#endif
{
	register struct rpcdata *d = _rpcdata();
	register struct rpcent *p;

	if (d == 0)
		return (0);
	setrpcent(0);
	while (NULL != (p = getrpcent())) {
		if (p->r_number == number)
			break;
	}
	endrpcent();
#ifdef YP
	if(NULL == p)
      p = _nis_getrpcbynumber(number);
#endif
    return (p);
}

struct rpcent *
#ifdef __linux__
getrpcbyname(const char *name)
#else
getrpcbyname(name)
	char *name;
#endif
{
	struct rpcent *rpc;
	char **rp;

	setrpcent(0);
	while(NULL != (rpc = getrpcent())) {
#ifdef __linux__
		if (strcmp(rpc->r_name, name) == 0) {
			endrpcent();
			return (rpc);
		}
#else
		if (strcmp(rpc->r_name, name) == 0)
			return (rpc);
#endif
		for (rp = rpc->r_aliases; *rp != NULL; rp++) {
#ifdef __linux__
			if (strcmp(*rp, name) == 0) {
				endrpcent();
				return (rpc);
			}
#else
			if (strcmp(*rp, name) == 0)
				return (rpc);
#endif
		}
	}
	endrpcent();
    return (NULL);
}

#ifdef __linux__
void
#endif
setrpcent(f)
	int f;
{
	register struct rpcdata *d = _rpcdata();

	if (d == 0)
		return;
	if (d->rpcf == NULL)
#ifdef __linux__
		d->rpcf = fopen(_PATH_RPC, "r");
#else
		d->rpcf = fopen(RPCDB, "r");
#endif
	else
		rewind(d->rpcf);
	if (d->current)
		free(d->current);
	d->current = NULL;
	d->stayopen |= f;
#ifdef YP
    ypmode = 0;
    if(NULL != inkey)
      {
        free (inkey);
        inkey = NULL;
      }
#endif    
}

#ifdef __linux__
void
#endif
endrpcent()
{
	register struct rpcdata *d = _rpcdata();

	if (d == 0)
		return;
	if (d->current && !d->stayopen) {
		free(d->current);
		d->current = NULL;
	}
	if (d->rpcf && !d->stayopen) {
		fclose(d->rpcf);
		d->rpcf = NULL;
	}
#ifdef YP
    ypmode = 0;
    if(NULL != inkey)
      {
        free (inkey);
        inkey = NULL;
      }
#endif    
}

struct rpcent *
getrpcent(void)
{
#ifdef YP
  register struct rpcent *entry;
  if(0 == ypmode)
    {
      entry = _fgetrpcent();
      if (NULL == entry)
        {
          ypmode = 1;
          entry = _nis_getrpcent(1);
        }
    }
  else
    {
      entry = _nis_getrpcent(0);
    }
  return entry;
#else
  return (_fgetrpcent());
#endif
}


static struct rpcent *
_fgetrpcent(void)
{
	register struct rpcdata *d = _rpcdata();

	if (d == 0)
		return(NULL);
#ifdef __linux__
	if (d->rpcf == NULL && (d->rpcf = fopen(_PATH_RPC, "r")) == NULL)
#else
	if (d->rpcf == NULL && (d->rpcf = fopen(RPCDB, "r")) == NULL)
#endif
		return (NULL);
    if (fgets(d->line, BUFSIZ, d->rpcf) == NULL)
		return (NULL);
	return interpret(d->line, strlen(d->line));
}

#ifdef __linux__
static char *
firstwhite(char *s)
{
	char *s1, *s2;

	s1 = index(s, ' ');
	s2 = index(s, '\t');
	if (s1) {
		if (s2)
			return (s1 < s2) ? s1 : s2;
		else
			return s1;
	}
	else
		return s2;
}
#endif

static struct rpcent *
#ifdef __linux__
interpret(char *val, int len)
#else
interpret(val, len)
    char *val;
	int len;
#endif
{
	register struct rpcdata *d = _rpcdata();
	char *p;
	register char *cp, **q;

	if (d == 0)
		return NULL;
	strncpy(d->line, val, len);
	p = d->line;
	d->line[len] = '\n';
	if (*p == '#')
		return (getrpcent());
	cp = index(p, '#');
	if (cp == NULL)
    {
		cp = index(p, '\n');
		if (cp == NULL)
			return (getrpcent());
	}
	*cp = '\0';
#ifdef __linux__
	if ((cp = firstwhite(p)))
		*cp++ = 0;
	else
		return (getrpcent());
#else
	cp = index(p, ' ');
	if (cp == NULL)
    {
		cp = index(p, '\t');
		if (cp == NULL)
			return (getrpcent());
	}
	*cp++ = '\0';
#endif
	/* THIS STUFF IS INTERNET SPECIFIC */
	d->rpc.r_name = d->line;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	d->rpc.r_number = atoi(cp);
	q = d->rpc.r_aliases = d->rpc_aliases;
#ifdef __linux__
	if ((cp = firstwhite(cp)))
		*cp++ = '\0';
#else
	cp = index(p, ' ');
	if (cp != NULL)
		*cp++ = '\0';
	else
    {
		cp = index(p, '\t');
		if (cp != NULL)
			*cp++ = '\0';
	}
#endif
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &(d->rpc_aliases[MAXALIASES - 1]))
			*q++ = cp;
#ifdef __linux__
		if ((cp = firstwhite(cp)))
			*cp++ = '\0';
#else
		cp = index(p, ' ');
		if (cp != NULL)
			*cp++ = '\0';
		else
	    {
			cp = index(p, '\t');
			if (cp != NULL)
				*cp++ = '\0';
		}
#endif
	}
	*q = NULL;
	return (&d->rpc);
}

#ifdef	YP
static struct rpcent *
_nis_getrpcent(int first)
{
  static char *nisdomain = NULL;
  char *outkey, *outval;
  int outkeylen, outvallen, status;
  struct rpcent *rptr;
  
  if (1 == __yp_check(NULL))
    {
      if (NULL == nisdomain)
        yp_get_default_domain(&nisdomain);
      if (1 == first)
        {
          status = yp_first(nisdomain, "rpc.bynumber",
                            &outkey, &outkeylen,
                            &outval, &outvallen);
          if (0 != status) 
            {
              if (NULL != outval) free(outval) ;
              if (NULL != outkey) free(outkey) ;
              return NULL; 
            }
          inkey = outkey;
          inkeylen = outkeylen;
          rptr = interpret(outval, outvallen);
          free (outval);
          return (rptr);
        }
      else
        {
          status = yp_next(nisdomain, "rpc.bynumber",
                           inkey, inkeylen,
                           &outkey, &outkeylen,
                           &outval, &outvallen);
          if (0 != status)
            {
              free(inkey);
              inkey = NULL;
              return NULL;
            }
          free (inkey);
          inkey = outkey;
          inkeylen = outkeylen;
          rptr = interpret(outval, outvallen);
          free (outval);
          return (rptr);
        }
    }
  return NULL;

}

static struct rpcent *
_nis_getrpcbynumber(int number)
{
  register struct rpcdata *d = _rpcdata();
  register struct rpcent *p;
  int reason;
  char adrstr[16];
  
  if (__yp_check(NULL))
    {
      if (NULL == d->domain)
        yp_get_default_domain(&d->domain);
      sprintf(adrstr, "%d", number);
      reason = yp_match(d->domain, "rpc.bynumber",
                        adrstr, strlen(adrstr),
                        &d->current, &d->currentlen);
      if (0 != reason)
        return(0);
      p = interpret(d->current, d->currentlen);
      free (d->current);
      return p;
	}
  return(NULL);
}

#endif

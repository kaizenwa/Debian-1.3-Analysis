/*
** ypclnt.h
**
** Copyright (c) 1993 Signum Support AB
**
** This file is part of the NYS Library.
**
** The NYS Library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with the NYS Library; see the file COPYING.LIB.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Peter Eriksson <pen@signum.se>
*/

#ifndef __YPCLNT_H__
#define __YPCLNT_H__


/* Standard NIS client side error codes */

#define YPERR_BADARGS	1
#define YPERR_RPC	2
#define YPERR_DOMAIN	3
#define YPERR_MAP	4
#define YPERR_KEY	5
#define YPERR_YPERR	6
#define YPERR_RESRC	7
#define YPERR_NOMORE	8
#define YPERR_PMAP	9
#define YPERR_YPBIND	10
#define YPERR_YPSERV	11
#define YPERR_NODOM	12
#define YPERR_BADDB	13
#define YPERR_VERS	14
#define YPERR_ACCESS	15
#define YPERR_BUSY	16

/* Types of update operations */
#define YPOP_CHANGE 1                   /* change, do not add */
#define YPOP_INSERT 2                   /* add, do not change */
#define YPOP_DELETE 3                   /* delete this entry */
#define YPOP_STORE  4                   /* add, or change */

/* Function prototypes */

extern int yp_bind(char *);

extern void yp_unbind(char *);

extern int yp_get_default_domain(char **outdomain);

extern int yp_match(const char *indomain,
		    const char *inmap,
		    const char *inkey,
		    int inkeylen,
		    char **outval,
		    int *outvallen);

extern int yp_first(const char *indomain,
		    const char *inmap,
		    char **outkey,
		    int *outkeylen,
		    char **outval,
		    int *outvallen);

extern int yp_next(const char *indomain,
		   const char *inmap,
		   const char *inkey,
		   int inkeylen,
		   char **outkey,
		   int *outkeylen,
		   char **outval,
		   int *outvallen);

struct ypall_callback
{
    int (*foreach)(int instatus,
		   char *key,
		   int keylen,
		   char *val,
		   int vallen,
		   char *data);
    char *data;
};

extern int yp_all(char *indomain,
		  char *inmap,
		  struct ypall_callback *incbp);

extern int yp_order(char *indomain,
		    char *inmap,
		    int *outorder);

extern int yp_master(char *indomain,
		     char *inmap,
		     char **outname);

extern char *yperr_string(int ecode);

extern int ypprot_err(int ecode);

#endif

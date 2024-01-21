/* @(#)xdr.c	2.1 88/07/29 4.0 RPCSRC */
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
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)xdr.c 1.35 87/08/12";
#endif

/*
 * xdr.c, Generic XDR routines implementation.
 *
 * Copyright (C) 1986, Sun Microsystems, Inc.
 *
 * These are the "generic" xdr routines used to serialize and de-serialize
 * most common data items.  See xdr.h for more info on the interface to
 * xdr.
 */

#include <stdio.h>

#ifdef __STDC__
#include <stdlib.h>
#else
char *malloc();
#endif

#include <rpc/types.h>
#include <rpc/xdr.h>

#if NLS
#include "nl_types.h"
#endif

/*
 * constants specific to the xdr "protocol"
 */
#define XDR_FALSE	((long) 0)
#define XDR_TRUE	((long) 1)
#define LASTUNSIGNED	((u_int) 0-1)

/*
 * for unit alignment
 */
static char xdr_zero[BYTES_PER_XDR_UNIT] = { 0, 0, 0, 0 };

#ifdef __STDC__
void   xdr_free        ( xdrproc_t, char * );
bool_t xdr_void        ( void );
bool_t xdr_int         ( XDR *, int * );
bool_t xdr_u_int       ( XDR *, u_int * );
bool_t xdr_long        ( register XDR *, long * );
bool_t xdr_u_long      ( register XDR *, u_long * );
bool_t xdr_short       ( register XDR *, short * );
bool_t xdr_u_short     ( register XDR *, u_short * );
bool_t xdr_char        ( XDR *, char * );
bool_t xdr_u_char      ( XDR *, u_char * );
bool_t xdr_bool        ( register XDR *, bool_t * );
bool_t xdr_enum        ( XDR *, enum_t * );
bool_t xdr_opaque      ( register XDR *, caddr_t, register u_int );
bool_t xdr_bytes       ( register XDR *, char **, register u_int *, u_int );
bool_t xdr_netobj      ( XDR *, struct netobj * );
bool_t xdr_union       ( register XDR *, enum_t *, char *,
                         struct xdr_discrim *, xdrproc_t );
bool_t xdr_string      ( register XDR *, char **, u_int );
bool_t xdr_wrapstring  ( XDR *, char ** );
#else
void xdr_free();
bool_t xdr_void();
bool_t xdr_int();
bool_t xdr_u_int();
bool_t xdr_long();
bool_t xdr_u_long();
bool_t xdr_short();
bool_t xdr_u_short();
bool_t xdr_char();
bool_t xdr_u_char();
bool_t xdr_bool();
bool_t xdr_enum();
bool_t xdr_opaque();
bool_t xdr_bytes();
bool_t xdr_netobj();
bool_t xdr_union();
bool_t xdr_string();
bool_t xdr_wrapstring();
#endif	/* __STDC__ */

/*
 * Free a data structure using XDR
 * Not a filter, but a convenient utility nonetheless
 */
#ifdef __STDC__
void
xdr_free( xdrproc_t proc, char *objp )
#else
void
xdr_free(proc, objp)
	xdrproc_t proc;
	char *objp;
#endif
{
	XDR x;
	
	x.x_op = XDR_FREE;
	(*proc)(&x, objp);
}

/*
 * XDR nothing
 */
#ifdef __STDC__
bool_t
xdr_void( void )
#else
bool_t
xdr_void(/* xdrs, addr */)
	/* XDR *xdrs; */
	/* caddr_t addr; */
#endif
{

	return (TRUE);
}

/*
 * XDR integers
 */
#ifdef __STDC__
bool_t
xdr_int( XDR *xdrs, int *ip )
#else
bool_t
xdr_int(xdrs, ip)
	XDR *xdrs;
	int *ip;
#endif
{

#ifdef lint
	(void) (xdr_short(xdrs, (short *)ip));
	return (xdr_long(xdrs, (long *)ip));
#else
	if (sizeof (int) == sizeof (long)) {
		return (xdr_long(xdrs, (long *)ip));
	} else {
		return (xdr_short(xdrs, (short *)ip));
	}
#endif
}

/*
 * XDR unsigned integers
 */
#ifdef __STDC__
bool_t
xdr_u_int( XDR *xdrs, u_int *up )
#else
bool_t
xdr_u_int(xdrs, up)
	XDR *xdrs;
	u_int *up;
#endif
{

#ifdef lint
	(void) (xdr_short(xdrs, (short *)up));
	return (xdr_u_long(xdrs, (u_long *)up));
#else
	if (sizeof (u_int) == sizeof (u_long)) {
		return (xdr_u_long(xdrs, (u_long *)up));
	} else {
		return (xdr_short(xdrs, (short *)up));
	}
#endif
}

/*
 * XDR long integers
 * same as xdr_u_long - open coded to save a proc call!
 */
#ifdef __STDC__
bool_t
xdr_long( register XDR *xdrs, long *lp )
#else
bool_t
xdr_long(xdrs, lp)
	register XDR *xdrs;
	long *lp;
#endif
{

	if (xdrs->x_op == XDR_ENCODE)
		return (XDR_PUTLONG(xdrs, lp));

	if (xdrs->x_op == XDR_DECODE)
		return (XDR_GETLONG(xdrs, lp));

	if (xdrs->x_op == XDR_FREE)
		return (TRUE);

	return (FALSE);
}

/*
 * XDR unsigned long integers
 * same as xdr_long - open coded to save a proc call!
 */
#ifdef __STDC__
bool_t
xdr_u_long( register XDR *xdrs, u_long *ulp )
#else
bool_t
xdr_u_long(xdrs, ulp)
	register XDR *xdrs;
	u_long *ulp;
#endif
{

	if (xdrs->x_op == XDR_DECODE)
		return (XDR_GETLONG(xdrs, (long *)ulp));
	if (xdrs->x_op == XDR_ENCODE)
		return (XDR_PUTLONG(xdrs, (long *)ulp));
	if (xdrs->x_op == XDR_FREE)
		return (TRUE);
	return (FALSE);
}

/*
 * XDR short integers
 */
#ifdef __STDC__
bool_t
xdr_short( register XDR *xdrs, short *sp )
#else
bool_t
xdr_short(xdrs, sp)
	register XDR *xdrs;
	short *sp;
#endif
{
	long l;

	switch (xdrs->x_op) {

	case XDR_ENCODE:
		l = (long) *sp;
		return (XDR_PUTLONG(xdrs, &l));

	case XDR_DECODE:
		if (!XDR_GETLONG(xdrs, &l)) {
			return (FALSE);
		}
		*sp = (short) l;
		return (TRUE);

	case XDR_FREE:
		return (TRUE);
	}
	return (FALSE);
}

/*
 * XDR unsigned short integers
 */
#ifdef __STDC__
bool_t
xdr_u_short( register XDR *xdrs, u_short *usp )
#else
bool_t
xdr_u_short(xdrs, usp)
	register XDR *xdrs;
	u_short *usp;
#endif
{
	u_long l;

	switch (xdrs->x_op) {

	case XDR_ENCODE:
		l = (u_long) *usp;
		return (XDR_PUTLONG(xdrs, &l));

	case XDR_DECODE:
		if (!XDR_GETLONG(xdrs, &l)) {
			return (FALSE);
		}
		*usp = (u_short) l;
		return (TRUE);

	case XDR_FREE:
		return (TRUE);
	}
	return (FALSE);
}


/*
 * XDR a char
 */
#ifdef __STDC__
bool_t
xdr_char( XDR *xdrs, char *cp )
#else
bool_t
xdr_char(xdrs, cp)
	XDR *xdrs;
	char *cp;
#endif
{
	int i;

	i = (*cp);
	if (!xdr_int(xdrs, &i)) {
		return (FALSE);
	}
	*cp = i;
	return (TRUE);
}

/*
 * XDR an unsigned char
 */
#ifdef __STDC__
bool_t
xdr_u_char( XDR *xdrs, u_char *ucp )
#else
bool_t
xdr_u_char(xdrs, ucp)
	XDR *xdrs;
	u_char *ucp;
#endif
{
	u_int u;

	u = (*ucp);
	if (!xdr_u_int(xdrs, &u)) {
		return (FALSE);
	}
	*ucp = u;
	return (TRUE);
}

/*
 * XDR booleans
 */
#ifdef __STDC__
bool_t
xdr_bool( register XDR *xdrs, bool_t *bp )
#else
bool_t
xdr_bool(xdrs, bp)
	register XDR *xdrs;
	bool_t *bp;
#endif
{
	long lb;

	switch (xdrs->x_op) {

	case XDR_ENCODE:
		lb = *bp ? XDR_TRUE : XDR_FALSE;
		return (XDR_PUTLONG(xdrs, &lb));

	case XDR_DECODE:
		if (!XDR_GETLONG(xdrs, &lb)) {
			return (FALSE);
		}
		*bp = (lb == XDR_FALSE) ? FALSE : TRUE;
		return (TRUE);

	case XDR_FREE:
		return (TRUE);
	}
	return (FALSE);
}

/*
 * XDR enumerations
 */
#ifdef __STDC__
bool_t
xdr_enum( XDR *xdrs, enum_t *ep )
#else
bool_t
xdr_enum(xdrs, ep)
	XDR *xdrs;
	enum_t *ep;
#endif
{
#ifndef lint
	enum sizecheck { SIZEVAL };	/* used to find the size of an enum */

	/*
	 * enums are treated as ints
	 */
	if (sizeof (enum sizecheck) == sizeof (long)) {
		return (xdr_long(xdrs, (long *)ep));
	} else if (sizeof (enum sizecheck) == sizeof (short)) {
		return (xdr_short(xdrs, (short *)ep));
	} else {
		return (FALSE);
	}
#else
	(void) (xdr_short(xdrs, (short *)ep));
	return (xdr_long(xdrs, (long *)ep));
#endif
}

/*
 * XDR opaque data
 * Allows the specification of a fixed size sequence of opaque bytes.
 * cp points to the opaque object and cnt gives the byte length.
 */
#ifdef __STDC__
bool_t
xdr_opaque( register XDR *xdrs, caddr_t cp, register u_int cnt )
#else
bool_t
xdr_opaque(xdrs, cp, cnt)
	register XDR *xdrs;
	caddr_t cp;
	register u_int cnt;
#endif
{
	register u_int rndup;
	static char crud[BYTES_PER_XDR_UNIT];

	/*
	 * if no data we are done
	 */
	if (cnt == 0)
		return (TRUE);

	/*
	 * round byte count to full xdr units
	 */
	rndup = cnt % BYTES_PER_XDR_UNIT;
	if (rndup > 0)
		rndup = BYTES_PER_XDR_UNIT - rndup;

	if (xdrs->x_op == XDR_DECODE) {
		if (!XDR_GETBYTES(xdrs, cp, cnt)) {
			return (FALSE);
		}
		if (rndup == 0)
			return (TRUE);
		return (XDR_GETBYTES(xdrs, (caddr_t)crud, rndup));
	}

	if (xdrs->x_op == XDR_ENCODE) {
		if (!XDR_PUTBYTES(xdrs, cp, cnt)) {
			return (FALSE);
		}
		if (rndup == 0)
			return (TRUE);
		return (XDR_PUTBYTES(xdrs, xdr_zero, rndup));
	}

	if (xdrs->x_op == XDR_FREE) {
		return (TRUE);
	}

	return (FALSE);
}

/*
 * XDR counted bytes
 * *cpp is a pointer to the bytes, *sizep is the count.
 * If *cpp is NULL maxsize bytes are allocated
 */
#ifdef __STDC__
bool_t
xdr_bytes( register XDR *xdrs, char **cpp, register u_int *sizep,
	u_int maxsize )
#else
bool_t
xdr_bytes(xdrs, cpp, sizep, maxsize)
	register XDR *xdrs;
	char **cpp;
	register u_int *sizep;
	u_int maxsize;
#endif
{
	register char *sp = *cpp;  /* sp is the actual string pointer */
	register u_int nodesize;

#if NLS
	libc_nls_init();
#endif

	/*
	 * first deal with the length since xdr bytes are counted
	 */
	if (! xdr_u_int(xdrs, sizep)) {
		return (FALSE);
	}
	nodesize = *sizep;
	if ((nodesize > maxsize) && (xdrs->x_op != XDR_FREE)) {
		return (FALSE);
	}

	/*
	 * now deal with the actual bytes
	 */
	switch (xdrs->x_op) {

	case XDR_DECODE:
		if (nodesize == 0) {
			return (TRUE);
		}
		if (sp == NULL) {
			*cpp = sp = (char *)mem_alloc(nodesize);
		}
		if (sp == NULL) {
#if NLS
			(void) fprintf(stderr, "xdr_bytes: %s\n",
                              catgets(_libc_cat, RpcMiscSet,
                                      RpcMiscOutOfMemory, "out of memory"));
#else
			(void) fprintf(stderr, "xdr_bytes: out of memory\n");
#endif
			return (FALSE);
		}
		/* fall into ... */

	case XDR_ENCODE:
		return (xdr_opaque(xdrs, sp, nodesize));

	case XDR_FREE:
		if (sp != NULL) {
			mem_free(sp, nodesize);
			*cpp = NULL;
		}
		return (TRUE);
	}
	return (FALSE);
}

/*
 * Implemented here due to commonality of the object.
 */
#ifdef __STDC__
bool_t
xdr_netobj( XDR *xdrs, struct netobj *np )
#else
bool_t
xdr_netobj(xdrs, np)
	XDR *xdrs;
	struct netobj *np;
#endif
{

	return (xdr_bytes(xdrs, &np->n_bytes, &np->n_len, MAX_NETOBJ_SZ));
}

/*
 * XDR a descriminated union
 * Support routine for discriminated unions.
 * You create an array of xdrdiscrim structures, terminated with
 * an entry with a null procedure pointer.  The routine gets
 * the discriminant value and then searches the array of xdrdiscrims
 * looking for that value.  It calls the procedure given in the xdrdiscrim
 * to handle the discriminant.  If there is no specific routine a default
 * routine may be called.
 * If there is no specific or default routine an error is returned.
 */
#ifdef __STDC__
bool_t
xdr_union( register XDR *xdrs, enum_t *dscmp, char *unp,
	struct xdr_discrim *choices, xdrproc_t dfault )
#else
bool_t
xdr_union(xdrs, dscmp, unp, choices, dfault)
	register XDR *xdrs;
	enum_t *dscmp;		/* enum to decide which arm to work on */
	char *unp;		/* the union itself */
	struct xdr_discrim *choices;	/* [value, xdr proc] for each arm */
	xdrproc_t dfault;	/* default xdr routine */
#endif
{
	register enum_t dscm;

	/*
	 * we deal with the discriminator;  it's an enum
	 */
	if (! xdr_enum(xdrs, dscmp)) {
		return (FALSE);
	}
	dscm = *dscmp;

	/*
	 * search choices for a value that matches the discriminator.
	 * if we find one, execute the xdr routine for that value.
	 */
	for (; choices->proc != NULL_xdrproc_t; choices++) {
		if (choices->value == dscm)
			return ((*(choices->proc))(xdrs, unp, LASTUNSIGNED));
	}

	/*
	 * no match - execute the default xdr routine if there is one
	 */
	return ((dfault == NULL_xdrproc_t) ? FALSE :
	    (*dfault)(xdrs, unp, LASTUNSIGNED));
}


/*
 * Non-portable xdr primitives.
 * Care should be taken when moving these routines to new architectures.
 */


/*
 * XDR null terminated ASCII strings
 * xdr_string deals with "C strings" - arrays of bytes that are
 * terminated by a NULL character.  The parameter cpp references a
 * pointer to storage; If the pointer is null, then the necessary
 * storage is allocated.  The last parameter is the max allowed length
 * of the string as specified by a protocol.
 */
#ifdef __STDC__
bool_t
xdr_string( register XDR *xdrs, char **cpp, u_int maxsize )
#else
bool_t
xdr_string(xdrs, cpp, maxsize)
	register XDR *xdrs;
	char **cpp;
	u_int maxsize;
#endif
{
	register char *sp = *cpp;  /* sp is the actual string pointer */
	u_int size;
	u_int nodesize;

#if NLS
	libc_nls_init();
#endif
	/*
	 * first deal with the length since xdr strings are counted-strings
	 */
	switch (xdrs->x_op) {
	case XDR_FREE:
		if (sp == NULL) {
			return(TRUE);	/* already free */
		}
		/* fall through... */
	case XDR_ENCODE:
		if (sp == NULL) return (FALSE);
		size = strlen(sp);
		break;
	default: break;
	}
	if (! xdr_u_int(xdrs, &size)) {
		return (FALSE);
	}
	if (size > maxsize) {
		return (FALSE);
	}
	nodesize = size + 1;

	/*
	 * now deal with the actual bytes
	 */
	switch (xdrs->x_op) {

	case XDR_DECODE:
		if (nodesize == 0) {
			return (TRUE);
		}
		if (sp == NULL)
			*cpp = sp = (char *)mem_alloc(nodesize);
		if (sp == NULL) {
#if NLS
			(void) fprintf(stderr, "xdr_string: %s\n",
                              catgets(_libc_cat, RpcMiscSet,
                                      RpcMiscOutOfMemory, "out of memory"));
#else
			(void) fprintf(stderr, "xdr_string: out of memory\n");
#endif
			return (FALSE);
		}
		sp[size] = 0;
		/* fall into ... */

	case XDR_ENCODE:
		return (xdr_opaque(xdrs, sp, size));

	case XDR_FREE:
		mem_free(sp, nodesize);
		*cpp = NULL;
		return (TRUE);
	default: break;
	}
	return (FALSE);
}

/* 
 * Wrapper for xdr_string that can be called directly from 
 * routines like clnt_call
 */
#ifdef __STDC__
bool_t
xdr_wrapstring( XDR *xdrs, char **cpp )
#else
bool_t
xdr_wrapstring(xdrs, cpp)
	XDR *xdrs;
	char **cpp;
#endif
{
	if (xdr_string(xdrs, cpp, LASTUNSIGNED)) {
		return (TRUE);
	}
	return (FALSE);
}

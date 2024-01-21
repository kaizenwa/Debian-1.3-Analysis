/* @(#)xdr_stdio.c	2.1 88/07/29 4.0 RPCSRC */
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
static char sccsid[] = "@(#)xdr_stdio.c 1.16 87/08/11 Copyr 1984 Sun Micro";
#endif

/*
 * xdr_stdio.c, XDR implementation on standard i/o file.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * This set of routines implements a XDR on a stdio stream.
 * XDR_ENCODE serializes onto the stream, XDR_DECODE de-serializes
 * from the stream.
 */

#include <rpc/types.h>
#include <stdio.h>
#include <rpc/xdr.h>
#include <netinet/in.h>

#ifdef __STDC__
void xdrstdio_create( register XDR *, FILE *, enum xdr_op );
static void xdrstdio_destroy( register XDR * );
static bool_t xdrstdio_getlong( XDR *, register long * );
static bool_t xdrstdio_putlong( XDR *, long * );
static bool_t xdrstdio_getbytes( XDR *, caddr_t, int );
static bool_t xdrstdio_putbytes( XDR *, caddr_t, int );
static u_int xdrstdio_getpos( XDR * );
static bool_t xdrstdio_setpos( XDR *, u_int );
static long *xdrstdio_inline( XDR *, int );
#else
void xdrstdio_create();
static void xdrstdio_destroy();
static bool_t xdrstdio_getlong();
static bool_t xdrstdio_putlong();
static bool_t xdrstdio_getbytes();
static bool_t xdrstdio_putbytes();
static u_int xdrstdio_getpos();
static bool_t xdrstdio_setpos();
static long *xdrstdio_inline();
#endif	/* __STDC__ */

/*
 * Ops vector for stdio type XDR
 */
static struct xdr_ops	xdrstdio_ops = {
	xdrstdio_getlong,	/* deseraialize a long int */
	xdrstdio_putlong,	/* seraialize a long int */
	xdrstdio_getbytes,	/* deserialize counted bytes */
	xdrstdio_putbytes,	/* serialize counted bytes */
	xdrstdio_getpos,	/* get offset in the stream */
	xdrstdio_setpos,	/* set offset in the stream */
	xdrstdio_inline,	/* prime stream for inline macros */
	xdrstdio_destroy	/* destroy stream */
};

/*
 * Initialize a stdio xdr stream.
 * Sets the xdr stream handle xdrs for use on the stream file.
 * Operation flag is set to op.
 */

#ifdef __STDC__
void
xdrstdio_create( register XDR *xdrs, FILE *file, enum xdr_op op )
#else
void
xdrstdio_create(xdrs, file, op)
	register XDR *xdrs;
	FILE *file;
	enum xdr_op op;
#endif
{

	xdrs->x_op = op;
	xdrs->x_ops = &xdrstdio_ops;
	xdrs->x_private = (caddr_t)file;
	xdrs->x_handy = 0;
	xdrs->x_base = 0;
}

/*
 * Destroy a stdio xdr stream.
 * Cleans up the xdr stream handle xdrs previously set up by xdrstdio_create.
 */
#ifdef __STDC__
static void
xdrstdio_destroy( register XDR *xdrs )
#else
static void
xdrstdio_destroy(xdrs)
	register XDR *xdrs;
#endif
{
	(void)fflush((FILE *)xdrs->x_private);
	/* xx should we close the file ?? */
};

#ifdef __STDC__
static bool_t
xdrstdio_getlong( XDR *xdrs, register long *lp )
#else
static bool_t
xdrstdio_getlong(xdrs, lp)
	XDR *xdrs;
	register long *lp;
#endif
{

	if (fread((caddr_t)lp, sizeof(long), 1, (FILE *)xdrs->x_private) != 1)
		return (FALSE);
#ifndef mc68000
	*lp = ntohl(*lp);
#endif
	return (TRUE);
}

#ifdef __STDC__
static bool_t
xdrstdio_putlong( XDR *xdrs, long *lp )
#else
static bool_t
xdrstdio_putlong(xdrs, lp)
	XDR *xdrs;
	long *lp;
#endif
{

#ifndef mc68000
	long mycopy = htonl(*lp);
	lp = &mycopy;
#endif
	if (fwrite((caddr_t)lp, sizeof(long), 1, (FILE *)xdrs->x_private) != 1)
		return (FALSE);
	return (TRUE);
}

#ifdef __STDC__
static bool_t
xdrstdio_getbytes( XDR *xdrs, caddr_t addr, int len )
#else
static bool_t
xdrstdio_getbytes(xdrs, addr, len)
	XDR *xdrs;
	caddr_t addr;
	int len;
#endif
{

	if ((len != 0) && (fread(addr, (int)len, 1, (FILE *)xdrs->x_private) != 1))
		return (FALSE);
	return (TRUE);
}

#ifdef __STDC__
static bool_t
xdrstdio_putbytes( XDR *xdrs, caddr_t addr, int len )
#else
static bool_t
xdrstdio_putbytes(xdrs, addr, len)
	XDR *xdrs;
	caddr_t addr;
	int len;
#endif
{

	if ((len != 0) && (fwrite(addr, (int)len, 1, (FILE *)xdrs->x_private) != 1))
		return (FALSE);
	return (TRUE);
}

#ifdef __STDC__
static u_int
xdrstdio_getpos( XDR *xdrs )
#else
static u_int
xdrstdio_getpos(xdrs)
	XDR *xdrs;
#endif
{

	return ((u_int) ftell((FILE *)xdrs->x_private));
}

#ifdef __STDC__
static bool_t
xdrstdio_setpos( XDR *xdrs, u_int pos )
#else
static bool_t
xdrstdio_setpos(xdrs, pos) 
	XDR *xdrs;
	u_int pos;
#endif
{ 

	return ((fseek((FILE *)xdrs->x_private, (long)pos, 0) < 0) ?
		FALSE : TRUE);
}

#ifdef __STDC__
static long *
xdrstdio_inline( XDR *xdrs, int len )
#else
static long *
xdrstdio_inline(xdrs, len)
	XDR *xdrs;
	u_int len;
#endif
{

	/*
	 * Must do some work to implement this: must insure
	 * enough data in the underlying stdio buffer,
	 * that the buffer is aligned so that we can indirect through a
	 * long *, and stuff this pointer in xdrs->x_buf.  Doing
	 * a fread or fwrite to a scratch buffer would defeat
	 * most of the gains to be had here and require storage
	 * management on this buffer, so we don't do this.
	 */
	return (NULL);
}

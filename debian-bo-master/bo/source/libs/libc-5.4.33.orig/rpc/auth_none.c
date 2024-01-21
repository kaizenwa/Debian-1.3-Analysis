/* @(#)auth_none.c	2.1 88/07/29 4.0 RPCSRC */
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
static char sccsid[] = "@(#)auth_none.c 1.19 87/08/11 Copyr 1984 Sun Micro";
#endif

/*
 * auth_none.c
 * Creates a client authentication handle for passing "null" 
 * credentials and verifiers to remote systems. 
 * 
 * Copyright (C) 1984, Sun Microsystems, Inc. 
 */

#include <rpc/types.h>
#include <rpc/xdr.h>
#include <rpc/auth.h>

#ifdef __STDC__
#include <stdlib.h>
#endif

#define MAX_MARSHEL_SIZE 20

/*
 * Authenticator operations routines
 */
#ifdef __STDC__
AUTH          *authnone_create   ( void );
static bool_t  authnone_marshal  ( AUTH *, XDR * );
static void    authnone_verf     ( AUTH * );         /* void */
static bool_t  authnone_validate ( AUTH *, struct opaque_auth * ); /* void */
static bool_t  authnone_refresh  ( AUTH * );         /* void */
static void    authnone_destroy  ( AUTH * );         /* void */
#else
AUTH *authnone_create();
static bool_t  authnone_marshal();
static void    authnone_verf();
static bool_t  authnone_validate();
static bool_t  authnone_refresh();
static void    authnone_destroy();
#endif

static struct auth_ops ops = {
	authnone_verf,
	authnone_marshal,
	authnone_validate,
	authnone_refresh,
	authnone_destroy
};

static struct authnone_private {
	AUTH	no_client;
	char	marshalled_client[MAX_MARSHEL_SIZE];
	u_int	mcnt;
} *authnone_private;

#ifdef __STDC__
AUTH *
authnone_create( void )
#else
AUTH *
authnone_create()
#endif
{
	register struct authnone_private *ap = authnone_private;
	XDR xdr_stream;
	register XDR *xdrs;

	if (ap == 0) {
		ap = (struct authnone_private *)calloc(1, sizeof (*ap));
		if (ap == 0)
			return (0);
		authnone_private = ap;
	}
	if (!ap->mcnt) {
		ap->no_client.ah_cred = ap->no_client.ah_verf = _null_auth;
		ap->no_client.ah_ops = &ops;
		xdrs = &xdr_stream;
		xdrmem_create(xdrs, ap->marshalled_client, (u_int)MAX_MARSHEL_SIZE,
		    XDR_ENCODE);
		(void)xdr_opaque_auth(xdrs, &ap->no_client.ah_cred);
		(void)xdr_opaque_auth(xdrs, &ap->no_client.ah_verf);
		ap->mcnt = XDR_GETPOS(xdrs);
		XDR_DESTROY(xdrs);
	}
	return (&ap->no_client);
}

/*ARGSUSED*/
#ifdef __STDC__
static bool_t
authnone_marshal( AUTH *client, XDR *xdrs )
#else
static bool_t
authnone_marshal(client, xdrs)
	AUTH *client;
	XDR *xdrs;
#endif
{
	register struct authnone_private *ap = authnone_private;

	if (ap == 0)
		return (0);
	return ((*xdrs->x_ops->x_putbytes)(xdrs,
	    ap->marshalled_client, ap->mcnt));
}

#ifdef __STDC__
/* This prototype changed from 'void' to satisfy gcc */
static void authnone_verf( AUTH *dummy )
#else
static void 
authnone_verf()
#endif
{
}

#ifdef __STDC__
/* This prototype changed from 'void' to satisfy gcc */
static bool_t
authnone_validate( AUTH *dummy, struct opaque_auth *idiot )
#else
static bool_t
authnone_validate()
#endif
{

	return (TRUE);
}

#ifdef __STDC__
static bool_t
/* This prototype changed from 'void' to satisfy gcc */
authnone_refresh( AUTH *dummy )
#else
static bool_t
authnone_refresh()
#endif
{

	return (FALSE);
}

#ifdef __STDC__
static void
/* This prototype changed from 'void' to satisfy gcc */
authnone_destroy( AUTH *dummy )
#else
static void
authnone_destroy()
#endif
{
}

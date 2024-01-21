/* @(#)svc_run.c	2.1 88/07/29 4.0 RPCSRC */
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)svc_run.c 1.1 87/10/13 Copyr 1984 Sun Micro";
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
 * This is the rpc server side idle loop
 * Wait for input, call server program.
 */
#include <rpc/rpc.h>
#include <sys/errno.h>

#if NLS
#include "nl_types.h"
#endif

static int svc_stop = 0;

void svc_exit (void)
{
  svc_stop = 1;
}

void
svc_run()
{
#ifdef FD_SETSIZE
	fd_set readfds;
#else
      int readfds;
#endif /* def FD_SETSIZE */
	extern int errno;

#if NLS
	libc_nls_init();
#endif

	svc_stop = 0;

	for (;;) {
		if (svc_stop)
			return;
#ifdef FD_SETSIZE
		readfds = svc_fdset;
#else
		readfds = svc_fds;
#endif /* def FD_SETSIZE */
		switch (select(_rpc_dtablesize(), &readfds, (void *)0, (void *)0,
			       (struct timeval *)0)) {
		case -1:
			if (errno == EINTR) {
				continue;
			}
#if NLS
			perror(catgets(_libc_cat, RpcMiscSet,
				       RpcMiscSelectFailed,
					"svc_run: - select failed"));
#else
			perror("svc_run: - select failed");
#endif
			return;
		case 0:
			continue;
		default:
			svc_getreqset(&readfds);
		}
	}
}

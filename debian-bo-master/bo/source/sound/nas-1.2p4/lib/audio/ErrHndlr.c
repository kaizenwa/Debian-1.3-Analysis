/* $NCDId: @(#)ErrHndlr.c,v 1.1 1994/08/09 01:12:56 greg Exp $ */
/* $XConsortium: XErrHndlr.c,v 11.16 91/11/09 15:39:57 keith Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	 */

/*
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 */

#include "Alibint.h"

extern int      _AuDefaultError();
extern int      _AuDefaultIOError();

/*
 * AuSetErrorHandler - This procedure sets the NAS non-fatal error handler
 * (aud->funcs.error_handler) to be the specified routine. If NULL is passed
 * in the original error handler is restored.
 */

AuErrorHandler 
#if NeedFunctionPrototypes
AuSetErrorHandler(AuServer *aud, AuErrorHandler handler)
#else
AuSetErrorHandler(aud, handler)
AuServer       *aud;
register AuErrorHandler handler;
#endif
{
    AuErrorHandler  oldhandler = aud->funcs.error_handler;

    if (!oldhandler)
	oldhandler = _AuDefaultError;

    aud->funcs.error_handler = handler ? handler : _AuDefaultError;
    return oldhandler;
}

/*
 * AuSetIOErrorHandler - This procedure sets the NAS fatal I/O error handler
 * (aud->funcs.ioerror_handler) to be the specified routine.  If NULL is
 * passed in the original error handler is restored.
 */

extern int      _AuIOError();
AuIOErrorHandler 
#if NeedFunctionPrototypes
AuSetIOErrorHandler(AuServer *aud, AuIOErrorHandler handler)
#else
AuSetIOErrorHandler(aud, handler)
AuServer       *aud;
register AuIOErrorHandler handler;
#endif
{
    AuIOErrorHandler oldhandler = aud->funcs.ioerror_handler;

    if (!oldhandler)
	oldhandler = _AuDefaultIOError;

    aud->funcs.ioerror_handler = handler ? handler : _AuDefaultIOError;
    return oldhandler;
}

/*
 * Copyright (c) 1990,1991 Regents of The University of Michigan.
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appears in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation, and that the name of The University
 * of Michigan not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission. This software is supplied as is without expressed or
 * implied warranties of any kind.
 *
 *	Research Systems Unix Group
 *	The University of Michigan
 *	c/o Mike Clark
 *	535 W. William Street
 *	Ann Arbor, Michigan
 *	+1-313-763-0525
 *	netatalk@itd.umich.edu
 */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/syslog.h>
#include <netatalk/at.h>
#include <sys/time.h>
#include <atalk/atp.h>
#include <atalk/asp.h>
#include <atalk/afp.h>

#include "switch.h"

extern int	afp_login();
extern int	afp_getsrvrparms();
extern int	afp_openvol();
extern int	afp_getfildirparams();
extern int	afp_closevol();
extern int	afp_opendt();
extern int	afp_getvolparams();
extern int	afp_enumerate();
extern int	afp_setfildirparams();
extern int	afp_openfork();
extern int	afp_getforkparams();
extern int	afp_read();
extern int	afp_createfile();
extern int	afp_setforkparams();
extern int	afp_flushfork();
extern int	afp_flush();
extern int	afp_write();
extern int	afp_closefork();
extern int	afp_addicon();
extern int	afp_geticoninfo();
extern int	afp_geticon();
extern int	afp_setfilparams();
extern int	afp_addcomment();
extern int	afp_getcomment();
extern int	afp_rmvcomment();
extern int	afp_addappl();
extern int	afp_rmvappl();
extern int	afp_getappl();
extern int	afp_createdir();

# ifdef AFS
extern int	afp_getdiracl();
extern int	afp_setdiracl();
# else AFS
#define afp_getdiracl	NULL
#define afp_setdiracl	NULL
# endif AFS

# if defined( AFS ) && defined( UAM_AFSKRB )
extern int	afp_afschangepw();
# else AFS UAM_AFSKRB
#define afp_afschangepw	NULL
# endif AFS UAM_AFSKRB

extern int	afp_rename();
extern int	afp_delete();
extern int	afp_logincont();
extern int	afp_copyfile();
extern int	afp_moveandrename();
extern int	afp_mapid();
extern int	afp_mapname();
extern int	afp_bytelock();
extern int	afp_setdirparams();

afp_null( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    syslog( LOG_INFO, "afp_null handle %d", *ibuf );
    *rbuflen = 0;
    return( AFPERR_NOOP );
}

/*
 * Routines marked "NULL" are not AFP functions.
 * Routines marked "afp_null" are AFP functions
 * which are not yet implemented. A fine line...
 */
int	(*preauth_switch[])() = {
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*   0 -   7 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*   8 -  15 */
    NULL, NULL, afp_login, afp_logincont,
    NULL, NULL, NULL, NULL,					/*  16 -  23 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  24 -  31 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  32 -  39 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  40 -  47 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  48 -  55 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  56 -  63 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  64 -  71 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  72 -  79 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  80 -  87 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  88 -  95 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  96 - 103 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 104 - 111 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 112 - 119 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 120 - 127 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 128 - 135 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 136 - 143 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 144 - 151 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 152 - 159 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 160 - 167 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 168 - 175 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 176 - 183 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 184 - 191 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 192 - 199 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 200 - 207 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 208 - 215 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 216 - 223 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 224 - 231 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 232 - 239 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 240 - 247 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 248 - 255 */
};

int	(**afp_switch)() = preauth_switch;

int	(*postauth_switch[])() = {
    NULL, afp_bytelock, afp_closevol, afp_null,
    afp_closefork, afp_copyfile, afp_createdir, afp_createfile,	/*   0 -   7 */
    afp_delete, afp_enumerate, afp_flush, afp_flushfork,
    afp_null, afp_null, afp_getforkparams, afp_null,		/*   8 -  15 */
    afp_getsrvrparms, afp_getvolparams, afp_login, afp_logincont,
    afp_null, afp_mapid, afp_mapname, afp_moveandrename,	/*  16 -  23 */
    afp_openvol, afp_null, afp_openfork, afp_read,
    afp_rename, afp_setdirparams, afp_setfilparams, afp_setforkparams,
								/*  24 -  31 */
    afp_null, afp_write, afp_getfildirparams, afp_setfildirparams,
    afp_null, afp_null, afp_null, afp_null,			/*  32 -  39 */
    afp_null, afp_null, afp_null, afp_null,
    afp_null, afp_null, afp_null, afp_null,			/*  40 -  47 */
    afp_opendt, afp_null, afp_null, afp_geticon,
    afp_geticoninfo, afp_addappl, afp_rmvappl, afp_getappl,	/*  48 -  55 */
    afp_addcomment, afp_rmvcomment, afp_getcomment, NULL,
    NULL, NULL, NULL, NULL,					/*  56 -  63 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  64 -  71 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  72 -  79 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  80 -  87 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/*  88 -  95 */
    NULL, NULL, NULL, NULL,
    afp_getdiracl, afp_setdiracl, afp_afschangepw, NULL,	/*  96 - 103 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 104 - 111 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 112 - 119 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 120 - 127 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 128 - 135 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 136 - 143 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 144 - 151 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 152 - 159 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 160 - 167 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 168 - 175 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 176 - 183 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 184 - 191 */
    afp_addicon, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 192 - 199 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 200 - 207 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 208 - 215 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 216 - 223 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 224 - 231 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 232 - 239 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 240 - 247 */
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,					/* 248 - 255 */
};

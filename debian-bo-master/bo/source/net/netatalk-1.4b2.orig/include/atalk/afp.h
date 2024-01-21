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

struct afp_status {
    u_short	as_machoff;
    u_short	as_versoff;
    u_short	as_uamsoff;
    u_short	as_iconoff;
    u_short	as_flags;
};

#define AFPSRVRINFO_COPY	(1<<0)
#define AFPSRVRINFO_PASSWD	(1<<1)
#define AFPSRVRINFO_FASTBOZO	(1<<15)

#define AFP_OK		0
#define AFPERR_ACCESS	-5000
#define AFPERR_AUTHCONT	-5001
#define AFPERR_BADUAM	-5002
#define AFPERR_BADVERS	-5003
#define AFPERR_BITMAP	-5004
#define AFPERR_DENYCONF	-5006
#define AFPERR_DIRNEMPT	-5007
#define AFPERR_DFULL	-5008
#define AFPERR_EOF	-5009
#define AFPERR_BUSY	-5010
#define AFPERR_NOITEM	-5012
#define AFPERR_NOSRVR	-5016
#define AFPERR_EXIST	-5017
#define AFPERR_NOOBJ	-5018
#define AFPERR_PARAM	-5019
#define AFPERR_NOTAUTH	-5023
#define AFPERR_NOOP	-5024
#define AFPERR_BADTYPE	-5025
#define AFPERR_NFILE	-5026
#define AFPERR_SHUTDOWN	-5027
#define AFPERR_NODIR	-5029
#define AFPERR_ITYPE	-5030
#define AFPERR_VLOCK	-5031

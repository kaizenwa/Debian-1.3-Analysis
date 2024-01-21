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

struct dir {
    struct dir	*d_left, *d_right, *d_parent, *d_child, *d_next;
    int		d_balance;
    int		d_did;
    int		d_flags;
    char	*d_name;
};

#define DIRF_FSMASK	(3<<0)
#define DIRF_NOFS	(0<<0)
#define DIRF_AFS	(1<<0)
#define DIRF_UFS	(2<<0)

#define AFPDIR_READ	(1<<0)

#define DIRPBIT_ATTR	0
#define DIRPBIT_PDID	1
#define DIRPBIT_CDATE	2
#define DIRPBIT_MDATE	3
#define DIRPBIT_BDATE	4
#define DIRPBIT_FINFO	5
#define DIRPBIT_LNAME	6
#define DIRPBIT_SNAME	7
#define DIRPBIT_DID	8
#define DIRPBIT_OFFCNT	9
#define DIRPBIT_UID	10
#define DIRPBIT_GID	11
#define DIRPBIT_ACCESS	12

struct maccess {
    u_char	ma_user;
    u_char	ma_world;
    u_char	ma_group;
    u_char	ma_owner;
};

#define	AR_USEARCH	(1<<0)
#define	AR_UREAD	(1<<1)
#define	AR_UWRITE	(1<<2)
#define	AR_UOWN		(1<<7)

extern struct dir	*dirsearch();
extern struct dir	*adddir();
extern char		*cname();
extern char		*ctoupath();

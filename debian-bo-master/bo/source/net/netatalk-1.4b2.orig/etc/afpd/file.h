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

extern u_char	ufinderi[];

#define FILPBIT_ATTR	0
#define FILPBIT_PDID	1
#define FILPBIT_CDATE	2
#define FILPBIT_MDATE	3
#define FILPBIT_BDATE	4
#define FILPBIT_FINFO	5
#define FILPBIT_LNAME	6
#define FILPBIT_SNAME	7
#define FILPBIT_FNUM	8
#define FILPBIT_DFLEN	9
#define FILPBIT_RFLEN	10

#define ATTRBIT_SETCLR	(1<<15)
#define FILEIOFF_CREATE	0
#define FILEIOFF_MODIFY	4
#define FILEIOFF_BACKUP	8
#define FILEIOFF_ATTR	14

struct extmap {
    struct extmap	*em_next;
    char		em_ext[ MAXNAMLEN ];
    char		em_creator[ 4 ];
    char		em_type[ 4 ];
};

extern struct extmap	*extmap;
struct extmap		*getextmap();

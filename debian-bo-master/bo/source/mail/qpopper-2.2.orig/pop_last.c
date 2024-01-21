/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char copyright[] = "Copyright (c) 1990 Regents of the University of California.\nAll rights reserved.\n";
static char SccsId[] = "@(#)@(#)pop_last.c	2.1  2.1 3/18/91";
#endif not lint

#include <stdio.h>
#include <sys/types.h>
#include "popper.h"

/* 
 *  last:   Display the last message touched in a POP session
 */

int pop_last (p)
POP     *   p;
{
    int	count = p->msg_count;
    MsgInfoList *mp;

    for (mp = p->mlp + p->msg_count - 1; count > 0; mp--, count--) {
	if ((mp->retr_flag == TRUE) && (mp->del_flag == FALSE))
	    break;
    }

    return (pop_msg(p,POP_SUCCESS,"%u is the last read message.",count));
}

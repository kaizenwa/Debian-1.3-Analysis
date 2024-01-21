/* Setpgid.c - Emulate setpgid for systems that don't have it.
   Copyright (C) 1996 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include <errno.h>
#include <sys/types.h>

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: setpgid.c,v 1.1 1996/03/17 01:08:15 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/*LINTLIBRARY*/
/****************************************************************************/
/* Global function declarations */

extern int getpid(), setpgrp(), getpgrp();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
int setpgid(pid, pgid)
int pid, pgid;
{
	/* Emulate setpgid by using setpgrp */

#ifdef PGRP_FUNCS_NEED_ARGS
	/* This is a straightforward substitution */

	return(setpgrp(pid, pgid));

#else /* ! PGRP_FUNCS_NEED_ARGS */
	/* In this case, there are things this library can't do */

	if (pid != 0 && pid != getpid() || pgid != 0 && pgid != pid) {
		/* This is valid, but we can't emulate it */
		errno = EINVAL;
		return(-1);
	}

	return(setpgrp());
#endif /* ! PGRP_FUNCS_NEED_ARGS */
}
/****************************************************************************/
int getpgid(pid)
int pid;
{
	/* Emulate setpgid by using setpgrp */

#ifdef PGRP_FUNCS_NEED_ARGS
	/* This is a straightforward substitution */

	return(getpgrp(pid));

#else /* ! PGRP_FUNCS_NEED_ARGS */
	/* In this case, there are things this library can't do */

	if (pid != 0 && pid != getpid()) {
		/* This is valid, but we can't emulate it */
		errno = EINVAL;
		return(-1);
	}

	return(getpgrp());
#endif /* ! PGRP_FUNCS_NEED_ARGS */
}
/****************************************************************************/

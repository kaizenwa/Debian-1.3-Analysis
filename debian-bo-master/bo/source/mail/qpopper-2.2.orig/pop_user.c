/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char copyright[] = "Copyright (c) 1990 Regents of the University of California.\nAll rights reserved.\n";
static char SccsId[] = "@(#)@(#)pop_user.c	2.1  2.1 3/18/91";
#endif /* not lint */

#include <stdio.h>
#include <sys/types.h>
#include <pwd.h>
#include<sys/stat.h>

#if defined(SOLARIS2) || defined(SYSV) || defined(AIX)
# include <string.h>
# include "flock.h"
# define index(s, c)	strchr(s, c)
#else
# include <strings.h>
#endif

#if defined(SOLARIS2) || defined(UNIXWARE) || defined(AIX) || defined(PTX) \
	|| defined(AUX) || defined(POPSCO) || defined(OSF1) || defined(ULTRIX)
# include <fcntl.h>
#else
# include <sys/file.h>
#endif

#include <ndbm.h>

#include "popper.h"

/* 
 *  user:   Prompt for the user name at the start of a POP session
 */

int pop_user (p)
POP     *   p;
{
    /* If there is an APOP database entry then don't allow a cleartext
       password over the net */
# ifdef APOP
    char apop_dir[BUFSIZ];
    DBM    *db;
    int	   fid;
    struct passwd *pw;
    struct stat st;
    datum	key, value;
# endif

#ifdef KERBEROS
    if (p->kerberos && strcmp(p->pop_parm[1], p->user)) {
	pop_log(p, LOG_WARNING, "%s: auth failed: %s.%s@@%s vs %s",
		p->client, kdata.pname, kdata.pinst, kdata.prealm, 
		p->pop_parm[1]);
        return(pop_msg(p,POP_FAILURE,
		       "Wrong username supplied (%s vs. %s).", p->user,
		       p->pop_parm[1]));
    }
#endif

    /*  Save the user name */
    (void)strncpy(p->user, p->pop_parm[1], sizeof(p->user));
    p->user[sizeof(p->user)-1] = 0;

# ifdef APOP_ONLY
	return(pop_auth_fail(p, POP_FAILURE,
	    "You must use APOP authentication to connect to this server"));
# endif

# ifdef APOP

	/* If this call fails then the database is not accessable (doesn't
	   exist?) in which case we can ignore an APOP user trying to
	   access the popper with a cleartext password.
         */
    if (((pw = getpwnam(p->user)) != NULL) &&
	((db = dbm_open(APOP, O_RDONLY, 0)) != NULL)) {

	(void) strncpy(apop_dir, APOP, sizeof(apop_dir) - 5);
# if defined(BSD44_DBM)
	(void) strcat(apop_dir, ".db");
# else
	(void) strcat(apop_dir, ".dir");
# endif
	if (stat (apop_dir, &st) != -1 && (st.st_mode & 0777) != 0600) {
	    dbm_close (db);
	    return(pop_auth_fail(p, POP_FAILURE,
		"POP authorization DB has wrong mode (0%o)",st.st_mode & 0777));
	}
	fid = open(apop_dir, O_RDONLY);
	if(fid == -1) {
	    int e = errno;
	    dbm_close (db);
	    return(pop_auth_fail(p, POP_FAILURE,
		    "unable to lock POP authorization DB (%s)", strerror(e)));
	}
	if (flock (fid, LOCK_SH) == -1) {
	    int e = errno;
	    (void) close(fid);
	    dbm_close (db);
	    return(pop_auth_fail(p, POP_FAILURE,
		    "unable to lock POP authorization DB (%s)", strerror(e)));
	}
	key.dsize = strlen (key.dptr = p->user) + 1;
	value = dbm_fetch (db, key);
	dbm_close (db);
	(void) close(fid);

	if (value.dptr != NULL) {
	    return(pop_auth_fail(p, POP_FAILURE,
		"You must use APOP to connect to this server"));
	}
    }
#endif /* APOP */

    /*  Tell the user that the password is required */
    return (pop_msg(p,POP_SUCCESS,"Password required for %s.",p->user));
}


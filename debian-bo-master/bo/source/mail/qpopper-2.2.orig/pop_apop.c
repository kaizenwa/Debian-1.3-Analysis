/*  */
/*
 * APOP authentication, derived from MH 6.8.3, with this ident:
 * ident[]="@(#)$Id: pop_apop.c,v 1.6 1996/05/22 17:39:34 mark Exp $";
 */

#ifdef APOP
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ndbm.h>
#include <fcntl.h>
#include <pwd.h>

#if defined(SOLARIS2) || defined(SYSV) || defined(AIX)
# include	<string.h>
# define bcopy(src,dest,len)	(void) (memcpy(dest,src,len))
# define bzero(dest,len)  	(void) (memset(dest, (char)NULL, len))
# define bcmp(b1,b2,n)		memcmp(b1,b2,n)
# ifndef index
#  define index(s,c)		strchr(s,c)
# endif
# ifndef rindex
#  define rindex(s,c)		strrchr(s,c)
# endif
# include "flock.h"
#else
# include <strings.h>
# include <sys/file.h>
#endif

#include "popper.h"
#include "md5.h"

/*
 * Obscure password so a cleartext search doesn't come up with
 * something interesting.
 *
 */

char *
obscure(string)
char *string;
{
    unsigned char *cp, *newstr;

    cp = newstr = (unsigned char *)strdup(string);

    while (*cp)
       *cp++ ^= 0xff;

    return((char *)newstr);
}


int 
pop_apop (p)
POP *p;
{
    register char *cp;
    char    buffer[BUFSIZ];
    register unsigned char *dp;
    unsigned char *ep,
		   digest[16];
    struct passwd *pw;
    char apop_dir[BUFSIZ];
    struct stat st;
    datum   key,
	    ddatum;
    DBM	   *db;
    MD5_CTX mdContext;
    int f;

    (void)strncpy(p->user, p->pop_parm[1], sizeof(p->user));
    p->user[sizeof(p->user)-1] = 0;

    pop_log(p, LOG_INFO, "apop \"%s\"", p->user);

#ifdef AUTHFILE
    if (checkauthfile(p->user) != 0)
        return (pop_msg(p,POP_FAILURE,
            "Permission denied.",p->user));
#endif

#ifdef NONAUTHFILE
    if (checknonauthfile(p->user) != 0)
        return (pop_msg(p,POP_FAILURE,
            "Permission denied.",p->user));
#endif

    if (((pw = getpwnam (p->user)) == NULL) || (pw->pw_passwd == NULL) ||
	 (*pw->pw_passwd == '\0')) {
	return (pop_auth_fail(p,POP_FAILURE, pwerrmsg ,p->user));
    }

    if(pw->pw_uid == 0)
	    return (pop_auth_fail(p,POP_FAILURE,
		    "User %s login denied.", p->user));

    if ((db = dbm_open (APOP, O_RDONLY, 0)) == NULL)
	return (pop_auth_fail(p,POP_FAILURE, 
		"POP authorization DB not available (%s)",
			p->user, strerror(errno)));

    (void) strncpy(apop_dir, APOP, sizeof(apop_dir) - 5);
#if defined(BSD44_DBM)
    (void) strcat(apop_dir, ".db");
#else
    (void) strcat(apop_dir, ".dir");
#endif
    if (stat (apop_dir, &st) != -1
	    && (st.st_mode & 0777) != 0600) {
	dbm_close (db);
	return(pop_auth_fail(p, POP_FAILURE, 
		"POP authorization DB has wrong mode (0%o)",
			st.st_mode & 0777));
    }
    f = open(apop_dir, O_RDONLY);
    if(f == -1) {
	int e = errno;
	dbm_close (db);
	return(pop_auth_fail(p, POP_FAILURE, 
		"unable to lock POP authorization DB (%s)",
				strerror(e)));
    }
    if (flock (f, LOCK_SH) == -1) {
	int e = errno;
	(void) close(f);
	dbm_close (db);
	return(pop_auth_fail(p, POP_FAILURE, 
		"unable to lock POP authorization DB (%s)",
				strerror(e)));
    }
    key.dsize = strlen (key.dptr = p->user) + 1;
    ddatum = dbm_fetch (db, key);
    if (ddatum.dptr == NULL) {
	(void) close(f);
	dbm_close (db);
	return(pop_auth_fail(p, POP_FAILURE, "not authorized"));
    }

    dbm_close (db);
    (void) close(f);

    MD5Init(&mdContext);
    MD5Update(&mdContext, (unsigned char *)p->md5str, strlen(p->md5str));
    MD5Update(&mdContext, (unsigned char *)obscure(ddatum.dptr), (ddatum.dsize - 1));
    MD5Final(digest, &mdContext);

    cp = buffer;
    for (ep = (dp = digest) + sizeof digest / sizeof digest[0];
	     dp < ep;
	     cp += 2)
	(void) sprintf (cp, "%02x", *dp++ & 0xff);
    *cp = '\0';

    if (strcmp(p->pop_parm[2], buffer)) {
	return(pop_auth_fail(p, POP_FAILURE, "authentication failure"));
    }

#ifdef DEBUG
    if (p->debug)
	pop_log(p, LOG_NOTICE, "APOP authentication ok for \"%s\"", p->user);
#endif

    if (genpath(p) < 0)
	return(pop_msg(p, POP_FAILURE, "Unable to create temporary drop name"));

    /*  Make a temporary copy of the user's maildrop */
    /*    and set the group and user id */
    if (pop_dropcopy(p, pw) != POP_SUCCESS) return (POP_FAILURE);

    /*  Initialize the last-message-accessed number */
    p->last_msg = 0;

    /*  Authorization completed successfully */
    return (pop_msg (p,POP_SUCCESS,
        "%s has %d message(s) (%d octets).",
            p->user,p->msg_count,p->drop_size));
}

#endif /* APOP */


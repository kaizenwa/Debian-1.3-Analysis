/* 
 * update.c	Common update routines for normal passwd file and
 *		J.F. Haugh's shadow password suite.
 *
 * Copyright 1994, 1995 Olaf Kirch, <okir@monad.swb.de>
 *
 * This program is covered by the GNU General Public License, version 2.
 * It is provided in the hope that it is useful. However, the author
 * disclaims ALL WARRANTIES, expressed or implied. See the GPL for details.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>
#include <time.h>
#include <pwd.h>
#ifdef SHADOWPWD
#include <shadow.h>
#endif

#include <syslog.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <errno.h>

#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include "yppasswd.h"

#ifndef _PATH_PASSWD
# define _PATH_PASSWD		"/etc/passwd"
#endif
#ifndef _PATH_SHADOW
# define _PATH_SHADOW		"/etc/shadow"
#endif
#ifndef _PATH_SHELLS
# define _PATH_SHELLS		"/etc/shells"
#endif
#ifdef DEBUG
# undef  _PATH_PASSWD
# define _PATH_PASSWD		"/tmp/passwd"
# undef  _PATH_SHADOW
# define _PATH_SHADOW		"/tmp/shadow"
#endif

/* Set to 1 if you want to chack against the root password.
 */
#define CHECKROOT   1

/* How often to retry locking the passwd file...
 */
#define MAX_RETRIES 5

static char	path_passwd_old[PATH_MAX] = { '\0', };
static char	path_ptmp[PATH_MAX] = { '\0', };
char *		path_passwd = _PATH_PASSWD;
char *		path_shadow = _PATH_SHADOW;

#ifndef svc_getcaller
/* This is a cheat. */
#define svc_getcaller(x) 	((struct sockaddr_in *) (x)->xp_rtaddr.buf)
#endif
#define xprt_addr(xprt)		(svc_getcaller(xprt)->sin_addr)
#define xprt_port(xprt)		ntohs(svc_getcaller(xprt)->sin_port)
void reaper( int sig);

/*===============================================================*
 * Argument validation. Avoid \n... (ouch).
 * We can't use isprint, because people may use 8bit chars which
 * aren't recognized as printable in the default locale.
 *===============================================================*/
static int
validate_string(char *what, char *str)
{
    while (*str && *str != ':' && !iscntrl(*str)) str++;
    if (*str == '\0')
	return 1;
    syslog(LOG_ALERT, "Invalid characters in %s argument: \"%s\"",
			what, str);
    return 0;
}

static int
validate_args(struct xpasswd *pw)
{
    if (pw->pw_name[0] == '-' || pw->pw_name[0] == '+') {
	syslog(LOG_ALERT, "attempt to modify NIS passwd entry \"%s\"",
			pw->pw_name);
	return 0;
    }

    return validate_string("password", pw->pw_passwd)
       &&  validate_string("shell", pw->pw_shell)
       &&  validate_string("gecos", pw->pw_gecos);
}

static int
shell_ok(char *shell)
{
    char	buffer[256];
    FILE *	fp;

    if ((fp = fopen(_PATH_SHELLS, "r")) == NULL) {
	syslog(LOG_WARNING, "can't open %s", _PATH_SHELLS);
	return 0;
    }
    while (fgets(buffer, 255, fp) != NULL) {
	buffer[255] = '\0';
	if (!strncmp(buffer, shell, strcspn(buffer, " \t\n"))) {
	    fclose(fp);
	    return 1;
	}
    }

    fclose(fp);
    return 0;
}

#if CHECKROOT
static int
password_ok(char *plain, char *crypted, char *root)
{
    if (*crypted == 0) return 1;
    if (strcmp(crypt(plain, crypted), crypted) == 0) return 1;
    if (strcmp(crypt(plain, root), root) == 0) return 1;

    return 0;
}
#endif

/*===============================================================*
 * The /etc/passwd update handler
 *===============================================================*/
int *
yppasswdproc_pwupdate_1(yppasswd *yppw, struct svc_req *rqstp)
{
    struct xpasswd *newpw;	/* passwd struct passed by the client */
    struct passwd *pw;		/* passwd struct obtained from getpwent() */
    int		retries, gotit, fd, c;
    int		chsh = 0, chfn = 0;
    FILE	*oldfp, *newfp;
    static int	res;
    char	logbuf[255];
#if CHECKROOT
    char	rootpass[32];

    if ((pw = getpwnam("root")) != NULL)
	strcpy(rootpass, pw->pw_passwd);
    else
	strcpy(rootpass, "x");
#endif

    if (path_passwd_old[0] == '\0') {
	char	*sp;
    	strcpy(path_passwd_old, path_passwd);
	strcat(path_passwd_old, ".OLD");
    	strcpy(path_ptmp, path_passwd);
	if ((sp = strrchr(path_ptmp, '/')) == NULL)
		sp = path_ptmp - 1;
    	strcpy(sp + 1, "ptmp");
    }

    newpw = &yppw->newpw;
    res = 1;

    sprintf(logbuf, "update %.12s (uid=%d) from host %s",
			    yppw->newpw.pw_name,
			    yppw->newpw.pw_uid,
			    inet_ntoa(xprt_addr(rqstp->rq_xprt)));

    if (!validate_args(newpw)) {
        syslog(LOG_ALERT, "%s failed", logbuf);
        return &res;
    }

    /* Lock the passwd file. We retry several times. Maybe it would be
     * better to just return an error condition and have the client reattempt
     * instead? This procedure is already slow enough...
     */
    retries = 0;
    while ((fd = open(path_ptmp, O_CREAT|O_WRONLY|O_EXCL, 0644)) < 0
      && errno == EEXIST && retries < MAX_RETRIES) {
        sleep (1);
        retries++;
    }

    if (retries == MAX_RETRIES) {
        syslog(LOG_NOTICE, "%s failed", logbuf);
        syslog(LOG_NOTICE, "password file locked");
        return &res;
    }

    if (fd < 0 || (newfp = fdopen(fd, "w")) == NULL) {
        syslog(LOG_ERR, "%s failed", logbuf);
        syslog(LOG_ERR, "Can't create %s: %m", path_ptmp);
        close (fd);
        return &res;
    }

    /* Open the passwd file for reading. We can't use getpwent and friends
     * here, because they go through the YP maps, too.
     */
    if ((oldfp = fopen(path_passwd, "r")) == NULL) {
        syslog(LOG_ERR, "%s failed", logbuf);
        syslog(LOG_ERR, "Can't open %s: %m", path_passwd);
        fclose (newfp);
        return &res;
    }

    gotit = 0;

    /*
     * Loop over all passwd entries
     */
    while((pw = fgetpwent(oldfp)) != NULL) {

        /* check if this is the uid we want to change. A few
         * sanity checks added for consistency.
         */
        if (newpw->pw_uid == pw->pw_uid && newpw->pw_gid == pw->pw_gid
         && !strcmp(newpw->pw_name, pw->pw_name) && !gotit) {

            /* Check the password.
             */
#if CHECKROOT
	    if (!password_ok(yppw->oldpass, pw->pw_passwd, rootpass)) {
#else
            if (pw->pw_passwd[0] != '\0' &&
		strcmp(crypt(yppw->oldpass, pw->pw_passwd), pw->pw_passwd)) {
#endif
        	syslog(LOG_WARNING, "%s rejected", logbuf);
                syslog(LOG_WARNING, "Invalid password.");
                sleep(1);
                break;
            }

            /* set the new passwd, shell, and full name
             */

            if (newpw->pw_passwd[0] != 'x' &&
		newpw->pw_passwd[0] != '*')
			pw->pw_passwd = newpw->pw_passwd;

	    chsh = (strcmp(pw->pw_shell, newpw->pw_shell) != 0);
	    if (chsh) {
		    if (!allow_chsh) {
			syslog(LOG_WARNING, "%s rejected", logbuf);
			syslog(LOG_WARNING, "chsh not permitted");
			break;
		    }
		    if (!shell_ok(newpw->pw_shell)) {
			syslog(LOG_WARNING, "%s rejected", logbuf);
			syslog(LOG_WARNING, "invalid shell: %s", 
					newpw->pw_shell);
			break;
		    }
		    pw->pw_shell = newpw->pw_shell;
	    }
	    chfn = (strcmp(pw->pw_gecos, newpw->pw_gecos) != 0);
	    if (chfn) {
		    if (!allow_chfn) {
			syslog(LOG_WARNING, "%s rejected", logbuf);
			syslog(LOG_WARNING, "chfn not permitted");
			break;
		    }
		    pw->pw_gecos = newpw->pw_gecos;
	    }
            gotit++;
        }

        /* write the passwd entry to /etc/ptmp
         */
        if (putpwent(pw, newfp) < 0) {
            syslog(LOG_ERR, "%s failed", logbuf);
            syslog(LOG_ERR, "Error while writing new password file: %m");
            break;
        }
        /* fflush (newfp); */
    }
    fclose (newfp);
    fclose (oldfp);

    /* Check if we dropped out of the loop because of an error.
     * If so, return an error to the client.
     */
    if (pw != NULL) {
        unlink(path_ptmp);
        return(&res);
    }

    /* Check whether we actually changed anything
     */
    if (!gotit) {
        syslog(LOG_WARNING, "%s failed", logbuf);
        syslog(LOG_WARNING, "User not in password file.");
        unlink(path_ptmp);
        return(&res);
    }

    unlink(path_passwd_old);
    link(path_passwd, path_passwd_old);
    rename(path_ptmp, path_passwd);	/* atomic */
    /* sync(); */

    /* Fork off process to rebuild NIS passwd.* maps.
     */
    if ((c = fork()) < 0) {
	/* Do NOT restore old password file. Someone else may already
	 * be using the new one. */
    	syslog(LOG_ERR, "%s failed", logbuf);
    	syslog(LOG_ERR, "Couldn't fork map update process: %m");
    	return (&res);
    }
    if (c == 0) {
    	execlp(MAP_UPDATE_PATH, MAP_UPDATE, path_passwd, NULL);
    	syslog(LOG_ERR, "Error: couldn't exec map update process: %m");
    	exit(1);
    }

    syslog(LOG_INFO, "%s successful. Password changed.", logbuf);
    if (chsh || chfn) {
    	syslog(LOG_INFO, "Shell %schanged (%s), GECOS %schanged (%s).",
    			chsh? "" : "un", newpw->pw_shell,
    			chfn? "" : "un", newpw->pw_gecos);
    }
    res = 0;

    return (&res);
}

#ifdef SHADOWPWD
/*===============================================================*
 * The /etc/shadow update handler
 *===============================================================*/
int *
yppasswdproc_spwupdate_1(yppasswd *yppw, struct svc_req *rqstp)
{
    struct xpasswd *newpw;	/* passwd struct passed by the client */
    struct spwd *spw;		/* shadow struct obtained from spw_locate */
    static int	res;		/* return value */
    int		retries;	/* number of retries to lock shadow file */
    int		c;
    int		chsh = 0, chfn = 0;
    char	logbuf[255];
#if CHECKROOT
    char	rootpass[32];

    if ((pw = getpwbynam("root")) != NULL)
	strcpy(rootpass, pw->pw_passwd);
    else
	strcpy(rootpass, "x");
#endif


    newpw = &yppw->newpw;
    res = 1;

    sprintf( logbuf, "update %.12s (uid=%d) from host %s",
			    yppw->newpw.pw_name,
			    yppw->newpw.pw_uid,
			    inet_ntoa(xprt_addr(rqstp->rq_xprt)));

    if (!validate_args(newpw)) {
        syslog(LOG_ALERT, "%s failed", logbuf);
        return &res;
    }

    /* Tell libshadow about the location of the password file */
    if (spw_name(path_shadow) < 0) {
        syslog(LOG_ERR, "Can't spw_name(%s): %m", path_shadow);
        syslog(LOG_ALERT, "%s failed", logbuf);
        return &res;
    }

    /* Lock the passwd file. We retry several times. Maybe it would be
     * better to just return an error condition and have the client reattempt
     * instead? This procedure is already slow enough...
     */
    retries = 0;
    while (!spw_lock() && retries < MAX_RETRIES) {
        sleep (1);
        retries++;
    }

    if (retries == MAX_RETRIES) {
        syslog(LOG_NOTICE, "%s failed", logbuf);
        syslog(LOG_NOTICE, "shadow password file locked");
        return &res;
    }


    if (!spw_open(O_RDWR)) {
        syslog(LOG_ERR, "%s failed", logbuf);
        syslog(LOG_ERR, "Can't open %s: %m", path_shadow);
        spw_unlock();
        return &res;
    }


    /*
     * Get old shadow password entry
     */
    if ((spw=spw_locate(newpw->pw_name)) == NULL) {
        syslog(LOG_WARNING, "%s failed", logbuf);
        syslog(LOG_WARNING, "User not in shadow file.");
	goto spwfail;
    }

    /*
     * Check the password.
     */
#if CHECKROOT
    if (!password_ok(yppw->oldpass, spw->sp_pwdp, rootpass)) {
#else
    if (spw->sp_pwdp[0] != '\0' &&
        strcmp(crypt(yppw->oldpass, spw->sp_pwdp), spw->sp_pwdp)) {
#endif
	syslog(LOG_WARNING, "%s rejected", logbuf);
	syslog(LOG_WARNING, "Invalid password.");
	goto spwfail;
    }

    /* Update GECOS and/or shell field. We don't support any fancy things
     * like /etc/shells or checking for restricted shells. Shadow passwords
     * are already messy enough.
     */
#if 1
    {
        struct passwd  *pw;		/* passwd struct from getpwnam */

        if ((pw = getpwnam(newpw->pw_name)) == NULL) {
	    syslog(LOG_WARNING, "%s failed", logbuf);
	    syslog(LOG_WARNING, "User not in passwd file.");
            spw_close(); spw_unlock();
            return (&res);
        }

        /* Check that GECOS or shell have indeed changed. Otherwise,
         * we won't bother with them.
         */
        chsh = (strcmp(newpw->pw_shell, pw->pw_shell) != 0);
        chfn = (strcmp(newpw->pw_gecos, pw->pw_gecos) != 0);
        if (chsh) {
                if (!allow_chsh) {
                    syslog(LOG_WARNING, "%s rejected", logbuf);
                    syslog(LOG_WARNING, "chsh not permitted");
		    goto spwfail;
                }
                if (!shell_ok(newpw->pw_shell)) {
                    syslog(LOG_WARNING, "%s rejected", logbuf);
                    syslog(LOG_WARNING, "invalid shell: %s", 
                                    newpw->pw_shell);
		    goto spwfail;
                }
        }
        if (chfn) {
                if (!allow_chfn) {
                    syslog(LOG_WARNING, "%s rejected", logbuf);
                    syslog(LOG_WARNING, "chfn not permitted");
		    goto spwfail;
                }
        }
        if (chsh || chfn) {
            retries = 0;
            while (!pw_lock() && retries < MAX_RETRIES) {
                sleep (1);
                retries++;
            }

            if (retries == MAX_RETRIES) {
                syslog(LOG_NOTICE, "%s failed", logbuf);
                syslog(LOG_NOTICE, "password file locked");
		goto spwfail;
            }


            if (!pw_open(O_RDWR)) {
                syslog(LOG_ERR, "%s failed", logbuf);
                syslog(LOG_ERR, "Can't open %s: %m", path_shadow);
		pw_unlock();
		goto spwfail;
            }

            /*
             * Get old passwd entry
             */
            if ((pw = pw_locate(newpw->pw_name)) == NULL) {
	        syslog(LOG_ERR, "%s failed", logbuf);
	        syslog(LOG_ERR, "getpwnam succeeds but pw_locate fails?!");
		goto pwfail;
            }

            pw->pw_shell = newpw->pw_shell;
            pw->pw_gecos = newpw->pw_gecos;
            if (!pw_update(pw)) {
	        syslog(LOG_ERR, "%s failed", logbuf);
	        syslog(LOG_ERR, "Error while updating %s",  path_shadow);
		goto pwfail;
            }
    
            pw_close();
            pw_unlock();
        }
    }
#endif

    /* Finally, update the password.
     */
    spw->sp_pwdp=newpw->pw_passwd;

    if (!spw_update(spw)) {
	syslog(LOG_ERR, "%s failed", logbuf);
	syslog(LOG_ERR, "Error while updating %s",  path_shadow);
	goto spwfail;
    }

    spw_close();
    spw_unlock();


    /* Fork off process to rebuild NIS passwd.* maps. If the fork
     * fails, restore old passwd file and return an error.
     */
    if ((c = fork()) < 0) {
    	syslog(LOG_ERR, "%s failed", logbuf);
    	syslog(LOG_ERR, "Couldn't fork map update process: %m");
    	return (&res);
    }
    if (c == 0) {
    	execlp(MAP_UPDATE_PATH, MAP_UPDATE, path_shadow, NULL);
    	syslog(LOG_ERR, "Error: couldn't exec map update process: %m");
    	exit(1);
    }

    syslog(LOG_INFO, "%s successful. Password changed", logbuf);
    if (chsh || chfn) {
    	syslog(LOG_INFO, "Shell %schanged (%s), GECOS %schanged (%s).",
    			chsh? "" : "un", newpw->pw_shell,
    			chfn? "" : "un", newpw->pw_gecos);
    }
    res = 0;

    return (&res);

pwfail:
    pw_close();
    pw_unlock();
spwfail:
    spw_close();
    spw_unlock();
    return &res;
}
#endif

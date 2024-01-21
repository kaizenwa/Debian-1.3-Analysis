/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char copyright[] = "Copyright (c) 1990 Regents of the University of California.\nAll rights reserved.\n";
static char SccsId[] = "@(#)@(#)pop_pass.c	2.3  2.3 4/2/91";
#endif not lint

#include <stdio.h>
#include <sys/types.h>
#if defined(SYSV) || defined(AIX)
# include <string.h>
#else
# include <strings.h>
#endif

#include <pwd.h>
#include "popper.h"

#define	SLEEP_SECONDS 10


/* This error message is vague on purpose to help reduce help improve
   security at the inconvience of administrators and users */

char	*pwerrmsg = "Password supplied for \"%s\" is incorrect.";


#ifdef NONAUTHFILE
checknonauthfile(user) 
     char *user;
{
    char buf[MAXUSERNAMELEN+1];
    FILE *fp;
    char cool = 0;

    if ((fp = fopen(NONAUTHFILE, "r")) != NULL) {
	while (fgets(buf, MAXUSERNAMELEN+1, fp)) {
	    buf[strlen(buf) -1] = '\0';
	    if (!strcmp(buf, user)) {
		fclose(fp);
		return(-1);
	    }
	}

	fclose(fp);
    }
    return(0);
}
#endif

#ifdef AUTHFILE
checkauthfile(user) 
     char *user;
{
    char buf[MAXUSERNAMELEN+1];
    FILE *fp;
    char cool = 0;

    if ((fp = fopen(AUTHFILE, "r")) != NULL) {
	while (fgets(buf, MAXUSERNAMELEN+1, fp)) {
	    buf[strlen(buf) -1] = '\0';
	    if (!strcmp(buf, user)) {
		fclose(fp);
		return(0);
	    }
	}

	fclose(fp);
    }
    return(-1);
}
#endif

int auth_user_kerberos (p, pw)
POP     *   p;
struct passwd *pw;
{
#ifdef KERBEROS
    char lrealm[REALM_SZ];
    int status;
    struct passwd *pwp;
 
    if ((status = krb_get_lrealm(lrealm,1)) == KFAILURE) {
        pop_log(p, LOG_WARNING, "%s: (%s.%s@%s) %s", p->client, kdata.pname, 
		kdata.pinst, kdata.prealm, krb_err_txt[status]);
        return(pop_msg(p,POP_FAILURE,
            "Kerberos error:  \"%s\".", krb_err_txt[status]));
    }

# ifdef KUSEROK
    if (kuserok(&kdata, p->user)) {
        pop_log(p, LOG_WARNING, "%s: (%s.%s@%s): not in %s's ACL.",
	    p->client, kdata.pname, kdata.pinst, kdata.prealm, p->user);
	return(pop_msg(p,POP_FAILURE, "Not in %s's ACL.", p->user));
    }
# else
    if (strcmp(kdata.prealm,lrealm))  {
         pop_log(p, LOG_WARNING, "%s: (%s.%s@%s) realm not accepted.", 
		 p->client, kdata.pname, kdata.pinst, kdata.prealm);
	 return(pop_msg(p,POP_FAILURE,
		     "Kerberos realm \"%s\" not accepted.", kdata.prealm));
    }

    if (strcmp(kdata.pinst,"")) {
        pop_log(p, LOG_WARNING, "%s: (%s.%s@%s) instance not accepted.", 
		 p->client, kdata.pname, kdata.pinst, kdata.prealm);
        return(pop_msg(p,POP_FAILURE,
	      "Must use null Kerberos(tm) instance -  \"%s.%s\" not accepted.",
	      kdata.pname, kdata.pinst));
    }
# endif /* KUSEROK */
    return(POP_SUCCESS);
#else	/* Kerberos not defined */
    return(pop_log(p, LOG_WARNING,
	"Kerberos failure: The popper has not been compiled with -DKERBEROS"));
#endif	/* KERBEROS */
}


#ifdef AUTH
    char *crypt();

#if defined(SUNOS4) && !defined(ISC)

#include <sys/label.h>
#include <sys/audit.h>
#include <pwdadj.h>

static int
auth_user(p, pw)
POP     *   p;
struct passwd *pw;
{
    struct passwd_adjunct *pwadj;

    /*  Look for the user in the shadow password file */
    if ((pwadj = getpwanam(p->user)) == NULL) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE,
		"(shadow) Password supplied for \"%s\" is empty.",p->user));
    } else {
        pw->pw_passwd = (char *)strdup(pwadj->pwa_passwd);
    }

    /*  We don't accept connections from users with null passwords */
    /*  Compare the supplied password with the password file entry */
    if ((pw->pw_passwd == NULL) || (*pw->pw_passwd == '\0') ||
		strcmp(crypt(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd)) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE,pwerrmsg, p->user));
    }

    return(POP_SUCCESS);
}

#endif	/* SUNOS4 */

#if defined(SOLARIS2) || defined(AUX)

#include <shadow.h>

static int
auth_user(p, pw)
POP     *   p;
struct passwd *pw;
{
    register struct spwd * pwd;
    long today;

    /*  Look for the user in the shadow password file */
    if ((pwd = getspnam(p->user)) == NULL) {
        if (!strcmp(pw->pw_passwd, "x")) {	/* This my be a YP entry */
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
	}
    } else {
	today = (long)time((time_t *)NULL)/24/60/60;

	/* Check for expiration date */
	if (pwd->sp_expire > 0 && today > pwd->sp_expire) {
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE,"\"%s\": account expired.",p->user));
	}

	/* Check if password is valid */
	if (pwd->sp_max > 0 && today > pwd->sp_lstchg+pwd->sp_max) {
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE,"\"%s\": account expired.",p->user));
	}

	pw->pw_passwd = (char *)strdup(pwd->sp_pwdp);
	endspent();
    }

    /*  We don't accept connections from users with null passwords */
    /*  Compare the supplied password with the password file entry */
    if ((pw->pw_passwd == NULL) || (*pw->pw_passwd == '\0') ||
		strcmp(crypt(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd)) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p, POP_FAILURE, pwerrmsg, p->user));
    }

    return(POP_SUCCESS);
}

#endif	/* SOLARIS2 || AUX */

#if defined(PTX) || defined(ISC)

#include <shadow.h>

static int
auth_user(p, pw)
POP     *   p;
struct passwd *pw;
{
    register struct spwd * pwd;
    long today;

    /*  Look for the user in the shadow password file */
    if ((pwd = getspnam(p->user)) == NULL) {
        if (!strcmp(pw->pw_passwd, "x")) {	/* This my be a YP entry */
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
	}
    } else {
	pw->pw_passwd = (char *)strdup(pwd->sp_pwdp);
    }

    /*  We don't accept connections from users with null passwords */
    /*  Compare the supplied password with the password file entry */
    if ((pw->pw_passwd == NULL) || (*pw->pw_passwd == '\0') ||
		strcmp(crypt(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd)) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
    }

    return(POP_SUCCESS);
}

#endif	/* PTX */

#if defined(POPSCO) || defined(HPUX10)

#ifdef POPSCO
# include <sys/security.h>
# include <sys/audit.h>
#else
# include <hpsecurity.h>
#endif

#include <prot.h>
#define PASSWD(p)	p->ufld.fd_encrypt

static int
auth_user(p, pw)
POP     *   p;
struct passwd *pw;
{
    register struct pr_passwd *pr;

    if ((pr = getprpwnam(p->user)) == NULL) {
        if (!strcmp(pw->pw_passwd, "x")) {
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
	}

	/*  We don't accept connections from users with null passwords */
	if ((pw->pw_passwd == NULL) || (*pw->pw_passwd == '\0') ||
             (strcmp(bigcrypt(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd) &&
	      strcmp(crypt (p->pop_parm[1], pw->pw_passwd), pw->pw_passwd))) {
	    sleep(SLEEP_SECONDS);
	    return(pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
	}
    } else {
	/*  We don't accept connections from users with null passwords */
	/*  Compare the supplied password with the password file entry */
	if ((PASSWD(pr) == NULL) || (*PASSWD(pr) == '\0')) {
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
	}
	
	if (strcmp(bigcrypt(p->pop_parm[1], PASSWD(pr)), PASSWD(pr)) &&
		    strcmp(crypt(p->pop_parm[1], PASSWD(pr)), PASSWD(pr))) {
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
	}
    }

    return(POP_SUCCESS);
}

#endif	/* POPSCO || HPUX10 */

#ifdef ULTRIX

#include <auth.h>

static int
auth_user(p, pw)
struct passwd  *   pw;
POP     *   p;
{
    AUTHORIZATION *auth, *getauthuid();

    if ((auth = getauthuid(pw->pw_uid)) == NULL) {
        if (!strcmp(pw->pw_passwd, "x")) {	/* This my be a YP entry */
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
	}
    } else {
	pw->pw_passwd = (char *)strdup(auth->a_password);
    }

    /*  We don't accept connections from users with null passwords */
    /*  Compare the supplied password with the password file entry */
    if ((pw->pw_passwd == NULL) || (*pw->pw_passwd == '\0')) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
    }

    if (strcmp(crypt16(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd) &&
		strcmp(crypt(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd)) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
    }

    return(POP_SUCCESS);
}

#endif	/* ULTRIX */

#ifdef OSF1
#include <sys/types.h>
#include <sys/security.h>
#include <prot.h>
#define   PASSWD(p)   (p->ufld.fd_encrypt)
static int
auth_user(p, pw)
POP     *   p;
struct passwd *pw;
{
    register struct pr_passwd *pr;

    if ((pr = getprpwnam(p->user)) == NULL) {
        if (!strcmp(pw->pw_passwd, "x")) {	/* This my be a YP entry */
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
	}
    } else {
	pw->pw_passwd = (char *)strdup(PASSWD(pr));
    }

    /*  We don't accept connections from users with null passwords */
    /*  Compare the supplied password with the password file entry */
    if ((pw->pw_passwd == NULL) || (*pw->pw_passwd == '\0')) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
    }

    if (strcmp(bigcrypt(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd) &&
		strcmp(crypt(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd)) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
    }

    return(POP_SUCCESS);
}

#endif        /* OSF1 */

#ifdef UNIXWARE

#include <shadow.h>

static int
auth_user(p, pw)
struct passwd  *   pw;
POP     *   p;
{
    register struct spwd * pwd;
    long today;

    /*  Look for the user in the shadow password file */
    if ((pwd = getspnam(p->user)) == NULL) {
        if (!strcmp(pw->pw_passwd, "x")) {	/* This my be a YP entry */
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
	}
    } else {
	today = (long)time((time_t *)NULL)/24/60/60;

	/* Check for expiration date */
	if (pwd->sp_expire > 0 && today > pwd->sp_expire) {
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE,"\"%s\": account expired.",p->user));
	}

	/* Check if password is valid */
	if (pwd->sp_max > 0 && today > pwd->sp_lstchg+pwd->sp_max) {
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE,"\"%s\": account expired.",p->user));
	}

	pw->pw_passwd = (char *)strdup(pwd->sp_pwdp);
	endspent();
    }

    /*  We don't accept connections from users with null passwords */
    /*  Compare the supplied password with the password file entry */
    if ((pw->pw_passwd == NULL) || (*pw->pw_passwd == '\0') ||
		strcmp(crypt(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd)) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
    }

    return(POP_SUCCESS);
}

#endif	/* UNIXWARE */

#ifdef LINUX
#include <shadow.h>
 
static int
auth_user(p, pw)
POP     *   p;
struct passwd *pw;
{
    register struct spwd * pwd;
    long today;

    /*  Look for the user in the shadow password file */
    if ((pwd = getspnam(p->user)) == NULL) {
        if (!strcmp(pw->pw_passwd, "x")) {	/* This my be a YP entry */
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
	}
    } else {
	today = (long)time((time_t *)NULL)/24/60/60;

	/* Check for expiration date */
	if (pwd->sp_expire > 0 && today > pwd->sp_expire) {
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE,"\"%s\": account expired.",p->user));
	}

	/* Check if password is valid */
	if (pwd->sp_max > 0 && today > pwd->sp_lstchg+pwd->sp_max) {
	    sleep(SLEEP_SECONDS);
	    return (pop_msg(p,POP_FAILURE,"\"%s\": account expired.",p->user));
	}

	pw->pw_passwd = (char *)strdup(pwd->sp_pwdp);
	endspent();
    }

    /*  We don't accept connections from users with null passwords */
    /*  Compare the supplied password with the password file entry */
    if ((pw->pw_passwd == NULL) || (*pw->pw_passwd == '\0') || 
	    (strcmp(crypt(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd) &&
	     strcmp(pw_encrypt(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd))){
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
    }

    return(POP_SUCCESS);
}

#endif  /* LINUX */

#else	/* NOT AUTH */

char *crypt();

static int
auth_user(p, pw)
POP     *   p;
struct passwd  *   pw;
{
    /*  We don't accept connections from users with null passwords */
    /*  Compare the supplied password with the password file entry */

    if ((pw->pw_passwd == NULL) || (*pw->pw_passwd == '\0') ||
		strcmp(crypt(p->pop_parm[1], pw->pw_passwd), pw->pw_passwd)) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
    }

    return(POP_SUCCESS);
}

#endif	/* AUTH */

/* 
 *  pass:   Obtain the user password from a POP client
 */

#ifdef SECURENISPLUS
/* AUTH gets defined in rpc/... */
# ifdef AUTH
#  undef AUTH
# endif
# include <rpc/rpc.h>
# include <rpc/key_prot.h>
#endif

int pop_pass (p)
POP     *   p;
{
    struct passwd pw, *pwp;
#ifdef CHECK_SHELL
    char *getusershell();
    void endusershell();
    char *shell;
    char *cp;
    int shellvalid;
#endif

#ifdef SECURENISPLUS
    UID_T uid_save;
    char net_name[MAXNETNAMELEN],
	 secretkey[HEXKEYBYTES + 1];

    *secretkey = '\0';
#endif

#ifdef NONAUTHFILE
    /* Is the user not authorized to use POP? */
    if (checknonauthfile(p->user) != 0) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
    }
#endif

#ifdef AUTHFILE
    /* Is the user authorized to use POP? */
    if (checkauthfile(p->user) != 0) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
    }
#endif

    /*  Look for the user in the password file */
    if ((pwp = getpwnam(p->user)) == NULL) {
	sleep(SLEEP_SECONDS);
	return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
    }

    pw = *pwp;

#ifdef SECURENISPLUS
    /*  we must do this keyserv stuff (as well as auth_user()!) as the user */
    uid_save = geteuid();
    seteuid(pw.pw_uid);

    /*  see if DES keys are already known to the keyserv(1m) */
    if (! key_secretkey_is_set()) {
	/*  keys are not known, so we must get the DES keys
	    and register with the keyserv(1m) */

	getnetname(net_name);

	if (getpublickey(net_name, secretkey)) {
	    if (strlen(p->pop_parm[1]) > 8) (p->pop_parm[1])[8] = '\0';

	    if (! getsecretkey(net_name, secretkey, p->pop_parm[1]) ||
							*secretkey == '\0') {
		sleep(SLEEP_SECONDS);
		return (pop_msg(p,POP_FAILURE, pwerrmsg, p->user));
	    }

	    key_setsecret(secretkey);
	    memset(secretkey, '\0', sizeof(secretkey));
	} else {
	    /* if there are no keys defined, we assume that password entry
	       either resides in /etc/shadow or "root" has access to the
	       corresponding NIS+ entry */
	    seteuid(0);
	}
    }
#endif

#ifdef BLOCK_UID
    if (pw.pw_uid <= BLOCK_UID)
	return (pop_msg(p,POP_FAILURE,
			    "Access is blocked for UIDs below %d", BLOCK_UID));
#endif

#ifdef CHECK_SHELL
    /*  Disallow anyone who does not have a standard shell as returned by
     *  getusershell(), unless the sys admin has included the wildcard
     *  shell in /etc/shells.  (default wildcard - /POPPER/ANY/SHELL)
     */
    if ((shell = pw.pw_shell) == NULL || *shell == 0)
/* You can default the shell, but I don't think it's a good idea */
/*	shell = "/usr/bin/sh"; */
	return(pop_msg(p, POP_FAILURE, "No user shell defined"));
    
    for (shellvalid = 0; !shellvalid && ((cp = getusershell()) != NULL);)
	if (!strcmp(cp, WILDCARD_SHELL) || !strcmp(cp, shell))
	     shellvalid = 1;
    endusershell();

    if (!shellvalid)
	return(pop_msg(p, POP_FAILURE, "\"%s\": shell not found.", p->user));
#endif

    if ((p->kerberos ? auth_user_kerberos(p, pw) : auth_user(p, pwp))
							!= POP_SUCCESS) {
	    pop_log(p,POP_PRIORITY,"Failed attempted login to %s from host %s",
							    p->user, p->client);
	return(POP_FAILURE);
    }

#ifdef SECURENISPLUS
    seteuid(uid_save);
#endif

    /*  Make a temporary copy of the user's maildrop */
    /*    and set the group and user id */
    /*    and get information about the maildrop */
    if (pop_dropcopy(p, &pw) != POP_SUCCESS) return (POP_FAILURE);

    /*  Initialize the last-message-accessed number */
    p->last_msg = 0;

    /*  Authorization completed successfully */
    return (pop_msg (p,POP_SUCCESS,
        "%s has %d message%s (%d octets).",
            p->user,p->msg_count, p->msg_count == 1 ? "" : "s", p->drop_size));
}


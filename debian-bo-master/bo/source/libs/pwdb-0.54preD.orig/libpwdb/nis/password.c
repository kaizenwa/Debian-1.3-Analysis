
#include "../_pwdb_internal.h"
/* _pwdb_dup_string and _pwdb_delete_string */
#include "../pwdb/pwdb_public.h" 
#define	SBUFSIZ	64
#define	NFIELDS	7

static	char	NISpwdbuf[BUFSIZ];
static	char	*NISpwdfields[NFIELDS];
static	struct	__pwdb_passwd	NISpwent;
static	int     nis_bound;
static	char	*nis_domain;
static	char	*nis_val;
static	int     nis_vallen;
#define	IS_NISCHAR(c) ((c)=='+')

/**********************************************************
 * These are defines for NIS update code ... 
 **********************************************************/
#define YPPASSWDPROG ((u_long)100009)
#define YPPASSWDVERS ((u_long)1)
#define YPPASSWDPROC_UPDATE ((u_long)1)
/* 
 * The updated password information, plus the old password.
 */
struct __pwdb_yppasswd {
        char *oldpass;
        struct __pwdb_passwd newpw;
};

/*
 * bind_nis - bind to NIS server
 */
static int __pwdbNIS_bind_nis (void)
{
    nis_bound = 0;
    nis_domain = NULL;

    if (yp_get_default_domain (&nis_domain))
        return -1;
    nis_bound = 1;
    return 0;
}

/*
 * sgetpwent - convert a string to a (struct passwd)
 *
 * sgetpwent() parses a string into the parts required for a password
 * structure.  Strict checking is made for the UID and GID fields and
 * presence of the correct number of colons.  Any failing tests result
 * in a NULL pointer being returned.
 *
 * NOTE: This function uses hard-coded string scanning functions for
 *	performance reasons.  I am going to come up with some conditional
 *	compilation glarp to improve on this in the future.
 */

static struct __pwdb_passwd * __pwdbNIS_sgetpwent (char *buf)
{
    register int	i;
    register char	*cp;
    char	*ep;
    int     nis_used = 0;

    /*
     * Copy the string to a static buffer so the pointers into
     * the password structure remain valid.
     */

	strncpy (NISpwdbuf, buf, BUFSIZ);
	NISpwdbuf[BUFSIZ-1] = '\0';

    /*
     * Save a pointer to the start of each colon separated
     * field.  The fields are converted into NUL terminated strings.
     */

	for (cp = NISpwdbuf, i = 0;i < NFIELDS && cp;i++) {
		NISpwdfields[i] = cp;
		while (*cp && *cp != ':')
			++cp;
		if (*cp)
			*cp++ = '\0';
		else
			cp = 0;
	}

	/*
	 * There must be exactly NFIELDS colon separated fields or
	 * the entry is invalid.  Also, the UID and GID must be non-blank.
	 */

	if (i != NFIELDS || *NISpwdfields[2] == '\0' || *NISpwdfields[3] == '\0') {
		if (! IS_NISCHAR (NISpwdfields[0][0]))
			return 0;
		else
			nis_used = 1;
	}

	/*
	 * Each of the fields is converted the appropriate data type
	 * and the result assigned to the password structure.  If the
	 * UID or GID does not convert to an integer value, a NULL
	 * pointer is returned.
	 */

	NISpwent.pw_name = NISpwdfields[0];
	if (IS_NISCHAR (NISpwdfields[0][0]))
		nis_used = 1;
	NISpwent.pw_passwd = NISpwdfields[1];
	if (NISpwdfields[2][0] == '\0' ||
		((NISpwent.pw_uid = strtol (NISpwdfields[2], &ep, 10)) == 0 && *ep)) {
		if (! nis_used)
			return 0;
		else
			NISpwent.pw_uid = -1;
	}
	if (NISpwdfields[3][0] == '\0' ||
		((NISpwent.pw_gid = strtol (NISpwdfields[3], &ep, 10)) == 0 && *ep)) {
		if (! nis_used)
			return 0;
		else
			NISpwent.pw_gid = -1;
	}
	NISpwent.pw_gecos = NISpwdfields[4];
	NISpwent.pw_dir = NISpwdfields[5];
	NISpwent.pw_shell = NISpwdfields[6];

	return (&NISpwent);
}

/*
 * getpwuid - locate the password entry for a given UID
 *
 * getpwuid() locates the first password file entry for the given UID.
 */

struct __pwdb_passwd * __pwdbNIS_getpwuid (uid_t uid)
{
	struct	__pwdb_passwd	*pwd;
	char	buf[BUFSIZ];
    char    *cp;
    char    mapname[] = "passwd.byuid";
        
	/*
	 * Search the passwd.byuid map for this user.
	 */
	if (!nis_bound)
    	__pwdbNIS_bind_nis();
	if (!nis_bound)
        return NULL;

    sprintf (buf, "%d", uid);

    if (yp_match (nis_domain, mapname, buf,
                  strlen (buf), &nis_val, &nis_vallen) == 0) {
        if ((cp = strchr (nis_val, '\n')))
            *cp = '\0';
        pwd = __pwdbNIS_sgetpwent (nis_val);
        if (pwd)
            return pwd;
    }

	return NULL;
}

/*
 * getpwnam - locate the password entry for a given name
 *
 * getpwnam() locates the first password file entry for the given name.
 * If there is a valid DBM file, the DBM files are queried first for
 * the entry.  Otherwise, a linear search is begun of the password file
 * searching for an entry which matches the provided name.
 */

struct __pwdb_passwd * __pwdbNIS_getpwnam (const char * name)
{
	struct	__pwdb_passwd	*pwd;
    char    *cp;
    char    mapname[] = "passwd.byname";
    char    *tname;
        
	/*
	 * Search the passwd.byname map for this user.
	 */
	if (!nis_bound)
		__pwdbNIS_bind_nis ();
	if (!nis_bound)
        return NULL;

    tname=_pwdb_dup_string(name);
    if (yp_match (nis_domain, mapname, tname,
                  strlen (tname), &nis_val, &nis_vallen) == 0) {
        if ((cp = strchr (nis_val, '\n')))
            *cp = '\0';
        pwd = __pwdbNIS_sgetpwent (nis_val);
        tname=_pwdb_delete_string(tname);
        if (pwd)
            return pwd;
	}
    if (tname)
        tname=_pwdb_delete_string(tname);
	return NULL;
}

static bool_t xdr_xpasswd(XDR *xdrs, struct __pwdb_passwd *objp)
{
	if (!xdr_string(xdrs, &objp->pw_name, ~0)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->pw_passwd, ~0)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, (int *)&objp->pw_uid)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, (int *)&objp->pw_gid)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->pw_gecos, ~0)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->pw_dir, ~0)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->pw_shell, ~0)) {
		return (FALSE);
	}
	return (TRUE);
}


static bool_t xdr_yppasswd(XDR *xdrs, struct __pwdb_yppasswd *objp)
{
	if (!xdr_string(xdrs, &objp->oldpass, ~0)) {
		return (FALSE);
	}
	if (!xdr_xpasswd(xdrs, &objp->newpw)) {
		return (FALSE);
	}
	return (TRUE);
}

/*
 * __pwdbNIS_update
 * Updates a NIS entry for an user.
 * Updating (changing) the user login name, uid and/or gid is not
 * currently supported. (I think that some yppasswdd daemons will
 * allow for uid/gid change, but I don't think changing the login
 * name is supported by any of them...
 *
 * Thanks & credit to:
 *     Theo de Raadt <deraadt@fsa.ca> and
 *     Olaf Kirch <okir@monad.swb.de>
 * as this procedure is based on the skeleton provided by their
 * yppasswd program. -- Cristian Gafton
 *
 * Return: 0 means okay...
 */
int __pwdbNIS_update (const char *oldpass, const struct __pwdb_passwd *pwd)
{
    char    mapname[] = "passwd.byname";
    struct  __pwdb_yppasswd yppasswd;    
    struct  timeval timeout;
    CLIENT  *clnt;
    int     retval, status;
    char    *master;
    int     port;
    struct __pwdb_passwd tpwd;
    
    memcpy(&tpwd, pwd, sizeof(struct __pwdb_passwd));
    
    /* sanity check */
    if (!pwd)
        return -1;

	/*
	 * Search the passwd.byname map for this user.
	 */
	if (!nis_bound)
		__pwdbNIS_bind_nis ();
	if (!nis_bound)
        return -1;

    if ((retval = yp_master(nis_domain, mapname, &master)) != 0)
      return -1;

    port = getrpcport(master, YPPASSWDPROG, YPPASSWDPROC_UPDATE, IPPROTO_UDP);
    if (port==0)
        return -1;
    if (port >= IPPORT_RESERVED)
        return -1;

    if (!yp_match (nis_domain, mapname, tpwd.pw_name,
                  strlen (pwd->pw_name), &nis_val, &nis_vallen) == 0)
        /* the user does not seem to exist in NIS maps... */
        return -1;

    /* Initialize password information */
    yppasswd.newpw.pw_passwd= tpwd.pw_passwd;
    yppasswd.newpw.pw_name  = tpwd.pw_name;
    yppasswd.newpw.pw_uid   = tpwd.pw_uid;
    yppasswd.newpw.pw_gid   = tpwd.pw_gid;
    yppasswd.newpw.pw_gecos = tpwd.pw_gecos;
    yppasswd.newpw.pw_dir   = tpwd.pw_dir;
    yppasswd.newpw.pw_shell = tpwd.pw_shell;
    yppasswd.oldpass        = _pwdb_dup_string(oldpass);

    clnt = clnt_create(master, YPPASSWDPROG, YPPASSWDVERS, "udp" );
    clnt->cl_auth = authunix_create_default();
    bzero( (char*)&status, sizeof(status) );
    timeout.tv_sec = 25;
    timeout.tv_usec = 0;
    retval = clnt_call( clnt, YPPASSWDPROC_UPDATE,
                 (xdrproc_t) xdr_yppasswd, (char*) &yppasswd,
                 (xdrproc_t) xdr_int,      (char*) &status,
                 timeout );
    auth_destroy( clnt->cl_auth );
    clnt_destroy( clnt );
    yppasswd.oldpass=_pwdb_delete_string(yppasswd.oldpass);
    
    /* if there was an error... */
    if (retval)
        return retval;
    if (status)
        return -1;
    return 0;
}


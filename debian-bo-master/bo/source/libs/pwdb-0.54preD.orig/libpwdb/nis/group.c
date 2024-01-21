
/* THIS DOES NOT WORK. STILL IN CODE WRITTING PHASE */


#include "../_pwdb_internal.h"
/* _pwdb_dup_string and _pwdb_delete_string */
#include "../pwdb/pwdb_public.h"

#define	NFIELDS	4
#define	MAXMEM	1024

static  char    NISgrpbuf[4*BUFSIZ];
static  char  * NISgrpfields[NFIELDS];
static  char  * NISmembers[MAXMEM+1];
static  struct  __pwdb_group NISgrent;

static	int     nis_bound;
static	char	*nis_domain;
static	char	*nis_val;
static	int     nis_vallen;
#define	IS_NISCHAR(c) ((c)=='+')


/*
 * bind_nis - bind to NIS server
 */

static int __pwdbNIS_bind_nis (void) {
    nis_bound = 0;
    nis_domain = NULL;
	if (yp_get_default_domain (&nis_domain))
		return -1;
	nis_bound = 1;
	return 0;
}

/*
 * list - turn a comma-separated string into an array of (char *)'s
 *
 *	list() converts the comma-separated list of member names into
 *	an array of character pointers.
 *
 *	WARNING: I profiled this once with and without strchr() calls
 *	and found that using a register variable and an explicit loop
 *	works best.  For large /etc/group files, this is a major win.
 */

static char ** list (register char *s) {
	int	nmembers = 0;

	while (s && *s) {
		NISmembers[nmembers++] = s;
		while (*s && *s != ',')
			s++;

		if (*s)
			*s++ = '\0';
	}
	NISmembers[nmembers] = (char *) 0;
	return NISmembers;
}

static struct __pwdb_group * __pwdbNIS_sgetgrent (const char *buf) {
	int	    i;
	char    *cp;
    int     nis_used = 0;
    
	strncpy (NISgrpbuf, buf, sizeof(NISgrpbuf));
	NISgrpbuf[sizeof(NISgrpbuf) - 1] = '\0';
	if ((cp = strrchr (NISgrpbuf, '\n')))
		*cp = '\0';

	for (cp = NISgrpbuf, i = 0;i < NFIELDS && cp;i++) {
		NISgrpfields[i] = cp;
		if ((cp = strchr (cp, ':')))
			*cp++ = 0;
	}
	if (i < (NFIELDS-1) || *NISgrpfields[2] == '\0')
		if (! IS_NISCHAR (NISgrpfields[0][0]))
			return 0;
		else
			nis_used = 1;
	NISgrent.gr_name = NISgrpfields[0];
	NISgrent.gr_passwd = NISgrpfields[1];
	NISgrent.gr_gid = atoi (NISgrpfields[2]);
	NISgrent.gr_mem = list (NISgrpfields[3]);
 
	return (&NISgrent);
}

/*
 * getgrgid - locate the group entry for a given GID
 *
 * getgrgid() locates the first group file entry for the given GID.
 */

struct __pwdb_group * __pwdbNIS_getgrgid (gid_t gid) {
	struct	__pwdb_group	*grp;
	char	buf[BUFSIZ];
    char    *cp;
    char    mapname[] = "group.bygid";
    
    /*
	 * Search the group.byuid map for this group.
     */

    if (! nis_bound)
        __pwdbNIS_bind_nis ();
    if (!nis_bound)
        return NULL;
    
    sprintf (buf, "%d", gid);

	if (yp_match (nis_domain, mapname, buf,
	              strlen (buf), &nis_val, &nis_vallen) == 0) {
        if ((cp = strchr (nis_val, '\n')))
            *cp = '\0';

        grp = __pwdbNIS_sgetgrent (nis_val);
        if (grp)
            return grp;
	}
	return NULL;
	
}

/*
 * getgrnam - locate the group entry for a given name
 *
 * getgrnam() locates the first group file entry for the given name.
 */

struct __pwdb_group * __pwdbNIS_getgrnam (const char * name) {
	struct	__pwdb_group	*grp;
    char    *cp;
    char    mapname[] = "group.byname";
    char    *tname;
        
    /*
	 * Search the group.byname map for this group.
     */

    if (! nis_bound)
        __pwdbNIS_bind_nis ();
    if (!nis_bound)
        return NULL;
    tname=_pwdb_dup_string(name);
    if (! yp_match (nis_domain, mapname, tname,
                    strlen (tname), &nis_val, &nis_vallen)) {
        if ((cp = strchr (nis_val, '\n')))
            *cp = '\0';
        grp = __pwdbNIS_sgetgrent (nis_val);
        tname=_pwdb_delete_string(tname);
        if (grp)
            return grp;
	}
    if (tname)
    	tname=_pwdb_delete_string(tname);
    return NULL;
}



#include "../_pwdb_internal.h"
#include <pwdb/_pwdb_macros.h>

#define	NFIELDS	4
#define	MAXMEM	1024

static	char	grpbuf[4*BUFSIZ];
static	char	*grpfields[NFIELDS];
static	char	*members[MAXMEM+1];
static	struct	__pwdb_group	grent;

static	FILE	*grpfp;

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

static char ** list (register char   *s)
{
    int	nmembers = 0;

    while (s && *s) {
	members[nmembers++] = s;
	while (*s && *s != ',')
	    s++;

	if (*s)
	    *s++ = '\0';
    }
    members[nmembers] = (char *) 0;
    return members;
}

struct __pwdb_group * __pwdb_sgetgrent (const char *buf)
{
    int	i;
    char	*cp;

    D(("called."));

    strncpy (grpbuf, buf, sizeof grpbuf);
    grpbuf[sizeof grpbuf - 1] = '\0';
    if ((cp = strrchr (grpbuf, '\n')))
	*cp = '\0';

    for (cp = grpbuf, i = 0;i < NFIELDS && cp;i++) {
	grpfields[i] = cp;
	if ((cp = strchr (cp, ':')))
	    *cp++ = 0;
    }
    if (i < (NFIELDS-1) || *grpfields[2] == '\0')
	return 0;
    grent.gr_name = grpfields[0];
    grent.gr_passwd = grpfields[1];
    grent.gr_gid = atoi (grpfields[2]);
    grent.gr_mem = list (grpfields[3]);

    return (&grent);
}

/*
 * fgetgrent - get a group file entry from a stream
 *
 * fgetgrent() reads the next line from a group file formatted stream
 * and returns a pointer to the group structure for that line.
 */

struct __pwdb_group * __pwdb_fgetgrent (FILE *fp) 
{
    char buf[BUFSIZ*4];
    char *cp;

    D(("called."));

    if (__pwdb_fgetsx (buf, sizeof buf, fp) != (char *) 0)
    {
	if ((cp = strchr (buf, '\n')))
	    *cp = '\0';
	return (__pwdb_sgetgrent (buf));
    }
    return 0;
}

/*
 * endgrent - close a group file
 *
 * endgrent() closes the group file if open.
 */

void __pwdb_endgrent (void)
{
    D(("called."));

    if (grpfp)
	if (fclose (grpfp))
	    return;
    grpfp = 0;
    return;
}

/*
 * getgrent - get a group entry from the group file
 *
 * getgrent() opens the group file, if not already opened, and reads
 * a single entry.  NULL is returned if any errors are encountered reading
 * the group file.
 */

struct __pwdb_group * __pwdb_getgrent (void)
{
    D(("called."));

    if (! grpfp) 
	__pwdb_setgrent();
    return __pwdb_fgetgrent (grpfp);
}

/*
 * getgrgid - locate the group entry for a given GID
 *
 * getgrgid() locates the first group file entry for the given GID.
 */

struct __pwdb_group * __pwdb_getgrgid (gid_t gid)
{
    struct __pwdb_group	*grp;

    D(("called."));

    __pwdb_setgrent();

    /*
     * Search for an entry which matches the GID.  Return the
     * entry when a match is found.
     */

    while ((grp = __pwdb_getgrent ())) {
	if (grp->gr_gid == gid)
	    break;
    }
    return grp;
}

/*
 * getgrnam - locate the group entry for a given name
 *
 * getgrnam() locates the first group file entry for the given name.
 */

struct __pwdb_group * __pwdb_getgrnam (const char *name)
{
    struct __pwdb_group	*grp;

    D(("called."));

    __pwdb_setgrent();

    /*
     * Search for an entry which matches the name.  Return the
     * entry when a match is found.
     */

    while ((grp = __pwdb_getgrent ())) {
	if (strcmp (grp->gr_name, name) == 0)
	    break;
    }
    return grp;
}

/*
 * setgrent - open the group file
 *
 * setgrent() opens the system group file. The system group file is
 * rewound if it was open already.
 */

void __pwdb_setgrent (void)
{
    D(("called."));

    if (! grpfp) {
	if (! (grpfp = fopen (__PWDB_GROUP_FILE, "r")))
	    return;
    } else {
	if (fseek (grpfp, (off_t) 0L, SEEK_SET) != 0)
	    return;
    }

    return;
}


/*
 * putgrent - output standard group entry in text form
 *
 * putgrent() converts the contents of a (struct group) to text and
 * writes the result to the given stream.  This is the logical
 * opposite of fgetgrent.
 */

int __pwdb_putgrent(const struct __pwdb_group *g, FILE *f)
{
    int	i;
    char	*cp;
    char *buf, *rbuf;
    size_t size;

    D(("called."));

    if (! g || ! f)
        return -1;

    /* be very safe first */
    if (!g->gr_name || !g->gr_passwd)
        return -1;
        
    size = strlen(g->gr_name) + strlen(g->gr_passwd) + 10;
            /* for two colons and an integer 10 should be enough */
    if (size < 1024)
        size=1024; /* alloc plenty, avoid reallocs if two small */
    if (!(buf = malloc(size)))
        return -1;

    bzero(buf,size);
    
    sprintf (buf, "%s:%s:%d:", g->gr_name, g->gr_passwd, g->gr_gid);
    if (g->gr_mem) {
        cp = buf + strlen(buf);
        for (i = 0;g->gr_mem[i];i++) {
	        if ((cp - buf) + strlen (g->gr_mem[i]) + 2 >= size) {
        		size += size;
		        rbuf = realloc(buf, size);
                if (!rbuf) {
                    free(buf);
                    return -1;  /* No more hope.. */
                }
                buf = rbuf;
            }

            if (i > 0) {
                strcpy (cp, ",");
                cp++;
            }
            strcpy (cp, g->gr_mem[i]);
            cp = strchr (cp, '\0');
        }
        strcat (cp, "\n");
    } else
        strcat (buf, "\n");

    if (__pwdb_fputsx (buf, f) == EOF || ferror (f)) {
        free(buf);
        return -1;
    }

    free(buf);
    return 0;
}

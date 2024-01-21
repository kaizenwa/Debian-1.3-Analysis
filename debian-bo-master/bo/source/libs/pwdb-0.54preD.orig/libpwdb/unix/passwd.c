
/* Here are the functions for /etc/passwd file manipulation
 * These functions are not NIS aware, nor DBM/NDBM aware
 * Support for these extensions should go into a separate
 * source file, in separate functions
 */

#include "../_pwdb_internal.h"

#define	NFIELDS	7

static	FILE	*pwdfp;

static	char	pwdbuf[BUFSIZ];
static	char	*pwdfields[NFIELDS];
static	struct	__pwdb_passwd	pwent;

static  passwd_file_end=1; /* if we reach the end of file */

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

struct __pwdb_passwd * __pwdb_sgetpwent (const char *buf)
{
     register int	i;
     register char	*cp;
     char	*ep;
     char	**fields;
     char	*buffer;
     struct	__pwdb_passwd	*pwd;

     fields = pwdfields;
     buffer = pwdbuf;
     pwd = &pwent;

     /*
      * Copy the string to a static buffer so the pointers into
      * the password structure remain valid.
      */

     strncpy (buffer, buf, BUFSIZ);
     pwdbuf[BUFSIZ-1] = '\0';

     /*
      * Save a pointer to the start of each colon separated
      * field.  The fields are converted into NUL terminated strings.
      */

     for (cp = buffer, i = 0;i < NFIELDS && cp;i++) {
	  fields[i] = cp;
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
     if (i != NFIELDS || *fields[2] == '\0' || *fields[3] == '\0')
	  return 0;

     /*
      * Each of the fields is converted the appropriate data type
      * and the result assigned to the password structure.  If the
      * UID or GID does not convert to an integer value, a NULL
      * pointer is returned.
      */

     pwd->pw_name = fields[0];
     pwd->pw_passwd = fields[1];
     if (fields[2][0] == '\0' ||
	 ((pwd->pw_uid = strtol (fields[2], &ep, 10)) == 0 && *ep)) {
	  return 0;
     }
     if (fields[3][0] == '\0' ||
	 ((pwd->pw_gid = strtol (fields[3], &ep, 10)) == 0 && *ep)) {
	  return 0;
     }
     pwd->pw_gecos = fields[4];
     pwd->pw_dir = fields[5];
     pwd->pw_shell = fields[6];

     return (pwd);
}

/*
 * fgetpwent - get a password file entry from a stream
 *
 * fgetpwent() reads the next line from a password file formatted stream
 * and returns a pointer to the password structure for that line.
 */

struct __pwdb_passwd * __pwdb_fgetpwent (FILE *fp)
{
    char	buf[BUFSIZ];

    passwd_file_end = 1;
    if (fgets (buf, BUFSIZ, fp) != (char *) 0) {
        buf[strlen (buf) - 1] = '\0';
        passwd_file_end = 0; 
        return (__pwdb_sgetpwent (buf));
    }
    return 0;
}

/*
 * endpwent - close a password file
 *
 * endpwent() closes the password file if open.  if autoshadowing is
 * enabled the system must also end access to the shadow files since
 * the user is probably unaware it was ever accessed.
 */

void __pwdb_endpwent ()
{
    if (pwdfp)
        if (fclose (pwdfp))
            return;
    pwdfp = 0;
    return;
}

/*
 * getpwent - get a password entry from the password file
 *
 * getpwent() opens the password file, if not already opened, and reads
 * a single entry.  NULL is returned if any errors are encountered reading
 * the password file.
 */

struct __pwdb_passwd * __pwdb_getpwent ()
{
    if (! pwdfp) {
        __pwdb_setpwent ();
        if (! pwdfp)
            return 0;
    }

    return __pwdb_fgetpwent (pwdfp);
}

/*
 * putpwent - Output a (struct passwd) in character format
 *
 *	putpwent() writes out a (struct passwd) in the format it appears
 *	in in flat ASCII files.
 */

int __pwdb_putpwent(const struct __pwdb_passwd *p, FILE *stream)
{
    if (!p || !stream)
        return -1;
        
    return (fprintf (stream, "%s:%s:%d:%d:%s:%s:%s\n",
                     p->pw_name,
                     p->pw_passwd,
                     p->pw_uid,
                     p->pw_gid,
                     p->pw_gecos,
                     p->pw_dir,
                     p->pw_shell
                    ) < 0);
}

/*
 * getpwuid - locate the password entry for a given UID
 *
 * getpwuid() locates the first password file entry for the given UID.
 * A linear search is begun of the password file
 * searching for an entry which matches the provided UID.
 */

struct __pwdb_passwd * __pwdb_getpwuid (uid_t uid)
{
    struct  __pwdb_passwd *pwd;

    __pwdb_setpwent();
    if (! pwdfp)
        return 0;
    /*
     * Search for an entry which matches the UID.  Return the
     * entry when a match is found.
     */
    do {
        pwd = __pwdb_getpwent();
        if (pwd && (pwd->pw_uid == uid))
            break;
    } while ( pwd || !passwd_file_end);
    
    __pwdb_endpwent();
    return pwd;
}

/*
 * getpwnam - locate the password entry for a given name
 *
 * getpwnam() locates the first password file entry for the given name.
 * A linear search is begun of the password file
 * searching for an entry which matches the provided name.
 */

struct __pwdb_passwd * __pwdb_getpwnam (const char *name)
{
    struct __pwdb_passwd *pwd;

    __pwdb_setpwent();
    if (! pwdfp)
        return NULL;

    /*
     * Search for an entry which matches the name.  Return the
     * entry when a match is found.
     */
    do {
        pwd = __pwdb_getpwent();
        if (pwd && (strcmp (pwd->pw_name, name) == 0))
            break;
    } while ( pwd || !passwd_file_end );

    __pwdb_endpwent(); 
    return pwd;
}

/*
 * setpwent - open the password file
 *
 * setpwent() opens the system password file. The system 
 * password file is rewound if it was open already.
 */

void __pwdb_setpwent ()
{
    if (! pwdfp) {
        if (! (pwdfp = fopen (__PWDB_PASSWD_FILE, "r")))
            return;
    } else {
        if (fseek (pwdfp, (off_t) 0L, SEEK_SET) != 0) {
            fclose (pwdfp);
            pwdfp = 0;
            return;
        }
    }

    return;
}


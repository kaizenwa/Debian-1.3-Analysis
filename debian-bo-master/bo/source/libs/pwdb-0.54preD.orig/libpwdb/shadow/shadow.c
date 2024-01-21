/*
 * In this file are defined the functions to operate on /etc/shadow file
 * No NIS support, no DBM/NDBM/DB support. These extensions should go
 * into a separate file, separate functions.
 */

#include "../_pwdb_internal.h"

static	FILE	*shadow;
static	char	spwbuf[BUFSIZ];
static	struct	__pwdb_spwd	spwd;

static int shadow_file_end=1;      /* XXX: AGM added this */

#define	FIELDS	9
#define	OFIELDS	5

/*
 * setspent - initialize access to shadow text 
 */

void __pwdb_setspent (void)
{
     if (shadow)
	  rewind(shadow);
     else
	  shadow = fopen(__PWDB_SHADOW_FILE, "r");

     return;
}

/*
 * endspent - terminate access to shadow text 
 */

void __pwdb_endspent (void)
{
     if (shadow)
	  (void) fclose (shadow);

     shadow = (FILE *) 0;
    
     return; 
}

/*
 * sgetspent - convert string in shadow file format to (struct spwd *)
 */

struct __pwdb_spwd *__pwdb_sgetspent (const char *string)
{
     char	*fields[FIELDS];
     char	*cp;
     char	*cpp;
     int	i;

     /*
      * Copy string to local buffer.  It has to be tokenized and we
      * have to do that to our private copy.
      */

     strncpy (spwbuf, string, BUFSIZ-1);
     spwbuf[BUFSIZ-1] = '\0';

     if ((cp = strrchr (spwbuf, '\n')))
	  *cp = '\0';

     /*
      * Tokenize the string into colon separated fields.  Allow up to
      * FIELDS different fields.
      */

     for (cp = spwbuf, i = 0;*cp && i < FIELDS;i++) {
	  fields[i] = cp;
	  while (*cp && *cp != ':')
	       cp++;

	  if (*cp)
	       *cp++ = '\0';
     }

     /*
      * It is acceptable for the last SVR4 field to be blank.  This
      * results in the loop being terminated early.  In which case,
      * we just make the last field be blank and be done with it.
      */

     if (i == (FIELDS-1))
	  fields[i++] = cp;

     if ((cp && *cp) || (i != FIELDS && i != OFIELDS))
	  return 0;

     /*
      * Start populating the structure.  The fields are all in
      * static storage, as is the structure we pass back.  If we
      * ever see a name with '+' as the first character, we try
      * to turn on NIS processing.
      */

     spwd.sp_namp = fields[0];
     spwd.sp_pwdp = fields[1];

     /*
      * Get the last changed date.  For all of the integer fields,
      * we check for proper format.  It is an error to have an
      * incorrectly formatted number, unless we are using NIS.
      */

     if ((spwd.sp_lstchg = strtol (fields[2], &cpp, 10)) == 0 && *cpp) {
	  return 0;
     } else if (fields[2][0] == '\0')
	  spwd.sp_lstchg = -1;

     /*
      * Get the minimum period between password changes.
      */

     if ((spwd.sp_min = strtol (fields[3], &cpp, 10)) == 0 && *cpp) {
	  return 0;
     } else if (fields[3][0] == '\0')
	  spwd.sp_min = -1;

     /*
      * Get the maximum number of days a password is valid.
      */

     if ((spwd.sp_max = strtol (fields[4], &cpp, 10)) == 0 && *cpp) {
	  return 0;
     } else if (fields[4][0] == '\0')
	  spwd.sp_max = -1;

     if (i == OFIELDS) {
	  spwd.sp_warn = spwd.sp_inact = spwd.sp_expire =
	       spwd.sp_flag = -1;

	  return &spwd;
     }

     /*
      * Get the number of days of password expiry warning.
      */

     if ((spwd.sp_warn = strtol (fields[5], &cpp, 10)) == 0 && *cpp) {
	  return 0;
     } else if (fields[5][0] == '\0')
	  spwd.sp_warn = -1;

     /*
      * Get the number of days of inactivity before an account is
      * disabled.
      */

     if ((spwd.sp_inact = strtol (fields[6], &cpp, 10)) == 0 && *cpp) {
	  return 0;
     } else if (fields[6][0] == '\0')
	  spwd.sp_inact = -1;

     /*
      * Get the number of days after the epoch before the account is
      * set to expire.
      */

     if ((spwd.sp_expire = strtol (fields[7], &cpp, 10)) == 0 && *cpp) {
	  return 0;
     } else if (fields[7][0] == '\0')
	  spwd.sp_expire = -1;

     /*
      * This field is reserved for future use.  But it isn't supposed
      * to have anything other than a valid integer in it.
      */

     if ((spwd.sp_flag = strtol (fields[8], &cpp, 10)) == 0 && *cpp) {
	  return 0;
     } else if (fields[8][0] == '\0')
	  spwd.sp_flag = -1;

     return (&spwd);
}

/*
 * fgetspent - get an entry from a /etc/shadow formatted stream
 */

struct __pwdb_spwd * __pwdb_fgetspent (FILE *fp)
{
	char	buf[BUFSIZ];
	char	*cp;

	shadow_file_end = 1;                   /* safety first */
	if (! fp)
		return (0);

	if (fgets (buf, sizeof buf, fp) != (char *) 0)
	{
		if ((cp = strchr (buf, '\n')))
			*cp = '\0';
		shadow_file_end = 0;                /* we read something */
		return (__pwdb_sgetspent (buf));
	}
	return 0;
}

/*
 * getspent - get a (struct spwd *) from the current shadow file
 */

struct __pwdb_spwd * __pwdb_getspent ()
{
	if (! shadow)
		__pwdb_setspent ();

	return (__pwdb_fgetspent (shadow));
}

/*
 * putspent - Output a (struct spwd) in character format
 *
 *      putspent() writes out a (struct spwd) in the format it appears
 *      in in flat ASCII files.
 */

int __pwdb_putspent(const struct __pwdb_spwd *sp, FILE *stream)
{
	if (! stream || ! sp)
		return -1;

	return (fprintf(stream, "%s:%s:%ld:%ld:%ld:%ld:%ld:%ld:%ld\n",
	                sp->sp_namp,
	                sp->sp_pwdp,
	                sp->sp_lstchg,
	                sp->sp_min,
	                sp->sp_max,
	                sp->sp_warn,
	                sp->sp_inact,
	                sp->sp_expire,
        	        sp->sp_flag
        	       ) < 0);
}

/*
 * getspnam - get a shadow entry by name
 */

struct __pwdb_spwd * __pwdb_getspnam (const char *name)
{
    struct __pwdb_spwd *sp;

    __pwdb_setspent();  /* rewind shadow file */

    /* loop through entries until correct one found or end of
     * file reached
     */
    do {
        sp = __pwdb_getspent();
        if (sp && (strcmp(name, sp->sp_namp) == 0) )
            break;
    } while ( sp || !shadow_file_end );

    __pwdb_endspent();	/* close shadow file */
    return (sp);        /* return entry found (?) */
}

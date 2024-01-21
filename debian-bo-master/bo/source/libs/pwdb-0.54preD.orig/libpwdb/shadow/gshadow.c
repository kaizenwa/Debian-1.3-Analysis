
/* The shadow group functions */

#include "../_pwdb_internal.h"

#define	MAXMEM	1024

static	FILE	*shadow;
static	char	sgrbuf[BUFSIZ*4];
static	char	*members[MAXMEM+1];
static	char	*admins[MAXMEM+1];
static	struct	__pwdb_sgrp	sgroup;

#define	FIELDS	4

static char ** list (char *s, char **l) 
{
	int	nmembers = 0;

	while (s && *s) {
		l[nmembers++] = s;
		if ((s = strchr (s, ',')))
			*s++ = '\0';
	}
	l[nmembers] = (char *) 0;
	return l;
}

void __pwdb_setsgent (void)
{
     if (shadow)
	  rewind (shadow);
     else
	  shadow = fopen(__PWDB_SGROUP_FILE, "r");
     return;
}

void __pwdb_endsgent (void)
{
     if (shadow)
	  (void) fclose (shadow);

     shadow = (FILE *) 0;
     return;
}

struct __pwdb_sgrp * __pwdb_sgetsgent (const char *string)
{
     char	*fields[FIELDS];
     char	*cp;
     int	i;

     strncpy (sgrbuf, string, (int) sizeof sgrbuf - 1);
     sgrbuf[sizeof sgrbuf - 1] = '\0';

     if ((cp = strrchr (sgrbuf, '\n')))
	  *cp = '\0';

     /*
      * There should be exactly 4 colon separated fields.  Find
      * all 4 of them and save the starting addresses in fields[].
      */

     for (cp = sgrbuf, i = 0;i < FIELDS && cp;i++) {
	  fields[i] = cp;
	  if ((cp = strchr (cp, ':')))
	       *cp++ = '\0';
     }

     /*
      * If there was an extra field somehow, or perhaps not enough,
      * the line is invalid.
      */

     if (cp || i != FIELDS)
	  return 0;

     sgroup.sg_name = fields[0];
     sgroup.sg_passwd = fields[1];
     sgroup.sg_adm = list (fields[2], admins);
     sgroup.sg_mem = list (fields[3], members);

     return &sgroup;
}

/*
 * fgetsgent - convert next line in stream to (struct sgrp)
 *
 * fgetsgent() reads the next line from the provided stream and
 * converts it to a (struct sgrp).  NULL is returned on EOF.
 */

struct __pwdb_sgrp * __pwdb_fgetsgent (FILE  *fp)
{
	char	buf[sizeof sgrbuf];
	char	*cp;

	if (! fp)
		return (0);

	if (__pwdb_fgetsx (buf, sizeof buf, fp) != (char *) 0)
	{
		if ((cp = strchr (buf, '\n')))
			*cp = '\0';
		return (__pwdb_sgetsgent (buf));
	}
	return 0;
}

/*
 * getsgent - get a single shadow group entry
 */

struct __pwdb_sgrp * __pwdb_getsgent (void)
{
	if (! shadow)
		__pwdb_setsgent ();

	return (__pwdb_fgetsgent (shadow));
}

/*
 * getsgnam - get a shadow group entry by name
 */

struct __pwdb_sgrp * __pwdb_getsgnam (const char *name)
{
	struct	__pwdb_sgrp	*sgrp;

	__pwdb_setsgent ();

	while ((sgrp = __pwdb_getsgent ()) != (struct __pwdb_sgrp *) 0) {
		if (strcmp (name, sgrp->sg_name) == 0)
			break;
	}
	if (sgrp)
		return sgrp;
	return (0);
}

/*
 * putsgent - output shadow group entry in text form
 *
 * putsgent() converts the contents of a (struct sgrp) to text and
 * writes the result to the given stream.  This is the logical
 * opposite of fgetsgent.
 */

int __pwdb_putsgent (const struct __pwdb_sgrp *sgrp, FILE *fp)
{
    int	i;
    char	*cp;
    char *buf, *rbuf;
    size_t size;

    if (! sgrp || ! fp)
        return -1;

    /* be very safe first */
    if (!sgrp->sg_name || !sgrp->sg_passwd)
        return -1;
        
    size=1024; /* alloc plenty, avoid reallocs if two small */
    if (!(buf = malloc(size)))
        return -1;

    bzero(buf,size);
    
    sprintf (buf, "%s:%s:", sgrp->sg_name, sgrp->sg_passwd);
    cp = buf + strlen(buf);
	/*
	 * Copy the administrators, separating each from the other
	 * with a ",".
	 */
	for (i = 0;sgrp->sg_adm[i];i++) {
        if ((cp - buf) + strlen (sgrp->sg_adm[i]) + 2 >= size) {
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

        strcpy (cp, sgrp->sg_adm[i]);
        cp += strlen (cp);
    }
	*cp++ = ':';

	/*
	 * Now do likewise with the group members.
	 */
	for (i = 0;sgrp->sg_mem[i];i++) {
        if ((cp - buf) + strlen (sgrp->sg_mem[i]) + 2 >= size) {
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

        strcpy (cp, sgrp->sg_mem[i]);
        cp += strlen (cp);
    }
	*cp++ = '\n';
	*cp = '\0';

	/*
	 * Output using the function which understands the line
	 * continuation conventions.
	 */
    if (__pwdb_fputsx (buf, fp) == EOF || ferror (fp)) {
        free(buf);
        return -1;
    }

    free(buf);
    return 0;
}

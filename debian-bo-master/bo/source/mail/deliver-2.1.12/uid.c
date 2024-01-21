/* $Id: uid.c,v 1.3 1993/10/28 16:49:51 chip Exp $
 *
 * I wish the System V "id" program were universally available; but it
 * isn't, so I've written this replacement.
 *
 * usage: uid [-options]
 *
 * Default action is to print one line in the manner of "id":
 *    uid=201(chip) gid=50(group) euid=0(root) egid=0(root) [groups=1(x),2(y)]
 * (Note that the "euid" and "egid" entries are not output if they are
 * the same as the "uid" and "gid" values, and that the "groups" entry
 * not output if there are no supplemental groups.)
 *
 * If an option string is specified, it disables the normal behavior in
 * favor of displaying id information, one per line, in the order that
 * the options appear in the option string.  Legal options are:
 *      u       real uid
 *      g       real gid
 *      U       effective uid
 *      G       effective gid
 *      S       supplemental groups
 *
 * NOTE: This program is not a paragon of good style.
 *       It's just something I needed for a Makefile.
 *
 * $Log: uid.c,v $
 * Revision 1.3  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.2  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.1  91/05/13  18:36:55  chip
 * Initial revision
 * 
 */

#include <stdio.h>
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include "config.h"
#include "misc.h"

#undef NULL
#define NULL 0

extern struct passwd *getpwuid();
extern struct group *getgrgid();

char *progname = "uid";

char *uid_desc();
char *gid_desc();

int
main(argc, argv)
int argc;
char **argv;
{
    int uid, gid, euid, egid;
    int c, lines, errcount;
#ifdef GROUP_VECTOR
    GRVEC_T *groups;
    int maxgroups, ngroups, g;
#endif

    uid = getuid();
    gid = getgid();
    euid = geteuid();
    egid = getegid();

#ifdef GROUP_VECTOR
    if ((maxgroups = MAXGROUPS()) == -1)
	maxgroups = 0;
    if (maxgroups <= 0)
    {
	groups = NULL;
	ngroups = 0;
    }
    else
    {
	if ((groups = (GRVEC_T *) malloc(sizeof(GRVEC_T) * maxgroups)) == NULL)
	{
	    fprintf(stderr, "uid: out of memory?!\n");
	    exit(1);
	}
	ngroups = GETGROUPS(maxgroups, groups);
    }
#endif	/* GROUP_VECTOR */

    errcount = 0;
    lines = 0;

    while ((c = getopt(argc, argv, "ugUGS")) != EOF)
    {
	switch (c)
	{
	case 'u':
	    (void) printf("%s\n", uid_desc(uid));
	    ++lines;
	    break;

	case 'g':
	    (void) printf("%s\n", gid_desc(gid));
	    ++lines;
	    break;

	case 'U':
	    (void) printf("%s\n", uid_desc(euid));
	    ++lines;
	    break;

	case 'G':
	    (void) printf("%s\n", gid_desc(egid));
	    ++lines;
	    break;

	case 'S':
#ifdef GROUP_VECTOR
	    for (g = 0; g < ngroups; ++g)
	    {
		if (g)
		    (void) putchar(',');
		(void) printf("%s", gid_desc(groups[g]));
	    }
	    if (ngroups)
		(void) putchar('\n');
#endif	/* GROUP_VECTOR */
	    ++lines;
	    break;

	case '?':
	    ++errcount;
	    break;
	}
    }

    if (errcount)
    {
	(void) fprintf(stderr, "usage: uid [-ugUGS]\n");
	exit(1);
    }

    if (lines == 0)
    {
	(void) printf("uid=%s", uid_desc(uid));
	(void) printf(" gid=%s", gid_desc(gid));

	if (euid != uid)
	    (void) printf(" euid=%s", uid_desc(euid));
	if (egid != gid)
	    (void) printf(" egid=%s", gid_desc(egid));

#ifdef GROUP_VECTOR
	if (ngroups)
	{
	    (void) printf(" groups=");
	    for (g = 0; g < ngroups; ++g)
	    {
		if (g)
		    (void) putchar(',');
		(void) printf("%s", gid_desc(groups[g]));
	    }
	}
#endif	/* GROUP_VECTOR */

	(void) printf("\n");
    }

    exit(0);
    /* NOTREACHED */
}

char *
uid_desc(uid)
int uid;
{
    struct passwd *pw;
    static char buf[80];

    (void) sprintf(buf, "%d", uid);
    if ((pw = getpwuid(uid)) != NULL)
	(void) sprintf(buf + strlen(buf), "(%s)", pw->pw_name);

    return buf;
}

char *
gid_desc(gid)
int gid;
{
    struct group *gr;
    static char buf[80];

    (void) sprintf(buf, "%d", gid);
    if ((gr = getgrgid(gid)) != NULL)
	(void) sprintf(buf + strlen(buf), "(%s)", gr->gr_name);

    return buf;
}

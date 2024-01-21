/* $Id: context.c,v 1.3 1991/08/26 17:42:02 chip Exp $
 *
 * User context manager.
 * This module exists for efficiency reasons; I could just call getpwnam()
 * every time I need context info.
 *
 * $Log: context.c,v $
 * Revision 1.3  1991/08/26  17:42:02  chip
 * Don't declare unused pw/gr functions.
 *
 * Revision 1.2  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.1  91/05/13  18:36:55  chip
 * Initial revision
 * 
 */

#include "deliver.h"
#include <pwd.h>
#include <grp.h>

extern struct passwd *getpwnam();
extern struct passwd *getpwuid();

extern void setgrent();
extern struct group *getgrent();
extern struct group *getgrnam();
extern struct group *getgrgid();

/*
 * Local functions.
 */

static CONTEXT *new_context();

/*
 * Local data.
 */

static CONTEXT *ctlist;		/* Chain of CONTEXT structures.		*/

/*----------------------------------------------------------------------
 * Look up a context by user name.
 */

CONTEXT *
name_context(name)
char *name;
{
    struct passwd *pw;
    CONTEXT *ct;

    for (ct = ctlist; ct; ct = ct->ct_next)
    {
	if (strcmp(ct->ct_name, name) == 0)
	    return ct;
    }

    if ((pw = getpwnam(name)) == NULL)
	return NULL;

    return new_context(pw);
}

/*----------------------------------------------------------------------
 * Look up a context by user ID.
 */

CONTEXT *
uid_context(uid)
int uid;
{
    struct passwd *pw;
    CONTEXT *ct;

    for (ct = ctlist; ct; ct = ct->ct_next)
    {
	if (ct->ct_uid == uid)
	    return ct;
    }

    if ((pw = getpwuid(uid)) == NULL)
	return NULL;

    return new_context(pw);
}

/*----------------------------------------------------------------------
 * Local function -- create a new context structure and return
 * its address.
 */

static CONTEXT *
new_context(pw)
struct passwd *pw;
{
    CONTEXT *ct;
#ifdef GROUP_VECTOR
    struct group *gr;
#endif

    ct = talloc(CONTEXT, 1);
    ct->ct_uid = pw->pw_uid;
    ct->ct_gid = pw->pw_gid;
    ct->ct_name = copystr(pw->pw_name);
    ct->ct_home = copystr(pw->pw_dir);

#ifdef GROUP_VECTOR
    ct->ct_numgroups = 0;
    ct->ct_groups = (maxgroups > 0)
	? talloc(GRVEC_T, maxgroups)
	: (GRVEC_T *) NULL;

    setgrent();
    while ((gr = getgrent()) != NULL)
    {
	int i;

	for (i = 0; gr->gr_mem[i]; ++i)
	{
	    if (strcmp(ct->ct_name, gr->gr_mem[i]) == 0
		&& ct->ct_numgroups < maxgroups)
	    {
		ct->ct_groups[ct->ct_numgroups++] = gr->gr_gid;
		break;
	    }
	}
    }
#endif	/* GROUP_VECTOR */

    ct->ct_next = ctlist;
    ctlist = ct;

    return ct;
}

/*----------------------------------------------------------------------
 * Report whether is is possible or not to go from the given
 * effective uid/real uid/real gid to the given context.
 */

int
ok_context(euid, ruid, rgid, ct)
int euid, ruid, rgid;
CONTEXT *ct;
{
    if (!ct)
	return FALSE;

    if (euid == 0
	|| ((ruid == ct->ct_uid) && (rgid == ct->ct_gid)))
	return TRUE;
    else
	return FALSE;
}

/*----------------------------------------------------------------------
 * Look up a group name by ID.
 */

char *
group_name(id)
int id;
{
    struct group *grp;

    if ((grp = getgrgid(id)) == NULL)
	return NULL;

    return grp->gr_name;
}

/*----------------------------------------------------------------------
 * Look up a group ID by name.
 */

int
group_id(name)
char *name;
{
    struct group *grp;

    if ((grp = getgrnam(name)) == NULL)
	return -1;

    return grp->gr_gid;
}

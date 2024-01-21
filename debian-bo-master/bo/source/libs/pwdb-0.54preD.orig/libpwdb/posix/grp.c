/*
 * $Id$
 *
 * This is a POSIX interface to libpwdb. It defines the two
 * getpw... functions as simple interfaces to the pwdb_locate()
 * functions.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pwdb/pwdb_public.h>

#include <pwdb/_pwdb_macros.h>

#define DEFAULT_MEMBERS     ""

static struct group *transcribe(const char *group, int gid)
{
    static int called_once=0;
    static struct group _grp;
    static struct group grp;
    static int member_size=0;
    const struct pwdb_entry
	*pwe_group=NULL,
	*pwe_gid=NULL,
	*pwe_users=NULL;
    const struct pwdb *pw=NULL;
    char *members;
    int retval;

    if (group == PWDB_NAME_UNKNOWN && gid == (gid_t) PWDB_ID_UNKNOWN)
	return NULL;

    /* identify the group */

    retval = pwdb_locate("group", PWDB_DEFAULT, group, (int) gid, &pw);
    if (retval != PWDB_SUCCESS || pw == NULL) {
	D(("could not find group/gid"));
	return NULL;
    }

    /* Forget last call's values */

    if (called_once) {
	_grp.gr_name  = _pwdb_delete_string(_grp.gr_name);
	if (member_size > 0) {
	    memset(_grp.gr_mem, 0, member_size);
	    member_size = 0;
	    free(_grp.gr_mem);
	    _grp.gr_mem = NULL;
	}
	called_once = 0;
    }

    /*
     * obtain all the info about the user - if they have a name, a uid
     * and a gid, then we set defaults for the other bits.
     */

    if (  pwdb_get_entry(pw, "group", &pwe_group) != PWDB_SUCCESS
	 || pwdb_get_entry(pw, "gid", &pwe_gid) != PWDB_SUCCESS ) {
	pwdb_entry_delete(&pwe_group);
	pwdb_entry_delete(&pwe_gid);

	return NULL;
    }

    _grp.gr_gid = * (const gid_t *) pwe_gid->value;
    pwdb_entry_delete(&pwe_gid);
    _grp.gr_name = _pwdb_dup_string((const char *)pwe_group->value);
    pwdb_entry_delete(&pwe_group);

    if (_grp.gr_name == NULL) {
	return NULL;
    }

    /* find the membership list for this group */
    if ( pwdb_get_entry(pw, "users", &pwe_users) != PWDB_SUCCESS ) {
	members = _pwdb_dup_string(DEFAULT_MEMBERS);
    } else {
	members = _pwdb_dup_string((const char *)pwe_users->value);
	pwdb_entry_delete(&pwe_users);
    }

    called_once = 1;

    if (members == NULL) {
	return NULL;
    }

    /* we want to make a block of memory which is a list of (char *)
       and then the individual char[]'s that fill the pointers. */

    {
	void *tmp;
	char **ptrs, *users;
	int nusers=0;

	/* count user entries:- nusers */
	for (users=members; *users; ) {
	    if (*users++ != ',') {
		++nusers;
		while (*users && *users++ != ',');
	    }
	}

	/* need to allocate (char *)[1+nusers] + strlen(members) */

	member_size = (1+nusers)*sizeof(char *) + 1+strlen(members);
	tmp = malloc( member_size );
	if (tmp == NULL) {
	    member_size = 0;
	    members = _pwdb_delete_string(members);
	    return NULL;
	}

	_grp.gr_mem = ptrs = (char **)tmp;
	users = (1+nusers)*sizeof(char *) + (char *)tmp;
	tmp = NULL;
	strcpy(users, members);
	_pwdb_delete_string(members);

	/* break line of members into a series of tokens, place
           pointers in leading array */

	while (*users) {
	    if (*users != ',') {
		++nusers;
		*ptrs++ = users;
		while (*users && *++users != ',');
		if (*users) {
		    *users++ = '\0';            /* replace ',' with '\0' */
		}
	    } else {
		++users;
	    }
	}
	*ptrs = NULL;
    }

    /*
     * return copy - will want to free() text next call so keep
     * original because it references the actual memory and will not
     * be overwritten by the user. POSIX should have required (const
     * char *) items here, to avoid trouble from the user attempting
     * to overwrite these entries.
     */

    memcpy(&grp, &_grp, sizeof(grp));
    return (&grp);
}

struct group *pwdb_posix_getgrnam(const char *group)
{
    struct group *group_entry;

    pwdb_start();                               /* initialize pwdb */
    group_entry = transcribe(group, PWDB_ID_UNKNOWN);
    pwdb_end();                                 /* terminate pwdb */
    return group_entry;
}

struct group *pwdb_posix_getgrgid(gid_t gid)
{
    struct group *gid_entry;

    pwdb_start();                               /* initialize pwdb */
    gid_entry = transcribe(PWDB_NAME_UNKNOWN, (int) gid);
    pwdb_end();                                 /* terminate pwdb */
    return gid_entry;
}

/*
 * $Log$
 */

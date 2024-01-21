/*
#ident	"@(#)smail/src:RELEASE-3_2:pwcache.c,v 1.8 1996/02/28 14:26:36 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * pwcache:
 *	manage a passwd and group entry cache.
 *
 * The mailer can make a large number of acesses to the passwd and
 * group files while processing a message.  To increase the efficiency
 * of this, we maintain a cache of entries read from each of these
 * files.
 */
#include <stdio.h>
#include <pwd.h>
#include <grp.h>
#include <ctype.h>
#include "defs.h"
#include "smail.h"
#include "hash.h"
#include "alloc.h"
#ifndef DEPEND
# include "extern.h"
#endif

/* functions imported from the C library */
#ifndef ANSI_C
extern struct passwd *getpwnam();
extern struct passwd *getpwuid();
extern struct group *getgrnam();
extern struct group *getgrgid();
#endif

/* functions local to this file */
static void fill_pw_cache();
static void fill_gr_cache();

/*
 * There are separate caches for four mappings:
 *
 * uid->username gid->groupname username->uid groupname->gid
 */

/* number of entries in each cache */
#ifdef	SMALL_MEMORY
# define N_CACHE	5
#else
# define N_CACHE	31
#endif

/* buffer size to hold user and group names (with ending nul byte) */
#define PW_SIZE		15

/* invalid names will begin with this char */
#define BAD_NAME	('@')

/* invalid id's are set to this unlikely id */
#define BAD_ID		(-10101)

/* entries for the passwd file cache */
struct	pw_cache {
    char pw_name[PW_SIZE];		/* name of user */
    int	pw_uid;				/* uid of user */
    int	pw_gid;				/* base gid of user */
    char *pw_dir;			/* home directory */
    int	size_pw_dir;			/* size allocated for home directory */
};

/* entries for the group file cache */
struct	gr_cache {
    char gr_name[PW_SIZE];		/* name of group */
    int gr_gid;				/* gid of group */
};

/* we rely on these being initialized to nulls */
static struct hash_table *uid_cache = NULL; /* uid->username cache */
static struct hash_table *gid_cache = NULL; /* gid->groupname cache */
static struct hash_table *user_cache = NULL; /* username->uid cache */
static struct hash_table *group_cache = NULL; /* groupname->gid cache */


/*
 * uid->username, through cache
 */
struct passwd *
getpwbyuid(uid)
    int uid;
{
    struct pw_cache *entp, ent;
    static struct passwd ret;
    static char uid_buf[50];		/* big enough to hold a number */

    if (uid_cache == NULL) {
	/* XXX - hash table should be associated with a block */
	uid_cache = new_hash_table(N_CACHE,
				   (struct block *)NULL,
				   HASH_DEFAULT);
    }
    (void) sprintf(uid_buf, "%d", uid);
    if (lookup_in_hash(uid_buf, (char **)&entp, (int *)NULL,
		       uid_cache) == ALREADY_HASHED)
    {
	if (entp->pw_name[0] == BAD_NAME) {
	    return NULL;
	}
	ret.pw_name = entp->pw_name;
	ret.pw_uid = entp->pw_uid;
	ret.pw_gid = entp->pw_gid;
	ret.pw_dir = entp->pw_dir;
	return &ret;
    } else {
	register struct passwd *pw;

	setpwent();
	pw = getpwuid(uid);
	if (pw == NULL) {
	    ent.pw_uid = uid;
	    ent.pw_name[0] = BAD_NAME;
	    (void) add_to_hash(uid_buf, (char *)&ent, sizeof(ent), uid_cache);
	    return NULL;
	}
	if (user_cache == NULL) {
	    /* XXX - hash table should be associated with a block */
	    user_cache = new_hash_table(N_CACHE,
					(struct block *)NULL,
					HASH_DEFAULT);
	}
	fill_pw_cache(pw);
	return pw;
    }
}

/*
 * gid->groupname, through cache
 */
struct group *
getgrbygid(gid)
    int gid;
{
    struct gr_cache *entp, ent;
    static struct group ret;
    static char gid_buf[50];		/* big enough to hold a number */

    if (gid_cache == NULL) {
	/* XXX - hash table should be associated with a block */
	gid_cache = new_hash_table(N_CACHE,
				   (struct block *)NULL,
				   HASH_DEFAULT);
    }
    (void) sprintf(gid_buf, "%d", gid);
    if (lookup_in_hash(gid_buf, (char **)&entp, (int *)NULL,
		       gid_cache) == ALREADY_HASHED)
    {
	if (entp->gr_name[0] == BAD_NAME) {
	    return NULL;
	}
	ret.gr_name = entp->gr_name;
	ret.gr_gid = entp->gr_gid;
	return &ret;
    } else {
	register struct group *gr;

	setgrent();
	gr = getgrgid(gid);
	if (gr == NULL) {
	    ent.gr_gid = gid;
	    ent.gr_name[0] = BAD_NAME;
	    (void) add_to_hash(gid_buf, (char *)&ent, sizeof(ent), gid_cache);
	    return NULL;
	}
	if (group_cache == NULL) {
	    /* XXX - hash table should be associated with a block */
	    group_cache = new_hash_table(N_CACHE,
					 (struct block *)NULL,
					 HASH_DEFAULT);
	}
	fill_gr_cache(gr);
	return gr;
    }
}

/*
 * username->uid, through cache
 */
struct passwd *
getpwbyname(user)
    char *user;
{
    struct pw_cache *entp, ent;
    static struct passwd ret;

    if ((int)strlen(user) > PW_SIZE - 1) {
	return NULL;
    }
    if (user_cache == NULL) {
	/* XXX - hash table should be associated with a block */
	user_cache = new_hash_table(N_CACHE,
				    (struct block *)NULL,
				    HASH_DEFAULT);
    }
    if (lookup_in_hash(user, (char **)&entp, (int *)NULL,
		       user_cache) == ALREADY_HASHED)
    {
	if (entp->pw_uid == BAD_ID) {
	    return NULL;
	}
	ret.pw_name = entp->pw_name;
	ret.pw_uid = entp->pw_uid;
	ret.pw_gid = entp->pw_gid;
	ret.pw_dir = entp->pw_dir;
	return &ret;
    } else {
	register struct passwd *pw;
	register char *p;

	(void) strncpy(ent.pw_name, user, PW_SIZE - 1);
	for (p = ent.pw_name; *p; p++) {
	    *p = lowercase(*p);
	}

	setpwent();
	pw = getpwnam(ent.pw_name);
	if (pw == NULL) {
	    (void) strncpy(ent.pw_name, user, PW_SIZE - 1);
	    ent.pw_uid = BAD_ID;
	    (void) add_to_hash(user, (char *)&ent, sizeof(ent), user_cache);
	    return NULL;
	}
	if (uid_cache == NULL) {
	    /* XXX - hash table should be associated with a block */
	    uid_cache = new_hash_table(N_CACHE,
				       (struct block *)NULL,
				       HASH_DEFAULT);
	}
	fill_pw_cache(pw);
	return pw;
    }
}

/*
 * groupname->gid, through cache
 */
struct group *
getgrbyname(group)
    char *group;
{
    struct gr_cache *entp, ent;
    static struct group ret;

    if ((int)strlen(group) > PW_SIZE - 1) {
	return NULL;
    }
    if (group_cache == NULL) {
	/* XXX - hash table should be associated with a block */
	group_cache = new_hash_table(N_CACHE,
				     (struct block *)NULL,
				     HASH_DEFAULT);
    }
    if (lookup_in_hash(group, (char **)&entp, (int *)NULL,
		       group_cache) == ALREADY_HASHED)
    {
	if (entp->gr_gid == BAD_ID) {
	    return NULL;
	}
	ret.gr_gid = entp->gr_gid;
	ret.gr_name = entp->gr_name;
	return &ret;
    } else {
	register struct group *gr;
	register char *p;

	(void) strncpy(ent.gr_name, group, PW_SIZE - 1);
	for (p = ent.gr_name; *p; p++) {
	    *p = lowercase(*p);
	}

	setgrent();
	gr = getgrnam(ent.gr_name);
	if (gr == NULL) {
	    (void) strncpy(ent.gr_name, group, PW_SIZE - 1);
	    ent.gr_gid = BAD_ID;
	    (void) add_to_hash(group, (char *)&ent, sizeof(ent), group_cache);
	    return NULL;
	}
	if (gid_cache == NULL) {
	    /* XXX - hash table should be associated with a block */
	    gid_cache = new_hash_table(N_CACHE,
				       (struct block *)NULL,
				       HASH_DEFAULT);
	}
	fill_gr_cache(gr);
	return gr;
    }
}

/* fill the uid and username caches from a passwd file entry */
static void
fill_pw_cache(pw)
    register struct passwd *pw;
{
    struct pw_cache ent;		/* fill this with pw info */
    char uid_buf[50];			/* big enough to hold a number */
    register char *p;
    int size_pw_dir;

    (void) strncpy(ent.pw_name, pw->pw_name, PW_SIZE - 1);
    for (p = ent.pw_name; *p; p++) {
	*p = lowercase(*p);
    }
    ent.pw_uid = pw->pw_uid;
    ent.pw_gid = pw->pw_gid;
    size_pw_dir = strlen(pw->pw_dir) + 1;
    ent.pw_dir = xmalloc(size_pw_dir);
    (void) strcpy(ent.pw_dir, pw->pw_dir);
    (void) sprintf(uid_buf, "%d", pw->pw_uid);
    (void) add_to_hash(uid_buf, (char *)&ent, sizeof(ent), uid_cache);
    (void) add_to_hash(ent.pw_name, (char *)&ent, sizeof(ent), user_cache);
}

/* fill the gid and groupname caches from a group file entry */
static void
fill_gr_cache(gr)
    register struct group *gr;
{
    struct gr_cache ent;		/* fill this with gr info */
    char gid_buf[50];			/* big enough to hold a number */
    register char *p;

    (void) strncpy(ent.gr_name, gr->gr_name, PW_SIZE - 1);
    for (p = ent.gr_name; *p; p++) {
	*p = lowercase(*p);
    }
    ent.gr_gid = gr->gr_gid;
    (void)sprintf(gid_buf, "%d", gr->gr_gid);
    (void) add_to_hash(gid_buf, (char *)&ent, sizeof(ent), gid_cache);
    (void) add_to_hash(ent.gr_name, (char *)&ent, sizeof(ent), group_cache);
}

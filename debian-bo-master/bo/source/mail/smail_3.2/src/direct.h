/* @(#) direct.h,v 1.7 1996/02/16 23:01:29 woods Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

#ifndef DIRECT_H
#define DIRECT_H

/*
 * direct.h:
 *	interface file for direct.c.  Also, types and macros for
 *	use by director drivers.
 */

/* structure of a director, as read from the director file */
struct director {
    char *name;				/* name of director */
    char *driver;			/* name of driver */
    struct director *succ;		/* next director in the list */
    long flags;				/* boolean flag values */
    char *owner;			/* string to expand into alias owner */
    char *default_user;			/* default user name for permissions */
    char *default_group;		/* default group name */
    char *default_home;			/* default home directory */
    char *domains;                      /* domains for this director */
    char *set_user;			/* set user for permissions */
    char *set_group;			/* set group for permissions */
    char *set_home;			/* set home directory */
    char *private;			/* private data storage */
    int default_uid;			/* cache - default uid for perms */
    int default_gid;			/* cache - default gid for perms */
    char *x_default_home;		/* cache - expanded default home */
    int set_uid;			/* cache - set uid for perms */
    int set_gid;			/* cache - set gid for perms */
    char *x_set_home;			/* cache - expanded set home */
};

/* compiled in director drivers */
struct direct_driver {
    char *name;				/* name of director driver */
    void (*cache)();			/* function to cache director info */
    struct addr *(*driver)();		/* function to perform directing */
    void (*verify)();			/* function to perform verification */
    void (*finish)();			/* function to free resources */
    char *(*builder)();			/* fun to read from director file */
    void (*dumper)();			/* fun to dump configuration */
};

/* flags for the director.flags field */
#define CAUTION_DIRECTOR	0x0001	/* secure source of addresses */
#define NOBODY_DIRECTOR		0x0002	/* use "nobody" if unsecure */
#define CACHED_DIRECTOR		0x0004	/* read in cache data */
#define SENDER_OKAY		0x0008	/* assumes me_too operation */
#define IGNORE_ALIAS_MATCH	0x0010	/* ignore aliases that match parent */
#define USER_IGNORE_CASE	0x0020	/* lower user b4 matching w/mailbox */

#endif	/* DIRECT_H */

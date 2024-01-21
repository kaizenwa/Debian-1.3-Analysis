/* xid.c -- Cached versions of the getpwuid/getgrgid functions.  These
   versions return only the user/group name, not the entire structure.  */

/* Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Tudor Hulubei and Andrei Pitis.  */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#include "xstring.h"
#include "xmalloc.h"
#include "xid.h"


#define UID_BUCKETS 127
#define GID_BUCKETS 127


typedef struct tag_xuid_t
{
    uid_t uid;
    char name[8];
    struct tag_xuid_t *next;
} xuid_t;


typedef struct tag_xgid_t
{
    gid_t gid;
    char name[8];
    struct tag_xgid_t *next;
} xgid_t;


xuid_t *uid_buckets[UID_BUCKETS];
xgid_t *gid_buckets[GID_BUCKETS];


/*
 * Search uid in the hash table. If it is there, return a pointer to the
 * coresponding string. If it is not, find the /etc/passwd entry and insert
 * it in the hash table.
 */

char *
xgetpwuid(uid)
    uid_t uid;
{
    struct passwd *pwd;
    int index = uid % UID_BUCKETS;
    xuid_t *current = uid_buckets[index];

    for (; current; current = current->next)
	if (current->uid == uid)
	    return current->name;

    /* Load the /etc/passwd entry.  */
    pwd = getpwuid(uid);

    current = (xuid_t *)xmalloc(sizeof(xuid_t));

    if (pwd)
	sprintf(current->name, "%-7s", pwd->pw_name);
    else
	sprintf(current->name, "%-7d", (int)uid);

    /* Add it to the hash table.  */
    current->uid  = uid;
    current->next = uid_buckets[index];
    uid_buckets[index] = current;

    /* Return the result.  */
    return current->name;
}


char *
xgetgrgid(gid)
    gid_t gid;
{
    struct group *grp;
    int index = gid % GID_BUCKETS;
    xgid_t *current = gid_buckets[index];

    for (; current; current = current->next)
	if (current->gid == gid)
	    return current->name;

    /* Load the /etc/group entry.  */
    grp = getgrgid(gid);

    current = (xgid_t *)xmalloc(sizeof(xgid_t));

    if (grp)
	sprintf(current->name, "%-7s", grp->gr_name);
    else
	sprintf(current->name, "%-7d", (int)gid);

    /* Add it to the hash table.  */
    current->gid  = gid;
    current->next = gid_buckets[index];
    gid_buckets[index] = current;

    /* Return the result.  */
    return current->name;
}


/*
 * Initialize the hash tables.
 */

void
xid_init()
{
    memset((void *)uid_buckets, 0, sizeof(uid_buckets));
    memset((void *)gid_buckets, 0, sizeof(gid_buckets));
}

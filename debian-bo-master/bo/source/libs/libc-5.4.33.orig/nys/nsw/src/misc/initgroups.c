/* initgroups() adapted from id.c in sh-utils-1.0 */
/* id -- print real and effective UIDs and GIDs
   Copyright (C) 89, 90, 91, 92, 93, 1994 Free Software Foundation, Inc.

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

/* Written by Arnold Robbins, arnold@audiofax.com.
   Major rewrite by David MacKenzie, djm@gnu.ai.mit.edu. */

#ifdef HAVE_CONFIG_H
#if defined (CONFIG_BROKETS)
/* We use <config.h> instead of "config.h" so that a compilation
   using -I. -I$srcdir will use ./config.h rather than $srcdir/config.h
   (which it would do because it found this file in $srcdir).  */
#include <config.h>
#else
#include "config.h"
#endif
#endif

#include <stdio.h>
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <malloc.h>

#ifdef _POSIX_VERSION
#include <unistd.h>
#include <limits.h>
#if !defined(NGROUPS_MAX) || NGROUPS_MAX < 1
#undef NGROUPS_MAX
#define NGROUPS_MAX sysconf (_SC_NGROUPS_MAX)
#endif /* !NGROUPS_MAX */

#else /* not _POSIX_VERSION */
struct passwd *getpwuid (uid_t);
struct group *getgrgid (gid_t);
uid_t getuid (void);
gid_t getgid (void);
uid_t geteuid (void);
gid_t getegid (void);
#include <sys/param.h>
#if !defined(NGROUPS_MAX) && defined(NGROUPS)
#define NGROUPS_MAX NGROUPS
#endif /* not NGROUPS_MAX and NGROUPS */
#endif /* not _POSIX_VERSION */


extern int getgroups (int, gid_t *);
extern int setgroups (int, const gid_t *);
extern int getugroups (int, gid_t *, char *);

int
initgroups (const char *username, gid_t basegid)
{
int err;

if (geteuid() != 0)
	return -1;

#ifdef NGROUPS_MAX
  {
    int ngroups;
    gid_t *groups;
    register int i;

    groups = (gid_t *) malloc (NGROUPS_MAX * sizeof (gid_t));
    if (username == 0)
      ngroups = getgroups (NGROUPS_MAX, groups);
    else
      ngroups = getugroups (NGROUPS_MAX, groups, username);
    if (ngroups < 0)
      {
	free (groups);
		return -1;
      }

    err = setgroups(ngroups, groups);
    free (groups);
    return err;
  }
#else
#error NO NGROUPS_MAX defined
#endif
}

/*
 * $Id$
 *
 * This file is intended to provide a simple mapping from old,
 * libc-based passwd manipulation functions to their equivalent
 * functions in the pwdb library.
 *
 * This file should be included in all 'pw' and 'sp' manipulating
 * source code that wishes to make use of these replacement database
 * functions.
 *
 * Written by Andrew G. Morgan <morgan@parc.power.net>
 *
 * [Please use at your own risk. I cannot be held accountable for any
 * problems you may encounter with this file. Life is too short. You
 * have the source: read it.]
 */

#ifndef PWDB_MAP_H
#define PWDB_MAP_H

#define _PWD_H      1           /* stops inclusion of <pwd.h> */
#define _SHADOW_H   1           /* stops inclusion of <shadow.h> */

#include <pwdb/pwdb_unix.h>
#include <pwdb/pwdb_shadow.h>
#include <pwdb/pwdb_common.h>

/*
 * What follows are a set of macros that completely redirect standard
 * 'pw' and 'sp' definitions to those associated with the pwdb library */

/* structures */

#define passwd __pwdb_passwd                 /* - vanilla UNIX - */
#define spwd   __pwdb_spwd                   /* - SHADOW - */

#define group  __pwdb_group                  /* - GROUP - */
#define sgrp   __pwdb_sgrp                   /* - GSHADOW - */

/* functions */

/* - vanilla UNIX - */

#define getpwent()                           __pwdb_getpwent()

#define setpwent()                           __pwdb_setpwent()
#define endpwent()                           __pwdb_endpwent()

#define fgetpwent(/* (FILE *) */ f)          __pwdb_fgetpwent(f)
#define putpwent(/* (const struct passwd *) */ p, /* (FILE *) */ f) \
                                             __pwdb_fgetpwent(p,f)

#define getpwnam(/* (const char *) */ user)  __pwdb_getpwnam(user)
#define getpwuid(/* (uid_t) */ uid)          __pwdb_getpwuid(uid)

#define sgetpwent(/* (char *) */buf)         __pwdb_sgetpwent(buf)

/* - SHADOW - */

#define getspent()                           __pwdb_getspent()

#define setspent()                           __pwdb_setspent()
#define endspent()                           __pwdb_endspent()

#define fgetspent(/* (FILE *) */ f)          __pwdb_fgetspent(f)
#define putspent(/* (const struct spwd *) */ s, /* (FILE *) */ f) \
                                             __pwdb_putspent(s,f)

#define getspnam(/* (const char *) */ name)  __pwdb_getspnam(name)

#define sgetspent(/* (const char *) */ e)    __pwdb_sgetspent(e)

/* - GROUP / GSHADOW - */

#define setgrent()                          __pwdb_setgrent()
#define setsgent()                          __pwdb_setsgent()

#define endgrent()                          __pwdb_endgrent()
#define endsgent()                          __pwdb_endsgent()

#define getgrent()                          __pwdb_getgrent()
#define getsgent()                          __pwdb_getsgent()

#define fgetgrent(/* (FILE *) */ f)         __pwdb_fgetgrent(f)
#define fgetsgent(/* (FILE *) */ f)         __pwdb_fgetsgent(f)

#define getgrnam(/* (const char *) */ name) __pwdb_getgrnam(name)
#define getsgnam(/* (const char *) */ name) __pwdb_getsgnam(name)

#define getgrgid(/* (gid_t) */ g)           __pwdb_getgrgid(g)

/* Locking functions */

#define lckpwdf()                            __pwdb_lckpwdf()
#define ulckpwdf()                           __pwdb_ulckpwdf()

#endif /* PWDB_MAP_H */

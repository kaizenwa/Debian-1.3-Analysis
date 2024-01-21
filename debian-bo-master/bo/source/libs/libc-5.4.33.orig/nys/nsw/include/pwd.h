/*
** pwd.h           Passwd "map" handling functions and structures
**
** Copyright (c) 1993 Signum Support AB, Sweden
**
** This file is part of the NYS Library.
**
** The NYS Library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with the NYS Library; see the file COPYING.LIB.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Peter Eriksson <pen@signum.se>
*/

#ifndef __PWD_H__
#define __PWD_H__

#include <sys/types.h>

#define _PATH_PASSWD "/etc/passwd"

struct passwd
{
    char *pw_name;
    char *pw_passwd;
    uid_t pw_uid;
    gid_t pw_gid;
    char *pw_gecos;
    char *pw_dir;
    char *pw_shell;
};


extern struct passwd *sgetpwent(const char *str);
extern struct passwd *fgetpwent(FILE *fp);
extern struct passwd *_fgetpwent(FILE *fp, char **buf, int *len);

extern int setpwfile(const char *file);

/* /etc-files access functions */
extern void _setpwent(void);
extern void _endpwent(void);
extern struct passwd *_getpwent(void);
extern struct passwd *_getpwnam(const char *name);
extern struct passwd *_getpwuid(uid_t uid);

/* YP access functions */
extern void _yp_setpwent(void);
extern void _yp_endpwent(void);
extern struct passwd *_yp_getpwent(void);
extern struct passwd *_yp_getpwnam(const char *name);
extern struct passwd *_yp_getpwuid(uid_t uid);

/* COMPAT access functions */
extern void _compat_setpwent(void);
extern void _compat_endpwent(void);
extern struct passwd *_compat_getpwent(void);
extern struct passwd *_compat_getpwnam(const char *name);
extern struct passwd *_compat_getpwuid(uid_t uid);

/* NIS+ access functions */
extern void _nis_setpwent(void);
extern void _nis_endpwent(void);
extern struct passwd *_nis_getpwent(void);
extern struct passwd *_nis_getpwnam(const char *name);
extern struct passwd *_nis_getpwuid(uid_t uid);

/* DNS/Hesiod access functions */
extern void _dns_setpwent(void);
extern void _dns_endpwent(void);
extern struct passwd *_dns_getpwent(void);
extern struct passwd *_dns_getpwnam(const char *name);
extern struct passwd *_dns_getpwuid(uid_t uid);

/* DBM access functions */
extern void _dbm_setpwent(void);
extern void _dbm_endpwent(void);
extern struct passwd *_dbm_getpwent(void);
extern struct passwd *_dbm_getpwnam(const char *name);
extern struct passwd *_dbm_getpwuid(uid_t uid);

/* Name Service Switch access functions */
extern void setpwent(void);
extern void endpwent(void);
extern struct passwd *getpwent(void);
extern struct passwd *getpwnam(const char *name);
extern struct passwd *getpwuid(uid_t uid);

#endif

/*
** grp.h           Group "map" handling functions and structures
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

#ifndef __GRP_H__
#define __GRP_H__

#include <sys/types.h>

#define _PATH_GROUP "/etc/group"

struct group
{
    char *gr_name;
    char *gr_passwd;
    gid_t gr_gid;
    char **gr_mem;
};

extern struct group *sgetgrent(const char *str);
extern struct group *fgetgrent(FILE *fp);

extern void _setgrent(void);
extern void _endgrent(void);
extern struct group *_getgrent(void);
extern struct group *_getgrnam(const char *name);
extern struct group *_getgrgid(gid_t gid);

extern void _yp_setgrent(void);
extern void _yp_endgrent(void);
extern struct group *_yp_getgrent(void);
extern struct group *_yp_getgrnam(const char *name);
extern struct group *_yp_getgrgid(gid_t gid);

extern void _compat_setgrent(void);
extern void _compat_endgrent(void);
extern struct group *_compat_getgrent(void);
extern struct group *_compat_getgrnam(const char *name);
extern struct group *_compat_getgrgid(gid_t gid);

extern void _nis_setgrent(void);
extern void _nis_endgrent(void);
extern struct group *_nis_getgrent(void);
extern struct group *_nis_getgrnam(const char *name);
extern struct group *_nis_getgrgid(gid_t gid);

extern void _dns_setgrent(void);
extern void _dns_endgrent(void);
extern struct group *_dns_getgrent(void);
extern struct group *_dns_getgrnam(const char *name);
extern struct group *_dns_getgrgid(gid_t gid);

extern void _dbm_setgrent(void);
extern void _dbm_endgrent(void);
extern struct group *_dbm_getgrent(void);
extern struct group *_dbm_getgrnam(const char *name);
extern struct group *_dbm_getgrgid(gid_t gid);

extern void setgrent(void);
extern void endgrent(void);
extern struct group *getgrent(void);
extern struct group *getgrnam(const char *name);
extern struct group *getgrgid(gid_t gid);

#endif


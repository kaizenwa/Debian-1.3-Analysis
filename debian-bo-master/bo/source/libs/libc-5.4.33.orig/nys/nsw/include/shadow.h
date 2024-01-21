/*
** shadow.h           Shadow passwd "map" handling functions
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

#ifndef __SHADOW_H__
#define __SHADOW_H__

#include <sys/types.h>

#define _PATH_SHADOW "/etc/shadow"


struct spwd
{
    char *sp_namp;
    char *sp_pwdp;
    long sp_lstchg;
    long sp_min;
    long sp_max;
    long sp_warn;
    long sp_inact;
    long sp_expire;
    unsigned long sp_flag;
};


/*
** Object transformation functions
*/
extern struct spwd *sgetspent(const char *buf);
extern struct spwd *fgetspent(FILE *fp);

extern int sputspent(const struct spwd *sp, char *buf);
extern int fputspent(const struct spwd *sp, FILE *fp);


/* John F. Haugh II shadow library compatibility stuff */
extern int putspent(const struct spwd *sp, FILE *fp);
#define SHADOW _PATH_SHADOW


/*
** Access functions for direct file access to /etc/shadow.
*/
extern void __setspent(void);
extern void __endspent(void);
extern struct spwd *__getspent(void);
extern struct spwd *__getspnam(const char *name);


/*
** Access functions for direct file access.
**
** These ones uses /etc/passwd if /etc/shadow doesn't exist.
*/
extern void _setspent(void);
extern void _endspent(void);
extern struct spwd *_getspent(void);
extern struct spwd *_getspnam(const char *name);


/*
** Access functions for YP (NIS version 2)
*/
extern void _yp_setspent(void);
extern void _yp_endspent(void);
extern struct spwd *_yp_getspent(void);
extern struct spwd *_yp_getspnam(const char *name);


/*
** Access functions for COMPAT
*/
extern void _compat_setspent(void);
extern void _compat_endspent(void);
extern struct spwd *_compat_getspent(void);
extern struct spwd *_compat_getspnam(const char *name);


/*
** Access functions for NIS+ (NIS version 3)
*/
extern void _nis_setspent(void);
extern void _nis_endspent(void);
extern struct spwd *_nis_getspent(void);
extern struct spwd *_nis_getspnam(const char *name);


/*
** Access functions for DNS/Hesiod
*/
extern void _dns_setspent(void);
extern void _dns_endspent(void);
extern struct spwd *_dns_getspent(void);
extern struct spwd *_dns_getspnam(const char *name);


/*
** Access functions for DBM
*/
extern void _dbm_setspent(void);
extern void _dbm_endspent(void);
extern struct spwd *_dbm_getspent(void);
extern struct spwd *_dbm_getspnam(const char *name);


/*
** Name Service Switch access functions
*/
extern void setspent(void);
extern void endspent(void);
extern struct spwd *getspent(void);
extern struct spwd *getspnam(const char *name);


#include <gshadow.h>

#endif

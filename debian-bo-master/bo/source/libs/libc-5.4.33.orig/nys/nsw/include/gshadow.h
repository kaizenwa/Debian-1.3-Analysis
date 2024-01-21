/*
** gshadow.h           Shadow group "map" handling functions
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

#ifndef __GSHADOW_H__
#define __GSHADOW_H__

#include <sys/types.h>

#define _PATH_GSHADOW "/etc/gshadow"


struct sgrp
{
    char *sg_name;
    char *sg_passwd;
    char **sg_adm;
    char **sg_mem;
};

extern struct sgrp *sgetsgent(const char *buf);
extern struct sgrp *fgetsgent(FILE *fp);

extern char *sputsgent(const struct sgrp *sp);
extern int fputsgent(const struct sgrp *sp, FILE *fp);

/* John F. Haugh II gshadow library compatibility stuff */
extern int putsgent(const struct sgrp *sp, FILE *fp);
#define GSHADOW _PATH_GSHADOW



extern void _setsgent(void);
extern void _endsgent(void);
extern struct sgrp *_getsgent(void);
extern struct sgrp *_getsgnam(const char *name);


extern void _yp_setsgent(void);
extern void _yp_endsgent(void);
extern struct sgrp *_yp_getsgent(void);
extern struct sgrp *_yp_getsgnam(const char *name);


extern void _compat_setsgent(void);
extern void _compat_endsgent(void);
extern struct sgrp *_compat_getsgent(void);
extern struct sgrp *_compat_getsgnam(const char *name);


extern void _nis_setsgent(void);
extern void _nis_endsgent(void);
extern struct sgrp *_nis_getsgent(void);
extern struct sgrp *_nis_getsgnam(const char *name);


extern void _dns_setsgent(void);
extern void _dns_endsgent(void);
extern struct sgrp *_dns_getsgent(void);
extern struct sgrp *_dns_getsgnam(const char *name);


extern void _dbm_setsgent(void);
extern void _dbm_endsgent(void);
extern struct sgrp *_dbm_getsgent(void);
extern struct sgrp *_dbm_getsgnam(const char *name);


extern void setsgent(void);
extern void endsgent(void);
extern struct sgrp *getsgent(void);
extern struct sgrp *getsgnam(const char *name);


#endif

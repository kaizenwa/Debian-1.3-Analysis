/* ndbm.h  -  The include file for ndbm users.  */

/*  This file is part of GDBM, the GNU data base manager, by Philip A. Nelson.
    Copyright (C) 1990, 1991  Free Software Foundation, Inc.

    GDBM is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    GDBM is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with GDBM; see the file COPYING.  If not, write to
    the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

    You may contact the author by:
       e-mail:  phil@cs.wwu.edu
      us-mail:  Philip A. Nelson
                Computer Science Department
                Western Washington University
                Bellingham, WA 98226
        phone:  (206) 676-3035
       
*************************************************************************/

#ifndef _NDBM_H /* allow multiple inclusions */
#define _NDBM_H

#include <features.h>

/* Parameters to dbm_store for simple insertion or replacement. */
#define  DBM_INSERT  0
#define  DBM_REPLACE 1


/* The data and key structure.  This structure is defined for compatibility. */
typedef struct {
	char *dptr;
	int   dsize;
      } datum;


/* The file information header. This is good enough for most applications. */
typedef struct {int dummy[10];} DBM;


/* These are the routines (with some macros defining them!) */

__BEGIN_DECLS

extern DBM *dbm_open __P((__const char *, int, int));
extern void dbm_close __P((DBM *));
extern datum dbm_fetch __P((DBM *, datum));
extern datum dbm_firstkey __P((DBM *));
extern datum dbm_nextkey __P((DBM *));
extern int dbm_delete __P((DBM *, datum));
extern int dbm_store __P((DBM *, datum, datum, int));
extern int dbm_dirfno __P((DBM *));
extern int dbm_pagfno __P((DBM *));

__END_DECLS

#define		 dbm_error(dbf)  0
#define		 dbm_clearerr(dbf)

#endif /* _NDBM_H */

#ifndef __YP_DB_H__
#define __YP_DB_H__

/* $Id: yp_db.h,v 1.4 1997/03/07 15:27:32 kukuk Exp $ */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define F_ALL   0x01
#define F_NEXT  0x02

#if defined(HAVE_LIBGDBM)
#include <gdbm.h>

#define DB_FILE GDBM_FILE
#define ypdb_fetch(a,b)  gdbm_fetch(a,b)
#define ypdb_exists(a,b)  gdbm_exists(a,b)

#else

#define DB_FILE int

#endif

extern DB_FILE ypdb_open(const char *domain, const char *map);
extern int ypdb_close(DB_FILE file);
extern int ypdb_read(DB_FILE dbp, const datum *ikey, datum *okey, 
		   datum *dval, int flags, int mangle);

#endif

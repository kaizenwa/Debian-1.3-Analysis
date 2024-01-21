/* @(#) dbm_compat.h,v 1.7 1996/02/16 14:56:40 woods Exp */

/*
 * dbm_compat.h:
 *	Low-functionality compatibility header for dbm and ndbm.
 */

#undef NULL		/* dbm.h often defines NULL */

#ifdef HAVE_DBM

#ifdef DBM_INCLUDE_FILE
#include DBM_INCLUDE_FILE
#else
#include <dbm.h>
#endif

#else	/* !HAVE_DBM */

#ifdef HAVE_NDBM
# ifdef NDBM_INCLUDE_FILE
# include NDBM_INCLUDE_FILE
# else
# include <ndbm.h>
# endif
#else	/* not HAVE_NDBM */
#include "sdbm.h"
#endif	/* HAVE_NDBM */

/* Imitate dbm using  */

static DBM *db;
#define dbminit(f) \
  ((db = dbm_open((f),O_RDWR|O_CREAT,0666)) ? 0 : (db = dbm_open((f),O_RDONLY,0666)) ? 0 : -1)
#define dbmclose()  ((db ? (dbm_close(db),0) : 0), db = (DBM *)NULL, 0)
#define store(k,v)  dbm_store(db,k,v,DBM_INSERT)
#define fetch(k)    dbm_fetch(db,(k))

#endif	/* !HAVE_DBM */

#undef NULL		/* in case dbm.h does not define NULL */
#define NULL 0

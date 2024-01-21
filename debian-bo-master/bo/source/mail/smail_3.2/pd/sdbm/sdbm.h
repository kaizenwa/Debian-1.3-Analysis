/* @(#) sdbm.h,v 1.2 1992/09/09 23:19:36 tron Exp */
/*
 * sdbm - ndbm work-alike hashed database library
 * based on Per-Ake Larson's Dynamic Hashing algorithms. BIT 18 (1978).
 * author: oz@nexus.yorku.ca
 * status: public domain. 
 */

#ifndef SDBM_H
#define SDBM_H
/*
 * this causes problems compiling on some systems
#if __GNUC__
# pragma once
#endif
#if __cplusplus
  extern "C" {
#endif
*/

#define DBLKSIZ 4096
#define PBLKSIZ 1024
#define PAIRMAX 1008			/* arbitrary on PBLKSIZ-N */
#define SPLTMAX	10			/* maximum allowed splits */
					/* for a single insertion */
#define DIRFEXT	".dir"
#define PAGFEXT	".pag"

typedef struct {
	int dirf;		       /* directory file descriptor */
	int pagf;		       /* page file descriptor */
	int flags;		       /* status/error flags, see below */
	long maxbno;		       /* size of dirfile in bits */
	long curbit;		       /* current bit number */
	long hmask;		       /* current hash mask */
	long blkptr;		       /* current block for nextkey */
	int keyptr;		       /* current key for nextkey */
	long blkno;		       /* current page to read/write */
	long pagbno;		       /* current page in pagbuf */
	char pagbuf[PBLKSIZ];	       /* page file block buffer */
	long dirbno;		       /* current block in dirbuf */
	char dirbuf[DBLKSIZ];	       /* directory file block buffer */
} DBM;

#define DBM_RDONLY	0x1	       /* data base open read-only */
#define DBM_IOERR	0x2	       /* data base I/O error */

/*
 * utility macros
 */
#define dbm_rdonly(db)		((db)->flags & DBM_RDONLY)
#define dbm_error(db)		((db)->flags & DBM_IOERR)

#define dbm_clearerr(db)	((db)->flags &= ~DBM_IOERR)  /* ouch */

#define dbm_dirfno(db)	((db)->dirf)
#define dbm_pagfno(db)	((db)->pagf)

typedef struct {
	char *dptr;
	int dsize;
} datum;

extern datum nullitem;

/*
 * flags to dbm_store
 */
#define DBM_INSERT	0
#define DBM_REPLACE	1

/*
 * functions
 */

#ifdef ANSI_C
#define P_(p) p
#else
#define P_(p) ()
#endif

/* ndbm interface */
extern DBM *dbm_open P_((char *, int, int));
extern void dbm_close P_((DBM *));
extern datum dbm_fetch P_((DBM *, datum));
extern int dbm_delete P_((DBM *, datum));
extern int dbm_store P_((DBM *, datum, datum, int));
extern datum dbm_firstkey P_((DBM *));
extern datum dbm_nextkey P_((DBM *));

/* other */
extern DBM *dbm_prep P_((char *, char *, int, int));
extern long dbm_hash P_((char *, int));

#undef P_

/*
#if __cplusplus
  }
#endif
*/
#endif /* not SDBM_H */

/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* This header has had its name changed from db.h to dbhdr.h because
BSDI (for one) has a file in /usr/include called db.h which gets called from
inside /usr/include/ndbm.h. */


/* Additional code (courtesy of Nigel Metheringham) added so that either dbm or
Berkeley db in native mode can be used for the db files. This is selected on
the USE_DB define. All the db functions that are used by Exim are redefined as
macros. */


/********************* Berkeley db definitions **********************/

#ifdef USE_DB

#include <db.h>

/* Basic DB type */
#define EXIM_DB       DB

/* The datum type used for queries */
#define EXIM_DATUM    DBT

/* Some text for messages */
#define EXIM_DBTYPE   "db"

/* Access functions */

/* EXIM_DBOPEN - returns a EXIM_DB *, NULL if failed */
#define EXIM_DBOPEN(name, flags, mode) \
       dbopen(name, flags, mode, DB_HASH, NULL)

/* EXIM_DBGET - returns TRUE if successful, FALSE otherwise */
#define EXIM_DBGET(db, key, data)      \
       ((db)->get(db, &key, &data, 0) == 0)

/* EXIM_DBPUT - returns nothing useful, assumes replace mode */
#define EXIM_DBPUT(db, key, data)      \
       (db)->put(db, &key, &data, 0)

/* EXIM_DBDEL */
#define EXIM_DBDEL(db, key)     (db)->del(db, &key, 0)

/* EXIM_DBSCAN - returns TRUE if data is returned, FALSE at end */
#define EXIM_DBSCAN(db, key, data, first)      \
       ((db)->seq(db, &key, &data, (first? R_FIRST : 0)) == 0)

/* EXIM_DBFD - returns fd useable for locking */
#define EXIM_DBFD(db)           (db)->fd(db)

/* EXIM_DBCLOSE */
#define EXIM_DBCLOSE(db)        (db)->close(db)

/* Datum access types - these are intended to be assignable */

#define EXIM_DATUM_SIZE(datum)  (datum).size
#define EXIM_DATUM_DATA(datum)  (datum).data

#else /* USE_DB */


/********************* dbm definitions **********************/

#include <ndbm.h>

/* Basic DB type */
#define EXIM_DB DBM

/* The datum type used for queries */
#define EXIM_DATUM datum

/* Some text for messages */

#define EXIM_DBTYPE "dbm"

/* Access functions */

/* EXIM_DBOPEN - returns a EXIM_DB *, NULL if failed */
#define EXIM_DBOPEN(name, flags, mode) \
       dbm_open(name, flags, mode)

/* EXIM_DBGET - returns TRUE if successful, FALSE otherwise */
#define EXIM_DBGET(db, key, data)      \
       (data = dbm_fetch(db, key), data.dptr != NULL)

/* EXIM_DBPUT - returns nothing useful, assumes replace mode */
#define EXIM_DBPUT(db, key, data)      \
       dbm_store(db, key, data, DBM_REPLACE)

/* EXIM_DBDEL */
#define EXIM_DBDEL(db, key) dbm_delete(db, key)

/* EXIM_DBSCAN */
#define EXIM_DBSCAN(db, key, data, first)      \
       (key = (first? dbm_firstkey(db) : dbm_nextkey(db)), key.dptr != NULL)

/* Most Unix systems have the function (usually a macro) dbm_pagfno(), and Exim
started life using that for getting a file descriptor in order to lock an open
database. The BSDI system, and others using the dbm compatibility interface to
db, do not have that function, though they does have dbm_dirfno(). Changing
over to using dbm_dirfno() would have meant that different versions of Exim on
other systems would not interwork (and the upgrade would be tricky) so instead
there is a macro that can be defined in the os.h file requesting that
dbm_dirfno() be used. Even though EXIM_DBFD is now used to get the fd for
locking, we still need to define dbm_pagfno to map to dbm_dirfno, as they are
both called when checking the mode and ownership of database files. */

/* EXIM_DBFD - returns fd useable for locking */
#ifdef USE_DBM_DIRFNO
#undef dbm_pagfno
#define dbm_pagfno(db) dbm_dirfno(db)
#define EXIM_DBFD(db)  dbm_dirfno(db)
#else
#define EXIM_DBFD(db)  dbm_pagfno(db)
#endif

/* EXIM_DBCLOSE */
#define EXIM_DBCLOSE(db) dbm_close(db)

/* Datum access types - these are intended to be assignable */

#define EXIM_DATUM_SIZE(datum) (datum).dsize
#define EXIM_DATUM_DATA(datum) (datum).dptr

#endif /* USE_DB */

/********************* End of db/dbm definitions **********************/



/* Structures for records stored in exim database dbm files. They all
start with the same fields, described in the generic type. */


typedef struct {
  time_t time_stamp;      /* Timestamp of writing */
} db_generic;


/* This structure keeps track of retry information for a host or a local
address. */

typedef struct {
  time_t time_stamp;
  /*************/
  time_t first_failed;    /* Time of first failure */
  time_t last_try;        /* Time of last try */
  time_t next_try;        /* Time of next try */
  BOOL   expired;         /* Retry time has expired */
  int    basic_errno;     /* Errno of last failure */
  int    more_errno;      /* Additional information */
  char   text[1];         /* Text message for last failure */
} db_retry;

/* This structure keeps track of messages that are waiting for a particular
host for a particular transport. */

typedef struct {
  time_t time_stamp;
  /*************/
  int    count;           /* Count of message ids */
  int    sequence;        /* Sequence for continued records */
  char   text[1];         /* One long character string */
} db_wait;

/* This structure records recent message rejections. The key is the bad
address + host identification. */

typedef struct {
  time_t time_stamp;
  /*************/
  BOOL   rejected_mail_from;  /* Already tried rejecting MAIL FROM */
} db_reject;

/* This structure records a connection to a particular host, for the
purpose of serializing access to certain hosts. For possible future extension,
a field is defined for holding the count of connections, but it is not
at present in use. */

typedef struct {
  time_t time_stamp;
  /*************/
  int    count;           /* Reserved for possible connection count */
} db_serialize;


/* Functions for reading/writing exim database files */

void  db_close(EXIM_DB *);
int   db_delete(EXIM_DB *, char *);
EXIM_DB *db_open(char *, int);
void *db_read_with_length(EXIM_DB *, char *, int *);
char *db_scan(EXIM_DB *, BOOL);
int   db_write(EXIM_DB *, char *, void *, int);

/* Macro for the common call to read without wanting to know the length. */

#define db_read(a, b) db_read_with_length(a, b, NULL)

/* End of dbhdr.h */

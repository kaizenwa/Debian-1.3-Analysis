/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "exim.h"


/* Functions for accessing Exim's database, which consists of a number
of different DBM files. This module does not contain code for reading DBM
files for (e.g.) alias expansion. That is all contained within the general
search functions. As Exim now has support for either ndbm or native db, all the
relevant functions are called as macros.

All the data in Exim's database is in the nature of *hints*. Therefore it
doesn't matter if it gets destroyed by accident. These functions are not
supposed to implement a "safe" database.

Keys are passed in as C strings, and the terminating zero *is* used when
building the dbm files. This just makes life easier when scanning the files
sequentially.

Synchronization is required on the database files, and this is achieved by
means of locking. Since callers may in general want to do more than one
read or write while holding the lock, there are separate open and close
functions. However, the calling modules should arrange to hold the locks
for the bare minimum of time. */



/*************************************************
*          Open and lock a database file         *
*************************************************/

/* Used for accessing Exim's hints databases.

Arguments:
  name    The single-component name of one of Exim's database files
  flags   Standard O_xxx flags indicating the type of open required

Returns:  a pointer to an EXIM_DB, which is either DBM or DB, depending on
          whether dbm or native db is in use.
*/

EXIM_DB *
db_open(char *name, int flags)
{
EXIM_DB *dbm;
flock_t lock_data;
int lock_count = 0;
int open_count = 0;
char buffer[256];

sprintf(buffer, "%s/db/%s", spool_directory, name);
dbm = EXIM_DBOPEN(buffer, flags, DB_MODE);

/* Try to make the directory in case that was the cause of the failure to
open. */

if (dbm == NULL)
  {
  directory_make(spool_directory, "db", DB_DIRECTORY_MODE);
  dbm = EXIM_DBOPEN(buffer, flags, DB_MODE);
  }

/* If Berkeley DB is in use, opening a new file and causing it to be created
does not cause anything to be written to the file initially. If another process
tries to open the file in this state, it gets the error EINVAL because the
contents of the file are invalid. So it can't then wait on the lock for the
first process to finish. To cope with this, try several times if EINVAL is
given - use the same parameters as for locking, as it is more or less the
same thing. */

while (dbm == NULL)
  {
  if (errno != EINVAL || open_count++ > DB_LOCK_RETRIES)
    {
    if (errno != ENOENT)
      log_write(0, LOG_MAIN, "failed to open DB file %s: %s\n", name,
        strerror(errno));
    return NULL;
    }
  DEBUG(1) debug_printf("opening DB file %s gave EINVAL; trying again\n",
    name);
  sleep(DB_LOCK_SLEEP);
  dbm = EXIM_DBOPEN(name, flags, DB_MODE);
  }

DEBUG(9) debug_printf("opened DB file %s: flags=%x fd = %d\n", buffer,
  flags, EXIM_DBFD(dbm));

/* Set up for locking */

lock_data.l_type =
  ((flags & (O_RDONLY|O_WRONLY|O_RDWR)) == O_RDONLY)? F_RDLCK : F_WRLCK;
lock_data.l_whence = lock_data.l_start = lock_data.l_len = 0;

/* EXIM_DBFD() yields a file descriptor; precisely which one when there
are two files is OS-dependent. */

while (fcntl(EXIM_DBFD(dbm), F_SETLK, &lock_data) < 0)
  {
  if (lock_count++ > DB_LOCK_RETRIES)
    {
    log_write(0, LOG_MAIN, "Failed to get %s lock for %s\n",
      ((flags & O_RDONLY) != 0)? "read" : "write", buffer);
    EXIM_DBCLOSE(dbm);
    return NULL;
    }

  DEBUG(1) debug_printf("Sleeping for %s lock: %s\n", EXIM_DBTYPE,
    strerror(errno));

  sleep(DB_LOCK_SLEEP);
  }

DEBUG(9) debug_printf("locked\n");

return dbm;
}




/*************************************************
*         Unlock and close a database file       *
*************************************************/

/* Closing a file automatically unlocks it, so we don't do an explicit
unlock. In fact, the DBM functions may want to do things before actually
closing the file, so unlocking here could be dangerous. There is no error
return given by dbm_close().

Argument: a pointer to an open database file
Returns:  nothing
*/

void
db_close(EXIM_DB *dbm)
{
EXIM_DBCLOSE(dbm);
}




/*************************************************
*             Read from database file            *
*************************************************/

/* Passing back the pointer unchanged is useless, because there is
no guarantee of alignment. Since all the records used by exim need
to be properly aligned to pick out the timestamps, etc., we might as
well do the copying centrally here.

Most calls don't need the length, so there is a macro called db_read which
has two arguments; it calls this function adding NULL as the third.

Arguments:
  dbm    a pointer to an open database file
  key    the key of the record to be read
  length a pointer to an int into which to return the length, if not NULL

Returns: a pointer to the retrieved record, or
         NULL if the record is not found
*/

void *
db_read_with_length(EXIM_DB *dbm, char *key, int *length)
{
void *yield;
EXIM_DATUM key_datum, result_datum;
EXIM_DATUM_DATA(key_datum) = key;
EXIM_DATUM_SIZE(key_datum) = (int)strlen(key) + 1;
if (!EXIM_DBGET(dbm, key_datum, result_datum)) return NULL;
yield = (void *)store_malloc(EXIM_DATUM_SIZE(result_datum));
memcpy(yield, EXIM_DATUM_DATA(result_datum), EXIM_DATUM_SIZE(result_datum));
if (length != NULL) *length = EXIM_DATUM_SIZE(result_datum);
return yield;
}



/*************************************************
*             Write to database file             *
*************************************************/

/*
Arguments:
  dbm     a pointer to an open database file
  key     the key of the record to be written
  ptr     a pointer to the record to be written
  length  the length of the record to be written

Returns:  the yield of the underlying dbm or db "write" function. If this
          is dbm, the value is zero for OK.
*/

int
db_write(EXIM_DB *dbm, char *key, void *ptr, int length)
{
EXIM_DATUM key_datum, value_datum;
db_generic *gptr = (db_generic *)ptr;
gptr->time_stamp = time(NULL);
EXIM_DATUM_DATA(key_datum) = key;
EXIM_DATUM_SIZE(key_datum) = (int)strlen(key) + 1;
EXIM_DATUM_DATA(value_datum) = (char *)ptr;
EXIM_DATUM_SIZE(value_datum) = length;
return EXIM_DBPUT(dbm, key_datum, value_datum);
}



/*************************************************
*           Delete record from database file     *
*************************************************/

/*
Arguments:
  dbm    a pointer to an open database file
  key    the key of the record to be deleted

Returns: the yield of the underlying dbm or db "delete" function.
*/

int
db_delete(EXIM_DB *dbm, char *key)
{
EXIM_DATUM key_datum;
EXIM_DATUM_DATA(key_datum) = key;
EXIM_DATUM_SIZE(key_datum) = (int)strlen(key) + 1;
return EXIM_DBDEL(dbm, key_datum);
}



/*************************************************
*         Scan the keys of a database file       *
*************************************************/

/*
Arguments:
  dbm    a pointer to an open database file
  start  TRUE if starting a new scan
         FALSE if continuing with the current scan

Returns: the next record from the file, or
         NULL if there are no more
*/

char *
db_scan(EXIM_DB *dbm, BOOL start)
{
EXIM_DATUM key, value;
return (EXIM_DBSCAN(dbm, key, value, start))? EXIM_DATUM_DATA(key) : NULL;
}

/* End of db.c */

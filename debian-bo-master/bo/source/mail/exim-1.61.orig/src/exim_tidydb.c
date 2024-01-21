/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/* Utility program to tidy the contents of an exim database file. The
arguments are two optional flags, followed by the spool directory name and the
database name. The flags are

   -f         full tidy - for the waiting database, checks are made on the
              existence of messages

   -t <time>  expiry time for old records - default 30 days

The final argument is the name of the database file. The available names are:

  retry:      retry delivery information
  wait-<t>:   message waiting information - <t> is transport name
  reject:     message rejection information
  serialize:  host serialization information

This argument is required. */


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <fcntl.h>
#include <sys/stat.h>

#include "macros.h"
#include "config.h"
#include "dbhdr.h"

#define type_retry 1
#define type_wait 2
#define type_reject 3
#define type_serialize 4



typedef struct key_item {
  struct key_item *next;
  char   key[1];
} key_item;



/* Need spool_directory from the globals module, but no other globals. */

extern char *spool_directory;



static void usage(void)
{
printf("Usage: exim_tidydb [-f] [-t <time>] <spool-directory> <database-name>\n");
printf("<database-name> = retry | wait-<transport> | reject | "
  "serialize-<transport>\n");
exit(1);
}


int main(int argc, char **argv)
{
struct stat statbuf;
int  db_type = 0;
int  maxkeep = 30 * 24 * 60 * 60;
int  i, oldest, path_len;
int db_arg = 0;
key_item *keychain = NULL;
BOOL full = FALSE;
EXIM_DB *dbm;
char buffer[256];
char *key;


/* Scan the arguments */

for (i = 1; i < argc; i++)
  {
  if (argv[i][0] != '-')
    {
    spool_directory = argv[i++];
    if (i != argc-1) usage();
    db_arg = i;
    if (strcmp(argv[i], "retry") == 0) db_type = type_retry;
    else if (strncmp(argv[i], "wait-", 5) == 0) db_type = type_wait;
    else if (strcmp(argv[i], "reject") == 0) db_type = type_reject;
    else if (strncmp(argv[i], "serialize-", 10) == 0) db_type = type_serialize;
    else usage();
    }
  else if (strcmp(argv[i], "-f") == 0) full = TRUE;
  else if (strcmp(argv[i], "-t") == 0)
    {
    char *s = argv[++i];
    maxkeep = 0;
    while (*s != 0)
      {
      int value, count;
      if (!isdigit(*s)) usage();
      (void)sscanf(s, "%d%n", &value, &count);
      s += count;
      switch (*s)
        {
        case 'w': value *= 7;
        case 'd': value *= 24;
        case 'h': value *= 60;
        case 'm': value *= 60;
        case 's': s++;
        break;
        default: usage();
        }
      maxkeep += value;
      }
    }
  else usage();
  }


/* Compute the oldest keep time */

oldest = time(NULL) - maxkeep;

/* Database name missing */

if (db_type == 0) usage();

/* Verify what we are working on */

printf("Exim %s database in spool %s\n", argv[db_arg], spool_directory);


/* Open the database - this gets a write lock as well. */

dbm = db_open(argv[db_arg], O_RDWR);

if (dbm == NULL)
  {
  printf("Failed to open database file %s\n", argv[db_arg]);
  exit(1);
  }


/* Prepare for building file names */

sprintf(buffer, "%s/input/", spool_directory);
path_len = (int)strlen(buffer);


/* It appears, by experiment, that it is a bad idea to make changes
to the file while scanning it. Pity the man page doesn't warn you about that.
Therefore, we scan and build a list of all the keys. Then we use that to
read the records and possibly update them. */

key = db_scan(dbm, TRUE);
while (key != NULL)
  {
  key_item *k = (key_item *)malloc(sizeof(key_item) + (int)strlen(key));
  k->next = keychain;
  keychain = k;
  strcpy(k->key, key);
  key = db_scan(dbm, FALSE);
  }

printf("keys read\n");


/* Now scan the collected keys and operate on the records. */

while (keychain != NULL)
  {
  db_generic *value;
  key = keychain->key;
  keychain = keychain->next;
  value = db_read(dbm, key);

  /* A continuation record may have been deleted or renamed already, so
  non-existence is not serious. */

  if (value == NULL) continue;

  /* Delete if too old */

  if (value->time_stamp < oldest)
    {
    printf("deleted %s (too old)\n", key);
    db_delete(dbm, key);
    free(value);
    continue;
    }

  /* If full processing not requested, do no more. */

  if (!full) { free(value); continue; }

  /* Otherwise do database-specific tidying for wait databases */

  if (db_type == type_wait)
    {
    db_wait *wait = (db_wait *)value;
    BOOL update = FALSE;

    /* Leave corrupt records alone */

    if (wait->count > WAIT_NAME_MAX)
      {
      printf("**** Data for %s corrupted\n  count=%d=0x%x max=%d\n",
        key, wait->count, wait->count, WAIT_NAME_MAX);
      continue;
      }

    /* Loop for renamed continuation records */

    for (;;)
      {
      int offset;
      int length = wait->count * MESSAGE_ID_LENGTH;

      for (offset = length - MESSAGE_ID_LENGTH;
           offset >= 0; offset -= MESSAGE_ID_LENGTH)
        {
        strncpy(buffer + path_len, wait->text + offset, MESSAGE_ID_LENGTH);
        sprintf(buffer + path_len + MESSAGE_ID_LENGTH, "-D");
        if (stat(buffer, &statbuf) != 0)
          {
          int left = length - offset - MESSAGE_ID_LENGTH;
          if (left > 0) strncpy(wait->text + offset,
            wait->text + offset + MESSAGE_ID_LENGTH, left);
          wait->count--;
          length -= MESSAGE_ID_LENGTH;
          update = TRUE;
          }
        }

      /* If record is empty and the main record, either delete it or rename
      the next continuation, repeating if that is also empty. */

      if (wait->count == 0 && strchr(key, ':') == NULL)
        {
        while (wait->count == 0 && wait->sequence > 0)
          {
          char newkey[256];
          db_generic *newvalue;
          sprintf(newkey, "%s:%d", key, wait->sequence - 1);
          newvalue = db_read(dbm, newkey);
          if (newvalue != NULL)
            {
            free(value);
            value = newvalue;
            wait = (db_wait *)newvalue;
            db_delete(dbm, newkey);
            printf("renamed %s\n", newkey);
            update = TRUE;
            }
          else wait->sequence--;
          }

        /* If we have ended up with an empty main record, delete it
        and break the loop. Otherwise the new record will be scanned. */

        if (wait->count == 0 && wait->sequence == 0)
          {
          db_delete(dbm, key);
          printf("deleted %s (empty)\n", key);
          update = FALSE;
          break;
          }
        }

      /* If not an empty main record, break the loop */

      else break;
      }

    /* Re-write the record if required */

    if (update)
      {
      printf("updated %s\n", key);
      db_write(dbm, key, wait, sizeof(db_wait) +
        wait->count * MESSAGE_ID_LENGTH);
      }
    }

  /* Recover the store */

  free(value);
  }

db_close(dbm);
return 0;
}

/* End of exim_tidydb.c */

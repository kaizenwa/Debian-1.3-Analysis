/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <fcntl.h>

#include <sys/stat.h>

#include <pwd.h>                 /* Because used in includes below */
#include "../regexp/regexp.h"    /* Ditto */

#include "macros.h"
#include "dbhdr.h"
#include "structs.h"
#include "functions.h"


#define type_retry 1
#define type_wait 2
#define type_reject 3
#define type_serialize 4


/* Utility program to dump the contents of an exim database file. The first
argument is the name of the spool directory. The second argument is the name of
the database file. The available names are:

  retry:      retry delivery information
  wait-<t>:   message waiting information - <t> is transport name
  reject:     record of message rejections
  serialize:  record of host connections for serializing

Output formatting is rudimentary. We want this utility to be as fast
as possible so as not to hold the lock any longer than necessary. Fancy
formatting can be done by some other program subsequently. Don't use
a pipe! */


/* Need spool_directory from the globals module, but no other globals. */

extern char *spool_directory;

static char time_buffer[sizeof("09-xxx-1999 hh:mm:ss  ")];

char *
print_time(time_t t)
{
struct tm *tmstr = localtime(&t);
strftime(time_buffer, sizeof(time_buffer), "%d-%b-%Y %H:%M:%S", tmstr);
return time_buffer;
}


int
main(int argc, char **argv)
{
int db_type = 0;
int yield = 0;
EXIM_DB *dbm;
char *key;
char keybuffer[1024];

/* Check up on the number of arguments */

if (argc != 3)
  {
  printf("Usage: exim_dumpdb <spool-directory> <database-name>\n");
  printf("<database-name> = retry | wait-<transport> | reject | "
    "serialize-<transport>\n");
  exit(1);
  }

if (strcmp(argv[2], "retry") == 0) db_type = type_retry;
  else if (strcmp(argv[2], "reject") == 0) db_type = type_reject;
  else if (strncmp(argv[2], "wait-", 5) == 0) db_type = type_wait;
  else if (strncmp(argv[2], "serialize-", 10) == 0) db_type = type_serialize;

if (db_type == 0)
  {
  printf("Usage: exim_dumpdb <spool-directory> <database-name>\n");
  printf("<database-name> = retry | wait-<transport> | reject | "
    "serialize-<transport>\n");
  exit(1);
  }

/* Set up the spool directory in the global place, open the database - this
gets a read lock as well. */

spool_directory = argv[1];
dbm = db_open(argv[2], O_RDONLY);

if (dbm == NULL)
  {
  printf("Failed to open database file %s (spool directory = %s)\n",
    argv[2], spool_directory);
  exit(1);
  }

/* Scan the file, formatting the information for each entry. Note
that data is returned in a malloc'ed block, in order that it be
correctly aligned. */

key = db_scan(dbm, TRUE);
while (key != NULL)
  {
  db_retry *retry;
  db_wait *wait;
  db_reject *reject;
  db_serialize *serialize;
  int count_bad = 0;
  int i, length;
  char *t;
  char name[MESSAGE_ID_LENGTH + 1];
  void *value;

  /* Keep a copy of the key separate, as in some DBM's the pointer is into data
  which might change. */

  if ((int)strlen(key) > sizeof(keybuffer) - 1)
    {
    printf("**** Overlong key encountered: %s\n", key);
    return 1;
    }
  strcpy(keybuffer, key);
  value = db_read_with_length(dbm, keybuffer, &length);

  if (value == NULL)
    fprintf(stderr, "**** Entry %s not found - shurely shome mistake!\n",
      keybuffer);
  else
    {
    /* Note: don't use print_time more than once in one statement, since
    it uses a single buffer. */

    switch(db_type)
      {
      case type_retry:
      retry = (db_retry *)value;
      printf("  %s %d %d %s\n%s  ", keybuffer, retry->basic_errno,
        retry->more_errno, string_printing(retry->text, FALSE),
        print_time(retry->first_failed));
      printf("%s  ", print_time(retry->last_try));
      printf("%s %s\n", print_time(retry->next_try),
        (retry->expired)? "*" : "");
      break;

      case type_wait:
      wait = (db_wait *)value;
      printf("%s ", keybuffer);
      t = wait->text;
      name[MESSAGE_ID_LENGTH] = 0;

      if (wait->count > WAIT_NAME_MAX)
        {
        fprintf(stderr,
          "**** Data for %s corrupted\n  count=%d=0x%x max=%d\n",
          keybuffer, wait->count, wait->count, WAIT_NAME_MAX);
        wait->count = WAIT_NAME_MAX;
        yield = count_bad = 1;
        }
      for (i = 1; i <= wait->count; i++)
        {
        strncpy(name, t, MESSAGE_ID_LENGTH);
        if (count_bad && name[0] == 0) break;
        if ((int)strlen(name) != MESSAGE_ID_LENGTH ||
            strspn(name, "0123456789"
                         "abcdefghijklmnopqrstuvwxyz"
                         "ABCDEFGHIJKLMNOPQRSTUVWXYZ-") != MESSAGE_ID_LENGTH)
          {
          int j;
          fprintf(stderr,
            "**** Data for %s corrupted: bad character in message id\n",
            keybuffer);
          for (j = 0; j < MESSAGE_ID_LENGTH; j++)
            fprintf(stderr, "%02x ", name[j]);
          fprintf(stderr, "\n");
          yield = 1;
          break;
          }
        printf("%s ", name);
        t += MESSAGE_ID_LENGTH;
        }
      printf("\n");
      break;

      case type_reject:
      reject = (db_reject *)value;
      if (length < sizeof(db_reject))
        printf("%s %s\n", print_time(reject->time_stamp), keybuffer);
      else
        printf("%s %c %s\n", print_time(reject->time_stamp),
          reject->rejected_mail_from? 'T':'F', keybuffer);
      break;

      case type_serialize:
      serialize = (db_serialize *)value;
      printf("%s %s\n", print_time(serialize->time_stamp), keybuffer);
      break;
      }
    store_free(value);
    }
  key = db_scan(dbm, FALSE);
  }

db_close(dbm);
return yield;
}

/* End of exim_dumpdb.c */

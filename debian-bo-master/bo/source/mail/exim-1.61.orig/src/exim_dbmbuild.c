/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/* A small freestanding program to build dbm databases from serial input. For
alias files, this program fulfils the function of the newaliases program used
by other mailers, but it can be used for other dbm data files too. It operates
by writing a new file or files, and then renaming; otherwise old entries can
never get flushed out.

This program is clever enough to cope with ndbm, which creates two files called
<name>.dir and <name>.pag, or with db, which creates a single file called
<name>.db. If native db is in use (USE_DB defined) then there is no extension
to the output filename. This is also handled. If there are any other variants,
the program won't cope.

The first argument to the program is the name of the serial file; the second
is the base name for the DBM file(s). When native db is in use, these must be
different.

Input lines beginning with # are ignored, as are blank lines. Entries begin
with a key terminated by a colon or end of line or whitespace and continue with
indented lines. */


#include "exim.h"
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>


#define maxsize 10000


int main(int argc, char **argv)
{
int started;
int count = 0;
int yield = 0;
#ifndef USE_DB
int is_db = 0;
struct stat statbuf;
#endif
FILE *f;
EXIM_DB *d;
EXIM_DATUM key, content;
char *bptr;
char  keybuffer[256];
char  temp_dbmname[256];
char  real_dbmname[256];
char *buffer = malloc(maxsize);
char *line = malloc(maxsize);

if (argc != 3)
  {
  printf("usage: exim_dbmbuild <source file> <dbm base name>\n");
  exit(1);
  }

if (strcmp(argv[1], "-") == 0) f = stdin; else
  {
  f = fopen(argv[1], "r");
  if (f == NULL)
    {
    printf("exim_dbmbuild: unable to open %s: %s\n", argv[1], strerror(errno));
    exit(1);
    }
  }

/* By default Berkeley db does not put extensions on... which
can be painful! */

#ifdef USE_DB
if (strcmp(argv[1], argv[2]) == 0)
  {
  printf("exim_dbmbuild: input and output filenames are the same\n");
  exit(1);
  }
#endif

strcpy(temp_dbmname, argv[2]);
strcat(temp_dbmname, ".dbmbuild_temp");

/* It is apparently necessary to open with O_RDWR for this to work
with gdbm-1.7.3, though no reading is actually going to be done. */

d = EXIM_DBOPEN(temp_dbmname, O_RDWR|O_CREAT|O_EXCL, 0444);

if (d == NULL)
  {
  printf("exim_dbmbuild: unable to create %s: %s\n", temp_dbmname,
    strerror(errno));
  fclose(f);
  exit(1);
  }

/* Unless using native db calls, see if we have created <name>.db; if not,
assume .dir & .pag */

#ifndef USE_DB
sprintf(real_dbmname, "%s.db", temp_dbmname);
is_db = stat(real_dbmname, &statbuf) == 0;
#endif

/* Now do the business */

bptr = buffer;
started = 0;

while (fgets(line, maxsize, f) != NULL)
  {
  char *p;

  if (line[0] == '#') continue;
  p = line + (int)strlen(line);
  while (p > line && isspace(p[-1])) p--;
  *p = 0;
  if (line[0] == 0) continue;

  /* A continuation line is valid only if there was a previous first
  line. */

  if (isspace(line[0]))
    {
    char *s = line;
    if (!started)
      {
      printf("Unexpected continuation line ignored\n%s\n\n", line);
      continue;
      }
    while (isspace(*s)) s++;
    strcpy (bptr, --s);
    bptr += p - s;
    }

  /* A first line must have a name followed by a colon or whitespace or
  end of line, but first finish with a previous line. The key is lower
  cased - this is what the newaliases program for sendmail does. */

  else
    {
    int i;
    char *s = line;
    if (started)
      {
      EXIM_DATUM_DATA(content) = buffer;
      EXIM_DATUM_SIZE(content) = bptr - buffer + 1;
      EXIM_DBPUT(d, key, content);
      count++;
      bptr = buffer;
      }
    while (*s != 0 && *s != ':' && !isspace(*s)) s++;
    EXIM_DATUM_DATA(key) = keybuffer;
    EXIM_DATUM_SIZE(key) = s - line + 1;

    if (EXIM_DATUM_SIZE(key) > 256)
      {
      printf("Keys longer than 255 characters cannot be handled\n");
      started = 0;
      yield = 1;
      break;
      }

    for (i = 0; i < EXIM_DATUM_SIZE(key) - 1; i++)
      keybuffer[i] = tolower(line[i]);
    keybuffer[i] = 0;
    started = 1;

    if (*s == ':') s++;
    while (isspace(*s))s++;
    if (*s != 0)
      {
      strcpy(bptr, s);
      bptr += p - s;
      }
    else buffer[0] = 0;
    }
  }

if (started)
  {
  EXIM_DATUM_DATA(content) = buffer;
  EXIM_DATUM_SIZE(content) = bptr - buffer + 1;
  EXIM_DBPUT(d, key, content);
  count++;
  }

EXIM_DBCLOSE(d);
fclose(f);

/* If successful, output the number of entries and rename the temporary
files. */

if (yield == 0)
  {
  printf("%d entries written\n", count);

  #ifdef USE_DB
  strcpy(real_dbmname, temp_dbmname);
  strcpy(buffer, argv[2]);
  if (rename(real_dbmname, buffer) != 0)
    {
    printf("Unable to rename %s as %s\n", real_dbmname, buffer);
    return 1;
    }
  #else

  /* Rename a single .db file */

  if (is_db)
    {
    sprintf(real_dbmname, "%s.db", temp_dbmname);
    sprintf(buffer, "%s.db", argv[2]);
    if (rename(real_dbmname, buffer) != 0)
      {
      printf("Unable to rename %s as %s\n", real_dbmname, buffer);
      return 1;
      }
    }

  /* Rename .dir and .pag files */

  else
    {
    sprintf(real_dbmname, "%s.dir", temp_dbmname);
    sprintf(buffer, "%s.dir", argv[2]);
    if (rename(real_dbmname, buffer) != 0)
      {
      printf("Unable to rename %s as %s\n", real_dbmname, buffer);
      return 1;
      }

    sprintf(real_dbmname, "%s.pag", temp_dbmname);
    sprintf(buffer, "%s.pag", argv[2]);
    if (rename(real_dbmname, buffer) != 0)
      {
      printf("Unable to rename %s as %s\n", real_dbmname, buffer);
      return 1;
      }
    }

  #endif /* USE_DB */
  }

/* Otherwise unlink the temporary files. */

else
  {
  printf("dbmbuild abandoned\n");
  #ifdef USE_DB
  unlink(temp_dbmname);
  #else
  if (is_db)
    {
    sprintf(real_dbmname, "%s.db", temp_dbmname);
    unlink(real_dbmname);
    }
  else
    {
    sprintf(real_dbmname, "%s.dir", temp_dbmname);
    unlink(real_dbmname);
    sprintf(real_dbmname, "%s.pag", temp_dbmname);
    unlink(real_dbmname);
    }
  #endif /* USE_DB */
  }

return yield;
}

/* End of exim_dbmbuild.c */

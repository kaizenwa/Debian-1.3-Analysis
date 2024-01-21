/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

#include "exim.h"


/*************************************************
*           Attempt to create a directory        *
*************************************************/

/* All the directories that Exim ever creates are within the spool
directory as defined by spool_directory. We are prepared to create
as many as necessary from that directory downwards, inclusive. If
a non-root uid has been specified for exim, and we are currently
running as root, ensure the directory is owned by the non-root id.

Arguments:
  parent    parent directory name
  name      directory name within the parent that we want
  mode      mode for the new directory

Returns:    nothing; panics on failure
*/

void
directory_make(char *parent, char *name, int mode)
{
BOOL failed = FALSE;
int p = (int)strlen(spool_directory);
struct stat statbuf;
char buffer[256];

sprintf(buffer, "%s/%s", parent, name);
if (strncmp(buffer, spool_directory, p) != 0)
  log_write(0, LOG_PANIC_DIE,
    "directory_make called for non-spool directory: %s", buffer);

while(buffer[p] == '/')
  {
  buffer[p] = 0;
  if (stat(buffer, &statbuf) != 0)
    {
    if (mkdir(buffer, mode) < 0 && errno != EEXIST)
      { failed = TRUE; break; }
    if (exim_uid_set && geteuid() == root_uid)
      chown(buffer, exim_uid, exim_gid);
    }
  buffer[p++] = '/';
  while (buffer[p] != 0 && buffer[p] != '/') p++;
  }

if (!failed && buffer[p-1] != '/')
  {
  if (mkdir(buffer, mode) < 0 && errno != EEXIST) failed = TRUE;
  if (exim_uid_set && geteuid() == root_uid)
    chown(buffer, exim_uid, exim_gid);
  }

if (failed) log_write(0, LOG_PANIC_DIE,
  "Failed to create directory %s: %s\n", buffer, strerror(errno));
}

/* End of directory.c */

/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions for writing spool files. */


#include "exim.h"



/*************************************************
*            Open file under temporary name      *
*************************************************/

/* This is used for opening spool files under a temporary name,
with a single attempt at deleting if they already exist.

Argument: temporary name for spool header file
Returns:  file descriptor of open file, or < 0 on failure, with errno unchanged
*/

int
spool_open_temp(char *temp_name)
{
int fd = open(temp_name, O_RDWR|O_CREAT|O_EXCL, SPOOL_MODE);

/* Try to make the directory if it isn't there. */

if (fd < 0 && errno == ENOENT)
  {
  directory_make(spool_directory, "input", INPUT_DIRECTORY_MODE);
  fd = open(temp_name, O_RDWR|O_CREAT|O_EXCL, SPOOL_MODE);
  }

/* If the file already exists, something has gone wrong. This process may well
have previously created the file if it is delivering more than one address, but
it should have renamed it almost immediately. A file could, however, be left
around as a result of a system crash, and by coincidence this process might
have the same pid. We therefore have one go at unlinking it before giving up.
*/

if (fd < 0 && errno == EEXIST)
  {
  DEBUG(2) debug_printf("%s exists: unlinking\n", temp_name);
  unlink(temp_name);
  fd = open(temp_name, O_RDWR|O_CREAT|O_EXCL, SPOOL_MODE);
  }

/* If the file has been opened, make sure the file's group is the Exim gid
if exim_uid exists (can't have exim_uid set without exim_gid), and double-check
the mode because the group setting doesn't always get set automatically. */

if (fd >= 0 && exim_uid_set)
  {
  fchown(fd, exim_uid, exim_gid);
  fchmod(fd, SPOOL_MODE);
  }

return fd;
}



/*************************************************
*          Write the header spool file           *
*************************************************/

/* Returns the size of the file for success; zero for failure. The file is
written under a temporary name, and then renamed. It's done this way so that it
works with re-writing the file on message deferral as well as for the initial
write. Whenever this function is called, the data file for the message should
be open and locked, thus preventing any other exim process from working on this
message.

Argument: the message id
Returns:  zero on failure (error number in errno); the size of the file on
            success
*/

int
spool_write_header(char *id)
{
int fd;
int i;
FILE *f;
header_line *h;
struct stat statbuf;
char name[256];
char temp_name[256];

sprintf(temp_name, "%s/input/hdr.%d", spool_directory, (int)getpid());
fd = spool_open_temp(temp_name);
if (fd < 0)
  {
  DEBUG(2) debug_printf("Failed to open %s\n", temp_name);
  return 0;
  }

f = fdopen(fd, "w");
DEBUG(9) debug_printf("Writing spool header file\n");

/* We now have an open file to which the header data is to be written. Start
with the message id, to make the file self-identifying. Continue with the
identity of the submitting user, followed by the sender's address. The sender's
address is enclosed in <> because it might be the null address. Then write the
received time and the number of warning messages that have been sent. */

fprintf(f, "%s-H\n", message_id);
fprintf(f, "%s %d %d\n", originator_login, (int)originator_uid,
  (int)originator_gid);
fprintf(f, "<%s>\n", sender_address);
fprintf(f, "%d %d\n", received_time, warning_count);

/* Now any flags that need to be remembered. */

if (sender_local) fprintf(f, "-local\n");
if (local_error_message) fprintf(f, "-localerror\n");
if (header_names != header_names_normal) fprintf(f, "-resent\n");
if (deliver_freeze) fprintf(f, "-frozen %d\n", deliver_frozen_at);
if (deliver_manual_thaw) fprintf(f, "-manual_thaw\n");
if (user_null_sender) fprintf(f, "-user_null_sender\n");

/* To complete the envelope, write out the tree of non-recipients, followed by
the list of recipients. These won't be disjoint the first time, when no
checking has been done. */

tree_write(tree_nonrecipients, f);
fprintf(f, "%d\n", recipients_count);
for (i = 0; i < recipients_count; i++)
  fprintf(f, "%s\n", recipients_list[i]);

/* Finally, write out the message's headers. To make it easier to read them
in again, precede each one with the count of its length. Make the count fixed
length simply to aid human eyes when debugging. It is followed by a space
for normal headers, a flagging letter for various other headers, or an asterisk
for old headers that have been rewritten. These are saved as a record for
debugging. */

fprintf(f, "\n");
for (h = header_list; h != NULL; h = h->next)
  fprintf(f, "%03d%c %s", h->slen, h->type, h->text);

/* Flush and check for any errors while writing */

if (fflush(f) != 0 || ferror(f))
  {
  unlink(temp_name);
  fclose(f);
  DEBUG(2) debug_printf("Error while writing %s\n", temp_name);
  return 0;
  }

/* Force the file's contents to be written to disc. Note that fflush()
just pushes it out of C, and fclose() doesn't guarantee to do the write
either. That's just the way Unix works... */

if (fsync(fileno(f)) < 0)
  {
  unlink(temp_name);
  fclose(f);
  DEBUG(2) debug_printf("fsync on %s failed: %s\n", temp_name, strerror(errno));
  return 0;
  }

/* Everything is now written. Get the size of the file, close it, and rename it
to its correct name, thereby replacing any previous incarnation. */

fstat(fd, &statbuf);
fclose(f);

sprintf(name, "%s/input/%s-H", spool_directory, id);

if (rename(temp_name, name) < 0)
  {
  unlink(temp_name);
  DEBUG(2) debug_printf("Failed to rename %s as %s\n", temp_name, name);
  return 0;
  }

return statbuf.st_size;
}

/* End of spool_out.c */

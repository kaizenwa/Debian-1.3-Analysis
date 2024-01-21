/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions for reading spool files. */


#include "exim.h"



/*************************************************
*           Open and lock data file              *
*************************************************/

/* The data file is the one that is used for locking, because the header file
can get replaced during delivery because of header rewriting. The file has
to opened with write access so that we can get an exclusive lock, but in
fact it won't be written to. Just in case there's a major disaster (e.g.
overwriting some other file descriptor with the value of this one), open it
with append.

Argument: the id of the message
Returns:  TRUE if file successfully opened and locked

Side effect: deliver_datafile is set to the fd of the open file.
*/

BOOL
spool_open_datafile(char *id)
{
struct stat statbuf;
flock_t lock_data;
char spoolname[256];

sprintf(spoolname, "%s/input/%s-D", spool_directory, id);
deliver_datafile = open(spoolname, O_RDWR | O_APPEND);

if (deliver_datafile < 0)
  {
  if (errno == ENOENT)
    {
    log_write(0, LOG_MAIN, "Spool file %s-D not found", id);
    return FALSE;
    }
  log_write(0, LOG_MAIN, "Spool error for %s: %s", spoolname, strerror(errno));
  return FALSE;
  }

lock_data.l_type = F_WRLCK;
lock_data.l_whence = lock_data.l_start = lock_data.l_len = 0;

if (fcntl(deliver_datafile, F_SETLK, &lock_data) < 0)
  {
  if (message_id[0] == 0 || strcmp(message_id, id) != 0)
    log_write(5, LOG_MAIN, "Spool file %s-D is locked", id);
  else
    log_write(5, LOG_MAIN, "Spool file is locked");
  close(deliver_datafile);
  deliver_datafile = -1;
  return FALSE;
  }

if (fstat(deliver_datafile, &statbuf) == 0) message_size = statbuf.st_size;
return TRUE;
}



/*************************************************
*    Read non-recipients tree from spool file    *
*************************************************/

/* The tree of non-recipients is written to the spool file in a form that
makes it easy to read back into a tree. The format is as follows:

   . Each node is preceded by two letter(Y/N) indicating whether it has left
     or right children. There's one space after the two flags, before the name.

   . The left subtree (if any) then follows, then the right subtree (if any).

This function is entered with the next input line in the buffer. Note we must
save the right flag before recursing with the same buffer.

Once the tree is read, we re-construct the balance fields by scanning the tree.
I forgot to write them out originally, and the compatible fix is to do it this
way. This initial local recursing function does the necessary.

Arguments:
  node      tree node

Returns:    maximum depth below the node, including the node itself
*/

static int
count_below(tree_node *node)
{
int nleft, nright;
if (node == NULL) return 0;
nleft = count_below(node->left);
nright = count_below(node->right);
node->balance = (nleft > nright)? 1 : ((nright > nleft)? 2 : 0);
return 1 + ((nleft > nright)? nleft : nright);
}

/* This is the real function...

Arguments:
  connect   pointer to the root of the tree
  f         FILE to read data from
  buffer    contains next input line; further lines read into it

Returns:    FALSE on format error
*/

static BOOL
read_nonrecipients_tree(tree_node **connect, FILE *f, char *buffer)
{
tree_node *node;
int n = (int)strlen(buffer);
BOOL right = buffer[1] == 'Y';

if (n < 5) return FALSE;    /* malformed line */
buffer[n-1] = 0;            /* Remove \n */
node = store_malloc(sizeof(tree_node) + n - 3);
*connect = node;
strcpy(node->name, buffer + 3);
node->data.ptr = NULL;

if (buffer[0] == 'Y')
  {
  if (fgets(buffer, 256, f) == NULL ||
    !read_nonrecipients_tree(&node->left, f, buffer)) return FALSE;
  }
else node->left = NULL;

if (right)
  {
  if (fgets(buffer, 256, f) == NULL ||
    !read_nonrecipients_tree(&node->right, f, buffer)) return FALSE;
  }
else node->right = NULL;

(void) count_below(*connect);
return TRUE;
}




/*************************************************
*             Read spool header file             *
*************************************************/

/* This function reads a spool header file and places the data into the
appropriate global variables. The header portion is always read, but header
structures are built only if read_headers is set true. It isn't, for example,
while generating -bp output.

It may be possible for blocks of nulls (binary zeroes) to get written on the
end of a file if there is a system crash during writing. It was observed on an
earlier version of Exim that omitted to fsync() the files - this is thought to
have been the cause of that incident, but in any case, this code must be robust
against such an event, and if such a file is encountered, it must be treated as
malformed.

Arguments:
  name          name of the header file, including the -H
  read_headers  TRUE if in-store header structures are to be built

Returns:        spool_read_OK        success
                spool_read_notopen   open failed
                spool_read_enverror  error in the envelope portion
                spool_read_hdrdrror  error in the header portion
*/

int
spool_read_header(char *name, BOOL read_headers)
{
FILE *f;
int n;
BOOL inheader = FALSE;
char originator[64];

/* Ensure all pointers to store that will be obtained are NULL. This
means that we can free partially got store if this function fails */

header_list = header_last = NULL;
sender_address = NULL;
recipients_list = NULL;
tree_nonrecipients = NULL;

/* Generate the full name and open the file */

sprintf(big_buffer, "%s/input/%s", spool_directory, name);
f = fopen(big_buffer, "r");
if (f == NULL) return spool_read_notopen;
errno = 0;

DEBUG(5) debug_printf("Opened spool file %s\n", name);

/* The first line of a spool file contains the message id followed by -H (i.e.
the file name), in order to make the file self-identifying. However, versions
of Exim before 0.57 did not do this, so make this line optional for
compatibility. Change made in October 1996. After a year or so, the optionality
can be removed. Set data_start_offset to the length of the message id + 3
because the identification will also be present in the data file if it is in
the header file. */

data_start_offset = 0;

if (fgets(big_buffer, big_buffer_size, f) == NULL) goto SPOOL_READ_ERROR;

if ((int)strlen(big_buffer) == MESSAGE_ID_LENGTH + 3)
  {
  if (strncmp(big_buffer, name, MESSAGE_ID_LENGTH + 2) == 0)
    {
    data_start_offset = MESSAGE_ID_LENGTH + 3;
    if (fgets(big_buffer, big_buffer_size, f) == NULL) goto SPOOL_READ_ERROR;
    }
  }

/* We now have the first proper data line in the big buffer. The first three
data lines of spool files are in a fixed format. The first contains the login,
uid, and gid of the user who caused the file to be written. The second contains
the mail address of the message's sender, enclosed in <>. The third contains
the time the message was received, and the number of warning messages for
delivery delays that have been sent. */

if (sscanf(big_buffer, "%s %d %d", originator, (int *)(&originator_uid),
  (int *)(&originator_gid)) != 3) goto SPOOL_FORMAT_ERROR;
originator_login = string_copy(originator);

if (fgets(big_buffer, big_buffer_size, f) == NULL) goto SPOOL_READ_ERROR;
n = (int)strlen(big_buffer);
if (n < 3 || big_buffer[0] != '<' || big_buffer[n-2] != '>')
  goto SPOOL_FORMAT_ERROR;
sender_address = store_malloc(n-2);
strncpy(sender_address, big_buffer+1, n-3);
sender_address[n-3] = 0;

if (fgets(big_buffer, big_buffer_size, f) == NULL) goto SPOOL_READ_ERROR;
if (sscanf(big_buffer, "%d %d", &received_time, &warning_count) != 2)
  goto SPOOL_FORMAT_ERROR;

DEBUG(5) debug_printf("user=%s uid=%d gid=%d sender=%s\n", originator_login,
  (int)originator_uid, (int)originator_gid, sender_address);

/* Now there may be a number of options lines, each starting with "-".
First set default values for the options. */

sender_local = FALSE;
header_names = header_names_normal;
deliver_freeze = FALSE;
deliver_frozen_at = 0;
local_error_message = FALSE;
user_null_sender = FALSE;

for (;;)
  {
  if (fgets(big_buffer, big_buffer_size, f) == NULL) goto SPOOL_READ_ERROR;
  if (big_buffer[0] != '-') break;
  if (strcmp(big_buffer, "-local\n") == 0) sender_local = TRUE;
    else if (strcmp(big_buffer, "-localerror\n") == 0)
      local_error_message = TRUE;
    else if (strcmp(big_buffer, "-resent\n") == 0)
      header_names = header_names_resent;
    else if (strncmp(big_buffer, "-frozen", 7) == 0)
      {
      deliver_freeze = TRUE;
      deliver_frozen_at = atoi(big_buffer + 7);
      }
    else if (strcmp(big_buffer, "-manual_thaw\n") == 0)
      deliver_manual_thaw = TRUE;
    else if (strcmp(big_buffer, "-user_null_sender\n") == 0)
      user_null_sender = TRUE;

  /* To allow new versions of Exim that add additional flags to interwork
  with older versions that do not understand them, just ignore any flagged
  lines that we don't recognize. Otherwise it wouldn't be possible to back
  off a new version that left new-style flags written on the spool. That's
  why the following line is commented out. */

    /* else goto SPOOL_FORMAT_ERROR; */
  }

DEBUG(5)
  debug_printf("sender_local=%d resent=%s\n", sender_local,
    (header_names == header_names_normal)? "no" : "yes");

/* We now have the tree of addresses NOT to deliver to, or a line
containing "XX", indicating no tree. */

if (strncmp(big_buffer, "XX\n", 3) != 0 &&
  !read_nonrecipients_tree(&tree_nonrecipients, f, big_buffer))
    goto SPOOL_FORMAT_ERROR;

DEBUG(5)
  {
  debug_printf("Non-recipients:\n");
  tree_print(tree_nonrecipients, debug_file);
  }

/* After reading the tree, the next line has not yet been read into the
buffer. It contains the count of recipients which follow on separate lines. */

if (fgets(big_buffer, big_buffer_size, f) == NULL) goto SPOOL_READ_ERROR;
if (sscanf(big_buffer, "%d", &recipients_count) != 1) goto SPOOL_FORMAT_ERROR;

DEBUG(5) debug_printf("recipients_count=%d\n", recipients_count);

recipients_list_max = recipients_count;
recipients_list = store_malloc(recipients_count * sizeof(char *));

for (n = 0; n < recipients_count; n++)
  {
  int nn;
  char *receiver;
  if (fgets(big_buffer, big_buffer_size, f) == NULL) goto SPOOL_READ_ERROR;
  nn = (int)strlen(big_buffer);
  if (nn < 2) goto SPOOL_FORMAT_ERROR;
  receiver = store_malloc(nn);
  big_buffer[nn-1] = 0;
  strcpy(receiver, big_buffer);
  recipients_list[n] = receiver;
  }

/* The remainder of the spool header file contains the headers for the message,
separated off from the previous data by a blank line. Each header is preceded
by a count of its length and either a certain letter (for various identified
headers), space (for a miscellaneous live header) or an asterisk (for a header
that has been rewritten). Count the Received: headers. We read the headers
always, in order to check on the format of the file, but only create a header
list if requested to do so. */

inheader = TRUE;
if (fgets(big_buffer, big_buffer_size, f) == NULL) goto SPOOL_READ_ERROR;
if (big_buffer[0] != '\n') goto SPOOL_FORMAT_ERROR;

while ((n = fgetc(f)) != EOF)
  {
  header_line *h;
  char flag[4];
  int i;

  if (!isdigit(n)) goto SPOOL_FORMAT_ERROR;

  ungetc(n, f);
  fscanf(f, "%d%c ", &n, flag);
  if (!isprint(flag[0])) goto SPOOL_FORMAT_ERROR;
  message_size += n;

  if (read_headers)
    {
    h = store_malloc(sizeof(header_line) + n);
    h->next = NULL;
    h->type = flag[0];
    h->slen = n;

    if (h->type == htype_received) received_count++;

    if (header_list == NULL)
      {
      header_list = h;
      h->prev = NULL;
      }
    else
      {
      header_last->next = h;
      h->prev = header_last;
      }
    header_last = h;

    for (i = 0; i < n; i++)
      {
      int c = fgetc(f);
      if (c == 0 || c == EOF) goto SPOOL_FORMAT_ERROR;
      h->text[i] = c;
      }
    h->text[i] = 0;

    /* Make the Precedence value available in a variable. This isn't
    a standard RFC 822 header, but it is in widespread use. */

    if (header_checkname(h, "Precedence", 10))
      {
      char *s = h->text + 10;
      while (isspace(*s)) s++;
      s++;
      while (isspace(*s)) s++;
      message_precedence = string_copy(s);
      }
    }

  /* Not requiring header data, just skip through the bytes */

  else for (i = 0; i < n; i++)
    {
    int c = fgetc(f);
    if (c == 0 || c == EOF) goto SPOOL_FORMAT_ERROR;
    }
  }

/* We have successfully read the data in the header file. Close it and
give a positive response. */

fclose(f);
return spool_read_OK;


/* There was an error reading the spool or there was missing data,
or there was a format error. A "read error" with no errno means an
unexpected EOF, which we treat as a format error. */

SPOOL_READ_ERROR:
if (errno != 0)
  {
  n = errno;
  DEBUG(1) debug_printf("Error while reading spool file %s\n", name);
  fclose(f);
  errno = n;
  return inheader? spool_read_hdrerror : spool_read_enverror;
  }

SPOOL_FORMAT_ERROR:
DEBUG(1) debug_printf("Format error in spool file %s\n", name);
fclose(f);
errno = ERRNO_SPOOLFORMAT;
return inheader? spool_read_hdrerror : spool_read_enverror;
}

/* End of spool_in.c */

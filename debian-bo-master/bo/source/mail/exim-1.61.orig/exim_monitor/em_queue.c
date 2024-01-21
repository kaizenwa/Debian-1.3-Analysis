/*************************************************
*                 Exim Monitor                   *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "em_hdr.h"


/* This module contains functions to do with scanning exim's
queue and displaying the data therefrom. */


/*************************************************
*                 Static variables               *
*************************************************/

static int input_mtime = 0;   /* mtime for input directory */
static int queue_total = 0;   /* number of items in queue */

/* Table for turning base-62 numbers into binary */

static char tab62[] =
          {0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0,     /* 0-9 */
           0,10,11,12,13,14,15,16,17,18,19,20,  /* A-K */
          21,22,23,24,25,26,27,28,29,30,31,32,  /* L-W */
          33,34,35, 0, 0, 0, 0, 0,              /* X-Z */
           0,36,37,38,39,40,41,42,43,44,45,46,  /* a-k */
          47,48,49,50,51,52,53,54,55,56,57,58,  /* l-w */
          59,60,61};                            /* x-z */

/* Index for quickly finding things in the ordered queue. */

static queue_item *queue_index[queue_index_size];



/*************************************************
*         Find/Create/Delete a destination       *
*************************************************/

/* If the action is dest_noop, then just return item or NULL;
if it is dest_add, then add if not present, and return item;
if it is dest_remove, remove if present and return NULL. The
address is lowercased to start with, unless it begins with
"*", which it does for error messages. */

dest_item *find_dest(queue_item *q, char *name, int action)
{
dest_item *dd;
dest_item **d = &(q->destinations);
char *s = name;

if (*s != '*') while (*s) { *s = tolower(*s); s++; }

while (*d != NULL)
  {
  if (strcmp(name, (*d)->address) == 0)
    {
    if (action != dest_remove) return *d;
    dd = *d;
    *d = dd->next;
    free(dd);
    return NULL;
    }
  d = &((*d)->next);
  }

if (action != dest_add) return NULL;

dd = (dest_item *)malloc(sizeof(dest_item) + (int)strlen(name));
strcpy(dd->address, name);
dd->next = NULL;
dd->parent = NULL;
*d = dd;
return dd;
}



/*************************************************
*            Clean up a dead queue item          *
*************************************************/

static void clean_up(queue_item *p)
{
dest_item *dd = p->destinations;
while (dd != NULL)
  {
  dest_item *next = dd->next;
  free(dd);
  dd = next;
  }
if (p->sender != NULL) free(p->sender);
free(p);
}


/*************************************************
*             Set up new queue item              *
*************************************************/

static queue_item *set_up(char *name)
{
int i, rc, save_errno;
struct stat statdata;
char *p;
queue_item *q = (queue_item *)malloc(sizeof(queue_item));
char buffer[256];

/* Initialize the block */

q->next = q->prev = NULL;
q->destinations = NULL;
strcpy(q->name, name);
q->seen = TRUE;
q->frozen = FALSE;
q->sender = NULL;
q->size = 0;

/* Read the header file from the spool; if there is a failure it
might mean inaccessibility as a result of protections.
A successful read will have caused sender_address to get set and
the recipients fields to be initialized. If there's a format error
in the headers, we can still display info from the envelope. */

message_size = 0;
sprintf(buffer, "%s-H", name);
rc =  spool_read_header(buffer, FALSE);
save_errno = errno;

/* If we failed to read the envelope, compute the input time by
interpreting the id as a base-62 number. */

if (rc != spool_read_OK && rc != spool_read_hdrerror)
  {
  int t = 0;
  for (i = 0; i < 6; i++) t = t * 62 + tab62[name[i] - '0'];
  q->update_time = q->input_time = t;
  }

/* Envelope read; get input time and sender. */

else
  {
  q->update_time = q->input_time = received_time;
  /* Remove qualify_domain from sender address, and then save it. */
  if ((p = strstric(sender_address+1, qualify_domain, FALSE)) != NULL &&
    *(--p) == '@') *p = 0;
  }

/* If we didn't read the whole header successfully, generate an error
message. If the envelope was read, this appears as a first recipient;
otherwise it sets set up in the sender field. */

if (rc != spool_read_OK)
  {
  char *msg;

  if (save_errno == ERRNO_SPOOLFORMAT)
    {
    struct stat statbuf;
    sprintf(big_buffer, "%s/input/%s", spool_directory, buffer);
    if (stat(big_buffer, &statbuf) == 0)
      msg = string_sprintf("*** Format error in spool file: size = %d ***",
        statbuf.st_size);
    else msg = string_sprintf("*** Format error in spool file ***");
    }
  else msg = string_sprintf("*** Cannot read spool file ***");

  if (rc == spool_read_hdrerror)
    {
    (void)find_dest(q, msg, dest_add);
    free(msg);
    }
  else
    {
    deliver_freeze = FALSE;
    sender_address = msg;
    }
  }

/* Now set up the remaining data */

q->frozen = deliver_freeze;
q->sender = sender_address;
sender_address = NULL;

sprintf(buffer, "%s/input/%s-D", spool_directory, name);
if (stat(buffer, &statdata) == 0) q->size = message_size + statdata.st_size;

/* Scan and process the recipients list, skipping any that have already
been delivered, and removing visible names. In the nonrecipients list,
domains are lower-cased. */

if (recipients_list != NULL)
  {
  for (i = 0; i < recipients_count; i++)
    {
    BOOL delivered = TRUE;
    char *r = recipients_list[i];
    if (tree_search(tree_nonrecipients, r) == NULL)
      {
      char *rr = r;
      while (*rr != 0 && *rr != '@') rr++;
      while (*rr != 0) { *rr = tolower(*rr); rr++; }
      if (tree_search(tree_nonrecipients, r) == NULL)
        delivered = FALSE;
      }

    if (!delivered)
      {
      if ((p = strstric(r+1, qualify_domain, FALSE)) != NULL &&
        *(--p) == '@') *p = 0;
      (void)find_dest(q, r, dest_add);
      }
    free(r);
    }
  free(recipients_list);
  }

tree_free(tree_nonrecipients);
return q;
}



/*************************************************
*             Find/Create a queue item           *
*************************************************/

/* The queue is kept as a doubly-linked list, sorted by name. However,
to speed up searches, an index into the list is used. This is maintained
by the scan_spool_input function when it goes down the list throwing
out entries that are no longer needed. When the action is "add" and
we don't need to add, mark the found item as seen. */


#ifdef never
static void debug_queue(void)
{
int i;
int count = 0;
queue_item *p;
printf("\nqueue_total=%d\n", queue_total);

for (i = 0; i < queue_index_size; i++)
  printf("index %d = %d %s\n", i, (int)(queue_index[i]),
    (queue_index[i])->name);

printf("Queue is:\n");
p = queue_index[0];
while (p != NULL)
  {
  count++;
  for (i = 0; i < queue_index_size; i++)
    {
    if (queue_index[i] == p) printf("count=%d index=%d\n", count, (int)p);
    }
  printf("%d %d %d %s\n", (int)p, (int)p->next, (int)p->prev, p->name);
  p = p->next;
  }
}
#endif



queue_item *find_queue(char *name, int action)
{
int first = 0;
int last = queue_index_size - 1;
int middle = (first + last)/2;
queue_item *p, *q, *qq;

/* Handle the empty queue as a special case. */

if (queue_total == 0)
  {
  if (action != queue_add) return NULL;
  if ((qq = set_up(name)) != NULL)
    {
    int i;
    for (i = 0; i < queue_index_size; i++) queue_index[i] = qq;
    queue_total++;
    return qq;
    }
  return NULL;
  }

/* Also handle insertion at the start or end of the queue
as special cases. */

if (strcmp(name, (queue_index[0])->name) < 0)
  {
  if (action != queue_add) return NULL;
  if ((qq = set_up(name)) != NULL)
    {
    qq->next = queue_index[0];
    (queue_index[0])->prev = qq;
    queue_index[0] = qq;
    queue_total++;
    return qq;
    }
  return NULL;
  }

if (strcmp(name, (queue_index[queue_index_size-1])->name) > 0)
  {
  if (action != queue_add) return NULL;
  if ((qq = set_up(name)) != NULL)
    {
    qq->prev = queue_index[queue_index_size-1];
    (queue_index[queue_index_size-1])->next = qq;
    queue_index[queue_index_size-1] = qq;
    queue_total++;
    return qq;
    }
  return NULL;
  }

/* Use binary chopping on the index to get a range of the queue to search
when the name is somewhere in the middle, if present. */

while (middle > first)
  {
  if (strcmp(name, (queue_index[middle])->name) >= 0) first = middle;
    else last = middle;
  middle = (first + last)/2;
  }

/* Now search down the part of the queue in which the item must
lie if it exists. Both end points are inclusive - though in fact
the bottom one can only be = if it is the original bottom. */

p = queue_index[first];
q = queue_index[last];

for (;;)
  {
  int c = strcmp(name, p->name);

  /* Already on queue; mark seen if required. */

  if (c == 0)
    {
    if (action == queue_add) p->seen = TRUE;
    return p;
    }

  /* Not on the queue; add an entry if required. Note that set-up might
  fail (the file might vanish under our feet). Note also that we know
  there is always a previous item to p because the end points are
  inclusive. */

  else if (c < 0)
    {
    if (action == queue_add)
      {
      if ((qq = set_up(name)) != NULL)
        {
        qq->next = p;
        qq->prev = p->prev;
        p->prev->next = qq;
        p->prev = qq;
        queue_total++;
        return qq;
        }
      }
    return NULL;
    }

  /* Control should not reach here if p == q, because the name
  is supposed to be <= the name of the bottom item. */

  if (p == q) return NULL;

  /* Else might be further down the queue; continue */

  p = p->next;
  }

/* Control should never reach here. */
}



/*************************************************
*        Scan the exim spool directory           *
*************************************************/

/* The scan happens only if the directory has been changed, unless forced. We
count the entries to set the value for the queue stripchart, and set up data
for the queue display window if the "full" option is given. */

void scan_spool_input(int full, int force)
{
struct stat statdata;
char input_dir[256];

sprintf(input_dir, "%s/input", spool_directory);
if (stat(input_dir, &statdata)) return;

if (force || input_mtime != statdata.st_mtime)
  {
  queue_item *p;
  int count = 0;
  int indexptr = 1;
  struct dirent *ent;
  DIR *dd = opendir(input_dir);
  if (dd == NULL) return;

  stripchart_total[0] = 0;

  /* Loop for each spool file on the queue - there can be temporary
  files with non-standard names that must also be ignored. Do a
  quick check pro tem. When initializing eximon, every file will
  have to be read. To show there is progress, output a dot for each
  one to the standard output. If not doing a full queue scan (but
  just counting) don't waste time searching the in-store queue. */

  while ((ent = readdir(dd)) != NULL)
    {
    char *name = ent->d_name;
    if ((int)strlen(name) == SPOOL_NAME_LENGTH &&
        name[SPOOL_NAME_LENGTH - 2] == '-' &&
        name[SPOOL_NAME_LENGTH - 1] == 'H')
      {
      char basename[SPOOL_NAME_LENGTH];
      stripchart_total[0]++;
      if (!eximon_initialized) { printf("."); fflush(stdout); }
      strcpy(basename, name);
      basename[SPOOL_NAME_LENGTH - 2] = 0;
      if (full) find_queue(basename, queue_add);
      }
    }
  closedir(dd);

  /* If simply counting the number, we are done; same if there are no
  items in the in-store queue. */

  if (!full || queue_total == 0) return;

  /* Now scan the queue and remove any items that were not
  in the directory. At the same time, set up the index pointers
  into the queue. Because we are removing items, the total that
  we are comparing against isn't actually correct, but in a long
  queue it won't make much difference, and in a short queue it
  doesn't matter anyway!*/

  p = queue_index[0];
  while (p != NULL)
    {
    if (!p->seen)
      {
      queue_item *next = p->next;
      if (p->prev == NULL) queue_index[0] = next;
        else p->prev->next = next;
      if (next == NULL)
        {
        int i;
        queue_item *q = queue_index[queue_index_size-1];
        for (i = queue_index_size - 1; i >= 0; i--)
          if (queue_index[i] == q) queue_index[i] = p->prev;
        }
      else next->prev = p->prev;
      clean_up(p);
      queue_total--;
      p = next;
      }
    else
      {
      if (++count > (queue_total * indexptr)/(queue_index_size-1))
        {
        queue_index[indexptr++] = p;
        }
      p->seen = FALSE;  /* for next time */
      p = p->next;
      }
    }

  /* If a lot of messages have been removed at the bottom, we may not
  have got the index all filled in yet. Make sure all the pointers
  are legal. */

  while (indexptr < queue_index_size - 1)
    {
    queue_index[indexptr++] = queue_index[queue_index_size-1];
    }

  input_mtime = statdata.st_mtime;
  }
}




/*************************************************
*    Update the recipients list for a message    *
*************************************************/

/* We read it only if its update time differs from last time. */

static void update_recipients(queue_item *p)
{
int i;
char buffer[1024];
struct stat statdata;

sprintf(buffer, "%s/input/%s-H", spool_directory, p->name);
if (stat(buffer, &statdata) < 0 || p->update_time == statdata.st_mtime)
  return;

sprintf(buffer, "%s-H", p->name);
if (spool_read_header(buffer, FALSE) != spool_read_OK) return;

/* Scan and process the recipients list, removing any that have already
been delivered, and removing visible names. In the nonrecipients tree,
domains are lower cased. */

if (recipients_list != NULL)
  {
  for (i = 0; i < recipients_count; i++)
    {
    char *pp;
    char *r = recipients_list[i];
    tree_node *node = tree_search(tree_nonrecipients, r);

    if (node == NULL)
      {
      char temp[256];
      char *rr = temp;
      strcpy(temp, r);
      while (*rr != 0 && *rr != '@') rr++;
      while (*rr != 0) { *rr = tolower(*rr); rr++; }
      node = tree_search(tree_nonrecipients, temp);
      }

    if ((pp = strstric(r+1, qualify_domain, FALSE)) != NULL &&
      *(--pp) == '@') *pp = 0;
    if (node == NULL)
      (void)find_dest(p, r, dest_add);
    else
      (void)find_dest(p, r, dest_remove);
    free(r);
    }
  free(recipients_list);
  }

tree_free(tree_nonrecipients);
p->update_time = statdata.st_mtime;
}



/*************************************************
*              Display queue data                *
*************************************************/

/* The present implementation simple re-writes the entire information each
time. Take some care to keep the scrolled position as it previously was, but,
if it was at the bottom, keep it at the bottom. Take note of any hide list, and
time out the entries as appropriate. */

void
queue_display(void)
{
int now = (int)time(NULL);
queue_item *p = queue_index[0];

if (menu_is_up) return;            /* Avoid nasty interactions */

text_empty(queue_widget);

while (p != NULL)
  {
  int count = 1;
  dest_item *dd, *ddd;
  char u = 'm';
  int t = (now - p->input_time)/60;  /* minutes on queue */

  if (t > 90)
    {
    u = 'h';
    t = (t + 30)/60;
    if (t > 72)
      {
      u = 'd';
      t = (t + 12)/24;
      }
    }

  update_recipients(p);                   /* update destinations */

  /* Can't set this earlier, as header data may change things. */

  dd = p->destinations;

  /* Check to see if this message is on the hide list; if any hide
  item has timed out, remove it from the list. Hide if all destinations
  are on the hide list. */

  for (ddd = dd; ddd != NULL; ddd = ddd->next)
    {
    skip_item *sk;
    skip_item **skp;
    int len_address;

    if (ddd->address[0] == '*') break;
    len_address = (int)strlen(ddd->address);

    for (skp = &queue_skip; ; skp = &(sk->next))
      {
      int len_skip;

      sk = *skp;
      while (sk != NULL && now >= sk->reveal)
        {
        *skp = sk->next;
        free(sk);
        sk = *skp;
        if (queue_skip == NULL)
          {
          XtDestroyWidget(unhide_widget);
          unhide_widget = NULL;
          }
        }
      if (sk == NULL) break;

      /* If this address matches the skip item, break (sk != NULL) */

      len_skip = (int)strlen(sk->text);
      if (len_skip <= len_address &&
          strcmp(ddd->address + len_address - len_skip, sk->text) == 0)
        break;
      }

    if (sk == NULL) break;
    }


  if (ddd != NULL)
    {
    text_showf(queue_widget, "%c%2d%c %s %s %-8s %s%s%s",
      (p->frozen)? '*' : ' ',
      t, u,
      string_formatsize(p->size, big_buffer),
      p->name,
      (p->sender == NULL)? "       " :
        (p->sender[0] == 0)? "<>     " : p->sender,
      (dd == NULL || dd->address[0] == '*')? "" : "<",
      (dd == NULL)? "" : dd->address,
      (dd == NULL || dd->address[0] == '*')? "" : ">");

    if (dd != NULL && dd->parent != NULL && dd->parent->address[0] != '*')
      text_showf(queue_widget, " parent <%s>", dd->parent->address);

    text_show(queue_widget, "\n");

    if (dd != NULL) dd = dd->next;
    while (dd != NULL && count++ < queue_max_addresses)
      {
      text_showf(queue_widget, "                                     <%s>",
        dd->address);
      if (dd->parent != NULL && dd->parent->address[0] != '*')
        text_showf(queue_widget, " parent <%s>", dd->parent->address);
      text_show(queue_widget, "\n");
      dd = dd->next;
      }
    if (dd != NULL)
      text_showf(queue_widget, "                               ...\n");
    }

  p = p->next;
  }

text_position(queue_widget, 999999);
}

/* End of em_queue.c */

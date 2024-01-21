/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions for reading the configuration file, and for displaying
overall configuration values. */

#include "exim.h"


typedef struct macro_item {
  struct macro_item *next;
  char *replacement;
  char name[1];
} macro_item;


static macro_item *macros = NULL;
static char time_buffer[24];


/*************************************************
*           Main configuration options           *
*************************************************/

/* The list of options that can be set in the main configuration file. This
must be in alphabetic order because it is searched by binary chop. */

static optionlist optionlist_config[] = {
  { "*set_exim_group",          opt_bool|opt_hidden, &exim_gid_set },
  { "*set_exim_user",           opt_bool|opt_hidden, &exim_uid_set },
  { "*set_message_filter_group",opt_bool|opt_hidden, &message_filter_gid_set },
  { "*set_message_filter_user", opt_bool|opt_hidden, &message_filter_uid_set },
  { "*set_nobody_group",        opt_bool|opt_hidden, &nobody_gid_set },
  { "*set_nobody_user",         opt_bool|opt_hidden, &nobody_uid_set },
  { "accept_8bitmime",          opt_bool,        &accept_8bitmime },
  { "accept_timeout",           opt_time,        &accept_timeout },
  { "address_file_transport",   opt_stringptr,   &address_file_transport },
  { "address_pipe_transport",   opt_stringptr,   &address_pipe_transport },
  { "address_reply_transport",  opt_stringptr,   &address_reply_transport },
  { "always_bcc",               opt_bool,        &always_bcc },
  { "auto_thaw",                opt_time,        &auto_thaw },
  { "bi_command",               opt_stringptr,   &bi_command },
  { "collapse_source_routes",   opt_bool,        &collapse_source_routes },
  { "debug_transport",          opt_stringptr,   &debug_transport_file },
  { "delay_warning",            opt_time,        &delay_warning },
  { "deliver_load_max",         opt_fixed,       &deliver_load_max },
  { "delivery_date_remove",     opt_bool,        &delivery_date_remove },
  { "dns_retrans",              opt_time,        &dns_retrans },
  { "dns_retry",                opt_int,         &dns_retry },
  { "envelope_to_remove",       opt_bool,        &envelope_to_remove },
  { "errors_address",           opt_stringptr,   &errors_address },
  { "errors_copy",              opt_stringptr,   &errors_copy },
  { "errors_reply_to",          opt_stringptr,   &errors_reply_to },
  { "exim_group",               opt_gid,         &exim_gid },
  { "exim_path",                opt_stringptr,   &exim_path },
  { "exim_user",                opt_uid,         &exim_uid },
  { "finduser_retries",         opt_int,         &finduser_retries },
  { "forbid_domain_literals",   opt_bool,        &forbid_domain_literals },
  { "freeze_tell_mailmaster",   opt_bool,        &freeze_tell_mailmaster },
  { "gecos_name",               opt_stringptr,   &gecos_name },
  { "gecos_pattern",            opt_stringptr,   &gecos_pattern },
  { "helo_verify_nets",         opt_stringptr,   &helo_verify_nets },
  { "ignore_errmsg_errors",     opt_bool,        &ignore_errmsg_errors },
  { "keep_malformed",           opt_time,        &keep_malformed },
  { "kill_ip_options",          opt_bool,        &kill_ip_options },
  { "local_domains",            opt_lcstringptr, &local_domains },
  { "local_domains_include_host", opt_bool,     &local_domains_include_host },
  { "local_domains_include_host_literals", opt_bool, &local_domains_include_host_literals },
  { "local_interfaces",         opt_stringptr,   &local_interfaces },
  { "locally_caseless",         opt_bool,        &locally_caseless },
  { "log_ip_options",           opt_bool,        &log_ip_options },
  { "log_level",                opt_int,         &log_level },
  { "log_received_recipients",  opt_bool,        &log_received_recipients },
  { "log_smtp_confirmation",    opt_bool,        &log_smtp_confirmation },
  { "log_subject",              opt_bool,        &log_subject },
  { "message_body_visible",     opt_mkint,       &message_body_visible },
  { "message_filter",           opt_stringptr,   &message_filter },
  { "message_filter_group",     opt_gid,         &message_filter_gid },
  { "message_filter_user",      opt_uid,         &message_filter_uid },
  { "message_id_header_text",   opt_stringptr,   &message_id_text },
  { "message_size_limit",       opt_mkint,       &message_size_limit },
  { "never_users",              opt_uidlist,     &never_users },
  { "nobody_group",             opt_gid,         &nobody_gid },
  { "nobody_user",              opt_uid,         &nobody_uid },
  { "percent_hack_domains",     opt_lcstringptr, &percent_hack_domains },
  { "preserve_message_logs",    opt_bool,        &preserve_message_logs },
  { "primary_hostname",         opt_lcstringptr, &primary_hostname },
  { "qualify_domain",           opt_lcstringptr, &qualify_domain_sender },
  { "qualify_recipient",        opt_lcstringptr, &qualify_domain_recipient },
  { "queue_only",               opt_bool,        &queue_only },
  { "queue_only_load",          opt_fixed,       &queue_only_load },
  { "queue_remote",             opt_bool,        &queue_remote },
  { "queue_run_max",            opt_int,         &queue_run_max },
  { "queue_smtp",               opt_bool,        &queue_smtp },
  { "received_header_text",     opt_stringptr,   &received_header_text },
  { "received_headers_max",     opt_int,         &received_headers_max },
  { "receiver_try_verify",      opt_bool,        &receiver_try_verify },
  { "receiver_unqualified_hosts", opt_lcstringptr,&receiver_unqualified_hosts },
  { "receiver_unqualified_nets", opt_stringptr, &receiver_unqualified_nets },
  { "receiver_verify",          opt_bool,        &receiver_verify },
  { "receiver_verify_except_hosts", opt_lcstringptr,&receiver_verify_except_hosts },
  { "receiver_verify_except_nets", opt_stringptr, &receiver_verify_except_nets },
  { "recipients_max",           opt_int,         &recipients_max },
  { "refuse_ip_options",        opt_bool,        &refuse_ip_options },
  { "relay_domains",            opt_lcstringptr, &relay_domains },
  { "remote_max_parallel",      opt_int,         &remote_max_parallel },
  { "remote_sort",              opt_stringptr,   &remote_sort },
  { "retry_interval_max",       opt_time,        &retry_interval_max },
  { "return_path_remove",       opt_bool,        &return_path_remove },
  { "return_size_limit",        opt_mkint,       &return_size_limit },
  { "rfc1413_except_hosts",     opt_lcstringptr, &rfc1413_except_hosts },
  { "rfc1413_except_nets",      opt_stringptr,   &rfc1413_except_nets },
  { "rfc1413_query_timeout",    opt_time,        &rfc1413_query_timeout },
  { "security",                 opt_lcstringptr, &security_type },
  { "sender_accept",            opt_stringptr,   &sender_accept },
  { "sender_accept_recipients", opt_stringptr,   &sender_accept_recipients },
  { "sender_address_relay",     opt_lcstringptr, &sender_address_relay },
  { "sender_host_accept",       opt_lcstringptr, &sender_host_accept },
  { "sender_host_accept_relay", opt_lcstringptr, &sender_host_accept_relay },
  { "sender_host_reject",       opt_lcstringptr, &sender_host_reject },
  { "sender_host_reject_recipients", opt_lcstringptr, &sender_host_reject_recipients },
  { "sender_host_reject_relay", opt_lcstringptr, &sender_host_reject_relay },
  { "sender_net_accept",        opt_stringptr,   &sender_net_accept },
  { "sender_net_accept_relay",  opt_stringptr,   &sender_net_accept_relay },
  { "sender_net_reject",        opt_stringptr,   &sender_net_reject },
  { "sender_net_reject_recipients", opt_stringptr, &sender_net_reject_recipients },
  { "sender_net_reject_relay",  opt_stringptr,   &sender_net_reject_relay },
  { "sender_reject",            opt_stringptr,   &sender_reject },
  { "sender_reject_recipients", opt_stringptr,   &sender_reject_recipients },
  { "sender_try_verify",        opt_bool,        &sender_try_verify },
  { "sender_unqualified_hosts", opt_lcstringptr,&sender_unqualified_hosts },
  { "sender_unqualified_nets",  opt_stringptr,   &sender_unqualified_nets },
  { "sender_verify",            opt_bool,        &sender_verify },
  { "sender_verify_except_hosts",opt_lcstringptr,&sender_verify_except_hosts },
  { "sender_verify_except_nets",opt_stringptr,  &sender_verify_except_nets },
  { "sender_verify_fixup",      opt_bool,        &sender_verify_fixup },
  { "sender_verify_log_details", opt_bool,      &sender_verify_log_details },
  { "sender_verify_reject",     opt_bool,        &sender_verify_reject },
  { "smtp_accept_max",          opt_int,         &smtp_accept_max },
  { "smtp_accept_queue",        opt_int,         &smtp_accept_queue },
  { "smtp_accept_reserve",      opt_int,         &smtp_accept_reserve },
  { "smtp_banner",              opt_stringptr,   &smtp_banner },
  { "smtp_connect_backlog",     opt_int,         &smtp_connect_backlog },
  { "smtp_etrn_hosts",          opt_lcstringptr, &smtp_etrn_hosts },
  { "smtp_etrn_nets",           opt_stringptr,   &smtp_etrn_nets },
  { "smtp_load_reserve",        opt_fixed,       &smtp_load_reserve },
  { "smtp_receive_timeout",     opt_time,        &smtp_receive_timeout },
  { "smtp_reserve_hosts",       opt_lcstringptr, &smtp_reserve_hosts },
  { "smtp_reserve_nets",        opt_stringptr,   &smtp_reserve_nets },
  { "smtp_verify",              opt_bool,        &smtp_verify },
  { "spool_directory",          opt_stringptr,   &spool_directory },
  { "strip_excess_angle_brackets", opt_bool,    &strip_excess_angle_brackets },
  { "strip_trailing_dot",       opt_bool,        &strip_trailing_dot },
  { "trusted_groups",           opt_gidlist,     &trusted_groups },
  { "trusted_users",            opt_uidlist,     &trusted_users },
  { "unknown_login",            opt_stringptr,   &unknown_login },
  { "unknown_username",         opt_stringptr,   &unknown_username }
};

static int optionlist_config_size =
  sizeof(optionlist_config)/sizeof(optionlist);



/*************************************************
*            Read configuration line             *
*************************************************/

/* A line of text is read from the configuration file into the big
buffer. Leading and trailing spaces are removed and the line is scanned
for macros, which are replaced. The size of big_buffer is increased if
necessary. The count of configuration lines is maintained. Comment lines are
always ignored. Continuation lines are handled if that is wanted.

Arguments:
  ignore_blank    if TRUE, blank lines are ignored
  get_continued   if TRUE and the line ends with '\', join the next one
                  onto it
  maybe_macro     if TRUE and the line starts with an upper case letter and
                  ends with '\', join the next ont onto it

Returns:          a pointer to the first non-blank in the line,
                  or NULL if eof is reached
*/

static char *
get_config_line(BOOL ignore_blank, BOOL get_continued, BOOL maybe_macro)
{
int n;
char *s, *ss;
macro_item *m;

/* Loop for skipping blanks lines and comments if required */

for (;;)
  {
  if (fgets(big_buffer, big_buffer_size, config_file) == NULL) return NULL;
  config_lineno++;
  n = (int)strlen(big_buffer);
  while (n > 0 && isspace(big_buffer[n-1])) n--;
  big_buffer[n] = 0;
  s = big_buffer;
  while (isspace(*s)) s++;
  if (*s != '#' && (*s != 0 || !ignore_blank)) break;
  }

/* Handle continuation lines if requested to do so - this isn't used for
strings, which handle continuation themselves. Otherwise the maximum is
limited by the size of the big buffer. */

if (get_continued || (maybe_macro && isupper(*s)))
  {
  while (big_buffer[n-1] == '\\')
    {
    int c = fgetc(config_file);
    while (c == ' ' || c == '\t') c = fgetc(config_file);
    if (c == '\n' || c == EOF) { config_lineno++; break; }

    if (big_buffer_size - n < 256)
      {
      char *newbuffer;
      big_buffer_size += BIG_BUFFER_SIZE;
      newbuffer = store_malloc(big_buffer_size);
      strcpy(newbuffer, s);
      s = newbuffer;
      n = (int)strlen(newbuffer);
      store_free(big_buffer);
      big_buffer = newbuffer;
      }

    if (fgets(big_buffer+n, big_buffer_size-n, config_file) == NULL) break;
    config_lineno++;
    if (c == '#') continue;
    big_buffer[n-1] = c;
    n = (int)strlen(big_buffer);
    while (n > 0 && isspace(big_buffer[n-1])) n--;
    big_buffer[n] = 0;
    }
  }

/* Handle macro expansion. This is done very simply. */

ss = big_buffer + n;
for (m = macros; m != NULL; m = m->next)
  {
  char *p, *pp;
  char *t = s;
  while ((p = strstr(t, m->name)) != NULL)
    {
    int moveby;
    int namelen = (int)strlen(m->name);
    int replen = (int)strlen(m->replacement);

    /* Expand the buffer if necessary */

    while (ss - big_buffer - namelen + replen + 1 > big_buffer_size)
      {
      int newsize = big_buffer_size + BIG_BUFFER_SIZE;
      char *newbuffer = store_malloc(newsize);
      strcpy(newbuffer, big_buffer);
      s = newbuffer  + (s - big_buffer);
      ss = newbuffer + (ss - big_buffer);
      t = newbuffer  + (t - big_buffer);
      p = newbuffer  + (p - big_buffer);
      big_buffer_size = newsize;
      store_free(big_buffer);
      big_buffer = newbuffer;
      }

    /* Shuffle the remaining characters up or down in the buffer before
    copying in the replacement text. Don't rescan the replacement for this same
    macro. */

    pp = p + namelen;
    moveby = replen - namelen;
    if (moveby != 0)
      {
      memmove(p + replen, pp, ss - pp + 1);
      ss += moveby;
      }
    strncpy(p, m->replacement, replen);
    t = p + replen;
    }
  }

/* Return the first non-blank character. */

return s;
}



/*************************************************
*             Read a name                        *
*************************************************/

/* The yield is the pointer to the next char. Names longer than the
output space are silently truncated.

Arguments:
  name      where to put the name
  len       length of name
  s         input pointer

Returns:    new input pointer
*/

char *
readconf_readname(char *name, int len, char *s)
{
int p = 0;
while (isspace(*s)) s++;
if (isalpha(*s))
  {
  while (isalnum(*s) || *s == '_')
    {
    if (p < len-1) name[p++] = *s;
    s++;
    }
  }
name[p] = 0;
while (isspace(*s)) s++;
return s;
}




/*************************************************
*          Read a time value                     *
*************************************************/

/* This function is also called from outside, to read argument
time values. The format of a time value is:

  [<n>w][<n>d][<n>h][<n>m][<n>s]

as long as at least one is present. If a format error is encountered,
return a negative value. The value must be terminated by the given
terminator.

Arguments:
  s           input pointer
  terminator  required terminating character

Returns:      the time value, in seconds, or -1 on syntax error
*/

int
readconf_readtime(char *s, int terminator)
{
int yield = 0;
for (;;)
  {
  int value, count;
  if (!isdigit(*s)) return -1;
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

    default: return -1;
    }
  yield += value;
  if (*s == terminator) return yield;
  }
/* Control never reaches here. */
}



/*************************************************
*          Read a fixed point value              *
*************************************************/

/* The value is returned *1000

Arguments:
  s           input pointer
  terminator  required terminator

Returns:      the value, or -1 on error
*/

static int
readconf_readfixed(char *s, int terminator)
{
int yield = 0;
int value, count;
if (!isdigit(*s)) return -1;
(void)sscanf(s, "%d%n", &value, &count);
s += count;
yield = value * 1000;
if (*s == '.')
  {
  int m = 100;
  while (isdigit(*(++s)))
    {
    yield += (*s - '0') * m;
    m /= 10;
    }
  }

return (*s == terminator)? yield : (-1);
}



/*************************************************
*            Find option in list                 *
*************************************************/

/* The lists are always in order, so binary chop can be used.

Arguments:
  name      the option name to search for
  ol        the first entry in the option list
  last      one more than the offset of the last entry in the option list

Returns:    pointer to an option entry, or NULL if not found
*/

static optionlist *
find_option(char *name, optionlist *ol, int last)
{
int first = 0;
while (last > first)
  {
  int middle = (first + last)/2;
  int c = strcmp(name, ol[middle].name);
  if (c == 0) return ol + middle;
    else if (c > 0) first = middle + 1;
      else last = middle;
  }
return NULL;
}



/*************************************************
*      Find a set flag in option list            *
*************************************************/

/* Because some versions of Unix make no restrictions on the values of uids and
gids (even negative ones), we cannot represent "unset" by a special value.
There is therefore a separate boolean variable for each one indicating whether
a value is set or not. This function returns a pointer to the boolean, given
the original option name. It is a major disaster if the flag cannot be found.

Arguments:
  name          the name of the uid or gid option
  oltop         points to the start of the relevant option list
  last          one more than the offset of the last item in the option list
  data_block    NULL when reading main options => data values in the option
                  list are absolute addresses; otherwise they are byte offsets
                  in data_block (used for driver options)

Returns:        a pointer to the boolean flag.
*/

static BOOL *
get_set_flag(char *name, optionlist *oltop, int last, void *data_block)
{
optionlist *ol;
char name2[64];
sprintf(name2, "*set_%s", name);
ol = find_option(name2, oltop, last);
if (ol == NULL) log_write(0, LOG_PANIC_DIE,
  "Exim internal error: missing set flag for %s", name);
return (data_block == NULL)? (BOOL *)(ol->value) :
  (BOOL *)((char *)data_block + (long int)(ol->value));
}





/*************************************************
*            Handle option line                  *
*************************************************/

/* This function is called from several places to process a line containing the
setting of an option. The first argument is the line to be decoded; it has been
checked not to be empty and not to start with '#'. Trailing newlines and white
space have been removed. The second argument is a pointer to the list of
variable names that are to be recognized, together with their types and
locations, and the third argument gives the number of entries in the list.

The fourth argument is a pointer to a data block. If it is NULL, then the data
values in the options list are absolute addresses. Otherwise, they are byte
offsets in the data block.

String option data may continue onto several lines; this function reads further
data from config_file if necessary.

The yield of this function is normally zero. If a string continues onto
multiple lines, then the data value is permitted to be followed by a comma
or a semicolon (for use in drivers) and the yield is that character.

Arguments:
  buffer        contains the configuration line to be handled
  oltop         points to the start of the relevant option list
  last          one more than the offset of the last item in the option list
  data_block    NULL when reading main options => data values in the option
                  list are absolute addresses; otherwise they are byte offsets
                  in data_block when they have opt_public set; otherwise
                  they are byte offsets in data_block->options_block.
Returns:        0 normally, or comma/semicolon after a continued string
*/

static int
readconf_handle_option(char *buffer, optionlist *oltop, int last,
  void *data_block)
{
int yield = 0;
int ptr = 0;
int offset = 0;
int n, count, type, value;
uid_t uid;
gid_t gid;
BOOL boolvalue = TRUE;
BOOL lc = FALSE;
BOOL freesptr = TRUE;
optionlist *ol, *ol2;
transport_instance *tp;
struct passwd *pw;
char *sptr;
char *s = buffer;
char name[64];
char name2[64];


/* There may be leading spaces */

while (isspace(*s)) s++;

/* Read the name of the option, and skip any subsequent white space. */

while (isalnum(*s) || *s == '_')
  {
  if (ptr < sizeof(name)-1) name[ptr++] = *s;
  s++;
  }
name[ptr] = 0;
while (isspace(*s)) s++;

/* Deal with "no_" or "not_" here for booleans */

if (strncmp(name, "no_", 3) == 0)
  {
  boolvalue = FALSE;
  offset = 3;
  }

if (strncmp(name, "not_", 4) == 0)
  {
  boolvalue = FALSE;
  offset = 4;
  }

/* Search the list for the given name. A non-existent name is a disaster. */

ol = find_option(name + offset, oltop, last);

if (ol == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG, "\"%s\" unknown in line %d", name,
  config_lineno);

type = ol->type & opt_mask;

/* Types with data values must be followed by '='; the "no[t]_" prefix
applies only to boolean values. */

if (type != opt_bool && type != opt_bool_verify)
  {
  if (offset != 0)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "negation prefix applied to a non-boolean option in line %d",
      config_lineno);
  if (*s != '=')
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "\"=\" expected after %s in line %d", name, config_lineno);
  }

/* If a boolean wasn't preceded by "no[t]_" it can be followed by = and
true/false/yes/no */

else if (*s != 0)
  {
  if (offset != 0)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "extra characters follow boolean value "
      "for variable %s in line %d", name, config_lineno);
  if (*s != '=')
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "\"=\" expected after %s in line %d", name, config_lineno);
  }

/* If there is a data block and the opt_public flag is not set, change
the data block pointer to the private options block. */

if (data_block != NULL && (ol->type & opt_public) == 0)
  data_block = (void *)(((driver_instance *)data_block)->options_block);

/* Skip white space after = */

while (isspace(*(++s)));

/* Now get the data according to the type. */

switch (type)
  {
  /* If a string value is not enclosed in quotes, it consists of
  the rest of the current line, verbatim. Otherwise, string escapes
  are processed, and the value can be continued onto subsequent lines
  by terminating it with a \, in which case white space at the start of
  the next line is skipped.

  A transport is specified as a string, which is then looked up in the
  list of transports. A search type is specified as one of a number of
  known strings.

  Uids and gids are specified as strings which are then looked up in the
  passwd file. Lists of uids and gids are similarly specified as colon-
  separated strings. */

  case opt_lcstringptr:
  lc = TRUE;

  case opt_stringptr:
  freesptr = FALSE;

  case opt_uid:
  case opt_gid:
  case opt_expand_uid:
  case opt_expand_gid:
  case opt_uidlist:
  case opt_gidlist:
  case opt_searchtype:
  case opt_transportptr:
  case opt_local_batch:

  n = (int)strlen(s) + 1;
  sptr = store_malloc(n);
  *sptr = 0;                   /* insurance */

  if (*s == '\"')
    {
    int count = 0;

    while (*(++s) != 0)
      {
      /* End of string. Trailing spaces should have been removed, but
      there may be trailing ; or , characters if the string has been
      split over several lines. */

      if (*s == '\"')
        {
        while (isspace(*(++s)));
        if (*s == ';' || *s == ',') yield = *s++;
        if (*s == 0) break;
        log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
          "extra characters follow string "
          "for variable %s in line %d", name, config_lineno);
        }

      /* Still in the string. Handle escape characters. */

      else if (*s == '\\')
        {
        if (s[1] == 0)
          {
          char *ss = get_config_line(FALSE, FALSE, FALSE);

          /* If we hit eof, nothing is done here, the main loop then
          retries and finds *(++s) == 0 and so exits. Otherwise, point
          s at the char before the start of the new line. Note that we
          have skipped comments. This is a feature, not a bug! */

          if (ss != NULL) s = ss - 1;
          }

        /* Handle '\' not at end of line */

        else
          {
          char ch = string_interpret_escape(&s);
          sptr = string_cat(sptr, &n, &count, &ch, 1);
          }
        }

      /* Neither " nor \ so just add to the string */

      else
        {
        if (lc) *s = tolower(*s);
        sptr = string_cat(sptr, &n, &count, s, 1);
        }
      }

    /* End of string scanning loop. Terminate the string; string_cat always
    leaves room */

    sptr[count] = 0;
    }

  /* If not quoted, just take the rest of the line. */

  else
    {
    if (lc)
      {
      int i = 0;
      while (*s != 0) sptr[i++] = tolower(*s++);
      sptr[i] = 0;
      }
    else strcpy(sptr, s);
    }

  /* Having read a string, we now have several different ways of using it,
  depending on the data type, so do another switch. */

  switch (type)
    {
    /* If this was a string, set the variable to point to the new string. */

    case opt_lcstringptr:
    case opt_stringptr:
    if (data_block == NULL)
      *((char **)(ol->value)) = sptr;
    else
      *((char **)((char *)data_block + (long int)(ol->value))) = sptr;
    break;

    /* If it was a transport - look it up in the transports list. */

    case opt_transportptr:
    for (tp = transports; tp != NULL; tp = tp->next)
      {
      if (strcmp(tp->name, sptr) == 0)
        {
        if (data_block == NULL)
          *((transport_instance **)(ol->value)) = tp;
        else
          *((transport_instance **)((char *)data_block +
            (long int)(ol->value))) = tp;
        break;
        }
      }
    if (tp == NULL)
      log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
        "%s transport, referred to in line %d, "
        "was not found", sptr, config_lineno);
    break;

    /* If it was an expanded uid, see if there is any expansion to be
    done by checking for the presence of a $ character. If there is, save it
    in the corresponding *expand_user option field. Otherwise, fall through
    to treat it as a fixed uid. Ensure mutual exclusivity of the two kinds
    of data. */

    case opt_expand_uid:
    sprintf(name2, "*expand_%s", name);
    ol2 = find_option(name2, oltop, last);
    if (ol2 != NULL)
      {
      char *ss = (strchr(sptr, '$') != NULL)? sptr : NULL;

      if (data_block == NULL)
        *((char **)(ol2->value)) = ss;
      else
        *((char **)((char *)data_block + (long int)(ol2->value))) = ss;

      if (ss != NULL)
        {
        *(get_set_flag(name, oltop, last, data_block)) = FALSE;
        freesptr = FALSE;
        break;
        }
      }

    /* Look up a fixed uid, and also make use of the corresponding gid
    if a passwd entry is returned and the gid has not been set. */

    case opt_uid:
    if (!direct_finduser(sptr, &pw, &uid))
      log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
        "user %s, referred to in line %d, "
        "was not found", sptr, config_lineno);
    if (data_block == NULL)
      *((uid_t *)(ol->value)) = uid;
    else
      *((uid_t *)((char *)data_block + (long int)(ol->value))) = uid;

    /* Set the flag indicating a fixed value is set */

    *(get_set_flag(name, oltop, last, data_block)) = TRUE;

    /* Handle matching gid if we have a passwd entry: done by finding the
    same name with terminating "user" changed to "group"; if not found,
    ignore. Also ignore if the value is already set. */

    if (pw == NULL) break;
    strcpy(name+(int)strlen(name)-4, "group");
    ol2 = find_option(name, oltop, last);
    if (ol2 != NULL && ((ol2->type & opt_mask) == opt_gid ||
        (ol2->type & opt_mask) == opt_expand_gid))
      {
      BOOL *set_flag = get_set_flag(name, oltop, last, data_block);
      if (! *set_flag)
        {
        if (data_block == NULL)
          *((gid_t *)(ol2->value)) = pw->pw_gid;
        else
          *((gid_t *)((char *)data_block + (long int)(ol2->value))) = pw->pw_gid;
        *set_flag = TRUE;
        }
      }
    break;

    /* If it was an expanded gid, see if there is any expansion to be
    done by checking for the presence of a $ character. If there is, save it
    in the corresponding *expand_user option field. Otherwise, fall through
    to treat it as a fixed gid. Ensure mutual exclusivity of the two kinds
    of data. */

    case opt_expand_gid:
    sprintf(name2, "*expand_%s", name);
    ol2 = find_option(name2, oltop, last);
    if (ol2 != NULL)
      {
      char *ss = (strchr(sptr, '$') != NULL)? sptr : NULL;

      if (data_block == NULL)
        *((char **)(ol2->value)) = ss;
      else
        *((char **)((char *)data_block + (long int)(ol2->value))) = ss;

      if (ss != NULL)
        {
        *(get_set_flag(name, oltop, last, data_block)) = FALSE;
        freesptr = FALSE;
        break;
        }
      }

    /* Handle freestanding gid */

    case opt_gid:
    if (!direct_findgroup(sptr, &gid))
      log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
        "group %s, referred to in line %d, "
        "was not found", sptr, config_lineno);
    if (data_block == NULL)
      *((gid_t *)(ol->value)) = gid;
    else
      *((gid_t *)((char *)data_block + (long int)(ol->value))) = gid;
    *(get_set_flag(name, oltop, last, data_block)) = TRUE;
    break;

    /* If it was a uid list, look up each individual entry, and build
    a vector of uids, with a count in the first element. */

    case opt_uidlist:
      {
      int count = 2;
      uid_t *list;
      int ptr = 0;
      char *p = sptr;

      while (*p != 0) if (*p++ == ':') count++;
      list = store_malloc(count*sizeof(uid_t));
      list[ptr++] = (uid_t)(count - 1);

      if (data_block == NULL)
        *((uid_t **)(ol->value)) = list;
      else
        *((uid_t **)((char *)data_block + (long int)(ol->value))) = list;

      p = sptr;
      while (count-- > 1)
        {
        char *q = strchr(p, ':');
        if (q != NULL) *q = 0;
        if (!direct_finduser(p, NULL, &uid))
          log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
            "user %s, referred to in line %d, "
            "was not found", p, config_lineno);
        list[ptr++] = uid;
        p = q+1;
        }
      }
    break;

    /* If it was a gid list, look up each individual entry, and build
    a vector of gids, with a count in the first element. */

    case opt_gidlist:
      {
      int count = 2;
      gid_t *list;
      int ptr = 0;
      char *p = sptr;

      while (*p != 0) if (*p++ == ':') count++;
      list = store_malloc(count*sizeof(gid_t));
      list[ptr++] = (gid_t)(count - 1);

      if (data_block == NULL)
        *((gid_t **)(ol->value)) = list;
      else
        *((gid_t **)((char *)data_block + (long int)(ol->value))) = list;

      p = sptr;
      while (count-- > 1)
        {
        char *q = strchr(p, ':');
        if (q != NULL) *q = 0;
        if (!direct_findgroup(p, &gid))
          log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
            "group %s, referred to in line %d, "
            "was not found", p, config_lineno);
        list[ptr++] = gid;
        p = q+1;
        }
      }
    break;

    /* Search types are known names, possibly preceded by "partial-",
    in which case we have to find the corresponding "*partial_match"
    option and set it. A number may precede the - sign. */

    case opt_searchtype:
      {
      char *sp = sptr;
      int type = 0;
      int pv = 0;

      if (strncmp(sp, "partial", 7) == 0)
        {
        void *value2;
        optionlist *ol2 = find_option("*partial_match", oltop, last);

        if (ol2 == NULL)
          log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
            "\"partial\" not permitted here (line %d)", config_lineno);

        value2 = ol2->value;
        if (data_block != NULL)
          value2 = (void *)((char *)data_block + (long int)value2);

        sp += 7;
        if (isdigit (*sp))
          while (isdigit(*sp)) pv = pv*10 + *sp++ - '0';
        else pv = 2;

        *((int *)value2) = pv;

        if (*sp++ != '-')
          log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
            "unrecognized search type \"%s\" in line %d", sptr,
            config_lineno);
        }

      if (strcmp(sp, "lsearch") == 0) type = stype_lsearch;
      else if (strcmp(sp, "dbm") == 0) type = stype_dbm;
      else if (strcmp(sp, "nis") == 0)
        {
        if (have_nis) type = stype_nis;
          else log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
            "search type \"nis\" unavailable (not in binary - see HAVE_NIS) "
            "in line %d", config_lineno);
        }
      else if (strcmp(sp, "nis0") == 0)
        {
        if (have_nis) type = stype_nis0;
          else log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
            "search type \"nis0\" unavailable (not in binary - see HAVE_NIS) "
            "in line %d", config_lineno);
        }
      else if (strcmp(sp, "nisplus") == 0)
        {
        if (have_nisplus) type = stype_nisplus;
          else log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
            "search type \"nisplus\" unavailable (not in binary - "
            "see HAVE_NISPLUS) in line %d", config_lineno);
        }
      else log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
        "search type %s unknown in line %d", sptr, config_lineno);
      if (data_block == NULL)
        *((int *)(ol->value)) = type;
      else
        *((int *)((char *)data_block + (long int)(ol->value))) = type;

      if (pv > 0 && type > stype_querystyle)
        log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
          "\"partial\" not supported for query styles (line %d)",
          config_lineno);
      }
    break;

    /* Local batch options are known names */

    case opt_local_batch:
      {
      int type = 0;
      if (strcmp(sptr, "one") == 0) type = local_batch_one;
      else if (strcmp(sptr, "domain") == 0) type = local_batch_domain;
      else if (strcmp(sptr, "all") == 0) type = local_batch_all;
      else if (strcmp(sptr, "none") == 0) type = local_batch_off;
      else log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
        "local batch type %s unknown in line %d", sptr, config_lineno);
      if (data_block == NULL)
        *((int *)(ol->value)) = type;
      else
        *((int *)((char *)data_block + (long int)(ol->value))) = type;
      }
    break;
    }

  /* Release store for temporary strings */

  if (freesptr) store_free(sptr);
  break;

  /* Boolean: if no characters follow, the value is boolvalue. Otherwise
  look for yes/not/true/false. There's a special fudge for verify settings;
  without a suffix they set both xx_sender and xx_recipient. The table
  points to the sender value; search subsequently for the recipient. */

  case opt_bool:
  case opt_bool_verify:
  if (*s != 0)
    {
    s = readconf_readname(name2, 64, s);
    if (strcmpic(name2, "true") == 0 || strcmpic(name2, "yes") == 0)
      boolvalue = TRUE;
    else if (strcmpic(name2, "false") == 0 || strcmpic(name2, "no") == 0)
      boolvalue = FALSE;
    else log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "\"%s\" is not a valid value for the \"%s\" option in line %d",
      name2, name, config_lineno);
    }
  if (data_block == NULL)
    *((BOOL *)(ol->value)) = boolvalue;
  else
    *((BOOL *)((char *)data_block + (long int)(ol->value))) = boolvalue;

  /* Verify fudge */

  if (type == opt_bool_verify)
    {
    sprintf(name2, "%s_recipient", name + offset);
    ol2 = find_option(name2, oltop, last);
    if (ol2 != NULL)
      {
      if (data_block == NULL)
        *((BOOL *)(ol2->value)) = boolvalue;
      else
        *((BOOL *)((char *)data_block + (long int)(ol2->value))) = boolvalue;
      }
    }
  break;

  /*  Integer: a simple(ish) case; allow octal and hex formats, and
  suffixes K and M. The different types affect output, not input. */

  case opt_mkint:
  case opt_octint:
  case opt_int:
  if (sscanf(s, "%i%n", &value, &count) != 1)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG, "integer expected "
      "for variable %s in line %d", name, config_lineno);

  if (tolower(s[count]) == 'k') { value *= 1024; count++; }
  if (tolower(s[count]) == 'm') { value *= 1024*1024; count++; }
  while (isspace(s[count])) count++;

  if (s[count] != 0)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "extra characters follow integer value "
      "for variable %s in line %d", name, config_lineno);

  if (data_block == NULL)
    *((int *)(ol->value)) = value;
  else
    *((int *)((char *)data_block + (long int)(ol->value))) = value;
  break;

  /*  Fixed-point number: held to 3 decimal places. */

  case opt_fixed:
  if (sscanf(s, "%d%n", &value, &count) != 1)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG, "fixed-point number expected "
      "for variable %s in line %d", name, config_lineno);

  value *= 1000;
  if (s[count] == '.')
    {
    int d = 100;
    while (isdigit(s[++count]))
      {
      value += (s[count] - '0') * d;
      d /= 10;
      }
    }

  while (isspace(s[count])) count++;

  if (s[count] != 0)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "extra characters follow fixed-point "
      "value for variable %s in line %d", name, config_lineno);

  if (data_block == NULL)
    *((int *)(ol->value)) = value;
  else
    *((int *)((char *)data_block + (long int)(ol->value))) = value;
  break;

  /* There's a special routine to read time values. */

  case opt_time:
  value = readconf_readtime(s, 0);
  if (value < 0)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG, "invalid time value "
      "for variable %s in line %d", name, config_lineno);
  if (data_block == NULL)
    *((int *)(ol->value)) = value;
  else
    *((int *)((char *)data_block + (long int)(ol->value))) = value;
  break;
  }

return yield;
}



/*************************************************
*               Print a time value               *
*************************************************/

/*
Argument:  a time value in seconds
Returns:   pointer to a fixed buffer containing the time as a string,
           in readconf_readtime() format
*/

char *
readconf_printtime(int t)
{
int s, m, h, d, w;
char *p = time_buffer;

s = t % 60;
t /= 60;
m = t % 60;
t /= 60;
h = t % 24;
t /= 24;
d = t % 7;
w = t/7;

if (w > 0) { sprintf(p, "%dw", w); while (*p) p++; }
if (d > 0) { sprintf(p, "%dd", d); while (*p) p++; }
if (h > 0) { sprintf(p, "%dh", h); while (*p) p++; }
if (m > 0) { sprintf(p, "%dm", m); while (*p) p++; }
if (s > 0 || p == time_buffer) sprintf(p, "%ds", s);

return time_buffer;
}



/*************************************************
*      Print an individual option value          *
*************************************************/

/* This is used by the -bP option, so prints to the standard output.
The entire options list is passed in as an argument, because some options come
in pairs - typically uid/gid settings, which can either be explicit numerical
values, or strings to be expanded later. If the numerical value is unset,
search for "*expand_<name>" to see if there is a string equivalent.

Arguments:
  ol             option entry, or NULL for an unknown option
  name           option name
  options_block  NULL for main configuration options; otherwise points to
                   a driver block; if the option doesn't have opt_public
                   set, then options_block->options_block is where the item
                   resides.
  oltop          points to the option list in which ol exists
  last           one more than the offset of the last entry in optop

Returns:         nothing
*/

static void
print_ol(optionlist *ol, char *name, void *options_block,
  optionlist *oltop, int last)
{
struct passwd *pw;
struct group *gr;
transport_instance *t;
optionlist *ol2;
void *value;
uid_t *uidlist;
gid_t *gidlist;
char *s;
char name2[64];

if (ol == NULL)
  {
  printf("%s is not a known option\n", name);
  return;
  }

value = ol->value;
if (options_block != NULL)
  {
  if ((ol->type & opt_public) == 0)
    options_block = (void *)(((driver_instance *)options_block)->options_block);
  value = (void *)((char *)options_block + (long int)value);
  }

switch(ol->type & opt_mask)
  {
  case opt_lcstringptr:
  case opt_stringptr:
  s = *((char **)value);
  printf("%s = %s\n", name, (s == NULL)? "" : string_printing(s, FALSE));
  break;

  case opt_transportptr:
  t = *((transport_instance **)value);
  printf("%s = %s\n", name, (t == NULL)? "" : t->name);
  break;

  case opt_int:
  printf("%s = %d\n", name, *((int *)value));
  break;

  case opt_mkint:
    {
    int x = *((int *)value);
    if (x != 0 && (x & 1023) == 0)
      {
      int c = 'K';
      x >>= 10;
      if ((x & 1023) == 0)
        {
        c = 'M';
        x >>= 10;
        }
      printf("%s = %d%c\n", name, x, c);
      }
    else printf("%s = %d\n", name, x);
    }
  break;

  case opt_octint:
  printf("%s = %#o\n", name, *((int *)value));
  break;

  /* Can be negative only when "unset", in which case integer */

  case opt_fixed:
    {
    int x = *((int *)value);
    int f = x % 1000;
    int d = 100;
    if (x < 0) printf("%s =\n", name); else
      {
      printf("%s = %d.", name, x/1000);
      do
        {
        printf("%d", f/d);
        f %= d;
        d /= 10;
        }
      while (f != 0);
      printf("\n");
      }
    }
  break;

  /* If the numerical value is unset, try for the string value */

  case opt_expand_uid:
  if (! *get_set_flag(name, oltop, last, options_block))
    {
    sprintf(name2, "*expand_%s", name);
    ol2 = find_option(name2, oltop, last);
    if (ol2 != NULL)
      {
      void *value2 = ol2->value;
      if (options_block != NULL)
        value2 = (void *)((char *)options_block + (long int)value2);
      s = *((char **)value2);
      printf("%s = %s\n", name, (s == NULL)? "" : string_printing(s, FALSE));
      break;
      }
    }

  /* Else fall through */

  case opt_uid:
  if (! *get_set_flag(name, oltop, last, options_block))
    printf("%s =\n", name);
  else
    {
    pw = getpwuid(*((int *)value));
    if (pw == NULL)
      printf("%s = %d\n", name, (int)(*((uid_t *)value)));
    else printf("%s = %s\n", name, pw->pw_name);
    }
  break;

  /* If the numerical value is unset, try for the string value */

  case opt_expand_gid:
  if (! *get_set_flag(name, oltop, last, options_block))
    {
    optionlist *ol2;
    sprintf(name2, "*expand_%s", name);
    ol2 = find_option(name2, oltop, last);
    if (ol2 != NULL && (ol2->type & opt_mask) == opt_stringptr)
      {
      void *value2 = ol2->value;
      if (options_block != NULL)
        value2 = (void *)((char *)options_block + (long int)value2);
      s = *((char **)value2);
      printf("%s = %s\n", name, (s == NULL)? "" : string_printing(s, FALSE));
      break;
      }
    }

  /* Else fall through */

  case opt_gid:
  if (! *get_set_flag(name, oltop, last, options_block))
    printf("%s =\n", name);
  else
    {
    gr = getgrgid(*((int *)value));
    if (gr == NULL)
       printf("%s = %d\n", name, *((int *)value));
    else printf("%s = %s\n", name, gr->gr_name);
    }
  break;

  case opt_uidlist:
  uidlist = *((uid_t **)value);
  printf("%s =", name);
  if (uidlist != NULL)
    {
    int i;
    char sep = ' ';
    for (i = 1; i <= (int)(uidlist[0]); i++)
      {
      char *name = NULL;
      pw = getpwuid(uidlist[i]);
      if (pw != NULL) name = pw->pw_name;
      if (name != NULL) printf("%c%s", sep, name);
        else printf("%c%d", sep, (int)(uidlist[i]));
      sep = ':';
      }
    }
  printf("\n");
  break;

  case opt_gidlist:
  gidlist = *((gid_t **)value);
  printf("%s =", name);
  if (gidlist != NULL)
    {
    int i;
    char sep = ' ';
    for (i = 1; i <= (int)(gidlist[0]); i++)
      {
      char *name = NULL;
      gr = getgrgid(gidlist[i]);
      if (gr != NULL) name = gr->gr_name;
      if (name != NULL) printf("%c%s", sep, name);
        else printf("%c%d", sep, (int)(gidlist[i]));
      sep = ':';
      }
    }
  printf("\n");
  break;

  case opt_time:
  printf("%s = %s\n", name, readconf_printtime(*((int *)value)));
  break;

  case opt_bool:
  case opt_bool_verify:
  printf("%s%s\n", (*((BOOL *)value))? "" : "no_", name);
  break;

  /* If *partial_match exists and the value is > 0, add "partial-"
  to the output. */

  case opt_searchtype:
    {
    int type = *((int *)value);
    char partial[32];
    optionlist *ol2 = find_option("*partial_match", oltop, last);

    partial[0] = 0;
    if (ol2 != NULL)
      {
      int pv;
      void *value2 = ol2->value;
      if (options_block != NULL)
        {
        value2 = (void *)((char *)options_block + (long int)value2);
        }
      pv = *((int *)value2);
      if (pv == 2) strcpy(partial, "partial-");
        else if (pv > 0) sprintf(partial, "partial%d-", pv);
      }

    printf("%s = %s%s\n", name, partial,
      (type == stype_lsearch)? "lsearch" :
      (type == stype_dbm)? "dbm" :
      (type == stype_nis)? "nis" :
      (type == stype_nis0)? "nis0" :
      (type == stype_nisplus)? "nisplus" :
      (type == -1)? "" : "?");
    }
  break;

  case opt_local_batch:
    {
    int type = *((int *)value);
    printf("%s = %s\n", name, (type == local_batch_one)? "one" :
      (type == local_batch_domain)? "domain" :
      (type == local_batch_all)? "all" : "none");
    }
  break;
  }
}



/*************************************************
*        Print value from main configuration     *
*************************************************/

/* This function, called as a result of encountering the -bP option,
causes the value of any main configuration variable to be output if the
second argument is NULL. There are some special values:

  all               print all main configuration options
  configure_file    print the name of the configuration file
  directors         print the directors' configurations
  routers           print the routers' configurations
  transports        print the transports' configuration
  director_list     print a list of director names
  router_list       print a list of router names
  transport_list    print a list of transport names

If the second argument is not NULL, it must be one of "director", "router",
or "transport", in which case the first argument identifies the driver whose
options are to be printed.

Arguments:
  name        option name if type == NULL; else driver name
  type        NULL or driver type name, as described above

Returns:      nothing
*/

void
readconf_print(char *name, char *type)
{
BOOL names_only = FALSE;
optionlist *ol;
optionlist *ol2 = NULL;
driver_instance *d = NULL;
int size = 0;

if (type == NULL)
  {
  if (strcmp(name, "configure_file") == 0)
    {
    printf("%s\n", config_filename);
    return;
    }

  if (strcmp(name, "log_file_path") == 0)
    {
    printf("%s\n", log_file_path);
    return;
    }

  if (strcmp(name, "pid_file_path") == 0)
    {
    printf("%s\n", pid_file_path);
    return;
    }

  if (strcmp(name, "all") == 0)
    {
    for (ol = optionlist_config;
         ol < optionlist_config + optionlist_config_size; ol++)
      {
      if ((ol->type & opt_hidden) == 0)
        print_ol(ol, ol->name, NULL, optionlist_config, optionlist_config_size);
      }
    return;
    }

  if (strcmp(name, "directors") == 0)
    {
    type = "director";
    name = NULL;
    }
  else if (strcmp(name, "routers") == 0)
    {
    type = "router";
    name = NULL;
    }
  else if (strcmp(name, "transports") == 0)
    {
    type = "transport";
    name = NULL;
    }
  else if (strcmp(name, "director_list") == 0)
    {
    type = "director";
    name = NULL;
    names_only = TRUE;
    }
  else if (strcmp(name, "router_list") == 0)
    {
    type = "router";
    name = NULL;
    names_only = TRUE;
    }
  else if (strcmp(name, "transport_list") == 0)
    {
    type = "transport";
    name = NULL;
    names_only = TRUE;
    }
  else
    {
    print_ol(find_option(name, optionlist_config, optionlist_config_size),
      name, NULL, optionlist_config, optionlist_config_size);
    return;
    }
  }

/* Handle the options for a director, router, or transport. Skip options that
are flagged as hidden. Some of these are options with names starting with '*',
used for internal alternative representations of other options (which the
printing function will sort out). Others are synonyms kept for backward
compatibility. */

if (strcmp(type, "director") == 0)
  {
  d = (driver_instance *)directors;
  ol2 = optionlist_directors;
  size = optionlist_directors_size;
  }
else if (strcmp(type, "router") == 0)
  {
  d = (driver_instance *)routers;
  ol2 = optionlist_routers;
  size = optionlist_routers_size;
  }
else if (strcmp(type, "transport") == 0)
  {
  d = (driver_instance *)transports;
  ol2 = optionlist_transports;
  size = optionlist_transports_size;
  }

if (names_only)
  {
  for (; d != NULL; d = d->next) printf("%s\n", d->name);
  return;
  }

/* Either search for a given driver, or print all of them */

for (; d != NULL; d = d->next)
  {
  if (name == NULL)
    printf("\n%s %s:\n", d->name, type);
  else if (strcmp(d->name, name) != 0) continue;

  for (ol = ol2; ol < ol2 + size; ol++)
    {
    if ((ol->type & opt_hidden) == 0)
      print_ol(ol, ol->name, d, ol2, size);
    }

  for (ol = d->info->options;
       ol < d->info->options + *(d->info->options_count); ol++)
    {
    if ((ol->type & opt_hidden) == 0)
      print_ol(ol, ol->name, d, d->info->options, *(d->info->options_count));
    }
  if (name != NULL) return;
  }
if (name != NULL) printf("%s %s not found\n", type, name);
}



/*************************************************
*             Syntax check a list of nets        *
*************************************************/

/* There is a regular expression that checks an entry in a net list.

Arguments:
  netregexp    regular expression for syntax of a net item
  list         colon-separated list of net items
  name         option name, for error message

Returns:       nothing
*/

static void
check_net_list(char *list, char *name)
{
char *s;
if (list == NULL) return;

for (s = string_firstinlist(list, ':'); s != NULL;
     s = string_nextinlist(':'))
  {
  if (!regexec(net_regexp, s))
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "error in IP net specification %s in %s option", s, name);
  }
}



/*************************************************
*         Read main configuration options        *
*************************************************/

/* This function is the first to be called for configuration reading.
It opens the configuration file and reads general configuration settings
until it reaches a line containing "end". The file is then left open so
that delivery configuration data can subsequently be read if needed.

The configuration file must be owned either by root or exim, and be writeable
only by root or uid/gid exim. The values for Exim's uid and gid can be changed
in the config file, so the test is done on the compiled in values. A slight
anomaly, to be carefully documented.

The name of the configuration file is included in the binary of Exim. It can
be altered from the command line, but if that is done, root privilege is
immediately withdrawn.

For use on multiple systems that share file systems, first look for a
configuration file whose name has the current node name on the end. If that
is not found, try the generic name. For really contorted configurations, that
run multiple Exims with different uid settings, first try adding the effective
uid before the node name. These complications are going to waste resources
on most systems. Therefore they are available only when requested by
compile-time options. */

void
readconf_main(void)
{
struct stat statbuf;
char *s;

/* Try for the node-specific file if a node name exists */

#ifdef CONFIGURE_FILE_USE_NODE
struct utsname uts;
if (uname(&uts) >= 0)
  {
  #ifdef CONFIGURE_FILE_USE_EUID
  sprintf(big_buffer, "%s.%d.%s", config_filename, (int)geteuid(), uts.nodename);
  config_file = fopen(big_buffer, "r");
  if (config_file == NULL)
  #endif
    {
    sprintf(big_buffer, "%s.%s", config_filename, uts.nodename);
    config_file = fopen(big_buffer, "r");
    }
  }
#endif

/* Otherwise, try the generic name, possibly with the euid added */

#ifdef CONFIGURE_FILE_USE_EUID
if (config_file == NULL)
  {
  sprintf(big_buffer, "%s.%d", config_filename, (int)geteuid());
  config_file = fopen(big_buffer, "r");
  }
#endif

/* Finally, try the unadorned name */

strcpy(big_buffer, config_filename);
if (config_file == NULL) config_file = fopen(big_buffer, "r");

/* Failure to open the configuration file is a serious disaster. */

if (config_file == NULL)
  log_write(0, LOG_PANIC_DIE, "Failed to open configuration file %s",
    big_buffer);

/* Check the status of the file we have opened, unless it was specified on
the command line, in which case privilege was given away at the start. */

if (!config_changed)
  {
  if (fstat(fileno(config_file), &statbuf) != 0)
    log_write(0, LOG_PANIC_DIE, "Failed to stat configuration file %s",
      big_buffer);

  if ((statbuf.st_uid != root_uid &&                     /* owner not root & */
      (!exim_uid_set || statbuf.st_uid != exim_uid)) ||  /* owner not exim   */
                                                         /* or */
      ((!exim_gid_set || statbuf.st_gid != exim_gid) &&  /* group not exim & */
       (statbuf.st_mode & 020) != 0) ||                  /* group writeable  */
                                                         /* or */
      ((statbuf.st_mode & 2) != 0))                      /* world writeable  */

    log_write(0, LOG_PANIC_DIE, "Exim configuration file %s has the wrong "
      "owner, group, or mode", big_buffer);
  }

/* Process the main configuration settings. They all begin with a lower case
letter. If we see something starting with an upper case letter, it is taken as
a macro definition. */

while ((s = get_config_line(TRUE, FALSE, TRUE)) != NULL &&
        strcmpic(s, "end") != 0)
  {
  if (isupper(s[0]))
    {
    macro_item *m;
    macro_item *mlast = NULL;
    char name[24];
    int ptr = 0;

    while (isalnum(*s) || *s == '_')
      {
      if (ptr < sizeof(name)-1) name[ptr++] = *s;
      s++;
      }
    name[ptr] = 0;
    while (isspace(*s)) s++;
    if (*s++ != '=')
      log_write(0, LOG_CONFIG|LOG_PANIC_DIE, "malformed macro definition in "
        "line %d", config_lineno);
    while (isspace(*s)) s++;

    for (m = macros; m != 0; m = m->next)
      {
      if (strcmp(m->name, name) == 0)
        log_write(0, LOG_CONFIG|LOG_PANIC_DIE, "macro \"%s\" is already "
          "defined", name);
      mlast = m;
      }

    m = store_malloc(sizeof(macro_item) + (int)strlen(name));

    m->next = NULL;
    if (mlast == NULL) macros = m; else mlast->next = m;
    strcpy(m->name, name);
    m->replacement = string_copy(s);
    }

  else readconf_handle_option(s, optionlist_config, optionlist_config_size,
    NULL);
  }

/* If any of the options that specify lists of IP networks are given,
syntax-check their values. */

check_net_list(helo_verify_nets, "helo_verify_nets");
check_net_list(sender_net_accept, "sender_net_accept");
check_net_list(sender_net_accept_relay, "sender_net_accept_relay");
check_net_list(sender_net_reject, "sender_net_reject");
check_net_list(sender_net_reject_recipients, "sender_net_reject_recipients");
check_net_list(sender_net_reject_relay, "sender_net_reject_relay");
check_net_list(sender_verify_except_nets, "sender_verify_except_nets");
check_net_list(smtp_reserve_nets, "smtp_reserve_nets");

/* Set check_relay if any relevant fields are set, to save having to
check lots of fields each time. */

check_relay = sender_host_accept_relay != NULL ||
              sender_host_reject_relay != NULL ||
              sender_net_accept_relay != NULL ||
              sender_net_reject_relay != NULL ||
              sender_address_relay != NULL;

/* If certain items were not given in the configuration file,
we must set them by other means. */

/* The primary host name is obtained from uname(), but if that yields
an unqualified value, make a FQDN by using gethostbyname to canonize it. */

if (primary_hostname == NULL)
  {
  struct utsname uts;
  struct hostent *host;
  if (uname(&uts) < 0)
    log_write(0, LOG_PANIC_DIE, "Failed to compute host name");
  if (strchr(uts.nodename, '.') == NULL &&
      (host = gethostbyname(uts.nodename)) != NULL)
    primary_hostname = string_copy((char *)host->h_name);
  else primary_hostname = string_copy(uts.nodename);
  }

/* The qualify domains default to the primary host name */

if (qualify_domain_sender == NULL)
  qualify_domain_sender = primary_hostname;
if (qualify_domain_recipient == NULL)
  qualify_domain_recipient = qualify_domain_sender;

/* Default local domains to the qualify domain. */

if (local_domains == NULL)
  local_domains = string_copy(qualify_domain_recipient);

/* Add the primary host name if requested, but avoid having it twice. */

if (local_domains_include_host &&
    !match_isinlist(primary_hostname, local_domains, &re_local_domains, TRUE))
  local_domains = string_sprintf("%s:%s", local_domains, primary_hostname);

/* Include host literals if requested */

if (local_domains_include_host_literals)
  {
  ip_address_item *ip;
  int ptr = (int)strlen(local_domains);
  int size = ptr + 1;
  for (ip = host_find_interfaces(); ip != NULL; ip = ip->next)
    {
    if (strcmp(ip->address, "127.0.0.1") == 0) continue;
    sprintf(big_buffer, ":[%s]", ip->address);
    local_domains = string_cat(local_domains, &size, &ptr, big_buffer,
      (int)strlen(big_buffer));
    }
  local_domains[ptr] = 0;
  }

/* Setting exim_user in the configuration sets the gid as well if a name is
given, but a numerical value does not. Also, we may have a compiled-in uid and
no gid. */

if (exim_uid_set && !exim_gid_set)
  {
  struct passwd *pw = getpwuid(exim_uid);
  if (pw == NULL)
    log_write(0, LOG_PANIC_DIE, "Failed to look up uid %d", (int)exim_uid);
  exim_gid = pw->pw_gid;
  exim_gid_set = TRUE;
  }

/* Similarly for message_filter_user */

if (message_filter_uid_set && !message_filter_gid_set)
  {
  struct passwd *pw = getpwuid(message_filter_uid);
  if (pw == NULL)
    log_write(0, LOG_PANIC_DIE, "Failed to look up uid %d",
      (int)message_filter_uid);
  message_filter_gid = pw->pw_gid;
  message_filter_gid_set = TRUE;
  }

/* If exim_uid is set at runtime to be root, it has the effect of unsetting
it. */

if (exim_uid == root_uid) exim_uid_set = exim_gid_set = FALSE;

/* If no security type was specified in the configuration file, use level
2 or 3 if an exim uid has been defined. */

if (security_type == NULL && exim_uid_set)
  security_type = have_seteuid? "setuid+seteuid" : "setuid";

/* If a security type is set and there is no exim_uid, complain. */

if (security_type != NULL && !exim_uid_set)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "%s security requested in configuration file but no exim user defined");

/* Check that the security type is valid, and set the int version of it. If
seteuid is requested, check that this operating system has it (or at least,
exim has been configured to say that it has!) */

if (exim_uid_set)
  {
  if (strcmp(security_type, "setuid") == 0) security_level = 2;
  else if (strcmp(security_type, "seteuid") == 0) security_level = 1;
  else if (strcmp(security_type, "setuid+seteuid") == 0) security_level = 3;
  else log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "Unknown security setting: %s", security_type);

  if (!have_seteuid && (security_level == 1 || security_level == 3))
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "seteuid requested in configuration file but not configured in Make.os");
  }

/* If the errors_reply_to field is set, check that it is syntactically valid
and ensure it contains a domain. */

if (errors_reply_to != NULL)
  {
  char *errmess;
  int start, end, domain;
  char *recipient = parse_extract_address(errors_reply_to, &errmess,
    &start, &end, &domain, FALSE);

  if (recipient == NULL)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "error in errors_reply_to (%s): %s", errors_reply_to, errmess);

  if (domain == 0)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "errors_reply_to (%s) does not contain a domain", errors_reply_to);

  store_free(recipient);
  }

/* Kill_ip_options => log_ip_options */

if (kill_ip_options) log_ip_options = TRUE;

/* The debug_transport option is supported only when Exim is compiled
with the debut transport available. */

#ifndef TRANSPORT_DEBUG
if (debug_transport_file != NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "debug_transport is available only when TRANSPORT_DEBUG is defined");
#endif
}



/*************************************************
*          Initialize one driver                 *
*************************************************/

/* This is called once the driver's generic options, if any, have been read.
We can now find the driver, and set up defaults for the private options.

Arguments:
  d                   pointer to driver instance block, with generic
                        options filled in
  drivers_available   vector of available drivers
  size_of_info        size of each block in drivers_available
  class               class of driver, for error message

Returns:              pointer to the driver info block
*/

static driver_info *
init_driver(driver_instance *d, driver_info *drivers_available,
  int size_of_info, char *class)
{
driver_info *dd;

if (d->driver_name == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "no driver name given for %s %s", class, d->name);

for (dd = drivers_available; dd->driver_name[0] != 0;
     dd = (driver_info *)(((char *)dd) + size_of_info))
  {
  if (strcmp(d->driver_name, dd->driver_name) == 0)
    {
    int len = dd->options_len;
    d->info = dd;
    d->options_block = store_malloc(len);
    memcpy(d->options_block, dd->options_block, len);
    return dd;
    }
  }

log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
  "%s %s: cannot find driver \"%s\" in line %d", class, d->name,
    d->driver_name, config_lineno);

return NULL;   /* never obeyed */
}




/*************************************************
*             Initialize driver list             *
*************************************************/

/* This function is called for directors, routers, and transports. It reads the
data from the current point in the configuration file up to a line containing
"end", and sets up a chain of instance blocks according to the file's contents.
The file will already have been opened by a call to readconf_main, and must be
left open for subsequent reading of further data.

Any errors cause a panic crash. Note that the blocks with names driver_info and
driver_instance must map the first portions of all the _info and _instance
blocks for this shared code to work. */

void
readconf_driver_init(
  char *class,                   /* "director", "router", or "transport" */
  driver_instance **anchor,      /* &directors, &routers, or &transports */
  driver_info *drivers_available,/* available drivers */
  int size_of_info,              /* size of each info block */
  void *instance_default,        /* points to default data */
  int  instance_size,            /* size of instance block */
  optionlist *driver_optionlist, /* generic option list */
  int  driver_optionlist_count)  /* count of same */
{
BOOL generic = FALSE;
driver_info *this_info = FALSE;
driver_instance **p = anchor;
driver_instance *d = NULL;
char *s, *buffer;

while ((buffer = get_config_line(TRUE, FALSE, FALSE)) != NULL &&
        strcmpic(buffer, "end") != 0)
  {
  BOOL was_semicolon = FALSE;
  int n = (int)strlen(buffer);
  char name[24];

  /* If the terminating character is a comma, just remove it; if it is
  a semicolon, remove it and set a flag. */

  if (buffer[n-1] == ',') buffer[n-1] = 0;
    else if (buffer[n-1] == ';')
      { buffer[n-1] = 0; was_semicolon = TRUE; }

  /* If the line starts with a name terminated by a colon, we are at the
  start of the definition of a new driver. The first option may be on the
  same line, or it may follow on the next line. */

  s = readconf_readname(name, 24, buffer);
  if (*s++ == ':')
    {
    /*If there was a previous driver and we have not yet looked up its
    details (which will be the case if no private options were given) then
    do so now. */

    if (d != NULL && d->info == NULL)
      this_info = init_driver(d, drivers_available, size_of_info, class);

    /* Finish off initializing the previous driver. */

    if (d != NULL) (d->info->init)(d);

    /* Set up a new driver instance data block on the chain, with
    its default values installed. */

    d = store_malloc(instance_size);
    memcpy(d, instance_default, instance_size);
    *p = d;
    p = &(d->next);
    d->name = string_copy(name);
    generic = TRUE;

    /* If nothing more on this line, do the next loop iteration. */

    while (isspace(*s)) s++;
    if (*s == 0) continue;
    }

  /* If not the start of a new driver, scan from start of line. */

  else s = buffer;

  /* Give an error if we have not set up a current driver yet. */

  if (d == NULL)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "%s name missing in line %d", class, config_lineno);

  /* Handle generic options, and finish with them if terminator was
  a semicolon. */

  if (generic)
    {
    if (readconf_handle_option(s, driver_optionlist,
        driver_optionlist_count, d) == ';') was_semicolon = TRUE;
    if (was_semicolon)
      {
      this_info = init_driver(d, drivers_available, size_of_info, class);
      generic = FALSE;
      }
    }

  /* Handle private options - pass the generic block because some may
  live therein. A flag with each option indicates if it is in the public
  block. */

  else readconf_handle_option(s, this_info->options,
    *(this_info->options_count), d);
  }

/* Initialize the final driver if we haven't already done so, and then
finish off its initialization. */

if (d != NULL && d->info == NULL)
  this_info = init_driver(d, drivers_available, size_of_info, class);
if (d != NULL) (d->info->init)(d);
}



/*************************************************
*            Check driver dependency             *
*************************************************/

/* This function is passed a driver instance and a string. It checks whether
any of the string options for the driver contains the given string as an
expansion variable.

Arguments:
  d        points to a driver instance block
  s        the string to search for

Returns:   TRUE if a dependency is found
*/

BOOL
readconf_depends(driver_instance *d, char *s)
{
int count = *(d->info->options_count);
optionlist *ol;
char *ss;

for (ol = d->info->options; ol < d->info->options + count; ol++)
  {
  void *options_block;
  char *value;
  int type = ol->type & opt_mask;
  if (type != opt_stringptr && type != opt_lcstringptr) continue;
  options_block = ((ol->type & opt_public) == 0)? d->options_block : (void *)d;
  value = *(char **)((char *)options_block + (long int)(ol->value));
  if (value != NULL && (ss = strstr(value, s)) != NULL)
    {
    if (ss <= value || (ss[-1] != '$' && ss[-1] != '{') ||
      isalnum(ss[(int)strlen(s)])) continue;
    DEBUG(9) debug_printf("driver %s option %s depends on %s\n",
      d->name, ol->name, s);
    return TRUE;
    }
  }

DEBUG(9) debug_printf("driver %s does not depend on %s\n", d->name, s);
return FALSE;
}




/*************************************************
*      Decode an error type for retries          *
*************************************************/

/* This function is global because it is also called from the main
program when testing retry information. It decodes strings such as "quota_7d"
into numerical error codes.

Arguments:
  pp          points to start of text
  p           points past end of text
  errno       points to an int to receive the main error number
  more_errno  points to an int to receive the secondary error data

Returns:      NULL if decoded correctly; else points to error text
*/

char *
readconf_retry_error(char *pp, char *p, int *errno, int *more_errno)
{
int len;
char *q = pp;
while (q < p && *q != '_') q++;
len = q - pp;

if (len == 5 && strncmpic(pp, "quota", len) == 0)
  {
  *errno = ERRNO_EXIMQUOTA;
  if (q != p & (*more_errno = readconf_readtime(q+1, *p)) < 0)
      return "bad time value";
  }

else if (len == 7 && strncmpic(pp, "refused", len) == 0)
  {
  *errno = ECONNREFUSED;
  if (q != p)
    {
    if (strncmpic(q+1, "MX", p-q-1) == 0) *more_errno = 'M';
    else if (strncmpic(q+1, "A", p-q-1) == 0) *more_errno = 'A';
    else return "A or MX expected after \"refused\"";
    }
  }

else if (len == 7 && strncmpic(pp, "timeout", len) == 0)
  {
  *errno = ETIMEDOUT;
  if (q != p)
    {
    if (strncmpic(q+1, "DNS", p-q-1) == 0) *more_errno = 'D';
    else if (strncmpic(q+1, "connect", p-q-1) == 0) *more_errno = 'C';
    else return "DNS or CONNECT expected after \"timeout\"";
    }
  }

else if (len != 1 || strncmp(pp, "*", 1) != 0) return "unknown error name";
return NULL;
}




/*************************************************
*                Read retry information          *
*************************************************/

/* Each line of retry information contains:

.  A domain name, possibly a regexp, or a name starting with *; for local
   retry data, local_part@domain is used.

.  An error name, possibly with additional data, or *;

.  An optional sequence of retry items, each consisting of an identifying
   letter, a cutoff time, and optional parameters.

All this is decoded and placed into a control block. */


/* Subroutine to read an argument, preceded by a comma and terminated
by comma, semicolon, whitespace, or newline. The types are: 0 = time value,
1 = fixed point number (returned *1000).

Arguments:
  paddr     pointer to pointer to current character; updated
  type      0 => read a time; 1 => read a fixed point number

Returns:    time in seconds or fixed point number * 1000
*/

static int
retry_arg(char **paddr, int type)
{
char *p = *paddr;
char *pp;

if (*p++ != ',')
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "comma expected in line %d", config_lineno);

while (isspace(*p)) p++;
pp = p;
while (isalnum(*p)|| (type == 1 && *p == '.')) p++;

if (*p != 0 && !isspace(*p) && *p != ',' && *p != ';')
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "comma or semicolon expected in line %d", config_lineno);

*paddr = p;
switch (type)
  {
  case 0:
  return readconf_readtime(pp, *p);
  break;

  case 1:
  return readconf_readfixed(pp, *p);
  break;
  }
return 0;    /* Keep picky compilers happy */
}

/* The function proper */

void
readconf_retries(void)
{
retry_config **chain = &retries;
retry_config *next;
char *p;

while ((p = get_config_line(TRUE, FALSE, FALSE)) != NULL &&
        strcmpic(p, "end") != 0)
  {
  retry_rule **rchain;
  char *pp, *error;

  next = store_malloc(sizeof(retry_config));
  next->next = NULL;
  *chain = next;
  chain = &(next->next);
  next->basic_errno = next->more_errno = 0;
  next->rules = NULL;
  rchain = &(next->rules);

  pp = p;
  while (isgraph(*p)) p++;
  if (p - pp <= 0)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "missing domain name in line %d", config_lineno);
  next->destination = string_copyn(pp, p-pp);
  next->re = NULL;

  if (next->destination[0] != '^')
    {
    char *at;
    if ((at = strchr(next->destination, '@')) != NULL)
      {
      if (!match_isinlist(at+1, local_domains, &re_local_domains, TRUE))
        log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
          "key %s in retry rules contains a local part with a non-local "
          "domain", next->destination);
      }
    }

  while (isspace(*p)) p++;
  pp = p;
  while (isgraph(*p)) p++;
  if (p - pp <= 0)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "missing error type in line %d", config_lineno);

  /* Test error names for things we understand. */

  if ((error = readconf_retry_error(pp, p, &(next->basic_errno),
       &(next->more_errno))) != NULL)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG, "%s in line %d", error,
      config_lineno);

  /* Now the retry rules. Keep the maximum timeout encountered. */

  while (isspace(*p)) p++;
  while (*p != 0)
    {
    retry_rule *rule = store_malloc(sizeof(retry_rule));
    *rchain = rule;
    rchain = &(rule->next);
    rule->next = NULL;

    rule->rule = toupper(*p++);
    rule->timeout = retry_arg(&p, 0);
    if (rule->timeout > retry_maximum_timeout)
      retry_maximum_timeout = rule->timeout;

    switch (rule->rule)
      {
      case 'F':   /* Fixed interval */
      rule->p1 = retry_arg(&p, 0);
      break;

      case 'G':   /* Geometrically increasing intervals */
      rule->p1 = retry_arg(&p, 0);
      rule->p2 = retry_arg(&p, 1);
      if (rule->p1 == 0 || rule->p2 < 1000)
        log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
          "bad parameters for geometric retry rule in line %d",
          config_lineno);
      break;

      default:
      log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
        "unknown retry rule letter in line %d", config_lineno);
      break;
      }

    while (isspace(*p)) p++;
    if (*p == ';')
      {
      p++;
      while (isspace(*p)) p++;
      }
    else if (*p != 0)
      log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "semicolon expected in line %d", config_lineno);
    }
  }
}



/*************************************************
*              Read rewrite information          *
*************************************************/

/* Each line of rewrite information contains:

.  A complete address in the form user@domain, possibly with
   leading * for each part; or alternatively, a regexp.

.  A replacement string (which will be expanded).

.  An optional sequence of one-letter flags, indicating which
   headers etc. to apply this rule to.

All this is decoded and placed into a control block. The global variable
rewrite_existflags is the OR of all the flag words. */

void
readconf_rewrites(void)
{
rewrite_rule **chain = &rewrite_rules;
rewrite_rule *next;
char *p, *pp;

while ((p = get_config_line(TRUE, TRUE, FALSE)) != NULL &&
        strcmpic(p, "end") != 0)
  {
  next = store_malloc(sizeof(rewrite_rule));
  next->next = NULL;
  *chain = next;
  chain = &(next->next);

  pp = p;
  while (*p != 0 && !isspace(*p)) p++;

  next->key = string_copyn(pp, p-pp);
  next->re = NULL;

  /* A non-regular expression must contain a local-part and a domain,
  unless is is a single file lookup. */

  if (pp[0] != '^' && strchr(next->key, ';') == NULL)
    {
    pp = strchr(next->key, '@');
    if (pp == NULL)
      log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
        "domain missing in rewrite key string \"%s\" in line %d",
          next->key, config_lineno);
    }

  while (isspace(*p)) p++;
  if (*p == 0)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "missing rewrite replacement string in line %d", config_lineno);

  next->flags = 0;

  /* The replacement is permitted to be entirely in quotes, to enable
  it to contain white space, which was not envisaged when I first wrote
  this code. Note that concatenation has been handled above. Pity I didn't
  organize a better "read string in quotes" function for Exim at the start,
  but rather than stir things now, just do the necessary here. */

  if (*p == '\"')
    {
    int size = 128;
    int count = 0;
    char *sptr = store_malloc(size);
    while (*(++p) != 0)
      {
      if (*p == '\"') { p++; break; }
      if (*p == '\\' && p[1] != 0)
        {
        char ch = string_interpret_escape(&p);
        sptr = string_cat(sptr, &size, &count, &ch, 1);
        }
      else sptr = string_cat(sptr, &size, &count, p, 1);
      }
    sptr[count] = 0;
    next->replacement = sptr;
    }

  /* Not in quotes; terminate on white space */

  else
    {
    pp = p;
    while (*p != 0 && !isspace(*p)) p++;
    next->replacement = string_copyn(pp, p-pp);
    }

  while (*p != 0) switch (*p++)
    {
    case ' ': case '\t': break;

    case 'h': next->flags |= rewrite_all_headers; break;
    case 's': next->flags |= rewrite_sender; break;
    case 'f': next->flags |= rewrite_from; break;
    case 't': next->flags |= rewrite_to;   break;
    case 'c': next->flags |= rewrite_cc;   break;
    case 'b': next->flags |= rewrite_bcc;  break;
    case 'r': next->flags |= rewrite_replyto; break;
    case 'w': next->flags |= rewrite_whole; break;

    case 'E': next->flags |= rewrite_all_envelope; break;
    case 'F': next->flags |= rewrite_envfrom; break;
    case 'T': next->flags |= rewrite_envto; break;

    default:
      log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
        "unknown rewrite flag character (%c) in line %d",
        p[-1], config_lineno);
    }

  if ((next->flags & rewrite_all) == 0) next->flags |= rewrite_all;
  rewrite_existflags |= next->flags;
  }
}

/* End of readconf.c */

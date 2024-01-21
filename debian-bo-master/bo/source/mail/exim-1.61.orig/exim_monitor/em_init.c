/*************************************************
*                  Exim monitor                  *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* This module contains code to initialize things from the
environment and the arguments. */


#include "em_hdr.h"



/*************************************************
*            Decode stripchart config            *
*************************************************/

/* First determine how many are requested, then compile the
regular expressions and save the title strings. Note that
stripchart_number is initialized to 1 or 2 to count the always-
present queue stripchart, and the optional size-monitoring
stripchart. */

static void decode_stripchart_config(char *s)
{
int i;

/* Loop: first time just counts, second time does the
work. */

for (i = 0; i <= 1; i++)

  {
  int first = 1;
  int count = 0;
  char *p = s;

  if (*p == '/') p++;   /* allow optional / at start */

  /* This loops for all the substrings, using the first flag
  to determine whether each is the first or second of the pairs. */

  while (*p)
    {
    char *pp;
    /* Handle continuations */
    if (*p == '\n')
      {
      while (*(++p) == ' ' || *p == '\t');
      if (*p == '/') p++;
      }

    /* Find the end of the string and count if first string */

    pp = p;
    while (*p && *p != '/') p++;
    if (first) count++;

    /* Take action on the second time round. So as to give more
    information from the regexp error routine, set a pointer to
    the string we are compiling. */

    if (i != 0)
      {
      char buffer[256];
      int indx = count + stripchart_varstart - 1;
      strncpy(buffer, pp, p-pp);
      buffer[p-pp] = 0;
      if (first)
        {
        regexp_compiling = buffer;
        stripchart_regexp[indx] = regcomp(buffer);
        regexp_compiling = NULL;
        }
      else stripchart_title[indx] = string_copy(buffer);
      }

    /* Advance past the delimiter and flip the first/second flag */

    p++;
    first = !first;
    }

  /* On the first pass, we now know the number of stripcharts. Get
  store for holding the pointers to the regular expressions and
  title strings. */

  if (i == 0)
    {
    stripchart_number += count;
    stripchart_regexp = (regexp **)malloc(stripchart_number * sizeof(regexp *));
    stripchart_title = (char **)malloc(stripchart_number * sizeof(char *));
    }
  }
}


/*************************************************
*                    Initialize                  *
*************************************************/

void init(int argc, char **argv)
{
int x;
char *s;

/* Deal with simple values in the environment. */

s = getenv("ACTION_OUTPUT");
if (s != NULL && strcmp(s, "no") == 0) action_output = FALSE;

s = getenv("ACTION_QUEUE_UPDATE");
if (s != NULL && strcmp(s, "no") == 0) action_queue_update = FALSE;

s = getenv("BODY_MAX");
if (s != NULL && (x = atoi(s)) != 0) body_max = x;

s = getenv("EXIM_PATH");
if (s != NULL) exim_path = string_copy(s);

s = getenv("LOG_BUFFER");
if (s != NULL)
  {
  char c[1];
  if (sscanf(s, "%d%c", &x, c) > 0)
    {
    if (c[0] == 'K' || c[0] == 'k') x *= 1024;
    if (x < 1024) x = 1024;
    log_buffer_size = x;
    }
  }

s = getenv("LOG_DEPTH");
if (s != NULL && (x = atoi(s)) != 0) log_depth = x;

s = getenv("LOG_FILE_NAME");
if (s != NULL) log_file = string_copy(s);

s = getenv("LOG_FONT");
if (s != NULL) log_font = string_copy(s);

s = getenv("LOG_WIDTH");
if (s != NULL && (x = atoi(s)) != 0) log_width = x;

s = getenv("MENU_EVENT");
if (s != NULL) menu_event = string_copy(s);

s = getenv("MIN_HEIGHT");
if (s != NULL && (x = atoi(s)) > 0) min_height = x;

s = getenv("MIN_WIDTH");
if (s != NULL && (x = atoi(s)) > 0) min_width = x;

s = getenv("QUALIFY_DOMAIN");
if (s != NULL) qualify_domain = string_copy(s);
  else qualify_domain = "";  /* Don't want NULL */

s = getenv("QUEUE_DEPTH");
if (s != NULL && (x = atoi(s)) != 0) queue_depth = x;

s = getenv("QUEUE_FONT");
if (s != NULL) queue_font = string_copy(s);

s = getenv("QUEUE_INTERVAL");
if (s != NULL && (x = atoi(s)) != 0) queue_update = x;

s = getenv("QUEUE_MAX_ADDRESSES");
if (s != NULL && (x = atoi(s)) != 0) queue_max_addresses = x;

s = getenv("QUEUE_WIDTH");
if (s != NULL && (x = atoi(s)) != 0) queue_width = x;

s = getenv("SPOOL_DIRECTORY");
if (s != NULL) spool_directory = string_copy(s);

s = getenv("WINDOW_TITLE");
if (s != NULL) window_title = string_copy(s);

/* Deal with stripchart configuration. First see if we are monitoring
the size of a partition, then deal with log stripcharts in a separate
function */

s = getenv("SIZE_STRIPCHART");
if (s != NULL)
  {
  stripchart_number++;
  stripchart_varstart++;
  size_stripchart = string_copy(s);
  }

s = getenv("LOG_STRIPCHARTS");
if (s != NULL) decode_stripchart_config(s);

s = getenv("STRIPCHART_INTERVAL");
if (s != NULL && (x = atoi(s)) != 0) stripchart_update = x;

/* Compile the regexp for matching yyyy-mm-dd at the start of a string. */

yyyymmdd_regexp = regcomp("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] ");
}

/* End of em_init.c */

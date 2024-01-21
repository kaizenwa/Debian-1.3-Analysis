/* File retrieval.
   Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* $Id: retr.c,v 1.1.1.1.2.1 1997/02/15 19:23:15 hniksic Exp $ */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>
#ifdef HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif /* HAVE_STRING_H */
#include <ctype.h>
#include <assert.h>

#include "wget.h"
#include "options.h"
#include "utils.h"
#include "retr.h"
#include "url.h"
#include "recur.h"
#include "ftp.h"
#include "html.h"
#include "http.h"
#include "host.h"
#include "connect.h"

/* global variables */
extern struct options opt;

/* Buffered input variables: */
char buffer[INPUT_BUFFER_SIZE]; /* The input buffer itself. */
char *buffer_pos;               /* The current position in the buffer. */
size_t buffer_left;             /* Number of bytes left in the buffer:
				   buffer_left = buffer_pos - buffer */

/* Variables used for the timer.  */
long internal_secs, internal_msecs;


/* The function for reading from file descriptor fd, char-by-char. If
   there is anything in the buffer, the character is returned from the
   buffer. Otherwise, refill the buffer and return the first
   character.

   The return value is the same as with read(2). On buffered read, the
   function returns 1. */
int
buf_readchar(int fd, char *ret)
{
   int res;

   if (buffer_left)
   {
      --buffer_left;
      *ret = *buffer_pos++;
   }
   else
   {
      buffer_pos = buffer;
      buffer_left = 0;
      res = iread(fd, buffer, INPUT_BUFFER_SIZE);
      if (res <= 0)
	 return res;
      buffer_left = res - 1;
      *ret = *buffer_pos++;
   }
   return 1;
}

/* This is similar to buf_readchar, only it doesn't move the buffer
   position.  */
int
buf_peek(int fd, char *ret)
{
   int res;

   if (buffer_left)
      *ret = *buffer_pos;
   else
   {
      buffer_pos = buffer;
      buffer_left = 0;
      res = iread(fd, buffer, INPUT_BUFFER_SIZE);
      if (res <= 0)
	 return res;
      buffer_left = res;
      *ret = *buffer_pos;
   }
   return 1;
}

/* Flush the buffer. Its arguments are the pointer where to copy the
   buffer contents and how much of the contents may be copied in a
   chunk. The return value is the number of bytes actually copied. If
   the buffer is empty, 0 is returned. */
int
buf_flush(char *where, int maxsize)
{
   int howmuch;
   
   if (!buffer_left)
      return 0;
   else
   {
      howmuch = buffer_left <= maxsize ? buffer_left : maxsize;
      if (where)
	 memcpy(where, buffer_pos, howmuch);
      buffer_left -= howmuch;
      buffer_pos += howmuch;
      return howmuch;
   }
}

/* Discard the contents of the input buffer. */
void
buf_discard(void)
{
   buffer_left = 0;
   buffer_pos = buffer;
}

/* Reads the contents of file descriptor fd, until it is closed, or a
   read error occurs. The data is read in parts BUFFER_SIZE bytes
   long, and stored to stream fp, which should have been open for
   writing.

   If opt.verbose is set, the progress is also shown. Variable restval
   is used to represent a value from which to start downloading (which
   will be shown accordingly). If restval is set, the stream should
   have been open for appending.

   The function exits and returns codes of 0, -1 and -2 if the
   connection was closed, there was a read error, or if it could not
   write to the output stream, respectively.

   IMPORTANT: The function flushes the contents of the buffer in
   buf_flush() before actually reading from fd. If you wish to read
   from fd immediately, flush or discard the buffer. */
int
get_contents(int fd, FILE *fp, long *len, long restval, int nobuf)
{
   int res;
   static char c[BUFFER_SIZE];

   *len = restval;
   /* Initialize show_progress. */
   if (opt.verbose)
      show_progress(restval, 1);
   /* Flush the input buffer if !nobuf. */
   if (!nobuf)
   {
      while ((res = buf_flush(c, BUFFER_SIZE)) != 0)
      {
	 if (!fwrite(c, sizeof(char), res, fp))
	    return -2;
	 if (opt.verbose)
	 {
	    if (show_progress(res, 0))
	       fflush(fp);
	 }
	 *len += res;
      }
   }
   /* Read from fd while there is available data. */
   do
   {
      res = iread(fd, c, BUFFER_SIZE);
      if (res > 0)
      {
	 if (!fwrite(c, sizeof(char), res, fp))
	    return -2;
	 if (opt.verbose)
	 {
	    if (show_progress(res, 0))
	       fflush(fp);
	 }
	 *len += res;
      }
   } while (res > 0);
   if (res < -1)
      res = -1;
   if (opt.verbose)
      fprintf(opt.lfile, "\n\n");
   return res;
}

/* Show the dotted progress report of file loading. Called with length
   and a flag to tell it whether to reset or not. It keeps the offset
   information in static local variables.

   Return value: 1 or 0, designating whether any dots have been drawn.

   If the init argument is set, the routine will initialize.

   If the res is non-zero, res/line_bytes lines are skipped
   (meaning the appropriate number ok kilobytes), and the number of
   "dots" fitting on the first line are drawn as ','. */
int
show_progress(long res, int init)
{
   static long line_bytes;
   static long offs;
   static int ndot, nrow;
   int any_output;

   any_output = 0;
   /* init set means initialization. If res is set, it also means that
      the retrieval is *not* done from the beginning. The part that
      was already retrieved is not shown again. */
   if (init == 1)
   {
      /* Generic initialization of static variables. */
      offs = 0L;
      ndot = nrow = 0;
      line_bytes = (long)opt.dots_in_line * opt.dot_bytes;
      if (res)
      {
	 if (res >= line_bytes)
	 {
	    nrow = res / line_bytes;
	    res %= line_bytes;
	    fprintf(opt.lfile, "\n          [ skipping %dK ]",
		    (int)((nrow * line_bytes) / 1024));
	    ndot = 0;
	 }
      }
      fprintf(opt.lfile, "\n%5ldK ->", nrow * line_bytes / 1024);
   }
   /* Offset gets incremented by current value. */
   offs += res;
   /* While offset is >= opt.dot_bytes, print dots, taking care
      that every 50th dot needs to be preceded by a status message. */
   for (; offs >= opt.dot_bytes; offs -= opt.dot_bytes)
   {
      if (!(ndot % opt.dot_spacing))
	 fputc(' ', opt.lfile);
      any_output = 1;
      if (init)
	 fputc(',', opt.lfile);
      else
	 fputc('.', opt.lfile);
      ++ndot;
      if (ndot == opt.dots_in_line)
	 ndot = 0;
      if (ndot == 0)
      {
	 ++nrow;
	 fprintf(opt.lfile, "\n%5ldK ->",
		 nrow * line_bytes / 1024);
      }
   }
   return any_output;
}

/* A function to reset the internal timer. */
void
reset_timer(void)
{
#ifdef HAVE_GETTIMEOFDAY
   struct timeval t;
   gettimeofday(&t, NULL);
   internal_secs = t.tv_sec;
   internal_msecs = t.tv_usec / 1000;
#else
   internal_secs = time(NULL);
   internal_msecs = 0;
#endif
}

/* The time elapsed from the last call to reset_timer, in msecs. */
long
elapsed_time(void)
{
#ifdef HAVE_GETTIMEOFDAY
   struct timeval t;
   gettimeofday(&t, NULL);
   return ((t.tv_sec - internal_secs) * 1000
	   + (t.tv_usec / 1000 - internal_msecs));
#else
   return (long)time(NULL) - internal_secs;
#endif
}

/* The function returns pointer to a static char[] buffer in which
   zero-terminated string-representation of time (in form hh:mm:ss) is
   printed. It is shamelessly non-reentrant, but who cares? :)

   If tm is non-NULL, it also returns the time_t of the current time.  */
char *
time_str(time_t *tm)
{
   static char tms[15];
   struct tm *ptm;
   time_t tim;

   *tms = '\0';
   tim = time(tm);
   if (tim == -1)
      return tms;
   ptm = localtime(&tim);
   sprintf(tms, "%02d:%02d:%02d", ptm->tm_hour, ptm->tm_min, ptm->tm_sec);
   return tms;
}

/* Print out the appropriate download rate.  Appropriate means that if
   rate is > 1024 bytes per second, kilobytes are used, and if rate >
   1024 * 1024 bps, megabytes are used. */
char *
rate(long bytes, long msecs)
{
   static char res[15];
   double dlrate;

   if (!msecs)
      ++msecs;
   dlrate = (double)1000 * bytes / msecs;
   if (dlrate < 1024.0)
      sprintf(res, "%.2f B/s", dlrate);
   else if (dlrate < 1024.0 * 1024.0)
      sprintf(res, "%.2f KB/s", dlrate / 1024.0);
   else
      sprintf(res, "%.2f MB/s", dlrate / (1024.0 * 1024.0));
   return res;
}

/* Retrieve the given URL. Decides which loop to call -- HTTP, FTP, or
   simply copy it with file://. */
uerr_t
retrieve_url(const char *origurl, char **file, char **newloc,
	     const char *refurl, int *dt)
{
   urlinfo *u;
   uerr_t result;
   char *url;
   int local_use_proxy, location_changed, dummy, oldrec;
   char *mynewloc, *proxy;

   /* If dt is NULL, just ignore it. */
   if (!dt)
      dt = &dummy;
   url = nstrdup(origurl);
   if (newloc)
      *newloc = NULL;
   if (file)
      *file = NULL;
   location_changed = 0;
   
   /* This ugly loop is because of Location headers. */
   do
   {
      u = newurl();
      /* Parse the URL.  If the new location was gained from the
         Location header, we need "strict" parsing.  RFC2068 is clear
         about `Location:' containing an absoluteURI.  */
      result = parseurl(url, u, location_changed);
      if (result != URLOK)
      {
	 freeurl(u, 1);
	 if (!opt.quiet)
	    fprintf(opt.lfile, "%s: %s.\n", url, uerrmsg(result));
	 return result;
      }

      /* Set the referer. */
      if (refurl)
	 u->referer = nstrdup(refurl);
      else
	 u->referer = NULL;
      
      local_use_proxy = USE_PROXY(u);
      if (local_use_proxy)
      {
	 urlinfo *pu;
	 
	 pu = newurl();
	 /* Copy the original URL to new location. */
	 memcpy(pu, u, sizeof(*u));
	 pu->proxy = NULL; /* A minor correction :) */
	 /* Initialize u to nil. */
	 memset(u, 0, sizeof(*u));
	 u->proxy = pu;
	 /* Get the appropriate proxy, according to protocol. */
	 proxy = getproxy(pu->proto);
	 if (!proxy)
	 {
	    if (!opt.quiet)
	       fprintf(opt.lfile, "Could not find proxy host.\n");
	    freeurl(u, 1);
	    return PROXERR;
	 }
	 /* Parse the proxy URL. */
	 result = parseurl(proxy, u, 0);
	 if (result != URLOK || u->proto != URLHTTP)
	 {
	    if (!opt.quiet)
	    {
	       if (u->proto == URLHTTP)
		  fprintf(opt.lfile, "Proxy %s: %s.\n", proxy,
			  uerrmsg(result));
	       else
		  fprintf(opt.lfile, "Proxy %s: Must be HTTP.\n", proxy);
	    }
	    freeurl(u, 1);
	    return PROXERR;
	 }
	 u->proto = URLHTTP;
      } /* local_use_proxy */
      
      assert(u->proto != URLFILE); /* For now... */      
      mynewloc = NULL;
      
      if (u->proto == URLHTTP)
	 result = http_loop(u, &mynewloc, dt);
      else if (u->proto == URLFTP)
      {
	 /* If the location has changed, we must not allow recursive
	    FTP retrieval, so we save recursion to oldrec, and restore
	    it later.  */
	 oldrec = opt.recursive;
	 if (location_changed)
	    opt.recursive = 0;
	 result = ftp_loop(u, dt);
	 if (location_changed)
	    opt.recursive = oldrec;
	 /* There is a possibility of having HTTP being redirected to
	    FTP.  In these cases we must decide whether the text is
	    HTML according to the suffix.  The HTML suffixes are
	    `.html' and (yuck!) `.htm', case-insensitive.  */
	 if (location_changed && u->local && (u->proto == URLFTP ))
	 {
	    char *suf = suffix(u->local);
	    if (suf && (!strcasecmp(suf, "html") || !strcasecmp(suf, "htm")))
	       *dt |= TEXTHTML;
	 }
      }
      location_changed = (result == NEWLOCATION);
      if (location_changed)
      {
	 /* Check for redirection to oneself. */
	 if (url_equal(url, mynewloc))
	 {
	    if (!opt.quiet)
	       fprintf(opt.lfile, "%s: Redirection to itself.\n", mynewloc);
	    return WRONGCODE;
	 }
      }
      if (mynewloc)
      {
	 free(url);
	 url = mynewloc;
      }
      if (!location_changed && file)
      {
	 if (u->local)
	    *file = nstrdup(u->local);
	 else
	    *file = NULL;
      }
      freeurl(u, 1);
   } while (location_changed);

   if (newloc)
      *newloc = url;
   else
      free(url);
   
   return result;
}

/* Find the URL-s in the file and call retrieve_url for each of
   them. If the html is non-zero, treat the file as HTML, and
   construct the URL-s accordingly.

   If recursive is set, recursive_retrieve will be called after each
   file. */
uerr_t
retrieve_from_file(const char *file, int html, int *count)
{
   char *filename, *new;
   int first_time, dt;
   uerr_t status;
   urlpos *url_list, *cur_url;

   /* If spider-mode is on, we do not want get_urls_html barfing
      errors on baseless links. */
   url_list = (html ? get_urls_html(file, NULL, opt.spider)
	       : get_urls_file(file));
   status = RETROK;             /* Suppose everything is OK. */
   *count = 0;                  /* Reset the URL count. */
   for (first_time = 1, cur_url = url_list; cur_url;
	cur_url = cur_url->next, ++*count)
   {
      if (opt.quota && opt.downloaded > opt.quota)
      {
	 status = QUOTEXC;
	 break;
      }
      status = retrieve_url(cur_url->url, &filename, &new, NULL, &dt);
      if (opt.recursive && status == RETROK && (dt & TEXTHTML))
	 status = recursive_retrieve(filename, new ? new : cur_url->url,
				     first_time ? RFIRST_TIME : 0);
      if (new)
	 free(new);
      if (filename)
	 free(filename);
      first_time = 0;
   }
   
   /* Free the linked list of URL-s. */
   free_urlpos(url_list);
   
   return status;
}

/* Print 'giving up', or 'retrying', depending on the action to
   do. Numbers represent the attempt number and the attempt limit
   (please don't ask which one is which). */
void
printwhat(int n1, int n2)
{
   if (opt.verbose)
   {
      if (n1 == n2)
	 fprintf(opt.lfile, "Giving up.\n\n");
      else
	 fprintf(opt.lfile, "Retrying.\n\n");
   }
}


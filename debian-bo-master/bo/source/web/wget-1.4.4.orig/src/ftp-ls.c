/* Dealing with Unix FTP servers.
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


/* $Id: ftp-ls.c,v 1.1.1.1.2.1 1997/02/15 19:22:51 hniksic Exp $ */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#include <sys/types.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>

#include "wget.h"
#include "url.h"
#include "utils.h"
#include "ftp.h"
#include "options.h"

extern struct options opt;

/* Converts symbolic permissions to number-style ones, e.g. string
   rwxr-xr-x to 755. For now, it knows nothing of
   setuid/setgid/sticky. ACLs are ignored. */
int
symperms(const char *s)
{
   int perms = 0, i;
   
   if (strlen(s) < 9)
      return 0;
   for (i = 0; i < 3; i++, s += 3)
   {
      perms <<= 3;
      perms += (((*s == 'r') << 2) + ((s[1] == 'w') << 1) +
		(s[2] == 'x' || s[2] == 's'));
   }
   return perms;
}

/* We support only Unix FTP servers for now. */
struct fileinfo *
ftp_parse_ls(const char *file)
{
   return ftp_parse_unix_ls(file);
}

/* The function parses the Un*x-ish style directory listing stored in
   file, and returns the linked list of fileinfo (system-independent)
   entries. The file is considered to be produced by the standard Unix
   ls -la output (whatever that meant:). BSD (no group) and SYSV (with
   group) listings are handled.

   The time stamps are stored in a separate variable, time_t
   compatible (I hope). The time-zones are ignored (unfortunately). */
struct fileinfo *
ftp_parse_unix_ls(const char *file)
{
   FILE *fp;
   static const char *months[] = {
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
   };
   int next, len, i, error, ignore;
   int year, month, day;        /* For time analysis.  */
   int hour, min, sec;
   struct tm timestruct, *tnow;
   time_t timenow;
   
   char *line, *tok;            /* Tokenizer. */
   struct fileinfo *dir, *l, cur; /* List creation. */

   fp = fopen(file, "r");
   if (!fp)
   {
      fprintf(opt.lfile, "%s: %s\n", file, mystrerror(errno));
      return NULL;
   }
   dir = l = NULL;

   /* Line loop to end of file: */
   while ((line = read_whole_line(fp)))
   {
#ifdef DEBUG
      if (opt.debug)
	 fprintf(opt.lfile, "%s\n", line);
#endif
      len = strlen(line);
      /* Destroy <CR> if there is one. */
      if (len && line[len - 1] == '\r')
	 line[--len] = '\0';

      /* Skip if total... */
      if (!strncasecmp(line, "total", 5))
      {
	 free(line);
	 continue;
      }
      /* Get the first token (permissions). */
      tok = strtok(line, " ");
      if (!tok)
      {
	 free(line);
	 continue;
      }

      cur.name = NULL;
      cur.linkto = NULL;

      /* Decide whether we deal with a file or a directory. */
      switch (*tok)
      {
	 case '-':
	    cur.type = PLAINFILE;
	    DEBUGP("PLAINFILE; ");
	    break;
	 case 'd':
	    cur.type = DIRECTORY;
	    DEBUGP("DIRECTORY; ");
	    break;
	 case 'l':
	    cur.type = SYMLINK;
	    DEBUGP("SYMLINK; ");
	    break;
	 default:
	    cur.type = UNKNOWN;
	    DEBUGP("UNKOWN; ");
	    break;
      }

      cur.perms = symperms(tok + 1);
#ifdef DEBUG
      if (opt.debug)
	 fprintf(opt.lfile, "perms %0o; ", cur.perms);
#endif
      
      error = ignore = 0;       /* Errnoeous and ignoring entries are
				   treated equally for now. */
      year = hour = min = sec = 0; /* Silence the compiler. */
      month = day = 0;
      next = -1;
      /* While there are tokens on the line, parse them.  Next is the
	 number of tokens left until the filename.

	 I use the month-name token as the "anchor" (the place I
	 "know" the position wrt the file name).  When a month name is
	 encountered, next is set to 5.  Also, the preceding
	 characters are parsed to get the file size.

	 This tactic is quite dubious when it comes to
	 internationalization issues (non-English month names), but it
	 works for now.  */
      while ((tok = strtok(NULL, " ")))
      {
	 --next;
	 if (next < 0)          /* I.e. a month name was not
				   encountered. */
	 {
	    for (i = 0; i < 12; i++)
	       if (!strcmp(tok, months[i]))
		  break;
	    /* If we got a month, it means the token before it is the
	       size, and the filename is three tokens away. */
	    if (i != 12)
	    {
	       char *t = tok - 2;
	       long mul = 1;
	       
	       for (cur.size = 0; t > line && isdigit(*t); mul *= 10, t--)
		  cur.size += mul * (*t - '0');
	       if (t == line)
	       {
		  /* Something is seriously wrong. */
		  error = 1;
		  break;
	       }
	       month = i;
	       next = 5;
#ifdef DEBUG
	       if (opt.debug)
		  fprintf(opt.lfile, "month: %s; ", months[month]);
#endif
	    }
	 }
	 else if (next == 4)	/* Days */
	 {
	    if (tok[1])         /* Two-digit... */
	       day = 10 * (*tok - '0') + tok[1] - '0';
	    else                /* ...or one-digit. */
	       day = *tok - '0';
#ifdef DEBUG
	    if (opt.debug)
	       fprintf(opt.lfile, "day: %d; ", day);
#endif
	 }
	 else if (next == 3)
	 {
	    /* This ought to be either the time, or the year.  Let's
	       be flexible!

	       If we have a number x, it's a year.  If we have x:y,
	       it's hours and minutes.  If we have x:y:z, z are
	       seconds.  */
	    year = 0;
	    min = hour = sec = 0;
	    /* We must deal with digits. */
	    if (isdigit(*tok))
	    {
	       /* Suppose it's year. */
	       for (; isdigit(*tok); tok++)
		  year = (*tok - '0') + 10 * year;
	       if (*tok == ':')
	       {
		  /* This means these were hours! */
		  hour = year;
		  year = 0;
		  ++tok;
		  /* Get the minutes. */
		  for (; isdigit(*tok); tok++)
		     min = (*tok - '0') + 10 * min;
		  if (*tok == ':')
		  {
		     /* The seconds too. */
		     ++tok;
		     for (; isdigit(*tok); tok++)
			sec = (*tok - '0') + 10 * sec;
		  }
	       }
	    } /* isdigit(*tok) */
#ifdef DEBUG
	    if (opt.debug)
	    {
	       if (year)
		  fprintf(opt.lfile, "year: %d (no tm); ", year);
	       else
		  fprintf(opt.lfile, "time: %02d:%02d:%02d (no yr); ", hour, min, sec);
	    }
#endif
	 } /* next == 3 */
	 else if (next == 2)    /* The file name */
	 {
	    int fnlen;
	    char *p;

	    /* Since the file name may contain a SPC, it is possible
               for strtok to handle it wrong.  */
	    fnlen = strlen(tok);
	    if (fnlen < len - (tok - line))
	    {
	       /* So we have a SPC in the file name.  Restore the
                  original.  */
	       tok[fnlen] = ' ';
	       /* If the file is a symbolic link, it should have a
		  ` -> ' somewhere.  */
	       if (cur.type == SYMLINK)
	       {
		  p = strstr(tok, " -> ");
		  if (!p)
		  {
		     error = 1;
		     break;
		  }
		  cur.linkto = nstrdup(p + 4);
#ifdef DEBUG
		  if (opt.debug)
		     fprintf(opt.lfile, "link to: %s\n", cur.linkto);
#endif
		  /* And separate it from the file name. */
		  *p = '\0';
	       }
	    }
	    /* If we have the filename, add it to the list of files or
	       directories. */
	    /* "." and ".." are an exception! */
	    if (!strcmp(tok, ".") || !strcmp(tok, ".."))
	    {
	       DEBUGP("\nIgnoring `.' and `..'; ");
	       ignore = 1;
	       break;
	    }
	    /* Some FTP sites choose to have ls -F as their default
	       LIST output, which marks the symlinks with a trailing
	       '@', directory names with a trailing '/' and
	       executables with a trailing '*'. This is no problem
	       unless encountering a symbolic link ending with '@', or
	       an executable ending with '*' on a server without
	       default -F output.  I believe these cases are very
	       rare.  */
	    fnlen = strlen(tok);    /* Re-calculate fnlen.  */
	    cur.name = (char *)nmalloc(fnlen + 1);
	    memcpy(cur.name, tok, fnlen + 1);
	    if (fnlen)
	    {
	       if (cur.type == DIRECTORY && cur.name[fnlen - 1] == '/')
	       {
		  cur.name[fnlen - 1] = '\0';
		  DEBUGP("trailing `/' on dir.\n");
	       }
	       else if (cur.type == SYMLINK && cur.name[fnlen - 1] == '@')
	       {
		  cur.name[fnlen - 1] = '\0';
		  DEBUGP("trailing `@' on link.\n");
	       }
	       else if (cur.type == PLAINFILE
			&& (cur.perms & 0111)
			&& cur.name[fnlen - 1] == '*')
	       {
		  cur.name[fnlen - 1] = '\0';
		  DEBUGP("trailing `*' on exec.\n");
	       }
	    } /* if (fnlen) */
	    else
	       error = 1;
	    break;
	 } /* next == 2 */
	 else
	    assert(0);
      }	/* while */

      if (!cur.name || (cur.type == SYMLINK && !cur.linkto))
	 error = 1;
      
      DEBUGP("\n");

      if (error || ignore)
      {
	 DEBUGP("Skipping.\n");
	 if (cur.name)
	    free(cur.name);
	 if (cur.linkto)
	    free(cur.linkto);
	 free(line);
	 continue;
      }
      
      if (!dir)
      {
	 l = dir = (struct fileinfo *)nmalloc(sizeof(struct fileinfo));
	 memcpy(l, &cur, sizeof(cur));
	 l->prev = l->next = NULL;
      }
      else
      {
	 cur.prev = l;
	 l->next = (struct fileinfo *)nmalloc(sizeof(struct fileinfo));
	 l = l->next;
	 memcpy(l, &cur, sizeof(cur));
	 l->next = NULL;
      }
      /* Get the current time. */
      timenow = time(NULL);
      tnow = localtime(&timenow);
      /* Build the time-stamp (the idea by zaga). */
      timestruct.tm_sec   = sec;
      timestruct.tm_min   = min;
      timestruct.tm_hour  = hour;
      timestruct.tm_mday  = day;
      timestruct.tm_mon   = month;
      if (year == 0)
      {
	 /* Some listings will not specify the year if it is "obvious"
	    that the file was from the previous year.  E.g. if today
	    is 97-01-12, and you see a file of Dec 15th, its year is
	    1996, not 1997.  Thanks to Vladimir Volovich for
	    mentioning this!  */
	 if (month > tnow->tm_mon)
	    timestruct.tm_year = tnow->tm_year - 1;
	 else
	    timestruct.tm_year = tnow->tm_year;
      }
      else
	 timestruct.tm_year = year;
      if (timestruct.tm_year >= 1900)
	 timestruct.tm_year -= 1900;
      timestruct.tm_wday  = 0;
      timestruct.tm_yday  = 0;
      timestruct.tm_isdst = -1;
      l->tstamp = mktime(&timestruct); /* Store the time-stamp. */

      /* Free the line. */
      free(line);
   }
   /* Close the file. */
   fclose(fp);
   return dir;
}


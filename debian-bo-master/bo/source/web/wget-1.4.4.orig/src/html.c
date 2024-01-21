/* Dealing with HTML (parsing, generating).
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


/* $Id: html.c,v 1.1.1.1.2.1 1997/02/15 19:23:00 hniksic Exp $ */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <ctype.h>
#ifdef HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <errno.h>

#include "wget.h"
#include "options.h"
#include "url.h"
#include "utils.h"
#include "ftp.h"
#include "html.h"
#include "http.h"

extern struct options opt;
extern int errno;

state_t global_state;

/* Match a string against a null-terminated list of identifiers. */
int
idmatch(tag_t *tags, const char *tag, const char *attr)
{
   int i;

   if (!tag || !attr)
      return 0;
   
   for (i = 0; tags[i].tag; i++)
      if (!strcasecmp(tags[i].tag, tag) && !strcasecmp(tags[i].attr, attr))
	 return 1;
   return 0;
}

/* The following function parses an HTML buffer buf, of size bufsize
   searching for tags. When a tag is encountered, the routine will
   extract its components. If "src", "href" or "background" are found,
   their address is returned, and so is the length of the
   string. Returns NULL if no URL is found.

   We use unsigned char instead of char to be able to use ctype macros
   properly (as suggested by dave@fly.cc.fer.hr).  */
const char *
htmlfindurl(const unsigned char *buf, int bufsize, int *size, int init)
{
   const unsigned char *p, *ph;
   state_t *s;
   /* NULL-terminated list of tags and modifiers someone would want to
      follow -- feel free to edit to suit your needs: */
   static tag_t html_allow[] = {
      { "a", "href" },
      { "img", "src" },
      { "img", "href" },
      { "body", "background" },
      { "frame", "src" },
      { "iframe", "src" },
      { "fig", "src" },
      { "overlay", "src" },
      { "applet", "code" },
      { "script", "src" },
      { "embed", "src" },
      { "bgsound", "src" },
      { "area", "href" },
      { "base", "href" },       /* This one is treated specially! */
      { "meta", "content" },    /* As is this one! */
      { NULL, NULL }
   };

   s = &global_state;
   
   if (init)
   {
      DEBUGP("Resetting a parser state.\n");
      memset(s, 0, sizeof(*s));
   }

   while (1)
   {
      if (!bufsize)
	 break;
      /* Let's look for a tag, if we are not already in one. */
      if (!s->at_value)
      {
	 /* Find '<'. */
	 if (*buf != '<')
	    for (; bufsize && *buf != '<'; ++buf, --bufsize);
	 if (!bufsize)
	    break;
	 /* Skip spaces. */
	 for (++buf, --bufsize; bufsize && isspace(*buf) && *buf != '>';
	      ++buf, --bufsize);
	 if (!bufsize)
	    break;
	 p = buf;
	 /* Find the tag end. */
	 for (; bufsize && !isspace(*buf) && *buf != '>' && *buf != '=';
	      ++buf, --bufsize);
	 if (!bufsize)
	    break;
	 if (*buf == '=')
	 {
	    /* This is the case of <tag=something>, which is
	       illegal. Just skip it. */
	    ++buf, --bufsize;
	    continue;
	 }
	 if (p == buf)
	 {
	    /* This most assuredly means that *buf == '>' */
	    ++buf, --bufsize;
	    continue;
	 }
	 s->tag = strdupdelim((char *)p, (char *)buf);
	 if (*buf == '>')
	 {
	    free(s->tag);
	    s->tag = NULL;
	    ++buf, --bufsize;
	    continue;
	 }
      }
      else                      /* s->at_value */
      {
	 /* Reset the at_value flag. */
	 s->at_value = 0;
	 /* If in quotes, just skip out of them and continue
	    living. */
	 if (s->in_quote)
	 {
	    s->in_quote = 0;
	    for (; bufsize && *buf != s->quote_char; ++buf, --bufsize);
	    if (!bufsize)
	       break;
	    ++buf, --bufsize;
	 }
	 if (!bufsize)
	    break;
	 if (*buf == '>')
	 {
	    if (s->tag)
	       free(s->tag);
	    if (s->attr)
	       free(s->attr);
	    s->tag = s->attr = NULL;
	    continue;
	 }
      }
      /* Find the attributes. */
      do
      {
	 if (s->attr)
	 {
	    free(s->attr);
	    s->attr = NULL;
	 }
	 if (!bufsize)
	    break;
	 /* Skip the spaces if we have them. We don't have them at
	    places like <img alt="something"src="something-else">
					    ^ no spaces here. */
	 if (isspace(*buf))
	    for (++buf, --bufsize; bufsize && isspace(*buf) && *buf != '>';
		 ++buf, --bufsize);
	 if (!bufsize || *buf == '>')
	    break;
	 if (*buf == '=')
	 {
	    /* This is the case of <tag = something>, which is
	       illegal. Just skip it. */
	    ++buf, --bufsize;
	    continue;
	 }
	 p = buf;
	 /* Find the attribute end. */
	 for (; bufsize && !isspace(*buf) && *buf != '>' && *buf != '=';
	      ++buf, --bufsize);
	 if (!bufsize || *buf == '>')
	    break;
	 /* Construct the attribute. */
	 s->attr = strdupdelim((char *)p, (char *)buf);
	 /* Now we must skip the spaces to find '='. */
	 if (*buf != '=')
	 {
	    for (; bufsize && isspace(*buf) && *buf != '>'; ++buf, --bufsize);
	    if (!bufsize || *buf == '>')
	       break;
	 }
	 /* If we still don't have '=', something is amiss. */
	 if (*buf != '=')
	    continue;
	 /* Find the beginning of attr. value by skipping the
	    spaces. */
	 ++buf, --bufsize;
	 for (; bufsize && isspace(*buf) && *buf != '>'; ++buf, --bufsize);
	 if (!bufsize || *buf == '>')
	    break;
	 ph = NULL;
	 /* The value of an attribute can, but does not have to be
	    quoted. */
	 if (*buf == '\"' || *buf == '\'')
	 {
	    s->in_quote = 1;
	    s->quote_char = *buf;
	    p = buf + 1;
	    for (++buf, --bufsize;
		 bufsize && *buf != s->quote_char && *buf != '\n';
		 ++buf, --bufsize)
	       if (*buf == '#')
		  ph = buf;
	    if (!bufsize)
	    {
	       s->in_quote = 0;
	       break;
	    }
	    if (*buf == '\n')
	    {
	       /* Obviously no longer in quote. It might be well to
		  check whether '>' was encountered, but that would be
		  encouraging writers of invalid HTMLs, and we don't
		  want that, now do we? */
	       s->in_quote = 0;
	       continue;
	    }
	 }
	 else
	 {
	    p = buf;
	    for (; bufsize && !isspace(*buf) && *buf != '>'; ++buf, --bufsize)
	       if (*buf == '#')
		  ph = buf;
	    if (!bufsize)
	       break;
	 }
	 /* If '#' was found unprotected in a URI, it is probably an
	    HTML marker, or color spec. */
	 *size = (ph ? ph : buf) - p;
	 /* The URI is liable to be returned if:
	    1) *size != 0;
	    2) its tag and attribute are found in html_allow. */
	 if (*size && idmatch(html_allow, s->tag, s->attr))
	 {
	    if (!strcasecmp(s->tag, "base") && !strcasecmp(s->attr, "href"))
	    {
	       if (s->base)
		  free(s->base);
	       s->base = strdupdelim((char *)p, (char *)buf);
	    }
	    else if (!strcasecmp(s->tag, "meta") && !strcasecmp(s->attr, "content"))
	    {
	       /* Some pages use a META tag to specify that the page
		  be refreshed by a new page after a given number of
		  seconds.  We need to attempt to extract an URL for
		  the new page from the other garbage present.  The
		  general format for this is:                  
		  <META HTTP-EQUIV=Refresh CONTENT="0; URL=index2.html">

		  So we just need to skip past the "0; URL=" garbage
		  to get to the URL.  META tags are also used for
		  specifying random things like the page author's name
		  and what editor was used to create it.  So we need
		  to be careful to ignore them and not assume that an
		  URL will be present at all. */
	       for (; *size && isdigit(*p); p++, *size -= 1);
	       if (*p == ';')
	       {
		  for (p++, *size -= 1; *size && isspace(*p); p++, *size -= 1) ;
		  if (!strncasecmp((char *)p, "URL=", 4))
		  {
		     p += 4, *size -= 4;
		     s->at_value = 1;
		     return (char *)p;
		  }
	       }
	    }
	    else
	    {
	       s->at_value = 1;
	       return (char *)p;
	    }
	 }
	 /* Exit from quote. */
	 if (*buf == s->quote_char)
	 {
	    s->in_quote = 0;
	    ++buf, --bufsize;
	 }
      } while (*buf != '>');
      if (s->tag)
	 free(s->tag);
      if (s->attr)
	 free(s->attr);
      s->tag = s->attr = NULL;
      if (!bufsize)
	 break;
   }

   if (s->tag)
      free(s->tag);
   if (s->attr)
      free(s->attr);
   if (s->base)
      free(s->base);
   memset(s, 0, sizeof(*s));    /* Just to be sure. */
   DEBUGP("HTML parser ends here (state destroyed).\n");
   return NULL;
}

/* The function returns the base reference of HTML buffer id, or NULL
   if one wasn't defined for that buffer. */
const char *
html_base(void)
{
   return global_state.base;
}

/* The function returns the pointer to the malloc-ed quoted version of
   string s.  It will recognize and quote numeric and special graphic
   entities, as per RFC1866:

   `&' -> `&amp;'
   `<' -> `&lt;'
   `>' -> `&gt;'
   `"' -> `&quot;'

   No other entities are recognized or replaced. */
char *
html_quote_string(const char *s)
{
   const char *b;
   char *p, *res;
   int i;

   b = s;
   /* Pass through the string, and count the new size. */
   for (i = 0; *s; s++, i++)
   {
      if (*s == '&')
	 i += 4;                /* `amp;' */
      else if (*s == '<' || *s == '>')
	 i += 3;                /* `lt;' and `gt;' */
      else if (*s == '\"')
	 i += 5;                /* `quot;' */
   }
   /* Allocate it. */
   res = (char *)nmalloc(i + 1);
   s = b;
   for (p = res; *s; s++)
   {
      switch (*s)
      {
	 case '&':
	    *p++ = '&';
	    *p++ = 'a';
	    *p++ = 'm';
	    *p++ = 'p';
	    *p++ = ';';
	    break;
	 case '<': case '>':
	    *p++ = '&';
	    *p++ = (*s == '<' ? 'l' : 'g');
	    *p++ = 't';
	    *p++ = ';';
	    break;
	 case '\"':
	    *p++ = '&';
	    *p++ = 'q';
	    *p++ = 'u';
	    *p++ = 'o';
	    *p++ = 't';
	    *p++ = ';';
	    break;
	 default:
	    *p++ = *s;
      }
   }
   *p = '\0';
   return res;
}

/* The function creates an HTML index containing references to given
   directories and files on the appropriate host. The references are
   FTP. */
uerr_t
ftp_index(const char *file, urlinfo *u, struct fileinfo *f)
{
   FILE *fp;
   struct tm *ptm;
   static char *months[] = {
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
   };
   char *upwd;
   char *htclfile;    /* HTML-clean file name. */

   if (!opt.dfp)
   {
      fp = fopen(file, "w");
      if (!fp)
      {
	 if (!opt.quiet)
	    fprintf(opt.lfile, "%s: %s\n", file, mystrerror(errno));
	 return FOPENERR;
      }
   }
   else
      fp = opt.dfp;
   if (u->user)
   {
      char *tmpu, *tmpp;        /* Temporary, clean user and
				   passwd. */
      tmpu = CLEANDUP(u->user);
      tmpp = u->passwd ? CLEANDUP(u->passwd) : NULL;
      upwd = (char *)nmalloc(strlen(tmpu)
			     + (tmpp ? (1 + strlen(tmpp)) : 0) + 2);
      sprintf(upwd, "%s%s%s@", tmpu, tmpp ? ":" : "", tmpp ? tmpp : "");
      free(tmpu);
      if (tmpp)
	 free(tmpp);
   }
   else
      upwd = nstrdup("");
   fprintf(fp, "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n");
   fprintf(fp,
	   "<html>\n<head>\n<title>Index of /%s on %s:%d</title>\n</head>\n",
	   u->dir, u->host, u->port);
   fprintf(fp, "<body>\n<h1>Index of /%s on %s:%d</h1>\n<hr>\n",
	   u->dir, u->host, u->port);
   fprintf(fp, "<pre>\n");
   while (f)
   {
      fprintf(fp, "  ");
      if (f->tstamp != -1)
      {
	 ptm = localtime((time_t *)&f->tstamp);
	 fprintf(fp, "%d %s %02d ", ptm->tm_year + 1900, months[ptm->tm_mon],
		 ptm->tm_mday);
	 if (ptm->tm_hour)
	    fprintf(fp, "%02d:%02d  ", ptm->tm_hour, ptm->tm_min);
	 else
	    fprintf(fp, "       ");
      }
      else
	 fprintf(fp, "time unknown       ");
      switch (f->type)
      {
	 case PLAINFILE:
	    fprintf(fp, "File        ");
	    break;
	 case DIRECTORY:
	    fprintf(fp, "Directory   ");
	    break;
	 case SYMLINK:
	    fprintf(fp, "Link        ");
	    break;
	 default:
	    fprintf(fp, "Not sure    ");
	    break;
      }
      htclfile = html_quote_string(f->name);
      fprintf(fp, "<a href=\"ftp://%s%s:%hu", upwd, u->host, u->port);
      if (*u->dir != '/')
	 putc('/', fp);
      fprintf(fp, "%s", u->dir);
      if (*u->dir)
	 putc('/', fp);
      fprintf(fp, "%s", htclfile);
      if (f->type == DIRECTORY)
	 putc('/', fp);
      fprintf(fp, "\">%s", htclfile);
      if (f->type == DIRECTORY)
	 putc('/', fp);
      fprintf(fp, "</a> ");
      if (f->type == PLAINFILE)
	 fprintf(fp, " (%s bytes)", legible(f->size));
      else if (f->type == SYMLINK)
	 fprintf(fp, "-> %s", f->linkto ? f->linkto : "(nil)");
      putc('\n', fp);
      free(htclfile);
      f = f->next;
   }
   fprintf(fp, "</pre>\n</body>\n</html>\n");
   free(upwd);
   if (!opt.dfp)
      fclose(fp);
   else
      fflush(fp);
   return FTPOK;
}


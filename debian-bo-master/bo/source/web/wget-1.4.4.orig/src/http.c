/* HTTP support.
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


/* $Id: http.c,v 1.1.1.1.2.5 1997/02/17 06:20:00 hniksic Exp $ */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <ctype.h>
#include <assert.h>

#ifdef WINDOWS
#  include <winsock.h>
#endif

#include "wget.h"
#include "options.h"
#include "utils.h"
#include "url.h"
#include "host.h"
#include "http.h"
#include "retr.h"
#include "connect.h"
#include "mtch.h"
#include "netrc.h"

extern char *version_string;
extern struct options opt;

#ifndef errno
extern int errno;
#endif
#ifndef h_errno
extern int h_errno;
#endif

/* Function to fetch a header from socket/file descriptor fd. The
   header may be of arbitrary length, since the function allocates as
   much memory as necessary for the header to fit. Most errors are
   handled.

   The header may be terminated by LF or CRLF.  If the character after
   LF is SP or HT (horizontal tab), the header spans to another line
   (continuation header), as per RFC2068.

   The trailing CRLF or LF are stripped from the header, and it is
   zero-terminated. */
uerr_t
fetch_next_header(int fd, char **hdr)
{
   int i, bufsize, res;
   char next;

   bufsize = DYNAMIC_LINE_BUFFER;
   *hdr = (char *)nmalloc(bufsize);
   for (i = 0; 1; i++)
   {
      if (i > bufsize - 1)
	 *hdr = (char *)nrealloc(*hdr, (bufsize <<= 1));
      res = buf_readchar(fd, *hdr + i);
      if (res == 1)
      {
	 if ((*hdr)[i] == '\n')
	 {
	    if (!(i == 0 || (i == 1 && (*hdr)[0] == '\r')))
	    {
	       /* If the header is non-empty, we need to check if it
                  continues on to the other line.  We do that by
                  getting the next character without actually
                  downloading it (i.e. peeking it).  */
	       res = buf_peek(fd, &next);
	       if (res == 0)
		  return HEOF;
	       else if (res == -1)
		  return HERR;
	       /*  If the next character is SP or HT, just continue.  */
	       if (next == '\t' || next == ' ')
		  continue;
	    }
	    /* The header ends.  */
	    (*hdr)[i] = '\0';
	    /* Get rid of '\r'. */
	    if (i > 0 && (*hdr)[i - 1] == '\r')
	       (*hdr)[i - 1] = '\0';
	    break;
	 }
      }
      else if (res == 0)
	 return HEOF;
      else
	 return HERR;
   }
#ifdef DEBUG
   if (opt.debug)
      fprintf(opt.lfile, "%s\n", *hdr);
#endif
   return HOK;
}

/* Parse the HTTP status line. It is of format:
   HTTP-Version SP Status-Code SP Reason-Phrase

   The function returns the status-code, or -1 if the status line is
   malformed. The pointer to reason-phrase is returned in rp. */
int
hparsestatline(const char *hdr, const char **rp)
{
   int mjr, mnr;                /* HTTP major and minor version. */
   int statcode;                /* HTTP status code. */
   const char *p;

   *rp = NULL;
   /* The standard format of HTTP-Version is:
      HTTP/x.y, where x is major version, and y is minor version. */
   if (strncmp(hdr, "HTTP/", 5) != 0)
      return -1;
   hdr += 5;
   p = hdr;
   for (mjr = 0; isdigit(*hdr); hdr++)
      mjr = 10 * mjr + (*hdr - '0');
   if (*hdr != '.' || p == hdr)
      return -1;
   ++hdr;
   p = hdr;
   for (mnr = 0; isdigit(*hdr); hdr++)
      mnr = 10 * mnr + (*hdr - '0');
   if (*hdr != ' ' || p == hdr)
      return -1;
   /* Wget will accept only 1.0 and higher HTTP-versions. The value
      of minor version can be safely ignored. */
   if (mjr < 1)
      return -1;
   /* Skip the space. */
   ++hdr;
   if (!(isdigit(*hdr) && isdigit(hdr[1]) && isdigit(hdr[2])))
      return -1;
   statcode = 100 * (*hdr - '0') + 10 * (hdr[1] - '0') + (hdr[2] - '0');
   /* RFC2068 requires a SPC here, even if there is no reason-phrase.
      As some servers/CGI are (incorrectly) setup to drop the SPC,
      we'll be liberal and allow the status line to end here.  */
   if (hdr[3] != ' ')
   {
      if (!hdr[3])
	 *rp = hdr + 3;
      else
	 return -1;
   }
   else
      *rp = hdr + 4;
   return statcode;
}

/* Skip LWS (linear white space), if present.  Returns number of
   characters to skip.  */
int
hskip_lws(const char *hdr)
{
   int i;
   
   for (i = 0;
	*hdr == ' ' || *hdr == '\t' || *hdr == '\r' || *hdr == '\n';
	++hdr)
      ++i;
   return i;
}

/* Return the content length of the document body, if this is
   Content-length header, -1 otherwise. */
long
hgetlen(const char *hdr)
{
   static const int l = 15;           /* strlen("content-length:") */
   long len;

   if (strncasecmp(hdr, CONTLEN_H, l))
      return -1;
   hdr += (l + hskip_lws(hdr + l));
   if (!*hdr)
      return -1;
   if (!isdigit(*hdr))
      return -1;
   for (len = 0; isdigit(*hdr); hdr++)
      len = 10 * len + (*hdr - '0');
   return len;
}

/* Return the content-range in bytes, as returned by the server, if
   this is Content-range header, -1 otherwise. */
long
hgetrange(const char *hdr)
{
   static const int l = 14;           /* strlen("content-range:") */
   long len;

   if (strncasecmp(hdr, CONTRANGE_H, l))
      return -1;
   hdr += (l + hskip_lws(hdr + l));
   if (!*hdr)
      return -1;
   /* Nutscape proxy server sends content-length without "bytes"
      specifier, which is a breach of HTTP/1.1 draft. But heck, I must
      support it... */
   if (!strncasecmp(hdr, "bytes", 5))
   {
      hdr += 5;
      hdr += hskip_lws(hdr);
      if (!*hdr)
	 return -1;
   }
   if (!isdigit(*hdr))
      return -1;
   for (len = 0; isdigit(*hdr); hdr++)
      len = 10 * len + (*hdr - '0');
   return len;
}

/* Returns the malloc-ed copy of the type of the header hdr, to the
   first ';', or NULL if the header does not begin with CONTTYPE_H
   string. */
char *
hgettype(const char *hdr)
{
   static const int l = 13;           /* strlen("content-type:") */
   char *type;

   if (strncasecmp(hdr, CONTTYPE_H, l))
      return NULL;
   hdr += (l + hskip_lws(hdr + l));
   if ((type = strrchr(hdr, ';')) != NULL)
      *type = '\0';
   return nstrdup(hdr);
}

/* Returns a malloc-ed copy of the location of the document, if the
   string hdr begins with LOCATION_H, or NULL. */
char *
hgetlocation(const char *hdr)
{
   static const int l = 9;           /* strlen("location:") */

   if (strncasecmp(hdr, LOCATION_H, l))
      return NULL;
   hdr += (l + hskip_lws(hdr + l));
   return nstrdup(hdr);
}

/* Returns a malloc-ed copy of the last-modified date of the document,
   if the hdr begins with LASTMODIFIED_H. */
char *
hgetmodified(const char *hdr)
{
   static const int l = 14;           /* strlen("last-modified:") */

   if (strncasecmp(hdr, LASTMODIFIED_H, l))
      return NULL;
   hdr += (l + hskip_lws(hdr + l));
   return nstrdup(hdr);
}

/* Returns 1 if the header is accept-ranges, and it contains the word
   "none", 0 otherwise.  */
int
haccepts_none(const char *hdr)
{
   static const int l = 14;           /* strlen("accept-ranges:") */

   if (strncasecmp(hdr, ACCEPTRANGES_H, l))
      return 0;
   hdr += (l + hskip_lws(hdr + l));
   if (strstr(hdr, "none"))
      return 1;
   else
      return 0;
}

/* Retrieves a document through HTTP protocol.  It recognizes status
   code, and correctly handles redirections.  It closes the network
   socket.  If it receives an error from the functions below it, it
   will print it if there is enough information to do so (almost
   always), returning the error to the caller (i.e. http_loop).

   Various HTTP parameters are stored to hs.  Although it parses the
   response code correctly, it is not used in a sane way.  The caller
   can do that, though.

   If u->proxy is non-NULL, the URL u will be taken as a proxy URL,
   and u->proxy->url will be given to the proxy server (bad naming,
   I'm afraid). */
uerr_t
gethttp(urlinfo *u, http_stat_t *hs, int *dt)
{
   char *request, *hdr, *type, *command, *path;
   char *user, *passwd;
   const char *error;
   char *pragma_h, *referer, *useragent, *range, *wwwauth, *remhost;
   char *proxyauth;
   char *all_headers;
   int sock, hcount, num_written, all_length, remport, statcode;
   long contlen, contrange;
   urlinfo *ou;
   uerr_t err;
   FILE *fp;

   /* Let the others worry about local filename... */
   if (!(*dt & HEAD_ONLY))
      assert(u->local != NULL);

   /* Initialize certain elements of struct hstat. */
   hs->len = 0L;
   hs->contlen = -1;
   hs->res = -1;
   hs->newloc = NULL;
   hs->remote_time = NULL;
   hs->error = NULL;

   /* Which structure to use to yield the original URL data. */
   if (u->proxy)
      ou = u->proxy;
   else
      ou = u;

   /* First: establish the connection. */
   if (opt.verbose)
      fprintf(opt.lfile, "Connecting to %s:%hu... ", u->host, u->port);
   err = make_connection(&sock, u->host, u->port);
   switch (err)
   {
      case HOSTERR:
	 if (!opt.quiet)
	 {
	    if (opt.verbose)
	       fprintf(opt.lfile, "\n");
	    fprintf(opt.lfile, "%s: %s.\n", u->host, herrmsg(h_errno));
	 }
	 return HOSTERR;
	 break;
      case CONSOCKERR:
	 if (!opt.quiet)
	 {
	    if (opt.verbose)
	       fprintf(opt.lfile, "\n");
	    fprintf(opt.lfile, "socket: %s\n", mystrerror(errno));
	 }
	 return CONSOCKERR;
	 break;
      case CONREFUSED:
	 if (!opt.quiet)
	 {
	    if (opt.verbose)
	       fprintf(opt.lfile, "\n");
	    fprintf(opt.lfile, "Connection to %s:%hu refused.\n", u->host, u->port);
	 }
	 CLOSE(sock);
	 return CONREFUSED;
      case CONERROR:
	 if (!opt.quiet)
	 {
	    if (opt.verbose)
	       fprintf(opt.lfile, "\n");
	    fprintf(opt.lfile, "connect: %s\n", mystrerror(errno));
	 }
	 CLOSE(sock);
	 return CONERROR;
	 break;
      case NOCONERROR:
	 /* Everything is fine! */
	 if (opt.verbose)
	    fprintf(opt.lfile, "connected!\n");
	 break;
      default:
	 assert(0);
	 break;
   } /* switch */

   if (u->proxy)
      path = u->proxy->url;
   else
      path = u->path;
   command = (*dt & HEAD_ONLY) ? "HEAD" : "GET";
   referer = NULL;
   if (ou->referer)
   {
      referer = (char *)nmalloc(9 + strlen(ou->referer) + 3);
      sprintf(referer, "Referer: %s\r\n", ou->referer);
   }
   if (*dt & SEND_NOCACHE)
      pragma_h = "Pragma: no-cache\r\n";
   else
      pragma_h = "";
   if (hs->restval)
   {
      range = (char *)nmalloc(13 + numdigit(hs->restval) + 4);
      sprintf(range, "Range: bytes=%ld-\r\n", hs->restval);
   }
   else
      range = NULL;
   useragent = opt.useragent ? opt.useragent : version_string;
   /* Construct the authentication, if userid is present. */
   user = ou->user;
   passwd = ou->passwd;
   search_netrc(u->host, (const char **)&user, (const char **)&passwd, 0);
   user = user ? user : opt.http_user;
   passwd = passwd ? passwd : opt.http_passwd;

   if (user && passwd)
   {
      char *t1, *t2;

      t1 = (char *)nmalloc(strlen(user) + 1 + 2 * strlen(passwd) + 1);
      sprintf(t1, "%s:%s", user, passwd);
      t2 = base64_encode_line(t1);
      free(t1);
      wwwauth = (char *)nmalloc(strlen(t2) + 24);
      sprintf(wwwauth, "Authorization: Basic %s\r\n", t2);
      free(t2);
   }
   else
      wwwauth = NULL;

   /* Construct the autorisation for proxy if supplied */
   if (opt.proxy_user && opt.proxy_passwd)
   {
      char *t1, *t2;

      t1 = (char *)nmalloc(strlen(opt.proxy_user)
			   + 1 + 2 * strlen(opt.proxy_passwd) + 1);
      sprintf(t1, "%s:%s", opt.proxy_user, opt.proxy_passwd);
      t2 = base64_encode_line(t1);
      free(t1);
      proxyauth = (char *)nmalloc(strlen(t2) + 24);
      sprintf(proxyauth, "Proxy-authorization: Basic %s\r\n", t2);
      free(t2);
   } else
     proxyauth = NULL;

   remhost = ou->host;
   remport = ou->port;
   /* Allocate the memory for the request. */
   request = (char *)nmalloc(strlen(command) + strlen(path)
			     + strlen(useragent)
			     + strlen(remhost) + numdigit(remport)
			     + strlen(HTTP_ACCEPT)
			     + (referer ? strlen(referer) : 0)
			     + (wwwauth ? strlen(wwwauth) : 0)
			     + (proxyauth ? strlen(proxyauth) : 0)
			     + (range ? strlen(range) : 0)
			     + strlen(pragma_h)
			     + (opt.user_header ? strlen(opt.user_header) : 0)
			     + 62);
   /* Construct the request. */
   sprintf(request, "%s %s HTTP/1.0\r\nUser-Agent: %s\r\nHost: %s:%d\r\nAccept: %s\r\n%s%s%s%s%s%s\r\n",
	   command, path, useragent, remhost, remport, HTTP_ACCEPT, 
	   referer ? referer : "", 
	   wwwauth ? wwwauth : "", 
	   proxyauth ? proxyauth : "", 
	   range ? range : "",
	   pragma_h, 
	   opt.user_header ? opt.user_header : "");
   /* Free the temporary memory. */
   if (referer)
      free(referer);
   if (range)
      free(range);
   if (wwwauth)
      free(wwwauth);

   DEBUGP(request);

   /* Send the request to server */
   num_written = iwrite(sock, request, strlen(request));
   if (num_written != strlen(request))
   {
      if (opt.verbose)
	 fprintf(opt.lfile, "Failed writing HTTP request.\n");
      free(request);
      CLOSE(sock);
      return WRITEFAILED;
   }
   if (opt.verbose)
      fprintf(opt.lfile, "%s request sent, fetching headers... ",
	      u->proxy ? "HTTP proxy" : "HTTP");
   free(request);
   contlen = contrange = -1;
   type = NULL;
   statcode = -1;
   *dt &= ~RETROKF;

   /* Since this is a new connection, we may safely discard anything
      left in the buffer. */
   buf_discard();
   
   all_headers = NULL;
   all_length = 0;
   /* Header-fetching loop. */
   hcount = 0;
   for (;;)
   {
      ++hcount;
      /* Get the header. */
      err = fetch_next_header(sock, &hdr);
      /* Check for errors. */
      if (err == HEOF)
      {
	 if (!opt.quiet)
	 {
	    if (opt.verbose)
	       fprintf(opt.lfile, "\n");
	    fprintf(opt.lfile, "End of file while parsing headers.\n");
	 }
	 free(hdr);
	 if (type)
	    free(type);
	 if (hs->newloc)
	    free(hs->newloc);
	 if (all_headers)
	    free(all_headers);
	 CLOSE(sock);
	 return HEOF;
      }
      else if (err == HERR)
      {
	 if (!opt.quiet)
	 {
	    if (opt.verbose)
	       fprintf(opt.lfile, "\n");
	    fprintf(opt.lfile, "Read error (%s) in headers.\n",
		    mystrerror(errno));
	 }
	 free(hdr);
	 if (type)
	    free(type);
	 if (hs->newloc)
	    free(hs->newloc);
	 if (all_headers)
	    free(all_headers);
	 CLOSE(sock);
	 return HERR;
      }

      /* If the headers are to be saved to a file later, save them to
	 memory now. */
      if (opt.save_headers)
      {
	 int lh = strlen(hdr);
	 all_headers = (char *)nrealloc(all_headers, all_length + lh + 2);
	 memcpy(all_headers + all_length, hdr, lh);
	 all_length += lh;
	 all_headers[all_length++] = '\n';
	 all_headers[all_length] = '\0';
      }
      
      /* Exit on empty header. */
      if (!*hdr)
      {
	 free(hdr);
	 break;
      }

      /* Print the header if necessary. */
      if (opt.verbose && opt.server_response)
	 fprintf(opt.lfile, "\n%d %s", hcount, hdr);

      /* Check for errors documented in the first header. */
      if (hcount == 1)
      {
	 /* Parse the first line of server response. */
	 statcode = hparsestatline(hdr, &error);
	 hs->statcode = statcode;
	 /* Store the descriptive response. */
	 if (statcode == -1) /* malformed request */
	    hs->error = nstrdup("UNKNOWN");
	 else if (!*error)
	    hs->error = nstrdup("(no description)");
	 else
	    hs->error = nstrdup(error);
      }
      /* Try getting content-length. */
      if (contlen == -1 && !opt.ignore_length)
	 contlen = hgetlen(hdr);
      /* Try getting content-type. */
      if (!type)
	 type = hgettype(hdr);
      /* Try getting location. */
      if (!hs->newloc)
	 hs->newloc = hgetlocation(hdr);
      /* Try getting last-modified. */
      if (!hs->remote_time)
	 hs->remote_time = hgetmodified(hdr);
      /* Check for accept-ranges header.  If it contains the word
	 `none', disable the ranges.  */
      if (*dt & ACCEPTRANGES)
	 if (haccepts_none(hdr))
	    *dt &= ~ACCEPTRANGES;
      /* Try getting content-range. */
      if (contrange == -1)
	 contrange = hgetrange(hdr);
      /* Free the current header. */
      free(hdr);
   } /* for (;;) */

   /* 20x responses are counted among successful by default. */
   if (H_20X(statcode))
      *dt |= RETROKF;

   if (type)
      (!strncasecmp(type, TEXTHTML_S, strlen(TEXTHTML_S))) ?
	 (*dt |= TEXTHTML) : (*dt &= ~TEXTHTML);
   else
      *dt &= ~TEXTHTML; /* NOT text/html by default */

   if (contrange == -1)
      hs->restval = 0;
   else if (contrange != hs->restval ||
	    (H_PARTIAL(statcode) && contrange == -1))
   {
      /* This means the whole request was somehow misunderstood by the
	 server. Bail out. */
      if (type)
	 free(type);
      if (hs->newloc)
	 free(hs->newloc);
      if (all_headers)
	 free(all_headers);
      CLOSE(sock);
      return RANGEERR;
   }

   if (hs->restval)
   {
      if (contlen != -1)
	 contlen += contrange;
      else
	 contrange = -1;        /* If conent-length was not sent,
				   content-range will be ignored. */
   }
   hs->contlen = contlen;
   if (opt.verbose)
   {
      if (!opt.server_response)
	  fprintf(opt.lfile, "done.");
      fprintf(opt.lfile, "\n");
   }
   /* Return if redirected.  */
   if (H_REDIRECTED(statcode) || statcode == HTTP_MULTIPLE_CHOICES)
   {
      /* RFC2068 says that in case of the 300 (multiple choices)
	 response, the server can output a preferred URL through
	 `Location' header; otherwise, the request should be treated
	 like GET.  So, if the location is set, it will be a
	 redirection; otherwise, just proceed normally.  */
      if (statcode == HTTP_MULTIPLE_CHOICES && !hs->newloc)
	 *dt |= RETROKF;
      else
      {
	 fprintf(opt.lfile, "Location: %s%s\n",
		 hs->newloc ? hs->newloc : "unspecified",
		 hs->newloc ? " [following]" : "");
	 CLOSE(sock);
	 if (all_headers)
	    free(all_headers);
	 if (type)
	    free(type);
	 return NEWLOCATION;
      }
   }
   if (opt.verbose)
   {
      if ((*dt & RETROKF) && !opt.server_response)
      {
	 /* No need tp print this output if the body won't be
	    downloaded at all, or if the original server response is
	    printed.  */
	 fprintf(opt.lfile, "Length: ");
	 if (contlen != -1)
	 {
	    fprintf(opt.lfile, "%s", legible(contlen));
	    if (contrange != -1)
	       fprintf(opt.lfile, " (%s to go)", legible(contlen - contrange));
	 }
	 else
	    fprintf(opt.lfile, opt.ignore_length ? "ignored" : "unspecified");
	 if (type)
	    fprintf(opt.lfile, " [%s]\n", type);
	 else
	    fprintf(opt.lfile, "\n");
      }
   }
   if (type)
      free(type);
   type = NULL;                 /* We don't need it any more. */

   /* Return if we have no intention of further downloading.  */
   if (!(*dt & RETROKF) || (*dt & HEAD_ONLY))
   {
      /* In case someone cares to look... */
      hs->len = 0L;
      hs->res = 0;
      if (all_headers)
	 free(all_headers);
      if (type)
	 free(type);
      CLOSE(sock);
      return RETRFINISHED;
   }

   /* Open the local file.  */
   if (!opt.dfp)
   {
      mkalldirs(u->local);
      fp = fopen(u->local, hs->restval ? "ab" : "wb");
      if (!fp)
      {
	 if (!opt.quiet)
	    fprintf(opt.lfile, "%s: %s\n", u->local, mystrerror(errno));
	 CLOSE(sock);
	 if (all_headers)
	    free(all_headers);
	 return FOPENERR;
      }
   }
   else                      /* opt.dfp */
      fp = opt.dfp;

   if (opt.save_headers)
      fwrite(all_headers, 1, all_length, fp);
   reset_timer();
   /* Get the contents of the document. */
   hs->res = get_contents(sock, fp, &hs->len, hs->restval, 0);
   hs->dltime = elapsed_time();
   if (!opt.dfp)
      fclose(fp);
   else
      fflush(fp);
   if (all_headers)
      free(all_headers);
   CLOSE(sock);
   if (hs->res == -2)
      return FWRITEERR;
   return RETRFINISHED;
}

/* The genuine HTTP loop! This is the part where the retrieval is
   retried, and retried, and retried, and... */
uerr_t
http_loop(urlinfo *u, char **newloc, int *dt)
{
   static int first_retrieval = 1;
   
   int count;
   int use_ts, got_head = 0;    /* Time-stamping info. */
   char *tms, *suf, *locf, *tmrate;
   uerr_t err;
   time_t tml = -1, tmr = -1;   /* Local and remote time-stamps. */
   long local_size = 0;         /* The size of the local file. */
   http_stat_t hstat;           /* HTTP status. */
   struct stat st;
   void my_touch PARAMS((char *, time_t));

   *newloc = NULL;
   /* Warn on wildcard usage in HTTP.  Don't use has_wildcards because
      it would also warn on '?', and we don't what that because of
      CGI. */
   if (opt.verbose && strchr(u->url, '*'))
      fprintf(opt.lfile, "Warning: wildcards not supported in HTTP.\n");
   /* Determine the local filename. */
   if (!u->local)
   {
      if (!opt.timestamping || opt.recursive)
	 u->local = url_filename(u->proxy ? u->proxy : u);
      else /* opt.timestamping && !recursive */
      {
	 if (*(u->proxy ? u->proxy->file : u->file))
	    u->local = nstrdup(u->proxy ? u->proxy->file : u->file);
	 else
	    u->local = nstrdup("index.html");
      }
   }

   if (!opt.output_document)
      locf = u->local;
   else
      locf = opt.output_document;

   if (opt.noclobber && exists(u->local))
   {
      /* If opt.noclobber is turned on and file already exists, do not
	 retrieve the file */
      if (opt.verbose)
	 fprintf(opt.lfile, "File `%s' already there, will not retrieve.\n",
		 u->local);
      /* If the file is there, we suppose it's retrieved OK. */
      *dt |= RETROKF;
      /* If its suffix is "html" or (yuck!) "htm", we suppose it's
	 text/html, a harmless lie. */
      if (((suf = suffix(u->local)) != NULL)
	  && (!strcmp(suf, "html") || !strcmp(suf, "htm")))
	 *dt |= TEXTHTML;
      free(suf);
      /* Another harmless lie: */
      return RETROK;
   }
   
   use_ts = 0;
   if (opt.timestamping)
   {
      if (stat(u->local, &st) == 0)
      {
	 use_ts = 1;
	 tml = st.st_mtime;
	 local_size = st.st_size;
	 got_head = 0;
      }
   }
   /* Reset the counter. */
   count = 0;
   *dt = 0 | ACCEPTRANGES;
   /* THE loop */
   do
   {
      /* Increment the pass counter. */
      ++count;
      /* Wait before the retrieval (unless this is the very first
	 retrieval). */
      if (!first_retrieval && opt.wait)
	 sleep(opt.wait);
      if (first_retrieval)
	 first_retrieval = 0;
      /* Get the current time string. */
      tms = time_str(NULL);
      /* Print fetch message, if opt.verbose. */
      if (opt.verbose)
      {
	 char *hurl = str_url(u->proxy ? u->proxy : u, 1);
	 char tmp[15];
	 strcpy(tmp, "        ");
	 if (count > 1)
	    sprintf(tmp, "(try:%2d)", count);
	 fprintf(opt.lfile,
		 "--%s--  %s\n  %s => `%s'\n",
		 tms, hurl, tmp, locf);
	 free(hurl);
      }

      /* Default document type is empty.  However, if spider mode is
	 on or time-stamping is employed, HEAD_ONLY commands is
	 encoded within *dt. */
      if (opt.spider || (use_ts && !got_head))
	 *dt |= HEAD_ONLY;
      else
	 *dt &= ~HEAD_ONLY;
      /* Assume no restarting. */
      hstat.restval = 0L;
      /* Decide whether or not to restart. */
      if (((count > 1 && (*dt & ACCEPTRANGES)) || opt.always_rest)
	  && exists(u->local))
	 if (stat(u->local, &st) == 0)
	    hstat.restval = st.st_size;
      /* Decide whether to send the no-cache directive. */
      if (u->proxy && (count > 1 || (opt.proxy_cache == 0)))
	 *dt |= SEND_NOCACHE;
      else
	 *dt &= ~SEND_NOCACHE;
      
      /* Try fetching the document, or at least its head. :-) */
      err = gethttp(u, &hstat, dt);
      /* Time? */
      tms = time_str(NULL);
      /* Get the new location (with or without the redirection).  */
      if (hstat.newloc)
	 *newloc = nstrdup(hstat.newloc);
      switch (err)
      {
	 case HERR: case HEOF: case CONSOCKERR: case CONCLOSED:
	 case CONERROR: case READERR: case WRITEFAILED:
	 case RANGEERR:
	    /* Non-fatal errors continue executing the loop, which
	       will bring them to "while" statement at the end, to
	       judge whether the number of tries was exceeded. */
	    FREEHSTAT(hstat);
	    printwhat(count, opt.ntry);
	    continue;
	    break;
	 case HOSTERR: case CONREFUSED: case PROXERR:
	    /* Fatal errors just return from the function. */
	    FREEHSTAT(hstat);
	    return err;
	    break;
	 case FWRITEERR: case FOPENERR:
	    /* Another fatal error. */
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "Cannot write to `%s' (%s).\n",
		       u->local, mystrerror(errno));
	    }
	    FREEHSTAT(hstat);
	    return err;
	    break;
	 case NEWLOCATION:
	    /* Return the new location to the caller. */
	    if (!hstat.newloc)
	    {
	       if (!opt.quiet)
		  fprintf(opt.lfile,
			  "ERROR: Redirection (%d) without location.\n",
			  hstat.statcode);
	       return WRONGCODE;
	    }
	    FREEHSTAT(hstat);
	    return NEWLOCATION;
	    break;
	 case RETRFINISHED:
	    /* Deal with you later. */
	    break;
	 default:
	    /* All possibilities should have been exhausted. */
	    assert(0);
      }
      if (!(*dt & RETROKF))
      {
	 if (!opt.quiet)
	 {
	    fprintf(opt.lfile, "%s ERROR %d: %s.\n", tms, hstat.statcode,
		    hstat.error);
	    if (opt.verbose)
	       fputc('\n', opt.lfile);
	 }
	 FREEHSTAT(hstat);
	 return WRONGCODE;
      }

      /* Did we get the time-stamp?  */
      if (!got_head)
      {
	 if (opt.timestamping && !hstat.remote_time)
	 {
	    if (!opt.quiet)
	       fprintf(opt.lfile, "Last-modified header missing -- time-stamps turned off.\n");
	 }
	 else if (hstat.remote_time)
	 {
	    /* Convert the date-string into struct tm. */
	    tmr = http_atotm(hstat.remote_time);
	    if (tmr == (time_t)(-1))
	       if (opt.verbose)
		  fprintf(opt.lfile, "Last-modified header invalid -- time-stamp ignored.\n");
	 }
      }

      /* The time-stamping section. */
      if (use_ts)
      {
	 got_head = 1;
	 *dt &= ~HEAD_ONLY;
	 use_ts = 0;            /* No more time-stamping. */
	 count = 0;             /* The retrieve count for HEAD is
				   reset. */
	 if (hstat.remote_time && tmr != (time_t)(-1))
	 {
	    /* Now time-stamping can be used validly.  Time -
	       stamping means that if the sizes of the local and
	       remote file match, and local file is newer than the
	       remote file, it will not be retrieved. Otherwise,
	       the normal download procedure is resumed. */
	    if (local_size == hstat.contlen && tml >= tmr)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "Local file `%s' is more recent, not retrieving.\n\n", u->local);
	       FREEHSTAT(hstat);
	       return RETROK;
	    }
	    else if (local_size != hstat.contlen)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "The sizes do not match (local %ld), retrieving.\n", local_size);
	    }
	    else
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "Remote file is newer, retrieving.\n");
	    }
	 }
	 FREEHSTAT(hstat);
	 continue;
      } /* use_ts */
      if (!opt.dfp
	  && (tmr != (time_t)(-1)) && !opt.spider &&
	  ((hstat.len == hstat.contlen) ||
	   ((hstat.res == 0) &&
	    ((hstat.contlen == -1) ||
	     (hstat.len >= hstat.contlen && !opt.kill_longer)))))
      {
	 my_touch(u->local, tmr);
      }
      /* End of time-stamping section. */

      if (opt.spider)
      {
	 fprintf(opt.lfile, "%d %s\n\n", hstat.statcode, hstat.error);
	 return RETROK;
      }

      /* It is now safe to free the remainder of hstat, since the
	 strings within it will no longer be used. */
      FREEHSTAT(hstat);

      tmrate = rate(hstat.len - hstat.restval, hstat.dltime);

      if (hstat.len == hstat.contlen)
      {
	 if (*dt & RETROKF)
	 {
	    if (opt.verbose)
	       fprintf(opt.lfile, "%s (%s) - `%s' saved [%ld/%ld]\n\n",
		       tms, tmrate, locf, hstat.len, hstat.contlen);
	    else if (!opt.quiet)
	       fprintf(opt.lfile, "%s URL:%s [%ld/%ld] -> \"%s\" [%d]\n",
		       tms, u->url, hstat.len, hstat.contlen, locf, count);
	 }
	 ++opt.numurls;
	 opt.downloaded += hstat.len;
	 return RETROK;
      }
      else if (hstat.res == 0) /* No read error */
      {
	 if (hstat.contlen == -1)  /* We don't know how much we were
				      supposed to get, so... */
	 {
	    if (*dt & RETROKF)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "%s (%s) - `%s' saved [%ld]\n\n",
			  tms, tmrate, locf, hstat.len);
	       else if (!opt.quiet)
		  fprintf(opt.lfile, "%s URL:%s [%ld] -> \"%s\" [%d]\n",
			  tms, u->url, hstat.len, locf, count);
	    }
	    ++opt.numurls;
	    opt.downloaded += hstat.len;
	    return RETROK;
	 }
	 else if (hstat.len < hstat.contlen) /* Meaning we lost the
						connection too soon */
	 {
	    if (opt.verbose)
	    {
	       fprintf(opt.lfile, "%s (%s) - Connection closed at byte %ld. ",
		       tms, tmrate, hstat.len);
	       printwhat(count, opt.ntry);
	    }
	    continue;
	 }
	 else if (!opt.kill_longer) /* Meaning we got more than expected */
	 {
	    if (opt.verbose)
	       fprintf(opt.lfile, "%s (%s) - `%s' saved [%ld/%ld])\n\n",
		       tms, tmrate, locf, hstat.len, hstat.contlen);
	    else if (!opt.quiet)
	       fprintf(opt.lfile, "%s URL:%s [%ld/%ld] -> \"%s\" [%d]\n",
		       tms, u->url, hstat.len, hstat.contlen, locf, count);
	    ++opt.numurls;
	    opt.downloaded += hstat.len;
	    return RETROK;
	 }
	 else /* The same, but not accepted */
	 {
	    if (opt.verbose)
	    {
	       fprintf(opt.lfile,
		       "%s (%s) - Connection closed at byte %ld/%ld. ",
		       tms, tmrate, hstat.len, hstat.contlen);
	       printwhat(count, opt.ntry);
	    }
	    continue;
	 }
      }
      else /* Now hstat.res can only be -1 */
      {
	 if (hstat.contlen == -1)
	 {
	    if (opt.verbose)
	    {
	       fprintf(opt.lfile,
		       "%s (%s) - Read error at byte %ld (%s).",
		       tms, tmrate, hstat.len, mystrerror(errno));
	       printwhat(count, opt.ntry);
	    }
	    continue;
	 }
	 else /* hstat.res == -1 and contlen is given */
	 {
	    if (opt.verbose)
	    {
	       fprintf(opt.lfile,
		       "%s (%s) - Read error at byte %ld/%ld (%s). ",
		       tms, tmrate, hstat.len, hstat.contlen,
		       mystrerror(errno));
	       printwhat(count, opt.ntry);
	    }
	    continue;
	 }
      }
      /* not reached */
      break;
   } while (!opt.ntry || (count < opt.ntry));
   return TRYLIMEXC;
}

/* Encode a zero-terminated string in base64.  Returns the malloc-ed
   encoded line.  This is useful for HTTP only.

   Note that the string may not contain NUL characters.  */
char *
base64_encode_line(const char *s)
{
   /* Conversion table. */
   static char tbl[64] = {
      'A','B','C','D','E','F','G','H',
      'I','J','K','L','M','N','O','P',
      'Q','R','S','T','U','V','W','X',
      'Y','Z','a','b','c','d','e','f',
      'g','h','i','j','k','l','m','n',
      'o','p','q','r','s','t','u','v',
      'w','x','y','z','0','1','2','3',
      '4','5','6','7','8','9','+','/'
   };
   int len, i;
   char *res;
   unsigned char *p;

   len = strlen(s);
   res = (char *)nmalloc(4 * ((len + 2) / 3) + 1);
   p = (unsigned char *)res;
   /* Transform the 3x8 bits to 4x6 bits, as required by
      base64.  */
   for (i = 0; i < len; i += 3)
   {
      *p++ = tbl[s[0] >> 2];
      *p++ = tbl[((s[0] & 3) << 4) + (s[1] >> 4)];
      *p++ = tbl[((s[1] & 0xf) << 2) + (s[2] >> 6)];
      *p++ = tbl[s[2] & 0x3f];
      s += 3;
   }
   /* Pad the result if necessary... */
   if (i == len + 1)
      *(p - 1) = '=';
   else if (i == len + 2)
      *(p - 1) = *(p - 2) = '=';
   /* ...and zero-teminate it.  */
   *p = '\0';
   return res;
}

/* Converts struct tm to time_t, assuming the data in tm is UTC rather
   than local timezone (as mktime assumes).
   
   Contributed by Roger Beeman <beeman@cisco.com>.  */
time_t
mktime_from_utc(struct tm *t)
{
   time_t tl, tb;

   tl = mktime(t);
   tb = mktime(gmtime(&tl));                               
   return (tl <= tb ? (tl + (tl - tb)) : (tl - (tb - tl)));
}

/* Check whether the string is processed well.  It is processed if the
   pointer is non-NULL, and it is either at the `GMT', or at the end
   of the string.  */
static int
check_end(char *p)
{
   if (!p)
      return 0;
   while (isspace(*p))
      ++p;
   if (!*p || (p[0] == 'G' && p[1] == 'M' && p[2] == 'T'))
      return 1;
   else
      return 0;
}

/* Converts ASCII time to time_t.  The time can be in three formats
   allowed for HTTP servers to send, as per RFC2068 -- RFC1123-date,
   RFC850-date or asctime-date.

   strptime() is used to recognize various dates, which makes it a
   little bit slacker than the RFC1123/RFC850/asctime (e.g. it always
   allows shortened dates and months, one-digit days, etc.).  It also
   allows more than one space anywhere where the specs require one SP.
   The routine should probably be even slacker (RFC2068 recommends
   this), but I do not have the time to write one.

   Returns the computed time_t representation, or -1 if all the
   schemes fail.  */
time_t
http_atotm(char *s)
{
   struct tm t;

   t.tm_isdst = -1;

   /* NOTE: We don't use `%n' for white space, as OSF's strptime uses
      it to eat all white space up to (and including) a newline, and
      the function fails (!) if there is no newline.

      Let's hope all strptime-s use ` ' to skipp *all* whitespace
      instead of just one (it works that way on all the systems I've
      tested it on). */
   
   /* Let's try RFC1123 date. */
   if (check_end(strptime(s, "%a, %d %b %Y %T", &t)))
      return mktime_from_utc(&t);
   /* RFC850 date. */
   if (check_end(strptime(s, "%a, %d-%b-%y %T", &t)))
      return mktime_from_utc(&t);
   /* asctime date. */
   if (check_end(strptime(s, "%a %b %d %T %Y", &t)))
      return mktime_from_utc(&t);
   /* Failure. */
   return -1;
}

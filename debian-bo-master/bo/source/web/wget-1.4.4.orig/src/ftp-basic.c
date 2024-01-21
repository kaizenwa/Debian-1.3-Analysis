/* Basic FTP routines.
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


/* $Id: ftp-basic.c,v 1.1.1.1.2.1 1997/02/15 19:22:49 hniksic Exp $ */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#include <sys/types.h>

#ifdef WINDOWS
#  include <winsock.h>
#endif

#include "wget.h"
#include "options.h"
#include "utils.h"
#include "ftp-basic.h"
#include "connect.h"
#include "host.h"
#include "retr.h"

extern struct options opt;

#ifndef errno
extern int errno;
#endif
#ifndef h_errno
extern int h_errno;
#endif

char ftp_last_respline[128];

/* Get the response of FTP server and allocate enough room to handle
   it.  <CR> and <LF> characters are stripped from the line, and the
   line is 0-terminated.  All the response lines but the last one are
   skipped. The last line is determined as described in RFC959. */
uerr_t
ftp_response(int fd, char **line)
{
   int i, bufsize, res;
   
   bufsize = DYNAMIC_LINE_BUFFER;
   *line = nmalloc(bufsize);
   do
   {
      for (i = 0; 1; i++)
      {
	 if (i > bufsize - 1)
	    *line = (char *)nrealloc(*line, (bufsize <<= 1));
	 res = buf_readchar(fd, *line + i);
	 /* res is number of bytes read. */
	 if (res == 1)
	 {
	    if ((*line)[i] == '\n')
	    {
	       (*line)[i] = '\0';
	       /* Get rid of \r. */
	       if (i > 0 && (*line)[i - 1] == '\r')
		  (*line)[i - 1] = '\0';
	       break;
	    }
	 }
	 else
	    return FTPRERR;
      }
#ifdef DEBUG
      if (opt.debug && !opt.server_response)
	 fprintf(opt.lfile, "%s\n", *line);
#endif
      if (opt.server_response)
	 fprintf(opt.lfile, "%s\n", *line);
   } while (!(i >= 3 && isdigit(**line) && isdigit((*line)[1]) &&
	      isdigit((*line)[2]) && (*line)[3] == ' '));
   strncpy(ftp_last_respline, *line, 128);
   ftp_last_respline[127] = '\0';
   return FTPOK;
}

/* Returns the malloc-ed FTP request, ending with <CR><LF>, printing
   it if printing is required.  If VALUE is NULL, just use
   command<CR><LF>.  */
char *
ftp_request(const char *command, const char *value)
{
   char *res;

   res = (char *)nmalloc(strlen(command)
			 + (value ? (1 + strlen(value)) : 0) + 2 + 1);
   sprintf(res, "%s%s%s\r\n", command, value ? " " : "", value ? value : "");
#ifdef DEBUG
   if (opt.debug && !opt.server_response)
      fprintf(opt.lfile, "\n--> %s\n", res);
#endif
   if (opt.server_response)
   {
      /* We don't want to print out passwords: */
      if (strncmp(res, "PASS", 4) != 0)
	 fprintf(opt.lfile, "--> %s\n", res);
      else
	 fprintf(opt.lfile, "--> PASS Turtle Power!\n");
   }
   return res;
}

/* Sends the USER and PASS commands to the server, to control
   connection socket csock. */
uerr_t
ftp_login(int csock, const char *acc, const char *pass)
{
   uerr_t err;
   char *request, *respline;
   int nwritten;

   /* Get greeting. */
   err = ftp_response(csock, &respline);
   if (err != FTPOK)
   {
      free(respline);
      return err;
   }
   if (*respline != '2')
   {
      free(respline);
      return FTPSRVERR;
   }
   free(respline);
   /* Send USER username. */
   request = ftp_request("USER", acc);
   nwritten = iwrite(csock, request, strlen(request));
   if (nwritten != strlen(request))
   {
      free(request);
      return WRITEFAILED;
   }
   free(request);
   /* Get appropriate response. */
   err = ftp_response(csock, &respline);
   if (err != FTPOK)
   {
      free(respline);
      return err;
   }
   /* An unprobable possibility of logging without a password. */
   if (*respline == '2')
   {
      free(respline);
      return FTPOK;
   }
   /* Else, only response 3 is appropriate. */
   if (*respline != '3')
   {
      free(respline);
      return FTPLOGREFUSED;
   }
   free(respline);
   /* Send PASS password. */
   request = ftp_request("PASS", pass);
   nwritten = iwrite(csock, request, strlen(request));
   if (nwritten != strlen(request))
   {
      free(request);
      return WRITEFAILED;
   }
   free(request);
   /* Get appropriate response. */
   err = ftp_response(csock, &respline);
   if (err != FTPOK)
   {
      free(respline);
      return err;
   }
   if (*respline != '2')
   {
      free(respline);
      return FTPLOGINC;
   }
   free(respline);
   /* All OK. */
   return FTPOK;
}

/* Binds a port, and sends the appropriate PORT command to the FTP
   server. Use acceptport after RETR, to get the socket of data
   connection. */
uerr_t
ftp_port(int csock)
{
   uerr_t err;
   char *request, *respline, *bytes;
   unsigned char *in_addr;
   int nwritten;
   unsigned short port;

   /* Setting port to 0 lets the system choose a free port. */
   port = 0;
   /* Bind the port. */
   err = bindport(&port);
   if (err != BINDOK)
      return err;
   /* Get the address of this side of the connection. */
   if (!(in_addr = conaddr(csock)))
      return HOSTERR;
   /* Construct the argument of PORT (of the form a,b,c,d,e,f) */
   bytes = (char *)nmalloc(6 * 4 + 1);
   sprintf(bytes, "%d,%d,%d,%d,%d,%d", in_addr[0], in_addr[1],
	   in_addr[2], in_addr[3], (unsigned)(port & 0xff00) >> 8,
	   port & 0xff);
   /* Send PORT request. */
   request = ftp_request("PORT", bytes);
   free(bytes);
   nwritten = iwrite(csock, request, strlen(request));
   if (nwritten != strlen(request))
   {
      free(request);
      return WRITEFAILED;
   }
   free(request);
   /* Get appropriate response. */
   err = ftp_response(csock, &respline);
   if (err != FTPOK)
   {
      free(respline);
      return err;
   }
   if (*respline != '2')
   {
      free(respline);
      return FTPPORTERR;
   }
   free(respline);
   return FTPOK;
}

/* Similar to ftp_port, but uses `PASV' to initiate the passive FTP
   transfer.  Reads the response from server and parses it.  Reads the
   host and port addresses and returns them. */
uerr_t
ftp_pasv(int csock, unsigned char *addr)
{
   char *request, *respline;
   unsigned char *s;
   int nwritten, i;
   uerr_t err;
   
   /* Form the request. */
   request = ftp_request("PASV", NULL);
   /* And send it. */
   nwritten = iwrite(csock, request, strlen(request));
   if (nwritten != strlen(request))
   {
      free(request);
      return WRITEFAILED;
   }
   free(request);
   /* Get the server response. */
   err = ftp_response(csock, &respline);
   if (err != FTPOK)
   {
      free(respline);
      return err;
   }
   if (*respline != '2')
   {
      free(respline);
      return FTPNOPASV;
   }
   /* Parse the request. */
   s = (unsigned char *)respline;
   for (s += 4; *s && !isdigit(*s); s++);
   if (!*s)
      return FTPINVPASV;
   for (i = 0; i < 6; i++)
   {
      addr[i] = 0;
      for (; isdigit(*s); s++)
	 addr[i] = (*s - '0') + 10 * addr[i];
      if (*s == ',')
	 s++;
      else if (i < 5)
      {
	 /* When on the last number, anything can be a terminator. */
	 free(respline);
	 return FTPINVPASV;
      }
   }
   free(respline);
   return FTPOK;
}

/* Sends the TYPE request to the server. */
uerr_t
ftp_type(int csock, int type)
{
   char *request, *respline;
   int nwritten;
   uerr_t err;
   char stype[2];

   /* Construct argument. */
   stype[0] = type;
   stype[1] = 0;
   /* Send TYPE request. */
   request = ftp_request("TYPE", stype);
   nwritten = iwrite(csock, request, strlen(request));
   if (nwritten != strlen(request))
   {
      free(request);
      return WRITEFAILED;
   }
   free(request);
   /* Get appropriate response. */
   err = ftp_response(csock, &respline);
   if (err != FTPOK)
   {
      free(respline);
      return err;
   }
   if (*respline != '2')
   {
      free(respline);
      return FTPUNKNOWNTYPE;
   }
   free(respline);
   /* All OK. */
   return FTPOK;
}

/* Changes the working directory by issuing a CWD command to the
   server. */
uerr_t
ftp_cwd(int csock, const char *dir)
{
   char *request, *respline;
   int nwritten;
   uerr_t err;

   /* Send CWD request. */
   request = ftp_request("CWD", dir);
   nwritten = iwrite(csock, request, strlen(request));
   if (nwritten != strlen(request))
   {
      free(request);
      return WRITEFAILED;
   }
   free(request);
   /* Get appropriate response. */
   err = ftp_response(csock, &respline);
   if (err != FTPOK)
   {
      free(respline);
      return err;
   }
   if (*respline == '5')
   {
      free(respline);
      return FTPNSFOD;
   }
   if (*respline != '2')
   {
      free(respline);
      return FTPRERR;
   }
   free(respline);
   /* All OK. */
   return FTPOK;
}

/* Sends REST command to the FTP server. */
uerr_t
ftp_rest(int csock, long offset)
{
   char *request, *respline;
   int nwritten;
   uerr_t err;
   static char buf[20]; /* Buffer for the number */

   sprintf(buf, "%ld", offset);
   request = ftp_request("REST", buf);
   nwritten = iwrite(csock, request, strlen(request));
   if (nwritten != strlen(request))
   {
      free(request);
      return WRITEFAILED;
   }
   free(request);
   /* Get appropriate response. */
   err = ftp_response(csock, &respline);
   if (err != FTPOK)
   {
      free(respline);
      return err;
   }
   if (*respline != '3')
   {
      free(respline);
      return FTPRESTFAIL;
   }
   free(respline);
   /* All OK. */
   return FTPOK;
}

/* Sends RETR command to the FTP server. */
uerr_t
ftp_retr(int csock, const char *file)
{
   char *request, *respline;
   int nwritten;
   uerr_t err;

   /* Send RETR request. */
   request = ftp_request("RETR", file);
   nwritten = iwrite(csock, request, strlen(request));
   if (nwritten != strlen(request))
   {
      free(request);
      return WRITEFAILED;
   }
   free(request);
   /* Get appropriate response. */
   err = ftp_response(csock, &respline);
   if (err != FTPOK)
   {
      free(respline);
      return err;
   }
   if (*respline == '5')
   {
      free(respline);
      return FTPNSFOD;
   }
   if (*respline != '1')
   {
      free(respline);
      return FTPRERR;
   }
   free(respline);
   /* All OK. */
   return FTPOK;
}

/* Sends the LIST command to the server.  If FILE is NULL, send just
   `LIST' (no space).  */
uerr_t
ftp_list(int csock, const char *file)
{
   char *request, *respline;
   int nwritten;
   uerr_t err;

   /* Send LIST request. */
   request = ftp_request("LIST", file);
   nwritten = iwrite(csock, request, strlen(request));
   if (nwritten != strlen(request))
   {
      free(request);
      return WRITEFAILED;
   }
   free(request);
   /* Get appropriate respone. */
   err = ftp_response(csock, &respline);
   if (err != FTPOK)
   {
      free(respline);
      return err;
   }
   if (*respline == '5')
   {
      free(respline);
      return FTPNSFOD;
   }
   if (*respline != '1')
   {
      free(respline);
      return FTPRERR;
   }
   free(respline);
   /* All OK. */
   return FTPOK;
}

/* Look for regexp "( *[0-9]+ *byte" (literal parenthesis) anywhere in
   the string, and return the number converted to long, if found, 0
   otherwise.  */
long
ftp_expected_bytes(const char *s)
{
   long res;

   do
   {
      while (*s && *s != '(')
	 ++s;
      if (!*s)
	 return 0;
      for (++s; *s && isspace(*s); s++);
      if (!*s)
	 return 0;
      if (!isdigit(*s))
	 continue;
      res = 0;
      do
      {
	 res = (*s - '0') + 10 * res;
	 ++s;
      } while (*s && isdigit(*s));
      if (!*s)
	 return 0;
      while (*s && isspace(*s))
	 ++s;
      if (!*s)
	 return 0;
      if (tolower(*s) != 'b')
	 continue;
      if (strncasecmp(s, "byte", 4))
	 continue;
      else
	 break;
   } while (1);
   return res;
}

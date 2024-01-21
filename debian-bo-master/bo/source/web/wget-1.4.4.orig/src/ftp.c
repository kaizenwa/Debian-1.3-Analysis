/* File Transfer Protocol support.
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


/* $Id: ftp.c,v 1.1.1.1.2.2 1997/02/15 19:22:53 hniksic Exp $ */

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
# include <winsock.h>
#else
# include <netinet/in.h>
#endif /* WINDOWS */


#include <assert.h>

#include "wget.h"
#include "options.h"
#include "utils.h"
#include "url.h"
#include "ftp.h"
#include "ftp-basic.h"
#include "html.h"
#include "connect.h"
#include "host.h"
#include "http.h"
#include "mtch.h"
#include "retr.h"
#include "netrc.h"

extern struct options opt;
extern char ftp_last_respline[128];

#ifndef errno
extern int errno;
#endif
#ifndef h_errno
extern int h_errno;
#endif

/* Retrieves a file with denoted parameters through opening an FTP
   connection to the server.  It always closes the data connection,
   and closes the control connection in case of error.  */
uerr_t
getftp(const urlinfo *u, long *len, long restval, ccon *con)
{
   int csock, dtsock, res;
   uerr_t err;
   FILE *fp;
   char *user, *passwd, *respline;
   char *tms, *tmrate;
   unsigned char pasv_addr[6];
   int cmd = con->cmd;
   int passive_mode_open= 0;
   long expected_bytes = 0L;

   assert(con != NULL);
   assert(u->local != NULL);
   /* Debug-check of the sanity of the request:
      Make sure that not both LIST and RETR are requested (since we
      can handle only one at a time. */
   assert(!((cmd & DO_LIST) && (cmd & DO_RETR)));
   /* Make sure that at least *something* is requested. */
   assert((cmd & (DO_LIST | DO_CWD | DO_RETR | DO_LOGIN)) != 0);

   user = u->user;
   passwd = u->passwd;
   search_netrc(u->host, (const char **)&user, (const char **)&passwd, 1);
   user = user ? user : opt.ftp_acc;
   passwd = passwd ? passwd : opt.ftp_pass;
   assert(user && passwd);

   dtsock = -1;
   con->dltime = 0;

   if (!(cmd & DO_LOGIN))
      csock = con->fd;
   else                         /* cmd & DO_LOGIN */
   {
      /* Login to the server: */
      
      /* First: Establish the control connection. */
      if (opt.verbose)
	 fprintf(opt.lfile, "Connecting to %s:%hu... ", u->host, u->port);
      err = make_connection(&csock, u->host, u->port);
      if (cmd & LEAVE_PENDING)
	 con->fd = csock;
      else
	 con->fd = -1;
      switch (err)
      {
	 /* Do not close the socket in first several cases,
	    since it wasn't created at all. */
	 case HOSTERR:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "%s: %s\n", u->host, herrmsg(h_errno));
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
	       fprintf(opt.lfile, "Connection to %s:%hu refused.\n", u->host,
		       u->port);
	    }
	    CLOSE(csock);
	    con->fd = -1;
	    return CONREFUSED;
	 case CONERROR:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "connect: %s\n", mystrerror(errno));
	    }
	    CLOSE(csock);
	    con->fd = -1;
	    return CONERROR;
	    break;
      }
      /* Since this is a new connection, we may safely discard
	 anything left in the buffer. */
      buf_discard();
      
      /* Second: Login with proper USER/PASS sequence. */
      if (opt.verbose)
      {
	 fprintf(opt.lfile, "connected!\n");
	 fprintf(opt.lfile, "Logging in as %s ... ", user);
	 if (opt.server_response)
	    fputc('\n', opt.lfile);
      }
      err = ftp_login(csock, user, passwd);
      /* FTPRERR, FTPSRVERR, WRITEFAILED, FTPLOGREFUSED, FTPLOGINC. */
      switch (err)
      {
	 case FTPRERR:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "Error in server response, closing control connection.\n");
	    }
	    CLOSE(csock);
	    con->fd = -1;
	    return err;
	    break;
	 case FTPSRVERR:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "Error in server greeting.\n");
	    }
	    CLOSE(csock);
	    con->fd = -1;
	    return err;
	 case WRITEFAILED:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile,
		       "Write failed, closing control connection.\n");
	    }
	    CLOSE(csock);
	    con->fd = -1;
	    return err;
	    break;
	 case FTPLOGREFUSED:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "The server refuses login.\n");
	    }
	    CLOSE(csock);
	    con->fd = -1;
	    return FTPLOGREFUSED;
	    break;
	 case FTPLOGINC:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "Login incorrect.\n");
	    }
	    CLOSE(csock);
	    con->fd = -1;
	    return FTPLOGINC;
	    break;
	 case FTPOK:
	    if (opt.verbose && !opt.server_response)
	       fprintf(opt.lfile, "Logged in!\n");
	    break;
	 default:
	    assert(0);
	    exit(1);
	    break;
      }
      /* Third: Set type to Image (binary). */
      if (opt.verbose && !opt.server_response)
	 fprintf(opt.lfile, "==> TYPE I ... ");
      err = ftp_type(csock, 'I');
      /* FTPRERR, WRITEFAILED, FTPUNKNOWNTYPE. */
      switch (err)
      {
	 case FTPRERR:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "Error in server response, closing control connection.\n");
	    }
	    CLOSE(csock);
	    con->fd = -1;
	    return err;
	    break;
	 case WRITEFAILED:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile,
		       "Write failed, closing control connection.\n");
	    }
	    CLOSE(csock);
	    con->fd = -1;
	    return err;
	    break;
	 case FTPUNKNOWNTYPE:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile,
		       "Unknown type(?), closing control connection.\n");
	    }
	    CLOSE(csock);
	    con->fd = -1;
	    return err;
	 case FTPOK:
	    /* Everything is OK. */
	    break;
	 default:
	    assert(0);
	    break;
      }
      if (opt.verbose && !opt.server_response)
	 fprintf(opt.lfile, "done.  ");
   } /* do login */
   
   if (cmd & DO_CWD)
   {
      if (!*u->dir)
      {
	 if (opt.verbose)
	    fprintf(opt.lfile, "==> CWD not needed.\n");
      }
      else
      {
	 /* Change working directory. */
	 if (opt.verbose && !opt.server_response)
	    fprintf(opt.lfile, "==> CWD %s ... ", u->dir);
	 err = ftp_cwd(csock, u->dir);
	 /* FTPRERR, WRITEFAILED, FTPNSFOD */
	 switch (err)
	 {
	    case FTPRERR:
	       if (!opt.quiet)
	       {
		  if (opt.verbose)
		     fprintf(opt.lfile, "\n");
		  fprintf(opt.lfile,
			  "Error in server response, closing control connection.\n");
	       }
	       CLOSE(csock);
	       con->fd = -1;
	       return err;
	       break;
	    case WRITEFAILED:
	       if (!opt.quiet)
	       {
		  if (opt.verbose)
		     fprintf(opt.lfile, "\n");
		  fprintf(opt.lfile,
			  "Write failed, closing control connection.\n");
	       }
	       CLOSE(csock);
	       con->fd = -1;
	       return err;
	       break;
	    case FTPNSFOD:
	       if (!opt.quiet)
	       {
		  if (opt.verbose)
		     fprintf(opt.lfile, "\n");
		  fprintf(opt.lfile, "No such directory `%s'.\n\n", u->dir);
	       }
	       CLOSE(csock);
	       con->fd = -1;
	       return err;
	       break;
	    case FTPOK:
	       /* fine and dandy */
	       break;
	    default:
	       assert(0);
	       break;
	 }
	 if (opt.verbose && !opt.server_response)
	    fprintf(opt.lfile, "done.\n");
      }
   }
   else /* do not CWD */
   {
      if (opt.verbose)
	 fprintf(opt.lfile, "==> CWD not required.\n");
   }
   
   /* If anything is to be retrieved, PORT (or PASV) must be sent. */
   if (cmd & (DO_LIST | DO_RETR))
   {
      if (opt.ftp_pasv)
      {
	 char thost[256];
	 unsigned short tport;

	 if (opt.verbose && !opt.server_response)
	    fprintf(opt.lfile, "==> PASV ... ");
	 err = ftp_pasv(csock, pasv_addr);
	 /* FTPRERR, WRITEFAILED, FTPNOPASV, FTPINVPASV */
	 switch(err)
	 {
	    case FTPRERR:
	       if (!opt.quiet)
	       {
		  if (opt.verbose)
		     fprintf(opt.lfile, "\n");
		  fprintf(opt.lfile, "Error in server response, closing control connection.\n");
	       }
	       CLOSE(csock);
	       con->fd = -1;
	       return err;
	       break;
	    case WRITEFAILED:
	       if (!opt.quiet)
	       {
		  if (opt.verbose)
		     fprintf(opt.lfile, "\n");
		  fprintf(opt.lfile,
			  "Write failed, closing control connection.\n");
	       }
	       CLOSE(csock);
	       con->fd = -1;
	       return err;
	       break;
	    case FTPNOPASV:
	       if (!opt.quiet)
	       {
		  if (opt.verbose)
		     fprintf(opt.lfile, "\n");
		  fprintf(opt.lfile, "Cannot initiate PASV transfer.\n");
	       }
	       break;
	    case FTPINVPASV:
	       if (!opt.quiet)
	       {
		  if (opt.verbose)
		     fprintf(opt.lfile, "\n");
		  fprintf(opt.lfile, "Cannot parse PASV response.\n");
	       }
	       break;
	    case FTPOK:
	       /* fine and dandy */
	       break;
	    default:
	       assert(0);
	       break;
	 }
	 if (err==FTPOK)
	 {
	    sprintf(thost, "%d.%d.%d.%d",
		    pasv_addr[0], pasv_addr[1], pasv_addr[2], pasv_addr[3]);
	    tport = (pasv_addr[4] << 8) + pasv_addr[5];
#ifdef DEBUG
	    if (opt.debug)
		fprintf(opt.lfile, "Will try connecting to %s:%hu.\n", thost,
			tport);
#endif
	    err = make_connection(&dtsock, thost, tport);
	    switch (err)
		{
		   /* Do not close the socket in first several cases,
		      since it wasn't created at all. */
		 case HOSTERR:
	       if (!opt.quiet)
		   {
		      if (opt.verbose)
			  fprintf(opt.lfile, "\n");
		      fprintf(opt.lfile, "%s: %s\n", thost, herrmsg(h_errno));
		   }
	       CLOSE(csock);
	       con->fd = -1;
	       return HOSTERR;
	       break;
	     case CONSOCKERR:
	       if (!opt.quiet)
		   {
		      if (opt.verbose)
			  fprintf(opt.lfile, "\n");
		      fprintf(opt.lfile, "socket: %s\n", mystrerror(errno));
		   }
	       CLOSE(csock);
	       con->fd = -1;
	       return CONSOCKERR;
	       break;
	     case CONREFUSED:
	       if (!opt.quiet)
		   {
		      if (opt.verbose)
			  fprintf(opt.lfile, "\n");
		      fprintf(opt.lfile, "Connection to %s:%hu refused.\n",
			      thost, tport);
		   }
	       CLOSE(csock);
	       con->fd = -1;
	       closeport(dtsock);
	       return CONREFUSED;
	     case CONERROR:
	       if (!opt.quiet)
		   {
		      if (opt.verbose)
			  fprintf(opt.lfile, "\n");
		      fprintf(opt.lfile, "connect: %s\n", mystrerror(errno));
		   }
	       CLOSE(csock);
	       con->fd = -1;
	       closeport(dtsock);
	       return CONERROR;
	       break;
	    }
	    passive_mode_open= 1;  /* Flag to avoid accept port */
	    if (opt.verbose && !opt.server_response)
		fprintf(opt.lfile, "done.    ");
	 } /* err==FTP_OK */
      }

      if (!passive_mode_open)   /* Try to use a port command if PASV failed */
      {
	 if (opt.verbose && !opt.server_response)
	     fprintf(opt.lfile, "==> PORT ... ");
	 err = ftp_port(csock);
	 /* FTPRERR, WRITEFAILED, bindport (CONSOCKERR, CONPORTERR, BINDERR,
	    LISTENERR), HOSTERR, FTPPORTERR */
	 switch(err)
	     {
	      case FTPRERR:
		if (!opt.quiet)
		    {
		       if (opt.verbose)
			   fprintf(opt.lfile, "\n");
		       fprintf(opt.lfile, "Error in server response, closing control connection.\n");
		    }
		CLOSE(csock);
		closeport(dtsock);
		con->fd = -1;
		return err;
		break;
	      case WRITEFAILED:
		if (!opt.quiet)
		    {
		       if (opt.verbose)
			   fprintf(opt.lfile, "\n");
		       fprintf(opt.lfile,
			       "Write failed, closing control connection.\n");
		    }
		CLOSE(csock);
		closeport(dtsock);
		con->fd = -1;
		return err;
		break;
	      case CONSOCKERR:
		if (!opt.quiet)
		    {
		       if (opt.verbose)
			   fprintf(opt.lfile, "\n");
		       fprintf(opt.lfile, "socket: %s\n", mystrerror(errno));
		    }
		CLOSE(csock);
		closeport(dtsock);
		con->fd = -1;
		return err;
		break;
	      case CONPORTERR: case BINDERR: case LISTENERR:
		/* What now? These are local troubles... */
		if (!opt.quiet)
		    {
		       if (opt.verbose)
			   fprintf(opt.lfile, "\n");
		       fprintf(opt.lfile, "Bind error (%s).\n", mystrerror(errno));
		    }
		closeport(dtsock);
		return err;
		break;
	      case HOSTERR:
		if (!opt.quiet)
		    {
		       if (opt.verbose)
			   fprintf(opt.lfile, "\n");
		       fprintf(opt.lfile, "%s: %s\n", u->host, herrmsg(h_errno));
		    }
		CLOSE(csock);
		closeport(dtsock);
		con->fd = -1;
		return HOSTERR;
		break;
	      case FTPPORTERR:
		if (!opt.quiet)
		    {
		       if (opt.verbose)
			   fprintf(opt.lfile, "\n");
		       fprintf(opt.lfile, "Invalid PORT.\n");
		    }
		CLOSE(csock);
		closeport(dtsock);
		con->fd = -1;
		return err;
		break;
	      case FTPOK:
		/* fine and dandy */
		break;
	      default:
		assert(0);
		break;
	     } /* port switch */
	 if (opt.verbose && !opt.server_response)
	     fprintf(opt.lfile, "done.    ");
      } /* dtsock==-1 */
   } /* cmd & (DO_LIST | DO_RETR) */
   
   /* Restart if needed. */
   if (restval && (cmd & DO_RETR))
   {
      if (opt.verbose && !opt.server_response)
	 fprintf(opt.lfile, "==> REST %ld ... ", restval);
      err = ftp_rest(csock, restval);
      
      /* FTPRERR, WRITEFAILED, FTPRESTFAIL. */
      switch (err)
      {
	 case FTPRERR:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "Error in server response, closing control connection.\n");
	    }
	    CLOSE(csock);
	    closeport(dtsock);
	    con->fd = -1;
	    return err;
	    break;
	 case WRITEFAILED:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile,
		       "Write failed, closing control connection.\n");
	    }
	    CLOSE(csock);
	    closeport(dtsock);
	    con->fd = -1;
	    return err;
	    break;
	 case FTPRESTFAIL:
	    if (opt.verbose)
	       fprintf(opt.lfile, "\nREST failed, starting from scratch.\n");
	    restval = 0L;
	    break;
	 case FTPOK:
	    /* fine and dandy */
	    break;
	 default:
	    assert(0);
	    break;
      }
      if (opt.verbose)
      {
	 if (err != FTPRESTFAIL && !opt.server_response)
	    fprintf(opt.lfile, "done.    ");
      }
   } /* restval && cmd & DO_RETR */
   
   if (cmd & DO_RETR)
   {
      if (opt.verbose)
      {
	 if (!opt.server_response)
	 {
	    if (restval)
	       fprintf(opt.lfile, "\n");
	    fprintf(opt.lfile, "==> RETR %s ... ", u->file);
	 }
      }
      err = ftp_retr(csock, u->file);
      /* FTPRERR, WRITEFAILED, FTPNSFOD */
      switch (err)
      {
	 case FTPRERR:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "Error in server response, closing control connection.\n");
	    }
	    CLOSE(csock);
	    closeport(dtsock);
	    con->fd = -1;
	    return err;
	    break;
	 case WRITEFAILED:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile,
		       "Write failed, closing control connection.\n");
	    }
	    CLOSE(csock);
	    closeport(dtsock);
	    con->fd = -1;
	    return err;
	    break;
	 case FTPNSFOD:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "No such file `%s'.\n\n", u->file);
	    }
	    closeport(dtsock);
	    return err;
	    break;
	 case FTPOK:
	    /* fine and dandy */
	    break;
	 default:
	    assert(0);
	    break;
      }
      
      if (opt.verbose && !opt.server_response)
	 fprintf(opt.lfile, "done.\n");
      expected_bytes = ftp_expected_bytes(ftp_last_respline);
   } /* do retrieve */

   if (cmd & DO_LIST)
   {
      if (opt.verbose && !opt.server_response)
	 fprintf(opt.lfile, "==> LIST     ");
      /* As Maciej W. Rozycki (macro@ds2.pg.gda.pl) says, `LIST'
	 without arguments is better than `LIST .'; confirmed by
	 RFC959.  */
      err = ftp_list(csock, NULL);
      /* FTPRERR, WRITEFAILED */
      switch (err)
      {
	 case FTPRERR:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "Error in server response, closing control connection.\n");
	    }
	    CLOSE(csock);
	    closeport(dtsock);
	    con->fd = -1;
	    return err;
	    break;
	 case WRITEFAILED:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile,
		       "Write failed, closing control connection.\n");
	    }
	    CLOSE(csock);
	    closeport(dtsock);
	    con->fd = -1;
	    return err;
	    break;
	 case FTPNSFOD:
	    if (!opt.quiet)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "\n");
	       fprintf(opt.lfile, "No such file or directory `%s'.\n\n", ".");
	    }
	    closeport(dtsock);
	    return err;
	    break;
	 case FTPOK:
	    /* fine and dandy */
	    break;
	 default:
	    assert(0);
	    break;
      }
      if (opt.verbose && !opt.server_response)
	 fprintf(opt.lfile, "done.\n");
      expected_bytes = ftp_expected_bytes(ftp_last_respline);
   } /* cmd & DO_LIST */

   /* If no transmission was required, then everything is OK. */
   if (!(cmd & (DO_LIST | DO_RETR)))
      return RETRFINISHED;

   if (!passive_mode_open)  /* We are not using pasive mode so we need to accept */
   {
      /* Open the data transmission socket by calling acceptport. */
      err = acceptport(&dtsock);
      /* Possible errors: ACCEPTERR. */
      if (err == ACCEPTERR)
      {
	 if (!opt.quiet)
	    fprintf(opt.lfile, "accept: %s\n", mystrerror(errno));
	 return err;
      }
   }
   
   /* Open the file -- if opt.dfp is set, use it instead. */
   if (!opt.dfp || con->cmd & DO_LIST)
   {
      mkalldirs(u->local);
      fp = fopen(u->local, restval ? "ab" : "wb");
      if (!fp)
      {
	 if (!opt.quiet)
	    fprintf(opt.lfile, "%s: %s\n", u->local, mystrerror(errno));
	 CLOSE(csock);
	 con->fd = -1;
	 closeport(dtsock);
	 return FOPENERR;
      }
   }
   else
      fp = opt.dfp;

   if (opt.verbose)
   {
      if (*len)
      {
	 fprintf(opt.lfile, "Length: %s", legible(*len));
	 if (restval)
	    fprintf(opt.lfile, " [%s to go]", legible(*len - restval));
	 putc('\n', opt.lfile);
      }
      else if (expected_bytes)
      {
	 fprintf(opt.lfile, "Length: %s", legible(expected_bytes));
	 if (restval)
	    fprintf(opt.lfile, " [%s to go]", legible(expected_bytes - restval));
	 fprintf(opt.lfile, " (unauthoritative)\n");
      }
   } /* opt.verbose */
   reset_timer();
   /* Get the contents of the document. */
   res = get_contents(dtsock, fp, len, restval, 1);
   con->dltime = elapsed_time();
   tms = time_str(NULL);
   tmrate = rate(*len - restval, con->dltime);
   /* Close data connection socket. */
   closeport(dtsock);
   /* Close the local file. */
   if (!opt.dfp || con->cmd & DO_LIST)
      fclose(fp);
   else
      fflush(fp);
   /* If get_contents couldn't write to fp, bail out. */
   if (res == -2)
   {
      if (!opt.quiet)
	 fprintf(opt.lfile, "%s: %s, closing control connection.\n",
		 u->local, mystrerror(errno));
      CLOSE(csock);
      con->fd = -1;
      return FWRITEERR;
   }
   else if (res == -1)
   {
      if (!opt.quiet)
      {
	 fprintf(opt.lfile, "%s (%s) - Data connection: %s; ",
		 tms, tmrate, mystrerror(errno));
	 if (opt.server_response)
	    fputc('\n', opt.lfile);
      }
   }

   /* Get the server to tell us if everything is retrieved. */
   err = ftp_response(csock, &respline);
   /* ...and empty the buffer. */
   buf_discard();
   if (err != FTPOK)
   {
      free(respline);
      if (!opt.quiet)
      {
	 /* The control connection is decidedly closed.  Print the
	    time only if it hasn't already been printed. */
	 if (res != -1)
	    fprintf(opt.lfile, "%s (%s) - ", tms, tmrate);
	 fprintf(opt.lfile, "Control connection closed.\n");
      }
      /* If there is an error on the control connection, close it, but
	 return FTPRETRINT, since there is a possibility that the
	 whole file was retrieved nevertheless (but that is for
	 ftp_loop_internal to decide).  */
      CLOSE(csock);
      con->fd = -1;
      return FTPRETRINT;
   } /* err != FTPOK */
   /* If retrieval failed for any reason, return FTPRETRINT, but do
      not close socket, since the control connection is still
      alive. If there is something wrong with the c. connection, it
      will become apparent later. */
   if (*respline != '2')
   {
      free(respline);
      if (res != -1)
	 fprintf(opt.lfile, "%s (%s) - ", tms, tmrate);
      fprintf(opt.lfile, "Data transfer aborted.\n");
      return FTPRETRINT;
   }
   free(respline);

   if (res == -1)
   {
      /* What now? The data connection was erroneous, whereas the
	 response says everything is OK. We shall play it safe. */
      return FTPRETRINT;
   }

   if (!(cmd & LEAVE_PENDING))
   {
      /* I should probably send 'QUIT' and check for a reply, but this
	 is faster. :-) */
      CLOSE(csock);
      con->fd = -1;
   }
   /* If it was a listing, and opt.server_response is true,
      print it out. */
   if (opt.server_response && (con->cmd & DO_LIST))
   {
      mkalldirs(u->local);
      fp = fopen(u->local, "r");
      if (!fp)
      {
	 if (!opt.quiet)
	    fprintf(opt.lfile, "%s: %s\n", u->local, mystrerror(errno));
      }
      else
      {
	 char *line;
	 /* The lines are being read with read_whole_line because of
	    no-buffering on opt.lfile. */
	 while ((line = read_whole_line(fp)))
	 {
	    fprintf(opt.lfile, "%s\n", line);
	    free(line);
	 }
	 fclose(fp);
      }
   } /* con->cmd & DO_LIST && server_response */
   
   return RETRFINISHED;
}

/* A one-file FTP loop. This is the part where FTP retrieval is
   retried, and retried, and retried, and...

   This loop either gets commands from con, or (if ON_YOUR_OWN is
   set), makes them up to retrieve the file given by the URL. */
uerr_t
ftp_loop_internal(urlinfo *u, struct fileinfo *f, ccon *con)
{
   static int first_retrieval = 1;

   int count, orig_lp;
   long restval, len;
   char *tms, *tmrate, *locf;
   uerr_t err;
   struct stat st;

   if (!u->local)
      u->local = url_filename(u);

   if (opt.noclobber && exists(u->local))
   {
      if (opt.verbose)
	 fprintf(opt.lfile, "File `%s' already there, not retrieving.\n",
		 u->local);
      /* If the file is there, we suppose it's retrieved OK. */
      return RETROK;
   }
   
   /* Remove it if it's a link. */
   remove_link(u->local);
   if (!opt.output_document)
      locf = u->local;
   else
      locf = opt.output_document;

   count = 0;
   
   if (con->st & ON_YOUR_OWN)
      con->st = ON_YOUR_OWN;

   orig_lp = con->cmd & LEAVE_PENDING ? 1 : 0;

   /* THE loop. */
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
      if (con->st & ON_YOUR_OWN)
      {
	 con->cmd = 0;
	 con->cmd |= (DO_RETR | LEAVE_PENDING);
	 if (con->fd != -1)
	    con->cmd &= ~(DO_LOGIN | DO_CWD);
	 else
	    con->cmd |= (DO_LOGIN | DO_CWD);
      }
      else /* not on your own */
      {
	 if (con->fd != -1)
	    con->cmd &= ~DO_LOGIN;
	 else
	    con->cmd |= DO_LOGIN;
	 if (con->st & DONE_CWD)
	    con->cmd &= ~DO_CWD;
	 else
	    con->cmd |= DO_CWD;
      }
      /* Assume no restarting. */
      restval = 0L;
      if ((count > 1 || opt.always_rest)
	  && !(con->cmd & DO_LIST)
	  && exists(u->local))
	 if (stat(u->local, &st) == 0)
	    restval = st.st_size;
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
      /* Send getftp the proper length, if fileinfo was provided. */
      if (f)
	 len = f->size;
      else
	 len = 0;
      err = getftp(u, &len, restval, con);
      /* Time? */
      tms = time_str(NULL);
      tmrate = rate(len - restval, con->dltime);
      
      if (con->fd == -1)
	 con->st &= ~DONE_CWD;
      else
	 con->st |= DONE_CWD;
      
      switch (err)
      {
	 case HOSTERR: case CONREFUSED: case FWRITEERR: case FOPENERR:
	 case FTPNSFOD: case FTPLOGINC: case FTPNOPASV:
	    /* Fatal errors, give up. */
	    return err;
	    break;
	 case CONSOCKERR: case CONERROR: case FTPSRVERR: case FTPRERR:
	 case WRITEFAILED: case FTPUNKNOWNTYPE: case CONPORTERR:
	 case BINDERR: case LISTENERR: case ACCEPTERR:
	 case FTPPORTERR: case FTPLOGREFUSED: case FTPINVPASV:
	    printwhat(count, opt.ntry);
	    /* Non-fatal errors */
	    continue;
	    break;
	 case FTPRETRINT:
	    /* If the control connection was closed, the retrieval
	       will be considered OK if f->size == len. */
	    if (!f || len != f->size)
	    {
	       printwhat(count, opt.ntry);
	       continue;
	    }
	    break;
	 case RETRFINISHED:
	    /* great! */
	    break;
	 default:
	    /* not-so-great */
	    assert(0);
      }
      if (con->st & ON_YOUR_OWN)
      {
	 CLOSE(con->fd);
	 con->fd = -1;
      }
      if (opt.verbose)
	 fprintf(opt.lfile, "%s (%s) - `%s' saved [%ld]\n\n",
		 tms, tmrate, locf, len);
      else if (!opt.quiet)
	 fprintf(opt.lfile, "%s URL: %s [%ld] -> \"%s\" [%d]\n",
		 tms, u->url, len, locf, count);
      /* Do not count listings among the downloaded stuff, since they
	 will get deleted anyway. */
      if (!(con->cmd & DO_LIST))
      {
	 ++opt.numurls;
	 opt.downloaded += len;
      }
      /* Restore the original leave-pendingness. */
      if (orig_lp)
	 con->cmd |= LEAVE_PENDING;
      else
	 con->cmd &= ~LEAVE_PENDING;
      return RETROK;
   } while (!opt.ntry || (count < opt.ntry));
   
   if (con->fd && (con->st & ON_YOUR_OWN))
   {
      CLOSE(con->fd);
      con->fd = -1;
   }
   return TRYLIMEXC;
}

/* Return the directory listing in a reusable format. The directory is
   speicifed in u->dir. */
struct fileinfo *
ftp_get_listing(urlinfo *u, ccon *con)
{
   struct fileinfo *f;
   uerr_t err;
   char *olocal = u->local;
   char *list_filename, *ofile;
   
   con->st &= ~ON_YOUR_OWN;
   con->cmd |= (DO_LIST | LEAVE_PENDING);
   con->cmd &= ~DO_RETR;
   /* Get the listing filename. */
   ofile = u->file;
   u->file = LIST_FILENAME;
   list_filename = url_filename(u);
   u->file = ofile;
   u->local = list_filename;
#ifdef DEBUG
   if (opt.debug)
      fprintf(opt.lfile, "Using `%s' as listing tmp file.\n", list_filename);
#endif
   err = ftp_loop_internal(u, NULL, con);
   u->local = olocal;
   if (err == RETROK)
      f = ftp_parse_ls(list_filename);
   else
      f = NULL;
   if (opt.remove_listing)
   {
      if (unlink(list_filename))
      {
	 if (!opt.quiet)
	    fprintf(opt.lfile, "unlink: %s\n", mystrerror(errno));
      }
      else if (opt.verbose)
	 fprintf(opt.lfile, "Removed `%s'.\n", list_filename);
   }
   free(list_filename);
   con->cmd &= ~DO_LIST;
   return f;
}

/* Retrieve a list of files given in struct fileinfo linked list. If a
   file is a symbolic link, do not retrieve it, but rather try to set
   up a similar link on the local disk, if the symlinks are supported.

   If opt.recursive is set, after all files have been retrieved,
   ftp_retrieve_dirs will be called to retrieve the directories. */
uerr_t
ftp_retrieve_list(urlinfo *u, struct fileinfo *f, ccon *con)
{
   void my_touch PARAMS((char *, time_t));
   static int depth = 0;
   uerr_t err;
   char *olocal, *ofile;
   struct fileinfo *orig;
   long local_size;
   time_t tml;
   int dlthis;

   /* Increase the depth. */
   ++depth;
   if (opt.maxreclevel && depth > opt.maxreclevel)
   {
#ifdef DEBUG
      if (opt.debug)
	 fprintf(opt.lfile, "Recursion depth %d exceeded max. depth %d.\n",
		 depth, opt.maxreclevel);
#endif
      --depth;
      return RECLEVELEXC;
   }

   assert(f != NULL);
   orig = f;

   con->st &= ~ON_YOUR_OWN;
   if (!(con->st & DONE_CWD))
      con->cmd |= DO_CWD;
   else
      con->cmd &= ~DO_CWD;
   con->cmd |= (DO_RETR | LEAVE_PENDING);
   
   if (con->fd == -1)
      con->cmd |= DO_LOGIN;
   else
      con->cmd &= ~DO_LOGIN;

   err = RETROK;                /* In case it's not used. */
   
   while (f)
   {
      if (opt.quota && opt.downloaded > opt.quota)
      {
	 --depth;
	 return QUOTEXC;
      }
      olocal = u->local;
      ofile = u->file;
      u->file = f->name;
      u->local = url_filename(u);
      err = RETROK;

      dlthis = 1;
      if (opt.timestamping && f->type == PLAINFILE)
      {
	 struct stat st;
	 if (!stat(u->local, &st))
	 {
	    /* Else, get it from the file. */
	    local_size = st.st_size;
	    tml = st.st_mtime;
	    if (local_size == f->size && tml >= f->tstamp)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "Local file `%s' is more recent, not retrieving.\n\n", u->local);
	       dlthis = 0;
	    }
	    else if (local_size != f->size)
	    {
	       if (opt.verbose)
		  fprintf(opt.lfile, "The sizes do not match (local %ld), retrieving.\n", local_size);
	    }
	 }
      }	/* opt.timestamping && f->type == PLAINFILE */
      switch (f->type)
      {
	 case SYMLINK:
	    /* If opt.retr_symlinks is defined, we treat symlinks as
	       if they were normal files.  There is currently no way
	       to distinguish whether they might be directories, and
	       follow them. */
	    if (!opt.retr_symlinks)
	    {
#ifdef HAVE_SYMLINK
	       if (!f->linkto)
	       {
		  if (!opt.quiet)
		     fprintf(opt.lfile, "Invalid name of the symlink, skipping.\n");
	       }
	       else             /* have f->linkto */
	       {
		  if (opt.verbose)
		     fprintf(opt.lfile, "Creating symlink %s -> %s\n",
			     u->local, f->linkto);
		  unlink(u->local); /* Unlink before creating symlink! */
		  if (symlink(f->linkto, u->local) == -1)
		  {
		     if (!opt.quiet)
			fprintf(opt.lfile, "symlink: %s\n", mystrerror(errno));
		  }
		  fputc('\n', opt.lfile);
	       } /* have f->linkto */
#else  /* not HAVE_SYMLINK */
	       if (!opt.quiet)
		  fprintf(opt.lfile,
			  "Symlinks not supported, skipping symlink `%s'.\n",
			  u->local);
#endif /* not HAVE_SYMLINK */
	    }
	    else                /* opt.retr_symlinks */
	    {
	       if (dlthis)
		  err = ftp_loop_internal(u, f, con);
	    } /* opt.retr_symlinks */
	    break;
	 case DIRECTORY:
	    if (!opt.recursive)
	       fprintf(opt.lfile, "Skipping directory `%s'.\n", f->name);
	    break;
	 case PLAINFILE:
	    /* Call the retrieve loop! */
	    if (dlthis)
	       err = ftp_loop_internal(u, f, con);
	    break;
	 case UNKNOWN:
	    if (!opt.quiet)
	       fprintf(opt.lfile, "%s: unknown/unsupported file type.\n", f->name);
	    break;
      }	/* switch */

      /* Set the time-stamp information to the local file. Symlinks
	 are not to be stamped because it sets the stamp on the
	 original. :( */
      if (!opt.dfp
	  && !(f->type == SYMLINK && !opt.retr_symlinks)
	  && f->tstamp != -1
	  && exists(u->local))
      {
	 my_touch(u->local, f->tstamp);
      }
      else if (f->tstamp == -1)
      {
	 if (!opt.quiet)
	    fprintf(opt.lfile, "%s: corrupt time-stamp.\n", u->local);
      }

      free(u->local);
      u->local = olocal;
      u->file = ofile;
      if (err != RETROK)
	 break;
      con->cmd &= ~(DO_CWD | DO_LOGIN);
      f = f->next;
   } /* while */
   /* We do not want to call ftp_retrieve_dirs here */
   if (opt.recursive && !(opt.maxreclevel && depth >= opt.maxreclevel))
       err = ftp_retrieve_dirs(u, orig, con);
   else if (opt.recursive)
   {
#ifdef DEBUG
      if (opt.debug)
	 fprintf(opt.lfile,
		 "Will not retrieve dirs since depth is %d (max %d).\n",
		 depth, opt.maxreclevel);
#endif
   }
   --depth;
   return err;
}

/* Retrieve the directories given in a file list.  This function works
   by simply going through the linked list and calling
   ftp_retrieve_glob on each directory entry.  The function knows
   about excluded directories.  */
uerr_t
ftp_retrieve_dirs(urlinfo *u, struct fileinfo *f, ccon *con)
{
   char *odir;
   int l;
   
   for (; f; f = f->next)
   {
      if (opt.quota && opt.downloaded > opt.quota)
	 break;
      if (f->type != DIRECTORY)
	 continue;
      odir = u->dir;
      l = strlen(u->dir);
      u->dir = nmalloc(1 + l + 1 + strlen(f->name) + 1);
      /* When retrieving recursively, all directories must be
	 absolute. This restriction will be lifted in the future. */
      sprintf(u->dir, "/%s%s%s", odir + (*odir == '/'),
	      (!*odir || (*odir == '/' && !*(odir + 1))) ? "" : "/", f->name);
      if (!accdir(u->dir, ALLABS))
      {
	 if (opt.verbose)
	    fprintf(opt.lfile, "Not descending to `%s' as it is excluded/not-included.\n",
		    u->dir);
	 free(u->dir);
	 u->dir = odir;
	 continue;
      }
      con->st &= ~DONE_CWD;
      ftp_retrieve_glob(u, con, GETALL);
      /* Set the time-stamp? */
      free(u->dir);
      u->dir = odir;
   }
   if (opt.quota && opt.downloaded > opt.quota)
      return QUOTEXC;
   else
      return RETROK;
}


/* A near-top-level function to retrieve the files in a directory.
   The function calls ftp_get_listing, to get a linked list of
   files. Then it weeds out the file names that do not match the
   pattern.  ftp_retrieve_list is called with this updated list as an
   argument.

   If the argument ACTION is GETONE, just download the file (but first
   get the listing, so that the time-stamp is heeded); if it's GLOBALL,
   use globbing' if it's GETALL, download the whole directory.  */
uerr_t
ftp_retrieve_glob(urlinfo *u, ccon *con, int action)
{
   struct fileinfo *f, *orig, *start;
   int matchres;
   uerr_t res;

   con->cmd |= LEAVE_PENDING;
   
   orig = ftp_get_listing(u, con);
   start = orig;
   /* First: weed out that do not conform the global rules given in
      opt.accepts and opt.rejects. */
   if (opt.accepts || opt.rejects)
   {
      f = orig;
      while (f)
      {
	 if (f->type != DIRECTORY && !acceptable(f->name))
	 {
	    if (opt.verbose)
	       fprintf(opt.lfile, "Rejecting `%s'.\n", f->name);
	    f = delelement(f, &start);
	 }
	 else
	    f = f->next;
      }
   }
   /* Now weed out the files that do not match our globbing pattern.
      If we are dealing with a globbing pattern, that is. */
   if (*u->file && (action == GLOBALL || action == GETONE))
   {
      matchres = 0;
      f = start;
      while (f)
      {
	 matchres = fnmatch(u->file, f->name, 0);
	 if (matchres == -1)
	 {
	    if (!opt.quiet)
	       fprintf(opt.lfile, "%s: %s\n", u->local, mystrerror(errno));
	    break;
	 }
	 if (matchres == FNM_NOMATCH)
	    f = delelement(f, &start); /* Delete the element from the list. */
	 else
	    f = f->next;        /* Leave the element in the list. */
      }
      if (matchres == -1)
      {
	 freefileinfo(start);
	 return RETRBADPATTERN;
      }
   }
   res = RETROK;
   if (start)
   {
      /* Just get everything.  */
      ftp_retrieve_list(u, start, con);
   }
   else if (!start)
   {
      if (action == GLOBALL)
      {
	 /* No luck.  */
	 if (opt.verbose)
	    fprintf(opt.lfile, "No matches on pattern `%s'.\n", u->file);
      }
      else /* GETONE or GETALL */
      {
	 /* Let's try retrieving it anyway.  */
	 con->st |= ON_YOUR_OWN;
	 res = ftp_loop_internal(u, NULL, con);
	 return res;
      }
   }
   freefileinfo(start);
   if (opt.quota && opt.downloaded > opt.quota)
      return QUOTEXC;
   else
      return RETROK;
}

/* The wrapper that calls an appropriate routine according to contents
   of URL. Inherently, its capabilities are limited on what can be
   encoded into a URL. */
uerr_t
ftp_loop(urlinfo *u, int *dt)
{
   ccon con;                    /* FTP connection. */
   uerr_t res;
   struct fileinfo *f;

   *dt = 0;

   con.fd = -1;
   con.st = ON_YOUR_OWN;
   res = RETROK;		/* In case it's not used. */

   /* If the file name is empty, the user probably wants a directory
      index. Wel'll provide one, properly HTML-ized.  Unless
      opt.htmlify is 0, of course. :-) */
   if (!*u->file && !opt.recursive)
   {
      f = ftp_get_listing(u, &con);
      if (f)
      {
	 if (opt.htmlify)
	 {
	    char *filename;
	    filename = url_filename(u);
	    res = ftp_index(filename, u, f);
	    if (res == FTPOK && opt.verbose)
	    {
	       struct stat st;
	       long sz;
	       if (stat(filename, &st) == 0)
		  sz = st.st_size;
	       else
		  sz = -1;
	       fprintf(opt.lfile, "Wrote HTML-ized index to `%s' [%ld].\n",
		       filename, (long)st.st_size);
	    }
	    free(filename);
	 }
	 freefileinfo(f);
      }
   }
   else
   {
      int wild = has_wildcards(u->file);
      if ((opt.ftp_glob && wild) || opt.recursive || opt.timestamping)
      {
	 /* ftp_retrieve_glob is a catch-all function that gets called
	    if we need globbing, time-stamping or recursion.  Its
	    third argument is just what we really need.  */
	 ftp_retrieve_glob(u, &con, (opt.ftp_glob && wild) ? GLOBALL : GETONE);
      }
      else
	 res = ftp_loop_internal(u, NULL, &con);
   }
   if (res == FTPOK)
      res = RETROK;
   if (res == RETROK)
      *dt |= RETROKF;
   /* If a connection was left, quench it. */
   if (con.fd != -1)
      CLOSE(con.fd);
   return res;
}

/* Deletes an element from the fileinfo linked list. Returns the
   address of the next element, or NULL if the list is exhausted. It
   can modify the start of the list. */
struct fileinfo *
delelement(struct fileinfo *f, struct fileinfo **start)
{
   struct fileinfo *prev, *next;
   
   prev = f->prev;
   next = f->next;
   if (next)
      next->prev = prev;
   if (!prev)
   {
      *start = next;
      if (!next)
	 return NULL;
      free(f->name);
      if (f->linkto)
	 free(f->linkto);
      free(f);
   }
   else
      prev->next = next;
   return next;
}

/* Free the fileinfo linked list of files. */
void
freefileinfo(struct fileinfo *f)
{
   struct fileinfo *p;

   while (f)
   {
      p = f->next;
      free(f->name);
      if (f->linkto)
	 free(f->linkto);
      free(f);
      f = p;
   }
}


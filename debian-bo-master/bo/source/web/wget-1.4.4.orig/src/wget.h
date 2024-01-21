/* Miscellaneous declarations.
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


/* This file contains some declarations that don't fit anywhere else.
   It also contains some useful includes, like the obnoxious TIME_H
   including.  */

/* $Id: wget.h,v 1.1.1.1.2.2 1997/02/15 19:23:22 hniksic Exp $ */

#ifndef WGET_H
#define WGET_H

#ifndef PARAMS
# if PROTOTYPES
#  define PARAMS(Args) Args
# else
#  define PARAMS(Args) ()
# endif
#endif

#include <sys/types.h>
#include <sys/stat.h>
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else /* not TIME_WITH_SYS_TIME_H */
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#else /* not HAVE_SYS_TIME_H */
#  include <time.h>
#endif /* HAVE_SYS_TIME_H */
#endif /* TIME_WITH_SYS_TIME_H */

#include "systhings.h"

/* Macro definitions */
#ifdef DEBUG
#  define DEBUGP(x) do { if (opt.debug) { fprintf(opt.lfile, "%s", x);}} while (0)
#else
#  define DEBUGP(x) do { } while (0)
#endif

#ifdef WINDOWS
# ifdef DEBUG
#   define CLOSE(x)                                                \
   do {                                                            \
      closesocket(x);                                              \
      if (opt.debug) fprintf(opt.lfile, "Closing fd %d\n", x);     \
   } while (0)
# else
#   define CLOSE(x) closesocket(x)
# endif /* DEBUG */
#else /* not WINDOWS */
# ifdef DEBUG
#   define CLOSE(x)                                                \
   do {                                                            \
      close(x);                                                    \
      if (opt.debug) fprintf(opt.lfile, "Closing fd %d\n", x);     \
   } while (0)
# else 
#   define CLOSE(x) close(x)
# endif /* DEBUG */
#endif /* not WINDOWS */

/* read & write don't work with sockets on win95 */
#ifdef WINDOWS
# define READ(fd, buf, cnt) recv((fd), (buf), (cnt), 0)
# define WRITE(fd, buf, cnt) send((fd), (buf), (cnt), 0)
#else 
# define READ(fd, buf, cnt) read((fd), (buf), (cnt))
# define WRITE(fd, buf, cnt) write((fd), (buf), (cnt))
#endif /* WINDOWS */

/* Document-type flags */
enum {
   TEXTHTML      = 0x0001,      /* Document is of type text/html */
   RETROKF       = 0x0002,      /* Retrieval was OK */
   HEAD_ONLY     = 0x0004,      /* Only send the HEAD request. */
   SEND_NOCACHE  = 0x0008,      /* Send Pragma: no-cache directive. */
   ACCEPTRANGES  = 0x0010       /* Accept-ranges header was found. */
};

/* Universal error type -- used almost everywhere */
typedef enum {NOCONERROR, HOSTERR, CONSOCKERR, CONERROR,
              CONREFUSED, NEWLOCATION, NOTENOUGHMEM, CONPORTERR,
              BINDERR, BINDOK, LISTENERR, ACCEPTERR, ACCEPTOK,
              CONCLOSED, FTPOK, FTPLOGINC, FTPLOGREFUSED, FTPPORTERR,
              FTPNSFOD, FTPRETROK, FTPUNKNOWNTYPE, FTPRERR,
              FTPREXC, FTPSRVERR, FTPRETRINT, FTPRESTFAIL,
              URLOK, URLHTTP, URLFTP, URLFILE, URLUNKNOWN, URLBADPORT,
              URLBADHOST, FOPENERR, FWRITEERR, HOK, HLEXC, HEOF,
              HERR, RETROK, RECLEVELEXC, FTPACCDENIED, WRONGCODE,
              FTPINVPASV, FTPNOPASV,
              RETRFINISHED, READERR, TRYLIMEXC, URLBADPATTERN,
              FILEBADFILE, RANGEERR, RETRBADPATTERN, RETNOTSUP,
              ROBOTSOK, NOROBOTS, PROXERR, QUOTEXC, WRITEFAILED } uerr_t;


#endif /* WGET_H */

/* Declarations for reading/parsing the initialization file.
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


/* $Id: init.h,v 1.1.1.1.2.4 1997/02/17 20:45:40 hniksic Exp $ */

#ifndef INIT_H
#define INIT_H

#define ISODIGIT(x) ((x) >= '0' && (x) <= '7')

/* Print definitions for default style. */
#define DOTS_IN_LINE 50
#define DOT_BYTES 1024
#define DOT_SPACING 10

/* Command enumeration. */
typedef enum {
   NONE, ACCEPT, ADDHOSTDIR, ALWAYSREST, BACKUPS, BASEHREF,
   CACHE, CONVERTLINKS, DEBUG_, DELETEAFTER,
   DIRMODE, DIRPREFIX, DIRSTRUCT, DOMAINS,
   DOTBYTES, DOTSINLINE, DOTSPACING, DOTSTYLE,
   EXCLUDEDIRECTORIES, EXCLUDEDOMAINS, FILEPREFIX,
   FOLLOWFTP, FORCEHTML, FTPPROXY, GLOB,
   HEADER, HTMLIFY, HTTPPASSWD, HTTPPROXY, HTTPUSER, IGNORELENGTH,
   INPUT, INCLUDEDIRECTORIES, KILLLONGER, LOGFILE, LOGIN,
   MIRROR, NETRC, NOCLOBBER, NOPARENT, NOPROXY, NUMTRIES,
   OUTPUTDOCUMENT, PASSIVEFTP, PASSWD, PREFIXFILES, QUIET, QUOTA_,
   RECLEVEL,
   RECURSIVE, REJECT, RELATIVEONLY, REMOVELISTING, RETRSYMLINKS,
   ROBOTS, SAVEHEADERS, SERVERRESPONSE, SIMPLEHOSTCHECK,
   SPANHOSTS, SPIDER, TIMEOUT, TIMESTAMPING, USEPROXY,
   USERAGENT, VERBOSE, WAIT, PROXYPASSWD, PROXYUSER,
   LAST
} cmdid;

/* Valid command types: */
enum cmdtype {
   CONOFF,                      /* ONOFF (toggle). */
   CNUM,                        /* Number. */
   CNUMINF,                     /* Number/infinity. */
   CSTR,                        /* String. */
   CVEC,                        /* Vector. */
   CVECDIR,                     /* Vector of directories. */
   CBYT,                        /* Bytes. */
   CSPEC                        /* Special. */
};

/* Command struct */
struct cmd {
   char *name;
   cmdid id;
   void *varp;
   enum cmdtype what_to_do;
};

enum { SINF = 1 };

int comind PARAMS((const struct cmd *, const char *));
char *home_dir PARAMS((void));
char *init_path PARAMS((void));
void run_wgetrc PARAMS((const char *));
void initialize PARAMS((void));
int parse_line PARAMS((const unsigned char *, unsigned char **, unsigned char **));
int setval PARAMS((const char *, const char *));
int onoff PARAMS((const char *));
int setonoff PARAMS((int *, const char *, const char *));
int setnum PARAMS((int *, const char *, const char *, int));
int setbytes PARAMS((long *, const char *, const char *));
void defaults PARAMS((void));
char *home_dir PARAMS((void));
int myatoi PARAMS((const char *));
int getperms PARAMS((const char *));
void cleanup PARAMS((void));

#endif /* INIT_H */

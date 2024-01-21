/* Declarations for FTP support.
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


/* $Id: ftp.h,v 1.1.1.1.2.1 1997/02/15 19:22:54 hniksic Exp $ */

#ifndef FTP_H
#define FTP_H

/* You can change this to something else, like "ftp", but I suggest
   "anonymous". */
#define DEFAULT_FTP_ACCT "anonymous"

/* File where the "ls -al" listing will be saved. */
#define LIST_FILENAME ".listing"

/* File types. */
enum ftype {
   PLAINFILE = 0,
   DIRECTORY,
   SYMLINK,
   UNKNOWN
};

/* Globbing (used by ftp_retrieve_glob). */
enum {
   GLOBALL, GETALL, GETONE
};

/* Information about one filename in a linked list. */
struct fileinfo {
   enum ftype type;             /* File type */
   char *name;                  /* File name */
   long size;                   /* File size */
   long tstamp;                 /* Time-stamp */
   int perms;                   /* File permissions */
   char *linkto;                /* Link to which file points */
   struct fileinfo *prev;       /* Previous... */
   struct fileinfo *next;       /* ...and next structure. */
};

/* Commands for FTP functions. */
enum command {
   DO_LOGIN      = 0x0001,      /* Connect and login to the server. */
   DO_CWD        = 0x0002,      /* Change current directory. */
   DO_RETR       = 0x0004,      /* Retrieve the file. */
   DO_LIST       = 0x0008,      /* Retrieve the directory list. */
   LEAVE_PENDING = 0x0010       /* Do not close the socket. */
};

enum fstatus {
   NOTHING       = 0x0000,      /* Nothing done yet. */
   ON_YOUR_OWN   = 0x0001,      /* The ftp_loop_internal sets the
                                   defaults. */
   DONE_CWD      = 0x0002       /* The current working directory is
                                   correct. */
};

typedef struct {
   int st;                      /* Connection status. */
   int cmd;                     /* Command code. */
   int fd;                      /* Control connection fd. */
   long dltime;                 /* Time of the download. */
} ccon;

uerr_t getftp PARAMS((const urlinfo *, long *, long, ccon *));
uerr_t ftp_loop_internal PARAMS((urlinfo *, struct fileinfo *, ccon *));
struct fileinfo *ftp_get_listing PARAMS((urlinfo *, ccon *));
struct fileinfo *ftp_parse_ls PARAMS((const char *));
uerr_t ftp_retrieve_list PARAMS((urlinfo *, struct fileinfo *, ccon *));
uerr_t ftp_retrieve_glob PARAMS((urlinfo *, ccon *, int));
uerr_t ftp_retrieve_dirs PARAMS((urlinfo *, struct fileinfo *, ccon *));
uerr_t ftp_loop PARAMS((urlinfo *, int *));

int symperms PARAMS((const char *));
struct fileinfo *ftp_parse_unix_ls PARAMS((const char *));
void freefileinfo PARAMS((struct fileinfo *));
struct fileinfo *delelement PARAMS((struct fileinfo *, struct fileinfo **));

#endif /* FTP_H */

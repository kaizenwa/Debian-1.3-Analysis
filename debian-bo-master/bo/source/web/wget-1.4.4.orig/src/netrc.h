/* Declarations for netrc.c
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


/* $Id: netrc.h,v 1.1.1.1.2.1 1997/02/15 19:23:11 hniksic Exp $ */

typedef struct _acc_t {
  char *host; /* This is NULL if this is the default machine entry. */
  char *acc;
  char *passwd; /* This is NULL if there is no password. */
  struct _acc_t *next;
} acc_t;

#define NETRC_FILE_NAME ".netrc"

void search_netrc PARAMS((const char *, const char **, const char **, int));
acc_t *parse_netrc PARAMS((const char *));
void free_netrc PARAMS((acc_t *l));

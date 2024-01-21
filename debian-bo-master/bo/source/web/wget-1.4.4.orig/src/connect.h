/* Declarations for connect.
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


/* $Id: connect.h,v 1.1.1.1.2.1 1997/02/15 19:22:48 hniksic Exp $ */

#ifndef CONNECT_H
#define CONNECT_H

#define BACKLOG 1

/* Function declarations */
uerr_t make_connection PARAMS((int *, char *, unsigned short));
uerr_t bindport PARAMS((unsigned short *));
uerr_t acceptport PARAMS((int *));
void closeport PARAMS((int));
unsigned char *conaddr PARAMS((int));

#ifdef HAVE_SELECT
int select_fd PARAMS((int, int));
#endif
int iread PARAMS((int, char *, int));
int iwrite PARAMS((int, char *, int));

#endif /* CONNECT_H */

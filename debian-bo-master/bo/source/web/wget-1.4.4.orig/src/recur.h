/* Declarations for recursive retrieving
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


/* $Id: recur.h,v 1.1.1.1.2.1 1997/02/15 19:23:14 hniksic Exp $ */

#ifndef RECUR_H
#define RECUR_H

/* Default robots filename */
#define ROBOTS_FILENAME "robots.txt"

enum {
   RFIRST_TIME = 1,
   RCLEANUP    = 2
};

void convert_all_links PARAMS((void));
uerr_t recursive_retrieve PARAMS((const char *, const char *, int));

/* Robots support: */
uerr_t retrieve_robots PARAMS((const char *, const char *));
urlinfo *robots_url PARAMS((const char *, const char *));
char **parse_robots PARAMS((const char *));
int robots_match PARAMS((urlinfo *, char **));

#endif /* RECUR_H */

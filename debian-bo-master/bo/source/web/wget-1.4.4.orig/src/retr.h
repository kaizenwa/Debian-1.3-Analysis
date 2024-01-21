/* Declarations for file retrieval.
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


/* $Id: retr.h,v 1.1.1.1.2.1 1997/02/15 19:23:16 hniksic Exp $ */

#ifndef RETR_H
#define RETR_H

/* Retrieval buffer size for get_contents. */
#define BUFFER_SIZE 8192

/* Input chunk size for buffered input (buf_readchar). */
#define INPUT_BUFFER_SIZE 4096

/* Function declarations */
int buf_readchar PARAMS((int, char *));
int buf_peek PARAMS((int, char *));
int buf_flush PARAMS((char *, int));
void buf_discard PARAMS((void));

int get_contents PARAMS((int, FILE *, long *, long, int));
int show_progress PARAMS((long, int));


uerr_t retrieve_url PARAMS((const char *, char **, char **, const char *, int *));
uerr_t retrieve_from_file PARAMS((const char *, int, int *));

void printwhat PARAMS((int, int));

void reset_timer PARAMS((void));
long elapsed_time PARAMS((void));
char *time_str PARAMS((time_t *));
char *rate PARAMS((long, long));

#endif /* RETR_H */

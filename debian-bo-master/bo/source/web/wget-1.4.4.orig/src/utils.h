/* Declarations for utilities.
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


/* $Id: utils.h,v 1.1.1.1.2.1 1997/02/15 19:23:21 hniksic Exp $ */

#ifndef UTILS_H
#define UTILS_H

#include "cmpt.h"

/* The size of the initial buffer for read_whole_line. */
#define DYNAMIC_LINE_BUFFER 40
/* Separator for legible numbers */
#define LEGIBLE_SEPARATOR ','
/* Is the string a hpyhen-only? */
#define ISHYPHEN(x) (*(x) == '-' && !*((x) + 1))
/* Buffer for file-loading */
#define FILE_BUFFER_SIZE 512

/* Flags for slist.  */
enum {
   NOSORT     = 1
};

enum accd {
   ALLABS = 1
};

/* A linked list of strings. The list is ordered alphabetically. */
typedef struct _slist {
   char *string;
   struct _slist *next;
} slist;

void *nmalloc PARAMS((size_t));
void *nrealloc PARAMS((void *, size_t));
char *nstrdup PARAMS((const char *));
void memfatal PARAMS((const char *));
const char *uerrmsg PARAMS((uerr_t));

char *strdupdelim PARAMS((const char *, const char *));
char **sepstring PARAMS((const char *));
int frontcmp PARAMS((const char *, const char *));
char *mycuserid PARAMS((char *));
void path_simplify PARAMS((char *));

int remove_link PARAMS((const char *));
int exists PARAMS((const char *));
int isfile PARAMS((const char *));
int mymkdir PARAMS((const char *));

int acceptable PARAMS((const char *));
int accdir PARAMS((const char *s, enum accd));
int match_backwards PARAMS((const char *, const char *));
int in_acclist PARAMS((const char **, const char *, int));
char *suffix PARAMS((const char *s));

char *read_whole_line PARAMS((FILE *));
void load_file PARAMS((FILE *, char **, long *));

void free_vec PARAMS((char **));
char **merge_vecs PARAMS((char **, char **));
slist *add_slist PARAMS((slist *, const char *, int));
int in_slist PARAMS((slist *, const char *));
void free_slist PARAMS((slist *));

char *legible PARAMS((long));
int numdigit PARAMS((long));
void prnum PARAMS((char *, long));

#endif /* UTILS_H */

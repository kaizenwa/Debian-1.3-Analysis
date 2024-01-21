/************************************************************************
 * utils.c -- various utilities						*
 *									*
 * This program is free software; you can redistribute it and/or modify *
 * it under the terms of the GNU General Public License as published by *
 * the Free Software Foundation; either version 2 of the License, or    *
 * (at your option) any later version.                                  *
 *									* 
 * This program is distributed in the hope that it will be useful,      *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 * GNU General Public License for more details.                         *
 *									* 
 * You should have received a copy of the GNU General Public License    *
 * along with this program; if not, write to the Free Software          *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.            *
 *									* 
 ************************************************************************/

/* $Id: utils.c,v 1.1 1996/02/11 20:44:55 david Rel $
 * $Log: utils.c,v $
 * Revision 1.1  1996/02/11 20:44:55  david
 * Initial revision
 * */

#include <stdio.h>
#include <stdlib.h>

#define LINEFRAGMENT 32

extern int errno;
extern char *progname;

void *xmalloc(size_t size)	/* safe malloc */
{
  void *ptr;

  errno=0;
  ptr=malloc(size);
  if ((ptr == NULL) || (errno != 0))
  {
    perror(progname); exit(1); 
  }

  return ptr;
}

void *xrealloc(void *ptr, size_t size)
{
  errno=0;
  ptr=realloc(ptr, size);
  if (ptr == NULL) 
  {
    perror(progname); exit(1); 
  }

  return ptr;
}

unsigned char *getline(FILE *stream)
/* Read a line of text without imposing an length limit */
/* From: Bison Manual */
{
  unsigned char *line=NULL;
  int c;
  int i;

  i=0; errno=0;
  line=(unsigned char *)xmalloc(LINEFRAGMENT+1);
  while (((c = fgetc(stream)) != EOF) && (c != '\n') && (c != '\0'))
  {
    if (errno != 0) perror(progname);

    if ((i > LINEFRAGMENT) &&  (i % LINEFRAGMENT) == 0) 
    line=(unsigned char *)xrealloc(line,i+LINEFRAGMENT+1);
    line[i]=c; i++;
  }
  if ((c != EOF) && (c != '\0')) line[i]='\0';
  else
  {
    free(line); line=NULL;
  }
  return line;
}

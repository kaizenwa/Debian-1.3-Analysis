/************************************************************************
 * utils.c -- various utilities for rpncalc				*
 *									*
 * Refer to rpncalc.c for copyrights and license permissions.           *
 ************************************************************************/

/* $Id: utils.c,v 1.4 1997/01/19 18:58:58 david Rel $
 * $Log: utils.c,v $
 * Revision 1.4  1997/01/19 18:58:58  david
 * Changed dupstr(char *) to dupstr(const char *)
 *
 * Revision 1.4  1997/01/19 18:19:23  david
 * Changed dupstr(char *) to dupstr(const char *)
 *
 * Revision 1.2  1996/09/13 20:21:29  david
 * lclint additions
 *
 * Revision 1.1  1996/07/13 20:58:36  david
 * Added dupstr()
 * Moved getline() from rpncalc.c into utils.c
 *
 * Revision 1.0  1995/12/31 18:19:34  david
 * Initial revision
 *
 * Revision 1.0  1995/11/25 20:02:25  david
 * Initial revision
 * */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "utils.h"

#ifdef HAVE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#define LINEFRAGMENT 32

/*@out@*/ void *xmalloc(size_t size)	/* safe malloc */
{
  void *ptr;

  ptr=malloc(size);
  if (ptr == NULL) 
  {
    perror("Out of memory!"); 
    exit(1); 
  }

  return ptr;
}

/*@out@*/ void *xrealloc(void *ptr, size_t size)	/* save realloc */
{
  ptr=realloc(ptr, size);
  if (ptr == NULL) 
  {
    perror("Out of memory!"); 
    exit(1); 
  }

  return ptr;
}

char *dupstr(const char *s)
{
  char *r;

  r = (char *)xmalloc (strlen (s) + 1);
  strcpy (r, s);
  return (r);
}

/*@null@*/ char *getline(void)
/* Read a line of text without imposing an length limit.
   If we readline support is compiled in, use the readline function. */
{
  char *line=NULL;
#ifndef HAVE_READLINE
  int c;
  int i;
#endif

#ifdef HAVE_READLINE
  line=(char*)readline(""); 
#else
  i=0; line=(char *)xmalloc((size_t)(LINEFRAGMENT+1));
  while (((c = getc(stdin)) != EOF) && (c != '\n') && (c != '\0'))
  {
   if ((i > LINEFRAGMENT) &&  (i % LINEFRAGMENT) == 0) 
     line=(char *)xrealloc(line,(size_t)(LINEFRAGMENT+1));
   line[i]=(char)c; i++;
  }
  if ((c != EOF) && (c != '\0')) line[i]='\0';
  else
  {
    free(line); line=NULL;   /* this is readline compatible; 
				return NULL when the input line is empty */
  }
#endif

  return line;
}

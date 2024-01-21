/*
 * File:	pstring.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	11 Aug 95   Framstag	initial version
 *
 * Functions to handle Pascal like strings.
 * Look at string.h for a list of the functions.
 * Strings start by definition at pstr.length[1]
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <memory.h> */

#include "pstring.h"


/*
 * pstr_create - create a pstring
 *
 * INPUT:  size	- size of pstring
 *
 * RETURN: the new pstring
 */
pstr_t *pstr_create(int size)
{ pstr_t *pstr;		/* pstring pointer */
  char *string;		/* the string contents */

  /* allocate memory */
  if ((pstr=(pstr_t *)malloc(sizeof(pstr_t))) == NULL) return(NULL);
  if ((string=(char *)malloc(size+1)) == NULL) return(NULL);

  /* form the new pstring */
  pstr->size=size;
  pstr->length=0;
  pstr->string=string;

  return(pstr);
}


/*
 * pstr_delete - delete a pstring
 *
 * INPUT:  pstr	- pstring to delete
 */
void pstr_delete(pstr_t *pstr)
{
  /* Freiheit fuer die Gummibaerchen! */
  free(pstr->string);
  free(pstr);
}


/*
 * pstr_addchar - add a char to a pstring
 *
 * INPUT:  c	- the char to add
 *         pstr - the pstring to add to
 *
 * OUTPUT: pstr - the pstring
 *
 * RETURN: 0 if ok, -1 if failed
 */
int pstr_addchar(pstr_t *pstr, char c)
{
  /* no more space for appending a char? */
  if (pstr->length >= pstr->size) return(-1);

  /* increment the pstring length information */
  pstr->length += 1;

  /* add the char, what else? :-) */
  pstr->string[pstr->length]=c;

  return(0);
}


/*
 * pstr_assign - assign one pstring to another (p1<-p2)
 *
 * INPUT:  p1	- destination pstring
 *         p1	- source pstring
 *
 * OUTPUT: p1	- assigned pstring
 *
 * RETURN: 0 if ok, -1 if failed
 */
int pstr_assign(pstr_t *p1, pstr_t *p2)
{
  /* is p1 big enough for p2? */
  if (p2->length > p1->size) return(-1);

  /* copy the string */
  memcpy(p1->string,p2->string,p2->length+1);

  /* copy the length information */
  p1->length = p2->length;

  return(0);
}


/*
 * pstr_addstring - add a string to a pstring
 *
 * INPUT:  s	- string to add
 *         pstr - the pstring to add to
 *
 * OUTPUT: pstr - the pstring
 *
 * RETURN: 0 if ok, -1 if failed
 */
int pstr_addstring(pstr_t *pstr, const char *s)
{ int plen,	/* length of pstring */
      slen;	/* length of string */

  /* get the lengths */
  slen=strlen(s);
  plen=pstr->length;

  /* does it fit into pstring? */
  if (plen+slen > pstr->size) return(-1);

  /* copy the string */
  memcpy(pstr->string+plen+1,s,slen);

  /* adjust the length information */
  pstr->length += slen;

  return(0);
}


/*
 * pstr_print - print a pstring
 *
 * INPUT:  pstr - pstring to print
 */
void pstr_print(pstr_t *pstr)
{ int i;	/* simple loop counter */

  /* print char by char */
  for (i=1; i <= pstr->length; i++)
    printf("%c",pstr->string[i]);
}


/*
 * test main routine for debugging purposes, not used
 *
void main()
{ pstr_t *p1,*p2;
  char *blubb="blabla";

  p1=pstr_create(100);
  p2=pstr_create(20);
  pstr_addchar(p1,'x');
  pstr_addstring(p1,"123");
  pstr_addstring(p2,"abcd");
  pstr_addstring(p1,blubb);
  pstr_assign(p2,p1);
  pstr_delete(p1);
  pstr_print(p2);
  printf("\n");
}
*/

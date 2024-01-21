/* $Id: lsearch.c,v 1.1.1.1 1995/02/16 05:23:01 hjl Exp $ */

#include <ansidecl.h>
#include <search.h>
#include <memory.h>

/* Perform a linear search for KEY in BASE which has *NMEMB elements
   of SIZE bytes each.  The comparisons are done by (*COMPAR)(), which
   must return zero if the elements being compared are equal, and
   non-zero otherwise. 
   If the datum is not found, it will be inserted at the end of the
   table. */
PTR
DEFUN(lsearch, (key, base, pnmemb, size, compar),
      register CONST PTR key AND CONST PTR base AND
      register size_t *nmemb AND register size_t size AND
      register int EXFUN((*compar), (CONST PTR, CONST PTR)))
{
  register size_t i;
  register CONST PTR p;

  for(i = 0, p = base; i < *nmemb; 
             p = (PTR) (((CONST char *) p) + size), i++)
      if ( !((*compar)(key, p)))
        return (PTR) p;

  (*nmemb)++;
  memmove ((PTR) p, (PTR) key, size);
  return (PTR) p;
}

/*
 * memory.c -- functions to copy and fill memory.
 */

#include "../h/gsupport.h"

#ifndef SysMem
pointer memcopy(to, from, n)
   register char *to, *from;
   register word n;
   {
   register char *p = to;

   while (--n >= 0)
      *to++ = *from++;

   return (pointer)p;
   }

pointer memfill(to, con, n)
   register char *to;
   register int con;
   register word n;
   {
   register char *p = to;

   while (--n >= 0)
      *to++ = con;

   return (pointer)p;
   }
#else					/* SysMem */
static char x;				/* prevent empty module */
#endif					/* SysMem */

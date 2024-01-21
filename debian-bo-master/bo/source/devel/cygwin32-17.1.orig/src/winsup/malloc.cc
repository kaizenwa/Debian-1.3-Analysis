/* malloc.cc for WIN32.

   Written by Steve Chamberlain of Cygnus Support.
   sac@cygnus.com

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "winsup.h"

/* we provide these stubs to call into a user's
   provided malloc if there is one - otherwise
   functions we provide - like strdup will cause
   problems if malloced on our heap and free'd on theirs

   Digusting but true.


*/

#undef in
#undef out
#define in(x) 
#define out(x)

/* Call though the application pointer,
   which either points to export_malloc, or the application's
   own version . */

void *malloc (size_t size)
{
  void *res;
  in ("malloc");
  res = u->malloc (size);
  out ("malloc");
  return res;
}


void free (void *p)
{
  in ("free");
  u->free (p);
  out ("free");
}


void *realloc (void *p, size_t size)
{
  void *res;
  in ("realloc");
  res = u->realloc (p, size);
  out ("realloc");
  return res;
}


/* These routines are used by the application if it
   doesn't provide it's own malloc. */
   
   
void export_free (void *p)
{
  in ("export_free");
  _free_r (_impure_ptr, p);
  out ("export_free");
}

void *export_malloc (int size)
{
  void *res;
  in ("export_malloc");
  res = _malloc_r (_impure_ptr, size);
  out ("export_malloc");
  return res;
}
void *export_realloc (void *p, int size)
{
  void *res;
  in ("export_realloc");
  res = _realloc_r (_impure_ptr, p, size);
  out ("export_realloc");
  return res;

}




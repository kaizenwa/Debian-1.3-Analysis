/* grp.c for WIN32.

   Written by Jason Molenda of Cygnus Support.
   crash@cygnus.com

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
*/

#include <grp.h>

struct group *
getgrgid (gid_t gid)
{
  static struct group grp;
  static char *namearray[2];

  grp.gr_name="everyone";
  grp.gr_gid=0;
  namearray[0]="everyone";
  namearray[1]=NULL;
  grp.gr_mem=namearray;

  return (&grp);

}

struct group *
getgrnam (const char *name)
{
  return (getgrgid (0));
}

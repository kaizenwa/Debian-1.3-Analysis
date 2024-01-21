/* pipe for WIN32.

   Written by Doug Evans and Steve Chamberlain of Cygnus Support.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "winsup.h"

int
pipe (int filedes[2])
{
  return  fhandler_make_pipe (filedes);
}

int
dup (int fd)
{
  return dup2 (fd, u->self->hmap.find_unused_handle (0));
}

int
dup2 (int oldfd, int newfd)
{
  return u->self->hmap.dup2 (oldfd, newfd);
}



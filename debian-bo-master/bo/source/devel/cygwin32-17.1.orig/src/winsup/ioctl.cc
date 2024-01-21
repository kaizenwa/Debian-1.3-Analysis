/* ioctl routines. 

   Written by Doug Evans of Cygnus Support.
   dje@cygnus.com

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */



#include "winsup.h"
#include "sys/termios.h"

int
ioctl (int fd, int cmd, void *buf)
{
  if (NOT_OPEN_FD(fd))
    {
      set_errno (EBADF);
      return -1;
    }

return  u->self->hmap[fd].h->ioctl ( cmd, buf);
}

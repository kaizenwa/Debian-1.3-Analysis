/*  fpoll(f, msec) -- wait for data from input file f  */
/*  if msec>= 0, fail if no data after waiting that many milliseconds  */

/*  does not work for sockets under Iris; poll(2) always returns 1  */

#include <stdio.h>
#include <poll.h>

#include "icall.h"

int fpoll(int argc, descriptor *argv)
{
   FILE *f;
   int msec, r;
   struct pollfd pfd;

   /* check arguments */
   if (argc < 1)
      Error(105);
   if ((IconType(argv[1]) != 'f') || (FileStat(argv[1]) & Fs_Window))
      ArgError(1, 105);
   if (!(FileStat(argv[1]) & Fs_Read))
      ArgError(1, 212);
   f = FileVal(argv[1]);

   if (argc < 2)
      msec = -1;
   else {
      ArgInteger(2);
      msec = IntegerVal(argv[2]);
      }

   /* check for data already in buffer */
   if (f->_cnt > 0)
      RetArg(1);

   /* poll the file */
   pfd.fd = fileno(f);
   pfd.events = POLLIN | POLLHUP;
   r = poll(&pfd, 1, msec);

   if (r > 0)
      RetArg(1);			/* success */
   else if (r == 0)			
      Fail;				/* timeout */
   else
      ArgError(1, 214);			/* I/O error */
}

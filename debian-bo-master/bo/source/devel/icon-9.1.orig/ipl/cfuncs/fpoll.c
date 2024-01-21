/*
############################################################################
#
#	File:     fpoll.c
#
#	Subject:  Function to poll file for input
#
#	Author:   Gregg M. Townsend
#
#	Date:     October 3, 1995
#
############################################################################
#
#  fpoll(f, msec) waits until data is available for input from file f,
#  and then returns.  It also returns when end-of-file is reached.
#  If msec is specified, and no data is available after waiting that
#  many milliseconds, then fpoll fails.  If msec is omitted, fpoll
#  waits indefinitely.
#
#  fpoll does not work for sockets under SGI Iris due to OS limitations.
#  
############################################################################
#
#  Requires:  UNIX, dynamic loading
#
############################################################################
*/

#include <stdio.h>
#include <poll.h>

#include "icall.h"

int fpoll(int argc, descriptor *argv)	/*: await data from file */
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

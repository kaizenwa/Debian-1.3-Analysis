/*
 *       open.c open a vt to run a new command (or shell).
 *       
 *	 Copyright (c) 1994 by Jon Tombs <jon@gtex02.us.es>
 *
 *       This program is free software; you can redistribute it and/or
 *       modify it under the terms of the GNU General Public License
 *       as published by the Free Software Foundation; either version
 *       2 of the License, or (at your option) any later version.
 */

#include "open.h"

const char *SWITCHTOversion = "switchto: 1.0 (c) Jon Tombs 1994";


int 
main(int argc, char *argv[])
{

   int fd;
   int vtno     = -1;


   if (argc < 2) 
 	usage(1);

   vtno = (int) atol(argv[1]);

   if (vtno < 0 || vtno > 63)
	usage(2);

   if ((fd = open("/dev/console",O_WRONLY,0)) < 0) {
	perror("switchto: Can't open /dev/console\n");
	return(3);
   }
   

   if (ioctl(fd, VT_ACTIVATE, vtno) < 0) {
        fprintf(stderr, "switcho: Failed to select VT %d (%s)\n", vtno,
                strerror(errno));
	return(3);
   }
	
   /* wait to be really sure we have switched */
   (void) ioctl(fd, VT_WAITACTIVE, vtno);

   return 0;
}
      

void usage(int stat)
{
   fprintf(stderr,
      "Usage: switchto vt_num\n");
   exit (stat);
}



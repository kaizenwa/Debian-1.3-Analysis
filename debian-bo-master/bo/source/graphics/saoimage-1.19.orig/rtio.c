#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	rtio.c (Real Time Input Output)
 * Purpose:	Read commands from a Berkeley Socket
 * Subroutine:	open_rtsock_connection()	returns: void
 * Subroutine:	open_rtfifo_connection()	returns: void
 * Subroutine:	sock_output()			returns: int
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0] Initial version	John Roll : May 90
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, FILE, NULL, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>		/* strlen, etc. for unenlightened BSD's */
#endif
#else
#include <string.h>
#endif

#include <X11/Xlib.h>		/* X window stuff */
#include "hfiles/control.h"	/* define struct connectRec */
#include "hfiles/rtcmd.h"	/* command protocol and ID's */

extern struct controlRec control;


/*
 * Subroutine:	open_rtsock_connection
 * Purpose:	open the connection for Sockets
 */
void open_rtsock_connection ()
{
  int open_connection();
  void read_rtio_packet();

  control.aux_in.func = read_rtio_packet;
  control.aux_in.type = IOP_socket;

  if( open_connection(&control.aux_in) != -1 ) {
    if( control.verbose )
      (void)printf("Open to accept real time Socket io\n");
  }
}


/*
 * Subroutine:	open_rtfifo_connection
 * Purpose:	open the connection for FiFo
 */
void open_rtfifo_connection ()
{
  int open_connection();
  void read_rtio_packet();

  control.aux_in.func = read_rtio_packet;
  control.aux_in.type = IOP_pipe;

  if( open_connection(&control.aux_in) != -1 ) {
    if( control.verbose )
      (void)printf("Open to accept feal time FiFo io\n");
  }
}

/*
 * Subroutine:	read_rtio_packet
 * Purpose:	read and decode a socket protical packet
 * Called by:	
 * Returns:	void
 */
void read_rtio_packet ( port )
     struct connectRec *port;
{
		int	bytes;
		cmdrec	cmd;

    bytes = read_connection(port, (char *)&cmd, sizeof(cmdrec));

    if ( bytes != sizeof(cmdrec) )
	if ( bytes == 0 )
	    close_connection(port);
	else
	  flush_connection(port);


    si_command(&cmd);
}

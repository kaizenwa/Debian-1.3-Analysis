#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	ctrlpipe.c (Control Pipe Device)
 * Purpose:	Open a pipe device for IO
 * Subroutine:	open_pipe()	returns: int
 * Subroutine:	close_pipe()	returns: void
 * Subroutine:	flush_pipe()	returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  25 May 1989
 *		{1} MVH simplified code	for modular strategy	10 March 1990
 *		{n} <who> -- <does what> -- <when>
 */

#ifndef VMS

#include <stdio.h>		/* define stderr, FILE, NULL, etc */

/*
 * Subroutine:	open_pipe
 * Purpose:	Open a channel on a named device for pipe I/O
 * Returns:	Channel number on success (0-64) else -1.
 * Note:	select_mask and mask_size are augmented, not initialized
 */
int open_pipe ( device_name, write_flag, flush_flag )
     char *device_name;		/* i: name of pipe device to open */
     int write_flag;		/* i: >0: write only, <=0: read */
     int flush_flag;		/* i: 1=flush, 0=don't flush */
{
  int ipc;			/* o: channel of new pipe connection */
  int open_disk();
  void fcntl_disk(), flush_disk(), close_disk();

  if( write_flag > 0 ) {
    int datain;
    /* open file to read (don't block) */
    if( (datain = open_disk(device_name, -1, 1)) == -1 )
      return( -1 );
    /* open same file to write (don't block) */
    if( (ipc = open_disk(device_name, 1, 1)) == -1 ) {
      close_disk(datain, device_name);
      return( -1 );
    }
    /* now enable blocking and close the reading connection */
    fcntl_disk(ipc, 1, 0, device_name);
    close_disk(datain, device_name);
  } else {
    /* open the device READ-WRITE, don't block - time_out on failure */
    if( (ipc = open_disk(device_name, 0, 1)) == -1 )
      return( -1 );
    if( flush_flag )
      /* if reading, flush old input, if any, with non-blocking reads */
      flush_disk(ipc, device_name);
    /* from now on block on read or write  */
    fcntl_disk(ipc, -1, 0, device_name);
  }
  return( ipc );
}

/*
 * Subroutine:	close_pipe
 * Purpose:	Close pipe connection
 */
void close_pipe ( ipc, device_name )
     int ipc;			/* i: channel number of open pipe */
     char *device_name;		/* i: name of pipe device */
{
  void close_disk();

  close_disk(ipc, device_name);
}

/*
 * Subroutine:	flush_pipe
 * Purpose:	Suck all bytes out of a pipe open for reading
 */
void flush_pipe ( ipc, filename )
     int ipc;
     char *filename;
{
  void flush_disk();

  flush_disk(ipc, filename);
}

#endif

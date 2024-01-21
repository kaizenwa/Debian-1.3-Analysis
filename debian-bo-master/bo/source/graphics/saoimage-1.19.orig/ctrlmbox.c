#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	ctrlmbox.c (ControlMailbox)
 * Purpose:	Handle a VMS mailbox for IO
 * Subroutine:	open_mailbox()		returns: int
 * Subroutine:	close_mailbox()		returns: void
 * Subroutine:	flush_mailbox()		returns: void
 * Copyright:	1989, 1990 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Jay Travisano (STScI)	initial version   10 Nov 1989
 *		{1} MVH simplified for new connect module	10 March 1990
 *		{n} <who> -- <does what> -- <when>
 */

#ifdef VMS
	/*
	 * VMS -- Use VMS/IRAF Imtool driver (ZFIOVI)
	 */

#include <stdio.h>		/* define stderr, FILE, NULL, etc */
#include <sys/file.h>		/* define open */
#include <X11/Xlib.h>		/* X window stuff */
#include "hfiles/control.h"	/* declare control structure types */

#define READ_ONLY  1		/* IRAF SPP definitions */
#define READ_WRITE 2
#define WRITE_ONLY 3

/*
 * Subroutine:	open_mailbox
 * Purpose:	Open a VMS mailbox I/O
 * Returns:	Channel number on success else -1.
 */
int open_mailbox ( device_name, direction, flush_flag )
     char *device_name;		/* i: name of mailbox device */
     int direction;		/* i: IOP_Write, IOP_Read, IOP_ReadWrite */
     int flush_flag;		/* i: 1=flush, 0=don't flush */
{
  int ipc;		/* o: channel of new pipe connection */
  int mode;

  if( direction == IOP_Write )
    mode = WRITE_ONLY;
  else if( direction == IOP_Read )
    mode = READ_ONLY;
  else if( direction == IOP_ReadWrite )
    mode = READ_WRITE;
  else
    return( -1 );
  zopnvi(device_name, &mode, &ipc);
  if( ipc == -1 ) {
    (void)fprintf(stderr,"Warning: cannot open %s\n", device_name);
    return( -1 );
  }

#ifdef VMS_FLUSH_INPUT 
  /* Don't think we want to do this.  Mailboxes are pretty well-behaved,
   * and we should be able to pick up pending input if we are (re)started
   * after IRAF has sent data.
   */
  if( flush_flag && (mode != WRITE_ONLY) )
    /* if reading, flush old input, if any */
    flush_mailbox(ipc, 0, device_name);
#endif

  return( ipc );
}

/*
 * Subroutine:	close_mailbox
 * Purpose:	Close a mailbox connection
 * Returns:	0 on success else -1.
 */
int close_mailbox ( ipc, device_name )
     int ipc;			/* i: mailbox channel number */
     char *device_name;		/* i: name of mailbox device */
{
  int chan, status;

  chan = ipc;
  zclsvi(&chan, &status);
  if( status == -1 ) {
    (void)fprintf(stderr,"Warning: cannot close mailbox %s\n", device_name);
    return( -1 );
  } else {
    return( 0 );
  }
}

/*
 * Subroutine:	flush_mailbox
 * Purpose:	Suck all bytes out of a pipe open for reading
 */
void flush_mailbox ( ipc, device_name )
     int  ipc;			/* i: mailbox channel number */
     char *device_name;		/* [i]: name of mailbox device */
{
  int bytes, total;
  char buf[8192];
  int read_mailbox();

  total = 0;
  while( ZPending(ipc) ) {
    bytes = read_mailbox(ipc, buf, sizeof(buf), 0, device_name, "junk");
    total += bytes;
  }
#ifdef DEBUG
  if( total > 0 )
    (void)fprintf(stderr, "Flushed %d bytes from pipe\n", total);
#endif
}

/*
 * Subroutine:	read_mailbox
 * Purpose:	Read VMS mailbox
 * Returns:	Byte count, 0 on EOF, -1 on error
 */
int read_mailbox ( ipc, buf, bytes, report_error, device_name, detail )
     int  ipc;			/* i: mailbox channel number */
     char *buf;			/* i: address of buffer to receive data */
     int  bytes;		/* i: maximum number of bytes to read */
     int  report_error;		/* i: report under-count read */
     char *device_name;		/* [i]: name of mailbox device */
     char *detail;		/* [i]: details of operation */
{
  int	chan = ipc;
  int	maxbytes = bytes;
  int	offset = 0;
  int	status;

  zardvi(&chan, buf, &maxbytes, &offset);
  zawtvi(&chan, &status);

  if( report_error ) {
    if( status <= 0 ) {
      (void)fprintf(stderr, "Error in reading");
      if( detail != NULL )
	(void)fprintf(stderr, " %s", detail);
      if( device_name != NULL )
	(void)fprintf(stderr, " from %s", device_name);
      (void)fprintf(stderr, "\n");
      (void)fflush(stderr);
    } else {
      if (status < maxbytes) {
	(void)fprintf(stderr, "Expected %d bytes, read %d\n", 
		      maxbytes, status);
	(void)fflush(stderr);
      }
    }
  }
  return( status );
}

/*
 * Subroutine:	write_mailbox
 * Purpose:	Write to VMS mailbox
 * Returns:	Byte count, 0 on EOF, -1 on error
 */
int write_mailbox ( ipc, buf, bytes, device_name )
     int  ipc;			/* i: mailbox channel number */
     char *buf;			/* i: address of buffer to write */
     int  bytes;		/* i: number of bytes to write */
     char *device_name;		/* i: name of mailbox device */
{
  int   chan = ipc;
  int	nbytes = bytes;
  int	offset = 0;
  int	status;

  zawrvi(&chan, buf, &nbytes, &offset);
  zawtvi(&chan, &status);

  if( status < bytes )
    (void)fprintf(stderr, "Write error on %s; wrote %d of %d bytes\n",
		  device_name, status, bytes);
  return( status );
}
#endif

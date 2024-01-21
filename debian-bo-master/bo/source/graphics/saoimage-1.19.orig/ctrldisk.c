#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	ctrldisk.c (Control Disk Access)
 * Purpose:	Open, close, read, and write disk files and streams.
 *		Centralized to facilitate selecting system specific code.
 * Subroutine:	open_disk()		returns: int
 * Subroutine:	fcntl_disk()		returns: void
 * Subroutine:	flush_disk()		returns: void
 * Subroutine:	lseek_disk()		returns: int
 * Subroutine:	read_disk()		returns: int
 * Subroutine:	write_disk()		returns: int
 * Subroutine:	close_disk()		returns: void
 * UNIX calls:	open(), fcntl(), lseek(), read(), write(), close()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  6 Oct 1989
 *		{1} Jay Travisano (STScI)  VMS changes           10 Nov 1989
 *		{2} Doug Mink  blocking set to 0 in flush_disk   18 Oct 1995
 *		{n} <who> -- <does what> -- <when>
 */

#ifndef VMS

#include <stdio.h>		/* define stderr, FILE, NULL, etc */
#include <sys/file.h>		/* define open */
#include <fcntl.h>		/* define fcntl */

/*
 * Subroutine:	open_disk
 * Returns:	File descriptor of open file (suitable for read())
 */
int open_disk ( filename, write_flag, no_block )
     char *filename;
     int write_flag;	/* i: 1=WriteOnly, 0=ReadWrite, -1=ReadOnly */
     int no_block;	/* i: flag to set non-blocking flag */
{
  int flags;
  int fd;

  if( write_flag > 0 ) {
    flags = O_WRONLY;
    if( write_flag > 1 )
      /* create file if it doesn't exist, else condition causes error */
      flags |= O_CREAT;
  } else {
    if( write_flag < 0 )
      flags = O_RDONLY;
    else
      flags = O_RDWR;
  }
  if( no_block )
    flags |= O_NDELAY;
  if( (fd = open(filename, flags, 0)) == -1 ) {
    perror("open error");
    (void)fprintf(stderr, "ERROR: unable to open %s\n", filename);
    (void)fflush(stderr);
  }
  return( fd );
}

/*
 * Subroutine:	fcntl_disk
 * Purpose:	Change an open file's flags, check for errors
 */
void fcntl_disk ( fd, write_flag, no_block, filename )
     int fd;		/* i: already open file descriptor */
     int write_flag;	/* i: 1=WriteOnly, 0=ReadWrite, -1=ReadOnly */
     int no_block;	/* i: include non-blocking flag */
     char *filename;	/* [i]: name of file being accessed */
{
  int flags;

  if( write_flag > 0 ) {
    flags = O_WRONLY;
  } else {
    if( write_flag < 0 )
      flags = O_RDWR;
    else
      flags = O_RDONLY;
  }
  if( no_block )
    flags |= O_NDELAY;
  if( fcntl(fd, F_SETFL, flags) == -1) {
    perror("fcntl error");
    if( filename != NULL ) {
      (void)fprintf(stderr, "Error changing flags on %s\n", filename);
      (void)fflush(stderr);
    }
  }
}

/*
 * Subroutine:	flush_disk
 * Purpose:	Read to functional end of file (useful with dynamic IO)
 */
void flush_disk ( fd, filename )
     int fd;
     char *filename;
{
  int flags, tflags;
  int blocking;
  int bytes, total;
  char buf[4096];

  /* get status of device */
  if( (flags = fcntl(fd, F_GETFL, 0)) == -1 ) {
    (void)fprintf(stderr, "Error determining flag status on: %s\n", filename);
    perror("fcntl error");
  }
  /* clear blocking if device would block on read */
  blocking = 0;
  if( (flags & O_NDELAY) == 0 ) {
    blocking = 1;
    tflags = flags & (~O_NDELAY);
    if( fcntl(fd, F_SETFL, tflags) == -1) {
      (void)fprintf(stderr,
		    "Error attempting to clear blocking on %s\n", filename);
      perror("fcntl error");
      return;
    }
  }
  total = 0;
  while( (bytes = read(fd, buf, 4096)) > 0 )
    total += bytes;
#ifdef DEBUG
  if( bytes < 0 ) {
    /* though non-blocking, some OS return "Operation would block" error */
    (void)fprintf(stderr, "Error attempting to flush %s\n", filename);
    perror("read error");
  }
  if( total > 0 )
    (void)fprintf(stderr, "Flushed %d bytes from pipe\n", total);
#endif
  /* restore device to oriiginal status if needed */
  if( blocking ) {
    if( fcntl(fd, F_SETFL, flags) == -1 ) {
      (void)fprintf(stderr,
		    "Error attempting to restore blocking on %s\n", filename);
      perror("fcntl");
      return;
    }
  }
}

/*
 * Subroutine:	lseek_disk
 * Purpose:	Skip into a disk file before reading, check for errors
 */
int lseek_disk ( fd, nbytes, filename )
     int fd;
     int nbytes;
     char *filename;		/* [i]: name of file being accessed */
{
  long lseek();			/* type not declared in <sys/file.h> */

  if( lseek(fd, (long)nbytes, L_SET) < 0 ) {
    perror("lseek");
    (void)fprintf(stderr, "Can't lseek %d bytes", nbytes);
    if( filename != NULL )
      (void)fprintf(stderr, "into file %s\n", filename);
    else
      (void)fprintf(stderr, "\n");
    (void)fflush(stderr);
    return( -1 );
  }
  return( 0 );
}

/*
 * Subroutine:	read_disk
 * Purpose:	Read bytes from a disk file, check for errors
 * Returns:	Number of bytes read, or error flag (-1)
 * Note:	If report_error is 0, reading fewer than nbytes
 *		is not treated as an error.
 * Note:	BSD4.3 interrupts reads on any signal.  Thus reads may
 *		be broken into (hopefully consecutive) pieces.
 */
int read_disk ( fd, buf, nbytes, report_error, filename, detail )
     int fd;
     char *buf;			/* i: buffer to receive read data */
     int nbytes;		/* i: number of bytes expected */
     int report_error;		/* i: report under-count read */
     char *filename;		/* [i]: name of file being read */
     char *detail;		/* [i]: "data", "header", etc */
{
  int got_this_read, got_so_far, left_to_get;
  int zero = 0;	/* Count of times with 0 bytes */

  for( got_so_far = 0; got_so_far < nbytes; got_so_far += got_this_read ) {
    left_to_get = nbytes - got_so_far;
    if( (got_this_read = read(fd, &(buf[got_so_far]), left_to_get) )
        != left_to_get ) {
      if( (got_this_read <= 0) &&
	  ((got_this_read < 0) || (++zero > 3)) ) {
	/* Unfortunately, we can't distinguish between a non-blocking ..  *
	 *  connection, and a read that was interrupted at the start. ... *
	 *  We assume the former is more likely and dangerous.		  *
	 * Lest we fall into an endless loop, allow only 3 0-byte reads.  */
	if( got_this_read < 0 )
	  perror("read error");
	if( report_error ) {
	  (void)fprintf(stderr, "Error in reading");
	  if( detail != NULL )
	    (void)fprintf(stderr, " %s", detail);
	  if( filename != NULL )
	    (void)fprintf(stderr, " from %s", filename);
	  (void)fprintf(stderr, "\n");
	  (void)fflush(stderr);
	  if( report_error && (got_so_far >= 0) ) {
	    (void)fprintf(stderr, "Expected %d bytes, read %d\n",
			  nbytes, got_so_far);
	    (void)fflush(stderr);
	  }
	}
	return got_so_far;
      }
    }
  }
  return got_so_far;
}

/*
 * Subroutine:	write_disk
 * Purpose:	Write data to the open disk file or stream
 */
int write_disk ( fd, buf, nbytes, filename )
     int fd;
     char *buf;
     int nbytes;
     char *filename;
{
  int gave;

  if( (gave = write(fd, buf, nbytes)) < nbytes ) {
    /* if interrupted (or non-blocking reader), try the rest again */
    if( gave > 0 ) {
      /* I haven't tested this way of handling the situation with IRAF */
      nbytes -= gave;
      buf += gave;
      if( (gave = write(fd, buf, nbytes)) == nbytes )
	return( 0 );
    }
    perror(filename);
    return( -1 );
  }
  return( 0 );
}
    
/*
 * Subroutine:	close_disk
 * Purpose:	Close a disk file
 */
void close_disk ( fd, filename )
     int fd;
     char *filename;
{
#ifdef DEBUG
  if( close(fd) == -1 ) {
    perror("close error");
    if( filename != NULL ) {
      (void)fprintf(stderr, "Error closing %s\n", filename);
      (void)fflush(stderr);
    }
  }
#else
  (void)close(fd);
#endif
}


#else


/*
 * VMS versions of disk functions.  This implementation of these functions
 * for VMS uses the standard C library calls, just as on Unix.  Although
 * this is the easiest approach for now, there are some problems:
 *
 *	Performance	The VAX/VMS C library functions dealing with
 *			file i/o have never been very efficient.  Better
 *			performance (with some loss of generality, of
 *			course) can be gained by using RMS system routines.
 *			The VMS_OPEN_OPTIONS below should help, though.
 *
 *	VMS Files	VMS supports a number of different file types,
 *			which are sometimes handled differently by the
 *			the C library functions.  For example, a read()
 *			operation on a file with a record size of 512
 *			bytes will return a maximum of 512 bytes, regardless
 *			of the number requested in the call.  Thus, for
 *			these functions, we must loop around until all the
 *			i/o requested has been performed.
 *
 *	Seeking		Depending on the type of file being accessed, the
 *			operation of seeking to a byte offset in a file
 *			is somewhat unpredictable.  For stream files, it
 *			is usually okay, but for record files, only byte
 *			offsets that are on a record boundary are allowed.
 *			(As a result of this behavior, the -skip and -header
 *			command line options may not always perform as
 *			expected on VMS.)
 *
 * Note:  perror(NULL) will not work on VMS; some character string must
 *	be specified, even if just "".
 */

#define VMS_OPEN_OPTIONS "mbc=32","mbf=4","rop=RAH"

#include <stdio.h>		/* define stderr, FILE, NULL, etc */
#include <sys/file.h>		/* define open */

/*
 * Subroutine:	open_disk
 * Returns:	File descriptor of open file (suitable for read())
 */
int open_disk ( filename, write_flag, no_block )
     char *filename;
     int write_flag;	/* i: 1=WriteOnly, 0=ReadWrite, -1=ReadOnly */
     int no_block;	/* i: flag to set non-blocking flag */
{
  int flags;
  int fd;

  if( write_flag > 0 ) {
    flags = O_WRONLY;
    if( write_flag > 1 )
      /* create file if it doesn't exist, else condition causes error */
      flags |= O_CREAT;
  } else {
    if( write_flag < 0 )
      flags = O_RDONLY;
    else
      flags = O_RDWR;
  }
  if( no_block )
    flags |= O_NDELAY;
  if( (fd = open(filename, flags, 0, VMS_OPEN_OPTIONS)) == -1 ) {
    (void)fprintf(stderr, "ERROR: unable to open %s\n", filename);
    fflush(stderr);
    perror("open error");
  }
  return( fd );
}

/*
 * Subroutine:	fcntl_disk
 * Purpose:	Change an open file's flags, check for errors
 */
void fcntl_disk ( fd, write_flag, no_block, filename )
     int fd;			/* i: already open file descriptor */
     int write_flag;		/* i: 0=read, else write */
     int no_block;		/* i: include non-blocking flag */
     char *filename;		/* [i]: name of file being accessed */
{
	/* fcntl() not supported on VMS */
}

/*
 * Subroutine:	lseek_disk
 * Purpose:	Skip into a disk file before reading, check for errors
 */
int lseek_disk ( fd, nbytes, filename )
     int fd;
     int nbytes;
     char *filename;		/* [i]: name of file being accessed */
{
  long lseek();			/* type not declared in <sys/file.h> */

  if( lseek(fd, (long)nbytes, SEEK_SET) < 0 ) {
    (void)fprintf(stderr, "Can't lseek %d bytes", nbytes);
    if( filename != NULL )
      (void)fprintf(stderr, "into file %s\n", filename);
    else
      (void)fprintf(stderr, "\n");
    fflush(stderr);
    perror("lseek error");
    return( -1 );
  }
  return( 0 );
}

/*
 * Subroutine:	read_disk
 * Purpose:	Read bytes from a disk file, check for errors
 * Returns:	Number of bytes read, or error flag (-1)
 * Note:	If report_error is 0, reading fewer than nbytes
 *		is not treated as an error.
 */
int read_disk ( fd, buf, nbytes, report_error, filename, detail )
     int fd;
     char *buf;			/* i: buffer to receive read data */
     int nbytes;		/* i: number of bytes expected */
     int report_error;		/* i: report under-count read */
     char *filename;		/* [i]: name of file being read */
     char *detail;		/* [i]: "data", "header", etc */
{
	/* Note: On VMS, read() will return the number of bytes requested
	 * or the logical record size, whichever is less.  Loop around
	 * until we get everything we want.
	 */
  int got, count=0;

  while (count < nbytes) {
    got = read(fd, (buf+count), (nbytes-count));
    if( got < 0 ) {
      (void)fprintf(stderr, "Error in reading");
      if( detail != NULL )
	(void)fprintf(stderr, " %s", detail);
      if( filename != NULL )
	(void)fprintf(stderr, " from %s", filename);
      (void)fprintf(stderr, "\n");
      fflush(stderr);
      perror("read error");
      return( got );
    } else if( got == 0 ) {			/* reached EOF ? */
      if( count < nbytes && report_error ) {
        (void)fprintf(stderr, "Expected %d bytes, read %d\n", nbytes, count);
        fflush(stderr);
      }
      break;
    } else {
      count += got;
    }
  }
  return( count );
}

/*
 * Subroutine:	write_disk
 * Purpose:	Write data to the open disk file or stream
 */
void write_disk ( fd, buf, nbytes, filename )
     int fd;
     char *buf;
     int nbytes;
     char *filename;
{
	/* Note: On VMS, write() may write the number of bytes requested
	 * or the logical record size, whichever is less.  Loop around
	 * until we write everything we want.
	 */
  int gave, count=0;

  while (count < nbytes) {
    gave = write(fd, (buf+count), (nbytes-count));
    if( gave < 0 ) {
      perror (filename);
      break;
    } else {
      count += gave;
    }
  }
}
    
/*
 * Subroutine:	close_disk
 * Purpose:	Close a disk file
 */
void close_disk ( fd, filename )
     int fd;
     char *filename;
{
  int close();

  if( close(fd) == -1 ) {
    if( filename != NULL ) {
      (void)fprintf(stderr,"Error closing %s\n", filename);
      fflush(stderr);
    }
    perror("close error");
  }
}

#endif

                                     
                                                               
                                                               
                                                               
                                                              

                                                               
                                                               
                                                               
                                                               
             

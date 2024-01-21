/*
 * File:	io.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	11 Aug 95   Framstag	initial version
 * 		23 Apr 96   Framstag	added file copying function
 * 		 2 May 96   Framstag	corrected file creating bug in fcopy()
 * 		24 Jun 96   Framstag	enhanced fcopy() to copy to stdout
 *
 * Socket read and write routines and file copy function of the
 * sendfile package.
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "io.h"			/* (socket) read/write */
#include "net.h"		/* the network routines */
#include "string.h"		/* extended string functions */
#include "config.h"		/* various #defines */
#include "message.h"		/* information, warning and error messages */

#ifdef NEXT
  #include <sys/uio.h>
#endif

#ifdef LINUX
  int fileno(FILE *);
#endif


/*
 * readn - read n bytes from network socket
 *
 * INPUT:  fd     - socket file descriptor
 *         ptr    - empty string
 *         nbytes - number of bytes to read
 *
 * RETURN: number of actual read bytes
 *
 * this function is derived from example code from
 * "Unix Networking Programming" by W. R. Stevens
 */
int readn(int fd, char *ptr, int nbytes)
{ int nleft, nread;

  nleft=nbytes;
  while (nleft>0)
  { nread=read(fd, ptr, nleft);
    if (nread<0)
      return(nread);
    else
      if (nread==0) break;
    nleft-=nread;
    ptr+=nread;
  }
  return(nbytes-nleft);
}


/*
 * writen - write n bytes to network socket
 *
 * INPUT:  fd     - socket file descriptor
 *         ptr    - string to send
 *         nbytes - number of bytes to send
 *
 * RETURN: number of actual written bytes
 *
 * this function is derived from example code from
 * "Unix Networking Programming" by W. R. Stevens
 */
int writen(int fd, char *ptr, int nbytes)
{ int nleft, nwritten;

  nleft=nbytes;
  while (nleft>0)
  { nwritten=write(fd, ptr, nleft);
    if (nwritten<0) return(nwritten);
    nleft-=nwritten;
    ptr+=nwritten;
  }
  return(nbytes-nleft);
}


/*
 * fcopy - copy a file (copy to stdout if there is no destination filename)
 *
 * INPUT:  from     - source file
 *         to       - destination file
 *         mode     - file protection mode
 *
 * RETURN: 0 on success, -1 on failure
 *
 */
int fcopy(const char *from, const char *to, mode_t mode)
{ int fdin, fdout;	/* file descriptor in/out */
  int bytes;		/* read bytes */
  unsigned long blksize;/* file system block size */
  char tmp[MAXLEN],	/* temporary string */
       *buf;		/* copy buffer */
  struct stat finfo;	/* information about a file */

  /* open source file */
  fdin=open(from,O_RDONLY,0);
  if (fdin<0)
  { sprintf(tmp,"error opening '%s'",from);
    message("",'E',tmp);
    return(-1);
  }

  /* destination file specified? */
  if (*to)
  { 
    /* open destination file */
    fdout=creat(to,mode);
    if (fdout<0)
    { sprintf(tmp,"error creating '%s'",to);
      message("",'E',tmp);
      close(fdin);
      return(-1);
    }

    /* get file system block size for copy operation */
    stat(to,&finfo);
    blksize=finfo.st_blksize;

    /* ANSI C can not dynamicly allocate with buf[blksize] */
    buf=(char *)malloc(blksize);
    if (!buf) message("",'F',"out of memory");

    /* read until EOF */
    while ((bytes=read(fdin,buf,blksize)) > 0)
    {
      /* write to destination file */
      if (write(fdout,buf,bytes)<0)
      {
	/* write error */
	close(fdin);
	close(fdout);
	free(buf);
	sprintf(tmp,"error writing '%s'",to);
	message("",'E',tmp);
	return(-1);
      }
    }

    close(fdout);

  }
  else /* copy to stdout */
  { 
    /* get file system block size for copy operation */
    stat(from,&finfo);
    blksize=finfo.st_blksize;

    /* ANSI C can not dynamicly allocate with buf[blksize] */
    buf=(char *)malloc(blksize);
    if (!buf) message("",'F',"out of memory");

    /* read until EOF */
    while ((bytes=read(fdin,buf,blksize)) > 0)
    {
      /* write to stdout */
      write(fileno(stdout),buf,bytes);
    }

  }

  close(fdin);
  free(buf);

  /* read error? */
  if (bytes<0)
  { sprintf(tmp,"error reading '%s'",from);
    message("",'E',tmp);
    return(-1);
  }

  return(0);
}

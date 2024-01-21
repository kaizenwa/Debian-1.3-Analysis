/*
 * Portions Copyright (C) 1994 The Santa Cruz Operation, Inc.
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software
 * for any purpose is hereby granted without fee, provided that the 
 * above copyright notice and this notice appear in all copies
 * and that both the copyright notice and this notice appear in
 * supporting documentation.  SCO makes no representations about
 * the suitability of this software for any purpose.  It is provided
 * "AS IS" without express or implied warranty.
 * 
 * SCO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
 * INCLUDING ALL IMPLIED WARRANTIES OR MERCHANTABILITY AND FITNESS.  
 * IN NO EVENT SHALL SCO BE LIABLE FOR ANY SPECIAL, INDIRECT, 
 * PUNITIVE, CONSEQUENTIAL OR INCIDENTAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF USE, LOSS OF DATA OR LOSS OF
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER 
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE.
 *
 *	THIS COPYRIGHT NOTICE DOES NOT APPLY TO THE FUNCTION
 *	spipe() WHICH WAS OBTAINED FROM:
 *
 *	Stevens, W. Richards.  UNIX Network Programming, Prentice-Hall,
 *	1990.  [If you program on UNIX, you want this book.  Really.
 *	One of the best. (tom@sco.com)]
 *		
 */

#define I_IOCTL
#define I_STROPT
#define I_FCNTL
#define I_ERRNO
#define I_TYPES
#define I_STREAM
#define I_STRING
#include "includes.h"

#if defined(STREAMS_PIPE)
#define SPX_DEVICE	"/dev/spx"

/*
 *	Create an un-named stream pipe to replace a socketpair.
 *	From Stevens: UNIX Network Programming.  This is not
 *	covered by SCO copyright.
 */

int
s_pipe(int fd[2])
{
	struct strfdinsert ins;
	queue_t	*pointer;

	/* open clone device /dev/spx twice */

	if ((fd[0] = open(SPX_DEVICE, O_RDWR)) < 0)
		return -1;
	if ((fd[1] = open(SPX_DEVICE, O_RDWR)) < 0) {
		x__close(fd[0]);
		return -1;
	}

	/* link streams together with I_FDINSERT */

	ins.ctlbuf.buf = (char *) &pointer;
	ins.ctlbuf.maxlen = sizeof(queue_t *);
	ins.ctlbuf.len = sizeof(queue_t *);

	ins.databuf.buf = (char *) 0;
	ins.databuf.len = -1;	/* magic -1 for streams pipes */
	ins.databuf.maxlen = 0;

	ins.fildes = fd[1];	/* the fd to connect */
	ins.flags = 0;
	ins.offset = 0;

	if (ioctl(fd[0], I_FDINSERT, (char *) &ins) < 0) {
		x__close(fd[0]);
		x__close(fd[1]);
		return -1;
	}
	return 0;
}

/*
 *	Code adapted from SCO X-server to do IPC over
 *	streams pipes.  Thanks SCO.
 */

/*
 *	This is the server side code
 */


#define X_UNIX_PATH	"/dev/X"

/* 
 *	This is executed by a server to establish a streams pipe
 *	on which connection requests can be made.
 *
 *	The parameter tells which device to use.  For X, it
 *	is the display number.  When term is configured for
 *	streams pipes, we "borrow" one of the X devices ...
 *
 */

int
open_stream_pipe (int number)
{
	struct flock mylock;		/* Used for fcntl F_SETLK  S012 */
	struct strfdinsert fdins;	/* Used in FDINSERT ioctl's */
	int connmaster;			/* Master pipe for making connections */
	int connother;			/* Other end of connmaster */
	char mybuf[sizeof (struct file *)]; /* Buffer for FDINSERT message */
	char strnamebuf[sizeof X_UNIX_PATH + 5];
  
  	/*
  	 * The server creates both ends of a stream pipe on two special
	 * minor devices of the stream pipe driver.  One of these will be the
	 * master connection, which is the value we return.  The other we
	 * simply forget;  we will hold it open as long as the server runs.
	 *
	 * The name of the special minor device is "/dev/X<n>[RS]", where
	 * <n> is the input parameter.  R is the request device (which is
	 * written to open a connection);  S is the server's end of the
	 * request device.
	 */

	sprintf(strnamebuf,"%s%dS", X_UNIX_PATH, number);

	connmaster = open (strnamebuf, O_RDWR | O_NDELAY);
	if (connmaster < 0) {
	    fprintf(stderr, "Can't open %s\n", strnamebuf);
	    return -1;
	}

	/* S012
	 * Lock the master connection device.
	 * This is the atomic operation that resolves
	 * duplicate server startup races.
	 * The lock will automagically disappear
	 * whenever the master is closed.
	 */
	mylock.l_type   = F_WRLCK;
	mylock.l_whence = 0;
	mylock.l_start  = 0;	/* lock entire "file" */
	mylock.l_len    = 0;
	if (x__fcntl (connmaster, F_SETLK, &mylock) < 0) {
	    if (errno == EACCES) {
		fprintf(stderr, "streams pipe %s is busy\n", strnamebuf);
	    } else {
		fprintf(stderr, "cannot lock %s\n", strnamebuf);
	    }
	    x__close (connmaster);
	    return -1;
	}

	sprintf(strnamebuf,"%s%dR", X_UNIX_PATH, number);

	connother = open (strnamebuf, O_RDWR | O_NDELAY);
	if (connother < 0) {
	    fprintf(stderr, "Can't open %s\n", strnamebuf);
	    x__close (connmaster);
	    return -1;
	}
	fdins.ctlbuf.maxlen = sizeof mybuf;
	fdins.ctlbuf.len = sizeof mybuf;
	fdins.ctlbuf.buf = mybuf;
	fdins.databuf.maxlen = 0;
	fdins.databuf.len = -1;
	fdins.databuf.buf = NULL;
	fdins.fildes = connother;
	fdins.offset = 0;
	fdins.flags = 0;

	if (ioctl (connmaster, I_FDINSERT, &fdins) < 0) {
	    fprintf(stderr, "cannot pass file descriptor\n");
	    x__close (connmaster);
	    x__close (connother);
	    return -1;
	}
	return connmaster;
}

/*
 *	This code (in the server) adds a connection to a new client.
 *	The client has already sent a byte on the "R" pipe, which the
 *	server has noticed.
 */

addclient (int connmaster)			/* Add connection to new slave */
{
	struct strfdinsert	fdins;	/* Used for FDINSERT on master */
	int			fd;	/* FD of new connection */
	char			mybuf[sizeof (struct file *)];

	if (read (connmaster, &fd, 1) != 1)	/* Read the dummy byte */
	    return -1;
	fd = open (SPX_DEVICE, O_RDWR);
	if (fd < 0) {
	    /* send zero-length msg to client to signal error */
	    fdins.ctlbuf.buf = mybuf;
	    fdins.ctlbuf.len = 0;
	    putmsg (connmaster, &fdins.ctlbuf, NULL, 0);
	    return -1;
	}

	fdins.ctlbuf.maxlen = sizeof mybuf;
	fdins.ctlbuf.len = sizeof mybuf;
	fdins.ctlbuf.buf = mybuf;
	fdins.databuf.maxlen = 0;
	fdins.databuf.len = -1;
	fdins.databuf.buf = NULL;
	fdins.fildes = fd;
	fdins.offset = 0;
	fdins.flags = 0;

	if (ioctl (connmaster, I_FDINSERT, &fdins) < 0) {
	    x__close (fd);
	    return -1;
	}
	return fd;
}


/*
 *	This is the client side code.  "number" is the X
 *	display number to use (or borrow).
 */

int MakeStreamPipeConnection (int number)
{
	int errsave;			/* Place to save errno if trouble */
	int flags;			/* Flags to getmsg call */
	int mfd;			/* Fd to talk to master */
	char mybuf[sizeof (struct file *)]; /* Buffer for linkup message */
	struct strbuf myctlbuf;		/* Control reception buffer */
	int retfd;			/* Resulting fd to talk on */
	static char strnamebuf[sizeof X_UNIX_PATH + 5];

	/*
	 * The server creates both ends of a stream pipe on two special
	 * minor devices of the stream pipe driver.  One of these is the
	 * connection we will make our request on.
	 */

	sprintf(strnamebuf,"%s%dR",X_UNIX_PATH,number);

	mfd = open (strnamebuf, O_RDWR);
	if (mfd < 0) {
	    x__perror(strnamebuf);
	    return -1;
	}

	retfd = open (SPX_DEVICE, O_RDWR);
	if (retfd < 0) {
	    x__perror(SPX_DEVICE);
	    errsave = errno;
	    x__close (mfd);
	    errno = errsave;
	    return -1;
	}

	if (write (mfd, (char *)&mfd, 1) != 1) { /* Ask for a connection */
	    errsave = errno;
	    x__close (retfd);
	    x__close (mfd);
	    errno = errsave;
	    return -1;
	}

	myctlbuf.maxlen = sizeof (mybuf);
	myctlbuf.buf = mybuf;
	flags = 0;

	/*
	 *	Get the file descriptor sent by the server
	 */

	if (getmsg (mfd, &myctlbuf, (struct strbuf *) NULL, &flags) < 0) {
	    errsave = errno;
	    x__close (retfd);
	    x__close (mfd);
	    errno = errsave;
	    return -1;
	}

	/*
	 *	Connect our endpoint to the server's endpoint
	 */

	if (putmsg (retfd, &myctlbuf, (struct strbuf *) NULL, 0) < 0) {
	    errsave = errno;
	    x__close (retfd);
	    x__close (mfd);
	    errno = errsave;
	    return -1;
	}
	x__close (mfd);
	return retfd;
}

int
CheckClientConnection(int sock)
{
	char dumb;
	int s;

	if (ioctl(sock, FIONREAD, &dumb) == -1) {
		if ((s = addclient(sock)) < 0) {
			return -1;	/* couldn't add client */
		} else
			return s;
	} else
		return -2;		/* not a streams pipe */
}

#else
/* This keeps "ranlib" from complaining */
int s_pipe(int fd[2]){return -1;};
#endif

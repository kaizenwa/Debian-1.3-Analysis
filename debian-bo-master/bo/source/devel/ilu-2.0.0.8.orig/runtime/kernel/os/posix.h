/*
 * posix.h -- POSIX definitions for system calls
 */
/* $Id: posix.h,v 1.7 1995/10/24 23:24:47 spreitze Exp $ */
/* Last edited by Mike Spreitzer October 24, 1995 4:24 pm PDT */

#include <unistd.h>

#define OS_READ			read	/* mainloop.c */
#define OS_WRITE		write	/* mainloop.c */

#define OS_SLEEP		sleep	/* memory.c */

#define OS_GETPID		getpid	/* bsdutils.c */

#define OS_UNAME		uname	/* bsdutils.c */

#define OS_UNLINK		unlink	/* simpbind.c */
#define OS_ACCESS		access	/* simpbind.c */
#define OS_ACCESS_R_OK		R_OK	/* simpbind.c */
#define OS_ACCESS_W_OK		W_OK	/* simpbind.c */

#include <sys/types.h>
#include <sys/stat.h>

#define OS_CHMOD		chmod	/* simpbind.c */
#define OS_CHMOD_S_IWUSR	S_IWUSR	/* simpbind.c */
#define OS_CHMOD_S_IRUSR	S_IRUSR	/* simpbind.c */
#define OS_CHMOD_S_IWGRP	S_IWGRP	/* simpbind.c */
#define OS_CHMOD_S_IRGRP	S_IRGRP	/* simpbind.c */
#define OS_CHMOD_S_IWOTH	S_IWOTH	/* simpbind.c */
#define OS_CHMOD_S_IROTH	S_IROTH	/* simpbind.c */


#define OS_SOCKIOCTL(fd,req,arg)	ioctl(fd,req,(int)arg)	/* tcp.c, udp.c */
/* Third arg of ioctl is declared as int, but we pass pointers; cf win.h */

#define OS_ACCEPT(fd,adr,len)	accept(fd,adr,len)	/* tcp.c */

#define OS_SOCKINV(x) ((x) < 0)	/* tcp.c, udp.c */
/*
 * Call this on result/errcode to test whether a fd-returning
 * sockets call is raising an error
 */

#define OS_SOCKERR(x) ((x) < 0)	/* tcp.c, udp.c */
/*
 * Call this on result/errcode to test whether a non-fd-returning
 * sockets call is raising an error
 */

#define OS_SOCKLOSE(x)	close(x)	/* tcp.c, udp.c */
/* Call this to close an FD for a socket */

#define SOCKERRID(x)	(E##x)
#define sockerrno	errno

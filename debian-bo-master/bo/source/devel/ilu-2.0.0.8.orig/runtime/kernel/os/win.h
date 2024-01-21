/*
 * win.h -- Windows definitions for system calls
 */
/* $Id: win.h,v 1.5 1995/10/24 23:24:47 spreitze Exp $ */
/* Last edited by Mike Spreitzer October 24, 1995 4:24 pm PDT */

// dll - <unistd.h> doesn't exist for VC++
#include <io.h>
#include <stdio.h>
#include <sys/stat.h>
#define OS_SLEEP		_sleep	/* memory.c */
#define OS_READ			_read	/* mainloop.c */
#define OS_WRITE		_write	/* mainloop.c */
#define OS_GETPID		_getpid	/* bsdutils.c */
#define OS_UNLINK		_unlink	/* simpbind.c */
#define OS_ACCESS		_access	/* simpbind.c */
#define OS_CHMOD		_chmod	/* simpbind.c */
#define OS_ACCESS_R_OK		0x04	/* simpbind.c */
#define OS_ACCESS_W_OK		0x02	/* simpbind.c */
#include <sys/types.h>
#include <sys/stat.h>
// xxx - dll - define these as all just read or write for now until
// determine what to do regarding NT ACLs - also, we may be on a FAT
// volume anyway, where there is no file ownership anyhow
#define OS_CHMOD_S_IWUSR	_S_IWRITE	/* simpbind.c */
#define OS_CHMOD_S_IRUSR	_S_IREAD	/* simpbind.c */
#define OS_CHMOD_S_IWGRP	_S_IWRITE	/* simpbind.c */
#define OS_CHMOD_S_IRGRP	_S_IREAD	/* simpbind.c */
#define OS_CHMOD_S_IWOTH	_S_IWRITE	/* simpbind.c */
#define OS_CHMOD_S_IROTH	_S_IREAD	/* simpbind.c */

#define OS_UNAME		uname	/* bsdutils.c */


#define OS_SOCKIOCTL(fd,req,arg)	ioctlsocket(fd,req,arg)	/* tcp.c, udp.c */
/* Larner removed casts to int from third arg in his WIN port */

#define OS_ACCEPT(fd,adr,len)	accept(fd,adr,len)	/* tcp.c */

#define OS_SOCKINV(x) ((x) == INVALID_SOCKET)	/* tcp.c, udp.c */
/*
 * Call this on result/errcode to test whether a fd-returning
 * sockets call is raising an error
 */

#define OS_SOCKERR(x) ((x) == SOCKET_ERROR)	/* tcp.c, udp.c */
/*
 * Call this on result/errcode to test whether a non-fd-returning
 * sockets call is raising an error
 */

#define OS_SOCKLOSE(x)	closesocket(x)	/* tcp.c, udp.c */
/* Call this to close an FD for a socket */

#define SOCKERRID(x)	(WSAE##x)
#define sockerrno	WSAGetLastError()

/*
 * send and receive file descriptors between processes on a single machine
 * (via Unix domain socket on 4.3BSD)
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>		/* defines struct msghdr */
#include <sys/uio.h>		/* defines struct iovec */

int
sendfd(medium, fd)
int medium;			/* send `fd' over this socket */
int fd;
{
	struct iovec iov[1];
	struct msghdr msg;
	char byte = 0;

	iov[0].iov_base = &byte;
	iov[0].iov_len = sizeof byte;	/* must send at least one byte */
	msg.msg_iov = iov;
	msg.msg_iovlen = sizeof iov / sizeof iov[0];
	msg.msg_name = NULL;
	msg.msg_namelen = 0;
	msg.msg_accrights = (caddr_t)&fd;
	msg.msg_accrightslen = sizeof fd;
	/* send fd over medium; 0 is flags */
	if (sendmsg(medium, &msg, 0) != sizeof iov / sizeof iov[0])
		return -1;
	return 0;
}

int
rcvfd(medium)
int medium;			/* receive an fd over this socket */
{
	struct iovec iov[1];
	struct msghdr msg;
	char byte = 0;
	int fd;

	iov[0].iov_base = &byte;
	iov[0].iov_len = sizeof byte;	/* must send at least one byte */
	msg.msg_iov = iov;
	msg.msg_iovlen = sizeof iov / sizeof iov[0];
	msg.msg_name = NULL;
	msg.msg_namelen = 0;
	msg.msg_accrights = (caddr_t)&fd;
	msg.msg_accrightslen = sizeof fd;
	/* receive fd from medium; 0 is flags */
	if (recvmsg(medium, &msg, 0) != sizeof iov / sizeof iov[0])
		return -1;
	return fd;
}

/*
 * send and receive file descriptors between processes on a single machine
 * (via Unix domain socket on 4.4BSD)
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>		/* defines struct msghdr */
#include <sys/uio.h>		/* defines struct iovec */

#define CTLSIZ (sizeof(struct cmsghdr) + sizeof(int))

int
sendfd(medium, fd)
int medium;			/* send `fd' over this socket */
int fd;
{
	struct iovec iov[1];
	struct msghdr msg;
	char byte = 0;
	struct cmsghdr *cmp;

	iov[0].iov_base = &byte;
	iov[0].iov_len = sizeof byte;	/* must send at least one byte */
	msg.msg_iov = iov;
	msg.msg_iovlen = sizeof iov / sizeof iov[0];
	msg.msg_name = NULL;
	msg.msg_namelen = 0;
	cmp = (struct cmsghdr *)malloc(CTLSIZ);
	if (cmp == NULL)
		return -1;
	cmp->cmsg_level = SOL_SOCKET;
	cmp->cmsg_type = SCM_RIGHTS;
	cmp->cmsg_len = CTLSIZ;
	msg.msg_control = (caddr_t)cmp;
	msg.msg_controllen = CTLSIZ;
	*(int *)CMSG_DATA(cmp) = fd;
	/* send fd over medium; 0 is flags */
	if (sendmsg(medium, &msg, 0) != sizeof iov / sizeof iov[0]) {
		free((char *)cmp);
		return -1;
	}
	free((char *)cmp);
	return 0;
}

int
rcvfd(medium)
int medium;			/* receive an fd over this socket */
{
	struct iovec iov[1];
	struct msghdr msg;
	char byte = 0;
	struct cmsghdr *cmp;

	iov[0].iov_base = &byte;
	iov[0].iov_len = sizeof byte;	/* must send at least one byte */
	msg.msg_iov = iov;
	msg.msg_iovlen = sizeof iov / sizeof iov[0];
	msg.msg_name = NULL;
	msg.msg_namelen = 0;
	cmp = (struct cmsghdr *)malloc(CTLSIZ);
	if (cmp == NULL)
		return -1;
	cmp->cmsg_level = SOL_SOCKET;
	cmp->cmsg_type = SCM_RIGHTS;
	cmp->cmsg_len = CTLSIZ;
	msg.msg_control = (caddr_t)cmp;
	msg.msg_controllen = CTLSIZ;
	/* receive fd from medium; 0 is flags */
	if (recvmsg(medium, &msg, 0) != sizeof iov / sizeof iov[0]) {
		free((char *)cmp);
		return -1;
	}
	fd = *(int *)CMSG_DATA(cmp);
	free((char *)cmp);
	return fd;
}

/*
 *  linux/ibcs/wysev386.c
 *
 *  Copyright 1994, 1996  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: wysev386.c,v 1.9 1996/07/31 13:31:58 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/wysev386.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/mm.h>
#include <linux/utsname.h>

#include <linux/wait.h>

#include <linux/net.h>
#include <linux/sys.h>

#include <ibcs/ibcs.h>
#include <ibcs/map.h>
#include <ibcs/socket.h>


int wv386_gethostname(char *name, int len)
{
	int	error;
	char	*p;

	if ((error = verify_area(VERIFY_WRITE, name, len)))
		return error;

	--len;
	for (p=system_utsname.nodename; *p && len; p++,len--)
		put_fs_byte(*p, name++);
	put_fs_byte('\0', name);

	return (0);
}


int wv386_getdomainname(char *name, int len)
{
	int	error;
	char	*p;

	if ((error = verify_area(VERIFY_WRITE, name, len)))
		return error;

	--len;
	for (p=system_utsname.domainname; *p && len; p++,len--)
		put_fs_byte(*p, name++);
	put_fs_byte('\0', name);

	return (0);
}


int wv386_wait3(int *loc)
{
	int pid;

	pid = SYS(wait4)(-1, loc, WNOHANG, 0);

	if(loc) {
		int res = get_fs_long((unsigned long *) loc);
		if ((res & 0xff) == 0x7f) {
			int sig = (res >> 8) & 0xff;
			sig = current->exec_domain->signal_map[sig];
			res = (res & (~0xff00)) | (sig << 8);
			put_fs_long(res, (unsigned long *)loc);
		} else if (res && res == (res & 0xff)) {
			res = current->exec_domain->signal_map[res & 0x7f];
			put_fs_long(res, (unsigned long *)loc);
		}
	}

	return pid;
}


/* It would probably be better to remove the statics in linux/net/socket.c
 * and go direct to the sock_ calls than via the indirection routine.
 */
int wv386_socket(struct pt_regs *regs)
{
	put_fs_long(
		map_value(af_map,
			get_fs_long(((unsigned long*)regs->esp)+1),
			0),
		((unsigned long *)regs->esp)+1);
	put_fs_long(
		map_value(type_map,
			get_fs_long(((unsigned long*)regs->esp)+2),
			0),
		((unsigned long *)regs->esp)+2);

	return SYS(socketcall)(SYS_SOCKET, ((unsigned long *)regs->esp) + 1);
}
int wv386_connect(struct pt_regs *regs)
{
	return SYS(socketcall)(SYS_CONNECT, ((unsigned long *)regs->esp) + 1);
}
int wv386_accept(struct pt_regs *regs)
{
	return SYS(socketcall)(SYS_ACCEPT, ((unsigned long *)regs->esp) + 1);
}
int wv386_send(struct pt_regs *regs)
{
	int err = SYS(socketcall)(SYS_SEND, ((unsigned long *)regs->esp) + 1);
	if (err == -EAGAIN) err = -EWOULDBLOCK;
	return err;
}
int wv386_recv(struct pt_regs *regs)
{
	int err = SYS(socketcall)(SYS_RECV, ((unsigned long *)regs->esp) + 1);
	if (err == -EAGAIN) err = -EWOULDBLOCK;
	return err;
}
int wv386_bind(struct pt_regs *regs)
{
	return SYS(socketcall)(SYS_BIND, ((unsigned long *)regs->esp) + 1);
}
int wv386_setsockopt(struct pt_regs *regs)
{
	return ibcs_setsockopt(((unsigned long *)regs->esp) + 1);
}
int wv386_listen(struct pt_regs *regs)
{
	return SYS(socketcall)(SYS_LISTEN, ((unsigned long *)regs->esp) + 1);
}
int wv386_getsockopt(struct pt_regs *regs)
{
	return ibcs_getsockopt(((unsigned long *)regs->esp) + 1);
}
int wv386_recvfrom(struct pt_regs *regs)
{
	int err = SYS(socketcall)(SYS_RECVFROM, ((unsigned long *)regs->esp) + 1);
	if (err == -EAGAIN) err = -EWOULDBLOCK;
	return err;
}
int wv386_sendto(struct pt_regs *regs)
{
	int err = SYS(socketcall)(SYS_SENDTO, ((unsigned long *)regs->esp) + 1);
	if (err == -EAGAIN) err = -EWOULDBLOCK;
	return err;
}
int wv386_shutdown(struct pt_regs *regs)
{
	return SYS(socketcall)(SYS_SHUTDOWN, ((unsigned long *)regs->esp) + 1);
}
int wv386_socketpair(struct pt_regs *regs)
{
	return SYS(socketcall)(SYS_SOCKETPAIR, ((unsigned long *)regs->esp) + 1);
}
int wv386_getpeername(struct pt_regs *regs)
{
	return SYS(socketcall)(SYS_GETPEERNAME, ((unsigned long *)regs->esp) + 1);
}
int wv386_getsockname(struct pt_regs *regs)
{
	return SYS(socketcall)(SYS_GETSOCKNAME, ((unsigned long *)regs->esp) + 1);
}

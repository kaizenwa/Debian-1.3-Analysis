/*
 *  linux/ibcs/stream.c
 *
 *  Copyright 1994, 1995  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: stream.c,v 1.9 1996/07/26 12:03:39 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/stream.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/types.h>
#include <linux/errno.h>
#include <linux/kernel.h>
#include <linux/ptrace.h>
#include <linux/net.h>
#include <linux/mm.h>
#include <linux/socket.h>

#include <ibcs/ibcs.h>
#include <ibcs/stream.h>
#include <ibcs/tli.h>

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif


int
ibcs_getmsg(struct pt_regs *regs)
{
	int fd;
	struct inode *ino;

	fd = (int)get_syscall_parameter (regs, 0);

	if (!current->FD[fd]
	|| !(ino = current->FD[fd]->f_inode))
		return -EBADF;

	if (!ino->i_sock)
		return -EBADF;

#if defined(EMU_XTI) || defined(SPX)
	return timod_getmsg(fd, ino, 0, regs);
#else
	return 0;
#endif /* EMU_XTI */
}


int
ibcs_putmsg(struct pt_regs *regs)
{
	int fd;
	struct inode *ino;

	fd = (int)get_syscall_parameter (regs, 0);

	if (!current->FD[fd]
	|| !(ino = current->FD[fd]->f_inode))
		return -EBADF;

	if (!ino->i_sock
	&& (MAJOR(ino->i_rdev) != 30 || MINOR(ino->i_rdev) != 1))
		return -EBADF;

#if defined(EMU_XTI) || defined(EMU_SPX)
	return timod_putmsg(fd, ino, 0, regs);
#else
	return 0;
#endif
}


#ifdef EMU_XTI
int
ibcs_getpmsg(struct pt_regs *regs)
{
	int fd;
	struct inode *ino;

	fd = (int)get_syscall_parameter (regs, 0);

	if (!current->FD[fd]
	|| !(ino = current->FD[fd]->f_inode))
		return -EBADF;

	if (!ino->i_sock)
		return -EBADF;

	return timod_getmsg(fd, ino, 1, regs);
}


int
ibcs_putpmsg(struct pt_regs *regs)
{
	int fd;
	struct inode *ino;

	fd = (int)get_syscall_parameter (regs, 0);

	if (!current->FD[fd]
	|| !(ino = current->FD[fd]->f_inode))
		return -EBADF;

	if (!ino->i_sock
	&& (MAJOR(ino->i_rdev) != 30 || MINOR(ino->i_rdev) != 1))
		return -EBADF;

	return timod_putmsg(fd, ino, 1, regs);
}
#endif /* EMU_XTI */

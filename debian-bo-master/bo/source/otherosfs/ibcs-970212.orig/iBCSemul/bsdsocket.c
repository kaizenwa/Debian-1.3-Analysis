/*
 *  linux/ibcs/bsdsocket.c
 *
 *  Copyright (C) 1994  Mike Jagdis
 *
 * $Id: bsdsocket.c,v 1.5 1996/07/26 12:03:32 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/bsdsocket.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/mm.h>
#include <linux/net.h>
#include <linux/ptrace.h>
#include <linux/socket.h>
#include <linux/sys.h>

#include <ibcs/ibcs.h>
#include <ibcs/bsd.h>


int
bsd_connect(struct pt_regs *regs)
{
	int error;
	char *addr;
	unsigned short s;

	/* With BSD the first byte of the sockaddr struct is a length and
	 * the second is the address family. With Linux the address family
	 * occupies both bytes.
	 */
	addr = get_syscall_parameter (regs, 1);
	if ((error = verify_area(VERIFY_READ, addr, 2)))
		return error;
	s = get_fs_word(addr);
	put_fs_word(s>>8, addr);

	error = SYS(socketcall)(SYS_CONNECT, get_syscall_parameter (regs, 0));

	put_fs_word(s, addr);
	return error;
}

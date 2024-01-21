/*
 *  linux/ibcs/socket.c
 *
 *  Copyright (C) 1994, 1996  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: socket.c,v 1.7 1996/03/29 17:33:33 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/socket.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/types.h>
#include <linux/fs.h>
#include <linux/mm.h>
#include <linux/ptrace.h>
#include <linux/net.h>
#include <linux/socket.h>
#include <linux/sys.h>

#include <ibcs/ibcs.h>
#include <ibcs/socket.h>
#include <ibcs/map.h>
#include <ibcs/trace.h>


int
ibcs_setsockopt(unsigned long *sp)
{
	int error;
	int level, optname;

	error = verify_area(VERIFY_READ,
			((unsigned long *)sp),
			5*sizeof(long));
	if (error)
		return error;

	level = (int)get_fs_long(((unsigned long *)sp)+1);
	optname = (int)get_fs_long(((unsigned long *)sp)+2);

	switch (level) {
		case 0xffff:
			put_fs_long(SOL_SOCKET, ((unsigned long *)sp)+1);
			optname = map_value(sopt_map, optname, 0);
			put_fs_long(optname, ((unsigned long *)sp)+2);

			switch (optname) {
				/* The following are not currently implemented
				 * under Linux so we must fake them in
				 * reasonable ways. (Only SO_PROTOTYPE is
				 * documented in SCO's man page).
				 */
				case SO_PROTOTYPE:
				case SO_ORDREL:
				case SO_SNDTIMEO:
				case SO_RCVTIMEO:
					return -ENOPROTOOPT;

				case SO_USELOOPBACK:
				case SO_SNDLOWAT:
				case SO_RCVLOWAT:
					return 0;

				/* The following are not currenty implemented
				 * under Linux and probably aren't settable
				 * anyway.
				 */
				case SO_IMASOCKET:
					return -ENOPROTOOPT;
			}

		default:
			/* FIXME: We assume everything else uses the
			 * same level and option numbers. This is true
			 * for IPPROTO_TCP(/SOL_TCP) and TCP_NDELAY
			 * but is known to be incorrect for other
			 * potential options :-(.
			 */
			break;
	}

	return SYS(socketcall)(SYS_SETSOCKOPT, sp);
}


int
ibcs_getsockopt(unsigned long *sp)
{
	int error;
	int level, optname;
	char *optval;
	long *optlen;

	error = verify_area(VERIFY_READ,
			((unsigned long *)sp),
			5*sizeof(long));
	if (error)
		return error;

	level = (int)get_fs_long(((unsigned long *)sp)+1);
	optname = (int)get_fs_long(((unsigned long *)sp)+2);
	optval = (char *)get_fs_long(((unsigned long *)sp)+3);
	optlen = (long *)get_fs_long(((unsigned long *)sp)+4);

#ifdef IBCS_TRACE
	if ((ibcs_trace & (TRACE_STREAMS|TRACE_SOCKSYS))) {
		printk(KERN_DEBUG "iBCS: getsockopt level=%d, "
			"optname=%d, optval=0x%08lx, optlen=0x%08lx\n",
			level, optname, (unsigned long)optval,
			(unsigned long)optlen);
	}
#endif

	switch (level) {
		case 0xffff:
			put_fs_long(SOL_SOCKET, ((unsigned long *)sp)+1);
			optname = map_value(sopt_map, optname, 0);
			put_fs_long(optname, ((unsigned long *)sp)+2);

			switch (optname) {
				/* The following are not currently implemented
				 * under Linux so we must fake them in
				 * reasonable ways. (Only SO_PROTOTYPE is
				 * documented in SCO's man page).
				 */
				case SO_PROTOTYPE: {
					error = verify_area(VERIFY_WRITE,
							(char *)optlen,
							sizeof(long));
					if (error)
						return error;
					if (get_fs_long(optlen) < sizeof(long))
						return -EINVAL;

					error = verify_area(VERIFY_WRITE,
							(char *)optval,
							sizeof(long));
					if (!error) {
						put_fs_long(0, optval);
						put_fs_long(sizeof(long),
							optlen);
					}
					return error;
				}

				case SO_ORDREL:
				case SO_SNDTIMEO:
				case SO_RCVTIMEO:
					return -ENOPROTOOPT;

				case SO_USELOOPBACK:
				case SO_SNDLOWAT:
				case SO_RCVLOWAT:
				case SO_IMASOCKET: {
					error = verify_area(VERIFY_WRITE,
							(char *)optlen,
							sizeof(long));
					if (error)
						return error;
					if (get_fs_long(optlen) < sizeof(long))
						return -EINVAL;

					error = verify_area(VERIFY_WRITE,
							(char *)optval,
							sizeof(long));
					if (!error) {
						put_fs_long(1, optval);
						put_fs_long(sizeof(long),
							optlen);
					}
					return error;
				}
			}

		default:
			/* FIXME: We assume everything else uses the
			 * same level and option numbers. This is true
			 * for IPPROTO_TCP(/SOL_TCP) and TCP_NDELAY
			 * but is known to be incorrect for other
			 * potential options :-(.
			 */
			break;
	}

	return SYS(socketcall)(SYS_GETSOCKOPT, sp);
}

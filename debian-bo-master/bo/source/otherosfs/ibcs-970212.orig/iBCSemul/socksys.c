/*
 *  linux/ibcs/socksys.h
 *
 *  Copyright 1994-1996  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: socksys.c,v 1.49 1996/10/02 14:58:47 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/socksys.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/config.h>
#endif

#include <linux/types.h>
#include <linux/errno.h>
#include <linux/fs.h>
#include <linux/fcntl.h>
#include <linux/major.h>
#include <linux/kernel.h>
#include <linux/in.h>
#include <linux/net.h>
#include <linux/sched.h>
#include <linux/signal.h>
#include <linux/socket.h>
#include <linux/malloc.h>
#include <linux/mm.h>
#include <linux/un.h>
#include <linux/utsname.h>
#include <linux/time.h>
#include <linux/termios.h>
#include <linux/sys.h>

#include <ibcs/ibcs.h>
#include <ibcs/map.h>

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif

#include <ibcs/socksys.h>
#ifdef EMU_XTI
#include <ibcs/tli.h>
#endif

static int socksys_open(struct inode *ino, struct file *filep);
static void socksys_close(struct inode *ino, struct file *filep);
static int socksys_select(struct inode *ino, struct file *filep,
			int sel_type, select_table *wait);

static int spx_write(struct inode *ino, struct file *filep,
			const char *buf, int count);

/* spx_fops defines the file operations that can be applied to the
 * /dev/spx server devices.
 */
static struct file_operations spx_fops = {
	NULL,			/* lseek */
	NULL,			/* read */
	spx_write,		/* write */
	NULL,			/* readdir */
	NULL,			/* select */
	NULL,			/* ioctl - ibcs_ioctl calls ibcs_ioctl_socksys*/
	NULL,			/* mmap */
	NULL,			/* open */
	socksys_close,		/* close */
	NULL			/* fsync */
};

/* socksys_fops defines the file operations that can be applied to the
 * /dev/socksys device.
 */
static struct file_operations socksys_fops = {
	NULL,			/* lseek */
	NULL,			/* read */
	NULL,			/* write */
	NULL,			/* readdir */
	NULL,			/* select */
	NULL,			/* ioctl - ibcs_ioctl calls ibcs_ioctl_socksys*/
	NULL,			/* mmap */
	socksys_open,		/* open */
	socksys_close,		/* close */
	NULL			/* fsync */
};

/* socksys_socket_fops defines the file operations that can be applied to
 * sockets themselves. This gets initialised when the first socket is
 * created.
 */
static struct file_operations socksys_socket_fops = {
	NULL,			/* lseek */
	NULL,			/* read */
	NULL,			/* write */
	NULL,			/* readdir */
	NULL,			/* select */
	NULL,			/* ioctl */
	NULL,			/* mmap */
	NULL,			/* open */
	NULL,			/* close */
	NULL			/* fsync */
};
static void (*sock_close)(struct inode *inode, struct file *file);
static int (*sock_select)(struct inode *ino, struct file *filep,
			int sel_type, select_table *wait);


static void
inherit_socksys_funcs(struct file *filep, int fd)
{
	/* If our file operations don't appear to match
	 * what the socket system is advertising it may
	 * be because we haven't initialised ours at all
	 * yet or it may be because the old socket system
	 * module was unloaded and reloaded. This isn't
	 * entirely safe because we may still have open
	 * sockets which *should* use the old routines
	 * until they close - tough, for now.
	 */
	if (socksys_socket_fops.read != current->FD[fd]->f_op->read) {
		memcpy(&socksys_socket_fops,
			current->FD[fd]->f_op,
			sizeof(struct file_operations));
		sock_close = socksys_socket_fops.release;
		sock_select = socksys_socket_fops.select;
		socksys_socket_fops.release = socksys_close;
		socksys_socket_fops.select = socksys_select;
	}
	filep->f_op = &socksys_socket_fops;
}


static inline int
socksys_syscall(int fd, int *sp)
{
	int cmd;

	cmd = (int)get_fs_long(sp);
	sp++;

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_SOCKSYS) {
		unsigned long a0, a1, a2, a3, a4, a5;
		static char *cmd_map[] = {
			"", "accept", "bind", "connect", "getpeername",
			"getsockname", "getsockopt", "listen", "recv",
			"recvfrom", "send", "sendto", "setsockopt", "shutdown",
			"socket", "select", "getipdomain", "setipdomain",
			"adjtime", "setreuid", "setregid", "gettimeofday",
			"settimeofday", "getitimer", "setitimer"
		};

		a0 = (unsigned long)get_fs_long(sp+0);
		a1 = (unsigned long)get_fs_long(sp+1);
		a2 = (unsigned long)get_fs_long(sp+2);
		a3 = (unsigned long)get_fs_long(sp+3);
		a4 = (unsigned long)get_fs_long(sp+4);
		a5 = (unsigned long)get_fs_long(sp+5);
		printk(KERN_DEBUG "socksys: [%d] %d: %s (%d) <0x%lx,0x%lx,0x%lx,0x%lx,0x%lx,0x%lx>\n",
			current->pid, fd,
			cmd_map[cmd], cmd, a0, a1, a2, a3, a4, a5);
	}
#endif

	switch (cmd) {
		case SSYS_SO_SOCKET: {
			/* Get a socket but replace the socket file
			 * operations with our own so we can do the
			 * right thing for ioctls.
			 */
			int fd;

			put_fs_long(
				map_value(af_map,
					get_fs_long(((unsigned long *)sp)+0),
					0),
 				((unsigned long *)sp)+0);
			put_fs_long(
				map_value(type_map,
					get_fs_long(((unsigned long *)sp)+1),
					0),
 				((unsigned long *)sp)+1);

			if ((fd = SYS(socketcall)(SYS_SOCKET, sp)) < 0)
				return fd;

			inherit_socksys_funcs(current->FD[fd], fd);
			MOD_INC_USE_COUNT;
			return fd;
		}

		case SSYS_SO_ACCEPT: {
			int fd;

			if ((fd = SYS(socketcall)(SYS_ACCEPT, sp)) < 0)
				return fd;

			inherit_socksys_funcs(current->FD[fd], fd);
			MOD_INC_USE_COUNT;
			return fd;
		}
		case SSYS_SO_BIND:
			return SYS(socketcall)(SYS_BIND, sp);
		case SSYS_SO_CONNECT: {
#ifndef INIT_MM
			struct sockaddr *sa;

			/* Trap attack... Some things (notably SVR
			 * syslog code) seems to think 0.0.0.0 is
			 * a valid address for localhost.
			 */
			sa = (struct sockaddr *)get_fs_long(sp+1);
			if (get_fs_word(&sa->sa_family) == AF_INET) {
				unsigned long *addr;

				addr = &(((struct sockaddr_in *)sa)->sin_addr.s_addr);
				if (get_fs_long(addr) == INADDR_ANY) {
					put_fs_long(0x0100007f, addr);
#ifdef IBCS_TRACE
					if (ibcs_trace & TRACE_SOCKSYS)
						printk(KERN_DEBUG "iBCS: socksys: remapped INADDR_ANY to localhost\n");
#endif
				}
			}
#endif
			return SYS(socketcall)(SYS_CONNECT, sp);
		}
		case SSYS_SO_GETPEERNAME:
			return SYS(socketcall)(SYS_GETPEERNAME, sp);
		case SSYS_SO_GETSOCKNAME:
			return SYS(socketcall)(SYS_GETSOCKNAME, sp);
		case SSYS_SO_GETSOCKOPT:
			return ibcs_getsockopt((unsigned long *)sp);
		case SSYS_SO_LISTEN:
			return SYS(socketcall)(SYS_LISTEN, sp);
		case SSYS_SO_RECV: {
			int err = SYS(socketcall)(SYS_RECV, sp);
			if (err == -EAGAIN) err = -EWOULDBLOCK;
			return err;
		}
		case SSYS_SO_RECVFROM: {
			int err = SYS(socketcall)(SYS_RECVFROM, sp);
			if (err == -EAGAIN) err = -EWOULDBLOCK;
			return err;
		}
		case SSYS_SO_SEND: {
			int err = SYS(socketcall)(SYS_SEND, sp);
			if (err == -EAGAIN) err = -EWOULDBLOCK;
			return err;
		}
		case SSYS_SO_SENDTO: {
			int err = SYS(socketcall)(SYS_SENDTO, sp);
			if (err == -EAGAIN) err = -EWOULDBLOCK;
			return err;
		}
		case SSYS_SO_SETSOCKOPT:
			return ibcs_setsockopt((unsigned long *)sp);
		case SSYS_SO_SHUTDOWN:
			return SYS(socketcall)(SYS_SHUTDOWN, sp);

		case SSYS_SO_GETIPDOMAIN: {
			int error, len;
			char *name, *p;

			if ((error = verify_area(VERIFY_READ, sp, 2*sizeof(long))))
				return error;

			name = (char *)get_fs_long(sp+0);
			len = (int)get_fs_long(sp+1);
			if ((error = verify_area(VERIFY_WRITE, name, len)))
				return error;

			--len;
			for (p=system_utsname.nodename; *p && *p != '.'; p++);
			if (*p == '.')
				p++;
			else
				p = system_utsname.domainname;
			if (strcmp(p, "(none)"))
				for (;*p && len > 0; p++,len--)
					put_fs_byte(*p, name++);
			put_fs_byte('\0', name);

			return (0);
		}
		case SSYS_SO_SETIPDOMAIN: {
			int error, len, togo;
			char *name, *p;

			if (!suser())
				return -EPERM;

			if ((error = verify_area(VERIFY_READ, sp, 2*sizeof(long))))
				return error;

			name = (char *)get_fs_long(sp+0);
			len = (int)get_fs_long(sp+1);

			togo = __NEW_UTS_LEN;
			for (p=system_utsname.nodename; *p && *p != '.'; p++,togo--);
			if (*p == '.')
				p++,togo--;

			if (len > togo)
				return -EINVAL;

			while (len-- > 0)
				*p++ = get_fs_byte(name++);
			*p = '\0';
			return 0;
		}

		case SSYS_SO_SETREUID:
		case SSYS_SO_SETREGID: {
			int error;
			uid_t ruid, euid;

			if ((error = verify_area(VERIFY_READ, sp, 2*sizeof(long))))
				return error;
			ruid = (uid_t)get_fs_long(sp+0);
			euid = (uid_t)get_fs_long(sp+1);
			return (cmd == SSYS_SO_SETREUID)
				? SYS(setreuid)(ruid, euid)
				: SYS(setregid)(ruid, euid);
		}

		case SSYS_SO_GETTIME:
		case SSYS_SO_SETTIME: {
			int error;
			struct timeval *tv;
			struct timezone *tz;

			if ((error = verify_area(VERIFY_READ, sp, 2*sizeof(long))))
				return error;
			tv = (struct timeval *)get_fs_long(sp+0);
			tz = (struct timezone *)get_fs_long(sp+1);
			return (cmd == SSYS_SO_GETTIME)
				? SYS(gettimeofday)(tv, tz)
				: SYS(settimeofday)(tv, tz);
		}

		case SSYS_SO_GETITIMER: {
			int error, which;
			struct itimerval *value;

			if ((error = verify_area(VERIFY_READ, sp, 2*sizeof(long))))
				return error;
			which = (int)get_fs_long(sp+0);
			value = (struct itimerval *)get_fs_long(sp+1);
			return SYS(getitimer)(which, value);
		}
		case SSYS_SO_SETITIMER: {
			int error, which;
			struct itimerval *value, *ovalue;

			if ((error = verify_area(VERIFY_READ, sp, 3*sizeof(long))))
				return error;
			which = (int)get_fs_long(sp+0);
			value = (struct itimerval *)get_fs_long(sp+1);
			ovalue = (struct itimerval *)get_fs_long(sp+2);
			return SYS(setitimer)(which, value, ovalue);
		}

		case SSYS_SO_SELECT:
			/* This may be wrong? I don't know how to trigger
			 * this case. Select seems to go via the Xenix
			 * select entry point.
			 */
			return SYS(select)(sp);

		case SSYS_SO_ADJTIME:
			return -EINVAL;
	}

	return -EINVAL;
}


int
ibcs_ioctl_socksys(int fd, unsigned int cmd, void *arg)
{
	int error;

	switch (cmd) {
		/* Strictly the ip domain and nis domain are separate and
		 * distinct under SCO but Linux only has the one domain.
		 */
		case NIOCGETDOMNAM: {
			struct domnam_args dn;
			char *p;

			if ((error = verify_area(VERIFY_READ, (char *)arg, sizeof(struct domnam_args))))
				return error;
			memcpy_fromfs(&dn, (char *)arg, sizeof(struct domnam_args));

			if ((error = verify_area(VERIFY_WRITE, dn.name, dn.namelen)))
				return error;

			--dn.namelen;
			for (p=system_utsname.domainname; *p && dn.namelen > 0; p++,dn.namelen--)
				put_fs_byte(*p, dn.name++);
			put_fs_byte('\0', dn.name);

			return (0);
		}
		case NIOCSETDOMNAM: {
			struct domnam_args dn;

			if ((error = verify_area(VERIFY_READ, (char *)arg, sizeof(struct domnam_args))))
				return error;
			memcpy_fromfs(&dn, (char *)arg, sizeof(struct domnam_args));

			return SYS(setdomainname)(dn.name, dn.namelen);
		}

		case NIOCLSTAT: {
			/* I think this was used before symlinks were added
			 * to the base SCO OS?
			 */
			struct lstat_args st;

			if ((error = verify_area(VERIFY_READ, (char *)arg, sizeof(struct lstat_args))))
				return error;
			memcpy_fromfs(&st, (char *)arg, sizeof(struct lstat_args));

			return ibcs_lstat(st.fname, st.statb);
		}

		case NIOCOLDGETFH:
		case NIOCGETFH: {
			struct getfh_args gf;
			struct inode *ino;

			if (!suser())
				return -EPERM;

			if ((error = verify_area(VERIFY_READ, (char *)arg, sizeof(struct getfh_args))))
				return error;
			memcpy_fromfs(&gf, (char *)arg, sizeof(struct getfh_args));

			if ((error = verify_area(VERIFY_WRITE, (char *)gf.fhp, sizeof(fhandle_t))))
				return error;

			if ((error = namei(gf.fname, &ino)))
				return error;

			put_fs_word(ino->i_dev, &gf.fhp->fh.fsid);
			put_fs_long(ino->i_ino, &gf.fhp->fh.fno);
			put_fs_long(0L, &gf.fhp->fh.fgen);
			put_fs_word(ino->i_dev, &gf.fhp->fh.ex_fsid);
			put_fs_long(ino->i_ino, &gf.fhp->fh.ex_fno);
			put_fs_long(0L, &gf.fhp->fh.ex_fgen);
			return 0;
		}

		case NIOCNFSD:
		case NIOCASYNCD:
		case NIOCCLNTHAND:
		case NIOCEXPORTFS:
			return -EINVAL;

		case SSYS_SIOCSOCKSYS:		/* Pseudo socket syscall */
		case SVR4_SIOCSOCKSYS:
			return socksys_syscall(fd, (int *)arg);

		case SSYS_SIOCSHIWAT:		/* set high watermark */
		case SVR4_SIOCSHIWAT:
		case SSYS_SIOCSLOWAT:		/* set low watermark */
		case SVR4_SIOCSLOWAT:
			/* Linux doesn't support them but lie anyway
			 * or some things take it as fatal (why?)
			 */
			return 0;

		case SSYS_SIOCGHIWAT:		/* get high watermark */
		case SVR4_SIOCGHIWAT:
		case SSYS_SIOCGLOWAT:		/* get low watermark */
		case SVR4_SIOCGLOWAT:
			/* Linux doesn't support them but lie anyway
			 * or some things take it as fatal (why?)
			 */
			if ((error = verify_area(VERIFY_WRITE, (char *)arg,
						sizeof(unsigned long))))
				return error;
			put_fs_long(0, (unsigned long *)arg);
			return 0;

		case SSYS_SIOCATMARK:		/* at oob mark? */
		case SVR4_SIOCATMARK:
			return SYS(ioctl)(fd, SIOCATMARK, arg);

		case SSYS_SIOCSPGRP:		/* set process group */
		case SVR4_SIOCSPGRP:
			return SYS(ioctl)(fd, SIOCSPGRP, arg);
		case SSYS_SIOCGPGRP:		/* get process group */
		case SVR4_SIOCGPGRP:
			return SYS(ioctl)(fd, SIOCGPGRP, arg);

		case FIONREAD:
		case SSYS_FIONREAD:		/* BSD compatibilty */
			error = SYS(ioctl)(fd, TIOCINQ, arg);
#ifdef IBCS_TRACE
			if (!error && (ibcs_trace & TRACE_SOCKSYS))
				printk(KERN_DEBUG
					"iBCS: socksys FIONREAD found %lu bytes ready\n",
					(unsigned long)get_fs_long(arg));
#endif
			return error;

		case SSYS_FIONBIO: 		/* BSD compatibilty */
			return SYS(ioctl)(fd, FIONBIO, arg);

		case SSYS_FIOASYNC: 		/* BSD compatibilty */
			return SYS(ioctl)(fd, FIOASYNC, arg);

		case SSYS_SIOCADDRT:		/* add route */
		case SVR4_SIOCADDRT:
			return SYS(ioctl)(fd, SIOCADDRT, arg);
		case SSYS_SIOCDELRT:		/* delete route */
		case SVR4_SIOCDELRT:
			return SYS(ioctl)(fd, SIOCDELRT, arg);

		case SSYS_SIOCSIFADDR:		/* set ifnet address */
		case SVR4_SIOCSIFADDR:
			return SYS(ioctl)(fd, SIOCSIFADDR, arg);
		case SSYS_SIOCGIFADDR:		/* get ifnet address */
		case SVR4_SIOCGIFADDR:
			return SYS(ioctl)(fd, SIOCGIFADDR, arg);

		case SSYS_SIOCSIFDSTADDR:	/* set p-p address */
		case SVR4_SIOCSIFDSTADDR:
			return SYS(ioctl)(fd, SIOCSIFDSTADDR, arg);
		case SSYS_SIOCGIFDSTADDR:	/* get p-p address */
		case SVR4_SIOCGIFDSTADDR:
			return SYS(ioctl)(fd, SIOCGIFDSTADDR, arg);

		case SSYS_SIOCSIFFLAGS:		/* set ifnet flags */
		case SVR4_SIOCSIFFLAGS:
			return SYS(ioctl)(fd, SIOCSIFFLAGS, arg);
		case SSYS_SIOCGIFFLAGS:		/* get ifnet flags */
		case SVR4_SIOCGIFFLAGS:
#if 0
		case SVRX_SIOCGIFFLAGS:
#endif
			return SYS(ioctl)(fd, SIOCGIFFLAGS, arg);

		case SSYS_SIOCGIFCONF:		/* get ifnet list */
		case SVR4_SIOCGIFCONF:
#if 0
		case SVRX_SIOCGIFCONF:
#endif
			return SYS(ioctl)(fd, SIOCGIFCONF, arg);

		case SSYS_SIOCGIFBRDADDR:	/* get broadcast addr */
		case SVR4_SIOCGIFBRDADDR:
			return SYS(ioctl)(fd, SIOCGIFBRDADDR, arg);
		case SSYS_SIOCSIFBRDADDR:	/* set broadcast addr */
		case SVR4_SIOCSIFBRDADDR:
			return SYS(ioctl)(fd, SIOCSIFBRDADDR, arg);

		case SSYS_SIOCGIFNETMASK:	/* get net addr mask */
		case SVR4_SIOCGIFNETMASK:
			return SYS(ioctl)(fd, SIOCGIFNETMASK, arg);
		case SSYS_SIOCSIFNETMASK:	/* set net addr mask */
			return SYS(ioctl)(fd, SIOCSIFNETMASK, arg);

		case SSYS_SIOCGIFMETRIC:	/* get IF metric */
		case SVR4_SIOCGIFMETRIC:
			return SYS(ioctl)(fd, SIOCGIFMETRIC, arg);
		case SSYS_SIOCSIFMETRIC:	/* set IF metric */
		case SVR4_SIOCSIFMETRIC:
			return SYS(ioctl)(fd, SIOCSIFMETRIC, arg);

		case SSYS_SIOCSARP:		/* set arp entry */
		case SVR4_SIOCSARP:
			return SYS(ioctl)(fd, SIOCSARP, arg);
		case SSYS_SIOCGARP:		/* get arp entry */
		case SVR4_SIOCGARP:
			return SYS(ioctl)(fd, SIOCGARP, arg);
		case SSYS_SIOCDARP:		/* delete arp entry */
		case SVR4_SIOCDARP:
			return SYS(ioctl)(fd, SIOCDARP, arg);

		case SSYS_SIOCGENADDR:		/* Get ethernet addr */
		case SVR4_SIOCGENADDR:
			return SYS(ioctl)(fd, SIOCGIFHWADDR, arg);

		case SSYS_SIOCSIFMTU:		/* get if_mtu */
		case SVR4_SIOCSIFMTU:
			return SYS(ioctl)(fd, SIOCSIFMTU, arg);
		case SSYS_SIOCGIFMTU:		/* set if_mtu */
		case SVR4_SIOCGIFMTU:
			return SYS(ioctl)(fd, SIOCGIFMTU, arg);

		case SSYS_SIOCGETNAME:		/* getsockname */
		case SVR4_SIOCGETNAME:
		case SSYS_SIOCGETPEER: 		/* getpeername */
		case SVR4_SIOCGETPEER:
		{
			struct sockaddr uaddr;
			int uaddr_len = sizeof(struct sockaddr);
			int op, args[3], old_fs;

			if ((error = verify_area(VERIFY_WRITE, (char *)arg, sizeof(struct sockaddr))))
				return error;
			if (cmd == SSYS_SIOCGETNAME || cmd == SVR4_SIOCGETNAME)
				op = SYS_GETSOCKNAME;
			else
				op = SYS_GETPEERNAME;
			args[0] = fd;
			args[1] = (int)&uaddr;
			args[2] = (int)&uaddr_len;
			old_fs = get_fs();
			set_fs (get_ds());
			error = SYS(socketcall)(op, args);
			set_fs(old_fs);
			if (error >= 0)
				memcpy_tofs((char *)arg, &uaddr, uaddr_len);
			return error;
		}

		case SSYS_IF_UNITSEL:		/* set unit number */
		case SVR4_IF_UNITSEL:
		case SSYS_SIOCXPROTO:		/* empty proto table */
		case SVR4_SIOCXPROTO:

		case SSYS_SIOCIFDETACH:		/* detach interface */
		case SVR4_SIOCIFDETACH:
		case SSYS_SIOCGENPSTATS:	/* get ENP stats */
		case SVR4_SIOCGENPSTATS:

		case SSYS_SIOCSIFNAME:		/* set interface name */
		case SVR4_SIOCSIFNAME:
		case SSYS_SIOCGIFONEP:		/* get one-packet params */
		case SSYS_SIOCSIFONEP:		/* set one-packet params */

		case SSYS_SIOCPROTO:		/* link proto */
		case SVR4_SIOCPROTO:
		case SSYS_SIOCX25XMT:
		case SVR4_SIOCX25XMT:
		case SSYS_SIOCX25RCV:
		case SVR4_SIOCX25RCV:
		case SSYS_SIOCX25TBL:
		case SVR4_SIOCX25TBL:

		default:
			printk(KERN_DEBUG "socksys: [%d] %d: ioctl 0x%x with argument 0x%lx requested\n",
				current->pid, fd,
				cmd, (unsigned long)arg);
			break;
	}

	return -EINVAL;
}


static int
spx_connect(struct inode *ino, struct file *filep, int addr)
{
	int newfd, err, args[3];
	struct sockaddr_un addr = {
		AF_UNIX, "/tmp/.X11-unix/X0"
	};
	int old_fs = get_fs();

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_SOCKSYS)
		printk(KERN_DEBUG "SPX: [%d] %lx choose service %d\n",
			current->pid, (unsigned long)filep, addr);
#endif

	/* Rather than use an explicit path to the X :0 server
	 * socket we should use the given number to look up a path
	 * name to use (we can't rely on servers registering their
	 * sockets either - for one thing we don't emulate that yet
	 * and for another thing different OS binaries do things in
	 * different ways but all must interoperate).
	 * I suggest putting the mapping in, say, /dev/spx.map/%d
	 * where each file is a symlink containing the path of the
	 * socket to use. Then we can just do a readlink() here to
	 * get the pathname.
	 *   Hey, this is what we do here now!
	 */
	addr.sun_family = AF_UNIX;
	sprintf(addr.sun_path, "/dev/spx.map/%d", addr);
	set_fs(get_ds());
	err = SYS(readlink)(addr.sun_path, addr.sun_path,
				sizeof(addr.sun_path)-1);
	set_fs(old_fs);
	if (err == -ENOENT) {
#ifdef IBCS_TRACE
		if (ibcs_trace & TRACE_SOCKSYS)
			printk(KERN_DEBUG
				"SPX: [%d] %lx no symlink \"%s\", try X :0\n",
				current->pid, (unsigned long)filep,
				addr.sun_path);
#endif
		strcpy(addr.sun_path, "/tmp/.X11-unix/X0");
	} else {
		if (err < 0)
			return err;
		addr.sun_path[err] = '\0';
	}

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_SOCKSYS)
		printk(KERN_DEBUG "SPX: [%d] %lx get a Unix domain socket\n",
			current->pid, (unsigned long)filep);
#endif
	args[0] = AF_UNIX;
	args[1] = SOCK_STREAM;
	args[2] = 0;
	set_fs(get_ds());
	newfd = SYS(socketcall)(SYS_SOCKET, args);
	set_fs(old_fs);
	if (newfd < 0)
		return newfd;

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_SOCKSYS)
		printk(KERN_DEBUG "SPX: [%d] %lx connect to \"%s\"\n",
			current->pid, (unsigned long)filep, addr.sun_path);
#endif
	args[0] = newfd;
	args[1] = (int)&addr;
	args[2] = sizeof(struct sockaddr_un);
	set_fs(get_ds());
	err = SYS(socketcall)(SYS_CONNECT, args);
	set_fs(old_fs);
	if (err) {
		SYS(close)(newfd);
		return err;
	}

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_SOCKSYS)
		printk(KERN_DEBUG "SPX: [%d] filep=0x%08lx, swap inodes old=0x%08lx, new=0x%08lx\n",
			current->pid, (unsigned long)filep, (unsigned long)ino,
			(unsigned long)current->FD[newfd]->f_inode);
#endif

	inherit_socksys_funcs(filep, newfd);
	filep->f_inode = current->FD[newfd]->f_inode;
	filep->f_inode->i_rdev = ino->i_rdev;
	filep->f_inode->i_flock = ino->i_flock;
	filep->f_inode->u.socket_i.file = filep;
	iput(ino);
	FD_CLR(newfd, &current->files->close_on_exec);
	FD_CLR(newfd, &current->files->open_fds);
	current->FD[newfd]->f_count--;
	current->FD[newfd] = NULL;

	return 1;
}


static int
spx_write(struct inode *ino, struct file *filep, const char *buf, int count)
{
	int error;

	if ((error = verify_area(VERIFY_READ, buf, 1*sizeof(char))))
		return error;

	return spx_connect(ino, filep, get_fs_byte(buf));
}


static int
socksys_open(struct inode *ino, struct file *filep)
{
	MOD_INC_USE_COUNT;
#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_SOCKSYS)
		printk(KERN_DEBUG
			"socksys: [%d] filep=0x%08lx, inode=0x%08lx opening\n",
			current->pid, (unsigned long)filep, (unsigned long)ino);
#endif
	/* Minor = 0 is the socksys device itself. No special handling
	 *           will be needed as it is controlled by the application
	 *           via ioctls.
	 */
	if (MINOR(ino->i_rdev) == 0)
		return 0;

	/* Minor = 1 is the spx device. This is the client side of a
	 *           streams pipe to the X server. Under SCO and friends
	 *           the library code messes around setting the connection
	 *           up itself. We do it ourselves - this means we don't
	 *           need to worry about the implementation of the server
	 *           side (/dev/X0R - which must exist but can be a link
	 *           to /dev/null) nor do we need to actually implement
	 *           getmsg/putmsg.
	 */
	if (MINOR(ino->i_rdev) == 1) {
		filep->f_op = &spx_fops;
#if 0
		/* It seems that there was an early spx implementation
		 * that only supported one destination and which
		 * required nothing but an open() to connect. When we
		 * find out how to identify such cases we can wrap this
		 * code in a conditional and enable it. In the meantime
		 * you might find "ln -s /tmp/.X11-unix/X0 /dev/spx"
		 * will work?
		 */
		return spx_connect(ino, filep, 1);
#else
		return 0;
#endif
	}

	/*
	 * Otherwise the high 4 bits specify the address/protocol
	 * family (AF_INET, AF_UNIX etc.) and the low 4 bits determine
	 * the protocol (IPPROTO_IP, IPPROTO_UDP, IPPROTO_TCP etc.)
	 * although not using a one-to-one mapping as the minor number
	 * is not big enough to hold everything directly. The socket
	 * type is inferrred from the protocol.
	 */
{ /* XTI */
#ifdef EMU_XTI
	struct T_private *priv;
#endif
	int fd, err, args[3];
	int old_fs = get_fs();

	/* Grab a socket. */
#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_SOCKSYS)
		printk(KERN_DEBUG
			"XTI: [%d] %lx get socket for transport end point"
			" (dev = 0x%04x)\n",
			current->pid, (unsigned long)filep,
			ino->i_rdev);
#endif
	switch ((args[0] = ((MINOR(ino->i_rdev) >> 4) & 0x0f))) {
		case AF_UNIX:
			args[0] = AF_UNIX;
			args[1] = SOCK_STREAM;
			args[2] = 0;
			break;

		case AF_INET: {
			int prot[16] = {
				IPPROTO_ICMP,	IPPROTO_ICMP,	IPPROTO_IGMP,
				IPPROTO_IPIP,	IPPROTO_TCP,	IPPROTO_EGP,
				IPPROTO_PUP,	IPPROTO_UDP,	IPPROTO_IDP,
				IPPROTO_RAW,
			};
			int type[16] = {
				SOCK_RAW,	SOCK_RAW,	SOCK_RAW,
				SOCK_RAW,	SOCK_STREAM,	SOCK_RAW,
				SOCK_RAW,	SOCK_DGRAM,	SOCK_RAW,
				SOCK_RAW,
			};
			int i = MINOR(ino->i_rdev) & 0x0f;
			args[2] = prot[i];
			args[1] = type[i];
			break;
		}

		default:
			args[1] = SOCK_RAW;
			args[2] = 0;
			break;
	}
#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_SOCKSYS)
		printk(KERN_DEBUG
			"XTI: [%d] %lx socket %d %d %d\n",
			current->pid, (unsigned long)filep,
			args[0], args[1], args[2]);
#endif
	set_fs(get_ds());
	fd = SYS(socketcall)(SYS_SOCKET, args);
	set_fs(old_fs);
	if (fd < 0) {
		MOD_DEC_USE_COUNT;
		return fd;
	}

#ifdef EMU_XTI
	priv = (struct T_private *)kmalloc(sizeof(struct T_private), GFP_KERNEL);
	if (!priv) {
		SYS(close)(fd);
		MOD_DEC_USE_COUNT;
		return err;
	}
	priv->state = TS_UNBND;
	priv->offset = 0;
	priv->pfirst = priv->plast = NULL;
#endif

	/* Release the inode we were given, replace with the socket
	 * inode, redirect operations to our emulation handlers then
	 * clear down the descriptor used for the socket.
	 */
#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_SOCKSYS)
		printk(KERN_DEBUG "XTI: [%d] %lx swap inodes\n",
			current->pid, (unsigned long)filep);
#endif
	inherit_socksys_funcs(filep, fd);
	filep->f_inode = current->FD[fd]->f_inode;
	filep->f_inode->i_rdev = ino->i_rdev;
	filep->f_inode->i_flock = ino->i_flock;
	filep->f_inode->u.socket_i.file = filep;
	iput(ino);
#ifdef EMU_XTI
	filep->private_data = priv;
#endif
	FD_CLR(fd, &current->files->close_on_exec);
	FD_CLR(fd, &current->files->open_fds);
	current->FD[fd]->f_count--;
	current->FD[fd] = NULL;

	return 0;
} /* XTI */
}


static int
socksys_select(struct inode *ino, struct file *filep,
		int sel_type, select_table *wait)
{
#ifdef EMU_XTI
	/* If we are checking for readable data and this is a
	 * timod transport end point and there is a control
	 * message queued we have readable data.
	 */
	if (sel_type == SEL_IN
	&& ino && ino->i_sock && MINOR(ino->i_rdev) != 1
	&& filep->private_data
	&& ((struct T_private *)filep->private_data)->pfirst)
		return 1;
#endif

	if (sock_select)
		return (*sock_select)(ino, filep, sel_type, wait);
	return 0;
}


static void
socksys_close(struct inode *ino, struct file *filep)
{
	if (ino && ino->i_sock) {
#ifdef EMU_XTI
		if (MINOR(ino->i_rdev) != 1 && filep->private_data) {
			struct T_primsg *it;
			it = ((struct T_private *)filep->private_data)->pfirst;
			while (it) {
				struct T_primsg *tmp = it;
				it = it->next;
				kfree(tmp);
			}
			kfree(filep->private_data);
		}
#endif
		sock_close(ino, filep);
	}

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_SOCKSYS)
		printk(KERN_DEBUG "socksys: [%d] %lx closed\n",
			current->pid, (unsigned long)filep);
#endif
	MOD_DEC_USE_COUNT;
}


static int socksys_major;


void
init_socksys(void)
{
	/* N.B. this is coded to allow for the possibility of auto
	 * assignment of major number by passing 0 and seeing what we
	 * get back. This isn't possible since I haven't reworked the
	 * device subsystems yet :-).
	 */
	socksys_major = register_chrdev(SOCKSYS_MAJOR, "socksys", &socksys_fops);
	if (socksys_major < 0) {
		printk(KERN_ERR "iBCS: couldn't register socksys on character major %d\n",
			SOCKSYS_MAJOR);
	} else {
		if (!socksys_major)
			socksys_major = SOCKSYS_MAJOR;
#ifdef IBCS_TRACE
		printk(KERN_INFO "iBCS: socksys registered on character major %d\n", socksys_major);
#endif
	}
}


void
cleanup_socksys(void)
{
	/* Remove the socksys socket interface to streams based TCP/IP */
	if (socksys_major > 0 && unregister_chrdev(socksys_major, "socksys") != 0)
		printk(KERN_ERR "iBCS: couldn't unregister socksys device!\n");
}

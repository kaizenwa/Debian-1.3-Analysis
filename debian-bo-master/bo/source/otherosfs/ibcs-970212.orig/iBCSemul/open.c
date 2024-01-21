/*
 *  linux/ibcs/open.c
 *
 *  Copyright (C) 1993  Joe Portman (baron@hebron.connected.com)
 *  Copyright (C) 1993, 1994  Drew Sullivan (re-worked for iBCS2)
 *
 * $Id: open.c,v 1.33 1997/02/10 22:37:52 jaggy Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/open.c,v $
 */

/* Keep track of which struct definition we really want */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/vfs.h>
#include <linux/types.h>
#include <linux/utime.h>
#include <linux/errno.h>
#include <linux/fcntl.h>
#include <linux/stat.h>
#include <linux/string.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/signal.h>
#include <linux/tty.h>
#include <linux/time.h>
#include <linux/malloc.h>
#include <linux/un.h>

#include <ibcs/ibcs.h>

#ifdef __NR_getdents
#include <linux/dirent.h>
#endif

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif


#ifdef __cplusplus
extern "C" 
#endif

/* ISC (at least) assumes O_CREAT if O_TRUNC is given. This is emulated
 * here but is it correct for iBCS in general? Do we care?
 */
static unsigned short fl_ibcs_to_linux[] = {
	0x0001, 0x0002, 0x0800, 0x0400, 0x1000, 0x0000, 0x0000, 0x0800,
	0x0040, 0x0240, 0x0080, 0x0100, 0x0000, 0x0000, 0x0000, 0x0000
};

static unsigned short fl_linux_to_ibcs[] = {
	0x0001, 0x0002, 0x0000, 0x0000, 0x0000, 0x0000, 0x0100, 0x0400,
	0x0800, 0x0200, 0x0008, 0x0004, 0x0010, 0x0000, 0x0000, 0x0000
};

static inline unsigned short map_flags(unsigned short f, unsigned short map[])
{
	int i;
	unsigned short m, r;

	r = 0;
	for (i=0,m=1; i < 16; i++,m<<=1)
		if (f & m)
			r |= map[i];

	return r;
}


int ibcs_statfs(const char * path, struct ibcs_statfs * buf, int len, int fstype)
{
	struct inode * inode;
	struct statfs lxstat;
	struct ibcs_statfs ibcsstat;
	int error;
	int old_fs;

	if (len > (int)sizeof(struct ibcs_statfs))
		return -EINVAL;

	error = verify_area(VERIFY_WRITE, buf, len);
	if (error)
		return error;

	if (!fstype) {
		error = namei(path,&inode);
		if (error)
			return error;
		if (!inode->i_sb->s_op->statfs) {
			iput(inode);
			return -ENOSYS;
		}
		old_fs = get_fs();
		set_fs(get_ds());

#ifdef __NR_getdents
		inode->i_sb->s_op->statfs(inode->i_sb, &lxstat, sizeof(lxstat));
#else
		inode->i_sb->s_op->statfs(inode->i_sb, &lxstat);
#endif

		set_fs(old_fs);
		iput(inode);

		ibcsstat.f_type = lxstat.f_type;
		ibcsstat.f_bsize = lxstat.f_bsize;
		ibcsstat.f_frsize = 0;
		ibcsstat.f_blocks = lxstat.f_blocks;
		ibcsstat.f_bfree = lxstat.f_bfree;
		ibcsstat.f_files = lxstat.f_files;
		ibcsstat.f_ffree = lxstat.f_ffree;
		memset(ibcsstat.f_fname, 0, sizeof(ibcsstat.f_fname));
		memset(ibcsstat.f_fpack, 0, sizeof(ibcsstat.f_fpack));

		/* Finally, copy it to the user's buffer */
		memcpy_tofs(buf, &ibcsstat, len);
		return 0;
	}

	/* Linux can't stat unmounted filesystems so we
	 * simply lie and claim 100MB of 1GB is free. Sorry.
	 */
	ibcsstat.f_bsize = 1024;
	ibcsstat.f_frsize = 0;
	ibcsstat.f_blocks = 1024 * 1024;	/* 1GB */
	ibcsstat.f_bfree = 100 * 1024;		/* 100MB */
	ibcsstat.f_files = 60000;
	ibcsstat.f_ffree = 50000;
	memset(ibcsstat.f_fname, 0, sizeof(ibcsstat.f_fname));
	memset(ibcsstat.f_fpack, 0, sizeof(ibcsstat.f_fpack));

	/* Finally, copy it to the user's buffer */
	memcpy_tofs(buf, &ibcsstat, len);
	return 0;
}

#ifdef __cplusplus
extern "C" 
#endif
int ibcs_fstatfs(unsigned int fd, struct ibcs_statfs * buf, int len, int fstype)
{
	struct inode * inode;
	struct file * file;
	struct statfs lxstat;
	struct ibcs_statfs ibcsstat;
	int error;
	int old_fs;

	if (len > (int)sizeof(struct ibcs_statfs))
		return -EINVAL;

	error = verify_area(VERIFY_WRITE, buf, len);
	if (error)
		return error;

	if (!fstype) {
		if (fd >= NR_OPEN || !(file = current->FD[fd]))
			return -EBADF;
		if (!(inode = file->f_inode))
			return -ENOENT;
		if (!inode->i_sb->s_op->statfs)
			return -ENOSYS;

		old_fs = get_fs();
		set_fs(get_ds());

#ifdef __NR_getdents
		inode->i_sb->s_op->statfs(inode->i_sb, &lxstat, sizeof(lxstat));
#else
		inode->i_sb->s_op->statfs(inode->i_sb, &lxstat);
#endif

		set_fs(old_fs);

		ibcsstat.f_type = lxstat.f_type;
		ibcsstat.f_bsize = lxstat.f_bsize;
		ibcsstat.f_frsize = 0;
		ibcsstat.f_blocks = lxstat.f_blocks;
		ibcsstat.f_bfree = lxstat.f_bfree;
		ibcsstat.f_files = lxstat.f_files;
		ibcsstat.f_ffree = lxstat.f_ffree;
		memset(ibcsstat.f_fname, 0, sizeof(ibcsstat.f_fname));
		memset(ibcsstat.f_fpack, 0, sizeof(ibcsstat.f_fpack));

		/* Finally, copy it to the user's buffer */
		memcpy_tofs(buf, &ibcsstat, len);
		return 0;
	}

	/* Linux can't stat unmounted filesystems so we
	 * simply lie and claim 100MB is of 1GB free. Sorry.
	 */
	ibcsstat.f_bsize = 1024;
	ibcsstat.f_frsize = 0;
	ibcsstat.f_blocks = 1024 * 1024;	/* 1GB */
	ibcsstat.f_bfree = 100 * 1024;		/* 100MB */
	ibcsstat.f_files = 60000;
	ibcsstat.f_ffree = 50000;
	memset(ibcsstat.f_fname, 0, sizeof(ibcsstat.f_fname));
	memset(ibcsstat.f_fpack, 0, sizeof(ibcsstat.f_fpack));

	/* Finally, copy it to the user's buffer */
	memcpy_tofs(buf, &ibcsstat, len);
	return 0;
}


int ibcs_mkdir(const char *fname, int mode)
{
	int error, old_fs;
	char *tmp, *p;

	if ((error = getname(fname, &tmp)))
		return error;

	/* Drop any trailing slash */
	for (p=tmp; *p; p++);
	p--;
	if (*p == '/')
		*p = '\0';

	old_fs = get_fs();
	set_fs(get_ds());
	error = SYS(mkdir)(tmp, mode);
	set_fs(old_fs);

	putname(tmp);
	return error;
}


int ibcs_mknod(const char *fname, int mode, int dev)
{
	/* Linux doesn't allow us to create a directory with mknod(). */
	if ((mode & 0017000) == 0040000)
		return ibcs_mkdir(fname, mode);
	return SYS(mknod)(fname, mode, dev);
}


int ibcs_open(const char *fname, int flag, int mode)
{
#ifdef __sparc__
	return SYS(open)(fname, map_flags(flag, fl_ibcs_to_linux), mode);
#else
	int error, fd, old_fs, args[3];
	char *p;
	struct sockaddr_un addr;

	fd = SYS(open)(fname, map_flags(flag, fl_ibcs_to_linux), mode);
	if (fd < 0)
		return fd;

	/* Sometimes a program may open a pathname which it expects
	 * to be a named pipe (or STREAMS named pipe) when the
	 * Linux domain equivalent is a Unix domain socket. (e.g.
	 * UnixWare uses a STREAMS named pipe /dev/X/Nserver.0 for
	 * X :0 but Linux uses a Unix domain socket /tmp/.X11-unix/X0)
	 * It isn't enough just to make the symlink because you cannot
	 * open() a socket and read/write it. If we spot the error we can
	 * switch to socket(), connect() and things will likely work
	 * as expected however.
	 */
	if (!S_ISSOCK(current->FD[fd]->f_inode->i_mode))
		return fd;

	SYS(close)(fd);
	args[0] = AF_UNIX;
	args[1] = SOCK_STREAM;
	args[2] = 0;
	old_fs = get_fs();
	set_fs(get_ds());
	fd = SYS(socketcall)(SYS_SOCKET, args);
	set_fs(old_fs);
	if (fd < 0)
		return fd;

	if ((error = getname(fname, &p))) {
		SYS(close)(fd);
		return error;
	}
	if (strlen(p) >= UNIX_PATH_MAX) {
		putname(p);
		SYS(close)(fd);
		return -E2BIG;
	}
	addr.sun_family = AF_UNIX;
	strcpy(addr.sun_path, p);
	putname(p);

	args[0] = fd;
	args[1] = (int)&addr;
	args[2] = sizeof(struct sockaddr_un);
	set_fs(get_ds());
	error = SYS(socketcall)(SYS_CONNECT, args);
	set_fs(old_fs);
	if (error) {
		SYS(close)(fd);
		return error;
	}

	return fd;
#endif
}


/* If/when the readdir function is changed to read multiple entries
 * at once this should be updated to take advantage of the fact.
 *
 * N.B. For Linux the reclen in a dirent is the number of characters
 * in the filename, for SCO (at least) reclen is the total size of
 * the particular dirent rounded up to the next multiple of 4. The SCO
 * behaviour is faithfully emulated here.
 *
 * XXXX
 * We don't truncate long filenames at all when copying. If we meet a
 * long filename and the buffer supplied by the application simply isn't
 * big enough to hold it we'll return without filling the buffer (i.e
 * return 0). The application will see this as a (premature) end of
 * directory. Is there a work around for this at all???
 */
int ibcs_getdents(int fd, char *buf, int nbytes)
{
	int error, here, posn, reclen;
	struct file *file;
	struct dirent *d;
	int old_fs;

	error = verify_area(VERIFY_WRITE, buf, nbytes);
	if (error)
		return error;

	/* Check the file handle here. This is so we can access the current
	 * position in the file structure safely without a tedious call
	 * to sys_lseek that does nothing useful.
	 */
	if (fd < 0 || fd >= NR_OPEN
	|| !(file=current->FD[fd])
	|| !(file->f_inode))
		return -EBADF;

	d = (struct dirent *)__get_free_page(GFP_KERNEL);
	if (!d)
		return -ENOMEM;

	error = posn = reclen = 0;
	while (posn + reclen < nbytes) {
		/* Save the current position and get another dirent */
		here = file->f_pos;
		old_fs = get_fs();
		set_fs (get_ds());
		error = SYS(readdir)(fd, d, 1);
		set_fs(old_fs);
		if (error <= 0)
			break;

		/* If it'll fit in the buffer save it otherwise back up
		 * so it is read next time around.
		 * Oh, if we're at the beginning of the buffer there's
		 * no chance that this entry will ever fit so don't
		 * copy it and don't back off - we'll just pretend it
		 * isn't here...
		 */
		reclen = (sizeof(long) + sizeof(off_t)
			+ sizeof(unsigned short) + d->d_reclen + 1
			+ 3) & (~3);
		if (posn + reclen <= nbytes) {
			/* According to SCO d_ino is only long in iBCS2
			 * mode. Otherwise we have a short d_ino followed
			 * by a short pad.
			 * XXX - How do we know if the program was built
			 * in iBCS mode?
			 * N.B. This attempts to match the way stat and
			 * read fold long inodes to shorts.
			 */
			if ((unsigned long)d->d_ino & 0xffff)
				d->d_ino = (unsigned long)d->d_ino & 0xffff;
			else
				d->d_ino = 0xfffe;
			d->d_reclen = reclen;
			d->d_off = file->f_pos;
			memcpy_tofs(buf+posn, d, reclen);
			posn += reclen;
		} else if (posn) {
			SYS(lseek)(fd, here, 0);
		} /* else posn == 0 */
	}

	/* Loose the intermediate buffer. */
	free_page((unsigned long)d);

	/* If we've put something in the buffer return the byte count
	 * otherwise return the error status.
	 */
	return ((posn > 0) ? posn : error);
}


struct ibcs_flock {
	short l_type;	/* numbers don't match */
	short l_whence;
	off_t l_start;
	off_t l_len;	/* 0 means to end of file */
	short l_sysid;
	short l_pid;
};


int
ibcs_fcntl(struct pt_regs *regs)
{
	int arg1, arg2, arg3;
	int error, retval;

#ifndef __sparc__
	error = verify_area(VERIFY_READ,
			((unsigned long *)regs->esp)+1,
			3*sizeof(long));
	if (error)
		return error;
#endif /* __sparc__ */
	arg1 = get_syscall_parameter (regs, 0);
	arg2 = get_syscall_parameter (regs, 1);
	arg3 = get_syscall_parameter (regs, 2);

	switch (arg2) {
		/* These match the Linux commands. */
		case 0: /* F_DUPFD */
		case 1: /* F_GETFD */
		case 2: /* F_SETFD */
			return SYS(fcntl)(arg1, arg2, arg3);

		/* The iBCS flags don't match Linux flags. */
		case 3: /* F_GETFL */
			return map_flags(SYS(fcntl)(arg1, arg2, arg3),
					fl_linux_to_ibcs);
		case 4: /* F_SETFL */
			arg3 = map_flags(arg3, fl_ibcs_to_linux);
			return SYS(fcntl)(arg1, arg2, arg3);

		/* The lock stucture is different. */
		case 14: /* F_GETLK SVR4 */
			arg2 = 5;
			/* fall through */
		case 5: /* F_GETLK */
		case 6: /* F_SETLK */
		case 7: /* F_SETLKW */
#ifdef __sparc__
		        (((struct ibcs_flock *)arg3)->l_type)--;
#else /* __sparc__ */
			__asm__("decw %%fs:%0"
				: /* no outputs */
				: "m" (((struct ibcs_flock *)arg3)->l_type));
#endif /* __sparc__ */
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
				struct ibcs_flock fl;

  				memcpy_fromfs(&fl,(void *)arg3,sizeof(fl));
				printk (KERN_DEBUG "iBCS: lock l_type: %d l_whence: %d l_start: %lu l_len: %lu l_sysid: %d l_pid: %d\n",
							fl.l_type,
							fl.l_whence,
							fl.l_start,
							fl.l_len,
							fl.l_sysid,
							fl.l_pid);

				}
#endif
			/* okay do some magic here */
			retval = SYS(fcntl)(arg1, arg2, arg3);
#ifdef __sparc__
			(((struct ibcs_flock *)arg3)->l_type)++;
#else /* __sparc__ */
			__asm__("incw %%fs:%0"
				: /* no outputs */
				: "m" (((struct ibcs_flock *)arg3)->l_type));
#endif /* __sparc__ */
			/* Linux doesn't have the sysid field so we need to
			 * roll the pid down a slot.
			 */
			put_fs_word(
				get_fs_word(&(((struct ibcs_flock *)arg3)->l_sysid)),
				&(((struct ibcs_flock *)arg3)->l_pid));
			put_fs_word(0,
				&(((struct ibcs_flock *)arg3)->l_sysid));

			return retval;

		case 10: /* F_ALLOCSP */
			/* Extend allocation for specified portion of file. */
		case 11: /* F_FREESP */
			/* Free a portion of a file. */
			return 0;

		/* These are intended to support the Xenix chsize() and
		 * rdchk() system calls. I don't know if these may be
		 * generated by applications or not.
		 */
		case 0x6000: /* F_CHSIZE */
			return SYS(ftruncate)(arg1, arg3);
#ifndef __sparc__
		case 0x6001: /* F_RDCHK */
			return xnx_rdchk(arg1);
#endif /* __sparc__ */
		/* The following are defined but reserved and unknown. */
		case  8: /* F_CHKFL */

		/* These are made from the Xenix locking() system call.
		 * According to available documentation these would
		 * never be generated by an application - only by the
		 * kernel Xenix support.
		 */
		case 0x6300: /* F_LK_UNLCK */
		case 0x7200: /* F_LK_LOCK */
		case 0x6200: /* F_LK_NBLCK */
		case 0x7100: /* F_LK_RLCK */
		case 0x6100: /* F_LK_NBRLCK */

		default:
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
					printk(KERN_ERR "iBCS: unsupported fcntl 0x%lx, arg 0x%lx\n",
					(unsigned long)arg2, (unsigned long)arg3);
			}
#endif
			return -EINVAL;
			break;
	}
			
			
}

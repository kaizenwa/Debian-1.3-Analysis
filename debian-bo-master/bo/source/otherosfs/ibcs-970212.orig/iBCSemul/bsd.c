/*
 *  linux/ibcs/bsd.c
 *
 *  Copyright (C) 1994  Mike Jagdis
 *
 * $Id: bsd.c,v 1.13 1996/07/26 12:03:32 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/bsd.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/fs.h>
#include <linux/malloc.h>
#include <linux/param.h>
#include <linux/ptrace.h>
#include <linux/sched.h>
#include <linux/string.h>

#include <ibcs/bsd.h>
#include <ibcs/ibcs.h>

#ifdef __NR_getdents
#include <linux/dirent.h>
#endif

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif


int
bsd_getpagesize(void)
{
	return EXEC_PAGESIZE;
}


int
bsd_geteuid(void)
{
	return current->euid;
}


int
bsd_getegid(void)
{
	return current->egid;
}


int
bsd_sbrk(unsigned long n)
{
	unsigned long newbrk;

	if (!n)
		return current->MM(brk);
	newbrk = current->MM(brk);
	if ((unsigned long)SYS(brk)(newbrk) != newbrk)
		return -ENOMEM;
	return 0;
}


int
bsd_getdtablesize(void)
{
	return NR_OPEN;
}


int
bsd_killpg(int pgrp, int sig)
{
	return SYS(kill)(-pgrp, sig);
}


int
bsd_setegid(int egid)
{
	return SYS(setregid)(-1, egid);
}


int
bsd_seteuid(int euid)
{
	return SYS(setreuid)(-1, euid);
}


static unsigned short fl_bsd_to_linux[] = {
	0x0000, 0x0001, 0x0002, 0x0800, 0x0400, 0x0000, 0x0080, 0x2000,
	0x1000, 0x0040, 0x0200, 0x0080, 0x0000, 0x0000, 0x0000, 0x0000
};

static unsigned short fl_linux_to_bsd[] = {
	0x0000, 0x0001, 0x0002, 0x0000, 0x0000, 0x0000, 0x0000, 0x0200,
	0x0800, 0x0000, 0x0400, 0x0008, 0x0004, 0x0080, 0x0040, 0x0000
};

static inline unsigned short map_flags(unsigned short f, unsigned short map[])
{
	int i;
	unsigned short m, r;

	r = 0;
	for (i=1,m=1; i < 16; i++,m<<=1)
		if (f & m)
			r |= map[i];

	return r;
}

int
bsd_open(const char * fname, int flag, int mode)
{
	return SYS(open)(fname, map_flags(flag, fl_bsd_to_linux), mode);
}

int
bsd_fcntl(struct pt_regs *regs)
{
	int arg1, arg2, arg3;
	int error, retval;

#ifndef __sparc__
	error = verify_area(VERIFY_READ,
			((unsigned long *)regs->esp)+1,
			3*sizeof(long));
	if (error)
		return error;
#endif
	arg1 = get_syscall_parameter (regs, 0);
	arg2 = get_syscall_parameter (regs, 1);
	arg3 = get_syscall_parameter (regs, 2);

	switch (arg2) {
		/* These match the Linux commands. */
		case 0: /* F_DUPFD */
		case 1: /* F_GETFD */
		case 2: /* F_SETFD */
			return SYS(fcntl)(arg1, arg2, arg3);

		/* The BSD flags don't match Linux flags. */
		case 3: /* F_GETFL */
			return map_flags(SYS(fcntl)(arg1, arg2, arg3),
					fl_linux_to_bsd);
		case 4: /* F_SETFL */
			arg3 = map_flags(arg3, fl_bsd_to_linux);
			return SYS(fcntl)(arg1, arg2, arg3);

		case 5: /* F_GETOWN */
			return SYS(fcntl)(arg1, F_GETOWN, arg3);
		case 6: /* F_SETOWN */
			return SYS(fcntl)(arg1, F_SETOWN, arg3);

		/* The lock types start at 1 for BSD, 0 for Linux. */
		case 7: /* F_GETLK */
		case 8: /* F_SETLK */
		case 9: /* F_SETLKW */
#ifdef __sparc__
		        (((struct flock *)arg3)->l_type)--;
#else
			__asm__("decw %%fs:%0"
				: /* no outputs */
				: "m" (((struct flock *)arg3)->l_type));
#endif
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
				struct flock fl;

  				memcpy_fromfs(&fl,(void *)arg3,sizeof(fl));
				printk (KERN_DEBUG "BSD: lock l_type: %d l_whence: %d l_start: %lu l_len: %lu l_pid: %d\n",
							fl.l_type,
							fl.l_whence,
							fl.l_start,
							fl.l_len,
							fl.l_pid);

				}
#endif
			/* okay do some magic here */
			retval = SYS(fcntl)(arg1, arg2-2, arg3);
#ifdef __sparc__
			(((struct flock *)arg3)->l_type)++;
#else
			__asm__("incw %%fs:%0"
				: /* no outputs */
				: "m" (((struct flock *)arg3)->l_type));
#endif
			return retval;

		default:
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
					printk(KERN_ERR "BSD: unsupported fcntl 0x%lx, arg 0x%lx\n",
					(unsigned long)arg2, (unsigned long)arg3);
			}
#endif
			return -EINVAL;
			break;
	}
			
			
}


static void
copy_statfs(struct bsd_statfs *buf, struct statfs *st)
{
	struct bsd_statfs bsdst;

	bsdst.f_type = st->f_type;
	bsdst.f_flags = 0;
	bsdst.f_fsize =
	bsdst.f_bsize = st->f_bsize;
	bsdst.f_blocks = st->f_blocks;
	bsdst.f_bavail =
	bsdst.f_bfree = st->f_bfree;
	bsdst.f_files = st->f_files;
	bsdst.f_ffree = st->f_ffree;
	bsdst.f_fsid = 0;
	memset(bsdst.f_mntonname, 0, sizeof(bsdst.f_mntonname));
	memset(bsdst.f_mntfromname, 0, sizeof(bsdst.f_mntfromname));

	/* Finally, copy it to the user's buffer */
	memcpy_tofs(buf, &bsdst, sizeof(bsd_statfs));
}

int
bsd_statfs(const char *path, struct bsd_statfs *buf)
{
	struct inode *inode;
	struct statfs st;
	int error;
	int old_fs;

	error = verify_area(VERIFY_WRITE, buf, sizeof(struct bsd_statfs));
	if (error)
		return error;

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
	inode->i_sb->s_op->statfs(inode->i_sb, &st, sizeof(st));
#else
	inode->i_sb->s_op->statfs(inode->i_sb, &st);
#endif
	set_fs(old_fs);
	iput(inode);

	copy_statfs(buf, &st);
	return 0;
}

int
bsd_fstatfs(unsigned int fd, struct bsd_statfs *buf)
{
	struct inode *inode;
	struct file *file;
	struct statfs st;
	int error;
	int old_fs;

	error = verify_area(VERIFY_WRITE, buf, sizeof(struct bsd_statfs));
	if (error)
		return error;
	if (fd >= NR_OPEN || !(file = current->FD[fd]))
		return -EBADF;
	if (!(inode = file->f_inode))
		return -ENOENT;
	if (!inode->i_sb->s_op->statfs)
		return -ENOSYS;

	old_fs = get_fs();
	set_fs(get_ds());
#ifdef __NR_getdents
	inode->i_sb->s_op->statfs(inode->i_sb, &st, sizeof(st));
#else
	inode->i_sb->s_op->statfs(inode->i_sb, &st);
#endif
	set_fs(old_fs);

	copy_statfs(buf, &st);
	return 0;
}


/* If/when the readdir function is changed to read multiple entries
 * at once this should be updated to take advantage of the fact.
 *
 * XXXX
 * We don't truncate long filenames at all when copying. If we meet a
 * long filename and the buffer supplied by the application simply isn't
 * big enough to hold it we'll return without filling the buffer (i.e
 * return 0). The application will see this as a (premature) end of
 * directory. Is there a work around for this at all???
 */
int
bsd_getdirentries(int fd, char *buf, int nbytes, char *end_posn)
{
	int error, here, posn;
	struct file *file;
	struct dirent *d;
	struct bsd_dirent *bsd_d;
	int old_fs;

	error = verify_area(VERIFY_WRITE, buf, nbytes);
	if (error)
		return error;

	if (end_posn) {
		error = verify_area(VERIFY_WRITE, end_posn, sizeof(long));
		if (error)
			return error;
	}

	/* Check the file handle here. This is so we can access the current
	 * position in the file structure safely without a tedious call
	 * to sys_lseek that does nothing useful.
	 */
	if (fd < 0 || fd >= NR_OPEN
	|| !(file=current->FD[fd])
	|| !(file->f_inode))
		return -EBADF;

	d = (struct dirent *)get_free_page(GFP_KERNEL);
	if (!d)
		return -ENOMEM;
	bsd_d = (struct bsd_dirent *)get_free_page(GFP_KERNEL);
	if (!bsd_d) {
		free_page((unsigned long)d);
		return -ENOMEM;
	}

	error = posn = bsd_d->d_reclen = 0;
	while (posn + bsd_d->d_reclen < nbytes) {
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
		bsd_d->d_reclen = (d->d_reclen + 1 + sizeof(long)
				+ 2 * sizeof(short) + 3) & ~3;
		if (posn + bsd_d->d_reclen <= nbytes) {
			bsd_d->d_fileno = d->d_ino;
			bsd_d->d_namlen = d->d_reclen;
			strcpy(bsd_d->d_name, d->d_name);
			memcpy_tofs(buf+posn, bsd_d, bsd_d->d_reclen);
			posn += bsd_d->d_reclen;
		} else if (posn) {
			SYS(lseek)(fd, here, 0);
		} /* else posn == 0 */
	}

	free_page((unsigned long)bsd_d);
	free_page((unsigned long)d);

	if (end_posn)
		put_fs_long(file->f_pos, end_posn);

	/* If we've put something in the buffer return the byte count
	 * otherwise return the error status.
	 */
	return ((posn > 0) ? posn : error);
}

/*
 *  linux/ibcs/xstat.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 *  Hacked by Eric Youngdale for iBCS (1993, 1994).
 *  Added to by Drew Sullivan, modified by EY for xstat (used by SVr4).
 *
 * $Id: xstat.c,v 1.10 1996/01/12 17:27:21 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/xstat.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/kernel.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <linux/fs.h>
#include <linux/errno.h>
#include <linux/mm.h>
#include <linux/sched.h>
#include <linux/sys.h>
#include <ibcs/ibcs.h>
#include <ibcs/abi4.h>
#include <ibcs/trace.h>

/*
 * The xstat interface is used by SVr4, and is effectively an extension
 * to stat.  The general idea is that stat has some limitations (chiefly
 * 16 bit inode numbers), and the solution in SVr4 was to add an entirely
 * new syscall.  The /usr/include/sys/stat.h header file defines stat as xstat
 * so that the new interface is used.  The one advantage of xstat is that
 * we pass a version number so that it is possible to tell exactly what
 * the application is expecting, and it is easy to do the right thing.
 * There is usually an inline wrapper function in /usr/include/sys/stat.h
 * to perform this conversion.
 */

#define R3_MKNOD_VERSION	1	/* SVr3 */
#define R4_MKNOD_VERSION	2	/* SVr4 */
#define R3_STAT_VERSION		1	/* SVr3 */
#define R4_STAT_VERSION		2	/* SVr4 */
#define SCO_STAT_VERSION	51	/* SCO OS5 */

/* Various functions to provide compatibility between the linux
   syscalls and the ABI ABI compliant calls */

/* Convert a linux dev number into the SVr4 equivalent. */
#define R4_DEV(DEV) ((DEV & 0xff) | ((DEV & 0xff00) << 10))


struct sco_xstat {
	short		st_dev;
	long		__pad1[3];
	unsigned long	st_ino;
	unsigned short	st_mode;
	short		st_nlink;
	unsigned short	st_uid;
	unsigned short	st_gid;
	short		st_rdev;
	long		__pad2[2];
	long		st_size;
	long		__pad3;
	long		st_atime;
	long		st_mtime;
	long		st_ctime;
	long		st_blksize;
	long		st_blocks;
	char		st_fstype[16];
	long		__pad4[7];
	long		st_sco_flags;
};


/*
 * st_blocks and st_blksize are approximated with a simple algorithm if
 * they aren't supported directly by the filesystem. The minix and msdos
 * filesystems don't keep track of blocks, so they would either have to
 * be counted explicitly (by delving into the file itself), or by using
 * this simple algorithm to get a reasonable (although not 100% accurate)
 * value.
 *
 * Use minix fs values for the number of direct and indirect blocks.  The
 * count is now exact for the minix fs except that it counts zero blocks.
 * Everything is in BLOCK_SIZE'd units until the assignment to
 * tmp.st_blksize.
 */
static void
set_blocks(struct inode *inode, long *st_blksize, long *st_blocks)
{
	long blocks, indirect;

#define D_B   7
#define I_B   (BLOCK_SIZE / sizeof(unsigned short))

	if (!inode->i_blksize) {
		blocks = (inode->i_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
		if (blocks > D_B) {
			indirect = (blocks - D_B + I_B - 1) / I_B;
			blocks += indirect;
			if (indirect > 1) {
				indirect = (indirect - 1 + I_B - 1) / I_B;
				blocks += indirect;
				if (indirect > 1)
					blocks++;
			}
		}
		*st_blocks = (BLOCK_SIZE / 512) * blocks;
		*st_blksize = BLOCK_SIZE;
	} else {
		*st_blocks = inode->i_blocks;
		*st_blksize = inode->i_blksize;
	}
}


static void
cp_sco_xstat(struct inode * inode, struct sco_xstat * statbuf)
{
	struct sco_xstat tmp = {0, };

	tmp.st_dev = inode->i_dev;
	tmp.st_ino = inode->i_ino;
	tmp.st_mode = inode->i_mode;
	tmp.st_nlink = inode->i_nlink;
	tmp.st_uid = inode->i_uid;
	tmp.st_gid = inode->i_gid;
	tmp.st_rdev = inode->i_rdev;
	tmp.st_size = inode->i_size;
	tmp.st_atime = inode->i_atime;
	tmp.st_mtime = inode->i_mtime;
	tmp.st_ctime = inode->i_ctime;
	set_blocks(inode, &tmp.st_blksize, &tmp.st_blocks);
	strcpy(tmp.st_fstype, "ext2");
	tmp.st_sco_flags = 0; /* 1 if remote */
	memcpy_tofs(statbuf, &tmp, sizeof(tmp));
}


static void
cp_svr4_xstat(struct inode * inode, struct svr4_xstat * statbuf)
{
	struct svr4_xstat tmp = {0, };

	tmp.st_dev = R4_DEV(inode->i_dev);
	tmp.st_ino = inode->i_ino;
	tmp.st_mode = inode->i_mode;
	tmp.st_nlink = inode->i_nlink;
	tmp.st_uid = inode->i_uid;
	tmp.st_gid = inode->i_gid;
	tmp.st_rdev = R4_DEV(inode->i_rdev);
	tmp.st_size = inode->i_size;
	tmp.st_atim.tv_sec = inode->i_atime;
	tmp.st_mtim.tv_sec = inode->i_mtime;
	tmp.st_ctim.tv_sec = inode->i_ctime;
	set_blocks(inode, &tmp.st_blksize, &tmp.st_blocks);
	strcpy(tmp.st_fstype, "ext2");
	memcpy_tofs(statbuf, &tmp, sizeof(tmp));
}


int
ibcs_xstat(int vers, char *path, void *buf)
{
	int error;
	struct inode *inode;

	if (vers == R3_STAT_VERSION)
		return ibcs_stat(path, (struct ibcs_stat *)buf);

	error = verify_area(VERIFY_WRITE, buf,
			vers == SCO_STAT_VERSION
				? sizeof(struct sco_xstat)
				: sizeof(struct svr4_xstat));
	if (error)
		return error;


	error = namei(path, &inode);
	if (error)
		return error;

	switch (vers) {
		case R4_STAT_VERSION:
			cp_svr4_xstat(inode, buf);
			iput(inode);
			return 0;

		case SCO_STAT_VERSION:
			cp_sco_xstat(inode, buf);
			iput(inode);
			return 0;
	}

	iput(inode);

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_API)
		printk(KERN_DEBUG "iBCS: [%d] xstat version %d not supported\n",
			current->pid, vers);
#endif
	return -EINVAL;
}


int
ibcs_lxstat(int vers, char *path, void *buf)
{
	int error;
	struct inode *inode;

	if (vers == R3_STAT_VERSION)
		return ibcs_lstat(path, (struct ibcs_stat *)buf);

	error = verify_area(VERIFY_WRITE, buf,
			vers == SCO_STAT_VERSION
				? sizeof(struct sco_xstat)
				: sizeof(struct svr4_xstat));
	if (error)
		return error;

	error = lnamei(path, &inode);
	if (error)
		return error;

	switch(vers) {
		case R4_STAT_VERSION:
			cp_svr4_xstat(inode, buf);
			iput(inode);
			return 0;

		case SCO_STAT_VERSION:
			cp_sco_xstat(inode, buf);
			iput(inode);
			return 0;
	}

	iput(inode);

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_API)
		printk(KERN_DEBUG "iBCS: [%d] lxstat version %d not supported\n",
			current->pid, vers);
#endif
	return -EINVAL;
}


int
ibcs_fxstat(int vers, int fd, void *buf)
{
	int error;

	if (vers == R3_STAT_VERSION)
		return ibcs_fstat(fd, (struct ibcs_stat *)buf);

	error = verify_area(VERIFY_WRITE, buf,
			vers == SCO_STAT_VERSION
				? sizeof(struct sco_xstat)
				: sizeof(struct svr4_xstat));
	if (error)
		return error;

	if (fd >= NR_OPEN
	|| !current->FD[fd] || !current->FD[fd]->f_inode)
		return -EBADF;

	switch(vers) {
		case R4_STAT_VERSION:
			cp_svr4_xstat(current->FD[fd]->f_inode, buf);
			return 0;

		case SCO_STAT_VERSION:
			cp_sco_xstat(current->FD[fd]->f_inode, buf);
			return 0;
	}

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_API)
		printk(KERN_DEBUG "iBCS: [%d] fxstat version %d not supported\n",
			current->pid, vers);
#endif
	return -EINVAL;
}


int
ibcs_xmknod(int vers, const char * path, mode_t mode, dev_t dev)
{
	unsigned int major, minor;

	switch(vers) {
		case R3_MKNOD_VERSION:
			return SYS(mknod)(path, mode, dev);

		case R4_MKNOD_VERSION:
			minor = dev & 0x3ffff;
			major = dev >> 18;
			if (minor > 0xff || major > 0xff)
				return -EINVAL;
			return SYS(mknod)(path, mode, ((major << 8) | minor));
	}

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_API)
		printk(KERN_DEBUG "iBCS: [%d] xmknod version %d not supported\n",
			current->pid, vers);
#endif
	return -EINVAL;
}


/*
 * The following code implements the statvfs function used by SVr4.
 */

static int
cp_statvfs(struct inode * inode, struct abi_statvfs * statbuf)
{
	unsigned int old_fs;
	int error;
	struct abi_statvfs tmp = {0, };
	struct statfs src;

	old_fs = get_fs();
	set_fs (get_ds());
	error = SYS(statfs)(inode->i_sb, &src);
	set_fs(old_fs);

	if (error) return error;

	tmp.f_blocks = src.f_blocks;

	tmp.f_bsize = src.f_bsize; 
	tmp.f_frsize = 0;
	tmp.f_blocks = src.f_blocks;;
	tmp.f_bfree = src.f_bfree;
	tmp.f_bavail = src.f_bavail;
	tmp.f_files = src.f_files;
	tmp.f_free = src.f_ffree;
	tmp.f_sid = inode->i_sb->s_dev;

	/* Get the name of the filesystem */
	strcpy(tmp.f_basetype, inode->i_sb->s_type->name);

	tmp.f_flag = 0;
	if (IS_RDONLY(inode)) tmp.f_flag |= 1;
	if (IS_NOSUID(inode)) tmp.f_flag |= 2;

	tmp.f_namemax = src.f_namelen;
    
	memcpy_tofs(statbuf,&tmp,sizeof(tmp));
	return 0;
}


int
abi_statvfs(char * path, struct abi_statvfs * stat)
{
	int error;
	struct inode * inode;
  
	error = verify_area(VERIFY_WRITE, stat, sizeof (*stat));
	if (error)
		return error;
  
	error = namei(path, &inode);
	if (error)
		return error;
 
	if (!inode->i_sb->s_op->statfs) {
		iput(inode);
		return -ENOSYS;
	}

	error = cp_statvfs(inode, stat);
	iput(inode);
	return error;
}


int
abi_fstatvfs(int fd, struct abi_statvfs * stat)
{
	int error;
	struct file * f;
	struct inode * inode;

	error = verify_area(VERIFY_WRITE, stat, sizeof (*stat));
	if (error)
		return error;

	if (fd >= NR_OPEN || !(f=current->FD[fd]) || !(inode=f->f_inode))
		return -EBADF;

	if (!inode->i_sb->s_op->statfs)
		return -ENOSYS;

	error = cp_statvfs(inode,stat);
	return error;
}

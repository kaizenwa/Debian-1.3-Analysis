/*
 *  linux/ibcs/bsdstat.c
 *
 *  Copyright (C) 1994  Mike Jagdis
 *
 * $Id: bsdstat.c,v 1.7 1995/12/12 10:31:04 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/bsdstat.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/errno.h>
#include <linux/stat.h>
#include <linux/fs.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>

#include <ibcs/bsd.h>
#include <ibcs/ibcs.h>


void
cp_bsd_stat(struct inode *inode, struct bsd_stat *st)
{
	struct bsd_stat tmp;

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
	tmp.st_blksize = inode->i_blksize;
	tmp.st_blocks = inode->i_blocks;
	tmp.st_flags = inode->i_flags;
	tmp.st_gen = 0;

	memcpy_tofs(st, &tmp, sizeof(struct bsd_stat));
}


int
bsd_stat(char *filename, struct bsd_stat *st)
{
	struct inode *inode;
	int error;

	error = verify_area(VERIFY_WRITE, st, sizeof(struct bsd_stat));
	if (error)
		return error;

	error = namei(filename, &inode);
	if (error)
		return error;

	cp_bsd_stat(inode, st);
	iput(inode);

	return 0;
}

int
bsd_lstat(char *filename, struct bsd_stat *st)
{
	struct inode *inode;
	int error;

	error = verify_area(VERIFY_WRITE, st, sizeof(struct bsd_stat));
	if (error)
		return error;

	error = lnamei(filename, &inode);
	if (error)
		return error;

	cp_bsd_stat(inode, st);
	iput(inode);

	return 0;
}

int
bsd_fstat(unsigned int fd, struct bsd_stat *st)
{
	struct file *f;
	struct inode *inode;
	int error;

	error = verify_area(VERIFY_WRITE, st, sizeof(struct bsd_stat));
	if (error)
		return error;

	if (fd >= NR_OPEN || !(f=current->FD[fd]) || !(inode=f->f_inode))
		return -EBADF;

	cp_bsd_stat(inode, st);

	return 0;
}

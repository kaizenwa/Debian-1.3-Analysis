/*
 *  linux/ibcs/stat.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 *  Hacked by Eric Youngdale for iBCS.
 *  Added to by Drew Sullivan.
 *
 * $Id: stat.c,v 1.11 1997/02/10 22:37:53 jaggy Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/stat.c,v $
 */
#define __KERNEL__ 1

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

#include <ibcs/ibcs.h>


#ifdef __sparc__
static void cp_ibcs_stat(struct inode * inode, struct ibcs_stat * statbuf)
{
	struct ibcs_stat tmp;

	memset ((void *) &tmp, 0, sizeof (tmp));
	tmp.st_dev = inode->i_dev;
	tmp.st_ino = inode->i_ino;
	tmp.st_mode = inode->i_mode;
	tmp.st_nlink = inode->i_nlink;
	tmp.st_uid = inode->i_uid;
	tmp.st_gid = inode->i_gid;
	tmp.st_rdev = inode->i_rdev;
	tmp.st_size = inode->i_size;
	tmp.st_atime.tv_sec = inode->i_atime;
	tmp.st_mtime.tv_sec = inode->i_mtime;
	tmp.st_ctime.tv_sec = inode->i_ctime;
	tmp.st_blksize = inode->i_blksize;
	tmp.st_blocks  = inode->i_blocks;
	memcpy_tofs(statbuf,&tmp,sizeof(tmp));
}

#else /* if not sparc... */

/*
 * Believe it or not, the original stat structure is compatible with ibcs2.
 * The xstat struct used by SVr4 is different than our new struct, but we will
 * deal with that later
 */
static void cp_ibcs_stat(struct inode * inode, struct ibcs_stat * statbuf)
{
	struct ibcs_stat tmp;

	/* Note that we have to fold a long inode number down to a short.
	 * This must match what happens in open.c:ibcs_getdents() since code
	 * that figures out cwd needs the inodes to match. I don't know
	 * if it should match coff.c:ibcs_read() on a directory. From
	 * tests it seems that that always sets a large inode number to
	 * 0xfffe. In theory this would break old pwd routines but SCO
	 * seems to do it. Many getcwd() library function just do a
	 * piped "pwd" anyway...
	 */
	if ((unsigned long)inode->i_ino & 0xffff)
		tmp.st_ino = (unsigned long)inode->i_ino & 0xffff;
	else
		tmp.st_ino = 0xfffe;

	tmp.st_dev = inode->i_dev;
	tmp.st_mode = inode->i_mode;
	tmp.st_nlink = inode->i_nlink;
	tmp.st_uid = inode->i_uid;
	tmp.st_gid = inode->i_gid;
	tmp.st_rdev = inode->i_rdev;
	tmp.st_size = inode->i_size;
	tmp.st_atime = inode->i_atime;
	tmp.st_mtime = inode->i_mtime;
	tmp.st_ctime = inode->i_ctime;
	memcpy_tofs(statbuf,&tmp,sizeof(tmp));
}
#endif /* not sparc */

#ifdef __cplusplus
extern "C" 
#endif
int ibcs_stat(char * filename, struct ibcs_stat * statbuf)
{
	struct inode * inode;
	int error;

	error = verify_area(VERIFY_WRITE,statbuf,sizeof (*statbuf));
	if (error)
		return error;
	error = namei(filename,&inode);
	if (error)
		return error;
	cp_ibcs_stat(inode,statbuf);
	iput(inode);
	return 0;
}

#ifdef __cplusplus
extern "C" 
#endif
int ibcs_lstat(char * filename, struct ibcs_stat * statbuf)
{
	struct inode * inode;
	int error;

	error = verify_area(VERIFY_WRITE,statbuf,sizeof (*statbuf));
	if (error)
		return error;
	error = lnamei(filename,&inode);
	if (error)
		return error;
	cp_ibcs_stat(inode,statbuf);
	iput(inode);
	return 0;
}

#ifdef __cplusplus
extern "C" 
#endif
int ibcs_fstat(unsigned int fd, struct ibcs_stat * statbuf)
{
	struct file * f;
	struct inode * inode;
	int error;

	error = verify_area(VERIFY_WRITE,statbuf,sizeof (*statbuf));
	if (error)
		return error;
	if (fd >= NR_OPEN || !(f=current->FD[fd]) || !(inode=f->f_inode))
		return -EBADF;
	cp_ibcs_stat(inode,statbuf);
	return 0;
}

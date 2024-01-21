#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <linux/types.h>
#include <linux/errno.h>
#include <linux/fs.h>
#include <linux/fcntl.h>
#include <linux/major.h>
#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif
#define __KERNEL__ 1

#include <linux/kernel.h>
#include <linux/net.h>
#include <linux/sched.h>
#include <linux/signal.h>
#include <linux/socket.h>
#include <linux/utsname.h>
#include <linux/time.h>
#include <linux/termios.h>
#include <linux/sys.h>


/* Zero for auto allocation. The major number allocated can be found
 * by looking in /proc/devices after the module is loaded. Auto
 * allocation of major numbers is a fairly new kernel feature. If you
 * aren't using a recent 1.1.x kernel you may have to set an explicit
 * major number here.
 */
#define DEVTRACE_MAJOR	0


char kernel_version[] = UTS_RELEASE;


static int
devtrace_lseek(struct inode *inode, struct file *file, off_t offset, int whence)
{
	printk(KERN_DEBUG "devtrace: [%d] %lx: lseek to 0x%08lx whence=%d\n",
		current->pid, (unsigned long)file,
		offset, whence);

	/* Easy way out. Most devices we wish to trace are either
	 * STREAMS devices or character devices which aren't seekable
	 * anyway.
	 */
	return -ESPIPE;
}


static int
devtrace_read(struct inode *inode, struct file *file, char *buf, int size)
{
	printk(KERN_DEBUG "devtrace: [%d] %lx: read %d bytes to 0x%lx\n",
		current->pid, (unsigned long)file,
		size, (unsigned long)buf);

	return size;
}


static int
devtrace_write(struct inode *inode, struct file *file, char *buf, int size)
{
	printk(KERN_DEBUG "devtrace: [%d] %lx: write %d bytes from 0x%lx\n",
		current->pid, (unsigned long)file,
		size, (unsigned long)buf);

	return size;
}


static int
devtrace_ioctl(struct inode *inode, struct file *file,
		unsigned int cmd, unsigned long arg)
{
	static char *code[] = {
		"",		"I_NREAD",	"I_PUSH",	"I_POP",
		"I_LOOK",	"I_FLUSH",	"I_SRDOPT",	"I_GRDOPT",
		"I_STR",	"I_SETSIG",	"I_GETSIG",	"I_FIND",
		"I_LINK",	"I_UNLINK",	"I_PEEK",	"I_FDINSERT",
		"I_SENDFD",	"I_RECVFD"
	};

	printk(KERN_DEBUG "devtrace: [%d] %lx: ioctl 0x%x (%s) with argument 0x%lx requested\n",
		current->pid, (unsigned long)file,
		cmd, (cmd & 0xff) <= 18 ? code[(cmd & 0xff)] : "????",
		(unsigned long)arg);

	return 0;
}


static int
devtrace_open(struct inode *ino, struct file *filep)
{
	MOD_INC_USE_COUNT;

	printk(KERN_DEBUG "devtrace: [%d] %lx opening\n",
		current->pid, (unsigned long)filep);

	return 0;
}


static void
devtrace_close(struct inode *ino, struct file *filep)
{
	MOD_DEC_USE_COUNT;

	printk(KERN_DEBUG "devtrace: [%d] %lx closed\n",
		current->pid, (unsigned long)filep);
}


static struct file_operations devtrace_fops = {
	devtrace_lseek,		/* lseek */
	devtrace_read,		/* read */
	devtrace_write,		/* write */
	NULL,			/* readdir */
	NULL,			/* select */
	devtrace_ioctl,		/* ioctl */
	NULL,			/* mmap */
	devtrace_open,		/* open */
	devtrace_close,		/* close */
	NULL			/* fsync */
};


static int devtrace_major;


int
init_module(void)
{
	devtrace_major = register_chrdev(DEVTRACE_MAJOR, "devtrace", &devtrace_fops);
	if (devtrace_major < 0) {
		printk(KERN_INFO "iBCS: couldn't register devtrace on character major %d\n",
			DEVTRACE_MAJOR);
		return 1;
	} else {
		if (!devtrace_major)
			devtrace_major = DEVTRACE_MAJOR;
		printk(KERN_INFO "iBCS: devtrace registered on character major %d\n", devtrace_major);

		return 0;
	}
}


void
cleanup_module(void)
{
	if (MOD_IN_USE)
		printk(KERN_INFO "iBCS: devtrace module is in use, remove delayed\n");

	if (devtrace_major > 0 && unregister_chrdev(devtrace_major, "devtrace") != 0)
		printk(KERN_NOTICE "iBCS: couldn't unregister devtrace device!\n");
}

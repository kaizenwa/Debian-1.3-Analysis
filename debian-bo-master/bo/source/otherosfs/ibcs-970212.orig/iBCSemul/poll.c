/*
 * This file contains the procedures for the handling of poll.
 *
 * Copyright (C) 1994 Eric Youngdale
 *
 * Created for Linux based loosely upon linux select code, which
 * in turn is loosely based upon Mathius Lattner's minix
 * patches by Peter MacDonald. Heavily edited by Linus.
 *
 * Poll is used by SVr4 instead of select, and it has considerably
 * more functionality.  Parts of it are related to STREAMS, and since
 * we do not have streams, we fake it.  In fact, select() still exists
 * under SVr4, but libc turns it into a poll() call instead.  We attempt
 * to do the inverse mapping.
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/types.h>
#include <linux/time.h>
#include <linux/fs.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/stat.h>
#include <linux/signal.h>
#include <linux/errno.h>
#include <linux/malloc.h>

#include <asm/system.h>

#include <ibcs/ibcs.h>
#include <ibcs/tli.h>

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif


#define ROUND_UP(x,y) (((x)+(y)-1)/(y))

#define POLLIN 1
#define POLLPRI 2
#define POLLOUT 4
#define POLLERR 8
#define POLLHUP 16
#define POLLNVAL 32
#define POLLRDNORM 64
#define POLLWRNORM POLLOUT
#define POLLRDBAND 128
#define POLLWRBAND 256

#define LINUX_POLLIN (POLLRDNORM | POLLRDBAND | POLLIN)
#define LINUX_POLLOUT (POLLWRBAND | POLLWRNORM | POLLOUT)
#define LINUX_POLLERR (POLLERR)

static inline void free_wait(select_table * p)
{
	struct select_table_entry * entry = p->entry + p->nr;

	while (p->nr > 0) {
		p->nr--;
		entry--;
		remove_wait_queue(entry->wait_address,&entry->wait);
	}
}


/* Copied directly from fs/select.c */

static int check(int flag, select_table * wait, struct file * file)
{
	struct inode * inode;
	struct file_operations *fops;
	int (*select) (struct inode *, struct file *, int, select_table *);

	inode = file->f_inode;
	if ((fops = file->f_op) && (select = fops->select))
		return select(inode, file, flag, wait)
		    || (wait && select(inode, file, flag, NULL));
	if (S_ISREG(inode->i_mode))
		return 1;
	return 0;
}


int ibcs_poll(struct poll * ufds, size_t nfds, int timeout)
{
        int i,j, count, fdcount, error;
	struct poll * fdpnt;
	struct poll * fds, *fds1;
	select_table wait_table, *wait;
	struct select_table_entry *entry;

	if ((error = verify_area(VERIFY_READ, ufds, nfds*sizeof(struct poll))))
		return error;

	if (nfds > NR_OPEN)
		return -EINVAL;

	if (!(entry = (struct select_table_entry*)__get_free_page(GFP_KERNEL))
	|| !(fds = (struct poll *)kmalloc(nfds*sizeof(struct poll), GFP_KERNEL)))
		return -ENOMEM;

	memcpy_fromfs(fds, ufds, nfds*sizeof(struct poll));

	if (timeout < 0)
		current->timeout = 0x7fffffff;
	else {
		current->timeout = jiffies + ROUND_UP(timeout, (1000/HZ));
		if (current->timeout <= jiffies)
			current->timeout = 0;
	}

	count = 0;
	wait_table.nr = 0;
	wait_table.entry = entry;
	wait = &wait_table;

	for(fdpnt = fds, j = 0; j < (int)nfds; j++, fdpnt++) {
		i = fdpnt->fd;
		fdpnt->revents = 0;
#ifdef IBCS_TRACE
		if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace)
			printk(KERN_DEBUG "iBCS: %d: want fd=%d events=0x%04x\n",
				current->pid, fdpnt->fd, fdpnt->events);
#endif
		if (!current->FD[i] || !current->FD[i]->f_inode)
			fdpnt->revents = POLLNVAL;
	}
repeat:
	current->state = TASK_INTERRUPTIBLE;
	for(fdpnt = fds, j = 0; j < (int)nfds; j++, fdpnt++) {
		i = fdpnt->fd;

		if(i < 0) continue;
		if (!current->FD[i] || !current->FD[i]->f_inode) continue;

#ifdef EMU_XTI
		if (fdpnt->events & POLLPRI
		&& current->FD[i]->f_inode->i_sock
		&& Priv(i) && Priv(i)->pfirst
		&& Priv(i)->pfirst->pri == MSG_HIPRI) {
			fdpnt->revents |= POLLPRI;
			count++;
		} else
#endif
		if ((fdpnt->events & LINUX_POLLIN)
		&& check(SEL_IN, wait, current->FD[i])) {
			int event = 0;

			if (fdpnt->events & POLLIN)
				event = POLLIN;
			if (fdpnt->events & POLLRDNORM)
				event = POLLRDNORM;
#if 0
			if (fdpnt->events & POLLRDBAND)
				event = POLLRDBAND;
#endif
			fdpnt->revents |= event;
			count++;
			wait = NULL;
		}

		if ((fdpnt->events & LINUX_POLLOUT) &&
		check(SEL_OUT, wait, current->FD[i])) {
			fdpnt->revents |= (LINUX_POLLOUT & fdpnt->events);
			count++;
			wait = NULL;
		}

		if (check(SEL_EX, wait, current->FD[i])) {
			fdpnt->revents |= POLLHUP;
			count++;
			wait = NULL;
		}
	}

	if ((current->signal & (~current->blocked)))
		return -EINTR;

	wait = NULL;
	if (!count && current->timeout > jiffies) {
		schedule();
		goto repeat;
	}

	free_wait(&wait_table);
	free_page((unsigned long) entry);

	/* OK, now copy the revents fields back to user space. */
	fds1 = fds;
	fdcount = 0;
	for(i=0; i < (int)nfds; i++, ufds++, fds++) {
		if (fds->revents) {
			fdcount++;
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace)
				printk(KERN_DEBUG "iBCS: %d: got fd=%d, events=0x%04x\n",
					current->pid, fds->fd, fds->revents);
#endif
		}
		put_fs_word(fds->revents, &ufds->revents);
	}
	kfree(fds1);
	current->timeout = 0;
	current->state = TASK_RUNNING;
	return fdcount;
}

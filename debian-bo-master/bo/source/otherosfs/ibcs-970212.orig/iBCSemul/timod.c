/*
 *  linux/ibcs/timod.c
 *
 *  Copyright 1995, 1996  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: timod.c,v 1.21 1996/07/26 12:03:40 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/timod.c,v $
 */

#ifdef EMU_XTI

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/types.h>
#include <linux/errno.h>
#include <linux/fs.h>
#include <linux/ptrace.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/malloc.h>
#include <linux/mm.h>
#include <linux/fcntl.h>
#include <linux/socket.h>
#include <linux/in.h>
#include <linux/un.h>

#include <ibcs/ibcs.h>
#include <ibcs/tli.h>

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif

#ifdef __sparc__
/* ibcs2 wants to access the stack pointer, let's give it an alias
 * for the sparc
 */
#define esp u_regs[UREG_FP]
#endif

extern void kill_fasync(struct fasync_struct *fa, int sig);

#ifdef IBCS_TRACE
static char *
xti_prim(int n)
{
	char *tab[] = {
		"T_CONN_REQ", "T_CONN_RES", "T_DISCON_REQ", "T_DATA_REQ",
		"T_EXDATA_REQ", "T_INFO_REQ", "T_BIND_REQ", "T_UNBIND_REQ",
		"T_UNITDATA_REQ", "T_OPTMGMT_REQ", "T_ORDREL_REQ",
		"T_CONN_IND", "T_CONN_CON", "T_DISCON_IND", "T_DATA_IND",
		"T_EXDATA_IND", "T_INFO_ACK", "T_BIND_ACK", "T_ERROR_ACK",
		"T_OK_ACK", "T_UNITDATA_IND", "T_UDERROR_IND",
		"T_OPTMGMT_ACK", "T_ORDREL_IND"
	};

	if (n < 0 || n >= sizeof(tab)/sizeof(tab[0]))
		return "<unknown>";
	return tab[n];
}
#endif


#define timod_mkctl(len) kmalloc(sizeof(struct T_primsg)-sizeof(long)+len, \
					GFP_KERNEL)


static void
timod_socket_wakeup(int fd)
{
	struct socket *sock;

	sock = &current->FD[fd]->f_inode->u.socket_i;

	wake_up_interruptible(sock->wait);
	if (sock->fasync_list && !(sock->flags & SO_WAITDATA))
		kill_fasync(sock->fasync_list, SIGIO);
}


static void
timod_ok(int fd, int prim)
{
	struct T_primsg *it;

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_STREAMS)
		printk(KERN_DEBUG "iBCS: [%d] %lx ok ack prim=%d\n",
			current->pid, (unsigned long)current->FD[fd],
			prim);
#endif
	it = timod_mkctl(sizeof(struct T_ok_ack));
	if (it) {
		struct T_ok_ack *ok = (struct T_ok_ack *)&it->type;
		ok->PRIM_type = T_OK_ACK;
		ok->CORRECT_prim = prim;
		it->pri = MSG_HIPRI;
		it->length = sizeof(struct T_ok_ack);
		it->next = Priv(fd)->pfirst;
		Priv(fd)->pfirst = it;
		if (!Priv(fd)->plast)
			Priv(fd)->plast = it;
		timod_socket_wakeup(fd);
	}
}


static void
timod_error(int fd, int prim, int terr, int uerr)
{
	struct T_primsg *it;

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_STREAMS)
		printk(KERN_DEBUG "iBCS: [%d] %lx error prim=%d, TLI=%d, UNIX=%d\n",
			current->pid, (unsigned long)current->FD[fd],
			prim, terr, uerr);
#endif
	it = timod_mkctl(sizeof(struct T_error_ack));
	if (it) {
		struct T_error_ack *err = (struct T_error_ack *)&it->type;
		err->PRIM_type = T_ERROR_ACK;
		err->ERROR_prim = prim;
		err->TLI_error = terr;
		err->UNIX_error = iABI_errors(uerr);
		it->pri = MSG_HIPRI;
		it->length = sizeof(struct T_error_ack);
		it->next = Priv(fd)->pfirst;
		Priv(fd)->pfirst = it;
		if (!Priv(fd)->plast)
			Priv(fd)->plast = it;
		timod_socket_wakeup(fd);
	}
}


#if defined(EMU_XTI_OPTMGMT) || defined(EMU_TLI_OPTMGMT)

#  if defined(EMU_XTI_OPTMGMT) && defined(EMU_TLI_OPTMGMT)
#    error unable to support _both_ TLI and XTI option management
	/* This is because TLI and XTI options buffers are
	 * incompatible and there is no clear way to detect
	 * which format we are dealing with here. Existing
	 * systems appear to have TLI options management
	 * implemented but return TNOTSUPPORT for XTI requests.
	 */
#  endif

static int
timod_optmgmt(int fd, struct pt_regs *regs,
	int flag, char *opt_buf, int opt_len, int do_ret)
{
	int is_tli, error, failed;
	unsigned long old_esp, *tsp;
	char *ret_buf, *ret_base;
	int ret_len, ret_space;

	if (opt_buf && opt_len > 0) {
		error = verify_area(VERIFY_READ, opt_buf, opt_len);
		if (error)
			return error;
	}

	/* FIXME: We should be able to detect the difference between
	 * TLI and XTI requests at run time?
	 */
#ifdef EMU_TLI_OPTMGMT
	is_tli = 1;
#else
	is_tli = 0;
#endif

	if (!do_ret && (!opt_buf || opt_len <= 0))
		return 0;

	/* Grab some space on the user stack to work with. We need 6 longs
	 * to build an argument frame for [gs]etsockopt calls. We also
	 * need space to build the return buffer. This will be at least
	 * as big as the given options buffer but the given options
	 * buffer may not include space for option values so we allow one
	 * long for each option multiple of the option header size
	 * and hope that big options will not exhaust our space and
	 * trash the stack.
	 */
	ret_space = 4*PAGE_SIZE + opt_len
		+ sizeof(long)*(opt_len / (is_tli ? sizeof(struct opthdr) : sizeof(struct t_opthdr)));
	ret_buf = ret_base = (char *)(regs->esp - ret_space);
	ret_len = 0;

	old_esp = regs->esp;
	regs->esp -= ret_space + 6*sizeof(long);
	tsp = (unsigned long *)(regs->esp);
	error = verify_area(VERIFY_WRITE, tsp, 6*sizeof(long));
	if (error) {
		regs->esp = old_esp;
		return error;
	}

	failed = 0;

#ifndef EMU_TLI_OPTMGMT
	if (is_tli) {
		printk(KERN_WARNING
			"iBCS: TLI optmgmt requested but not supported\n");
	}
#else
	if (is_tli) while (opt_len >= sizeof(struct opthdr)) {
		struct opthdr opt;

#ifdef IBCS_TRACE
		if ((ibcs_trace & TRACE_STREAMS)) {
			printk(KERN_DEBUG "iBCS: TLI optmgmt opt_len=%d, "
				"ret_buf=0x%08lx, ret_len=%d, ret_space=%d\n",
				opt_len, (unsigned long)ret_buf,
				ret_len, ret_space);
		}
#endif
		memcpy_fromfs(&opt, opt_buf, sizeof(struct opthdr));
		if (opt.len > opt_len) {
			failed = TBADOPT;
			break;
		}

#ifdef IBCS_TRACE
		if ((ibcs_trace & TRACE_STREAMS)) {
			printk(KERN_DEBUG "iBCS: TLI optmgmt fd=%d, level=%ld,"
				" name=%ld, value=%ld\n",
				fd, opt.level, opt.name,
				get_fs_long(opt_buf+sizeof(struct opthdr)));
		}
#endif
		/* Check writable space in the return buffer. */
		error = verify_area(VERIFY_WRITE, ret_buf, sizeof(struct opthdr));
		if (error) {
			failed = TSYSERR;
			break;
		}

		/* Flag values:
		 * T_NEGOTIATE means try and set it.
		 * T_DEFAULT means get the default value.
		 *           (return the current for now)
		 * T_CHECK means get the current value.
		 */
		error = 0;
		if (flag == T_NEGOTIATE) {
			put_fs_long(fd, tsp);
			put_fs_long(opt.level, tsp+1);
			put_fs_long(opt.name, tsp+2);
			put_fs_long((int)(opt_buf+sizeof(struct opthdr)), tsp+3);
			put_fs_long(opt_len, tsp+4);
			error = ibcs_setsockopt(tsp);
#ifdef IBCS_TRACE
			if (error && (ibcs_trace & TRACE_STREAMS)) {
				printk(KERN_DEBUG "iBCS: setsockopt failed: %d\n",
					error);
			}
#endif
			if (error) {
				failed = TBADOPT;
				break;
			}
		}
		if (!error) {
			put_fs_long(fd, tsp);
			put_fs_long(opt.level, tsp+1);
			put_fs_long(opt.name, tsp+2);
			put_fs_long((int)(ret_buf+sizeof(struct opthdr)), tsp+3);
			put_fs_long((int)(tsp+5), tsp+4);
			put_fs_long(ret_space, tsp+5);
			error = ibcs_getsockopt(tsp);
#ifdef IBCS_TRACE
			if (error && (ibcs_trace & TRACE_STREAMS)) {
				printk(KERN_DEBUG "iBCS: getsockopt failed: %d\n",
					error);
			}
#endif
			if (error) {
				failed = TBADOPT;
				break;
			}

			error = get_fs_long(tsp+5);
			memcpy_tofs(ret_buf, &opt, sizeof(opt));
			put_fs_long(error,
				&((struct opthdr *)opt_buf)->len);
			ret_space -= sizeof(struct opthdr) + error;
			ret_len += sizeof(struct opthdr) + error;
			ret_buf += sizeof(struct opthdr) + error;
		}

		opt_len -= sizeof(struct opthdr) + opt.len;
		opt_buf += sizeof(struct opthdr) + opt.len;
	}
#endif /* EMU_TLI_OPTMGMT */
#ifndef EMU_XTI_OPTMGMT
	else {
		printk(KERN_WARNING
			"iBCS: XTI optmgmt requested but not supported\n");
	}
#else
	else while (opt_len >= sizeof(struct t_opthdr)) {
		struct t_opthdr opt;

		memcpy_fromfs(&opt, opt_buf, sizeof(struct t_opthdr));
		if (opt.len > opt_len) {
			failed = 1;
			break;
		}

#ifdef IBCS_TRACE
		if ((ibcs_trace & TRACE_STREAMS)) {
			printk(KERN_DEBUG "iBCS: XTI optmgmt fd=%d, level=%ld,"
				" name=%ld, value=%ld\n",
				fd, opt.level, opt.name,
				get_fs_long(opt_buf+sizeof(struct t_opthdr)));
		}
#endif
		/* Check writable space in the return buffer. */
		if (verify_area(VERIFY_WRITE, ret_buf, sizeof(struct t_opthdr))) {
			failed = 1;
			break;
		}

		/* Flag values:
		 * T_NEGOTIATE means try and set it.
		 * T_CHECK means see if we could set it.
		 *         (so we just set it for now)
		 * T_DEFAULT means get the default value.
		 *           (return the current for now)
		 * T_CURRENT means get the current value (SCO xti.h has
		 * no T_CURRENT???).
		 */
		error = 0;
		if (flag == T_NEGOTIATE || flag == T_CHECK) {
			put_fs_long(fd, tsp);
			put_fs_long(opt.level, tsp+1);
			put_fs_long(opt.name, tsp+2);
			put_fs_long((int)(opt_buf+sizeof(struct t_opthdr)), tsp+3);
			put_fs_long(opt_len-sizeof(struct t_opthdr), tsp+4);
			error = ibcs_setsockopt(tsp);
		}
		if (!error) {
			put_fs_long(fd, tsp);
			put_fs_long(opt.level, tsp+1);
			put_fs_long(opt.name, tsp+2);
			put_fs_long((int)(ret_buf+sizeof(struct t_opthdr)), tsp+3);
			put_fs_long((int)(tsp+5), tsp+4);
			put_fs_long(ret_space, tsp+5);
			error = ibcs_getsockopt(tsp);
			if (!error) {
				int len = get_fs_long(tsp+5);
				/* FIXME: opt.status should be set... */
				memcpy_tofs(ret_buf, &opt, sizeof(opt));
				put_fs_long(len+sizeof(struct t_opthdr),
					&((struct t_opthdr *)opt_buf)->len);
				ret_space -= sizeof(struct t_opthdr) + len;
				ret_len += sizeof(struct t_opthdr) + len;
				ret_buf += sizeof(struct t_opthdr) + len;
			}
		}

		failed |= error;
		opt_len -= opt.len;
		opt_buf += opt.len;
	}
#endif /* EMU_XTI_OPTMGMT */

#if 0
	/* If there is left over data the supplied options buffer was
	 * formatted incorrectly. But we might have done some work so
	 * we must fall through and return an acknowledgement I think.
	 */
	if (opt_len) {
		regs->esp = old_esp;
		return -EINVAL;
	}
#endif

	if (do_ret) {
		struct T_primsg *it;

		if (failed) {
			timod_error(fd, T_OPTMGMT_REQ, failed, -error);
			regs->esp = old_esp;
			return 0;
		}

#ifdef IBCS_TRACE
		if ((ibcs_trace & TRACE_STREAMS)) {
			printk(KERN_DEBUG "iBCS: optmgmt returns %d bytes,"
				" failed=%d\n",
				ret_len, failed);
		}
#endif
		/* Convert the return buffer in the user stack to a
		 * T_OPTMGMT_ACK
		 * message and queue it.
		 */
		it = timod_mkctl(sizeof(struct T_optmgmt_ack) + ret_len);
		if (it) {
			struct T_optmgmt_ack *ack
				= (struct T_optmgmt_ack *)&it->type;
			ack->PRIM_type = T_OPTMGMT_ACK;
			ack->OPT_length = ret_len;
			ack->OPT_offset = sizeof(struct T_optmgmt_ack);
			ack->MGMT_flags = (failed ? T_FAILURE : flag);
			memcpy_fromfs(((char *)ack)+sizeof(struct T_optmgmt_ack),
				ret_base, ret_len);
			it->pri = MSG_HIPRI;
			it->length = sizeof(struct T_optmgmt_ack) + ret_len;
			it->next = Priv(fd)->pfirst;
			Priv(fd)->pfirst = it;
			if (!Priv(fd)->plast)
				Priv(fd)->plast = it;
			timod_socket_wakeup(fd);
		}
	}

	regs->esp = old_esp;
	return 0;
}
#else /* no EMU_XTI_OPTMGMT or EMU_TLI_OPTMGMT */
static int
timod_optmgmt(int fd, struct pt_regs *regs,
	int flag, char *opt_buf, int opt_len, int do_ret)
{
	return -EINVAL;
}
#endif /* EMU_XTI_OPTMGMT or EMU_TLI_OPTMGMT */


static inline void
free_wait(select_table *p)
{
	struct select_table_entry *entry = p->entry + p->nr;

	while (p->nr > 0) {
		p->nr--;
		entry--;
		remove_wait_queue(entry->wait_address,&entry->wait);
	}
}


static int
do_getmsg(int fd, struct pt_regs *regs,
	char *ctl_buf, int ctl_maxlen, int *ctl_len,
	char *dat_buf, int dat_maxlen, int *dat_len,
	int *flags_p)
{
	int error;
	long old_esp;
	unsigned long *tsp;
	unsigned short oldflags;
	struct T_unitdata_ind udi;

#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_STREAMS) || ibcs_func_p->trace) {
		printk(KERN_DEBUG "iBCS: getmsg %d, 0x%lx[%d], 0x%lx[%d], %x\n",
			fd,
			(unsigned long)ctl_buf, ctl_maxlen,
			(unsigned long)dat_buf, dat_maxlen,
			*flags_p);
	}
#endif

	/* We need some user space to build syscall argument vectors
	 * later. Set it up now and page it in if necessary. This will
	 * avoid (most?) potential blocking after the select().
	 */
	old_esp = regs->esp;
	regs->esp -= 4*PAGE_SIZE;
	tsp = (unsigned long *)regs->esp;
	error = verify_area(VERIFY_WRITE, tsp, 6*sizeof(long));
	regs->esp = old_esp;
	if (error)
		return error;

	/* If this a SOCK_STREAM and is in the TS_IDLE state and we want
	 * control data but none is available we assume this is a probe
	 * for incoming connection events. If we haven't previously
	 * done a listen() we need to do one now.
	 */
	if (ctl_maxlen > 0 && !Priv(fd)->pfirst
	&& current->FD[fd]->f_inode->u.socket_i.type == SOCK_STREAM
	&& Priv(fd)->state == TS_IDLE) {
		regs->esp = (unsigned long)tsp;
		put_fs_long(fd, tsp);
		put_fs_long(-1, tsp+1);
		SYS(socketcall)(SYS_LISTEN, tsp);
		regs->esp = old_esp;
	}

	/* If the TEP is non-blocking we must use select() to test
	 * for something to do. We don't necessarily know what order
	 * events will be happening on the socket so we have to
	 * watch for evrything at once.
	 * N.B. If we weren't asked for data we should only be looking
	 * for connection requests. There are socket type issues to
	 * consider here.
	 */
	if (!(current->FD[fd]->f_flags & O_NONBLOCK)) {
		select_table wait_table, *wait;
		struct select_table_entry *entry;

		/* Select and wait, then do a non-blocking select to
		 * see if we are the first process to wake up or if
		 * someone else has already handled the event.
		 */
		entry = (struct select_table_entry *)__get_free_page(GFP_KERNEL);
		if (!entry)
			return -ENOMEM;
		wait_table.nr = 0;
		wait_table.entry = entry;
		wait = &wait_table;
repeat:
		current->state = TASK_INTERRUPTIBLE;

		/* Yes, this condition is a bugger to read... */
		if ((ctl_maxlen < 0
			|| (ctl_maxlen >= 0
				&& (!Priv(fd)->pfirst
				|| (*flags_p == MSG_HIPRI
					&& Priv(fd)->pfirst->pri != MSG_HIPRI)
				)
			)
		)
		&& (
#if 1
		/* Don't select for a high priority message. They aren't
		 * generated by the socket layer - only by timod actions.
		 */
		*flags_p == MSG_HIPRI ||
#endif
		(!current->FD[fd]->f_op->select(current->FD[fd]->f_inode,
				current->FD[fd], SEL_IN, wait)
		&& !current->FD[fd]->f_op->select(current->FD[fd]->f_inode,
				current->FD[fd], SEL_IN, NULL)
		&& !(current->signal & ~current->blocked)))) {
			schedule();
			goto repeat;
		}
		current->state = TASK_RUNNING;
		free_wait(&wait_table);
		free_page((unsigned long)entry);

		if ((current->signal & ~current->blocked))
			return -EINTR;
	}

	/* If we were asked for a control part and there is an outstanding
	 * message queued as a result of some other operation we'll
	 * return that.
	 */
	if (ctl_maxlen >= 0 && Priv(fd)->pfirst) {
		int l = ctl_maxlen <= Priv(fd)->pfirst->length
				? ctl_maxlen : Priv(fd)->pfirst->length;
		error = verify_area(VERIFY_WRITE, ctl_buf, l);
		if (error)
			return error;
#ifdef IBCS_TRACE
		if ((ibcs_trace & TRACE_STREAMS) || ibcs_func_p->trace) {
			printk(KERN_DEBUG "iBCS: priority message %ld %s\n",
				Priv(fd)->pfirst->type,
				xti_prim(Priv(fd)->pfirst->type));
		}
#endif
		memcpy_tofs(ctl_buf, ((char *)&Priv(fd)->pfirst->type)
					+ Priv(fd)->offset, l);
		put_fs_long(l, ctl_len);
		if (dat_maxlen >= 0)
			put_fs_long(0, dat_len);
		*flags_p = Priv(fd)->pfirst->pri;
		Priv(fd)->pfirst->length -= l;
		if (Priv(fd)->pfirst->length) {
			Priv(fd)->offset += l;
			return MORECTL;
		} else {
			struct T_primsg *it = Priv(fd)->pfirst;
			Priv(fd)->pfirst = it->next;
			if (!Priv(fd)->pfirst)
				Priv(fd)->plast = NULL;
			kfree(it);
			Priv(fd)->offset = 0;
			return 0;
		}
	}

	*flags_p = 0;

	/* If we have been asked for a control part we must probe the
	 * socket status and create an indication message if necessary.
	 * Be careful not to block while doing this. Either there is
	 * an event to process or the socket has data.
	 */
	if (ctl_maxlen >= 0) {
		/* If this a SOCK_STREAM and is in the TS_IDLE state
		 * we assume we are supposed to be looking for an
		 * incoming connection.
		 */
		if (current->FD[fd]->f_inode->u.socket_i.type == SOCK_STREAM
		&& Priv(fd)->state == TS_IDLE) {
			struct T_conn_ind ind;

/* FIXME: We are going to assume the user supplied buffer is big enough.
 * Strictly it should be but equally strictly we shouldn't assume it.
 */
			/* If the select() slept we may have had our temp
			 * space paged out. The re-verify_area is only really
			 * needed for pre-486 chips which don't handle write
			 * faults from kernel mode.
			 */
			regs->esp = (unsigned long)tsp;
			error = verify_area(VERIFY_WRITE, tsp, 3*sizeof(long));
			if (error) {
				regs->esp = old_esp;
				return error;
			}
			put_fs_long(fd, tsp);
			put_fs_long((unsigned long)(ctl_buf+sizeof(ind)), tsp+1);
			put_fs_long(ctl_maxlen-sizeof(ind), ctl_len);
			put_fs_long((int)ctl_len, tsp+2);

			/* We don't want to block in the accept(). Any
			 * blocking is handled by the select stuff above.
			 */
			oldflags = current->FD[fd]->f_flags;
			current->FD[fd]->f_flags |= O_NONBLOCK;
			error = SYS(socketcall)(SYS_ACCEPT, tsp);
			current->FD[fd]->f_flags = oldflags;

			regs->esp = old_esp;
			if (error < 0)
				return error;
			if (error) {
				ind.PRIM_type = T_CONN_IND;
				ind.SRC_length = get_fs_long(ctl_len);
				ind.SRC_offset = sizeof(ind);
				ind.OPT_length = ind.OPT_offset = 0;
				ind.SEQ_number = error;
				memcpy_tofs(ctl_buf, &ind, (int)sizeof(ind));
				put_fs_long(sizeof(ind)+ind.SRC_length,
					ctl_len);
				if (dat_maxlen >= 0)
					put_fs_long(0, dat_len);
				return 0;
			}
		}
	}

	/* If we weren't asked for data there is nothing more to do. */
	if (dat_maxlen <= 0) {
		if (dat_maxlen == 0)
			put_fs_long(0, dat_len);
		if (ctl_maxlen >= 0)
			put_fs_long(0, ctl_len);
		return -EAGAIN;
	}

	/* If the select() slept we may have had our temp space paged
	 * out. The re-verify_area is only really needed for pre-486
	 * chips which don't handle write faults from kernel mode.
	 */
	regs->esp = (unsigned long)tsp;
	error = verify_area(VERIFY_WRITE, tsp, 6*sizeof(long));
	if (error) {
		regs->esp = old_esp;
		return error;
	}
	put_fs_long(fd, tsp);
	put_fs_long((unsigned long)dat_buf, tsp+1);
	put_fs_long((dat_maxlen < 0 ? 0 : dat_maxlen), tsp+2);
	put_fs_long(0, tsp+3);
	if (ctl_maxlen > (int)sizeof(udi) && Priv(fd)->state == TS_IDLE) {
		put_fs_long((unsigned long)ctl_buf+sizeof(udi), tsp+4);
		put_fs_long(ctl_maxlen-sizeof(udi), ctl_len);
		put_fs_long((int)ctl_len, tsp+5);
	} else {
		put_fs_long(0, tsp+4);
		put_fs_long(0, ctl_len);
		put_fs_long((int)ctl_len, tsp+5);
	}

	/* We don't want to block in the recvfrom(). Any blocking is
	 * handled by the select stuff above.
	 */
	oldflags = current->FD[fd]->f_flags;
	current->FD[fd]->f_flags |= O_NONBLOCK;
	error = SYS(socketcall)(SYS_RECVFROM, tsp);
	current->FD[fd]->f_flags = oldflags;

	regs->esp = old_esp;
	if (error < 0)
		return error;
	if (error
	&& ctl_maxlen > (int)sizeof(udi)
	&& Priv(fd)->state == TS_IDLE) {
		udi.PRIM_type = T_UNITDATA_IND;
		udi.SRC_length = get_fs_long(ctl_len);
		udi.SRC_offset = sizeof(udi);
		udi.OPT_length = udi.OPT_offset = 0;
		memcpy_tofs(ctl_buf, &udi, (int)sizeof(udi));
		put_fs_long(sizeof(udi)+udi.SRC_length, ctl_len);
#if 0
if (ctl_buf && udi.SRC_length > 0) { int i;
char *buf = ctl_buf + sizeof(udi);
for (i=0; i<udi.SRC_length && i<64; i+=4)
printk(KERN_ERR "dat: 0x%08lx\n", get_fs_long(buf+i));
if (i != udi.SRC_length) printk(KERN_ERR "dat: ...\n");
}
#endif
	} else {
		put_fs_long(0, ctl_len);
	}
	put_fs_long(error, dat_len);

	return 0;
}


static int
do_putmsg(int fd, struct pt_regs *regs, char *ctl_buf, int ctl_len,
	char *dat_buf, int dat_len, int flags)
{
	int error, terror;

#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_STREAMS) || ibcs_func_p->trace) {
		printk(KERN_DEBUG "iBCS: putmsg %d, 0x%lx[%d], 0x%lx[%d], %x\n",
			fd,
			(unsigned long)ctl_buf, ctl_len,
			(unsigned long)dat_buf, dat_len,
			flags);
		printk(KERN_DEBUG "iBCS: putmsg prim: %ld %s\n",
			get_fs_long(ctl_buf),
			xti_prim(get_fs_long(ctl_buf)));
	}
#if 0
if (ctl_buf && ctl_len > 0) { int i;
for (i=0; i<ctl_len && i<64; i+=4)
printk(KERN_ERR "ctl: 0x%08lx\n", get_fs_long(ctl_buf+i));
if (i != ctl_len) printk(KERN_ERR "ctl: ...\n");
}
if (dat_buf && dat_len > 0) { int i;
for (i=0; i<dat_len && i<64; i+=4)
printk(KERN_ERR "dat: 0x%08lx\n", get_fs_long(dat_buf+i));
if (i != dat_len) printk(KERN_ERR "dat: ...\n");
}
#endif
#endif

	error = verify_area(VERIFY_READ, ctl_buf, sizeof(long));
	if (error)
		return error;

	switch (get_fs_long(ctl_buf)) {
		case T_BIND_REQ: {
			struct T_bind_req req;
			long old_esp;
			unsigned long *tsp;

#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx bind req\n",
					current->pid,
					(unsigned long)current->FD[fd]);
#endif
			error = verify_area(VERIFY_READ, ctl_buf, sizeof(req));
			if (error)
				return error;

			if (Priv(fd)->state != TS_UNBND) {
				timod_error(fd, T_BIND_REQ, TOUTSTATE, 0);
				return 0;
			}

			old_esp = regs->esp;
			regs->esp -= 4*PAGE_SIZE;
			tsp = (unsigned long *)(regs->esp);
			error = verify_area(VERIFY_WRITE, tsp, 3*sizeof(long));
			if (error) {
				timod_error(fd, T_BIND_REQ, TSYSERR, -error);
				regs->esp = old_esp;
				return 0;
			}

			memcpy_fromfs(&req, ctl_buf, sizeof(req));
			if (req.ADDR_offset && req.ADDR_length) {
				put_fs_long(fd, tsp);
				put_fs_long((unsigned long)ctl_buf
						+ req.ADDR_offset, tsp+1);
				put_fs_long(req.ADDR_length, tsp+2);
				error = SYS(socketcall)(SYS_BIND, tsp);
			} else {
				error = 0;
			}

			if (!error && req.CONIND_number) {
				put_fs_long(fd, tsp);
				put_fs_long(req.CONIND_number, tsp+1);
				SYS(socketcall)(SYS_LISTEN, tsp);
			}

			regs->esp = old_esp;

			if (!error) {
				struct T_primsg *it;
				it = timod_mkctl(ctl_len);
				if (it) {
					struct T_bind_ack *ack = (struct T_bind_ack *)&it->type;
					memcpy_fromfs(ack, ctl_buf, ctl_len);
					ack->PRIM_type = T_BIND_ACK;
					it->pri = MSG_HIPRI;
					it->length = ctl_len;
					it->next = NULL;
					Priv(fd)->state = TS_IDLE;
					timod_ok(fd, T_BIND_REQ);
					Priv(fd)->plast->next = it;
					Priv(fd)->plast = it;
					return 0;
				}
			}
			switch (error) {
				case -EINVAL:
					terror = TOUTSTATE;
					error = 0;
					break;
				case -EACCES:
					terror = TACCES;
					error = 0;
					break;
				case -EADDRNOTAVAIL:
				case -EADDRINUSE:
					terror = TNOADDR;
					error = 0;
					break;
				default:
					terror = TSYSERR;
					break;
			}
			timod_error(fd, T_BIND_REQ, terror, -error);
			return 0;
		}
		case T_CONN_RES: {
			struct T_conn_res *res = (struct T_conn_res *)ctl_buf;
			int conn_fd;
#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx connect accept(?)\n",
					current->pid,
					(unsigned long)current->FD[fd]);
#endif
			error = verify_area(VERIFY_READ, ctl_buf, sizeof(res));
			if (error)
				return error;

			conn_fd = get_fs_long(&res->SEQ_number);
			error = SYS(dup2)(conn_fd, fd);
			if (error < 0)
				return error;
			return 0;
		}
		case T_CONN_REQ: {
			struct T_conn_req req;
			long old_esp;
			unsigned short oldflags;
			unsigned long *tsp;
			struct T_primsg *it;

#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx connect req\n",
					current->pid,
					(unsigned long)current->FD[fd]);
#endif
			error = verify_area(VERIFY_READ, ctl_buf, sizeof(req));
			if (error)
				return error;

			if (Priv(fd)->state != TS_UNBND
			&& Priv(fd)->state != TS_IDLE) {
				timod_error(fd, T_CONN_REQ, TOUTSTATE, 0);
				return 0;
			}

			old_esp = regs->esp;
			regs->esp -= 4*PAGE_SIZE;
			tsp = (unsigned long *)(regs->esp);
			error = verify_area(VERIFY_WRITE, tsp, 3*sizeof(long));
			if (error) {
				timod_error(fd, T_CONN_REQ, TSYSERR, -error);
				regs->esp = old_esp;
				return 0;
			}
			memcpy_fromfs(&req, ctl_buf, sizeof(req));
			put_fs_long(fd, tsp);
			put_fs_long((unsigned long)ctl_buf + req.DEST_offset, tsp+1);
			put_fs_long(req.DEST_length, tsp+2);
#if 1
			/* Sheesh... ISC telnet seems to give the port
			 * number low byte first as I expected but the
			 * X programs seem to be giving high byte first.
			 * One is broken of course but clearly both
			 * should work. No, I don't understand this
			 * either but I can at least try...
			 * A better solution would be for you to change
			 * the definition of xserver0 in ISC's /etc/services
			 * but then it wouldn't work out of the box...
			 */
			if (current->personality == PER_SVR4) {
				struct sockaddr *sa;
				struct sockaddr_in *sin;

				sa = (struct sockaddr *)(ctl_buf
					+ req.DEST_offset);
				sin = (struct sockaddr_in *)sa;
				if (get_fs_word(&sa->sa_family) == AF_INET) {
					unsigned short port;
					port = get_fs_word(&sin->sin_port);
					if (port == 0x1770)
						put_fs_word(htons(port),
							&sin->sin_port);
				}
			}
#endif
			/* FIXME: We should honour non-blocking mode
			 * here but that means that the select probe
			 * needs to know that if select returns ok and
			 * we are in T_OUTCON we have a connection
			 * completion. This isn't so bad but the real
			 * problem is that the connection acknowledgement
			 * is supposed to contain the destination
			 * address.
			 */
			oldflags = current->FD[fd]->f_flags;
			current->FD[fd]->f_flags &= ~O_NONBLOCK;
			error = SYS(socketcall)(SYS_CONNECT, tsp);
			current->FD[fd]->f_flags = oldflags;
			regs->esp = old_esp;

			if (!error) {
				struct T_conn_con *con;

				it = timod_mkctl(ctl_len);
				if (!it)
					return -ENOMEM;
				it->length = ctl_len;
				con = (struct T_conn_con *)&it->type;
				memcpy_fromfs(con, ctl_buf, ctl_len);
				con->PRIM_type = T_CONN_CON;
				Priv(fd)->state = TS_DATA_XFER;
			} else {
				struct T_discon_ind *dis;

#ifdef IBCS_TRACE
				if (ibcs_trace & TRACE_STREAMS)
					printk(KERN_DEBUG "iBCS: [%d] %lx "
						"connect failed (errno=%d)\n",
						current->pid,
						(unsigned long)current->FD[fd],
						error);
#endif
				it = timod_mkctl(sizeof(struct T_discon_ind));
				if (!it)
					return -ENOMEM;
				it->length = sizeof(struct T_discon_ind);
				dis = (struct T_discon_ind *)&it->type;
				dis->PRIM_type = T_DISCON_IND;
				dis->DISCON_reason = iABI_errors(-error);
				dis->SEQ_number = 0;
			}
			timod_ok(fd, T_CONN_REQ);
			it->pri = 0;
			it->next = NULL;
			Priv(fd)->plast->next = it;
			Priv(fd)->plast = it;
			return 0;
		}

		case T_DISCON_REQ: {
			struct T_discon_req *req;

			error = verify_area(VERIFY_READ, ctl_buf, sizeof(req));
			if (error)
				return error;
			req = (struct T_discon_req *)ctl_buf;
#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx disconnect %ld\n",
					current->pid,
					(unsigned long)current->FD[fd],
					get_fs_long(&req->SEQ_number));
#endif
			fd = get_fs_long(&req->SEQ_number);
			/* Fall through... */
		}
		case T_ORDREL_REQ: {
			SYS(close)(fd);
			return 0;
		}

		case T_DATA_REQ: {
			long old_esp;
			unsigned long *tsp;

#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx data req\n",
					current->pid,
					(unsigned long)current->FD[fd]);
#endif
			if (Priv(fd)->state != TS_DATA_XFER) {
				return 0;
			}

			old_esp = regs->esp;
			regs->esp -= 4*PAGE_SIZE;
			tsp = (unsigned long *)(regs->esp);
			error = verify_area(VERIFY_WRITE, tsp, 6*sizeof(long));
			if (error) {
				regs->esp = old_esp;
				return 0;
			}
			put_fs_long(fd, tsp);
			put_fs_long((unsigned long)dat_buf, tsp+1);
			put_fs_long(dat_len, tsp+2);
			put_fs_long(0, tsp+3);
			error = SYS(socketcall)(SYS_SEND, tsp);
			regs->esp = old_esp;
			return error;
		}

		case T_UNITDATA_REQ: {
			struct T_unitdata_req req;
			long old_esp;
			unsigned long *tsp;

#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx unitdata req\n",
					current->pid,
					(unsigned long)current->FD[fd]);
#endif
			error = verify_area(VERIFY_READ, ctl_buf, sizeof(req));
			if (error)
				return error;

			if (Priv(fd)->state != TS_IDLE
			&& Priv(fd)->state != TS_DATA_XFER) {
				timod_error(fd, T_UNITDATA_REQ, TOUTSTATE, 0);
				return 0;
			}

			old_esp = regs->esp;
			regs->esp -= 4*PAGE_SIZE;
			tsp = (unsigned long *)(regs->esp);
			error = verify_area(VERIFY_WRITE, tsp, 6*sizeof(long));
			if (error) {
				timod_error(fd, T_UNITDATA_REQ, TSYSERR, -error);
				regs->esp = old_esp;
				return 0;
			}
			put_fs_long(fd, tsp);
			put_fs_long((unsigned long)dat_buf, tsp+1);
			put_fs_long(dat_len, tsp+2);
			put_fs_long(0, tsp+3);
			memcpy_fromfs(&req, ctl_buf, sizeof(req));
			if (req.DEST_length > 0) {
				put_fs_long((unsigned long)(ctl_buf+req.DEST_offset), tsp+4);
				put_fs_long(req.DEST_length, tsp+5);
				error = SYS(socketcall)(SYS_SENDTO, tsp);
				regs->esp = old_esp;
				return error;
			}
			error = SYS(socketcall)(SYS_SEND, tsp);
			regs->esp = old_esp;
			return error;
		}

		case T_UNBIND_REQ:
			Priv(fd)->state = TS_UNBND;
			timod_ok(fd, T_UNBIND_REQ);
			return 0;

		case T_OPTMGMT_REQ: {
			struct T_optmgmt_req req;
#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx optmgmt req\n",
					current->pid,
					(unsigned long)current->FD[fd]);
#endif
			error = verify_area(VERIFY_READ, ctl_buf, sizeof(req));
			if (error)
				return error;
			memcpy_fromfs(&req, ctl_buf, sizeof(req));

			return timod_optmgmt(fd, regs, req.MGMT_flags,
					req.OPT_offset > 0
						? ctl_buf+req.OPT_offset
						: NULL,
					req.OPT_length,
					1);
		}
	}
#if IBCS_TRACE
	if (ctl_buf && ctl_len > 0) {
		int i;
		for (i=0; i<ctl_len && i<32; i+=4)
			printk(KERN_ERR "ctl: 0x%08lx\n",
				get_fs_long(ctl_buf+i));
		if (i != ctl_len)
			printk(KERN_ERR "ctl: ...\n");
	}
	if (dat_buf && dat_len > 0) {
		int i;
		for (i=0; i<dat_len && i<32; i+=4)
			printk(KERN_ERR "dat: 0x%08lx\n",
				get_fs_long(dat_buf+i));
		if (i != dat_len)
			printk(KERN_ERR "dat: ...\n");
	}
#endif
	return -EINVAL;
}


int
timod_ioctl(struct pt_regs *regs,
	int fd, unsigned int func, void *arg, int len, int *len_p)
{
	struct inode *ino;
	int error;

	if (!current->FD[fd]
	|| !(ino = current->FD[fd]->f_inode)
	|| !ino->i_sock)
		return TBADF;

	error = verify_area(VERIFY_WRITE, len_p, sizeof(int));
	if (error)
		return (-error << 8) | TSYSERR;

	/* SCO/SVR3 starts at 100, ISC/SVR4 starts at 140. */
	switch (func >= 140 ? func-140 : func-100) {
		case 0: /* TI_GETINFO */
		{
			struct T_info_ack it;

#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx getinfo\n",
					current->pid,
					(unsigned long)current->FD[fd]);
#endif
			/* The pre-SVR4 T_info_ack structure didn't have
			 * the PROVIDER_flag on the end.
			 */
			error = verify_area(VERIFY_WRITE, arg,
				func == 140
				? sizeof(struct T_info_ack)
				: sizeof(struct T_info_ack)-sizeof(long));
			if (error)
				return (-error << 8) | TSYSERR;

			if (get_fs_long(&((struct T_info_req *)arg)->PRIM_type) != T_INFO_REQ)
				return (EINVAL << 8) | TSYSERR;

			it.PRIM_type = T_INFO_ACK;
			it.CURRENT_state = Priv(fd)->state;
			it.CDATA_size = -2;
			it.DDATA_size = -2;
			it.OPT_size = -1;
			it.TIDU_size = 16384;
			switch ((MINOR(ino->i_rdev)>>4) & 0x0f) {
				case AF_UNIX:
					it.ADDR_size = sizeof(struct sockaddr_un);
					break;
				case AF_INET:
					it.ADDR_size = sizeof(struct sockaddr_in);
					break;
				default:
					/* Uh... dunno... play safe(?) */
					it.ADDR_size = 1024;
					break;
			}
			switch (ino->u.socket_i.type) {
				case SOCK_STREAM:
					it.ETSDU_size = 1;
					it.TSDU_size = 0;
					it.SERV_type = 2;
					break;
				default:
					it.ETSDU_size = -2;
					it.TSDU_size = 16384;
					it.SERV_type = 3;
					break;
			}

			/* The pre-SVR4 T_info_ack structure didn't have
			 * the PROVIDER_flag on the end.
			 */
			if (func == 140) {
				it.PROVIDER_flag = 0;
				memcpy_tofs(arg, &it, sizeof(it));
				put_fs_long(sizeof(it), len_p);
				return 0;
			}
			memcpy_tofs(arg, &it, sizeof(it)-sizeof(long));
			put_fs_long(sizeof(it)-sizeof(long), len_p);
			return 0;
		}

		case 2: /* TI_BIND */
		{
			int i;
			long prim;

#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx bind\n",
					current->pid,
					(unsigned long)current->FD[fd]);
#endif
			error = do_putmsg(fd, regs, arg, len,
					NULL, -1, 0);
			if (error)
				return (-error << 8) | TSYSERR;

			/* Get the response. This should be either
			 * T_OK_ACK or T_ERROR_ACK.
			 */
			i = MSG_HIPRI;
			error = do_getmsg(fd, regs,
					arg, len, len_p,
					NULL, -1, NULL,
					&i);
			if (error)
				return (-error << 8) | TSYSERR;

			prim = get_fs_long((unsigned long *)arg);
			if (prim == T_ERROR_ACK)
				return (get_fs_long(((unsigned long *)arg)+3) << 8)
					| get_fs_long(((unsigned long *)arg)+2);
			if (prim != T_OK_ACK)
				return TBADSEQ;

			/* Get the response to the bind request. */
			i = MSG_HIPRI;
			error = do_getmsg(fd, regs,
					arg, len, len_p,
					NULL, -1, NULL,
					&i);
			if (error)
				return (-error << 8) | TSYSERR;

			return 0;
		}

		case 3: /* TI_UNBIND */
			if (Priv(fd)->state != TS_IDLE)
				return TOUTSTATE;
			Priv(fd)->state = TS_UNBND;
			return 0;

		case 1: { /* TI_OPTMGMT */
#if defined(EMU_XTI_OPTMGMT) || defined(EMU_TLI_OPTMGMT)
			int i;
			long prim;
#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx optmgmt\n",
					current->pid,
					(unsigned long)current->FD[fd]);
#endif
			error = do_putmsg(fd, regs, arg, len,
					NULL, -1, 0);
			if (error)
				return (-error << 8) | TSYSERR;

			/* Get the response to the optmgmt request. */
			i = MSG_HIPRI;
			error = do_getmsg(fd, regs,
					arg, len, len_p,
					NULL, -1, NULL,
					&i);
			if (error)
				return (-error << 8) | TSYSERR;

			prim = get_fs_long((unsigned long *)arg);
			if (prim == T_ERROR_ACK)
				return (get_fs_long(((unsigned long *)arg)+3) << 8)
					| get_fs_long(((unsigned long *)arg)+2);

			return 0;
#else /* no EMU_XTI_OPTMGMT or EMU_TLI_OPTMGMT */
			return TNOTSUPPORT;
#endif /* EMU_XTI_OPTMGMT or EMU_TLI_OPTMGMT */
		}

#ifdef EMU_SVR4
		case 4: /* TI_GETMYNAME */
		case 5: /* TI_SETPEERNAME */
		case 6: /* TI_GETMYNAME */
		case 7: /* TI_SETPEERNAME */
#endif /* EMU_SVR4 */
	}

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_STREAMS)
		printk(KERN_ERR "iBCS: STREAMS timod op %d not supported\n",
			func);
#endif
	return TNOTSUPPORT;
}


#ifdef EMU_SVR4
int
ibcs_ioctl_sockmod(int fd, unsigned int func, void *arg)
{
	int error;
	struct inode *ino;

	if (!current->FD[fd]
	|| !(ino = current->FD[fd]->f_inode)
	|| !ino->i_sock)
		return TBADF;

	switch (func) {
#ifdef __sparc__
            	case 110: { /* SI_GETUDATA -- Solaris */
			struct {
				int tidusize, addrsize, optsize, etsdusize;
				int servtype, so_state, so_options;
			        int tsdusize;
			    
			        /* Socket parameters */
			        int family, type, protocol;
			} *it = arg;

#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx new_getudata\n",
					current->pid,
					(unsigned long)current->FD[fd]);
#endif
			error = verify_area(VERIFY_WRITE, it, sizeof(*it));
			if (error)
				return (-error << 8) | TSYSERR;

			put_fs_long(16384, &it->tidusize);
			put_fs_long(sizeof(struct sockaddr), &it->addrsize);
			put_fs_long(-1, &it->optsize);
			put_fs_long(0, &it->so_state);
			put_fs_long(0, &it->so_options);
			put_fs_long(16384, &it->tsdusize);
			
			switch (ino->u.socket_i.type) {
				case SOCK_STREAM:
					put_fs_long(1, &it->etsdusize);
					put_fs_long(2, &it->servtype);
					break;
				default:
					put_fs_long(-2, &it->etsdusize);
					put_fs_long(3, &it->servtype);
					break;
			}
			put_fs_long (ino->u.socket_i.ops->family, &it->family);
			put_fs_long (ino->u.socket_i.type, &it->type);
			put_fs_long (ino->u.socket_i.ops->family, &it->protocol);
			return 0;
		}
		    
#endif
		case 101: { /* SI_GETUDATA */
			struct {
				int tidusize, addrsize, optsize, etsdusize;
				int servtype, so_state, so_options;
#ifdef __sparc__
			        int tsdusize;
#endif
			} *it = arg;

#ifdef IBCS_TRACE
			if (ibcs_trace & TRACE_STREAMS)
				printk(KERN_DEBUG "iBCS: [%d] %lx getudata\n",
					current->pid,
					(unsigned long)current->FD[fd]);
#endif
			error = verify_area(VERIFY_WRITE, it, sizeof(*it));
			if (error)
				return (-error << 8) | TSYSERR;

			put_fs_long(16384, &it->tidusize);
			put_fs_long(sizeof(struct sockaddr), &it->addrsize);
			put_fs_long(-1, &it->optsize);
			put_fs_long(0, &it->so_state);
			put_fs_long(0, &it->so_options);

#ifdef __sparc__
			put_fs_long (16384, &it->tsdusize);
#endif
			switch (ino->u.socket_i.type) {
				case SOCK_STREAM:
					put_fs_long(1, &it->etsdusize);
					put_fs_long(2, &it->servtype);
					break;
				default:
					put_fs_long(-2, &it->etsdusize);
					put_fs_long(3, &it->servtype);
					break;
			}
			return 0;
		}

		case 102: /* SI_SHUTDOWN */
		case 103: /* SI_LISTEN */
		case 104: /* SI_SETMYNAME */
		case 105: /* SI_SETPEERNAME */
		case 106: /* SI_GETINTRANSIT */
		case 107: /* SI_TCL_LINK */
		case 108: /* SI_TCL_UNLINK */
	}

#ifdef IBCS_TRACE
	if (ibcs_trace & TRACE_STREAMS)
		printk(KERN_ERR "iBCS: STREAMS sockmod op %d not supported\n",
			func);
#endif
	return TNOTSUPPORT;
}
#endif /* EMU_SVR4 */
#endif /* EMU_XTI */


#if defined(EMU_XTI) || defined(EMU_SPX)

int
timod_getmsg(int fd, struct inode *ino, int is_pmsg, struct pt_regs *regs)
{
	struct strbuf *ctlptr, *datptr;
	int *flags_p, flags, *band_p;
	int error;
	struct strbuf ctl, dat;

	ctlptr = get_syscall_parameter (regs, 1);
	datptr = get_syscall_parameter (regs, 2);
	if (!is_pmsg) {
		flags_p = (int *)get_syscall_parameter (regs, 3);
	} else {
		band_p = (int *)get_syscall_parameter (regs, 3);
		flags_p = (int *)get_syscall_parameter (regs, 4);
		error = verify_area(VERIFY_WRITE, band_p, sizeof(int));
		if (error)
			return error;
	}

	error = verify_area(VERIFY_WRITE, flags_p, sizeof(int));
	if (error)
		return error;

	if (ctlptr) {
		error = verify_area(VERIFY_WRITE, ctlptr, sizeof(ctl));
		if (error)
			return error;
		memcpy_fromfs(&ctl, ctlptr, sizeof(ctl));
		put_fs_long(-1, &ctlptr->len);
	} else {
		ctl.maxlen = -1;
	}

	if (datptr) {
		error = verify_area(VERIFY_WRITE, datptr, sizeof(dat));
		if (error)
			return error;
		memcpy_fromfs(&dat, datptr, sizeof(dat));
		put_fs_long(-1, &datptr->len);
	} else {
		dat.maxlen = -1;
	}

	error = verify_area(VERIFY_WRITE, flags_p, sizeof(int));
	if (error)
		return error;
	flags = (int)get_fs_long(flags_p);

#ifdef EMU_SPX
	if (MAJOR(ino->i_rdev) == 30 && MINOR(ino->i_rdev) == 1) {
#ifdef IBCS_TRACE
		if (ibcs_trace & TRACE_STREAMS)
			printk(KERN_DEBUG
				"iBCS: [%d] %d getmsg offers descriptor %d\n",
				current->pid, fd, fd);
#endif
		put_fs_long((unsigned long)fd, ctl.buf);
		put_fs_long(4, &ctlptr->len);
		return 0;
	}
#endif /* EMU_SPX */

#ifdef EMU_XTI
	if (flags != 0 && flags != MSG_HIPRI && flags != MSG_ANY
	&& flags != MSG_BAND) {
#ifdef IBCS_TRACE
		if (ibcs_trace & TRACE_STREAMS)
			printk(KERN_DEBUG
				"iBCS: [%d] %d getmsg flags value bad (%d)\n",
				current->pid, fd, flags);
#endif
		return -EINVAL;
	}

	error = do_getmsg(fd, regs,
			ctl.buf, ctl.maxlen, &ctlptr->len,
			dat.buf, dat.maxlen, &datptr->len,
			&flags);
	if (!error)
		put_fs_long(flags, flags_p);
	return error;
#else /* EMU_XTI */
	return -EINVAL;
#endif /* EMU_XTI */
}


int
timod_putmsg(int fd, struct inode *ino, int is_pmsg, struct pt_regs *regs)
{
	struct strbuf *ctlptr, *datptr;
	int flags, error, band;
	struct strbuf ctl, dat;

	ctlptr = (struct strbuf *)get_syscall_parameter (regs, 1);
	datptr = (struct strbuf *)get_syscall_parameter (regs, 2);
	if (!is_pmsg) {
		flags = (int)get_syscall_parameter (regs, 3);
	} else {
		band = (int)get_syscall_parameter (regs, 3);
		flags = (int)get_syscall_parameter (regs, 4);
	}

	if (ctlptr) {
		error = verify_area(VERIFY_READ, ctlptr, sizeof(ctl));
		if (error)
			return error;
		memcpy_fromfs(&ctl, ctlptr, sizeof(ctl));
		if (ctl.len < 0 && flags)
			return -EINVAL;
	} else {
		ctl.len = 0;
		ctl.buf = NULL;
	}

	if (datptr) {
		error = verify_area(VERIFY_READ, datptr, sizeof(dat));
		if (error)
			return error;
		memcpy_fromfs(&dat, datptr, sizeof(dat));
	} else {
		dat.len = 0;
		dat.buf = NULL;
	}

#ifdef EMU_SPX
	if (MAJOR(ino->i_rdev) == 30 && MINOR(ino->i_rdev) == 1) {
		unsigned int newfd;

		if (ctl.len != 4)
			return -EIO;

		newfd = get_fs_long(ctl.buf);
#ifdef IBCS_TRACE
		if (ibcs_trace & TRACE_STREAMS)
			printk(KERN_DEBUG
				"iBCS: [%d] %d putmsg dups descriptor %d\n",
				current->pid, fd, newfd);
#endif
		error = SYS(dup2)(newfd, fd);
		if (error < 0)
			return error;

		return 0;
	}
#endif /* EMU_SPX */

#ifdef EMU_XTI
	return do_putmsg(fd, regs, ctl.buf, ctl.len,
			dat.buf, dat.len, flags);
#else /* EMU_XTI */
	return -EINVAL;
#endif /* EMU_XTI */
}

#endif /* defined(EMU_XTI) || defined(EMU_SPX) */

/*
 *  linux/ibcs/ipc.c
 *
 *  (C) Copyright 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 *  Massive work over with a fine tooth comb, lots of rewriting. There
 *  were a *lot* of bugs in this - mismatched structs that weren't
 *  mapped, wrong pointers etc. I've tested this version with the
 *  demo programs from the Wyse V/386 IPC documentation which exercise
 *  all the functions. I don't have any major IPC using applications
 *  to test it with - as far as I know...
 *
 *  Original copyright etc. follows:
 *
 *  Copyright (C) 1993,1994  Joe Portman (baron@hebron.connected.com)
 *	 First stab at ibcs shm, sem and msg handlers
 *
 *  NOTE:
 *  Please contact the author above before blindly making changes
 *  to this file. You will break things.
 *
 *  04-15-1994 JLP III
 *  Still no msgsys, but IPC_STAT now works for shm calls
 *  Corrected argument order for sys_ipc calls, to accomodate Mike's
 *  changes, so that we can just call sys_ipc instead of the internal
 *  sys_* calls for ipc functions.
 *  Cleaned up translation of perm structures
 *  tstshm for Oracle now works.
 *
 *  04-23-1994 JLP III
 *  Added in msgsys calls, Tested and working
 *  Added translation for IPC_SET portions of all xxxctl functions.
 *  Added SHM_LOCK and SHM_UNLOCK to shmsys
 *
 *  04-28-1994 JLP III
 *  Special thanks to Brad Pepers for adding in the GETALL and SETALL
 *  case of semaphores. (pepersb@cuug.ab.ca)
 *
 * $Id: ipc.c,v 1.19 1996/08/19 14:05:27 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/ipc.c,v $
 */
#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/stddef.h>
#include <linux/unistd.h>
#include <linux/ptrace.h>

#include <asm/system.h>
#include <linux/fs.h>
#include <linux/sys.h>
#include <linux/ipc.h>
#include <linux/sem.h>
#include <linux/shm.h>
#include <linux/msg.h>
#include <linux/string.h>

#include <ibcs/ibcs.h>
#include <ibcs/trace.h>


struct ibcs_ipc_perm {
	unsigned short uid;		/* owner's user id */
	unsigned short gid;		/* owner's group id */
	unsigned short cuid;		/* creator's user id */
	unsigned short cgid;		/* creator's group id */
	unsigned short mode;		/* access modes */
	unsigned short seq;		/* slot usage sequence number */
	long key;			/* key */
};

struct ibcs_semid_ds {
	struct ibcs_ipc_perm sem_perm;
	struct sem *sem_base;
	unsigned short sem_nsems;
	char __pad[2];
	time_t sem_otime;
	time_t sem_ctime;
};

struct ibcs_shmid_ds {
	struct ibcs_ipc_perm shm_perm;	/* operation permission struct */
	int shm_segsz;			/* size of segment in bytes */
	struct region *__pad1;		/* ptr to region structure */
	char __pad2[4];			/* for swap compatibility */
	ushort shm_lpid;		/* pid of last shmop */
	ushort shm_cpid;		/* pid of creator */
	unsigned short shm_nattch;	/* used only for shminfo */
	unsigned short __pad3;
	time_t shm_atime;		/* last shmat time */
	time_t shm_dtime;		/* last shmdt time */
	time_t shm_ctime;		/* last change time */
};

struct ibcs_msqid_ds {
	struct ibcs_ipc_perm msg_perm;
	struct msg *msg_first;
	struct msg *msg_last;
	ushort msg_cbytes;
	ushort msg_qnum;
	ushort msg_qbytes;
	ushort msg_lspid;
	ushort msg_lrpid;
	time_t msg_stime;
	time_t msg_rtime;
	time_t msg_ctime;
};


static inline void
ip_to_lp(struct ibcs_ipc_perm *ip, struct ipc_perm *lp)
{
	lp->uid = ip->uid;
	lp->gid = ip->gid;
	lp->cuid = ip->cuid;
	lp->cgid = ip->cgid;
	lp->mode = ip->mode;
	lp->seq = ip->seq;
	lp->key = ip->key;
}

static inline void
lp_to_ip(struct ipc_perm *lp, struct ibcs_ipc_perm *ip)
{
	ip->uid = lp->uid;
	ip->gid = lp->gid;
	ip->cuid = lp->cuid;
	ip->cgid = lp->cgid;
	ip->mode = lp->mode;
	ip->seq = lp->seq;
	ip->key = lp->key;
}


#define U_SEMCTL    (0)
#define U_SEMGET    (1)
#define U_SEMOP     (2)
#define U_SHMLOCK   (3)
#define U_SHMUNLOCK (4)

#define U_GETNCNT	(3)
#define U_GETPID	(4)
#define U_GETVAL	(5)
#define U_GETALL	(6)
#define U_GETZCNT	(7)
#define U_SETVAL	(8)
#define U_SETALL	(9)

static inline int
ibcs_sem_trans (int arg)
{
	switch (arg) {
		case IPC_RMID:	return IPC_RMID;
		case IPC_SET:	return IPC_SET;
		case IPC_STAT:	return IPC_STAT;
		case U_GETNCNT:	return GETNCNT;
		case U_GETPID:	return GETPID;
		case U_GETVAL:	return GETVAL;
		case U_GETALL:	return GETALL;
		case U_GETZCNT:	return GETZCNT;
		case U_SETVAL:	return SETVAL;
		case U_SETALL:	return SETALL;
	}
	return -1;
}

static void
isem_to_lsem (struct ibcs_semid_ds *is, struct semid_ds *ls)
{
	ip_to_lp(&is->sem_perm, &ls->sem_perm);
	ls->sem_base = is->sem_base;
	ls->sem_nsems = is->sem_nsems;
	ls->sem_otime = is->sem_otime;
	ls->sem_ctime = is->sem_ctime;
}

static void
lsem_to_isem (struct semid_ds *ls, struct ibcs_semid_ds *is)
{
	lp_to_ip(&ls->sem_perm, &is->sem_perm);
	is->sem_base = ls->sem_base;
	is->sem_nsems = ls->sem_nsems;
	is->sem_otime = ls->sem_otime;
	is->sem_ctime = ls->sem_ctime;
}

int
ibcs_semsys (struct pt_regs *regs)
{
	int command = get_syscall_parameter (regs, 0);
	int arg1, arg2, arg3;
	union semun *arg4;
	struct ibcs_semid_ds is, *is_p;
	struct semid_ds ls;
	union semun lsemun;
	int old_fs, retval, cmd;

	arg1 = get_syscall_parameter (regs, 1);
	arg2 = get_syscall_parameter (regs, 2);
	arg3 = get_syscall_parameter (regs, 3);
	switch (command) {
		case U_SEMCTL:
			cmd = ibcs_sem_trans(arg3);
			/* XXX - The value for arg4 depends on how union
			 * passing is implement on this architecture and
			 * compiler. The following is *only* known to be
			 * right for Intel (the default else case).
			 */
#ifdef __sparc__
			arg4 = (union semun *)get_syscall_parameter (regs, 4);
#else
			arg4 = (union semun *)(((unsigned long *) regs->esp) + (5));
#endif
			is_p = (struct ibcs_semid_ds *)get_fs_long(arg4->buf);
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace)
			printk(KERN_DEBUG "iBCS: ibcs_semctl: args: %d %d %d %lx\n",
					 arg1, arg2, arg3, (unsigned long)arg4);
#endif
			switch (cmd) {
				case IPC_SET:
					is_p = (struct ibcs_semid_ds *)get_fs_long(&arg4->buf);
					retval = verify_area(VERIFY_WRITE, is_p, sizeof(is));
					if (retval)
						return retval;

					memcpy_fromfs(&is, (char *)is_p, sizeof(is));
					isem_to_lsem(&is, &ls);

					lsemun.buf = &ls;
					old_fs = get_fs();
					set_fs (get_ds());
					retval = SYS (ipc) (SEMCTL, arg1, arg2, cmd, &lsemun);
					set_fs(old_fs);

					lsem_to_isem(&ls, &is);
					memcpy_tofs((char *)is_p, &is, sizeof(is));

					return retval;

				case IPC_RMID:
				case SETVAL:
				case GETVAL:
				case GETPID:
				case GETNCNT:
				case GETZCNT:
					return SYS (ipc) (SEMCTL, arg1, arg2, cmd, arg4);

				case SETALL:
				case GETALL:
					return SYS (ipc) (SEMCTL, arg1, 0, cmd, arg4);

				case IPC_STAT:
					is_p = (struct ibcs_semid_ds *)get_fs_long(&arg4->buf);
					retval = verify_area(VERIFY_WRITE, (char *)is_p, sizeof(is));
					if (retval)
						return retval;

					lsemun.buf = &ls;
					old_fs = get_fs();
					set_fs(get_ds());
					retval = SYS (ipc) (SEMCTL, arg1, 0, cmd, &lsemun);
					set_fs(old_fs);
					if (retval < 0)
						return retval;

					lsem_to_isem(&ls, &is);
					memcpy_tofs((char *)is_p, &is, sizeof(is));
					return retval;

				default:
					printk(KERN_ERR "ibcs_semctl: unsupported command %d\n",
						arg3);
					return -EINVAL;
		  	}

		case U_SEMGET:
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace)
				printk(KERN_DEBUG "iBCS: ibcs_semget: args: %d %d %o \n",
					arg1, arg2, arg3);
#endif
			return SYS (ipc) (SEMGET, arg1, arg2, arg3, 0);

		case U_SEMOP:
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
				int x;
				struct sembuf tmp;
				struct sembuf *tp = (struct sembuf *) arg2;

				printk(KERN_DEBUG "iBCS: ibcs_semop: args: %d %d %o \n",
					arg1, arg2, arg3);
				for (x = 0; x < arg3; x++) {
					memcpy_fromfs (&tmp, tp, sizeof (tmp));
					printk(KERN_DEBUG "iBCS: ibcs_semop args: %d %d %o \n",
						tmp.sem_num, tmp.sem_op, tmp.sem_flg);
					tp++;
				}
			}
#endif
			return SYS (ipc) (SEMOP, arg1, arg3, 0, (struct sembuf *) arg2);
	}
	return -EINVAL;
}


#define U_SHMAT  (0)
#define U_SHMCTL (1)
#define U_SHMDT  (2)
#define U_SHMGET (3)

static void
ishm_to_lshm(struct ibcs_shmid_ds *is, struct shmid_ds *ls)
{
	ip_to_lp(&is->shm_perm, &ls->shm_perm);
	ls->shm_segsz = is->shm_segsz;
	ls->shm_lpid = is->shm_lpid;
	ls->shm_cpid = is->shm_cpid;
	ls->shm_nattch = is->shm_nattch;
	ls->shm_atime = is->shm_atime;
	ls->shm_dtime = is->shm_dtime;
	ls->shm_ctime = is->shm_ctime;
}

static void
lshm_to_ishm(struct shmid_ds *ls, struct ibcs_shmid_ds *is)
{
	lp_to_ip(&ls->shm_perm, &is->shm_perm);
	is->shm_segsz = ls->shm_segsz;
	is->shm_lpid = ls->shm_lpid;
	is->shm_cpid = ls->shm_cpid;
	is->shm_nattch = ls->shm_nattch;
	is->shm_atime = ls->shm_atime;
	is->shm_dtime = ls->shm_dtime;
	is->shm_ctime = ls->shm_ctime;
}


int
ibcs_shmsys (struct pt_regs *regs)
{
	int command = get_syscall_parameter (regs, 0);
	int arg1, arg2, arg3;
	struct ibcs_shmid_ds is;
	struct shmid_ds ls;
	int old_fs;
	long retval = 0;
	char *addr = 0;

	arg1 = arg2 = arg3 = 0;
	switch (command) {
		case U_SHMAT:
		case U_SHMCTL:
		case U_SHMGET:
			arg1 = get_syscall_parameter (regs, 1);
			arg2 = get_syscall_parameter (regs, 2);
			arg3 = get_syscall_parameter (regs, 3);
			break;
		case U_SHMDT:
			addr = (char *) get_syscall_parameter (regs, 1);
			break;
		default:
			printk(KERN_ERR "iBCS: bad SHM command %d\n", command);
			retval = -EINVAL;
			goto test_exit;
	}

	switch (command) {
		case U_SHMAT: {
#ifdef IPCCALL
			unsigned long raddr;
#endif
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace)
				printk(KERN_DEBUG "iBCS: ibcs_shmat: args: %d %x %o \n",
					arg1, arg2, arg3);
#endif
			/*
			 * raddr = 0 tells sys_shmat to limit to 2G
			 *	and we are IBCS, no raddr value to return
			 */
#ifdef IPCCALL
			old_fs = get_fs();
			set_fs(get_ds());
			retval = SYS (ipc) (IPCCALL(1,SHMAT), arg1, arg3, &raddr, (char *) arg2);
			set_fs(old_fs);
			if (retval >= 0)
				retval = raddr;
#else
			retval = SYS (ipc) (SHMAT, arg1, arg3, 0, (char *) arg2);
#endif

#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace)
				printk(KERN_DEBUG "iBCS: ibcs_shmat: return val is %lx\n",
					retval);
#endif
			goto test_exit;
		}

		case U_SHMGET:
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace)
				printk(KERN_DEBUG "iBCS: ibcs_shmget: args: %d %x %o \n",
					arg1, arg2, arg3);
#endif
			retval = SYS (ipc) (SHMGET, arg1, arg2, arg3, 0);
			goto test_exit;

		case U_SHMDT:
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace)
				printk(KERN_DEBUG "iBCS: ibcs_shmdt: arg: %lx\n",
					(unsigned long) addr);
#endif
			retval = SYS (ipc) (SHMDT, 0, 0, 0, addr);
			goto test_exit;

		case U_SHMCTL:
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace)
				printk(KERN_DEBUG "iBCS: ibcs_shmctl: args: %d %x %o %d %x\n",
					arg1, arg2, arg3, arg3, arg3);
#endif
			switch (arg2) {
				case U_SHMLOCK:
					retval = SYS (ipc) (SHMCTL, arg1, SHM_LOCK, 0, arg3);
					goto test_exit;

				case U_SHMUNLOCK:
					retval = SYS (ipc) (SHMCTL, arg1, SHM_UNLOCK, 0, arg3);
					goto test_exit;

				case IPC_SET:
					memcpy_fromfs(&is, (char *)arg3, sizeof(is));
					ishm_to_lshm(&is, &ls);

					old_fs = get_fs();
					set_fs (get_ds());
					retval = SYS (ipc) (SHMCTL, arg1, arg2, 0, &ls);
					set_fs(old_fs);

					lshm_to_ishm(&ls, &is);
					memcpy_tofs((char *)arg3, &is, sizeof(is));

					goto test_exit;

				case IPC_RMID:
					retval = (SYS (ipc) (SHMCTL, arg1, arg2, arg3));
					goto test_exit;

				case IPC_STAT:
					old_fs = get_fs();
					set_fs (get_ds());
					retval = SYS (ipc) (SHMCTL, arg1, arg2, 0, &ls);
					set_fs(old_fs);
					if (retval < 0)
						goto test_exit;

					lshm_to_ishm(&ls, &is);
					memcpy_tofs((char *)arg3, &is, sizeof(is));
					goto test_exit;

				default:
					printk(KERN_ERR "iBCS: ibcs_shmctl: unsupported command %d\n", arg2);
			}
			retval = -EINVAL;
			goto test_exit;

		default:
#ifdef IBCS_TRACE
			printk(KERN_DEBUG "iBCS: ibcs_shm: command: %x\n", command);
#endif
			retval = -EINVAL;
			goto test_exit;
	}

test_exit:;
	if ((retval < 0) && (retval > -255)) {
	        set_error (regs, iABI_errors (-retval));
#ifdef IBCS_TRACE
		if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace)
			printk(KERN_DEBUG "iBCS: Error %ld\n", get_result (regs));
#endif
	} else {
	    	clear_error (regs);
		set_result (regs, retval);
	}

	return 0;
}


#define U_MSGGET  (0)
#define U_MSGCTL  (1)
#define U_MSGRCV  (2)
#define U_MSGSND  (3)

static void
imsq_to_lmsq(struct ibcs_msqid_ds *im, struct msqid_ds *lm)
{
	ip_to_lp(&im->msg_perm, &lm->msg_perm);
	lm->msg_first = im->msg_first;
	lm->msg_last = im->msg_last;
	lm->msg_cbytes = im->msg_cbytes;
	lm->msg_qnum = im->msg_qnum;
	lm->msg_qbytes = im->msg_qbytes;
	lm->msg_lspid = im->msg_lspid;
	lm->msg_lrpid = im->msg_lrpid;
	lm->msg_stime = im->msg_stime;
	lm->msg_rtime = im->msg_rtime;
	lm->msg_ctime = im->msg_ctime;
}

static void
lmsq_to_imsq(struct msqid_ds *lm, struct ibcs_msqid_ds *im)
{
	lp_to_ip(&lm->msg_perm, &im->msg_perm);
	im->msg_first = lm->msg_first;
	im->msg_last = lm->msg_last;
	im->msg_cbytes = lm->msg_cbytes;
	im->msg_qnum = lm->msg_qnum;
	im->msg_qbytes = lm->msg_qbytes;
	im->msg_lspid = lm->msg_lspid;
	im->msg_lrpid = lm->msg_lrpid;
	im->msg_stime = lm->msg_stime;
	im->msg_rtime = lm->msg_rtime;
	im->msg_ctime = lm->msg_ctime;
}

int
ibcs_msgsys (struct pt_regs *regs)
{
	int command = get_syscall_parameter (regs, 0);
	int arg1, arg2, arg4, arg5;
	struct ibcs_msqid_ds im;
	struct msqid_ds lm;
	int old_fs;
	char *arg3;
	int retval;

	arg1 = get_syscall_parameter (regs, 1);
	arg2 = get_syscall_parameter (regs, 2);
	arg3 = (char *) get_syscall_parameter (regs, 3);

	switch (command) {
		/* hard one first */
		case U_MSGCTL:
			switch ((arg2)) {
				case IPC_SET:
					retval = verify_area(VERIFY_WRITE, arg3, sizeof(im));
					if (retval)
						return retval;

					memcpy_fromfs(&im, (char *) arg3, sizeof(im));
					imsq_to_lmsq(&im, &lm);

					old_fs = get_fs();
					set_fs (get_ds());
					retval = SYS (ipc) (MSGCTL, arg1, arg2, 0, &lm);
					set_fs (old_fs);

					lmsq_to_imsq(&lm, &im);
					memcpy_tofs((char *)arg3, &im, sizeof(im));

					return retval;

			case IPC_RMID:
				return SYS (ipc) (MSGCTL, arg1, arg2, 0, arg3);

			case IPC_STAT:
				retval = verify_area(VERIFY_WRITE, arg3, sizeof(im));
				if (retval)
					return retval;

				old_fs = get_fs();
				set_fs (get_ds());
				retval = SYS (ipc) (MSGCTL, arg1, arg2, 0, &lm);
				set_fs (old_fs);

				if (retval < 0)
					return retval;

				lmsq_to_imsq(&lm, &im);
				memcpy_tofs((char *)arg3, &im, sizeof(im));
				return retval;

			default:
				printk(KERN_ERR "ibcs_msgctl: unsupported command %d\n", arg2);
		}

		case U_MSGGET:
			return SYS (ipc) (MSGGET, arg1, arg2, 0, 0);

		case U_MSGSND:
			arg4 = get_syscall_parameter (regs, 4);
			retval = SYS (ipc) (MSGSND, arg1, arg3, arg4, (char *) arg2);
			return ((retval > 0) ? 0 : retval);

		case U_MSGRCV: {
#ifdef IPCCALL
			arg4 = get_syscall_parameter (regs, 4);
			arg5 = get_syscall_parameter (regs, 5);
			return SYS(ipc)(IPCCALL(1,MSGRCV), arg1, arg3, arg5, arg2, arg4);
#else
#ifdef __sparc__
		        printk(KERN_ERR
				"Sparc/IBCS: Kludgy U_MSGRCV not implemented\n");
			return -EINVAL;
#else /* __sparc__ */
			struct ipc_kludge *scratch;
			long old_esp = regs->esp;

			scratch = (struct ipc_kludge *)((regs->esp-PAGE_SIZE-sizeof(struct ipc_kludge)) & 0xfffffffc);
			regs->esp = (long)scratch;
			arg4 = get_fs_long(((unsigned long *) regs->esp) + (5));
			arg5 = get_fs_long(((unsigned long *) regs->esp) + (6));
			put_fs_long((long)arg2, &scratch->msgp);
			put_fs_long((long)arg4, &scratch->msgtyp);
			retval = SYS (ipc) (MSGRCV, arg1, arg3, arg5, scratch);
			regs->esp = old_esp;
			return retval;
#endif /* sparc */
#endif /* IPCCALL */
		}

		default:
			printk(KERN_ERR "ibcs_msgctl: unsupported command %d\n", command);
	}
	return -EINVAL;
}

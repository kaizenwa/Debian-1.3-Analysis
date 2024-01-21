/*
 *  Function prototypes used by the iBCS2 emulator
 *
 * $Id: ibcs.h,v 1.46 1996/10/02 15:30:52 mike Exp $
 * $Source: /usr/CVS/ibcs/include/ibcs/ibcs.h,v $
 */
#include <linux/ptrace.h>	/* for pt_regs */
#include <linux/sched.h>
#include <linux/signal.h>
#include <linux/unistd.h>

#include <ibcs/stream.h>

#ifdef __sparc__
#  include <ibcs/arch-sparc.h>
#else
#  include <ibcs/arch-i386.h>
#endif


/* Magic... 1.1.10 changes the format of a task_struct. This is where
 * we figure out what we are doing.
 */
#ifdef INIT_FILES
#  define FD	files->fd
#  define FDI(x)	files->x
#else
#  define FD	filp
#  define FDI(x)	x
#endif
#ifdef INIT_MM
#  define MM(x)	mm->x
#else
#  define MM(x)	x
#endif
#ifdef INIT_SIGNALS
#  define SIGACTION	sig->action
#else
#  define SIGACTION	sigaction
#endif


typedef int (*sysfun_p)();


extern sysfun_p sys_call_table[];

#define SYS(name)	(sys_call_table[__NR_##name])


/*
 * the function prefix sys_... are used by linux in native mode.
 * ibcs_... are emulation interfaces for routine that differ from iBCS2 
 * and linux.  The xnx_... are xenix routines.  Note, just because
 * xnx routines are listed doesn't mean we support native xenix binaries.
 * (yet :-)
 */
typedef struct IBCS_function {
	void *	kfunc;	/* function to call (sys_..., ibcs_... or xnx_...)
			 * or pointer to a sub class.
			 */
	short	nargs;	/* number of args to kfunc or Ukn, Spl or Fast */
#ifdef IBCS_TRACE
	short	trace;	/* trace function we can turn tracing on or off */
	char *	name;	/* name of function (for tracing) */
	char *	args;	/* how to print the arg list (see plist) */
#endif
} IBCS_func;

struct ibcs_statfs {
	short f_type;
	long f_bsize;
	long f_frsize;
	long f_blocks;
	long f_bfree;
	long f_files;
	long f_ffree;
	char f_fname[6];
	char f_fpack[6];
};


#ifdef __sparc__

typedef struct {
	long tv_sec;
	long tv_nsec;
} timestruct_t;

struct ibcs_stat {
	unsigned long st_dev;
	long          st_pad1[3];     /* network id */
	unsigned long st_ino;
	unsigned long st_mode;
        unsigned long st_nlink;
        unsigned long st_uid;
        unsigned long st_gid;
        unsigned long st_rdev;
        long          st_pad2[2];
        long          st_size;
        long          st_pad3;        /* st_size, off_t expansion */
        timestruct_t  st_atime;
        timestruct_t  st_mtime;
        timestruct_t  st_ctime;
        long          st_blksize;
        long          st_blocks;
        char          st_fstype[16];
        long          st_pad4[8];     /* expansion area */
};
#else
struct ibcs_stat {
        unsigned short st_dev;
        unsigned short st_ino;
        unsigned short st_mode;
        unsigned short st_nlink;
        unsigned short st_uid;
        unsigned short st_gid;
        unsigned short st_rdev;
        unsigned long  st_size;
        unsigned long  st_atime;
        unsigned long  st_mtime;
        unsigned long  st_ctime;
};
#endif


struct ibcs_iovec {
	unsigned long addr;
	int len;
};

/* coff.c */
extern int ibcs_brk(unsigned long newbrk);
extern int ibcs_lseek(int fd, unsigned long offset, int whence);
extern int ibcs_fork(struct pt_regs * regs);
extern int ibcs_pipe(struct pt_regs * regs);
extern int ibcs_getpid(struct pt_regs * regs);
extern int ibcs_getuid(struct pt_regs * regs);
extern int ibcs_getgid(struct pt_regs * regs);
extern int ibcs_wait(struct pt_regs * regs);
extern int ibcs_execv(struct pt_regs * regs);
extern int ibcs_exec(struct pt_regs * regs);
extern int ibcs_read(int fd, char *buf, int nbytes);
extern int ibcs_procids(struct pt_regs * regs);
extern int ibcs_select(int n, void *rfds, void *wfds, void *efds,
			struct timeval *t);
extern int ibcs_time(void);
extern int ibcs_writev(int fd, struct ibcs_iovec *it, int n);

/* emulate.c */
#ifdef INIT_MM
extern void iABI_emulate_real(struct pt_regs * regs);
#else
extern void iABI_emulate(struct pt_regs *regs);
#endif
#if !defined(CONFIG_BINFMT_IBCS) && !defined(INIT_MM)
extern void ibcs_exit(int n);
#endif
extern int iABI_errors(int errno);
extern int ibcs_syscall(struct pt_regs *regs);

/* fcntl.c */
extern int ibcs_fcntl(struct pt_regs *regs);

/* ioctl.c */
extern int ibcs_ioctl(struct pt_regs *regs);

/* ipc.c */
extern int ibcs_semsys (struct pt_regs *regs);
extern int ibcs_shmsys (struct pt_regs *regs);
extern int ibcs_msgsys (struct pt_regs *regs);

/* mmap.c */
extern int ibcs_mmap(unsigned int vaddr, unsigned int vsize, int prot,
 		     int flags, int fd, unsigned int file_offset);

/* open.c */
extern int ibcs_statfs(const char * path, struct ibcs_statfs * buf, int len, int fstype);
extern int ibcs_fstatfs(unsigned int fd, struct ibcs_statfs * buf, int len, int fstype);
extern int ibcs_mkdir(const char *fname, int mode);
extern int ibcs_mknod(const char *fname, int mode, int dev);
extern int ibcs_open(const char * fname, int flag, int mode);
extern int ibcs_getdents(int fd, char *buf, int nybtes);

/* poll.c */
struct poll{
	int fd;
	short events;
	short revents;
};
extern int ibcs_poll(struct poll * ufds, size_t nfds, int timeout);

/* ptrace.c */
extern int ibcs_ptrace(int req, int pid, unsigned long addr, unsigned long data);

/* hrtsys.c */
extern int ibcs_hrtsys(struct pt_regs * regs);

/* secureware.c */
extern int sw_security(int cmd, void *p1, void *p2, void *p3, void *p4, void *p5);

/* signal.c */
/* For mapping signal numbers */
#ifndef INIT_MM
extern unsigned long *signal_map_to_linux[];
extern unsigned long *signal_map_from_linux[];
#endif
void ibcs_sig_handler (struct pt_regs * regs, int sig,
			__sighandler_t handler, int oneshot);
extern int ibcs_signal(struct pt_regs * regs);
extern int ibcs_sigset(struct pt_regs * regs);
void ibcs_sighold (struct pt_regs * regs);
void ibcs_sigrelse (struct pt_regs * regs);
void ibcs_sigignore (struct pt_regs * regs);
void ibcs_sigpause (struct pt_regs * regs);
extern int ibcs_sigfunc(struct pt_regs * regs);
extern int ibcs_kill(int pid, int sig);

/* socket.c */
extern int ibcs_getsockopt(unsigned long *sp);
extern int ibcs_setsockopt(unsigned long *sp);

/* stat.c */
extern int ibcs_stat(char * filename, struct ibcs_stat * statbuf);
extern int ibcs_lstat(char * filename, struct ibcs_stat * statbuf);
extern int ibcs_fstat(unsigned int fd, struct ibcs_stat * statbuf);

/* stream.c */
extern int ibcs_getmsg(struct pt_regs *regs);
extern int ibcs_putmsg(struct pt_regs *regs);
extern int ibcs_getpmsg(struct pt_regs *regs);
extern int ibcs_putpmsg(struct pt_regs *regs);

/* svr4.c */
struct siginfo {
	int si_signo;
	int si_code;
	int si_errno;
	union {
		struct {	/* kill(), SIGCLD */
			long _pid;
			union {
				struct {
					long _uid;
				} _kill;
				struct {
					long _utime;
					int _status;
					long _stime;
				} _cld;
			} _pdata;
		} _proc;
		struct {	/* SIGSEGV, SIGBUS, SIGILL, SIGFPE */
			char *_addr;
		} _fault;
		struct {	/* SIGPOLL, SIGXFSZ */
			int _fd;
			long _band;
		} _file;
	} _data;
};
#define CLD_EXITED	1
#define CLD_KILLED	2
#define CLD_DUMPED	3
#define CLD_TRAPPED	4
#define CLD_STOPPED	5
#define CLD_CONTINUED	6
extern int svr4_getgroups(int n, unsigned long *buf);
extern int svr4_setgroups(int n, unsigned long *buf);
extern int svr4_waitid(int idtype, int id, struct siginfo *infop, int options);
extern int svr4_access(char *path, int mode);

/* sysinfo.c */
extern int ibcs_sysinfo(int, char *, long);

/* sysconf.c */
extern int ibcs_sysconf(int name);

/* Kernels 1.1.10 and later have sysfs as a system call. */
#ifndef __NR_sysfs
/* sysfs.c */
extern int ibcs_sysfs(struct pt_regs * regs);
#endif

/* sysi86.c */
extern int ibcs_sysi86(struct pt_regs * regs);

/* syslocal.c */
extern int ibcs_syslocal(struct pt_regs * regs);

/* ulimit.c */
extern int ibcs_ulimit(int cmd, int val);

/* utsname.c */
extern int abi_utsname(unsigned long addr);
extern int sco_utsname(unsigned long addr);
extern int v7_utsname(unsigned long addr);

/* wysev386.c */
extern int wv386_gethostname(char *name, int len);
extern int wv386_getdomainname(char *name, int len);
extern int wv386_wait3(int *loc);
extern int wv386_socket(struct pt_regs *regs);
extern int wv386_connect(struct pt_regs *regs);
extern int wv386_accept(struct pt_regs *regs);
extern int wv386_send(struct pt_regs *regs);
extern int wv386_recv(struct pt_regs *regs);
extern int wv386_bind(struct pt_regs *regs);
extern int wv386_setsockopt(struct pt_regs *regs);
extern int wv386_listen(struct pt_regs *regs);
extern int wv386_getsockopt(struct pt_regs *regs);
extern int wv386_recvfrom(struct pt_regs *regs);
extern int wv386_sendto(struct pt_regs *regs);
extern int wv386_shutdown(struct pt_regs *regs);
extern int wv386_socketpair(struct pt_regs *regs);
extern int wv386_getpeername(struct pt_regs *regs);
extern int wv386_getsockname(struct pt_regs *regs);

/* From wysev386i.c */
extern int wv386_ioctl(int fd, unsigned int ioctl_num, void *arg);

/* From socksys.c */
extern void init_socksys(void);
extern void cleanup_socksys(void);

/* From sysisc.c */
extern int isc_setostype(int);

/* From vtkd.c */
extern int ibcs_ioctl_vtkd(int, int, void *);

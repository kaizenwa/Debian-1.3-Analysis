#include <errno.h>
#include <signal.h>
#include <stdio.h>

#include "ldt.h"
#include "syscall.h"


#ifdef DEBUG
# include "debug.h"
# define NAME(X)	X,
#else
# define NAME(X)
#endif


#define SC_SAS		1


typedef int (*syscall_func_t)(struct sigcontext_struct *);

typedef struct syscall {
#ifdef DEBUG
	char *name;
#endif
	syscall_func_t func;
} syscall_t;


static syscall_t unix_syscall[] = {			   /* T == tried */
	{ NAME("null")		(syscall_func_t)0	}, /*    0   */
	{ NAME("exit")		emu_i_s			}, /*    1 T */
	{ NAME("fork")		emu_i_v			}, /*    2   */
	{ NAME("read")		emu_i_sas		}, /*    3 T */
	{ NAME("write")		emu_i_sas		}, /*    4 T */
	{ NAME("open")		emu_i_ass		}, /*    5 T */
	{ NAME("close")		emu_i_s			}, /*    6 T */
	{ NAME("wait")		emu_i_a			}, /*    7   */
	{ NAME("creat")		emu_i_ass		}, /*    8   */
	{ NAME("link")		emu_i_aa		}, /*    9   */
	{ NAME("unlink")	emu_i_a			}, /*   10 T */
	{ NAME("exec")		emu_exec		}, /*   11   */
	{ NAME("chdir")		emu_i_a			}, /*   12   */
	{ NAME("time")		emu_i_v			}, /*   13 T */
	{ NAME("mknod")		emu_i_ass		}, /*   14   */
	{ NAME("chmod")		emu_i_ass		}, /*   15   */
	{ NAME("chown")		emu_i_ass		}, /*   16   */
	{ NAME("brk/break")	(syscall_func_t)0	}, /*   17   */
	{ NAME("stat")		emu_i_aa		}, /*   18   */
	{ NAME("seek/lseek")	emu_i_sls		}, /*   19 T */
	{ NAME("getpid")	(syscall_func_t)0	}, /*   20   */
	{ NAME("mount")		(syscall_func_t)0	}, /*   21   */
	{ NAME("umount")	(syscall_func_t)0	}, /*   22   */
	{ NAME("setuid")	emu_i_s			}, /*   23   */
	{ NAME("getuid")	(syscall_func_t)0	}, /*   24   */
	{ NAME("stime")		(syscall_func_t)0	}, /*   25   */
	{ NAME("ptrace")	(syscall_func_t)0	}, /*   26   */
	{ NAME("alarm")		(syscall_func_t)0	}, /*   27   */
	{ NAME("fstat")		emu_i_sas		}, /*   28   */
	{ NAME("pause")		emu_i_v			}, /*   29   */
	{ NAME("utime")		emu_i_aa		}, /*   30   */
	{ NAME("stty")		(syscall_func_t)0	}, /*   31   */
	{ NAME("gtty")		(syscall_func_t)0	}, /*   32   */
	{ NAME("access")	emu_i_ass		}, /*   33   */
	{ NAME("nice")		emu_i_s			}, /*   34   */
	{ NAME("statfs")	(syscall_func_t)0	}, /*   35   */
	{ NAME("sync")		emu_i_v			}, /*   36   */
	{ NAME("kill")		emu_i_ssa		}, /*   37   */
	{ NAME("fstatfs")	(syscall_func_t)0	}, /*   38   */
	{ NAME("procids")	(syscall_func_t)0	}, /*   39   */
	{ NAME("cxenix")	(syscall_func_t)0	}, /*   40   */
	{ NAME("dup")		emu_i_s			}, /*   41   */
	{ NAME("pipe")		(syscall_func_t)0	}, /*   42   */
	{ NAME("times")		(syscall_func_t)0	}, /*   43   */
	{ NAME("prof")		(syscall_func_t)0	}, /*   44   */
	{ NAME("lock/plock")	(syscall_func_t)0	}, /*   45   */
	{ NAME("setgid")	(syscall_func_t)0	}, /*   46   */
	{ NAME("getgid")	(syscall_func_t)0	}, /*   47   */
	{ NAME("signal")	(syscall_func_t)0	}, /*   48   */
	{ NAME("msgsys")	(syscall_func_t)0	}, /*   49   */
	{ NAME("sysi86/sys3b")	(syscall_func_t)0	}, /*   50   */
	{ NAME("acct/sysacct")	(syscall_func_t)0	}, /*   51   */
	{ NAME("shmsys")	(syscall_func_t)0	}, /*   52   */
	{ NAME("semsys")	(syscall_func_t)0	}, /*   53   */
	{ NAME("ioctl")		emu_i_ssa		}, /*   54 T */
	{ NAME("uadmin")	(syscall_func_t)0	}, /*   55   */
	{ NAME("?")		(syscall_func_t)0	}, /*   56   */
	{ NAME("utsys")		(syscall_func_t)0	}, /*   57   */
	{ NAME("fsync")		(syscall_func_t)0	}, /*   58   */
	{ NAME("execv")		emu_exec		}, /*   59   */
	{ NAME("umask")		(syscall_func_t)0	}, /*   60   */
	{ NAME("chroot")	(syscall_func_t)0	}, /*   61   */
	{ NAME("fcntl")		(syscall_func_t)0	}, /*   62   */
	{ NAME("ulimit")	(syscall_func_t)0	}, /*   63   */
	{ NAME("?")		(syscall_func_t)0	}, /*   64   */
	{ NAME("?")		(syscall_func_t)0	}, /*   65   */
	{ NAME("?")		(syscall_func_t)0	}, /*   66   */
	{ NAME("?")		(syscall_func_t)0	}, /*   67   */
	{ NAME("?")		(syscall_func_t)0	}, /*   68   */
	{ NAME("?")		(syscall_func_t)0	}, /*   69   */
	{ NAME("advfs")		(syscall_func_t)0	}, /*   70   */
	{ NAME("unadvfs")	(syscall_func_t)0	}, /*   71   */
	{ NAME("rmount")	(syscall_func_t)0	}, /*   72   */
	{ NAME("rumount")	(syscall_func_t)0	}, /*   73   */
	{ NAME("rfstart")	(syscall_func_t)0	}, /*   74   */
	{ NAME("?")		(syscall_func_t)0	}, /*   75   */
	{ NAME("rdebug")	(syscall_func_t)0	}, /*   76   */
	{ NAME("rfstop")	(syscall_func_t)0	}, /*   77   */
	{ NAME("rfsys")		(syscall_func_t)0	}, /*   78   */
	{ NAME("rmdir")		(syscall_func_t)0	}, /*   79   */
	{ NAME("mkdir")		(syscall_func_t)0	}, /*   80   */
	{ NAME("getdents")	(syscall_func_t)0	}, /*   81   */
	{ NAME("libattach")	(syscall_func_t)0	}, /*   82   */
	{ NAME("libdetach")	(syscall_func_t)0	}, /*   83   */
	{ NAME("sysfs")		(syscall_func_t)0	}, /*   84   */
	{ NAME("getmsg")	(syscall_func_t)0	}, /*   85   */
	{ NAME("putmsg")	(syscall_func_t)0	}, /*   86   */
	{ NAME("poll")		(syscall_func_t)0	}, /*   87   */
	{ NAME("?")		(syscall_func_t)0	}, /*   88   */
	{ NAME("security")	(syscall_func_t)0	}, /*   89   */
	{ NAME("symlink")	(syscall_func_t)0	}, /*   90   */
	{ NAME("lstat")		(syscall_func_t)0	}, /*   91   */
	{ NAME("readlink")	(syscall_func_t)0	}, /*   92   */
	{ NAME("?")		(syscall_func_t)0	}, /*   93   */
	{ NAME("?")		(syscall_func_t)0	}, /*   94   */
	{ NAME("?")		(syscall_func_t)0	}, /*   95   */
	{ NAME("sigsuspend")	(syscall_func_t)0	}, /*   96   */
	{ NAME("sigaltstack")	(syscall_func_t)0	}, /*   97   */
	{ NAME("sigaction")	(syscall_func_t)0	}, /*   98   */
	{ NAME("sigpending")	(syscall_func_t)0	}, /*   99   */
	{ NAME("context")	(syscall_func_t)0	}, /*  100   */
	{ NAME("evsys")		(syscall_func_t)0	}, /*  101   */
	{ NAME("evtrapret")	(syscall_func_t)0	}, /*  102   */
	{ NAME("statvfs")	(syscall_func_t)0	}, /*  103   */
	{ NAME("fstatvfs")	(syscall_func_t)0	}, /*  104   */
	{ NAME("sysisc")	(syscall_func_t)0	}, /*  105   */
	{ NAME("nfssys")	(syscall_func_t)0	}, /*  106   */
	{ NAME("waitsys")	(syscall_func_t)0	}, /*  107   */
	{ NAME("sigsendsys")	(syscall_func_t)0	}, /*  108   */
	{ NAME("hrtsys")	(syscall_func_t)0	}, /*  109   */
	{ NAME("acancel")	(syscall_func_t)0	}, /*  110   */
	{ NAME("async")		(syscall_func_t)0	}, /*  111   */
	{ NAME("priocntlsys")	(syscall_func_t)0	}, /*  112   */
	{ NAME("pathconf")	(syscall_func_t)0	}, /*  113   */
	{ NAME("mincore")	(syscall_func_t)0	}, /*  114   */
	{ NAME("mmap")		(syscall_func_t)0	}, /*  115   */
	{ NAME("mprotect")	(syscall_func_t)0	}, /*  116   */
	{ NAME("munmap")	(syscall_func_t)0	}, /*  117   */
	{ NAME("fpathconf")	(syscall_func_t)0	}, /*  118   */
	{ NAME("vfork")		(syscall_func_t)0	}, /*  119   */
	{ NAME("fchdir")	(syscall_func_t)0	}, /*  120   */
	{ NAME("readv")		(syscall_func_t)0	}, /*  121   */
	{ NAME("writev")	(syscall_func_t)0	}, /*  122   */
	{ NAME("xstat")		(syscall_func_t)0	}, /*  123   */
	{ NAME("lxstat")	(syscall_func_t)0	}, /*  124   */
	{ NAME("fxstat")	(syscall_func_t)0	}, /*  125   */
	{ NAME("xmknod")	(syscall_func_t)0	}, /*  126   */
	{ NAME("clocal")	(syscall_func_t)0	}, /*  127   */
	{ NAME("setrlimit")	(syscall_func_t)0	}, /*  128   */
	{ NAME("getrlimit")	(syscall_func_t)0	}, /*  129   */
	{ NAME("lchown")	(syscall_func_t)0	}, /*  130   */
	{ NAME("memcntl")	(syscall_func_t)0	}, /*  131   */
	{ NAME("getpmsg")	(syscall_func_t)0	}, /*  132   */
	{ NAME("putpmsg")	(syscall_func_t)0	}, /*  133   */
	{ NAME("rename")	(syscall_func_t)0	}, /*  134   */
	{ NAME("uname")		(syscall_func_t)0	}, /*  135   */
	{ NAME("setegid")	(syscall_func_t)0	}, /*  136   */
	{ NAME("sysconfig")	(syscall_func_t)0	}, /*  137   */
	{ NAME("adjtime")	(syscall_func_t)0	}, /*  138   */
	{ NAME("systeminfo")	(syscall_func_t)0	}, /*  139   */
	{ NAME("?")		(syscall_func_t)0	}, /*  140   */
	{ NAME("seteuid")	(syscall_func_t)0	}, /*  141   */
	{ NAME("?")		(syscall_func_t)0	}, /*  142   */
	{ NAME("?")		(syscall_func_t)0	}  /*  143   */
};

static syscall_t xenix_syscall[] = {
	{ NAME("null")		(syscall_func_t)0	}, /*  0   */
	{ NAME("locking")	(syscall_func_t)0	}, /*  1   */
	{ NAME("creatsem")	(syscall_func_t)0	}, /*  2   */
	{ NAME("opensem")	(syscall_func_t)0	}, /*  3   */
	{ NAME("sigsem")	(syscall_func_t)0	}, /*  4   */
	{ NAME("waitsem")	(syscall_func_t)0	}, /*  5   */
	{ NAME("nbwaitsem")	(syscall_func_t)0	}, /*  6   */
	{ NAME("rdchk")		emu_i_s			}, /*  7   */
	{ NAME("stkgro")	emu_stkgro		}, /*  8 T */
	{ NAME("?")		(syscall_func_t)0	}, /*  9   */
	{ NAME("chsize")	emu_i_sls		}, /* 10   */
	{ NAME("ftime")		emu_i_a			}, /* 11 T */
	{ NAME("nap")		emu_i_s			}, /* 12   */
	{ NAME("sdget")		(syscall_func_t)0	}, /* 13   */
	{ NAME("sdfree")	(syscall_func_t)0	}, /* 14   */
	{ NAME("sdenter")	(syscall_func_t)0	}, /* 15   */
	{ NAME("sdleave")	(syscall_func_t)0	}, /* 16   */
	{ NAME("sdgetv")	(syscall_func_t)0	}, /* 17   */
	{ NAME("sdwaitv")	(syscall_func_t)0	}, /* 18   */
	{ NAME("brkctl")	emu_brkctl		}, /* 19 T */
	{ NAME("?")		(syscall_func_t)0	}, /* 20   */
	{ NAME("?")		(syscall_func_t)0	}, /* 21   */
	{ NAME("?")		(syscall_func_t)0	}, /* 22   */
	{ NAME("?")		(syscall_func_t)0	}, /* 23   */
	{ NAME("proctl")	(syscall_func_t)0	}, /* 32   */
	{ NAME("execseg")	(syscall_func_t)0	}, /* 33   */
	{ NAME("unexecseg")	(syscall_func_t)0	}, /* 34   */
	{ NAME("?")		(syscall_func_t)0	}, /* 35   */
	{ NAME("select")	(syscall_func_t)0	}, /* 36   */
	{ NAME("eaccess")	emu_i_ass		}, /* 37   */
	{ NAME("paccess")	(syscall_func_t)0	}, /* 38   */
	{ NAME("sigaction")	(syscall_func_t)0	}, /* 39   */
	{ NAME("sigprocmask")	(syscall_func_t)0	}, /* 40   */
	{ NAME("sigpending")	(syscall_func_t)0	}, /* 41   */
	{ NAME("sigsuspend")	(syscall_func_t)0	}, /* 42   */
	{ NAME("getgroups")	(syscall_func_t)0	}, /* 43   */
	{ NAME("setgroups")	(syscall_func_t)0	}, /* 44   */
	{ NAME("sysconf")	(syscall_func_t)0	}, /* 45   */
	{ NAME("pathconf")	(syscall_func_t)0	}, /* 46   */
	{ NAME("fpathconf")	(syscall_func_t)0	}, /* 47   */
	{ NAME("rename")	emu_i_aa		}, /* 48   */
	{ NAME("?")		(syscall_func_t)0	}, /* 49   */
	{ NAME("utsname")	emu_i_a			}, /* 50   */
	{ NAME("?")		(syscall_func_t)0	}, /* 51   */
	{ NAME("?")		(syscall_func_t)0	}, /* 52   */
	{ NAME("?")		(syscall_func_t)0	}, /* 53   */
	{ NAME("?")		(syscall_func_t)0	}, /* 54   */
	{ NAME("gettimer")	(syscall_func_t)0	}, /* 55   */
	{ NAME("setitimer")	(syscall_func_t)0	}, /* 56   */
	{ NAME("?")		(syscall_func_t)0	}, /* 57   */
	{ NAME("?")		(syscall_func_t)0	}, /* 58   */
	{ NAME("?")		(syscall_func_t)0	}, /* 59   */
	{ NAME("?")		(syscall_func_t)0	}, /* 60   */
	{ NAME("?")		(syscall_func_t)0	}, /* 61   */
	{ NAME("?")		(syscall_func_t)0	}, /* 62   */
	{ NAME("?")		(syscall_func_t)0	}  /* 63   */
};


/* Entry:
 * The syscall number is in ax and the arguments are in bx, cx, si, di
 * (in that order reading the C function call left to right).
 *
 * Returns:
 *	int or char types in ax
 *	long in ax and bx with high word in bx
 *	long addresses in ax and bx with the segment selector in bx
 *	struct and floats are placed in static memory and a pointer returned
 *
 * Note: I think documentation for 286 function entry/exit says dx rather
 * than bx. However after a syscall the first thing that happens is that
 * bx is moved into dx. Syscalls do it one way, functions another. Love
 * the consistency :-).
 */
int
x286syscall(struct sigcontext_struct *sc)
{
	int error;
	unsigned int call_no;
#ifdef DEBUG
	unsigned int opcode = (sc->eax & 0xffff);
#endif

	error = -1;
	errno = ENOSYS;
	if ((sc->eax & 0xff) == 0x28) {
		call_no = (sc->eax >> 8) & 0xff;
		if (call_no < sizeof(xenix_syscall)/sizeof(xenix_syscall[0])) {
#ifdef DEBUG
			if (__dbf) fprintf(__dbf, "x286emul: %s (0x%04lx, 0x%04lx, 0x%04lx, 0x%04lx)\n",
				xenix_syscall[call_no].name,
				sc->ebx & 0xffff, sc->ecx & 0xffff,
				sc->esi & 0xffff, sc->edi & 0xffff);
#endif
			if (xenix_syscall[call_no].func)
				error = xenix_syscall[call_no].func(sc);
		}
	} else {
		call_no = sc->eax & 0xff;
		if (call_no < sizeof(unix_syscall)/sizeof(unix_syscall[0])) {
#ifdef DEBUG
			if (__dbf) fprintf(__dbf, "x286emul: %s (0x%04lx, 0x%04lx, 0x%04lx, 0x%04lx)\n",
				unix_syscall[call_no].name,
				sc->ebx & 0xffff, sc->ecx & 0xffff,
				sc->esi & 0xffff, sc->edi & 0xffff);
#endif
			if (unix_syscall[call_no].func)
				error = unix_syscall[call_no].func(sc);
		}
	}

	if (error < 0) {
		sc->eax = errno;
		sc->ebx = 0xff;
		sc->eflags |= 1;
	} else {
		if ((sc->eax = error) > 0xffff)
			sc->ebx = error >> 16;
		sc->eflags &= (~1);
	}

#ifdef DEBUG
	if (__dbf)
		if (error < 0)
			fprintf(__dbf, "x286emul:   syscall 0x%04lx gave error %d\n",
				opcode, errno);
		else
			fprintf(__dbf, "x286emul:   syscall 0x%04lx returned 0x%x:0x%x\n",
				opcode, sc->ebx, sc->eax);
#endif
	return;
}

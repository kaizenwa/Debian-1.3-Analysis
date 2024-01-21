/*
 * Call Map -- if you can follow how this works, then you
 *	get a feeling of complexity that we are dealing with
 *	in this emulator.
 *
 * WARNING: It is entirely possible to create loops in these structures
 *          which will cause the kernel to hang either when attempting
 *          to resolve a syscall or when changing trace settings.
 *          Careful!
 *
 * $Id: callmap.inc,v 1.67 1996/10/02 15:30:52 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/maps/callmap.inc,v $
 */

#define SC(name)	(void *)__NR_##name

#ifdef IBCS_TRACE
#  define ITR(trace, name, args)	,trace,name,args
#else
#  define ITR(trace, name, args)
#endif

/* ------------------------------------------------------------------ */

static IBCS_func Unused[] = {
   { 0,			Ukn	ITR(1, "?",	"")	},
   { 0,			Ukn	ITR(1, "?",	"")	},
   { 0,			Ukn	ITR(1, "?",	"")	},
   { 0,			Ukn	ITR(1, "?",	"")	},
   { 0,			Ukn	ITR(1, "?",	"")	},
   { 0,			Ukn	ITR(1, "?",	"")	},
   { 0,			Ukn	ITR(1, "?",	"")	},
   { 0,			Ukn	ITR(1, "?",	"")	}
};

/* ------------------------------------------------------------------ */

#ifdef IBCS_TRACE
static IBCS_func LNX_func_0x00[] = {
   { ibcs_trace_set,	1	ITR(0, "trace",		"d")	}, /*  0 */
   { ibcs_trace_func,	3	ITR(0, "trace",		"ddd")	}, /*  1 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*  2 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*  3 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*  4 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*  5 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*  6 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /*  7 */
};
static IBCS_func *iBCS_group_LNX[] = {
	LNX_func_0x00
};
#endif

/* ------------------------------------------------------------------ */

static IBCS_func WYSETCP_func_0x00[] = {
#ifdef IBCS_TRACE
   { ibcs_select,	5	ITR(0, "select",	"dxxxx")},/*  0 */
#else
   { SC(_newselect),	-5	ITR(0, "select",	"dxxxx")},/*  0 */
#endif
   { wv386_socket,	Spl	ITR(0, "socket",	"ddd")	}, /*  1 */
   { wv386_connect,	Spl	ITR(0, "connect",	"dxd")	}, /*  2 */
   { wv386_accept,	Spl	ITR(0, "accept",	"dxx")	}, /*  3 */
   { wv386_send,	Spl	ITR(0, "send",		"dxdd")}, /*  4 */
   { wv386_recv,	Spl	ITR(0, "recv",		"dxdd")}, /*  5 */
   { wv386_bind,	Spl	ITR(0, "bind",		"dxd")	}, /*  6 */
   { wv386_setsockopt,	Spl	ITR(0, "setsockopt",	"")	}  /*  7 */
};
static IBCS_func WYSETCP_func_0x08[] = {
   { wv386_listen,	Spl	ITR(0, "listen",	"dd")	}, /*  8 */
   { 0,			3	ITR(1, "recvmsg",	"dxd")	}, /*  9 */
   { 0,			3	ITR(1, "sendmsg",	"dxd")	}, /* 10 */
   { wv386_getsockopt,	Spl	ITR(0, "getsockopt",	"dddxx")}, /* 11 */
   { wv386_recvfrom,	Spl	ITR(0, "recvfrom",	"dxddxd")},/* 12 */
   { wv386_sendto,	Spl	ITR(0, "sendto",	"dxddxd")},/* 13 */
   { wv386_shutdown,	Spl	ITR(0, "shutdown",	"dd")	}, /* 14 */
   { wv386_socketpair,	Spl	ITR(0, "socketpair",	"dddx")}  /* 15 */
};
static IBCS_func WYSETCP_func_0x10[] = {
   { 0,			Ukn	ITR(1, "trace",		"")	}, /* 16 */
   { wv386_getpeername,	Spl	ITR(0, "getpeername",	"dxx")	}, /* 17 */
   { wv386_getsockname,	Spl	ITR(0, "getsockname",	"")	}, /* 18 */
   { wv386_wait3,	1	ITR(0, "wait3",		"x")	}, /* 19 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 20 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 21 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 22 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /* 23 */
};
static IBCS_func *iBCS_group_WYSETCP[] = {
	WYSETCP_func_0x00,
	WYSETCP_func_0x08,
	WYSETCP_func_0x10
};

/* ------------------------------------------------------------------ */

static IBCS_func WYSENFS_func_0x00[] = {
   { 0,			Ukn	ITR(1, "nfs_svc",	"")	}, /*  0 */
   { 0,			Ukn	ITR(1, "async_daemon",	"")	}, /*  1 */
   { 0,			Ukn	ITR(1, "nfs_getfh",	"")	}, /*  2 */
   { 0,			Ukn	ITR(1, "nfsmount",	"")	}, /*  3 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*  4 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*  5 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*  6 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /*  7 */
};
static IBCS_func *iBCS_group_WYSENFS[] = {
	WYSENFS_func_0x00
};

/* ------------------------------------------------------------------ */

static IBCS_func XNX_func_0x00[] = {
   { 0,			Ukn	ITR(1, "syscall",	"")	}, /*  0 */
   { xnx_locking,	3	ITR(0, "locking",	"ddd")	}, /*  1 */
   { xnx_creatsem,	2	ITR(1, "creatsem",	"sd")	}, /*  2 */
   { xnx_opensem,	1	ITR(1, "opensem",	"s")	}, /*  3 */
   { xnx_sigsem,	1	ITR(1, "sigsem",	"d")	}, /*  4 */
   { xnx_waitsem,	1	ITR(1, "waitsem",	"d")	}, /*  5 */
   { xnx_nbwaitsem,	1	ITR(1, "nbwaitsem",	"d")	}, /*  6 */
   { xnx_rdchk,		1	ITR(0, "rdchk",		"d")	}  /*  7 */
};
static IBCS_func XNX_func_0x08[] = {
   { 0,			Ukn	ITR(1, "stkgro",	"")	}, /*  8 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*  9 */
   { SC(ftruncate),	-2	ITR(0, "chsize",	"dd")	}, /* 10 */
   { xnx_ftime,		1	ITR(0, "ftime",		"x")	}, /* 11 */
   { xnx_nap,		1	ITR(0, "nap",		"d")	}, /* 12 */
   { xnx_sdget,		4	ITR(1, "sdget",		"sddd") }, /* 13 */
   { xnx_sdfree,	1	ITR(1, "sdfree",	"x")	}, /* 14 */
   { xnx_sdenter,	2	ITR(1, "sdenter",	"xd")	}  /* 15 */
};
static IBCS_func XNX_func_0x10[] = {
   { xnx_sdleave,	1	ITR(1, "sdleave",	"x")	}, /* 16 */
   { xnx_sdgetv,	1	ITR(1, "sdgetv",	"x")	}, /* 17 */
   { xnx_sdwaitv,	2	ITR(1, "sdwaitv",	"xd")	}, /* 18 */
   { 0,			Ukn	ITR(1, "brkctl",	"")	}, /* 19 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 20 */
   { 0,			2	ITR(0, "sco-getcwd?",	"dx")	}, /* 21 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 22 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /* 23 */
};
static IBCS_func XNX_func_0x20[] = {
   { xnx_proctl,	3	ITR(0, "proctl",	"ddx")	}, /* 32 */
   { xnx_execseg,	2	ITR(1, "execseg",	"xd")	}, /* 33 */
   { xnx_unexecseg,	1	ITR(1, "unexecseg",	"x")	}, /* 34 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 35 */
#ifdef IBCS_TRACE
   { ibcs_select,	5	ITR(0, "select",	"dxxxx")}, /* 36 */
#else
   { SC(_newselect),	-5	ITR(0, "select",	"dxxxx")}, /* 36 */
#endif
   { xnx_eaccess,	2	ITR(0, "eaccess",	"so")	}, /* 37 */
   { xnx_paccess,	5	ITR(1, "paccess",	"dddds")},/* 38 */
   { sco_sigaction,	3	ITR(0, "sigaction",	"dxx")	}  /* 39 */
};
static IBCS_func XNX_func_0x28[] = {
   { abi_sigprocmask,	3	ITR(0, "sigprocmask",	"dxx")	}, /* 40 */
   { xnx_sigpending,	1	ITR(1, "sigpending",	"x")	}, /* 41 */
   { abi_sigsuspend,	Spl	ITR(0, "sigsuspend",	"x")	}, /* 42 */
   { SC(getgroups),	-2	ITR(0, "getgroups",	"dx")	}, /* 43 */
   { SC(setgroups),	-2	ITR(0, "setgroups",	"dx")	}, /* 44 */
   { ibcs_sysconf,	1	ITR(0, "sysconf",	"d")	}, /* 45 */
   { xnx_pathconf,	2	ITR(0, "pathconf",	"sd")	}, /* 46 */
   { xnx_fpathconf,	2	ITR(0, "fpathconf",	"dd")	}  /* 47 */
};
static IBCS_func XNX_func_0x30[] = {
   { SC(rename),	-2	ITR(0, "rename",	"ss")	}, /* 48 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 49 */
   { sco_utsname,	1	ITR(0, "sco_utsname",	"x")	}, /* 50 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 51 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 52 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 53 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 54 */
   { SC(getitimer),	-2	ITR(0, "gettimer",	"dx")	}  /* 55 */
};
static IBCS_func XNX_func_0x38[] = {
   { SC(setitimer),	-3	ITR(0, "setitimer",	"dxx")	}, /* 56 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 57 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 58 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 59 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 60 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 61 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /* 62 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /* 63 */
};
static IBCS_func *iBCS_group_XNX[] = {
	XNX_func_0x00,
	XNX_func_0x08,
	XNX_func_0x10,
	Unused,
	XNX_func_0x20,
	XNX_func_0x28,
	XNX_func_0x30,
	XNX_func_0x38
};

static IBCS_func ISC_sysisc_func_0x00[] = {
   { 0,			Ukn	ITR(1, "isc_sysisc0",	"")	}, /* 00 */
   { isc_setostype,	1	ITR(0, "isc_setostype",	"d")	}, /* 01 */
   { SC(rename),	-2	ITR(0, "isc_rename",	"ss")	}, /* 02 */
   { abi_sigaction,	3	ITR(0, "isc_sigaction",	"dxx")	}, /* 03 */
   { abi_sigprocmask,	3	ITR(0, "isc_sicprocmask","dxx")	}, /* 04 */
   { 0,			1	ITR(0, "isc_sigpending","x")	}, /* 05 */
   { SC(getgroups),	-2	ITR(0, "isc_getgroups",	"dp")	}, /* 06 */
   { SC(setgroups),	-2	ITR(0, "isc_setgroups",	"dp")	}  /* 07 */
};
static IBCS_func ISC_sysisc_func_0x08[] = {
   { 0,			Ukn	ITR(1, "pathconf",	"")	}, /* 08 */
   { 0,			Ukn	ITR(1, "fpathconf",	"")	}, /* 09 */
   { ibcs_sysconf,	1	ITR(0, "sysconf",	"d")	}, /* 10 */
   { SC(waitpid),	-3	ITR(0, "isc_waitpid",	"dxx")	}, /* 11 */
   { SC(setsid),	-ZERO	ITR(0, "isc_setsid",	"")	}, /* 12 */
   { SC(setpgid),	-2	ITR(0, "isc_setpgid",	"dd")	}, /* 13 */
   { 0,			Ukn	ITR(1, "isc_adduser",	"")	}, /* 14 */
   { 0,			Ukn	ITR(1, "isc_setuser",	"")	}  /* 15 */
};
static IBCS_func ISC_sysisc_func_0x10[] = {
   { 0,			Ukn	ITR(1, "isc_sysisc16",	"")	}, /* 16 */
   { abi_sigsuspend,	Spl	ITR(0, "isc_sigsuspend","x")	}, /* 17 */
   { SC(symlink),	-2	ITR(0, "isc_symlink",	"ss")	}, /* 18 */
   { SC(readlink),	-3	ITR(0, "isc_readlink",	"spd")	}, /* 19 */
   { 0,			Ukn	ITR(1, "isc_getmajor",	"")	}, /* 20 */
   { 0,			Ukn	ITR(1, "sysisc21",	"")	}, /* 21 */
   { 0,			Ukn	ITR(1, "sysisc22",	"")	}, /* 22 */
   { 0,			Ukn	ITR(1, "sysisc23",	"")	}  /* 23 */
};

static IBCS_func *iBCS_group_ISC_sysisc[] = {
	ISC_sysisc_func_0x00,
	ISC_sysisc_func_0x08,
	ISC_sysisc_func_0x10,
};
/* ------------------------------------------------------------------ */

static IBCS_func iBCS_func_0x00[] = {
   { ibcs_syscall,	Fast	ITR(0, "syscall",	"")	}, /*    0 */
   { SC(exit),		-1	ITR(0, "exit",		"d")	}, /*    1 */
   { ibcs_fork,		Spl	ITR(0, "fork",		"")	}, /*    2 */
   { ibcs_read,		3	ITR(0, "read",		"dpd")	}, /*    3 */
   { SC(write),		-3	ITR(0, "write",		"dpd")	}, /*    4 */
   { ibcs_open,		3	ITR(0, "open",		"soo")	}, /*    5 */
   { SC(close),		-1	ITR(0, "close",		"d")	}, /*    6 */
   { ibcs_wait,		Spl	ITR(0, "wait",		"xxx")	}  /*    7 */
};
static IBCS_func iBCS_func_0x08[] = {
   { SC(creat),		-2	ITR(0, "creat",		"so")	}, /*    8 */
   { SC(link),		-2	ITR(0, "link",		"ss")	}, /*    9 */
   { SC(unlink),	-1	ITR(0, "unlink",	"s")	}, /*   10 */
   { ibcs_exec,		Spl	ITR(0, "exec",		"sxx")	}, /*   11 */
   { SC(chdir),		-1	ITR(0, "chdir",		"s")	}, /*   12 */
   { ibcs_time,		0	ITR(0, "time",		"")	}, /*   13 */
   { ibcs_mknod,	3	ITR(0, "mknod",		"soo")	}, /*   14 */
   { SC(chmod),		-2	ITR(0, "chmod",		"so")	}  /*   15 */
};
static IBCS_func iBCS_func_0x10[] = {
   { SC(chown),		-3	ITR(0, "chown",		"sdd")	}, /*   16 */
   { ibcs_brk,		1	ITR(0, "brk/break",	"x")	}, /*   17 */
   { ibcs_stat,		2	ITR(0, "stat",		"sp")	}, /*   18 */
   { ibcs_lseek,	3	ITR(0, "seek/lseek",	"ddd")	}, /*   19 */
   { ibcs_getpid,	Spl	ITR(0, "getpid",	"")	}, /*   20 */
   { 0,			Ukn	ITR(1, "mount",		"")	}, /*   21 */
   { SC(umount),	-1	ITR(0, "umount",	"s")	}, /*   22 */
   { SC(setuid),	-1	ITR(0, "setuid",	"d")	}  /*   23 */
};
static IBCS_func iBCS_func_0x18[] = {
   { ibcs_getuid,	Spl	ITR(0, "getuid",	"")	}, /*   24 */
   { SC(stime),		-1	ITR(0, "stime",		"d")	}, /*   25 */
   { ibcs_ptrace,	4	ITR(0, "ptrace",	"xdxx") }, /*   26 */
   { SC(alarm),		-1	ITR(0, "alarm",		"d")	}, /*   27 */
   { ibcs_fstat,	2	ITR(0, "fstat",		"dp")	}, /*   28 */
   { SC(pause),		-ZERO	ITR(0, "pause",		"")	}, /*   29 */
   { SC(utime),		-2	ITR(0, "utime",		"xx")	}, /*   30 */
   { 0,			Ukn	ITR(0, "stty",		"")	}  /*   31 */
};
static IBCS_func iBCS_func_0x20[] = {
   { 0,			Ukn	ITR(1, "gtty",		"")	}, /*   32 */
#ifdef EMU_SVR4
   { svr4_access,	2	ITR(0, "access",	"so")	}, /*   33 */
#else
   { SC(access),	-2	ITR(0, "access",	"so")	}, /*   33 */
#endif
   { SC(nice),		-1	ITR(0, "nice",		"d")	}, /*   34 */
   { ibcs_statfs,	4	ITR(0, "statfs",	"spdd")	}, /*   35 */
   { SC(sync),		-ZERO	ITR(0, "sync",		"")	}, /*   36 */
   { ibcs_kill,		2	ITR(0, "kill",		"dd")	}, /*   37 */
   { ibcs_fstatfs,	4	ITR(0, "fstatfs",	"dpdd")	}, /*   38 */
   { ibcs_procids,	Spl	ITR(0, "ibcs_procids",	"d")	}  /*   39 */
};
static IBCS_func iBCS_func_0x28[] = {
   { iBCS_group_XNX,	Class	ITR(0, "cxenix",	"")	}, /*   40 */
   { SC(dup),		-1	ITR(0, "dup",		"d")	}, /*   41 */
   { ibcs_pipe,		Spl	ITR(0, "pipe",		"")	}, /*   42 */
   { SC(times),		-1	ITR(0, "times",		"p")	}, /*   43 */
   { SC(profil),	-4	ITR(0, "prof",		"xxxx")}, /*   44 */
   { 0,			Ukn	ITR(1, "lock/plock",	"")	}, /*   45 */
   { SC(setgid),	-1	ITR(0, "setgid",	"d")	}, /*   46 */
   { ibcs_getgid,	Spl	ITR(0, "getgid",	"")	}  /*   47 */
};
static IBCS_func iBCS_func_0x30[] = {
   { ibcs_sigfunc,	Fast	ITR(0, "sigfunc",	"xxx")	}, /*   48 */
   { ibcs_msgsys,	Spl	ITR(0, "msgsys",	"dxddd")}, /*   49 */
   { ibcs_sysi86,	Spl	ITR(0, "sysi86/sys3b",	"d")	}, /*   50 */
   { SC(acct),		-1	ITR(0, "acct/sysacct",	"x")	}, /*   51 */
   { ibcs_shmsys,	Fast	ITR(0, "shmsys",	"ddxo")}, /*   52 */
   { ibcs_semsys,	Spl	ITR(0, "semsys",	"dddx")}, /*   53 */
   { ibcs_ioctl,	Spl	ITR(0, "ioctl",		"dxx")	}, /*   54 */
   { 0,			3	ITR(0, "uadmin",	"xxx")	}  /*   55 */
};
static IBCS_func iBCS_func_0x38[] = {
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   56 */
   { v7_utsname,	1	ITR(0, "utsys",		"x")	}, /*   57 */
   { SC(fsync),		-1	ITR(0, "fsync",		"d")	}, /*   58 */
   { ibcs_exec,		Spl	ITR(0, "execv",		"spp")	}, /*   59 */
   { SC(umask),		-1	ITR(0, "umask",		"o")	}, /*   60 */
   { SC(chroot),	-1	ITR(0, "chroot",	"s")	}, /*   61 */
   { ibcs_fcntl,	Spl	ITR(0, "fcntl",		"dxx")	}, /*   62 */
   { ibcs_ulimit,	2	ITR(0, "ulimit",	"xx")	}  /*   63 */
};
static IBCS_func iBCS_func_0x40[] = {
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   64 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   65 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   66 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   67 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   68 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   69 */
   { 0,			Ukn	ITR(1, "advfs",		"")	}, /*   70 */
   { 0,			Ukn	ITR(1, "unadvfs",	"")	}  /*   71 */
};
static IBCS_func iBCS_func_0x48[] = {
   { 0,			Ukn	ITR(1, "rmount",	"")	}, /*   72 */
   { 0,			Ukn	ITR(1, "rumount",	"")	}, /*   73 */
   { 0,			Ukn	ITR(1, "rfstart",	"")	}, /*   74 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   75 */
   { 0,			Ukn	ITR(1, "rdebug",	"")	}, /*   76 */
   { 0,			Ukn	ITR(1, "rfstop",	"")	}, /*   77 */
   { 0,			Ukn	ITR(1, "rfsys",		"")	}, /*   78 */
   { SC(rmdir),		-1	ITR(0, "rmdir",		"s")	}  /*   79 */
};
static IBCS_func iBCS_func_0x50[] = {
   { ibcs_mkdir,	2	ITR(0, "mkdir",		"so")	}, /*   80 */
   { ibcs_getdents,	3	ITR(0, "getdents",	"dxd")	}, /*   81 */
   { 0,			Ukn	ITR(1, "libattach",	"")	}, /*   82 */
   { 0,			Ukn	ITR(1, "libdetach",	"")	}, /*   83 */
#ifndef __NR_sysfs
   { ibcs_sysfs,	Spl	ITR(0, "sysfs",		"dxx")	}, /*   84 */
#else
   { SC(sysfs),		-3	ITR(0, "sysfs",		"dxx")	}, /*	84 */
#endif
   { ibcs_getmsg,	Spl	ITR(0, "getmsg",	"dxxx")	}, /*   85 */
   { ibcs_putmsg,	Spl	ITR(0, "putmsg",	"dxxd")	}, /*   86 */
   { ibcs_poll,		3	ITR(0, "poll",		"xdd")	}  /*   87 */
};
static IBCS_func iBCS_func_0x58[] = {
   { ibcs_lstat,	2	ITR(0, "lstat",		"sp")	}, /*   88 */
   { SC(symlink),	-2	ITR(0, "symlink",	"ss")	}, /*   89 */
   { SC(readlink),	-3	ITR(0, "readlink",	"spd")	}, /*   90 */
#ifdef EMU_SVR4
   { svr4_setgroups,	2	ITR(0, "setgroups",	"dp")	}, /*   91 */
   { svr4_getgroups,	2	ITR(0, "getgroups",	"dp")	}, /*   92 */
#else
   { 0,			Ukn	ITR(0, "svr4_setgroups","dp")	}, /*   91 */
   { 0,			Ukn	ITR(0, "svr4_getgroups","dp")	}, /*   92 */
#endif
   { SC(fchmod),	-2	ITR(0, "fchmod",	"do")	}, /*   93 */
   { SC(fchown),	-3	ITR(0, "fchown",	"ddd")	}, /*   94 */
   { abi_sigprocmask,	3	ITR(0, "sigprocmask",	"dxx")	}  /*   95 */
};
static IBCS_func SCOSVR3_func_0x58[] = {
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   88 */
   { sw_security,	6	ITR(0, "security",	"dxxxxx")},/*   89 */
   { SC(symlink),	-2	ITR(0, "symlink",	"ss")	}, /*   90 */
   { ibcs_lstat,	2	ITR(0, "lstat",		"sp")	}, /*   91 */
   { SC(readlink),	-3	ITR(0, "readlink",	"spd")	}, /*   92 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   93 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   94 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /*   95 */
};
static IBCS_func iBCS_func_0x60[] = {
   { abi_sigsuspend,	Spl	ITR(0, "sigsuspend",	"x")	}, /*   96 */
   { 0,			2	ITR(1, "sigaltstack",	"xx")	}, /*   97 */
   { abi_sigaction,	3	ITR(0, "sigaction",	"dxx")	}, /*   98 */
   { 0,			1	ITR(1, "sigpending",	"x")	}, /*   99 */
   { 0,			Ukn	ITR(0, "context",	"")	}, /*   100 */
   { 0,			Ukn	ITR(1, "evsys",		"")	}, /*   101 */
   { 0,			Ukn	ITR(1, "evtrapret",	"")	}, /*   102 */
   { abi_statvfs,	2	ITR(0, "statvfs",	"sp")	}  /*   103 */
};
static IBCS_func iBCS_func_0x68[] = {
   { abi_statvfs,	2	ITR(0, "fstatvfs",	"dp")	}, /*   104 */
   { iBCS_group_ISC_sysisc,ISC  ITR(0, "sysisc",	"")	}, /*   105 */
   { 0,			Ukn	ITR(1, "nfssys",	"")	}, /*   106 */
#ifdef EMU_SVR4
   { svr4_waitid,	4	ITR(0, "waitid",	"ddxd")	}, /*   107 */
#else
   { 0,			4	ITR(0, "waitid",	"ddxd")	}, /*   107 */
#endif
   { 0,			3	ITR(1, "sigsendsys",	"ddd")	}, /*   108 */
   { ibcs_hrtsys,	Spl	ITR(0, "hrtsys",	"xxx")	}, /*   109 */
   { 0,			3	ITR(1, "acancel",	"dxd")	}, /*   110 */
   { 0,			Ukn	ITR(1, "async",		"")	}  /*   111 */
};
static IBCS_func iBCS_func_0x70[] = {
   { 0,			Ukn	ITR(1, "priocntlsys",	"")	}, /*   112 */
   { 0,			2	ITR(1, "pathconf",	"sd")	}, /*   113 */
   { 0,			3	ITR(1, "mincore",	"xdx")	}, /*   114 */
   { ibcs_mmap,		6	ITR(0, "mmap",		"xxxxdx") },/*   115 */
   { SC(mprotect),	-3	ITR(0, "mprotect",	"xdx")  },/*   116 */
   { SC(munmap),	-2	ITR(0, "munmap",	"xd")   },/*   117 */
   { 0,			2	ITR(1, "fpathconf",	"dd")	}, /*   118 */
   { ibcs_fork,		Spl	ITR(0, "vfork",		"")	}  /*   119 */
};
static IBCS_func iBCS_func_0x78[] = {
   { 0,			1	ITR(1, "fchdir",	"d")	}, /*   120 */
#ifdef __NR_readv /* Around kernel 1.3.31 */
   { SC(readv),		-3	ITR(0, "readv",		"dxd")	}, /*   121 */
   { SC(writev),	-3	ITR(0, "writev",	"dxd")	}, /*   122 */
#else
   { 0,			3	ITR(1, "readv",		"dxd")	}, /*   121 */
   { ibcs_writev,	3	ITR(1, "writev",	"dxd")	}, /*   122 */
#endif
   { ibcs_xstat,	3	ITR(0, "xstat",		"dsx")	}, /*   123 */
   { ibcs_lxstat,     	3	ITR(0, "lxstat",	"dsx")	}, /*   124 */
   { ibcs_fxstat,	3	ITR(0, "fxstat",	"ddx")	}, /*   125 */
   { ibcs_xmknod,	4	ITR(0, "xmknod",	"dsox")}, /*   126 */
   { ibcs_syslocal,	Spl	ITR(0, "syslocal",	"d")	}  /*   127 */
};
static IBCS_func iBCS_func_0x80[] = {
   { 0,			Ukn	ITR(1, "setrlimit",	"")	}, /*   128 */
   { 0,			Ukn	ITR(1, "getrlimit",	"")	}, /*   129 */
   { 0,			3	ITR(1, "lchown",	"sdd")	}, /*   130 */
   { 0,			Ukn	ITR(1, "memcntl",	"")	}, /*   131 */
#ifdef EMU_XTI
   { ibcs_getpmsg,	5	ITR(0, "getpmsg",	"dxxxx")}, /*   132 */
   { ibcs_putpmsg,	5	ITR(0, "putpmsg",	"dxxdd")}, /*   133 */
#else
   { 0,			5	ITR(0, "getpmsg",	"dxxxx")}, /*   132 */
   { 0,			5	ITR(0, "putpmsg",	"dxxdd")}, /*   133 */
#endif
   { SC(rename),	-2	ITR(0, "rename",	"ss")	}, /*   134 */
   { abi_utsname,	1	ITR(0, "uname",		"x")	}  /*   135 */
};
static IBCS_func WYSEV386_func_0x80[] = {
   { ibcs_lstat,	2	ITR(0, "lstat",		"sp")	}, /*   128 */
   { SC(readlink),	-3	ITR(0, "readlink",	"spd")	}, /*   129 */
   { SC(symlink),	-2	ITR(0, "symlink",	"ss")	}, /*   130 */
   { iBCS_group_WYSETCP,Class	ITR(0, "?",		"")	}, /*   131 */
   { iBCS_group_WYSENFS,Class	ITR(0, "?",		"")	}, /*   132 */
   { wv386_gethostname,	2	ITR(0, "gethostname",	"xd")	}, /*   133 */
   { SC(sethostname),	-2	ITR(0, "sethostname",	"sd")	}, /*   134 */
   { wv386_getdomainname,2	ITR(0, "getdomainname","xd")	}  /*   135 */
};
static IBCS_func iBCS_func_0x88[] = {
   { 0,			Ukn	ITR(1, "setegid",	"")	}, /*   136 */
   { 0,			Ukn	ITR(1, "sysconfig",	"")	}, /*   137 */
   { 0,			Ukn	ITR(1, "adjtime",	"")	}, /*   138 */
   { ibcs_sysinfo,	3	ITR(0, "systeminfo",	"dsd")	}, /*   139 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   140 */
   { 0,			Ukn	ITR(1, "seteuid",	"")	}, /*   141 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   142 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /*   143 */
};
static IBCS_func WYSEV386_func_0x88[] = {
   { SC(setdomainname),	-2	ITR(0, "setdomainname","sd")	}, /*   136 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   137 */
   { SC(setreuid),	-2	ITR(0, "setreuid",	"dd")	}, /*   138 */
   { SC(setregid),	-2	ITR(0, "setregid",	"dd")	}, /*   139 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   140 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   141 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   142 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /*   143 */
};
static IBCS_func iBCS_func_0x90[] = {
   { 0,			2	ITR(1, "secsys",	"dx")	}, /*	144 */
   { 0,			4	ITR(1, "filepriv",	"sdxd")	}, /*	145 */
   { 0,			3	ITR(1, "procpriv",	"dxd")	}, /*	146 */
   { 0,			3	ITR(1, "devstat",	"sdx")	}, /*	147 */
   { 0,			5	ITR(1, "aclipc",	"ddddx")}, /*	148 */
   { 0,			3	ITR(1, "fdevstat",	"ddx")	}, /*	149 */
   { 0,			3	ITR(1, "flvlfile",	"ddx")	}, /*	150 */
   { 0,			3	ITR(1, "lvlfile",	"sdx")	}  /*	151 */
};
static IBCS_func iBCS_func_0x98[] = {
   { 0,			Ukn	ITR(1, "?",		"")	}, /*	152 */
   { 0,			2	ITR(1, "lvlequal",	"xx")	}, /*	153 */
   { 0,			2	ITR(1, "lvlproc",	"dx")	}, /*	154 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*	155 */
   { 0,			4	ITR(1, "lvlipc",	"dddx")	}, /*	156 */
   { 0,			4	ITR(1, "acl",		"sddx")	}, /*	157 */
   { 0,			Ukn	ITR(1, "auditevt",	"")	}, /*	158 */
   { 0,			Ukn	ITR(1, "auditctl",	"")	}  /*	159 */
};
static IBCS_func iBCS_func_0xa0[] = {
   { 0,			Ukn	ITR(1, "auditdmp",	"")	}, /*	160 */
   { 0,			Ukn	ITR(1, "auditlog",	"")	}, /*	161 */
   { 0,			Ukn	ITR(1, "auditbuf",	"")	}, /*	162 */
   { 0,			2	ITR(1, "lvldom",	"xx")	}, /*	163 */
   { 0,			Ukn	ITR(1, "lvlvfs",	"")	}, /*	164 */
   { 0,			2	ITR(1, "mkmld",		"so")	}, /*	165 */
   { 0,			Ukn	ITR(1, "mlddone",	"")	}, /*	166 */
   { 0,			2	ITR(0, "secadvise",	"xx")	}  /*	167 */
};
static IBCS_func iBCS_func_0xa8[] = {
   { 0,			Ukn	ITR(1, "online",	"")	}, /*	168 */
   { SC(setitimer),	-3	ITR(0, "setitimer",	"dxx")	}, /*	169 */
   { SC(getitimer),	-2	ITR(0, "getitimer",	"dx")	}, /*	170 */
   { SC(gettimeofday),	-2	ITR(0, "gettimeofday",	"xx")	}, /*	171 */
   { SC(settimeofday),	-2	ITR(0, "settimeofday",	"xx")	}, /*	172 */
   { 0,			Ukn	ITR(1, "lwpcreate",	"")	}, /*	173 */
   { 0,			Ukn	ITR(1, "lwpexit",	"")	}, /*	174 */
   { 0,			Ukn	ITR(1, "lwpwait",	"")	}  /*	175 */
};
static IBCS_func iBCS_func_0xb0[] = {
   { 0,			Ukn	ITR(1, "lwpself",	"")	}, /*	176 */
   { 0,			Ukn	ITR(1, "lwpinfo",	"")	}, /*	177 */
   { 0,			Ukn	ITR(1, "lwpprivate",	"")	}, /*	178 */
   { 0,			Ukn	ITR(1, "processorbind",	"")	}, /*	179 */
   { 0,			Ukn	ITR(1, "processorexbind","")	}, /*	180 */
   { 0,			Ukn	ITR(1, "",		"")	}, /*	181 */
   { 0,			Ukn	ITR(1, "sync_mailbox",	"")	}, /*	182 */
   { 0,			Ukn	ITR(1, "prepblock",	"")	}  /*	183 */
};
static IBCS_func iBCS_func_0xb8[] = {
   { 0,			Ukn	ITR(1, "block",		"")	}, /*	184 */
   { 0,			Ukn	ITR(1, "rdblock",	"")	}, /*	185 */
   { 0,			Ukn	ITR(1, "unblock",	"")	}, /*	186 */
   { 0,			Ukn	ITR(1, "cancelblock",	"")	}, /*	187 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*	188 */
   { 0,			Ukn	ITR(1, "pread",		"")	}, /*	189 */
   { 0,			Ukn	ITR(1, "pwrite",	"")	}, /*	190 */
   { SC(truncate),	-2	ITR(0, "truncate",	"sd")	}  /*	191 */
};
static IBCS_func iBCS_func_0xc0[] = {
   { SC(ftruncate),	-2	ITR(0, "ftruncate",	"dd")	}, /*	192 */
   { 0,			Ukn	ITR(1, "lwpkill",	"")	}, /*	193 */
   { 0,			Ukn	ITR(1, "sigwait",	"")	}, /*	194 */
   { 0,			Ukn	ITR(1, "fork1",		"")	}, /*	195 */
   { 0,			Ukn	ITR(1, "forkall",	"")	}, /*	196 */
   { 0,			Ukn	ITR(1, "modload",	"")	}, /*	197 */
   { 0,			Ukn	ITR(1, "moduload",	"")	}, /*	198 */
   { 0,			Ukn	ITR(1, "modpath",	"")	}  /*	199 */
};
static IBCS_func iBCS_func_0xc8[] = {
   { 0,			Ukn	ITR(1, "modstat",	"")	}, /*	200 */
   { 0,			Ukn	ITR(1, "modadm",	"")	}, /*	201 */
   { 0,			Ukn	ITR(1, "getksym",	"")	}, /*	202 */
   { 0,			Ukn	ITR(1, "lwpsuspend",	"")	}, /*	203 */
   { 0,			Ukn	ITR(1, "lwpcontinue",	"")	}, /*	204 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*	205 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*	206 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /*	207 */
};
static IBCS_func iBCS_func_0xf8[] = {
   { 0,			Ukn	ITR(1, "?",		"")	},
   { 0,			Ukn	ITR(1, "?",		"")	},
   { 0,			Ukn	ITR(1, "?",		"")	},
   { 0,			Ukn	ITR(1, "?",		"")	},
   { 0,			Ukn	ITR(1, "?",		"")	},
   { 0,			Ukn	ITR(1, "?",		"")	},
   { iBCS_group_WYSETCP,Class	ITR(1, "?",		"")	},
#ifdef IBCS_TRACE
   { iBCS_group_LNX,	Class	ITR(0, "?",		"")	}
#else
   { 0,			Ukn	ITR(1, "?",		"")	}
#endif
};

/* ------------------------------------------------------------------ */

#ifdef EMU_BSD

static IBCS_func BSD_func_0x00[] = {
   { ibcs_syscall,	Fast	ITR(0, "syscall",	"")	}, /*    0 */
   { SC(exit),		-1	ITR(0, "exit",		"d")	}, /*    1 */
   { ibcs_fork,		Spl	ITR(0, "fork",		"")	}, /*    2 */
   { ibcs_read,		3	ITR(0, "read",		"dpd")	}, /*    3 */
   { SC(write),		-3	ITR(0, "write",		"dpd")	}, /*    4 */
   { bsd_open,		3	ITR(0, "open",		"soo")	}, /*    5 */
   { SC(close),		-1	ITR(0, "close",		"d")	}, /*    6 */
   { SC(wait4),		-4	ITR(0, "wait4",		"dxxx")	}  /*    7 */
};
static IBCS_func BSD_func_0x08[] = {
   { SC(creat),		-2	ITR(0, "creat",		"so")	}, /*    8 */
   { SC(link),		-2	ITR(0, "link",		"ss")	}, /*    9 */
   { SC(unlink),	-1	ITR(0, "unlink",	"s")	}, /*   10 */
   { ibcs_exec,		Spl	ITR(0, "exec",		"sxx")	}, /*   11 */
   { SC(chdir),		-1	ITR(0, "chdir",		"s")	}, /*   12 */
   { SC(fchdir),	-1	ITR(0, "fchdir",	"d")	}, /*   13 */
   { ibcs_mknod,	3	ITR(0, "mknod",		"soo")	}, /*   14 */
   { SC(chmod),		-2	ITR(0, "chmod",		"so")	}  /*   15 */
};
static IBCS_func BSD_func_0x10[] = {
   { SC(chown),		-3	ITR(0, "chown",		"sdd")	}, /*   16 */
   { ibcs_brk,		1	ITR(0, "brk/break",	"x")	}, /*   17 */
   { 0,			3	ITR(1, "getfsstat",	"xdd")	}, /*   18 */
   { SC(lseek),		-3	ITR(0, "seek/lseek",	"ddd")	}, /*   19 */
   { ibcs_getpid,	Spl	ITR(0, "getpid",	"")	}, /*   20 */
   { 0,			4	ITR(1, "mount",		"dsdx")	}, /*   21 */
   { 0,			2	ITR(0, "umount",	"sd")	}, /*   22 */
   { SC(setuid),	-1	ITR(0, "setuid",	"d")	}  /*   23 */
};
static IBCS_func BSD_func_0x18[] = {
   { ibcs_getuid,	Spl	ITR(0, "getuid",	"")	}, /*   24 */
   { bsd_geteuid,	0	ITR(0, "geteuid",	"")	}, /*   25 */
   { SC(ptrace),	-4	ITR(0, "ptrace",	"xxxx")	}, /*   26 */
   { 0,			3	ITR(1, "recvmsg",	"dxd")	}, /*   27 */
   { 0,			3	ITR(1, "sendmsg",	"dxd")	}, /*   28 */
   { wv386_recvfrom,	6	ITR(0, "recvfrom",	"dxddxd")},/*   29 */
   { wv386_accept,	3	ITR(0, "accept",	"dxx")	}, /*   30 */
   { wv386_getpeername,	3	ITR(0, "getpeername",	"dxx")	}  /*   31 */
};
static IBCS_func BSD_func_0x20[] = {
   { wv386_getsockname,	Spl	ITR(0, "getsockname",	"")	}, /*   32 */
   { SC(access),	-2	ITR(0, "access",	"so")	}, /*   33 */
   { 0,			2	ITR(1, "chflags",	"sx")	}, /*   34 */
   { 0,			2	ITR(1, "fchflags",	"dx")	}, /*   35 */
   { SC(sync),		-ZERO	ITR(0, "sync",		"")	}, /*   36 */
   { ibcs_kill,		2	ITR(0, "kill",		"dd")	}, /*   37 */
   { bsd_stat,		2	ITR(0, "stat",		"sp")	}, /*   38 */
   { SC(getppid),	0	ITR(0, "getppid",	"")	}  /*   39 */
};
static IBCS_func BSD_func_0x28[] = {
   { bsd_lstat,		2	ITR(0, "lstat",		"sp")	}, /*   40 */
   { SC(dup),		-1	ITR(0, "dup",		"d")	}, /*   41 */
   { ibcs_pipe,		Spl	ITR(0, "pipe",		"")	}, /*   42 */
   { bsd_getegid,	0	ITR(0, "getegid",	"")	}, /*   43 */
   { SC(profil),	-4	ITR(0, "prof",		"xxxx")	}, /*   44 */
   { 0,			4	ITR(1, "ktrace",	"xddd")	}, /*   45 */
   { bsd_sigaction,	3	ITR(0, "sigaction",	"dxx")	}, /*   46 */
   { ibcs_getgid,	Spl	ITR(0, "getgid",	"")	}  /*   47 */
};
static IBCS_func BSD_func_0x30[] = {
   { bsd_sigprocmask,	3	ITR(0, "sigprocmask",	"dxx")	}, /*   48 */
   { 0,			0	ITR(1, "getlogin",	"")	}, /*   49 */
   { 0,			1	ITR(1, "setlogin",	"1")	}, /*   50 */
   { SC(acct),		-1	ITR(0, "acct/sysacct",	"x")	}, /*   51 */
   { bsd_sigpending,	1	ITR(1, "sigpending",	"x")	}, /*   52 */
   { 0,			Ukn	ITR(1, "sigaltstack",	"")	}, /*   53 */
   { ibcs_ioctl,	Spl	ITR(0, "ioctl",		"dxx")	}, /*   54 */
   { 0,			1	ITR(1, "reboot",	"x")	}  /*   55 */
};
static IBCS_func BSD_func_0x38[] = {
   { 0,			Ukn	ITR(1, "revoke",	"")	}, /*   56 */
   { SC(symlink),	-2	ITR(0, "symlink",	"ss")	}, /*   57 */
   { SC(readlink),	-3	ITR(0, "readlink",	"spd")	}, /*   58 */
   { ibcs_exec,		Spl	ITR(0, "execv",		"spp")	}, /*   59 */
   { SC(umask),		-1	ITR(0, "umask",		"o")	}, /*   60 */
   { SC(chroot),	-1	ITR(0, "chroot",	"s")	}, /*   61 */
   { bsd_fstat,		2	ITR(0, "fstat",		"dp")	}, /*   62 */
   { 0,			4	ITR(1, "getkerninfo",	"dxxd")	}  /*   63 */
};
static IBCS_func BSD_func_0x40[] = {
   { bsd_getpagesize,	0	ITR(0, "getpagesize",	"")	}, /*   64 */
   { 0,			2	ITR(1, "msync",		"xx")	}, /*   65 */
   { ibcs_fork,		Spl	ITR(0, "vfork",		"")	}, /*   66 */
   { 0,			Ukn	ITR(1, "vread",		"")	}, /*   67 */
   { 0,			Ukn	ITR(1, "vwrite",	"")	}, /*   68 */
   { bsd_sbrk,		1	ITR(0, "sbrk",		"d")	}, /*   69 */
   { 0,			Ukn	ITR(1, "sstk",		"")	}, /*   70 */
   { 0,			5	ITR(1, "mmap",		"xddddd")} /*   71 */
};
static IBCS_func BSD_func_0x48[] = {
   { 0,			Ukn	ITR(1, "vadvise",	"")	}, /*   72 */
   { 0,			2	ITR(1, "munmap",	"xx")	}, /*   73 */
   { 0,			3	ITR(1, "mprotect",	"xxx")	}, /*   74 */
   { 0,			3	ITR(1, "madvise",	"xxd")	}, /*   75 */
   { SC(vhangup),	-ZERO	ITR(0, "vhangup",	"")	}, /*   76 */
   { 0,			Ukn	ITR(1, "vlimit",	"")	}, /*   77 */
   { 0,			3	ITR(1, "mincore",	"xxx")	}, /*   78 */
   { SC(getgroups),	-2	ITR(0, "getgroups",	"dx")	}  /*   79 */
};
static IBCS_func BSD_func_0x50[] = {
   { SC(setgroups),	-2	ITR(0, "setgroups",	"dx")	}, /*   80 */
   { SC(getpgrp),	-ZERO	ITR(0, "getpgrp",	"")	}, /*   81 */
   { SC(setpgid),	-2	ITR(0, "setpgid",	"dd")	}, /*   82 */
   { SC(setitimer),	-3	ITR(0, "setitimer",	"dxx")	}, /*   83 */
   { SC(wait4),		-4	ITR(0, "wait4",		"dxxx")	}, /*   84 */
   { 0,			1	ITR(1, "swapon",	"s")	}, /*   85 */
   { SC(getitimer),	-2	ITR(0, "gettimer",	"dx")	}, /*   86 */
   { wv386_gethostname,	2	ITR(0, "gethostname",	"xd")	}  /*   87 */
};
static IBCS_func BSD_func_0x58[] = {
   { SC(sethostname),	-2	ITR(0, "sethostname",	"sd")	}, /*   88 */
   { bsd_getdtablesize,	0	ITR(0, "getdtablesize",	"")	}, /*   89 */
   { SC(dup2),		-2	ITR(0, "dup2",		"dd")	}, /*   90 */
   { 0,			Ukn	ITR(0, "?",		"")	}, /*   91 */
   { bsd_fcntl,		Spl	ITR(0, "fcntl",		"ddd")	}, /*   92 */
#ifdef IBCS_TRACE
   { ibcs_select,	5	ITR(0, "select",	"dxxxx")}, /*   93 */
#else
   { SC(_newselect),	-5	ITR(0, "select",	"dxxxx")}, /*   93 */
#endif
   { 0,			Ukn	ITR(0, "?",		"")	}, /*   94 */
   { SC(fsync),		-1	ITR(0, "fsync",		"d")	}  /*   95 */
};
static IBCS_func BSD_func_0x60[] = {
   { 0,			3	ITR(1, "setpriority",	"ddd")	}, /*   96 */
   { wv386_socket,	Spl	ITR(0, "socket",	"ddd")	}, /*   97 */
   { bsd_connect,	Spl	ITR(0, "connect",	"dxd")	}, /*   98 */
   { wv386_accept,	Spl	ITR(0, "accept",	"dxx")	}, /*   99 */
   { 0,			2	ITR(1, "getpriority",	"dd")	}, /*   100 */
   { wv386_send,	Spl	ITR(0, "send",		"dxdd")	}, /*   101 */
   { wv386_recv,	Spl	ITR(0, "recv",		"dxdd")	}, /*   102 */
   { 0,			1	ITR(1, "sigreturn",	"x")	}  /*   103 */
};
static IBCS_func BSD_func_0x68[] = {
   { wv386_bind,	Spl	ITR(0, "bind",		"dxd")	}, /*   104 */
   { wv386_setsockopt,	Spl	ITR(0, "setsockopt",	"dddxd")}, /*   105 */
   { wv386_listen,	Spl	ITR(0, "listen",	"dd")	}, /*   106 */
   { 0,			Ukn	ITR(1, "vtimes",	"")	}, /*   107 */
   { 0,			3	ITR(1, "sigvec",	"dxx")	}, /*   108 */
   { 0,			1	ITR(1, "sigblock",	"x")	}, /*   109 */
   { 0,			1	ITR(1, "sigsetmask",	"x")	}, /*   110 */
   { abi_sigsuspend,	Spl	ITR(0, "sigsuspend",	"x")	}  /*   111 */
};
static IBCS_func BSD_func_0x70[] = {
   { 0,			2	ITR(1, "sigstack",	"xx")	}, /*   112 */
   { 0,			4	ITR(1, "recvmsg",	"dxdx")	}, /*   113 */
   { 0,			3	ITR(1, "sendmsg",	"dxx")	}, /*   114 */
   { 0,			2	ITR(1, "vtrace",	"dd")	}, /*   115 */
   { SC(gettimeofday),	-2	ITR(0, "gettimeofday",	"xx")	}, /*   116 */
   { SC(getrusage),	-2	ITR(0, "getrusage",	"dx")	}, /*   117 */
   { wv386_getsockopt,	Spl	ITR(0, "getsockopt",	"")	}, /*   118 */
   { 0,			Ukn	ITR(1, "resuba",	"")	}  /*   119 */
};
static IBCS_func BSD_func_0x78[] = {
#ifdef __NR_readv /* Around kernel 1.3.31 */
   { SC(readv),		-3	ITR(0, "readv",		"dxd")	}, /*   121 */
   { SC(writev),	-3	ITR(0, "writev",	"dxd")	}, /*   122 */
#else
   { 0,			Ukn	ITR(1, "readv",		"")	}, /*   121 */
   { ibcs_writev,	3	ITR(0, "writev",	"dxd")	}, /*   122 */
#endif
   { SC(settimeofday),	-2	ITR(0, "settimeofday",	"xx")	}, /*   122 */
   { SC(fchown),	-3	ITR(0, "fchown",	"ddd")	}, /*   123 */
   { SC(fchmod),     	-2	ITR(0, "fchmod",	"do")	}, /*   124 */
   { wv386_recvfrom,	Spl	ITR(0, "recvfrom",	"dxddxd")},/*   125 */
   { SC(setreuid),	-2	ITR(0, "setreuid",	"dd")	}, /*   126 */
   { SC(setregid),	-2	ITR(0, "setregid",	"dd")	}  /*   127 */
};
static IBCS_func BSD_func_0x80[] = {
   { SC(rename),	-2	ITR(0, "rename",	"ss")	}, /*   128 */
   { SC(truncate),	-2	ITR(0, "truncate",	"sd")	}, /*   129 */
   { SC(ftruncate),	-2	ITR(0, "ftruncate",	"dd")	}, /*   130 */
   { 0,			2	ITR(1, "flock",		"dd")	}, /*   131 */
   { 0,			2	ITR(1, "mkfifo",	"so")	}, /*   132 */
   { wv386_sendto,	Spl	ITR(0, "sendto",	"dxddxd")},/*   133 */
   { wv386_shutdown,	Spl	ITR(0, "shutdown",	"dd")	}, /*   134 */
   { wv386_socketpair,	Spl	ITR(0, "socketpair",	"dddx")	}  /*   135 */
};
static IBCS_func BSD_func_0x88[] = {
   { ibcs_mkdir,	2	ITR(0, "mkdir",		"so")	}, /*   136 */
   { SC(rmdir),		-1	ITR(0, "rmdir",		"s")	}, /*   137 */
   { 0,			Ukn	ITR(1, "utimes",	"")	}, /*   138 */
   { 0,			Ukn	ITR(1, "sigreturn",	"x")	}, /*   139 */
   { 0,			Ukn	ITR(1, "adjtime",	"xx")	}, /*   140 */
   { wv386_getpeername,	Spl	ITR(0, "getpeername",	"dxx")	}, /*   141 */
   { 0,			0	ITR(1, "gethostid",	"")	}, /*   142 */
   { 0,			1	ITR(1, "sethostid",	"x")	}  /*   143 */
};
static IBCS_func BSD_func_0x90[] = {
   { SC(getrlimit),	-2	ITR(0, "getrlimit",	"dx")	}, /*   144 */
   { SC(setrlimit),	-2	ITR(0, "setrlimit",	"dx")	}, /*   145 */
   { bsd_killpg,	2	ITR(0, "killpg",	"dd")	}, /*   146 */
   { SC(setsid),	-ZERO	ITR(0, "setsid",	"")	}, /*   147 */
   { 0,			Ukn	ITR(1, "quotactl",	"")	}, /*   148 */
   { 0,			Ukn	ITR(1, "quota",		"")	}, /*   149 */
   { wv386_getsockname,	Spl	ITR(0, "getsockname",	"")	}, /*   150 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /*   151 */
};
static IBCS_func BSD_func_0x98[] = {
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   152 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   153 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   154 */
   { 0,			1	ITR(1, "nfssvc",	"d")	}, /*   155 */
   { bsd_getdirentries,	4	ITR(0, "getdirentries",	"dxdx")	}, /*   156 */
   { bsd_statfs,	2	ITR(0, "statfs",	"sx")	}, /*   157 */
   { bsd_fstatfs,	2	ITR(0, "fstatfs",	"dx")	}, /*   158 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /*   159 */
};
static IBCS_func BSD_func_0xa0[] = {
   { 0,			0	ITR(1, "async_daemon",	"")	}, /*   160 */
   { 0,			2	ITR(1, "getfh",		"sx")	}, /*   161 */
   { wv386_getdomainname,2	ITR(0, "getdomainname","xd")	}, /*   162 */
   { SC(setdomainname),	-2	ITR(0, "setdomainname","sd")	}, /*   163 */
   { 0,			Ukn	ITR(1, "uname",		"")	}, /*   164 */
   { 0,			Ukn	ITR(1, "sysarch",	"")	}, /*   165 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   166 */
   { 0,			Ukn	ITR(1, "?",		"")	}  /*   167 */
};
static IBCS_func BSD_func_0xa8[] = {
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   168 */
   { 0,			Ukn	ITR(1, "semsys",	"")	}, /*   169 */
   { 0,			Ukn	ITR(1, "msgsys",	"")	}, /*   170 */
   { 0,			Ukn	ITR(1, "shmsys",	"")	}, /*   171 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   172 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   173 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   174 */
   { 0,			Ukn	ITR(1, "ntp_gettime",	"")	}  /*   175 */
};
static IBCS_func BSD_func_0xb0[] = {
   { 0,			Ukn	ITR(1, "ntp_adjtime",	"")	}, /*   176 */
   { 0,			Ukn	ITR(1, "vm_allocate",	"")	}, /*   177 */
   { 0,			Ukn	ITR(1, "vm_deallocate",	"")	}, /*   178 */
   { 0,			Ukn	ITR(1, "vm_inherit",	"")	}, /*   179 */
   { 0,			Ukn	ITR(1, "vm_protect",	"")	}, /*   180 */
   { SC(setgid),	-1	ITR(0, "setgid",	"d")	}, /*   181 */
   { bsd_setegid,	1	ITR(0, "setegid",	"d")	}, /*   182 */
   { bsd_seteuid,	1	ITR(0, "seteuid",	"d")	}  /*   183 */
};

#endif /* EMU_BSD */

/* ------------------------------------------------------------------ */

static IBCS_func *iBCS_group_SVR4[] = {
	iBCS_func_0x00,
	iBCS_func_0x08,
	iBCS_func_0x10,
	iBCS_func_0x18,
	iBCS_func_0x20,
	iBCS_func_0x28,
	iBCS_func_0x30,
	iBCS_func_0x38,
	iBCS_func_0x40,
	iBCS_func_0x48,
	iBCS_func_0x50,
	iBCS_func_0x58,
	iBCS_func_0x60,
	iBCS_func_0x68,
	iBCS_func_0x70,
	iBCS_func_0x78,
	iBCS_func_0x80,
	iBCS_func_0x88,
	iBCS_func_0x90,
	iBCS_func_0x98,
	iBCS_func_0xa0,
	iBCS_func_0xa8,
	iBCS_func_0xb0,
	iBCS_func_0xb8,
	iBCS_func_0xc0,
	iBCS_func_0xc8,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	iBCS_func_0xf8
};
static IBCS_func *iBCS_group_SCOSVR3[] = {
	iBCS_func_0x00,
	iBCS_func_0x08,
	iBCS_func_0x10,
	iBCS_func_0x18,
	iBCS_func_0x20,
	iBCS_func_0x28,
	iBCS_func_0x30,
	iBCS_func_0x38,
	iBCS_func_0x40,
	iBCS_func_0x48,
	iBCS_func_0x50,
	SCOSVR3_func_0x58,
	Unused,
	iBCS_func_0x68,
	iBCS_func_0x70,
	iBCS_func_0x78,
	iBCS_func_0x80,
	iBCS_func_0x88,
	Unused,
	Unused,
	Unused,
	iBCS_func_0xa8,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	iBCS_func_0xf8
};
static IBCS_func *iBCS_group_WYSEV386[] = {
	iBCS_func_0x00,
	iBCS_func_0x08,
	iBCS_func_0x10,
	iBCS_func_0x18,
	iBCS_func_0x20,
	iBCS_func_0x28,
	iBCS_func_0x30,
	iBCS_func_0x38,
	iBCS_func_0x40,
	iBCS_func_0x48,
	iBCS_func_0x50,
	Unused,
	Unused,
	Unused,
	Unused,
	iBCS_func_0x78,
	WYSEV386_func_0x80,
	WYSEV386_func_0x88,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	iBCS_func_0xf8
};

#ifdef EMU_BSD
static IBCS_func *iBCS_group_BSD[] = {
	BSD_func_0x00,
	BSD_func_0x08,
	BSD_func_0x10,
	BSD_func_0x18,
	BSD_func_0x20,
	BSD_func_0x28,
	BSD_func_0x30,
	BSD_func_0x38,
	BSD_func_0x40,
	BSD_func_0x48,
	BSD_func_0x50,
	BSD_func_0x58,
	BSD_func_0x60,
	BSD_func_0x68,
	BSD_func_0x70,
	BSD_func_0x78,
	BSD_func_0x80,
	BSD_func_0x88,
	BSD_func_0x90,
	BSD_func_0x98,
	BSD_func_0xa0,
	BSD_func_0xa8,
	BSD_func_0xb0,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	Unused,
	iBCS_func_0xf8
};
#endif /* EMU_BSD */

/* The following sequence should match the defined values in the
 * header linux/include/linux/personality.h
 */
static IBCS_func **iBCS_personality_map[] = {
	iBCS_group_SVR4,	/* This (Linux) is used to access trace. */
	iBCS_group_SVR4,	/* SVR4 (base iBCS) */
	iBCS_group_SVR4,	/* Base SVR3 (no extensions) is a subset
				 * of SVR4.
				 */
	iBCS_group_SCOSVR3,	/* SVR3 with SCO extensions. */
	iBCS_group_WYSEV386,	/* SVR3 with Wyse extensions. */
	iBCS_group_SCOSVR3,	/* ISC 4.0 uses the same mapping as SCO. */
#ifdef EMU_BSD
	iBCS_group_BSD,		/* 386bsd, BSDI, FreeBSD, NetBSD */
#else
	NULL,
#endif
	iBCS_group_SVR4		/* Xenix is a subset of SVR3 */
};

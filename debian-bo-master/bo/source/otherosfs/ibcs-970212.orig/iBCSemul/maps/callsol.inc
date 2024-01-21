#define SC(name)	(void *)__NR_##name

#define ABORT SC(exit)
#define ABORTITR  ITR(0, "exit",  "d")

#ifdef IBCS_TRACE
#  define ITR(trace, name, args)	,trace,name,args
#else
#  define ITR(trace, name, args)
#endif

static IBCS_func Solaris_funcs [] = {
   { 0,			Ukn     ITR(1, "nosys",        	"")     },
   { SC(exit),     	-1   	ITR(0, "exit",          "d")    },
   { ABORT,        	-1   	ABORTITR                       },
   { SC(read),     	-3   	ITR(0, "read",         	"dpd")  },
   { SC(write),    	-3   	ITR(0, "write",		"dpd")	},
   { ibcs_open,    	 3   	ITR(0, "open",		"soo")	},
   { SC(close),    	-1   	ITR(0, "close",		"d")	},
   { ibcs_wait,    	Spl  	ITR(0, "wait",         	"xxx")  },
   { SC(creat),    	-2   	ITR(0, "creat",		"so")	},
   { SC(link),     	-2   	ITR(0, "link",	        "ss")	},
   { SC(unlink),   	-1   	ITR(0, "unlink",	"s")	},
   { ibcs_exec,    	Spl	ITR(0, "exec",		"sxx")	},
   { SC(chdir),    	-1   	ITR(0, "chdir",		"s")	},
   { ibcs_time,		0	ITR(0, "time",		"")	},
   { SC(mknod),		-3	ITR(0, "mknod",		"soo")	},
   { SC(chmod),		-2	ITR(0, "chmod",		"so")	}, 
   { SC(chown),		-3	ITR(0, "chown",		"sdd")	},
   { ibcs_brk,		1	ITR(0, "brk/break",	"x")	},
   { ibcs_stat,		2	ITR(0, "stat",		"sp")	},
   { SC(lseek),		-3	ITR(0, "seek/lseek",	"ddd")	},
   { ibcs_getpid,	Spl	ITR(0, "getpid",	"")	},
   { 0,			Ukn	ITR(1, "mount",		"")	},
   { SC(umount),	-1	ITR(0, "umount",	"s")	},
   { SC(setuid),	-1	ITR(0, "setuid",	"d")	},
   { ibcs_getuid,	Spl	ITR(0, "getuid",	"")	}, /*   24 */
   { SC(stime),		-1	ITR(0, "stime",		"d")	}, /*   25 */
   { ibcs_ptrace,	4	ITR(0, "ptrace",	"xdxx") }, /*   26 */
   { SC(alarm),		-1	ITR(0, "alarm",		"d")	}, /*   27 */
   { ibcs_fstat,	2	ITR(0, "fstat",		"dp")	}, /*   28 */
   { SC(pause),		-ZERO	ITR(0, "pause",		"")	}, /*   29 */
   { SC(utime),		-2	ITR(0, "utime",		"xx")	}, /*   30 */
   { 0,			Ukn	ITR(0, "stty",		"")	}, /*   31 */
   { 0,			Ukn	ITR(1, "gtty",		"")	}, /*   32 */
   { svr4_access,	2	ITR(0, "access",	"so")	}, /*   33 */
   { SC(nice),		-1	ITR(0, "nice",		"d")	}, /*   34 */
   { ibcs_statfs,	4	ITR(0, "statfs",	"spdd")	}, /*   35 */
   { SC(sync),		-ZERO	ITR(0, "sync",		"")	}, /*   36 */
   { ibcs_kill,		2	ITR(0, "kill",		"dd")	}, /*   37 */
   { ibcs_fstatfs,	4	ITR(0, "fstatfs",	"dpdd")	}, /*   38 */
   { 0,			Ukn	ITR(0, "pgrpsys",       "d")	}, /*   39 */
   { 0,			Ukn	ITR(1, "xenix",		"")	}, /*   40 */
   { SC(dup),		-1	ITR(0, "dup",		"d")	}, /*   41 */
   { ibcs_pipe,		Spl	ITR(0, "pipe",		"")	}, /*   42 */
   { SC(times),		-1	ITR(0, "times",		"p")	}, /*   43 */
   { SC(profil),	-4	ITR(0, "prof",		"xxxx")}, /*   44 */
   { 0,			Ukn	ITR(1, "lock/plock",	"")	}, /*   45 */
   { SC(setgid),	-1	ITR(0, "setgid",	"d")	}, /*   46 */
   { ibcs_getgid,	Spl	ITR(0, "getgid",	"")	}, /*   47 */
   { ibcs_sigfunc,	Fast	ITR(0, "sigfunc",	"xxx")	}, /*   48 */
   { ibcs_msgsys,	Spl	ITR(0, "msgsys",	"dxddd")}, /*   49 */
   { 0,			Ukn	ITR(1, "syssun/3b",	"")	}, /*   50 */
   { SC(acct),		-1	ITR(0, "acct/sysacct",	"x")	}, /*   51 */
   { ibcs_shmsys,	Fast	ITR(0, "shmsys",	"ddxo")}, /*   52 */
   { ibcs_semsys,	Spl	ITR(0, "semsys",	"dddx")}, /*   53 */
   { ibcs_ioctl,	Spl	ITR(0, "ioctl",		"dxx")	}, /*   54 */
   { 0,			3	ITR(0, "uadmin",	"xxx")	}, /*   55 */
   { 0,			Ukn	ITR(1, "reserved:exch",	"")	}, /*   56 */
   { v7_utsname,	1	ITR(0, "utsys",		"x")	}, /*   57 */
   { SC(fsync),		-1	ITR(0, "fsync",		"d")	}, /*   58 */
   { ibcs_exec,		Spl	ITR(0, "execv",		"spp")	}, /*   59 */
   { SC(umask),		-1	ITR(0, "umask",		"o")	}, /*   60 */
   { SC(chroot),	-1	ITR(0, "chroot",	"s")	}, /*   61 */
   { ibcs_fcntl,	Spl	ITR(0, "fcntl",		"dxx")	}, /*   62 */
   { ibcs_ulimit,	2	ITR(0, "ulimit",	"xx")	}, /*   63 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   64 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   65 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   66 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   67 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   68 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   69 */
   { 0,			Ukn	ITR(1, "advfs",		"")	}, /*   70 */
   { 0,			Ukn	ITR(1, "unadvfs",	"")	}, /*   71 */
   { 0,			Ukn	ITR(1, "rmount",	"")	}, /*   72 */
   { 0,			Ukn	ITR(1, "rumount",	"")	}, /*   73 */
   { 0,			Ukn	ITR(1, "rfstart",	"")	}, /*   74 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   75 */
   { 0,			Ukn	ITR(1, "rdebug",	"")	}, /*   76 */
   { 0,			Ukn	ITR(1, "rfstop",	"")	}, /*   77 */
   { 0,			Ukn	ITR(1, "rfsys",		"")	}, /*   78 */
   { SC(rmdir),		-1	ITR(0, "rmdir",		"s")	}, /*   79 */
   { ibcs_mkdir,	2	ITR(0, "mkdir",		"so")	}, /*   80 */
   { ibcs_getdents,	3	ITR(0, "getdents",	"dxd")	}, /*   81 */
   { 0,			Ukn	ITR(1, "libattach",	"")	}, /*   82 */
   { 0,			Ukn	ITR(1, "libdetach",	"")	}, /*   83 */
   { SC(sysfs),		-3	ITR(0, "sysfs",		"dxx")	}, /*	84 */
   { ibcs_getmsg,	Spl	ITR(0, "getmsg",	"dxxx")	}, /*   85 */
   { ibcs_putmsg,	Spl	ITR(0, "putmsg",	"dxxd")	}, /*   86 */
   { ibcs_poll,		3	ITR(0, "poll",		"xdd")	}, /*   87 */
   { ibcs_lstat,	2	ITR(0, "lstat",		"sp")	}, /*   88 */
   { SC(symlink),	-2	ITR(0, "symlink",	"ss")	}, /*   89 */
   { SC(readlink),	-3	ITR(0, "readlink",	"spd")	}, /*   90 */
   { svr4_setgroups,	2	ITR(0, "setgroups",	"dp")	}, /*   91 */
   { svr4_getgroups,	2	ITR(0, "getgroups",	"dp")	}, /*   92 */
   { SC(fchmod),	-2	ITR(0, "fchmod",	"do")	}, /*   93 */
   { SC(fchown),	-3	ITR(0, "fchown",	"ddd")	}, /*   94 */
   { abi_sigprocmask,	3	ITR(0, "sigprocmask",	"dxx")	}, /*   95 */
   { do_sigsuspend,	Spl	ITR(0, "sigsuspend",	"x")	}, /*   96 */
   { 0,			2	ITR(1, "sigaltstack",	"xx")	}, /*   97 */
   { abi_sigaction,	3	ITR(0, "sigaction",	"dxx")	}, /*   98 */
   { svr4_sigpending,	2	ITR(1, "sigpending",	"dd")	}, /*   99 */
   { svr4_context,	Spl	ITR(0, "context",	"")	}, /*   100 */
   { 0,			Ukn	ITR(1, "evsys",		"")	}, /*   101 */
   { 0,			Ukn	ITR(1, "evtrapret",	"")	}, /*   102 */
   { abi_statvfs,	2	ITR(0, "statvfs",	"sp")	}, /*   103 */
   { abi_statvfs,	2	ITR(0, "fstatvfs",	"dp")	}, /*   104 */
   { 0,			Ukn	ITR(1, "unknown",	"")	}, /*   105 */
   { 0,			Ukn	ITR(1, "nfssys",	"")	}, /*   106 */
   { svr4_waitid,	4	ITR(0, "waitid",	"ddxd")	}, /*   107 */
   { 0,			3	ITR(1, "sigsendsys",	"ddd")	}, /*   108 */
   { ibcs_hrtsys,	Spl	ITR(0, "hrtsys",	"xxx")	}, /*   109 */
   { 0,			3	ITR(1, "acancel",	"dxd")	}, /*   110 */
   { 0,			Ukn	ITR(1, "async",		"")	}, /*   111 */
   { 0,			Ukn	ITR(1, "priocntlsys",	"")	}, /*   112 */
   { 0,			2	ITR(1, "pathconf",	"sd")	}, /*   113 */
   { 0,			3	ITR(1, "mincore",	"xdx")	}, /*   114 */
   { ibcs_mmap,		6	ITR(0, "mmap",		"xxxxdx") },/*   115 */
   { SC(mprotect),	-3	ITR(0, "mprotect",	"xdx")  },/*   116 */
   { SC(munmap),	-2	ITR(0, "munmap",	"xd")   },/*   117 */
   { 0,			2	ITR(1, "fpathconf",	"dd")	}, /*   118 */
   { ibcs_fork,		Spl	ITR(0, "vfork",		"")	}, /*   119 */
   { 0,			1	ITR(1, "fchdir",	"d")	}, /*   120 */
   { SC(readv),		-3	ITR(0, "readv",		"dxd")	}, /*   121 */
   { SC(writev),	-3	ITR(0, "writev",	"dxd")	}, /*   122 */
   { ibcs_xstat,	3	ITR(0, "xstat",		"dsx")	}, /*   123 */
   { ibcs_lxstat,     	3	ITR(0, "lxstat",	"dsx")	}, /*   124 */
   { ibcs_fxstat,	3	ITR(0, "fxstat",	"ddx")	}, /*   125 */
   { ibcs_xmknod,	4	ITR(0, "xmknod",	"dsox")}, /*   126 */
   { ibcs_syslocal,	Spl	ITR(0, "syslocal",	"d")	}, /*   127 */
   { 0,			Ukn	ITR(1, "setrlimit",	"")	}, /*   128 */
   { 0,			Ukn	ITR(1, "getrlimit",	"")	}, /*   129 */
   { 0,			3	ITR(1, "lchown",	"sdd")	}, /*   130 */
   { 0,			Ukn	ITR(1, "memcntl",	"")	}, /*   131 */
   { ibcs_getpmsg,	5	ITR(0, "getpmsg",	"dxxxx")}, /*   132 */
   { ibcs_putpmsg,	5	ITR(0, "putpmsg",	"dxxdd")}, /*   133 */
   { SC(rename),	-2	ITR(0, "rename",	"ss")	}, /*   134 */
   { abi_utsname,	1	ITR(0, "uname",		"x")	}, /*   135 */
   { 0,			Ukn	ITR(1, "setegid",	"")	}, /*   136 */
   { ibcs_sysconf,	Ukn	ITR(1, "sysconfig",	"")	}, /*   137 */
   { 0,			Ukn	ITR(1, "adjtime",	"")	}, /*   138 */
   { ibcs_sysinfo,	3	ITR(0, "systeminfo",	"dsd")	}, /*   139 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   140 */
   { 0,			Ukn	ITR(1, "seteuid",	"")	}, /*   141 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   142 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*   143 */
   { 0,			2	ITR(1, "secsys",	"dx")	}, /*	144 */
   { 0,			4	ITR(1, "filepriv",	"sdxd")	}, /*	145 */
   { 0,			3	ITR(1, "procpriv",	"dxd")	}, /*	146 */
   { 0,			3	ITR(1, "devstat",	"sdx")	}, /*	147 */
   { 0,			5	ITR(1, "aclipc",	"ddddx")}, /*	148 */
   { 0,			3	ITR(1, "fdevstat",	"ddx")	}, /*	149 */
   { 0,			3	ITR(1, "flvlfile",	"ddx")	}, /*	150 */
   { 0,			3	ITR(1, "lvlfile",	"sdx")	}, /*	151 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*	152 */
   { 0,			2	ITR(1, "lvlequal",	"xx")	}, /*	153 */
   { 0,			2	ITR(1, "lvlproc",	"dx")	}, /*	154 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*	155 */
   { svr4_gettimeofday,	2	ITR(0, "gettimeofday",	"xx")	}, /*	156 */
   { SC(getitimer),	-2	ITR(0, "getitimer",	"dx")	}, /*	157 */
   { SC(setitimer),	-3	ITR(0, "setitimer",	"dxx")	}, /*	158 */
   { 0,			Ukn	ITR(1, "lwp-xxx",	"")	}, /*	159 */
   { 0,			Ukn	ITR(1, "lwp-xxx",	"")	}, /*	160 */
   { 0,			Ukn	ITR(1, "lwp-xxx",	"")	}, /*	161 */
   { 0,			Ukn	ITR(1, "lwp-xxx",	"")	}, /*	162 */
   { 0,			Ukn	ITR(1, "lwp-xxx",	"")	}, /*	163 */
   { 0,			Ukn	ITR(1, "lwp-xxx",	"")	}, /*	164 */
   { 0,			Ukn	ITR(1, "lwp-xxx",	"")	}, /*	165 */
   { 0,			Ukn	ITR(1, "lwp-xxx",	"")	}, /*	166 */
   { 0,			Ukn	ITR(0, "lwp-xxx",	"")	}, /*	167 */
   { 0,			Ukn	ITR(1, "lwp-xxx",	"")	}, /*	168 */
   { 0,			Ukn	ITR(0, "lwp-xxx",	"")	}, /*	169 */
   { 0,			Ukn	ITR(0, "lwp-xxx",	"")	}, /*	170 */
   { 0,			Ukn	ITR(0, "lwp-xxx",	"")	}, /*	171 */
   { 0,			Ukn	ITR(0, "lwp-xxx",	"")	}, /*	172 */
   { 0,			Ukn	ITR(1, "pread",		"")	}, /*	173 */
   { 0,			Ukn	ITR(1, "pwrite",	"")	}, /*	174 */
   { SC(lseek),		-3	ITR(0, "seek/lseek",	"ddd")	}, /*   175 */
   { 0,			Ukn	ITR(1, "lwpself",	"")	}, /*	176 */
   { 0,			Ukn	ITR(1, "lwpinfo",	"")	}, /*	177 */
   { 0,			Ukn	ITR(1, "lwpprivate",	"")	}, /*	178 */
   { 0,			Ukn	ITR(1, "processorbind",	"")	}, /*	179 */
   { 0,			Ukn	ITR(1, "processorexbind","")	}, /*	180 */
   { 0,			Ukn	ITR(1, "",		"")	}, /*	181 */
   { 0,			Ukn	ITR(1, "sync_mailbox",	"")	}, /*	182 */
   { 0,			Ukn	ITR(1, "prepblock",	"")	}, /*	183 */
   { 0,			Ukn	ITR(1, "block",		"")	}, /*	184 */
   { 0,			Ukn	ITR(1, "rdblock",	"")	}, /*	185 */
   { 0,			Ukn	ITR(1, "unblock",	"")	}, /*	186 */
   { 0,			Ukn	ITR(1, "cancelblock",	"")	}, /*	187 */
   { 0,			Ukn	ITR(1, "?",		"")	}, /*	188 */
   { 0,			Ukn	ITR(1, "xxxxx",		"")	}, /*	189 */
   { 0,			Ukn	ITR(1, "xxxxxe",	"")	}, /*	190 */
   { 0,			Ukn	ITR(1, "0",		"")	}, /*	191 */
   { 0,			Ukn	ITR(1, "0",		"")	}, /*	192 */
   { ibcs_trace_set,	1	ITR(0, "trace",		"d")	}, /* 193   */
   { ibcs_trace_func,	3	ITR(0, "trace",		"ddd")	}, /*  194 */
   { 0,			Ukn	ITR(1, "",		"")	}, /*	195 */
   { 0,			Ukn	ITR(1, "",		"")	}, /*	196 */
   { 0,			Ukn	ITR(1, "",		"")	}, /*	197 */
   { 0,			Ukn	ITR(1, "",		"")	}, /*	198 */
   { SC(nanosleep),	-2	ITR(1, "nanosleep",	"dd")	}, /*	199 */
};


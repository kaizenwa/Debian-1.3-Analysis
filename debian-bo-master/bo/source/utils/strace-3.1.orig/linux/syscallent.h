/*
 * Copyright (c) 1993 Branko Lankester <branko@hacktic.nl>
 * Copyright (c) 1993, 1994, 1995 Rick Sladkey <jrs@world.std.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *	$Id: syscallent.h,v 2.25 1996/05/21 03:21:06 jrs Exp $
 */

	{ 0,	0,	sys_setup,		"setup"		}, /* 0 */
	{ 1,	TP,	sys_exit,		"_exit"		}, /* 1 */
	{ 0,	TP,	sys_fork,		"fork"		}, /* 2 */
	{ 3,	0,	sys_read,		"read"		}, /* 3 */
	{ 3,	0,	sys_write,		"write"		}, /* 4 */
	{ 3,	TF,	sys_open,		"open"		}, /* 5 */
	{ 1,	0,	sys_close,		"close"		}, /* 6 */
	{ 3,	TP,	sys_waitpid,		"waitpid"	}, /* 7 */
	{ 2,	TF,	sys_creat,		"creat"		}, /* 8 */
	{ 2,	TF,	sys_link,		"link"		}, /* 9 */
	{ 1,	TF,	sys_unlink,		"unlink"	}, /* 10 */
	{ 3,	TF|TP,	sys_execve,		"execve"	}, /* 11 */
	{ 1,	TF,	sys_chdir,		"chdir"		}, /* 12 */
	{ 1,	0,	sys_time,		"time"		}, /* 13 */
	{ 3,	TF,	sys_mknod,		"mknod"		}, /* 14 */
	{ 2,	TF,	sys_chmod,		"chmod"		}, /* 15 */
	{ 3,	TF,	sys_chown,		"chown"		}, /* 16 */
	{ 0,	0,	sys_break,		"break"		}, /* 17 */
	{ 2,	TF,	sys_oldstat,		"oldstat"	}, /* 18 */
	{ 3,	0,	sys_lseek,		"lseek"		}, /* 19 */
	{ 0,	0,	sys_getpid,		"getpid"	}, /* 20 */
	{ 5,	TF,	sys_mount,		"mount"		}, /* 21 */
	{ 1,	TF,	sys_umount,		"umount"	}, /* 22 */
	{ 1,	0,	sys_setuid,		"setuid"	}, /* 23 */
	{ 0,	0,	sys_getuid,		"getuid"	}, /* 24 */
	{ 1,	0,	sys_stime,		"stime"		}, /* 25 */
	{ 4,	0,	sys_ptrace,		"ptrace"	}, /* 26 */
	{ 1,	0,	sys_alarm,		"alarm"		}, /* 27 */
	{ 2,	0,	sys_oldfstat,		"oldfstat"	}, /* 28 */
	{ 0,	TS,	sys_pause,		"pause"		}, /* 29 */
	{ 2,	TF,	sys_utime,		"utime"		}, /* 30 */
	{ 2,	0,	sys_stty,		"stty"		}, /* 31 */
	{ 2,	0,	sys_gtty,		"gtty"		}, /* 32 */
	{ 2,	TF,	sys_access,		"access"	}, /* 33 */
	{ 1,	0,	sys_nice,		"nice"		}, /* 34 */
	{ 0,	0,	sys_ftime,		"ftime"		}, /* 35 */
	{ 0,	0,	sys_sync,		"sync"		}, /* 36 */
	{ 2,	TS,	sys_kill,		"kill"		}, /* 37 */
	{ 2,	TF,	sys_rename,		"rename"	}, /* 38 */
	{ 2,	TF,	sys_mkdir,		"mkdir"		}, /* 39 */
	{ 1,	TF,	sys_rmdir,		"rmdir"		}, /* 40 */
	{ 1,	0,	sys_dup,		"dup"		}, /* 41 */
	{ 1,	0,	sys_pipe,		"pipe"		}, /* 42 */
	{ 1,	0,	sys_times,		"times"		}, /* 43 */
	{ 0,	0,	sys_prof,		"prof"		}, /* 44 */
	{ 1,	0,	sys_brk,		"brk"		}, /* 45 */
	{ 1,	0,	sys_setgid,		"setgid"	}, /* 46 */
	{ 0,	0,	sys_getgid,		"getgid"	}, /* 47 */
	{ 3,	TS,	sys_signal,		"signal"	}, /* 48 */
	{ 0,	0,	sys_geteuid,		"geteuid"	}, /* 49 */
	{ 0,	0,	sys_getegid,		"getegid"	}, /* 50 */
	{ 1,	TF,	sys_acct,		"acct"		}, /* 51 */
	{ 0,	0,	sys_phys,		"phys"		}, /* 52 */
	{ 0,	0,	sys_lock,		"lock"		}, /* 53 */
	{ 3,	0,	sys_ioctl,		"ioctl"		}, /* 54 */
	{ 3,	0,	sys_fcntl,		"fcntl"		}, /* 55 */
	{ 0,	0,	sys_mpx,		"mpx"		}, /* 56 */
	{ 2,	0,	sys_setpgid,		"setpgid"	}, /* 57 */
	{ 2,	0,	sys_ulimit,		"ulimit"	}, /* 58 */
	{ 1,	0,	sys_oldolduname,	"oldolduname"	}, /* 59 */
	{ 1,	0,	sys_umask,		"umask"		}, /* 60 */
	{ 1,	TF,	sys_chroot,		"chroot"	}, /* 61 */
	{ 2,	0,	sys_ustat,		"ustat"		}, /* 62 */
	{ 2,	0,	sys_dup2,		"dup2"		}, /* 63 */
	{ 0,	0,	sys_getppid,		"getppid"	}, /* 64 */
	{ 0,	0,	sys_getpgrp,		"getpgrp"	}, /* 65 */
	{ 0,	0,	sys_setsid,		"setsid"	}, /* 66 */
	{ 3,	TS,	sys_sigaction,		"sigaction"	}, /* 67 */
	{ 0,	TS,	sys_siggetmask,		"siggetmask"	}, /* 68 */
	{ 1,	TS,	sys_sigsetmask,		"sigsetmask"	}, /* 69 */
	{ 2,	0,	sys_setreuid,		"setreuid"	}, /* 70 */
	{ 2,	0,	sys_setregid,		"setregid"	}, /* 71 */
	{ 3,	TS,	sys_sigsuspend,		"sigsuspend"	}, /* 72 */
	{ 1,	TS,	sys_sigpending,		"sigpending"	}, /* 73 */
	{ 2,	0,	sys_sethostname,	"sethostname"	}, /* 74 */
	{ 2,	0,	sys_setrlimit,		"setrlimit"	}, /* 75 */
	{ 2,	0,	sys_getrlimit,		"getrlimit"	}, /* 76 */
	{ 2,	0,	sys_getrusage,		"getrusage"	}, /* 77 */
	{ 2,	0,	sys_gettimeofday,	"gettimeofday"	}, /* 78 */
	{ 2,	0,	sys_settimeofday,	"settimeofday"	}, /* 79 */
	{ 2,	0,	sys_getgroups,		"getgroups"	}, /* 80 */
	{ 2,	0,	sys_setgroups,		"setgroups"	}, /* 81 */
	{ 1,	0,	sys_oldselect,		"oldselect"	}, /* 82 */
	{ 2,	TF,	sys_symlink,		"symlink"	}, /* 83 */
	{ 2,	TF,	sys_oldlstat,		"oldlstat"	}, /* 84 */
	{ 3,	TF,	sys_readlink,		"readlink"	}, /* 85 */
	{ 1,	TF,	sys_uselib,		"uselib"	}, /* 86 */
	{ 1,	TF,	sys_swapon,		"swapon"	}, /* 87 */
	{ 3,	0,	sys_reboot,		"reboot"	}, /* 88 */
	{ 3,	0,	sys_readdir,		"readdir"	}, /* 89 */
	{ 1,	0,	sys_mmap,		"mmap"		}, /* 90 */
	{ 2,	0,	sys_munmap,		"munmap"	}, /* 91 */
	{ 2,	TF,	sys_truncate,		"truncate"	}, /* 92 */
	{ 2,	0,	sys_ftruncate,		"ftruncate"	}, /* 93 */
	{ 2,	0,	sys_fchmod,		"fchmod"	}, /* 94 */
	{ 3,	0,	sys_fchown,		"fchown"	}, /* 95 */
	{ 2,	0,	sys_getpriority,	"getpriority"	}, /* 96 */
	{ 3,	0,	sys_setpriority,	"setpriority"	}, /* 97 */
	{ 4,	0,	sys_profil,		"profil"	}, /* 98 */
	{ 2,	TF,	sys_statfs,		"statfs"	}, /* 99 */
	{ 2,	0,	sys_fstatfs,		"fstatfs"	}, /* 100 */
	{ 3,	0,	sys_ioperm,		"ioperm"	}, /* 101 */
	{ 2,	0,	sys_socketcall,		"socketcall"	}, /* 102 */
	{ 3,	0,	sys_syslog,		"syslog"	}, /* 103 */
	{ 3,	0,	sys_setitimer,		"setitimer"	}, /* 104 */
	{ 2,	0,	sys_getitimer,		"getitimer"	}, /* 105 */
	{ 2,	TF,	sys_stat,		"stat"		}, /* 106 */
	{ 2,	TF,	sys_lstat,		"lstat"		}, /* 107 */
	{ 2,	0,	sys_fstat,		"fstat"		}, /* 108 */
	{ 1,	0,	sys_olduname,		"olduname"	}, /* 109 */
	{ 1,	0,	sys_iopl,		"iopl"		}, /* 110 */
	{ 0,	0,	sys_vhangup,		"vhangup"	}, /* 111 */
	{ 0,	0,	sys_idle,		"idle"		}, /* 112 */
	{ 1,	0,	sys_vm86,		"vm86"		}, /* 113 */
	{ 4,	TP,	sys_wait4,		"wait4"		}, /* 114 */
	{ 1,	0,	sys_swapoff,		"swapoff"	}, /* 115 */
	{ 1,	0,	sys_sysinfo,		"sysinfo"	}, /* 116 */
	{ 5,	0,	sys_ipc,		"ipc"		}, /* 117 */
	{ 1,	0,	sys_fsync,		"fsync"		}, /* 118 */
	{ 1,	TS,	sys_sigreturn,		"sigreturn"	}, /* 119 */
	{ 2,	TP,	sys_clone,		"clone"		}, /* 120 */
	{ 2,	0,	sys_setdomainname,	"setdomainname"	}, /* 121 */
	{ 1,	0,	sys_uname,		"uname"		}, /* 122 */
	{ 3,	0,	sys_modify_ldt,		"modify_ldt"	}, /* 123 */
	{ 1,	0,	sys_adjtimex,		"adjtimex"	}, /* 124 */
	{ 3,	0,	sys_mprotect,		"mprotect"	}, /* 125 */
	{ 3,	TS,	sys_sigprocmask,	"sigprocmask"	}, /* 126 */
	{ 2,	0,	sys_create_module,	"create_module"	}, /* 127 */
	{ 4,	0,	sys_init_module,	"init_module"	}, /* 128 */
	{ 1,	0,	sys_delete_module,	"delete_module"	}, /* 129 */
	{ 1,	0,	sys_get_kernel_syms,	"get_kernel_syms"}, /* 130 */
	{ 4,	0,	sys_quotactl,		"quotactl"	}, /* 131 */
	{ 1,	0,	sys_getpgid,		"getpgid"	}, /* 132 */
	{ 1,	0,	sys_fchdir,		"fchdir"	}, /* 133 */
	{ 0,	0,	sys_bdflush,		"bdflush"	}, /* 134 */
	{ 3,	0,	sys_sysfs,		"sysfs"		}, /* 135 */
	{ 1,	0,	sys_personality,	"personality"	}, /* 136 */
	{ 5,	0,	sys_afs_syscall,	"afs_syscall"	}, /* 137 */
	{ 1,	0,	sys_setfsuid,		"setfsuid"	}, /* 138 */
	{ 1,	0,	sys_setfsgid,		"setfsgid"	}, /* 139 */
	{ 5,	0,	sys_llseek,		"_llseek"	}, /* 140 */
	{ 3,	0,	sys_getdents,		"getdents"	}, /* 141 */
	{ 5,	0,	sys_select,		"select"	}, /* 142 */
	{ 2,	0,	sys_flock,		"flock"		}, /* 143 */
	{ 3,	0,	sys_msync,		"msync"		}, /* 144 */
	{ 5,	0,	sys_readv,		"readv"		}, /* 145 */
	{ 5,	0,	sys_writev,		"writev"	}, /* 146 */
	{ 5,	0,	sys_getsid,		"getsid"	}, /* 147 */
	{ 5,	0,	sys_fdatasync,		"fdatasync"	}, /* 148 */
	{ 5,	0,	sys_sysctl,		"_sysctl"	}, /* 149 */
	{ 5,	0,	sys_mlock,		"mlock"		}, /* 150 */
	{ 5,	0,	sys_munlock,		"munlock"	}, /* 151 */
	{ 5,	0,	sys_mlockall,		"mlockall"	}, /* 152 */
	{ 5,	0,	sys_munlockall,		"munlockall"	}, /* 153 */
	{ 5,	0,	sys_sched_setparam,	"sched_setparam"}, /* 154 */
	{ 5,	0,	sys_sched_getparam,	"sched_getparam"}, /* 155 */
	{ 5,	0,	sys_sched_setscheduler,	"sched_setscheduler"}, /* 156 */
	{ 5,	0,	sys_sched_getscheduler,	"sched_getscheduler"}, /* 157 */
	{ 5,	0,	sys_sched_yield,	"sched_yield"}, /* 158 */
	{ 5,	0,	sys_sched_get_priority_max,"sched_get_priority_max"}, /* 159 */
	{ 5,	0,	sys_sched_get_priority_min,"sched_get_priority_min"}, /* 160 */
	{ 5,	0,	sys_sched_rr_get_interval,"sched_rr_get_interval"}, /* 161 */
	{ 5,	0,	sys_nanosleep,		"nanosleep"	}, /* 162 */
	{ 5,	0,	sys_mremap,		"mremap"	}, /* 163 */
	{ 5,	0,	printargs,		"SYS_164"	}, /* 164 */
	{ 5,	0,	printargs,		"SYS_165"	}, /* 165 */
	{ 5,	0,	printargs,		"SYS_166"	}, /* 166 */
	{ 5,	0,	printargs,		"SYS_167"	}, /* 167 */
	{ 5,	0,	printargs,		"SYS_168"	}, /* 168 */
	{ 5,	0,	printargs,		"SYS_169"	}, /* 169 */
	{ 5,	0,	printargs,		"SYS_170"	}, /* 170 */
	{ 5,	0,	printargs,		"SYS_171"	}, /* 171 */
	{ 5,	0,	printargs,		"SYS_172"	}, /* 172 */
	{ 5,	0,	printargs,		"SYS_173"	}, /* 173 */
	{ 5,	0,	printargs,		"SYS_174"	}, /* 174 */
	{ 5,	0,	printargs,		"SYS_175"	}, /* 175 */
	{ 5,	0,	printargs,		"SYS_176"	}, /* 176 */
	{ 5,	0,	printargs,		"SYS_177"	}, /* 177 */
	{ 5,	0,	printargs,		"SYS_178"	}, /* 178 */
	{ 5,	0,	printargs,		"SYS_179"	}, /* 179 */

	{ 8,	0,	printargs,		"socket_subcall"}, /* 180 */
	{ 3,	TN,	sys_socket,		"socket"	}, /* 181 */
	{ 3,	TN,	sys_bind,		"bind"		}, /* 182 */
	{ 3,	TN,	sys_connect,		"connect"	}, /* 183 */
	{ 2,	TN,	sys_listen,		"listen"	}, /* 184 */
	{ 3,	TN,	sys_accept,		"accept"	}, /* 185 */
	{ 3,	TN,	sys_getsockname,	"getsockname"	}, /* 186 */
	{ 3,	TN,	sys_getpeername,	"getpeername"	}, /* 187 */
	{ 4,	TN,	sys_socketpair,		"socketpair"	}, /* 188 */
	{ 4,	TN,	sys_send,		"send"		}, /* 189 */
	{ 4,	TN,	sys_recv,		"recv"		}, /* 180 */
	{ 6,	TN,	sys_sendto,		"sendto"	}, /* 191 */
	{ 6,	TN,	sys_recvfrom,		"recvfrom"	}, /* 192 */
	{ 2,	TN,	sys_shutdown,		"shutdown"	}, /* 193 */
	{ 5,	TN,	sys_setsockopt,		"setsockopt"	}, /* 194 */
	{ 5,	TN,	sys_getsockopt,		"getsockopt"	}, /* 195 */
	{ 5,	TN,	sys_sendmsg,		"sendmsg"	}, /* 196 */
	{ 5,	TN,	sys_recvmsg,		"recvmsg"	}, /* 197 */
	{ 5,	0,	printargs,		"SYS_198"	}, /* 198 */
	{ 5,	0,	printargs,		"SYS_199"	}, /* 199 */

	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 200 */
	{ 4,	TI,	printargs,		"semop"		}, /* 201 */
	{ 4,	TI,	sys_semget,		"semget"	}, /* 202 */
	{ 4,	TI,	sys_semctl,		"semctl"	}, /* 203 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 204 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 205 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 206 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 207 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 208 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 209 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 210 */
	{ 4,	TI,	sys_msgsnd,		"msgsnd"	}, /* 211 */
	{ 4,	TI,	sys_msgrcv,		"msgrcv"	}, /* 212 */
	{ 4,	TI,	sys_msgget,		"msgget"	}, /* 213 */
	{ 4,	TI,	sys_msgctl,		"msgctl"	}, /* 214 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 215 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 216 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 217 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 218 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 219 */
	{ 4,	0,	printargs,		"ipc_subcall"	}, /* 220 */
	{ 4,	TI,	sys_shmat,		"shmat"		}, /* 221 */
	{ 4,	TI,	sys_shmdt,		"shmdt"		}, /* 222 */
	{ 4,	TI,	sys_shmget,		"shmget"	}, /* 223 */
	{ 4,	TI,	sys_shmctl,		"shmctl"	}, /* 224 */
	{ 5,	0,	printargs,		"SYS_225"	}, /* 225 */
	{ 5,	0,	printargs,		"SYS_226"	}, /* 226 */
	{ 5,	0,	printargs,		"SYS_227"	}, /* 227 */
	{ 5,	0,	printargs,		"SYS_228"	}, /* 228 */
	{ 5,	0,	printargs,		"SYS_229"	}, /* 229 */

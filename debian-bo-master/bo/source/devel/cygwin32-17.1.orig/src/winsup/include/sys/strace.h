/* sys/strace.h */

/* This file contains routines for tracing system calls and other internal
   phenomenon.

   When tracing system calls, try to use the same style throughout:

   result = syscall (arg1, arg2, arg3) [optional extra stuff]

   If a system call can block (eg: read, write, wait), print another message
   before hanging so the user will know why the program has stopped.

   Note: __seterrno will also print a trace message.  Have that printed
   *first*.  This will make it easy to always know what __seterrno is
   refering to.  For the same reason, try not to have __seterrno messages
   printed alone.
*/

#ifndef _SYS_STRACE_H
#define _SYS_STRACE_H

#ifdef __cplusplus
extern "C" {
#endif

/* Bitmasks of tracing messages to print.  */

#define _STRACE_ALL	0x01  /* so behaviour of strace=1 is unchanged */
#define _STRACE_FLUSH	0x02  /* flush output buffer after every message */
#define _STRACE_INHERIT 0x04  /* children inherit mask from parent */
#define _STRACE_UHOH	0x08  /* unusual or weird phenomenon */
#define _STRACE_SYSCALL	0x10  /* system calls */
#define _STRACE_STARTUP	0x20  /* argc/envp printout at startup */
#define _STRACE_DEBUG   0x40  /* info to help debugging */
#define _STRACE_PARANOID 0x80 /* paranoid info */
#define _STRACE_TERMIOS	0x100 /* info for debugging termios stuff */
#define _STRACE_SELECT	0x200 /* info on ugly select internals */
#define _STRACE_WM	0x400 /* trace windows messages (enable _strace_wm) */

/* Return the current strace mask.  */
int strace();

void __sys_printf (const char *, ...);

void _strace_wm(int message, int word, int lon);

#define debug_printf if (strace() &(_STRACE_DEBUG|1)) __sys_printf
#define syscall_printf if (strace() &(_STRACE_SYSCALL|1)) __sys_printf
#define paranoid_printf if (strace() &(_STRACE_PARANOID|1)) __sys_printf
#define termios_printf if (strace() &(_STRACE_TERMIOS|1)) __sys_printf
#define select_printf if (strace() &(_STRACE_SELECT|1)) __sys_printf

#ifdef __cplusplus
}
#endif

#endif

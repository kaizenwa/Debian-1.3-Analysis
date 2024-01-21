#include <ansidecl.h>
#include <stddef.h>
#include <signal.h>


/* This is a list of all known signal numbers.  */

CONST char *CONST _sys_siglist[] =
  {
    "Unknown signal",
    "Hangup",
    "Interrupt",
    "Quit",
    "Illegal instruction",
    "Trace/breakpoint trap",
    "IOT trap/Abort",
    "Bus error",
    "Floating point exception",
    "Killed",
    "User defined signal 1",
    "Segmentation fault",
    "User defined signal 2",
    "Broken pipe",
    "Alarm clock",
    "Terminated",
    "Stack fault",
    "Child exited",
    "Continued",
    "Stopped (signal)",
    "Stopped",
    "Stopped (tty input)",
    "Stopped (tty output)",
    "Possible I/O",
    "CPU time limit exceeded",
    "File size limit exceeded",
    "Virtual time alarm",
    "Profile signal",
    "Window size changed",
    "File lock lost",
    "Power failure",
    "Unused signal",
    NULL
  };

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (_sys_siglist, sys_siglist);
#endif

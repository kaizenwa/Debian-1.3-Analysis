/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#ifndef _JED_SIG_H_
#define _JED_SIG_H_

#ifndef msdos
extern void init_signals (void);
#endif
extern int Stdin_Is_TTY;

#ifdef __unix__
# ifdef SIGTSTP
extern void sig_sys_spawn_cmd(int);
extern int Signal_Sys_Spawn_Flag;      /* used if something else sends stop */
# endif
#endif

#ifdef HAS_RESIZE_PENDING
extern volatile int Jed_Resize_Pending;
extern void jed_resize_display (int);
#endif

#endif /* _JED_SIG_H_ */

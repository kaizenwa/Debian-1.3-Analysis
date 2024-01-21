/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
typedef void (*ChildCallback)(void*, int, char*);

extern void	 init_child_contexts(void);
extern void	 suspend_child_contexts(void);
extern void	 resume_child_contexts(void);
extern pid_t	 fork_nicely(void*, ChildCallback, int);
extern pid_t	 wait_for_pid(pid_t, int*, char*);
extern char	*signal_string(int);

extern void	 block_sighup(void);
extern void	 unblock_sighup(void);

#define STDERR_BUFFLEN	1024

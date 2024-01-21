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
extern char	thread_ahead_char(GROUP*);
extern void	thread_ahead_shutdown(void);
extern int	thread_ahead_check(GROUP*);
extern void	thread_ahead_leave_group(GROUP*);
extern void	thread_ahead_init(void);
extern int	thread_ahead_todo(void);

extern void	action_schedule_thread_ahead(Widget w, XEvent *event,
					     String *params,
					     Cardinal *no_params);


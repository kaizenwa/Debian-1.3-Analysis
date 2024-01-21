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
#define SAVE_BOGUS_FROM	(1<<0)
#define SAVE_BOGUS_SUBJ	(1<<1)
#define SAVE_HEAD	(1<<2)
#define SAVE_BODY	(1<<3)
#define SAVE_EMPTY	(1<<4)

extern void	popup_save(void);
extern void	popdown_save(void);
extern void	action_save(Widget, XEvent*, String*, Cardinal*);
extern void	action_pipe(Widget, XEvent*, String*, Cardinal*);
extern void	pipe_context_callback(void*, int, char*);
extern void	set_busy_save(int);
extern int	save_to_file(FILE*, char*, ARTICLE**, long, int, long*);
extern void	text_url_callback(Widget, XtPointer, XtPointer);

/*
 * timer.h: header for timer.c 
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#ifndef _TIMER_H_
#define _TIMER_H_

/* functions that may be called by others */
extern	void	timercmd _((char *, char *, char *));
extern	void	ExecuteTimers _((void));
extern	char	*add_timer _((char *, long, int (*) (void *), char *, char *));
extern	int	delete_timer _((char *));
extern	time_t	TimerTimeout _((void));
char		*tcl_add_timer _((TimerList **, int, char *, unsigned long));
int 		tcl_remove_timer _((TimerList **, unsigned long));


#endif /* _TIMER_H_ */

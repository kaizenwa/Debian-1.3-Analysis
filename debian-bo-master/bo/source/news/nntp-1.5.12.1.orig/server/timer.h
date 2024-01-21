/* @(#) $Id: timer.h,v 1.4 1994/11/01 05:57:31 sob Exp sob $ */

struct timer {
	void (*subr)();		/* routine to invoke at timeout */
	int resetoninput;	/* if true, reset timer on input */
	long seconds;		/* seconds until a timeout */
	long left;		/* seconds left until next timeout */
};

void timer_init();
int timer_sleep();

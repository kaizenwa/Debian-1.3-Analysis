/*
 * timer.c - This emulates the kernel timer routines in user space.
 *           Ugly, but it works.
 *
 * Copyright (c) 1994, 1995, 1996 Eric Schenk.
 * All rights reserved. Please see the file LICENSE which should be
 * distributed with this software for terms of use.
 */

#include "diald.h"

#include <signal.h>
#include <sys/time.h>
#include <sys/times.h>
#include <unistd.h>

static struct timer_lst head = {&head,&head,0,0,0,0};

/*
 * The following gets a time stamp based on the number of system
 * clock ticks since the system has been up.
 * Note that this measure of time is immune to changes in the
 * wall clock setting, and so we don't have to worry about the
 * wall clock getting mucked with.
 */

unsigned long timestamp()
{
   struct tms buf;
   return times(&buf)/CLK_TCK;
}

void init_timer(struct timer_lst * timer)
{
    timer->next = NULL;
    timer->prev = NULL;
}

/*
 * Basic idea: store time outs in order.
 * I'd be happier with a basic priority queue of some kind,
 * but this was the most direct way to code it, and it
 * matched the original kernel type definition I was using.
 */

void add_timer(struct timer_lst *timer)
{
    struct timer_lst *c;
    unsigned long atime;
    atime = timestamp();
    timer->expected = atime+timer->expires;
    c = head.next;
    /* march down the list looking for a home */
    while (c != &head) {
	if (timer->expected < c->expected) break;
	c = c->next;
    }

    timer->next = c;
    c->prev->next = timer;
    timer->prev = c->prev;
    c->prev = timer;
}

int del_timer(struct timer_lst *timer)
{
    unsigned long atime;
    atime = timestamp();
    /* return 0 if timer was not active */
    if (!timer->next) { return 0; }
    timer->next->prev = timer->prev;
    timer->prev->next = timer->next;
    timer->next = timer->prev = NULL;
    return 1;
}

int validate_function(struct timer_lst *c)
{
	if (c->function == del_impulse
	|| c->function == del_connection
	|| c->function == slip_start_fail)
		return 1;
	syslog(LOG_ERR, "Caught a bad function value %p. Tell Eric.\n",
		c->function);
	return 0;
}

void fire_timers()
{
    struct timer_lst *cn;
    struct timer_lst *c = head.next;
    unsigned long atime = timestamp();
    
    while (c != &head && c->expected <= atime) {
	c->next->prev = c->prev;
	c->prev->next = c->next;
	cn = c->next;
	c->next = c->prev = NULL;
	/* process the data, this may free the timer entry,
	 * so we can't refer to the contents of c again.
	 */
	if (validate_function(c))
		(*c->function)(c->data);
	c = cn;
    }
}

int next_alarm()
{
    unsigned long expires;
    if (head.next == &head)
	return 0;
    expires = head.next->expected - timestamp();
    if (expires < 0) expires = 0;
    return expires;
}

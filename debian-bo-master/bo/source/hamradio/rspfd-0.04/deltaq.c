/*
 * deltaq.c: Implementation of deltaq, see header file
 * rspfd: Radio Shortest Path Daemon. A router for packet radio networks.
 * Copyright (C) 1995 Craig Small VK2XLZ
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <signal.h> 
#include <stdio.h> 
#include <stdlib.h>
#include <unistd.h>
#include "deltaq.h"

static void dq_alarm(int sig);
static int dq_id = 1;		/* A counter to ID dq entries */

struct dq_node	*deltaq_head;

int dq_add(u_int delay, void (*funct)(int))
{
	struct dq_node *ptr;
	struct dq_node *newptr;
	struct dq_node *prevptr;
	u_int	tmr_delay;
	int oldmask;

	/* First we check that there is some delay, we cannot handle putting
	 * something on the queue that is 0 seconds away.  It means the timer
	 * event would go off before the caller got the timer id
	 * If we get a delay of 0, we send back an error
	 */
	if (delay == 0)
		return 0;
		
	/* We cannot have a id of 0, that means an error */
	if (dq_id == 0)
		dq_id++;

	/* Get time remaining from first delay and disable timer */
	oldmask = sigblock( sigmask(SIGALRM));
	tmr_delay = alarm(0);	
	
	if (deltaq_head == NULL) {
		ptr = (struct dq_node*)malloc(sizeof(struct dq_node));
		if (ptr == NULL) {
			fprintf(stderr,"RSPFd: dq_add(): Memory squeze.\n");
			signal(SIGALRM, dq_alarm);
			/* Sanity check on tmr_delay, must be 0 if we have null */
			alarm(0);
			sigsetmask(oldmask);
			return 0;
		}
		ptr->delay = delay;
		ptr->id = dq_id++;
		ptr->funct = funct;
		ptr->next = NULL;
		deltaq_head = ptr;
		/*
		 * Set-up timer 
		 */
		signal(SIGALRM, dq_alarm);
		alarm(delay);
		sigsetmask(oldmask);
		return ptr->id;
	}
	/* Special case of first node */
	/* The real delay is what is left on the timer, not what we started
	 * with for the first entry in the delta q 
	 */
	ptr = deltaq_head;
	if ( delay < tmr_delay) {
		newptr = (struct dq_node*)malloc(sizeof(struct dq_node));
		if (newptr == NULL) {
			fprintf(stderr,"RSPFd: dq_add(): Memory squeze.\n");
			/* Reset timer */
			signal(SIGALRM, dq_alarm);
			/* Sanity check on tmr_delay */
			if ( (tmr_delay > deltaq_head->delay) ||
			     (tmr_delay < 1) )
				alarm(1);
			else	
				alarm(tmr_delay);			
			sigsetmask(oldmask);
			return 0;
		}
		deltaq_head = newptr;
		newptr->delay = delay;
		newptr->id = dq_id++;
		newptr->funct = funct;
		newptr->next = ptr;			
		/* 
		 * New delay of old timer event is how many secs left on the
		 * timer minus the time it will take for the new event in
		 * front of us to time-out
		 */
		newptr->next->delay = tmr_delay - delay;
		/* 
		 * Signal is ok, but set the alarm 
		 */
		signal(SIGALRM, dq_alarm);
		alarm(delay);
		sigsetmask(oldmask);
		return newptr->id;
	} 
	/* 
	 * The amount we have to wait here is how many secs left on timer
	 */
	delay -= tmr_delay;
	prevptr = ptr;
	ptr = prevptr->next;
		
	while (ptr != NULL) {
		if (ptr->delay > delay) {
			newptr = (struct dq_node*)malloc(sizeof(struct dq_node));
			newptr->delay = delay;
			newptr->id = dq_id++;
			newptr->funct = funct;
			newptr->next = ptr;
			prevptr->next = newptr;
			ptr->delay -= delay;
			/*
			 * Set alarm back on 
			 */
			signal(SIGALRM, dq_alarm);
			/* Sanity check on tmr_delay */
			if ( (tmr_delay > deltaq_head->delay) ||
			     (tmr_delay < 1) )
				alarm(1);
			else	
				alarm(tmr_delay);			
			sigsetmask(oldmask);
			return newptr->id;
		}
		delay -= ptr->delay;
		prevptr = ptr;
		ptr = prevptr->next;
	}
	/* 
	 * We've fallen off the end of the queue, so this is 
	 * the longest delay 
	 */
	newptr = (struct dq_node*)malloc(sizeof(struct dq_node));
	if (newptr == NULL) {
		fprintf(stderr,"RSPFd: dqadd(): Memory squeze.\n");
		sigsetmask(oldmask);
		signal(SIGALRM, dq_alarm);
		/* Sanity check on tmr_delay */
		if ( (tmr_delay > deltaq_head->delay) ||
		     (tmr_delay < 1) )
			alarm(1);
		else	
			alarm(tmr_delay);
		return 0;
	}
	newptr->delay = delay;
	newptr->id = dq_id++;
	newptr->funct = funct;
	newptr->next = (struct dq_node*)NULL;
	prevptr->next = newptr;
	signal(SIGALRM, dq_alarm);
	/* Sanity check on tmr_delay */
	if ( (tmr_delay > deltaq_head->delay) ||
	     (tmr_delay < 1) )
		alarm(1);
	else	
		alarm(tmr_delay);
	sigsetmask(oldmask);
	return newptr->id;
} /* dq_add() */	

int dq_del(int id)
{
	struct dq_node *ptr;
	struct dq_node *prevptr;
	u_int delay;
	u_int tmr_delay;
	
	if (deltaq_head == NULL) 
		return 0;
	/*		
	 * Disable Alarm
	 */
	tmr_delay = alarm(0);
	/* Sanity check on tmr_delay */
	if (tmr_delay == 0)
		tmr_delay = 1;

	ptr = deltaq_head;	
	
	/* Special case for first in queue */
	if (ptr->id == id) 
	{
		deltaq_head = ptr->next;
		delay = ptr->delay;
		free(ptr);
		/* Fix up timer */
		ptr = deltaq_head;
		if (ptr == NULL) 
		{
			alarm(0);
		} else 
		{
			ptr->delay += delay;
			signal(SIGALRM, dq_alarm);
			alarm(ptr->delay);
		}
		return 1;
	}
	prevptr = ptr;
	ptr = prevptr->next;
	while (ptr != NULL)		
	{
		if (ptr->id == id) 
		{
			delay = ptr->delay;
			prevptr->next = ptr->next;
			if (ptr->next != NULL)
				ptr->next->delay += delay;
			free(ptr);
			ptr = NULL;
			signal(SIGALRM, dq_alarm);
			alarm(tmr_delay);
			return 1;
		}
		prevptr = ptr;
		ptr = prevptr->next;
	}
	/* Fell off, couldn't find it */
	signal(SIGALRM, dq_alarm);
	alarm(tmr_delay);
	return 0;
} /* dq_del */

								
void dq_alarm(int sig)
{
	struct dq_node *ptr;
	void (*funct)(int);
	int id;
	int oldmask;
	
	/* 
	 * Turn off alarm 
	 */
	oldmask = sigblock(sigmask(SIGALRM)); 
	alarm(0);

	if (deltaq_head == NULL) {
		fprintf(stderr,"RSPFd: dq_alarm(): Got alarm with empty deltaq.\n");
		sigsetmask(oldmask);
		return;
	}
	
	funct = deltaq_head->funct;
	ptr = deltaq_head->next;
	id = deltaq_head->id;
	/*
	 * Clear deltaq entry and setup for next one */
	free(deltaq_head);
	deltaq_head = ptr;
	/*
	 * Set up ready for the next alarm, we are blocking the alarm
	 * signal so we'll get the programs running in order.  This is 
	 * done to avoid the race conditions on the delta queue head we
	 * had when this stuff was done after the function was processed
	 */
	if (deltaq_head != NULL) 
	{
		/* Special case with no delay */
		if (deltaq_head->delay == 0) 
		{
			signal(SIGALRM, dq_alarm);
			raise(SIGALRM);
		} else 
		{
			signal(SIGALRM, dq_alarm);
			alarm(deltaq_head->delay);
		}
	} 
	/* Run the actual function */
	(void) funct(id);

	/* Re-enable the signals */	
	sigsetmask(oldmask);
} /* dq_alarm() */


/*
 * dq_dump()
 *
 * Debugging routine to print the contents of a delta queue
 *
 * Returns:
 *	Nothing
 */
void dq_dump(char *c, int len)
{
	struct dq_node *ptr;
	int oldmask;
	u_int tmr_delay;

	if (len < 100)
		return;
	sprintf(c,"\nDelta queue contents\n");
		
	if (deltaq_head == NULL) {
		sprintf(c, "*** EMPTY LIST ***\n");
		return;
	}
	oldmask = sigblock(sigmask(SIGALRM));		 
	tmr_delay = alarm(0);
	/* Sanity check on tmr_delay */
	if ( tmr_delay < 1 )
		alarm(1);
	else	
		alarm(tmr_delay);
	ptr = deltaq_head;
		sprintf(c, "ID  Delay Funct\n");	
	while (ptr != NULL) {
		if (len - strlen(c) < 15)
			break;
		sprintf(c, "%#3x %4d  ", ptr->id, ptr->delay);
		if (ptr->funct == NULL)
			printf("NO FUNCTION\n");
		else
			printf("%#3lx\n", (long)(ptr->funct));
		ptr = ptr->next;
	}
	if (len - strlen(c) > 30)
	{
		sprintf(c, "Time left on alarm %d\n", tmr_delay);
		sprintf(c, "---------\n");
	}
	sigsetmask(oldmask);
	/* Sanity check on tmr_delay */
	if (tmr_delay < 1)
		alarm(1);
	else
		alarm(tmr_delay);
}

		
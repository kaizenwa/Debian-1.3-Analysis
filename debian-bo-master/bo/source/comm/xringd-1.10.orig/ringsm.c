/*
 * File:	ringsm.c
 *
 * Synopsis:	ring state machine
 *
 * System:	unix
 *
 * Reads and parses a configuration file of lines of this form:
 * R secs[-secs] [ R secs[-secs] ] ... : command
 * which specify the pattern of rings+delays leading to the execution of 
 * a command. 
 * The "alphabet" of rings+delays can be used to create a relatively 
 * interesting number of possible sequences which activate specific
 * commands when recognised.
 *
 * This module is suitable (through available hooks) for use in any getty 
 * wishing to add this extra ring intelligence. Alarms are scheduled via 
 * a callback so the caller of this module can take care of alarm reporting.
 * My intention is that people writing getty-like programs include this
 * functionality in their sw; I am open to any suggestion they might have.
 *
 * Copyright (c) 1995-1996 Angelo Haritsis. All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *                                                                     
 * $Id: ringsm.c,v 1.4 1996/02/17 15:50:07 ah Exp ah $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <fcntl.h>
#include <signal.h>
#include "ringsm.h"

#define MAX_LINE_LEN 512

#define DBG(x)	x		/* for callback logging */
/* #define DBG(x) */

enum event_type {
	ev_RING,		/* ring event (min,max time it may occur) */
	ev_FINALTIMEOUT,	/* final tmout event */
};

struct s_ringsm_event {
	enum event_type etype;
	time_t min, max;	/* min, max time scale for this event */
};
	
struct s_ringsm_seq {
	int active;			/* sequence still active? */
	int ev_now;			/* active event */
	int num_events;
	struct s_ringsm_event *event;	/* dynamic array of events */
	char *cmd;			/* command to run if succeded */
};

#define ringsm_ev_now(s)	( (s)->event[(s)->ev_now] )
#define ringsm_ev_next(s)	( (s)->event[(s)->ev_now+1] )

static time_t init_timeout = 10;	/* normally set by caller */
static ringsm_cback success_callback = NULL;
static alarm_cback alarm_callback = NULL;
static log_cback log_cb = NULL;
static time_t time_lastring = 0;
static int doing_init_timeout = TRUE;

/* dynamic array of sequences */
static struct s_ringsm_seq *sequence = NULL;
static int num_seqs = 0;

/* the sequence waiting for final timeout (NULL => none waiting) */
static struct s_ringsm_seq *timeout_seq = NULL;

/* the max time waiting sequence (NULL => none waiting) */
static struct s_ringsm_seq *max_wait_seq = NULL;


static void *
xrealloc(void *p, int newsize)
{
	if (!p)
		return malloc(newsize);
	return realloc(p, newsize);
}

static void
schedule_final_timeout(struct s_ringsm_seq *seq)
{
	time_t secs_until_run;

	seq->active = FALSE;	/* timeouts can only be last sequence events */
	/* schedule it if none already or one before but with later time */
	secs_until_run = ringsm_ev_now(seq).min;
	if (NULL == timeout_seq || 
	    ringsm_ev_now(timeout_seq).min > secs_until_run) {
		timeout_seq = seq;	/* keep a note of it */
		(*log_cb)(10, "Scheduling T in %ld secs", secs_until_run);
		if (secs_until_run <= 0)
			secs_until_run = 1;
		(*alarm_callback)(secs_until_run);
	}
}

static inline int 
in_time(time_t diff, time_t t1, time_t t2)
{
	if (0 == diff)
		return TRUE;
	if (t2 < t1)
		return FALSE;
	return (diff <= t2 && diff >= t1);
}

/*
 * ringsm_reset -- reset the state machine
 *
 * - do_tmout: if TRUE, wait init_timeout before (re)starting the state machine
 */
void
ringsm_reset(int do_tmout)
{
	struct s_ringsm_seq *seq;

	(*alarm_callback)(0);		/* disable any pending alarms */
	for (seq = sequence; seq && (seq < num_seqs + sequence); seq++) {
		seq->ev_now = -1;
		seq->active = TRUE;
	}
	if (do_tmout) {
		doing_init_timeout = TRUE;
		(*alarm_callback)(init_timeout);
	}
	timeout_seq = max_wait_seq = NULL;
	time_lastring = 0;
	DBG((*log_cb)(10, "RESET. Wait %ld sec", do_tmout ? init_timeout : 0));
}

static void
seq_completed(struct s_ringsm_seq *seq)
{
	(*success_callback)(seq->cmd);
	ringsm_reset(TRUE);
}


static void
do_new_seq(void)
{
	sequence = xrealloc(sequence, (num_seqs+1) * sizeof(struct s_ringsm_seq));
	sequence[num_seqs].num_events = 0;
	sequence[num_seqs].ev_now = -1;
	sequence[num_seqs].cmd = NULL;
	sequence[num_seqs].event = NULL;
}

static void
do_new_event(enum event_type etype, int t1, int t2)
{
	int num;

	assert(num_seqs > 0);
	num = sequence[num_seqs-1].num_events;
	sequence[num_seqs-1].event = xrealloc(sequence[num_seqs-1].event, 
			(num + 1) * sizeof(struct s_ringsm_event));
	sequence[num_seqs-1].event[num].etype = etype;
	sequence[num_seqs-1].event[num].min = t1;
	sequence[num_seqs-1].event[num].max = t2;
	sequence[num_seqs-1].num_events++;
}

static void
eat_wspace(char **p)
{
	while (**p && (**p == ' ' || **p == '\t'))
		(*p)++;
}

static int
parse_range(char **p, int *t1, int *t2)
{
	char *start;

	start = *p;
	while (isdigit(**p))
		(*p)++;
	if (*p > start)
		*t1 = *t2 = atoi(start);
	else
		return FALSE;
	if (**p == '-') {
		start = ++(*p);
		while (isdigit(**p))
			(*p)++;
		if (*p > start)
			*t2 = atoi(start);
		else
			return FALSE;
	} else if (**p != ':' && **p != ' ' && **p != '\t')
		return FALSE;
	eat_wspace(p);
	return TRUE;
}

/* 
 * simple parser for the configuration file - check for a few errors
 * static line_now has the line number where parser is now
 */
static line_now;

static int
conf_parse(FILE *f, argparse_cback argparse)
{
	char line[MAX_LINE_LEN], *p, *end;
	int len;
	int t1, t2;
	enum event_type etype;

	line_now = 0;
	while (fgets(line, MAX_LINE_LEN, f) != NULL) {
		line_now++;
		len = strlen(line);
		end = &line[--len];
		*end = '\0';
		if (len <= 0 || strspn(line,"# \t") == len)	/* empty */
			continue;
		p = line;
		eat_wspace(&p);
		if (*p == '#' || *p == '\n')	/* comment or empty */
			continue;
		eat_wspace(&p);
		if (*p == '-' && argparse) {	/* it is an option */
			char opt;

			opt = *++p;
			p++;
			eat_wspace(&p); /* p spans to end of line (\0) */ 
			(*argparse)(opt, p, -line_now);	/* callback */
			continue;
		}

		/* first must be R */
		if (*p++ != 'R')
			return FALSE;
		do_new_seq(); /* new sequence */
		num_seqs++;
		while (p < end) {
			eat_wspace(&p);
			if (isdigit(*p)) {
				if (!parse_range(&p, &t1, &t2))
					return(FALSE);
			} else
				return FALSE;
			eat_wspace(&p);
			if (*p == 'R') {
				etype = ev_RING;
				p++;
			} else if (*p == ':') {
				do_new_event(ev_FINALTIMEOUT, t1, t2); /* new event */
				p++;
				eat_wspace(&p);
				sequence[num_seqs-1].cmd = (char *) strdup(p);
				break;
			} else
				return FALSE;
			do_new_event(etype, t1, t2); /* new event */
		}
	}
	return TRUE;
}

/*
 * RET: -line_number where the parse error appears
 */
int
ringsm_parse(char * config_file, argparse_cback argparse)
{
	FILE *f;

	f = fopen(config_file, "r");	/* config_file MUST be readable */
	if (!conf_parse(f, argparse)) {
		fclose(f);
		ringsm_close();
		return -line_now;
	}
	fclose(f);
	ringsm_dump();
	return 0;
}

/*
 * f_callback: function to call when a sequence reached completion
 * f_alarm: c/back alarm function: must have the nehaviour of alarm(2) !sysdep!
 */
void
ringsm_init(time_t timeout, ringsm_cback f_callback, alarm_cback f_alarm, log_cback f_log)
{
	init_timeout = timeout;
	success_callback = f_callback;
	alarm_callback = f_alarm;
	log_cb = f_log;
	ringsm_reset(TRUE);
}

void
ringsm_close(void)
{
	struct s_ringsm_seq *seq;

	for (seq = sequence; seq && (seq < num_seqs + sequence); seq++) {
		if (seq->cmd)
			free(seq->cmd);
		if (seq->num_events > 0 && seq->event)
			free(seq->event);
	}
	free(sequence);
	sequence = NULL;
	num_seqs = 0;
}

/* for debugging */
void
ringsm_dump(void)
{
#ifdef RINGSM_DUMP
	struct s_ringsm_seq *seq;
	struct s_ringsm_event *ev;

	fprintf(stderr, "=====\n");
	for (seq = sequence; seq && (seq < num_seqs + sequence); seq++) {
		fprintf(stderr, "SEQ: num_evs=%d, active=%c, ev_now=%d, cmd=%s\n",
		 seq->num_events, seq->active? 'Y' : 'N', seq->ev_now, seq->cmd);
		fprintf(stderr, "  Events:\n");
		for (ev = seq->event; ev && (ev <  seq->num_events + seq->event); ev++) {
			fprintf(stderr, "\ttype=%c min=%ld, max=%ld\n",
			(ev->etype==ev_RING) ? 'R' : 'E', ev->min, ev->max);
		}
	}
#endif /* RINGSM_DUMP */
}

/*
 * ringsm_process_timeout -- timeout (alarm) handler
 */
void
ringsm_process_timeout(void)
{
	(*alarm_callback)(0);
	if (doing_init_timeout) {	/* initial timeout */
		DBG((*log_cb)(10, "ALARM init"));
		doing_init_timeout = FALSE;
		return;
	}
	if (max_wait_seq) {
		DBG((*log_cb)(10, "ALARM max wait %ld", ringsm_ev_now(max_wait_seq).max));
		ringsm_reset(TRUE);
		max_wait_seq = NULL;
		return;
	}
	DBG((*log_cb)(10, "ALARM final seq event timeout"));
	/* it is a final timeout of a sequence */
	assert(timeout_seq != NULL);	/* paranoia: check if spurious */
	seq_completed(timeout_seq);
	timeout_seq = NULL;
}

/*
 * ringsm_process_a_ring -- got a RING; recalculate our state
 * could do with building a proper lexcal-analyser here but it might
 * be overkill; this is less code and works fast
 */
int
ringsm_process_a_ring(void)
{
	time_t diff;
	struct s_ringsm_seq *seq;
	int one_was_active = FALSE;
	time_t t1, t2, max_wait;
	struct s_ringsm_seq *max_wseq_now;

	if (doing_init_timeout)	/* wait for initial timeout */
		return FALSE;

	/* back to rings; cancel any pending final-timeout (or maxwait) event */
	(*alarm_callback)(0);
	timeout_seq = NULL;

	if (0 == time_lastring)
		diff = 0;
	else
		diff = time(NULL) - time_lastring;
	DBG((*log_cb)(10, "time diff: %ld", diff));

	time_lastring = time(NULL);
	max_wait = 0;	/* max time to wait before resetting */
	max_wait_seq = max_wseq_now = NULL;
	for (seq = sequence; seq < num_seqs + sequence; seq++) {
		if (!seq->active)
			continue;
		t1 = t2 = 0;
		if (seq->ev_now >= 0) {		/* not the 1st ring */
			t1 = ringsm_ev_now(seq).min;
			t2 = ringsm_ev_now(seq).max;
		}
		if (!in_time(diff, t1, t2))
			seq->active = FALSE;
		else {	/* ring in proper timing for this seq */
			one_was_active = TRUE;
			DBG((*log_cb)(10, "seq %d: %ld IN [%ld->%ld]", seq-sequence, diff, t1, t2));
#if 0
			if (seq->ev_now >= 0 &&
			    seq->ev_now == seq->num_events - 1)
				seq_completed(seq);
#endif
			seq->ev_now++;
			/* next state is final timeout; run it */
			if (seq->ev_now < seq->num_events &&
			    ringsm_ev_now(seq).etype == ev_FINALTIMEOUT)
				schedule_final_timeout(seq);
			else if (max_wait < ringsm_ev_now(seq).max) {
				max_wait = ringsm_ev_now(seq).max;
				max_wseq_now = seq;
			}
		}
	}
	/* no sequence completed; reset state machine (wait init_timeout) */
	if (!one_was_active) {
                /* reset state machine */
                ringsm_reset(TRUE);
		return TRUE;
	}
	#if 1
	/* wait to the max time of all currently active events (if no final) */
	if (NULL == timeout_seq && 0 != max_wait) {
		DBG((*log_cb)(100, "Scheduling max wait in %ld secs", max_wait));
		max_wait_seq = max_wseq_now;
		(*alarm_callback)(max_wait);
	}
	#endif
	ringsm_dump();
	return TRUE;
}

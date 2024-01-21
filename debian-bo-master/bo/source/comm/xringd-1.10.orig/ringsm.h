/*
 * File:	ringsm.h
 *
 * Synopsis:	ring state machine
 *
 * System:	unix
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
 * $Id: ringsm.h,v 1.5 1996/02/17 15:50:07 ah Exp ah $
 */

#ifndef __RINGSM_INCLUDED
#define __RINGSM_INCLUDED

#ifndef TRUE
#define TRUE (1==1)
#define FALSE (!TRUE)
#endif

/* called when a sequence reaches success */
typedef void (*ringsm_cback) (char *seq_cmd);
typedef int (*argparse_cback) (char opt, char *s, int line);
/* called when an alarm needs to be scheduled for ringsm */
typedef unsigned int (*alarm_cback) (unsigned int);
typedef void (*log_cback) (int level, char *fmt, ...);

int ringsm_parse(char *config_file, argparse_cback argparse);
void ringsm_init(time_t timeout, ringsm_cback f_callback, alarm_cback f_alarm, log_cback f_log);
void ringsm_reset(int do_tmout);
void ringsm_close(void);
void ringsm_dump(void);
void ringsm_process_timeout(void);
int ringsm_process_a_ring(void);

#endif /* __RINGSM_INCLUDED */

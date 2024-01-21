/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	WRAPPER_H
#define	WRAPPER_H

extern	void		do_init_modules(void);

extern	const char *	progname;
extern	int		mercury_argc;
extern	char **		mercury_argv;
extern	int		mercury_exit_status;

extern	unsigned	heap_size;
extern	unsigned	detstack_size;
extern	unsigned	nondstack_size;

extern	unsigned	heap_zone_size;
extern	unsigned	detstack_zone_size;
extern	unsigned	nondstack_zone_size;

extern	unsigned	pcache_size;

extern	int		r1val;
extern	int		r2val;
extern	int		r3val;

extern	bool		check_space;

extern	int		time_at_start;
extern	int		time_at_last_stat;

#endif

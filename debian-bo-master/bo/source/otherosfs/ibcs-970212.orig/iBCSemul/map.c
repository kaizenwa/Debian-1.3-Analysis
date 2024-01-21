/*
 *  linux/ibcs/map.c
 *
 *  Copyright (C) 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: map.c,v 1.10 1996/05/31 10:39:27 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/map.c,v $
 */
#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <linux/sched.h>
#include <linux/net.h>
#include <linux/socket.h>
#include <linux/signal.h>
#include <linux/errno.h>
#include <linux/personality.h>

#include <ibcs/socket.h>
#include <ibcs/map.h>

#ifndef NULL
#define NULL	((void *)0)
#endif


/* Error code maps. */
#include "maps/errmap.inc"

/* Signal maps - these are pulled in to emulate.c where they are need to
 * initialise the execution domain descriptors with 1.1 kernels.
 */
#ifndef INIT_MM
#include "maps/signal.inc"
#endif

/* Socket subsystem maps. */
#include "maps/sockaf.inc"		/* Address families/formats */
#include "maps/sockopt.inc"		/* Socket options */
#include "maps/socktype.inc"		/* Socket types */


long
map_bitvec(unsigned long vec, long map[])
{
	unsigned long newvec, m;
	int i;

	newvec = 0;
	for (m=1,i=1; i<=32; m<<=1,i++)
		if ((vec & m) && map[i] != -1)
			newvec |= (1 << map[i]);

	return newvec;
}


int
map_value(struct map_segment *m[], int val, int def) {
	int pers;
	struct map_segment *seg;

	pers = current->personality & PER_MASK;

	/* If no mapping exists in this personality just return the
	 * number we were given.
	 */
	if (!m[pers])
		return val;

	/* Search the map looking for a mapping for the given number. */
	for (seg=m[pers]; seg->start != -1; seg++) {
		if (seg->start <= val && val <= seg->end) {
			/* If the start and end are the same then this
			 * segment has one entry and the map is the value
			 * it maps to. Otherwise if we have a vector we
			 * pick out the relevant value, if we don't have
			 * a vector we give identity mapping.
			 */
			if (seg->start == seg->end)
				return (int)seg->map;
			else
				return (seg->map ? seg->map[val-seg->start] : val);
		}
	}

	/* Number isn't mapped. Returned the requested default. */
	return def;
}


/* Translate the errno numbers from linux to current personality.
 * This should be removed and all other sources changed to call the
 * map function above directly.
 */
int iABI_errors(int errno)
{
	return map_value(err_map, errno, 1);
}

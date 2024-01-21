/*
 *  linux/ibcs/map.h
 *
 *  Copyright (C) 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: map.h,v 1.4 1995/04/12 10:12:43 mike Exp $
 * $Source: /usr/CVS/ibcs/include/ibcs/map.h,v $
 */

struct map_segment {
	int start, end;
	unsigned char *map;
};


extern struct map_segment *af_map[];
extern struct map_segment *type_map[];
extern struct map_segment *sopt_map[];

extern long map_bitvec(unsigned long vec, long map[]);
extern int map_value(struct map_segment *m[], int val, int def);

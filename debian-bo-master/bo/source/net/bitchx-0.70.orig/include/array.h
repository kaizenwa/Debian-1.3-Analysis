/* 
 * IRC-II (C) 1990, 1995 Michael Sandroff, Matthew Green
 * This file (C) 1993, 1995 Aaron Gifford and Jeremy Nelson
 *
 * array.h -- header file for array.c
 * See the COPYRIGHT file for copyright information
 *
 */

#ifndef ARRAY_H
#define ARRAY_H

#include "irc_std.h"

extern char *function_indextoitem _((char *));
extern char *function_itemtoindex _((char *));
extern char *function_igetitem _((char *));
extern char *function_getitem _((char *));
extern char *function_setitem _((char *));
extern char *function_finditem _((char *));
extern char *function_matchitem _((char *));
extern char *function_rmatchitem _((char *));
extern char *function_getmatches _((char *));
extern char *function_getrmatches _((char *));
extern char *function_delitem _((char *));
extern char *function_numitems _((char *));
extern char *function_getarrays _((char *));
extern char *function_numarrays _((char *));
extern char *function_delarray _((char *));
extern char *function_ifinditem _((char *));
extern char *function_ifindfirst _((char *));

#endif

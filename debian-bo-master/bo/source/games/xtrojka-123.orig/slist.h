/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	12.iii.1996
 *	modified:
 *
 *	header file for slist.c
 */

#ifndef _slist_h_
#define _slist_h_

#include "scores.h"

/*
 *	function prototypes
 */
void init_slist_mgr(void);
void slist_intr(Widget, XtIntervalId *);
void copy_oldscores(void);
void compare_scores(void);
flag is_newscore(SCORES);
void reset_changed(void);
int cmp_score(SCORES, SCORES);
flag file_changed(char *);
void compose_score_string(char*, int, SCORES, char*, char*);

#endif /* _slist_h_ */

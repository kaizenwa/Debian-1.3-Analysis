/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	12.iii.1996
 *	modified:
 *
 *	header file for scores.c
 */

#ifndef _scores_h_
#define _scores_h_

#include "xtrojka.h"

/*
 *	lock file should be shared, if the highscore file is shared
 */
#define LOCKFILE	"/tmp/xtrojka.lock"


#define NUMSCORES 10

typedef struct {
	int user;
	char wizard;
	char host[20];
	unsigned long score;
	unsigned long date;
	int speed;
} SCORES;


#define NO_SCORE 0


/*
 *	function prototypes
 */
void init_scores(void);
void do_hiscores(void);
flag is_hiscore(void);
void shift_scores(void);
void read_scores(void);
void write_scores(void);
void create_scorefile(void);
void show_scores_offline(void);

#endif /* _scores_h_ */

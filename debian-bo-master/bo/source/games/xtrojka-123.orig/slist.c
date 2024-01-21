/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	27.xi.1995
 *	modified:
 *
 *	This module handles the live score overview
 */

#include "debug.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <signal.h>
#include <pwd.h>

#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <X11/StringDefs.h>

#include "slist.h"

#include "xtrojka.h"
#include "scores.h"
#include "sh_slist.h"


#define YPOS(y)	(((y)+1)*18)
#define XPOS 30
#define SCORE_STATUS_TIMEOUT	5000		/* millisecs */

extern Widget slist_screen;
extern XtAppContext app_context;
extern SCORES scores[NUMSCORES];
extern SCORES old_scores[NUMSCORES];
extern SCORES cur;

extern int errno;

extern int position;
extern GAME_STATE game_state;


flag changed[NUMSCORES];		/* index table of new scores */

time_t last_time;		/* time last changed of hiscores file */

XtIntervalId slist_clocker;


void init_slist_mgr(void)
{
	int pause = SCORE_STATUS_TIMEOUT;

	DEBUG("slist.c", "init_slist_mgr")

	slist_clocker =
		XtAppAddTimeOut(app_context, pause, 
			(XtTimerCallbackProc)slist_intr, 0);
	last_time = 0L;
}


void slist_intr(w, id)
Widget w;
XtIntervalId *id;
{
	int pause = SCORE_STATUS_TIMEOUT;

	DEBUG("slist.c", "slist_intr")

	slist_clocker=XtAppAddTimeOut(app_context, pause,
			(XtTimerCallbackProc)slist_intr, 0);

	if((game_state == st_idle))
		draw_slist(kUNFORCED);
}



void copy_oldscores(void)
{
	int i;

	DEBUG("slist.c", "copy_oldscores")

	reset_changed();
	
	if(position < NUMSCORES) {
		memcpy((char*)&cur, (char*)&scores[position], sizeof(cur));
	}

	/* make a backup of the old scores */
	for(i = 0; i < NUMSCORES; i++)
		old_scores[i] = scores[i];
}


void compare_scores(void)
{
/*
 *	Check out which score is new in the list
 *	by comparing the old scorelist with the new one
 */

	int i,j;

	DEBUG("slist.c", "compare_scores")

	position = NUMSCORES;

	/* now compare old scores with new scores */

	j = 0;
	for(i = 0; i < NUMSCORES; i++) {
		if(is_newscore(scores[i]))
			changed[i] = 1;
	}
	
	/*
	 *	set the current position in the highscore list to
	 *	that of the new score
	 */
	for(i = 0; i < NUMSCORES; i++) {
		if(!cmp_score(cur, scores[i])) {
			position = i;
			return;
		}
	}
}


flag is_newscore(s)
SCORES s;
/*
 *	see if a score entry is new, by checking it
 *	against all other entries.
 */
{
	int i;

	DEBUG("slist.c", "is_newscore")

	for(i = 0; i < NUMSCORES; i++)
		if(cmp_score(s, old_scores[i]) == 0)
			return 0;
	return 1;
}


void reset_changed(void)
{
	int i;

	DEBUG("slist.c", "reset_changed")

	/* clear changed index */
	for(i = 0; i < NUMSCORES; i++)
		changed[i] = 0;
}


int cmp_score(s1, s2)
SCORES s1, s2;
{
/*
 *	compare two score entries by looking at the _entire_ info.
 *	Two entries can have the same points, but might originate
 *	from two different players
 */
	DEBUG("slist.c", "cmp_score")

	return memcmp((char*)&s1, (char*)&s2, sizeof(s1));
}

	


flag file_changed(s)
char *s;
{
	struct stat info;

	DEBUG("slist.c", "file_changed")

	if(stat(s, &info) < 0) {
		fprintf(stderr,"%s '%s' (%d)\n", app_data.wstr_stat, 
						SCOREFILE, errno);
		return 0;
	}
	if(info.st_mtime != last_time) {
		last_time = info.st_mtime;
		return 1;
	}
	return 0;
}


void compose_score_string(s,pos,score, user, date)
char *s;
int pos;
SCORES score;
char *user;
char *date;
{
	DEBUG("slist.c", "compose_score_string")

	sprintf(s,"%2d %9ld %2d %10s@%-12s %s %c",
		pos+1, score.score, score.speed, user, score.host, 
		date, score.wizard ? '*' : ' ');
}




/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:
 *
 *	This module does atomic score handling
 */


#include "debug.h"
#include "tr_core.h"

#include "_strdefs.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>
#include <pwd.h>
#include <time.h>
#include "xtrojka.h"
#include "scores.h"
#include "slist.h"
#include "sh_slist.h"


SCORES scores[NUMSCORES];
SCORES old_scores[NUMSCORES];
SCORES cur;


extern flag is_wizard;
extern time_t last_time;

extern int errno;

int position;


void init_scores(void)
{
	DEBUG("scores.c", "init_scores")

	read_scores();
}


void do_hiscores(void)
{
	char hostname[20];

	DEBUG("scores.c", "do_hiscores")

	position = NUMSCORES+1;
	
	copy_oldscores();
	read_scores();

	/* see if there is a highscore */
	if(is_hiscore()) {

		shift_scores();
		
		gethostname(hostname,20);
		
		scores[position].score = tv_score;
		scores[position].date = time(0);
		scores[position].speed = tv_speed;
		scores[position].user = getuid();
		scores[position].wizard = is_wizard;
		strcpy(scores[position].host, hostname);
		memcpy((char*)&cur, (char*)&scores[position], sizeof(cur));

		write_scores();
		utime(SCOREFILE, NULL);
		sleep(1);
		last_time = 0;
		file_changed(SCOREFILE);
		
	} else {
		memset((char*)&cur, 0, sizeof(cur));
	}
	compare_scores();

	draw_slist(kFORCED);
}


flag is_hiscore(void)
{
	int i;

	DEBUG("scores.c", "is_hiscore")

	for(i = 0; i < NUMSCORES; i++)
		if(tv_score > scores[i].score) {
			position = i;
			return 1;
		}
	return 0;
}


void shift_scores(void)
{
	int i;

	DEBUG("scores.c", "shift_scores")

	for(i = NUMSCORES-1; i > position; i--)
		scores[i] = scores[i-1];
}


void read_scores(void)
{
	int i;
	int fd;

	DEBUG("scores.c", "read_scores")

	/* clear scores */
	for(i = 0; i < NUMSCORES; i++) {
		scores[i].score = NO_SCORE;
	}

	if((fd = open(SCOREFILE, O_RDONLY)) < 0) {
		create_scorefile();
		write_scores();
	} else 
	if(read(fd, scores, sizeof(scores)) < 0) 
		fprintf(stderr,"%s\n", STR_R_SCORES);

	if(fd >= 0)
		close(fd);
}



void write_scores(void)
{
	int fd;

	DEBUG("scores.c", "write_scores")

	DEBUG("scores.c", "write_scores A")
	if((fd = open(SCOREFILE, O_WRONLY)) < 0)  {
		fprintf(stderr,"%s (%s)\n", STR_O_SCORES, SCOREFILE);
	}

#ifdef LOCKING
	DEBUG("scores.c", "write_scores")
	if(lockf(fd, F_LOCK, sizeof(scores)) < 0) 
		fprintf(stderr,"%s (%d)\n", STR_L_SCORES, errno);
#endif
	DEBUG("scores.c", "write_scores B")
	if(write(fd, scores, sizeof(scores)) < 0)
		fprintf(stderr,"%s (%s)\n", STR_W_SCORES, SCOREFILE);

	if(fd >= 0)
		close(fd);
}


void create_scorefile(void)
{
	int fd;
	int oldumask = umask(0);

	DEBUG("scores.c", "create_scorefile")

	if((fd = creat(SCOREFILE, 0666)) < 0) 
		fprintf(stderr,"%s (%s)\n", STR_C_SCORES,
			SCOREFILE);
	
	umask(oldumask);

	if(fd >= 0)
		close(fd);
}


void show_scores_offline(void)
{
	int i;
	char date_st[30];
	char user_st[30];
	char string[100];
	struct passwd *pw;

	DEBUG("scores.c", "show_scores_offline")

	for(i = 0;  i < NUMSCORES; i++) {
		if(scores[i].score == 0)
			break;
		strcpy(date_st, ctime(&(scores[i].date)));
		date_st[strlen(date_st)-1] = '\0';
		pw = getpwuid(scores[i].user);
		if(pw)
			strcpy(user_st, pw->pw_name);
		else
			sprintf(user_st,"(uid=%d)",scores[i].user);

		compose_score_string(string,i,scores[i],user_st, date_st);
		fprintf(stderr,"%s\n", string);
	}

	putchar('\n');
}



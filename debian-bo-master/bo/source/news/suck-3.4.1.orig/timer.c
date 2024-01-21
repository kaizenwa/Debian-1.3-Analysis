#include <config.h>

#include <stdio.h>
#include "suck_config.h"
#include "timer.h"
#include "phrases.h"
#include "both.h"

#ifdef TIMEOUT
# if TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  if HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
# endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/*internal functions */
double get_elapsed(struct timeval *);

/*-----------------------------------------------------------*/
void TimerFunc(int which_function, long nradd, FILE *fpi) { 

	static struct timeval start;
	static long nrbytes = 0L;	/* just in case */
	char strbytes[32], strmins[32], strsecs[32], strbps[32];

	double elapsed, bps;
	long mins;		/* long so can get mins to exact decimal */

	switch(which_function) {
	  case TIMER_START:
		nrbytes = 0L;
		gettimeofday(&start, NULL);
		break;
	  case TIMER_ADDBYTES:
		nrbytes += nradd;
		break;
	  case TIMER_DISPLAY:
		if(nrbytes > 0) {
			elapsed = get_elapsed(&start);
			bps = (elapsed > 0.0) ? bps = nrbytes / elapsed : 0.0;
			sprintf(strbps,"%.1f", bps);
			print_phrases(fpi, timer_phrases[2], strbps, NULL);
		}
		break;
	  case TIMER_TIMEONLY:
		elapsed = get_elapsed(&start);
		mins = ((long) elapsed) / 60 ;		/* get minutes */
		elapsed -= (mins * 60);		/* subtract to get remainder */
		sprintf(strmins, "%ld", mins);
		sprintf(strsecs, "%.2f", elapsed);
		print_phrases(fpi, timer_phrases[0], strmins, strsecs, NULL);
		break;
	  case TIMER_TOTALS:
		elapsed = get_elapsed(&start);
		bps = (elapsed > 0.0 && nrbytes > 0) ? bps = nrbytes / elapsed : 0.0;
		mins = ((long) elapsed) / 60 ;		/* get minutes */
		elapsed -= (mins * 60);		/* subtract to get remainder */
		sprintf(strbytes, "%ld", nrbytes);
		sprintf(strmins, "%ld", mins);
		sprintf(strsecs, "%.2f", elapsed);
		sprintf(strbps, "%.1f", bps);
		print_phrases(fpi, timer_phrases[1], strbytes, strmins, strsecs, strbps, NULL);
		break;	
	  default:
		/* ignore invalid commands */
		break;
	}
	return;
}
/*-----------------------------------------------------------------------------------*/
double get_elapsed(struct timeval *start) {
	struct timeval curr;
	double elapsed;

	/* compute elapsed time, in seconds */
	gettimeofday(&curr, NULL);
	elapsed = curr.tv_sec - start->tv_sec;
	elapsed += (((double) (curr.tv_usec - start->tv_usec)) / 1000000.0);

	return elapsed;
}

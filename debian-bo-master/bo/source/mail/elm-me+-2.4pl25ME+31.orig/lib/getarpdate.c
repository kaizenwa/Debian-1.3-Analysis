static char rcsid[] = "@(#)$Id: getarpdate.c,v 5.10 1994/03/11 20:55:23 syd Exp $";

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 5.10 $   $State: Exp $
 *
 *			Copyright (c) 1988-1992 USENET Community Trust
 *			Copyright (c) 1986,1987 Dave Taylor
 *******************************************************************************
 * Bug reports, patches, comments, suggestions should be sent to:
 *
 *	Syd Weinstein, Elm Coordinator
 *	elm@DSI.COM			dsinc!elm
 *
 *******************************************************************************
 * $Log: getarpdate.c,v $
 * Revision 5.10  1994/03/11  20:55:23  syd
 * Replaced the get_tz_mins() routine in lib/get_tz.c with a portable
 * version that requires no configuration.  Rewrote the test case in
 * lib/get_tz.c, and also wrote in support to simplify configuration
 * tests.  Dropped TZMINS_USE_XXXX configuration parameter.  Simplified
 * timezone determination in Configure.  The new get_tz_mins() introduces
 * two side effects.  First it steps on the static data returned by
 * localtime().  Second, it uses a routine in lib/date_util.c, which
 * requires debug initializations.  Rewrote get_arpa_date() to work around
 * the first issue.  Rather than doing the same for lib/strftime.c, just
 * removed the timezone stuff with a conditional compile.  Elm does not
 * use it.  Added debugging initializations to util/fastmail.c.
 * From: chip@chinacat.unicom.com (Chip Rosenthal)
 *
 * Revision 5.9  1993/08/03  19:17:33  syd
 * Implement new timezone handling.  New file lib/get_tz.c with new timezone
 * routines.  Added new TZMINS_USE_xxxxxx and TZNAME_USE_xxxxxx configuration
 * definitions.  Obsoleted TZNAME, ALTCHECK, and TZ_MINUTESWEST configuration
 * definitions.  Updated Configure.  Modified lib/getarpdate.c and
 * lib/strftime.c to use new timezone routines.
 *
 * From: chip@chinacat.unicom.com (Chip Rosenthal)
 *
 * Revision 5.8  1993/05/08  19:22:46  syd
 * On the DEC Alpha, OSF/1 the following change made things happy.
 * From: dave@opus.csd.uwm.edu (Dave Rasmussen)
 *
 * Revision 5.7  1993/04/21  01:45:39  syd
 * Try and get getarpdate to work on AIX
 * From: "William F. Pemberton" <wfp5p@holmes.acc.virginia.edu>
 *
 * Revision 5.6  1993/04/16  03:42:38  syd
 * As per "William F. Pemberton" <wfp5p@holmes.acc.virginia.edu>
 * IBMs have the date already adjusted for dst in the min decode
 * From: Syd
 *
 * Revision 5.5  1992/12/12  01:29:26  syd
 * Fix double inclusion of sys/types.h
 * From: Tom Moore <tmoore@wnas.DaytonOH.NCR.COM>
 *
 * Revision 5.4  1992/11/15  02:18:15  syd
 * Change most of the rest of the BSDs to TZNAME
 * From: Syd
 *
 * Revision 5.3  1992/11/15  02:10:58  syd
 * change tzname ifdef from ndefBSD to ifdef TZNAME on its own
 * configure variable
 * From: Syd
 *
 * Revision 5.2  1992/11/07  19:27:30  syd
 * Symbol change for AIX370
 * From: uri@watson.ibm.com
 *
 * Revision 5.1  1992/10/03  22:41:36  syd
 * Initial checkin as of 2.4 Release at PL0
 *
 *
 ******************************************************************************/

#include "headers.h"

static char *arpa_dayname[] = { "Sun", "Mon", "Tue", "Wed", "Thu",
		  "Fri", "Sat", "" };

static char *arpa_monname[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
		  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", ""};

extern int get_tz_mins();
extern char *get_tz_name();

char * get_arpa_date()
{
	/** returns an ARPA standard date.  The format for the date
	    according to DARPA document RFC-822 is exemplified by;

	       	      Mon, 12 Aug 85 6:29:08 MST

	**/

	static char buffer[SLEN];	/* static character buffer       */
	time_t	  curr_time;		/* time in seconds....		 */
	struct tm curr_tm;		/* Time structure, see CTIME(3C) */
	long      tzmin;		/* number of minutes off gmt 	 */
	int	  tzsign;		/* + or - gmt 			 */
	int	  year;			/* current year - with century	 */

	/*
	 * The get_tz_mins() routine steps on the static data returned
	 * by localtime(), so we need to save off the value obtained here.
	 */
	(void) time(&curr_time);
	curr_tm = *localtime(&curr_time);

	if ((year = curr_tm.tm_year) < 100)
		year += 1900;

	if ((tzmin = -get_tz_mins()) >= 0) {
		tzsign = '+';
	} else {
		tzsign = '-';
		tzmin = -tzmin;
	}

	sprintf(buffer, "%s, %d %s %d %02d:%02d:%02d %c%02d%02d (%s)",
	  arpa_dayname[curr_tm.tm_wday],
	  curr_tm.tm_mday, arpa_monname[curr_tm.tm_mon], year,
	  curr_tm.tm_hour, curr_tm.tm_min, curr_tm.tm_sec,
	  tzsign, tzmin / 60, tzmin % 60, get_tz_name(&curr_tm));
	
	return buffer;
}


#ifdef _TEST
int debug = 1;
FILE *debugfile = stderr;
main()
{
	printf("system(\"date\") says:   ");
	fflush(stdout);
	system("date");
	fflush(stdout);
	printf("get_arpa_date() says:  %s\n", get_arpa_date());
	exit(0);
}
#endif


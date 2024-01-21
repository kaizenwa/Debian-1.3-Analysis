/* Copyright (C) 1989, 1992, 1993, 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gp_dvx.c */
/* Desqview/X-specific routines for Ghostscript */
#include "string_.h"
#include "gx.h"
#include "gsexit.h"
#include "gp.h"
#include "time_.h"

/* Do platform-dependent initialization. */
void
gp_init(void)
{
}

/* Do platform-dependent cleanup. */
void
gp_exit(int exit_status, int code)
{
}

/* ------ Miscellaneous ------ */

/* Get the string corresponding to an OS error number. */
/* All reasonable compilers support it. */
const char *
gp_strerror(int errnum)
{	return strerror(errnum);
}

/* ------ Date and time ------ */

/* Read the current date (in days since Jan. 1, 1980) */
/* and time (in milliseconds since midnight). */
void
gp_get_clock(long *pdt)
{	long secs_since_1980;
	struct timeval tp;
	struct timezone tzp;
	time_t tsec;
	struct tm *tm, *localtime();

	if ( gettimeofday(&tp, &tzp) == -1 )
	   {	lprintf("Ghostscript: gettimeofday failed!\n");
		gs_exit(1);
	   }

	/* tp.tv_sec is #secs since Jan 1, 1970 */

	/* subtract off number of seconds in 10 years */
	/* leap seconds are not accounted for */
	secs_since_1980 = tp.tv_sec - (long)(60 * 60 * 24 * 365.25 * 10);
 
	/* adjust for timezone */
	secs_since_1980 -= (tzp.tz_minuteswest * 60);

	/* adjust for daylight savings time - assume dst offset is 1 hour */
	tsec = tp.tv_sec;
	tm = localtime(&tsec);
	if ( tm->tm_isdst )
		secs_since_1980 += (60 * 60);

	/* divide secs by #secs/day to get #days (integer division truncates) */
	pdt[0] = secs_since_1980 / (60 * 60 * 24);
	/* modulo + microsecs/1000 gives number of millisecs since midnight */
	pdt[1] = (secs_since_1980 % (60 * 60 * 24)) * 1000;
	/* Some Unix systems (e.g., Interactive 3.2 r3.0) return garbage */
	/* in tp.tv_usec.  Try to filter out the worst of it here. */
	if ( tp.tv_usec >= 0 && tp.tv_usec < 1000000 )
		pdt[1] += tp.tv_usec / 1000;

#ifdef DEBUG_CLOCK
	printf("tp.tv_sec = %d  tp.tv_usec = %d  pdt[0] = %ld  pdt[1] = %ld\n",
		tp.tv_sec, tp.tv_usec, pdt[0], pdt[1]);
#endif
}

/* ------ Printer accessing ------ */

/* Open a connection to a printer.  A null file name means use the */
/* standard printer connected to the machine, if any. */
/* Return NULL if the connection could not be opened. */
extern void gp_set_printer_binary(P2(int, int));
FILE *
gp_open_printer(char *fname, int binary_mode)
{       if ( strlen(fname) == 0 || !strcmp(fname, "PRN") )
        {       if ( binary_mode )
		  gp_set_printer_binary(fileno(stdprn), 1);
		stdprn->_flag = _IOWRT;	/* Make stdprn buffered to improve performance */
                return stdprn;
        }
        else
                return fopen(fname, (binary_mode ? "wb" : "w"));
}

/* Close the connection to the printer. */
void
gp_close_printer(FILE *pfile, const char *fname)
{       if ( pfile == stdprn )
	  fflush(pfile);
	else
	  fclose(pfile);
}

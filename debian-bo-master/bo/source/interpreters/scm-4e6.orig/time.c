/* Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */

/* "time.c" functions dealing with time.
   Author: Aubrey Jaffer */

#include "scm.h"

#ifdef HAVE_CONFIG_H

# ifndef HAVE_FTIME
#  define LACK_FTIME
# endif
# ifndef HAVE_TIMES
#  define LACK_TIMES
# endif
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif
# ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   ifdef HAVE_TIME_H
#    include <time.h>
#   endif
#  endif
# endif
# ifdef HAVE_SYS_TIMES_H
#  include <sys/times.h>
# else
#  ifdef HAVE_SYS_TIMEB_H
#   include <sys/timeb.h>
#  endif
# endif
# ifdef HAVE_FTIME
#  ifdef unix
#   ifndef GO32
#    include <sys/timeb.h>
#   endif
#  endif
# endif

#else

# ifdef STDC_HEADERS
#  include <time.h>
#  ifdef M_SYSV
#   include <sys/types.h>
#   include <sys/times.h>
#  endif
#  ifdef sun
#   include <sys/types.h>
#   include <sys/times.h>
#  endif
#  ifdef ultrix
#   include <sys/types.h>
#   include <sys/times.h>
#  endif
#  ifdef nosve
#   include <sys/types.h>
#   include <sys/times.h>
#  endif
#  ifdef _UNICOS
#   include <sys/types.h>
#   include <sys/times.h>
#  endif
#  ifdef __IBMC__
#   include <sys/timeb.h>
#  endif
# else
#  ifdef SVR2
#   include <time.h>
#  else
#   ifndef ARM_ULIB
#    include <sys/time.h>
#   else
#    include <time.h>
#   endif
#  endif
#  include <sys/types.h>

#  ifndef ARM_ULIB
#   include <sys/times.h>
#  else
#   include <time.h>
#  endif

# endif

/* Define this if your system lacks ftime(). */
/* #define LACK_FTIME */
/* Define this if your system lacks times(). */
/* #define LACK_TIMES */

# ifdef __TURBOC__
#  define LACK_TIMES
# endif
# if (__TURBOC__==1) /* Needed for TURBOC V1.0 */
#  define LACK_FTIME
#  undef MSDOS
# endif
# ifdef __HIGHC__
#  define LACK_TIMES
# endif
# ifdef THINK_C
#  define LACK_FTIME
#  define LACK_TIMES
#  define CLK_TCK 60
# endif
# ifdef SVR2
#  define LACK_FTIME
# endif
# ifdef SVR4
#  define LACK_FTIME
# endif
# ifdef __svr4__
#  define LACK_FTIME
# endif
# ifdef nosve
#  define LACK_FTIME
# endif
# ifdef GO32
#  define LACK_FTIME
#  define LACK_TIMES
# endif
# ifdef atarist
#  define LACK_FTIME
#  define LACK_TIMES
# endif
# ifdef ARM_ULIB
#  define LACK_FTIME
#  define LACK_TIMES
# endif
# ifdef _DCC
#  define LACK_FTIME
# endif
# ifdef MSDOS
#  ifndef GO32
#   include <sys/types.h>
#   include <sys/timeb.h>
#  endif
# endif
# ifdef _UNICOS
#  define LACK_FTIME
# endif

# ifndef LACK_FTIME
#  ifdef unix
#   ifndef GO32
#    include <sys/timeb.h>
#   endif
#  endif
# endif

# ifdef __EMX__
#  define LACK_TIMES
#  include <sys/types.h>
#  include <sys/timeb.h>
# endif

# ifdef MWC
#  include <time.h>
#  include <sys/timeb.h>
# endif

# ifdef ARM_ULIB
#  include <sys/types.h>
#  include <time.h>
# endif

#endif /* HAVE_CONFIG_H */

#ifdef vms
# define LACK_TIMES
# define LACK_FTIME
#endif

#ifdef CLK_TCK
# define CLKTCK CLK_TCK
# ifdef CLOCKS_PER_SEC
#  ifdef unix
#   ifndef ARM_ULIB
#    include <sys/times.h>
#   endif
#   define LACK_CLOCK
    /* This is because clock() might be POSIX rather than ANSI.
       This occurs on HP-UX machines */
#  endif
# endif
#else
# ifdef CLOCKS_PER_SEC
#  define CLKTCK CLOCKS_PER_SEC
# else
#  define LACK_CLOCK
#  ifdef AMIGA
#   include <stddef.h>
#   define LACK_TIMES
#   define LACK_FTIME
#   define CLKTCK 1000
#  else
#   define CLKTCK 60
#  endif
# endif
#endif

#ifdef __STDC__
# define timet time_t
#else
# define timet long
#endif

#ifdef LACK_TIMES
# ifdef LACK_CLOCK
#  ifdef AMIGA
/* From: "Fred Bayer" <bayerf@lan.informatik.tu-muenchen.de> */
#   ifdef AZTEC_C		/* AZTEC_C */
#    include <devices/timer.h>
static long mytime()
{
	long sec, mic, mili = 0;
	struct timerequest *timermsg;
	struct MsgPort *timerport;
	if(!(timerport = (struct MsgPort *)CreatePort(0, 0))){
	lputs("No mem for port.\n", cur_errp);
		return mili;
	}
	if(!(timermsg = (struct timerequest *)
		 CreateExtIO(timerport, sizeof(struct timerequest)))){
		lputs("No mem for timerequest.\n", cur_errp);
		DeletePort(timermsg->tr_node.io_Message.mn_ReplyPort);
	return mili;
	}
	if(!(OpenDevice(TIMERNAME, UNIT_MICROHZ, timermsg, 0))){
		timermsg->tr_node.io_Command = TR_GETSYSTIME;
		timermsg->tr_node.io_Flags = 0;
		DoIO(timermsg);
		sec = timermsg->tr_time.tv_secs;
		mic = timermsg->tr_time.tv_micro;
		mili = sec*1000+mic/1000;
		CloseDevice(timermsg);
	}
	else lputs("No Timer available.\n", cur_errp);
	DeletePort(timermsg->tr_node.io_Message.mn_ReplyPort);
	DeleteExtIO(timermsg);
	return mili ;
}
#   else			/* this is for SAS/C */
static long mytime()
{
   unsigned int cl[2];
   timer(cl);
   return(cl[0]*1000+cl[1]/1000);
}
#   endif /* AZTEC_C */
#  else /* AMIGA */
#   define mytime() ((time((timet*)0) - your_base) * CLKTCK)
#  endif /* AMIGA */
# else /* LACK_CLOCK */
#  define mytime clock
# endif /* LACK_CLOCK */
#else /* LACK_TIMES */
static long mytime()
{
  struct tms time_buffer;
  times(&time_buffer);
  return time_buffer.tms_utime + time_buffer.tms_stime;
}
#endif /* LACK_TIMES */

#ifdef LACK_FTIME
# ifdef AMIGA
SCM your_time()
{
  return MAKINUM(mytime());
}
# else
timet your_base = 0;
SCM your_time()
{
	return MAKINUM((time((timet*)0) - your_base) * (int)CLKTCK);
}
# endif /* AMIGA */
#else /* LACK_FTIME */
struct timeb your_base = {0};
SCM your_time()
{
	struct timeb time_buffer;
	long tmp;
	ftime(&time_buffer);
	time_buffer.time -= your_base.time;
	tmp = time_buffer.millitm - your_base.millitm;
	tmp = time_buffer.time*1000L + tmp;
	tmp *= CLKTCK;
	tmp /= 1000;
	return MAKINUM(tmp);
}
#endif /* LACK_FTIME */

long my_base = 0;
SCM my_time()
{
  return MAKINUM(mytime()-my_base);
}

SCM curtime()
{
  timet timv = time((timet*)0);
  SCM ans;
#ifndef _DCC
# ifdef STDC_HEADERS
#  if (__TURBOC__ > 0x201)
  timv = mktime(gmtime(&timv));
#  endif
# endif
#endif
  ans = ulong2num(timv);
  return BOOL_F==ans ? MAKINUM(timv) : ans;
}

long time_in_msec(x)
     long x;
{
  if (CLKTCK==60) return (x*50)/3;
  else
    return (CLKTCK < 1000 ? x*(1000L/(long)CLKTCK) : (x*1000L)/(long)CLKTCK);
}

static iproc subr0s[] = {
	{"get-internal-run-time", my_time},
	{"get-internal-real-time", your_time},
	{"current-time", curtime},
	{0, 0}};

void init_time()
{
	sysintern("internal-time-units-per-second",
		  MAKINUM((long)CLKTCK));
#ifdef LACK_FTIME
# ifndef AMIGA
	if (!your_base) time(&your_base);
# endif
#else
	if (!your_base.time) ftime(&your_base);
#endif
	if (!my_base) my_base = mytime();
	init_iprocs(subr0s, tc7_subr_0);
}

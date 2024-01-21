#include "../h/gsupport.h"

/*
 * The following code is operating-system dependent [@time.01].  Include files
 *  that are system-dependent.
 */

#if PORT
   /* probably needs something */
Deliberate Syntax Error
#endif					/* PORT */

#if AMIGA
#include "time.h"
#endif					/* AMIGA */

#if ATARI_ST
   /* nothing is needed */
#endif					/* ATARI_ST */

#if ARM || MVS || VM
#include <time.h>
#endif					/* ARM || MVS || ... */

#if MACINTOSH
#if LSC
#include <time.h>
#endif					/* LSC */
#if MPW
#include <types.h>
#include "time.h"
#include <OSUtils.h>
#include <Events.h>
#endif					/* MPW */
#if THINK_C
#include <time.h>
#endif					/* THINK_C */
#endif					/* MACINTOSH */

#if MSDOS
#include <time.h>
#if MICROSOFT
#include <sys/types.h>
#endif					/* MICROSOFT */
#if ZTC_386 || SCCX_CX
#include <dos.h>
#endif					/* ZTC_386 || SCCX_MX */
#endif					/* MSDOS */

#if OS2
#include <time.h>
#include <sys/types.h>
#endif					/* OS2 */

#if UNIX
#ifdef CRAY
#define word	fubar_word
#include <sys/types.h>
#include <sys/times.h>
#undef word
#else					/* CRAY */
#include <sys/types.h>
#include <sys/times.h>
#endif					/* CRAY */
#include SysTime
#endif					/* UNIX */

#if VMS
#include <types.h>
#include <time.h>
struct tms {
    time_t    tms_utime;	/* user time */
    time_t    tms_stime;	/* system time */
    time_t    tms_cutime;	/* user time, children */
    time_t    tms_cstime;	/* system time, children */
};
#endif					/* VMS */

/*
 * End of operating-system specific code.
 */

int first_time = 1;

static char *day[] = {
   "Sunday", "Monday", "Tuesday", "Wednesday",
   "Thursday", "Friday", "Saturday"
   };

static char *month[] = {
   "January", "February", "March", "April", "May", "June",
   "July", "August", "September", "October", "November", "December"
   };


/*
 * getitime - fill in a "struct cal_time" with information about the current
 *  time and date.
 */
novalue getitime(ct)
struct cal_time *ct;
   {

/*
 * The following code is operating-system dependent [@time.02]. Declarations
 *  for getting time.
 */

#if PORT
   long time();
   long xclock;
Deliberate Syntax Error
#endif					/* PORT */

#if AMIGA || OS2 || UNIX || SCCX_MX
#ifdef StandardLib
   time_t time();
   time_t xclock;
#else					/* StandardLib */
   long time();
   long xclock;
#endif					/* StandardLib */
#endif					/* AMIGA || OS2 || UNIX || SCCX_MX */

#if ARM || VMS
   time_t xclock;
#endif					/* ARM || VMS */

#if ATARI_ST
   struct tm {
       short tm_year;
       short tm_mon;
       short tm_wday;
       short tm_mday;
       short tm_hour;
       short tm_min;
       short tm_sec;
   };
   long xclock;
#endif					/* ATARI_ST */

#if MACINTOSH
#if LSC
   unsigned long xclock;
   unsigned long time();
#else					/* LSC */
   time_t xclock;
#endif					/* LSC */
#endif					/* MACINTOSH */

#if MSDOS
#if MICROSOFT || NT
   time_t time();
   long xclock;
#endif					/* MICROSOFT || NT */
#if WATCOM
   time_t time();
   time_t xclock;
#endif					/* WATCOM */
#if TURBO || INTEL_386 || BORLAND_286 || BORLAND_386
   long time();
   long xclock;
#endif					/* TURBO || ... */
#if HIGHC_386
   long time();
   time_t xclock;
#endif					/* HIGHC_386 */
#endif					/* MSDOS */

#if MVS || VM
   time_t xclock;
#endif					/* MVS || VM */

/*
 * End of operating-system specific code.
 */

   struct tm *tbuf, *localtime();
/*
 * The following code is operating-system dependent [@time.03]. Code for
 *  getting time.
 */

#if PORT
   time(&xclock);
   tbuf = localtime(&xclock);
Deliberate Syntax Error
#endif					/* PORT */

#if AMIGA || ARM || MACINTOSH || OS2 || UNIX || VMS || MVS || VM
   time(&xclock);
   tbuf = localtime(&xclock);
#endif					/* AMIGA || ARM || ... */

#if MSDOS
#if ZTC_386
   /*
    * Symantec has made a balls-up of localtime() in Zortech C++ 3.1 
    * so use MS-DOS calls
    */
   struct tm tbufa;
   struct dos_date_t d;
   struct dos_time_t t;

   tbuf = &tbufa;
   dos_getdate(&d);
   dos_gettime(&t);

   tbufa.tm_year = d.year - 1900;
   tbufa.tm_mon = d.month-1;
   tbufa.tm_mday = d.day;
   tbufa.tm_wday = d.dayofweek;
   tbufa.tm_hour = t.hour;
   tbufa.tm_min = t.minute;
   tbufa.tm_sec = t.second;
#else           			/* ZTC_386 */
   time(&xclock);
   tbuf = localtime(&xclock);
#endif           			/* ZTC_386 */
#endif					/* MSDOS */

#if ATARI_ST
    tbuf = localtime(&xclock);
#endif					/* ATARI_ST */

/*
 * End of operating-system specific code.
 */

   ct->year = 1900 + tbuf->tm_year;
   ct->month_no = tbuf->tm_mon+1;
   ct->month_nm = month[tbuf->tm_mon];
   ct->mday = tbuf->tm_mday;
   ct->wday = day[tbuf->tm_wday];
   ct->hour = tbuf->tm_hour;
   ct->minute = tbuf->tm_min;
   ct->second = tbuf->tm_sec;
   return;
   }

/*
 * getctime - fill a buffer with the "ctime" representation of the current
 *  time and date. The buffer must be at least 26 characters.
 */
novalue getctime(sbuf)
char *sbuf;
   {
   struct cal_time ct;

   getitime(&ct);
   sprintf(sbuf, "%.3s %.3s%3d %.2d:%.2d:%.2d %d\n", ct.wday, ct.month_nm,
      ct.mday, ct.hour, ct.minute, ct.second, ct.year);
   return;
   }

/*
 * millisec - returns execution time in milliseconds. Time is measured
 *  from the function's first call. The granularity of the time is
 *  generally more than one millisecond and on some systems it my only
 *  be accurate to the second.
 */
long millisec()
   {

/*
 * The following code is operating-system dependent [@time.04]. Declarations
 *   that are system-dependent.
 */

#if PORT
   static long starttime;
   long time();
Deliberate Syntax Error
#endif					/* PORT */

#if AMIGA || ATARI_ST
   static long starttime;
   long time();
#endif					/* AMIGA || ATARI_ST */

#if ARM
   static clock_t strtime;
#endif					/* ARM */


#if MACINTOSH
   static long starttime;
#endif					/* MACINTOSH */

#if MSDOS

#if MICROSOFT
   static long starttime;
   time_t time();
#endif					/* MICROSOFT */

#if WATCOM
   time_t time();
   static time_t starttime;
#endif					/* WATCOM */

#if ZTC_386 || NT
   static long starttime;
#endif					/* ZTC_386 || NT */

#if SCCX_MX
    static tile_t starttime;
#endif					/* SCCX_MX */

#if TURBO || INTEL_386 || BORLAND_286 || BORLAND_386
   static long starttime;
   long time();
#endif					/* TURBO || INTEL_386 ... */

#if HIGHC_386
   static clock_t hc_strtime;
#endif					/* HIGHC_386 */

#endif					/* MSDOS */

#if MVS || VM || OS2
   static clock_t starttime;
#endif                                  /* MVS || VM */

#if UNIX || VMS
   struct tms tp;
   static long starttime;
#endif					/* UNIX || VMS */

/*
 * End of operating-system specific code.
 */

   if (first_time) {
      first_time = 0;

/*
 * The following code is operating-system dependent [@time.05].  Get start
 *  time.
 */

#if PORT
      /* needs something */
Deliberate Syntax Error
#endif					/* PORT */

#if AMIGA || ATARI_ST
      time(&starttime);	/* note: this obtains time in various units */
#endif					/* AMIGA || ATARI_ST */

#if MSDOS
#if MICROSOFT || TURBO || WATCOM || SCCX_MX
      time(&starttime);	/* note: this obtains time in various units */
#endif					/* MICROSOFT || TURBO ... */
#if ZTC_386
      starttime = clock();
#endif					/* ZTC_386 */
#if INTEL_386 || NT || BORLAND_286 || BORLAND_386
      starttime = (long)clock();
#endif					/* INTEL_386 || NT ... */
#if HIGHC_386
      hc_strtime = clock();
#endif					/* HIGHC_386 */
#endif					/* MSDOS */

#if ARM
      strtime = clock();
#endif					/* ARM */


#if MACINTOSH
      starttime = TickCount();	/* 60 ticks / second */
#endif					/* MACINTOSH */

#if MVS || VM || OS2
      starttime = clock();		/* clock ticks */
#endif					/* MVS || VM || OS2 */

#if UNIX || VMS
      times(&tp);
      starttime = tp.tms_utime;
#endif					/* UNIX || VMS */

/*
 * End of operating-system specific code.
 */

      return 0L;
      }
   else {	/* not first time */
/*
 * The following code is operating-system dependent [@time.06].  Get time.
 */

#if PORT
   /* needs something */
Deliberate Syntax Error
#endif					/* PORT */

#if AMIGA
      return 1000 * (time(NULL) - starttime);
#endif					/* AMIGA */

#if MSDOS

#if HIGHC_386
      return (clock() - hc_strtime) * 10L;	/* thousandths */
#endif					/* HIGHC_386 */

#if INTEL_386 || NT || BORLAND_286 || BORLAND_386
      return ((long)clock() - starttime) * (1000L / CLOCKS_PER_SEC);
#endif					/* INTEL_386 || NT ... */

#if MICROSOFT || TURBO
      return 1000 * (time(NULL) - starttime);
#endif					/* MICROSOFT || TURBO */

#if ZTC_386
      return (1000/CLOCKS_PER_SEC) * (clock() - starttime);
#endif					/* ZTC_386 */

#if SCCX_MX
      return 10 * clock();
#endif					/* SCCX_MX */

#endif					/* MSDOS */

#if ARM
      /* thousandths; assumes CLOCKS_PER-SEC is 100 */
      return (clock() - strtime) * 10L;
#endif					/* ARM */

#if ATARI_ST
      return (time(NULL) - starttime) / 10;
#endif					/* ATARI_ST */

#if MACINTOSH
      return 1000 * ((extended)(TickCount() - starttime) / (extended)Hz);
#endif					/* MACINTOSH */

#if MVS || VM || OS2
      return (1000.0/CLOCKS_PER_SEC) * (clock() - starttime);
#endif					/* MVS || VM */

#if UNIX || VMS
      times(&tp);
      return 1000 * ((tp.tms_utime - starttime) / (double)Hz);
#endif					/* UNIX || VMS */

/*
 * End of operating-system specific code.
 */
      }
   }

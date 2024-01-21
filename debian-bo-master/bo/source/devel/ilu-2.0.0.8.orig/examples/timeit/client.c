/* $Id: client.c,v 1.13 1996/04/03 16:09:59 spreitze Exp $ */
/* Last edited by Mike Spreitzer April 2, 1996 9:43 pm PST */

#include <stdlib.h>		/* for atoi */
#include <stdio.h>

#ifdef WIN32

#include <sys/types.h>
#include <sys/timeb.h>
/* dll - to pick up gethostname */
#include <winsock.h>
#include "timeit.h"

int main(int ac, char **av)
{
  char hostname[1000], sid[1000];
  timeit_p handle;
  struct _timeb timebegin, timeend;
  char *tests[] = { "cardinal (C unsigned long)", "real (C double)", "ilu.CString (10 chars)" };
  ilu_CString retstr;
  ilu_cardinal i, repeat, j, testtype;
  double utime, stime, cutime, cstime;
  /* xxx dll note - currently don't know how to pick out user vs. system time on NT
  struct tms beginning, ending; */
  ILU_C_ENVIRONMENT e;

  timeit__Initialize( );

  if (ac < 3)
    {
      fprintf (stderr, "Usage:  client TEST { 1=cardinal, 2=real, 3=string } REPEATCOUNT [ HOSTNAME ]\n");
      exit(1);
    }
  testtype = atoi(av[1]);
  repeat = atoi(av[2]);
  if (testtype == 0 || testtype > 3 || repeat < 1)
    {
      fprintf (stderr, "error:  testtype of %ld, repeatcount of %ld specified.\n",
	       testtype, repeat);
      exit(1);
    }
  if (ac > 3)
    strcpy (hostname, av[3]);
  else
    gethostname(hostname, sizeof(hostname));
  sprintf (sid, "TimingTest.%s.parc.xerox.com", hostname);
  if ((handle = ILU_C_LookupObject (sid, "0", timeit_p__MSType)) == NULL)
    {
      fprintf (stderr, "error:  Can't obtain object <%s>\n", sid);
      exit(1);
    }

  _ftime(&timebegin);
  switch (testtype)
    {
    case 1:

      /* times(&beginning); */
      for (i = 0;  i < repeat;  i++)
	{
	  if (j = timeit_p_ping1 (handle, i, &e), e.returnCode != NULL)
	    {
	      fprintf (stderr, "Failure %p in call %lu to ping1\n", e.returnCode, i+1);
	      exit(2);
	    }
	}
      /* times(&ending); */
      break;

    case 2:

      /* times(&beginning); */
      for (i = 0;  i < repeat;  i++)
	{
	   utime = i;
	  if (stime = timeit_p_ping2 (handle, utime, &e), e.returnCode != NULL)
	    {
	      fprintf (stderr, "Failure %p in call %lu to ping2\n", e.returnCode, i+1);
	      exit(2);
	    }
	}
      /* times(&ending); */
      break;

      break;

    case 3:

      /* times(&beginning); */
      for (i = 0;  i < repeat;  i++)
	{
	  if (retstr = timeit_p_ping3 (handle,  "some string", &e), e.returnCode != NULL)
	    {
	      fprintf (stderr, "Failure %p in call %lu to ping3\n", e.returnCode, i+1);
	      exit(2);
	    }
	}
      /* times(&ending); */
      break;

      break;

    }
  _ftime(&timeend);

  /*
  utime = ((ending.tms_utime - beginning.tms_utime) * 1000) / (repeat * 60.0);
  stime = ((ending.tms_stime - beginning.tms_stime) * 1000) / (repeat * 60.0);
  cutime = ((ending.tms_cutime - beginning.tms_cutime) * 1000) / (repeat * 60.0);
  cstime = ((ending.tms_cstime - beginning.tms_cstime) * 1000) / (repeat * 60.0);
  */

  printf ("test \"%s\", repeat %lu:\n", tests[testtype - 1], repeat);
  printf ("  elapsed time, sec per call:  %.3f      total:  %.2f sec\n",
	  ((((double) timeend.time * 1000.0) + timeend.millitm) -
	   (((double) timebegin.time * 1000.0) + timebegin.millitm))
	  / (((double) repeat) * 1000.0),
	  ((((double) timeend.time * 1000.0) + timeend.millitm) -
	   (((double) timebegin.time * 1000.0) + timebegin.millitm)) / 1000.0);
  /*
  printf ("     user time:  %3lu - %3lu = %.3f millisec/call\n", ending.tms_utime, beginning.tms_utime, utime);
  printf ("   system time:  %3lu - %3lu = %.3f millisec/call\n", ending.tms_stime, beginning.tms_stime, stime);
  */

  exit(0);
}


#else /* not WIN32 */

#include <unistd.h>		/* for gethostname */
#include <math.h>		/* for sqrt */

#include <sys/time.h>
#include <sys/times.h>

#include "timeit.h"

#define ILU_TEST_DOMAIN	"parc.xerox.com"

typedef struct {
  double          minX, maxX, sumX, sumXX;
  unsigned        n;
}               StatSums;

static void AddStat(StatSums * ss, double x)
{
  if (ss->n == 0)
    ss->minX = ss->maxX = x;
  else {
    if (x > ss->maxX)
      ss->maxX = x;
    if (x < ss->minX)
      ss->minX = x;
  }
  ss->n += 1;
  ss->sumX += x;
  ss->sumXX += x * x;
  return;
}

static char    *
FmtStats(StatSums * ss)
{
  static char     buf[100];
  double          avg, stdev;
  if (ss->n > 0) {
    avg = ss->sumX / ss->n;
    if (ss->n > 1) {
      stdev = (ss->sumXX - ss->sumX * ss->sumX / ss->n) / (ss->n - 1);
      if (stdev >= 0.0) {
	stdev = sqrt(stdev);
	sprintf(buf, "%6.1f / %6.1f +- %6.1f / %6.1f",
		ss->minX, avg, stdev, ss->maxX);
      } else
	sprintf(buf, "%6.1f / %6.1f +- sqrt(%6.1f) / %6.1f",
		ss->minX, avg, stdev, ss->maxX);
    } else
      sprintf(buf, "%6.1f once", ss->sumX);
  } else
    sprintf(buf, "no samples");
  return buf;
}

static char    *progname = NULL;

static void
Usage()
{
  fprintf(stderr,
	  "Usage:  %s TEST OUTERCOUNT INNERCOUNT [ HOSTNAME ]\n",
	  progname);
  fprintf(stderr,
       "   or   %s 4 OUTERCOUNT PAGECOUNT PAGESIZE [ HOSTNAME ]\n",
	  progname);
  fprintf(stderr,
	  "For TEST: 1=cardinal, 2=real, 3=string, 4=docfetch\n");
  fprintf(stderr, "A test is repeated OUTERCOUNT*INNERCOUNT times,\n");
  fprintf(stderr, "with each block of INNERCOUNT timed.\n");
  fprintf(stderr, "INNERCOUNT=1 for test 4=docfetch.\n");
  exit(1);
}

static char    *tests[] = {"cardinal (C unsigned long)", "real (C double)", "ilu.CString", "Document"};
static char test_string[] = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\0";

int
main(int ac, char **av)
{
  char            hostname[1000], sid[1000];
  timeit_p        handle;
  ilu_CString     retstr;
  double          tfactor;
  CORBA_unsigned_long    i, outerCount, k, innerCount, j, testtype;
  CORBA_unsigned_long    pagecount = 0, pagesize = 0;
  timeit_pageSeq *doc;
  ILU_C_ENVIRONMENT e;
  struct tms      beginning, ending;
  struct timeval  timebegin, timeend;
  StatSums        tsums = {0}, usums = {0}, ssums = {0};

  timeit__Initialize();
  progname = av[0];
  if (ac < 4)
    Usage();
  testtype = atoi(av[1]);
  outerCount = atoi(av[2]);
  innerCount = (testtype == 4) ? 1 : atoi(av[3]);
  if (testtype < 1 || testtype > 5 || outerCount < 1 || innerCount < 1
      || (testtype == 4 && ac < 5))
    Usage();
  if ((testtype == 4 && ac > 5) || (testtype != 4 && ac > 4))
    strcpy(hostname, av[(testtype == 4) ? 5 : 4]);
  else
    gethostname(hostname, sizeof(hostname));
  if (testtype == 4) {
    pagecount = atoi(av[3]);
    pagesize = atoi(av[4]);
  }
  sprintf(sid, "TimingTest.%s.%s", hostname, ILU_TEST_DOMAIN);
  handle = ILU_C_LookupObject(sid, "0", timeit_p__MSType);
  if (handle == NULL) {
    fprintf(stderr, "error:  Can't obtain object <%s>\n", sid);
    exit(1);
  }
  tfactor = 1000.0 / (innerCount * 60.0);
  switch (testtype) {
  case 1:

    for (i = 0; i < outerCount; i++) {
      gettimeofday(&timebegin, NULL);
      times(&beginning);
      for (k = 0; k < innerCount; k++) {
	j = timeit_p_ping1(handle, i, &e);
	if (e.returnCode != NULL) {
	  fprintf(stderr, "Failure <%s> in call (%lu,%lu) to ping1\n",
		  e.returnCode, i, k);
	  exit(2);
	}
      }
      times(&ending);
      gettimeofday(&timeend, NULL);
      AddStat(&tsums,
	      (((timeend.tv_sec * 1.0E6 + timeend.tv_usec)
		- (timebegin.tv_sec * 1.0E6 + timebegin.tv_usec))
	       / (innerCount * 1000.0)));
      AddStat(&usums,
	      (ending.tms_utime - beginning.tms_utime) * tfactor);
      AddStat(&ssums,
	      (ending.tms_stime - beginning.tms_stime) * tfactor);
    }
    break;

  case 2:

    for (i = 0; i < outerCount; i++) {
      gettimeofday(&timebegin, NULL);
      times(&beginning);
      for (k = 0; k < innerCount; k++) {
	CORBA_double          x = i, y;
	y = timeit_p_ping2(handle, x, &e);
	if (e.returnCode != NULL) {
	  fprintf(stderr, "Failure <%s> in call (%lu,%lu) to ping2\n",
		  e.returnCode, i, k);
	  exit(2);
	}
      }
      times(&ending);
      gettimeofday(&timeend, NULL);
      AddStat(&tsums,
	      (((timeend.tv_sec * 1.0E6 + timeend.tv_usec)
		- (timebegin.tv_sec * 1.0E6 + timebegin.tv_usec))
	       / (innerCount * 1000.0)));
      AddStat(&usums,
	      (ending.tms_utime - beginning.tms_utime) * tfactor);
      AddStat(&ssums,
	      (ending.tms_stime - beginning.tms_stime) * tfactor);
    }
    break;

  case 3:

    for (i = 0; i < outerCount; i++) {
      gettimeofday(&timebegin, NULL);
      times(&beginning);
      for (k = 0; k < innerCount; k++) {
	retstr = timeit_p_ping3(handle, test_string, &e);
	if (e.returnCode != NULL) {
	  fprintf(stderr, "Failure <%s> in call (%lu,%lu) to ping3\n",
		  e.returnCode, i, k);
	  exit(2);
	}
      }
      times(&ending);
      gettimeofday(&timeend, NULL);
      AddStat(&tsums,
	      (((timeend.tv_sec * 1.0E6 + timeend.tv_usec)
		- (timebegin.tv_sec * 1.0E6 + timebegin.tv_usec))
	       / (innerCount * 1000.0)));
      AddStat(&usums,
	      (ending.tms_utime - beginning.tms_utime) * tfactor);
      AddStat(&ssums,
	      (ending.tms_stime - beginning.tms_stime) * tfactor);
    }
    break;

  case 4:

    for (i = 0; i < outerCount; i++) {
      gettimeofday(&timebegin, NULL);
      times(&beginning);
      doc = timeit_p_doctest(handle,
			     "some document, possibly with search criteria",
			     pagecount, pagesize, &e);
      if (e.returnCode != NULL) {
	fprintf(stderr, "Failure <%s> in call %lu to doctest\n",
		e.returnCode, i);
	exit(2);
      } else {
	timeit_pageSeq__Free(doc);
      }
      times(&ending);
      gettimeofday(&timeend, NULL);
      AddStat(&tsums,
	      (((timeend.tv_sec * 1.0E6 + timeend.tv_usec)
		- (timebegin.tv_sec * 1.0E6 + timebegin.tv_usec))
	       / (innerCount * 1000.0)));
      AddStat(&usums,
	      (ending.tms_utime - beginning.tms_utime) * tfactor);
      AddStat(&ssums,
	      (ending.tms_stime - beginning.tms_stime) * tfactor);
    }
    break;

  }


  printf("test \"%s\", outerCount %lu, innerCount %lu:\n",
	 tests[testtype - 1], outerCount, innerCount);
  printf("   category:    min /    avg +-  stdev /    max milliseconds/call\n");
  printf(" total time: %s\n", FmtStats(&tsums));
  printf("  user time: %s\n", FmtStats(&usums));
  printf("system time: %s\n", FmtStats(&ssums));


  exit(0);
}

#endif /* WIN32 */

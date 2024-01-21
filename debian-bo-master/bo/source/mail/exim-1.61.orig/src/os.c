/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1996 */
/* See the file NOTICE for conditions of use and distribution. */

/* This source file contains "default" system-dependent functions which
provide functionality (or lack of it) in cases where the OS-specific os.c
file has not. Some of them are tailored by macros defined in os.h files. */


#ifndef OS_RESTARTING_SIGNAL
/*************************************************
*          Set up restarting signal              *
*************************************************/

/* This function has the same functionality as the ANSI C signal() function,
except that it arranges that, if the signal happens during a system call, the
system call gets restarted. Different versions of Unix have different defaults,
and different ways of achieving this. If the functionality is not available,
the signal should be set to be ignored. */

void
os_restarting_signal(int sig, void (*handler)(int))
{
/* Many systems have the SA_RESTART sigaction for specifying that a signal
should restart system calls. These include SunOS5, AIX, BSDI, IRIX, FreeBSD,
OSF1, Linux and HP-UX 10 (but *not* HP-UX 9). */

#ifdef	SA_RESTART
struct sigaction act;
act.sa_handler = handler;
sigemptyset(&(act.sa_mask));
act.sa_flags = SA_RESTART;
sigaction(sig, &act, NULL);

/* SunOS4 and Ultrix default to non-interruptable signals, with SV_INTERRUPT
for making them interruptable. This seems to be a dying fashion. */

#elif defined SV_INTERRUPT
signal(sig, handler);

/* If neither SA_RESTART nor SV_INTERRUPT is available we don't know how to
set up a restarting signal, so simply suppress the facility. */

#else
signal(sig, SIG_IGN);
#endif
}

#endif	/* OS_RESTARTING_SIGNAL */


#ifdef STRERROR_FROM_ERRLIST
/*************************************************
*     Provide strerror() for non-ANSI libraries  *
*************************************************/

/* Some old-fashioned systems still around (e.g. SunOS4) don't have strerror()
in their libraries, but can provide the same facility by this simple
alternative function. */

char *
strerror(int n)
{
if (n < 0 || n >= sys_nerr) return "unknown error number";
return sys_errlist[n];
}
#endif /* STRERROR_FROM_ERRLIST */



/***********************************************************
*                   Load average function                  *
***********************************************************/

/* Although every Unix seems to have a different way of getting the load
average, a number of them have things in common. Some common variants are
provided below, but if an OS has unique requirements it can be handled in
a specific os.c file. What is required is a function called os_getloadavg
which takes no arguments and passes back the load average * 1000 as an int,
or -1 if no data is available. */



/* ----------------------------------------------------------------------- */
/* If the OS has got a BSD getloadavg() function, life is very easy. */

#if !defined(OS_LOAD_AVERAGE) && defined(HAVE_BSD_GETLOADAVG)
#define OS_LOAD_AVERAGE

int
os_getloadavg(void)
{
double avg;
int loads = getloadavg (&avg, 1);
if (loads != 1) return -1;
return (int)(avg * 1000.0);
}
#endif
/* ----------------------------------------------------------------------- */



/* ----------------------------------------------------------------------- */
/* Only SunOS5 has the kstat functions as far as I know, but put the code
here as there is the -hal variant, and other systems might follow this road one
day. */

#if !defined(OS_LOAD_AVERAGE) && defined(HAVE_KSTAT)
#define OS_LOAD_AVERAGE

#include <kstat.h>

int
os_getloadavg(void)
{
int avg;
kstat_ctl_t *kc;
kstat_t *ksp;
kstat_named_t *kn;

if ((kc = kstat_open()) == NULL ||
    (ksp = kstat_lookup(kc, LOAD_AVG_KSTAT_MODULE, 0, LOAD_AVG_KSTAT))
        == NULL ||
     kstat_read(kc, ksp, NULL) < 0 ||
    (kn = kstat_data_lookup(ksp, LOAD_AVG_SYMBOL)) == NULL)
  return -1;

avg = (int)(((double)(kn->LOAD_AVG_FIELD)/FSCALE) * 1000.0);

kstat_close(kc);
return avg;
}

#endif
/* ----------------------------------------------------------------------- */



/* ----------------------------------------------------------------------- */
/* Handle OS where a kernel symbol has to be read from /dev/kmem */

#if !defined(OS_LOAD_AVERAGE) && defined(HAVE_DEV_KMEM)
#define OS_LOAD_AVERAGE

#include <nlist.h>

static int  avg_kd = -1;
static long avg_offset;

int
os_getloadavg(void)
{
LOAD_AVG_TYPE avg;

if (avg_kd < 0)
  {
  struct nlist nl[2];
  nl[0].n_name = LOAD_AVG_SYMBOL;
  nl[1].n_name = "";
  nlist (KERNEL_PATH, nl);
  avg_offset = nl[0].n_value;
  avg_kd = open ("/dev/kmem", 0);
  if (avg_kd < 0) return -1;
  (void) fcntl(avg_kd, F_SETFD, FD_CLOEXEC);
  }

if (lseek (avg_kd, avg_offset, 0) == -1L
    || read (avg_kd, (char *)(&avg), sizeof (avg)) != sizeof(avg))
  return -1;

return (int)(((double)avg/FSCALE)*1000.0);
}

#endif
/* ----------------------------------------------------------------------- */



/* ----------------------------------------------------------------------- */
/* If nothing is known about this OS, then the load average facility is
not available. */

#ifndef OS_LOAD_AVERAGE

int
os_getloadavg(void)
{
return -1;
}

#endif

/* ----------------------------------------------------------------------- */



/*************************************************
**************************************************
*             Stand-alone test program           *
**************************************************
*************************************************/

/* This is for testing os_getloadavg(). */

#ifdef STAND_ALONE

#ifdef CLOCKS_PER_SEC
#define REAL_CLOCK_TICK CLOCKS_PER_SEC
#else
  #ifdef CLK_TCK
  #define REAL_CLOCK_TICK CLK_TCK
  #else
  #define REAL_CLOCK_TICK 1000000   /* SunOS4 */
  #endif
#endif

int main(int argc, char **argv)
{
for (;;)
  {
  int avg;
  clock_t used;
  clock_t before = clock();
  avg = os_getloadavg();
  used = clock() - before;
  printf("cpu time = %.2f ", (double)used/REAL_CLOCK_TICK);
  if (avg < 0)
    {
    printf("load average not available\n");
    break;
    }
  printf("load average = %.2f\n", (double)avg/1000.0);
  sleep(2);
  }
return 0;
}

#endif

/* End of os.c */

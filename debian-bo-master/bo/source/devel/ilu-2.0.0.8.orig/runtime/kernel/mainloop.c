/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: mainloop.c,v 1.55 1996/04/24 04:45:11 janssen Exp $ */
/* Last edited by Mike Spreitzer March 14, 1996 3:37 pm PST */

#include <math.h>
#include <time.h>

#include "iluntrnl.h"
#include "oscalls.h"	/* for OS_READ and OS_WRITE */

#if (defined(WIN32) || defined(WIN16))
#include <winsock.h>   /* for recv */
#endif /*  WIN32 or WIN16 */


/* ================ FineTime Utilities ================ */
/*L1, L2, Main unconstrained*/

ilu_FineTime ilu_FineTime_Add(ilu_FineTime a, ilu_FineTime b)
{
  ilu_FineTime c;
  c.ft_s = a.ft_s + b.ft_s;
  c.ft_t = a.ft_t + b.ft_t;
  if (c.ft_t >= ilu_FineTimeRate) {
      c.ft_t -= ilu_FineTimeRate;
      c.ft_s += 1;
    }
  return c;
}

ilu_FineTime ilu_FineTime_Sub(ilu_FineTime a, ilu_FineTime b)
{
  ilu_FineTime c;
  c.ft_s = a.ft_s - b.ft_s - 1;
  c.ft_t = a.ft_t + ilu_FineTimeRate - b.ft_t;
  if (c.ft_t >= ilu_FineTimeRate) {
      c.ft_t -= ilu_FineTimeRate;
      c.ft_s += 1;
    }
  return c;
}

ilu_FineTime ilu_FineTime_Mul(ilu_FineTime a, float b)
{
  double as  = a.ft_s;
  double at  = a.ft_t;
  double rate = ilu_FineTimeRate;
  double amd = as + (at/rate);
  double amp = amd * b;
  return ilu_FineTime_FromDouble(amp);
}

ilu_FineTime ilu_FineTime_FromDouble(double seconds)
{
  double rate = ilu_FineTimeRate;
  double usecs = seconds * rate;
  double csd = floor(usecs/rate);
  double ctd = usecs - (csd * rate);
  ilu_FineTime c;
  c.ft_s = (ilu_integer) csd;
  c.ft_t = (ilu_cardinal) ctd;
  return (c);
}

ilu_integer ilu_FineTime_Cmp(ilu_FineTime a, ilu_FineTime b)
{
   if (a.ft_s != b.ft_s)
        return (a.ft_s - b.ft_s);
   else return ( ((ilu_integer) a.ft_t) - ((ilu_integer) b.ft_t) );
}

ilu_cardinal ilu_rescale(ilu_cardinal n, ilu_cardinal dfrom,
					 ilu_cardinal dto)
{
  if (dfrom == dto)
      return n;
  else {
      double from = dfrom ? dfrom : (1.0 + (double) (ilu_cardinal) -1);
      double to   = dto   ? dto   : (1.0 + (double) (ilu_cardinal) -1);
      double ans = floor(to * n / from);
      int ians = (int) ans;	/* dll avoid conversion warnings */
      return (ians);
    }
}

/* ================ Main Loop ================ */

/*L1, L2, Main unconstrained*/

static ilu_MainLoop *theMainLoop = &_ilu_DefaultMainLoop;
static int mlPhase = 0;	/* !=0 => can't change theMainLoop */

void ilu_SetMainLoop(ilu_MainLoop *ml)
{
  _ilu_AutoSetDebugLevel();
  _ilu_Assert(ml != NIL, "setting NIL MainLoop");
  _ilu_Assert(mlPhase == 0, "MainLoop already set");
  theMainLoop = ml;
  mlPhase = 1;
  _ilu_gcoAlarm = ilu_CreateAlarm();
  _ilu_gccAlarm = ilu_CreateAlarm();
  _ilu_ioTimeoutAlarm = ilu_CreateAlarm();
  _ilu_grAlarm = ilu_CreateAlarm();
  _ilu_udpAlarm = ilu_CreateAlarm();
}

ilu_boolean ilu_RegisterInputSource (int fd,
	ilu_IOHandler proc,
	ilu_private rock)
{
  mlPhase = 1;
  _ilu_AutoSetDebugLevel();
  DEBUG(MAINLOOP_DEBUG, (stderr, "RegisterInputSource(%d, %p, %p)\n",
			 fd, proc, rock));
  _ilu_Assert(theMainLoop->ml_register_input != NULLFN,
	      "RegisterInputSource in threaded runtime");
  return ((*theMainLoop->ml_register_input) (fd, proc, rock));
}

ilu_boolean ilu_UnregisterInputSource (int fd)
{
  mlPhase = 1;
  DEBUG(MAINLOOP_DEBUG, (stderr, "UnregisterInputSource(%d)\n", fd));
  if (theMainLoop->ml_unregister_input == NULLFN)
    return ilu_FALSE;
  return ((*theMainLoop->ml_unregister_input) (fd));
}

ilu_boolean ilu_RegisterOutputSource (int fd,
	ilu_IOHandler proc,
	ilu_private rock)
{
  mlPhase = 1;
  _ilu_AutoSetDebugLevel();
  DEBUG(MAINLOOP_DEBUG, (stderr, "RegisterOutputSource(%d, %p, %p)\n",
			 fd, proc, rock));
  _ilu_Assert(theMainLoop->ml_register_output != NULLFN,
	      "RegisterOutputSource in threaded runtime");
  return ((*theMainLoop->ml_register_output) (fd, proc, rock));
}

ilu_boolean ilu_UnregisterOutputSource (int fd)
{
  mlPhase = 1;
  DEBUG(MAINLOOP_DEBUG, (stderr, "UnregisterOutputSource(%d)\n", fd));
  if (theMainLoop->ml_unregister_output == NULLFN)
    return ilu_FALSE;
  return ( (*theMainLoop->ml_unregister_output)(fd) );
}

/* timu == ilu_daimu == mxamu */

/*L1 >= {daimu}; L2, Main unconstrained*/

ilu_refany      _ilu_gcoAlarm = &_ilu_gcoDefaultAlarm_s;
ilu_refany      _ilu_gccAlarm = &_ilu_gccDefaultAlarm_s;
ilu_refany      _ilu_ioTimeoutAlarm = &_ilu_iotDefaultAlarm_s;
ilu_refany      _ilu_grAlarm = &_ilu_grDefaultAlarm_s;
ilu_refany      _ilu_udpAlarm = &_ilu_udpDefaultAlarm_s;

/*L1_sup < timu*/

ilu_refany ilu_CreateAlarm(void)
{
  mlPhase = 1;
  return ( (*theMainLoop->ml_create_alarm)() );
}

void ilu_SetAlarm(ilu_refany alrm, ilu_FineTime t,
		  /*for invoking: Main Invariant holds*/
		  void (*proc)(ilu_private rock),
		  ilu_private rock)
{
  mlPhase = 1;
  (*theMainLoop->ml_set_alarm)(alrm, t, proc, rock);
  return;
}

void ilu_UnsetAlarm(ilu_refany alrm)
{
  mlPhase = 1;
  (*theMainLoop->ml_unset_alarm)(alrm);
  return;
}

/*Main Invariant holds; L2 otherwise unconstrained*/
void ilu_RunMainLoop(int *stop)
{
  mlPhase = 1;
  DEBUG(MAINLOOP_DEBUG, (stderr, "RunMainLoop(%p): start\n", stop));
  (*theMainLoop->ml_run)(stop);
  DEBUG(MAINLOOP_DEBUG, (stderr, "RunMainLoop(%p): finish\n", stop));
  return;
}

/*L1, L2 unconstrained*/

void ilu_ExitMainLoop(int *stop)
{
  mlPhase = 1;
  DEBUG(MAINLOOP_DEBUG, (stderr, "ExitMainLoop(%p)\n", stop));
  (*theMainLoop->ml_exit)(stop);
  return;
}


/* ================ Other Concurrent IO Stuff ================ */


typedef struct wait_frame_s WaitFrame;
struct wait_frame_s {		/* A chained stack frame */
  /* L1, L2, Main unconstrained --- single-threaded */

  ilu_Alarmette_s wake;		/* for timing out */
  WaitFrame      *fd_next;	/* next in chain */
  WaitFrame      *hotter, *cooler;	/* stack is doubly linked */
  int             fd;		/* half of chain key */
  int             input;	/* half of chain key */
  int             stop;		/* the stacked value */
  ilu_boolean     sure;		/* was this innermost? */
  ilu_boolean     regd;		/* is FoundFD still registered? */
};

/*L1, L2, Main unconstrained --- single-threaded*/

static WaitFrame *wfs = NIL;
/*
 * A chain of stacks.  The chain runs through the hottest frame of
 * each stack.  If wf->regd, all hotter frames are registered; if
 * !wf->regd, all cooler frames are not registered.  The hottest
 * frame's regd member indicates whether FoundFD is registered as
 * the input handler for the stack's FD.
 */

static void TakeTimeout(ilu_private rock);

static void TAInvoke(ilu_Alarmette a)
{
  WaitFrame      *wf = (WaitFrame *) a;
  ilu_boolean     regd, input = wf->input;
  while (wf->cooler != NIL)
    wf = wf->cooler;
  for (wf = wf; wf != NIL; wf = wf->hotter) {
    regd = wf->regd;
    wf->sure = wf->regd = FALSE;
    ilu_ExitMainLoop(&(wf->stop));
  }
  if (regd)
    ((input ? ilu_UnregisterInputSource : ilu_UnregisterOutputSource)
     (wf->fd));
  return;
}

static void TASet(ilu_FineTime t)
{
  ilu_SetAlarm(_ilu_ioTimeoutAlarm, t, TakeTimeout, NIL);
}

static void TACancel(void)
{
  ilu_UnsetAlarm(_ilu_ioTimeoutAlarm);
}

static ilu_Alarmette_s timeHead = {&timeHead, &timeHead, FALSE, {0, 0}};
static ilu_AlarmRep timeAlarm = {&timeHead, TAInvoke, TASet, TACancel};

static void TakeTimeout(ilu_private rock)
{
  ilu_MXAProc(ilu_FineTime_Now(), &timeAlarm);
}

static void FoundFD(int fd, ilu_private rock)
{
  WaitFrame      *wf = (WaitFrame *) rock;
  ilu_boolean     regd, input = wf->input;
  while (wf->cooler != NIL)
    wf = wf->cooler;
  for (wf = wf; wf != NIL; wf = wf->hotter) {
    regd = wf->regd;
    wf->sure = (wf->hotter == NIL) && (wf->stop == 0);
    wf->regd = FALSE;
    ilu_ExitMainLoop(&(wf->stop));
  }
  if (regd)
    ((input ? ilu_UnregisterInputSource : ilu_UnregisterOutputSource)
     (fd));
  return;
}

static ilu_WaitTech *nsWT = NIL;
static int wtPhase = 0;

void ilu_SetWaitTech(ilu_WaitTech *wt)
{
  _ilu_Assert(wtPhase == 0, "SetWaitTech");
  nsWT = wt;
  wtPhase = 1;
}

/*Main Invariant holds; L2 otherwise unconstrained*/

static void IOWait(int fd, int input, ilu_boolean *sure,
		   ilu_FineTime *limit)
{
  WaitFrame       this, **pp;
  ilu_boolean     bottom;
  static ilu_Alarmette_s nullAlarmette = {NIL, NIL, FALSE, {0, 0}};
  this.wake = nullAlarmette;
  this.hotter = NIL;
  this.regd = TRUE;
  for (pp = &wfs; (*pp) != NIL; pp = &((*pp)->fd_next)) {
    if (((*pp)->fd == fd) && ((*pp)->input == input)) {
      _ilu_Assert((*pp)->hotter == NIL,
		  "mainloop.c:IOWait (*pp)->hotter != NIL");
      this.cooler = *pp;
      (*pp)->hotter = &this;
      this.fd_next = (*pp)->fd_next;
      *pp = &this;
      bottom = FALSE;
      if (this.cooler->regd)
	goto redy;
      goto regit;
    }
  }
  this.cooler = NIL;
  this.fd_next = wfs;
  wfs = &this;
  bottom = TRUE;
regit:
  if (input)
       ilu_RegisterInputSource (fd, FoundFD, (ilu_private) &this);
  else ilu_RegisterOutputSource(fd, FoundFD, (ilu_private) &this);
redy:
  this.fd = fd;
  this.input = input;
  this.stop = 0;
  this.sure = 2;
#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & MAINLOOP_DEBUG) {
    WaitFrame      *q, *r;
    for (q = wfs; q != NIL; q = q->fd_next) {
      _ilu_Assert(q->hotter == NIL,
		  "mainloop.c:IOWait q->hotter != NIL");
      ILU_ERRPRINTF("wait(%d,%d):", q->fd, q->input);
      for (r = q; r != NIL; r = r->cooler) {
	ILU_ERRPRINTF(" *%p=%d, %d", &r->stop, r->stop, r->sure);
      }
      ILU_ERRPRINTF("\n");
    }
  }
#endif /* ENABLE_DEBUGGING */
  if (limit != NIL)
      ilu_MXASet(&timeAlarm, &this.wake, *limit);
  ilu_RunMainLoop(&this.stop);
  *sure = this.sure;
  if (limit != NIL)
      ilu_MXAClear(&timeAlarm, &this.wake);
  if ( bottom ) {
      _ilu_Assert(wfs == &this, "IOWait: pop new");
      wfs = this.fd_next;
    }
  else {
      _ilu_Assert(this.cooler != NIL, "IOWait: this.cooler == NIL");
      _ilu_Assert(this.fd_next == this.cooler->fd_next,
		  "IOWait: pop old");
      *pp = this.cooler;
      (*pp)->hotter = NIL;
    }
  return;
}

void 
_ilu_WaitForInputOnFD(int fd, ilu_boolean * sure,
		      ilu_FineTime * limit,
		      ILU_ERRS((interrupt)) * err)
{
  wtPhase = 1;
  if (nsWT != NIL) {
    (*nsWT->wt_read_wait) (fd, sure, limit, err);
  } else {
    ILU_CLER(*err);
    IOWait(fd, 1, sure, limit);
  }
  return;
}

void
_ilu_WaitForOutputOnFD(int fd, ilu_boolean * sure,
		       ilu_FineTime * limit,
		       ILU_ERRS((interrupt)) * err)
{
  wtPhase = 1;
  if (nsWT != NIL) {
    (*nsWT->wt_write_wait) (fd, sure, limit, err);
    *sure = TRUE;
  } else {
    ILU_CLER(*err);
    IOWait(fd, 0, sure, limit);
  }
  return;
}

ilu_boolean
_ilu_InterruptFD(int fd, ILU_ERRS((bad_param, internal)) * err)
{
  WaitFrame      *wf1, *wf2;
  wtPhase = 1;
  if (nsWT != NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_threading, FALSE);
  for (wf1 = wfs; wf1 != NIL; wf1 = wf1->fd_next) {
    if (!ilu_Check(wf1->hotter == NIL, err))
      return FALSE;
    if (wf1->fd == fd && wf1->regd) {
      ((wf1->input ? ilu_UnregisterInputSource
	: ilu_UnregisterOutputSource)
       (fd));
      for (wf2 = wf1; wf2 != NIL; wf2 = wf2->cooler) {
	wf2->sure = FALSE;
	wf2->regd = FALSE;
	ilu_ExitMainLoop(&(wf2->stop));
      }
    }
  }
  ILU_CLER(*err);
  return TRUE;
}


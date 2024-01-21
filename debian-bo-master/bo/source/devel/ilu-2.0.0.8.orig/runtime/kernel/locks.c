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
/* $Id: locks.c,v 1.39 1996/06/10 14:45:22 spreitze Exp $ */
/* Last tweaked by Mike Spreitzer June 10, 1996 7:45 am PDT */

#define _POSIX_SOURCE

#include "iluntrnl.h"

/*L1 not meaningful; this module implements L1 locks;
  L2, Main unconstrained*/


typedef struct {
  ilu_string d1, d2;
  int held;
} DefaultMutex;

static          ilu_Mutex
Default_CreateMutex(ilu_string d1, ilu_string d2)
{
  DefaultMutex   *dm = (DefaultMutex *) ilu_malloc(sizeof(DefaultMutex));
  if (dm == NIL)
    return NIL;
  dm->d1 = _ilu_Strdup(d1);
  dm->d2 = _ilu_Strdup(d2);
  if ((d1 != NIL && dm->d1 == NIL) || (d2 != NIL && dm->d2 == NIL))
    return NIL;
  dm->held = 0;
  return ((ilu_Mutex) dm);
}

static void
Default_UnconsMutex(ilu_Mutex m, ilu_string * d1, ilu_string * d2,
		    ILU_ERRS((bad_param)) * err)
{
  DefaultMutex   *dm = (DefaultMutex *) m;
  if (dm == NIL)
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, NIL);
  else {
    ILU_CLER(*err);
    *d1 = dm->d1;
    *d2 = dm->d2;
  }
  return;
}

static void 
Default_AcquireMutex(ilu_private m,
		     ILU_ERRS((bad_param, bad_locks)) * err)
{
  DefaultMutex   *dm = (DefaultMutex *) m;
  if (dm == NIL)
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (dm->held)
    ILU_ERR_CONS0(bad_locks, err, 0);
  else {
    ILU_CLER(*err);
    dm->held = 1;
  }
  return;
}

static void 
Default_HoldMutex(ilu_private m,
		  ILU_ERRS((bad_param, bad_locks)) * err)
{
  DefaultMutex   *dm = (DefaultMutex *) m;
  if (dm == NIL)
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!dm->held)
    ILU_ERR_CONS0(bad_locks, err, 0);
  else
    ILU_CLER(*err);
  return;
}

static void 
Default_ReleaseMutex(ilu_private m,
		     ILU_ERRS((bad_param, bad_locks)) * err)
{
  DefaultMutex   *dm = (DefaultMutex *) m;
  if (dm == NIL)
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!dm->held)
    ILU_ERR_CONS0(bad_locks, err, 0);
  else {
    ILU_CLER(*err);
    dm->held = 0;
  }
  return;
}

static ilu_LockTech Default_LockTech[1] = {
  {Default_CreateMutex, Default_UnconsMutex,
    Default_AcquireMutex, Default_HoldMutex, Default_ReleaseMutex,
    NULLFN, NULLFN, NULLFN, NULLFN, NULLFN} };

static ilu_LockTech *theLockTech = Default_LockTech;
static int ltPhase = 0;
static int nIn = 0;
static int stdDumped = 0;

static DefaultMutex def_smu = {"global ", "smu", 0};
static DefaultMutex def_otmu = {"global ", "otmu", 0};
static DefaultMutex def_cmu = {"global ", "cmu", 0};
static DefaultMutex def_prmu = {"global ", "prmu", 0};
static DefaultMutex def_trmu = {"global ", "trmu", 0};
static DefaultMutex def_gcmu = {"global ", "gcmu", 0};
static DefaultMutex def_daimu = {"global ", "daimu", 0};

ilu_Mutex ilu_smu = &def_smu;
ilu_Mutex ilu_otmu = &def_otmu;
ilu_Mutex ilu_cmu = &def_cmu;
ilu_Mutex ilu_prmu = &def_prmu;
ilu_Mutex ilu_trmu = &def_trmu;
ilu_Mutex ilu_gcmu = &def_gcmu;
ilu_Mutex ilu_daimu = &def_daimu;

static ilu_boolean _ilu_GetMutexNames (ilu_Mutex m, ilu_string *d1, ilu_string *d2)
{
  ilu_boolean val;
  ilu_Error err = ILU_INIT_NO_ERR;
  (*(theLockTech->lt_muncons))(m, d1, d2, &err);
  val = ILU_ERROK(err);
  ILU_HANDLED(err);
  if (!val) { *d1 = "?"; *d2 = "?"; };
  return val;
}

ilu_Mutex ilu_CreateMutex(ilu_string d1, ilu_string d2)
{ return _ilu_CreateMutex(d1, d2); }

ilu_Mutex _ilu_CreateMutex(ilu_string d1, ilu_string d2)
{
  ilu_private m;
  _ilu_AutoSetDebugLevel();
  ltPhase = 1;
  DEBUG(LOCK_DEBUG, (stderr, "_ilu_CreateMutex:  %s, %s\n", d1, d2));
  m = (theLockTech->lt_mcreate)(d1, d2);
  DEBUG(LOCK_DEBUG, (stderr, "_ilu_CreateMutex:  => %p\n", m));
  return m;
}

void ilu_AcquireMutex(ilu_Mutex m)
{ _ilu_AcquireMutex(m); }

void _ilu_AcquireMutex(ilu_Mutex m)
{
  ILU_ERRS((bad_param, bad_locks)) lerr = ILU_INIT_NO_ERR;
  _ilu_AutoSetDebugLevel();
#ifdef ENABLE_DEBUGGING
  if ((_ilu_DebugLevel & LOCK_DEBUG) != 0)
    {
      ilu_string d1, d2;
      (void) _ilu_GetMutexNames(m, &d1, &d2);
      ilu_DebugPrintf ("_ilu_AcquireMutex:  %p (%s%s)\n", m, d1, d2);
    };
#endif
  nIn++;
  if (!stdDumped) {
    DEBUG(LOCK_DEBUG, (stderr, "ilu_smu = %p\n", ilu_smu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_otmu = %p\n", ilu_otmu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_cmu = %p\n", ilu_cmu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_prmu = %p\n", ilu_prmu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_trmu = %p\n", ilu_trmu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_gcmu = %p\n", ilu_gcmu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_daimu = %p\n", ilu_daimu));
    stdDumped = 1;
  }
  (theLockTech->lt_acquire) (m, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return;
}

ilu_boolean
ilu_EnterMutexWork(ilu_Mutex m, ilu_boolean hard,
	       ILU_ERRS((bad_locks, internal, broken_locks)) * err,
		   const char *file, int line)
{
  ILU_ERRS((bad_param, bad_locks)) lerr = ILU_INIT_NO_ERR;
  _ilu_AutoSetDebugLevel();
  if ((_ilu_DebugLevel & LOCK_DEBUG) != 0)
    {
      ilu_string d1, d2;
      (void) _ilu_GetMutexNames(m, &d1, &d2);
      ilu_DebugPrintf ("ilu_EnterMutex(%s, %s:%d):  %p (%s%s)\n",
		       (hard ? "hard" : "soft"), file, line, m, d1, d2);
    };
  nIn++;
  if (!stdDumped) {
    DEBUG(LOCK_DEBUG, (stderr, "ilu_smu = %p\n", ilu_smu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_otmu = %p\n", ilu_otmu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_cmu = %p\n", ilu_cmu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_prmu = %p\n", ilu_prmu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_trmu = %p\n", ilu_trmu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_gcmu = %p\n", ilu_gcmu));
    DEBUG(LOCK_DEBUG, (stderr, "ilu_daimu = %p\n", ilu_daimu));
    stdDumped = 1;
  }
  (theLockTech->lt_acquire) (m, &lerr);
  if (ILU_ERROK(lerr)) {
    if (!hard)
      ILU_CLER(*err);
    return TRUE;
  }
  if (hard)
    ILU_ERR_FULLCONS0(broken_locks, err, 0, file, line);
  else
    ILU_ERR_SWITCH(lerr) {
    ILU_ERR_CASE(bad_param, v)
      ILU_ERR_FULLCONS1(internal, err, minor, ilu_im_inv_mutex, 0,
			file, line);
    ILU_ERR_CASE(bad_locks, v)
      ILU_ERR_FULLCONS0(bad_locks, err, 0, file, line);
    ILU_ERR_ELSE
      ILU_ERR_FULLCONS1(internal, err, minor, ilu_im_unhandled, 0,
			file, line);
    } ILU_ERR_ENDSWITCH;
  ILU_HANDLED(lerr);
  return FALSE;
}

ilu_boolean
ilu_ExitMutexWork(ilu_Mutex m, ilu_boolean hard,
	       ILU_ERRS((bad_locks, internal, broken_locks)) * err,
		  const char *file, int line)
{
  ILU_ERRS((bad_param, bad_locks)) lerr = ILU_INIT_NO_ERR;
#ifdef ENABLE_DEBUGGING
  if ((_ilu_DebugLevel & LOCK_DEBUG) != 0)
    {
      ilu_string d1, d2;
      (void) _ilu_GetMutexNames(m, &d1, &d2);
      ilu_DebugPrintf ("ilu_ExitMutex(%s:%d):  %p (%s%s)\n", file, line, m, d1, d2);
    };
#endif
  (theLockTech->lt_release) (m, &lerr);
  nIn--;
  if (ILU_ERROK(lerr))
    return TRUE;
  if (hard)
    ILU_ERR_FULLCONS0(broken_locks, err, 0, file, line);
  else
    ILU_ERR_SWITCH(lerr) {
    ILU_ERR_CASE(bad_param, v)
      ILU_ERR_FULLCONS1(internal, err, minor, ilu_im_inv_mutex, 0,
			file, line);
    ILU_ERR_CASE(bad_locks, v)
      ILU_ERR_FULLCONS0(bad_locks, err, 0, file, line);
    ILU_ERR_ELSE
      ILU_ERR_FULLCONS1(internal, err, minor, ilu_im_unhandled, 0,
			file, line);
    } ILU_ERR_ENDSWITCH;
  ILU_HANDLED(lerr);
  return FALSE;
}

void ilu_HoldMutex(ilu_Mutex m)
{
  _ilu_HoldMutex(m);
}

void _ilu_HoldMutex(ilu_Mutex m)
{
  ilu_Error lerr;
#ifdef ENABLE_DEBUGGING
  if ((_ilu_DebugLevel & LOCK_DEBUG) != 0)
    {
      ilu_string d1, d2;
      (void) _ilu_GetMutexNames(m, &d1, &d2);
      ilu_DebugPrintf ("_ilu_HoldMutex:  %p (%s%s)\n", m, d1, d2);
    };
#endif
  (theLockTech->lt_hold)(m, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return;
}

void ilu_ReleaseMutex(ilu_Mutex m)
{ _ilu_ReleaseMutex(m); }

void _ilu_ReleaseMutex(ilu_Mutex m)
{
  ILU_ERRS((bad_param, bad_locks)) lerr = ILU_INIT_NO_ERR;
#ifdef ENABLE_DEBUGGING
  if ((_ilu_DebugLevel & LOCK_DEBUG) != 0)
    {
      ilu_string d1, d2;
      (void) _ilu_GetMutexNames(m, &d1, &d2);
      ilu_DebugPrintf ("_ilu_ReleaseMutex:  %p (%s%s)\n", m, d1, d2);
    };
#endif
  (theLockTech->lt_release)(m, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  nIn--;
  return;
}

ilu_boolean ilu_CanCondition(void)
{
  return _ilu_CanCondition();
}

ilu_boolean _ilu_CanCondition(void)
{
  if (theLockTech->lt_ccreate != NULLFN)
       return TRUE;
  else return FALSE;
}

ILU_DEF_ERR(CantCondition, "Condition variables not available to kernel")
{}

ilu_Condition 
ilu_CreateCondition(ilu_string d1, ilu_string d2,
		     ILU_ERRS((CantCondition)) * err)
{ return _ilu_CreateCondition(d1, d2, err); }

ilu_Condition 
_ilu_CreateCondition(ilu_string d1, ilu_string d2,
		     ILU_ERRS((CantCondition)) * err)
{
  ilu_Condition     c;

  ltPhase = 1;
  if (theLockTech->lt_ccreate == NULLFN)
    return ILU_ERR_CONS0(CantCondition, err, NIL);
  else *err = ILU_NO_ERR;
  DEBUG(LOCK_DEBUG, (stderr, "_ilu_CreateCondition:  %s, %s\n", d1, d2));
  c = (*theLockTech->lt_ccreate) (d1, d2);
  DEBUG(LOCK_DEBUG, (stderr, "_ilu_CreateCondition:  => %p\n", c));
  return (c);
}

ilu_boolean
ilu_CondNotify(ilu_Condition c,
	       ILU_ERRS((broken_locks)) * err)
{
  ILU_ERRS((bad_param)) lerr;
  ltPhase = 1;
  if (theLockTech->lt_notify == NULLFN)
    return ILU_ERR_CONS0(broken_locks, err, FALSE);
  DEBUG(LOCK_DEBUG, (stderr, "ilu_CondNotify:  %p\n", c));
  (*theLockTech->lt_notify) (c, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    return ILU_ERR_CONS0(broken_locks, err, FALSE);
  }
  return TRUE;
}

ILU_ERRS((CantCondition)) ilu_NotifyCondition(ilu_Condition c)
{ return _ilu_NotifyCondition(c); }

ILU_ERRS((CantCondition)) _ilu_NotifyCondition(ilu_Condition c)
{
  ilu_Error       err;
  ltPhase = 1;
  if (theLockTech->lt_notify == NULLFN)
    return ILU_ERR_CONS0(CantCondition, &err, err);
  DEBUG(LOCK_DEBUG, (stderr, "_ilu_NotifyCondition:  %p\n", c));
  (*theLockTech->lt_notify) (c, &err);
  return err;
}

ILU_ERRS((CantCondition)) ilu_DestroyCondition(ilu_Condition c)
{ return _ilu_DestroyCondition(c); }

ILU_ERRS((CantCondition)) _ilu_DestroyCondition(ilu_Condition c)
{
  ilu_Error       err;
  ltPhase = 1;
  if (theLockTech->lt_cdestroy == NULLFN)
    return ILU_ERR_CONS0(CantCondition, &err, err);
  DEBUG(LOCK_DEBUG, (stderr, "_ilu_DestroyCondition:  %p\n", c));
  (*theLockTech->lt_cdestroy)(c, &err);
  return err;
}

ilu_boolean
ilu_CMWait2(ilu_Condition c, ilu_Mutex m, ilu_Mutex m2,
	    ILU_ERRS((broken_locks)) * err)
{
  ILU_ERRS((bad_param, bad_locks)) lerr;
  ltPhase = 1;
  if (theLockTech->lt_wait == NULLFN)
    return ILU_ERR_CONS0(broken_locks, err, FALSE);
  DEBUG(LOCK_DEBUG, (stderr, "ilu_CMWait:  c = %p, m = %p\n",
		     c, m));
  (*theLockTech->lt_wait) (c, m, m2, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    return ILU_ERR_CONS0(broken_locks, err, FALSE);
  }
  return TRUE;
}

ILU_ERRS((CantCondition)) ilu_WaitCondition(ilu_Condition c, ilu_Mutex m)
{ return _ilu_WaitCondition(c, m); }

ILU_ERRS((CantCondition)) _ilu_WaitCondition(ilu_Condition c, ilu_Mutex m)
{
  ilu_Error       err;
  ltPhase = 1;
  if (theLockTech->lt_wait == NULLFN)
    return ILU_ERR_CONS0(CantCondition, &err, err);
  DEBUG(LOCK_DEBUG, (stderr, "_ilu_WaitCondition:  c = %p, m = %p\n",
		     c, m));
  (*theLockTech->lt_wait)(c, m, m, &err);
  return err;
}

void
ilu_SetLockTech(ilu_LockTech * lt,
		ILU_ERRS((bad_param, no_memory)) * err)
{
  ilu_LockTech   *old = theLockTech;
  _ilu_AutoSetDebugLevel();
  DEBUG(LOCK_DEBUG,
	(stderr,
	 "ilu_RegisterLockTech (%p), ltPhase == %d, nIn == %d\n",
	 lt, ltPhase, nIn));
  if (ltPhase != 0 || nIn != 0) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_late, 0);
    return;
  }
  if (lt != NIL)
    theLockTech = lt;
  {
    ilu_Mutex       nu_smu = _ilu_CreateMutex("global ", "smu");
    ilu_Mutex       nu_otmu = _ilu_CreateMutex("global ", "otmu");
    ilu_Mutex       nu_cmu = _ilu_CreateMutex("global ", "cmu");
    ilu_Mutex       nu_prmu = _ilu_CreateMutex("global ", "prmu");
    ilu_Mutex       nu_trmu = _ilu_CreateMutex("global ", "trmu");
    ilu_Mutex       nu_gcmu = _ilu_CreateMutex("global ", "gcmu");
    ilu_Mutex       nu_daimu = _ilu_CreateMutex("global ", "daimu");
    if (nu_smu != NIL && nu_otmu != NIL && nu_cmu != NIL &&
	nu_prmu != NIL && nu_trmu != NIL && nu_gcmu != NIL &&
	nu_daimu != NIL) {
      ilu_smu = nu_smu;
      ilu_otmu = nu_otmu;
      ilu_cmu = nu_cmu;
      ilu_prmu = nu_prmu;
      ilu_trmu = nu_trmu;
      ilu_gcmu = nu_gcmu;
      ilu_daimu = nu_daimu;
      _ilu_connHandoffChange = ilu_CreateCondition("global ",
					   "conn handoff buff chg",
						   err);
    } else {
      theLockTech = old;
      ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
    }
  }
  return;
}

ilu_Mutex ilu_GetOTMutex(void)
{
  return ilu_otmu;
}

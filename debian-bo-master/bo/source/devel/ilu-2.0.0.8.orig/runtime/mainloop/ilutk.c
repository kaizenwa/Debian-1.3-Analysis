/* ilutk.c */
/* Last edited by Mike Spreitzer October 19, 1995 11:12 am PDT */

#include <tk.h>
#include <iluxport.h>
#include "ilutk.h"
#include <iluhash.h>
#include <iluntrnl.h>		/* for Assert */
#include <limits.h>		/* for INT_MAX */

static void
Run(int *stop)
{
  *stop = 0;
  while (!stop)
    (void) Tk_DoOneEvent(0);
  return;
}

static void
Exit(int *stop)
{
  *stop = 1;
}

typedef struct {
  struct {
    ilu_IOHandler   h;		/* NULLFN when not significant */
    ilu_private     r;
  }               for_in, for_out;
  int             fd;
}              *Handlers;

static HashTable fdh = NIL;

static          Handlers
GetHandlers(int fd)
{
  Handlers        hs;
  if (fdh == NIL) {
    fdh = _ilu_hash_MakeNewTable(16, _ilu_hash_HashPointer,
				 _ilu_hash_PointerCompare);
    if (fdh == NIL)
      return NIL;
  }
  hs = (Handlers) _ilu_hash_FindInTable(fdh, (ilu_refany) fd);
  if (hs == NULL) {
    hs = ilu_malloc(sizeof(*hs));
    if (hs == NIL)
      return NIL;
    hs->for_in.h = hs->for_out.h = NULLFN;
    hs->for_in.r = hs->for_out.r = NIL;
    hs->fd = fd;
    _ilu_Assert(_ilu_hash_AddToTable(fdh, (ilu_refany) fd, hs),
		"ilutk.c:GetHandlers");
  }
  return hs;
}

static void
CallHandlers(void *clientData, int mask)
{
  Handlers        hs = (Handlers) clientData;
  int             fd = hs->fd;
  while (hs != NIL) {
    if (hs->for_in.h != NULLFN && (mask & TK_READABLE | TK_EXCEPTION)) {
      mask &= ~(TK_READABLE | TK_EXCEPTION);
      (*hs->for_in.h) (fd, hs->for_in.r);
    } else if (hs->for_out.h != NULLFN && (mask & TK_WRITABLE)) {
      mask &= ~TK_WRITABLE;
      (*hs->for_out.h) (fd, hs->for_out.r);
    } else
      break;
    hs = (Handlers) _ilu_hash_FindInTable(fdh, (ilu_refany) fd);
  }
  return;
}

static          ilu_boolean
RegisterInput(int fd, ilu_IOHandler handler, ilu_private rock)
{
  Handlers        hs = GetHandlers(fd);
  int             mask;
  if (hs == NIL)
    return FALSE;
  if (hs->for_in.h != NULLFN || hs->for_out.h != NULLFN)
    Tk_DeleteFileHandler(fd);
  hs->for_in.h = handler;
  hs->for_in.r = rock;
  mask = (TK_READABLE | TK_EXCEPTION |
	  (hs->for_out.h != NULLFN ? TK_WRITABLE : 0));
  Tk_CreateFileHandler(fd, mask, CallHandlers, hs);
  return TRUE;
}

static          ilu_boolean
UnregisterInput(int fd)
{
  Handlers        hs;
  hs = (Handlers) _ilu_hash_FindInTable(fdh, (ilu_refany) fd);
  if (hs == NIL)
    return FALSE;
  Tk_DeleteFileHandler(fd);
  hs->for_in.h = NULLFN;
  hs->for_in.r = NIL;
  if (hs->for_out.h != NULLFN)
    Tk_CreateFileHandler(fd, TK_WRITABLE, CallHandlers, hs);
  else
    _ilu_Assert(_ilu_hash_RemoveFromTable(fdh, (ilu_refany) fd) != NIL,
		"ilutk.c:UnregisterInput");
  return TRUE;
}

static          ilu_boolean
RegisterOutput(int fd, ilu_IOHandler handler, ilu_private rock)
{
  Handlers        hs = GetHandlers(fd);
  int             mask;
  if (hs == NIL)
    return FALSE;
  if (hs->for_in.h != NULLFN || hs->for_out.h != NULLFN)
    Tk_DeleteFileHandler(fd);
  hs->for_out.h = handler;
  hs->for_out.r = rock;
  mask = (TK_WRITABLE |
      (hs->for_in.h != NULLFN ? (TK_READABLE | TK_EXCEPTION) : 0));
  Tk_CreateFileHandler(fd, mask, CallHandlers, hs);
  return TRUE;
}

static          ilu_boolean
UnregisterOutput(int fd)
{
  Handlers        hs;
  hs = (Handlers) _ilu_hash_FindInTable(fdh, (ilu_refany) fd);
  if (hs == NIL)
    return FALSE;
  Tk_DeleteFileHandler(fd);
  hs->for_out.h = NULLFN;
  hs->for_out.r = NIL;
  if (hs->for_in.h != NULLFN)
    Tk_CreateFileHandler(fd, (TK_READABLE | TK_EXCEPTION),
			 CallHandlers, hs);
  else
    _ilu_Assert(_ilu_hash_RemoveFromTable(fdh, (ilu_refany) fd) != NIL,
		"ilutk.c:UnregisterOutput");
  return TRUE;
}

/* Main Invariant holds; L2 otherwise unconstrained */
typedef void    (*AlarmProc) (ilu_private rock);

typedef struct {
  AlarmProc       proc;
  ilu_private     rock;
  ilu_boolean     isset;
  Tk_TimerToken   tt;		/* significant iff isset */
}              *IluTkAlarm;

static          ilu_refany
CreateAlarm(void)
{
  IluTkAlarm      ita = (IluTkAlarm) ilu_malloc(sizeof(*ita));
  if (ita == NIL)
    return NIL;
  ita->proc = NULLFN;
  ita->rock = NIL;
  ita->isset = FALSE;
  ita->tt = NIL;
  return ita;
}

static void
CallAlarm(void *clientData)
{
  IluTkAlarm      ita = (IluTkAlarm) clientData;
  _ilu_Assert(ita->isset, "ilutk.c:CallAlarm");
  ita->isset = FALSE;
  (*ita->proc) (ita->rock);
  return;
}

static void
SetAlarm(ilu_refany alarm, ilu_FineTime t,
	 AlarmProc proc, ilu_private rock)
{
  IluTkAlarm      ita = (IluTkAlarm) alarm;
  static ilu_FineTime zero = {0, 0};
  ilu_cardinal    ms;
  ilu_FineTime    now, dt;
  if (ita->isset)
    Tk_DeleteTimerHandler(ita->tt);
  now = ilu_FineTime_Now();
  dt = ilu_FineTime_Sub(t, now);
  if (ilu_FineTime_Cmp(dt, zero) < 0)
    dt = zero;
  ms = ilu_rescale(dt.ft_t, ilu_FineTimeRate, 1000);
  if (dt.ft_s > (INT_MAX - ms) / 1000)
    ms = INT_MAX;
  else
    ms += dt.ft_s * 1000;
  ita->tt = Tk_CreateTimerHandler(ms, CallAlarm, ita);
  ita->isset = TRUE;
  ita->proc = proc;
  ita->rock = rock;
  return;
}

static void
UnsetAlarm(ilu_refany alarm)
{
  IluTkAlarm      ita = (IluTkAlarm) alarm;
  if (ita->isset) {
    Tk_DeleteTimerHandler(ita->tt);
    ita->isset = FALSE;
  }
  return;
}

static ilu_MainLoop synth = {Run, Exit, RegisterInput, UnregisterInput, RegisterOutput, UnregisterOutput, CreateAlarm, SetAlarm, UnsetAlarm};

static ilu_boolean initted = FALSE;

void
IluTk_Init(void)
{
  if (initted)
    return;
  ilu_SetMainLoop(&synth);
  initted = TRUE;
  return;
}

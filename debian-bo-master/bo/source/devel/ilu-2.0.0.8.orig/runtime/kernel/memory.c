/*
Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.  

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
/* $Id: memory.c,v 1.23 1996/04/26 21:56:25 mdavidso Exp $ */
/* Last edited by Mike Spreitzer January 31, 1996 8:50 am PST */

#include "iluntrnl.h"

#include "oscalls.h"	/* for OS_SLEEP */

#ifdef USE_BOEHM_GC_MALLOC
#ifdef GC_DEBUG		/* this is the ILU GC_DEBUG... */
#undef GC_DEBUG		/* ...so undefine it so we can use the Boehm GC GC_DEBUG! */
#endif

#ifdef USE_BOEHM_GC_MALLOC_DEBUGGING
#define GC_DEBUG
#endif /* USE_BOEHM_GC_MALLOC_DEBUGGING */

#include BOEHM_GC_MALLOC_HEADER_FILE

#ifdef USE_BOEHM_GC_MALLOC_DEBUGGING
#define REAL_MALLOC(x)		GC_debug_malloc((x), (char *) file, (int) line)
#define REAL_FREE(x)		GC_debug_free(x)
#define REAL_REALLOC(x,s)	GC_debug_realloc((x), (s), (char *) file, (int) line)
#else
#define REAL_MALLOC(x)		GC_malloc(x)
#define REAL_FREE(x)		GC_free(x)
#define REAL_REALLOC(x,s)	GC_realloc((x),(s))
#endif /* USE_BOEHM_GC_MALLOC_DEBUGGING */

#else /* !USE_BOEHM_GC_MALLOC */

#define REAL_MALLOC	malloc
#define REAL_FREE	free
#define REAL_REALLOC	realloc

#endif

/*L1, L2, Main unconstrained*/

typedef struct ilu_FreerCons_s FreerCons, *FreerList;

struct ilu_FreerCons_s {
	FreerList next;
	void (*free)(ilu_cardinal size);
};

static FreerList freers = NIL;

ilu_boolean
ilu_AddFreer(void (*free_rtn) (ilu_cardinal size))
{
  FreerList       fl;
  DEBUG(MALLOC_DEBUG, (stderr, "ilu_AddFreer(%p)\n", free_rtn));
  fl = (FreerList) ilu_malloc(sizeof(FreerCons));
  if (fl == NIL)
    return ilu_FALSE;
  fl->free = free_rtn;
  fl->next = freers;
  freers = fl;
  return ilu_TRUE;
}

void           *
ilu_full_malloc(ilu_cardinal size, const char *file, int line)
{
  void           *ans;
  FreerList       fl;
  ans = REAL_MALLOC((SIZE_T) size);
  DEBUG(MALLOC_DEBUG,
	(stderr, "ilu_malloc(%lu=0x%lx)@%s:%d => %p\n",
	 size, size, file, line, ans));
  if (ans != NIL)
    return ans;
  for (fl = freers; fl != NIL; fl = fl->next) {
    DEBUG(MALLOC_DEBUG,
	  (stderr, "ilu_malloc: trying freer %p\n", fl->free));
    fl->free(size);
    ans = REAL_MALLOC((SIZE_T) size);
    DEBUG(MALLOC_DEBUG,
	  (stderr, "ilu_malloc(%lu=0x%lx)@%s:%d finally => %p\n",
	   size, size, file, line, ans));
    if (ans != NIL)
      return ans;
  }
  DEBUG(MALLOC_DEBUG,
	(stderr, "ilu_malloc(%lu=0x%lx)@%s:%d fails.\n",
	 size, size, file, line));
  return NIL;
}

void           *
ilu_full_realloc(void *p, ilu_cardinal size, const char *file, int line)
{
  void           *ans;
  FreerList       fl;
  ans = REAL_REALLOC(p, (SIZE_T) size);
  DEBUG(MALLOC_DEBUG,
	(stderr, "ilu_realloc(%p, %lu=0x%lx)@%s:%d => %p\n",
	 p, size, size, file, line, ans));
  if (ans != NIL)
    return ans;
  for (fl = freers; fl != NIL; fl = fl->next) {
    DEBUG(MALLOC_DEBUG,
	  (stderr, "ilu_realloc: trying freer %p\n", fl->free));
    fl->free(size);
    ans = REAL_REALLOC(p, (SIZE_T) size);
    DEBUG(MALLOC_DEBUG,
	  (stderr,
	   "ilu_realloc(%p, %lu=0x%lx)@%s:%d finally => %p\n",
	   p, size, size, file, line, ans));
    if (ans != NIL)
      return ans;
  }
  DEBUG(MALLOC_DEBUG,
	(stderr, "ilu_realloc(%p, %lu=0x%lx)@%s:%d fails.\n",
	 p, size, size, file, line));
  return NIL;
}

void ilu_full_free(void *p, const char *file, int line)
{
  DEBUG(MALLOC_DEBUG, (stderr, "ilu_free(%p)@%s:%d\n", p,
		       file, line));
  if (p != NIL)
    REAL_FREE(p);
}

static _ilu_FailureHandler theMFC = {_ilu_ConsumeByLoop, TRUE};

void ilu_SetMemFailureAction(int mfa)
{
  DEBUG(MALLOC_DEBUG,
	(stderr, "ilu_SetMemFaultAction: to %d.\n", mfa));
  theMFC = _ilu_FailureActionToConsumer(mfa, 0);
  return;
}

void ilu_SetMemFailureConsumer(ilu_FailureConsumer mfc)
{
  _ilu_Assert(mfc != NULLFN, "SetMemFailureConsumer(NIL)");
  DEBUG(MALLOC_DEBUG,
	(stderr, "ilu_SetMemFailureConsumer: to %p.\n", mfc));
  theMFC.fc = mfc;
  theMFC.printMsg = FALSE;
  return;
}

void           *
ilu_full_must_malloc(ilu_cardinal size,
		     const char *file, int line)
{
  long unsigned   req = size;
  void           *ans = ilu_full_malloc(size, file, line);
  if (ans != NIL)
    return ans;
  DEBUG(MALLOC_DEBUG,
	(stderr, "ilu_must_malloc(%lu=0x%lx) failed!\n",
	 size, size));
  if (theMFC.printMsg) {
    ILU_ERRPRINTF(
     "\nILU %s:  unrecoverable failure to allocate dynamic memory",
		  ilu_GetILUVersion());
    ILU_ERRPRINTF(" (%lu=0x%lx bytes requested)", req, req);
    ILU_ERRPRINTF(" at line %d in file %s.\n", line, file);
    ILU_ERRPRINTF("For information on how to debug or report this,");
    ILU_ERRPRINTF(" see the Debugging section of the ILU manual.\n");
  }
  (*theMFC.fc) (file, line);
  ILU_ERRPRINTF("ilu_FailureConsumer %p returned!", theMFC);
  ILU_ERRPRINTF("going into sleep loop!\n");
  _ilu_ConsumeByLoop(__FILE__, __LINE__);
  return (NIL);
}

void           *
ilu_full_MallocE(ilu_cardinal size,
		 ILU_ERRS((no_memory)) * err,
		 const char *file, int line)
{
  void           *ans = ilu_full_malloc(size, file, line);
  if (ans == NIL)
    return ILU_ERR_FULLCONS1(no_memory, err, nbytes, size, NIL,
			     file, line);
  return (ILU_CLER(*err), ans);
}

void           *
ilu_full_ReallocE(void *p, ilu_cardinal size,
		  ILU_ERRS((no_memory)) * err,
		  const char *file, int line)
{
  void           *ans = ilu_full_realloc(p, size, file, line);
  if (ans == NIL)
    return ILU_ERR_FULLCONS1(no_memory, err, nbytes, size, NIL,
			     file, line);
  return (ILU_CLER(*err), ans);
}

ilu_string ilu_full_StrdupE(const ilu_string str, ILU_ERRS((no_memory)) *err, const char *file, int line)
{
  ilu_string      p;
  if (str == NIL)
    return (ILU_CLER(*err), NIL);
  if ((p = ilu_full_MallocE(strlen(str) + 1, err, file, line)) == NIL)
    return (NIL);
  strcpy(p, str);
  return (p);
}

ilu_string _ilu_full_Strdup(const ilu_string str, const char *file, int line)
{
  ilu_string      p;

  if (str == NIL)
    return (NIL);
  else if ((p = ilu_full_malloc(strlen(str) + 1, file, line)) == NIL)
    return (NIL);
  else {
    strcpy(p, str);
    return (p);
  }
}

ilu_string
ilu_Strcat3E(const ilu_string s1, const ilu_string s2,
	     const ilu_string s3, ILU_ERRS((no_memory)) * err)
{
  int             l1 = (s1 == NIL) ? 0 : strlen(s1);
  int             l2 = (s2 == NIL) ? 0 : strlen(s2);
  int             l3 = (s3 == NIL) ? 0 : strlen(s3);
  ilu_string      t = ilu_MallocE(1 + l1 + l2 + l3, err);
  ilu_string      u = t;
  if (t == NIL)
    return t;
  if (s1 != NIL) {
    strcpy(u, s1);
    u += l1;
  }
  if (s2 != NIL) {
    strcpy(u, s2);
    u += l2;
  }
  if (s3 != NIL) {
    strcpy(u, s3);
    u += l3;
  }
  *u = 0;
  return t;
}

ilu_string 
_ilu_Strcat3(const ilu_string s1, const ilu_string s2,
	     const ilu_string s3)
{
  ILU_ERRS((no_memory)) lerr;
  ilu_string      ans = ilu_Strcat3E(s1, s2, s3, &lerr);
  ILU_HANDLED(lerr);
  return ans;
}

ilu_string _ilu_Strcat5(const ilu_string s1, const ilu_string s2,
			const ilu_string s3, const ilu_string s4,
			const ilu_string s5)
{
  int l1 = (s1==NIL) ? 0 : strlen(s1);
  int l2 = (s2==NIL) ? 0 : strlen(s2);
  int l3 = (s3==NIL) ? 0 : strlen(s3);
  int l4 = (s4==NIL) ? 0 : strlen(s4);
  int l5 = (s5==NIL) ? 0 : strlen(s5);
  ilu_string t = ilu_malloc(1 + l1 + l2 + l3 + l4 + l5);
  ilu_string u = t;
  if (t==NIL)
    return t;
  if (s1 != NIL) {
    strcpy(u, s1);
    u += l1;
  }
  if (s2 != NIL) {
    strcpy(u, s2);
    u += l2;
  }
  if (s3 != NIL) {
    strcpy(u, s3);
    u += l3;
  }
  if (s4 != NIL) {
    strcpy(u, s4);
    u += l4;
  }
  if (s5 != NIL) {
    strcpy(u, s5);
    u += l5;
  }
  *u = 0;
  return t;
}

/*
 * ... and we provide actual procedures, for clients that extract
 * and use procedure pointers.
 */

#undef ilu_malloc
#undef ilu_free
#undef ilu_realloc
#undef ilu_StrdupE
#undef _ilu_Strdup

void           *ilu_malloc(ilu_cardinal size)
{
  return ilu_full_malloc(size, __FILE__, __LINE__);
}

void           *ilu_realloc(void *p, ilu_cardinal size)
{
  return ilu_full_realloc(p, size, __FILE__, __LINE__);
}

void           ilu_free(void *p)
{
  ilu_full_free(p, __FILE__, __LINE__);
}

ilu_string	ilu_StrdupE (const ilu_string s, ilu_Error *err)
{
  return (ilu_full_StrdupE (s, err, __FILE__, __LINE__));
}

ilu_string	_ilu_Strdup (const ilu_string s)
{
  return (_ilu_full_Strdup (s, __FILE__, __LINE__));
}


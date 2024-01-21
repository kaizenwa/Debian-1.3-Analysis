/* $Id: c-clnt.c,v 1.8 1996/03/12 18:19:21 spreitze Exp $ */
/* Last edited by Mike Spreitzer March 12, 1996 10:17 am PST */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "ChgUp.h"

#define TIME ((unsigned long) time(ILU_NIL))

static int      always, destroy;

static int
ShowEnv(ILU_C_ENVIRONMENT * env, const char *what)
{
  switch (env->_major) {
  case ILU_C_NO_EXCEPTION:
    return 1;
  case ILU_C_USER_EXCEPTION:
    printf("%lu %s => user exn %s\n", TIME, what,
	   ILU_C_EXCEPTION_ID(env));
    break;
  case ILU_C_SYSTEM_EXCEPTION:{
      ILU_C_SYSEXN_BODY *stdbod;
      stdbod = (ILU_C_SYSEXN_BODY *) ILU_C_EXCEPTION_VALUE(env);
      printf("%lu %s => sys exn %s, minor=%lu, completed=%u\n",
	     TIME, what, ILU_C_EXCEPTION_ID(env),
	     stdbod->minor, stdbod->completed);
      break;
    }
  default:
    printf("%lu %s => Invalid exception!\n", TIME, what);
  }
  return 0;
}

static void
ImportEm(ChgUp_T * o1, ChgUp_T * o2)
{
  int             chg;
  ILU_C_ENVIRONMENT env;
  while (1) {
    if (destroy && *o1 != ILU_NIL) {
      ILU_C_DestroyObjectAndServer(*o1, &env);
      ShowEnv(&env, "DestroyObjectAndServer(*o1)");
      *o1 = ILU_NIL;
    }
    (*o1) = ILU_C_ReLookupObject("ChgUp-Server", "A",
				 ChgUp_T__MSType, &chg);
    if (*o1 == ILU_NIL) {
      printf("%lu Lookup 1 failed.\n", TIME);
    } else {
      printf("%lu Lookup 1 succeeded (chg=%d, sbh=%s).\n", TIME,
	     chg, ILU_C_SBHOfObject(*o1));
      if (destroy && *o2 != ILU_NIL) {
	ILU_C_DestroyObjectAndServer(*o2, &env);
	ShowEnv(&env, "DestroyObjectAndServer(*o2)");
	*o2 = ILU_NIL;
      }
      (*o2) = ILU_C_ReLookupObject("ChgUp-Server", "B",
				   ChgUp_T__MSType, &chg);
      if (*o2 == ILU_NIL)
	printf("%lu Lookup 2 failed.\n", TIME);
      else
	printf("%lu Lookup 2 succeeded (chg=%d, sbh=%s).\n", TIME,
	       chg, ILU_C_SBHOfObject(*o2));
    }
    if (*o1 == ILU_NIL || *o2 == ILU_NIL)
      sleep(1);
    else
      return;
  }
}

static void
DestroyEm(ChgUp_T * o1, ChgUp_T * o2)
{
  ILU_C_ENVIRONMENT env;
  ILU_C_DestroyObjectAndServer(*o1, &env);
  ShowEnv(&env, "DestroyObjectAndServer(*o1)");
  *o1 = ILU_NIL;
  ILU_C_DestroyObjectAndServer(*o2, &env);
  ShowEnv(&env, "DestroyObjectAndServer(*o2)");
  *o2 = ILU_NIL;
  return;
}

static ilu_refany theAlarm = ILU_NIL;
static int theStop = 0;

static void SetStop(ilu_private rock)
{
  ilu_ExitMainLoop(&theStop);
}

static void SleepByMainLoop(unsigned int period)
{
  ilu_FineTime    now, diff, then;
  now = ilu_FineTime_Now();
  diff = ilu_FineTime_FromDouble(period);
  then = ilu_FineTime_Add(now, diff);
  ilu_SetAlarm(theAlarm, then, SetStop, ILU_NIL);
  ilu_RunMainLoop(&theStop);
  return;
}

int
main(int ac, char **av)
{
  ChgUp_T         o1 = ILU_NIL, o2 = ILU_NIL, b1, b2;
  ILU_C_ENVIRONMENT env;
  unsigned int    period;
  ilu_cardinal    g1, g2, iter = 0;
  int             dosleep = 0;
  time_t          startT;

  if (ac != 4) {
    fprintf(stderr,
	    "Usage: %s <always: C-bool> "
	    "<destroy: {0, 1, 2}> <period: unsigned (secs)>\n",
	    av[0]);
    return 1;
  }
  if (sscanf(av[1], "%d", &always) != 1) {
    fprintf(stderr,
	    "Conversion of \"%s\" to <always: C-bool> failed!\n",
	    av[1]);
    fprintf(stderr, "A C-bool is a C int, in %%d format.\n");
    return 2;
  }
  if (sscanf(av[2], "%d", &destroy) != 1 || destroy < 0 || destroy > 2) {
    fprintf(stderr,
	    "Conversion of \"%s\" to {0, 1, 2} failed!\n",
	    av[2]);
    return 3;
  }
  if (sscanf(av[3], "%u", &period) != 1) {
    fprintf(stderr, "Conversion of \"%s\" to unsigned int failed!\n",
	    av[3]);
    return 4;
  }
  ChgUp__Initialize();
  theAlarm = ilu_CreateAlarm();
  printf("ChgUp test, always=%s, destroy=%d, period=%u\n",
	 always ? "T" : "F", destroy, period);
  startT = time(ILU_NIL);
  printf("%lu = %s", (unsigned long) startT, ctime(&startT));

  if (!always)
    ImportEm(&o1, &o2);
  while (1) {
    if (dosleep) {
#if 0
      sleep(period);
#else
      SleepByMainLoop(period);
#endif
    }
    printf("\n%lu At top of loop.\n", TIME);
    if (always) {
      int             ok;
      if (destroy == 1 && o1 != ILU_NIL && o2 != ILU_NIL) {
	ok = ILU_C_ValidateOrDestroyObjSvr(o1, &env);
	ShowEnv(&env, "ValidateOrDestroyObjSvr(o1)");
	if (ok) {
	  ok = ILU_C_ValidateOrDestroyObjSvr(o2, &env);
	  ShowEnv(&env, "ValidateOrDestroyObjSvr(o2)");
	  if (!ok)
	    o2 = ILU_NIL;
	} else
	  o1 = ILU_NIL;
      } else
	ok = 0;
      if (!ok)
	ImportEm(&o1, &o2);
    }
    b1 = ChgUp_T_GetBrother(o1, &env);
    if (!ShowEnv(&env, "GetBrother(o1)"))
      goto failed;
    b2 = ChgUp_T_GetBrother(o2, &env);
    if (!ShowEnv(&env, "GetBrother(o2)"))
      goto failed;
    g1 = ChgUp_T_GetGeneration(o1, &env);
    if (!ShowEnv(&env, "GetGeneration(o1)"))
      goto failed;
    g2 = ChgUp_T_GetGeneration(o2, &env);
    if (!ShowEnv(&env, "GetGeneration(o2)"))
      goto failed;
    printf("%lu Iteration %lu: g1=%lu, g2=%lu",
	   TIME, iter++, g1, g2);
    if (b1 != o2 || b2 != o1)
      printf(" (not brothers!)");
    printf("\n");
    if (always && destroy == 2)
      DestroyEm(&o1, &o2);
    dosleep = 1;
    continue;
failed:
    dosleep = 0;
    if (always)
      continue;
    if (destroy)
      DestroyEm(&o1, &o2);
    ImportEm(&o1, &o2);
  }
}

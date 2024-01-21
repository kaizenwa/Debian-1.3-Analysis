/* fprog.c */
/* $Id: fprog.c,v 1.10 1996/06/17 06:09:22 spreitze Exp $ */
/* Last edited by Mike Spreitzer June 16, 1996 11:07 pm PDT */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Fibber.h"

typedef
ilu_cardinal(*Proc) (ILU_C_OBJECT, ilu_cardinal, ilu_cardinal,
		     ILU_C_OBJECT, ILU_C_ENVIRONMENT *);

static char pad[] = "................................................";

static ilu_boolean pareval = ilu_FALSE;

static          ilu_boolean
DoCall(Fibber_T ask,
       ilu_cardinal d,
       ilu_cardinal n,
       Fibber_T self,
       ilu_cardinal * ans,
       ILU_C_ENVIRONMENT * Env)
{
  ILU_C_ENVIRONMENT lenv;
  Fibber_StringSeq *ss, *ssp;
  ILU_C_SET_SUCCESSFUL(Env);
  *ans = Fibber_T_Calc(ask, d, n, self, &lenv);
  switch (lenv._major) {
  case ILU_C_NO_EXCEPTION:
    return ilu_TRUE;
  case ILU_C_USER_EXCEPTION:
    if (ILU_C_EXCEPTION_ID(&lenv) == ex_Fibber_Failed) {
      char            buf[30];
      sprintf(buf, "Fib(%lu) =>", n);
      ss = (Fibber_StringSeq *) lenv.ptr;
      Fibber_StringSeq_Push(ss, (Fibber_String) ILU_C_Strdup(buf));
    } else {
      ss = Fibber_StringSeq_Create(2, NULL);
      Fibber_StringSeq_Append(ss,
	(Fibber_String) ILU_C_Strdup("Unexpected user exception"));
      Fibber_StringSeq_Append(ss,
	  (Fibber_String) ILU_C_Strdup(ILU_C_EXCEPTION_ID(&lenv)));
    }
    break;
  case ILU_C_SYSTEM_EXCEPTION:{
      ILU_C_SYSEXN_BODY *sysExn;
      char            buf[200];
      sysExn = (ILU_C_SYSEXN_BODY *) ILU_C_EXCEPTION_VALUE(&lenv);
      sprintf(buf, "%s(minor=%lu, completed=%d)",
	      ILU_C_EXCEPTION_ID(&lenv), sysExn->minor,
	      sysExn->completed);
      ss = Fibber_StringSeq_Create(2, NULL);
      Fibber_StringSeq_Append(ss,
		 (Fibber_String) ILU_C_Strdup("system exception"));
      Fibber_StringSeq_Append(ss, (Fibber_String) ILU_C_Strdup(buf));
      break;
    }
  default:
    ss = Fibber_StringSeq_Create(1, NULL);
    Fibber_StringSeq_Append(ss,
	   (Fibber_String) ILU_C_Strdup("Unexpected lenv._major"));
  }
  ssp = (Fibber_StringSeq *) ilu_must_malloc(sizeof(*ssp));
  *ssp = *ss;
  Env->_major = ILU_C_USER_EXCEPTION;
  Env->returnCode = ex_Fibber_Failed;
  Env->ptr = (void *) ssp;
  Env->freeRoutine = 0;
  return ilu_FALSE;
}

typedef struct {
  ilu_cardinal    d, n, ans;
  int             step;
  Fibber_T        asker, askee;
  ilu_Mutex       mu;
  ilu_Condition   chg;
  Fibber_StringSeq ss;
  ilu_boolean     done;
}               Problem;

static void Solve(void *arg)
{
  Problem        *p = (Problem *) arg;
  ILU_C_ENVIRONMENT lenv;
  ILU_ERRS((bad_locks, internal, broken_locks)) lerr;
  int             n = p->n, d = p->d;
  ilu_DebugPrintf("%0.*sfib(%d, d=%d): calling step %d.\n",
		  d, pad, n, d, p->step);
  p->ans = Fibber_T_Calc(p->askee, p->d + 1, p->n - p->step,
			 p->asker, &lenv);
  switch (lenv._major) {
  case ILU_C_NO_EXCEPTION:
    ilu_DebugPrintf("%0.*sfib(%d, d=%d):"
		    " call for step %d returned %lu.\n",
		    d, pad, n, d, p->step, (long unsigned) p->ans);
    Fibber_StringSeq_Init(&p->ss, 0, NULL);
    break;
  case ILU_C_USER_EXCEPTION:
    ilu_DebugPrintf("%0.*sfib(%d, d=%d):"
		    " call for step %d raised user exn %s.\n",
		    d, pad, n, d,
		    p->step, ILU_C_EXCEPTION_ID(&lenv));
    if (ILU_C_EXCEPTION_ID(&lenv) == ex_Fibber_Failed) {
      char            buf[30];
      sprintf(buf, "Fib(%lu) =>", p->n - p->step);
      p->ss = *(Fibber_StringSeq *) lenv.ptr;
      Fibber_StringSeq_Push(&p->ss, (Fibber_String) ILU_C_Strdup(buf));
    } else {
      Fibber_StringSeq_Init(&p->ss, 2, NULL);
      Fibber_StringSeq_Append(&p->ss,
	(Fibber_String) ILU_C_Strdup("Unexpected user exception"));
      Fibber_StringSeq_Append(&p->ss,
	  (Fibber_String) ILU_C_Strdup(ILU_C_EXCEPTION_ID(&lenv)));
    }
    break;
  case ILU_C_SYSTEM_EXCEPTION:{
      ILU_C_SYSEXN_BODY *sysExn;
      char            buf[200];
      ilu_DebugPrintf("%0.*sfib(%d, d=%d):"
		      " call for step %d raised sys exn %s.\n",
		      d, pad, n, d, p->step, ILU_C_EXCEPTION_ID(&lenv));
      sysExn = (ILU_C_SYSEXN_BODY *) ILU_C_EXCEPTION_VALUE(&lenv);
      sprintf(buf, "%s(minor=%lu, completed=%d)",
	      ILU_C_EXCEPTION_ID(&lenv), sysExn->minor,
	      sysExn->completed);
      Fibber_StringSeq_Init(&p->ss, 2, NULL);
      Fibber_StringSeq_Append(&p->ss,
		 (Fibber_String) ILU_C_Strdup("system exception"));
      Fibber_StringSeq_Append(&p->ss, (Fibber_String) ILU_C_Strdup(buf));
      break;
    }
  default:
    Fibber_StringSeq_Init(&p->ss, 1, NULL);
    ilu_DebugPrintf("%0.*sfib(%d, d=%d):"
		 " call for step %d set unexpected lenv._major.\n",
		    d, pad, n, d, p->step);
    Fibber_StringSeq_Append(&p->ss,
	   (Fibber_String) ILU_C_Strdup("Unexpected lenv._major"));
  }
  (void) ilu_EnterMutex(p->mu, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);	/* sheer laziness */
  p->done = ilu_TRUE;
  (void) ilu_CondNotify(p->chg, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  (void) ilu_ExitMutex(p->mu, ilu_TRUE, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return;
}

ilu_cardinal
server_Fibber_T_Calc(Fibber_T self,
		     ilu_cardinal d,
		     ilu_cardinal n,
		     Fibber_T ask,
		     ILU_C_ENVIRONMENT * Env)
{
  ilu_cardinal    f1, f2, ans = 0;
  ILU_ERRS((no_memory, internal)) lerr;
  ILU_C_SET_SUCCESSFUL(Env);
  if (n < 2) {
    ans = 1;
    ilu_DebugPrintf("%0.*sfib(%d, d=%d): returning 1.\n",
	       (int) d, pad, (int) n, (int) d);
#ifdef ILU_OS_THREADED
  } else if (pareval) {
    Problem         p1, p2;
    char            buf1[24], buf2a[24], buf2b[24];
    ilu_DebugPrintf("%0.*sfib(%d, d=%d): forking threads.\n",
		    (int) d, pad, (int) n, (int) d);
    p1.askee = p2.askee = ask;
    p1.asker = p2.asker = self;
    p1.d = p2.d = d;
    p1.n = p2.n = n;
    p1.step = 1;
    p2.step = 2;
    p1.done = p2.done = ilu_FALSE;
    Fibber_StringSeq_Init(&p1.ss, 0, NULL);
    Fibber_StringSeq_Init(&p2.ss, 0, NULL);
    sprintf(buf1, "%d", p1.d);
    sprintf(buf2a, "%d", p1.n);
    sprintf(buf2b, "%d", p1.n);
    p1.mu = ilu_CreateMutex(buf1, buf2a);
    p2.mu = ilu_CreateMutex(buf1, buf2b);
    p1.chg = ilu_CreateCondition(buf1, buf2a, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    p2.chg = ilu_CreateCondition(buf1, buf2b, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    (void) ilu_OSForkNewThread(Solve, &p1, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    (void) ilu_OSForkNewThread(Solve, &p2, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    (void) ilu_EnterMutex(p1.mu, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    while (!p1.done) {
      (void) ilu_CMWait1(p1.chg, p1.mu, &lerr);
      ILU_MUST_BE_SUCCESS(lerr);
    }
    (void) ilu_ExitMutex(p1.mu, ilu_TRUE, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    (void) ilu_EnterMutex(p2.mu, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    while (!p2.done) {
      (void) ilu_CMWait1(p2.chg, p2.mu, &lerr);
      ILU_MUST_BE_SUCCESS(lerr);
    }
    (void) ilu_ExitMutex(p2.mu, ilu_TRUE, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    (void) ilu_DestroyCondition(p1.chg);
    (void) ilu_DestroyCondition(p2.chg);
    if (p1.ss._length == 0 && p2.ss._length == 0) {
      ans = p1.ans + p2.ans;
      ilu_DebugPrintf("%0.*sfib(%d, d=%d): returning %lu.\n",
		      (int) d, pad, (int) n, (int) d,
		      (long unsigned) ans);
    } else {
      char            buf[100];
      Fibber_StringSeq *ssp;
      int             i;
      if (p1.ss._length == 0) {
	Fibber_StringSeq_Init(&p1.ss, 1, NULL);
	sprintf(buf, "fib(%d, %d)=%lu", (int) p1.d, (int) p1.n,
		(long unsigned) p1.ans);
	Fibber_StringSeq_Append(&p1.ss,
				(Fibber_String) ILU_C_Strdup(buf));
      }
      if (p2.ss._length == 0) {
	Fibber_StringSeq_Init(&p2.ss, 1, NULL);
	sprintf(buf, "fib(%d, %d)=%lu", (int) p2.d, (int) p2.n,
		(long unsigned) p2.ans);
	Fibber_StringSeq_Append(&p2.ss,
				(Fibber_String) ILU_C_Strdup(buf));
      }
      ssp = Fibber_StringSeq_Create(p1.ss._length + p2.ss._length + 1,
				    NULL);
      for (i = 0; i < p1.ss._length; i++)
	Fibber_StringSeq_Append(ssp, p1.ss._buffer[i]);
      Fibber_StringSeq_Append(ssp, (Fibber_String) ILU_C_Strdup("and"));
      for (i = 0; i < p2.ss._length; i++)
	Fibber_StringSeq_Append(ssp, p2.ss._buffer[i]);
      Env->_major = ILU_C_USER_EXCEPTION;
      Env->returnCode = ex_Fibber_Failed;
      Env->ptr = (void *) ssp;
      Env->freeRoutine = 0;
      ilu_DebugPrintf("%0.*sfib(%d, d=%d): raising Failed.\n",
		      (int) d, pad, (int) n, (int) d);
    }
#endif	/* defined(ILU_OS_THREADED) */
  } else {
    ilu_DebugPrintf("%0.*sfib(%d, d=%d): calling step 1.\n",
		    (int) d, pad, (int) n, (int) d);
    if (!DoCall(ask, d + 1, n - 1, self, &f1, Env))
      return 0;
    ilu_DebugPrintf("%0.*sfib(%d, d=%d): calling step 2.\n",
		    (int) d, pad, (int) n, (int) d);
    if (!DoCall(ask, d + 1, n - 2, self, &f2, Env))
      return 0;
    ans = f1 + f2;
    ilu_DebugPrintf("%0.*sfib(%d, d=%d): returning %lu.\n",
		    (int) d, pad, (int) n, (int) d,
		    (long unsigned) ans);
  }
  return (ans);
}

#define TISIZE 10

static char    *pinfo = NULL;
static char    *tinfo[TISIZE] = {"sunrpcrm", "tcp_0_0", ILU_NIL};
static ilu_Server s;
static ILU_C_OBJECT mine;
static ilu_string sbh;

static void 
Export(char *sid)
{
  s = ILU_C_InitializeServer(sid, NULL, pinfo, tinfo, ILU_NIL, ilu_TRUE);
  if (s == NULL) {
    fprintf(stderr, "Unable to create my ILU server!\n");
    exit(1);
  }
  mine = Fibber_T__CreateTrue("it", s, NULL);
  if (mine == NULL) {
    fprintf(stderr, "Unable to create my ILU object!\n");
    exit(1);
  }
  sbh = ILU_C_SBHOfObject(mine);
  printf("my SBH = '%s'\n", sbh);
}

int
main(int argc, char **argv)
{
  char           *progname = argv[0];
  ilu_boolean     threadit = ilu_FALSE;

  argv++;
  argc--;
  while (argc > 1) {
    if (strcmp(argv[0], "-c") == 0)
      break;
    else if (strcmp(argv[0], "-st") == 0) {
      threadit = pareval = ilu_FALSE;
      argv += 1;
      argc -= 1;
      continue;
    } else if (strcmp(argv[0], "-mt") == 0) {
      threadit = ilu_TRUE;
      pareval = ilu_FALSE;
      argv += 1;
      argc -= 1;
      continue;
    } else if (strcmp(argv[0], "-mte") == 0) {
      threadit = pareval = ilu_TRUE;
      argv += 1;
      argc -= 1;
      continue;
    } else if (strcmp(argv[0], "-p") == 0)
      pinfo = argv[1];
    else if (strcmp(argv[0], "-t") == 0) {
      int             j = 1;
      while ((j < argc) && (j < TISIZE) && (argv[j][0] != '-')) {
	tinfo[j - 1] = argv[j];
	j++;
      }
      tinfo[j - 1] = ILU_NIL;
      argv += j;
      argc -= j;
      continue;
    } else
      goto usage;
    argv += 2;
    argc -= 2;
  }
  if (argc < 1)
    goto usage;
  if (threadit) {
#ifdef ILU_OS_THREADED
    ILU_C_USE_OS_THREADS;
#else
    fprintf(stderr,
       "OS-supplied thread support not configured into ILU!\n");
    return 2;
#endif				/* threading */
  }
  Fibber__Initialize();
  Fibber__InitializeServer();
  if (strcmp(argv[0], "-c") == 0) {
    Fibber_T        peer;
    ILU_C_ENVIRONMENT env;
    ilu_cardinal    n, ans, i;
    Fibber_StringSeq *ss;
    if (argc != 2)
      goto usage;
    Export(NULL);
    n = atoi(argv[1]);
    if (n > 30) {
      fprintf(stderr, "%lu is an unreasonably large argument;"
	      " you don't want more than 46.\n",
	      (long unsigned) n);
      return 1;
    }
    peer = ILU_C_LookupObject("Fibber-Test-Server", "it",
			      Fibber_T__MSType);
    if (peer == NULL) {
      fprintf(stderr, "Unable to import peer!\n");
      exit(1);
    }
    ans = Fibber_T_Calc(peer, 1, n, mine, &env);
    switch (env._major) {
    case ILU_C_NO_EXCEPTION:
      printf("Fib(%lu) = %lu\n", n, ans);
      break;
    case ILU_C_USER_EXCEPTION:
      if (ILU_C_EXCEPTION_ID(&env) == ex_Fibber_Failed) {
	ss = (Fibber_StringSeq *) ILU_C_EXCEPTION_VALUE(&env);
	printf("Fib(%lu) => Failed %d", n, ss->_length);
	for (i = 0; i < ss->_length; i++)
	  printf(" \"%s\"", ss->_buffer[i]);
	printf("\n");
      } else {
	printf("Fib(%lu) => %s\n", n, ILU_C_EXCEPTION_ID(&env));
      }
      break;
    case ILU_C_SYSTEM_EXCEPTION:{
	CORBA_ex_body  *sysExn;
	sysExn = (CORBA_ex_body *) CORBA_exception_value(&env);
	printf("Fib(%lu) => SYSTEM %s(%lu, %d)\n", n,
	       ILU_C_EXCEPTION_ID(&env),
	       sysExn->minor, sysExn->completed);
	break;
      }
    default:
      printf("Fib(%lu) => Unexpected _major!\n", n);
    }
    return 0;
  } else if (strcmp(argv[0], "-s") == 0) {
    int             i;
    if (argc > 1)
      goto usage;
    Export("Fibber-Test-Server");
    if (ILU_C_PublishObject(mine) == NULL) {
      fprintf(stderr, "Can't publish object!\n");
      exit(1);
    }
    printf("Serving...\n");
    ILU_C_Run();
  } else
    goto usage;
usage:
  fprintf(stderr,
	"Usage: %s [-p pinfo] [-t tinfo [tinfo...]] [-st | -mt | -mte]"
	" (-s | -c n)\n",
	  progname);
  return 1;
}

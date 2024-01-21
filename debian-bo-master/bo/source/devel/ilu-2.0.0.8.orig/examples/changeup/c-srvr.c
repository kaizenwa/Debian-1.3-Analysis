/* $Id: c-srvr.c,v 1.8 1996/05/04 05:18:17 janssen Exp $ */
/* Last edited by Mike Spreitzer April 23, 1996 3:11 pm PDT */

#include <stdio.h>
#include <time.h>
#include "ChgUp.h"

#define TIME ((unsigned long) time(ILU_NIL))

static unsigned int period;
static ilu_FineTime periodFT;
static int      style;
/*
 * 0 = squeeze FD budget; 1 = change ports; 2 = close & restart
 * server
 */

static ilu_refany alarm;
static ilu_cardinal generation = 0;
static ilu_Server theServer = ILU_NIL;
static ChgUp_T  o1 = ILU_NIL, o2 = ILU_NIL;
static ilu_string p1 = ILU_NIL, p2 = ILU_NIL;

ChgUp_T
server_ChgUp_T_GetBrother(ChgUp_T self, ILU_C_ENVIRONMENT * env)
{
  ChgUp_T         b = (ChgUp_T) ChgUp_T__GetUserData(self);
  ILU_C_SET_SUCCESSFUL(env);
  return b;
}

ilu_cardinal
server_ChgUp_T_GetGeneration(ChgUp_T self, ILU_C_ENVIRONMENT * env)
{
  ILU_C_SET_SUCCESSFUL(env);
  return generation;
}

static char    *pinfo = ILU_NIL;
static char *  *tinfo = ILU_NIL;

static void CreateEm()
{
  theServer = ILU_C_InitializeServer("ChgUp-Server", ILU_NIL,
				     pinfo, tinfo, ILU_NIL, ilu_TRUE);
  o1 = ChgUp_T__CreateTrue("A", theServer, ILU_NIL);
  o2 = ChgUp_T__CreateTrue("B", theServer, o1);
  ChgUp_T__SetUserData(o1, o2);
  p1 = ILU_C_PublishObject(o1);
  p2 = ILU_C_PublishObject(o2);
  printf("A's SBH is %s\n", ILU_C_SBHOfObject(o1));
}

static void
PerAlarm(ilu_private rock)
{
  ilu_cardinal    fdb;
  ILU_C_ENVIRONMENT env;
  switch (style) {
  case 0:
    fdb = ilu_SetFDBudget(0);
    printf("%lu FD usage = %lu\n", TIME, fdb);
    fdb = ilu_SetFDBudget(32);
    break;
  case 1:
    fprintf(stderr, "Style 1 not implementable yet!\n");
    exit(1);
    break;
  case 2:
    printf("%lu Closing server...\n", TIME);
    ILU_C_CloseServer(theServer, ILU_NIL, ILU_NIL, ILU_NIL, &env);
    if (!ILU_C_SUCCESSFUL(&env))
      printf("%lu close => exn %s\n", TIME, ILU_C_EXCEPTION_ID(&env));
    CreateEm();
    printf("%lu Server restarted\n", TIME);
    break;
  default:
    fprintf(stderr, "Unexpected style %d\n", style);
  }
  ilu_SetAlarm(alarm, ilu_FineTime_Add(ilu_FineTime_Now(), periodFT),
	       PerAlarm, ILU_NIL);
  generation++;
}

int
main(int ac, char **av)
{
  time_t          startT;
  char           *progname = av[0];
  char *	  newtinfo[10] = { ILU_NIL };
  av++;
  ac--;
  while (ac > 3) {
    if (strcmp(av[0], "-p") == 0)
      pinfo = av[1];
    else if (strcmp(av[0], "-t") == 0)
      {
	int j = 0;
	av++;
	ac--;
	tinfo = newtinfo;
	while (ac > 2 && av[0][0] != '-') {
	  newtinfo[j++] = av[0];
	  av++;
	  ac--;
	};
	newtinfo[j] = ILU_NIL;
	continue;
      }
    else
      goto usage;
    ac -= 2;
    av += 2;
  }
  if (ac != 2)
    goto usage;
  if (sscanf(av[0], "%d", &style) != 1 || style < 0 || style > 2) {
    fprintf(stderr,
	    "Conversion of \"%s\" to {0, 1, 2} failed!\n",
	    av[0]);
    return 2;
  }
  if (sscanf(av[1], "%u", &period) != 1) {
    fprintf(stderr, "Conversion of \"%s\" to unsigned int failed!\n",
	    av[1]);
    return 3;
  }
  periodFT = ilu_FineTime_FromDouble(period);
  ChgUp__InitializeServer();
  CreateEm();
  alarm = ilu_CreateAlarm();
  ilu_SetAlarm(alarm, ilu_FineTime_Add(ilu_FineTime_Now(), periodFT),
	       PerAlarm, ILU_NIL);
  printf("ChgUp server, style=%d, period=%u\n", style, period);
  startT = time(ILU_NIL);
  printf("%lu = %s", (unsigned long) startT, ctime(&startT));
  ILU_C_Run();
  return -1;
usage:
  fprintf(stderr,
	  "Usage: %s [-p <pinfo>] [-t <tinfo>]", progname);
  fprintf(stderr,
	  " <style: {0, 1, 2}> <period: unsigned (secs)>\n");
  return 1;
}

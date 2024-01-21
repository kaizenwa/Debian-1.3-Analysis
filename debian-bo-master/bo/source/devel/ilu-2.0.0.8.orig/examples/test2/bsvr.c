/* bsvr.c */
/* $Id: bsvr.c,v 1.8 1996/06/17 06:09:22 spreitze Exp $ */
/* Last edited by Mike Spreitzer June 16, 1996 11:04 pm PDT */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>	/* for exit() */

#include "Batcher.h"

static Batcher_TimeSeq times;
static int verbose = 0;

void
server_Batcher_T_Send(Batcher_T self,
		      Batcher_Time s, ILU_C_ENVIRONMENT * Env)
{
  ilu_FineTime    now = ilu_FineTime_Now();
  Batcher_TimeRec nu;
  if (verbose)
    printf("Send(%lu:%lu -> %lu:%lu)\n",
	   (unsigned long) ILU_LONGCARD_HIGH_WORD(&s),
	   (unsigned long) ILU_LONGCARD_LOW_WORD(&s),
	   (unsigned long) now.ft_s, (unsigned long) now.ft_t);
  nu.s = s;
  ILU_LONGCARD_HIGH_WORD(&nu.r) = now.ft_s;
  ILU_LONGCARD_LOW_WORD(&nu.r) = now.ft_t;
  Batcher_TimeSeq_Append(&times, &nu);
  ILU_C_SET_SUCCESSFUL(Env);
  return;
}

Batcher_TimeSeq *
server_Batcher_T_Sync(Batcher_T self,
		      Batcher_Time s, ILU_C_ENVIRONMENT * Env)
{
  Batcher_TimeSeq *ans = Batcher_TimeSeq__alloc();
  ilu_cardinal    i;
  server_Batcher_T_Send(self, s, Env);
  Batcher_TimeSeq_Init(ans, times._length, NULL);
  for (i = 0; i < times._length; i++)
    Batcher_TimeSeq_Append(ans, &times._buffer[i]);
  times._length = 0;
  return ans;
}

int
main(int argc, char **argv)
{
  ilu_Server s;
  Batcher_T t;
  int i;
  ilu_string progname, sbh, pinfo = NULL;
  ilu_string tinfo[10] = { "sunrpcrm", "tcp_0_0", NULL };

  Batcher__InitializeServer();
  Batcher_TimeSeq_Init(&times, 50, NULL);
  progname = argv[0];
  argv++;
  argc--;
  for (i = 0;  i < argc;  i++) {
    if (strcmp(argv[i], "-p") == 0 && ((i+1) < argc))
      pinfo = argv[++i];
    else if (strcmp(argv[0], "-t") == 0 && ((i+1) < argc)) {
      int j = 0;
      ++i;
      while ((i < argc) && (argv[i][0] != '-'))
	tinfo[j++] = argv[i++];
      tinfo[j] = NULL;
    }
    else if (strcmp(argv[0], "-v") == 0)
      verbose = 1;
    else
      goto usage;
  }
  s = ILU_C_InitializeServer("Batcher-Server", NULL, pinfo, tinfo,
			     ILU_NIL, ilu_TRUE);
  if (s == NULL) {
    fprintf(stderr, "Unable to create ILU server!\n");
    exit(1);
  }
  t = Batcher_T__CreateTrue("it", s, NULL);
  if (t == NULL) {
    fprintf(stderr, "Unable to create ILU object!\n");
    exit(1);
  }
  if (ILU_C_PublishObject(t) == NULL) {
    fprintf(stderr, "Can't publish object!\n");
    exit(1);
  }
  sbh = ILU_C_SBHOfObject(t);
  printf("SBH = '%s'\n", sbh);
  ILU_C_Run();
usage:
  fprintf(stderr, "Usage: %s [-v] [-p pinfo] [-t tinfo [tinfo...]]\n", progname);
  return 1;
}

/* bclnt.c */
/* $Id: bclnt.c,v 1.10 1996/07/17 00:46:26 larner Exp $ */
/* Last edited by Mike Spreitzer June 16, 1996 11:06 pm PDT */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>	/* for exit() */

#ifndef WIN32
#include <unistd.h>	/* for sleep() */
#endif

#include "Batcher.h"

int
main(int argc, char *argv[])
{
  Batcher_T       t;
  unsigned        i, j, k, nsends = 0, nsyncs = 0;
  int             verbose = 0, retry = 0;
  ILU_C_ENVIRONMENT env;
  if (argc < 3)
    goto usage;
  if (sscanf(argv[1], "%u", &nsends) != 1)
    goto usage;
  if (sscanf(argv[2], "%u", &nsyncs) != 1)
    goto usage;
  for (i = 3; i < argc; i++) {
    if (strcmp(argv[i], "-v") == 0)
      verbose = 1;
    else if (strcmp(argv[i], "-r") == 0)
      retry = 1;
    else
      goto usage;
  }
  Batcher__Initialize();
  t = ILU_C_LookupObject("Batcher-Server", "it", Batcher_T__MSType);
  if (t == NULL) {
    fprintf(stderr, "Unable to import server!\n");
    exit(1);
  }
  for (i = 0; i < nsyncs; i++) {
    ilu_FineTime    now;
    Batcher_Time    tnow;
    Batcher_TimeSeq *ans;
    for (j = 0; j < nsends; j++) {
      now = ilu_FineTime_Now();
      ILU_LONGCARD_HIGH_WORD(&tnow) = now.ft_s;
      ILU_LONGCARD_LOW_WORD(&tnow) = now.ft_t;
      if (verbose)
	printf("Sending %lu:%lu\n", now.ft_s, now.ft_t);
      Batcher_T_Send(t, tnow, &env);
      if (!ILU_C_SUCCESSFUL(&env)) {
	fprintf(stderr, "Send() => %s\n", ILU_C_EXCEPTION_ID(&env));
	if (!retry)
	  exit(1);
      }
#if defined(WIN32)
      _sleep(1);
#else
      sleep(1);
#endif
    }
    now = ilu_FineTime_Now();
    ILU_LONGCARD_HIGH_WORD(&tnow) = now.ft_s;
    ILU_LONGCARD_LOW_WORD(&tnow) = now.ft_t;
    if (verbose)
      printf("Syncing %lu:%lu\n", now.ft_s, now.ft_t);
    ans = Batcher_T_Sync(t, tnow, &env);
    if (!ILU_C_SUCCESSFUL(&env)) {
      fprintf(stderr, "Sync() => %s\n", ILU_C_EXCEPTION_ID(&env));
      if (!retry)
	exit(1);
    }
    printf("Ans(%d) = %d\n", i, ans->_length);
    for (k = 0; k < ans->_length; k++)
      printf("\t%lu.%06lu->%lu.%06lu\n",
	     (unsigned long) ILU_LONGCARD_HIGH_WORD(&ans->_buffer[k].s),
	     (unsigned long) ILU_LONGCARD_LOW_WORD(&ans->_buffer[k].s),
	     (unsigned long) ILU_LONGCARD_HIGH_WORD(&ans->_buffer[k].r),
	     (unsigned long) ILU_LONGCARD_LOW_WORD(&ans->_buffer[k].r));
    Batcher_TimeSeq__Free(ans);
    printf("\n");
  }
  return 0;
usage:
  fprintf(stderr, "Usage: %s n-syncs n-sends-per-sync [-v] [-r]\n", argv[0]);
  return (1);
}

/* $Id: dclnt.c,v 1.5 1996/04/23 21:49:16 spreitze Exp $ */
/* Last edited by Mike Spreitzer April 23, 1996 2:45 pm PDT */

#include <stdio.h>

#include "Dialog.h"

static ILU_C_InterruptHandle intH = ILU_NIL;

static void
Timeout(ilu_private rock)
{
  ILU_C_InterruptHandleCalls(intH);
  return;
}

int
main(int argc, char *argv[])
{
  Dialog_T        t;
  ILU_C_ENVIRONMENT env;
  ilu_refany      timeout_alarm;
  ilu_FineTime    now, dt, timeout;
  if (argc != 2) {
    fprintf(stderr, "Usage: %s server-sbh\n", argv[0]);
    exit(1);
  }
  Dialog__Initialize();
  intH = ILU_C_NewInterruptHandle();
  timeout_alarm = ilu_CreateAlarm();
  t = Dialog_T__CreateFromSBH(argv[1], &env);
  if (t == NULL) {
    fprintf(stderr, "Unable to create ILU object!\n");
    exit(1);
  }
  ILU_C_SetObjectInterruptHandle(t, intH);
  dt = ilu_FineTime_FromDouble(60);
  for (;;) {
    char            buf[1000], *s1, *s2;
    unsigned char  *s1u, *s2u;
    printf("Next: ");
    s1 = fgets(buf, 1000, stdin);
    if (s1 != buf) {
      fprintf(stderr, "Error reading arg!\n");
      exit(1);
    }
    s1u = (unsigned char *) s1;
    now = ilu_FineTime_Now();
    timeout = ilu_FineTime_Add(now, dt);
    ilu_SetAlarm(timeout_alarm, timeout, Timeout, ILU_NIL);
    s2u = Dialog_T_M(t, s1u, &env);
    if (!ILU_C_SUCCESSFUL(&env)) {
      fprintf(stderr, "M() => %s\n", ILU_C_EXCEPTION_ID(&env));
    } else {
      s2 = (char *) s2u;
      printf("Answer is '%s'\n", s2);
    }
  }
}

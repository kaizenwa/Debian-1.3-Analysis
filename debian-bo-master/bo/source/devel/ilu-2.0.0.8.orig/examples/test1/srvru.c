/* $Id: srvru.c,v 1.5 1996/06/17 05:40:56 spreitze Exp $ */
/* Last edited by Mike Spreitzer June 16, 1996 10:29 pm PDT */

#include "srvr.h"

#include <stdio.h>
#include <math.h>
#include <string.h>

int main (int ac, char **av)
{
  char           *pinfo = NULL;
  ilu_string      tinfo_space[10] = {ILU_NIL};
  ilu_string     *tinfo = NULL, *newti = NULL;
  ilu_boolean     sec = 0, cred = 0, mt = 0;
  int             i = 1;
  while (i < ac) {
    if (strcmp(av[i], "-p") == 0) {
      if (i++ < ac)
	pinfo = av[i++];
      else
	goto usage;
    } else if (strcmp(av[i], "-t") == 0) {
      int             j = 0;
      tinfo = tinfo_space;
      ++i;
      while ((i < ac) && (av[i][0] != '-'))
	tinfo[j++] = av[i++];
      tinfo[j] = ILU_NIL;
    } else if (strcmp(av[i], "-st") == 0) {
      mt = 0; i++;
    } else if (strcmp(av[i], "-mt") == 0) {
      mt = 1; i++;
    } else if (strcmp(av[i], "-cred") == 0) {
      cred = 1; i++;
    } else if (strcmp(av[i], "-nosec") == 0) {
      cred = 0; i++;
      tinfo = DefaultTInfo(0);
    } else if (strcmp(av[i], "-sec") == 0) {
      cred = 1; i++;
      newti = DefaultTInfo(1);
      if (newti == NULL) {
	fprintf(stderr, "Security support not configured into ILU!\n");
	return -1;
      } else
	tinfo = newti;
    } else
      goto usage;
  }
  return doit(pinfo, tinfo, mt, cred);
usage:
  fprintf(stderr,
	  "Usage: %s [-t tinfo [tinfo...] [-cred] | -sec | -nosec ]"
	  " [-p pinfo] [-st | -mt]\n",
	  av[0]);
  return 2;
}

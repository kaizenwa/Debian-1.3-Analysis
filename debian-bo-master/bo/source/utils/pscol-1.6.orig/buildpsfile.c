#include "config.h"
#include "pscol.h"
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

char **parsevar(char *var);

/********************************************************************/
/*                                                                  */
/* buildpsfile: reads PS_COLORS and/or TOP_COLORS and writes <file> */
/*              will convert uids if told to                        */
/*                                                                  */
/********************************************************************/

void buildpsfile(char *filename, int type, int convuid, int convpls,
                 int convtyp, int convcol, int verbose)
{
 char *psv, *topv, **ps, **top;
 int cnt;
 FILE *pscol;

 if (verbose) fprintf(stderr, "Getting environment variables\n");
 psv = getenv("PS_COLORS");
 topv = getenv("TOP_COLORS");
 if ((type == 1 && psv == NULL) || (type == 2 && topv == NULL))
 {
  fprintf(stderr, "error: %s: can't read %s_COLORS environment variable",
          progname, type == 1?"PS":"TOP");
  exit(0);
 }
 else if (psv == NULL || topv == NULL) /* doesn't matter if the one we're */
 {                                /* doing is defined and the other isn't */
  fprintf(stderr, "error: %s: can't read PS_COLORS and/or TOP_COLORS "
                  "environment variable\n", progname);
  exit(0);
 }
 if ((pscol = fopen(filename, "w")) == NULL)
 {
  fprintf(stderr, "error: %s: cannot open %s\n", progname, filename);
  exit(0);
 }
 fprintf(pscol, "%s\n", NEW_FILE_HEADER);
 switch (type)
 {
  case 0: fprintf(pscol, "[PS]\n");
          if (verbose) fprintf(stderr, "Parsing variables\n");
          ps = parsevar(psv);
          if (convpls && verbose) fprintf(stderr, "Converting +'s\n");
          if (convpls) convplus(ps);
          if (convuid && verbose) fprintf(stderr, "Converting uids\n");
          if (convuid) convuids(ps);
          if (convcol && verbose) fprintf(stderr, "Converting colors\n");
          if (convcol) convcolor(ps);
          if (convtyp && verbose) fprintf(stderr, "Converting type\n");
          if (convtyp) convtype(ps);
          for (cnt = 0;ps[cnt];cnt++) fprintf(pscol, "%s\n", ps[cnt]);
          fprintf(pscol, "\n");
          if (!strcmp(psv, topv))
           fprintf(pscol, "[TOP]\n=PS\n");
          else
          {
           top = parsevar(topv);
           if (convpls) convplus(top);
           if (convuid) convuids(top);
           if (convcol) convcolor(top);
           if (convtyp) convtype(top);
           for (cnt = 0;top[cnt];cnt++) fprintf(pscol, "%s\n", top[cnt]);
          }
          break;
  case 1: fprintf(pscol, "[PS]\n");
          if (verbose) fprintf(stderr, "Parsing variables\n");
          ps = parsevar(psv);
          if (convpls && verbose) fprintf(stderr, "Converting +'s\n");
          if (convpls) convplus(ps);
          if (convuid && verbose) fprintf(stderr, "Converting uids\n");
          if (convuid) convuids(ps);
          if (convcol && verbose) fprintf(stderr, "Converting colors\n");
          if (convcol) convcolor(ps);
          if (convtyp && verbose) fprintf(stderr, "Converting type\n");
          if (convtyp) convtype(ps);
          for (cnt = 0;ps[cnt];cnt++) fprintf(pscol, "%s\n", ps[cnt]);
          break;
  case 2: fprintf(pscol, "[TOP]\n");
          if (verbose) fprintf(stderr, "Parsing variables\n");
          top = parsevar(topv);
          if (convpls && verbose) fprintf(stderr, "Converting +'s\n");
          if (convpls) convplus(top);
          if (convuid && verbose) fprintf(stderr, "Converting uids\n");
          if (convuid) convuids(top);
          if (convcol && verbose) fprintf(stderr, "Converting colors\n");
          if (convcol) convcolor(top);
          if (convtyp && verbose) fprintf(stderr, "Converting type\n");
          if (convtyp) convtype(top);
          for (cnt = 0;top[cnt];cnt++) fprintf(pscol, "%s\n", top[cnt]);
          break;
 }
 fclose(pscol);
}

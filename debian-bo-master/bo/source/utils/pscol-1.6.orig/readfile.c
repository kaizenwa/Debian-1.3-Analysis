#include "config.h"
#include "pscol.h"
#include <stdio.h>
#include <malloc.h>

/*******************************************************************/
/*                                                                 */
/* readfile: reads the pscolors file and returns a set of poiners  */
/*           to ps and top colors                                  */
/*                                                                 */
/*******************************************************************/

void readfile(char *filename, int useetcfile, colorrec *cols)
{
 FILE *pscol;
 int pscnt = 0, topcnt = 0, pscnt1 = 0, topcnt1 = 0, type = 0, ltype = 0, 
     cnt = 0;
 char *line;
 char **ps, **top;

 line = malloc(100); /* if you have a line longer than this you're screwed */
 if ((pscol = fopen(filename, "r")) == NULL)
  if (useetcfile)
   if ((pscol = fopen(ETC_FILE, "r")) == NULL)
   {
    fprintf(stderr, "error: %s: cannot read either %s or %s\n", progname,
            filename, ETC_FILE);
    exit(0);
   }
   else; /* dummy else */
  else
  {
   fprintf(stderr, "error: %s: cannot read %s\n", progname, filename);
   exit(0);
  }
 /* count the lines of [TOP] and [PS] */
 while (!feof(pscol))
 {
  for (cnt = 0;cnt < 100;cnt++) line[cnt] = 0;
  fgets(line, 99, pscol);
  if (line[strlen(line) - 1] == '\n') line[strlen(line) - 1] = 0;
  if (*line != '#' && *line) /* comment or blank line*/
   switch (type) {
    case 0: if (!strcmp(line, "[PS]")) type = 1;
            if (!strcmp(line, "[TOP]")) type = 2;
            if (!type)
            {
             fprintf(stderr, "error: %s: inproper defines: %s\n", progname,
                     filename);
             exit(0);
            }
            break;
    case 1: if (!strcmp(line, "[PS]"))
            {
             fprintf(stderr, "error: %s: [PS] already defined: %s\n",
                     progname, filename);
             exit(0);
            }
            if (!strcmp(line, "[TOP]"))
             if (ltype == 2)
             {
              fprintf(stderr, "error: %s: [TOP] already defined: %s\n",
                      progname, filename);
              exit(0);
             }
             else
             {
              ltype = 1; 
              type = 2;
             }
            else
             pscnt++;
            break;
    case 2: if (!strcmp(line, "[TOP]"))
            {
             fprintf(stderr, "error: %s: [TOP] already defined: %s\n",
                     progname, filename);
             exit(0);
            }
            if (!strcmp(line, "[PS]"))
             if (ltype == 1)
             {
              fprintf(stderr, "error: %s: [PS] already defined: %s\n",
                      progname, filename);
              exit(0);
             }
             else
             {
              ltype = 2;
              type = 1;
             }
            else
             topcnt++;
            break;
   }
 }
 if (!type)
 {
  fprintf(stderr, "error: %s: error, nothing defined: %s\n", progname,
          filename);
  exit(0);
 }
 ps = malloc((pscnt * 4) + 4);
 top = malloc((topcnt * 4) + 4);
 rewind(pscol);
 type = 0;
 while (!feof(pscol))
 {
  for (cnt = 0;cnt < 100;cnt++) line[cnt] = 0;
  fgets(line, 99, pscol);
  if (line[strlen(line) - 1] == '\n') line[strlen(line) - 1] = 0;
  if (*line != '#')
  {
   for (cnt = 0;line[cnt] && line[cnt] != '#';cnt++);
   line[cnt--] = 0;
   for (;cnt >= 0 && (line[cnt] == ' ' || line[cnt] == 9);cnt--);
   line[cnt + 1] = 0;
  }
  if (*line != '#' && *line) /* comment or space */
   switch (type) {
    case 0: if (!strcmp(line, "[PS]")) type = 1;
            if (!strcmp(line, "[TOP]")) type = 2;
            break;
    case 1: if (!strcmp(line, "[TOP]")) 
             type = 2;
            else
            {
             ps[pscnt1] = malloc(strlen(line) + 1);
             strcpy(ps[pscnt1++], line);
            }
            break;
    case 2: if (!strcmp(line, "[PS]"))
             type = 1;
            else
            {
             top[topcnt1] = malloc(strlen(line) + 1);
             strcpy(top[topcnt1++], line);
            }
            break;
   }
 }
 fclose(pscol);
 ps[pscnt1] = 0;
 top[topcnt1] = 0;
 if (!*ps && *top && !strcmp(*top, "=PS"))
 {
  fprintf(stderr, "error: %s: ps not defined: %s\n", progname, filename);
  exit(0);
 }
 if (*ps && !*top && !strcmp(*ps, "=TOP"))
 {
  fprintf(stderr, "error: %s: top not defined: %s\n", progname, filename);
  exit(0);
 }
 if ( ((*ps && *top) && (!strcmp(*top, "=PS") && !strcmp(*ps, "=TOP"))) ||
     (*top && !strcmp(*top, "=TOP")) || (*ps && !strcmp(*ps, "=PS")) 
    )
 {
  fprintf(stderr, "error: %s: infinit loop detected: %s\n", progname, filename);
  exit(0);
 }
 if (*top && !strcmp(*top, "=PS"))
 {
  for (cnt = 0;top[cnt];cnt++) free(top[cnt]);
  free(top);
  top = ps;
 }
 if (*ps && !strcmp(*ps, "=TOP"))
 {
  for (cnt = 0;ps[cnt];cnt++) free(ps[cnt]);
  free(ps);
  ps = top;
 }
 cols->ps = ps;
 cols->top = top;
}

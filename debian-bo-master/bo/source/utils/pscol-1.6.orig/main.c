#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include "config.h"
#include "pscol.h"

#ifndef PS_COL_VER
#error PS_COL_VER not defined, use make instead of gcc
#endif

char *progname;

void main(int argc, char **argv)
{
 int verbose = 0,  /* verbose output to stderr */
     cshstyle = 0, /* use csh's "SETENV" instead of bash style export */
     newfile = 0,  /* use file instead of default */
     help = 0,     /* Give this idiot some help */
     output = 0,   /* Output variable as value, not var=val */
     create = 0,   /* create file from top/ps var */
     set = 0,      /* set specific variable only */
     convuid = 1,  /* used with create..  convert uids or not, default to do */
     convpls = 1,  /* same as above, but converts the current uid to a + */
     convtyp = 1,  /* convert the markup type from a # to a word */
     convcol = 1,  /* convert the ansi color to words */
     type = 0,     /* PS (1) or TOP (2) default is no type */
     version = 0,  /* display version */
     useetc = 1,   /* use /etc/pscolors set to 0 if create is specified a file */
     deffile = 1,
     cnt;
 char *filename;
 char *defaultfile;
 char *home;
 char **ps, **top;
 colorrec *cols;

 cols = malloc(sizeof(colorrec));
 progname = argv[0];
 defaultfile = malloc(100); /* you got a home dir this long? */
 home = getenv("HOME");
 if (home) sprintf(defaultfile, "%s/%s", home, PERSONAL_FILE);
 if (argc > 1)
 {
  argc--;
  argv++;
  while (argc)
  {
   if (argv[0][0] != '-')
   {
    fprintf(stderr, "error: %s: unknown parameter: %s\n", progname, argv[0]);
    exit(0);
   }
   (*argv)++;
   switch (**argv)
   {
    case 'C': cshstyle = 1;
              break;
    case 'V': verbose = 1;
              break;
    case 'c': (*argv)++;
              create = 1;
              for (;**argv;(*argv)++)
               switch (**argv)
               {
                case 'c': convcol = 0;
                          break;
                case '+': convpls = 0;
                          break;
                case 'm': convtyp = 0;
                          break;
                case 'p': if (type)
                          {
                           fprintf(stderr, "error: %s: make up your mind, t or"
                                   "p?\n", progname);
                           exit(0);
                          }
                          type = 1;
                          break;
                case 't': if (type)
                          {
                           fprintf(stderr, "error: %s: make up your mind, t or"
                                   "p?\n", progname);
                           exit(0);
                          }
                          type = 2;
                          break;
                case 'u': convuid = 0;
                          break;
               }
              break;
    case 'f': newfile = 1;
              argv++;
              argc--;
              deffile = useetc = 0;
              if (*argv == NULL)
              {
               fprintf(stderr, "error: %s: no file specified\n", progname);
               exit(0);
              }
              filename = *argv;
              break;
    case 'h': help = 1;
              break;
    case 'o': output = 1;
              (*argv)++;
              if (**argv != 'p' && **argv != 't')
              {
               fprintf(stderr, "error: %s: -o requires p or t only\n",
                       progname);
               exit(0);
              }
              type = (**argv == 'p'?1:2);
              break;
    case 's': set = 1;
              (*argv)++;
              if (**argv != 'p' && **argv != 't')
              {
               fprintf(stderr, "%s: -s requres p or t only\n", progname);
               exit(0);
              }
              type = (**argv == 'p'?1:2);
              break;
    case 'v': version = 1;
              break;
    default:  fprintf(stderr, "error: %s: unknown option %s\n",
                      progname, *argv);
              exit(0);
   }
   argv++;
   argc--;
  }
 }
 if (help || version)
 {
  printf("%s -- version %s\n", progname, PS_COL_VER);
  if (version) exit(0);
 }
 if (help)
 {
  printf("\nusage: eval `%s [-s<p|t>] [-C] [-f <file>] [-V]`\n",
         progname);
  printf(" or    VAR=`%s [-o<p|t>] [-f <file>] [-V]`\n",
         progname);
  printf(" or    %s [-c[u][+][c][m][t|p]] [-f <file>] [-V]\n", progname);
  printf(" or    %s [-h] [-v]\n\n", progname);
  printf("       -o<p|t> output the value without var= (p = ps, t = top)\n"
         "          this is to do something like NEWCOL=`<prog/param>`\n"
         "       -C use csh style SETENV instead of export\n"
         "       -f use <file> instead of ~/.pscolors or /etc/pscolors\n"
         "       -s<p|t> set this var only (p = ps, t = top)\n"
         "          use this as: eval `<prog/params>`\n"
         "       -V verbose output (to stderr)\n"
         "       -c[u][+][c][m][p|t] read the environment and write the file\n"
         "          'u' doesn't convert uids to names\n"
         "          '+' doesn't convert the current UID to a +\n"
         "          'c' doesn't convert the ansi color to readable format\n"
         "          'm' doesn't convert the markup type to readable format\n"
         "          'p' = ps or 't' = top\n"
         "       -h How'd you know this was an option?  anyway you're\n"
         "          looking at it =)\n"
         "       -v display version and exit\n"
         "NOTE: more info in pscol.doc\n");
  exit(0);
 }
 if (create && (output || set || cshstyle))
 {
  fprintf(stderr, "error: %s: -c can't be used with -o, -s, or -C\n",
           progname);
  exit(0);
 }
 if (output && (set || cshstyle))
 {
  fprintf(stderr, "error: %s: -o and -", progname);
  if (set) fprintf(stderr, "s"); else fprintf(stderr, "C");
  fprintf(stderr, " can't be used together\n");
  exit(0);
 }
 if (deffile) if (home)
  filename = defaultfile;
 else
 {
  fprintf(stderr, "error: %s: HOME env var not found and no file specified",
          progname);
  exit(0);
 }
 if (create) 
 {
  if (verbose) fprintf(stderr, "Starting to make %s\n", filename);
  buildpsfile(filename, type, convuid, convpls, convtyp, convcol, verbose);
  exit(0);
 }
 /* since there's only 1 way for the build thing and more than 1 for the
    var=val part, I have to do it differently, put the common parts here
    and then work from there... don't understand this comment? don't
    worry  =) */
 if (verbose) fprintf(stderr, "Reading %s...\n", filename);
 if (verbose && useetc) 
  fprintf(stderr, "If not found, I'll read %s\n", ETC_FILE);
 readfile(filename, useetc, cols);
 ps = cols->ps;
 top = cols->top;
 convname(ps);
 if (ps != top) convname(top);
 if (output)
 {
  if (verbose)
   fprintf(stderr, "Outputting value for %s\n", type == 1?"PS":"TOP");
  if ((type == 1 && !*ps) || (type == 2 && !*top))
  {
   fprintf(stderr, "error: %s: requested variable not defined\n", progname);
   exit(0);
  }
  printf("\"%s\"\n", buildvar(type == 1?ps:top));
  exit(0);
 }
 if (set)
 {
  if (verbose)
  {
   fprintf(stderr, "Outputting environment variable %s_COLORS",
           type == 1?"PS":"TOP");
   if (cshstyle) fprintf(stderr, " csh style");
   fprintf(stderr, "\n");
  }
  if ((type == 1 && !*ps) || (type == 2 && !*top))
  {
   fprintf(stderr, "error: %s: requested variable not defined\n", progname);
   exit(0);
  }
  printf("%s %s_COLORS=\"%s\"\n", cshstyle?"SETENV":"export",
         type == 1?"PS":"TOP", buildvar(type == 1?ps:top));
  exit(0);
 }
 /* if we get here, they want both set */
 if (verbose)
 {
  fprintf(stderr, "Outputting environment variables");
  if (cshstyle) fprintf(stderr, " csh style");
  fprintf(stderr, "\n");
 }
 if (*ps) printf("%s PS_COLORS=\"%s\"\n", cshstyle?"SETENV":"export",
                 buildvar(ps));
 if (*top) printf("%s TOP_COLORS=\"%s\"\n", cshstyle?"SETENV":"export",
                  buildvar(top));
}

/*
 if you haven't noticed by now, I've put all my procedures/functions in
 seperate files...  Makes it easier for me to read. besides, it's not a
 multiple utility package <g>
*/

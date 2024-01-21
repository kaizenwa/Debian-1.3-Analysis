/* vtmain.c - main program for variant translators */

#include "../h/gsupport.h"
#include "tproto.h"
#include "lfile.h"

#ifdef strlen
#undef strlen				/* pre-defined in some contexts */
#endif					/* strlen */

/*
 *  Prototypes.
 */

hidden	novalue	report	Params((char *s));
hidden	novalue	usage	Params((noargs));

/*
 *  Define global variables.
 */

#define Global
#define Init(v) = v
#include "tglobals.h"

/*
 * The following code is operating-system dependent [@tmain.01].
 *  -- include system-dependent files and declarations
 *  -- define the legal options and a corresponding usage message fragment
 *  -- define the extension used for icode files
 */

#if PORT
#define OPTIONS "Ps"
#define USAGE "[-Ps]"
#endif					/* PORT */

#if AMIGA
#include <dos.h>
#define OPTIONS "Ps"
#define USAGE "[-Ps]"
#endif					/* AMIGA */

#if ATARI_ST
#define OPTIONS "Ps"
#define USAGE "[-Ps]"
#endif					/* ATARI_ST */

#if HIGHC_386
   /* not yet implemented */
#endif					/* HIGHC_386 */

#if MACINTOSH
#define OPTIONS "Ps"
#define USAGE "[-Ps]"
#if MPW
#endif					/* MPW */
#if LSC
#endif					/* LSC */
#endif					/* MACINTOSH */

#if MSDOS
#define OPTIONS "Ps"
#define USAGE "[-Ps]"
#endif					/* MSDOS */

#if UNIX
#define OPTIONS "Pms"
#define USAGE "[-Pms]"
#endif					/* UNIX */

#if VMS
#define OPTIONS "Ps"
#define USAGE "[-Ps]"
#endif					/* VMS */

#if VM || MVS
#endif					/* VM || MVS */

/*
 * End of operating-system specific code.
 */

/*
 * getopt() variables
 */
extern int optindex;		/* index into parent argv vector */
extern int optopt;		/* character checked for validity */
extern char *optarg;		/* argument associated with option */

int output_line = 1;            /* flag: output #line */

/*
 * other variables expected by shared code
 */
struct lfile *lfiles = NULL;
char *ofile = NULL;


/*
 *  main program
 */
novalue main(argc,argv)
int argc;
char **argv;
   {
   int errors = 0;			/* translator errors */
   char **tfiles, **tptr;		/* list of files to translate */
   char buf[MaxFileName];		/* file name construction buffer */
   int c, n;
   struct fileparts *fp;

   progname = "vitran";

   /*
    * Process options.
    */
   while ((c = getopt(argc,argv,OPTIONS)) != EOF)
       switch (c) {
         case 'P':			/* -P: don't output #line */
            output_line = 0;
            break;
         case 'm':			/* -m: preprocess using m4(1) [UNIX] */
            m4pre = 1;
            break;
         case 's':			/* -s: suppress informative messages */
            silent = 1;
            break;
         default:
            usage();
         }

   /*
    * Allocate space for lists of file names.
    */
   n = argc - optindex + 1;
   tptr = tfiles = (char **)alloc((unsigned int)(n * sizeof(char *)));

   /*
    * Scan file name arguments.
    */
   while (optindex < argc)  {
      if (strcmp(argv[optindex],"-") == 0)
         *tptr++ = "-";				/* "-" means standard input */
      else {
         fp = fparse(argv[optindex]);		/* parse file name */
         if (*fp->ext == '\0' || smatch(fp->ext,".icn")) {
            makename(buf,(char*)NULL,argv[optindex],".icn");
            *tptr++ = salloc(buf);		/* translate the .icn file */
            }
         else
            quitf("bad argument %s",argv[optindex]);
         }
      optindex++;
      }

   *tptr = NULL;			/* terminate filename lists */
   if (tptr == tfiles)
      usage();				/* error -- no files named */

   /*
    * Translate .icn files
    */
   if (!silent)
      report("Translating");
   errors = trans(tfiles);
   if (errors > 0)			/* exit if errors seen */
      exit(ErrorExit);

   exit(NormalExit);
   }

static novalue report(s)
char *s;
   {

/*
 * The following code is operating-system dependent [@tmain.03].  Report
 *  phase.
 */

#if PORT
   fprintf(stderr,"%s:\n",s);
#endif					/* PORT */

#if AMIGA || ATARI_ST || HIGHC_386 || MSDOS || UNIX || VMS
   fprintf(stderr,"%s:\n",s);
#endif					/* AMIGA || ATARI_ST || HIGHC_386 ... */

#if MACINTOSH
#if MPW
   printf("Echo %s:\n",s);
#endif					/* MPW */
#if LSC
   fprintf(stderr,"%s:\n",s);
#endif					/* LSC */
#endif					/* MACINTOSH */

#if VM || MVS
#endif					/* VM || MVS */

/*
 * End of operating-system specific code.
 */

   }

/*
 * Print an error message if called incorrectly.  The message depends
 *  on the legal options for this system.
 */
static novalue usage()
   {
   fprintf(stderr,"usage: %s %s file ...\n", progname, USAGE);
   exit(ErrorExit);
   }

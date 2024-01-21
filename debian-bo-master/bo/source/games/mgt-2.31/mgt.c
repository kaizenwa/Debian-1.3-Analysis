/* "mgt" Copyright (c) 1991 Shodan  */


#include <signal.h>
#include "mgt.h"

int retVal = 0;
interface *io;			/* current interface routines */
int boardsize;			/* global for board size */
char *info[LASTINFO - FIRSTINFO + 2];
int handicap;
char *commentBuf;		/* pointer to current node's comment */
extern interface asciiInterface;
int prisoners[2];
FILE *input = 0;
char name_buf[512];
char *files[MAX_FILES];
int filecount, currentfile;

#ifdef DEBUG
FILE *debug;
unsigned long totalmemory;
#endif

#ifdef MGT_IBM
extern unsigned _stklen = 32000;
#endif

typedef struct {
   char *arg;
   int *flag;
   char **str;
}  argType;


int mailFlag = 0;
int saveShort = 0;
int tutor = 0;
char *saveName;



static argType argTable[] =
{
   {"-m", &mailFlag, &saveName},
   {"-s", &saveShort, NULL},
   {"-t", &tutor, NULL}
};


FUNCTION main(argc, argv)
int argc;
char **argv;
{

#ifdef DEBUG
   debug = fopen("debug", "w");
#endif

   io = &asciiInterface;
   input = stdin;
   init(argv, &argc);
   parseLine(argc, argv);
   doit();
   myexit();
}



FUNCTION void die()
{
   signal(SIGINT, SIG_DFL);
   myexit();
}


FUNCTION void myexit()
{
   (*io->clearScreen) ();
   (*io->refreshIO) ();
   (*io->close) ();
   printf("Thank you for using mgt\n");
   exit(retVal);
}



FUNCTION void openfile(f)
char *f;
{
#ifdef MGT_LIB
   static char game_lib[] = MGT_LIB;
#endif				/* MGT_LIB */
   strcpy(name_buf, f);
   if (!(input = fopen(f, "r")))
      strcpy(name_buf, "");
#ifdef MGT_LIB
   /* if not in local directory, try library */
   if (!input) {
      strcpy(name_buf, game_lib);
      strcat(name_buf, f);
      input = fopen(name_buf, "r");
   }
#endif				/* MGT_LIB */
}


FUNCTION void barf(s)
char *s;
{
   int i;
   for (i = 24; i--;)
      fprintf(stderr, "\n");
   fprintf(stderr, "%s\n", s);
   exit(1);
}


FUNCTION void initEnv()
{
   extern char *getenv();
   char *env;

   if (env = getenv("MGT"))
      while (*env) {
	 while (*env && *env != '_')
	    env++;
	 if (*env)
	    env++;
	 if (*env)
	    (*io->readEnv) (&env);
      }
}


FUNCTION void init(argv, argc)
char *argv[];
int *argc;
{
   initEnv();
   (*io->open) (argv, argc);
   signal(SIGINT, die);
   readInit();
}



FUNCTION void helpCommandLine(errMesg)
char *errMesg;
{
   (*io->close) ();
   fprintf(stderr, "\nERROR: %s\n\n", errMesg);
   fprintf(stderr, "                   MGT %s\n", VERSION);
   fprintf(stderr, "\nUsage: mgt [opts] [files]\n");
   fprintf(stderr, "[opts] is any of:\n");
   fprintf(stderr, "-m filename        mail mode.  Autosave on quit.\n");
   fprintf(stderr, "-s                 use short format when saving.\n\n");
   exit(2);
}


FUNCTION void parseLine(argc, argv)
int argc;
char **argv;
{
   int i;

   filecount = 0;

   while (++argv, --argc > 0) {
      if (**argv != '-') {
	 files[filecount++] = *argv;
	 if (filecount == MAX_FILES)
	    helpCommandLine("Too many files specified");
      } else {
	 for (i = 0; i < sizeof(argTable) / sizeof(argType); i++)
	    if (!strcmp(argTable[i].arg, *argv)) {
	       if (argTable[i].str)
		  if (--argc)
		     *(argTable[i].str) = (*++argv);
		  else
		     helpCommandLine("String expected on option");
	       (*(argTable[i].flag))--;
	       break;
	    }
	 if (i == sizeof(argTable) / sizeof(argType))
	    helpCommandLine("Bad option");
      }
   }
   if (!filecount)
      input = stdin;
   else {
      currentfile = 0;
      openfile(files[0]);
      if (!input)
	 helpCommandLine("File not found");
   }

}

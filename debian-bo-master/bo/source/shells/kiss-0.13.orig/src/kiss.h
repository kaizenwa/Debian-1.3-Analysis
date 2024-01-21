/* includes */
#include <ctype.h> 
#include <errno.h>
#include <fcntl.h> 
#include <glob.h>
#include <grp.h> 
#include <malloc.h>
#include <pwd.h>
#include <regex.h> 
#include <signal.h> 
#include <stdarg.h> 
#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <time.h> 
#include <unistd.h>
#include <utime.h>
#include <sys/mount.h> 
#include <sys/stat.h> 
#include <sys/types.h>
#include <sys/wait.h>

#ifdef USE_READLINE
#   include <readline/readline.h>
#   include <readline/history.h>
#endif

#ifdef USE_GETLINE
#   include <getline.h>
#endif

/* types and such */
typedef struct
{
    char **str;
    int nstr;
} Stringstack;

/* flags for kiss program */
typedef struct
{
    int debug	    : 1;
    int ctrlc	    : 1;
    int version	    : 1;
    int supressstat : 1;
    int noenviron   : 1;
    int controlkids : 1;
} Flags;

/* flags for cp program */
typedef struct
{
    int verbose	    : 1;
    int recursive   : 1;
    int preserve    : 1;
    int noderef	    : 1;
    int interactive : 1;
} CpFlags;

/* flags for echo program */
typedef struct
{
    int nonewline   : 1;
    int escape	    : 1;
} EchoFlags;

/* flags for mv program */
typedef struct
{
    int interactive : 1;
    int verbose	    : 1;
    int protect	    : 1;
} MvFlags;

/* flags for rm program */
typedef struct
{
    int verbose	    : 1;
    int interactive : 1;
    int recursive   : 1;
    int forced	    : 1;
} RmFlags;

/* flags for grep program */
typedef struct
{
    int ignorecase  : 1;
    int reverse	    : 1;
} GrepFlags;

/* flags for ls program */
typedef struct
{
    int longoutput  : 1;
    int oneperline  : 1;
    int column	    : 1;
    int listtype    : 1;
    int showall	    : 1;
} LsFlags;

typedef enum
{
    none,
    writeto,
    appendto,
    readfrom,
} Redirect;

typedef struct
{
    char
	*cmdname;
    int
	(*cmd)(Stringstack s);
    int
	firstlevel;
} Cmdtable;

/* line buffer length */
#define LINELEN 100

/* max file name len, including directory */
#define FILENAMELEN 2028

/* buffer when copying files etc. */
#define BUFSIZE (10*1024)

/* indices for pipes */
#define P_SUCK 0
#define P_BLOW 1

/* alternate stdin file, when stdin is redirected */
#define DEVTTY	"/dev/tty"

/* # of history commands that are remembered */
#define MAXHIST 100

/* creation flags for directories and such */
#define CREATEFLAGS (S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH)

/* # of bytes that "cp" should consider as a 'hole', reported with -v */
#define HOLESIZE 1024

/* system-wide startup file */
#define SYSTEMRC    "/etc/kiss.rc"
/* user-owned startup file, relative to $HOME */
#define USERRC	    ".kissrc"

/* file where mount/umount store the mountpoints */
#define MTAB	    "/etc/mtab"

/* variables */
extern Stringstack *alias;			/* table of aliases */
extern int nalias;				/* # entries in table */
extern char bufferedinput [];			/* buffered stuff to run */
extern Cmdtable cmdtable [];			/* table of internal cmds */
extern Flags flags;				/* overall kiss flags */
extern Stringstack *hislist;			/* table of history */
extern int nhislist;				/* # entries in history */
extern char homedir [];				/* user homedir */
extern int inputparsed;				/* parser done anything? */
extern int lastchildpid;			/* $!: last child's pid */
extern int lastfile;				/* last parsed script */
extern int laststatus;				/* $?: last child's exitstat */
extern int orgargc;				/* original argc to main() */
extern char **orgargv;				/* and argv */
extern char *progname;				/* bare progname ("kiss") */
extern int shlvl;				/* shell level */
extern char username [];			/* username of invoker */
extern char *yytext;				/* lexer buffer */
extern FILE *yyin;				/* lexer input file */

/* functions */
extern Stringstack addstring (Stringstack a, Stringstack b);
extern void addstringstack (Stringstack *what, Stringstack addition);
extern void addstringtostack (Stringstack *what, char *extrastring);
extern void addtoenv (char *var, char *setting);
extern void addtohistory (Stringstack cmd);
extern void banner ();
extern void catfile (FILE *inf, char *name, int showname, int printonly);
extern void clearstack (Stringstack *what);
extern Stringstack copystringstack (Stringstack src, int first, int last);
extern void command (int argc);
extern int copydirtodir (char *srcdir, char *destdir, CpFlags fl,
			 int makethedir);
extern int copyfiletodir (char *file, char *dir, CpFlags fl);
extern int copyfiletofile (char *src, char *dest, CpFlags fl);
extern void debug (char *msg, ...);
extern int doalias (Stringstack s);
extern int docat (Stringstack s);
extern int docd (Stringstack s);
extern int dochgrp (Stringstack s);
extern int dochmod (Stringstack s);
extern int dochown (Stringstack s);
extern int docp (Stringstack s);
extern int doecho (Stringstack s);
extern int doexec (Stringstack s);
extern int dogrep (Stringstack s);
extern int dohelp (Stringstack s);
extern int dohistory (Stringstack s);
extern int dokill (Stringstack s);
extern int dokiss (int argc, char **argv);
extern int doln (Stringstack s);
extern int dols (Stringstack s);
extern int domkdir (Stringstack s);
extern int domknod (Stringstack s);
extern int domore (Stringstack s);
extern int domount (Stringstack s);
extern int domv (Stringstack s);
extern int doprintenv (Stringstack s);
extern int dopwd (Stringstack s);
extern int doquit (Stringstack s);
extern int doread (Stringstack s);
extern int dorecall (Stringstack s);
extern int dorm (Stringstack s);
extern int dormdir (Stringstack s);
extern int dotouch (Stringstack s);
extern int dosetenv (Stringstack s);
extern int dosleep (Stringstack s);
extern int dosource (Stringstack s);
extern int doumount (Stringstack s);
extern int dounsetenv (Stringstack s);
extern int dover (Stringstack s);
extern int dowc (Stringstack s);
extern int dowhere (Stringstack s);
extern void dumpstack (char *msg, Stringstack what);
extern void error (char *msg, ...);
extern Stringstack expandalias (Stringstack s);
extern Stringstack expandbackquotes (Stringstack s);
extern void expandtilde (Stringstack s);
extern int expandvars (char *name, char *destbuf);
extern int file2file (FILE *inf, FILE *outf);
extern void getbasename (char *file, char *basename);
extern void getprogname (char *av0);
extern char *getprompt ();
extern int getinput (FILE *stdinfile);
extern int isalias (char *cmd);
extern int isinternal (char *cmd);
extern int islink (char *src, char *whereto);
extern void launch (Stringstack child, int inf, int outf);
extern int listdir (char *dir, LsFlags fl);
extern int listfile (char *file, LsFlags fl);
extern void listoutput (char *buf);
extern void listoutputflush ();
extern int morefile (FILE *f, char *name, int showname, FILE *mystdin);
extern int movefiletodir (char *file, char *dir, MvFlags fl);
extern int movefiletofile (char *src, char *dest, MvFlags fl);
extern void onechild (Stringstack what, int stdin_desc, int stdout_desc,
		      int allowfrom, int allowto);
extern int recursiveremove (char *dir, RmFlags fl);		      
extern Redirect redirected (Stringstack *what, char *fname);
extern Stringstack reexpand (Stringstack cmd);
extern int removefile (char *name, RmFlags fl);
extern void runchild (Stringstack what, int background);
extern void runcmd (Stringstack what);
extern int runinternal (Stringstack what);
extern Stringstack setexpandedstring (char *newst);
extern void setshlvl ();
extern Stringstack setstring (char *newst);
extern Stringstack setquotedstring (char *newst, int quote);
extern Stringstack setvariable (char *what);
extern void sighandler (int sig);
extern void sourcefile (char *name);
extern int splitcmd (Stringstack series, Stringstack *dest, int from);
extern void startupfiles ();
extern void waitforchild (char *childname, int childpid, int background);
extern int warning (char *msg, ...);
extern void *xmalloc (int sz);
extern void *xrealloc (void *mem, int newsz);
extern char *xstrdup (char *s);
extern int yyerror (char *msg);
extern int yylex ();
extern int yyparse ();
extern int yypopfile ();
extern void yypushfile (FILE *inf);


/*  eep.h

This is the common header file for the eep program.

*/

#ifdef UNIX
#define NEWSBASE "/usr/spool/news/"  /* where news lives */
#define ACTIVEFILE "/usr/lib/news/active" /* active file */
#define NEWSGROUPS "/usr/lib/news/newsgroups" /* descriptions of newsgroups */
#define NEWSLOCAL "/usr/lib/news/newslocal" /* local newsgroups */
#define NEWSRC ".newsrc" /* filename for .newsrc */
#define ONEWSRC ".oldnewsrc" /* filename for old .newsrc  -sp */
#define RNLOCK ".rnlock" /* lock file that says rn or trn is active */
#define CRLF	"\n"
#define SHELL "/bin/ksh" /* default shell */
#define ALIGNMENT 4      /* some machines need pointers aligned 
                          * on every 4th byte, e.g. PA-RISC */
#define MAXLINES 20000   /* thank Cthulu for virtual memory */
#define MAXARTS 2000	
#define WALLOP_SIZE 32768  /* size used when malloc() called */
#endif /* UNIX */

#ifdef DOS
#define NEWSBASE "/news/"  /* where news lives */
#define ACTIVEFILE "active" /* active file */
#define NEWSGROUPS "newsgroups" /* descriptions of newsgroups */
#define NEWSLOCAL "newslocal" /* local newsgroups */
#define NEWSRC "newsrc" /* filename for .newsrc */
#define ONEWSRC "newsrc.old" /* filename for old .newsrc  -sp */
#define CRLF	"\r\n"
#define ALIGNMENT 2      /* some machines need pointers aligned 
                          * on every 4th byte, e.g. PA-RISC */
#define MAXLINES 5000   /* max. number of news groups */
#define MAXARTS 600
#define WALLOP_SIZE 8192  /* size used when malloc() called */
#endif /* DOS */

#ifndef FALSE
#define FALSE 0		/* this may be redefined elsewhere */
#endif
#ifndef TRUE
#define TRUE 1		/* this may be redefined elsewhere */
#endif
#define POINTER TRUE	/* true = arrow, false = bar   -sp */
#define VERBOSE FALSE	/* default for verbosity */
#define EEPLINES 24	/* lines shown on screen */
#define EEPCOLUMNS 80	/* columns shown on screen */
#define EEPPAGE EEPLINES-2	/* page size for scrolling */
#define BUFSIZE 4096	/* general line buffer length */
#define MAXLEVELS 20	/* depth of news hierachies */
#define MAXDIST 40	/* number of top level distribution names */
#define MAXBUFS 1024    /* size of array of pointers to malloc() buffers */
#define TIMEOUT 2	/* keyboard inactivity timer in minutes */
#define SHELLDENY 3	/* uid for user to deny shell access to */
#define LEFT 15         /* These definitions are used to control ... */
#define MIDDLE 40       /* where the newsgroup name ends and the ...*/
#define RIGHT 65        /* description begins on screen. */

/* This is the primary data storage structure.  It combines both the
active file and the .newsrc file.  We'll malloc() space for each
element of the structure, and use an array of pointers to access it
*/

struct actif	{

char	*name,  /* news group */
	*desc,	/* description of newsgroup */
	*hilo;	/* high to low range from my .newsrc */

long    hi;     /* hi message number from active file */

int	index,	/* used as alternate index into array */
        mark;   /* used to mark groups to be moved */

char	flag,	/* [ynm] from active file */
	status;	/* [:!] from .newsrc file */

struct  actif *depth;	/* pointer to next entry in ``depth'' list */
};


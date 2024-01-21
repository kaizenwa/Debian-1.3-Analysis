/*	SC	A Spreadsheet Calculator
 *		Main driver
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		Currently supported by gator!sawmill!buhrt (Jeff Buhrt)
 *		$Revision: 1.1 $
 *
 *		More mods by Dan Coppersmith, 5/94
 */

#include <config.h>
#include <curses.h>

#include <sys/types.h>
#include <signal.h>
#ifdef HAVE_X11_X_H
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#endif /* HAVE_X11_X_H */
#include <ctype.h>
#include "sc.h"

#ifdef HAVE_X11_X_H
#include "scXstuff.h"
static void	redraw_current(/*GC*/);
#endif /* HAVE_X11_X_H */

#ifndef SAVENAME
#define	SAVENAME "SC.SAVE" /* file name to use for emergency saves */
#endif /* SAVENAME */

#ifndef DFLT_PAGER
#define	DFLT_PAGER "more"	/* more is probably more widespread than less */
#endif /* DFLT_PAGER */

#define MAXCMD 160	/* for ! command below */

#ifdef HAVE_X11_X_H
char	laststring[1024];    /* string at position lastmx,lastmy */
int	lstringstart = -1,      /* table column laststring starts in */
	lstringend = -1;        /*  ... and column it ends in */
/* FIXME- all three above */
#endif /* HAVE_X11_X_H */

static void	File_Menu PROTO((int));
static void	Option_Menu1 PROTO((int));
static void	Option_Menu2 PROTO((int));
static void	Matrix_Menu PROTO((void));
static void	Row_Col_Menu PROTO((int));
static void	Range_Menu PROTO((int));

/* Globals defined in sc.h */

struct ent ***tbl;
int strow = 0, stcol = 0;
int currow = 0, curcol = 0;
int savedrow, savedcol;
int FullUpdate = 0;
int ClearScreen = 0;	/* don't try to be smart */
int maxrow, maxcol;
int maxrows, maxcols;
int *fwidth;
int *precision;
int *realfmt;
char *col_hidden;
char *row_hidden;
char line[FBUFLEN];
int changed;
struct ent *to_fix;
int modflg;
char *mdir;
int showsc, showsr;	/* Starting cell for highlighted range */
#ifdef RIGHT_CBUG
int	wasforw	= FALSE;
#endif
int        maintextrows,    /* text rows in main window */
	   maintextcols;    /* text cols in main window */

char	stringbuf[1024];     /* build misc. strings for display on screen */

char	curfile[PATHLEN];
char    *revmsg = NULL;

int  linelim = -1;

int  showtop   = 1;	/* Causes current cell value display in top line  */
int  showcell  = 1;	/* Causes current cell to be highlighted	  */
int  showrange = 0;	/* Causes ranges to be highlighted		  */
int  showneed  = 0;	/* Causes cells needing values to be highlighted  */
int  showexpr  = 0;	/* Causes cell exprs to be displayed, highlighted */

int  numeric;		/* data entry mode: 1 for numeric                 */ /* Peter Doemel, 11-Feb-1993 */
int  autocalc;		/* 1 to calculate after each update               */
int  calc_order;	/* order of calculation: BYROWS or BYCOLS         */
int  craction = 0;	/* CRCOLS for down, CRROWS for right              */

int  autolabel = 1;     /* If room, causes label to be created after a define*/
int  showcursor = SHOWCURSOR;	/* Causes cursor to be displayed */
int  tbl_style = 0;	/* headers for T command output */
int  rndinfinity = 0;
int  numeric_field = 0; /* Started the line editing with a number */
int  rowlimit = -1;
int  collimit = -1;
#if defined(SIGWINCH)
int  hitwinch = 0;	/* got a SIGWINCH? */
#endif
int  using_X = FALSE;	/*
			 * are we doing X? (vs curses) 
			 * set once we find $DISPLAY is working
			*/

extern	int lastmx, lastmy;	/* Screen address of the cursor */
extern	int lastcol, lcols;	/* Spreadsheet Column the cursor was in last */

/* a linked list of free [struct ent]'s, uses .next as the pointer */
struct ent *freeents = NULL;

extern	char	*rev;
int	anychanged = FALSE;

#ifdef VMS
int VMS_read_raw = 0;
#endif

/* return a pointer to a cell's [struct ent *], creating if needed */
struct ent *
lookat(row,col)
int	row, col;
{
    register struct ent **pp;

    checkbounds(&row, &col);
    pp = ATBL(tbl, row, col);
    if (*pp == (struct ent *)0) {
        if (freeents != NULL)
	{	*pp = freeents;
		freeents = freeents->next;
	}
	else
		*pp = (struct ent *) scxmalloc((unsigned)sizeof(struct ent));
	if (row>maxrow) maxrow = row;
	if (col>maxcol) maxcol = col;
	(*pp)->label = (char *)0;
	(*pp)->row = row;
	(*pp)->col = col;
	(*pp)->flags = 0;
	(*pp)->expr = (struct enode *)0;
	(*pp)->v = (double) 0.0;
	(*pp)->format = (char *)0;
	(*pp)->cellerror = CELLOK;
	(*pp)->next = NULL;
    }
    return *pp;
}

/*
 * This structure is used to keep ent structs around before they
 * are deleted to allow the sync_refs routine a chance to fix the
 * variable references.
 * We also use it as a last-deleted buffer for the 'p' command.
 */
void
free_ent(p)
register struct ent *p;
{
    p->next = to_fix;
    to_fix = p;
    p->flags |= is_deleted;
    p->flags &= ~is_locked;
}

/* free deleted cells */
void
flush_saved()
{
    register struct ent *p;
    register struct ent *q;

    if (!(p = to_fix))
	return;
    while (p) {
	(void) clearent(p);
	q = p->next;
	p->next = freeents;	/* put this ent on the front of freeents */
	freeents = p;
	p = q;
    }
    to_fix = NULL;
}

char    *progname;
static	int	arg = 1;
int	running = TRUE;

int
main (argc, argv)
int argc;
char  **argv;
{
#ifdef HAVE_X11_X_H
    /* The following is the XSpread event structure and input buffer for key events */
    /* XEvent event; */
    /* KeySym  key; */
    /* char buffer[8]; */
    /* int count; */
    /* int done; */
    /* int complete; */
    /* int pixel;  */
    int try_X = TRUE;	/* should we try to use X? */
#endif /* HAVE_X11_X_H */

    int     inloop = 1;
    register int   c;
    int     edistate = -1;
    int     narg;
    int     nedistate;
    char    *revi;

    /*
     * Keep command line options around until the file is read so the
     * command line overrides file options
     */

    int Mopt = 0;
    int Nopt = 0;
    int Copt = 0; 
    int Ropt = 0;

#ifdef HAVE_X11_X_H
    char **copyv = argv;	  /*  Added to change font size with  -fn */
    int copyc = argc;
#endif /* HAVE_X11_X_H */
    int tempx, tempy; 	/* Temp versions of curx, cury */

#ifdef HAVE_X11_X_H
    userfont = NULL;
#endif /* HAVE_X11_X_H */

#if defined(MSDOS)
    if ((revi = strrchr(argv[0], '\\')) != NULL)
#else
#ifdef VMS
    if ((revi = strrchr(argv[0], ']')) != NULL)
#else
    if ((revi = strrchr(argv[0], '/')) != NULL)
#endif
#endif
	progname = revi+1;
    else
	progname = argv[0];


    while (argc > 1 && argv[1][0] == '-') {
	argv++;
	argc--;
    	switch (argv[0][1]) {
	    case 'x':
#if defined(VMS) || defined(MSDOS) || !defined(CRYPT_PATH)
		    (void) fprintf(stderr, "Crypt not available\n");
		    exit(1);
#else 
		    Crypt = 1;
#endif
		    break;
	    case 'm':
		    Mopt = 1;
		    break;
	    case 'n':
		    Nopt = 1;
		    break;
	    case 'c':
		    Copt = 1;
		    break;
	    case 'r':
		    Ropt = 1;
		    break;
	    case 'f':	 	/*  -fn font size */
	    	    if (argv[0][2] != 'n' && argv[0][2] != 'g') goto ILLEGALOPTION;
                    switch (argv[0][2]){
#ifdef HAVE_X11_X_H
			case 'n': 
				argv++;
				argc--;
				/*userfont = strdup(argv[0]);*/
				userfont = argv[0];
				break;
			case 'g':
				argv++;
		                argc--;
                                strcpy(foreg, argv[0]);
                                break;
#endif /* HAVE_X11_X_H */
                        }
                    break;
#ifdef HAVE_X11_X_H
	    case 'X':
		    try_X = FALSE;
		    break;
  
            case 'b':
                    if (argv[0][2] != 'g' ) goto ILLEGALOPTION;
		                argv++;
		                argc--;
                                strcpy(backg, argv[0]);
                                break;
#endif /* HAVE_X11_X_H */
	    case 'N':
		    showcursor = !SHOWCURSOR;
		    break;
	    case 'C':
		    craction = CRCOLS;
		    break;
	    case 'R':
		    craction = CRROWS;
		    break;
            case 'h':
		    print_qref(FALSE, TRUE);
                    exit(1);
            case 'q':
		    print_qref((argv[0][2] == 't') || (argv[0][2] == 'T'),
			       FALSE);
                    exit(1);
	    default:
ILLEGALOPTION:	    (void) fprintf(stderr,"%s: unrecognized option: \"%c\"\n",
			progname,argv[0][1]);
		    print_help();
		    exit(1);
	}
    }

#ifdef HAVE_X11_X_H
    if (try_X)	/* sets using_X if X seems to work... */
	using_X = sc_Xinit(copyc, copyv);
#endif /* HAVE_X11_X_H */

    *curfile ='\0';

    signals();
    graphic_init();  /* initialize graphing parameters */

    if (!using_X)
	startdisp();

	/* setup the spreadsheet arrays, initscr() will get the screen size */
    if (!growtbl(GROWNEW, 0, 0))
    {	stopdisp();
	exit(1);
    }

    /* Build revision message for later use: */
    if (revmsg = (char *) scxmalloc((unsigned)(strlen(version)+30)))
    	sprintf(revmsg, "%s: Type '?' for help.", version);

	/* set in case someone does an 'evalall' in the file */
    autocalc = Mopt ? FALSE : TRUE;
    numeric = Nopt ? FALSE : TRUE;
    calc_order = Copt ? BYCOLS : BYROWS;
    calc_order = Ropt ? BYROWS : calc_order;

    if (argc > 1) {	/* moved here by Peter Doemel, 25-Feb-1993 */
	(void) strcpy(curfile,argv[1]);
	readfile (argv[1], 0);
    }

	/*
	 * make sure command line overrides when was in the file we read,
	 * if no flags set, leave values set.
	 */
    autocalc = Mopt ? FALSE : autocalc;
    numeric = Nopt ? FALSE : numeric;
    calc_order = Copt ? BYCOLS : calc_order;
    calc_order = Ropt ? BYROWS : calc_order;

#ifdef VENIX
    setbuf (stdin, NULL);
#endif
    modflg = 0;
    FullUpdate++;

    while (inloop) { running = TRUE;
    while (running) {
	nedistate = -1;
	narg = 1;
	if (edistate < 0 && linelim < 0 && autocalc && (changed || FullUpdate))
	{    EvalAll ();
	     if (changed)		/* if EvalAll changed or was before */
		anychanged = TRUE;
	     changed = 0;
	}
	else		/* any cells change? */
	if (changed)
	     anychanged = TRUE;

#if defined(SIGWINCH)
	/* got a SIGWINCH? */
	if (!using_X && hitwinch)
	{	hitwinch = 0;
		stopdisp();
		startdisp();
		FullUpdate++;
	}
#endif
	update(anychanged);
	anychanged = FALSE;

	showneed = 0;	/* reset after each update */
	showexpr = 0;
#ifdef HAVE_X11_X_H
	if (using_X)
	{	
	   if (!seenerr)
		clearlines(1,1);
	   seenerr = 0;	/* note, this is here (vs below),
			because in X nmgetch() may set seenerr */

	    c = nmgetch();
	    if (!running)	/* nmgetch would set this in Main_Menu() */
		break;
	} else /* curses below */
#endif /* HAVE_X11_X_H */
	{
	    seenerr = 0;
# ifndef SYSV3	/* HP/Ux 3.1 this may not be wanted */
	    (void) refresh(); /* 5.3 does a refresh in getch */ 
# endif
	    c = nmgetch();
	    getyx(stdscr, tempy, tempx);
	    clearlines(1,1);
	    (void) move(tempy, tempx);
	} /* HAVE_X11_X_H, end curses */

	/*
	 * there seems to be some question about what to do w/ the iscntrl
	 * some BSD systems are reportedly broken as well
	 */
	/* if ((c < ' ') || ( c == DEL ))   how about international here ? PB */
#if	pyr
	   if ( iscntrl(c) || (c >= 011 && c <= 015) )	/* iscntrl broken in OSx4.1 */
#else
	   if (isascii(c) && (iscntrl(c) || (c == 020)) )	/* iscntrl broken in OSx4.1 */
#endif
      {   switch (c) {
#ifdef SIGTSTP
		case ctl('z'):
		    if (!using_X)
			(void) deraw();
		    (void) kill(0, SIGTSTP); /* Nail process group */

		    /* the pc stops here */
		    if (!using_X)
			(void) goraw();
		    break;
#endif
		case ctl('r'):
		    showneed = 1;
		case ctl('l'):
		    FullUpdate++;
		    ClearScreen++;
		    if (!using_X)
			(void) clearok(stdscr,1);

		    /* Centering the display with cursor for middle */
		    if(currow > (maintextrows - RESROW)/2)
			strow = currow - ((maintextrows - RESROW) / 2);
		    break;
		case ctl('x'):
		    FullUpdate++;
		    showexpr = 1;
		    if (!using_X)
			(void) clearok(stdscr,1);
		    break;
		default:
		    sprintf(stringbuf, "No such command (^%c)", c + 0100);
		    scerror(stringbuf);
		    break;
		case ctl('b'):
		    if (numeric_field) {
			write_line(ctl('m'));
			numeric_field = 0;
		    }
		    backcol(arg);
		    break;
		case ctl('c'):
		    running = FALSE;
		    break;

		case ctl('e'):

		    switch (nmgetch()) {
		    case ctl('p'): case 'k':	doend (-1, 0);	break;
		    case ctl('n'): case 'j':	doend ( 1, 0);	break;
		    case ctl('b'): case 'h':
		    case ctl('h'):		doend ( 0,-1);	break;
		    case ctl('f'): case 'l':
		    case ctl('i'): case ' ':	doend ( 0, 1);	break;

		    case ESC:
		    case ctl('g'):
			break;

		    default:
			scerror("Invalid ^E command");
			break;
		    }
		    break;

		case ctl('f'):
		    if (numeric_field) {
			write_line(ctl('m'));
			numeric_field = 0;
		    }
		    forwcol(arg);
#ifdef RIGHT_CBUG
		    wasforw++;
#endif
		    break;

		case ctl('g'):
		    showrange = 0;
		    linelim = -1;
		    clearlines(1,1);
		    break;

		case ESC:	/* ctl('[') */
		    write_line(ESC);
		    break;

		case ctl('d'):
		    write_line(ctl('d'));
		    break;

		case DEL:
		case ctl('h'):
		    if (linelim < 0) {	/* not editing line */
			backcol(arg);	/* treat like ^B    */
			break;
		    }
		    write_line(ctl('h'));
		    break;

		case ctl('i'): 		/* tab */
		    if (linelim < 0) {	/* not editing line */
			forwcol(arg);
			break;
		    }
		    if (!showrange) {
			startshow();
		    } else {
			showdr();
			linelim = strlen(line);
			line[linelim++] = ' ';
			line[linelim] = '\0';
			showrange = 0;
		    }
		    linelim = strlen (line);
		    break;

		case ctl('m'):
		case ctl('j'):
		    numeric_field = 0;
		    write_line(ctl('m'));
		    switch(craction) {
		      case CRCOLS:
			if ((rowlimit >= 0) && (currow >= rowlimit)) {
			    forwcol(1);
			    currow = 0;
			}
			else {
			    forwrow(1);
			}
			break;
		      case CRROWS:
			if ((collimit >= 0) && (curcol >= collimit)) {
			    forwrow(1);
			    curcol = 0;
			}
			else {
			    forwcol(1);
			}
			break;
		      default:
			break;
		      }
		    break;

		case ctl('n'):
		    if (numeric_field) {
			write_line(ctl('m'));
			numeric_field = 0;
		    }
		    forwrow(arg);
		    break;

		case ctl('p'):
		    if (numeric_field) {
			write_line(ctl('m'));
			numeric_field = 0;
		    }
		    backrow(arg);
		    break;

		case ctl('q'):
		    break;	/* ignore flow control */

		case ctl('s'):
		    break;	/* ignore flow control */

		case ctl('t'):
#if !defined(VMS) && !defined(MSDOS)
		    scerror(
"Toggle: a:auto,e:ext funcs,n:numeric,t:top,x:encrypt,$:pre-scale,<MORE>");
#else 				/* no encryption available */
		    scerror(
"Toggle: a:auto, e:ext funcs, n:numeric, t:top, $:pre-scale, <MORE>");
#endif

		    if (!using_X)
			(void) refresh();

		    switch (nmgetch()) {
			case 'a': case 'A':
			case 'm': case 'M':	/* Auto */
				Option_Menu1(1); break;
			case 'n': case 'N':	/* Numeric */
				Option_Menu1(2); break;
			case 't': case 'T':	/* Top */
				Option_Menu1(3); break;
			/* not working well, not usefull anyways */
			/* case 'c': case 'C':	*/  /* Cell */
				/* Option_Menu1(4); break;  */
			case 'x': case 'X':	/* Encrypt */
				Option_Menu1(5); break;
			case 'l': case 'L':	/* Auto-Label */
				Option_Menu1(6); break;
			case '$':		/* Pre-scale */
				Option_Menu2(1); break;
			case 'e': case 'E':	/* Ext-funcs */
				Option_Menu2(2); break;
			case 'r': case 'R':	/* NL-action */
				Option_Menu2(3); break;
			case 'z': case 'Z':	/* Row/Col-Limits */
				Option_Menu2(4); break;
			default:
			    scerror("Invalid toggle command");
				/* note fall thru */
			case ESC:
			case ctl('g'):
			    --modflg;	/* negate the modflg++ */
		    }
		    FullUpdate++;
		    modflg++;
		    break;

		case ctl('u'):
		    narg = arg * 4;
		    nedistate = 1;
		    break;

		case ctl('v'):	/* insert variable name */
		    if (linelim > 0)
		        ins_string(v_name(currow, curcol));
		    break;

		case ctl('w'):	/* insert variable expression */
		    if (linelim > 0)  {
			static	char *temp = NULL, *temp1 = NULL;
			static	unsigned	templen = 0;
			int templim;

			/* scxrealloc will scxmalloc if needed */
			if (strlen(line)+1 > templen)
			{	templen = strlen(line)+40;

				temp = scxrealloc(temp, templen);
				temp1= scxrealloc(temp1, templen);
			}
			strcpy(temp, line);
			templim = linelim;
			linelim = 0;		/* reset line to empty	*/
			editexp(currow,curcol);
			strcpy(temp1, line);
			strcpy(line, temp);
			linelim = templim;
			ins_string(temp1);
		    }
		    break;

		case ctl('a'):	/* insert variable value */
		    if (linelim > 0) {
			struct ent *p = *ATBL(tbl, currow, curcol);
			char temp[100];

			if (p && p -> flags & is_valid) {
			    (void) sprintf (temp, "%.*f",
					precision[curcol],p -> v);
			    ins_string(temp);
			}
		    }
		    break;

	    } /* End of the control char switch stmt */
        }
	else if (isascii(c) && isdigit(c) && ((numeric && edistate >= 0) ||
			(!numeric && (linelim < 0 || edistate >= 0)))) {
	    /* we got a leading number */
	    if (edistate != 0) {
		/* First char of the count */
		if (c == '0')      /* just a '0' goes to left col */
		    curcol = 0;
		else {
		    nedistate = 0;
		    narg = c - '0';
		}
	    } else {
		/* Succeeding count chars */
		nedistate = 0;
		narg = arg * 10 + (c - '0');
	    }
	} else if (linelim >= 0) {
	    /* Editing line */
	    switch(c) {
	    case ')':
		if (showrange) {
		    showdr();
		    showrange = 0;
		    linelim = strlen (line);
		}
		break;
	    default:
		break;
	    }
	    write_line(c);

	} else if (!numeric && ( c == '+' || c == '-' ) ) {
	    /* increment/decrement ops */
	    register struct ent *p = *ATBL(tbl, currow, curcol);
	    if (!p)
		continue;
	    if (p->expr && !(p->flags & is_strexpr)) {
		scerror("Can't increment/decrement a formula\n");
		continue;
	    }
	    FullUpdate++;
	    modflg++;
	    if( c == '+' )
	    	p -> v += (double) arg;
	    else
		p -> v -= (double) arg;
	} else
	{    /* switch on a normal command character */
	    switch (c) {
		case ':':
		    break;	/* Be nice to vi users */

		case '@':
		    EvalAll ();
		    changed = 0;
		    anychanged = TRUE;
		    break;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		case '-': case '.': case '+':
		    if (locked_cell(currow, curcol))
			break;
		    numeric_field = 1;
		    (void) sprintf(line,"let %s = %c",
				v_name(currow, curcol), c);
		    linelim = strlen (line);
		    insert_mode();
		    break;

		case '=':
		    if (locked_cell(currow, curcol))
			break;
		    (void) sprintf(line,"let %s = ",
					v_name(currow, curcol));
		    linelim = strlen (line);
		    insert_mode();
		    break;

		case '!':
		    {
		    /*
		     *  "! command"  executes command
		     *  "!"	forks a shell
		     *  "!!" repeats last command
		     */
#if VMS || MSDOS
		    scerror("Not implemented on VMS or MS-DOS");
#else /* VMS */
		    char *shl;
		    int pid, temp;
		    char cmd[MAXCMD];
		    static char lastcmd[MAXCMD];

		    if (!(shl = getenv("SHELL")))
			shl = "/bin/sh";

		    if (!using_X)
			deraw();

		    (void) fputs("! ", stdout);
		    (void) fflush(stdout);
		    (void) fgets(cmd, MAXCMD, stdin);
		    cmd[strlen(cmd) - 1] = '\0';	/* clobber \n */
		    if(strcmp(cmd,"!") == 0)		/* repeat? */
			    (void) strcpy(cmd, lastcmd);
		    else
			    (void) strcpy(lastcmd, cmd);

		    if (modflg)
		    {
			(void) puts ("[No write since last change]");
			(void) fflush (stdout);
		    }

		    if (!(pid = fork()))
		    {
			(void) signal (SIGINT, SIG_DFL);  /* reset */
			if(strlen(cmd))
				(void)execl(shl,shl,"-c",cmd,(char *)0);
			else
				(void) execl(shl, shl, (char *)0);
			exit(-127);
		    }

		    while (pid != wait(&temp));

		    (void) printf("Press RETURN to continue ");
		    fflush(stdout);
#ifndef notdef
		    (void)nmgetch();
		    if (!using_X)
			goraw();
#endif
#endif /* VMS */
		    break;
		    }

		/*
		 * Range commands:
		 */

		case 'r':
		    scerror(
"Range: x:erase v:value c:copy f:fill d:def u:undef s:show l:lock U:unlock F:fmt");
		    if (!using_X)
			(void) refresh();

		    switch (nmgetch()) {
			case 'x': Range_Menu(1); break;
			case 'v': Range_Menu(2); break;
			case 'c': Range_Menu(3); break;
			case 'f': Range_Menu(4); break;
			case 'd': Range_Menu(5); break;
			case 'u': Range_Menu(6); break;
			case 's': Range_Menu(7); break;
			case 'l': Range_Menu(8); break;
			case 'U': Range_Menu(9); break;
			case 'F': Range_Menu(10); break;
			case ESC:
			case ctl('g'):
				clearlines(1,1);
				break;
			default:
				scerror("Invalid region command");
				break;
		    }
		    break;

		/*
		 * Row/column commands:
		 */
		case 'i': Row_Col_Menu(1); break;	/* insert */
		case 'a': Row_Col_Menu(2); break;	/* append */
		case 'd': Row_Col_Menu(3); break;	/* delete */
		case 'p': Row_Col_Menu(4); break;	/* pull */
		case 'v': Row_Col_Menu(5); break;	/* valueize */
		case 'z': Row_Col_Menu(6); break;	/* hide */
		case 's': Row_Col_Menu(7); break;	/* show */

		case '$':
		    {
		    register struct ent *p;

		    curcol = maxcols - 1;
		    while (!VALID_CELL(p, currow, curcol) && curcol > 0)
			curcol--;
		    break;
		    }
		case '#':
		    {
		    register struct ent *p;

		    currow = maxrows - 1;
		    while (!VALID_CELL(p, currow, curcol) && currow > 0)
			currow--;
		    break;
		    }
		case 'w':
		    {
		    register struct ent *p;

		    while (--arg>=0) {
			do {
			    if (curcol < maxcols - 1)
				curcol++;
			    else {
				if (currow < maxrows - 1) {
				    while(++currow < maxrows - 1 &&
					    row_hidden[currow]) /* */;
				    curcol = 0;
				} else {
				    scerror("At end of table");
				    break;
				}
			    }
			} while(col_hidden[curcol] ||
				!VALID_CELL(p, currow, curcol));
		    }
		    break;
		    }
		case 'b':
		    {
		    register struct ent *p;

		    while (--arg>=0) {
			do {
			    if (curcol) 
				curcol--;
			    else {
				if (currow) {
				    while(--currow &&
					row_hidden[currow]) /* */;
				    curcol = maxcols - 1;
				} else {
				    scerror("At start of table");
				    break;
				}
			    }
			} while(col_hidden[curcol] ||
				!VALID_CELL(p, currow, curcol));
		    }
		    break;
		    }
		case '^':
		    currow = 0;
		    break;
#ifdef XK_Help
		case XK_Help:
#endif
		case '?':
		    help();
		    break;
		case '"':
		    if (!locked_cell(currow, curcol)) {
		       (void) sprintf (line, "label %s = \"",
					   v_name(currow, curcol));
		       linelim = strlen (line);
		       insert_mode();
		    }
		    break;

		case '<':
		    if (!locked_cell(currow, curcol)) {
		       (void) sprintf (line, "leftstring %s = \"",
			       v_name(currow, curcol));
		       linelim = strlen (line);
		       insert_mode();
		    }
		    break;

		case '>':
		    if (!locked_cell(currow, curcol)) {
		       (void) sprintf (line, "rightstring %s = \"",
			      v_name(currow, curcol));
		       linelim = strlen (line);
		       insert_mode();
		    }
		    break;
		case 'e':
		    if (!locked_cell(currow, curcol)) {
		       editv (currow, curcol);
		       edit_mode();
		    }
		    break;
		case 'E':
		    if (!locked_cell(currow, curcol)) {
		       edits (currow, curcol);
		       edit_mode();
		    }
		    break;
		case 'f':
		    if (arg == 1)
			(void) sprintf (line, "format [for column] %s ",
				coltoa(curcol));
		    else {
			(void) sprintf(line, "format [for columns] %s:",
				coltoa(curcol));
			(void) sprintf(line+strlen(line), "%s ",
				coltoa(curcol+arg-1));
		    }
		    sprintf(stringbuf, "Current format is %d %d %d",
			fwidth[curcol],precision[curcol],realfmt[curcol]);
		    scerror(stringbuf);
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'F': {
		    register struct ent *p = *ATBL(tbl, currow, curcol);
		    if (p && p->format)
		    {	(void) sprintf(line, "fmt [format] %s \"%s",
				   v_name(currow, curcol), p->format);
			edit_mode();
		    }
		    else
		    {	(void) sprintf(line, "fmt [format] %s \"",
				   v_name(currow, curcol));
			insert_mode();
		    }
		    linelim = strlen(line);
		    break;
		}
		case 'g':
		    (void) sprintf (line, "goto [v] ");
		    linelim = strlen (line);
		    insert_mode();
		    break;

		case 'G': File_Menu(1); break;	/* Get "file" */
		case 'P': File_Menu(2); break;	/* Put "file" */
		case 'W': File_Menu(3); break;	/* Write "file" range */
		case 'T': File_Menu(4); break;	/* Tbl "file" range */
		case 'M': File_Menu(6); break;	/* Merge "file" */
		case 'R': File_Menu(7); break;	/* Merge "macrofile" */
		case 'D': File_Menu(8); break;	/* mdir "macrodir" */

		case 'S':	/* set options */
		    (void) sprintf (line, "set ");
		    scerror("Options:byrows,bycols,iterations=n,tblstyle=(0|tbl|latex|slatex|tex|frame),<MORE>");
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'x':
		    {
		    register struct ent **pp;
		    register int c1;

		    flush_saved();
		    if(calc_order == BYROWS) {
		      for (c1 = curcol; arg-- && c1 < maxcols; c1++) {
			pp = ATBL(tbl, currow, c1);
		        if ((*pp) && !locked_cell(currow, curcol)) {
			   if (*pp) {
			       free_ent(*pp);
			       *pp = (struct ent *)0;
			   }
		        }
		      }
		    }
		    else {
		      for (c1 = currow; arg-- && c1 < maxrows; c1++) {
			pp = ATBL(tbl, c1, curcol);
		        if ((*pp) && !locked_cell(currow, curcol)) {
			   if (*pp) {
			       free_ent(*pp);
			       *pp = (struct ent *)0;
			   }
		        }
		      }
		    }
		    sync_refs();
		    modflg++;
		    FullUpdate++;
		    }
		    break;
		case 'Q':
		case 'q':
		    running = FALSE;
		    break;
		case 'h':
		    backcol(arg);
		    break;
		case 'j':
		    forwrow(arg);
		    break;
		case 'k':
		    backrow(arg);
		    break;
		case 'H':
			backcol((curcol-stcol+1)+1);
			break;
#ifdef XK_Next
		case XK_Next:
#endif
#ifdef KEY_NPAGE
		case KEY_NPAGE:			/* page precedente */
#endif
		case 'J':
			forwrow(maintextrows - RESROW - (currow-strow) + 1);
			break;
#ifdef XK_Prior
		case XK_Prior:
#endif
#ifdef	KEY_PPAGE
		case KEY_PPAGE:			/* page suivante */
#endif
		case 'K':
			backrow((currow-strow+1)+3);
			break;
#if defined(KEY_HOME) || defined(XK_Home)
#ifdef XK_Home
		case XK_Home:
#endif
#ifdef KEY_HOME
		case KEY_HOME:
#endif
			currow = 0;
			curcol = 0;
			FullUpdate++;
			break;
#endif
		case 'L':
			forwcol(lcols -(curcol-stcol)+1);
			break;
		case ' ':
		case 'l':
		    forwcol(arg);
		    break;
		case 'm':
		    savedrow = currow;
		    savedcol = curcol;
		    break;
		case 'c': {
		    register struct ent *p = *ATBL(tbl, savedrow, savedcol);
		    register c1;
		    register struct ent *n;
		    if (!p)
			break;
		    FullUpdate++;
		    modflg++;
		    for (c1 = curcol; arg-- && c1 < maxcols; c1++) {
			n = lookat (currow, c1);
			(void) clearent(n);
			copyent( n, p, currow - savedrow, c1 - savedcol);
		    }
		    break;
		}
		case '/': Main_Menu(); break;
		default:
		    if ((toascii(c)) != c)
			sprintf(stringbuf, "Weird character, decimal %d\n",
				(int) c);
		    else
			    sprintf(stringbuf, "No such command (%c)", c);
		    scerror(stringbuf);
		    break;
	    }
        }
	edistate = nedistate;
	arg = narg;
    }				/* while (running) */
    inloop = modcheck(" before exiting");
    }				/*  while (inloop) */
    stopdisp();
#ifdef VMS	/* Until VMS "fixes" exit we should say 1 here */
    exit(1);
#else
    exit(0);
#endif
    /*NOTREACHED*/
}

/* show the current range (see ^I), we are moving around to define a range */
void
startshow()
{
    showrange = 1;
    showsr = currow;
    showsc = curcol;
}

/* insert the range we defined by moving around the screen, see startshow() */
void
showdr()
{
    int     minsr, minsc, maxsr, maxsc;

    minsr = showsr < currow ? showsr : currow;
    minsc = showsc < curcol ? showsc : curcol;
    maxsr = showsr > currow ? showsr : currow;
    maxsc = showsc > curcol ? showsc : curcol;
    (void) sprintf (line+linelim,"%s", r_name(minsr, minsc, maxsr, maxsc));
}

/* set the calculation order */
void
setorder(i)
int i;
{
	if((i == BYROWS)||(i == BYCOLS))
	    calc_order = i;
}

void
setauto(i)
int i;
{
	autocalc = i;
}

/* signal handling section below */

/* signal handler: were exiting... */
RETSIGTYPE
doquit(arg)
int	arg;
{
    diesave();
    stopdisp();
    exit(1);
}

/* signal handler: dump core */
static RETSIGTYPE
dump_me(arg)
int	arg;
{
    diesave();
    if (!using_X)
	deraw();
    abort();
}

/* signal handler: screen size change */
#if defined(SIGWINCH)
static RETSIGTYPE
winchg(sig)
int	sig;
{	hitwinch++;
	(void) signal(SIGWINCH, winchg);
}
#endif

/* install signal handlers */
void
signals()
{
    (void) signal(SIGINT, SIG_IGN);
#if !defined(MSDOS)
    (void) signal(SIGQUIT, dump_me);
    (void) signal(SIGPIPE, doquit);
    (void) signal(SIGBUS, doquit);
#endif
    (void) signal(SIGTERM, doquit);
    (void) signal(SIGFPE, doquit);
#ifdef SIGWINCH
    if (!using_X)
	(void) signal(SIGWINCH, winchg);
#endif
}


/* try to save the current spreadsheet if we can */
void
diesave()
{   char	path[PATHLEN];

    if (modcheck(" before Spreadsheet dies") == 1)
    {	(void) sprintf(path, "~/%s", SAVENAME);
	if (writefile(path, 0, 0, maxrow, maxcol) < 0)
	{
	    (void) sprintf(path, "/tmp/%s", SAVENAME);
	    if (writefile(path, 0, 0, maxrow, maxcol) < 0)
		scerror("Couldn't save current spreadsheet, Sorry");
	}
    }
}

/* check if tbl was modified and ask to save
 * returning 0 if the user wants to quit (don't keep running), or 1 if 
 * the program should keep running
 */
int
modcheck(endstr)
char *endstr;
{
    if (modflg && curfile[0]) {
	int	yn_ans;
	char	lin[100];

	(void) sprintf (lin,"File \"%s\" is modified, save%s? ",curfile,endstr);
	if ((yn_ans = yn_ask(lin)) < 0)
		return(1);
	else
	if (yn_ans == 1)
	{    if (writefile(curfile, 0, 0, maxrow, maxcol) < 0)
 		return (1);
	}
    } else if (modflg) {
	int	yn_ans;

	if ((yn_ans = yn_ask("Do you want a chance to save the data? ")) < 0)
		return(1);
	else
		return(yn_ans);
    }
    return(0);
}

/* Returns 1 if cell is locked, 0 otherwise */
int
locked_cell (r, c)
	int r, c;
{
	struct ent *p = *ATBL(tbl, r, c);
	if (p && (p->flags & is_locked)) {
		sprintf(stringbuf, "Cell %s%d is locked", coltoa(c), r);
		scerror(stringbuf);
		return(1);
	}
	return(0);
}

/* Check if area contains locked cells */
int
any_locked_cells(r1, c1, r2, c2)
	int r1, c1, r2, c2 ;
{
	int r, c;
	struct ent *p ;

	for (r=r1; r<=r2; r++)
		for (c=c1; c<=c2; c++) {
			p = *ATBL(tbl, r, c);
			if (p && (p->flags & is_locked))
				return(1);
		}
	return(0);
}

/*===========================================================================
 *
 * The following funtions were writen to implement the cursor style menu
 *
 *                                 by SWLin, 8/90
 */

/* handle a non-X type keypress, returns if we are done */
static int
handle_keypress(c, choice, item, item_no)
int	c;
int	*choice;
char	*item[];
int	item_no;
{
	int i, done;

	done = FALSE;
	switch (c) {
		case ctl ('f'):
		case 'l': 
		case 'L': 
			(*choice)++;
			if (*choice == item_no + 1)
				*choice = 1;
			break; 
		case ctl ('b'):
		case ' ':
		case 'h':
		case 'H':
			(*choice)--;
			if (*choice == 0)
				*choice = item_no;
			break;
		case ctl ('p'):
		case 'k':
		case 'K':
			*choice = 1;
			break;
		case ctl ('n'):
		case 'j':
		case 'J':
			*choice = item_no;
			break;
		case 27:
		case ctl ('g'):
			*choice = 0;
			done = 1;
			break;
		case 13:
		case 10:
			done = 1;
			break;
		default:
			for (i=0; i < item_no; i++) {
			    if ((c == *item[i]) || ((c > 91)
				&& (c == *item[i] + 32))) {
					*choice = i+1;
					done = 1;
					break;
			    }
			}
			if (!done) { 
				fprintf(stderr, "\7");
			}     
			break;
	}
	return(done);
}

unsigned int
menu(item_no, item, help_prompt)
   unsigned int item_no;
   char         *item[], *help_prompt[]; 

{  int  i, choice, done;
   char c;
   int show_screen;
#ifdef HAVE_X11_X_H
   int cursor_temp, choice_temp, /* TempX_Loc, */ col;
   XEvent event;
   KeySym key;
   char buffer[8];
#endif

   choice = 1;
   done = 0;
   show_screen = 1;

   clearlines(1,1);

   while (!done) {
    if (show_screen)
    {
#ifdef HAVE_X11_X_H
      if (using_X)
      {
	for (i=0, col=0; i < item_no; i++) {
		XDrawImageString(dpy, mainwin,
		    (i== choice-1) ? maingcreversed : maingc,
		    textcol(col), textrow(1),
		    item[i], strlen(item[i]));
		col = col + strlen(item[i]) + 2;
	}
      } else
#endif /* HAVE_X11_X_H */
      {	(void) move(1, 0);
	for (i=0; i < item_no; i++) {
		if (i == choice-1)
			standout();
		addstr(item[i]);
		if (i == choice-1)
			standend();
		addstr("  ");
	}
	clrtoeol();
      } /* HAVE_X11_X_H, end curses */

	clearlines(2,2);
#ifdef HAVE_X11_X_H
	if (using_X)
	{   XDrawImageString(dpy,mainwin,maingc,
	       textcol(0), textrow(2),
	       help_prompt[choice - 1], strlen(help_prompt[choice - 1]));
	} else
#endif /* HAVE_X11_X_H */
	{   move(2, 0);
	    addstr(help_prompt[choice - 1]);
	    clrtoeol();
	} /* HAVE_X11_X_H, end curses */

	show_screen = 0;
    }

#ifdef HAVE_X11_X_H
    if (using_X)
    {
      XNextEvent(dpy,&event);

      switch(event.type)
      {
	case MotionNotify:
		if ( (event.xbutton.y >= 10) && 
		     (event.xbutton.y <= 2 * curfontheight)  )
		{
			choice_temp = 1;
			cursor_temp = ((strlen(item[0])+2) * curfontwidth);
			while (cursor_temp < event.xbutton.x) {
				if (choice_temp < item_no) {
					cursor_temp += ( (strlen(
							  item[choice_temp++])
							  + 2) * curfontwidth);
				} else
					break;
			}

			if (choice_temp <= item_no)
			{	if (choice_temp != choice)
				{	choice = choice_temp;
					show_screen = 1;
				} /* end of if choice_temp */
			} /* only if choice_temp is within bounds */
		}
		break;

       case ButtonPress:
		if ((event.xbutton.y > 2 * curfontheight))
		{	choice=0;
			done=1;
		}
		else /* Let the mouse select the exact choice */
			show_screen = done = 1;
		break;
	 
       case Expose:
		FullUpdate++;
		update(FALSE);
		show_screen = 1;
		break;

       case MappingNotify:
		XRefreshKeyboardMapping (&(event.xmapping));
		break;

       case ConfigureNotify:
		sc_handleresize(&event);
		break;

       /***** NEEDS TO BE FIXED *****/

       case KeyPress:
		if((XLookupString (&(event.xkey),buffer,8,&key,0)) ||
		   (IsCursorKey(key)))
		{ 
		    show_screen = 1;
		    if (!IsCursorKey(key))
		    {
			c = buffer[0];
			done = handle_keypress(c, &choice, item, item_no);
		    } /* if !IsCursorKey(key) */
		    else switch(key) {
			case XK_Up :
				choice = 1;
				break;
			case XK_Down:
				choice = item_no;
				break;
			case XK_Left:
				choice--;
				if (choice == 0)
					choice = item_no;
				break;
			case XK_Right:
				choice++;
				if (choice == item_no + 1)
					choice = 1;
				break; 
		    }
		    break;
		} /* if XLookupString(..) */
      } /* switch (event.type) */
    } else
#endif /* HAVE_X11_X_H above */
    {	show_screen = 1;
	refresh();
	c = nmgetch();
	done = handle_keypress(c, &choice, item, item_no);
    } /* HAVE_X11_X_H, end curses */
   } /* while (!done) */
   clearlines(1,2);
   return choice;
}


/*
 *============================================================================
 */


static void
Range_Menu(opt)
int	opt;
{   int	doneok;

    static char *item[] = { 
			"Erase",
			"Value",
			"Copy",
			"Fill",
			"Define",
			"Undefine",
			"Show",
			"Lock",
			"Unlock",
			"Fmt"
    };

    static char *help_prompt[] = {
			"Clear a range", 
			"Remove the expressions from a range, leaving just the values",
			"Copy a source range to a destination range",
			"Fill a range with constant values",
			"Assign a name to a cell or a range of cells",
			"Use this command to undefine a previously defined range name",
			"Shows the currently defined range names",
			"Lock a range of cells (disallow input)",
			"Unlock a range of cells (allow input)",
			"Format a range of cells",
    };

    doneok = FALSE;
    switch (opt >= 0 ? opt : menu(10, item, help_prompt)) {
	case 0:
		if (opt < 0)
			Main_Menu();
		break;
	case 1: /* Erase */
		(void) sprintf(line,"erase [range] ");
		doneok = TRUE;
		break;
	case 2: /* Value */
		(void) sprintf(line, "value [range] ");
		doneok = TRUE;
		break;
	case 3: /* Copy */
		(void) sprintf(line,"copy [dest_range src_range] ");
		doneok = TRUE;
		break;
	case 4: /* Fill */
		(void) sprintf(line,"fill [range start inc] ");
		doneok = TRUE;
		break;
	case 5: /* Define */
		(void) sprintf(line,"define [string range] \"");
		doneok = TRUE;
		modflg++;
		break;
	case 6: /* Undefine */
		(void) sprintf(line,"undefine [range] ");
		doneok = TRUE;
		break;
	case 7: /* Show */
		if(are_ranges()) {
			FILE *f;
			int pid;
			char px[MAXCMD] ;
			char *pager;
      
			(void) strcpy(px, "| sort | ");
			if (!(pager = getenv("PAGER")))
				pager = DFLT_PAGER;
			(void) strcat(px,pager);
			f = openout(px, &pid);
			if (!f) {
				scerror("Can't open pipe to sort");
				break;
			}
			list_range(f);
			closeout(f, pid);
		}
		else
			scerror("No ranges defined");
		break;
	case 8: /* Lock */
		(void) sprintf(line,"lock [range] ");
		doneok = TRUE;
		break;
	case 9: /* Unlock */
		(void) sprintf(line,"unlock [range] ");
		doneok = TRUE;
		break;
	case 10: /* Fmt */
		(void) sprintf(line, "fmt [range \"format\"] ");
		doneok = TRUE;
	default:
		scerror("Invalid region command");
		break;
    }

    if (doneok)	/* wrote something to edit */
    {	linelim = strlen (line);
/*	startshow(); */ /* let user do it when he is ready */
	insert_mode();
	show_top_line();
    }
}

/*
 *============================================================================
 */

static void
Row_Col_Menu(opt)
int	opt;
{
    static char *item[] = { 
			"Insert",
			"Append",
			"Delete", 
			"Pull", 
			"Remove", 
			"Hide", 
			"Show",
			"Format"
    };

    static  char *help_prompt[] = {
			"Insert a new, empty row (column)",
			"Append a new copy of the current row (column)",
			"Delete the current row (column)",
			"Pull deleted cells back into the spreadsheet",
			"Remove/Valuize expressions from the affected rows (columns)",
			"Hide (``zap'') the current row (column)",
			"Show hidden rows (columns)",
			"Set the output format"
    };

    static char *item_1[] = {"Row", "Column"};

    static char *help_promt_1[] = {"Make change to rows",
				"Make change to columns"};

    switch (opt >= 0 ? opt : menu(8, item, help_prompt)) {

	case 0: 
		if (opt < 0)
			Main_Menu();
		break;

	case 1: /* Insert */
		switch(menu(2, item_1, help_promt_1) ) {
			case 0: Row_Col_Menu(-1);  break;
			case 1: insertrow (arg); break;
			case 2: opencol (curcol, arg); break;
		}
		break;

	case 2: /* Append */
		switch(menu(2, item_1, help_promt_1)) {
			case 0:	Row_Col_Menu(-1);  break;
			case 1: while (arg--) duprow(); break;
			case 2: while (arg--) dupcol(); break;
		}
		break;

	case 3: /* Delete */
		switch(menu(2, item_1, help_promt_1)) {
			case 0: Row_Col_Menu(-1);  break;
			case 1: deleterow (arg); break;
			case 2: closecol (curcol, arg); break;
		}
		break;

	case 4: /* Pull */
		switch(menu(2, item_1, help_promt_1)) {
			case 0: Row_Col_Menu(-1);  break;
			case 1: while (arg--)
					pullcells ('r');
				break;
			case 2: while (arg--)
					pullcells ('c');
				break;
		}
		break;

	case 5: /*
		 * Remove
		 * -turn an area starting at currow/curcol into
		 * constants vs expressions - not reversable
		 */
		switch(menu(2, item_1, help_promt_1)){ 
			case 0: Row_Col_Menu(-1);  break;
			case 1:	valueize_area(currow, 0, currow + arg - 1, maxcol);
				modflg = 1;
				break;
			case 2: valueize_area(0, curcol, maxrow, curcol + arg - 1);
				modflg = 1;
				break;
		}
		break;

	case 6: /* Hide */
		switch (menu(2, item_1, help_promt_1)) {
			case 0: Row_Col_Menu(-1);  break;
			case 1: hiderow (arg); break;
			case 2: hidecol (arg); break;
		}
		modflg++;
		break;

	case 7:   /* Show: special case; no repeat count */
		switch(menu(2, item_1, help_promt_1)) {
			case 0: Row_Col_Menu(-1);  break;
			case 1: rowshow_op(); break;
			case 2: colshow_op(); break;
		}
		break;
	case 8:   /* Format */
		if (arg == 1)
			(void) sprintf (line, "format [for column] %s ",
				coltoa(curcol));
		else {
			(void) sprintf(line, "format [for columns] %s:",
				       coltoa(curcol));
			(void) sprintf(line+strlen(line), "%s ",
				       coltoa(curcol+arg-1));
		}
		sprintf(stringbuf,"Current format is %d %d",
					fwidth[curcol],precision[curcol]);
		clearlines(1,1);
#ifdef HAVE_X11_X_H
		if (using_X)
		{	XDrawImageString(dpy,mainwin,maingc,
				 textcol(0), textrow(1),
				 stringbuf, strlen(stringbuf));
			XFlush(dpy);
		} else
#endif /* HAVE_X11_X_H */
		{	addstr(stringbuf);
			refresh();
		} /* HAVE_X11_X_H, end curses */

		seenerr = 1;
		linelim = strlen (line);
		break;
    }
}

		       

/*
 *============================================================================
 */

static void
Option_Menu1(opt)
int	opt;
{
    int	doneok;
    static char *item[] = {
			"Auto",
			"Numeric",
			"Top",
			"Cell", /* currently commented out */
			"Encrypt",
			"Auto-Label",
			"Set",
			"MORE_OPTIONS"
    };

    static char *help_prompt[] = {
			"Recalculate automatically or on ``@'' commands",
			"Make a digit starts a numeric value",  
			"Top line display enable/disable", 
			"Current cell highlighting enable/disable",
			"Encrypt/decrypt database and listing files",
			"Auto-Label cells",
			"Set other options",
			"NEXT OPTION MENU"
    };

    doneok = FALSE;
    switch (opt >= 0 ? opt : menu(8, item, help_prompt)) {
	case 0:
		if (opt < 0)
			Main_Menu();
		break; 
	case 1:
		autocalc = (! autocalc);
		sprintf(stringbuf, "Automatic recalculation %sabled.",
			autocalc ? "en":"dis"); 
		scerror(stringbuf);
		doneok = TRUE;
		break;
	case 2:
		numeric = (! numeric);
		sprintf(stringbuf,"Numeric input %sabled.",
			numeric ? "en" : "dis");
		scerror(stringbuf);
		doneok = TRUE;
		break;
	case 3:
		showtop = (! showtop);
#ifdef HAVE_X11_X_H
		if (using_X)
			redraw_current(maingc);
		else
#endif /* HAVE_X11_X_H */
			repaint(lastmx, lastmy, fwidth[lastcol]);

		sprintf(stringbuf,"Top line %sabled.", showtop ? "en" : "dis");
		scerror(stringbuf);
		doneok = TRUE;
		break;
	case 4: 
		showcell = (! showcell);
#ifdef HAVE_X11_X_H
		if (using_X)
			redraw_current(maingc);
		else
#endif /* HAVE_X11_X_H */
			repaint(lastmx, lastmy, fwidth[lastcol]);

		sprintf(stringbuf,"Cell highlighting %sabled.",
			showcell ? "en" : "dis");
		scerror(stringbuf);
		doneok = TRUE;
		break;
	case 5:
#if defined(VMS) || defined(MSDOS) || !defined(CRYPT_PATH)
		scerror("Encryption not available.");
#else 
		Crypt = (! Crypt);
		sprintf(stringbuf,"Encryption %sabled.", Crypt? "en" : "dis");
		scerror(stringbuf);
#endif
		doneok = TRUE;
		break;
	case 6:
		autolabel = (! autolabel);
		sprintf(stringbuf, "Autolabel %sabled.",
			autolabel ? "en" : "dis");
		scerror(stringbuf);
		doneok = TRUE;
		break;
	case 7:
		(void) sprintf (line, "set ");
    scerror("Options: byrows, bycols, iterations=n, tblstyle=(0|tbl|latex|tex)");
		linelim = strlen (line);
		break;
	case 8:
		Option_Menu2(-1);
		break;
    }
    if (doneok)
    {	FullUpdate++;
	modflg++;
    }
}

static void
Option_Menu2(opt)
int	opt;
{
    int	doneok;
    static char *item[] = {
			"Pre-scale",
			"Ext-funcs",
			"NL-action",
			"Row/Col-Limits",
			"Set"
    };

    static char *help_prompt[] = {
			"Numeric constants entered are multipled by 0.01",
			"External function execution enable/disable",
			"Newline action (direction)",
			"Set the current cell to the limit for NL-Limit",
			"Set other options"
    };

    doneok = FALSE;
    switch (opt >= 0 ? opt : menu(5, item, help_prompt)) {
	case 0:
		if (opt < 0)
			Main_Menu();
		break; 
	case 1:
		if (prescale == 1.0) {
			scerror("Prescale enabled.");
			prescale = 0.01;
		} else {
			prescale = 1.0;
			scerror("Prescale disabled.");
		}
		doneok = TRUE;
		break;
	case 2: 
		extfunc = (! extfunc);
		sprintf(stringbuf,"External functions %sabled.",
			extfunc? "en" : "dis");
		scerror(stringbuf);
		doneok = TRUE;
		break;
	case 3:
		++craction;
		if (craction >= 3)
			craction = 0;
		switch(craction) {
			case CRCOLS:
				scerror("Down row after new line");
				break;
			case CRROWS:
				scerror("Right column after new line");
				break;
			default:
				craction = 0; /* fall through */
			case 0:
				scerror("No action after new line");
				break;
		}
		doneok = TRUE;
		break;
	case 4:
		rowlimit = currow;
		collimit = curcol;
		scerror("Row and column limits set");
		doneok = TRUE;
		break;
	case 5:
		(void) sprintf (line, "set ");
    scerror("Options: byrows, bycols, iterations=n, tblstyle=(0|tbl|latex|tex)");
		linelim = strlen (line);
		break;
    }
    if (doneok)
    {	FullUpdate++;
	modflg++;
    }
}


/*
 *============================================================================
 */

static void
File_Menu(opt)
int	opt;
{
    int	doneok /* ,length */;
    char	*defwfile;
    static char *item[] = {
			"Get",
			"Put",
			"Write",
			"Table",
			"Read",
			"Merge",
			"Combine",
			"Directory"
    };

    static char *help_prompt[] = {
		"Get a new database from a file",
		"Put the current database into a file",
		"Write the current database into a file in its screen format",
		"Write the current database to a file in table format",
		"Read text file as strings into a range",
		"Merge files",
		"Combine macro files",
		"Set directory"
    };

    doneok = FALSE;
    switch (opt >= 0 ? opt : menu(8, item, help_prompt)) {
	case 0:
		if (opt < 0)
			Main_Menu();
		break;
	case 1:		/* Get */
		if (curfile)
		{	defwfile = what_file(curfile, NULL);
			(void) sprintf (line, "get [\"source\"] \"%s",
					defwfile);
			scxfree(defwfile);
		}
		else
			(void) sprintf (line, "get [\"source\"] \"");
				
		doneok = TRUE;
		break;
	case 2:		/* Put */
		if (*curfile)
		{	defwfile = what_file(curfile, NULL);
			(void) sprintf (line, "put [\"dest\" range] \"%s", 
					defwfile);
			scxfree(defwfile);
		}
		else
			(void) sprintf (line, "put [\"dest\" range] \"");
		doneok = TRUE;
		break;
	case 3:		/* Write */
		if (*curfile)
		{	defwfile = what_file(curfile, ".asc");
			(void) sprintf (line, "write [\"dest\" range] \"%s", 
					defwfile);
			scxfree(defwfile);
		}
		else
			(void) sprintf (line, "write [\"dest\" range] \"");
		doneok = TRUE;
		break;

	case 4:	/* tbl output */
		if (*curfile)
		{	char	*defwfile;

			/* what_file() scmalloc's space */
			defwfile = what_file(curfile, printfile_suffix());
			(void) sprintf(line, "tbl [\"dest\" range] \"%s", 
				       defwfile);
			scxfree(defwfile);
		}
		else
			(void) sprintf(line, "tbl [\"dest\" range] \"");
		doneok = TRUE;
		break;

	case 5:	/* Read */
		if (*curfile)
		{	char	*defwfile;

			/* what_file() scmalloc's space */
			defwfile = what_file(curfile, NULL);
			(void) sprintf (line, "read [\"source\" range] \"%s", 
					defwfile);
			scxfree(defwfile);
		}
		else
			(void) sprintf (line, "read [\"source\" range] \"");
		doneok = TRUE;
		break;

	case 6:	/* Merge */
		if (*curfile)
		{	char	*defwfile;

			/* what_file() scmalloc's space */
			defwfile = what_file(curfile, NULL);
			(void) sprintf(line,"merge [\"merge_file\"] \"%s", 
				       defwfile);
			scxfree(defwfile);
		}
		else
			(void) sprintf(line,"merge [\"merge_file\"] \"");
		doneok = TRUE;
		break;

	case 7:	/* Combine */
		if (*curfile)
		{	char	*defwfile;

			/* what_file() scmalloc's space */
			defwfile = what_file(curfile, NULL);
			(void) sprintf(line,"merge [\"macro_file\"] \"%s", 
				       defwfile);
			scxfree(defwfile);
		}
		else
		(void) sprintf(line,"merge [\"macro_file\"] \"");
/*
		get_default_dir(tmp);
		if (mdir)
			(void) sprintf (line,"merge [\"macro_file\"] \"%s/%s", 
					mdir, tmp);
		else
			(void) sprintf (line,"merge [\"macro_file\"] \"%s",tmp);
*/
		doneok = TRUE;
		break;

	case 8:	/* Directory */
		if (mdir)
			(void) sprintf(line, "mdir [\"macro_dir\"] \"%s",mdir);
		else
			(void) sprintf(line, "mdir [\"macro_dir\"] \"");
		doneok = TRUE;
		break;
    } /* switch */

    if (doneok)	/* wrote something to edit */
    {	linelim = strlen (line);
	insert_mode();
	show_top_line();
    }
}


/*
 *===========================================================================
 */
/****************************************************************************/
/*                                                                          */
/* The follwing function displays the Matrix_Menu                           */
/*                                                                          */
/*                                           - Fang Wang     12/91          */
/****************************************************************************/

static void
Matrix_Menu()
{   /* int	doneok; */

    static char *item[] = {
			"Transpose",
			"Addition",
			"Subtraction",
			"Multiplication",
			"Invert"
    };
    static char *help_prompt[] = {
			"Transpose the matrix",
			"Add two matrices",
			"Subtract the 2nd matrix from the 1st one",
			"Multiply the 1st matrix by the 2nd one",
			"Invert the matrix"
    };

    /* doneok = FALSE; */
    switch (menu(5, item, help_prompt)) {
	case 0:
		Main_Menu();
		break;
	case 1:
		get_trans();
		break;
	case 2:
		get_add();
		break;
	case 3:
		get_sub();
		break;
	case 4:
                get_mult();
		break;
	case 5:
	/*(void) sprintf(line,"matrix_inv [dest_range source_range] ");*/
		/*doneok = TRUE;*/
		get_invert();
		break;
	default:
		scerror("Invalid region command");
		break;
    }

    /*if (doneok)	 wrote something to edit 
    {	linelim = strlen (line);
	startshow();
	insert_mode();
	show_top_line();
    }*/
}

/*============================================================================
*/

void
Main_Menu()
{
    static char *item[] = {
			"Range",
			"Column/Row",
			"Option",
			"File",
			"Graph",
			"Matrix",
                        "Sort" ,
                        "Search",
                        "Color",
			"Quit"
    };

    static char *help_prompt[] = {
	"Erase  Value  Copy  Fill  Define  Show  Undefine Lock Unlock Fmt",
	"Insert  Append  Delete  Pull  Remove  Hide  Show  Format",
	"Auto  Numeric  Top  Cell  Encrypt  Pre-scale  Ext-funcs  Set",
	"Get  Put  Write  Table  Merge  Combine  Directory",
	"Type  A  B  C  D  E  F  Reset  View  Option",
	"Transpose  Addition  Substraction  Multiplication  Invert ",
        "Column/Row Dec/Inc Label/Value Reset",
        "Next Label/Value Column/Row EntireTable/Range Reset",
        "Foreground Background",
	"Quit this spreadsheet"
    };

      /* switch(menu(7, item, help_prompt)) { */ /* trial */
      switch(menu(10, item, help_prompt)) {
	case 0:
		show_top_line();
		break;
	case 1:
		Range_Menu(-1);
		break;
	case 2:
		Row_Col_Menu(-1);
		break;
	case 3:
		Option_Menu1(-1);
		break;
	case 4:
		File_Menu(-1);
		break;
	case 5:
#ifdef HAVE_X11_X_H
		if (using_X)
			Graph_Menu();
		else
#endif /* HAVE_X11_X_H */
			scerror("Graphing is not yet supported in curses");

		break;
	case 6:
		Matrix_Menu();
		break;
        case 7:
                Sort_Menu();
                break;
        case 8: 
                Search_Menu();
                break;
        case 9: 
#ifdef HAVE_X11_X_H	/* many things are X specific ... */
		if (using_X)
			Color_Menu();
		else
#endif /* HAVE_X11_X_H */
			message("Color curses isn't yet supported");

		break;
	case 10:
		running = FALSE;
		break;
      }
}

#ifdef HAVE_X11_X_H
/*** B. Backman 7-27-91
 * redraw_current will redraw the cell at the cursor, using the graphics
 * context specified by the argument gc.  The current cursor position on 
 * the screen must be stored in lastmx and lastmy (the col. and row), the 
 * string containing the cursor is stored in laststring and covers table
 * columns lstringstart through lstringend.  Since the cursor is in there,
 * we must, therefore have:  lstrinstart <= lastcol <= lstringend. 
 ***/
static void
redraw_current(gc)
  GC gc;
{
    /*
	int i;
	int skip = 0;
    */

	XDrawImageString(dpy, mainwin, gc, 0, 0, "curr", 4);
/* FIXME
	for (i=lstringstart; i<curcol; i++)
		skip += fwidth[i];
	XDrawImageString(dpy, mainwin, gc,
			 textcol(lastmx+skip), textrow(lastmy),
			 laststring+skip,
			 ((fwidth[curcol] < (int)strlen(laststring+skip)) 
				? fwidth[curcol] : strlen(laststring+skip)));
*/
}
#endif /* HAVE_X11_X_H */

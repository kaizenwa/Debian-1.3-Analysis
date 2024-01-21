/*	SC	A Spreadsheet Calculator
 *		Curses based Screen driver
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		More mods by Peter Doemel, 2/93: static prvstring in update
 *		Currently supported by gator!sawmill!buhrt (Jeff Buhrt)
 *		$Revision: 1.1 $
 *
 */

#include <config.h>
#include <curses.h>

#ifdef HAVE_X11_X_H
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif /* HAVE_X11_X_H */

#include "sc.h"
#ifdef HAVE_X11_X_H
#include "scXstuff.h"
#endif /* HAVE_X11_X_H */

#ifdef TIOCGWINSZ
# include <sys/types.h>
# ifdef HAVE_SYS_STREAM_H
#  include <sys/stream.h>
# endif
	/* was: ifndef QENAB	  this works for AT&T SvsV R3.2 */
# ifdef HAVE_SYS_PTEM_H
#   include <sys/ptem.h>
# endif
#endif

#ifdef VMS
extern int VMS_read_raw;   /*sigh*/
    VMS_read_raw = 1;
#endif

#ifdef CURSES_NO_NONL_BUG
		/* nl/nonl bug fix */
#undef nl
#undef nonl
#define nl()	 (_tty.sg_flags |= CRMOD,_pfast = _rawmode,stty(_tty_ch, &_tty))
#define nonl()	 (_tty.sg_flags &= ~CRMOD, _pfast = TRUE, stty(_tty_ch, &_tty))
#endif

char	under_cursor = ' '; /* Data under the < cursor */
char	mode_ind = '.';
extern	char    *revmsg;

int prvmx, prvmy, prvcol;   /* line added by Bob Parbs 12-92 */

int	rows, lcols;
int	lastmx, lastmy;	/* Screen address of the cursor */
int	lastcol;	/* Spreadsheet Column the cursor was in last */
extern	int *fwidth;
extern	int showrange;	/* Causes ranges to be highlighted	*/
extern	int showneed;	/* Causes cells needing values to be highlighted */
extern	int showexpr;	/* Causes cell exprs to be displayed, highlighted */
extern	int showcursor;	/* Causes arrow cursor to be displayed, highlighted */
#ifdef RIGHT_CBUG
extern	int wasforw;	/* Causes screen to be redisplay if on lastcol */
#endif

int called = 0;
/*
 * update() does general screen update
 *
 * standout last time in update()?
 *	At this point we will let curses do work
 */
int	standlast	= FALSE;

void
update (anychanged)
int	anychanged;	/* did any cell really change in value? */
{
    register    row, col;
    register struct ent **pp;
    int	mxrow, mxcol;
    int	minsr = 0, minsc = 0, maxsr = 0, maxsc = 0;
    register r;
    register i;
    static	int	lastcurcol = -1, lastcurrow = -1;
#if 0	/* this was the original code of xspread 2.1 */
    char field[FBUFLEN]; /*moved by B.P. 12-92 */
    char prvstring[FBUFLEN];
#else	/* the modified (better working) code looks like this:
	 * (Peter Doemel, 10-Feb-1993)
	 */
    char field[FBUFLEN];
#ifdef HAVE_X11_X_H
    static char prvstring[FBUFLEN];
#endif /* HAVE_X11_X_H */

#endif
    /*
     * place the cursor on the screen, set col, curcol, stcol, lastcol as
     * needed
     */
if ((curcol != lastcurcol) || FullUpdate)
    {
	while (col_hidden[curcol])   /* You can't hide the last row or col */
		curcol++;

	/* First see if the last display still covers curcol */
	if (stcol <= curcol){
		for (i = stcol, lcols = 0, col = RESCOL;
		      (col + fwidth[i]) < maintextcols-1 && i < maxcols; i++) {
			lcols++;

			if (col_hidden[i])
				continue;
			col += fwidth[i];
		}
	}
	while (stcol + lcols - 1 < curcol || curcol < stcol) {
		FullUpdate++;
		if (stcol - 1 == curcol) {    /* How about back one? */
			stcol--;
		} else if (stcol + lcols == curcol) {   /* Forward one? */
			stcol++;
		} else {
			/* Try to put the cursor in the center of the screen */
			col = (maintextcols - RESCOL - fwidth[curcol]) / 2 + RESCOL; 
			stcol = curcol;
			for (i=curcol-1; i >= 0 && col-fwidth[i] > RESCOL; i--)
			{	stcol--;
				if (col_hidden[i])
					continue;
				col -= fwidth[i];
			}
		}
		/* Now pick up the counts again */
		for (i = stcol, lcols = 0,col = RESCOL;
			(col + fwidth[i]) < maintextcols-1 && i < maxcols; i++) {
			lcols++;
			if (col_hidden[i])
				continue;
			col += fwidth[i];
		}
	}
	lastcurcol = curcol;
    }

    /* Now - same process on the rows as the columns */
    if ((currow != lastcurrow) || FullUpdate)
    {
	while (row_hidden[currow])   /* You can't hide the last row or col */
		currow++;
	if (strow <= currow){
		for (i = strow, rows = 0, row=RESROW; row<maintextrows && i<maxrows; i++)
		{	rows++;
			if (row_hidden[i])
				continue;
			row++;
		}
	}

	while (strow + rows - 1 < currow || currow < strow) {
		FullUpdate++;
		if (strow - 1 == currow) {    /* How about up one? */
			strow--;
		} else if (strow + rows == currow) {   /* Down one? */
			strow++;
		} else {
			/* Try to put the cursor in the center of the screen */
			row = (maintextrows - RESROW) / 2 + RESROW; 
			strow = currow;
			for (i=currow-1; i >= 0 && row-1 > RESROW; i--) {
				strow--;
				if (row_hidden[i])
					continue;
				row--;
			}
		}
		/* Now pick up the counts again */
		for (i = strow, rows = 0, row=RESROW; row<maintextrows && i<maxrows; i++) {
			rows++;
			if (row_hidden[i])
				continue;
			row++;
		}
	}
	lastcurrow = currow;
    }
    mxcol = stcol + lcols - 1;
    mxrow = strow + rows - 1;

    /* Get rid of cursor standout on the cell at previous cursor position */
#ifdef HAVE_X11_X_H
    if (using_X)
    {
	if (showcell && !FullUpdate) {
		XDrawImageString(dpy, mainwin, invertgc,
			textcol(prvmx), textrow(prvmy),
		         prvstring, strlen(prvstring) );
		XFillRectangle(dpy, mainwin, invertgc, textcol(prvmx),
		       (textrow(prvmy-1)+4),
		       textcol(fwidth[prvcol]),textrow(0));
      }
    } else /* curses below */
#endif /* HAVE_X11_X_H */
    {	if (!FullUpdate)
	{	if (showcell)
			repaint(lastmx, lastmy, fwidth[lastcol]);

		(void) move(lastmy, lastmx+fwidth[lastcol]);

		if ((inch() & A_CHARTEXT ) == '<')
			(void) addch(under_cursor);
	}
    } /* HAVE_X11_X_H, end curses */

    /* where is the the cursor now? */
    lastmy =  RESROW;
    for (row = strow; row < currow; row++)
	if (!row_hidden[row])
		lastmy++;

    lastmx = RESCOL;
    for (col = stcol; col < curcol; col++)
	if (!col_hidden[col])
		lastmx += fwidth[col];
    lastcol = curcol;

    /* relabel the rows and columns if a full update is performed */
    if (FullUpdate || standlast) {
#ifdef HAVE_X11_X_H
	if (using_X)
		cleardownfrom(2);
	else
#endif /* HAVE_X11_X_H */
	{	(void) move(2, 0);
		(void) clrtobot();
		(void) standout();
	} /* HAVE_X11_X_H, end curses */

	for (row=RESROW, i=strow; i <= mxrow; i++) {
		if (row_hidden[i]) 
			continue;
#ifdef HAVE_X11_X_H
		if (using_X)
		{
		   if (maxrows < 1000)
			sprintf(stringbuf, "%-*d", RESCOL-1, i);
		   else
			sprintf(stringbuf, "%-*d", RESCOL, i);
		   XDrawImageString(dpy, mainwin, maingcreversed,
			textcol(0), textrow(row),
			stringbuf, strlen(stringbuf) );
		} else
#endif /* HAVE_X11_X_H */
		{   (void) move(row,0);
		   if (maxrows < 1000)
			(void) printw("%-*d", RESCOL-1, i);
		   else
			(void) printw("%-*d", RESCOL, i);
		} /* HAVE_X11_X_H, end curses */

		row++;
	}

#ifdef HAVE_X11_X_H
	if (using_X)
	{
		/* the following will work as long as RESCOL<20.  It is 
		 * more efficient than the usual sprintf(stringbuf..) and
		 * XDrawString(...strlen(stringbuf)) combination
		 */
		XDrawImageString(dpy,mainwin,maingcreversed,
			 textcol(0),textrow(3),
			 "                    ",RESCOL);
	} else
#endif /* HAVE_X11_X_H */
	{
#ifdef RIGHT_CBUG
		if(wasforw) {
			clearok(stdscr, TRUE);
			wasforw = 0;
		}
#endif
		(void) move(RESCOL-1, 0);
		(void) printw("%*s", RESCOL, " ");
	} /* HAVE_X11_X_H, end curses */

	for (col=RESCOL, i = stcol; i <= mxcol; i++) {
	    register int k;
	    if (col_hidden[i])
		continue;
	    if (!using_X)
		(void) move(RESCOL-1, col);

	    k = fwidth[i]/2;
#ifdef HAVE_X11_X_H
	    if (using_X)
	    {
		if (k == 0)
		    sprintf(stringbuf,"%1s", coltoa(i));
		else
		    sprintf(stringbuf,"%*s%-*s", k, " ", fwidth[i]-k,
			    coltoa(i));
		XDrawImageString(dpy,mainwin,maingcreversed,
                             textcol(col), textrow(3),
                             stringbuf, strlen(stringbuf) );
	    } else
#endif /* HAVE_X11_X_H */
	    {	if (k == 0)
		    (void) printw("%1s", coltoa(i));
		else
		    (void) printw("%*s%-*s", k, " ", fwidth[i]-k, coltoa(i));
	    } /* HAVE_X11_X_H, end curses */

	    col += fwidth[i];
	}
	if (!using_X)
		(void) standend();
    }

    if (showrange) {
	minsr = showsr < currow ? showsr : currow;
	minsc = showsc < curcol ? showsc : curcol;
	maxsr = showsr > currow ? showsr : currow;
	maxsc = showsc > curcol ? showsc : curcol;

	if (showtop) {
#ifdef HAVE_X11_X_H
	    if (using_X)
	    {	sprintf(stringbuf,"Default range:  %s",
			    r_name(minsr, minsc, maxsr, maxsc)); 
		XDrawImageString(dpy,mainwin,maingc,
                             textcol(0), textrow(1),
                             stringbuf, strlen(stringbuf) );
	    } else
#endif /* HAVE_X11_X_H */
	    {	(void) move(1,0);
		(void) clrtoeol();
		(void) printw("Default range:  %s",
			    r_name(minsr, minsc, maxsr, maxsc));
	    } /* HAVE_X11_X_H, end curses */
	}
    }

#ifdef FIXME
    /* if the current cell was previously painted over by laststring, we
     * don't need to look it up again, unless it has changed.  So, we can 
     * say we have the cursor accounted for */
    if ((row == currow) && (lstringstart <= curcol) && (lstringend >= curcol))
    {	found_cursor = 1;
    }
#endif

    /* Repaint the visible screen */
    if (showrange || anychanged || FullUpdate || standlast)
    {
	/* may be reset in loop, if not next time we will do a FullUpdate */
      if (standlast)
      {	FullUpdate = TRUE;
	standlast = FALSE;
      }

      for (row = strow, r = RESROW; row <= mxrow; row++) {
	register c = RESCOL;
	int do_stand = 0;
	int fieldlen;
	int nextcol;

	if (row_hidden[row])
	    continue;
	for (pp = ATBL(tbl, row, col = stcol); col <= mxcol;
	         pp += nextcol - col,  col = nextcol, c += fieldlen) {

	    nextcol = col+1;
	    if (col_hidden[col]) {
		fieldlen = 0;
		continue;
	    }

#ifdef FIXME
	    /*
	     * if we are on the cursor row, and we have not accounted for the
	     * cursor, look at contents of all cells, in order to get correct
	     * laststring
	     */
	    if ((row == currow) && !found_cursor)
		get_value = 1;
#endif

	    fieldlen = fwidth[col];

	    /*
	     * Set standout if:
	     *
	     * - showing ranges, and not showing cells which need to be filled
	     *	 in, and not showing cell expressions, and in a range, OR
	     *
	     * - if showing cells which need to be filled in and this one is
	     *	 of that type (has a value and doesn't have an expression,
	     *	 or it is a string expression), OR
	     *
	     * - if showing cells which have expressions and this one does.
	     */
	    if ((showrange && (! showneed) && (! showexpr)
			   && (row >= minsr) && (row <= maxsr)
			   && (col >= minsc) && (col <= maxsc))
		    || (showneed && (*pp) && ((*pp) -> flags & is_valid) &&
			  (((*pp) -> flags & is_strexpr) || !((*pp) -> expr)))
		    || (showexpr && (*pp) && ((*pp) -> expr)))
	    {
		if (!using_X)
		{	(void) move(r, c);
			(void) standout();
		}

		standlast++;
		if (!*pp)	/* no cell, but standing out */
		{
#ifdef HAVE_X11_X_H
		    if (using_X)
		    {	sprintf(stringbuf, "%*s", fwidth[col], " ");
			XDrawImageString(dpy, mainwin, maingcreversed,
					 textcol(c), textrow(r),
					 stringbuf, strlen(stringbuf) );
		    } else
#endif /* HAVE_X11_X_H */
		    {	(void) printw("%*s", fwidth[col], " ");
			(void) standend();
		    } /* HAVE_X11_X_H, end curses */
		    continue;
		}
		else
		    do_stand = 1;
	    }
	    else
		do_stand = 0;

	    if ((*pp) && (((*pp) -> flags & is_changed || FullUpdate) || do_stand)) {
		if (do_stand) {
		    (*pp) -> flags |= is_changed; 
		} else {
		    if (!using_X)
			(void) move(r, c);

		    (*pp) -> flags &= ~is_changed;
		}

		/*
		 * Show expression; takes priority over other displays:
		 */

		if ((*pp)->cellerror) {
#ifdef HAVE_X11_X_H
		    if (using_X)
		    {	sprintf(stringbuf, "%*.*s", fwidth[col], fwidth[col],
			  (*pp)->cellerror == CELLERROR ? "ERROR" : "INVALID");
			XDrawImageString(dpy,mainwin,
				    do_stand ? maingcreversed : maingc,
				    textcol(c),textrow(r),
				    stringbuf, strlen(stringbuf));
		    } else
#endif /* HAVE_X11_X_H */
			(void) printw("%*.*s", fwidth[col], fwidth[col],
			  (*pp)->cellerror == CELLERROR ? "ERROR" : "INVALID");
		}
		else
		if (showexpr && ((*pp) -> expr)) {
		    linelim = 0;
		    editexp(row, col);		/* set line to expr */
		    linelim = -1;
		    showstring(line, /* leftflush = */ 1, /* hasvalue = */ 0,
				row, col, & nextcol, mxcol, & fieldlen, r, c, do_stand);
		} else {
		    /*
		     * Show cell's numeric value:
                     */

		    if ((*pp) -> flags & is_valid) {
			/*char field[FBUFLEN]; moved to top fnctn B.P. */

			if ((*pp) -> format) {
				(void) format((*pp) -> format, (*pp) -> v,
					     field, sizeof(field));
			} else {
				(void) engformat(realfmt[col], fwidth[col], 
                                             precision[col], (*pp) -> v, 
                                             field, sizeof(field));
			}
                        if (strlen(field) > fwidth[col]) {
#ifdef HAVE_X11_X_H
			   if (using_X)
			   {
				for(i = 0; i<fwidth[col]; i++)
					field[i] = '*';
				field[fwidth[col]] = '\0';

/* following statement to print case of field too long Bob Parbs 12-92 */

				XDrawImageString(dpy,mainwin,
                                  do_stand ? maingcreversed : maingc,
                                  textcol(c + fwidth[col] - strlen(field)),
                                  textrow(r), field, strlen(field));
			   } else
#endif /* HAVE_X11_X_H */
				for(i = 0; i<fwidth[col]; i++)
					(void)addch('*');

			} else {
#ifdef HAVE_X11_X_H
			    if (using_X)
			    {
				XDrawImageString(dpy,mainwin,
				    do_stand ? maingcreversed : maingc,
				    textcol(c + fwidth[col] - strlen(field)),
				    textrow(r), field, strlen(field));
#if 0	/* this was the original code of xspread 2.1 */
                                    *prvstring = *field;
#else	/* I'm not sure what the intention was for this, but I think
	 * the following looks safer: (Peter Doemel, 10-Feb-1993)
	 */
				    strncpy( prvstring, field, FBUFLEN);
#endif
			    } else
#endif /* HAVE_X11_X_H */
			    {	for(i = 0; i < fwidth[col] - strlen(field);i++)
					(void)addch(' ');
				(void)addstr(field);
			    } /* HAVE_X11_X_H, end curses */
			}
		    }

		    /*
		     * Show cell's label string:
		     */

		    if ((*pp) -> label) {
			showstring((*pp) -> label,
				    (*pp) -> flags & (is_leftflush|is_label),
				    (*pp) -> flags & is_valid,
				    row, col, & nextcol, mxcol,
				    & fieldlen, r, c, do_stand);
		    }
		    else	/* repaint a blank cell: */
		    if ((do_stand || !FullUpdate) &&
				((*pp)->flags & is_changed) &&
				!((*pp)->flags & is_valid) && !(*pp)->label) {
#ifdef HAVE_X11_X_H
			if (using_X)
			{
				sprintf (stringbuf, "%*s", fwidth[col], " ");
				XDrawImageString(dpy,mainwin,
				    do_stand ? maingcreversed : maingc,
				    textcol(c),textrow(r),
				    stringbuf, strlen(stringbuf));
			} else
#endif /* HAVE_X11_X_H */
				(void) printw("%*s", fwidth[col], " ");
		    }
		} /* else */

		if (do_stand) {
		    if (!using_X)
			(void) standend();

		    do_stand = 0;
		}
	    }
	}
	r++;
      }
    }
#ifdef HAVE_X11_X_H
    if (using_X)
    {
	called++;

/* next two statements commented out by Bob Parbs 12-92 */

/*sprintf(stringbuf,"tc1 %d, tr1 %d, tc2 %d, tr2 %d (%d, %d), called %d",
	textcol(lastmx), textrow(lastmy),
	textcol(lastmx+fwidth[lastcol]), textrow(lastmy+1),
	textcol(fwidth[lastcol]), textrow(1), called);*/
/*XDrawImageString(dpy, mainwin, maingc,
	textcol(0), textrow(5), stringbuf, strlen(stringbuf));*/

/* following statement modified by Bob Parbs to fix cursor position, size 12-92 */

	if (called) {
	    XFillRectangle(dpy,mainwin, invertgc, textcol(lastmx),
		       (textrow(lastmy-1)+4),
		       textcol(fwidth[lastcol]), textrow(0));
/*		       textcol(lastmx+fwidth[lastcol]), curfontheight);*/
/*		       textrow(lastmy+1));*/

	    prvmx = lastmx;   /* three statements added by Bob Parbs 12-92 */
	    prvmy = lastmy;   /* to prevent screen write if no cursor      */
	    *prvstring = '\0';/* movement but other command executed.      */
	}
    } else
#endif /* HAVE_X11_X_H */
    {	/* place 'cursor marker' */
	if (showcell && (! showneed) && (! showexpr)) {
	    (void) move(lastmy, lastmx);
	    (void) standout();
	    repaint(lastmx, lastmy, fwidth[lastcol]);
	    (void) standend();
	}
	(void) move(lastmy, lastmx+fwidth[lastcol]);
	under_cursor = (inch() & A_CHARTEXT);
	if ( showcursor )
	    (void) addch('<');
    } /* HAVE_X11_X_H, end curses */

    (void) show_top_line();

    FullUpdate = FALSE;
}

/* redraw what is under the cursor from curses' idea of the screen */
void
repaint(x, y, len)
int x, y, len;
{
    int c;

    while(len-- > 0) {
	(void) move(y, x);
	c = inch() & A_CHARTEXT;
	(void) addch(c);
	x++;
    }
}

int seenerr;

/* error routine for yacc (gram.y) */
void
yyerror(err)
char *err; {
    if (seenerr) return;
    seenerr++;

    clearlines(1,1);
#ifdef HAVE_X11_X_H
    if (using_X)
    {    sprintf(stringbuf,"%s: %.*s<=%s",err,linelim,line,line+linelim);
	XDrawImageString(dpy, mainwin, maingc,
		     textcol(0), textrow(1), stringbuf, strlen(stringbuf));
	XFlush(dpy);
    } else
#endif /* HAVE_X11_X_H */
	(void) printw("%s: %.*s<=%s",err,linelim,line,line+linelim);
}

#ifdef XENIX2_3
struct termio tmio;
#endif

void
startdisp()
{
#if sun
    int	 fd;
    fd = dup(0);
#endif

#ifdef TIOCGSIZE
    {	struct ttysize size;
	if (ioctl(0, TIOCGSIZE, &size) == 0)
	{ 	maintextrows = LINES = size.ts_lines;
		maintextcols = COLS = size.ts_cols;
	}
    }
#endif

#if defined(TIOCGWINSZ) && !defined(SYSV3)
{
	struct	winsize w;

	if(ioctl(0, TIOCGWINSZ, &w) == 0)
	{	maintextrows = LINES = w.ws_row;
		maintextcols = COLS  = w.ws_col;
	}
}
#endif

#ifdef XENIX2_3
    (void) ioctl (fileno (stdin), TCGETA, & tmio);
#endif
    (void) initscr();
#if sun
    close(0);
    dup(fd);
    close(fd);
#endif
    (void) clear();
#ifdef VMS
    VMS_read_raw = 1;
#else
    nonl();
    noecho ();
    cbreak();
#endif
    initkbd();
    scrollok(stdscr, 1);

/*#if defined(SYSV3) && !defined(NOIDLOK)*/
#if defined(HAVE_IDLOK)
# ifndef IDLOKBAD
    /*
     * turn hardware insert/delete on, if possible.
     * turn on scrolling for systems with SYSVr3.{1,2} (SYSVr3.0 has this set
     * as the default)
     */
     idlok(stdscr,TRUE);
# else	/*
	 * This seems to fix (with an empty spreadsheet):
	 *	a) Redrawing the bottom half of the screen when you
	 *		move between row 9 <-> 10
	 *	b) the highlighted row labels being trash when you
	 *		move between row 9 <-> 10
	 *	c) On an xterm on Esix Rev. D+ from eating lines
	 *	 -goto (or move) a few lines (or more) past the bottom
	 *	 of the screen, goto (or move) to the top line on the
	 *	 screen, move upward and the current line is deleted, the
	 *	 others move up even when they should not, check by
	 *	 noticing the rows become 2, 3, 40, 41, 42... (etc).
	 */
     idlok(stdscr,FALSE);
# endif
#endif

	/* safety, also needed when stdin is a file vs a tty */
    if (maintextrows == 0)
	maintextrows = LINES;
    if (maintextcols == 0)
	maintextcols = COLS;

    FullUpdate++;
}

void
stopdisp()
{
    if (!using_X)
    {   deraw();
	resetkbd();
	endwin();
#ifdef XENIX2_3
	(void) ioctl (fileno (stdin), TCSETAW, & tmio);
#endif
    }
}

/* init curses */
#ifdef VMS

/* start up curses (set to curses  mode) */
goraw()
{
    VMS_read_raw = 1;
    FullUpdate++;
}

/* clean up curses (reset to normal terminal mode) */
deraw()
{
    if (!using_X)
    {	(void) move (maintextrows - 1, 0);
	(void) clrtoeol();
	(void) refresh();
    }
    VMS_read_raw = 0;
}

#else /* VMS */

/* start up curses (set to curses  mode) */
void
goraw()
{
/* was: #if SYSV2 || SYSV3 */
#ifdef HAVE_RESET_PROG_MODE
    reset_prog_mode();
#elif defined(HAVE_FIXTERM)
    fixterm();
#else
    cbreak();
    nonl();
    noecho ();
#endif
    kbd_again();
    (void) clear();
    FullUpdate++;
}

/* clean up curses (reset to normal terminal mode) */
void
deraw()
{
    (void) move (maintextrows - 1, 0);
    (void) clrtoeol();
    (void) refresh();
/* was: if SYSV2 || SYSV3 || _AIX */
#ifdef HAVE_RESET_TERM_MODE
    reset_term_mode();
#elif defined(HAVE_RESETTERM)
    resetterm();
#else
    nocbreak();
    nl();
    echo();
#endif
    resetkbd();
}
#endif /* VMS */

void
show_top_line()
{   
    clearlines(0,0);

    stringbuf[0] = '\0';
    if (linelim >= 0) {
	sprintf(stringbuf,"%c> %s_", mode_ind, line);
#ifdef HAVE_X11_X_H
	if (using_X)
	{	XDrawImageString(dpy, mainwin, maingc,
			textcol(0), textrow(0), stringbuf, linelim+3);
		XDrawImageString(dpy, mainwin, maingcreversed,
			textcol(linelim+3), textrow(0),	&stringbuf[linelim+3],
			1);
		XDrawImageString(dpy, mainwin, maingc,
			textcol(linelim+4), textrow(0), &stringbuf[linelim+4],
			strlen(stringbuf)-linelim-4);
	} else
#endif /* HAVE_X11_X_H */
	{	(void) addstr(stringbuf);
		(void) move((linelim + 3) / maintextcols,
			    (linelim+3) % maintextcols);
	}
    }
    else {
      if (showtop) {			/* show top line */
	    register struct ent *p1;
	    int printed = 0;		/* printed something? */
	    
            /* show the current cell's format */
	    if ((p1 = *ATBL(tbl, currow, curcol)) && p1->format)
		sprintf(stringbuf, "%s%d (%s) ", coltoa(curcol), currow,
			p1->format);
	    else
		sprintf(stringbuf, "%s%d (%d %d %d) ", coltoa(curcol), currow,
			fwidth[curcol], precision[curcol], realfmt[curcol]);

	    if (p1) {
		if (p1 -> expr) {
		    /* has expr of some type */
		    linelim = 0;
		    editexp(currow, curcol);	/* set line to expr */
		    linelim = -1;
		}

		/*
		 * Display string part of cell:
		 */

		if ((p1 -> expr) && (p1 -> flags & is_strexpr)) {
 		    if( (p1-> flags & is_label) )
			strcat(stringbuf, "|{");
		    else
			strcat(stringbuf, ((p1 -> flags & is_leftflush) ? "<{" : ">{"));
		    strcat(stringbuf, line);
		    strcat(stringbuf, "} ");	/* and this '}' is for vi % */
		    printed = 1;

		} else if (p1 -> label) {
		    /* has constant label only */
		    if( (p1-> flags & is_label) )
			strcat(stringbuf, "|\"");
		    else
			strcat(stringbuf, (p1 -> flags & is_leftflush) ? "<\"" : ">\"");
		    strcat(stringbuf, p1 -> label);
		    strcat(stringbuf, "\" ");
		    printed = 1;
		}

		/*
		 * Display value part of cell:
		 */

		if (p1 -> flags & is_valid) {
		    /* has value or num expr */
		    if ((! (p1 -> expr)) || (p1 -> flags & is_strexpr))
			(void) sprintf (line, "%.15g", p1 -> v);

		    strcat(stringbuf, "[");
		    strcat(stringbuf, line);
		    strcat(stringbuf, "]");
		    *line = '\0'; /* this is the input buffer ! */
		    printed = 1;
		}
	    }
	    if (! printed)
		strcat(stringbuf, "[]");
	    /* Display if cell is locked */
	    if (p1 && p1->flags&is_locked)
		strcat(stringbuf, " locked");

	    if (!using_X)
	    {	addstr(stringbuf);
		(void) move(lastmy, lastmx+fwidth[lastcol] + (showcursor ? 0 : -1 ));
	    }
	}
#ifdef HAVE_X11_X_H
	if (using_X)
	    XDrawImageString(dpy, mainwin, maingc,
			textcol(0), textrow(0), stringbuf, strlen(stringbuf));
#endif
    }

    if (revmsg) {
	clearlines(0,0);
#ifdef HAVE_X11_X_H
	if (using_X)
		XDrawImageString(dpy,mainwin,maingc,
			 textcol(0), textrow(0),
			 revmsg, strlen(revmsg));
	else
#endif /* HAVE_X11_X_H */
	{	(void) printw(revmsg);
		(void) move (lastmy, lastmx + fwidth[lastcol] + (showcursor ? 0 : -1));
	}

	scxfree(revmsg);
	revmsg = NULL;		/* don't show it again */
    }
    if (!using_X)
	refresh();

    FullUpdate = 0;
}

/*
 * function clearlines() clears the lines start through end, given in text
 * coordinates
 */
void
clearlines(start, end)
int start, end;
{
	int temp; /* swap variable */
	if (start>end)  
	{
		temp = end;
		end = start;
		start = temp;
	}
#ifdef HAVE_X11_X_H
	if (using_X)
	{  XClearArea(dpy,mainwin,
	     0,textrow(start) - curfontheight + 3,          /*top left corner of area */
	     0,                             /* 0 width => full window width*/
	     textrow(end) - textrow(start) + curfontheight  + 1,/* height of area */
	     0);                            /* don't send Expose events */
	} else
#endif /* HAVE_X11_X_H */
	for(; start <= end; start++)
	{	(void) move(start, 0);
		(void) clrtoeol ();	/* get rid of topline display */
	}
}


static char rcsid[] = "@(#)$Id: curses.c,v 5.18 1994/05/30 16:27:44 syd Exp $";

/*****************************************************************************
 *  The Elm Mail System  -  $Revision: 5.18 $   $State: Exp $
 *
 * 			Copyright (c) 1988-1992 USENET Community Trust
 * 			Copyright (c) 1986,1987 Dave Taylor
 *
 *****************************************************************************/

/**  This library gives programs the ability to easily access the
     termcap information and write screen oriented and raw input
     programs.  The routines can be called as needed, except that
     to use the cursor / screen routines there must be a call to
     InitScreen() first.  The 'Raw' input routine can be used
     independently, however.

**/

/** NOTE THE ADDITION OF: the #ifndef ELM stuff around routines that
    we don't use.  This is for code size and compile time speed...
**/

#include "headers.h"
#include <errno.h>
#include "me.h"

#ifdef TERMIOS
# include <termios.h>
# ifndef sun
#  include <sys/ioctl.h>	/* for TIOCGWINSZ */
# endif
#else
# ifdef TERMIO
#  include <termio.h>
# else
#  include <sgtty.h>
# endif
#endif

#include    <errno.h>

#ifdef PTEM
#  include <sys/stream.h>
#  include <sys/ptem.h>
#endif

#define TTYIN	0

#ifdef SHORTNAMES
# define _clearinverse	_clrinv
# define _cleartoeoln	_clrtoeoln
# define _cleartoeos	_clr2eos
# define _transmit_off	xmit_off
# define _transmit_on	xmit_on
#endif

#ifdef TERMIOS
struct termios _raw_tty,
	       _original_tty;
#define	ttgetattr(fd,where)	tcgetattr((fd),(where))
#define	ttsetattr(fd,where)	tcsetattr((fd),TCSADRAIN,(where))
#else	/*TERMIOS*/
# ifdef TERMIO
struct termio _raw_tty, 
              _original_tty;
#define	ttgetattr(fd,where)	ioctl((fd),TCGETA,(where))
#define	ttsetattr(fd,where)	ioctl((fd),TCSETAW,(where))
# else
struct tty_modes {
  struct sgttyb sgttyb;
  struct tchars tchars;
}  _raw_tty, _original_tty;
static int ttgetattr P_((int,struct tty_modes *));  /* Prototype */
static int ttsetattr P_((int,struct tty_modes *)); /* Prototype */
# endif	/*TERMIO*/
#endif	/*TERMIOS*/

static int _inraw = 0;                  /* are we IN rawmode?    */

#define DEFAULT_LINES_ON_TERMINAL	24
#define DEFAULT_COLUMNS_ON_TERMINAL	80

static int _memory_locked = 0;		/* are we IN memlock??   */
static int _line  = -1,			/* initialize to "trash" */
           _col   = -1;

static int _intransmit = -1;	        /* are we transmitting keys? */

static
char *_clearscreen = NULL, *_moveto = NULL, *_up = NULL, *_down = NULL, 
  *_right = NULL, *_left = NULL, *_setbold = NULL, *_clearbold = NULL, 
  *_setunderline = NULL, *_clearunderline = NULL, *_sethalfbright = NULL, 
  *_clearhalfbright = NULL, *_setinverse = NULL, *_clearinverse = NULL,
  *_cleartoeoln = NULL, *_cleartoeos = NULL, *_transmit_on = NULL, 
  *_transmit_off = NULL, *_set_memlock = NULL, *_clear_memlock = NULL, 
  *_start_termcap = NULL, *_end_termcap = NULL,

  *_key_up = NULL, *_key_down = NULL, *_key_left = NULL, *_key_right = NULL,
  *_key_pageup = NULL, *_key_pagedown = NULL, *_key_home = NULL, 
  *_key_help = NULL, *_key_find = NULL;

static int _lines, _columns, _automargin, _eatnewlineglitch;
int tabspacing;
static int tabexpand = 0;          /* Is terminal driver expanding tabs? */

static char _terminal[1024];              /* Storage for terminal entry */
static char _capabilities[1024];           /* String for cursor motion */

static char *ptr = _capabilities;	/* for buffering         */

static int  cursor_control    = 0;
static int  need_moveabsolute = 0;

int    outchar();			/* char output for tputs */
char  *tgetstr(),     		       /* Get termcap capability */
      *tgoto();				/* and the goto stuff    */

InitScreen()
{
	/* Set up all this fun stuff: returns zero if all okay, or;
        -1 indicating no terminal name associated with this shell,
        -2..-n  No termcap for this terminal type known
   */

	int  tgetent(),      /* get termcap entry */
	     err;
	char termname[40];
	char *termenv;
	
	if ((termenv = getenv("TERM")) == NULL) return(-1);

	if (strcpy(termname, termenv) == NULL)
		return(-1);

	if ((err = tgetent(_terminal, termname)) != 1)
		return(err-2);

	_line  =  0;		/* where are we right now?? */
	_col   =  0;		/* assume zero, zero...     */

	/* load in all those pesky values */
	_clearscreen       = tgetstr("cl", &ptr);
	_moveto            = tgetstr("cm", &ptr);
	_up                = tgetstr("up", &ptr);
	_down              = tgetstr("do", &ptr);
	_right             = tgetstr("nd", &ptr);
	_left              = tgetstr("bc", &ptr);
	_setbold           = tgetstr("so", &ptr);
	_clearbold         = tgetstr("se", &ptr);
	_setunderline      = tgetstr("us", &ptr);
	_clearunderline    = tgetstr("ue", &ptr);
	_setinverse        = tgetstr("so", &ptr);
	_clearinverse      = tgetstr("se", &ptr);
	_sethalfbright     = tgetstr("hs", &ptr);
	_clearhalfbright   = tgetstr("he", &ptr);
	_cleartoeoln       = tgetstr("ce", &ptr);
	_cleartoeos        = tgetstr("cd", &ptr);
	_lines	      	   = tgetnum("li");
	_columns	   = tgetnum("co");
	tabspacing	   = ((tabspacing=tgetnum("it"))<= 0 ? 8 : tabspacing);
	_automargin	   = tgetflag("am");
	_eatnewlineglitch   = tgetflag("xn");
	_transmit_on	   = tgetstr("ks", &ptr);
	_transmit_off      = tgetstr("ke", &ptr);
	_set_memlock	   = tgetstr("ml", &ptr);
	_clear_memlock	   = tgetstr("mu", &ptr);
	_start_termcap	   = tgetstr("ti", &ptr);
	_end_termcap	   = tgetstr("te", &ptr);

	_key_up            = tgetstr("ku", &ptr);
	_key_down          = tgetstr("kd", &ptr);
	_key_left          = tgetstr("kl", &ptr);
	_key_right         = tgetstr("kr", &ptr);
	_key_pageup        = tgetstr("kP", &ptr);
	_key_pagedown      = tgetstr("kN", &ptr);
	_key_home          = tgetstr("kh", &ptr);
	_key_help          = tgetstr("%1", &ptr);
	_key_find          = tgetstr("@0", &ptr);

	if (_transmit_on && _transmit_off && _key_up && _key_down) {
	  cursor_control = TRUE;
	}

	if (!_left) {
		_left = "\b";
	}

	return(0);
}

char *return_value_of(termcap_label)
char *termcap_label;
{
	/** This will return the string kept by termcap for the 
	    specified capability. Modified to ensure that if 
	    tgetstr returns a pointer to a transient address	
	    that we won't bomb out with a later segmentation
	    fault (thanks to Dave@Infopro for this one!)

	    Tweaked to remove padding sequences.
	 **/

	static char escape_sequence[20];
	register int i=0,j=0;
	char buffer[20];
	char *myptr, *tgetstr();     		/* Get termcap capability */

	if (strlen(termcap_label) < 2)
	  return(NULL);

	if (termcap_label[0] == 's' && termcap_label[1] == 'o')
	  {
	  if (_setinverse)
	    strcpy(escape_sequence, _setinverse);
	  else
	    return( (char *) NULL );
	  }
	else if (termcap_label[0] == 's' && termcap_label[1] == 'e')
	  {
	  if (_clearinverse)
	    strcpy(escape_sequence, _clearinverse);
	  else
	    return( (char *) NULL );
	  }
	else if ((myptr = tgetstr(termcap_label, &ptr)) == NULL)
	  return( (char *) NULL );
	else
	  strcpy(escape_sequence, myptr);

	if (chloc(escape_sequence, '$') != -1) {
	  while (escape_sequence[i] != '\0') {
	    while (escape_sequence[i] != '$' && escape_sequence[i] != '\0')
	      buffer[j++] = escape_sequence[i++];
	    if (escape_sequence[i] == '$') {
	      while (escape_sequence[i] != '>') i++;
	      i++;
	    }
	  }
	  buffer[j] = '\0';
	  strcpy(escape_sequence, buffer);
	}

	return( (char *) escape_sequence);
}

transmit_functions(newstate)
int newstate;
{
	/** turn function key transmission to ON | OFF **/

	if (newstate != _intransmit) {
		_intransmit = newstate;
		if (newstate == ON)
		  tputs(_transmit_on, 1, outchar);
		else 
		  tputs(_transmit_off, 1, outchar);
		fflush(stdout);      /* clear the output buffer */
	}
}

/****** now into the 'meat' of the routines...the cursor stuff ******/

void ScreenSize(lines, columns)
     int *lines, *columns;
{
	/** returns the number of lines and columns on the display. **/

#ifdef TIOCGWINSZ
	struct winsize w;

	if (ioctl(1,TIOCGWINSZ,&w) != -1) {
		if (w.ws_row > 0)
			_lines = w.ws_row;
		if (w.ws_col > 0)
			_columns = w.ws_col;
	}
#endif

	if (_lines == 0) _lines = DEFAULT_LINES_ON_TERMINAL;
	if (_columns == 0) _columns = DEFAULT_COLUMNS_ON_TERMINAL;


	/* WARNING: elm_LINES and elm_COLUMNS are inconsistent!
	 *
	 * elm_LINES    == number of lines in screen -1
	 * elm_COLUMNS  == number of rows in screen
	 *
	 *
	 * Check code in MoveCursor!
	 *
	 * row          == 0 .. elm_LINES
	 * col          == 0 .. elm_COLUMNS-1
	 *
	 *
	 * Who was this smart programmer!!!!!!!!!!!!!!!!!!!!!
	 *
	 *                         - K E H <hurtta@ozone.FMI.FI>
	 */

	*lines = _lines - 1;	     
	*columns = _columns;
}

SetXYLocation(x,y)
int x,y;
{
	/* declare where the cursor is on the screen - useful after using
	 * a function that moves cursor in predictable fasion but doesn't
	 * set the static x and y variables used in this source file -
	 * e.g. getpass().
	 */

	_line = x;
	_col = y;
}

GetXYLocation(x,y)
int *x,*y;
{
	/* return the current cursor location on the screen */

	*x = _line;
	*y = _col;
}

ClearScreen()
{
	/* clear the screen: returns -1 if not capable */

	_line = 0;	/* clear leaves us at top... */
	_col  = 0;

	redraw_screen = 0;
	_intransmit = -1;   /* Re-set state */

	if (!_clearscreen)
		return(-1);

	tputs(_clearscreen, 1, outchar);
	fflush(stdout);      /* clear the output buffer */
	return(0);
}

static void moveabsolute P_((int col, int row));

static CursorUp(n)
     int n;
{
  /** move the cursor up 'n' lines **/
  /** Calling function must check that _up is not null before calling **/

  if (need_moveabsolute)
    moveabsolute(_col, _line);

  _line = (_line-n > 0? _line - n: 0);	/* up 'n' lines... */

  while (n-- > 0)
    tputs(_up, 1, outchar);

  fflush(stdout);
  return(0);
}


static CursorDown(n)
     int n;
{
  /** move the cursor down 'n' lines **/
  /** Caller must check that _down is not null before calling **/

  if (need_moveabsolute)
    moveabsolute(_col, _line);;

  _line = (_line+n <= elm_LINES? _line + n: elm_LINES);    /* down 'n' lines... */

  while (n-- > 0)
    tputs(_down, 1, outchar);
  
  fflush(stdout);
  return(0);
}


static CursorLeft(n)
     int n;
{
	/** move the cursor 'n' characters to the left **/
  /** Caller must check that _left is not null before calling **/
  
  if (need_moveabsolute)
    moveabsolute(_col, _line);

  _col = (_col - n> 0? _col - n: 0);	/* left 'n' chars... */
  
  while (n-- > 0)
    tputs(_left, 1, outchar);
  
  fflush(stdout);
  return(0);
}


static CursorRight(n)
     int n;
{
  /** move the cursor 'n' characters to the right (nondestructive) **/
  /** Caller must check that _right is not null before calling **/
  
  if (need_moveabsolute)
    moveabsolute(_col, _line);

  _col = (_col+n < elm_COLUMNS? _col + n: elm_COLUMNS);	
  /* right 'n' chars... */

  while (n-- > 0)
    tputs(_right, 1, outchar);

  fflush(stdout);
  return(0);
}

static void moveabsolute(col, row)
     int col, row;
{

  char *stuff, *tgoto();

  if (need_moveabsolute) {
    dprint(4,(debugfile,
	      "Curses: moveabsolute: Syncronizing cursos position (col=%d,row=%d)\n",
	      col,row));
  }

  stuff = tgoto(_moveto, col, row);
  tputs(stuff, 1, outchar);
  fflush(stdout);
  
  need_moveabsolute = 0;
}

MoveCursor(row, col)
int row, col;
{
	/** move cursor to the specified row column on the screen.
            0,0 is the top left! **/

	int scrollafter = 0;

	/* we don't want to change "rows" or we'll mangle scrolling... */

	if (need_moveabsolute)
	  moveabsolute(_col, _line);;

	if (col < 0)
	  col = 0;
	if (col >= elm_COLUMNS)
	  col = elm_COLUMNS - 1;
	if (row < 0)
	  row = 0;
	if (row > elm_LINES) {
	  if (col == 0)
	    scrollafter = row - elm_LINES;
	  row = elm_LINES;
	}

	if (!_moveto)
	  return(-1);

	if (row == _line) {
	  if (col == _col)
	    return(0);				/* already there! */

	  else if (abs(col - _col) < 5) {	/* within 5 spaces... */
	    if (col > _col && _right)
	      CursorRight(col - _col);
	    else if (col < _col &&  _left)
	      CursorLeft(_col - col);
	    else
	      moveabsolute(col, row);
          }
	  else 		/* move along to the new x,y loc */
	    moveabsolute(col, row);
	}
	else if (_line == row-1 && col == 0) {
	  if (_col != 0)
	    putchar('\r');
	  putchar('\n');
	  fflush(stdout);
	}
	else if (col == _col && abs(row - _line) < 5) {
	  if (row < _line && _up)
	    CursorUp(_line - row);
	  else if (row > _line && _down)
	    CursorDown(row - _line);
	  else
	    moveabsolute(col, row);
	}
	else 
	  moveabsolute(col, row);

	_line = row;	/* to ensure we're really there... */
	_col  = col;

	if (scrollafter) {
	  putchar('\r');
	  while (scrollafter--)
	    putchar('\n');
	}

	return(0);
}

CarriageReturn()
{
	/** move the cursor to the beginning of the current line **/
	Writechar('\r');
}

NewLine()
{
	/** move the cursor to the beginning of the next line **/

	Writechar('\r');
	Writechar('\n');
}

StartBold()
{
	/** start boldface/standout mode **/

	if (!_setbold)
		return(-1);

	tputs(_setbold, 1, outchar);
	fflush(stdout);
	return(0);
}


EndBold()
{
	/** compliment of startbold **/

	if (!_clearbold)
		return(-1);

	tputs(_clearbold, 1, outchar);
	fflush(stdout);
	return(0);
}

#ifndef ELM

StartUnderline()
{
	/** start underline mode **/

	if (!_setunderline)
		return(-1);

	tputs(_setunderline, 1, outchar);
	fflush(stdout);
	return(0);
}


EndUnderline()
{
	/** the compliment of start underline mode **/

	if (!_clearunderline)
		return(-1);

	tputs(_clearunderline, 1, outchar);
	fflush(stdout);
	return(0);
}


StartHalfbright()
{
	/** start half intensity mode **/

	if (!_sethalfbright)
		return(-1);

	tputs(_sethalfbright, 1, outchar);
	fflush(stdout);
	return(0);
}

EndHalfbright()
{
	/** compliment of starthalfbright **/

	if (!_clearhalfbright)
		return(-1);

	tputs(_clearhalfbright, 1, outchar);
	fflush(stdout);
	return(0);
}

StartInverse()
{
	/** set inverse video mode **/

	if (!_setinverse)
		return(-1);

	tputs(_setinverse, 1, outchar);
	fflush(stdout);
	return(0);
}


EndInverse()
{
	/** compliment of startinverse **/

	if (!_clearinverse)
		return(-1);

	tputs(_clearinverse, 1, outchar);
	fflush(stdout);
	return(0);
}

int
HasMemlock()
{
	/** returns TRUE iff memory locking is available (a terminal
	    feature that allows a specified portion of the screen to
	    be "locked" & not cleared/scrolled... **/

	return ( _set_memlock && _clear_memlock );
}

static int _old_LINES;

int
StartMemlock()
{
	/** mark the current line as the "last" line of the portion to 
	    be memory locked (always relative to the top line of the
	    screen) Note that this will alter LINES so that it knows
	    the top is locked.  This means that (plus) the program 
	    will scroll nicely but (minus) End memlock MUST be called
	    whenever we leave the locked-memory part of the program! **/

	if (! _set_memlock)
	  return(-1);

	if (! _memory_locked) {

	  _old_LINES = elm_LINES;
	  elm_LINES -= _line;		/* we can't use this for scrolling */

	  tputs(_set_memlock, 1, outchar);
	  fflush(stdout);
	  _memory_locked = TRUE;
	}

	return(0);
}

int
EndMemlock()
{
	/** Clear the locked memory condition...  **/

	if (! _set_memlock)
	  return(-1);

	if (_memory_locked) {
	  elm_LINES = _old_LINES;		/* back to old setting */
  
	  tputs(_clear_memlock, 1, outchar);
	  fflush(stdout);
	  _memory_locked = FALSE;
	}
	return(0);
}

#endif /* ndef ELM */

Writechar(ch)
register int ch;
{
	/** write a character to the current screen location. **/

	static int wrappedlastchar = 0;
	int justwrapped, nt;

	if (need_moveabsolute)
	  moveabsolute(_col, _line);

	ch &= 0xFF;
	justwrapped = 0;

	/* if return, just go to left column. */
	if(ch == '\r') {
	  if (wrappedlastchar)
	    justwrapped = 1;                /* preserve wrap flag */
	  else {
	    putchar('\r');
	    _col = 0;
	  }
	}

	/* if newline and terminal just did a newline without our asking,
	 * do nothing, else output a newline and increment the line count */
	else if (ch == '\n') {
	  if (!wrappedlastchar) {
	    putchar('\n');
	    if (_line < elm_LINES)
	      ++_line;
	  }
	}

	/* if backspace, move back  one space  if not already in column 0 */
	else if (ch == BACKSPACE) {
	    if (_col != 0) {
		tputs(_left, 1, outchar);
		_col--;
	    }
	    else if (_line > 0) {
		_col = elm_COLUMNS - 1;
		_line--;
		moveabsolute (_col, _line);
	    }
	    /* else BACKSPACE does nothing */
	}

	/* if bell, ring the bell but don't advance the column */
	else if (ch == '\007') {
	  putchar(ch);
	}

	/* if a tab, output it */
	else if (ch == '\t') {
	  /* If terminal driver is expanding tabs, don't trust it ... */
	  if (!tabexpand)
	    putchar(ch);
	  if((nt=next_tab(_col+1)) > prev_tab(elm_COLUMNS))
	    _col = elm_COLUMNS-1;
	  else
	    _col = nt-1;
	  if (tabexpand)
	    moveabsolute (_col, _line);
	}

	else {
	  /* if some kind of non-printable character change to a '?' */
#ifdef ASCII_CTYPE
	  if(!isascii(ch) || !isprint(ch))
#else
	  if(!isprint((unsigned char)ch))
#endif
	    ch = '?';

	  /* if we only have one column left, simulate automargins if
	   * the terminal doesn't have them */
	  if (_col == elm_COLUMNS - 1) {
	    putchar(ch);
	    if (!_automargin || _eatnewlineglitch) {
	      putchar('\r');
	      putchar('\n');
	    }
	    if (_line < elm_LINES)
	      ++_line;
	    _col = 0;
	    justwrapped = 1;
	  }

	  /* if we are here this means we have no interference from the
	   * right margin - just output the character and increment the
	   * column position. */
	  else {
	    putchar(ch);
	    _col++;
	  }
	}

	wrappedlastchar = justwrapped;

	return(0);
}

/*VARARGS2*/

void Write_to_screen (
#if ANSI_C
		      char *line, int argcount, ...
#else
		      line, argcount, va_alist
#endif
		      )
#if !ANSI_C
     char *line;
     int   argcount; 
     va_dcl
#endif
{
	char *arg1, *arg2, *arg3;
	va_list vl;

	Va_start(vl, argcount);           /* defined in defs.h */

	/** This routine writes to the screen at the current location.
  	    when done, it increments lines & columns accordingly by
	    looking for "\n" sequences... **/

	switch (argcount) {
	case 0 :
		PutLine0(_line, _col, line);
		break;
	case 1 :
		arg1 = va_arg(vl, char *);
		PutLine1(_line, _col, line, arg1);
		break;
	case 2 :
		arg1 = va_arg(vl, char *);
		arg2 = va_arg(vl, char *);
		PutLine2(_line, _col, line, arg1, arg2);
		break;
	case 3 :
		arg1 = va_arg(vl, char *);
		arg2 = va_arg(vl, char *);
		arg3 = va_arg(vl, char *);
		PutLine3(_line, _col, line, arg1, arg2, arg3);
		break;
	}
}

PutLine0(x, y, line)
int x,y;
register char *line;
{
	/** Write a zero argument line at location x,y **/

	MoveCursor(x,y);
	while(*line)
	  Writechar(*line++);
	fflush(stdout);
}

/*VARARGS2*/
PutLine1(x,y, line, arg1)
int x,y;
char *line;
char *arg1;
{
	/** write line at location x,y - one argument... **/

	char buffer[VERY_LONG_STRING];

	sprintf(buffer, line, arg1);

	PutLine0(x, y, buffer);
        fflush(stdout);
}

/*VARARGS2*/
PutLine2(x,y, line, arg1, arg2)
int x,y;
char *line;
char *arg1, *arg2;
{
	/** write line at location x,y - one argument... **/

	char buffer[VERY_LONG_STRING];

	MCsprintf(buffer, line, arg1, arg2);

	PutLine0(x, y, buffer);
        fflush(stdout);
}

/*VARARGS2*/
PutLine3(x,y, line, arg1, arg2, arg3)
int x,y;
char *line;
char *arg1, *arg2, *arg3;
{
	/** write line at location x,y - one argument... **/

	char buffer[VERY_LONG_STRING];

	MCsprintf(buffer, line, arg1, arg2, arg3);

	PutLine0(x, y, buffer);
        fflush(stdout);
}

CleartoEOLN()
{
	/** clear to end of line **/

	if (need_moveabsolute)
	  moveabsolute(_col, _line);

	if (!_cleartoeoln)
		return(-1);

	tputs(_cleartoeoln, 1, outchar);
	fflush(stdout);  /* clear the output buffer */
	return(0);
}

CleartoEOS()
{
	/** clear to end of screen **/

	if (need_moveabsolute)
	  moveabsolute(_col, _line);

	if (!_cleartoeos)
		return(-1);

	tputs(_cleartoeos, 1, outchar);
	fflush(stdout);  /* clear the output buffer */
	return(0);
}


RawState()
{
	/** returns either 1 or 0, for ON or OFF **/

	return( _inraw );
}

#ifdef ultrix
force_raw()
{
	(void) ttsetattr(TTYIN,&_original_tty);
	_inraw = 0;
	Raw(ON);
}
#endif

Raw(state)
int state;
{
	int do_tite = (state & NO_TITE) == 0;

	state = state & ~NO_TITE;

	dprint(4,(debugfile,
		  "curses: Raw: state=%d do_tite=%d\n",state,do_tite));

	/** state is either ON or OFF, as indicated by call **/

	if (state == OFF && _inraw) {
	  dprint(4,(debugfile,
		    "curses: Raw: Setting Raw state OFF\n"));;

	  if (cursor_control)
	    transmit_functions(OFF);

	  if (use_tite && _end_termcap && do_tite) {
	    tputs(_end_termcap, 1, outchar);
	    fflush(stdout);
	  }
	  (void) ttsetattr(TTYIN,&_original_tty);
	  _inraw = 0;
	}
	else if (state == ON && ! _inraw) {

	  dprint(4,(debugfile,
		    "curses: Raw: Setting Raw state ON\n"));;

	  (void) ttgetattr(TTYIN, &_original_tty);
	  (void) ttgetattr(TTYIN, &_raw_tty);    /** again! **/

#if !defined(TERMIO) && !defined(TERMIOS)
	  _raw_tty.sg_flags &= ~(ECHO);	/* echo off */
	  _raw_tty.sg_flags |= CBREAK;	/* raw on    */

#else
	  _raw_tty.c_lflag &= ~(ICANON | ECHO);	/* noecho raw mode        */

	  _raw_tty.c_cc[VMIN] = '\01';	/* minimum # of chars to queue    */
	  _raw_tty.c_cc[VTIME] = '\0';	/* minimum time to wait for input */


#if defined(OXTABS)
	  /* Some systems use OXTABS bit */
	  if (_raw_tty.c_oflag & OXTABS) {
	    dprint(4,(debugfile,
		      "curses: [OXTABS] Terminal driver is expanding tabs...\n"));
	    tabexpand = 1;
	  } else {
	    if (tabexpand) 	      
	      dprint(4,(debugfile,
			"curses: [OXTABS] Terminal driver isn't expanding tabs...\n"));
	    tabexpand = 0;
	  }
#endif /* defined(OXTABS) */

#if defined(TABDLY) && !defined(OXTABS)
	  /* Some systems expands tab when TABDLY is XTABS */
	  if ((_raw_tty.c_oflag & TABDLY) == 
#ifdef XTABS
	      XTABS
#else
	      TAB3
#endif
	      ) {
	    dprint(4,(debugfile,
		      "curses: [TABDLY] Terminal driver is expanding tabs...\n"));
	    tabexpand = 1;
	  } else {
	    if (tabexpand) 
	      dprint(4,(debugfile,
			"curses: [TABDLY] Terminal driver isn't expanding tabs...\n"));
	    tabexpand = 0;
	  }
#endif /* defined(TABDLY) && !defined(OXTABS) */

#if !defined(TABDLY) && !defined(OXTABS)

	  /* If _POSIX_SOURCE is defined then OXTABS or TABDLY are not 
	   * defined -- so print warning anyway
	   */
	  dprint(4,(debugfile,
		    "curses: No information is terminal driver expanding tabs!\n"));	  
#endif /* !defined(TABDLY) && !defined(OXTABS) */

#endif
	  (void) ttsetattr(TTYIN, &_raw_tty);
	  if (use_tite && _start_termcap && do_tite) 
	    tputs(_start_termcap, 1, outchar);
	  _intransmit = -1; /* state unclear */
	  _inraw = 1;
	  need_moveabsolute = 1;
	}
}

#if !defined(TERMIO) && !defined(TERMIOS)
static int ttgetattr(fd, where)
     int fd;
     struct tty_modes *where;
{
  if (ioctl(fd, TIOCGETP, &where->sgttyb) < 0)
    return(-1);
  if (ioctl(fd, TIOCGETC, &where->tchars) < 0)
    return(-1);
  return(0);
}

static int ttsetattr(fd, where)
     int fd;
     struct tty_modes *where;
{
  if (ioctl(fd, TIOCSETP, &where->sgttyb) < 0)
    return(-1);
  if (ioctl(fd, TIOCSETC, &where->tchars) < 0)
    return(-1);
  return(0);
}
#endif

int ReadCh(flags)
     int flags;
{
  int redraw      = (flags & READCH_MASK);
  int cursorkeys  = (flags & READCH_CURSOR) != 0;
  int nocursor    = (flags & READCH_NOCURSOR) != 0 && cursor_control;

#ifdef DEBUG
  unsigned char debug_buffer[20];
#endif

  /*
   *	read a character with Raw mode set!
   *
   *	EAGAIN & EWOULDBLOCK are recognized just in case
   *	O_NONBLOCK happens to be in effect.
   */

  /* This is static array so we can initialize it in here ...
   */
  static struct {
    char ** const str;
    const int result;
    int maybe;
  } keytable[] =
    { { &_key_up,       UP_MARK,       0 },
      { &_key_down,     DOWN_MARK,     0 },
      { &_key_left,     LEFT_MARK,     0 },
      { &_key_right,    RIGHT_MARK,    0 },

      { &_key_pageup,   PAGEUP_MARK,   0 },
      { &_key_pagedown, PAGEDOWN_MARK, 0 },

      { &_key_home,     HOME_MARK,     0 },
      { &_key_help,     HELP_MARK,     0 },
      { &_key_find,     FIND_MARK,     0 },
      { NULL,           0,             0 }
    };
    
  int read_p,found_key;

reinit_ReadChar:
  read_p = 0;
  found_key = 0;

  fflush(stdout);

  if (redraw && !RawState()) { /* Check that we have in 'raw' mode */
    dprint(4,(debugfile,
	      "ReadCh: Going to Raw mode\n"));
    Raw(ON);
    ClearScreen();
    return redraw;
  }

  if (redraw && redraw_screen) {
    dprint(4,(debugfile,
	      "ReadCh: Pending redraw...\n"));
    ClearScreen();
    return redraw;
  }
  
  if ((_intransmit != ON || redraw_screen) &&
       cursorkeys && _transmit_on) {
    dprint(4,(debugfile,
	      "ReadCh: Enabling cursor keys...\n"));
    transmit_functions(ON);
  }

  if ((_intransmit != OFF || redraw_screen) &&
      nocursor && _transmit_off) {
    dprint(4,(debugfile,
	      "ReadCh: Disabling cursor keys...\n"));
    transmit_functions(OFF);
  }

  if (cursorkeys) {
    int i;
    dprint(8,(debugfile,
	      "ReadCh: Available function keys:"));

    for (i = 0; keytable[i].str != 0; i++) {
      char * const str = *(keytable[i].str);
      if(str && str[0] != '\0') {
	keytable[i].maybe = 1;  /* Initially ewery function key is possible */
	dprint(8,(debugfile,
		  " [%d] %d",i,keytable[i].result));

      }
    }      
    dprint(8,(debugfile,
	      "\n"));
  }

  while (found_key == 0) {
    int    result;
    unsigned char   ch;
    
    result = read(0, &ch, 1);

    if (result < 0) {
      int err = errno;
      dprint(4,(debugfile,
		"ReadCh: errno = %d [%s]\n",err,
		error_description(err)));

      /* Return error: */
      if (redraw && redraw_screen
	  || InGetPrompt   /* GetPrompt wants to see errors! */
	  ) {
	found_key = -1;
	continue;
      }
      if((errno == EINTR)
#ifdef	EAGAIN
	 || (errno == EAGAIN)
#endif
#ifdef	EWOULDBLOCK
	 || (errno == EWOULDBLOCK)
#endif
	 ) {
	continue;  /* Retry */
      }
    }
    if (0 == result) {
      dprint(4,(debugfile,
		"ReadCh: Got zero bytes...\n"));
      found_key = -1;
      continue;
    }

    if (cursorkeys) {
      int match = 0;
      int i;
      for (i = 0; keytable[i].str != NULL; i++) {
	if (keytable[i].maybe) {
	  unsigned char * const str = 
	    (unsigned char * const) *(keytable[i].str);
	  if (str[read_p] == ch) {
	    match++;
	    if (str[read_p+1] == '\0') {
	      found_key = keytable[i].result;
	      dprint(4,(debugfile,
			"ReadCh: Found function key = %d (keytable = %d, read_p =%d)\n",
			found_key,i,read_p));
	    }
	  } else {
	    keytable[i].maybe = 0;
	  }
	}
      }
#ifdef DEBUG
      if (read_p < sizeof debug_buffer -1) {
	debug_buffer[read_p] = ch;
	debug_buffer[read_p+1] = '\0';
      }
#endif 
      if (match == 0) {    /* Not in keytable */
	if (read_p == 0) 
	  found_key = ch;  /* Normal key */
	else {
	  int i;
	  dprint(4,(debugfile,
		    "ReadCh: Bad escape sequence; ch = %d, read_p = %d\n",
		    ch,read_p));
#ifdef DEBUG
	  dprint(4,(debugfile,"ReadCh: Sequence was:"));
	  for (i = 0; i <= read_p && i < sizeof debug_buffer -1; i++) {
	    if (isascii(debug_buffer[i]) && isprint(debug_buffer[i])) {
	      dprint(4,(debugfile," %c (0x%02X)", 
			debug_buffer[i],debug_buffer[i]));
	    } else {
	      dprint(4,(debugfile," 0x%02X", debug_buffer[i]));
	    }
	  }
	  if (read_p > sizeof debug_buffer -1)
	    dprint(4,(debugfile," ..."));
	  dprint(4,(debugfile,"\n"));
#endif

	  /* Ring a bell */
	  Writechar('\007');
	  goto reinit_ReadChar;
	}
      } else
	read_p++;
    } else
      found_key = ch;
    
  }

  if (found_key <= 0 && redraw && redraw_screen) {
    dprint(4,(debugfile,
	      "ReadCh: Redraw...\n"));
    if(!RawState()) {  /* Check that we have in 'raw' mode */
      dprint(4,(debugfile,
		"ReadCh: Going to Raw mode\n"));
      Raw(ON);      
    }
    ClearScreen();
    return redraw;
  }
  
  return found_key;
}

outchar(c)
char c;
{
	/** output the given character.  From tputs... **/
	/** Note: this CANNOT be a macro!              **/

	putc(c, stdout);
}

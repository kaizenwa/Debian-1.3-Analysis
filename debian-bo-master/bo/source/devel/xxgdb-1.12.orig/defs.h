/*****************************************************************************
 *
 *  xdbx - X Window System interface to the dbx debugger
 *
 *  Copyright 1989 The University of Texas at Austin
 *  Copyright 1990 Microelectronics and Computer Technology Corporation
 *
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of The University of Texas
 *  and Microelectronics and Computer Technology Corporation (MCC) not be 
 *  used in advertising or publicity pertaining to distribution of
 *  the software without specific, written prior permission.  The
 *  University of Texas and MCC makes no representations about the 
 *  suitability of this software for any purpose.  It is provided "as is" 
 *  without express or implied warranty.
 *
 *  THE UNIVERSITY OF TEXAS AND MCC DISCLAIMS ALL WARRANTIES WITH REGARD TO
 *  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TEXAS OR MCC BE LIABLE FOR
 *  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 *  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 *  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Author:  	Po Cheung
 *  Created:   	March 10, 1989
 * 
 *****************************************************************************
 * 
 *  xxgdb - X Window System interface to the gdb debugger
 *  
 * 	Copyright 1990,1993 Thomson Consumer Electronics, Inc.
 *  
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of Thomson Consumer
 *  Electronics (TCE) not be used in advertising or publicity pertaining
 *  to distribution of the software without specific, written prior
 *  permission.  TCE makes no representations about the suitability of
 *  this software for any purpose.  It is provided "as is" without express
 *  or implied warranty.
 *
 *  TCE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 *  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
 *  SHALL TCE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES
 *  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 *  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 *  SOFTWARE.
 *
 *  Adaptation to GDB:  Pierre Willard
 *  XXGDB Created:   	December, 1990
 *
 *  AJK, May 91
 *   1. Define help message for new dbx commands (whereis/whatis/which)
 *****************************************************************************/

/*  defs.h
 *
 *    Contain #includes, #defines and typedefs
 */

#include <stdio.h>
#include <sys/param.h>
#include <X11/Xos.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Grip.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/TextP.h>
#include <X11/Xaw/TextSrc.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Label.h>
#include <X11/cursorfont.h>

#if XtVersion < 11005
/* incompatibility of header file with X11R4 documentation */
#define XawChainTop	XtChainTop
#define XawChainBottom	XtChainBottom
#define XawChainLeft	XtChainLeft
#define XawChainRight	XtChainRight
#endif

#ifndef AssignMax
#define AssignMax(x, y) 	if ((y) > (x)) x = (y)
#endif
#ifndef AssignMin
#define AssignMin(x, y) 	if ((y) < (x)) x = (y)
#endif

/* increase input LINESIZ to 4k from .5k (could still be too small) */
#define LINESIZ         4096		/* input line length */

#define MAXNAME         256		/* max identifier length */
#define MAXARGS 	20		/* max number of args */
#define ADD_SIZE 	16		/* # of files added during Realloc */
#define CHARS_PER_LINE  20		/* estimated number of chars per line */
#define ADD_LINES       50		/* # of lines to be added in realloc */
#define NTOKENS		6		/* number of tokens */
#ifdef GDB
#define DEBUGGER      	"gdb"		/* name of executable */
#define XDBXPROMPT  	"(xxgdb) "	/* xdbx prompt string */
#else
#define DEBUGGER      	"dbx"		/* name of executable */
#define XDBXPROMPT  	"(xdbx) "	/* xdbx prompt string */
#endif	/* GDB */
#define DELIMITERS	" !%^&*()+=~|;:{},/#<?\"\n\t"

#ifdef GDB
#define BREAK_HELP	"Please select a function name or an address, or put cursor on a line to break at"
#else
#define STOP_AT_HELP	"Please select a line to stop at"
#define STOP_IN_HELP	"Please select a function to stop in"
/* AJK */
#define WHATIS_HELP	"Please select a variable"
#endif /* GDB */
#define DELETE_HELP	"Please select a stop sign to delete"
#define PRINT_HELP	"Please select an expression to print"
#ifdef GDB
#define UNDISPLAY_HELP	"Please select a number to undisplay"
#else
#define UNDISPLAY_HELP	"Please select an expression to undisplay"
#endif /* GDB */
#define SEARCH_HELP	"Nothing to search"

#define	LASTCH(s)	(s[strlen(s)-1])
#define	SECLASTCH(s)	(s[strlen(s)-2])

#ifdef SUNOS4
typedef struct dirent 	Directory;
#else
typedef struct direct 	Directory;
#endif

typedef struct {
    Boolean   bell;		/* if True, bell on */
    Boolean   displayWindow;	/* if True, display window on startup */
    String    delimiters;	/* set of delimiters for word selection */
    String    prompt;		/* prompt string for xdbx */

/* CRL mod 4 3/15/91 GWC - added two new app res */
    String    db_name;  	/* name for dbx child */
    String    db_prompt;	/* prompt for dbx child */

	Boolean   nx;			/* if True, do not execute gdbinit */

    Pixel     stop_color;	/* color of stop sign */
    Pixel     arrow_color;	/* color of arrow sign */
    Pixel     updown_color;	/* color of updown sign */
    Pixel     bomb_color;	/* color of bomb sign */

    Dimension dataDpyMaxHeight;	/* data display window maximum height */
    Dimension dataDpyMaxWidth;	/* data display window maximum width */

    Boolean   bigicon;		/* xdbx option -bigicon */
    Boolean   debug;		/* xdbx option -debug */

    Boolean   dbxopt_r;		/* dbx option -r */
    Boolean   dbxopt_i;		/* dbx option -i */
    String    includeDir;	/* dbx option -I includeDir */
    Boolean   dbxopt_k;		/* dbx option -k */
    String    cfile;		/* Berkeley  dbx option -c file */
    Boolean   dbxopt_kbd;	/* Sun dbx option -kbd */
    String    fcount;		/* SunOS 4.0 dbx option -f fcount */
#ifdef GDB
    String    gdbinit;		/* to overwrite ".gdbinit" default name */
#endif
    String    startup;		/* SunOS 4.0 dbx option -s startup */
    String    tstartup;		/* SunOS 4.0 dbx option -sr tstartup */
    String    bindings;     /* which style text editing to use */
    Boolean   pixie;		/* Mips dbx option -pixie */
	int       tabstop;		/* tab widths */
} XdbxResources;

typedef struct {
    char 		*filename;	/* name of file */
    char 		*pathname;	/* full path name of file */
    char 		*buf;		/* buffer holding source file */
    long		filesize;	/* size of file in bytes */
    time_t		mtime;		/* time last modified */
    int	 		lines;		/* # of lines on display */
    int	 		currentline;	/* line where caret is */
    int	 		topline;	/* top line number in the window */
    int	 		bottomline;	/* bottom line number in window */
    int	 		lastline;	/* number of lines in source file */
    XawTextPosition 	topPosition;	/* top display position of buffer */
    XawTextPosition 	*linepos;	/* array of text pos for each newline */
} FileRec, *FileRecPtr;

typedef struct {
    char	*mesg;			/* part of matched string */
    unsigned	stop;			/* stop number */
    char 	*func;			/* function name */
    int		line;			/* line number */
    char	*file;			/* file name */
    char	*display;		/* variable display output */
} Tokens;

typedef struct dataDpyList {
    struct dataDpyRec	*dataDpy;
    struct dataDpyList	*next;
} DataDpyList;

typedef struct dataDpyRec {
    int			id;
    Widget              popupshell;             /* parent of popup */
    Widget              popup;                  /* form widget */
    Widget              label;                  /* label widget */
    Widget              dataDpyWindow;          /* window for displaying data */
    char                *buf;                   /* text buffer */
    int                 buflen;
    XawTextPosition     *linepos;
    int                 numlines;
    int                 maxLineLength;
    int			state;			/* EMPTY, UNUSED, USED */
    struct dataDpyRec	*parent;		/* pointer to parent */
    struct dataDpyList	*childlist;		/* list of children */
} DataDpyRec, *DataDpyRecPtr;

typedef struct {
    char			*pat;		/* regular expression */
    struct re_pattern_buffer	*buf;		/* buffer for compile regex */
    int				reg_token[NTOKENS];	/* register number */
} PatternRec, *PatternRecPtr;

typedef struct commandRec {
    char		*command;
    struct commandRec	*next;
} CommandRec, *CommandRecPtr;

typedef struct {
    Cardinal	i;			/* index to arrowsign[] */
    char	file[MAXNAME];		/* file associated with */
    int	 	line;			/* line number */
    char 	func[MAXNAME];		/* function name associated with */
} Arrow;

typedef struct {
    Cardinal	i;			/* index to updownsign[] */
    char	file[MAXNAME];		/* file associated with */
    int	 	line;			/* line number */
    char 	func[MAXNAME];		/* function name associated with */
} Updown;

typedef struct {
    char	*file;			/* file associated with */
    int	 	line;			/* line number of stop */
    unsigned    tag;                    /* used in deleting stops */
} Stops;

typedef struct {
    Cardinal	i;			/* index to bombsign[] */
    char	file[MAXNAME];		/* file associated with */
    int	 	line;			/* line number */
    char 	func[MAXNAME];		/* function name associated with */
} Bomb;


#ifdef GDB
/* defined flags for query_gdb_gen() and read_gdb() */
#define PARSE_ON	1
#define PARSE_OFF	0
#define ECHO_ON		2
#define ECHO_OFF	0
#define FILTER_ON	4
#define FILTER_OFF	0
#endif

#define EDIT_BUTTON /* to get editor button and functionality */

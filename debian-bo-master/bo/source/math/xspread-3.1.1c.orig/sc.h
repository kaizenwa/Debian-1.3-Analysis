/*	SC	A Table Calculator
 *		Common definitions
 *
 *		original by James Gosling, September 1982
 *		modified by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *		R. Bond  12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		$Revision: 1.1 $
 *
 */

#if defined(MSDOS)
#include <stdio.h>
#endif

/*#if defined(BSD42) || defined(BSD43) && !defined(SYSV)
#include <strings.h>
#else
#ifndef SYSIII
#include <string.h>
#endif
#endif
*/

#ifndef PI
#define PI (double)3.14159265358979323846
#endif

#define	ATBL(tbl, row, col)	(*(tbl + row) + (col))

#define MINROWS 100 	/* minimum size at startup */
#define MINCOLS 30 
#define	ABSMAXCOLS 702	/* absolute cols: ZZ (base 26) */

#ifdef PSC
#define error(s) fprintf(stderr,s)
#endif

#ifdef HAVE_X11_X_H
/*the following give the minimum size of the main window, in text coordinates*/ 
#define MIN_COLS 80
#define MIN_ROWS 25
#endif

#define REG_LEN 28      /* following eight moved here by Bob Parbs 12-92 */
#define ROWLIM  5       /* increased to five from four after move */
#define COLIM   3 
#define TSSIZE  15
#define MAXSS   2000
#define MINMAX  25
#define MAXROW  25 
#define MAXCOL  25      /* moved from matrix.c */

#define CRCOLS 1
#define CRROWS 2
#define RESCOL 4	/* columns reserved for row numbers */
#define RESROW 4  /* rows reserved for prompt, error, and column numbers */

/* formats for engformat() */
#define REFMTFIX	0	/* fixed point: -0.00010 */
#define REFMTFLT	1	/* scientific notation: -1.00E\-04 */
#define REFMTENG	2	/* engineering notation: -100.00u */
#define REFMTDATE	3	/* dates: 05/15/92 */
#define REFMTEXP	4	/* modulo 3 exponent notation: -100.E-06 */

#define DEFWIDTH 10	/* Default column width and precision */
#define DEFPREC   2
#define DEFREFMT  REFMTFIX /* Make default format fixed point  THA 10/14/90 */

#define HISTLEN  10	/* Number of history entries for vi emulation */

#define	FBUFLEN	1024	/* buffer size for a single field */

	/* maximum path length */
#ifdef PATH_MAX
# define	PATHLEN	PATH_MAX
#else
# ifdef PATHSIZE
#  define	PATHLEN	PATHSIZE
# else
#  define	PATHLEN	1024
# endif
#endif

#ifndef A_CHARTEXT	/* Should be defined in curses.h */
# ifdef INTERNATIONAL
#  define A_CHARTEXT 0xff
# else
#  define A_CHARTEXT 0x7f
# endif
#endif

#ifndef FALSE
# define	FALSE	0
# define	TRUE	1
#endif /* !FALSE */


/*
 * ent_ptr holds the row/col # and address type of a cell
 *
 * vf is the type of cell address, 0 non-fixed, or bitwise OR of FIX_ROW or
 *	FIX_COL
 * vp : we just use vp->row or vp->col, vp may be a new cell just for holding
 *	row/col (say in gram.y) or a pointer to an existing cell
 */
struct ent_ptr {
    int vf;
    struct ent *vp;
};

/* holds the beginning/ending cells of a range */
struct range_s {
	struct ent_ptr left, right;
};

/*
 * Some not too obvious things about the flags:
 *    is_valid means there is a valid number in v.
 *    is_locked means that the cell cannot be edited.
 *    label set means it points to a valid constant string.
 *    is_strexpr set means expr yields a string expression.
 *    If is_strexpr is not set, and expr points to an expression tree, the
 *        expression yields a numeric expression.
 *    So, either v or label can be set to a constant. 
 *        Either (but not both at the same time) can be set from an expression.
 */

#define VALID_CELL(p, r, c) ((p = *ATBL(tbl, r, c)) && \
			     ((p->flags & is_valid) || p->label))

/* info for each cell, only alloc'd when something is stored in a cell */
struct ent {
    double v;		/* v && label are set in EvalAll() */
    char *label;
    struct enode *expr;	/* cell's contents */
    short flags;	
    short row, col;
    struct ent *next;	/* next deleted ent (pulled, deleted cells) */
    char *format;	/* printf format for this cell */
    char cellerror;	/* error in a cell? */
};

/* stores a range (left, right) */
struct range {
    struct ent_ptr r_left, r_right;
    char *r_name;			/* possible name for this range */
    struct range *r_next, *r_prev;	/* chained ranges */
    int r_is_range;
};

#define FIX_ROW 1
#define FIX_COL 2

/* stores type of operation this cell will preform */
struct enode {
    int op;
    union {
	int gram_match;         /* some compilers (hp9000ipc) need this */
	double k;		/* constant # */
	struct ent_ptr v;	/* ref. another cell */
	struct range_s r;	/* op is on a range */
	char *s;		/* string part of a cell */
	struct {		/* other cells use to eval()/seval() */
	    struct enode *left, *right;
	} o;
    } e;
};

/* following structure moved from matrix.c by Bob Parbs 12-92 */
struct m_range_sd{
    int ssr,ser,ssc,sec;
    int dsr,der,dsc,dec;
    };

/* this struct also moved from matrix.c by Bob Parbs 12-92 */
struct m_range{
    int sr,er,sc,ec;
    };

/* op values */
#define O_VAR		'v'
#define O_CONST		'k'
#define O_ECONST	'E'	/* constant cell w/ an error */
#define O_SCONST	'$'
#define REDUCE		0200	/* Or'ed into OP if operand is a range */

#define OP_BASE	256
#define ACOS	(OP_BASE + 0)
#define ASIN	(OP_BASE + 1)
#define ATAN	(OP_BASE + 2)
#define CEIL	(OP_BASE + 3)
#define COS	(OP_BASE + 4)
#define EXP	(OP_BASE + 5)
#define FABS	(OP_BASE + 6)
#define FLOOR	(OP_BASE + 7)
#define HYPOT	(OP_BASE + 8)
#define LOG	(OP_BASE + 9)
#define LOG10	(OP_BASE + 10)
#define POW	(OP_BASE + 11)
#define SIN	(OP_BASE + 12)
#define SQRT	(OP_BASE + 13)
#define TAN	(OP_BASE + 14)
#define DTR	(OP_BASE + 15)
#define RTD	(OP_BASE + 16)
#define MINR	(OP_BASE + 17)	/* MIN/MAX might already be macros ... */
#define MAXR	(OP_BASE + 18)
#define RND	(OP_BASE + 19)
#define HOUR	(OP_BASE + 20)
#define MINUTE	(OP_BASE + 21)
#define SECOND	(OP_BASE + 22)
#define MONTH	(OP_BASE + 23)
#define DAY	(OP_BASE + 24)
#define YEAR	(OP_BASE + 25)
#define NOW	(OP_BASE + 26)
#define DATE	(OP_BASE + 27)
#define FMT	(OP_BASE + 28)
#define SUBSTR	(OP_BASE + 29)
#define STON	(OP_BASE + 30)
#define EQS	(OP_BASE + 31)
#define EXT	(OP_BASE + 32)
#define ELIST	(OP_BASE + 33)	/* List of expressions */
#define LMAX	(OP_BASE + 34)
#define LMIN	(OP_BASE + 35)
#define NVAL	(OP_BASE + 36)
#define SVAL	(OP_BASE + 37)
#define PV	(OP_BASE + 38)
#define FV	(OP_BASE + 39)
#define PMT	(OP_BASE + 40)
#define STINDEX	(OP_BASE + 41)
#define LOOKUP	(OP_BASE + 42)
#define ATAN2	(OP_BASE + 43)
#define INDEX	(OP_BASE + 44)
#define DTS	(OP_BASE + 45)
#define TTS	(OP_BASE + 46)
#define ABS	(OP_BASE + 47)
#define HLOOKUP	(OP_BASE + 48)
#define VLOOKUP	(OP_BASE + 49)
#define ROUND	(OP_BASE + 50)
#define IF	(OP_BASE + 51)
#define MYROW	(OP_BASE + 52)
#define MYCOL	(OP_BASE + 53)
#define COLTOA	(OP_BASE + 54)
#define UPPER	(OP_BASE + 55)
#define LOWER	(OP_BASE + 56)
#define CAPITAL	(OP_BASE + 57)
#define NUMITER	(OP_BASE + 58)
#define MATRIX_ADD   (OP_BASE + 59)
#define MATRIX_SUB   (OP_BASE + 60)
#define MATRIX_INV   (OP_BASE + 61)
#define MATRIX_MULT  (OP_BASE + 62)
#define MATRIX_TRANS (OP_BASE + 63)
#define IRR	(OP_BASE+64)

/* flag values */
#define is_valid     0001
#define is_changed   0002
#define is_strexpr   0004
#define is_leftflush 0010
#define is_deleted   0020
#define is_locked    0040
#define is_label     0100

/* cell error (1st generation (ERROR) or 2nd+ (INVALID)) */
#define	CELLOK		0
#define	CELLERROR	1
#define	CELLINVALID	2

#define ctl(c)	((c)&037)
#define ESC	033
#define DEL	0177

/* calculation order */
#define BYCOLS	1
#define BYROWS	2

/* tblprint style output for: */
#define	TBL	1		/* 'tbl' */
#define	LATEX	2		/* 'LaTeX' */
#define	TEX	3		/* 'TeX' */
#define	SLATEX	4		/* 'SLaTeX' (Scandinavian LaTeX) */
#define	FRAME	5		/* tblprint style output for FrameMaker */

/* Types for etype() */
#define NUM	1
#define STR	2

#define	GROWAMT	30	/* default minimum amount to grow */

#define	GROWNEW		1	/* first time table */
#define	GROWROW		2	/* add rows */
#define	GROWCOL		3	/* add columns */
#define	GROWBOTH	4	/* grow both */
extern	struct ent ***tbl;	/* data table ref. in vmtbl.c and ATBL() */

/* a linked list of free [struct ent]'s, uses .next as the pointer */
extern	struct ent *freeents;

/* a linked list of free [struct enodes]'s, uses .e.o.left as the pointer */
extern	struct enode *freeenodes;

extern char	curfile[];
extern int	strow, stcol;
extern int	currow, curcol;
extern int	savedrow, savedcol;
extern int	FullUpdate;
extern int	maxrow, maxcol;
extern int	maxrows, maxcols;	/* # cells currently allocated */
extern int	*fwidth;
extern int	*precision;
extern int	*realfmt;
extern char	*col_hidden;
extern char	*row_hidden;
extern char	line[FBUFLEN];
extern int	linelim;
extern int	changed;
extern struct ent *to_fix;
extern int	seenerr;	/*
				 * yyerror() not to redisplay error
				 * 1 if error just been displayed, 0 otherwise
				 */
extern int	showsc, showsr;
extern char	stringbuf[FBUFLEN];
extern int	maintextrows,	/* text rows in mainwin */
		maintextcols;	/* text cols in mainwin */
extern int	running;	/* are we done? -be careful on use... */
extern int	using_X;	/* are we doing X? (vs curses) */

extern void	Color_Menu PROTO((void));
extern void	EvalAll PROTO((void));
extern void	Main_Menu PROTO((void));
extern void	Graph_Menu PROTO((void));
extern void	Search_Menu PROTO((void));
extern void	Sort_Menu PROTO((void));

extern void	add_range PROTO((char *, struct ent_ptr, struct ent_ptr, int));
extern int	any_locked_cells PROTO((int, int, int, int));
extern int	are_ranges PROTO((void));
extern int	atocol PROTO((char *, int));
extern void	backcol PROTO((int));
extern void	backrow PROTO((int));
#ifdef DOBACKUPS
extern int	backup_file PROTO((char *));
#endif
extern void	checkbounds PROTO((int *, int *));
extern void	clean_range PROTO((void));
extern void	clearent PROTO((struct ent *));
extern void	clearlines PROTO((int, int));
extern void	closecol PROTO((int, int));
extern void	closeout PROTO((FILE *, int));
extern void	closerow PROTO((int));
extern void	colshow_op PROTO((void));
extern char *	coltoa PROTO((int));
extern void	copy PROTO((struct ent *, struct ent *, struct ent *, struct ent *));
extern struct enode *	copye PROTO((struct enode *, int, int));
extern void	copyent PROTO((struct ent *, struct ent *, int, int));
extern void	creadfile PROTO((char *, int));
extern void	cr_line PROTO((void));
extern int	cwritefile PROTO((char *, int, int, int, int));
extern void	del_range PROTO((struct ent *, struct ent *));
extern void	deleterow PROTO((int));
extern void	deraw PROTO((void));
extern void	diesave PROTO((void));
extern double	dodts PROTO((int, int, int));
extern void	doend PROTO((int, int));
extern void	doformat PROTO((int, int, int, int, int));
extern double	doirr PROTO((int, int, int, int));
extern RETSIGTYPE doquit PROTO((int));
extern void	dupcol PROTO((void));
extern void	duprow PROTO((void));
extern void	edit_mode PROTO((void));
extern void	editexp PROTO((int, int));
extern void	editfmt PROTO((int, int));
extern void	edits PROTO((int, int));
extern void	editv PROTO((int, int));
extern void	efree PROTO((struct enode *));
extern int	engformat PROTO((int, int, int, double, char *, int));
extern void	erase_area PROTO((int, int, int, int));
extern void	erasedb PROTO((void));
extern void	eraser PROTO((struct ent *, struct ent *));
extern int	etype PROTO((struct enode *));
extern void	fill PROTO((struct ent *, struct ent *, double, double));
extern struct range *	find_range PROTO((char *, int, struct ent *, struct ent *));
extern char *	findhome PROTO((char *));
extern void	flush_saved PROTO((void));
extern int	format PROTO((char *, double, char *, int));
extern void	format_cell PROTO((struct ent *, struct ent *, char *));
extern void	forwcol PROTO((int));
extern void	forwrow PROTO((int));
extern void	free_ent PROTO((struct ent *));
extern char *	fsuffix PROTO((char *, char *, char *));

/* from matrix.c */
extern void	addmatrix PROTO((int, int, int, int, int, int,int,int,int,int));
extern int	convert PROTO((int, char [], int ));
struct m_range_sd *find_rge PROTO((char *));
struct m_range *findrge PROTO((char *));
extern void	get_add PROTO((void));
extern void	get_invert PROTO((void));
extern void	get_mult PROTO((void));
extern void	get_sub PROTO((void));
extern void	get_trans PROTO((void));
extern void	invertmatrix PROTO((int, int, int, int, int, int));
extern void	multmatrix1 PROTO((int, int, int, int, int, int, int, int, int, int));
extern void	multmatrix PROTO((int, int, int, int, int, int, int, int, int, int));
extern void	submatrix PROTO((int, int, int, int, int, int, int, int, int, int));
extern void	transpose PROTO((int, int, int, int, int, int));

extern void	get_default_dir PROTO((char *tmp));
extern void	get_default_path PROTO((char *tmp));
/*extern int	get_rcqual PROTO((int);*/
extern char *	get_str PROTO((char *, int));
extern void	go_last PROTO((void));
extern void	goraw PROTO((void));
extern void	graphic_read_defn PROTO((FILE *));
extern void	graphic_write_defn PROTO((FILE *));
extern int	growtbl PROTO((int, int, int));
extern void	help PROTO((void));
extern void	hide_col PROTO((int));
extern void	hide_row PROTO((int));
extern void	hidecol PROTO((int));
extern void	hiderow PROTO((int));
extern void	initkbd PROTO((void));
extern void	ins_string PROTO((char *));
extern void	insert_mode PROTO((void));
extern void	insertrow PROTO((int));
extern void	kbd_again PROTO((void));
extern void	label PROTO((struct ent *, char *, int));
extern void	let PROTO((struct ent *, struct enode *));
extern void	list_range PROTO((FILE *));
extern void	lock_cells PROTO((struct ent *, struct ent *));
extern int	locked_cell PROTO((int, int));
extern struct ent *	lookat PROTO((int, int));
extern unsigned int menu PROTO((unsigned int, char **, char **));
extern void	message PROTO((char *));
extern int	modcheck PROTO((char *));
extern void	moveto PROTO((int, int));
extern char *	mystrtof PROTO((char *, double *));
extern struct enode *	new PROTO((int, struct enode *, struct enode *));
extern struct enode *	new_const PROTO((int, double));
extern struct enode *	new_range PROTO((int, struct range_s));
extern struct enode *	new_str PROTO((char *));
extern struct enode *	new_var PROTO((int, struct ent_ptr));
extern int	nmgetch PROTO((void));
extern void	num_search PROTO((double, int));
extern void	opencol PROTO((int, int));
extern FILE *	openout PROTO((char *, int *));
extern void	printfile PROTO((char *, int, int, int, int));
extern char *	printfile_suffix PROTO((void));
extern void	print_help PROTO((void));
extern void	pullcells PROTO((int));
extern char *	r_name PROTO((int, int, int, int));
extern void	readfile PROTO((char *, int));
extern void	repaint PROTO((int, int, int));
extern void	resetkbd PROTO((void));
extern void	rowshow_op PROTO((void));
extern void	scerror PROTO((char *));
extern void	scxfree PROTO((char *));
extern char *	scxmalloc PROTO((unsigned int));
extern char *	scxrealloc PROTO((char *, unsigned int));
extern void	setauto PROTO((int));
extern void	setiterations PROTO((int));
extern void	setorder PROTO((int));
extern void	show_top_line PROTO((void));
extern void	showcol PROTO((int, int));
extern void	showdr PROTO((void));
extern void	showrow PROTO((int, int));
extern void	showstring PROTO((char *, int, int, int, int, int *, int, int *,int,int,int));
extern void	signals PROTO((void));
extern void	slet PROTO((struct ent *, struct enode *, int));
extern void	startdisp PROTO((void));
extern void	startshow PROTO((void));
extern void	stopdisp PROTO((void));
extern void	str_search PROTO((char *));
extern void	sync_ranges PROTO((void));
extern void	sync_refs PROTO((void));
extern void	tblprintfile PROTO((char *, int, int, int, int));
extern void	unlock_cells PROTO((struct ent *, struct ent *));
extern void	update PROTO((int));
extern char *	v_name PROTO((int, int));
extern void	valueize_area PROTO((int, int, int, int));
extern char *	what_file PROTO((char *, char *));
extern void	write_fd PROTO((FILE *, int, int, int, int));
extern void	write_line PROTO((int));
extern void	write_range PROTO((FILE *));
extern int	writefile PROTO((char *, int, int, int, int));
extern int	yn_ask PROTO((char *));
extern void	yyerror PROTO((char *));


#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
extern int	Crypt;
#endif
extern int	autocalc;
extern int	autolabel;
extern int	calc_order;
extern int	collimit;
extern int	craction;
extern int	extfunc;
extern int	getrcqual;
extern int	loading;
extern char *	mdir;
extern int	modflg;
extern int	numeric;
extern double	prescale;
extern char *	progname;
extern int	propagation;
extern int	repct;
extern int	rndinfinity;
extern int	rowlimit;
extern int	showcell;
extern int	showtop;
extern int	tbl_style;
extern char *	version;

#if BSD42 || SYSIII

#ifndef cbreak
#define	cbreak		crmode
#define	nocbreak	nocrmode
#endif

#endif

/* Old (pre-autoconf):
#ifndef SYSV
#if ( defined(BSD42) || defined(BSD43) || defined(__convex__) ) && !defined(ultrix) && !defined(__osf__)
#define	memcpy(dest, source, len)	bcopy(source, dest, (unsigned int)len);
#define	memset(dest, zero, len)		bzero((dest), (unsigned int)(len));
#else
#include <memory.h>
#endif
#endif
*/
#if STDC_HEADERS
/*# include <string.h>*/
# define MEMZERO(dest, len)		memset((dest), 0, (unsigned int)(len));
#else
# ifndef HAVE_STRCHR
#  define strchr index
#  define strrchr rindex
# endif
extern char *strchr (), *strrchr ();
# ifndef HAVE_MEMCPY
#  define memcpy(d, s, n)		bcopy ((s), (d), (n))
#  define memmove(d, s, n)		bcopy ((s), (d), (n))
#  define MEMZERO(dest, len)		bzero((dest), (unsigned int)(len));
# else
#  define MEMZERO(dest, len)		memset((dest), 0, (unsigned int)(len));
# endif
#endif

/*
 * Feature-setting declarations.  Can be used to control the default
 * setting of various features.
 */

#ifndef	SHOWCURSOR
#define	SHOWCURSOR	TRUE
#endif

#ifndef CASEINSENSITIVECMDLINE	/* Peter Doemel, 11-Feb-1993 */
# if defined(VMS) || defined(MSDOS)
#  define CASEINSENSITIVECMDLINE
# endif
#endif

/*
 * Declarations of standard functions used in various places.  Mostly
 * to shut up line and 'gcc -Wall'.
 */

#ifdef __STDC__
/*
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
*/

extern int	re_exec();
#ifndef __FreeBSD__
extern int	stty();
#endif
extern int	wait();
extern int	yylex(void);
extern int	yyparse();

#else	/* __STDC__ */

extern int	_filbuf();
extern int	_flsbuf();
extern int	alarm();
extern int	close();
extern int	dup();
extern int	endwin();
extern int	execl();
#ifdef SYSV3
extern void	exit();
#endif
extern int	fclose();
extern FILE *	fdopen();
extern int	fflush();
extern FILE *	fopen();
extern int	fork();
extern int	fprintf();
extern int	fputs();
extern char	*getenv();
extern int	ioctl();
extern int	kill();
extern int	pclose();
extern int	pipe();
extern FILE *	popen();
extern double	pow();
extern int	printf();
extern int	printw();
extern int	puts();
extern int	re_exec();
extern int	read();
/*extern int	strlen();*/
/*extern size_t	strlen();*/
extern int	stty();
extern long	time();
extern int	tolower();
extern int	toupper();
extern int	waddch();
extern int	waddstr();
extern int	wait();
extern int	wclear();
extern int	wclrtobot();
extern int	wclrtoeol();
extern int	wmove() ;
extern int	wmove();
extern int	wrefresh();
extern int	write();
extern int	wstandend();
extern int	wstandout();
extern int	yylex();
extern int	yyparse();

#endif	/* __STDC__ */

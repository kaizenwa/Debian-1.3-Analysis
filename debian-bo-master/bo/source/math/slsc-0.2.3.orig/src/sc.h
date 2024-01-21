/*	SC	A Table Calculator
 *              Common definitions
 *
 *              original by James Gosling, September 1982
 *              modified by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              R. Bond  12/86
 *              More mods by Alan Silverstein, 3-4/88, see list of changes.
 * 
 *              User Interface completely rewritten, colors added, lots
 *              of cleanup performed.  
 *                   John E. Davis  (July 94)
 *              $Revision: 6.1 $
 *
 */



#define MAXROWS 200
#define MAXCOLS 124
#define RESCOL 4  /* columns reserved for row numbers */
#define RESROW 3  /* rows reserved for prompt, error, and column numbers */
#define DEFWIDTH 10 /* Default column width and precision */
#define DEFPREC   2


struct ent_ptr {
    int vf;
    struct ent *vp;
};

struct range_s {
	struct ent_ptr left, right;
};

/*
 * If you want to save room, make row and col below into unsigned
 * chars and make sure MAXROWS and MAXCOLS above are both less
 * than 256.  (128 if your compiler doesn't support unsigned char).
 *
 * Some not too obvious things about the flags:
 *    IS_VALID means there is a valid number in v.
 *    label set means it points to a valid constant string.
 *    IS_STREXPR set means expr yields a string expression.
 *    If IS_STREXPR is not set, and expr points to an expression tree, the
 *        expression yields a numeric expression.
 *    So, either v or label can be set to a constant. 
 *        Either (but not both at the same time) can be set from an expression.
 */

#define VALID_CELL(p, r, c) ((NULL != ((p = tbl[r][c]))) \
			     && ((p->flags & IS_VALID) || (p->label != NULL)))

struct ent {
    double v;
    char *label;
    struct enode *expr;
    short flags;
    short row, col;
    struct ent *next;
};

struct range {
    struct ent_ptr r_left, r_right;
    char *r_name;
    struct range *r_next, *r_prev;
    int r_is_range;
};

#define FIX_ROW 1
#define FIX_COL 2

struct enode {
    int op;
    union {
	double k;
	struct ent_ptr v;
	struct range_s r;
	char *s;
	struct {
	    struct enode *left, *right;
	} o;
    } e;
};

/* op values */
#define O_VAR 'v'
#define O_CONST 'k'
#define O_SCONST '$'
#define REDUCE 0200	/* Or'ed into OP if operand is a range */

#define OP_BASE 256
#define ACOS (OP_BASE + 0)
#define ASIN (OP_BASE + 1)
#define ATAN (OP_BASE + 2)
#define CEIL (OP_BASE + 3)
#define COS (OP_BASE + 4)
#define EXP (OP_BASE + 5)
#define FABS (OP_BASE + 6)
#define FLOOR (OP_BASE + 7)
#define HYPOT (OP_BASE + 8)
#define LOG (OP_BASE + 9)
#define LOG10 (OP_BASE + 10)
#define POW (OP_BASE + 11)
#define SIN (OP_BASE + 12)
#define SQRT (OP_BASE + 13)
#define TAN (OP_BASE + 14)
#define DTR (OP_BASE + 15)
#define RTD (OP_BASE + 16)
#define MIN_FUN (OP_BASE + 17)
#define MAX_FUN (OP_BASE + 18)
#define RND (OP_BASE + 19)
#define HOUR (OP_BASE + 20)
#define MINUTE (OP_BASE + 21)
#define SECOND (OP_BASE + 22)
#define MONTH (OP_BASE + 23)
#define DAY (OP_BASE + 24)
#define YEAR (OP_BASE + 25)
#define NOW (OP_BASE + 26)
#define DATE (OP_BASE + 27)
#define FMT (OP_BASE + 28)
#define SUBSTR (OP_BASE + 29)
#define STON (OP_BASE + 30)
#define EQS (OP_BASE + 31)
#define EXT (OP_BASE + 32)
#define ELIST (OP_BASE + 33)	/* List of expressions */
#define LMAX  (OP_BASE + 34)
#define LMIN  (OP_BASE + 35)
#define NVAL (OP_BASE + 36)
#define SVAL (OP_BASE + 37)
#define PV (OP_BASE + 38)
#define FV (OP_BASE + 39)
#define PMT (OP_BASE + 40)
#define STINDEX (OP_BASE + 41)
#define LOOKUP (OP_BASE + 42)
#define ATAN2 (OP_BASE + 43)
#define INDEX (OP_BASE + 44)

/* flag values */
#define IS_VALID     0001
#define IS_CHANGED   0002
#define IS_STREXPR   0004
#define IS_LEFTFLUSH 0010
#define IS_DELETED   0020

#define ctl(c) (c & 037) 

#define ESC 033
#define DEL 0177

#define BYCOLS 1
#define BYROWS 2
#define BYGRAPH 4		/* Future */

#define	TBL	1		/* tblprint style output for 'tbl' */
#define	LATEX	2		/* tblprint style output for 'LaTeX' */
#define	TEX	3		/* tblprint style output for 'TeX' */
#define	FRAME	4		/* tblprint style output for 'Frame' */

/* Types for etype(void) */

#define NUM	1
#define STR	2

extern struct ent *tbl[MAXROWS][MAXCOLS];
extern struct ent *Sc_Row_Pastebuffer;
extern struct ent *Sc_Col_Pastebuffer;
extern struct ent *Sc_Block_Pastebuffer;

extern char curfile[];
extern int currow, curcol;
extern int savedrow, savedcol;
extern int FullUpdate;
extern int maxrow, maxcol;
extern int Sc_Col_Width[MAXCOLS];
extern int precision[MAXCOLS];
extern char col_hidden[MAXCOLS];
extern char row_hidden[MAXROWS];
extern char line[1000];
extern int linelim;
extern int Sc_Changed;
extern int modflg;
extern int loading;
extern char *Progname;
extern char *mdir;
extern int showsc, showsr;
extern int Highlight_Cell;
extern int showtop;
extern int tbl_style;
extern int numeric;
extern int extfunc;
extern double prescale;
extern int Crypt;
extern int calc_order;
extern int autocalc;
extern int propagation;
/*
extern int strow, stcol;
extern struct enode *new(void);
extern struct enode *copye(void);
extern char *coltoa(void);
extern FILE *openout(void);
extern struct range *find_range(void);
*/
extern void sc_flush_saved (void);
extern void sc_free_ent (register struct ent *);

extern int modcheck(char *);

extern void sc_reset_display (void);
extern void sc_init_display (void);
extern void sc_message (char *, ...);
extern void slsc_error (char *, ...);
extern void sc_clear_message (void);
extern void sc_update(int);

extern void sc_quit (int);

extern void sc_set_auto (int);
extern void sc_set_order(int);

extern double eval(register struct enode *);

extern double Sc_Epsilon;
extern int Sc_Indicator_Start_Column;

extern struct ent *lookat(int, int);
extern char *sc_r_name (int, int, int, int);
extern char *sc_v_name (int, int);

extern struct enode *new_var (int, struct ent_ptr);
extern struct enode *new_range (int, struct range_s);
extern struct enode *new_const (int, double);
extern struct enode *new_str (char *);



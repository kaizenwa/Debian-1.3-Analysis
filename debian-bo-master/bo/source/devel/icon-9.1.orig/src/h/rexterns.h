/*
 * External declarations for the run-time system.
 */

/*
 * External declarations common to the compiler and interpreter.
 */
extern unsigned char allchars[];/* array for making one-character strings */
#ifndef MultiThread
extern struct region *curstring;
extern struct region *curblock;
#endif				/* MultiThread */
extern char *blkname[];		/* print names for block types. */
extern char *currend;		/* current end of memory region */
extern dptr *quallist;		/* start of qualifier list */
extern dptr argp;		/* argument pointer */
extern int bsizes[];		/* sizes of blocks */
extern int firstd[];		/* offset (words) of first descrip. */
extern uword segsize[];		/* size of hash bucket segment */
extern int k_level;		/* value of &level */
extern struct b_coexpr *stklist;/* base of co-expression stack list */
extern struct b_cset blankcs;   /* ' ' */
extern struct b_cset lparcs;    /* '(' */
extern struct b_cset rparcs;    /* ')' */
extern struct b_cset fullcs;    /* cset containing all characters */
extern struct descrip blank;	/* blank */
extern struct descrip emptystr;	/* empty string */
extern struct descrip k_current;/* &current */

#ifndef MultiThread
extern char *k_errortext;	/* value of &errortext */
extern int have_errval;		/* &errorvalue has a legal value */
extern int k_errornumber;	/* value of &errornumber */
extern int t_errornumber;	/* tentative k_errornumber value */
extern int t_have_val;		/* tentative have_errval flag */
extern struct b_file k_errout;	/* value of &errout */
extern struct b_file k_input;	/* value of &input */
extern struct b_file k_output;	/* value of &output */
extern struct descrip k_errorvalue;	/* value of &errorvalue */
extern struct descrip kywd_err; /* &error */
extern struct descrip kywd_pos; /* descriptor for &pos */
extern struct descrip kywd_prog; /* descriptor for &prog */
extern struct descrip kywd_ran; /* descriptor for &random */
extern struct descrip k_subject;/* &subject */
extern struct descrip kywd_trc; /* descriptor for &trace */
extern struct descrip k_eventcode; /* &eventcode */
extern struct descrip k_eventsource; /* &eventsource */
extern struct descrip k_eventvalue; /* &eventvalue */
extern struct descrip k_main;	/* value of &main */
#endif				/* MultiThread */

extern struct descrip kywd_dmp; /* descriptor for &dump */

#ifdef FncTrace
extern struct descrip kywd_ftrc; /* descriptor for &ftrace */
#endif					/* FncTrace */

extern struct descrip nullptr;	/* descriptor with null block pointer */
extern struct descrip lcase;	/* lowercase string */
extern struct descrip letr;	/* letter "r" */
extern struct descrip maps2;	/* second argument to map() */
extern struct descrip maps3;	/* third argument to map() */
extern struct descrip nulldesc;	/* null value */
extern struct descrip onedesc;	/* one */

#ifndef MultiThread
extern struct descrip t_errorvalue; /* tentative k_errorvalue value */
#endif					/* MultiThread */

extern struct descrip ucase;	/* uppercase string */
extern struct descrip zerodesc;	/* zero */

#ifndef MultiThread
extern uword blktotal;		/* cumulative total of all block allocations */
extern uword strtotal;		/* cumulative total of all string allocations */
#endif				/* MultiThread */

extern uword stattotal;		/* cumulative total of all static allocations */
				/* N.B. not currently set */

#ifndef MultiThread
extern word coll_tot;		/* total number of collections */
extern word coll_stat;		/* number of collections from static reqests */
extern word coll_str;		/* number of collections from string requests */
extern word coll_blk;		/* number of collections from block requests */
extern dptr globals; 		/* start of global variables */
extern dptr eglobals;		/* end of global variables */
extern dptr gnames;		/* start of global variable names */
extern dptr egnames; 		/* end of global variable names */
extern dptr estatics;		/* end of static variables */
#endif				/* MultiThread */

extern word mstksize;		/* size of main stack in words */
extern word qualsize;		/* size of string qualifier list */

extern word memcushion;		/* memory region cushion factor */
extern word memgrowth;		/* memory region growth factor */

extern word stksize;		/* size of co-expression stacks in words */
extern struct tend_desc *tend;  /* chain of tended descriptors */

#ifdef DosFncs
extern char *zptr;
#endif					/* DosFncs */

#if EBCDIC == 2
extern char ToEBCDIC[], FromEBCDIC[]; /* ASCII<->EBCDIC maps */
#endif					/* EBCDIC == 2 */

#ifdef StandardLib
#ifdef MacGraph
extern unsigned long starttime;	/* start time in milliseconds */
#else					/* MacGraph */
extern clock_t starttime;	/* start time in milliseconds */
#endif					/* MacGraph */
#else					/* StandardLib */
extern long starttime;		/* start time in milliseconds */
#endif					/* StandardLib */

#if !COMPILER
/*
 * External declarations for the interpreter.
 */

extern inst ipc;		/* interpreter program counter */
extern int ilevel;		/* interpreter level */
extern int ntended;		/* number of active tended descriptors */
extern struct b_cset k_ascii;	/* value of &ascii */
extern struct b_cset k_cset;	/* value of &cset */
extern struct b_cset k_digits;	/* value of &lcase */
extern struct b_cset k_lcase;	/* value of &lcase */
extern struct b_cset k_letters;	/* value of &letters */
extern struct b_cset k_ucase;	/* value of &ucase */
extern struct descrip tended[];	/* tended descriptors */
extern struct ef_marker *efp;	/* expression frame pointer */
extern struct gf_marker *gfp;	/* generator frame pointer */
extern struct pf_marker *pfp;	/* procedure frame pointer */
extern word *sp;		/* interpreter stack pointer */
extern word *stack;		/* interpreter stack base */
extern word *stackend;		/* end of evaluation stack */

#ifdef MultiThread
extern struct progstate *curpstate;
extern struct progstate rootpstate;
extern int noMTevents;		/* no MT events during GC */
#else					/* MultiThread */
extern char *code;		/* start of icode */
extern dptr statics; 		/* start of static variables */
extern char *strcons;		/* start of the string constants */
extern dptr fnames;		/* field names */
extern dptr efnames;		/* end of field names */
extern word *records;
extern int *ftabp;		/* field table pointer */
extern dptr xargp;
extern word xnargs;

extern word lastop;
#endif					/* MultiThread */

#ifdef ExecImages
extern int dumped;		/* the interpreter has been dumped */
#endif					/* ExecImages */


extern struct pstrnm pntab[];
extern int pnsize;

#else					/* COMPILER */
extern struct descrip statics[];/* array of static variables */
extern struct b_proc *builtins[];/* array of pointers to builtin functions */
extern int noerrbuf;		/* error buffering */
extern struct p_frame *pfp;	/* procedure frame pointer */
extern struct descrip trashcan; /* dummy descriptor that is never read */
extern int largeints;		/* flag: large integers supported */
#endif					/* COMPILER */

extern struct b_proc *op_tbl;   /* operators available for string invocation */
extern int op_tbl_sz;           /* number of operators in op_tbl */
extern int debug_info;		/* flag: debugging information is available */
extern int err_conv;		/* flag: error conversion is supported */
extern int dodump;		/* termination dump */
extern int line_info;		/* flag: line information is available */
extern char *file_name;		/* source file for current execution point */
extern int line_num;		/* line number for current execution point */
#ifndef MultiThread
extern int n_globals;		/* number of global variables */
extern int n_statics;		/* number of static variables */
extern struct b_coexpr *mainhead;	/* &main */
#endif				/* MultiThread */

#ifdef Polling
extern int pollctr;
#endif					/* Polling */

#ifdef Graphics
extern wbp wbndngs;
extern wcp wcntxts;
extern wsp wstates;
extern int GraphicsLeft, GraphicsUp, GraphicsRight, GraphicsDown;
extern int GraphicsHome, GraphicsPrior, GraphicsNext, GraphicsEnd;
extern int win_highwater, canvas_serial, context_serial;
#ifndef MultiThread
extern struct descrip kywd_xwin[];
extern struct descrip lastEventWin;
extern int lastEvFWidth, lastEvLeading, lastEvAscent;
extern struct descrip amperCol;
extern struct descrip amperRow;
extern struct descrip amperX;
extern struct descrip amperY;
extern struct descrip amperInterval;
extern uword xmod_control, xmod_shift, xmod_meta;
#endif					/* MultiThread */

extern stringint attribs[], drawops[];
#ifdef XWindows
extern struct _wdisplay * wdsplys;
extern stringint cursorsyms[];
#endif					/* XWindows */
#ifdef MSWindows
extern HINSTANCE mswinInstance;
extern int ncmdShow;
#endif					/* MSWindows */
#ifdef PresentationManager
/* this is the handle to the interpreter thread's anchor block */
extern HAB HInterpAnchorBlock;
extern HAB HMainAnchorBlock;
extern HMQ HInterpMessageQueue;
extern HMQ HMainMessageQueue;
extern lclIdentifier *LocalIds;
extern stringint siMixModes[];
extern stringint siLineTypes[];
extern stringint siColorNames[];
extern stringint siCursorSyms[];
extern LONG ScreenWidth;
extern LONG ScreenHeight;
extern LONG NumWindows;
extern LONG MaxPSColors;
extern colorEntry *ColorTable;
extern ULONG areaAttrs;
extern ULONG lineAttrs;
extern ULONG charAttrs;
extern ULONG imageAttrs;
#endif					/* PresentationManager */
#ifdef ConsoleWindow
extern FILE *ConsoleBinding;
extern unsigned long ConsoleFlags;
extern char ConsoleStringBuf[];
extern char *ConsoleStringBufPtr;
#endif					/* ConsoleWindow */
#endif					/* Graphics */

#if UNIX
#ifdef KeyboardFncs
extern int keyboard_error;
#endif					/* KeyboardFncs */
#endif					/* UNIX */

#ifdef EventMon
extern word oldsum;
extern struct descrip csetdesc;		/* cset descriptor */
extern struct descrip eventdesc;	/* event descriptor */
extern struct b_iproc mt_llist;
extern struct descrip rzerodesc;	/* real descriptor */
extern struct b_real realzero;		/* real zero block */
#endif				/* EventMon */

/*
     YABASIC --- a tiny integrated Basic Compiler/Interpreter

     header-file
     
     this Program is subject to the GNU General Public License;
     see the file basic.c for details.
*/

/* ------------- variables needed in all files ------------------------ */

extern char *string;

/* ------------- enum type error is needed in ba.bison ---------------- */

#ifdef FATAL
#undef FATAL
#endif
#ifdef ERROR
#undef ERROR
#endif
#ifdef WARNING
#undef WARNING
#endif
#ifdef NOTE
#undef NOTE
#endif
#ifdef DIAGNOSTIC
#undef DIAGNOSTIC
#endif
enum error {  /* error levels  */
  FATAL,ERROR,WARNING,NOTE,DIAGNOSTIC
};



/* ------------- function prototypes for ... ---------------- */
/* ------------- main program and auxiliary functions ---------------- */
int main(int,char *argv[]);
int yyparse(void); /* yyparse is supplied by BISON */
struct symbol *get_sym(char *,int,int); /* find and/or add a symbol */
struct command *add_command(int); /* get room for new command */
void parse_arguments(int,char *argv[]); /* parse arguments from command line */
void initialize(void); /* give correct values to pointers etc ... */
void reset(void); /* reset pointers to their initial values */
void signal_handler(int);  /* handle various signals */
void error(int,char *); /* reports an error and possibly exits */
void uif(int); /* issues warning on screen */
char *my_strdup(char *); /* my own version of strdup */
void *my_malloc(unsigned); /* my own version of malloc */
void run_it(void); /* execute the compiled code */

/* ------------- WIN95-specific stuff ----------- */
#ifdef WIN95
LRESULT CALLBACK mywindowproc(HWND,unsigned,UINT,DWORD); /* window-proc */
void win_startup(void); /* startup operations specific for WIN95 */
void win_shutdown(void); /* perform shutdown operations for WIN95 */
void chop_command(char *,int *,char ***); /* chops WIN95-commandline */
void startdraw(void); /* prepare for drawing */
DWORD winthread(LPWORD); /* window-thread */
char *getreg(char *); /* get default from Registry */
BOOL CtrlHandler(DWORD);  /* Handle signals */ 

#endif

/* ------------- double handling ---------------- */
void create_negate(void); /* creates command negate */
void negate(void);  /* negates top of stack */
void create_pushdblsym(char *); /* create command 'pushdblsym' */
void pushdblsym(struct command *); /* push double symbol onto stack */
void create_popdblsym(char *); /* create command 'popdblsym' */
void popdblsym(struct command *); /* pop double from stack */
void create_pushdbl(double); /* create command 'pushdbl' */
void pushdbl(struct command *); /* push double onto stack */
void create_dblbin(char); /* create binary expression calculation */
void dblbin(struct command *); /* compute with two numbers from stack */
void create_dblrelop(char); /* create command dblrelop */ 
void dblrelop(struct command *);  /* compare topmost double-values */

/* ------------- string handling ---------------- */
void create_pushstrsym(char *); /* push string symbol onto stack */
void pushstrsym(struct command *);   /* push string symbol onto stack */
void create_popstrsym(char *); /* create command 'popstrsym' */
void popstrsym(struct command *); /* pop string from stack */
void create_concat(void); /* creates command concat */
void concat(void); /* concetenates two strings from stack */
void create_pushstr(char *); /* creates command pushstr */
void pushstr(struct command *); /* push string onto stack */
void create_strrelop(char); /* create command strrelop */ 
void strrelop(struct command *);  /* compare topmost string-values */

/* ------------- i/o ---------------- */
void create_print(char); /* create command 'print' */
void print(struct command *); /* print on screen */
void create_myread(char); /* create command 'read' */
void myread(struct command *); /* read from file or stdin */
void create_prompt(char *); /* create command 'prompt' */
void prompt(struct command *); /* set input prompt */
void create_myopen(double,char *); /* create command 'myopen' */
void myopen(struct command *); /* open specified file for given name */
void create_myclose(double); /* create command 'myclose' */
void myclose(struct command *); /* close the specified stream */
void create_myswitch(double); /* create command 'myswitch' */
void myswitch(struct command *); /* switch to specified stream */
int badstream(int); /* test for valid stream id */

/* ------------- grafics ---------------- */
void create_openwin(int); /* create Command 'openwin' */
void openwin(struct command *); /* open a Window */
void create_openps(double,int); /* create Command 'open_ps' */
void openps(struct command *); /* store information for postscript-output */
void create_closeps();  /* create Command 'closeps' */
void closeps(); /* close the ps-file */
void create_openprinter(int); /* create command 'openprinter' */
void openprinter(struct command *); /* opens a printer for WIN95 */
void create_closeprinter(); /* create command 'closeprinter' */
void closeprinter(); /* closes printer for WIN95 */
void create_dot(void); /* create Command 'dot' */
void dot(void); /* draw a dot */
void create_line(void); /* create Command 'line' */
void line(void); /* draw a line */
void create_circle(); /* create Command 'circle' */
void circle(); /* draw a circle */
void create_text(int); /* create Command 'text' */
void text(struct command *); /* write a text */
void create_closewin(void); /* create Command 'closewin' */
void closewin(void); /* close the window */
void create_clearwin(void); /* create Command 'clearwin' */
void clearwin(void); /* clear the window */
void noXwarn(void); /* issue warning that no X-Grafic is available */
void calc_psscale();  /* calculate scale-factor for postscript */

/* ------------- flow--control ---------------- */
void create_goto(char *); /* creates command goto */
void create_gosub(char *); /* creates command gosub */
void create_label(char *); /* creates command label */
void pushgoto(void); /* generate label and push goto on stack */
void popgoto(void); /* pops a goto and generates the matching command */
void jump(struct command *); /* jump to specific Label */
void create_return(void); /* creates command return */
void myreturn(void); /* return from gosub */
void create_skipper(void); /* creating command skipper */
void skipper(void); /* used for on_goto/gosub, skip commands */
void create_nop(void); /* does nothing */
void create_myend(void); /* create command 'end' */
void myend(void); /* is called at the end of program execution */
void create_decide(void); /* creates command decide */
void decide(void); /*  skips next command, if 0 on stack */

/* ------------- miscellanous basic commands ---------------- */
void create_boole(char); /* create command boole */ 
void boole(struct command *);  /* perform and/or/not */
void create_call(char *); /* creates the command call */ 
void callfun(struct command *); /* call a function */
void callarr(struct command *); /* call an array */
void create_dim(char *,char); /* create command 'dim' */
void dim(struct command *); /* get room for array */
void create_restore(char *); /* create command 'restore' */
void restore(struct command *); /* reset data pointer to given label */
void create_dbldata(double);  /* create command dbldata */
void create_strdata(char *);  /* create command strdata */
void create_readdata(char); /* create command readdata */
void readdata(struct command *); /* read data items */
void create_wait(); /* create Command 'wait' */
void wait(); /* wait given number of seconds */
void create_bell(); /* create Command 'bell' */
void bell(); /* ring ascii bell */
void setinfolevel(void); /* set infolevel to content of variable */
void setwinheight(void);  /* set winheight to content of variable */
void setwinwidth(void); /* set winwidth to content of variable */
void setfontheight(void); /* set fontheight to content of variable */



/* ------------- basic functions ---------------- */
void add_function(char *,char *,void f(int i,char c)); /* add function */
void mysin(int,char); /* wraparound for 'sin'-function */
void myasin(int,char); /* wraparound for 'asin'-function */
void mycos(int,char); /* wraparound for 'cos'-function */
void myacos(int,char); /* wraparound for 'acos'-function */
void mytan(int,char); /* wraparound for 'tan'-function */
void myatan(int,char); /* wraparound for 'atan'-function */
void mylog(int,char); /* wraparound for 'log'-function */
void myexp(int,char); /* wraparound for 'exp'-function */
void mysqrt(int,char); /* wraparound for 'sqrt'-function */
void myint(int,char); /* wraparound for 'int'-function */
void myfrac(int,char); /* wraparound for 'frac'-function */
void myleft(int,char); /* 'left$'  string--function */
void myright(int,char); /* 'right$'  string--function */
void mymid(int,char); /* 'mid$'  string--function */
char *fromto(char *,int,int); /* portion of string (for mid$,left$,right$) */
void mylen(int,char); /* 'len' function */
void myval(int,char); /* 'val' function: transform string into number */
void mystr(int,char); /* 'str' function: transform number into string */

/* ------------- other stack operations ---------------- */
void pushletter(char *); /* push letter on stack (for argument test) */
void pushlabel(void); /* generate goto and push label on stack */
void poplabel(void); /* pops a label and generates the matching command */
void swap(void); /*swap topmost elements on stack */
struct stackentry *push(void); /* push element on stack and enlarge it*/
struct stackentry *pop(void); /* pops element to memory */

/* ------------- flex ------------------------ */
void yyerror(char *); /* yyerror message */

/* ------------- global types ---------------- */ 

struct stackentry { /* one element on stack */ 
  int type;     /* contents of entry */
  struct stackentry *next;
  struct stackentry *prev;
  void *pointer; /* multiuse ptr */
  double value;  /* double value, only one of pointer or value is used */
};

struct symbol {   /* general symbol; either variable, string, label or call */
  int type;
  struct symbol *next;
  char *name;
  void *pointer;   /* general pointer */
  char *args;      /* used to store number of arguments for functions/array */
  double value;
};

struct command { /* one interpreter command */
  int type;    /* type of command */
  struct command *next;  /* link to next command */
  void *pointer;       /* pointer to command specific data */
  int args;  /* number of arguments for function/array call */
             /* or stream number for open/close             */
  char tag;  /* letter to pass some information */
  int line; /* line this command has been created for */
};

struct array { /* data structure for arrays */
  int bounds[10];  /* index boundaries */
  int dimension; /* dimension of array */
  int total; /* product of all dimensions */
  int dimed;      /* Flag to mark if array has been dimed already */
  void *pointer; /* contents of array */
  char type;  /* decide between string- ('s') and double-Arrays ('d') */
};



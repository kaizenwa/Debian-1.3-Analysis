/*           
     YABASIC ---  a tiny integrated Basic Compiler/Interpreter
     written by Marc-Oliver Ihm in 1995/96.
     e-mail: ihm@kph.uni-mainz.de

     Current Version:
*/
#define BASIC_VERSION            "2.0"
/*

     Date of last change: 
*/
#define DOLC                     "November 2, 1996"
/*
     This program is free software; you can redistribute it and/or
     modify it under the terms of the GNU General Public License
     as published by the Free Software Foundation; either version 2
     of the License, or (at your option) any later version.
     
     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.
     
     You should have received a copy of the GNU General Public License
     along with this program (the file is named "COPYING");
     if not, write to the Free Software
     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

/* ------------- defines ---------------- */

/*
   Define one and only one of the following symbols, depending on your
   System:
            - PLAIN : Only most common Ansi and K&R-Features are used, 
	              i.e. NO grafics, NO timers
            - UNIX : includes PLAIN and additionally timers-fucntions
            - UNIX_X11 : includes UNIX and additionally X11-based grafics
            - WIN95 : includes PLAIN and additionally Windows95
                      timers and grafics

   Look at the following lines to see what happens, if you define multiple
   symbols:
*/

#ifdef PLAIN
#undef UNIX
#undef UNIX_X11
#undef WIN95
#endif

#ifdef UNIX
#undef UNIX_X11
#undef WIN95
#endif

#ifdef UNIX_X11
#define UNIX
#undef WIN95
#endif

#ifdef WIN95
#define BRIGHT_TEXT FOREGROUND_INTENSITY
#define DIM_TEXT 0
#endif

#define BASIC_NAME "yabasic"

#define DONE {current=current->next;break;}  /* reduces type-work */

#if !defined(TRUE)
#define TRUE (1==1)
#endif

#ifndef FALSE
#define FALSE (1!=1)
#endif


/* ------------- includes ---------------- */

#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <string.h>
#include <math.h>
#include <time.h>

#ifdef UNIX
#include <sys/time.h>
#endif

#ifdef WIN95
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#endif

#include <unistd.h>
#include <signal.h>
#include <ctype.h>

#ifdef UNIX_X11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif

#include "yabasic.h"          /* all prototypes and structures */


/* ------------- external references ---------------- */

extern int yylineno;   /* current line number; counted in ba.y */
extern void switch_to_my_file(FILE *inputfile); /* switches lex input  */
                                                /* to given file       */ 
extern int yyparse();  /* call bison parser */


/* ------------- enum types ---------------- */

enum stck_or_sym { /* contents of a stack element or type of a symbol */
  STRING,NUMBER,LBL,GTO,JUMP,FREE,FUNC,ARRAY,RETADD,NIL
};

enum cmd_type { /* type of command */
  LABEL,GOTO,QGOTO,GOSUB,QGOSUB,RETURN,          /* flow control */
  END,DECIDE,SKIPPER,NOP,

  DIM,CALLFUN,CALLARR,                           /* everything with "()"*/

  DBLADD,DBLMIN,DBLMUL,DBLDIV,DBLPOW,NEGATE,     /* double operations */
  PUSHDBLSYM,POPDBLSYM,PUSHDBL,

  SETINFOLEVEL,SETFONTHEIGHT,SETWINHEIGHT,       /* internal vars */
  SETWINWIDTH,

  AND,OR,NOT,LT,GT,LE,GE,EQ,NE,STREQ,STRNE,      /* comparisons */

  PUSHSTRSYM,POPSTRSYM,PUSHSTR,CONCAT,           /* string operations */

  PRINT,MYREAD,PROMPT,RESTORE,QRESTORE,          /* i/o operations */
  READDATA,DATA,MYOPEN,MYCLOSE,MYSWITCH,
  WAIT,BELL,
  
  OPENWIN,DOT,LINE,CIRCLE,TEXT,CLOSEWIN,CLEARWIN,       /* grafics */
  OPENPRN,CLOSEPRN
};

enum states { /* current state of program */
  HATCHED,INITIALIZED,COMPILING,RUNNING,FINISHED
};



/* ------------- global variables ---------------- */

struct symbol *symroot; /* first element in symbol list */
struct symbol *symhead; /* last element ind symbol list */
struct stackentry *stackroot; /* lowest element in double stack */
struct stackentry *stackhead; /* topmost element in double stack */
struct command *current; /* currently executed command */
struct command *cmdroot; /* first command */
struct command *cmdhead; /* last command */
struct command *datapointer; /* current location for read-command */
int dimcount; /* used in ba.bison to count array-dimension */
int infolevel; /* controls issuing of error messages */
int errorlevel; /* highest level of error message seen til now */
int diagnostic_count; /* number of diagnostic messages */
int note_count; /* number of notes */
int warning_count; /* number of warning messages */
int error_count; /* number of error messages */
int interactive; /* true, if commands come from stdin */
char *string; /* for trash-strings */
int labelcount=0; /* count self-generated labels */
int commandcount; /* total number of commands */
int program_state;  /* state of program */
char inputprompt[100]; /* input prompt */
FILE *streams[10]; /* file streams */
FILE *currentstream; /* current stream for output ... */

/* printer-related */
FILE *printerfile=NULL; /* file to print on */
char *prfilename=NULL; /* filename to print on */
int print_to_file; /* print to file ? */

/* ------------- global variables for Graphics ---------------- */

#ifdef UNIX_X11
Display *display; /* X-Display */
Window window,root; /* ID of window and root window */
GC gc; /* GC for drawing */
#endif
#ifdef WIN95
WNDCLASS myclass; /* window class for my program */
HWND window;   /* handle of my window */
HINSTANCE this_instance;
HDC devcon; /* device context */
char *my_class="my_class";
struct { /* contains all information for window-thread */
  HANDLE handle; /* handle of thread */
  DWORD id;    /* id of my thread */
  HANDLE winevent; /* handle for window event */
} thread;
LOGFONT logfont; /* structure for font-characteristics */
HFONT myfont; /* handle of font for screen */
HFONT printerfont; /* handle of printer-font */
HPEN printerpen; /* handle of printer-pen */
HDC printer=NULL; /* handle of printer */
float prnscale;  /* scaling factor for printer */
float prnoff;  /* offset for printer */
#endif

int Xwarncount=0; /* count issuing of noXwarn */
unsigned long winwidth,winheight;  /* size of window */
int winopened=FALSE; /* flag if window is open already */
double psscale; /* from pixels to points */
int fontheight; /* height of used font in pixel */
char *foreground=NULL;
char *background=NULL;
char *geometry=NULL;
char *displayname=NULL;
char *font=NULL;

/* ------------- main program ---------------- */

#ifdef WIN95
int PASCAL WinMain(HINSTANCE _this_instance,/* Handle for this instance */
		   HINSTANCE prev_instance,        /* not needed here */
		   LPSTR commandline,              /* command line */
		   int windowstate)                /* state of window */
{
  time_t start,compiled,ended;
  int argc;
  char **argv;
  
  this_instance=_this_instance; /* copy into global variable */
  
  chop_command(commandline,&argc,&argv); /* chop commandline */
  
  /* allocate console window */
  AllocConsole();
  SetConsoleTitle(" "BASIC_NAME" - Main Window");
  /* bent standard-handles to console */
  stdin->_handle=_hdopen((long)GetStdHandle(STD_INPUT_HANDLE),_O_TEXT);
  stdout->_handle=_hdopen((long)GetStdHandle(STD_OUTPUT_HANDLE),_O_TEXT);
  stderr->_handle=_hdopen((long)GetStdHandle(STD_ERROR_HANDLE),_O_TEXT);
  setvbuf(stdin,NULL,_IONBF,0);
  setvbuf(stdout,NULL,_IONBF,0);
  setvbuf(stderr,NULL,_IONBF,0);

  /* define my window class */
  myclass.style=0;
  myclass.lpfnWndProc=(LPVOID) mywindowproc;
  myclass.cbClsExtra=0; /* no extra bytes */
  myclass.cbWndExtra=0;
  myclass.hInstance=this_instance;
  myclass.hIcon=LoadIcon(this_instance,"yabasicIcon");
  myclass.hCursor=LoadCursor(NULL,IDC_ARROW); /*  standard cursor */
  myclass.hbrBackground=(HBRUSH) COLOR_WINDOW; /* default-background */
  myclass.lpszMenuName=NULL;
  myclass.lpszClassName=my_class;

  /* register class */
  RegisterClass(&myclass);
#else
  int main(int argc,char *argv[])
    {
      time_t start,compiled,ended;
#endif

  string=(char *)malloc(sizeof(char)*1000);

  program_state=HATCHED;
  infolevel=WARNING; /* set the initial Infolevel */

  parse_arguments(argc,argv);

#ifdef WIN95
  win_startup();
#endif

  time(&start);
  initialize();
  program_state=INITIALIZED;

  error(DIAGNOSTIC,"Stacks and lists Initialized"); 
  error(NOTE,"Calling parser/compiler");
  if (interactive) {
    printf("Enter your program; type <RETURN> twice, when done.\n");
  }
  program_state=COMPILING;
  yyparse();
  create_myend();
  sprintf(string,"Read %d line(s)",yylineno);
  error(NOTE,string);
  sprintf(string,"Generated %d command(s)",commandcount);
  error(NOTE,string);

  time(&compiled);
          
  if (errorlevel>ERROR) {
    error(DIAGNOSTIC,"executing compiled program");
    program_state=RUNNING;
    run_it();}
  else {
    program_state=FINISHED;
    error(ERROR,"Program not executed");
  }

  sprintf(string,"%d diagnostic(s), %d note(s), %d warning(s), %d error(s)",
          diagnostic_count,note_count,warning_count,error_count);
  error(NOTE,string);
  time(&ended);
  sprintf(string,"Compilation time = %g second(s)",difftime(compiled,start));
  error(NOTE,string);
  sprintf(string,"Execution time = %g second(s)",difftime(ended,compiled));
  error(NOTE,string);

#ifdef WIN95 /* press return */
  win_shutdown();
#endif
  
  if (errorlevel>ERROR) 
    return TRUE;
  else
    return FALSE;
  }



/* ------------- subroutines ---------------- */


struct symbol *get_sym(char *name,int type,int add) 
/* gets the value of a symbol, or creates it with initial value type */
{
  struct symbol *curr,*new;

  curr=symroot;
  while(curr!=symhead) {   /* search 'til head of list */
    if (curr->type==type  /* do the types match ? */
        &&!strcmp(name,curr->name))  /* do the names match ? */
      return curr; /* give back address */
    curr=curr->next; /* try next entry */
  }
  /* run (ppp) through all variables. */
  if (!add) return NULL;  /* dont create, if not found */
  /* end of list, create new element */
  new=(struct symbol *)my_malloc(sizeof(struct symbol)); /* ceate new item */
  symhead=new;  /* advance head */
  curr->name=my_strdup(name);  /* store new name */
  curr->next=new;
  curr->value=0.0;
  curr->pointer=NULL;
  curr->type=type;
  curr->value=0.0;
  if (type==STRING) {   /* create empty string */
    curr->pointer=my_malloc(sizeof(char));
    *(char *)(curr->pointer)='\0';
  }
  return curr;
}
    
    
void create_pushdbl(double value) /* create command 'pushdbl' */
{
  struct command *cmd;
  
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Creating command 'pushdbl', value=%g",value);
    error(DIAGNOSTIC,string);
  }
  cmd=add_command(PUSHDBL);
  if (cmd->pointer==NULL) cmd->pointer=my_malloc(sizeof(double));
  *(double *)(cmd->pointer)=value;
}


void pushdbl(struct command *cmd) 
{
  /* push double onto stack */
  struct stackentry *p;

  p=push();
  p->value= *(double *)cmd->pointer;
  p->type=NUMBER;
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Pushing number %g",p->value);
    error(DIAGNOSTIC,string);
  }
}


void create_pushdblsym(char *symbol) /* create command 'pushdblsym' */
{
  struct command *cmd;

  sprintf(string,"Creating command 'pushdblsym' from symbol '%s'",symbol);
  error(DIAGNOSTIC,string);
  cmd=add_command(PUSHDBLSYM);
  /* get room to store specific information */
  cmd->pointer= &(get_sym(symbol,NUMBER,TRUE)->value);
}


void pushdblsym(struct command *cmd) 
{
  /* push double symbol onto stack */
  struct stackentry *p;
  
  p=push();
  p->value= *(double *)cmd->pointer;
  p->type=NUMBER;
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Pushing symbol %g",*(double *)cmd->pointer);
    error(DIAGNOSTIC,string);
  }
}


void create_popdblsym(char *symbol) /* create command 'popdblsym' */
{
  struct command *cmd;
  struct symbol *s;

  sprintf(string,"Creating command 'popdblsym' to symbol '%s'",symbol);
  error(DIAGNOSTIC,string);
  cmd=add_command(POPDBLSYM);
  /* storing specific information: */
  s=get_sym(symbol,NUMBER,TRUE);
  cmd->pointer= &(s->value);
  
  /* treat internal vars */
  if (!strncmp(symbol,"yab",3)) {
    if (!strcmp(symbol,"yabinfolevel")) {
      error(DIAGNOSTIC,"Creating command 'set infolevel'");
      cmd=add_command(SETINFOLEVEL);
    }
    if (!strcmp(symbol,"yabwinheight")) {
      error(DIAGNOSTIC,"Creating command 'set winheight'");
      cmd=add_command(SETWINHEIGHT);
    }
    if (!strcmp(symbol,"yabwinwidth")) {
      error(DIAGNOSTIC,"Creating command 'set winwidth'");
      cmd=add_command(SETWINWIDTH);
    }
    if (!strcmp(symbol,"yabfontheight")) {
      error(DIAGNOSTIC,"Creating command 'set fontheight'");
      cmd=add_command(SETFONTHEIGHT);
    }
  }
}


void setinfolevel(void)
{
  /* set infolevel to content of variable infolevel */
  int i;
  static char *levels[]={"FATAL","ERROR","WARNING","NOTE","DIAGNOSTIC"};
  
  i=get_sym("yabinfolevel",NUMBER,FALSE)->value;
  
  if (i!=DIAGNOSTIC && i!=NOTE && i!=WARNING && 
      i!=ERROR && i!=FATAL) return;
  
  if (infolevel<i) infolevel=i;
  if (infolevel<=DIAGNOSTIC) {
    sprintf(string,"setting infolevel to %s",levels[i-FATAL]);
    error(DIAGNOSTIC,string);
  }
  infolevel=i;
}


void setwinheight(void)
{
  /* set winheight to content of variable winheight */
  
  winheight=get_sym("yabwinheight",NUMBER,FALSE)->value;
  get_sym("yabwinheight",NUMBER,FALSE)->value=winheight;

  if (infolevel<=DIAGNOSTIC) {
    sprintf(string,"setting winheight to %d",(int)winheight);
    error(DIAGNOSTIC,string);
  }
  calc_psscale();
}


void setwinwidth(void)
{
  /* set winwidth to content of variable winwidth */
  
  winwidth=get_sym("yabwinwidth",NUMBER,FALSE)->value;
  get_sym("yabwinwidth",NUMBER,FALSE)->value=winwidth;

  if (infolevel<=DIAGNOSTIC) {
    sprintf(string,"setting fontheight to %d",(int)winwidth);
    error(DIAGNOSTIC,string);
  }
  calc_psscale();
}


void setfontheight(void)
{
  /* set fontheight to content of variable fontheight */
  
  fontheight=get_sym("yabfontheight",NUMBER,FALSE)->value;
  get_sym("yabfontheight",NUMBER,FALSE)->value=fontheight;

  if (infolevel<=DIAGNOSTIC) {
    sprintf(string,"setting fontheight to %d",fontheight);
    error(DIAGNOSTIC,string);
  }
  calc_psscale();
}


void popdblsym(struct command *cmd) 
{
  /* pop double from stack */
  double d;

  d=pop()->value;
  *(double *)(cmd->pointer)=d;
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Popping to symbol; value=%g",d);
    error(DIAGNOSTIC,string);
  }
}


void create_dblbin(char c) /* create command for binary double operation */
{
  sprintf(string,"Creating command 'dblbin %c'",c);
  error(DIAGNOSTIC,string);
  switch(c) {
  case '+':add_command(DBLADD);break;
  case '-':add_command(DBLMIN);break;
  case '*':add_command(DBLMUL);break;
  case '/':add_command(DBLDIV);break;
  case '^':add_command(DBLPOW);break;
  }
  /* no specific information needed */
}


void dblbin(struct command *cmd) /* compute with two numbers from stack */
{
  struct stackentry *d;
  double a,b,c;

  b=pop()->value;
  a=pop()->value;
  d=push();
  switch(cmd->type) {
  case(DBLADD):c=a+b; break;
  case(DBLMIN):c=a-b; break;
  case(DBLMUL):c=a*b; break;
  case(DBLDIV): 
    if (fabs(b)<DBL_MIN) {
      sprintf(string,"Division by zero, set to %g",DBL_MAX);
      error(NOTE,string);
      c=DBL_MAX;}
    else
      c=a/b;
    break;
  case(DBLPOW):
    if (b==2) 
      c=a*a;
    else if (a<0) {
      error(ERROR,"Power of negative value. Don't now what to do");
      return;}
    else
      c=exp(b*log(a));
    break;
  }
  d->value=c;
  d->type=NUMBER;
}


void create_negate() /* creates command negate */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'negate'");
  cmd=add_command(NEGATE);
}


void negate() /* negates top of stack */
{
  struct stackentry *a,*b;
  double d;

  a=pop();
  d=a->value;
  b=push();
  b->type=NUMBER;
  b->value= -d;
}


void create_pushstrsym(char *symbol) /* push string-symbol onto stack */
{
  struct command *cmd;

  sprintf(string,"Creating command 'pushstrsym' from symbol '%s'",symbol);
  error(DIAGNOSTIC,string);
  cmd=add_command(PUSHSTRSYM);
  /* get room to store specific information */
  cmd->pointer=&get_sym(symbol,STRING,TRUE)->pointer;
}


void pushstrsym(struct command *cmd) 
{
  /* push string-symbol onto stack */
  struct stackentry *p;
  
  p=push();
  p->pointer=my_strdup(*(char **)cmd->pointer);
  p->type=STRING;
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Pushing from string-symbol; value='%s'",
            (char *)p->pointer);
    error(DIAGNOSTIC,string);
  }
}


void create_popstrsym(char *symbol) /* create command 'popstrsym' */
{
  struct command *cmd;
  struct symbol *s;

  sprintf(string,"Creating command 'popstrsym' to symbol '%s'",symbol);
  error(DIAGNOSTIC,string);
  cmd=add_command(POPSTRSYM);
  /* storing specific information: */
  s=get_sym(symbol,STRING,TRUE);
  cmd->pointer=(char **)&(s->pointer);
}


void popstrsym(struct command *cmd)    /* pop string from stack */
{
  struct stackentry *p;

  p=pop();
  if (*(char **)cmd->pointer!=NULL) free(*(char **)cmd->pointer);
  *(char **)cmd->pointer=my_strdup(p->pointer);
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Popping to string-symbol; value='%s'"
            ,(char *)p->pointer);
    error(DIAGNOSTIC,string);
  }
}


void create_concat() /* creates command concat */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'concat'");
  cmd=add_command(CONCAT);
}


void concat() /* concetenates two strings from stack */
{
  struct stackentry *a,*b,*c;
  char *aa,*bb,*cc;

  a=pop();
  b=pop();
  if (a->type!=STRING || b->type!=STRING) {
    error(FATAL,"Need strings to concat");
    return;
  }
  aa=a->pointer;
  bb=b->pointer;
  cc=(char *) my_malloc(sizeof(char)*(strlen(aa)+strlen(bb)+1));
  strcpy(cc,bb);
  strcat(cc,aa);
  c=push();
  c->type=STRING;
  c->pointer=cc;
}  


void create_pushstr(char *s) /* creates command pushstr */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'pushstr'");
  cmd=add_command(PUSHSTR);
  cmd->pointer=my_strdup(s); /* store string */
}


void pushstr(struct command *cmd) 
{
  /* push string onto stack */
  struct stackentry *p;
  
  p=push();
  p->pointer=my_strdup((char *)cmd->pointer);
  p->type=STRING;
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Pushing string '%s'",(char *)p->pointer);
    error(DIAGNOSTIC,string);
  }
}


void create_goto(char *label) /* creates command goto */
{
  struct command *cmd;

  sprintf(string,"Creating command 'goto %s'",label);
  error(DIAGNOSTIC,string);
  cmd=add_command(GOTO);
  /* specific info */
  cmd->pointer=my_strdup(label);
}


void create_gosub(char *label) /* creates command gosub */
{
  struct command *cmd;

  sprintf(string,"Creating command 'gosub %s'",label);
  error(DIAGNOSTIC,string);
  cmd=add_command(GOSUB);
  /* specific info */
  cmd->pointer=my_strdup(label);
}


void jump(struct command *cmd) 
/* jump to specific Label; used as goto or gosub */
{
  struct command *curr;
  struct stackentry *ret;
  int type;

  type=cmd->type;
  if (type==QGOSUB || type==GOSUB) {
    ret=push();
    ret->pointer=current;
    ret->type=RETADD;
  }
  if (type==QGOSUB || type==QGOTO) {
    current=(struct command *)cmd->pointer;
    error(DIAGNOSTIC,"Performing quick jump");
    return;
  }
  curr=cmdroot;
  sprintf(string,"Searching Label '%s'",(char *)cmd->pointer);
  error(DIAGNOSTIC,string);
  while(curr!=cmdhead) {   /* go through all commands */
    if (curr->type==LABEL && !strcmp(curr->pointer,cmd->pointer)) {
      /* found right Label ! */
      current=curr; /* jump to new location */
      sprintf(string,"Resolved label '%s', performing jump",
              (char *)cmd->pointer);
      /* use the address instead of the name next time ! */
      cmd->pointer=curr;
      cmd->type=(type==GOTO) ? QGOTO:QGOSUB; /* quick jump from now on */
      error(DIAGNOSTIC,string);
      return;
    }
    curr=curr->next;
  }
  /* label not found */
  sprintf(string,"Cant find label '%s'",(char *)cmd->pointer);
  error(ERROR,string);
}


void create_return() /* creates command return */
{
  struct command *cmd;

  error(DIAGNOSTIC,"Creating command 'return'");
  cmd=add_command(RETURN);
}


void myreturn() /* return from gosub */
{
  struct stackentry *address;

  address=pop();
  if (address->type!=RETADD) {
    error(ERROR,"RETURN without GOSUB");
    return;
  }
  current=(struct command *)address->pointer;
  error(DIAGNOSTIC,"Returning from subroutine");
  return;
}


void create_label(char *label) /* creates command label */
{
  struct command *cmd;

  sprintf(string,"Creating command 'label %s'",label);
  error(DIAGNOSTIC,string);
  cmd=add_command(LABEL);
  /* specific info */
  cmd->pointer=my_strdup(label);
}


void create_skipper() /* creating command skipper */
{
  error(DIAGNOSTIC,"Creating command 'skipper'");
  add_command(SKIPPER);
}


void skipper()
/* used for on_goto/gosub, skip specified number of commands */
{
  int i,n;

  n=pop()->value;
  i=1;
  current=current->next;
  while(i<n && (current->next)->type!=NOP) { /* go through all labels */
    current=current->next; /* next label */
    i++;
  }
  if (infolevel>=DIAGNOSTIC) {
    sprintf(string,"skipped %d labels",i);
    error(DIAGNOSTIC,string);
  }
}


void create_nop() /* does nothing */
{
  error(DIAGNOSTIC,"Creating command 'nop'");
  add_command(NOP);
  /* thats all folks !*/
}


void create_myend() /* create command 'end' */
{
  error(DIAGNOSTIC,"Creating command 'end'");
  add_command(END);
}


void myend() /* is called at the end of program execution */
{
}


void create_print(char type) /* create command 'print' */
{
  struct command *cmd;

  error(DIAGNOSTIC,"Creating command 'print'");
  cmd=add_command(PRINT);
  cmd->pointer=my_malloc(sizeof(int));
  /* store type of print  */
  cmd->tag=type;
}


void print(struct command *cmd) /* print on screen */
{
  int type;
  struct stackentry *p;
  FILE *str;
  static int last;

  if (currentstream==NULL) str=stdout;
  else str=currentstream;

  type=cmd->tag;
  switch(type) {
  case 'n': 
    fprintf(str,"\n");break;  /* print newline */
  case 'd': 
    if (last=='d')
      fprintf(str," ");   /* print double value */
    p=pop();
    fprintf(str,"%g",p->value);
    break;
  case 's': 
    p=pop();
    fprintf(str,"%s",(char *)p->pointer);
    break;
  }
  fflush(str);
  last=type;
}


void create_myopen(double stream,char *mode) /* create command 'myopen' */
{
  struct command *cmd;

  sprintf(string,"Creating command 'myopen' with mode '%s'",mode);
  error(DIAGNOSTIC,string);
  if (badstream((int)stream)) return;
  cmd=add_command(MYOPEN);
  cmd->args=(int) stream;
  cmd->pointer=my_strdup(mode);
}


void myopen(struct command *cmd) /* open specified file for given name */
{
  FILE *handle;
  int stream;
  char *name;
  char *mode;
  
  mode=(char *)cmd->pointer;
  name=pop()->pointer;
  stream=cmd->args;
  if (streams[stream]!=NULL) {
    error(ERROR,"Stream already in use");
    return;
  }
  handle=fopen(name,mode);
  if (handle==NULL) {
    sprintf(string,"Could not open '%s'",name);
    error(ERROR,string);
    return;
  }
  if (infolevel>=DIAGNOSTIC) {
    sprintf(string,"Opened '%s' with mode '%s'",name,mode);
    error(DIAGNOSTIC,string);
  }
  streams[stream]=handle;
}


void create_myclose(double stream) /* create command 'myclose' */
{
  struct command *cmd;

  error(DIAGNOSTIC,"Creating command 'myclose'");
  if (badstream((int)stream)) return;
  cmd=add_command(MYCLOSE);
  cmd->args=(int) stream;
  return;
}


void myclose(struct command *cmd) /* close the specified stream */
{
  int s;
  
  s=cmd->args;
  if (streams[s]==NULL) {
    sprintf(string,"Stream %d already closed",s);
    error(WARNING,string);
    return;
  }
}


void create_myswitch(double stream) /* create command myswitch */
{
  struct command *cmd;

  if (stream!=0.0 && badstream((int)stream)) return;
  error(DIAGNOSTIC,"Creating command 'myswitch'");
  cmd=add_command(MYSWITCH);
  cmd->args=(int) stream;
}


void myswitch(struct command *cmd) /* switch to specified stream */
{
  int stream;

  stream=cmd->args;
  if (stream==0) 
    currentstream=NULL;
  else  {
    currentstream=streams[stream]; /* switch to stream */
    if (streams[stream]==NULL) {
      sprintf(string,"Stream %d not opened",stream);
      error(ERROR,string);
      return;
    } 
  }
  if (infolevel>=DIAGNOSTIC) {
    sprintf(string,"Switching to stream %d",stream);
    error(DIAGNOSTIC,string);
  }
  return;
}


int badstream(int stream) /* test for valid stream id */
{
  int max;

  max=(9>FOPEN_MAX)?(FOPEN_MAX-3):9;
  if (stream>max || stream<1) {
    sprintf(string,"Can handle only streams from 3 to %d",max);
    error(ERROR,string);
    return TRUE;
  }
  return FALSE;
}


void create_myread(char type) /* create command 'read' */
{
  struct command *cmd;

  error(DIAGNOSTIC,"Creating command 'read'");
  cmd=add_command(MYREAD);
  cmd->tag=type;
}


void myread(struct command *cmd) /* read string or double */
{
  double d;
  int i;
  char c;
  static char *buffer=NULL;
  struct stackentry *s;
  FILE *str;

  if (buffer==NULL) buffer=my_malloc(1000*sizeof(char));
  if (DIAGNOSTIC<=infolevel) error(DIAGNOSTIC,"Waiting for input");
  if (currentstream==NULL) {
    str=stdin;
    printf("%s",inputprompt);}
  else {
    str=currentstream;
    fflush(currentstream);
  }

  i=0;
  while (isspace(c=fgetc(str)) && c!='\n');  /* skip whitespace */
  if (c!='\n') {
    do {
      *(buffer+i)=c;
      i++;
    } while (!isspace(c=fgetc(str)) && i<1000);
  }
  strcpy(inputprompt,(c=='\n')?"?":"");
  *(buffer+i)='\0';

  if (cmd->tag=='s') { /* read string */
    s=push();
    s->type=STRING;
    s->pointer=my_strdup(buffer);}
  else { /* read double */
    s=push();
    s->type=NUMBER;
    if (sscanf(buffer,"%lf",&d)==0)
      s->value=0.0;
    else 
      s->value=d;
  }
}
  

void create_prompt(char *p) /* create command 'prompt' */
{
  struct command *cmd;

  error(DIAGNOSTIC,"Creating command 'prompt'");
  cmd=add_command(PROMPT);
  cmd->pointer=my_strdup(p);
}


void prompt(struct command *cmd) /* set input prompt */
{
  strncpy(inputprompt,cmd->pointer,80);
}


void create_restore(char *label) /* create command 'restore' */
{
  struct command *c;
  
  error(DIAGNOSTIC,"Creating command 'restore'");
  c=add_command(RESTORE);
  c->pointer=my_strdup(label);
}


void restore(struct command *cmd) /* reset data pointer to given label */
{
  struct command *curr;

  if (cmd->type==RESTORE) { /* first time; got to search the label */
    if (*((char *)cmd->pointer)=='\0') {
      error(DIAGNOSTIC,"Restore to first data command");
      cmd->pointer=cmdroot;
      cmd->type=QRESTORE;
      goto found; /* restore to first command */
    }
    curr=cmdroot;
    sprintf(string,"Searching Label '%s'",(char *)cmd->pointer);
    error(DIAGNOSTIC,string);
    while(curr!=cmdhead) {   /* go through all commands */
      if (curr->type==LABEL && !strcmp(curr->pointer,cmd->pointer)) {
        /* found right Label ! */
        sprintf(string,"Restore to label '%s'",(char *)cmd->pointer);
        /* use the address instead of the name next time ! */
        cmd->pointer=curr;
        cmd->type=QRESTORE;
        error(DIAGNOSTIC,string);
        goto found;
      }
      curr=curr->next;
    }
    /* did not found label */
    sprintf(string,"Didn't found label '%s'",(char *)cmd->pointer);
    error(ERROR,string);
    return;
  }
 found:
  datapointer=cmd->pointer;
  return;
}


void create_dbldata(double value)  /* create command dbldata */
{
  struct command *c;

  error(DIAGNOSTIC,"Creating command 'data'");
  c=add_command(DATA);
  c->pointer=my_malloc(sizeof(double));
  *((double *)c->pointer)=value;
  c->tag='d'; /* double value */
}


void create_strdata(char *value)  /* create command strdata */
{
  struct command *c;

  error(DIAGNOSTIC,"Creating command 'data'");
  c=add_command(DATA);
  c->pointer=my_strdup(value);
  c->tag='s'; /* string value */
}


void create_readdata(char type) /* create command readdata */
{
  struct command *cmd;

  error(DIAGNOSTIC,"Creating command 'readdata'");
  cmd=add_command(READDATA);
  cmd->tag=type;
}


void readdata(struct command *cmd) /* read data items */
{
  struct stackentry *read;
  char type;

  type=cmd->tag;
  while(datapointer->type!=DATA) {
    if (datapointer==cmdhead) {
      error(ERROR,"Run out of data items");
      return;
    }
    datapointer=datapointer->next;
  }
  if (type!=datapointer->tag) {
    error(ERROR,"Type of READ and DATA don't match");
    return;
  }
  read=push();
  if (type=='d') { /* read a double value */
    read->type=NUMBER;
    read->value= *((double *)datapointer->pointer);}
  else {
    read->type=STRING;
    read->pointer=my_strdup(datapointer->pointer);
  }
  datapointer=datapointer->next; /* next item */
}


void create_dblrelop(char c) /* create command dblrelop */ 
{
  int type;

  switch(c) {
  case '=': type=EQ;break;
  case '!': type=NE;break;
  case '<': type=LT;break;
  case '{': type=LE;break;
  case '>': type=GT;break;
  case '}': type=GE;break;
  }
  sprintf(string,"Creating command 'dblrelop %c'",c);
  error(DIAGNOSTIC,string);
  add_command(type);
}


void dblrelop(struct command *type)  /* compare topmost double-values */
{
  double a,b,c;
  struct stackentry *result;

  b=pop()->value;
  a=pop()->value;
  switch(current->type) {
  case EQ:c=(a==b);break;
  case NE:c=(a!=b);break;
  case LE:c=(a<=b);break;
  case LT:c=(a<b);break;
  case GE:c=(a>=b);break;
  case GT:c=(a>b);break;
  }
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Comparison resulted in %g",c);
    error(DIAGNOSTIC,string);
  }
  result=push();
  result->value=c;
  result->type=NUMBER;
}    


void create_strrelop(char c) /* create command strrelop */ 
{
  int type;

  switch(c) {
  case '=': type=STREQ;break;
  case '!': type=STRNE;break;
  }
  sprintf(string,"Creating command 'strrelop %c'",c);
  error(DIAGNOSTIC,string);
  add_command(type);
}


void strrelop(struct command *type)  /* compare topmost string-values */
{
  char *a,*b;
  double c;
  struct stackentry *result;

  b=pop()->pointer;
  a=pop()->pointer;
  switch(current->type) {
  case STREQ:c=(strcmp(a,b)==0);break;
  case STRNE:c=(strcmp(a,b)!=0);break;
  }
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Comparison resulted in %g",c);
    error(DIAGNOSTIC,string);
  }
  result=push();
  result->value=c;
  result->type=NUMBER;
}    


void create_boole(char c) /* create command boole */ 
{
  int type;

  switch(c) {
  case '|': type=OR;break;
  case '&': type=AND;break;
  case '!': type=NOT;break;
  }
  sprintf(string,"Creating command 'boole %c'",c);
  error(DIAGNOSTIC,string);
  add_command(type);
}


void boole(struct command *type)  /* perform and/or/not */
{
  int a,b,c;
  struct stackentry *result;

  a=pop()->value;
  if (current->type==NOT) 
    c=!a;
  else {
    b=pop()->value;
    if (current->type==AND)
      c=a&&b;
    else
      c=a||b;
  }
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Boolean operation resulted in %d",c);
    error(DIAGNOSTIC,string);
  }
  result=push();
  result->value=c;
  result->type=NUMBER;
}    


void create_decide() /* creates command decide */
{
  error(DIAGNOSTIC,"Creating command 'decide'");
  add_command(DECIDE);
}
    

void decide() /*  skips next command, if 0 on stack */
{
  struct stackentry *a;

  a=pop();
  if (a->type!=NUMBER) {
    error(FATAL,"Dont find number to decide");
    return;
  }
  if (a->value!=0) current=current->next; /* skip one command */
}


void create_call(char *symbol) /* creates the command 'call' */ 
{
  struct command *cmd;
  struct symbol *f,*a;
  struct array *ar;
  char *args;
  int dim,i=0;

  a=get_sym(symbol,ARRAY,FALSE);
  do symbol[i]=tolower(symbol[i]); while(symbol[++i]!='\0');
  f=get_sym(symbol,FUNC,FALSE);
  if (f==NULL && a==NULL) {
    sprintf(string,
            "Symbol '%s' neither known as function nor as array",symbol);
    error(ERROR,string);
    return;
  }
  if (f!=NULL && a!=NULL) {
    error(ERROR,"Symbol known both as function and array");
    return;
  }
  if (f!=NULL) { /* it is a function call */
    if (infolevel>=DIAGNOSTIC) {
      sprintf(string,"Creating command 'callfun %s'",symbol);
      error(DIAGNOSTIC,string);
    }
    cmd=add_command(CALLFUN);
    /* get type of arguments from stack */
    args=pop()->pointer;
    /* compare with required number; '-' means: accept anything */
    if (!(*(f->args)=='-'           
          || !strcmp(f->args,args)   /* exact match of arguments */
          /* or arguments match, and accept lval as well as rval */
          || (*(f->args)=='b' && !strcmp(f->args+1,args+1)))) {
      sprintf(string,"Arguments don't match requirement for function '%s'",symbol);
      error(ERROR,string);
    }
    cmd->args=strlen(args)-1;
    cmd->tag= *args; /* called as l- or rvalue ? */
    cmd->pointer=f->pointer;}
  else {   /* it is an array */
    if (infolevel>=DIAGNOSTIC) {
      sprintf(string,"Creating command 'callarr %s'",symbol);
      error(DIAGNOSTIC,string);
    }
    cmd=add_command(CALLARR);
    cmd->pointer=a->pointer;
    args=pop()->pointer;
    cmd->tag= *args;          /* called as l- or rvalue ? */
    dim=strlen(args)-1;
    ar=a->pointer;
    if (dim!=ar->dimension || strchr(args,'s')!=NULL) {
      error(ERROR,"Improper array boundaries");
      return;
    }
  }
}


void add_function(char *name,char *args,void fun(int,char))
/* add a function with arguments (type given in args) to symbol-list */  
{
  struct symbol *s;

  s=get_sym(name,FUNC,TRUE);
  s->pointer=(void *)fun;
  s->args=my_strdup(args);
}
  

void callfun(struct command *current) /* call a function */
{
  /* call the function ! */
  ((void (*)(int,char))current->pointer)(current->args,current->tag); 
}


void callarr(struct command *current) /* call an array */
{
  struct array *ar;
  struct stackentry *s;
  void *p;
  char **str;
  double *dbl;
  int i,j,bnd,dim,index,lval;
  char type;

  ar=(struct array *)current->pointer;
  dim=ar->dimension;
  type=ar->type;
  lval=(current->tag=='l');
  if (lval) s=pop();  /* pop rhs of assignment */
  index=0;
  for(i=0;i<dim;i++) {
    bnd=(ar->bounds[i]);
    index*=bnd;
    j=pop()->value;
    if (j<0 || j>=bnd) {
      error(ERROR,"Index values out of range");
      return;
    }
    index+=j;
  }
  if (!lval) s=push();
  p=ar->pointer;
  if (type=='s') { /* string array */
    str=((char **)p+index);
    if (lval) {
      if (*str!=NULL) free(*str);
      *str=my_strdup(s->pointer);}
    else {
      s->pointer=my_strdup(*str);
      s->type=STRING;}}
  else { /* double array */
    dbl=(double *)p+index;
    if (lval)
      *dbl=s->value;
    else {
      s->value= *dbl;
      s->type=NUMBER;
    }
  }
}


void create_dim(char *name,char type) /* create command 'dim' */
/* type can be 's'=string or 'd'=double Array */
{ 
  struct command *cmd;
  struct symbol *s;
  struct array *ar;


  s=get_sym(name,FUNC,FALSE);
  if (s!=NULL) {
    error(ERROR,"This Arrayname is known as a function");
    return;
  }
  if (infolevel>=DIAGNOSTIC) {
    sprintf(string,"Creating command 'dim %s' with dimension %d",
            name,dimcount);
    error(DIAGNOSTIC,string);
  }
  cmd=add_command(DIM);
  s=get_sym(name,ARRAY,TRUE); /* create array */
  ar=my_malloc(sizeof(struct array));
  cmd->pointer=ar;
  s->pointer=ar;
  ar->type=type;
  ar->dimed=FALSE;
  ar->dimension=dimcount;
  if (dimcount>10) {
    error(ERROR,"Dimension larger than 10");
    return;
  }
}


void dim(struct command *cmd) /* get room for array */
{
  struct array *ar;
  struct stackentry *s;
  char *nul,**str;
  double *dbl;
  int total,size,i;
  
  ar=(struct array *)cmd->pointer;
  if (ar->dimed) {
    error(ERROR,"Array has been dimed already");
    return;
  }
  total=1; /* count total amount of memory */
  for(i=0;i<ar->dimension;i++) {
    s=pop();
    if (s->type!=NUMBER) {
      error(ERROR,"Improper index in dim statement");
      return;
    }
    size=(int) s->value;
    if (size<=0) {
      error(ERROR,"One bound is less or equal zero");
      return;
    }
    size++; /* allow for zero-index-element */
    (ar->bounds)[i]=size;
    total*=size;
  }
  if (infolevel>=DIAGNOSTIC) {
    sprintf(string,"Getting memory for %d elements",total);
    error(DIAGNOSTIC,string);
  }
  ar->total=total;
  if (ar->type=='s')         /* it is a string array */
    ar->pointer=my_malloc(total*sizeof(char *));
  else
    ar->pointer=my_malloc(total*sizeof(double));
  if (ar->pointer==NULL) {
    error(ERROR,"Could not get enough memory for dim");
    return;
  }
  /* initialize Array */
  if (ar->type=='s') { 
    str=ar->pointer;
    for(i=0;i<total;i++) {
      nul=my_malloc(sizeof(char));
      *nul='\0';
      *(str+i)=nul;}}
  else {
    dbl=ar->pointer;
    for(i=0;i<total;i++) *(dbl+i)=0.0;
  }
  ar->dimed=TRUE;
}


void mysin(int n,char s) /* wraparound for 'sin'-function */
{
  struct stackentry *a;

  a=stackhead->prev;;
  a->value=sin(a->value);   
}


void myasin(int n,char s) /* wraparound for 'asin'-function */
{
  struct stackentry *a;

  a=stackhead->prev;;
  a->value=asin(a->value);   
}


void mycos(int n,char s) /* wraparound for 'cos'-function */
{
  struct stackentry *a;

  a=stackhead->prev;;
  a->value=cos(a->value);   
}


void myacos(int n,char s) /* wraparound for 'acos'-function */
{
  struct stackentry *a;

  a=stackhead->prev;;
  a->value=acos(a->value);   
}


void mytan(int n,char s) /* wraparound for 'tan'-function */
{
  struct stackentry *a;

  a=stackhead->prev;;
  a->value=tan(a->value);   
}


void myatan(int n,char s) /* wraparound for 'atan'-function */
{
  struct stackentry *c;
  double a,b;

  if (s=='l') 
    error(ERROR,"'atan' can only be called as an rvalue");
  switch(n) {
  case 1: a=pop()->value;b=1.0;break;
  case 2: a=pop()->value;b=pop()->value;break;
  default:
    error(ERROR,"Expecting one or two arguments for function 'atan'");
    return;
  }
  c=push();
  c->value=atan2(a,b);
}


void mylog(int n,char s) /* wraparound for 'log'-function */
{
  struct stackentry *a;

  a=stackhead->prev;;
  a->value=log(a->value);   
}


void myexp(int n,char s) /* wraparound for 'exp'-function */
{
  struct stackentry *a;

  a=stackhead->prev;;
  a->value=exp(a->value);   
}


void mysqrt(int n,char s) /* wraparound for 'sqrt'-function */
{
  struct stackentry *a;

  a=stackhead->prev;;
  a->value=sqrt(a->value);   
}


void myint(int n,char s) /* wraparound for 'int'-function */
{
  struct stackentry *a;

  a=stackhead->prev;;
  a->value=(int) a->value;   
}

void myfrac(int n,char s) /* wraparound for 'frac'-function */
{
  struct stackentry *a;

  a=stackhead->prev;;
  a->value=a->value - (int) a->value;   
}


void myleft(int n,char s) /* 'left$'  string--function */
{
  struct stackentry *result;
  char *str,*part;
  int len;

  len=pop()->value;  /* numeric argument */
  str=pop()->pointer;  /* string--argument */
  part=fromto(str,1,len);
  result=push();
  result->type=STRING;
  result->pointer=part;
}


void myright(int n,char s) /* 'right$'  string--function */
{
  struct stackentry *result;
  char *str,*part;
  int len;

  len=pop()->value;  /* numeric argument */
  str=pop()->pointer;  /* string--argument */
  part=fromto(str,-len,-1);
  result=push();
  result->type=STRING;
  result->pointer=part;
}


void mymid(int n,char s) /* 'mid$'  string--function */
{
  struct stackentry *result;
  char *str,*part;
  int len,start;

  len=pop()->value;  /* length */
  start=pop()->value;  /* start */
  str=pop()->pointer;  /* string--argument */
  part=fromto(str,start,start+len-1);
  result=push();
  result->type=STRING;
  result->pointer=part;
}


char *fromto(char *str,int from,int to) /* gives back portion of string */
/* from and to can be in the range 1...strlen(str) */
{
  int len,i;
  char *part;
  
  len=strlen(str);
  /* negative values count from the right */
  if (from<0) from=len+1+from;
  if (to<0) to=len+1+to;
  if (to<from) to=from;  /* from must be less than to */
  if (from>len) from=len; /* to and from must be less than len */
  if (to>len) to=len;
  part=my_malloc(sizeof(char)*(to-from+2)); /* characters and '/0' */
  for(i=from;i<=to;i++) part[i-from]=str[i-1]; /* copy */
  part[i-from]='\0';
  return part;
}


void mylen(int n,char s) /* 'len' string function (returns length of string) */
{
  struct stackentry *a,*b;
  int len;

  a=pop();
  len=strlen(a->pointer);
  b=push();
  b->type=NUMBER;
  b->value=(double) len;
}


void myval(int n,char s) /* 'val' function: transform string into number */
{
  struct stackentry *a,*b;
  double d;
  int i;

  a=pop();
  i=sscanf((char *) a->pointer,"%lf",&d);
  b=push();
  b->type=NUMBER;
  b->value=(i==1)?d:0.0; 
}


void mystr(int n,char s) /* 'str' function: transform number into string */
{
  struct stackentry *b;
  double d;

  d=pop()->value;
  sprintf(string,"%g",d);
  b=push();
  b->type=STRING;
  b->pointer=my_strdup(string);
}

void noXwarn() /* issue a warning if Program is compiled without X */
{

  Xwarncount++;
  if (Xwarncount<10) 
    error(WARNING,"No Grafic available");
  else if (Xwarncount==10)
    error(WARNING,"No further warnings for missing Grafics ...");
}


void create_openwin(int num) /* create Command 'openwin' */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'openwin'");
  cmd=add_command(OPENWIN);
  cmd->args=num;
}


void openwin(struct command *cmd) /* open a Window */
{
  static first=TRUE; /* flag to decide if initialization is necessary */
#ifdef UNIX_X11
  static int screen; /* Number of Screen on Display */
  static XGCValues xgcvalues; /* Values for Graphics Context */
  static unsigned long f_colour,b_colour; /* colors */
  static XEvent event; /* what has happened ? */
  static XColor asked,got; /* color is complex ... */
  static Colormap map; /* my color map */
  static XSizeHints sizehints; /* hints for window manager */
  XFontStruct *myfont; /* properties of default font */
  static int x,y,w,h;
#endif

  if (winopened) {
    error(WARNING,"Window already open");
    return;
  }

  if (cmd->args==3) {
    fontheight=pop()->value;
    get_sym("yabfontheight",NUMBER,FALSE)->value=fontheight;
  }
  if (cmd->args==2 || cmd->args==3) {
    winheight=(unsigned long) pop()->value;
    get_sym("yabwinheight",NUMBER,FALSE)->value=winheight;
    winwidth=(unsigned long) pop()->value;
    get_sym("yabwinwidth",NUMBER,FALSE)->value=winwidth;
  }

#ifdef UNIX_X11  
  if (first) {
    /* get display */
    display=XOpenDisplay(displayname);
    if (display==NULL) {
      error(ERROR,"Can't open Display");
      return;
    }
    
    /* get screen */
    screen=DefaultScreen(display);
    root=RootWindow(display,screen);
  }

  /* care for colors */
  if (first) {
    if (DefaultDepth(display,screen)==1) {  /* BW-Screen ? */
      f_colour=BlackPixel(display,screen);
      b_colour=WhitePixel(display,screen); }
    else {
      map=DefaultColormap(display,screen);
      if (foreground==NULL) 
        foreground=XGetDefault(display,BASIC_NAME,"foreground");
      if (foreground==NULL) foreground="black";
      
      if (!XAllocNamedColor(display,map,foreground,&got,&asked)) {
        sprintf(string,"could not get fg-color '%s', trying 'black' instead",
                foreground);
        error(WARNING,string);
        if (!XAllocNamedColor(display,map,"black",&got,&asked)) {
          error(ERROR,"could not get it");
          return;
        }
      }
      f_colour=got.pixel;
      
      if (background==NULL) 
        background=XGetDefault(display,BASIC_NAME,"background");
      if (background==NULL) background="white";
      if (!XAllocNamedColor(display,map,background, &got,&asked)) {
        sprintf(string,"could not get bg-color '%s', trying 'white' instead",
                background);
        error(WARNING,string);
        if (!XAllocNamedColor(display,map,"white", &got,&asked)) {
          error(ERROR,"could not get it");
          return;
        }
      }
      b_colour=got.pixel;
    }
  }
  
  if (first) {
    /* get size hints */
    if (geometry==NULL) geometry=XGetDefault(display,BASIC_NAME,"geometry");
    x=y=0;
    XParseGeometry(geometry,&x,&y,&w,&h);
    sizehints.x=x;
    sizehints.y=y;
    sizehints.flags=USPosition;
  }

  /* create the window */
  window=XCreateSimpleWindow(display,root,x,y,winwidth,winheight,0,0,b_colour);
  if (window==None) {
    error(ERROR,"Could not create window");
    return;
  }

  /* name for the window */
  XStoreName(display,window,BASIC_NAME);

  /* set size hints */
  XSetWMNormalHints(display,window,&sizehints);

  /* display it */
  XMapWindow(display,window);

  /* wait for exposure */
  XSelectInput (display,window,ExposureMask); 
  XFlush(display);
  XNextEvent(display,&event);

  if (first) {    
    /* get font height */
    if (font==NULL) font=XGetDefault(display,BASIC_NAME,"font");
    if (font==NULL) font="fixed";
    myfont=XLoadQueryFont(display,font);
    if (myfont==NULL) {
      sprintf(string,"could not load font '%s', trying 'fixed' instead",font);
      error(WARNING,string);
      myfont=XLoadQueryFont(display,"fixed");
      if (myfont==NULL) {
        error(ERROR,"could not get it");
        return;
      }
    }
    xgcvalues.font=myfont->fid;
    fontheight=myfont->ascent;
    get_sym("yabfontheight",NUMBER,FALSE)->value=fontheight;
    
    /* create graphics context, accept defaults ... */
    xgcvalues.foreground=f_colour;
    xgcvalues.background=b_colour;
    gc=XCreateGC(display,window,GCForeground | GCBackground | GCFont,
		 &xgcvalues);
  }

  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Opened window, fg=%s, bg=%s",foreground,background);
    error(DIAGNOSTIC,string);
  }
  first=FALSE;

#elif defined(WIN95)
  if (winopened) {
    error(WARNING,"Window already open");
    return;
  }

  if (first) {
    int n;
    int f; /* int-value of font */
    char *family; /* font family */
    
    /* choose font */
    if (!font) font=getreg("font");
    if (!font) font="swiss30";
    f=FF_SWISS;
    fontheight=30;    
    get_sym("yabfontheight",NUMBER,FALSE)->value=fontheight;
    
    family=my_strdup(font);
    for(n=0;*(family+n)!='\0' && !isdigit(*(family+n));n++)
      *(family+n)=tolower(*(family+n));
    if (isdigit(*(family+n))) sscanf(family+n,"%d",&fontheight);
    *(family+n)='\0';
    
    if (!strcmp("decorative",family)) f=FF_DECORATIVE;
    if (!strcmp("dontcare",family)) f=FF_DONTCARE;
    if (!strcmp("modern",family)) f=FF_MODERN;
    if (!strcmp("roman",family)) f=FF_ROMAN;
    if (!strcmp("script",family)) f=FF_SCRIPT;
    if (*family=='\0' || !strcmp("swiss",family)) f=FF_SWISS;
    
    logfont.lfHeight=fontheight;
    logfont.lfWidth=0;
    logfont.lfEscapement=0;
    logfont.lfOrientation=0;
    logfont.lfWeight=FW_DONTCARE;
    logfont.lfItalic=FALSE;
    logfont.lfUnderline=FALSE;
    logfont.lfStrikeOut=FALSE;
    logfont.lfCharSet=DEFAULT_CHARSET;
    logfont.lfOutPrecision=OUT_DEFAULT_PRECIS;
    logfont.lfClipPrecision=CLIP_DEFAULT_PRECIS;
    logfont.lfQuality=DEFAULT_QUALITY;
    logfont.lfPitchAndFamily=DEFAULT_PITCH | f;
    logfont.lfFaceName[0]='\0';
    myfont=CreateFontIndirect(&logfont);

    if (myfont==NULL) {
      sprintf(string,"Could not create font '%s' for screen",font);
      error(ERROR,string);
      return;
    }
  }
  thread.winevent=CreateEvent(NULL,FALSE,FALSE,"winevent");
  /* create thread to care for window */
  thread.handle=CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)winthread,
			     0,0,(LPDWORD)&thread.id);
  if (thread.handle==NULL) {
    error(ERROR,"can't create thread for window");
    return;
  }
  
  WaitForSingleObject(thread.winevent,INFINITE);
  DeleteObject(thread.winevent);

  first=FALSE;
#else  /* ... no X Grafic */
  noXwarn();
#endif
  winopened=TRUE;
  calc_psscale();
}


void calc_psscale()  /* calculate scale-factor for postscript */
{
  if ((float)winwidth/winheight>(float)18/25)
    psscale=18*0.39*72/winwidth;
  else
    psscale=25*0.39*72/winheight;
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"calculating psscale to %g",psscale);
    error(DIAGNOSTIC,string);
  }
}


void create_dot() /* create Command 'dot' */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'dot'");
  cmd=add_command(DOT);
}


void dot() /* draw a dot */
{
  unsigned long x,y;

  y=(unsigned long) pop()->value;
  x=(unsigned long) pop()->value;
  if (!winopened && !printerfile) {
    error(ERROR,"Got no window to draw");
    return;
  }
#if defined(UNIX_X11) || defined(WIN95)
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Drawing a dot at %d,%d",(int)x,(int)y);
    error(DIAGNOSTIC,string);
  }
#endif
  if (winopened) {
#ifdef UNIX_X11
    XDrawPoint(display,window,gc,x,y);
    XFlush(display);
#elif defined(WIN95)
    startdraw();
    SetPixelV(devcon,x,y,RGB(0,0,0));
    if (printer) {
      MoveToEx(printer,x*prnscale+prnoff,y*prnscale+prnoff,NULL);
      LineTo(printer,x*prnscale+prnoff,y*prnscale+prnoff);
    }
    ReleaseDC(window,devcon);
#else  /* ... no X Grafic */
    noXwarn();
#endif
  }
  if (printerfile) {
    fprintf(printerfile,"%i %i D\n",
            (int)((x-0.5)*psscale),(int)((winheight-(y+0.5))*psscale));
  }
}


void create_line() /* create Command 'line' */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'line'");
  cmd=add_command(LINE);
}


void line() /* draw a line */
{
  unsigned long x1,y1,x2,y2;
  
  y2=(unsigned long) pop()->value;
  x2=(unsigned long) pop()->value;
  y1=(unsigned long) pop()->value;
  x1=(unsigned long) pop()->value;
  if (!winopened && !printerfile) {
    error(ERROR,"Got no window to draw");
    return;
  }
  
#if defined(UNIX_X11) || defined(WIN95)
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Drawing a line from %d,%d to %d,%d",
	    (int)x1,(int)y1,(int)x2,(int)y2);
    error(DIAGNOSTIC,string);
  }
#endif
  if (winopened) {
#ifdef UNIX_X11
    XDrawLine(display,window,gc,x1,y1,x2,y2);
    XFlush(display);
#elif WIN95
    startdraw();
    MoveToEx(devcon,x1,y1,NULL);
    LineTo(devcon,x2,y2);
    if (printer) {
      MoveToEx(printer,x1*prnscale+prnoff,y1*prnscale+prnoff,NULL);
      LineTo(printer,x2*prnscale+prnoff,y2*prnscale+prnoff);
    }
    ReleaseDC(window,devcon);
#else  /* ... no X Grafic */
    noXwarn();
#endif
  }
#ifndef WIN95
  if(printerfile) {
    fprintf(printerfile,"N\n");
    fprintf(printerfile,"%i %i M\n",
            (int)(x1*psscale),(int)((winheight-y1)*psscale));
    fprintf(printerfile,"%i %i L S\n",
            (int)(x2*psscale),(int)((winheight-y2)*psscale));
    fflush(printerfile);
  }
#endif
}


void create_circle() /* create Command 'circle' */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'circle'");
  cmd=add_command(CIRCLE);
}


void circle() /* draw a circle */
{
  int x,y,r;
  
  r=pop()->value;
  y=pop()->value;
  x=pop()->value;
  if (!winopened && !printerfile) {
    error(ERROR,"Got no window to draw");
    return;
  }

#if defined(UNIX_X11) || defined(WIN95)
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Drawing a circle around %d,%d with radius %d",x,y,r);
    error(DIAGNOSTIC,string);
  }
#endif
  if (winopened) {
#ifdef UNIX_X11
    XDrawArc(display,window,gc,x-r,y-r,2*r,2*r,0*64,360*64);
    XFlush(display);
#elif defined(WIN95)
    startdraw();
    Arc(devcon,x-r,y-r,x+r,y+r,0,0,0,0);
    if (printer) {
      Arc(printer,(x-r)*prnscale+prnoff,(y-r)*prnscale+prnoff,
	          (x+r)*prnscale+prnoff,(y+r)*prnscale+prnoff,0,0,0,0); 
    }
    ReleaseDC(window,devcon);
#else  /* ... no X Grafic */
    noXwarn();
#endif
  }
#ifndef WIN95
  if(printerfile) {
    fprintf(printerfile,"N\n");
    fprintf(printerfile,"%i %i %i C S\n",
            (int)(x*psscale),(int)((winheight-y)*psscale),(int)(r*psscale));
    fflush(printerfile);
  }
#endif
}


void create_text(int str_first) /* create Command 'text' */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'text'");
  cmd=add_command(TEXT);
  cmd->tag=str_first ? 'n' : 's';
}


void text(struct command *cmd) /* write a text */
{
  unsigned long x,y;
  char *text;

  if (cmd->tag=='s') text=(char *)pop()->pointer;
  y=(unsigned long) pop()->value;
  x=(unsigned long) pop()->value;
  if (cmd->tag!='s') text=(char *)pop()->pointer;
#if defined(UNIX_X11) || defined(WIN95)
  if (!winopened && !printerfile) {
    error(ERROR,"Got no window to draw");
    return;
  }
#endif
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Writing Text '%s' at %d,%d",text,(int)x,(int)y);
    error(DIAGNOSTIC,string);
  }
  if (winopened) {
#ifdef UNIX_X11
    XDrawString(display,window,gc,x,y,text,strlen(text));
    XFlush(display);
#elif defined(WIN95)
    startdraw();
    SelectObject(devcon,myfont);
    SetBkMode(devcon,TRANSPARENT);
    SetTextAlign(devcon,TA_LEFT | TA_BOTTOM);
    TextOut(devcon,x,y,text,strlen(text));
    ReleaseDC(window,devcon);
    if (printer) {
      SelectObject(printer,printerfont);
      SetBkMode(printer,TRANSPARENT);
      SetTextAlign(printer,TA_LEFT | TA_BOTTOM);
      TextOut(printer,x*prnscale+prnoff,y*prnscale+prnoff,text,strlen(text));
    }
#else  /* ... no X Grafic */
    noXwarn();
#endif
  }
#ifndef WIN95
  if (printerfile) {
    fprintf(printerfile,"%i %i M\n",(int)(x*psscale),(int)((winheight-y)*psscale));
    fprintf(printerfile,"(%s) show\n",text);
  }
#endif
}


void create_closewin() /* create Command 'closewin' */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'closewin'");
  cmd=add_command(CLOSEWIN);
}


void closewin() /* close the window */
{
  if (!winopened) {
    error(WARNING,"Got no window to close");
    return;
  }
  winopened=FALSE;
#ifdef UNIX_X11
  XDestroyWindow(display,window); 
  XFlush(display);
#elif defined(WIN95)
  error(DIAGNOSTIC,"Terminating window-thread");
  winopened=FALSE;
  PostThreadMessage(thread.id,WM_QUIT,0,0);
#else  /* ... no X Grafic */
  noXwarn();
#endif
}


void create_clearwin() /* create Command 'clearwin' */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'clearwin'");
  cmd=add_command(CLEARWIN);
}


void clearwin() /* clear the window */
{
  if (!winopened && !printerfile) {
    error(WARNING,"Got no window to clear");
    return;
  }
  if (winopened) {
#ifdef UNIX_X11
    XClearWindow(display,window); 
#elif defined(WIN95)
    RECT interior;
    
    startdraw();
    GetClientRect(window,&interior);
    FillRect(devcon,&interior,(HBRUSH) COLOR_WINDOW);
    if (printer) {
      EndPage(printer);
      StartPage(printer);
    }
#else  /* ... no X Grafic */
    noXwarn();
#endif
  }
  if (printerfile) {
    fprintf(printerfile,"showpage\n");
    fflush(printerfile);
  }
}


void create_wait() /* create Command 'wait' */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'wait'");
  cmd=add_command(WAIT);
}


void wait() /* wait given number of seconds */
{
  double delay;

#ifdef UNIX
  struct itimerval new;
#endif

#ifdef WIN95
  MSG msg;
  int timerid;
#endif
  
#ifdef PLAIN
  time_t start,now;
#endif
  
  delay=fabs(pop()->value);
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Wait %g second(s)",delay);
    error(DIAGNOSTIC,string);
  }
#ifdef UNIX
  
  new.it_interval.tv_sec=0.0;
  new.it_interval.tv_usec=0.0;
  new.it_value.tv_sec=floor(delay);
  new.it_value.tv_usec=1000000*(delay-floor(delay));
  setitimer(ITIMER_REAL,&new,NULL);
  signal(SIGALRM,signal_handler);
  pause();
#elif defined(WIN95)
  timerid=SetTimer(NULL,0,delay*1000,(TIMERPROC) NULL);
  GetMessage((LPMSG)&msg,NULL,WM_TIMER,WM_TIMER);
  KillTimer(NULL,timerid);
#elif defined(PLAIN)
  time(&start);
  do {
    time(&now);
  } while(difftime(now,start)<delay);
#endif
}


void create_bell() /* create Command 'bell' */
{
  struct command *cmd;
  
  error(DIAGNOSTIC,"Creating command 'bell'");
  cmd=add_command(BELL);
}


void bell() /* ring ascii bell */
{
  error(DIAGNOSTIC,"ringing bell");
  printf("\007");
  fflush(stdout);
}


void pushlabel() /* generate goto and push label on stack */
{
  char *st;
  struct stackentry *en;
  
  st=(char *) my_malloc(sizeof(char)*20);
  sprintf(st,"***%d",labelcount);
  sprintf(string,"Generating 'goto %s', pushing the label",st);
  error(DIAGNOSTIC,string);
  labelcount++;
  create_goto(st);
  en=push();
  en->type=LBL;
  en->pointer=st;
}
  

void poplabel() /* pops a label and generates the matching command */
{
  struct stackentry *en;
  
  en=pop();   /* get name of label */
  if (en->type!=LBL) {
    error(FATAL,"Not a goto on stack");
    return;
  }
  create_label(en->pointer);  /* and create it */
}


void pushgoto() /* generate label and push goto on stack */
{
  char *st;
  struct stackentry *en;
  
  st=(char *) my_malloc(sizeof(char)*20);
  sprintf(st,"***%d",labelcount);
  sprintf(string,"Generating 'label %s', pushing the goto",st);
  error(DIAGNOSTIC,string);
  labelcount++;
  create_label(st);
  en=push();
  en->type=GTO;
  en->pointer=st;
}
  

void popgoto() /* pops a goto and generates the matching command */
{
  struct stackentry *en;
  
  en=pop();   /* get name of goto */
  if (en->type!=GTO) {
    error(FATAL,"Not a goto on stack");
    return;
  }
  create_goto(en->pointer);  /* and create it */
}


void pushletter(char *s) 
{
  /* push letter ('d' for double or 's' for string) 
     onto stack; will be used to determine, if
     a function gets its correct arguments */

  struct stackentry *p;
  
  p=push();
  p->pointer=my_strdup(s);
  p->type=STRING;
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Pushing letter '%s'",(char *)p->pointer);
    error(DIAGNOSTIC,string);
  }
}


void swap() /*swap topmost elements on stack */
{
  struct stackentry *a,*b;
  
  error(DIAGNOSTIC,"Swapping on stack");
  if ((a=stackhead->prev)==NULL || (b=a->prev)==NULL) {
    error(ERROR,"Nothing to swap on stack !");
    return;
  }
  a->prev=b->prev;b->next=a->next;   /* just swap the pointers */
  a->next=b;b->prev=a;
  stackhead->prev=b;
  if (b==stackroot) stackroot=a;  /* treat root special */
  else (a->prev)->next=a;
}


struct stackentry *push() 
/* push element on stack and enlarge it*/
{
  struct stackentry *new;
  
  if (stackhead->next==NULL) { /* no next element */
    /* create new element */
    new=(struct stackentry *)my_malloc(sizeof(struct stackentry)); 
    /* and initialize it */
    new->next=NULL;  
    new->value=0.0;
    new->type=FREE;
    new->prev=stackhead;
    new->pointer=NULL;
    stackhead->next=new;
  }
  stackhead=stackhead->next; /* advance head */
  /* any content is set free */
  if ((stackhead->prev)->pointer!=NULL && (stackhead->prev)->type==STRING) 
    free((stackhead->prev)->pointer);
  (stackhead->prev)->pointer=NULL;
  return stackhead->prev;
}
    

struct stackentry *pop()
/* pops element to memory and looks for pop-error */
{
  /* test if there is something on the stack */
  if (stackhead==stackroot) {
    error(FATAL,"Popped too much.");
    return stackhead;
  }
  stackhead=stackhead->prev; /* move down in stack */
  if (stackhead->type==FREE) 
    error(WARNING,"Popped element without content.");
  return stackhead;  /* this is your value; use it quickly ! */
}

    
struct command *add_command(int type) 
/* get room for new command, and make a link from old one */
{
  struct command *new,*old;

  cmdhead->type=type;  /* store command */
  cmdhead->line=yylineno;
  commandcount++;
  cmdhead->pointer=NULL;  /* no data yet */ 
  /* no next element, so, create it: */
  new=(struct command *)my_malloc(sizeof(struct command)); 
  /* and initialize */
  new->next=NULL;
  new->pointer=NULL;
  cmdhead->next=new;
  old=cmdhead;
  cmdhead=cmdhead->next;
  return old;
}


void parse_arguments(int argc,char *argv[])
     /* parse arguments from the command line */
{
  int ar;
  FILE *inputfile;
  char *option;
  char info;
  int opened=FALSE;

  for(ar=1;ar<argc;ar++) {
    option=argv[ar];
    if (!strcmp("-help",option) || !strcmp("--help",option) ||
        !strcmp("-h",option)    || !strcmp("-?",option))
      goto usage;
    else if (!strcmp("-i",option) || !strcmp("-info",option) ||
             !strcmp("-infolevel",option)) {
      ar++;
      info=tolower(*argv[ar]);
      switch(info) {
      case 'd':infolevel=DIAGNOSTIC;break;
      case 'n':infolevel=NOTE;break;
      case 'w':infolevel=WARNING;break;
      case 'e':infolevel=ERROR;break;
      case 'f':infolevel=FATAL;break;
      default:
        fprintf(stderr,"There's  no infolevel '%s'.\n",argv[ar]);
        goto usage;
      }
    }
    else if (!strcmp("-fg",option)) {   
      ar++;
      foreground=my_strdup(argv[ar]);
    }
    else if (!strcmp("-bg",option)) {           
      ar++;
      background=my_strdup(argv[ar]);   
    }
    else if (!strcmp("-geometry",option)) {             
      ar++;
      geometry=my_strdup(argv[ar]);     
    }
    else if (!strcmp("-display",option)) {              
      ar++;
      displayname=my_strdup(argv[ar]);
    }
    else if (!strcmp("-font",option)) {         
      ar++;
      font=my_strdup(argv[ar]);
    }
    else if (*option=='-') {
      fprintf(stderr,"Don't know option '%s'.\n",option);
      goto usage;
    }
    else if (!opened) { /* not an option */
      inputfile=fopen(argv[ar],"r");
      if (inputfile==NULL) {
        fprintf(stderr,"Could not open '%s'.\n",argv[ar]);
#ifdef WIN95
        win_shutdown();
#endif
        exit(TRUE);}
      else
        opened=TRUE;
    }
    else {    /* two filename arguments */
      fprintf(stderr,"Can handle only one file.\n");
      goto usage;
    }
  }
  
  interactive=FALSE;
  if (!opened) {
    interactive=TRUE;
    inputfile=stdin;
  }
    
  /* open a flex buffer for the file */
  switch_to_my_file(inputfile);
  return;

 usage: /* print a short usage message and then exit */
  fprintf(stderr,"\n"BASIC_NAME" "BASIC_VERSION" , last change on "DOLC", subject to GNU copyleft.\n");
  fprintf(stderr,"Usage:\n");
  fprintf(stderr,"                "BASIC_NAME" [options] [filename]\n");
  fprintf(stderr,"\n  options:\n");
  fprintf(stderr,"    -i [d|n|w|e|f]  :  set infolevel to: diagnostic,note,warning(default),\n                       error or fatal respectively.\n");
  fprintf(stderr,"    -?,-help,--help :  issue this message.\n");
  fprintf(stderr,"    -geometry       :  e.g. 10+20 to position window at x=10,y=20.\n");
#ifdef UNIX_X11
  fprintf(stderr,"    -fg,-bg         :  colors for grafic.\n");
  fprintf(stderr,"    -display        :  Screen, where window will be displayed.\n");
  fprintf(stderr,"    -font           :  Font for grafics\n");
#endif
#ifdef WIN95
  fprintf(stderr,"    -font           :  Font for grafics, specify style and size, e.g swiss10\n");
  fprintf(stderr,"                       style=[decorative|dontcare|modern|roman|script|swiss]\n");
#endif  
  fprintf(stderr,"\n  filename: File to read basic--code from. If "BASIC_NAME" is called\n");
  fprintf(stderr,"            without any filename, it will read and execute a program\n");
  fprintf(stderr,"            from STDIN and then exit.\n\n");
#ifdef UNIX
  fprintf(stderr,"For further information on "BASIC_NAME" see the files \""BASIC_NAME".html\" or \""BASIC_NAME".txt\".\n");
  fprintf(stderr,"NOTE: This version of "BASIC_NAME" has been compiled without grafics.\n");
#endif
#ifdef WIN95
  fprintf(stderr,"NOTE: This is the WIN95 version of "BASIC_NAME".\n");
  win_shutdown();
#else
  fprintf(stderr,"\n");
#endif
  exit(FALSE);
}


void chop_command(char *command,int *argc,char ***argv)
     /* chop the WIN95-commandline into seperate strings */
{
  int i,j,count;
  int quote;
  char c,last;
  char *curr;
  char **list;

  /* count, how many arguments */
  count=i=0;
  last=' ';
  quote=FALSE;
  while((c=*(command+i))!='\0') {
    if (!quote && c!=' ' && last==' ') count++;
    if (c=='\"') quote=!quote;
    last=c;
    i++;
  }

  /* fill BASIC_NAME into argv[0] */
  *argv=malloc((count+1)*sizeof(char *));
  list=*argv;
  *argc=count+1;
  *list=malloc(sizeof(char)*strlen(BASIC_NAME));
  strncpy(*list,BASIC_NAME,strlen(BASIC_NAME));

  /* fill in other strings */
  i=0;
  count=1;
  last=' ';
  quote=FALSE;
  do {
    c=*(command+i);
    if (!quote && c!=' ' && last==' ') j=i;
    if (c=='\"') {
      quote=!quote;
      if (quote) j++;
    }
    if (((c==' ' && !quote) || c=='\0') && last!=' ') {
      *(list+count)=malloc((i-j+1)*sizeof(char));
      strncpy(*(list+count),command+j,i-j);
      curr=*(list+count)+i-j;
      *curr='\0';
      if (*(curr-1)=='\"') *(curr-1)='\0';
      count++;
    }
    last=c;
    i++;
  } while(c!='\0');
}

#ifdef WIN95  /* routines needed only for WIN95 */
void win_startup(void)
     /* open input file, read registry, etc. */
{
}


void win_shutdown(void)
     /* waits for key, etc. */
{
  printf("Program done --- Press <RETURN>");
  fgetc(stdin);
  if (printerfont) DeleteObject(printerfont);
  if (myfont) DeleteObject(myfont);
  if (printer) DeleteDC(printer);
  FreeConsole();
}


/* procedure to process WIN95 messages */
LRESULT CALLBACK mywindowproc(HWND handle,unsigned msg,
                              UINT wparam,DWORD lparam)
{
  switch(msg) {
  default:
    return(DefWindowProc(handle,msg,wparam,lparam));
  }
}


/* procedure for win95-thread */
DWORD winthread(LPWORD par) {
  MSG msg;
  int w,h,x,y;

  error(DIAGNOSTIC,"Starting window-thread");
  x=y=10;

  if (!geometry) geometry=getreg("geometry");
  if (geometry) 
    if (sscanf(geometry,"%ix%i+%i+%i",&w,&h,&x,&y)!=4)
      if (sscanf(geometry,"+%i+%i",&x,&y)!=2)
	if (sscanf(geometry,"%i+%i",&x,&y)!=2) 
	  x=y=10;
  
  /* create my window */
  window=CreateWindowEx(WS_EX_TOPMOST,
			my_class, 
			NULL,            /* my style */
			WS_VISIBLE | WS_CAPTION | WS_SYSMENU,/* window style */
			x,               /* initial x-position */
			y,               /* initial y-position */
			winwidth+2,      /* initial x-size */
			winheight+21,    /* initial y-size */
			NULL,            /* parent window */
			NULL,            /* menu handle */
			this_instance,   /* my instance */
			(LPVOID) NULL);  /* dont know why */
  
  /* show my window */
  SetWindowText(window,BASIC_NAME" - Grafic Window");
  ShowWindow(this_instance,SW_SHOWDEFAULT);
  UpdateWindow(this_instance);
  winopened=TRUE;
  SetEvent(thread.winevent);

  /* get and dispatch messages */
  while(GetMessage((LPMSG)&msg,NULL,0,0))
    DispatchMessage((LPMSG)&msg);
  
  DestroyWindow(window);
  ExitThread(0);
  return 0;
}


void startdraw() /* prepare for drawing */
{
  RECT interior;
  
  devcon=GetDC(window);
  GetClientRect(window,&interior);
  SelectClipRgn(devcon,NULL);
  IntersectClipRect(devcon,interior.left,interior.top,
		    interior.right,interior.bottom);
}


char *getreg(char *name) /* get default from Registry */
{
  char *keyname="SOFTWARE\\"BASIC_NAME;
  HKEY key;
  char reg[80];
  DWORD n;

  RegOpenKeyEx(HKEY_LOCAL_MACHINE,keyname,0,KEY_ALL_ACCESS,&key);
  n=80;
  reg[0]='\0';
  RegQueryValueEx(key,name,NULL,NULL,reg,&n);
  if (reg[0]=='\0') return NULL;
  return my_strdup(reg);
}
#endif  /* WIN95-stuff ... */


void create_openprinter(int num) /* create command 'openprinter' */
{
  struct command *cmd;
  
  if (DIAGNOSTIC<=infolevel)
    error(DIAGNOSTIC,"Creating command 'openprinter'");
  
  cmd=add_command(OPENPRN);
  cmd->args=num;
}


void openprinter(struct command *cmd) /* opens a printer for WIN95 */
{
  static int first=TRUE;
#ifdef WIN95
  char printername[80];
  char *keyname;
  HKEY key;
  DWORD len; 
  DOCINFO di;
  float prnwidth,prnheight;
  LOGBRUSH mybrush;
  RECT interior;
#endif  

  /* close file, if already open */
#ifndef WIN95
  if (printerfile) closeprinter();
#endif

  if (cmd->args==1) {
    prfilename=strdup((char *)(pop()->pointer));
    print_to_file=TRUE;}
  else {
    prfilename="\0";
    print_to_file=FALSE;
  }
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Printing to %s.",(print_to_file)?"file":"printer");
    error(DIAGNOSTIC,string);
  }
#ifndef WIN95 
  if (*prfilename=='\0') {
    free(prfilename);
    prfilename=strdup("/tmp/yabasic.ps");
  }
  if (*prfilename!='\0') {
    printerfile=fopen(prfilename,"w");
    if (printerfile) {
      sprintf(string,"printing to '%s'",prfilename);
      error(DIAGNOSTIC,string);}
    else {
      sprintf(string,"could not open file '%s' for printing",prfilename);
      error(ERROR,string);
    }
  }
#endif
  if (!first) return;

#ifdef WIN95
  printername[0]='\0';
  keyname="Config\\0001\\System\\CurrentControlSet\\Control\\Print\\Printers";

  len=80;
  RegOpenKeyEx(HKEY_LOCAL_MACHINE,keyname,NULL,NULL,&key);
  RegQueryValueEx(key,"Default",NULL,NULL,printername,&len);

  printer=CreateDC(NULL,printername,NULL,NULL);
  if (!printer) {
    sprintf(string,"Couldn't get handle for printer '%s'",printername);
    error(ERROR,string);
    return;}
  else {
    sprintf(string,"Opened printer '%s'",printername);
    error(DIAGNOSTIC,string);
  }
  if (SelectObject(printer,CreateSolidBrush(RGB(0,0,0)))==NULL) {
    error(ERROR,"Could not select brush for printer");
    return;
  }
  
  /* calculate scaling-factors */
  prnwidth=(float)GetDeviceCaps(printer,HORZRES);
  prnheight=(float)GetDeviceCaps(printer,VERTRES);
  if (prnwidth/winwidth>prnheight/winheight) {
    prnscale=0.7*prnheight/winheight;
    prnoff=0.15*prnheight;}
  else {
    prnscale=0.7*prnwidth/winwidth;
    prnoff=0.15*prnwidth;
  }
  if (DIAGNOSTIC<=infolevel) {
    sprintf(string,"Scaling factor for printer is %f",prnscale);
    error(DIAGNOSTIC,string);
  }
  
  /* set clipping region */
  GetClientRect(window,&interior);
  SelectClipRgn(printer,NULL);
  IntersectClipRect(printer,
		    interior.left*prnscale+prnoff,
		    interior.top*prnscale+prnoff,
		    interior.right*prnscale+prnoff,
		    interior.bottom*prnscale+prnoff);
  
  /* create printerfont */
  logfont.lfHeight=fontheight*prnscale;
  printerfont=CreateFontIndirect(&logfont);
  if (printerfont==NULL) {
    sprintf(string,"Could not create font for printer");
    error(ERROR,string);
    return;
  }
  

  /* create printerpen */
  mybrush.lbStyle=BS_SOLID;
  mybrush.lbColor=DIB_RGB_COLORS;
  mybrush.lbHatch=HS_DIAGCROSS;
  printerpen=ExtCreatePen(PS_GEOMETRIC,prnscale,&mybrush,0,NULL);
  if (SelectObject(printer,printerpen)==NULL) {
    error(ERROR,"Couldn't select printerpen");
    return;
  }
  
  di.cbSize=sizeof(DOCINFO);
  di.lpszDocName=BASIC_NAME" grafics";
  di.lpszOutput=(print_to_file) ? prfilename : (LPTSTR)NULL;
  di.lpszDatatype=(LPTSTR)NULL;
  di.fwType=0;
  if (StartDoc(printer,&di)==SP_ERROR) {
    error(ERROR,"Couldn't start printing");
    return;
  }
  StartPage(printer);
  first=FALSE;
#else
  fprintf(printerfile,"%%!PS-Adobe-1.0\n");
  fprintf(printerfile,"%%%%Title: "BASIC_NAME"-Grafics\n");
  fprintf(printerfile,"%%%%BoundingBox: 0 0 %i %i\n",
	  (int)(winwidth*psscale),(int)(winheight*psscale));
  fprintf(printerfile,"%%%%DocumentFonts: Helvetica\n");
  fprintf(printerfile,"%%%%Creator: "BASIC_NAME"\n");
  fprintf(printerfile,"%%%%Pages: (atend)\n");
  fprintf(printerfile,"%%%%EndComments\n");
  fprintf(printerfile,"gsave\n");
  fprintf(printerfile,"/M {moveto} def\n");
  fprintf(printerfile,"/RL {rlineto} def\n");
  fprintf(printerfile,"/L {lineto} def\n");
  fprintf(printerfile,"/N {newpath} def\n");
  fprintf(printerfile,"/S {stroke} def\n");
  fprintf(printerfile,"/D {N M 0 %g RL %g 0 RL 0 %g RL closepath fill} def\n",
	  psscale,psscale,-psscale);
  fprintf(printerfile,"/C {N 0 360 arc} def\n");
  fprintf(printerfile,"/Helvetica findfont\n");
  fprintf(printerfile,"%g scalefont setfont\n",(double)fontheight*psscale);
  fprintf(printerfile,"30 30 translate\n");
  fflush(printerfile);
#endif
}


void create_closeprinter() /* create command 'closeprinter' */
{
  struct command *cmd;
  
  if (DIAGNOSTIC<=infolevel)
    error(DIAGNOSTIC,"Creating command 'closeprinter'");
  
  cmd=add_command(CLOSEPRN);
}


void closeprinter() /* closes printer for WIN95 */
{
#ifdef WIN95
  EndPage(printer);
  EndDoc(printer);
#else
  if (printerfile) {
    fprintf(printerfile,"showpage\ngrestore\n%%%%Trailer\n");
    fclose(printerfile);
    printerfile=NULL;
    if (!strncmp(prfilename,"/tmp/",5)) {
      sprintf(string,"lpr %s",prfilename);
      if (system(string)) {
	sprintf(string,"couldn't print '%s'",prfilename);
	error(ERROR,string);
	return;
      }
      remove(prfilename);
    }
  }
#endif
  if (prfilename) free(prfilename);
}


void initialize() 
     /* give correct values to pointers etc ... */
{
  int i;

  /* install exception handler */
  signal(SIGFPE,signal_handler);
  signal(SIGSEGV,signal_handler);
 
  /* initialize error handling: no errors seen 'til now */
  errorlevel=DIAGNOSTIC;  
  diagnostic_count=0;
  note_count=0;
  warning_count=0;
  error_count=0;

  /* initialize symbol table */
  symroot=(struct symbol *)my_malloc(sizeof(struct symbol)); /* ceate first */
  symroot->type=FREE;
  symroot->pointer=NULL;
  symroot->next=NULL;
  symroot->name=NULL;
  symroot->value=0.0;
  
  /* initialize numeric stack */
  /* create first : */
  stackroot=(struct stackentry *)my_malloc(sizeof(struct stackentry)); 
  stackroot->next=NULL;
  stackroot->prev=NULL;
  stackroot->value=0.0;

  /* initialize command stack */
  /* create first : */
  cmdroot=(struct command *)my_malloc(sizeof(struct command)); 
  cmdroot->next=NULL;

  reset();

  datapointer=cmdroot; /* restore for read data */

  /* file stuff */
  for(i=1;i<=9;i++) streams[i]=NULL;
  printerfile=NULL; /* no ps-file yet */
}


void signal_handler(int sig)   /* handle signals */
{
  switch (sig) {
  case SIGFPE:
    error(FATAL,"Floating point exception, cannot proceed.");
    exit(TRUE);
  case SIGSEGV:
    error(FATAL,"Segmentation violation, cannot proceed.");
    exit(TRUE);
#ifdef UNIX
  case SIGALRM: /* ignore */
    break;
#endif
  default:
    break;
  }
}


void reset() 
/*
   reset pointers to their initial values, 
   initialize variables and functions 
*/
{
  struct symbol *s;
  struct stackentry *base;
  int i;

  symhead=symroot; /* list of double symbols */
  stackhead=stackroot; /* stack of double values */
  base=push();
  base->type=NIL; /* push nil, so that pop will not crash */
  cmdhead=cmdroot; /* list of commands */;
  commandcount=0;

  /* create useful variables */
  s=get_sym("PI",NUMBER,TRUE);
  s->value=3.14159265359;
  s=get_sym("EULER",NUMBER,TRUE);
  s->value=2.71828182864;

  /* add internal variables */
  get_sym("yabinfolevel",NUMBER,TRUE)->value=infolevel;
  get_sym("yabdiagnostic",NUMBER,TRUE)->value=DIAGNOSTIC;
  get_sym("yabnote",NUMBER,TRUE)->value=NOTE;
  get_sym("yabwarning",NUMBER,TRUE)->value=WARNING;
  get_sym("yaberror",NUMBER,TRUE)->value=ERROR;
  get_sym("yabfatal",NUMBER,TRUE)->value=FATAL;
  get_sym("yabfontheight",NUMBER,TRUE)->value=10;
  get_sym("yabwinheight",NUMBER,TRUE)->value=100;
  get_sym("yabwinwidth",NUMBER,TRUE)->value=100;

  /* set default-scales for grafics */
  fontheight=10;
  get_sym("yabfontheight",NUMBER,FALSE)->value=fontheight;
  winheight=100;
  get_sym("yabwinheight",NUMBER,FALSE)->value=winheight;
  winwidth=100;
  get_sym("yabwinwidth",NUMBER,FALSE)->value=winwidth;
  calc_psscale();

  /* add functions */
  add_function("sin","rd",mysin);
  add_function("asin","rd",myasin);
  add_function("cos","rd",mycos);
  add_function("acos","rd",myacos);
  add_function("tan","rd",mytan);
  add_function("atan","-",myatan);
  add_function("exp","rd",myexp);
  add_function("log","rd",mylog);
  add_function("sqrt","rd",mysqrt);
  sprintf(string,"Adding functions sin,asin,cos,acos,tan,atan,exp,log,sqrt"); 
  error(DIAGNOSTIC,string);
  add_function("int","rd",myint);
  add_function("frac","rd",myfrac);
  add_function("left$","rsd",myleft);
  add_function("right$","rsd",myright);
  add_function("mid$","rsdd",mymid);
  add_function("len","rs",mylen);
  add_function("val","rs",myval);
  add_function("str$","rd",mystr);
  sprintf(string,"Adding functions int,frac,left$,right$,mid$,len,val,str$"); 
  error(DIAGNOSTIC,string);

  /* file stuff */
  for(i=1;i<=9;i++) 
    if (streams[i]!=NULL) {
      sprintf(string,"Stream %d not closed; closing it now",i);
      error(NOTE,string);
      fclose(streams[i]);
    }
}


void run_it()
/* execute the compiled code */
{
  int endflag=FALSE;

  current=cmdroot; /* start with first comand */
  while(current!=cmdhead && !endflag && errorlevel>ERROR) {
    switch(current->type) {
    case GOTO:case QGOTO:case GOSUB:case QGOSUB:
      jump(current); DONE;
    case SKIPPER: 
      skipper(); break;
    case LABEL:case DATA:case NOP: 
      DONE;
    case RETURN:
      myreturn(); DONE;
    case PUSHDBLSYM: 
      pushdblsym(current); DONE;
    case PUSHDBL:
      pushdbl(current); DONE;
    case POPDBLSYM:
      popdblsym(current); DONE;
    case POPSTRSYM:
      popstrsym(current); DONE;
    case PUSHSTRSYM: 
      pushstrsym(current); DONE;
    case PUSHSTR:
      pushstr(current); DONE;
    case CONCAT:
      concat(); DONE;
    case PRINT:
      print(current); DONE;
    case MYOPEN:
      myopen(current); DONE;
    case MYCLOSE:
        myclose(current); DONE;
    case MYSWITCH:
      myswitch(current); DONE;
    case MYREAD:
      myread(current); DONE;
    case RESTORE:case QRESTORE:
      restore(current); DONE;
    case READDATA:
      readdata(current); DONE;
    case PROMPT:
      prompt(current); DONE;
    case DBLADD:case DBLMIN:case DBLMUL:case DBLDIV:case DBLPOW:
      dblbin(current); DONE;
    case NEGATE:
      negate(); DONE;
    case EQ:case NE:case GT:case GE:case LT:case LE:
      dblrelop(current); DONE;
    case STREQ:case STRNE:
      strrelop(current); DONE;
    case AND:case OR:case NOT:
      boole(current); DONE;
    case CALLFUN:
      callfun(current); DONE;
    case CALLARR:
      callarr(current); DONE;
    case DIM:
      dim(current); DONE;
    case DECIDE:
      decide(); DONE;
    case OPENWIN:
      openwin(current); DONE;
    case OPENPRN:
      openprinter(current); DONE;
    case CLOSEPRN:
      closeprinter(); DONE;
    case DOT:
      dot(); DONE;
    case LINE:
      line(); DONE;
    case CIRCLE:
      circle(); DONE;
    case TEXT:
      text(current); DONE;
    case CLOSEWIN:
      closewin(); DONE;
    case CLEARWIN:
      clearwin(); DONE;
    case WAIT:
      wait(); DONE;
    case BELL:
      bell(); DONE;
    case SETINFOLEVEL:
      setinfolevel(); DONE;
    case SETFONTHEIGHT:
      setfontheight(); DONE;
    case SETWINHEIGHT:
      setwinheight(); DONE;
    case SETWINWIDTH:
      setwinwidth(); DONE;
    case END:
      endflag=TRUE; myend(); break;
    default:
      sprintf(string,"Unkown Interpreter-Command, Token %d.",current->type);
      error(ERROR,string);
      break;
    }
  }
  program_state=FINISHED;
  switch(errorlevel) {
  case NOTE:case DIAGNOSTIC: 
    error(NOTE,"Program ended normally."); break;
  case WARNING:
    error(WARNING,"Program ended with a warning"); break;
  case ERROR:
    error(ERROR,"Program stopped due to an error"); break;
  case FATAL: /* should not come here ... */
    error(FATAL,"Program terminated due to FATAL error");
    break;
  }
}


void error(int severe, char *message) 
/* reports an basic error to the user and possibly exits */
{
  if (severe<=infolevel) {
    fprintf(stderr,"---");
    switch(severe) {
    case(DIAGNOSTIC): 
      fprintf(stderr,"Diagnostic"); 
      diagnostic_count++;
      break;
    case(NOTE): 
      fprintf(stderr,"Note"); 
      note_count++;
      break;
    case(WARNING): 
      fprintf(stderr,"Warning"); 
      warning_count++;
      break;
    case(ERROR): 
      fprintf(stderr,"Error"); 
      error_count++;
      break;
    case(FATAL): 
      fprintf(stderr,"Fatal"); 
      break;
    }
    if (program_state==COMPILING || program_state==RUNNING) 
      fprintf(stderr," in line %d",
              (program_state==RUNNING) ? current->line : yylineno);
    fprintf(stderr,": %s\n",message);
  }
  if (severe<errorlevel) errorlevel=severe;
  if (severe<=FATAL) {
    fprintf(stderr,"---Immediate exit to system, due to a fatal error.\n");
#ifdef WIN95
    win_shutdown();
#endif
    exit(TRUE);
  }
}


void uif(int id) /* issues warning on screen */
{
  sprintf(string,
          "Feature at line %d not yet implemented, id %d (sorry !)",
          yylineno,id);
  error(WARNING,string);
}


char *my_strdup(char *arg)  /* my own version of strdup, checks for failure */
{
  char *ret;
  int l;

  l=strlen(arg);

  ret=my_malloc(l+1);
  strncpy(ret,arg,l);
  *(ret+l)='\0';
  
  return ret;
}


void *my_malloc(unsigned num) /* Alloc memory and issue warning on failure */
{
  void *room;
  
  room=malloc(num);
  if (room==NULL) {
    sprintf(string,"Can't malloc %d bytes of memory",num);
    error(FATAL,string);
  }
  return room;
}

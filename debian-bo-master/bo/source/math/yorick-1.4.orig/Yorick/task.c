/*
    TASK.C
    Implement Yorick virtual machine.

    $Id: task.c,v 1.1 1993/08/27 18:32:09 munro Exp munro $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include "ydata.h"
#include "yio.h"
#include "defstr.h"
#include "defmem.h"

/* packages that need to clean up before Yorick exits should supply
   CleanUpForExit, then call the one they found */
extern void (*CleanUpForExit)(void);
void (*CleanUpForExit)(void)= 0;

extern int YpParse(void *func);  /* argument non-zero for YpReparse */
extern char *YpPeekLine(void);
extern char *YpNextLine(int);

/* The offsetof macro may be defined in <stddef.h> (it is ANSI standard).  */
/* #define offsetof(structure, member)  ((long)&(((structure*)0)->member))
   (does not work on Crays) */
#ifndef offsetof
#define offsetof(structure, member) \
  ((long)((char *)&(((structure*)0)->member) - (char *)0))
#endif

#ifndef JMP_BUF_DECLARATION
#include <setjmp.h>

#else
/* If the file setjmp.h cannot be found, you must find the correct
   definition of the data type jmp_buf, which is normally an array
   big enough to hold the state information necessary to make the
   longjmp.  Put the declaration in sysdep.h.  */
JMP_BUF_DECLARATION
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);
#endif

jmp_buf yMainLoop;

/*--------------------------------------------------------------------------*/

extern BuiltIn Y_quit, Y_include, Y_require, Y_help, Y_exit, Y_error, Y_batch;

extern void YRun(void);
extern void YHalt(void);

extern int CheckForTasks(int wait);
extern void RunTaskNow(Function *task);
extern void IncludeNow(void);
extern void ResetStack(int hard);

extern VMaction Eval, Return, PushVariable;

extern jmp_buf yMainLoop;
extern void YMainLoop(void);

extern void ClearTasks(void);
extern void BuryTask(void);
extern int DoTask(void);

extern Function *FuncContaining(Instruction *pc);

/* If yAutoDebug!=0, debug mode will be entered automatically whenever
   a runtime error occurs.  Otherwise, you must type "debug".  */
extern int yAutoDebug;
int yAutoDebug= 0;

int yDebugLevel= 0;

/* most recent error message was built in yErrorMsg */
char yErrorMsg[132];
char yWarningMsg[132];

static int inYError= 0;

/* from fnctn.c */
extern Instruction *ClearStack(void);
extern Instruction *AbortReturn(void);

extern void SetSignals(int flags);  /* in sysdep.c */

/* don't bother to try to include <stdlib.h> for one lousy function */
extern void exit(int status);

extern long ypBeginLine;

extern TextStream *NewTextStream(char *fullname,
				 void *stream, int permissions,
				 long line, long pos);

typedef struct DebugBlk DebugBlk;
extern DebugBlk *NewDebugBlk(Function *f, long pcerr, char *errFile,
			     long lnum);

/* stuff to implement catch */
extern BuiltIn Y_catch;
typedef struct Catcher Catcher;
struct Catcher {
  Instruction *task;
  Instruction *pc;  /* of conditional branch instruction */
  long isp;         /* sp-spBottom of returnSym for calling function */
  int category;     /* of error to be caught */
};
static long n_catchers= 0;
static long max_catchers= 0;
static Catcher *catchers= 0;
extern void YCatchDrop(long isp);        /* used in fnctn.c */
extern long ispCatch;                    /* used in fnctn.c */
long ispCatch= 0;
extern int y_catch_category;
int y_catch_category= 0x08;
static Catcher *CatchScan(const char *msg, int category);
static Catcher *CatchNew(void);
static int caughtTask= 0;

/* stuff to implement set_idler */
extern BuiltIn Y_set_idler;
extern Function *y_idler_function;
Function *y_idler_function= 0;

/*--------------------------------------------------------------------------*/

Instruction *pc= 0;

static Instruction *pcHalt;
static Instruction halt[2];

static int vmRunning= 0;

void YRun(void)
{
#ifdef FAKE_INTERRUPTS
  extern int check_except(void);
  extern short check_brk_flag;
#endif
  register VMaction *Action;
  vmRunning= 1;
  pcHalt= 0;
#ifndef FAKE_INTERRUPTS
  while ((Action= (pc++)->Action)) Action();
#else
  while ((Action= (pc++)->Action)) {
    if (check_brk_flag) check_except();
    Action();
  }
  if (check_brk_flag) check_except();
#endif
  pc= pcHalt;
  pcHalt= 0;
  vmRunning= 0;
}

void YHalt(void)
{
  halt[0].Action= 0;
  pcHalt= pc-1;      /* pcHalt->Action called YHalt */
  pc= halt;
}

/*--------------------------------------------------------------------------*/

static Function **tasks= 0;
static int nTasks= 0;
static int maxTasks= 0;

void ClearTasks(void)
{
  while (nTasks) Unref(tasks[--nTasks]);
  if (maxTasks>16) {
    maxTasks= 0;
    Yfree(tasks);
    tasks= 0;
  }
}

void PushTask(Function *task)
{
  if (nTasks>=maxTasks) {
    int newSize= maxTasks+16;
    tasks= Yrealloc(tasks, sizeof(Function *)*newSize);
    maxTasks= newSize;
  }
  tasks[nTasks++]= Ref(task);  /* WARNING-- this is the reference for when
				  DoTask pushes the task onto the stack--
				  you are still giving your reference to the
				  Function* away when you call PushTask */
}

void BuryTask(void)
{
  int n= nTasks;
  if (n) {
    Function *task= tasks[--n];
    while (n) {
      tasks[n]= tasks[n-1];
      n--;
    }
    tasks[0]= task;
  }
}

/* The task pseudo-code MUST be static, since YError may longjmp out
   of the DoTask procedure.  */
static Instruction taskCode[4];
static int taskCodeInit= 0;

int DoTask(void)
{
  if (caughtTask) {
    caughtTask= 0;
    YRun();
    Drop(1);
  } else if (nTasks>0) {
    if (!taskCodeInit) {
      taskCode[0].Action= &Eval;
      taskCode[1].count= 0;
      taskCode[2].Action= 0;
      taskCode[3].index= 0;
      taskCodeInit= 1;
    }

    CheckStack(1);
    (sp+1)->ops= &dataBlockSym;
    (sp+1)->value.db= (DataBlock *)tasks[--nTasks];  /* use owned by stack */
    sp++;

    pc= taskCode;     /* note that original pc is clobbered */
    YRun();
    Drop(1);
  }
  return nTasks;
}

/*--------------------------------------------------------------------------*/

static int allDone;
static int detectRecursion= 0;

void YMainLoop(void)
{
  allDone= 0;
  setjmp(yMainLoop);
  for (;;) {
    while (nTasks+caughtTask && !allDone) DoTask();
    if (allDone) break;
    CheckForTasks(1);
  }

  /* close all binary files properly */
  {
    IOStream *file;
    detectRecursion= 0;
    while (yBinaryFiles) {
      file= yBinaryFiles->ios;
      file->references= 0;
      if (setjmp(yMainLoop)==0) {
	Unref(file);
      } else {
	if (yBinaryFiles && file==yBinaryFiles->ios) {
	  if (detectRecursion) break;
	  detectRecursion= 1;
	  RemoveIOLink(yBinaryFiles, file);
	  detectRecursion= 0;
	}
      }
    }
  }

  if (CleanUpForExit) CleanUpForExit();
}

void Y_quit(int nArgs)
{
  allDone= 1;
  ResetStack(0);
  longjmp(yMainLoop, 1);
}

extern int nYpInputs;   /* from yinput.c */
extern int yImpossible;
int yImpossible= 0;

int CheckForTasks(int wait)
{
  do {
    if (yImpossible<=3) YMaybeDispatch();
    if (wait && y_idler_function &&
	!nTasks && !nYpIncludes && !nYpInputs) {
      /* note !yPendingIn would be an error above, because if there
	 is a y_idler_function, PutPrompt will force YpParse to quit
	 without delivering a line, which would cause an infinite loop */
      Function *f= y_idler_function;
      y_idler_function= 0;
      PushTask(f);
      Unref(f);
    } else if (wait || yPendingIn) {
      YpParse((void *)0);
    }
  } while (wait && !nTasks);
  return nTasks;
}

void RunTaskNow(Function *task)
{
  Instruction *pcHere= pc;
  int t0= nTasks;
  PushTask(Ref(task));
  while (nTasks>t0 && !allDone) DoTask();
  pc= pcHere;           /* may have been clobbered by DoTask */
  if (allDone) YHalt();
}

void IncludeNow(void)
{
  Instruction *pcHere= pc;
  int i0= nYpIncludes;
  int t0= nTasks;
  for (;;) {
    while (nTasks<=t0 && (nYpIncludes>i0 || (nYpIncludes==i0 &&
	   ypIncludes[i0-1].file))) YpParse((void *)0);
    while (nTasks>t0 && !allDone) DoTask();
    if (allDone || nYpIncludes<i0 || !ypIncludes[i0-1].file) break;
  }
  pc= pcHere;           /* may have been clobbered by DoTask */
  if (allDone) YHalt();
}

void Y_include(int nArgs)
{
  long now;
  char *name;
  if (nArgs!=1 && nArgs!=2)
    YError("include function takes exactly one or two arguments");
  if (nArgs>1) now= YGetInteger(sp);
  else now= 0;
  name= YGetString(sp-nArgs+1);
  if (now>=0 && !YpPushInclude(name))
    YError("missing include file specified in include function");
  else if (now<0)
    YpPush(name);          /* defer until all pending input parsed */
  Drop(nArgs);
  if (now>0) IncludeNow(); /* parse and maybe execute file to be included
			      -- without now, this won't happen until the
			      next line is parsed naturally */
}

void Y_require(int nArgs)
{
  char *full, *name, *tail= 0;
  long i;
  if (nArgs!=1) YError("require function takes exactly one argument");
  full= YGetString(sp);
  name= YNameTail(full);
  for (i=0 ; i<sourceTab.nItems ; i++) {
    tail= YNameTail(sourceTab.names[i]);
    if (name && tail && strcmp(tail, name)==0) break;
    StrFree(tail);
    tail= 0;
  }
  StrFree(name);
  StrFree(tail);
  if (i>=sourceTab.nItems && !YpPushInclude(full))
    YError("missing include file specified in require function");
  Drop(nArgs);
  if (i>=sourceTab.nItems)
    IncludeNow();   /* parse and maybe execute file to be included */
}

/*--------------------------------------------------------------------------*/

static int findingFunc= 0;

Function *FuncContaining(Instruction *pc)
{
  Function *func= 0;

  if (!findingFunc && pc) {
    long i= -1;
    findingFunc= 1;
    for (;; i++) {
      while (pc[i].Action) i++;
      if (pc[i-1].Action==&Return) break;
    }
    i++;
    /* Now pc[i] is the Instruction generated by following line
       in parse.c (YpFunc):
          vmCode[nextPC].index= codeSize= nPos+nKey+nLocal+ nextPC;
       (nextPC does NOT include the parameters or locals)
     */
    i-= pc[i].index;
    if (i<0) {
      /* see also Pointee function in ydata.c */
      func= (Function *)((char *)(pc+i) - offsetof(Function, code));
      findingFunc= 0;
    }
  }

  if (findingFunc) {
    /* may get here after a disaster causing an interrupt above, as well
       as after scanning from a garbled initial pc */
    findingFunc= 0;
    YputsErr("(BUG) lost function produced following error:");
  }
  return func;
}

/*--------------------------------------------------------------------------*/

void ResetStack(int hard)
{
  Instruction *pcRet;
  while ((pcRet= AbortReturn())) if (pcRet==&taskCode[2] && !hard) break;
}

/*--------------------------------------------------------------------------*/

char discard[81];

void YWarning(const char *msg)
{
  strcpy(yWarningMsg, "WARNING ");
  strncat(yWarningMsg, msg, 120);
  YputsErr(yWarningMsg);
}

static char *includeFile= 0;
static long mainIndex= -1;
Instruction *yErrorPC= 0;   /* for dbup function in debug.c */

extern int yBatchMode;  /* may be set with -batch, see std0.c */
int yBatchMode= 0;

void YError(const char *msg)
{
  long beginLine= ypBeginLine;
  Instruction *pcDebug= pcHalt? pcHalt : pc;
  Function *func;
  char *name;
  DebugBlk *dbg;
  Instruction *pcUp= yErrorPC;
  int category;

  int recursing= inYError;
  inYError++;
  yErrorPC= 0;

  category= y_catch_category;
  y_catch_category= 0x08;

  if (y_idler_function) {
    /* remove any idler on error - catch can reset if desired */
    Function *f= y_idler_function;
    y_idler_function= 0;
    Unref(f);
  }

  if (recursing>8 || yImpossible>8) {
    if (recursing<12 && yImpossible<12) {
      YputsErr("****SEVERE**** YError looping -- "
	       "turning off signal handlers, quit ASAP");
      SetSignals(0);
    } else {
      YputsErr("****FATAL**** YError still looping -- "
	       "calling exit");
      exit(1);
    }
  }
  yImpossible++;  /* zeroed only by GetNextLine and after CatchScan */

  if (CatchScan(msg, category)) {
    /* resume at catch PC if this error has been caught --
       is this really proof against catastrophic looping? */
    inYError= 0;
    yImpossible= 0;
    caughtTask= 1;
    longjmp(yMainLoop, 1);
  }

  func= (vmRunning && !recursing && pcUp!=&taskCode[2])?
    FuncContaining(pcDebug) : 0;
  name= func? globalTable.names[func->code[0].index] : "VM idle or lost";

  /* Clear out include stack, but remember current include file name.
     If the error happened while executing a main program which came from
     the include file, then includeFile will be the filename, and
     ypBeginLine will be the line number at which the errant main
     program began.  (No other line number information will be available
     for a main program?)  */
  if (!recursing) {
    char *tmp= includeFile;
    includeFile= 0;
    StrFree(tmp);
    if (nYpIncludes) {
      includeFile= StrCpy(ypIncludes[nYpIncludes-1].filename);
      YpClearIncludes();
    }
  } else if (nYpIncludes) YpClearIncludes();

  /* Clean up any Array temporaries (used for data format conversions).  */
  if (recursing<2) ClearTmpArray();

  /* Clear out any pending keyboard input or tasks.  */
  if (recursing<3) {
    int warn= 0;
    YDMaybeDispatch();  /* DO NOT use YMaybeDispatch to avoid X problems. */
    while (yPendingIn) {
      warn= 1;
      if (!YgetsIn(discard, 80)) break;
      YDMaybeDispatch();
    }
    if (warn) YputsErr("WARNING discarding pending keyboard input");
    ClearTasks();
  } else if (nTasks) {
    YputsErr("WARNING unable to free task pointers in YError");
    nTasks= 0;
  }

  /* Print error message, with name of current Yorick function prepended:
        ERROR (yorick_function) msg passed to YError
	ERROR (VM idle or lost) msg passed to YError
     The second form is used when the virtual machine is idle at the
     time of the error.  */
  if (!pcUp || recursing)
    strcpy(yErrorMsg, "ERROR (");
  else
    strcpy(yErrorMsg, "Up to (");
  strncat(yErrorMsg, name, 40);
  strcat(yErrorMsg, ") ");
  if (!pcUp || recursing)
    strncat(yErrorMsg, msg, 80);
  YputsErr(yErrorMsg);

  if (recursing) {
    func= 0;
    YputsErr("WARNING aborting on recursive calls to YError");
  }

  if (func) {
    /* Try to find the source code for this function.  */
    long index= func->code[0].index;
    if (mainIndex<0) mainIndex= Globalize("*main*", 0L);
    if (index==mainIndex) {
      name= includeFile;
    } else {
      char *mess= YpReparse(func);
      if (nTasks) ClearTasks();
      if (mess[0]!='*') {
	/* reparse succeeded, skip to filename */
	name= 0;
	while (mess[0] && mess[0]!=':') mess++;
	if (mess[0]) do { mess++; } while (mess[0] && mess[0]!=':');
	if (mess[0]) {
	  mess++;
	  if (mess[0]==' ') mess++;
	  if (mess[0]) name= mess;
	}
      } else {
	/* reparse failed */
	name= 0;
      }
      beginLine= 0;  /* used only for *main* from includeFile */
    }

    /* Push debug info (function and code index) onto stack.  */
    ClearStack();
    CheckStack(2);
    dbg= NewDebugBlk(func, pcDebug-func->code, name, beginLine);
    if (dbg) {
      PushDataBlock(dbg);
    } else {
      ResetStack(0);
      YputsErr("Function corrupted, cannot enter debug mode.");
    }

    if (!yBatchMode && !pcUp && (!yAutoDebug || yDebugLevel>1)) {
      /* Get input hopefully typed in response to error message.  */
      char *line;
      if (yDebugLevel>1) {
	YputsErr(" To enter recursive debug level, type <RETURN> now");
      } else {
	YputsErr(" To enter debug mode, type <RETURN> now"
		 " (then dbexit to get out)");
      }
      line= YpPeekLine();
      if (line) while (*line && (*line==' ' || *line=='\t')) line++;
      /* Enter debug mode if response was blank line, otherwise, don't.  */
      if (!line || *line) goto oops;
    }

  } else {
    /* Clear the stack back to the most recent debugging level.  */
  oops:
    if (recursing<5) ResetStack(0);
    else {
      YputsErr("****SEVERE**** YError unable to reset stack -- "
	       "probably lost variables");
      sp= spBottom;
    }
  }

  if (allDone) {
    YputsErr("****TERMINATING**** on error after main loop exit");
    exit(1);
  }

  if (yBatchMode || !strncmp(msg,"(FATAL)",7)) {
    if (yBatchMode) YputsErr("yorick: quitting on error in batch mode");
    else YputsErr("yorick: quitting on fatal error");
    ResetStack(0);
    allDone= 1;
  }

  /* Go back to the main loop.  */
  inYError= 0;
  longjmp(yMainLoop, 1);
}

/*--------------------------------------------------------------------------*/

static Function *help_worker= 0;

void Y_help(int nArgs)
{
  Symbol *stack= sp-nArgs+1;
  long index, worker_arg;
  int nAbove;
  FILE *file;

  worker_arg= Globalize("help_topic", 0L);

  while (stack<=sp && !stack->ops) stack+=2;  /* skip any keywords */
  nAbove= sp-stack;
  if (nAbove>=0) {
    /* a legal argument has been supplied */
    if (stack->ops==&referenceSym) {
      index= stack->index;
      ReplaceRef(stack);
    } else {
      index= -1;
    }
    if (stack->ops==&dataBlockSym) {
      DataBlock *db= stack->value.db;
      Operations *ops= db->ops;
      if (ops==&functionOps) {
	Function *f= (Function *)db;
	index= f->code[0].index;
      } else if (ops==&structDefOps) {
	StructDef *base= (StructDef *)db;
	while (base->model) base= base->model;
	index= Globalize(yStructTable.names[base->index], 0L);
      } else if (ops==&builtinOps) {
	BIFunction *f= (BIFunction *)db;
	index= f->index;
      }
    }
    Drop(nAbove);
    nArgs-= nAbove+1;
  } else {
    /* no legal arguments, help function itself is target */
    BIFunction *f= (BIFunction *)(sp-nArgs)->value.db;
    index= f->index;
    PushDataBlock(Ref(&nilDB));
  }

  /* move help topic argument off stack into help_topic extern variable */
  PopTo(&globTab[worker_arg]);  /* help_topic */
  Drop(nArgs);  /* only argument of any conceivable value just saved */

  if (!help_worker) {
    long help_index= Globalize("help_worker", 0L);
    if (globTab[help_index].ops!=&dataBlockSym ||
	(help_worker= (Function *)Ref(globTab[help_index].value.db))->ops
	!=&functionOps)
      YError("(BUG) help_worker function not found -- help unavailable");
  }

  /* create help_file extern variable */
  if (index>=0 && (file= OpenSource(index)))
    PushDataBlock(NewTextStream(StrCpy(ypIncludes[nYpIncludes-1].filename),
				file, 1, ypBeginLine-1, ftell(file)));
  else
    PushDataBlock(Ref(&nilDB));
  worker_arg= Globalize("help_file", 0L);
  PopTo(&globTab[worker_arg]);  /* help_file */

  PushTask(help_worker);
  BuryTask();
}

/*--------------------------------------------------------------------------*/

void Y_exit(int nArgs)
{
  char *msg= 0;
  if (nArgs>1) YError("exit takes exactly zero or one argument");
  if (nArgs==1 && YNotNil(sp)) msg= YGetString(sp);
  if (msg) YputsOut(msg);
  else YputsOut("EXIT called, back to main loop");
  ResetStack(0);
  longjmp(yMainLoop, 1);
}

void Y_error(int nArgs)
{
  char *msg= 0;
  if (nArgs>1) YError("error takes exactly zero or one argument");
  if (nArgs==1 && YNotNil(sp)) msg= YGetString(sp);
  y_catch_category= 0x10;
  if (msg) YError(msg);
  else YError("<interpreted error function called>");
}

void Y_batch(int nArgs)
{
  int flag= 2;
  if (nArgs>1) YError("batch takes exactly zero or one argument");
  if (nArgs==1 && YNotNil(sp)) {
    flag= (YGetInteger(sp)!=0);
    Drop(1);
  }
  PushIntValue(yBatchMode);
  if (flag!=2) yBatchMode= flag;
}

/*--------------------------------------------------------------------------*/

static Catcher *CatchNew(void)
{
  if (n_catchers>=max_catchers) {
    catchers= Yrealloc(catchers, (max_catchers+16)*sizeof(Catcher));
    max_catchers+= 16;
  }
  catchers[n_catchers].task= 0;
  catchers[n_catchers].pc= 0;
  catchers[n_catchers].isp= 0;
  catchers[n_catchers].category= 0;
  return &catchers[n_catchers++];
}

void YCatchDrop(long isp)
{
  while (n_catchers>0 && catchers[n_catchers-1].isp>=isp) {
    n_catchers--;
    catchers[n_catchers].category= 0;
  }

  if (n_catchers>0) ispCatch= catchers[n_catchers-1].isp;
  else ispCatch= 0;

  if ((max_catchers>>6) > n_catchers) {
    /* attempt to limit damage from runaway catch calls */
    catchers= Yrealloc(catchers, (max_catchers>>6)*sizeof(Catcher));
    max_catchers>>= 6;
  }
}

static Catcher *CatchScan(const char *msg, int category)
{
  long i= n_catchers-1;
  while (i>=0 && !(category&catchers[i].category)) {
    if (catchers[i].task != &taskCode[2]) i= 0;
    i--;
  }

  if (i>=0) {
    char tmsg[84];
    Array *array;
    long cmsg;
    Instruction *pcRet;
    Symbol *spCatch= spBottom + catchers[i].isp;
    catchers[i].category= 0;  /* disable this catcher */

    /* note: msg itself might be on stack! */
    strncpy(tmsg, msg, 80);
    tmsg[80]= '\0';

    for (;;) {
      ClearStack();
      if (spCatch >= sp) break;
      pcRet= AbortReturn();
      if (!pcRet || pcRet==&taskCode[2]) YError("(BUG) impossible catch");
    }
    if (spCatch!=sp) YError("(BUG) impossible catch or corrupt stack");
    pc= catchers[i].pc;
    PushIntValue(1);

    /* set catch_message variable (after stack cleared) */
    cmsg= Globalize("catch_message", 0L);
    if (globTab[cmsg].ops==&dataBlockSym) {
      globTab[cmsg].ops= &intScalar;
      Unref(globTab[cmsg].value.db);
    }
    array= NewArray(&stringStruct, (Dimension *)0);
    globTab[cmsg].value.db= (DataBlock *)array;
    globTab[cmsg].ops= &dataBlockSym;
    array->value.q[0]= StrCpy(tmsg);

    return &catchers[i];

  } else {
    return 0;
  }
}

extern VMaction BranchFalse, BranchTrue;

void Y_catch(int nArgs)
{
  Catcher *catcher= 0;
  long i= n_catchers-1;
  long isp= (sp-2) - spBottom;
  int category;
  if (nArgs!=1) YError("catch takes exactly one argument");
  category= YGetInteger(sp);
  if ((sp-2)->ops != &returnSym ||
      (pc->Action!=&BranchFalse && pc->Action!=&BranchTrue))
    YError("catch() must be the condition in an if or while statement");

  while (i>=0 && catchers[i].task==&taskCode[2] && catchers[i].isp==isp) {
    if (catchers[i].pc==pc) {
      catcher= &catchers[i];
      break;
    }
    i--;
  }
  if (!catcher) catcher= CatchNew();
  catcher->task= &taskCode[2];
  catcher->pc= pc;
  catcher->isp= ispCatch= isp;
  catcher->category= category;

  PushIntValue(0);
}

/*--------------------------------------------------------------------------*/

void Y_set_idler(int nArgs)
{
  Function *f;
  if (nArgs>1)
    YError("set_idler function takes exactly zero or one argument");

  if (nArgs>0 && YNotNil(sp)) {
    f= (Function *)sp->value.db;
    if (sp->ops!=&dataBlockSym || f->ops!=&functionOps)
      YError("expecting function as argument");
    y_idler_function= Ref(f);

  } else if (y_idler_function) {
    f= y_idler_function;
    y_idler_function= 0;
    Unref(f);
  }
}

/*--------------------------------------------------------------------------*/

/* A reduced tclInt.h for itcl 1.5, compatible with Tcl 7.4 and 7.5. */

#ifndef _TCLINT
#define _TCLINT

#include <stdio.h>

#ifndef _TCL
#include "tcl.h"
#endif

#include <limits.h>
#include <stdlib.h>
#include <string.h>

#if defined(__STDC__) || defined(HAS_STDARG)
#   include <stdarg.h>
#else
#   include <varargs.h>
#endif

/* (the following structure is from tclRegexp.h) */

#define NSUBEXP  50
typedef struct regexp {
	char *startp[NSUBEXP];
	char *endp[NSUBEXP];
	char regstart;		/* Internal use only. */
	char reganch;		/* Internal use only. */
	char *regmust;		/* Internal use only. */
	int regmlen;		/* Internal use only. */
	char program[1];	/* Unwarranted chumminess with compiler. */
} regexp;

/*
 * The following structure defines a variable trace, which is used to
 * invoke a specific C procedure whenever certain operations are performed
 * on a variable.
 */

typedef struct VarTrace {
    Tcl_VarTraceProc *traceProc;/* Procedure to call when operations given
				 * by flags are performed on variable. */
    ClientData clientData;	/* Argument to pass to proc. */
    int flags;			/* What events the trace procedure is
				 * interested in:  OR-ed combination of
				 * TCL_TRACE_READS, TCL_TRACE_WRITES, and
				 * TCL_TRACE_UNSETS. */
    struct VarTrace *nextPtr;	/* Next in list of traces associated with
				 * a particular variable. */
} VarTrace;

/*
 * When a variable trace is active (i.e. its associated procedure is
 * executing), one of the following structures is linked into a list
 * associated with the variable's interpreter.  The information in
 * the structure is needed in order for Tcl to behave reasonably
 * if traces are deleted while traces are active.
 */

typedef struct ActiveVarTrace {
    struct Var *varPtr;		/* Variable that's being traced. */
    struct ActiveVarTrace *nextPtr;
				/* Next in list of all active variable
				 * traces for the interpreter, or NULL
				 * if no more. */
    VarTrace *nextTracePtr;	/* Next trace to check after current
				 * trace procedure returns;  if this
				 * trace gets deleted, must update pointer
				 * to avoid using free'd memory. */
} ActiveVarTrace;

/*
 * The following structure describes an enumerative search in progress on
 * an array variable;  this are invoked with options to the "array"
 * command.
 */

typedef struct ArraySearch {
    int id;			/* Integer id used to distinguish among
				 * multiple concurrent searches for the
				 * same array. */
    struct Var *varPtr;		/* Pointer to array variable that's being
				 * searched. */
    Tcl_HashSearch search;	/* Info kept by the hash module about
				 * progress through the array. */
    Tcl_HashEntry *nextEntry;	/* Non-null means this is the next element
				 * to be enumerated (it's leftover from
				 * the Tcl_FirstHashEntry call or from
				 * an "array anymore" command).  NULL
				 * means must call Tcl_NextHashEntry
				 * to get value to return. */
    struct ArraySearch *nextPtr;/* Next in list of all active searches
				 * for this variable, or NULL if this is
				 * the last one. */
} ArraySearch;

/*
 * The structure below defines a variable, which associates a string name
 * with a string value.  Pointers to these structures are kept as the
 * values of hash table entries, and the name of each variable is stored
 * in the hash entry.
 */

typedef struct Var {
    int valueLength;		/* Holds the number of non-null bytes
				 * actually occupied by the variable's
				 * current value in value.string (extra
				 * space is sometimes left for expansion).
				 * For array and global variables this is
				 * meaningless. */
    int valueSpace;		/* Total number of bytes of space allocated
				 * at value.string.  0 means there is no
				 * space allocated. */
    union {
	char *string;		/* String value of variable, used for scalar
				 * variables and array elements.  Malloc-ed. */
	Tcl_HashTable *tablePtr;/* For array variables, this points to
				 * information about the hash table used
				 * to implement the associative array. 
				 * Points to malloc-ed data. */
	struct Var *upvarPtr;	/* If this is a global variable being
				 * referred to in a procedure, or a variable
				 * created by "upvar", this field points to
				 * the record for the higher-level variable. */
    } value;
    Tcl_HashEntry *hPtr;	/* Hash table entry that refers to this
				 * variable, or NULL if the variable has
				 * been detached from its hash table (e.g.
				 * an array is deleted, but some of its
				 * elements are still referred to in upvars). */
    int refCount;		/* Counts number of active uses of this
				 * variable, not including its main hash
				 * table entry: 1 for each additional variable
				 * whose upVarPtr points here, 1 for each
				 * nested trace active on variable.  This
				 * record can't be deleted until refCount
				 * becomes 0. */
    VarTrace *tracePtr;		/* First in list of all traces set for this
				 * variable. */
    ArraySearch *searchPtr;	/* First in list of all searches active
				 * for this variable, or NULL if none. */
    int flags;			/* Miscellaneous bits of information about
				 * variable.  See below for definitions. */
} Var;

/*
 * Flag bits for variables:
 *
 * VAR_ARRAY	-		1 means this is an array variable rather
 *				than a scalar variable.
 * VAR_UPVAR - 			1 means this variable just contains a
 *				pointer to another variable that has the
 *				real value.  Variables like this come
 *				about through the "upvar" and "global"
 *				commands.
 * VAR_UNDEFINED -		1 means that the variable is currently
 *				undefined.  Undefined variables usually
 *				go away completely, but if an undefined
 *				variable has a trace on it, or if it is
 *				a global variable being used by a procedure,
 *				then it stays around even when undefined.
 * VAR_TRACE_ACTIVE -		1 means that trace processing is currently
 *				underway for a read or write access, so
 *				new read or write accesses should not cause
 *				trace procedures to be called and the
 *				variable can't be deleted.
 */

#define VAR_ARRAY		1
#define VAR_UPVAR		2
#define VAR_UNDEFINED		4
#define VAR_TRACE_ACTIVE	0x10

/*
 * The structure below defines an argument to a procedure, which
 * consists of a name and an (optional) default value.
 */

typedef struct Arg {
    struct Arg *nextPtr;	/* Next argument for this procedure,
				 * or NULL if this is the last argument. */
    char *defValue;		/* Pointer to arg's default value, or NULL
				 * if no default value. */
    char name[4];		/* Name of argument starts here.  The name
				 * is followed by space for the default,
				 * if there is one.  The actual size of this
				 * field will be as large as necessary to
				 * hold both name and default value.  THIS
				 * MUST BE THE LAST FIELD IN THE STRUCTURE!! */
} Arg;

/*
 * The structure below defines a command trace.  This is used to allow Tcl
 * clients to find out whenever a command is about to be executed.
 */

typedef struct Trace {
    int level;			/* Only trace commands at nesting level
				 * less than or equal to this. */
    Tcl_CmdTraceProc *proc;	/* Procedure to call to trace command. */
    ClientData clientData;	/* Arbitrary value to pass to proc. */
    struct Trace *nextPtr;	/* Next in list of traces for this interp. */
} Trace;

#if (TCL_MINOR_VERSION == 4)
/*
 * The stucture below defines a deletion callback, which is
 * a procedure to invoke just before an interpreter is deleted.
 */

typedef struct DeleteCallback {
    Tcl_InterpDeleteProc *proc;	/* Procedure to call. */
    ClientData clientData;	/* Value to pass to procedure. */
    struct DeleteCallback *nextPtr;
				/* Next in list of callbacks for this
				 * interpreter (or NULL for end of list). */
} DeleteCallback;
#endif

/*
 * The structure below defines a frame, which is a procedure invocation.
 * These structures exist only while procedures are being executed, and
 * provide a sort of call stack.
 */

typedef struct CallFrame {
    Tcl_HashTable varTable;	/* Hash table containing all of procedure's
				 * local variables. */
    int level;			/* Level of this procedure, for "uplevel"
				 * purposes (i.e. corresponds to nesting of
				 * callerVarPtr's, not callerPtr's).  1 means
				 * outer-most procedure, 0 means top-level. */
    int argc;			/* This and argv below describe name and
				 * arguments for this procedure invocation. */
    char **argv;		/* Array of arguments. */
    struct CallFrame *callerPtr;
				/* Value of interp->framePtr when this
				 * procedure was invoked (i.e. next in
				 * stack of all active procedures). */
    struct CallFrame *callerVarPtr;
				/* Value of interp->varFramePtr when this
				 * procedure was invoked (i.e. determines
				 * variable scoping within caller;  same
				 * as callerPtr unless an "uplevel" command
				 * or something equivalent was active in
				 * the caller). */
} CallFrame;

/*
 * The structure below defines one history event (a previously-executed
 * command that can be re-executed in whole or in part).
 */

typedef struct {
    char *command;		/* String containing previously-executed
				 * command. */
    int bytesAvl;		/* Total # of bytes available at *event (not
				 * all are necessarily in use now). */
} HistoryEvent;

/*
 * The structure below defines a pending revision to the most recent
 * history event.  Changes are linked together into a list and applied
 * during the next call to Tcl_RecordHistory.  See the comments at the
 * beginning of tclHistory.c for information on revisions.
 */

typedef struct HistoryRev {
    int firstIndex;		/* Index of the first byte to replace in
				 * current history event. */
    int lastIndex;		/* Index of last byte to replace in
				 * current history event. */
    int newSize;		/* Number of bytes in newBytes. */
    char *newBytes;		/* Replacement for the range given by
				 * firstIndex and lastIndex (malloced). */
    struct HistoryRev *nextPtr;	/* Next in chain of revisions to apply, or
				 * NULL for end of list. */
} HistoryRev;

/*
 *----------------------------------------------------------------
 * This structure defines an interpreter, which is a collection of
 * commands plus other state information related to interpreting
 * commands, such as variable storage.  Primary responsibility for
 * this data structure is in tclBasic.c, but almost every Tcl
 * source file uses something in here.
 *----------------------------------------------------------------
 */

typedef struct Interp {

    /*
     * Note:  the first three fields must match exactly the fields in
     * a Tcl_Interp struct (see tcl.h).  If you change one, be sure to
     * change the other.
     */

    char *result;		/* Points to result returned by last
				 * command. */
    Tcl_FreeProc *freeProc;	/* Zero means result is statically allocated.
				 * If non-zero, gives address of procedure
				 * to invoke to free the result.  Must be
				 * freed by Tcl_Eval before executing next
				 * command. */
    int errorLine;		/* When TCL_ERROR is returned, this gives
				 * the line number within the command where
				 * the error occurred (1 means first line). */
    Tcl_HashTable commandTable;	/* Contains all of the commands currently
				 * registered in this interpreter.  Indexed
				 * by strings; values have type (Command *). */
    Tcl_HashTable mathFuncTable;/* Contains all of the math functions currently
				 * defined for the interpreter.  Indexed by
				 * strings (function names);  values have
				 * type (MathFunc *). */

    /*
     * Information related to procedures and variables.  See tclProc.c
     * and tclvar.c for usage.
     */

    Tcl_HashTable globalTable;	/* Contains all global variables for
				 * interpreter. */
    int numLevels;		/* Keeps track of how many nested calls to
				 * Tcl_Eval are in progress for this
				 * interpreter.  It's used to delay deletion
				 * of the table until all Tcl_Eval invocations
				 * are completed. */
    int maxNestingDepth;	/* If numLevels exceeds this value then Tcl
				 * assumes that infinite recursion has
				 * occurred and it generates an error. */
    CallFrame *framePtr;	/* Points to top-most in stack of all nested
				 * procedure invocations.  NULL means there
				 * are no active procedures. */
    CallFrame *varFramePtr;	/* Points to the call frame whose variables
				 * are currently in use (same as framePtr
				 * unless an "uplevel" command is being
				 * executed).  NULL means no procedure is
				 * active or "uplevel 0" is being exec'ed. */
    ActiveVarTrace *activeTracePtr;
				/* First in list of active traces for interp,
				 * or NULL if no active traces. */
    int returnCode;		/* Completion code to return if current
				 * procedure exits with a TCL_RETURN code. */
    char *errorInfo;		/* Value to store in errorInfo if returnCode
				 * is TCL_ERROR.  Malloc'ed, may be NULL */
    char *errorCode;		/* Value to store in errorCode if returnCode
				 * is TCL_ERROR.  Malloc'ed, may be NULL */

    /*
     * Information related to history:
     */

    int numEvents;		/* Number of previously-executed commands
				 * to retain. */
    HistoryEvent *events;	/* Array containing numEvents entries
				 * (dynamically allocated). */
    int curEvent;		/* Index into events of place where current
				 * (or most recent) command is recorded. */
    int curEventNum;		/* Event number associated with the slot
				 * given by curEvent. */
    HistoryRev *revPtr;		/* First in list of pending revisions. */
    char *historyFirst;		/* First char. of current command executed
				 * from history module or NULL if none. */
    int revDisables;		/* 0 means history revision OK;  > 0 gives
				 * a count of number of times revision has
				 * been disabled. */
    char *evalFirst;		/* If TCL_RECORD_BOUNDS flag set, Tcl_Eval
				 * sets this field to point to the first
				 * char. of text from which the current
				 * command came.  Otherwise Tcl_Eval sets
				 * this to NULL. */
    char *evalLast;		/* Similar to evalFirst, except points to
				 * last character of current command. */

    /*
     * Information used by Tcl_AppendResult to keep track of partial
     * results.  See Tcl_AppendResult code for details.
     */

    char *appendResult;		/* Storage space for results generated
				 * by Tcl_AppendResult.  Malloc-ed.  NULL
				 * means not yet allocated. */
    int appendAvl;		/* Total amount of space available at
				 * partialResult. */
    int appendUsed;		/* Number of non-null bytes currently
				 * stored at partialResult. */

    /*
     * A cache of compiled regular expressions.  See Tcl_RegExpCompile
     * in tclUtil.c for details.
     */

#define NUM_REGEXPS 5
    char *patterns[NUM_REGEXPS];/* Strings corresponding to compiled
				 * regular expression patterns.  NULL
				 * means that this slot isn't used.
				 * Malloc-ed. */
    int patLengths[NUM_REGEXPS];/* Number of non-null characters in
				 * corresponding entry in patterns.
				 * -1 means entry isn't used. */
    regexp *regexps[NUM_REGEXPS];
				/* Compiled forms of above strings.  Also
				 * malloc-ed, or NULL if not in use yet. */

#if (TCL_MINOR_VERSION >= 5)
    /*
     * Information about packages.  Used only in tclPkg.c.
     */

    Tcl_HashTable packageTable;	/* Describes all of the packages loaded
				 * in or available to this interpreter.
				 * Keys are package names, values are
				 * (Package *) pointers. */
    char *packageUnknown;	/* Command to invoke during "package
				 * require" commands for packages that
				 * aren't described in packageTable. 
				 * Malloc'ed, may be NULL. */
#endif

    /*
     * Information used by Tcl_PrintDouble:
     */

    char pdFormat[10];		/* Format string used by Tcl_PrintDouble. */
    int pdPrec;			/* Current precision (used to restore the
				 * the tcl_precision variable after a bogus
				 * value has been put into it). */

    /*
     * Miscellaneous information:
     */

    int cmdCount;		/* Total number of times a command procedure
				 * has been called for this interpreter. */
    int noEval;			/* Non-zero means no commands should actually
				 * be executed:  just parse only.  Used in
				 * expressions when the result is already
				 * determined. */
    int evalFlags;		/* Flags to control next call to Tcl_Eval.
				 * Normally zero, but may be set before
				 * calling Tcl_Eval.  See below for valid
				 * values. */
    char *termPtr;		/* Character just after the last one in
				 * a command.  Set by Tcl_Eval before
				 * returning. */
    char *scriptFile;		/* NULL means there is no nested source
				 * command active;  otherwise this points to
				 * the name of the file being sourced (it's
				 * not malloc-ed:  it points to an argument
				 * to Tcl_EvalFile. */
    int flags;			/* Various flag bits.  See below. */
    Trace *tracePtr;		/* List of traces for this interpreter. */
#if (TCL_MINOR_VERSION == 4)
    DeleteCallback *deleteCallbackPtr;
				/* First in list of callbacks to invoke when
				 * interpreter is deleted. */
#else
    Tcl_HashTable *assocData;	/* Hash table for associating data with
                                 * this interpreter. Cleaned up when
                                 * this interpreter is deleted. */
#endif
    char resultSpace[TCL_RESULT_SIZE+1];
				/* Static space for storing small results. */
} Interp;

/*
 * Flag bits for Interp structures:
 *
 * DELETED:		Non-zero means the interpreter has been deleted:
 *			don't process any more commands for it, and destroy
 *			the structure as soon as all nested invocations of
 *			Tcl_Eval are done.
 * ERR_IN_PROGRESS:	Non-zero means an error unwind is already in progress.
 *			Zero means a command proc has been invoked since last
 *			error occured.
 * ERR_ALREADY_LOGGED:	Non-zero means information has already been logged
 *			in $errorInfo for the current Tcl_Eval instance,
 *			so Tcl_Eval needn't log it (used to implement the
 *			"error message log" command).
 * ERROR_CODE_SET:	Non-zero means that Tcl_SetErrorCode has been
 *			called to record information for the current
 *			error.  Zero means Tcl_Eval must clear the
 *			errorCode variable if an error is returned.
 * EXPR_INITIALIZED:	1 means initialization specific to expressions has
 *			been carried out.
 */

#define DELETED			1
#define ERR_IN_PROGRESS		2
#define ERR_ALREADY_LOGGED	4
#define ERROR_CODE_SET		8
#define EXPR_INITIALIZED	0x10


EXTERN void		panic();
EXTERN void		TclDeleteVars _ANSI_ARGS_((Interp *iPtr,
			    Tcl_HashTable *tablePtr));
EXTERN int		TclGetFrame _ANSI_ARGS_((Tcl_Interp *interp,
			    char *string, CallFrame **framePtrPtr));

#endif /* _TCLINT */

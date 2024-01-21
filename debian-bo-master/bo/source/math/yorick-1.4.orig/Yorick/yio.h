/*
    YIO.H
    Declare Yorick I/O functions.

    $Id: yio.h,v 1.1 1993/08/27 18:32:09 munro Exp munro $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#ifndef YIO_H
#define YIO_H

#include "sysdep.h"
#include "hash.h"

/*--------------------------------------------------------------------------*/

#ifndef NO_STDIO_H
#include <stdio.h>
#endif

/* Here are the ANSI standard prototypes for all of the stdio.h functions
   used by Yorick.  The defintion of FILE is, of course, bogus,
   but FILE is normally used as an opaque pointer.
   (FILE really is a macro for SunOS, a typedef for HPUX.)
   More seriously, stdin, stdout, and stderr are normally macros, so
   it is unlikely that this substitute for stdio.h could ever be made
   viable...  */
#ifdef NO_STDIO_H
#define FILE struct Bogus_FILE
/* The following doesn't work because stdin, etc. are normally macros...  */
extern FILE *stdin, *stdout, *stderr;

/* Since these have non-int return types, they must be in <stdio.h>.  */
extern FILE *fopen(const char *filename, const char *mode);
extern char *fgets(char *s, int n, FILE *stream);

/* *********WARNING*********
   old SunOS (BSD?) declaration of sprintf is "char *sprintf()", so it is
   best to avoid using the return value from this function... */
extern int sprintf(char *s, const char *format, ...);

/* These are macros under SunOS (and BSD? UNIX in general?).  */
extern int feof(FILE *stream);
extern int ferror(FILE *stream);
extern void clearerr(FILE *stream);

#undef NON_ANSI_STDIO
#define NON_ANSI_STDIO
#endif

#ifdef NON_ANSI_STDIO
/* GCC on a Sun tends to spew out warnings about these not being explicitly
   declared in stdio.h.  */

extern void fclose(FILE *stream);
extern int fflush(FILE *stream);
extern int remove(const char *filename);
extern int rename(const char *oldname, const char *newname);

extern int fputs(const char *s, FILE *stream);

extern long fread(void *ptr, long size, long nobj, FILE *stream);
extern long fwrite(void *ptr, long size, long nobj, FILE *stream);

extern int fseek(FILE *stream, long offset, int origin);
extern long ftell(FILE *stream);

extern int printf(const char *format, ...);
extern int sscanf(char *s, const char *format, ...);
#endif

#ifndef SEEK_SET
/* These appear to be universal, but, of course, there's no guarantee...
   If they aren't defined in <stdio.h> and they aren't these values,
   then fseek probably doesn't exist and Yorick can't be built anyway...  */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

/*--------------------------------------------------------------------------*/

/* Yorick wrappers for fgets, feof, and ferror deal with case that
   stream==0, which is interpreted as a fancy case of stream==stdin.
   This allows more sophistocated input models than the standard C
   library.  The ability to not block X windows events is one example.  */
extern char *Yfgets(char *s, int n, FILE *stream);
extern int Yfeof(FILE *stream);
extern int Yferror(FILE *stream);

/* Yorick routines use the following virtual functions to do fgets and
   fputs to stdin, stdout, and stderr.  Other packages may replace the
   default routines when initializing if more sophistocated I/O is required,
   as for an X window graphics package.  */
extern char *(*YgetsIn)(char *s, int n);   /* called by Yfgets( , , 0) */
extern int (*YPrompt)(const char *s);      /* no \n appended, like fputs */
extern int (*YputsOut)(const char *s);     /* \n appended, like puts */
extern int (*YputsErr)(const char *s);     /* \n appended, like puts */
extern char *YDgetsIn(char *s, int n);     /* default versions */
extern int YDPrompt(const char *s);
extern int YDputsOut(const char *s);
extern int YDputsErr(const char *s);

/* X windows events may arrive in addition to stdin, so YDgetsIn
   has the following behavior:
   (1) Dispatch the next pending input event to its handler.
       *NOTE* THIS MUST BLOCK IF NO EVENTS OF ANY TYPE ARE PENDING.
   (2) If no input has arrived on stdin, loop to (1).
   (3) Return the input from stdin.
   This guarantees that X events can be processed while Yorick is waiting
   for input on stdin, but also guarantees that some input will be
   accepted from stdin.
   Yorick also requires the ability to "peek" to see if input is
   available, without blocking to wait if not.  It is acceptable for
   pending X events to be processed during these "peeks", but it is
   NOT acceptable to block if no input is available, as in (1) above.
   When Yorick wants to "peek" for keyboard input it does the following:
      YMaybeDispatch();
      if (yPendingIn) {
        ... next call to YgetsIn will not block
        YgetsIn(s, n);
        ...
      } else {
        ... continue processing which didn't want to wait
      }

   Most graphics packages (e.g.- GKS) do not allow for this behavior,
   but the following interface allows Gist graphics to both work
   with and be independent of Yorick.  The key is Gist's DispatchEvents
   and MaybeDispatch functions, with the ability to add stdin to the
   set of events that DispatchEvents selects.  For Yorick's part, the
   default interface routines are both trivial virtual functions, which
   can easily be replaced by a graphics (or other) package.  */

extern void (*YDispatch)(void);
extern void (*YMaybeDispatch)(void);
extern void YDDispatch(void);
extern void YDMaybeDispatch(void);

/* Yorick has a trivial keyboard event handler YDMaybeDispatch, which
   sets yPendingIn==1 if input is waiting on stdin, 0 if not.  An
   event handler called by a non-default YDispatch or YMaybeDispatch
   should set yPendingIn==1 when standard keyboard input arrives.
   In the case of YDispatch, the dispatching loop should exit
   when this happens, to allow YgetsIn to complete.  */
extern int yPendingIn;

/*--------------------------------------------------------------------------*/
/* The gets function is handy, but dicey if you aren't sure how long
   the input line is.  Ygets circumvents this problem at the cost of
   a new data type, the YgetsLine, which manages an input line buffer.
   Each call to Ygets returns either getsLine->line, or 0 if EOF or
   an error:
      if (!Ygets(&buf, file)) {
         if (Yferror(file))     { there really was a read error }
         else if (!Yfeof(file)) { no \n after MAX_LINE chars read }
         else                   { end-of-file (no characters first) }
      }
   The returned buf.line contains one line of input, with the trailing
   \n stripped (if EOF before EOL, nothing was stripped).
   Ygets keeps buf.max between MIN_LINE and BIG_LINE; if it is bigger
   than BIG_LINE initially, it is reallocated with length MIN_LINE.
   More characters are added to the buffer as needed in increments of
   INC_LINE.  If more than MAX_LINE characters are read without a \n,
   Ygets gives up, sets buf.line="", and returns 0.
   Ygets calls Yfgets, and so obeys the latter's convention for stream==0.
 */
typedef struct YgetsLine YgetsLine;
struct YgetsLine {
  char *line;    /* pointer to current line buffer, no '\n' */
  int max;       /* line created with StrAlloc(max) */
  int n;         /* strlen(line) */
};

extern char *Ygets(YgetsLine *getsLine, FILE *stream);

/*--------------------------------------------------------------------------*/

/* Filename gymnastics are inevitable, but system dependent (at least
   on non-UNIX platforms).  The following are intended to help.
   Free char* results with StrFree when finished.  */
extern char *YExpandName(const char *name);    /* return absolute pathname */
extern char *YNameTail(const char *name);
extern char *YNameHead(const char *name);           /* includes trailing / */
extern void YNameToHead(char **name);         /* ensure trailing / present */
extern int YIsAbsolute(const char *name);       /* Does name start with /? */
extern int YIsDotRelative(const char *name);  /* ., .., or with ./ or ../? */

extern char *YPATHstrtok(char *paths);   /* returns successive directories */

extern char *yCWD;      /* current working directory, including trailing / */
extern char *yHOME;     /* home directory from $HOME, including trailing / */

/* Use StrFree to get rid of return value from Ygetenv.  */
extern char *Ygetenv(const char *name);

/* YSetCWD returns non-0 if operation fails, resets yCWD on success.
   If name==0, just sets yCWD to ".".  */
extern int YSetCWD(const char *name);
extern void YGetHOME(void);  /* sets yHOME */

/*--------------------------------------------------------------------------*/

/* Scan for C-style escape sequences in quoted strings (e.g.- \n, \t),
   returning the (single character) value of the escape sequence, and,
   if the 2nd parameter is non-0, the character which stopped the scan.
   Thus, if s=="tXYZ", then YpEscapeSeq returns 9 (ASCII tab), and
   endp=="XYZ"; the same results would obtain if s=="011XYZ".  */
extern int YpEscapeSeq(const char *s, char **endp);

/*--------------------------------------------------------------------------*/
/* Yorick include files are managed using the following variables
   and functions.  */

/* ypIncludes-- the stack of currently open include files, maintain using
   YpPushInclude and YpClearIncludes */
typedef struct IncludeFile IncludeFile;
struct IncludeFile {
  FILE *file;
  char *filename;     /* expanded filename (allocated with StrAlloc) */
  long lastLineRead;  /* number of times Ygets has been called */
};

extern IncludeFile *ypIncludes;
extern int nYpIncludes;

/* YpPushInclude opens a file and pushes it onto the include stack.
      The very next input will be taken from that file.
   YpPush pushes a filename onto the pending input sources list.
      When all other sources of input are exhausted, Yorick will
      attempt to include these files in reverse order to the YpPush calls.
   YpClearIncludes closes all files on the current include stack, and
      forgets the contents of the pending input list.  */
extern FILE *YpPushInclude(const char *filename);
extern void YpPush(const char *filename);      /* filename will be copied */
extern FILE *YpPop(void);    /* call YpPushInclude with top of input list */
extern void YpClearIncludes(void);

/* sourceTab-- a permanent list of all files which have ever been included */
extern HashTable sourceTab;
extern long **sourceList;   /* maxItems array of pointers to lists of
			       globTab indices of func/struct/extern
			       definitions in corresponding source file */
extern long *lenSourceList; /* length of sourceList[i] is lenSourceList[i] */

/* Record the given globTab index in the sourceList.  This index
   corresponds to either a func definition, a struct definition, or an
   extern statement outside of any functions.  */
extern void RecordSource(long index);

extern long ypBeginLine;  /* source line number at which current parse
			     began (<=0 if input from keyboard) */

/* Given an index into globTab, OpenSource searches through the source
   lists to find the the source file in which the corresponding func,
   struct, or variable was defined, then returns the source file,
   positioned for read at the beginning of the definition.  The
   number of that line is in ypBeginLine.  */
extern FILE *OpenSource(long index);

/* Given a function, YpReparse searches through the source lists to find
   the the source file in which the corresponding func or struct was
   defined.  It then reparses this func or struct definition, replacing
   the corresponding globTab entry with the new definition, and
   returns a pointer to an error message string which begins with "*****"
   if YpReparse failed, otherwise gives the line number and filename
   of the function.  */
extern char *YpReparse(void *function);

/* ypPrefixes-- the ordered list of directories which Yorick will search
     to locate an include file (./ for CWD), set using YpSetPaths */
extern char **ypPrefixes;
extern int nYpPrefixes;

/* Yorick has two search paths for include files.  The first is the
   startup search path, which must allow std.i and other compiled-in
   package include files to be found.  After initialization, the
   normal search path is installed.  The first path is:
      yLaunchDir:ySiteDir:ySiteDir/contrib
   where yLaunchDir is the directory containing the executable, as
   determined at runtime, and ySiteDir is a compiled-in value (set
   by Codger from the value in the Makefile).  If yLaunchDir contains
   a paths.i file (the first file Yorick includes), that file may set
   the interpreted variable Y_SITE in order to change ySiteDir in both
   the first and second search paths.
   The second path is initialized in stdx.i to:
      .:~/Yorick:ySiteDir/include:ySiteDir/contrib
   This can be overridden by an interpreted command to set the search
   path (in, say, custom.i).  Alternatively, yLaunchDir can contain a
   special version of stdx.i.  */
extern void YpSetPaths(const char *pathlist);    /* also sets yCWD, yHOME */
extern char *yLaunchDir, *ySiteDir, *yHomeDir;

/*--------------------------------------------------------------------------*/

/* PrintFunc prints a string (as in the Y_print built-in function),
   breaking the result into multiple lines if required.  To accomplish this,
   PrintFunc remembers the most recent call to PermitLine and first flushes
   the line to that point.  If s still doesn't fit within printLength
   characters, then PrintFunc prints the first printLength-1, a '\',
   and so on until s is exhausted.  ForceNewline immediately flushes any
   pending line.
   PrintInit initializes PrintFunc (e.g.- PrintInit(YputsOut)).
   The Print and Y_print commands will not print more than
   maxPrintLines lines of output.  */
extern void PrintFunc(const char *s);
extern void PermitNewline(int nSpaces);
extern void ForceNewline(void);
extern void PrintInit(int (*puts)(const char *));
extern int printLength;     /* forced between 40 and 256 inclusive */
extern long maxPrintLines;  /* default 5000 */

/* formats used by Print and Y_print (be careful...) */
extern char *yCharFormat, *yShortFormat, *yIntFormat, *yLongFormat,
  *yFloatFormat, *yDoubleFormat, *yComplexFormat, *yPointerFormat;
extern void DefaultPrintFormat(int type);  /* type == (1<<T_CHAR)|etc */

extern char *ScanForEscape(char *s);
extern int AddEscapeSeq(char *s, int esc);

/*--------------------------------------------------------------------------*/

/* linked list of open files */
typedef struct IOFileLink IOFileLink;
struct IOFileLink {
  struct IOFileLink *next;
  struct IOFileLink **prev;  /* for unlinking only, can't go backwards */
  void *ios;                 /* either TextStream or IOStream */
};

extern IOFileLink *yTextFiles, *yBinaryFiles;

extern void AddIOLink(IOFileLink** list, void *ios);
extern void RemoveIOLink(IOFileLink* list, void *ios);

/*--------------------------------------------------------------------------*/

#endif

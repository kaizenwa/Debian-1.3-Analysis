/*
    YINPUT.C
    Implement Yorick program text reader.

    $Id: yinput.c,v 1.1 1993/08/27 18:32:09 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include "yio.h"
#include "parse.h"
#include "defstr.h"
#include "defmem.h"
#include "hash.h"

extern int yDebugLevel;        /* declared in ydata.h */
extern HashTable globalTable;  /* declared in ydata.h */

extern long ypBeginLine; /* defined in this file -- line number where
			    most recent parse began for YError */

extern int ypSkipIncludes; /* defined in this file -- flag set when
			      scanning for a variable definition */

extern char *YpPeekLine(void);  /* used by YError in task.c */
extern long YpStandby(void);    /* used by ScanForFunc in yorick.y */
extern long ScanForFunc(const char *fname, int notExtern);

extern char *MakeErrorLine(long lineNumber, const char *filename);

extern long YpLineNumber(void);

/*--------------------------------------------------------------------------*/

/* The context argument to YpNextLine determines how that routine will
   prompt for input, if input is from a terminal.
   NL_MAIN      -- read first line for parse
   NL_NOINPUT   -- no non-comment input yet,
                     read next line (if file) or return EOF (if terminal)
   NL_CONTINUE  -- need more input to complete program unit (cont>)
   NL_QUOTE     -- inside multi-line quoted string (quot>)
   NL_COMMENT   -- inside multi-line comment (comm>)
 */
/* Since NL_NOINPUT cannot result in a prompt being issued, its "spot"
   is used for the debugging mode prompt.  */
static char *prompts[]= { "> ", "dbug> ", "cont> ", "quot> ", "comm> " };

IncludeFile *ypIncludes= 0;
int nYpIncludes= 0;
static int maxYpIncludes= 0;

char **ypInputs= 0;
int nYpInputs= 0;
static int maxYpInputs= 0;

HashTable sourceTab;
long **sourceList= 0;
long *lenSourceList= 0;

char **ypPrefixes= 0;
int nYpPrefixes= 0;
static int maxYpPrefixes= 0;

static long rememberLine;  /* set in YpNextLine, used in YpStandby */

static int PutPrompt(int context);

static void ClearPrefixes(void);
static void AddPrefix(char *prefix);

static void ClearSourceList(const char *name);

static int GetNextLine(FILE *file, int context);

static long FindSource(long index);
extern long ReopenSource(long index, int notExtern);

void YpSetPaths(const char *pathlist)
{
  char *paths= StrCpy(pathlist);
  char *token= strtok(paths, PATH_SEP_DELIMIT);
  char *prefix;

  ClearPrefixes();

  /* crack colon-or-space-delimited list of directory pathnames */
  while (token) {
    if (YIsDotRelative(token)) prefix= StrCpy(token);
    else prefix= YExpandName(token);
    AddPrefix(prefix);
    token= strtok((char *)0, PATH_SEP_DELIMIT);
  }

  StrFree(paths);

  /* Set yCWD and yHOME if they haven't been initialized.  */
  if (!yCWD) YSetCWD((char *)0);
  if (!yHOME) YGetHOME();
}

static void ClearPrefixes(void)
{
  int i, n= nYpPrefixes;
  char **prefixes= ypPrefixes;
  nYpPrefixes= 0;
  for (i=0 ; i<n ; i++) StrFree(prefixes[i]);
  maxYpPrefixes= 0;
  ypPrefixes= 0;
  Yfree(prefixes);
}

static void AddPrefix(char *prefix)
{
  if (nYpPrefixes>=maxYpPrefixes) {
    int newSize= maxYpPrefixes+4;
    ypPrefixes= Yrealloc(ypPrefixes, sizeof(char *)*newSize);
    maxYpPrefixes= newSize;
  }
  YNameToHead(&prefix);
  ypPrefixes[nYpPrefixes++]= prefix;
}

/* state for YpError initialized in YpPushInclude */
static long prevErrLine;

static FILE *PushInclude(const char *filename, int clear);

FILE *YpPushInclude(const char *filename)
{
  return PushInclude(filename, 1);
}

static FILE *PushInclude(const char *filename, int clear)
{
  FILE *file= 0;
  char *name= 0;
  long i;

  if (YIsAbsolute(filename)) {
    /* absolute pathname doesn't need any prefix */
    file= fopen(filename, "r");
    if (!file) return 0;
    name= StrCpy(filename);

  } else {
    char *tmp;
    for (i=0 ; i<=nYpPrefixes ; i++) {
      if (i<nYpPrefixes) {
	tmp= StrCat(ypPrefixes[i], filename);
	name= YExpandName(tmp);
	StrFree(tmp);
      } else {
	name= YExpandName(filename);
	if (!YIsAbsolute(name)) break;
      }
      file= fopen(name, "r");
      if (file) break;
      StrFree(name);
    }
    if (!file) return 0;
  }

  if (nYpIncludes>=maxYpIncludes) {
    int newSize= maxYpIncludes+4;
    ypIncludes= Yrealloc(ypIncludes, sizeof(IncludeFile)*newSize);
    maxYpIncludes= newSize;
  }

  if (clear) ClearSourceList(name);

  ypIncludes[nYpIncludes].file= file;
  ypIncludes[nYpIncludes].filename= name;
  ypIncludes[nYpIncludes++].lastLineRead= 0;
  prevErrLine= -1;
  return file;
}

static int need_endif= 0;

void YpClearIncludes(void)
{
  need_endif= 0;
  if (nYpIncludes>0) {
    do {
      nYpIncludes--;
      /* Note <stdio.h> might not contain any prototype for fclose, or it
	 might contain the correct ANSI prototype, or it might contain
	 a K+R style function declaration...  */
      if (ypIncludes[nYpIncludes].file) fclose(ypIncludes[nYpIncludes].file);
      StrFree(ypIncludes[nYpIncludes].filename);
    } while (nYpIncludes);
  } else {
    nYpIncludes= 0;
  }
  YaltNextLine= 0;
  if (nYpInputs>0) do StrFree(ypInputs[--nYpInputs]); while (nYpInputs);
  else nYpInputs= 0;
}

static void ClearSourceList(const char *name)
{
  if (HashFind(&sourceTab, name, 0L)) {
    Yfree(sourceList[hashIndex]);
    sourceList[hashIndex]= 0;
    lenSourceList[hashIndex]= 0;
  }
}

/* Record the given globTab index in the sourceList.  This index
   corresponds to either a func definition, a struct definition, or an
   extern statement outside of any functions.  */
void RecordSource(long index)
{
  if (nYpIncludes) {
    long *list, len;
    if (HashAdd(&sourceTab, ypIncludes[nYpIncludes-1].filename, 0L)) {
      list= sourceList[hashIndex];
      len= lenSourceList[hashIndex];
    } else {
      HASH_MANAGE(sourceTab, long *, sourceList);
      HASH_MANAGE(sourceTab, long, lenSourceList);
      sourceList[hashIndex]= list= 0;
      lenSourceList[hashIndex]= len= 0;
    }
    if (!(len&7))
      sourceList[hashIndex]= list= Yrealloc(list, sizeof(long)*(len+8));
    list[len++]= index;
    lenSourceList[hashIndex]= len;
  }
}

void YpPush(const char *input)
{
  if (nYpInputs>=maxYpInputs) {
    int newSize= maxYpInputs+4;
    ypInputs= Yrealloc(ypInputs, sizeof(char *)*newSize);
    maxYpInputs= newSize;
  }
  ypInputs[nYpInputs++]= StrCpy(input);
}

FILE *YpPop(void)
{
  char *filename;
  FILE *file;
  if (nYpInputs<=0) return 0;
  filename= ypInputs[--nYpInputs];
  file= YpPushInclude(filename);
  if (!file) {
    char *msg;
    msg= StrCat("missing include file ", filename);
    YpError(msg);
    StrFree(msg);
  }
  StrFree(filename);
  return file;
}

/*--------------------------------------------------------------------------*/

long ypBeginLine= 0;
int ypSkipIncludes= 0;

extern int yImpossible;   /* used by YError, task.c */

static int needPrompt= 1;

static YgetsLine ypBuffer;
static int ypPeek= 0;
static void (*OrigYDispatch)(void)= 0;

char *(*YaltNextLine)(int context)= 0;

static int GetNextLine(FILE *file, int context)
{
  /* Handle case that YpPeekLine was just called (see task.c).  */
  if (ypPeek) {
    ypPeek= 0;
    return 1;
  }

  if (OrigYDispatch || yImpossible>3) {
    /* Previous call to Ygets caused fault to YError, which longjumped
       out of this routine.  Reset to the default YgetsIn in an attempt
       to avoid an infinite loop.  */
    if (YDispatch!=&YDDispatch)
      YputsErr("WARNING disabling graphics, try winkill if this persists");
    YDispatch= &YDDispatch;
  } else {
    OrigYDispatch= YDispatch;
  }

  if (!Ygets(&ypBuffer, file)) {
    if (YDispatch==&YDDispatch) YDispatch= OrigYDispatch;
    OrigYDispatch= 0;
    if (file) {
      int hadError= Yferror(file);
      int hadEOF= Yfeof(file);
      fclose(file);
      ypIncludes[nYpIncludes-1].file= 0;
      /* Any errors here are serious enough to warrant a panic stop.  */
      if (hadError)
	YError("****ABORTING PARSE**** error reading include file");
      if (!hadEOF)
	YError("****ABORTING PARSE**** include file not ASCII text");
    }
    return 0;			/* just a normal EOF */
  }
  if (YDispatch==&YDDispatch) YDispatch= OrigYDispatch;
  OrigYDispatch= 0;
  if (nYpIncludes) {
    long lnum= ++ypIncludes[nYpIncludes-1].lastLineRead;
    if (context==NL_MAIN || context==NL_NOINPUT) ypBeginLine= lnum;
  } else {
    if (context==NL_MAIN || context==NL_NOINPUT) ypBeginLine= 0;
    else ypBeginLine--;
  }
  return 1;
}

long YpLineNumber(void)
{
  if (nYpIncludes>0) return ypIncludes[nYpIncludes-1].lastLineRead-1;
  else return -1;
}

/* YpPeekLine is used by YError to "look ahead" one keyboard line
   to see whether to clear the stack or leave it for debugging.  */
char *YpPeekLine(void)
{
  ypPeek= 0;
  if (PutPrompt(-1) && GetNextLine((FILE *)0, NL_MAIN)) {
    ypPeek= 1;
    return ypBuffer.line;
  } else {
    ypPeek= 0;
    return 0;
  }
}

/* YpStandby returns the file position remembered before the previous
   line was read by YpNextLine.  */
long YpStandby(void)
{
  return rememberLine;
}

extern struct Function *y_idler_function;  /* see task.c */

static int PutPrompt(int context)
{
  /* return EOF to exit parser if previous line produced a parse error */
  if (ypErrors) return 0;

  /* exit parser if there is an idler function, like !yPendingIn below */
  if (y_idler_function && context==NL_MAIN) return -1;

  /* if previous line has already been read by YpPeekLine, stop now */
  if (ypPeek) return 1;

  if (needPrompt) {
    int iprompt= context;
    if (yDebugLevel) {
      if (iprompt==NL_MAIN) iprompt= NL_NOINPUT;
      else if (iprompt<0 && yDebugLevel>1) iprompt= NL_NOINPUT;
    } else {
      if (iprompt==NL_NOINPUT) iprompt= NL_MAIN;
    }
    if (iprompt<0) iprompt= NL_MAIN;
    YPrompt(prompts[iprompt]);
    /* Don't prompt again until response to this prompt arrives.
       Actually, this just guarantees one that exactly one prompt will
       be generated before Yorick READS each line of input.  If an
       input line is entered while Yorick is busy with the previous
       line, this prompt will appear AFTER the input was entered.       */
    needPrompt= 0;
  }
  if (context<0) context= NL_MAIN;  /* from YpPeekLine */

  /* return EOF to exit parser if no input last time */
  if (context==NL_NOINPUT) return 0;

  if (context==NL_MAIN) {
    /* Wait for standard input, but exit parse if some other input
       may have produced a task that Yorick can execute while waiting.  */
    YDispatch();
    if (!yPendingIn) return -1;
  } /* Else YgetsIn might wait for keyboard input, although input
       from other sources will still be dispatched to its handler. */

  needPrompt= 1;        /* finally got response to previous prompt */
  return 1;
}

char *YpNextLine(int context)
{
  FILE *file;
  char *line;

  /* In skip-includes mode, remember where each line begins for
     use by YpStandby.  */
  if (ypSkipIncludes && nYpIncludes && ypIncludes[nYpIncludes-1].file)
    rememberLine= ftell(ypIncludes[nYpIncludes-1].file);
  else
    rememberLine= -1;

  /* If there is an alternate input source, use it.  */
  if (YaltNextLine) {
    line= YaltNextLine(context);
    if (!line) YaltNextLine= 0;  /* "close" alternate input source */
    return line;
  }

  /* Get the current include file -- use file=0 for "standard input"
     instead of stdin, allowing Yorick to make more sophistocated
     arrangements in that case.  */
  if (nYpIncludes==0) {
    file= 0;
  } else do {
    if ((file= ypIncludes[nYpIncludes-1].file)) break;
    need_endif= 0;
    StrFree(ypIncludes[nYpIncludes-1].filename);
  } while (--nYpIncludes);
  if (!file && nYpInputs) do file= YpPop(); while (!file && nYpInputs);

  /* Handle prompt if input is not from include file.  */
  if (!file && PutPrompt(context)<=0) return 0;

  for (;;) {
    /* Get an arbitrary (okay, < MAX_LINE) length input line.  */
    if (!GetNextLine(file, context)) return 0;
    if (nYpIncludes && ypIncludes[nYpIncludes-1].lastLineRead==1 &&
	ypBuffer.line[0]=='#' && ypBuffer.line[1]=='!') {
      /* If first line of file begins with #! discard it.
	 This allows yorick to run UNIX scripts on most systems.  */
      if (!GetNextLine(file, context)) return 0;
    }

    /* Check whether this is an include line or an if line.
       Correct format is ([OWS] means optional whitespace characters):
       [OWS]#[OWS]include[OWS]"filename"[OWS]
       or
       [OWS]#[OWS]include[OWS]<filename>[OWS]
       or
     */
    if ((line=ypBuffer.line) && context<=NL_CONTINUE) {
      while (*line && (*line==' ' || *line=='\t' || *line=='\f')) line++;
      if (*line=='#' && line[1]) {
	char *incl= "include";
	line++;
	while (*line && (*line==' ' || *line=='\t')) line++;
	while (*line && *incl && (*line++ == *incl++));
	if (!*incl && context<NL_CONTINUE) {
	  char delim;
	  if (ypSkipIncludes) continue;
	  while (*line && (*line==' ' || *line=='\t')) line++;
	  delim= *line;
	  if (delim=='\"' || (delim=='<' && (delim='>'))) {
	    char *filename= ++line;
	    while (*line && *line!=delim) line++;
	    if (*line && line>filename) {
	      *line++= '\0';  /* 0-terminate filename */
	      while (*line && (*line==' ' || *line=='\t')) line++;
	      if (!*line) {
		char *msg;
		if ((file= YpPushInclude(filename))) continue;
		msg= StrCat("missing include file ", filename);
		YpError(msg);
		StrFree(msg);
		break;
	      }
	    }
	  }

	} else if (incl[-1]=='n' && line[-1]=='f' &&
		   (line[0]==' ' || line[0]=='\t') && file) {
	  /* this is #if line, maybe should skip to matching #endif */
	  line++;
	  while (*line && (*line==' ' || *line=='\t')) line++;
	  if ((line[0]=='0' || line[0]=='1')
	      && (!line[1] || line[1]==' ' || line[1]=='\t')) {
	    if (line[0]=='0') {
	      int count= 0;
	      for (;;) {
		if (!GetNextLine(file, context)) return 0;
		line= ypBuffer.line;
		while (*line && (*line==' ' || *line=='\t' || *line=='\f'))
		  line++;
		if (*line=='#') {
		  line++;
		  while (*line && (*line==' ' || *line=='\t')) line++;
		  if (line[0]=='i' && line[1]=='f' &&
		      (line[2]==' ' || line[2]=='\t')) {
		    count++;	/* nested #if (don't bother checking 0) */
		  } else {
		    char *endi= "endif";
		    while (*line && *endi && (*line++ == *endi++));
		    if (!*endi &&
			(!line[0] || line[0]==' ' || line[0]=='\t') &&
			!count--) break;
		  }
		}
	      }
	    } else {  /* #if 1 */
	      need_endif++;
	    }
	    continue;  /* read line after #endif or #if 1 */
	  }

	} else if (need_endif && incl[-1]=='i' && line[-1]=='e' &&
		   line[0]=='n' && line[1]=='d' && line[2]=='i' &&
		   line[3]=='f' &&
		   (!line[4] || line[4]==' ' || line[4]=='\t')) {
	  need_endif--;
	  continue;  /* read line after #endif */
	}
      }
      line= ypBuffer.line;
    }
    break;   /* continue only if a new include file has been opened */
  }

  yImpossible= 0;  /* tell YError that a line has come in */
  return line;
}

/*--------------------------------------------------------------------------*/

static char pErrorMsg[128];
static char *errorLoc= 0;

int ypErrors;        /* error count for most recent call to YpParse */
int ypMaxErrors= 16; /* give up after this many */

char *MakeErrorLine(long lineNumber, const char *filename)
{
  char *tmp= errorLoc;
  errorLoc= 0;
  StrFree(tmp);
  if (lineNumber<0) pErrorMsg[0]= '\0';
  else sprintf(pErrorMsg, "  LINE: %ld  FILE: ", lineNumber);
  errorLoc= StrCat(pErrorMsg, filename);
  return errorLoc;
}

extern int yBatchMode;  /* may be set with -batch, see std0.c */

void YpError(char *msg)
{
  if (ypSkipIncludes) return;  /* this is not a real parse */
  ypErrors++;
  strcpy(pErrorMsg, "SYNTAX: ");
  strncat(pErrorMsg, msg, 110);
  YputsErr(pErrorMsg);
  if (nYpIncludes) {
    long lineNumber= ypIncludes[nYpIncludes-1].lastLineRead;
    if (lineNumber!=prevErrLine) {
      char *filename= ypIncludes[nYpIncludes-1].filename;
      YputsErr(MakeErrorLine(lineNumber, filename));
      prevErrLine= lineNumber;
    }
  }
  if (yBatchMode || ypErrors>=ypMaxErrors) {
    ypErrors= 0;
    if (ypMaxErrors<1) ypMaxErrors= 1;
    YError("****ABORTING PARSE**** too many errors");
  }
}

/*--------------------------------------------------------------------------*/

static long FindSource(long index)
{
  long i, j, len, *list;

  /* Each source file i has an associated list of indices of the variables
     which were defined as func, struct, or *main* extern in that source
     file.  Search them in reverse order of the first time they were
     included -- note that this probably, but not necessarily, picks up
     the most recent definition.  This quirk constrains Yorick programming
     style somewhat -- you can't be as free in reusing a single function
     name as you might like...  */
  for (i=sourceTab.nItems-1 ; i>=0 ; i--) {
    list= sourceList[i];
    len= lenSourceList[i];
    /* inner loop is on variables defined in this source file */
    for (j=len-1 ; j>=0 ; j--) if (list[j]==index) break;
    if (j>=0) break;
  }

  return i;  /* -1 on failure */
}

long ReopenSource(long index, int notExtern)
{
  long source, position;
  FILE *file;

  source= FindSource(index);
  if (source<0) return -1;    /* source of func unknown */

  file= PushInclude(sourceTab.names[source], 0);
  if (!file) return -2;       /* unable to open source file */

  position= ScanForFunc(globalTable.names[index], notExtern);
  if (position<0 || fseek(file, position, SEEK_SET)) {
    fclose(file);
    ypIncludes[nYpIncludes-1].file= 0;
    if (position<0) return -3;  /* func no longer in source file */
    else return -4;             /* seek error */
  }

  return position;
}

FILE *OpenSource(long index)
{
  long position= ReopenSource(index, 0);
  FILE *file;
  if (position>=0) {
    file= ypIncludes[nYpIncludes-1].file;
    ypIncludes[nYpIncludes-1].file= 0;
  } else {
    file= 0;
  }
  return file;
}

/*--------------------------------------------------------------------------*/

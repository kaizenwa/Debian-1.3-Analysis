/*
    ASCIO.C
    Define standard Yorick built-in functions for ASCII I/O

    See std.i for documentation on the interface functions defined here.

    $Id: ascio.c,v 1.1 1993/08/27 18:32:09 munro Exp munro $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include "ydata.h"
#include "yio.h"
#include "defstr.h"
#include "defmem.h"

extern BuiltIn Y_open, Y_close, Y_read, Y_write, Y_sread, Y_swrite;
extern BuiltIn Y_rdline, Y_bookmark, Y_backup, Y_popen, Y_fflush;

extern char *MakeErrorLine(long lineNumber, const char *filename);

extern void YErrorIO(const char *msg);

/*--------------------------------------------------------------------------*/

static char *CheckBuf(long len);
static char *CrackScan(char *format);
static char *CrackPrint(char *format);
static int CrackFormat(char *format, char *(*Cracker)(char *));
static void AddFormat(char *format, int type, int danger);
static void FreeFormats(void);
static void CheckOps(int nArgs);
static char *NextInLine(Operand *op, long n);
static void ReadWorker(Operand *sourceOp, Symbol *stack, char *format);
static void WriteWorker(Operand *sinkOp, Symbol *stack, char *format);
static char *CheckOut(long len);

static char *fmtBuf= 0;
static long fmtLen= 0;
static int fmtType;
#define IO_NONE 0
#define IO_STRING 1
#define IO_LONG 2
#define IO_DOUBLE 3
#define IO_POINTER 4
#define IO_CHAR 5

struct FormatList { char *format; int type; int typeID; int danger; };
static struct FormatList *fmtList= 0;
static long fmtMax= 0;
static long fmtNow= 0;

static long fmtAssigns;
static long fmtWidth, fmtTotal;
static int fmtDanger;

static Operand *ioOps= 0;
static long maxIOops= 0;

static int typeMatch[]= { IO_LONG, IO_LONG, IO_LONG, IO_LONG,
		IO_DOUBLE, IO_DOUBLE, IO_NONE, IO_STRING, IO_POINTER };
static char *scanDefaults[]= { 0, "%s%n", "%ld%n", "%le%n" };
static char *printDefaults[]= { 0, " %s", " %8ld", " %#14.6g", " %8p" };

typedef int ScanFunc(Operand *op, char *format, char **text);
static ScanFunc CScanner, SScanner, IScanner, LScanner,
  FScanner, DScanner, QScanner;
static ScanFunc *Scanner[]= { &CScanner, &SScanner, &IScanner, &LScanner,
  &FScanner, &DScanner, 0, &QScanner };

static YgetsLine inputBuffer= { 0, 0, 0 };

static Dimension *pDims= 0;
static char *outBuf= 0;
static long outLen= 0;
static long lineSize;

typedef void PrtFunc(Operand *op, char *format, char *text);
static PrtFunc CPrinter, SPrinter, IPrinter, LPrinter,
  FPrinter, DPrinter, QPrinter, PPrinter;
static PrtFunc *Printer[]= { &CPrinter, &SPrinter, &IPrinter, &LPrinter,
  &FPrinter, &DPrinter, 0, &QPrinter, &PPrinter };
static PrtFunc CPrintC, SPrintC, IPrintC, LPrintC;
static PrtFunc *PrintC[]= { &CPrintC, &SPrintC, &IPrintC, &LPrintC };
static PrtFunc FPrintD, DPrintD;
static PrtFunc *PrintD[]= { &FPrintD, &DPrintD };

/*--------------------------------------------------------------------------*/

/* Two data types which are "foreign" to Yorick are defined in this
   file: the TextStream and the Bookmark.  */

extern PromoteOp PromXX;
extern UnaryOp ToAnyX, NegateX, ComplementX, NotX, TrueX;
extern BinaryOp AddX, SubtractX, MultiplyX, DivideX, ModuloX, PowerX;
extern BinaryOp EqualX, NotEqualX, GreaterX, GreaterEQX;
extern BinaryOp ShiftLX, ShiftRX, OrX, AndX, XorX;
extern BinaryOp AssignX, MatMultX;
extern UnaryOp EvalX, SetupX, PrintX;
extern MemberOp GetMemberX;

static UnaryOp PrintBM, PrintTX;

/* Implement text streams as a foreign Yorick data type.  */
struct TextStream {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  FILE *stream;        /* 0 indicates file has been closed */
  char *fullname;      /* filename after YExpandName */
  int permissions;     /* +1 read permission, +2 write permission
			  +4 append mode, +8 binary mode, +16 pipe */
  /* ------ begin specific text stream part ------- */
  long lastLineRead;   /* 1-origin line number of last line read */
  long readPosition;   /* file position (ftell) after lastLineRead */
  long lastPosition;   /* file position (ftell) before lastLineRead --
			  after backup, lastPosition==readPosition,
			  and lastPosition is not valid */
  int readWrite;       /* 0 initially, 1 after read, 2 after write */
  long fileID;         /* unique number used to recognize this file */
};

extern TextStream *NewTextStream(char *fullname,
				 void *stream, int permissions,
				 long line, long pos);
extern void FreeTextStream(void *ts);  /* ******* Use Unref(ts) ******* */

Operations textOps = {
  &FreeTextStream, T_OPAQUE, 0, T_STRING, "text_stream",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &EvalX, &SetupX, &GetMemberX, &MatMultX, &PrintTX
};

/* Implement bookmarks as a foreign Yorick data type.  */
typedef struct Bookmark Bookmark;
struct Bookmark {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  long lastLineRead;
  long lastPosition, readPosition;
  long fileID;
};

extern Bookmark *NewBookmark(long line, long last, long next, long id);
extern void FreeBookmark(void *bm);  /* ******* Use Unref(bm) ******* */

Operations bookOps = {
  &FreeBookmark, T_OPAQUE, 0, T_STRING, "bookmark",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &EvalX, &SetupX, &GetMemberX, &MatMultX, &PrintBM
};

/*--------------------------------------------------------------------------*/

void Y_open(int nArgs)
{
  Symbol *stack= sp-nArgs+1;
  char *filename, *filemode, *fullname;
  int errmode, permissions;
  FILE *file;

  if (nArgs<1 || nArgs>3) YError("bad argument list to open function");
  filename= YGetString(stack);
  if (nArgs<2) filemode= 0;
  else filemode= YGetString(stack+1);
  if (nArgs<3) errmode= 0;
  else errmode= YGetInteger(stack+2);

  if (!filemode || !filemode[0])
    filemode= "r";
  else if (filemode[0]!='r' && filemode[0]!='w' && filemode[0]!='a')
    YError("2nd argument to open must begin with r, w, or a");

  fullname= YExpandName(filename);
  file= fopen(fullname, filemode);

  if (file) {
    /* set permission bits and push result IOStream */
    if (filemode[0]=='r') permissions= 1;
    else if (filemode[0]=='w') permissions= 2;
    else permissions= 6;
    if (filemode[1]=='+') {
      if (filemode[2]=='b') permissions|= 11;
      else permissions|= 3;
    } else if (filemode[1]=='b') {
      if (filemode[2]=='+') permissions|= 11;
      else permissions|= 8;
    }
    if (permissions&8) {
      IOStream *ios= NewIOStream(fullname, file, permissions);
      PushDataBlock(ios);
      if (permissions&2) CLupdate(ios);
    } else {
      PushDataBlock(NewTextStream(fullname, file, permissions, 0L, 0L));
    }

  } else if (errmode) {
    /* fail silently if optional errmode flag is set */
    StrFree(fullname);
    permissions= 0;
    PushDataBlock(Ref(&nilDB));

  } else {
    /* blow up if optional errmode flag is not set */
    char *dots= strlen(filename)>40? "..." : "";
    char message[80];
    StrFree(fullname);
    sprintf(message, "cannot open file %.40s%s (mode %.6s)",
	    filename, dots, filemode);
    YErrorIO(message);
    return;
  }

  PopTo(sp-nArgs-1);
  Drop(nArgs);
  return;
}

void Y_close(int nArgs)
{
  DataBlock *db;
  IOStream *binary= 0;
  if (nArgs!=1) YError("close function takes exactly one argument");

  /* If argument is a simple variable reference, nil the variable.  */
  if (sp->ops==&referenceSym) {
    Symbol *s= &globTab[sp->index];
    ReplaceRef(sp);
    if (s->ops==&dataBlockSym &&
	(s->value.db->ops==&textOps || s->value.db->ops==&streamOps)) {
      s->ops= &intScalar;
      Unref(s->value.db);
      s->value.db= Ref(&nilDB);
      s->ops= &dataBlockSym;
    }
  }
  db= sp->value.db;

  if (db->ops==&textOps) {
    TextStream *text= (TextStream *)db;
    if (text->stream) {
#ifndef NO_POPEN
      if (text->permissions&16)
	pclose(text->stream);
      else 
#endif
	fclose(text->stream);
    }
    text->stream= 0;
  } else if (db->ops==&streamOps) {
    /* Make sure that the binary->stream gets closed, even if the IOStream
       itself is not freed by the Drop() below.  */
    if (db->references) binary= (IOStream *)db;
  } else if (db->ops!=&voidOps) {
    YError("bad argument type to close function");
  }

  if (db->references && db->ops!=&voidOps) {
    char message[80];
    sprintf(message, "%d outstanding references to closed file",
	    db->references);
    YWarning(message);
  }

  Drop(1);
  if (binary) {
    if (binary->stream) binary->ioOps->Close(binary);
    binary->stream= 0;
  }
}

/*--------------------------------------------------------------------------*/

void Y_read(int nArgs)
{
  Symbol *stack, *s;
  char *format, *keyNames[3];
  Symbol *keySymbols[2];
  Operand sourceOp;

  keyNames[0]= "format";
  keyNames[1]= "prompt";
  keyNames[2]= 0;
  stack= YGetKeywords(sp-nArgs+1, nArgs, keyNames, keySymbols);

  /* Get 1st argument if it is an IOStream or nil, otherwise will use
     nil stream (keyboard).  */
  if (stack>sp) YError("read function takes at least one argument");
  /* treat references gently -- don't want to suck reference to
     scalar int, long, or double onto the stack */
  if (stack->ops==&referenceSym) s= &globTab[stack->index];
  else s= stack;
  if (s->ops==&dataBlockSym &&
      (s->value.db->ops==&textOps || s->value.db->ops==&voidOps)) {
    stack->ops->FormOperand(stack, &sourceOp);
    stack++;
    if (sourceOp.ops==&voidOps) sourceOp.ops= 0;
    else {
      TextStream *ts= sourceOp.value;
      FILE *file= ts->stream;
      if (!file)
	YErrorIO("attempt to read from closed I/O stream");
      else if (!(ts->permissions & 1))
	YErrorIO("attempt to read from file opened in w or a mode");
      if (ts->readWrite&2 && fseek(file, ts->readPosition, SEEK_SET)) {
	clearerr(file);  /* don't prejudice future I/O attempts */
	YErrorIO("fseek failed to find current position in ASCII read");
      }
      ts->readWrite= 1;
    }
  } else {
    sourceOp.ops= 0;  /* special form recognized by NextInLine */
  }
  if (!sourceOp.ops) {
    /* set prompt string for NextInLine.  */
    sourceOp.value= keySymbols[1]? YGetString(keySymbols[1]) : "read> ";
    if (!sourceOp.value) sourceOp.value= "";
  }

  /* get format keyword, if any */
  format= keySymbols[0]? YGetString(keySymbols[0]) : 0;

  ReadWorker(&sourceOp, stack, format);
}

void Y_sread(int nArgs)
{
  Symbol *stack;
  char *format, *keyNames[2];
  Symbol *keySymbols[1];
  Operand sourceOp;

  keyNames[0]= "format";
  keyNames[1]= 0;
  stack= YGetKeywords(sp-nArgs+1, nArgs, keyNames, keySymbols);

  /* Get 1st argument if it is an IOStream or nil, otherwise will use
     nil stream (keyboard).  */
  if (stack>sp) YError("sread function takes at least one argument");
  stack->ops->FormOperand(stack, &sourceOp);
  if (sourceOp.ops!=&stringOps)
    YError("1st argument to sread must be source string or string array");
  stack++;

  /* get format keyword, if any */
  format= keySymbols[0]? YGetString(keySymbols[0]) : 0;

  ReadWorker(&sourceOp, stack, format);
}

static void ReadWorker(Operand *sourceOp, Symbol *stack, char *format)
{
  Symbol *s;
  char *text;
  Dimension *dims= 0;
  Operand *op;
  long i, number, lineCount;
  int j, nConversions, nArgs, typeID;

  /* crack format string */
  nConversions= CrackFormat(format, &CrackScan);

  /* First pass through arguments counts them, checks data types and
     conformability, and matches them to conversions in the format list.  */
  CheckOps((int)(sp-stack+1));
  nArgs= 0;
  for ( ; stack<=sp ; stack++) {
    if (!stack->ops) { /* skip keywords */
      stack++;
      continue;
    }
    if (stack->ops==&referenceSym &&
	globTab[stack->index].ops!=&dataBlockSym) s= &globTab[stack->index];
    else s= stack;
    op= &ioOps[nArgs];
    s->ops->FormOperand(s, op);
    typeID= op->ops->typeID;
    if (typeID>T_DOUBLE && typeID!=T_STRING)
      YError("read cannot handle non-array, complex, pointer, or structure");
    if (nArgs<nConversions) {
      if (typeMatch[typeID]!=fmtList[nArgs].type)
	YError("read format/read output data type mismatch");
    } else if (!nArgs && !nConversions && fmtNow==1) {
      format= fmtList[0].format;
      fmtList[0].format= StrCat(format, scanDefaults[typeMatch[typeID]]);
      StrFree(format);
    } else {
      AddFormat(scanDefaults[typeMatch[typeID]], typeMatch[typeID], 0);
    }
    fmtList[nArgs].typeID= typeID;
    if (nArgs) {
      Dimension *tmp= op->type.dims;
      while (tmp && dims && tmp->number==dims->number) {
	tmp= tmp->next;
	dims= dims->next;
      }
      if (tmp || dims)
	YError("all outputs from formatted read must have same dimensions");
    }
    dims= op->type.dims;
    nArgs++;
  }
  number= TotalNumber(dims);

  /* outer loop is on input array elements */
  lineCount= 0;
  text= NextInLine(sourceOp, lineCount++);
  fmtAssigns= 0;
  for (i=0 ; i<number ; i++) {
    /* inner loop is on arguments to be read */
    for (j=0 ; j<nArgs ; j++) {
      do {
	while (text && !text[0]) text= NextInLine(sourceOp, lineCount++);
	/* If input exhausted, NextInLine returns text==0.
	   If matching failure, Scanner returns text==0.  */

	/* Scanner returns zero if object was found, non-zero if not
	   found.  The text pointer is advanced past the number of
	   characters scanned, or set to zero if a matching failure
	   occurred.  */
      } while (Scanner[fmtList[j].typeID](&ioOps[j], fmtList[j].format,
					  &text));
    }
  }

  /* release excessive temporary space */
  FreeFormats();

  /* return total number of objects actually assigned */
  PushLongValue(fmtAssigns);
}

void Y_rdline(int nArgs)
{
  Symbol *stack;
  Operand op;
  long i, nLines= 0;
  Array *result;
  Dimension *dims;
  char *keyNames[2];
  Symbol *keySymbols[1];

  keyNames[0]= "prompt";
  keyNames[1]= 0;
  stack= YGetKeywords(sp-nArgs+1, nArgs, keyNames, keySymbols);

  for (nArgs=0 ; stack<=sp ; stack++) {
    if (!stack->ops) {
      stack++;
      continue;
    }
    if (nArgs==1) nLines= YGetInteger(stack);
    else if (nArgs==0) {
      stack->ops->FormOperand(stack, &op);
      if (op.ops!=&textOps && op.ops!=&voidOps)
	YError("1st argument to rdline function not a text stream or nil");
    } else {
      YError("rdline function takes exactly one or two arguments");
    }
    nArgs++;
  }

  if (!nArgs || op.ops==&voidOps) op.ops= 0;
  else {
    TextStream *ts= op.value;
    FILE *file= ts->stream;
    if (!file)
      YErrorIO("attempt to read from closed I/O stream");
    else if (!(ts->permissions & 1))
      YErrorIO("attempt to read from file opened in w or a mode");
    if (ts->readWrite&2 && fseek(file, ts->readPosition, SEEK_SET)) {
      clearerr(file);		/* don't prejudice future I/O attempts */
      YErrorIO("fseek failed to find current position in ASCII read");
    }
    ts->readWrite= 1;
  }
  if (!op.ops) {
    /* set prompt string for NextInLine.  */
    op.value= keySymbols[0]? YGetString(keySymbols[0]) : "read> ";
    if (!op.value) op.value= "";
  }

  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  if (nLines>0) tmpDims= NewDimension(nLines, 1L, tmpDims);
  else nLines= 1;
  result= PushDataBlock(NewArray(&stringStruct, tmpDims));

  for (i=0 ; i<nLines ; i++)
    result->value.q[i]= StrCpy(NextInLine(&op, i));
}

/*--------------------------------------------------------------------------*/

void Y_write(int nArgs)
{
  Symbol *stack;
  char *format, *keyNames[3];
  Symbol *keySymbols[2];
  Operand sinkOp;

  keyNames[0]= "format";
  keyNames[1]= "linesize";
  keyNames[2]= 0;
  stack= YGetKeywords(sp-nArgs+1, nArgs, keyNames, keySymbols);

  /* Get 1st argument if it is an IOStream or nil, otherwise will use
     nil stream (keyboard).  */
  if (stack>sp) YError("write function takes at least one argument");
  stack->ops->FormOperand(stack, &sinkOp);
  if (sinkOp.ops==&textOps) {
    TextStream *ts= sinkOp.value;
    FILE *file= ts->stream;
    stack++;
    if (!file)
      YErrorIO("attempt to write to closed I/O stream");
    else if (!(ts->permissions & 2))
      YErrorIO("attempt to write to file opened in r mode");
    if (ts->readWrite&1 && fseek(file, 0L, SEEK_END)) {
      clearerr(file);		/* don't prejudice future I/O attempts */
      YErrorIO("fseek failed to find current position in ASCII write");
    }
    ts->readWrite= 2;
  } else {
    if (sinkOp.ops==&voidOps) stack++;
    sinkOp.ops= 0;
  }

  /* get format keyword, if any */
  format= keySymbols[0]? YGetString(keySymbols[0]) : 0;

  /* get linesize keyword, if any */
  lineSize= keySymbols[1]? YGetInteger(keySymbols[1]) : 80;

  WriteWorker(&sinkOp, stack, format);
}

void Y_swrite(int nArgs)
{
  Symbol *stack;
  char *format, *keyNames[2];
  Symbol *keySymbols[1];

  keyNames[0]= "format";
  keyNames[1]= 0;
  stack= YGetKeywords(sp-nArgs+1, nArgs, keyNames, keySymbols);

  /* get format keyword, if any */
  format= keySymbols[0]? YGetString(keySymbols[0]) : 0;

  WriteWorker((Operand *)0, stack, format);
}

static void WriteWorker(Operand *sinkOp, Symbol *stack, char *format)
{
#ifdef FAKE_INTERRUPTS
  extern int check_except(void);
  extern short check_brk_flag;
#endif
  Dimension *dims;
  Operand *op;
  long i, number;
  int j, nConversions, nArgs, typeID, nChars, nLine;
  char *text;
  Array *result;  /* for swrite only (sinkOp==0) */
  FILE *file= (sinkOp && sinkOp->ops)? ((TextStream *)sinkOp->value)->stream :
                                       (FILE *)0;

  /* crack format string, set fmtTotal */
  nConversions= CrackFormat(format, &CrackPrint);

  dims= pDims;
  pDims= 0;
  FreeDimension(dims);

  /* First pass through arguments counts them, checks data types and
     conformability, and matches them to conversions in the format list.  */
  CheckOps((int)(sp-stack+1));
  nArgs= 0;
  for ( ; stack<=sp ; stack++) {
    if (!stack->ops) { /* skip keywords */
      stack++;
      continue;
    }
    op= &ioOps[nArgs];
    stack->ops->FormOperand(stack, op);
    typeID= op->ops->typeID;
    if (typeID>T_DOUBLE && typeID!=T_STRING && typeID!=T_POINTER)
      YError("write cannot handle non-array, complex, or structure");
    if (nArgs<nConversions) {
      int ftype= fmtList[nArgs].type;
      if (ftype==IO_CHAR) ftype= IO_LONG;
      if (typeMatch[typeID]!=ftype)
	YError("write format/write input data type mismatch");
    } else if (!nArgs && !nConversions && fmtNow==1) {
      format= fmtList[0].format;
      fmtList[0].format= StrCat(format, printDefaults[typeMatch[typeID]]);
      StrFree(format);
    } else {
      AddFormat(printDefaults[typeMatch[typeID]], typeMatch[typeID], 0);
    }
    fmtList[nArgs].typeID= typeID;
    if (typeID==T_STRING) {
      /* A string field might expand by as much as the length
	 of the longest string to be printed.  */
      char **q= op->value;
      long len;
      fmtWidth= 0;
      number= op->type.number;
      for (i=0 ; i<number ; i++)
	if (q[i] && (len= strlen(q[i]))>fmtWidth) fmtWidth= len;
      fmtTotal+= fmtWidth;
    } else {
      fmtTotal+= 25;  /* difficult to imagine longer numeric field */
    }
    if (nArgs) {
      if (Conform(pDims, op->type.dims) & 4)
	YError("all inputs to formatted write must be conformable");
      dims= pDims;
      pDims= Ref(tmpDims);
      FreeDimension(dims);
    } else {
      pDims= Ref(op->type.dims);
    }
    nArgs++;
  }
  number= TotalNumber(pDims);

  /* second pass broadcasts arguments to same size */
  for (j=0 ; j<nArgs ; j++) RightConform(pDims, &ioOps[j]);

  /* Make sure the outBuf has at least fmtTotal characters, which should
     be a very conservative estimate of the maximum number of characters
     required by the Printer output routines.  */
  CheckOut(fmtTotal>80? fmtTotal : 80);
  outBuf[0]= '\0';

  if (!sinkOp)
    /* this is swrite call -- it's time to create the result array */
    result= PushDataBlock(NewArray(&stringStruct, pDims));
  else
    result= 0;

  /* outer loop is on output array elements */
  nLine= 0;
  fmtTotal= 0;
  for (i=0 ; i<number ; i++) {
#ifdef FAKE_INTERRUPTS
    if (check_brk_flag) check_except();
#endif
    /* inner loop is on arguments to be written */
    text= outBuf;
    text[0]= '\0';
    nChars= 0;
    for (j=0 ; j<nArgs ; j++) {
      if (fmtList[j].type == IO_CHAR)
	PrintC[fmtList[j].typeID](&ioOps[j], fmtList[j].format, text);
      else if (fmtList[j].danger)
	PrintD[fmtList[j].typeID-T_FLOAT](&ioOps[j], fmtList[j].format, text);
      else
	Printer[fmtList[j].typeID](&ioOps[j], fmtList[j].format, text);
      nChars= (int)strlen(text);  /* can't rely on sprintf to return this */
      text+= nChars;
      fmtTotal+= nChars;
      nLine+= nChars;
    }

    /* one result printed/stored per array element */
    if (!sinkOp) {
      result->value.q[i]= StrCpy(outBuf);
    } else {
      /* YPrompt does NOT append \n, as desired (YputsOut does) */
      if (i && ((nArgs>nConversions && nArgs>1) || nLine>lineSize)) {
	if (sinkOp->ops) fputs("\n", file);
	else YPrompt("\n");
	fmtTotal++;
	nLine= strlen(outBuf);
      }
      if (sinkOp->ops) fputs(outBuf, file);
      else YPrompt(outBuf);
      if (nChars && text[-1]=='\n') nLine= 0;
    }
  }

  /* add trailing newline if it seems reasonable to do so */
  if (sinkOp && nArgs>nConversions) {
    if (sinkOp->ops) fputs("\n", file);
    else YPrompt("\n");
    fmtTotal++;
  }

  /* release excessive temporary space */
  FreeFormats();
  CheckOut(0L);

  /* if this is write (not swrite), result is character count */
  if (!result) PushLongValue(fmtTotal);
}

/*--------------------------------------------------------------------------*/

static long aFileID= 0;  /* unique file ID number for ASCII files */
IOFileLink *yTextFiles= 0;

/* Set up a block allocator which grabs space for 16 TextStream objects
   at a time.  Since TextStream contains several pointers, the alignment
   of an TextStream must be at least as strict as a void*.  */
static MemoryBlock txtsBlock= {0, 0, sizeof(TextStream),
				  16*sizeof(TextStream)};

TextStream *NewTextStream(char *fullname, void *stream, int permissions,
			  long line, long pos)
{
  TextStream *ios= NextUnit(&txtsBlock);
  FILE *file= stream;

  ios->references= 0;
  ios->ops= &textOps;
  ios->stream= file;
  ios->fullname= fullname;
  ios->permissions= permissions;
  ios->lastLineRead= line;
  ios->readPosition= ios->lastPosition= pos;
  if (file) fseek(file, pos, SEEK_SET);
  ios->readWrite= 0;
  ios->fileID= aFileID++;

  AddIOLink(&yTextFiles, ios);
  return ios;
}

void FreeTextStream(void *ios)
{
  TextStream *io= ios;
  FILE *stream= io->stream;
  if (stream) fclose(stream);
  StrFree(io->fullname);
  RemoveIOLink(yTextFiles, io);
  FreeUnit(&txtsBlock, io);
}

static char *txStatus[]=
  { "<illegal>", "read-only", "write-only", "read-write" };

static void PrintTX(Operand *op)
{
  TextStream *ts= op->value;
  long line= ts->lastLineRead+1;
  ForceNewline();
  if (ts->stream) {
    char text[32];
    sprintf(text, "%s text stream at:", txStatus[ts->permissions&3]);
    PrintFunc(text);
  } else {
    PrintFunc("text stream <closed> was:");
    line= 0;
  }
  ForceNewline();
  PrintFunc(MakeErrorLine(line, ts->fullname));
  ForceNewline();
}

/* Set up a block allocator which grabs space for 16 bookmark objects
   at a time.  Since Bookmark contains an ops pointer, the alignment
   of a Bookmark must be at least as strict as a void*.  */
static MemoryBlock bookBlock= {0, 0, sizeof(Bookmark),
				 16*sizeof(Bookmark)};

Bookmark *NewBookmark(long line, long last, long next, long id)
{
  Bookmark *bookmark= NextUnit(&bookBlock);
  bookmark->references= 0;
  bookmark->ops= &bookOps;
  bookmark->lastLineRead= line;
  bookmark->lastPosition= last;
  bookmark->readPosition= next;
  bookmark->fileID= id;
  return bookmark;
}

void FreeBookmark(void *bm)  /* ******* Use Unref(bm) ******* */
{
  FreeUnit(&bookBlock , bm);
}

static void PrintBM(Operand *op)
{
  Bookmark *bm= op->value;
  IOFileLink *iofl;
  for (iofl=yTextFiles ; iofl ; iofl=iofl->next)
    if (((TextStream *)iofl->ios)->fileID == bm->fileID) break;
  if (iofl) {
    TextStream *ts= iofl->ios;
    ForceNewline();
    PrintFunc("bookmark at:");
    ForceNewline();
    PrintFunc(MakeErrorLine(bm->lastLineRead+1, ts->fullname));
    ForceNewline();
  } else {
    PrintFunc("<lost bookmark>");
  }
}

void Y_bookmark(int nArgs)
{
  Operand op;
  TextStream *ios;
  Bookmark *bm;
  if (nArgs!=1) YError("bookmark function takes exactly one argument");
  sp->ops->FormOperand(sp, &op);
  ios= op.value;
  if (op.ops!=&textOps)
    YError("argument to bookmark function not a text stream");
  if (ios->permissions&16) YError("can't place a bookmark in a pipe");
  bm= PushDataBlock(NewBookmark(ios->lastLineRead, ios->lastPosition,
				ios->readPosition, ios->fileID));
}

void Y_backup(int nArgs)
{
  Operand op;
  TextStream *ios;
  Bookmark *bm= 0;
  FILE *file;

  if (nArgs!=1 && nArgs!=2)
    YError("backup function takes exactly one or two arguments");
  if (nArgs==2) {
    sp->ops->FormOperand(sp, &op);
    if (op.ops==&bookOps)
      bm= op.value;
    else if (op.ops!=&voidOps)
      YError("2nd argument to backup function is not nil or bookmark");
    Drop(1);
  }
  sp->ops->FormOperand(sp, &op);
  ios= op.value;
  if (op.ops!=&textOps)
    YError("1st argument to backup function not a text stream");
  if (ios->permissions&16) YError("can't backup a pipe");
  file= ios->stream;

  /* don't try to detect no-op, as side effect of this routine is
     to ensure that fseek is called to make a read operation
     legal (previous operation may have been a write) */

  if (bm) {
    /* reset state to bookmark */
    struct IOFileLink *iofl;
    for (iofl=yTextFiles ; iofl ; iofl=iofl->next)
      if (((TextStream *)iofl->ios)->fileID == bm->fileID) break;
    if (!iofl) YError("no file for bookmark passed to backup function");
    if (iofl->ios!=ios)
      YError("wrong file for bookmark passed to backup function");
    if (fseek(file, bm->readPosition, SEEK_SET))
      YErrorIO("fseek failed in backup function");
    ios->lastLineRead= bm->lastLineRead;
    ios->lastPosition= bm->lastPosition;
    ios->readPosition= bm->readPosition;

  } else {
    /* back up to previous line */
    if (fseek(file, ios->lastPosition, SEEK_SET))
      YErrorIO("fseek failed in backup function");
    ios->readPosition= ios->lastPosition;
    ios->lastLineRead--;
  }

  ios->readWrite= 1;   /* fseek equivalent to read here */
}

/*--------------------------------------------------------------------------*/

static char *NextInLine(Operand *op, long n)
{
  char *text;
  if (!op->ops) {
    char *prompt= op->value;
    YPrompt(prompt);
    /* This will not block other input sources if no keyboard input
       is available.  */
    text= Ygets(&inputBuffer, (FILE *)0);
  } else if (op->ops==&textOps) {
    TextStream *stream= op->value;
    FILE *file= stream->stream;
    text= Ygets(&inputBuffer, file);
    if (!text) {
      int hadError= Yferror(file);
      int hadEOF= Yfeof(file);
      clearerr(file);  /* don't prejudice later I/O attempts */
      if (hadError)
	YErrorIO("****ABORTING READ**** error reading input file");
      if (!hadEOF)
	YErrorIO("****ABORTING READ**** input file not ASCII text");
    }
    stream->lastLineRead++;
    stream->lastPosition= stream->readPosition;
    if (!(stream->permissions&16)) stream->readPosition= ftell(file);
  } else if (op->ops==&stringOps) {
    char **q= op->value;
    if (n<op->type.number) text= q[n];
    else text= 0;
  } else {
    YError("(BUG) impossible operand to NextInLine");
    text= 0;
  }
  return text;
}

/*--------------------------------------------------------------------------*/

#undef OPERATION
#define OPERATION(opname, type1, type2) \
static int opname(Operand *op, char *format, char **text) \
{ \
  type1 *x= op->value; \
  type2 v;  int i, n; \
  if (*text) { \
    i= sscanf(*text, format, &v, &n); \
    if (i==1) { *x= v; fmtAssigns++; *text+= n; i= 0; \
    } else if (i==0)    {         *text= 0;     i= 1; \
    } else {         *text+= strlen(*text);     i= 1; \
    } \
  } else { \
    *x= 0; i= 0; \
  } \
  if (!i) op->value= x+1; \
  return i; \
}

OPERATION(CScanner, unsigned char, long)
OPERATION(SScanner, short, long)
OPERATION(IScanner, int, long)
OPERATION(LScanner, long, long)

/* modify floating point read operations to cope with Fortran
   "D" exponent format */
static int retry_sscanf(char *text, char *format, double *pv, int *pn);
#undef OPERATION
#define OPERATION(opname, type1, type2) \
static int opname(Operand *op, char *format, char **text) \
{ \
  type1 *x= op->value; \
  type2 v;  int i, n; \
  if (*text) { \
    i= retry_sscanf(*text, format, &v, &n); \
    if (i==1) { *x= v; fmtAssigns++; *text+= n; i= 0; \
    } else if (i==0)    {         *text= 0;     i= 1; \
    } else {         *text+= strlen(*text);     i= 1; \
    } \
  } else { \
    *x= 0; i= 0; \
  } \
  if (!i) op->value= x+1; \
  return i; \
}

OPERATION(FScanner, float, double)
OPERATION(DScanner, double, double)

static int QScanner(Operand *op, char *format, char **text)
{
  char **x= op->value;
  char *v; int i, n;
  if (*x) { StrFree(*x); *x= 0; }
  if (*text) {
    long len= strlen(*text);
    v= CheckBuf(len);  /* allow enough space for worst case */
    i= sscanf(*text, format, v, &n);
    if (i==1) { *x= StrCpy(v); fmtAssigns++; *text+= n; i= 0;
    } else if (i==0)    {                 *text= 0;     i= 1;
    } else {                           *text+= len;     i= 1;
    }
  } else {
    i= 0;
  }
  if (!i) op->value= x+1;
  return i;
}

/* Try to recognize things like 1.234d-21 or 1.234D-21 that
   Fortran emits.
   The only reasonably cheap recourse is also a bit dangerous:
      We temporarily modify the text buffer and attempt a
      rescan.  Of course, if we're interrupted before we restore
      the modified text buffer, we may have modified the caller's
      text illegally.
   Hopefully, a significant performance penalty accrues only if
   the text you are reading contains things like "1.234e-21dumb".
   However, a file full of "1.234D-21" style numbers will take
   twice as long to read as one with ANSI C acceptable formats.  */
static int retry_sscanf(char *text, char *format, double *pv, int *pn)
{
  int i= sscanf(text, format, pv, pn);
  int n= *pn;
  char c= text[n];
  if (i==1 && (c=='D' || c=='d')) {
    /* this may represent only limited success if the scan
       stopped at a "d" or "D" character */
    text[n]= 'e';
    i= sscanf(text, format, pv, pn);
    text[n]= c;
  }
  return i;
}

/*--------------------------------------------------------------------------*/

#undef OPERATION
#define OPERATION(opname, type1, type2) \
static void opname(Operand *op, char *format, char *text) \
{ \
  type1 *x= op->value; \
  type2 v= *x;  op->value= x+1; \
  sprintf(text, format, v); \
}

OPERATION(CPrinter, unsigned char, long)
OPERATION(SPrinter, short, long)
OPERATION(IPrinter, int, long)
OPERATION(LPrinter, long, long)
OPERATION(FPrinter, float, double)
OPERATION(DPrinter, double, double)
OPERATION(PPrinter, void *, void *)

static void QPrinter(Operand *op, char *format, char *text)
{
  char **x= op->value;
  char *v= *x;  op->value= x+1;
  sprintf(text, format, v? v : "");
}

#undef OPERATION
#define OPERATION(opname, type1) \
static void opname(Operand *op, char *format, char *text) \
{ \
  type1 *x= op->value; \
  double v= *x;  op->value= x+1; \
  if (v>1.e15) v= 1.e15;  else if (v<-1.e15) v= -1.e15; \
  sprintf(text, format, v); \
}

OPERATION(FPrintD, float)
OPERATION(DPrintD, double)

#undef OPERATION
#define OPERATION(opname, type1) \
static void opname(Operand *op, char *format, char *text) \
{ \
  type1 *x= op->value; \
  int v= *x;  op->value= x+1; \
  sprintf(text, format, v); \
}

OPERATION(CPrintC, unsigned char)
OPERATION(SPrintC, short)
OPERATION(IPrintC, int)
OPERATION(LPrintC, long)

/*--------------------------------------------------------------------------*/

static void CheckOps(int nArgs)
{
  if (nArgs >= maxIOops) {
    long newMax= maxIOops+16;
    while (nArgs >= newMax) newMax+= 16;
    ioOps= Yrealloc(ioOps, sizeof(Operand)*newMax);
    maxIOops= newMax;
  }
}

static void AddFormat(char *format, int type, int danger)
{
  if (fmtNow >= fmtMax) {
    fmtList= Yrealloc(fmtList, sizeof(struct FormatList)*(fmtMax+16));
    fmtMax+= 16;
  }
  fmtList[fmtNow].format= StrCpy(format);
  fmtList[fmtNow].type= type;
  fmtList[fmtNow].typeID= -1;
  fmtList[fmtNow++].danger= danger;
}

static void FreeFormats(void)
{
  while (fmtNow) StrFree(fmtList[--fmtNow].format);
  if (fmtMax>32) {
    fmtList= Yrealloc(fmtList, sizeof(struct FormatList)*32);
    fmtMax= 32;
  }
  if (maxIOops>32) {
    ioOps= Yrealloc(ioOps, sizeof(Operand)*32);
    maxIOops= 32;
  }
}

static int CrackFormat(char *format, char *(*Cracker)(char *))
{
  int nConversions= 0;
  fmtTotal= 0;

  /* free fmtList left over from last time, if any */
  while (fmtNow) StrFree(fmtList[--fmtNow].format);

  /* Use either CrackScan or CrackPrint to split the format into pieces
     containing a single conversion specification corresponding to one
     item in the read or write argument list.  After this, either
     fmtNow==nConversions, or possibly fmtNow==1 and nConversions==0.  */
  if (format) {
    while (format[0]) {
      format= Cracker(format);
      if (fmtType!=IO_NONE) nConversions++;
      AddFormat(fmtBuf, fmtType, fmtDanger);
      fmtTotal+= fmtWidth;
    }
  }
  return nConversions;
}

static char *CheckBuf(long len)
{
  if (len+4 > fmtLen) {
    long newSize= 80*(1 + (len+4)/80);
    fmtBuf= Yrealloc(fmtBuf, newSize);
    fmtLen= newSize;
  } else if (fmtLen>80 && len+4<=80) {
    fmtBuf= Yrealloc(fmtBuf, 80L);
    fmtLen= 80L;
  }
  return fmtBuf;  /* used by CrackScan and CrackPrint for format strings */
}

static char *CheckOut(long len)
{
  if (len > outLen) {
    long newSize= 256*(1 + (len-1)/256);
    outBuf= Yrealloc(outBuf, newSize);
    outLen= newSize;
  } else if (outLen>512 && len<=512) {
    outBuf= Yrealloc(outBuf, 512L);
    outLen= 512L;
  }
  return outBuf;  /* used by Printer routines to hold sprintf results */
}

static char *CrackScan(char *format)
{
  int got_one;
  long i, n;
  char *part= CheckBuf(strlen(format));
  part[0]= '\0';
  fmtType= IO_NONE;
  got_one= 0;
  fmtWidth= 0;
  fmtDanger= 0;

  while (!got_one) { /* loop on conversion specifiers which do not assign */
    /* copy format until first conversion specifier */
    i= strcspn(format, "%");
    strncat(part, format, i);
    part+= i;
    format+= i;
    if (got_one || !format[0]) break;
    *part++= '%';
    *part= '\0';
    format++;

    /* find conversion type character */
    i= strcspn(format, "diouxXfeEgGs[cpn%");
    if (!format[i]) break;
    got_one= (format[i]!='%' && format[0]!='*');

    switch (format[i]) {
    case '%':
      i++;
      strncat(part, format, i);
      part+= i;
      format+= i;
      break;
    case '[':
      if (format[i+1]==']') i+= 2;
      else if (format[i+1]=='^' && format[i+2]==']') i+= 3;
      i+= strcspn(&format[i], "]");
    case 's':    /* actually can use case drop through here... */
      i++;
      strncat(part, format, i);
      part+= i;
      format+= i;
      fmtType= IO_STRING;
      break;
    case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
      /* all integers are handled as longs */
      if (format[i-1]=='h' || format[i-1]=='l') n= i-1;
      else n= i;
      strncat(part, format, n);
      part+= n;
      *part++= 'l';
      *part++= format[i];
      *part= '\0';
      format+= i+1;
      fmtType= IO_LONG;
      break;
    case 'e': case 'E': case 'f': case 'g': case 'G':
      /* all reals are handled as doubles */
      if (format[i-1]=='h' || format[i-1]=='l' ||
	  format[i-1]=='L') n= i-1;
      else n= i;
      strncat(part, format, n);
      part+= n;
      *part++= 'l';
      *part++= format[i];
      *part= '\0';
      format+= i+1;
      fmtType= IO_DOUBLE;
      break;
    case 'p':
      YError("Yorick read cannot handle %p format, use %i");
      break;
    case 'c':
      YError("Yorick read cannot handle %c format, use %1s or %1[...]");
      break;
    case 'n':
      YError("Yorick read cannot handle %n format");
      break;
    }
  }

  /* append final character count to be able to advance input pointer */
  if (got_one) strcat(part, "%n");

  return format;
}

static char *CrackPrint(char *format)
{
  int got_one;
  long i, n;
  char *part= CheckBuf(strlen(format));
  part[0]= '\0';
  fmtType= IO_NONE;
  got_one= 0;
  fmtWidth= 0;
  fmtDanger= 0;

  for (;;) { /* loop on conversion specifiers which do not eat arguments */
    /* copy format until first conversion specifier */
    i= strcspn(format, "%");
    while (format[i]=='%' && format[i+1]=='%')
      i+= 2+strcspn(format+i+2, "%");    /* skip %% immediately */
    strncat(part, format, i);
    part+= i;
    format+= i;
    if (got_one || !format[0]) break;
    *part++= '%';
    *part= '\0';
    format++;

    /* find conversion type character */
    i= strcspn(format, "diouxXfeEgGscpn");
    if (!format[i]) break;
    for (n=0 ; n<i ; n++) {
      if (format[n] == '*')
	YError("Yorick write cannot handle %*.* format, compute format");
      if (!fmtWidth && format[n]>='1' && format[n]<='9') {
	/* get minimum field width, if specified */
	fmtWidth= format[n]-'0';
	for (n++ ; n<i && format[n]>='0' && format[n]<='9' ; n++)
	  fmtWidth= 10*fmtWidth + format[n]-'0';
      }
    }
    got_one= 1;

    switch (format[i]) {
    case 's':
      i++;
      strncat(part, format, i);
      part+= i;
      format+= i;
      fmtType= IO_STRING;
      break;
    case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
      /* all integers are handled as longs */
      if (format[i-1]=='h' || format[i-1]=='l') n= i-1;
      else n= i;
      strncat(part, format, n);
      part+= n;
      *part++= 'l';
      *part++= format[i];
      *part= '\0';
      format+= i+1;
      fmtType= IO_LONG;
      break;
    case 'f':
      fmtDanger= 1;
    case 'e': case 'E': case 'g': case 'G':
      /* all reals are handled as doubles */
      if (format[i-1]=='L') n= i-1;
      else n= i;
      strncat(part, format, n);
      part+= n;
      *part++= format[i];
      *part= '\0';
      format+= i+1;
      fmtType= IO_DOUBLE;
      break;
    case 'c':
      i++;
      strncat(part, format, i);
      part+= i;
      format+= i;
      fmtType= IO_CHAR;
      break;
    case 'p':
      i++;
      strncat(part, format, i);
      part+= i;
      format+= i;
      fmtType= IO_POINTER;
      break;
    case 'n':
      YError("Yorick write cannot handle %n format, use strlen(swrite())");
      break;
    }
  }

  fmtWidth+= strlen(fmtBuf);
  return format;
}

/*--------------------------------------------------------------------------*/

void YErrorIO(const char *msg)
{
  extern int y_catch_category;
  y_catch_category= 0x02;
  YError(msg);
}

/*--------------------------------------------------------------------------*/

void Y_popen(int nArgs)
{
  char *command;
  int mode;
  TextStream *stream;
  FILE *file;
  if (nArgs!=2) YError("popen needs exactly two arguments");

#ifndef NO_POPEN

  command= YGetString(sp-1);
  mode= (int)YGetInteger(sp);
  Drop(1);
  file= popen(command, mode?"w":"r");
  if (!file) YError("system popen function failed");
  PushDataBlock(NewTextStream(StrCpy(command), file, mode?18:17, 0L, 0L));

#else
  YError("popen function not implemented on this platform");
#endif
}

void Y_fflush(int nArgs)
{
  TextStream *ts;
  Operand op;
  if (nArgs!=1) YError("fflush needs exactly one argument");
  sp->ops->FormOperand(sp, &op);
  if (sp->value.db->ops!=&textOps)
    YError("fflush only works for text files");
  ts= (TextStream *)sp->value.db;
  if (ts->stream) fflush(ts->stream);
}

/*--------------------------------------------------------------------------*/

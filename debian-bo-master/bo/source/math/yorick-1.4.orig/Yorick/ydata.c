/*
    YDATA.C
    Implement functions for Yorick-specific types of data.

    $Id: ydata.c,v 1.1 1993/08/27 18:32:09 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include "bcast.h"
#include "defmem.h"
#include "defstr.h"

/* Intended for use by the print() and grow() functions -- dangerous
   because it zeroes the contents of the source array to avoid
   having to deal with pointers.  */
extern Array *GrowArray(Array *array, long extra);

extern BuiltIn Y_yorick_stats, Y_symbol_def, Y_symbol_set;

/* Required for FetchLValue, StoreLValue */
extern void ReadGather(void *dst, void *srcM, long srcD, StructDef *base,
		       long number, const Strider *strider);
extern void WriteScatter(void *src, void *dstM, long dstD, StructDef *base,
			 long number, const Strider *strider);

/* function defined in yrdwr.c required for StoreLValue */
extern void SetSequentialWrite(IOStream *file, long last);

/*--------------------------------------------------------------------------*/

Array *GrowArray(Array *array, long extra)
{
  Array *result;
  StructDef *base= array->type.base;
  long number= array->type.number;
  Dimension *tmp= tmpDims;
  if (extra<=0) return array;
  if (!array->type.dims)
    array->type.dims= NewDimension(1L, 1L, (Dimension *)0);
  tmpDims= 0;
  FreeDimension(tmp);
  tmpDims= CopyDims(array->type.dims, (Dimension *)0, 1);
  tmpDims->number+= extra;
  result= NewArray(base, tmpDims);

  /* do direct copy of array to result, then ZERO array -- this avoids
     potential cost of pointer copies */
  memcpy(result->value.c, array->value.c, number*base->size);
  if (base->Copy!=&CopyX) memset(array->value.c, 0, number*base->size);
  return result;
}

Function *NewFunction(Symbol *consts, long nConsts, int nPos, int nKey,
		      int nLocal, long hasPL, int maxStackDepth,
		      Instruction *code, long codeSize)
{
  Function *func= Ymalloc(sizeof(Function)+codeSize*sizeof(Instruction));
  long frameSize= 1+nPos+(hasPL&1)+nKey+nLocal;
  Instruction *fcode= &func->code[frameSize];
  long i;
  func->references= 0;
  func->ops= &functionOps;
  func->constantTable= consts;
  func->nConstants= nConsts;
  func->nReq= frameSize+maxStackDepth+2;
  func->nPos= nPos;
  func->nKey= nKey;
  func->nLocal= nLocal;
  func->hasPosList= hasPL;
  codeSize-= frameSize-1;
  /* YpFunc puts the frame variables (parameters and locals) at the end
     of the code, switch them to the beginning now.  */
  for (i=0 ; i<codeSize ; i++) fcode[i]= code[i];
  code+= codeSize;
  fcode= func->code;
  for (i=0 ; i<frameSize ; i++) fcode[i]= code[i];
  return func;
}

void FreeFunction(void *v)  /* ******* Use Unref(func) ******* */
{
  Function *func= v;
  Symbol *cnst= func->constantTable;
  if (cnst) {  /* must free any string constants */
    long n= func->nConstants;
    while (n--) {
      if (cnst->ops==&dataBlockSym) Unref(cnst->value.db);
      cnst++;
    }
    Yfree(func->constantTable);
  }
  Yfree(func);
}

/* Set up a block allocator which grabs space for 64 range objects
   at a time.  Since Range contains an ops pointer, the alignment
   of a Range must be at least as strict as a void*.  */
static MemoryBlock rangeBlock= {0, 0, sizeof(Range),
				   64*sizeof(Range)};

Range *NewRange(long min, long max, long inc, int nilFlags)
{
  Range *range= NextUnit(&rangeBlock);
  range->references= 0;
  range->ops= &rangeOps;
  range->min= min;
  range->max= max;
  range->inc= inc;
  range->nilFlags= nilFlags;
  range->rf= 0;
  return range;
}

void FreeRange(void *range)  /* ******* Use Unref(range) ******* */
{
  FreeUnit(&rangeBlock , range);
}

/* Set up a block allocator which grabs space for 64 lvalue objects
   at a time.  Since LValue contains several pointers, the alignment
   of an LValue must be at least as strict as a void*.  */
static MemoryBlock lvalueBlock= {0, 0, sizeof(LValue),
				    64*sizeof(LValue)};

LValue *NewLValueD(long address, StructDef *base, Dimension *dims)
{
  long number= TotalNumber(dims);
  LValue *lvalue= NextUnit(&lvalueBlock);
  lvalue->references= 0;
  lvalue->ops= &lvalueOps;
  lvalue->owner= 0;
  lvalue->type.base= Ref(base);
  lvalue->type.dims= Ref(dims);
  lvalue->type.number= number;
  lvalue->address.d= address;
  lvalue->strider= 0;
  return lvalue;
}

LValue *NewLValueM(Array *owner, void *address,
		   StructDef *base, Dimension *dims)
{
  long number= TotalNumber(dims);
  LValue *lvalue= NextUnit(&lvalueBlock);
  lvalue->references= 0;
  lvalue->ops= &lvalueOps;
  lvalue->owner= Ref(owner);
  lvalue->type.base= Ref(base);
  lvalue->type.dims= Ref(dims);
  lvalue->type.number= number;
  lvalue->address.m= address;
  lvalue->strider= 0;
  return lvalue;
}

void FreeLValue(void *v)  /* ******* Use Unref(lvalue) ******* */
{
  LValue *lvalue= v;
  Unref(lvalue->owner);
  Unref(lvalue->type.base);
  FreeDimension(lvalue->type.dims);
  FreeStrider(lvalue->strider);
  FreeUnit(&lvalueBlock, lvalue);
}

/* Set up a block allocator which grabs space for 16 BIFunction
   objeccts at a time. */
static MemoryBlock bifBlock= {0, 0, sizeof(BIFunction),
		                 16*sizeof(BIFunction)};

BIFunction *NewBIFunction(BuiltIn *bi, long index)
{
  BIFunction *func= NextUnit(&bifBlock);
  func->references= 0;
  func->ops= &builtinOps;
  func->function= bi;
  func->index= index;
  return func;
}

static char warning[72];

void FreeBIFunction(void *bif)
{
  BIFunction *func= bif;
  strcpy(warning, "freeing builtin function ");
  strncat(warning,
	  func->index>=0? globalTable.names[func->index] : "<nameless>", 40L);
  YWarning(warning);
  FreeUnit(&bifBlock, func);
}

/*--------------------------------------------------------------------------*/

/* The basic idea of FetchLValue is to convert an LValue into an Array.
   This Array is created on the stack, but then popped into the given
   destination Symbol (usually the one holding the input LValue).  */
Array *FetchLValue(void *db, Symbol *dsts)
{
  LValue *lvalue= db;
  StructDef *base= lvalue->type.base;
  StructDef *model= base;
  IOStream *file= base->file;
  char *memory= file? 0 : lvalue->address.m;
  Array *darray;
  void *data;

  while (model->model) model= model->model;

  darray= PushDataBlock(NewArray(model, lvalue->type.dims));
  data= darray->value.c;

  ReadGather(data, memory, lvalue->address.d,
	     base, lvalue->type.number, lvalue->strider);
  if (file && file->pointeeList.table.nItems) ClearPointees(file, 0);

  PopTo(dsts);
  return darray;
}

/* StoreLValue stores the data in data to the specified LValue.
   This may require data conversion operations.  The input data
   MUST be of the type given by base->model->...->model, NOT base.  */
void StoreLValue(void *db, void *data)
{
  LValue *lvalue= db;
  StructDef *base= lvalue->type.base;
  IOStream *file= base->file;
  char *memory;
  long disk, number= lvalue->type.number;

  if (file) {
    disk= lvalue->address.d;
    memory= 0;  /* signal for WriteScatter to call WritePointees */
    if (base->addressType==2)
      SetSequentialWrite(file, disk+base->size*number);
  } else {
    memory= lvalue->address.m;
    disk= 0;
  }

  WriteScatter(data, memory, disk, base, number, lvalue->strider);
  if (file && file->pointeeList.table.nItems) ClearPointees(file, 1);
}

/*--------------------------------------------------------------------------*/

Symbol *globTab= 0;
HashTable globalTable;

long Globalize(const char *name, long n)
{
  if (!HashAdd(&globalTable, name, n)) {
    HASH_MANAGE(globalTable, Symbol, globTab);
    globTab[hashIndex].ops= &dataBlockSym;
    globTab[hashIndex].value.db= Ref(&nilDB);
  }
  return hashIndex;
}

long GlobalizeDB(const char *name, long n, void *db)
{
  long index= Globalize(name, n);
  if (globTab[index].ops==&dataBlockSym) {
    Unref(globTab[index].value.db);
    globTab[index].value.db= db;
  } else {
    globTab[index].value.db= db;
    globTab[index].ops= &dataBlockSym;
  }
  return index;
}

/*--------------------------------------------------------------------------*/

extern long yStackBlock, yStackSize;
long yStackBlock= 64;  /* number of stack elements to allocate at a time */
long yStackSize= 0;    /* current virtual machine stack size */

Symbol *spBottom= 0;   /* bottom of virtual machine stack */
Symbol *sp= 0;         /* current top of stack */
/* spBottom may change if the stack must be lengthened by CheckStack -
   spBottom is an extern so that the comination sp-spBottom may be
   computed and saved to refer to a given stack element */

int CheckStack(int n)
{
  long nNow= sp-spBottom;
  long nRequired= nNow + n;
  if (nRequired >= yStackSize) {
    nRequired= yStackBlock*(1 + nRequired/yStackBlock);
    sp= Yrealloc(spBottom, sizeof(Symbol)*nRequired);
    if (!sp) YError("memory manager failed in CheckStack");
    spBottom= sp;
    spBottom->ops= &intScalar;
    spBottom->value.i= 0;
    sp+= nNow;
    yStackSize= nRequired;
    return 1;
  } else {
    return 0;
  }
}

void PushIntValue(int i)
{
  register Symbol *stack= sp+1;
  stack->ops= &intScalar;
  stack->value.i= i;
  sp= stack;           /* sp updated AFTER new stack element intact */
}

void PushLongValue(long l)
{
  register Symbol *stack= sp+1;
  stack->ops= &longScalar;
  stack->value.l= l;
  sp= stack;           /* sp updated AFTER new stack element intact */
}

void PushDoubleValue(double d)
{
  register Symbol *stack= sp+1;
  stack->ops= &doubleScalar;
  stack->value.d= d;
  sp= stack;           /* sp updated AFTER new stack element intact */
}

int PushCopy(Symbol *s)
{
  register int isDB= (s->ops==&dataBlockSym);
  register Symbol *stack= sp+1;
  stack->ops= s->ops;
  if (isDB) stack->value.db= Ref(s->value.db);
  else stack->value= s->value;
  sp= stack;           /* sp updated AFTER new stack element intact */
  return isDB;
}

void *PushDataBlock(void *db)
{
  register Symbol *stack= sp+1;
  stack->ops= &dataBlockSym;
  stack->value.db= db;      /* does NOT increment reference counter */
  sp= stack;           /* sp updated AFTER new stack element intact */
  return db;
}

void Drop(int n)
{
  register Symbol *stack;
  while (n--) {
    stack= sp--;
    /* sp decremented BEFORE stack element is deleted */
    if (stack->ops==&dataBlockSym) Unref(stack->value.db);
  }
}

void PopTo(Symbol *s)
{
  DataBlock *old= s->ops==&dataBlockSym? s->value.db : 0;
  Symbol *stack= sp--;
  s->value= stack->value;
  s->ops= stack->ops;
  Unref(old);
}

/*--------------------------------------------------------------------------*/

void ReplaceRef(Symbol *stack)
{
  Symbol *ref;
  if (stack->ops!=&referenceSym) return;
  ref= &globTab[stack->index];
  if (ref->ops==&dataBlockSym) stack->value.db= Ref(ref->value.db);
  else stack->value= ref->value;
  stack->ops= ref->ops;     /* change ops only AFTER value updated */
}

/*--------------------------------------------------------------------------*/

static int EvenConform(Dimension *ldims, Dimension *rdims);
static void BdCast(Operand *op, Dimension *ddims);

/* Conform returns a bit mask indicating the status of the conformability
   test:
   1  - right operand must be broadcast
   2  - left operand must be broadcast
   4  - not conformable
   8  - right operand has at least as many dimensions as left
   16 - left operand has at least as many dimensions as right   */
int Conform(Dimension *ldims, Dimension *rdims)
{
  int nl= CountDims(ldims);
  int nr= CountDims(rdims);
  int n= nl-nr;
  int casts;

  Dimension *prev, *next= tmpDims;
  tmpDims= 0;
  FreeDimension(next);

  if (n==0) {
    /* same number of dimensions */
    if (nl==0) return 24;  /* both scalar */
    casts= 24 | EvenConform(ldims, rdims);

  } else if (n>0) {
    /* left array is higher rank */
    casts= 16;
    while (n--) {
      if (ldims->number>1) casts|= 1;  /* must broadcast right operand */
      tmpDims= NewDimension(ldims->number, ldims->origin, tmpDims);
      ldims= ldims->next;
    }
    casts|= EvenConform(ldims, rdims);

  } else {
    /* right array is higher rank */
    casts= 8;
    while (n++) {
      if (rdims->number>1) casts|= 2;  /* must broadcast left operand */
      tmpDims= NewDimension(rdims->number, rdims->origin, tmpDims);
      rdims= rdims->next;
    }
    casts|= EvenConform(ldims, rdims);
  }

  /* tmpDims is reversed, switch it back (already did tmpDims!=0) */
  prev= 0;
  for (;;) {
    next= tmpDims->next;
    tmpDims->next= prev;
    if (!next) break;
    prev= tmpDims;
    tmpDims= next;
  }

  return casts;
}

static int EvenConform(Dimension *ldims, Dimension *rdims)
{
  long lnum, rnum;
  int casts= 0;
  while (ldims) {
    lnum= ldims->number;
    ldims= ldims->next;
    rnum= rdims->number;
    rdims= rdims->next;
    if (lnum!=rnum) {
      if (lnum==1) {
	casts|= 2;  /* must broadcast left operand */
	lnum= rnum;
      } else if (rnum==1) {
	casts|= 1;  /* must broadcast right operand */
      } else {
	casts|= 4;  /* not conformable */
	lnum= -lnum-rnum;  /* error marker */
      }
    }
    tmpDims= NewDimension(lnum, 1L, tmpDims);
  }
  return casts;
}

static void BdCast(Operand *op, Dimension *ddims)
{
  StructDef *base= op->type.base;
  Array *dst= PushDataBlock(NewArray(base, ddims));
  Broadcast(dst->value.c, ddims, op->value, op->type.dims, base);
  PopTo(op->owner);
  op->references= 0;
  op->type.dims= ddims;
  op->type.number= dst->type.number;
  op->value= dst->value.c;
}

int BinaryConform(Operand *l, Operand *r)
{
  int casts= Conform(l->type.dims, r->type.dims);
  if (casts&4) return 4;
  if (casts&2) BdCast(l, tmpDims);
  else if ((casts&16) && !l->references) casts|= 2;
  if (casts&1) BdCast(r, tmpDims);
  else if ((casts&8) && !r->references) casts|= 1;
  return casts & 7;
}

int RightConform(Dimension *ldims, Operand *r)
{
  int casts= Conform(ldims, r->type.dims);
  if (casts&6) return 4;
  else if (casts&1) BdCast(r, tmpDims);
  return 0;
}

/*--------------------------------------------------------------------------*/

extern VMaction DropTop;
int CalledAsSubroutine(void)
{
  return pc->Action==&DropTop;
}

long YGetInteger(Symbol *s)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  s->ops->FormOperand(s, &op);
  if (op.ops->typeID<=T_LONG && !op.type.dims) {
    op.ops->ToLong(&op);
  } else {
    YError("expecting scalar integer argument");
  }
  return *(long *)op.value;
}

double YGetReal(Symbol *s)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  s->ops->FormOperand(s, &op);
  if (op.ops->typeID<=T_DOUBLE && !op.type.dims) {
    op.ops->ToDouble(&op);
  } else {
    YError("expecting scalar real argument");
  }
  return *(double *)op.value;
}

char *YGetString(Symbol *s)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  s->ops->FormOperand(s, &op);
  if (op.ops->typeID!=T_STRING || op.type.dims)
    YError("expecting scalar string argument");
  return *(char **)op.value;
}

int YNotNil(Symbol *s)
{
  if (!s) return 0;  /* for use with YGetKeywords */
  if (s->ops==&referenceSym) ReplaceRef(s);
  return !(s->ops==&dataBlockSym && s->value.db==&nilDB);
}

Symbol *YGetKeywords(Symbol *stack, int nArgs, char **keyNames,
		     Symbol **symbols)
{
  int i;
  char *globName;
  Symbol *s0= 0;
  for (i=0 ; keyNames[i] ; i++) symbols[i]= 0;
  for ( ; nArgs>0 ; stack++, nArgs--) {
    if (stack->ops) {
      if (!s0) s0= stack;
      continue;
    }
    globName= globalTable.names[stack->index];
    for (i=0 ; keyNames[i] ; i++)
      if (strcmp(globName, keyNames[i])==0) break;
    if (!keyNames[i])
      YError("unrecognized keyword in builtin function call");
    symbols[i]= ++stack;
    nArgs--;
  }
  return s0? s0 : stack+1;
}

IOStream *YGetFile(Symbol *stack)
{
  IOStream *file;
  Operand op;
  op.ops= 0;
  if (stack->ops) stack->ops->FormOperand(stack, &op);
  if (op.ops!=&streamOps)
    YError("expecting binary file as function argument");
  file= op.value;
  return file;
}

/*--------------------------------------------------------------------------*/

/* Retrieve array arguments for foreign code wrappers,
   applying type conversion (modifies s) if necessary.
   -- Just cast YGetInteger, YGetReal for scalar arguments, and
      use YGetString for scalar strings.  */

char *YGet_C(Symbol *s, int nilOK, Dimension **dims)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  s->ops->FormOperand(s, &op);
  if (nilOK && op.ops==&voidOps) { if (dims) *dims= 0;  return 0;}
  op.ops->ToChar(&op);
  if (dims) *dims= op.type.dims;
  return op.value;
}

short *YGet_S(Symbol *s, int nilOK, Dimension **dims)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  s->ops->FormOperand(s, &op);
  if (nilOK && op.ops==&voidOps) { if (dims) *dims= 0;  return 0;}
  op.ops->ToShort(&op);
  if (dims) *dims= op.type.dims;
  return op.value;
}

int *YGet_I(Symbol *s, int nilOK, Dimension **dims)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  if (s->ops==&referenceSym && globTab[s->index].ops==&intScalar) {
    if (dims) *dims= 0;
    return &globTab[s->index].value.i;
  }
  s->ops->FormOperand(s, &op);
  if (nilOK && op.ops==&voidOps) { if (dims) *dims= 0;  return 0;}
  op.ops->ToInt(&op);
  if (dims) *dims= op.type.dims;
  return op.value;
}

long *YGet_L(Symbol *s, int nilOK, Dimension **dims)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  if (s->ops==&referenceSym && globTab[s->index].ops==&longScalar) {
    if (dims) *dims= 0;
    return &globTab[s->index].value.l;
  }
  s->ops->FormOperand(s, &op);
  if (nilOK && op.ops==&voidOps) { if (dims) *dims= 0;  return 0;}
  op.ops->ToLong(&op);
  if (dims) *dims= op.type.dims;
  return op.value;
}

float *YGet_F(Symbol *s, int nilOK, Dimension **dims)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  s->ops->FormOperand(s, &op);
  if (nilOK && op.ops==&voidOps) { if (dims) *dims= 0;  return 0;}
  op.ops->ToFloat(&op);
  if (dims) *dims= op.type.dims;
  return op.value;
}

double *YGet_D(Symbol *s, int nilOK, Dimension **dims)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  if (s->ops==&referenceSym && globTab[s->index].ops==&doubleScalar) {
    if (dims) *dims= 0;
    return &globTab[s->index].value.d;
  }
  s->ops->FormOperand(s, &op);
  if (nilOK && op.ops==&voidOps) { if (dims) *dims= 0;  return 0;}
  op.ops->ToDouble(&op);
  if (dims) *dims= op.type.dims;
  return op.value;
}

double *YGet_Z(Symbol *s, int nilOK, Dimension **dims)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  s->ops->FormOperand(s, &op);
  if (nilOK && op.ops==&voidOps) { if (dims) *dims= 0;  return 0;}
  op.ops->ToComplex(&op);
  if (dims) *dims= op.type.dims;
  return op.value;
}

char **YGet_Q(Symbol *s, int nilOK, Dimension **dims)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  s->ops->FormOperand(s, &op);
  if (nilOK && op.ops==&voidOps) { if (dims) *dims= 0;  return 0;}
  if (op.ops->typeID!=T_STRING) YError("expecting string argument");
  if (dims) *dims= op.type.dims;
  return op.value;
}

void **YGet_P(Symbol *s, int nilOK, Dimension **dims)
{
  Operand op;
  if (!s->ops) YError("unexpected keyword argument");
  s->ops->FormOperand(s, &op);
  if (nilOK && op.ops==&voidOps) { if (dims) *dims= 0;  return 0;}
  if (op.ops->typeID!=T_POINTER) YError("expecting pointer argument");
  if (dims) *dims= op.type.dims;
  return op.value;
}

int YGet_dims(const Dimension *dims, long *dlist, int maxDims)
{
  int i, n= CountDims(dims);
  for (i=1 ; i<=n ; i++) {
    if (n-i < maxDims) dlist[n-i]= dims->number;
    dims= dims->next;
  }
  return n;
}

long YGet_Ref(Symbol *s)
{
  if (s->ops!=&referenceSym)
    YError("expecting simple variable reference as argument");
  return s->index;
}

void YPut_Result(Symbol *s, long index)
{
  Symbol *sout= &globTab[index];
  if (sout->ops==&dataBlockSym) {
    sout->ops= &intScalar;
    Unref(sout->value.db);
  }
  if (s->ops==&dataBlockSym) sout->value.db= Ref(s->value.db);
  else sout->value= s->value;
  sout->ops= s->ops;
}

/*--------------------------------------------------------------------------*/

extern long y_net_malloc, y_net_blocks;   /* in defmem.c */

static long ExtraNilRefs(void)
{
  long i, expectNil= 0;
  Symbol *s;
  for (i=0 ; i<globalTable.nItems ; i++)
    if (globTab[i].value.db==&nilDB && globTab[i].ops==&dataBlockSym)
      expectNil++;
  for (s=spBottom ; s<=sp ; s++)
    if (s->value.db==&nilDB && s->ops==&dataBlockSym) expectNil++;
  return nilDB.references - expectNil;
}

void Y_yorick_stats(int nArgs)
{
  Array *result;
  Dimension *dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  tmpDims= NewDimension(14L, 1L, (Dimension *)0);
  result= PushDataBlock(NewArray(&longStruct, tmpDims));
  result->value.l[0]= globalTable.nItems;
  result->value.l[1]= yStackSize;
  result->value.l[2]= y_net_malloc;
  result->value.l[3]= y_net_blocks;
  result->value.l[4]= ExtraNilRefs();
  result->value.l[5]= charStruct.references;
  result->value.l[6]= shortStruct.references;
  result->value.l[7]= intStruct.references;
  result->value.l[8]= longStruct.references;
  result->value.l[9]= floatStruct.references;
  result->value.l[10]= doubleStruct.references;
  result->value.l[11]= complexStruct.references;
  result->value.l[12]= stringStruct.references;
  result->value.l[13]= pointerStruct.references;
}

void Y_symbol_def(int nArgs)
{
  long index;
  Symbol *spp;
  if (nArgs!=1) YError("symbol_def takes exactly one argument");
  if (!HashFind(&globalTable, YGetString(sp), 0L))
    YError("symbol_def name not in global symbol table");
  index= hashIndex;
  Drop(2);
  spp= sp+1;
  spp->ops= &referenceSym;
  spp->index= index;
  sp= spp;
  ReplaceRef(sp);
}

void Y_symbol_set(int nArgs)
{
  Symbol *glob, *spp= sp-1;
  if (nArgs!=2 ||
      !spp->ops) YError("symbol_set takes exactly two arguments");
  glob= &globTab[Globalize(YGetString(spp), 0L)];
  ReplaceRef(sp);
  /* following copied from Define function in ops3.c */
  if (sp->ops==&dataBlockSym) {
    Array *array= (Array *)sp->value.db;
    if (array->references && array->ops->isArray) {
      /* copy non-temporary arrays to avoid unexpected aliasing */
      Array *result= NewArray(array->type.base, array->type.dims);
      glob->value.db= (DataBlock *)result;
      array->type.base->Copy(array->type.base, result->value.c,
			     array->value.c, array->type.number);
    } else {
      if (array->ops==&lvalueOps) FetchLValue(array, sp);
      glob->value.db= Ref(sp->value.db);
    }
  } else {
    glob->value= sp->value;
  }
  glob->ops= sp->ops;
}

/*--------------------------------------------------------------------------*/

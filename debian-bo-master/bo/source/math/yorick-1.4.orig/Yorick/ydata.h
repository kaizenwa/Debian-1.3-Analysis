/*
    YDATA.H
    Declare structures and functions for Yorick's "private" data.

    $Id: ydata.h,v 1.1 1993/08/27 18:32:09 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#ifndef YDATA_H
#define YDATA_H

#include "hash.h"
#include "binio.h"

/* "Universal" data types -- arrays of numbers, text strings, and
   data structures compounded of these -- are declared in binio.h.
   This file contains declarations of Yorick's "private" data types
   -- interpreted functions, built-in functions, index ranges --
   the virtual function tables, Yorick's program stack, and the like.  */

/*--------------------------------------------------------------------------*/

/* The DataBlock and Symbol structures are fundamental to the
   Yorick interpreter.

   A DataBlock is a virtual base class (in C++ parlance) for storing
   information of an unknown data type.  The 2nd edition of K&R blesses
   the programming style I have adopted here in the last paragraph of
   section A8.3; namely, a struct is derived (in the sense of a C++
   derived class) from a DataBlock if its first members match the
   members of the generic DataBlock struct.  In this case, a DataBlock
   consists of a reference counter and an Operations table.  The
   Operations table is a virtual function table defining all of the
   operations whose precise meaning depends on the particular data
   type.  For example, addition of two ints is quite a different thing
   than addition of two doubles, so the Add member of the Operations
   table for an int and the Add member of the Operations table for a
   double will point to different functions.

   A Symbol represents an variable or an intermediate result in an
   expression evaluation; Yorick's global symtab and program stack
   consist of Symbols.  Symbols are distinct from DataBlocks in an
   attempt to speed up simple operations on common data types (that is,
   on ints, longs, and doubles), without drastically increasing the
   number of virtual functions.  Binary operations cause the conflict
   between table size and speed, since for a fast binary function, each
   possible ordered *pair* of operand types requires a separate function.
   Since there are only 4 different Symbol types, the OpTable virtual
   function table is not too unwieldy; with about 16 DataBlock types,
   a comparable Operations table would have to be 10 times as large.
 */

typedef struct DataBlock DataBlock;
struct DataBlock {
  int references;   /* reference counter */
  Operations *ops;  /* virtual function table */
};

typedef struct Symbol Symbol;

/* OpTable is required to define Symbol, and is itself defined below. */
typedef struct OpTable OpTable;     /* operations on a Symbol */

/* A range function takes an Array and an integer indicating which index
   of the array to operate on (0 for fastest varying, 1 for next, etc.).
   The result of the operation is placed on top of the stack, and the
   function returns 0 if the result Array (the result MUST be an Array)
   has the same rank as the input Array, 1 if the result array has
   reduced rank (the input index is missing in the result).  */
typedef int RangeFunc(Array *array, int index);  /* also in yio.h */

typedef void VMaction(void);
typedef union Instruction Instruction;
union Instruction {
  VMaction *Action;      /* do something */
  int count;             /* parameter count */
  long index;            /* index into global symbol table */
  long displace;         /* branch displacement */
  Symbol *constant;      /* pointer into function's constant table */
  RangeFunc *rf;
};

typedef union SymbolValue SymbolValue;
union SymbolValue {
  int i;            /* intScalar */
  long l;           /* longScalar */
  double d;         /* doubleScalar */
  DataBlock *db;    /* dataBlockSym */

  /* A fifth Symbol type, referenceSym, is used ONLY on the stack in
     function parameter lists, i.e.- as an argument to the Eval function
     (see array.c and fnctn.c).  On the stack, a Symbol with ops==0
     is used to mark keyword parameters and function return addresses
     as well (again, see array.c and fnctn.c).  */

  /* The offset and pc are used keywords and returnSym;
     offset is also used in referenceSym (see referenceSym below): */
  Instruction *pc;
  long offset;
};

struct Symbol {
  OpTable *ops;                             /* virtual function table */
  long index;   /* into global symtab (for replacing local variables) */
  SymbolValue value;            /* appropriate member selected by ops */
};

/*--------------------------------------------------------------------------*/

/* The structures derived from DataBlock are the objects the Yorick
   interpreter works with.

   Array, StructDef, and IOStream are DataBlocks defined in
   binio.h.  These are the types necessary for representing "universal"
   data in memory and on disk.

   Function is a list of virtual machine instructions, which results
   from parsing interpreter input.  The source file and line numbers
   corresponding to the function are recorded, although this information
   is only used for error messages.

   BIFunction points to a builtin (i.e.- compiled) function, which
   must have type BuiltIn.  A compiled function may be called from the
   Yorick interpreter by defining a wrapper of type BuiltIn which pulls
   its arguments off of the Yorick program stack, converts them to the
   appropriate types, then calls the compiled function.  The Codger
   code generator can generate simple wrapper functions automatically.
 */

/* LValue represents an object into which an Array may be stored, or
   from which an Array can be fetched.  It has a data type (StructDef),
   dimensions, and an optional list of Strider (see below) specifications
   to indicate how the data is to be extracted as a multidimensional
   subset of a larger object.  An LValue represents data on disk or in
   memory, according to whether its type is a disk or memory StructDef.

    LValues are used for 3 purposes:
    (1) As a temporary describing the result of an indexing operation.
        Further indexing, or other operations not immediately
	requiring data, may then procede without actually fetching
	the data.  This is crucial for extracting parts of complicated
	data structures (especially data in disk files), which
	often requires several member extractions and/or subarray
	specifications to get to the interesting data.
    (2) As an Array with "remote" data -- the result of a reshape
        operation.  This is Yorick's version of FORTRAN equivalence
	statements.
    (3) To point to data not owned by the Yorick interpreter --
        for example, a global data structure from a compiled routine,
	like a FORTRAN common block.
 */
typedef struct LValue LValue;
struct LValue {
  int references;   /* reference counter */
  Operations *ops;  /* virtual function table */
  Array *owner;     /* 0 if data is in disk file or unowned */
  Member type;      /* full data type of result, base x[]..[] */
  union {
    long d;         /* byte address on disk if file!=0 */
    char *m;        /* memory address if file==0 */
  } address;
  Strider *strider; /* hierachy of strides to take */
  /* If non-0, the strider MUST be set up to include a list element
     for any contiguous blocks of type->base.size;
     the Gather and Scatter routines in bcast.c, and the YRead and
     YWrite routines recognize that no loop is required for
     contiguous blocks. */
};

typedef struct Function Function;
struct Function {
  int references;     /* reference counter */
  Operations *ops;    /* pointer to virtual functions */
  long source;            /* index into ypSources */
  Symbol *constantTable;  /* constants used by this function */
  long nConstants;        /* length of coonstantTable */
  int nReq;               /* worst case number of stack elements required */
  int nPos, nKey, nLocal; /* number of positionals, keywords, and locals */
  long hasPosList;        /* bit 0- 0 unless declared with .. parameter
			     bits 1-30 set if that positional parameter
			     marked as an output */
  Instruction code[1];    /* virtual machine instructions begin here */
  /* First 1+nPos+hasPosList+nKey+nLocal instructions are code[i].index
     for function name (in definition), positional parameters,
     optional .. ("*va*") parameter, keyword parameters, and local variables.
     End of code is marked by code[i].Action==&Return, code[i+1].Action==0,
     code[i+2].index==(length of code).  */
};

/* Built-in functions are called with a single argument -- the number
   of actual parameters on the top of the stack (keyword parameters
   count as two).  The function should leave its result on the top of
   the stack.  As many other items may be left on the stack as
   necessary, but if more than the input parameters plus one scratch
   plus the return value are necessary, then the built-in must call
   CheckStack.  (The built-in may clean up the stack itself, as long
   as its result is at or above sp-n when it returns.)  */
typedef void BuiltIn(int);

typedef struct BIFunction BIFunction;
struct BIFunction {
  int references;     /* reference counter */
  Operations *ops;    /* pointer to virtual functions */
  BuiltIn *function;
  long index;         /* to globTab -- shorthand for function name */
};

/* TextStream is a "foreign" data block defined in ascio.c */
typedef struct TextStream TextStream;

/*--------------------------------------------------------------------------*/

/* Range     - A range triple min:max:inc or rf:min:max:inc

    Memory management for these objects is via a special block allocator
    for reasons of efficiency.
 */

typedef struct Range Range;
struct Range {
  int references;     /* reference counter */
  Operations *ops;    /* virtual function table */
  long min, max, inc; /* min:max:inc, inc guaranteed non-zero */
  int nilFlags;       /* + 1 if min is nil (:N)
			 + 2 if max is nil (N:)
			     Note: if inc is nil, inc==1
			 + 4 if marked index (+:min:max:inc)
			 + 8 if pseudo index (-:min:max:inc)
			 +16 if rubber index (..)
			 +32 if nullifying index, e.g.-  where(0)  */
#define R_MINNIL 1
#define R_MAXNIL 2
#define R_MARKED 4
#define R_PSEUDO 8
#define R_RUBBER 16
#define R_NULLER 32
  RangeFunc *rf;      /* possible range function,  rf:min:max:inc */
};

/*--------------------------------------------------------------------------*/

/* Operations on DataBlocks take Operand arguments -- this is an
   abbreviated sort of LValue, allowing reshaped Arrays (in LValues) or
   scalar Symbols to interact efficiently with DataBlocks.  The data
   type of an operand may mutate before it is actually used, hence
   an Operand includes the Symbol *owner, which will be updated as
   the Operand changes.  */
typedef struct Operand Operand;
struct Operand {
  Symbol *owner;
  Operations *ops;  /* NEVER &lvalueOps */
  int references;   /* 0 if owner points to temporary Array */
  Member type;      /* all 0 unless ops is an Array type */
  void *value;      /* 0 unless ops is an Array type */
};

typedef void StackOp(void);

struct OpTable {  /* virtual function table for Symbol */
  int id;  /* index into binary operations array (0-3) */
  Operand *(*FormOperand)(Symbol *owner, Operand *op);
  StackOp *ToChar, *ToShort, *ToInt, *ToLong, *ToFloat, *ToDouble, *ToComplex;
  StackOp *Negate, *Complement, *Not, *True;
  StackOp *Add[4], *Subtract[4], *Multiply[4], *Divide[4], *Modulo[4],
          *Power[4];
  StackOp *Equal[4], *NotEqual[4],
          *Greater[4], *Less[4], *GreaterEQ[4], *LessEQ[4];
  StackOp *ShiftL[4], *ShiftR[4];
  StackOp *Or[4], *And[4], *Xor[4];
};

/* Virtual function tables for the 5 Symbol types: */
extern OpTable intScalar;
extern OpTable longScalar;
extern OpTable doubleScalar;
extern OpTable dataBlockSym;
extern OpTable referenceSym;   /* referenceSym is not a "complete" Symbol
				  in the sense that the parser ensures it
				  never appears in a binary operation
			ops          - &referenceSym
			index        - to globTab entry
			value.offset - stack offset (for Return only)  */
extern OpTable returnSym;      /* returnSym is not a "complete" Symbol
				  in the sense that the parser ensures it
				  never appears in a binary operation
			ops      - &returnSym
			index    - (unused)
			value.pc - VM program counter (for Return only)  */
/* Keywords may also appear on the program stack--
   these are marked by ops==0.  */

/*--------------------------------------------------------------------------*/

/* Unary operators take an Operand*, perform the required operation,
   and replace the Symbol at op->owner with the result.  */
typedef void UnaryOp(Operand *op);

/* Binary operators take two Operand* representing the left and right
   operands, perform the required operation, and replace the Symbol
   at l->owner with the result.  The r->owner may be changed as well,
   if the right operand had to be type converted or broadcast.  */
typedef void BinaryOp(Operand *l, Operand *r);

/* Type promotion operators take two Operand*s, and do
   arithmetic promotion (using one stack element for protected
   scratch space).  Either the left or the right Symbol (but not both)
   is updated, and the operator returns the Operations* for the result
   type, or 0 if the required promotion operation was impossible.
   Legal types for promotion are: char, short, int, long, float, double,
   complex, for either Array or LValue (7 Array types plus LValue).
   Non-numeric types return dl->ops if the dl->ops==dr->ops, else 0.  */
typedef Operations *PromoteOp(Operand *l, Operand *r);

typedef void MemberOp(Operand *op, char *name);

struct Operations {                 /* virtual function table for DataBlock */
  void (*Free)(void *);  /* crucial member for Unref -- first in struct
			    to allow alternate Operations to be used */
  int typeID;     /* unique type ID number */
  int isArray;    /* 1 if this is one of the Array DataBlocks */
  int promoteID;  /* index into Promote array (0-7, 7 means illegal) */
  char *typeName; /* ASCII name describing this data type */
  PromoteOp *Promote[8];
  UnaryOp *ToChar, *ToShort, *ToInt, *ToLong, *ToFloat, *ToDouble, *ToComplex;
  UnaryOp *Negate, *Complement, *Not, *True;
  BinaryOp *Add, *Subtract, *Multiply, *Divide, *Modulo, *Power;
  BinaryOp *Equal, *NotEqual, *Greater, *GreaterEQ;
  BinaryOp *ShiftL, *ShiftR, *Or, *And, *Xor;
  BinaryOp *Assign;    /* WARNING- first parameter non-standard, see ops3.c */
  UnaryOp *Eval;       /* WARNING- parameter non-standard, see ops3.c */
  UnaryOp *Setup;      /* see array.c -- set up for array indexing */
  MemberOp *GetMember; /* WARNING- parameter non-standard, see ops3.c */
  BinaryOp *MatMult;   /* WARNING- non-standard semantics, see ops.c */
  UnaryOp *Print;      /* uses PrintFunc to output each line, see yio.h */
};

/* Virtual function tables for the DataBlock types: */
extern Operations charOps;
extern Operations shortOps;
extern Operations intOps;
extern Operations longOps;
extern Operations floatOps;
extern Operations doubleOps;
extern Operations complexOps;
extern Operations stringOps;
extern Operations pointerOps;
extern Operations structOps;

extern Operations rangeOps;
extern Operations lvalueOps;
extern Operations voidOps;
extern Operations functionOps;
extern Operations builtinOps;
extern Operations structDefOps;
extern Operations streamOps;

/*--------------------------------------------------------------------------*/

extern DataBlock nilDB;       /* Nil, or [], the one instance of a void.  */

extern Instruction *pc;       /* virtual machine program counter */
extern Symbol *sp;            /* virtual machine stack pointer */
extern Symbol *spBottom;      /* current bottom of stack */
extern HashTable globalTable; /* hash table for globTab symbols */
extern Symbol *globTab;       /* global symbol table, contains any
			         variable referenced by a Function */

/*--------------------------------------------------------------------------*/

/* typeIDs for the basic Array data types */
#define T_CHAR 0
#define T_SHORT 1
#define T_INT 2
#define T_LONG 3
#define T_FLOAT 4
#define T_DOUBLE 5
#define T_COMPLEX 6
#define T_STRING 7
#define T_POINTER 8
#define T_STRUCT 9

/* typeIDs for the non-Array data types */
#define T_RANGE 10
#define T_LVALUE 11
#define T_VOID 12
#define T_FUNCTION 13
#define T_BUILTIN 14
#define T_STRUCTDEF 15
#define T_STREAM 16

/* typeID for data types which are opaque to Yorick */
#define T_OPAQUE 17

/*--------------------------------------------------------------------------*/

extern LValue *NewLValueD(long address, StructDef *base, Dimension *dims);
extern LValue *NewLValueM(Array *owner, void *address,
			  StructDef *base, Dimension *dims);
extern void FreeLValue(void *lvalue);     /* *** Use Unref(lvalue) *** */

extern Function *NewFunction(Symbol *consts, long nConsts, int nPos, int nKey,
			     int nLocal, long hasPL, int maxStackDepth,
			     Instruction *code, long codeSize);
extern void FreeFunction(void *func);       /* *** Use Unref(func) *** */

extern Range *NewRange(long min, long max, long inc, int nilFlags);
extern void FreeRange(void *range);        /* *** Use Unref(range) *** */

extern BIFunction *NewBIFunction(BuiltIn *bi, long index);
extern void FreeBIFunction(void *bif);    /* *** Use Unref(bif) *** */

/*--------------------------------------------------------------------------*/

extern int yDebugLevel;

extern long Globalize(const char *name, long n);
extern long GlobalizeDB(const char *name, long n, void *db);

/* CheckStack ensures that at least n more elements are available at
   the top of the virtual machine stack.  It returns 1 if the stack had
   to be copied to get more space (so that sp changed), else 0.  */
extern int CheckStack(int n);
extern void PushIntValue(int i);
extern void PushLongValue(long l);
extern void PushDoubleValue(double d);
extern int PushCopy(Symbol *s);  /* returns 1 if s is DataBlock, else 0 */
extern void *PushDataBlock(void *db);  /* returns db */
extern void Drop(int n);
extern void PopTo(Symbol *s);

void ReplaceRef(Symbol *stack);

/* Conform sets 4 bit if not conformable, sets 1 bit if ldims==1 where
   rdims>1, sets 2 bit if rdims==1 where ldims>1.  The result dimension
   list is returned in tmpDims.  */
extern int Conform(Dimension *ldims, Dimension *rdims);
/* BinaryConform assures that the operands are conformable; either or
   both may be broadcast.  Result dimensions are left in tmpDims.
   RightConform is the same except an error is signaled if ldims would
   need to be broadcast.
   Both functions return 0 on success, 1 on failure.   */
extern int BinaryConform(Operand *l, Operand *r);
extern int RightConform(Dimension *ldims, Operand *r);

extern Array *FetchLValue(void *db, Symbol *dsts);
extern void StoreLValue(void *db, void *data);

/*--------------------------------------------------------------------------*/

extern BuiltIn *yBuiltIns[];
extern void *yGlobals[];
extern char *yLinkNames[];
extern char *initialIncludes[];
extern void YorickInit(BuiltIn **, void **, char **, char **);
extern int ym_argc;
extern char **ym_argv;

extern void PushTask(Function *task);

extern int CalledAsSubroutine(void);

/* Extract scalar integers, reals, and strings from the stack */
extern long YGetInteger(Symbol *s);
extern double YGetReal(Symbol *s);
extern char *YGetString(Symbol *s);

extern int YNotNil(Symbol *s);

/* Scan stack[0],...,stack[nArgs-1] for keywords with names in the
   0-terminated list keyNames, putting the corresponding Symbol*s in
   the array symbols, or 0 if the keyName was not found.  Returns the
   Symbol* of the first non-keyword in the input stack list, or 0 if
   none.  */
extern Symbol *YGetKeywords(Symbol *stack, int nArgs, char **keyNames,
			    Symbol **symbols);

extern IOStream *YGetFile(Symbol *stack);

/* Retrieve array arguments for foreign code wrappers,
   applying type conversion (modifies s) if necessary.
   If dims is non-zero, *dims is set to the Dimension * for the argument.
   -- Just cast YGetInteger, YGetReal for scalar arguments, and
      use YGetString for scalar strings.  */
extern char *YGet_C(Symbol *s, int nilOK, Dimension **dims);
extern short *YGet_S(Symbol *s, int nilOK, Dimension **dims);
extern int *YGet_I(Symbol *s, int nilOK, Dimension **dims);
extern long *YGet_L(Symbol *s, int nilOK, Dimension **dims);
extern float *YGet_F(Symbol *s, int nilOK, Dimension **dims);
extern double *YGet_D(Symbol *s, int nilOK, Dimension **dims);
extern double *YGet_Z(Symbol *s, int nilOK, Dimension **dims);
extern char **YGet_Q(Symbol *s, int nilOK, Dimension **dims);
extern void **YGet_P(Symbol *s, int nilOK, Dimension **dims);
/* Convenience routine YGet_dims converts a Dimension * linked list
   to a dimension list dlist --
   dlist[0] is the first dimension length, dlist[1] the second, and so on.
   The return value is the actual number of dimensions.
   No dimensions beyond dlist[maxDims-1] will be set, so dlist need have
   at most maxDims elements.
   Use the TotalNumber and CountDims functions to get just the length
   or number of dimensions of an array.  */
extern int YGet_dims(const Dimension *dims, long *dlist, int maxDims);
/* If a compiled function is to treat a parameter as an update or an
   output, call YPut_Result after you call the function.  The required
   index is obtained from YGet_Ref, which must be called BEFORE YGet_...
   gets the corresponding data:
      long index= YGet_Ref(sp);
      double *x= YGet_D(sp, 0, (Dimension **)0);
      your_function(sp);
      YPut_Result(sp, index);                */
extern long YGet_Ref(Symbol *s);
extern void YPut_Result(Symbol *s, long index);

/*--------------------------------------------------------------------------*/

#endif

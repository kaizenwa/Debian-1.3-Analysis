/*
    STD0.C
    Define various standard Yorick built-in functions declared in std.i

    See std.i for documentation on the functions defined here.

    $Id: std0.c,v 1.1 1993/08/27 18:32:09 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include "bcast.h"
#include "yio.h"
#include "defstr.h"
#include "defmem.h"

extern char *yVersion;
#ifndef Y_VERSION
#define Y_VERSION "<unknown>"
#endif
char *yVersion= Y_VERSION;

extern BuiltIn Y_yorick_init, Y_set_path, Y_reshape, Y_array, Y_structof,
  Y_dimsof, Y_orgsof, Y_sizeof, Y_numberof, Y_typeof, Y_nameof, Y_is_array,
  Y_is_func, Y_is_void, Y_is_range, Y_is_struct, Y_is_stream, Y_am_subroutine,
  Y_sin, Y_cos, Y_tan, Y_asin, Y_acos, Y_atan, Y_sinh, Y_cosh, Y_tanh,
  Y_exp, Y_log, Y_log10, Y_sqrt, Y_ceil, Y_floor, Y_abs, Y_sign, Y_conj,
  Y_min, Y_max, Y_sum, Y_avg, Y_allof, Y_anyof, Y_noneof, Y_nallof, Y_where,
  Y_get_cwd, Y_get_home, Y_cd, Y_get_env, Y_get_argv, Y_use_origins;

extern BuiltIn Y_set_site;

/*--------------------------------------------------------------------------*/

extern void InitStructDefs(void);  /* opsv.c */

extern Operand *FormOperandDB(Symbol *owner, Operand *op);
extern DataBlock *ForceToDB(Symbol *s);

extern void BuildDimList(Symbol *stack, int nArgs);  /* ops3.c */

extern int ym_argc;      /* set in main.c */
extern char **ym_argv;

extern void SetSignals(int flags);  /* in sysdep.c */

/*--------------------------------------------------------------------------*/

static int signalFlags= 31;  /* catch all signals by default */

extern void Ytimer(double *cpu, double *sys, double *wall);

static void YInit(BuiltIn **builtin, void **global,
		  char **name, char **include);

extern int YGetLaunchDir(char *argv0);  /* sysdep.c */
extern char *yLaunchDir;

void YorickInit(BuiltIn **builtin, void **global,
		char **name, char **include)
{
  YInit(builtin, global, name, include);
}

void Y_yorick_init(int nArgs)
{
  Drop(nArgs);
  YInit(yBuiltIns, yGlobals, yLinkNames, initialIncludes);
}

static char *defaultPath;
extern char *yUserPath;   /* defined in ycode.c */

static void YInit(BuiltIn **builtin, void **global,
		  char **name, char **include)
{
  long index;
  DataBlock *oldDB;
  double cpu, sys, wall;

  /* initialize Yorick timer */
  Ytimer(&cpu, &sys, &wall);

  /* initialize basic data types */
  InitStructDefs();                /* opsv.c */

  /* initialize built-in functions */
  while (*builtin) {
    index= Globalize(*name++, 0L);
    oldDB= globTab[index].ops==&dataBlockSym? globTab[index].value.db : 0;
    if (!oldDB || oldDB->ops!=&builtinOps ||
	((BIFunction *)oldDB)->function!=*builtin) {
      globTab[index].value.db= (DataBlock *)NewBIFunction(*builtin++, index);
      globTab[index].ops= &dataBlockSym;
      Unref(oldDB);
    }
  }

  /* initialize compiled variables */
  while (*global) {
    index= Globalize(*name++, 0L);
    oldDB= globTab[index].ops==&dataBlockSym? globTab[index].value.db : 0;
    globTab[index].value.db=
      (DataBlock *)NewLValueM((Array *)0, *global++,
			      &charStruct, (Dimension *)0);
    /* note: everything starts out as a scalar char at the correct address
             -- the reshape operation when the associated package include
	     file is read (below) will set the correct data type and
	     dimension list */
    globTab[index].ops= &dataBlockSym;
    Unref(oldDB);
  }

  /* set up to read user customizations
     If the first two arguments are -batch alternate.i, use alternate.i
     -- useful for scripts which must NOT risk user customization.  */
  if (ym_argc<2 || strcmp(ym_argv[1],"-batch")) {
    YpPush("custom.i");
  } else {
    extern int yBatchMode;
    yBatchMode= 1;  /* data loaded to zero in task.c */
    if (ym_argc==2) {
      ym_argc= 1;
    } else {
      int i= 3;
      YpPush(ym_argv[2]);
      while (i<ym_argc) { ym_argv[i-2]= ym_argv[i]; i++; }
      ym_argc-= 2;
    }
  }

  /* set up to read std.i and package include files */
  while (*include) YpPush(*include++);

  /* first file read is always paths.i, which may override ySiteDir */
  YpPush("paths.i");

  /* set startup include file search path:
        yLaunchDir:ySiteDir/startup:ySiteDir/contrib

     the switch to the normal include file search path is done in
     stdx.i, which is included just before custom.i
     -- create the default path here:
        .:~/Yorick:ySiteDir/include:ySiteDir/contrib  */
  YGetLaunchDir(ym_argv[0]);
  Y_set_site(0);

  SetSignals(signalFlags);
}

void Y_set_path(int nArgs)
{
  char *path;
  if (nArgs<1) path= defaultPath;
  else if (nArgs==1) path= YGetString(sp);
  else { YError("set_path takes at most one argument"); path= 0; }
  YpSetPaths(path);
}

void Y_set_site(int nArgs)
{
  /* This function should ONLY be called from paths.i, or by YInit  */

  char *path, *pathtmp, *site;
  Array *dirName;
  DataBlock *oldDB;
  long index;

  if (nArgs>2) YError("expecting zero, one, or two arguments");

  /* record Y_LAUNCH in global variable */
  index= Globalize("Y_LAUNCH", 0L);
  oldDB= globTab[index].ops==&dataBlockSym? globTab[index].value.db : 0;
  dirName= NewArray(&stringStruct, (Dimension *)0);
  globTab[index].value.db= (DataBlock *)dirName;
  globTab[index].ops= &dataBlockSym;
  Unref(oldDB);
  dirName->value.q[0]= StrCpy(yLaunchDir);

  if (nArgs>=1) {
    path= YGetString(sp-nArgs+1);
    if (nArgs==2) yHomeDir= YGetString(sp);
  } else {
#ifndef NO_FALLBACK_TEST
    FILE *test;
    path= StrCat(ySiteDir, "startup/std.i");
    test= fopen(path, "r");
    StrFree(path);
    if (test) {
      fclose(test);
    } else {
      /* ySiteDir is not correct, make last ditch effort to find it */
      path= StrCat(yLaunchDir, "../Yorick/startup/std.i");
      test= fopen(path, "r");
      StrFree(path);
      if (test) {
	fclose(test);
	ySiteDir= StrCat(yLaunchDir, "../Yorick/");
      }
    }
#endif
    path= ySiteDir;
  }

  /* record Y_VERSION in global variable */
  index= Globalize("Y_VERSION", 0L);
  oldDB= globTab[index].ops==&dataBlockSym? globTab[index].value.db : 0;
  dirName= NewArray(&stringStruct, (Dimension *)0);
  globTab[index].value.db= (DataBlock *)dirName;
  globTab[index].ops= &dataBlockSym;
  Unref(oldDB);
  dirName->value.q[0]= StrCpy(yVersion);

  /* record Y_HOME in global variable */
  index= Globalize("Y_HOME", 0L);
  oldDB= globTab[index].ops==&dataBlockSym? globTab[index].value.db : 0;
  dirName= NewArray(&stringStruct, (Dimension *)0);
  globTab[index].value.db= (DataBlock *)dirName;
  globTab[index].ops= &dataBlockSym;
  Unref(oldDB);
  dirName->value.q[0]= StrCpy(yHomeDir);
  YNameToHead(dirName->value.q);

  /* record Y_SITE in global variable */
  index= Globalize("Y_SITE", 0L);
  oldDB= globTab[index].ops==&dataBlockSym? globTab[index].value.db : 0;
  dirName= NewArray(&stringStruct, (Dimension *)0);
  globTab[index].value.db= (DataBlock *)dirName;
  globTab[index].ops= &dataBlockSym;
  Unref(oldDB);
  dirName->value.q[0]= StrCpy(path);
  YNameToHead(dirName->value.q);
  site= dirName->value.q[0];

  /* construct startup include file search path:
        yLaunchDir:ySiteDir/startup:ySiteDir/contrib        */
  if (yLaunchDir && strlen(yLaunchDir) &&
      strcmp(yLaunchDir, site)) pathtmp= StrCat(yLaunchDir, PATH_SEP);
  else pathtmp= 0;
  path= StrCat(pathtmp, site);
  StrFree(pathtmp);
  pathtmp= StrCat(path, "startup");
  StrFree(path);
  path= StrCat(pathtmp, PATH_SEP);
  StrFree(pathtmp);
  pathtmp= StrCat(path, site);
  StrFree(path);
  path= StrCat(pathtmp, "contrib");
  StrFree(pathtmp);

  /* set the initial include path */
  YpSetPaths(path);
  StrFree(path);

  /* the switch to the normal include file search path is done in
     stdx.i, which is included just before custom.i
     -- create the default path here:
        .:~/Yorick:ySiteDir/include:ySiteDir/contrib  */
  if (yUserPath)
    path= StrCat(yUserPath, PATH_SEP);
  else
    path= 0;
  /* if (yLaunchDir && yLaunchDir[0] &&
   *     strcmp(yLaunchDir, site)) {
   *   pathtmp= StrCat(path, yLaunchDir);
   *   StrFree(path);
   *   path= StrCat(pathtmp, "include" PATH_SEP);
   *   StrFree(pathtmp);
   * }
   */
  pathtmp= StrCat(path, site);
  StrFree(path);
  path= StrCat(pathtmp, "include" PATH_SEP);
  StrFree(pathtmp);
  pathtmp= StrCat(path, site);
  StrFree(path);
  path= StrCat(pathtmp, "contrib");
  StrFree(pathtmp);

  defaultPath= path;
}

/*--------------------------------------------------------------------------*/

void Y_reshape(int nArgs)
{
  Symbol *arg= sp-nArgs+1;
  long index;
  DataBlock *db;
  if (nArgs<1 || arg->ops!=&referenceSym)
    YError("first argument to reshape must be variable reference");

  index= arg->index;
  arg++;
  nArgs--;
  db= globTab[index].value.db;  /* might not be meaningful... */

  if (!nArgs) {
    /* reshape, var
       same as var=[], but works for LValues as well */
    globTab[index].value.db= Ref(&nilDB);
    if (globTab[index].ops==&dataBlockSym) { Unref(db); }
    else globTab[index].ops= &dataBlockSym;
    Drop(1);

  } else {
    StructDef *base= 0;
    void *address= 0;
    Array *owner= 0;
    LValue *result;

    if (arg->ops==&referenceSym) ReplaceRef(arg);
    if (arg->ops==&dataBlockSym) {
      DataBlock *adb= arg->value.db;
      if (adb->ops==&structDefOps) {
	/* reshape, var, ->type, dimlist
	   uses address of the current value of var */
	base= (StructDef *)adb;
	db= ForceToDB(&globTab[index]);
	if (db->ops==&lvalueOps) {
	  LValue *lvalue= (LValue *)db;
	  if (lvalue->type.base->file)
	    YError("cannot reshape a disk-resident variable");
	  address= lvalue->address.m;
	  owner= lvalue->owner;
	} else if (db->ops->isArray) {
	  Array *array= (Array *)db;
	  address= array->value.c;
	  owner= array;
	} else {
	  YError("cannot reshape non-array object");
	}

      } else if ((adb->ops==&pointerOps ||
		  adb->ops==&intOps || adb->ops==&longOps) &&
		 !((Array *)adb)->type.dims) {
	/* reshape, var, ->address, type, dimlist */
	Array *array= (Array *)adb;
	if (adb->ops==&pointerOps) {
	  address= array->value.p[0];
	  owner= Pointee(address);
	} else if (adb->ops==&intOps) {
	  address= (char *)0 + array->value.i[0];
	  owner= 0;
	} else {
	  address= (char *)0 + array->value.l[0];
	  owner= 0;
	}

      } else {
      badd:
	YError("reshape address must be scalar pointer, int, or long");
      }
    } else if (arg->ops==&intScalar) {
      /* reshape, var, ->address, type, dimlist */
      address= (char *)0 + arg->value.i;
      owner= 0;
    } else if (arg->ops==&longScalar) {
      /* reshape, var, ->address, type, dimlist */
      address= (char *)0 + arg->value.l;
      owner= 0;
    } else {
      goto badd;
    }

    arg++;
    nArgs--;
    if (!base) {
      if (!nArgs) YError("no data type specified for reshape");
      /* reshape, var, address, ->type, dimlist */
      if (arg->ops==&referenceSym) ReplaceRef(arg);
      if (arg->ops==&dataBlockSym && arg->value.db->ops==&structDefOps)
	base= (StructDef *)arg->value.db;
      else
	YError("bad data type specified for reshape");
      arg++;
      nArgs--;
    }

    BuildDimList(arg, nArgs);
    result= PushDataBlock(NewLValueM(owner, address, base, tmpDims));

    if (owner) {
      char *end= owner->value.c +
	owner->type.number*owner->type.base->size;
      char *last= result->address.m +
	result->type.number*result->type.base->size;
      if (last>end) {
	Drop(1);
	YError("reshape beyond bounds of array owner");
      }
    }

    PopTo(&globTab[index]);
  }
}

/*--------------------------------------------------------------------------*/

void Y_array(int nArgs)
{
  Symbol *stack= sp-nArgs+1;
  StructDef *base;
  Operand op;
  Dimension *dims;
  void *value;
  Array *result;
  if (nArgs < 1) YError("too few arguments to array() function");

  /* get type or value argument */
  if (!stack->ops) goto barf;
  stack->ops->FormOperand(stack, &op);
  if (op.ops==&structDefOps) {
    dims= 0;
    base= op.value;
    value= 0;
  } else if (op.ops->isArray) {
    dims= op.type.dims;
    base= op.type.base;
    value= op.value;
  } else {
  barf:
    YError("first argument to array() must be either type or value");
    dims= 0;
    base= 0;
    value= 0;
  }

  BuildDimList(stack+1, nArgs-1);
  if (dims) {
    if (tmpDims) {
      Dimension *d= tmpDims;
      while (d->next) d= d->next;
      d->next= Ref(dims);
    } else {
      tmpDims= Ref(dims);
    }
  }

  /* push result Array, then fill it with either value or 0 */
  result= PushDataBlock(NewArray(base, tmpDims));
  if (value) {
    Broadcast(result->value.c, tmpDims, value, dims, base);
  } else if (base->Copy==&CopyX) {
    memset(result->value.c, 0, result->type.number*result->type.base->size);
  }
}

/*--------------------------------------------------------------------------*/

static Member type;

/* sets static type variable and returns 0, or sets type.base==0 and
   returns DataBlock* to non-Array object */
static DataBlock *GetInfo(Symbol *s);

static DataBlock *GetInfo(Symbol *s)
{
  DataBlock *db= 0;
  for (;;) {
    if (s->ops==&doubleScalar) {
      type.base= &doubleStruct;
      type.dims= 0;
      type.number= 1;
      break;
    } else if (s->ops==&longScalar) {
      type.base= &longStruct;
      type.dims= 0;
      type.number= 1;
      break;
    } else if (s->ops==&intScalar) {
      type.base= &intStruct;
      type.dims= 0;
      type.number= 1;
      break;
    } else if (s->ops==&dataBlockSym) {
      db= s->value.db;
      if (db->ops==&lvalueOps) {
	LValue *lvalue= (LValue *)db;
	type.base= lvalue->type.base;
	type.dims= lvalue->type.dims;
	type.number= lvalue->type.number;
      } else if (db->ops->isArray) {
	Array *array= (Array *)db;
	type.base= array->type.base;
	type.dims= array->type.dims;
	type.number= array->type.number;
      } else {
	type.base= 0;
	type.dims= 0;
	type.number= 0;
      }
      break;
    } else if (s->ops==&referenceSym) {
      s= &globTab[s->index];
    } else {
      YError("unexpected keyword argument");
    }
  }
  return type.base? 0 : db;
}

void Y_structof(int nArgs)
{
  DataBlock *db;
  if (nArgs != 1) YError("structof takes exactly one argument");
  db= GetInfo(sp);
  if (!db) PushDataBlock(Ref(type.base));
  else PushDataBlock(Ref(&nilDB));
}

Dimension *dimsofDims= 0;

void Y_dimsof(int nArgs)
{
  DataBlock *db;
  Symbol *stack= sp-nArgs+1;
  Dimension *tmp= dimsofDims;
  dimsofDims= 0;
  FreeDimension(tmp);
  if (nArgs < 1) YError("dimsof requires at least one argument");

  db= GetInfo(stack++);
  nArgs--;
  dimsofDims= Ref(type.dims);

  while (nArgs--) {
    db= GetInfo(stack++);
    if (db) break;
    if (Conform(dimsofDims, type.dims) & 4) {
      db= (DataBlock *)&charStruct;  /* anything non-0 */
      break;
    }
    tmp= dimsofDims;
    dimsofDims= 0;
    FreeDimension(tmp);
    dimsofDims= Ref(tmpDims);
  }

  if (!db) {
    int nDims= CountDims(dimsofDims);
    long *l;
    Array *result;
    tmp= tmpDims;
    tmpDims= 0;
    FreeDimension(tmp);
    tmpDims= NewDimension((long)(nDims+1), 1L, (Dimension *)0);
    result= PushDataBlock(NewArray(&longStruct, tmpDims));
    l= result->value.l;
    *l= nDims;
    l+= nDims;
    tmp= dimsofDims;
    while (tmp) {
      *l--= tmp->number;
      tmp= tmp->next;
    }
  } else {
    PushDataBlock(Ref(&nilDB));
  }
}

void Y_orgsof(int nArgs)
{
  DataBlock *db;
  if (nArgs != 1) YError("orgsof takes exactly one argument");
  db= GetInfo(sp);
  if (!db) {
    int nDims= CountDims(type.dims);
    long *l;
    Array *result;
    Dimension *tmp= tmpDims;
    tmpDims= 0;
    FreeDimension(tmp);
    tmpDims= NewDimension((long)(nDims+1), 1L, (Dimension *)0);
    result= PushDataBlock(NewArray(&longStruct, tmpDims));
    l= result->value.l;
    *l= nDims;
    l+= nDims;
    while (type.dims) {
      *l--= yForceOrigin? 1L : type.dims->origin;
      type.dims= type.dims->next;
    }
  } else {
    PushDataBlock(Ref(&nilDB));
  }
}

void Y_sizeof(int nArgs)
{
  DataBlock *db;
  long size;
  if (nArgs != 1) YError("sizeof takes exactly one argument");
  db= GetInfo(sp);
  if (!db) {
    size= type.number*type.base->size;
  } else if (db->ops==&structDefOps) {
    size= ((StructDef *)db)->size;
  } else if (db->ops==&streamOps) {
    /* return length of file in bytes */
    IOStream *file= (IOStream *)db;
    IOOperations *ioOps= file->ioOps;
    long address;
    if (file->history) file= file->history->child;
    ioOps= file->ioOps;
    address= ioOps->Tell(file, 0L);
    ioOps->SeekEnd(file, 0L);
    size= ioOps->Tell(file, 0L);
    ioOps->Seek(file, address);
  } else {
    size= 0;
  }
  Drop(2);
  PushLongValue(size);
}

void Y_numberof(int nArgs)
{
  DataBlock *db;
  long number;
  if (nArgs != 1) YError("numberof takes exactly one argument");
  db= GetInfo(sp);
  if (!db) number= type.number;
  else number= 0;
  Drop(2);
  PushLongValue(number);
}

void Y_typeof(int nArgs)
{
  DataBlock *db;
  char *typeName;
  Array *result;
  if (nArgs != 1) YError("typeof takes exactly one argument");
  db= GetInfo(sp);
  if (!db) typeName= type.base->dataOps->typeName;
  else typeName= db->ops->typeName;
  Drop(2);
  result= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  result->value.q[0]= StrCpy(typeName);
}

void Y_nameof(int nArgs)
{
  DataBlock *db;
  char *name= 0;
  if (nArgs != 1) YError("nameof takes exactly one argument");

  db= GetInfo(sp);
  if (!db) name= 0;
  else if (db->ops==&structDefOps) {
    name= StructName((StructDef *)db);
  } else {
    long index;
    if (db->ops==&functionOps) index= ((Function *)db)->code[0].index;
    else if (db->ops==&builtinOps) index= ((BIFunction *)db)->index;
    else index= -1;
    if (index>=0) name= globalTable.names[index];
    else name= 0;
  }

  Drop(2);
  if (!name) {
    PushDataBlock(Ref(&nilDB));
  } else {
    Array *result= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
    result->value.q[0]= StrCpy(name);
  }
}

/*--------------------------------------------------------------------------*/

void Y_is_array(int nArgs)
{
  Symbol *s= sp;
  int isArray;
  if (nArgs != 1) YError("is_array takes exactly one argument");

  for (;;) {
    if (s->ops==&dataBlockSym) {
      Operations *ops= s->value.db->ops;
      isArray= (ops==&lvalueOps || ops->isArray);
      break;
    } else if (s->ops!=&referenceSym) {
      isArray= 1;
      break;
    }
    s= &globTab[s->index];
  }

  PushIntValue(isArray);
}

void Y_is_func(int nArgs)
{
  Symbol *s= sp;
  int isFunc;
  if (nArgs != 1) YError("is_func takes exactly one argument");

  for (;;) {
    if (s->ops==&dataBlockSym) {
      Operations *ops= s->value.db->ops;
      if (ops==&functionOps) isFunc= 1;
      else if (ops==&builtinOps) isFunc= 2;
      else isFunc= 0;
      break;
    } else if (s->ops!=&referenceSym) {
      isFunc= 0;
      break;
    }
    s= &globTab[s->index];
  }

  PushIntValue(isFunc);
}

void Y_is_void(int nArgs)
{
  Symbol *s= sp;
  int isVoid;
  if (nArgs != 1) YError("is_void takes exactly one argument");

  for (;;) {
    if (s->ops==&dataBlockSym) {
      Operations *ops= s->value.db->ops;
      isVoid= (ops==&voidOps);
      break;
    } else if (s->ops!=&referenceSym) {
      isVoid= 0;
      break;
    }
    s= &globTab[s->index];
  }

  PushIntValue(isVoid);
}

void Y_is_range(int nArgs)
{
  Symbol *s= sp;
  int isRange;
  if (nArgs != 1) YError("is_range takes exactly one argument");

  for (;;) {
    if (s->ops==&dataBlockSym) {
      Operations *ops= s->value.db->ops;
      isRange= (ops==&rangeOps);
      break;
    } else if (s->ops!=&referenceSym) {
      isRange= 0;
      break;
    }
    s= &globTab[s->index];
  }

  PushIntValue(isRange);
}

void Y_is_struct(int nArgs)
{
  Symbol *s= sp;
  int isStruct;
  if (nArgs != 1) YError("is_struct takes exactly one argument");

  for (;;) {
    if (s->ops==&dataBlockSym) {
      Operations *ops= s->value.db->ops;
      isStruct= (ops==&structDefOps);
      break;
    } else if (s->ops!=&referenceSym) {
      isStruct= 0;
      break;
    }
    s= &globTab[s->index];
  }

  PushIntValue(isStruct);
}

void Y_is_stream(int nArgs)
{
  Symbol *s= sp;
  int isStream;
  if (nArgs != 1) YError("is_stream takes exactly one argument");

  for (;;) {
    if (s->ops==&dataBlockSym) {
      Operations *ops= s->value.db->ops;
      isStream= (ops==&streamOps);
      break;
    } else if (s->ops!=&referenceSym) {
      isStream= 0;
      break;
    }
    s= &globTab[s->index];
  }

  PushIntValue(isStream);
}

/*--------------------------------------------------------------------------*/

extern VMaction DropTop;
void Y_am_subroutine(int nArgs)
{
  Symbol *stack= sp;
  Instruction *pcRet= 0;
  while (stack>spBottom) {
    if (stack->ops==&returnSym) {
      pcRet= stack->value.pc;
      break;
    }
    stack--;
  }
  if (!pcRet) YError("am_subroutine lost return pc -- corrupt stack?");
  PushIntValue(pcRet->Action==&DropTop);
}

/*--------------------------------------------------------------------------*/

extern void *BuildResultU(Operand *op, StructDef *base);
extern void *BuildResult2(Operand *l, Operand *r);
extern void PopToI(Symbol *s);
extern void PopToL(Symbol *s);
extern void PopToD(Symbol *s);

static void PopToX(Symbol *s, int typeID);

static void PopToX(Symbol *s, int typeID)
{
  if (typeID==T_INT) PopToI(s);
  else if (typeID==T_LONG) PopToL(s);
  else if (typeID==T_DOUBLE) PopToD(s);
  else PopTo(s);
}

static Array *Force1D(Operand *op);

typedef void Looper(double *dst, double *src, long n);

static void UnaryTemplate(int nArgs, Looper *DLooper, Looper *ZLooper);

static void UnaryTemplate(int nArgs, Looper *DLooper, Looper *ZLooper)
{
  Operand op;
  int promoteID;
  if (nArgs != 1) YError("expecting exactly one argument");
  sp->ops->FormOperand(sp, &op);
  promoteID= op.ops->promoteID;
  if (promoteID<=T_DOUBLE) {
    if (promoteID<T_DOUBLE) op.ops->ToDouble(&op);
    DLooper(BuildResultU(&op, &doubleStruct), op.value, op.type.number);
    PopToD(sp-2);
  } else {
    if (promoteID>T_COMPLEX) YError("expecting numeric argument");
    ZLooper(BuildResultU(&op, &complexStruct), op.value, 2*op.type.number);
    PopTo(sp-2);
  }
  Drop(1);
}

/* ANSI standard math.h functions */
extern double sin(double);
extern double cos(double);
extern double tan(double);
extern double asin(double);
extern double acos(double);
extern double atan(double);
extern double atan2(double, double);
extern double sinh(double);
extern double cosh(double);
extern double tanh(double);
extern double exp(double);
extern double log(double);
extern double log10(double);
extern double sqrt(double);
extern double ceil(double);
extern double floor(double);

/* function either present in math library or implemented in nonc.c */
extern double hypot(double, double);

/* complex equivalents are defined in nonc.c */
extern void sinZ(double z[2], double x[2]);
extern void cosZ(double z[2], double x[2]);
extern void tanZ(double z[2], double x[2]);
extern void asinZ(double z[2], double x[2]);
extern void acosZ(double z[2], double x[2]);
extern void atanZ(double z[2], double x[2]);
extern void sinhZ(double z[2], double x[2]);
extern void coshZ(double z[2], double x[2]);
extern void tanhZ(double z[2], double x[2]);
extern void expZ(double z[2], double x[2]);
extern void logZ(double z[2], double x[2]);
extern void sqrtZ(double z[2], double x[2]);

extern void signZ(double z[2], double x[2]);

/* ----- pi ----- */

double y_pi;   /* value filled in by interpreter */

/* ----- sin ----- */

static Looper sinLoop, sinZLoop;

static void sinLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= sin(src[i]); }
static void sinZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) sinZ(&dst[i], &src[i]); }

void Y_sin(int nArgs) { UnaryTemplate(nArgs, &sinLoop, &sinZLoop); }

/* ----- cos ----- */

static Looper cosLoop, cosZLoop;

static void cosLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= cos(src[i]); }
static void cosZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) cosZ(&dst[i], &src[i]); }

void Y_cos(int nArgs) { UnaryTemplate(nArgs, &cosLoop, &cosZLoop); }

/* ----- tan ----- */

static Looper tanLoop, tanZLoop;

static void tanLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= tan(src[i]); }
static void tanZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) tanZ(&dst[i], &src[i]); }

void Y_tan(int nArgs) { UnaryTemplate(nArgs, &tanLoop, &tanZLoop); }

/* ----- asin ----- */

static Looper asinLoop, asinZLoop;

static void asinLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= asin(src[i]); }
static void asinZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) asinZ(&dst[i], &src[i]); }

void Y_asin(int nArgs) { UnaryTemplate(nArgs, &asinLoop, &asinZLoop); }

/* ----- acos ----- */

static Looper acosLoop, acosZLoop;

static void acosLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= acos(src[i]); }
static void acosZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) acosZ(&dst[i], &src[i]); }

void Y_acos(int nArgs) { UnaryTemplate(nArgs, &acosLoop, &acosZLoop); }

/* ----- atan ----- */

static Looper atanLoop, atanZLoop;

static void atanLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= atan(src[i]); }
static void atanZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) atanZ(&dst[i], &src[i]); }

void Y_atan(int nArgs)
{
  if (nArgs<2) UnaryTemplate(nArgs, &atanLoop, &atanZLoop);
  else if (nArgs>2) YError("atan takes one or two arguments");
  else {
    Operand opy, opx;
    double *y, *x, *dst;
    Operations *ops;
    long i;
    sp->ops->FormOperand(sp, &opx);
    if (opx.ops->promoteID<T_DOUBLE) opx.ops->ToDouble(&opx);
    (sp-1)->ops->FormOperand(sp-1, &opy);
    ops= opy.ops->Promote[opx.ops->promoteID](&opy, &opx);
    if (!ops ||
	ops->promoteID>T_DOUBLE) YError("illegal data type to atan(y, x)");
    dst= BuildResult2(&opy, &opx);
    if (!dst) YError("operands not conformable in binary atan");
    y= opy.value;
    x= opx.value;
    for (i=0 ; i<opy.type.number ; i++) dst[i]= atan2(y[i], x[i]);
    PopToD(sp-3);
    Drop(2);
  }
}

/* ----- sinh ----- */

static Looper sinhLoop, sinhZLoop;

static void sinhLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= sinh(src[i]); }
static void sinhZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) sinhZ(&dst[i], &src[i]); }

void Y_sinh(int nArgs) { UnaryTemplate(nArgs, &sinhLoop, &sinhZLoop); }

/* ----- cosh ----- */

static Looper coshLoop, coshZLoop;

static void coshLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= cosh(src[i]); }
static void coshZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) coshZ(&dst[i], &src[i]); }

void Y_cosh(int nArgs) { UnaryTemplate(nArgs, &coshLoop, &coshZLoop); }

/* ----- tanh ----- */

static Looper tanhLoop, tanhZLoop;

static void tanhLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= tanh(src[i]); }
static void tanhZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) tanhZ(&dst[i], &src[i]); }

void Y_tanh(int nArgs) { UnaryTemplate(nArgs, &tanhLoop, &tanhZLoop); }

/* ----- exp ----- */

static Looper expLoop, expZLoop;

static void expLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= exp(src[i]); }
static void expZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) expZ(&dst[i], &src[i]); }

void Y_exp(int nArgs) { UnaryTemplate(nArgs, &expLoop, &expZLoop); }

/* ----- log ----- */

static Looper logLoop, logZLoop;

static void logLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= log(src[i]); }
static void logZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) logZ(&dst[i], &src[i]); }

void Y_log(int nArgs) { UnaryTemplate(nArgs, &logLoop, &logZLoop); }

/* ----- log10 ----- */

static Looper log10Loop, log10ZLoop;

static void log10Loop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= log10(src[i]); }
static void log10ZLoop(double *dst, double *src, long n)
{ YError("log10(complex) not implemented, use log"); }

void Y_log10(int nArgs) { UnaryTemplate(nArgs, &log10Loop, &log10ZLoop); }

/* ----- sqrt ----- */

static Looper sqrtLoop, sqrtZLoop;

static void sqrtLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= sqrt(src[i]); }
static void sqrtZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) sqrtZ(&dst[i], &src[i]); }

void Y_sqrt(int nArgs) { UnaryTemplate(nArgs, &sqrtLoop, &sqrtZLoop); }

/* ----- ceil ----- */

static Looper ceilLoop, ceilZLoop;

static void ceilLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= ceil(src[i]); }
static void ceilZLoop(double *dst, double *src, long n)
{ YError("ceil(complex) not defined"); }

void Y_ceil(int nArgs) { UnaryTemplate(nArgs, &ceilLoop, &ceilZLoop); }

/* ----- floor ----- */

static Looper floorLoop, floorZLoop;

static void floorLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= floor(src[i]); }
static void floorZLoop(double *dst, double *src, long n)
{ YError("floor(complex) not defined"); }

void Y_floor(int nArgs) { UnaryTemplate(nArgs, &floorLoop, &floorZLoop); }

/* ----- abs ----- */

typedef void AbsLooper(void *, void *, long);
static AbsLooper absCLoop, absSLoop, absILoop, absLLoop,
  absFLoop, absDLoop, absZLoop;

static void absCLoop(void *d, void *s, long n)
{ long i; char *dst= d; char *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -src[i] : src[i]; }
static void absSLoop(void *d, void *s, long n)
{ long i; short *dst= d; short *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -src[i] : src[i]; }
static void absILoop(void *d, void *s, long n)
{ long i; int *dst= d; int *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -src[i] : src[i]; }
static void absLLoop(void *d, void *s, long n)
{ long i; long *dst= d; long *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -src[i] : src[i]; }
static void absFLoop(void *d, void *s, long n)
{ long i; float *dst= d; float *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -src[i] : src[i]; }
static void absDLoop(void *d, void *s, long n)
{ long i; double *dst= d; double *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -src[i] : src[i]; }
static void absZLoop(void *d, void *s, long n)
{ long i; double *dst= d; double *src= s;
  for (i=0 ; i<n ; i++) dst[i]= hypot(src[2*i], src[2*i+1]); }

static AbsLooper *absLoop[]= { &absCLoop, &absSLoop, &absILoop, &absLLoop,
		               &absFLoop, &absDLoop };

void Y_abs(int nArgs)
{
  if (nArgs<2) {
    Operand op;
    int promoteID;
    long n;
    if (nArgs != 1) YError("abs requires at least one argument");
    sp->ops->FormOperand(sp, &op);
    promoteID= op.ops->promoteID;
    n= op.type.number;
    if (promoteID>T_COMPLEX) YError("abs requires numeric argument");
    if (promoteID<T_COMPLEX) {
      absLoop[promoteID](BuildResultU(&op, op.type.base), op.value, n);
      PopToX(sp-2, promoteID);
    } else {
      absZLoop(BuildResultU(&op, &doubleStruct), op.value, n);
      PopToD(sp-2);
    }
    Drop(1);

  } else {
    Operand opy, opx;
    double *y, *x, *dst;
    Operations *ops;
    long i;
    while (--nArgs) {
      sp->ops->FormOperand(sp, &opx);
      if (opx.ops->promoteID<T_DOUBLE) opx.ops->ToDouble(&opx);
      (sp-1)->ops->FormOperand(sp-1, &opy);
      ops= opy.ops->Promote[opx.ops->promoteID](&opy, &opx);
      if (!ops || ops->promoteID>T_DOUBLE) {
	if (!ops ||
	    ops->promoteID>T_COMPLEX) YError("illegal data type to abs(x,y)");
	else YError("no abs(complex,complex), take abs of argument(s)");
      }
      dst= BuildResult2(&opy, &opx);
      if (!dst) YError("operands not conformable in binary abs");
      y= opy.value;
      x= opx.value;
      for (i=0 ; i<opy.type.number ; i++) dst[i]= hypot(y[i], x[i]);
      PopToD(opy.owner);
      Drop(1);
    }
    PopToD(sp-1);
  }
}

/* ----- sign ----- */

static AbsLooper signCLoop, signSLoop, signILoop, signLLoop,
  signFLoop, signDLoop, signZLoop;

static void signCLoop(void *d, void *s, long n)
{ long i; char *dst= d; char *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -1 : 1; }
static void signSLoop(void *d, void *s, long n)
{ long i; short *dst= d; short *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -1 : 1; }
static void signILoop(void *d, void *s, long n)
{ long i; int *dst= d; int *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -1 : 1; }
static void signLLoop(void *d, void *s, long n)
{ long i; long *dst= d; long *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -1 : 1; }
static void signFLoop(void *d, void *s, long n)
{ long i; float *dst= d; float *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -1 : 1; }
static void signDLoop(void *d, void *s, long n)
{ long i; double *dst= d; double *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -1 : 1; }
static void signZLoop(void *d, void *s, long n)
{ long i; double *dst= d; double *src= s;
  if (dst!=src) for (i=0 ; i<n ; i++) dst[i]= src[i];
  for (i=0 ; i<n ; i+=2) signZ(&dst[i], &src[i]); }

static AbsLooper *signLoop[]= { &signCLoop, &signSLoop, &signILoop,
		                &signLLoop, &signFLoop, &signDLoop,
				&signZLoop };

void Y_sign(int nArgs)
{
  Operand op;
  int promoteID;
  long n;
  if (nArgs != 1) YError("sign function requires exactly one argument");
  sp->ops->FormOperand(sp, &op);
  promoteID= op.ops->promoteID;
  n= op.type.number;
  if (promoteID>T_COMPLEX) YError("sign requires numeric argument");
  if (promoteID==T_COMPLEX) n*= 2;
  signLoop[promoteID](BuildResultU(&op, op.type.base), op.value, n);
  PopToX(sp-2, promoteID);
  Drop(1);
}

/* ----- conj ----- */

void Y_conj(int nArgs)
{
  Operand op;
  int promoteID;
  long i, n;
  double *dst, *src;
  if (nArgs != 1) YError("conj requires exactly one argument");
  sp->ops->FormOperand(sp, &op);
  promoteID= op.ops->promoteID;
  if (promoteID>T_COMPLEX) YError("conj requires numeric argument");
  if (promoteID<T_COMPLEX) {
    PopTo(sp-1);
    return;
  }
  n= 2*op.type.number;
  src= op.value;
  dst= BuildResultU(&op, &complexStruct);
  for (i=0 ; i<n ; i+=2) {
    dst[i]= src[i];
    dst[i+1]= -src[i+1];
  }
  PopTo(sp-2);
  Drop(1);
}

/*--------------------------------------------------------------------------*/

extern RangeFunc RFmin, RFmax, RFsum, RFavg;

static Array *Force1D(Operand *op)
{
  Array *array;
  if (!op->type.dims) return 0;
  if (op->owner->value.db->ops==&lvalueOps)
    array= FetchLValue(op->owner->value.db, op->owner);
  else
    array= (Array *)op->owner->value.db;
  if (op->type.dims->next) {
    /* need to copy the array to a single dimensional version */
    Dimension *dim= tmpDims;
    tmpDims= 0;
    FreeDimension(dim);
    tmpDims= NewDimension(op->type.number, 1L, (Dimension *)0);
    if (op->references) {
      array= PushDataBlock(NewArray(op->type.base, tmpDims));
      op->type.base->Copy(op->type.base, array->value.c, op->value,
			  op->type.number);
      PopTo(op->owner);
      op->value= array->value.c;
    } else {
      array->type.dims= Ref(tmpDims);
      FreeDimension(op->type.dims);
    }
    op->type.dims= tmpDims;
  }
  return array;
}

/* ----- min ----- */

extern BinaryOp MinC, MinS, MinI, MinL, MinF, MinD;

static void MinError(void);
static void MinError(void)
{ YError("operands not conformable in binary min"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]<=rv[i]? lv[i] : rv[i]; \
    Popper(l->owner); \
  } else MinError(); }

OPERATION(MinC, char, PopTo)
OPERATION(MinS, short, PopTo)
OPERATION(MinI, int, PopToI)
OPERATION(MinL, long, PopToL)
OPERATION(MinF, float, PopTo)
OPERATION(MinD, double, PopToD)

static BinaryOp *MinOps[]= { &MinC, &MinS, &MinI, &MinL, &MinF, &MinD };

void Y_min(int nArgs)
{
  if (nArgs<2) {
    Operand op;
    Array *array;
    int promoteID;
    if (nArgs != 1) YError("min requires at least one argument");
    sp->ops->FormOperand(sp, &op);
    promoteID= op.ops->promoteID;
    if (promoteID>=T_COMPLEX) YError("min requires numeric argument");
    array= Force1D(&op);
    if (!array) {  /* scalars are trivial */
      PopTo(sp-1);
      return;
    }
    RFmin(array, 0);
    PopToX(sp-2, promoteID);  /* min does not change data type */
    Drop(1);

  } else {
    Operand opy, opx;
    Operations *ops;
    int promoteID= -1;
    while (--nArgs) {
      sp->ops->FormOperand(sp, &opx);
      (sp-1)->ops->FormOperand(sp-1, &opy);
      ops= opy.ops->Promote[opx.ops->promoteID](&opy, &opx);
      promoteID= ops? ops->promoteID : T_COMPLEX;
      if (promoteID>T_DOUBLE) YError("illegal data type to min(x,y)");
      MinOps[promoteID](&opy, &opx);
      Drop(1);
    }
    PopToX(sp-1, promoteID);
  }
}

/* ----- max ----- */

extern BinaryOp MaxC, MaxS, MaxI, MaxL, MaxF, MaxD;

static void MaxError(void);
static void MaxError(void)
{ YError("operands not conformable in binary max"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]>=rv[i]? lv[i] : rv[i]; \
    Popper(l->owner); \
  } else MaxError(); }

OPERATION(MaxC, char, PopTo)
OPERATION(MaxS, short, PopTo)
OPERATION(MaxI, int, PopToI)
OPERATION(MaxL, long, PopToL)
OPERATION(MaxF, float, PopTo)
OPERATION(MaxD, double, PopToD)

static BinaryOp *MaxOps[]= { &MaxC, &MaxS, &MaxI, &MaxL, &MaxF, &MaxD };

void Y_max(int nArgs)
{
  if (nArgs<2) {
    Operand op;
    Array *array;
    int promoteID;
    if (nArgs != 1) YError("max requires at least one argument");
    sp->ops->FormOperand(sp, &op);
    promoteID= op.ops->promoteID;
    if (promoteID>=T_COMPLEX) YError("max requires numeric argument");
    array= Force1D(&op);
    if (!array) {  /* scalars are trivial */
      PopTo(sp-1);
      return;
    }
    RFmax(array, 0);
    PopToX(sp-2, promoteID);  /* max does not change data type */
    Drop(1);

  } else {
    Operand opy, opx;
    Operations *ops;
    int promoteID= -1;
    while (--nArgs) {
      sp->ops->FormOperand(sp, &opx);
      (sp-1)->ops->FormOperand(sp-1, &opy);
      ops= opy.ops->Promote[opx.ops->promoteID](&opy, &opx);
      promoteID= ops? ops->promoteID : T_COMPLEX;
      if (promoteID>T_DOUBLE) YError("illegal data type to max(x,y)");
      MaxOps[promoteID](&opy, &opx);
      Drop(1);
    }
    PopToX(sp-1, promoteID);
  }
}

/* ----- sum ----- */

void Y_sum(int nArgs)
{
  Operand op;
  Array *array;
  int promoteID;
  if (nArgs != 1) YError("sum requires exactly one argument");
  sp->ops->FormOperand(sp, &op);
  promoteID= op.ops->promoteID;
  if (promoteID>T_COMPLEX) YError("sum requires numeric argument");
  array= Force1D(&op);
  if (!array) {			/* scalars are trivial */
    PopTo(sp-1);
    return;
  }
  RFsum(array, 0);
  /* sum range function may change data type */
  PopToX(sp-2, sp->value.db->ops->promoteID);
  Drop(1);
}

/* ----- avg ----- */

void Y_avg(int nArgs)
{
  Operand op;
  Array *array;
  int promoteID;
  if (nArgs != 1) YError("avg requires exactly one argument");
  sp->ops->FormOperand(sp, &op);
  promoteID= op.ops->promoteID;
  if (promoteID>T_COMPLEX) YError("avg requires numeric argument");
  array= Force1D(&op);
  if (!array) {			/* scalars are trivial */
    PopTo(sp-1);
    return;
  }
  RFavg(array, 0);
  /* avg range function may change data type */
  PopToX(sp-2, sp->value.db->ops->promoteID);
  Drop(1);
}

/*--------------------------------------------------------------------------*/

extern VMaction True, Not;

static void AllofWorker(VMaction *TrueOrNot, int trueIfFinishes)
{
  Operand op;
  int *src;
  long i;
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  TrueOrNot();   /* converts top of stack to 0-or-1 int array */
  sp->ops->FormOperand(sp, &op);
  src= op.value;
  for (i=0 ; i<op.type.number ; i++) if (src[i]) break;
  Drop(2);
  PushIntValue(trueIfFinishes? i>=op.type.number : i<op.type.number);
}

/* ----- allof ----- */

void Y_allof(int nArgs)
{
  if (nArgs != 1) YError("allof requires exactly one argument");
  AllofWorker(&Not, 1);
}

/* ----- anyof ----- */

void Y_anyof(int nArgs)
{
  if (nArgs != 1) YError("anyof requires exactly one argument");
  AllofWorker(&True, 0);
}

/* ----- noneof ----- */

void Y_noneof(int nArgs)
{
  if (nArgs != 1) YError("noneof requires exactly one argument");
  AllofWorker(&True, 1);
}

/* ----- nallof ----- */

void Y_nallof(int nArgs)
{
  if (nArgs != 1) YError("nallof requires exactly one argument");
  AllofWorker(&Not, 0);
}

/* ----- where ----- */

void Y_where(int nArgs)
{
  Operand op;
  int *src;
  long i, n;
  if (nArgs!=1) YError("where takes exactly one argument");
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  True();   /* converts top of stack to 0-or-1 int array */
  sp->ops->FormOperand(sp, &op);
  src= op.value;

  /* first pass counts number of non-zero points */
  n= 0;
  for (i=0 ; i<op.type.number ; i++) if (src[i]) n++;

  if (n && op.type.dims) {
    long j;
    Array *array;
    long origin= 1L;
    Dimension *dims= tmpDims;
    tmpDims= 0;
    FreeDimension(dims);
    tmpDims= NewDimension(n, 1L, (Dimension *)0);
    array= PushDataBlock(NewArray(&longStruct, tmpDims));
    dims= op.type.dims;
    if (dims) {
      while (dims->next) dims= dims->next;
      origin= yForceOrigin? 1L : dims->origin;
    } else {
      origin= 1L;
    }
    j= 0;
    for (i=0 ; i<op.type.number ; i++)
      if (src[i]) array->value.l[j++]= i+origin;

  } else if (n) {
    /* Return scalar index for scalar input.  This provides maximum
       performance when used in conjunction with merge (std1.c).  */
    PushLongValue(1L);

  } else {
    /* Return R_NULLER index range.  This prevents bogus results if
       the caller should use the result of where to extract a subset
       of an array without checking whether anything was returned.  */
    PushDataBlock(NewRange(0L, 0L, 1L, R_NULLER));
  }

  PopTo(sp-2);
  Drop(1);
}

/*--------------------------------------------------------------------------*/

/* ARGSUSED */
void Y_get_cwd(int nArgs)
{
  Array *result= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  result->value.q[0]= StrCpy(yCWD);
}

/* ARGSUSED */
void Y_get_home(int nArgs)
{
  Array *result= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  result->value.q[0]= StrCpy(yHOME);
}

void Y_cd(int nArgs)
{
  char *q;
  char *name;
  int notOK;
  int amSubroutine= CalledAsSubroutine();
  if (nArgs!=1) YError("cd function takes exactly one argument");
  q= YGetString(sp);
  if (!q) YError("argument to cd must be a non-nil scalar string");
  name= YExpandName(q);
  notOK= YSetCWD(name);
  StrFree(name);
  if (notOK) {
    if (amSubroutine) YError("cd failed -- no such directory");
    PushDataBlock(Ref(&nilDB));
  } else if (!amSubroutine) {
    Y_get_cwd(nArgs);
  }
}

void Y_get_env(int nArgs)
{
  Array *result;
  char *q;
  if (nArgs!=1) YError("getenv function takes exactly one argument");
  q= YGetString(sp);
  result= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  if (q) result->value.q[0]= Ygetenv(q);
}

int ym_argc= 0;
char **ym_argv= 0;

/* ARGSUSED */
void Y_get_argv(int nArgs)
{
  Array *result;
  Dimension *dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  if (ym_argc>0) tmpDims= NewDimension((long)ym_argc, 1L, (Dimension *)0);
  result= PushDataBlock(NewArray(&stringStruct, tmpDims));
  if (ym_argc>0) {
    int i;
    for (i=0 ; i<ym_argc ; i++) result->value.q[i]= StrCpy(ym_argv[i]);
  }
}

/*--------------------------------------------------------------------------*/

/* The OriginStatus stores yForceOrigin -- when it is destroyed, this is
   automatically copied back to the yForceOrigin global variable.  */
typedef struct OriginStatus OriginStatus;
struct OriginStatus {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  int force;
};

extern OriginStatus *NewOrg(int force);
extern void FreeOrg(void *ob);  /* ******* Use Unref(bm) ******* */

extern PromoteOp PromXX;
extern UnaryOp ToAnyX, NegateX, ComplementX, NotX, TrueX;
extern BinaryOp AddX, SubtractX, MultiplyX, DivideX, ModuloX, PowerX;
extern BinaryOp EqualX, NotEqualX, GreaterX, GreaterEQX;
extern BinaryOp ShiftLX, ShiftRX, OrX, AndX, XorX;
extern BinaryOp AssignX, MatMultX;
extern UnaryOp EvalX, SetupX, PrintX;
extern MemberOp GetMemberX;

static UnaryOp PrintOB;

Operations orgsOps = {
  &FreeOrg, T_OPAQUE, 0, T_STRING, "origin_status",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &EvalX, &SetupX, &GetMemberX, &MatMultX, &PrintOB
};

static MemoryBlock orgBlock= {0, 0, sizeof(OriginStatus),
				16*sizeof(OriginStatus)};

void Y_use_origins(int nArgs)
{
  int nf;
  if (nArgs!=1) YError("use_origins takes exactly one argument");
  nf= (int)YGetInteger(sp);
  PushDataBlock(NewOrg(yForceOrigin));
  yForceOrigin= !nf;
}

OriginStatus *NewOrg(int force)
{
  OriginStatus *ob= NextUnit(&orgBlock);
  ob->references= 0;
  ob->ops= &orgsOps;
  ob->force= force;
  return ob;
}

void FreeOrg(void *ob)
{
  OriginStatus *oblk= ob;
  yForceOrigin= oblk->force;
  FreeUnit(&orgBlock, ob);
}

static void PrintOB(Operand *op)
{
  OriginStatus *ob= op->value;
  char line[80];
  sprintf(line, "origin_status: index origins %s",
	  ob->force? "force" : "default");
  ForceNewline();
  PrintFunc(line);
  ForceNewline();
}

/*--------------------------------------------------------------------------*/

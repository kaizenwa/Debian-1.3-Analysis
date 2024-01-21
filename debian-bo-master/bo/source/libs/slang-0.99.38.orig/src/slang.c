/* slang.c  --- guts of S-Lang interpreter */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 *
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */

#include "config.h"

#include <stdio.h>

#ifdef FLOAT_TYPE
# include <math.h>
#endif

#define SL_BYTE_COMPILING 

#include "slang.h"
#include "_slang.h"
int SLang_Version = SLANG_VERSION;

/* If non null, these call C functions before and after a slang function. */
void (*SLang_Enter_Function)(char *) = NULL;
void (*SLang_Exit_Function)(char *) = NULL;

int SLang_Trace = 0;
char SLang_Trace_Function[32];

#ifdef MSWINDOWS
SLang_Name_Type *SLang_Name_Table;
#else
SLang_Name_Type SLang_Name_Table[SLANG_MAX_SYMBOLS];
#endif

static int SLang_Name_Table_Ofs[256];
SLName_Table *SLName_Table_Root;


static SLang_Name_Type *Lang_Local_Variable_Table;
static int Local_Variable_Number;
#define MAX_LOCAL_VARIABLES 50

static int Lang_Break_Condition = 0;	       /* true if any one below is true */
static int Lang_Break = 0;
static int Lang_Return = 0;
static int Lang_Continue = 0;

/* These are initialized in add_table below.  I cannot init a Union!! */
static SLBlock_Type SLShort_Blocks[3];

/* this stack is used by the inner interpreter to execute top level
 *   interpreter commands which by definition are immediate so stack is
 * only of maximum 10; sorry... */
#define SLANG_MAX_TOP_STACK 10
static SLBlock_Type Lang_Interp_Stack_Static[SLANG_MAX_TOP_STACK];
static SLBlock_Type *Lang_Interp_Stack_Ptr = Lang_Interp_Stack_Static;
static SLBlock_Type *Lang_Interp_Stack = Lang_Interp_Stack_Static;

#ifdef MSWINDOWS
SLang_Object_Type *SLRun_Stack;
SLang_Object_Type *SLStack_Pointer;
static SLang_Object_Type *SLStack_Pointer_Max;
#else
SLang_Object_Type SLRun_Stack[SLANG_MAX_STACK_LEN];
SLang_Object_Type *SLStack_Pointer = SLRun_Stack;
static SLang_Object_Type *SLStack_Pointer_Max = SLRun_Stack + SLANG_MAX_STACK_LEN;
#endif

/* Might want to increase this. */
#define MAX_LOCAL_STACK 200
static SLang_Object_Type Local_Variable_Stack[MAX_LOCAL_STACK];

static SLang_Object_Type *Local_Variable_Frame = Local_Variable_Stack;

int SLang_Traceback = 0;		       /* non zero means do traceback */

static int inner_interp(register SLBlock_Type *);

static int Lang_Defining_Function = 0;   /* true if defining a function */
static SLBlock_Type *Lang_Function_Body;
static SLBlock_Type *Lang_FBody_Ptr;
static unsigned int Lang_FBody_Size = 0;

#define SLANG_MAX_BLOCKS 30
/* max number of nested blocks--- was 10 but I once exceeded it! */

typedef struct Lang_Block_Type
{
   unsigned int size;                         /* current nuber of objects malloced */
   SLBlock_Type *body;           /* beginning of body definition */
   SLBlock_Type *ptr;            /* current location */
}
Lang_Block_Type;

static int Lang_Defining_Block = 0;   /* true if defining a block */
static Lang_Block_Type Lang_Block_Stack[SLANG_MAX_BLOCKS];
static SLBlock_Type *Lang_Block_Body;
static unsigned int Lang_BBody_Size;

static int Lang_Block_Depth = -1;

static SLBlock_Type *Lang_Object_Ptr = Lang_Interp_Stack_Static;
/* next location for compiled obj -- points to interpreter stack initially */

/* main_type MUST come back 0 if there is a stack underflow !!! */
/* This routine is assumed to work even in the presence of a SLang_Error. */
int SLang_pop(SLang_Object_Type *x)
{
   register SLang_Object_Type *y;
   
   y = SLStack_Pointer;
#ifdef SLANG_NOOP
   try_again:
#endif
   if (y == SLRun_Stack)
     {
	x->main_type = 0;
	if (SLang_Error == 0) SLang_Error = STACK_UNDERFLOW;
	SLStack_Pointer = SLRun_Stack;
	return 1;
     }
   y--;
#ifdef SLANG_NOOP
   if (y->main_type == SLANG_NOOP) goto try_again;
#endif
   *x = *y;
   
   SLStack_Pointer = y;
   return(0);
}

void SLang_free_object (SLang_Object_Type *obj)
{
   if (obj->main_type != SLANG_DATA)
     return;
     
   if (obj->sub_type == STRING_TYPE) 
     SLFREE(obj->v.s_val);
   else if (obj->sub_type >= ARRAY_TYPE)
     SLang_free_user_object (obj->v.uobj);
}
   

#ifdef SLANG_NOOP
static void pop_noop (void)
{
   SLang_Object_Type *y = SLStack_Pointer;
   
   if (y != SLRun_Stack)
     {
	y--;
	if (y->main_type == SLANG_NOOP)
	  SLStack_Pointer = y;
     }
}
#endif
void SLang_push(SLang_Object_Type *x)
{
   register SLang_Object_Type *y;
   y = SLStack_Pointer;
   
   /* if there is a SLang_Error, probably not much harm will be done
    if it is ignored here */
   /* if (SLang_Error) return; */
   
   /* flag it now */
   if (y >= SLStack_Pointer_Max)
     {
	if (!SLang_Error) SLang_Error = STACK_OVERFLOW;
	return;
     }
   
   *y = *x;
   SLStack_Pointer = y + 1;
}

SLuser_Object_Type *SLang_pop_user_object (unsigned char stype)
{
   SLang_Object_Type obj;
   
   if (SLang_pop (&obj)) return NULL;
   if ((obj.sub_type == stype) && (stype >= ARRAY_TYPE))
     {
	return obj.v.uobj;
     }
   
   SLang_free_object (&obj);
   if (!SLang_Error) SLang_Error = TYPE_MISMATCH;
   return NULL;
}





/* Pop anything but an object type */
int SLang_pop_non_object (SLang_Object_Type *obj)
{
   if (SLang_pop (obj)) return 1;
   
   if (obj->sub_type >= ARRAY_TYPE)
     {
	SLang_free_object (obj);
	SLang_Error = TYPE_MISMATCH;
	return 1;
     }
   
   return 0;
}

/* If it returns 0, DO NOT FREE p */
static int lang_free_branch(SLBlock_Type *p)
{
   unsigned char main_type;
   unsigned char stype;

#if 0
   main_type = p->main_type;
   /* These guys were not all allocated.  See end_block for details */
   if ((main_type == SLANG_RETURN)
       || (main_type == SLANG_BREAK)
       || (main_type == SLANG_CONTINUE))
     return 0;
#else
   if ((p == SLShort_Blocks)
       || (p == SLShort_Blocks + 1)
       || (p == SLShort_Blocks + 2)
       )
     return 0;
#endif
   
   while(1)
     {
        main_type = p->main_type;
	if (main_type == SLANG_BLOCK)
	  {
	     if (lang_free_branch(p->b.blk)) SLFREE(p->b.blk);
	  }
	else if (main_type == SLANG_DATA)
	  {
	     stype = p->sub_type;
	     if (stype == STRING_TYPE)
	       {
		  SLFREE (p->b.s_blk);
	       }
	     
	  }
#ifdef FLOAT_TYPE
	else if ((main_type == SLANG_LITERAL) && (p->sub_type == FLOAT_TYPE))
	  {
	     SLFREE (p->b.f_blk);
	  }
#endif
#if 0
	else if (type == string_type) SLFREE(p->value);
#endif
	else if (main_type == 0) break;
	p++;
     }
   return 1;
}

int SLang_pop_integer(int *i)
{
   SLang_Object_Type obj;
   
   if (SLang_pop_non_object (&obj)) return 1;
   
   if (obj.sub_type != INT_TYPE)
     {
	SLang_free_object (&obj);
	if (!SLang_Error) SLang_Error = TYPE_MISMATCH;
	return(1);
     }
   *i = obj.v.i_val;
   
   return(0);
}

#ifdef FLOAT_TYPE
int SLang_pop_float(float64 *x, int *convert, int *ip)
{
   SLang_Object_Type obj;
   register unsigned char stype;
   
   if (SLang_pop_non_object (&obj)) return(1);
   
   stype = obj.sub_type;
   
   if (stype == FLOAT_TYPE)
     {
	*x = obj.v.f_val;
	*convert = 0;
     }
   else if (stype == INT_TYPE)
     {
	*ip = obj.v.i_val;
	*x = (float64) obj.v.i_val;
	*convert = 1;
     }
   else
     {
	SLang_free_object (&obj);
	SLang_Error = TYPE_MISMATCH;
	return(1);
     }
   return(0);
}

void SLang_push_float(float64 x)
{
   SLang_Object_Type obj;
   
   obj.main_type = SLANG_DATA; obj.sub_type = FLOAT_TYPE;
   obj.v.f_val = x;
   SLang_push (&obj);
}

#endif

int SLpop_string (char **s)
{
   SLang_Object_Type obj;
   
   if (SLang_pop (&obj))
     return 1;
   
   if (obj.sub_type != STRING_TYPE)
     {
	SLang_free_object (&obj);
	if (!SLang_Error) SLang_Error = TYPE_MISMATCH;
	*s = NULL;
	return 1;
     }
   
   *s = obj.v.s_val;
   return 0;
}

/* if *data != 0, string should be freed upon use. */
int SLang_pop_string(char **s, int *data)
{
   if (SLpop_string (s))
     return 1;
   
   *data = 1;
   return 0;
}

void SLang_push_integer(int i)
{
   SLang_Object_Type obj;
   
   obj.main_type = SLANG_DATA; obj.sub_type = INT_TYPE;
   obj.v.i_val = i;
   SLang_push (&obj);
}


void SLang_push_string(char *t)
{
   SLang_Object_Type obj;
   if (NULL == (obj.v.s_val = SLmake_string(t))) return;
   obj.main_type = SLANG_DATA; obj.sub_type = STRING_TYPE;
   SLang_push(&obj);
}



void SLang_push_malloced_string(char *c)
{
   SLang_Object_Type obj;
   
   if (c == NULL)
     {
	SLang_Error = SL_MALLOC_ERROR;
	return;
     }
   obj.main_type = SLANG_DATA; obj.sub_type = STRING_TYPE;
   obj.v.s_val = c;
   SLang_push(&obj);
}



void SLang_push_user_object (SLuser_Object_Type *uobj)
{
   SLang_Object_Type obj;
   
   /* Always maintain the correct count.  It is possible for applications to
    * switch the main_type from IVARIABLE to SLANG_DATA.  For example, JED
    * does this for its subprocess marks.  This is done so that an intrinsic
    * user object may be freed when it is nolonger needed.
    */
   uobj->count++;
   obj.sub_type = uobj->sub_type; obj.main_type = uobj->main_type;
   obj.v.uobj = uobj;
   SLang_push (&obj);
}


static void call_funptr(SLang_Name_Type *);

static void SLang_push_variable(SLang_Object_Type *obj)
{
   register unsigned char subtype;
   subtype = obj->sub_type;
   
   if (obj->main_type == 0)
     {
	SLang_doerror ("Variable uninitialized.");
	return;
     }
   
   if (subtype == STRING_TYPE)
     {
	SLang_push_string(obj->v.s_val);
	return;
     }
   else if (subtype == SLANG_OBJ_TYPE)
     {
	call_funptr(obj->v.n_val);
	return;
     }
   else if (subtype >= ARRAY_TYPE)
     {
	SLang_push_user_object (obj->v.uobj);
	return;
     }
   
   SLang_push(obj);
}

/* This routine pops an integer off the stack.  It then adds dn to the
 *   value producing n. Then it reverses the
 *  next n items on the stack.  Some functions may require this.
 *  This returns a pointer to the last item.
 */
SLang_Object_Type *_SLreverse_stack (int n)
{
#ifdef SLANG_NOOP
   int i;
#endif
   SLang_Object_Type *otop, *obot, tmp;
   
   otop = SLStack_Pointer;
#ifdef SLANG_NOOP
   i = 0;
   obot = otop;
   while (i < n) 
     {
	obot--;
	if (obot < SLRun_Stack)
	  {
	     SLang_Error = STACK_UNDERFLOW;
	     return NULL;
	  }
	if (obot->main_type != SLANG_NOOP) i++;
     }
#else
   if ((n > otop - SLRun_Stack) || (n < 0))
     {
	SLang_Error = STACK_UNDERFLOW;
	return (NULL);
     }
   obot = otop - n;
#endif
   otop--;
   while (otop > obot)
     {
	tmp = *obot;
	*obot = *otop;
	*otop = tmp;
	otop--;
	obot++;
     }
   return (SLStack_Pointer - n);
}


void SLroll_stack (int *np)
{
   int n;
   SLang_Object_Type *otop, *obot, tmp;
   
   if ((n = abs(*np)) <= 1) return;    /* identity */
   
   otop = SLStack_Pointer;
   if (n > (int) (otop - SLRun_Stack))
     {
	SLang_Error = STACK_UNDERFLOW;
	return;
     }
   obot = otop - n;
   otop--;
   
   if (*np > 0)
     {
	/* Put top on bottom and roll rest up. */
	tmp = *otop;
	while (otop > obot)
	  {
	     *otop = *(otop - 1);
	     otop--;
	  }
	*otop = tmp;
     }
   else
     {
	/* Put bottom on top and roll rest down. */
	tmp = *obot;
	while (obot < otop)
	  {
	     *obot = *(obot + 1);
	     obot++;
	  }
	*obot = tmp;
     }
}




/* local and global variable assignments */

/*  Pop a data item from the stack and return a pointer to it.
 *  Strings are not freed from stack so use another routine to do it.
 *
 *  In addition, I need to make this work with the array types.
 *  see pop string for discussion of do_free
 */
long *SLang_pop_pointer(unsigned char *main_type, unsigned char *sub_type, int *do_free)
{
   SLang_Object_Type obj;
   register SLang_Object_Type *p;
   long *val;
   
   if (SLang_pop_non_object (&obj)) return(NULL);
   p = SLStack_Pointer;
   
   /* use this because the stack is static but obj is not.
    * do not even try to make it static either. See the intrinsic
    * routine for details */
   *do_free = 0;
   *main_type = p->main_type;   *sub_type = p->sub_type;
   switch (*sub_type)
     {
#ifdef FLOAT_TYPE
      case FLOAT_TYPE: val = (long *) &(p->v.f_val);
	break;
#endif
      case INT_TYPE: val = (long *) &(p->v.i_val); break;
      case STRING_TYPE:
	*do_free = 1;
	/* drop */
      default:
        val = (long *) p->v.s_val;
     }
   
   return (val);
}


static void lang_do_eqs (SLBlock_Type *obj)
{
   int y;
#ifdef FLOAT_TYPE   
   int ifloat, float_convert;
#endif
   register unsigned char sub_type;
   register SLang_Object_Type *addr;
   register long val;
   unsigned char stype;
   
   
   sub_type = obj->sub_type;
   /* calculate address.  This depends upon whether or not variable is
    * local, global or intrinsic.
    */
   if (sub_type <= SLANG_LMM)
     {
	/* local variable */
	val = 0;
	addr = Local_Variable_Frame - obj->b.i_blk;
	stype = addr->sub_type;
     }

   else if (sub_type <= SLANG_GMM)	       /* global */
     {
	addr = (SLang_Object_Type *) obj->b.n_blk->addr;
	val = 0;
	stype = addr->sub_type;
     }
   else				       /* intrinsic */
     {
	addr = NULL;
	val = obj->b.n_blk->addr;      /* address of variable. */
	stype = obj->b.n_blk->sub_type;
     }
   
   if ((sub_type == SLANG_LEQS) || (sub_type == SLANG_GEQS))
     {
	SLang_free_object (addr);
        SLang_pop(addr);
	return;
     }
   
   /* everything else applies to integers -- later I will extend to float */
   
   if (INT_TYPE != stype)
     {
#ifdef FLOAT_TYPE
	/* A quick hack for float */
	if ((FLOAT_TYPE == stype) && (sub_type == SLANG_IEQS))
	  {
	     SLang_pop_float ((float64 *) val, &float_convert, &ifloat);
	     return;
	  }
	
#endif
	if (INTP_TYPE != stype)	       /* integer pointer type */
	  {
	     SLang_Error = TYPE_MISMATCH;
	     return;
	  }
	/* AT this point, val is int **.  Below, we assume that val is
	 * an int *.  Note that this type is only defined for intrinsics.
	 */
	val = (long) *(int **) val;
     }
   
   /* make this fast for local variables avoiding switch bottleneck */
   if (sub_type == SLANG_LPP)
     {
	addr->v.i_val += 1;
	return;
     }
   else if (sub_type == SLANG_LMM)
     {
	addr->v.i_val -= 1;
	return;
     }
   
   y = 1;
   switch (sub_type)
     {
      case SLANG_LPEQS:
      case SLANG_GPEQS:
	if (SLang_pop_integer(&y)) return;
	/* drop */
      case SLANG_GPP:
	addr->v.i_val += y;
	break;
	
      case SLANG_GMEQS:
      case SLANG_LMEQS:
	if (SLang_pop_integer(&y)) return;
	/* drop */
      case SLANG_GMM:
	addr->v.i_val -= y;
	break;
	
      case SLANG_IEQS:
	if (SLang_pop_integer(&y)) return;
	*(int *) val = y;
	break;
	
      case SLANG_IPEQS:
	if (SLang_pop_integer(&y)) return;
	/* drop */
      case SLANG_IPP:
	*(int *) val += y;
	break;
	
      case SLANG_IMEQS:
	if (SLang_pop_integer(&y)) return;
	/* drop */
      case SLANG_IMM:
	*(int *) val -= y;
	break;
      default:
	SLang_Error = UNKNOWN_ERROR;
     }
}

/* lower 4 bits represent the return type, e.g., void, int, etc...
 The next 4 bits represent the number of parameters, 0 -> 15 */
#define SLANG_INTRINSIC_ARGC(f) ((f).sub_type >> 4)
#define SLANG_INTRINSIC_TYPE(f) ((f).sub_type & 0x0F)

static void lang_do_intrinsic(SLang_Name_Type *objf)
{
   typedef void (*VF0_Type)(void);
   typedef void (*VF1_Type)(char *);
   typedef void (*VF2_Type)(char *, char *);
   typedef void (*VF3_Type)(char *, char *, char *);
   typedef void (*VF4_Type)(char *, char *, char *, char *);
   typedef void (*VF5_Type)(char *, char *, char *, char *, char *);
   typedef void (*VF6_Type)(char *, char *, char *, char *, char *, char *);
   typedef void (*VF7_Type)(char *, char *, char *, char *, char *, char *, char *);
   typedef long (*LF0_Type)(void);
   typedef long (*LF1_Type)(char *);
   typedef long (*LF2_Type)(char *, char *);
   typedef long (*LF3_Type)(char *, char *, char *);
   typedef long (*LF4_Type)(char *, char *, char *, char *);
   typedef long (*LF5_Type)(char *, char *, char *, char *, char *);
   typedef long (*LF6_Type)(char *, char *, char *, char *, char *, char *);
   typedef long (*LF7_Type)(char *, char *, char *, char *, char *, char *, char *);
#ifdef FLOAT_TYPE
   typedef float64 (*FF0_Type)(void);
   typedef float64 (*FF1_Type)(char *);
   typedef float64 (*FF2_Type)(char *, char *);
   typedef float64 (*FF3_Type)(char *, char *, char *);
   typedef float64 (*FF4_Type)(char *, char *, char *, char *);
   typedef float64 (*FF5_Type)(char *, char *, char *, char *, char *);
   typedef float64 (*FF6_Type)(char *, char *, char *, char *, char *, char *);
   typedef float64 (*FF7_Type)(char *, char *, char *, char *, char *, char *, char *);
#endif
   long ret, fptr;
   char *p1, *p2, *p3, *p4, *p5, *p6, *p7;
   int free_p5 = 0, free_p4 = 0, free_p3 = 0, free_p2 = 0, free_p1 = 0;
   int free_p7 = 0, free_p6 = 0;
   unsigned char type;
   int argc;
#ifdef FLOAT_TYPE
   float64 xf;
#endif
   
   fptr = objf->addr;
   
   argc = SLANG_INTRINSIC_ARGC(*objf);
   type = SLANG_INTRINSIC_TYPE(*objf);
   
   /* These initializations are NOT needed but they shut up compilers
    * who try to to too smart by warning about un-initialized variables.
    * Unfortunately, the compilers are not smart enough.
    */
   p7 = p6 = p5 = p4 = p3 = p2 = p1 = NULL; 
   ret = 0;
#ifdef FLOAT_TYPE
   xf = 0.0;
#endif
   
   switch (argc)
     {
	unsigned char tmp1, tmp2;
      case 7: p7 = (char *) SLang_pop_pointer(&tmp1, &tmp2, &free_p7);
      case 6: p6 = (char *) SLang_pop_pointer(&tmp1, &tmp2, &free_p6);
      case 5: p5 = (char *) SLang_pop_pointer(&tmp1, &tmp2, &free_p5);
      case 4: p4 = (char *) SLang_pop_pointer(&tmp1, &tmp2, &free_p4);
      case 3: p3 = (char *) SLang_pop_pointer(&tmp1, &tmp2, &free_p3);
      case 2: p2 = (char *) SLang_pop_pointer(&tmp1, &tmp2, &free_p2);
      case 1: p1 = (char *) SLang_pop_pointer(&tmp1, &tmp2, &free_p1);
     }
   
   /* I need to put a setjmp here so to catch any long jmps that occur
    in the user program */
   if (SLang_Error)
     SLang_doerror (NULL);
   else switch (argc)
     {
	
      case 0:
	if (type == VOID_TYPE) ((VF0_Type) fptr) ();
#ifdef FLOAT_TYPE
	else if (type == FLOAT_TYPE) xf = ((FF0_Type) fptr)();
#endif
	else ret = ((LF0_Type) fptr)();
	break;
	
      case 1:
	if (type == VOID_TYPE) ((VF1_Type) fptr)(p1);
#ifdef FLOAT_TYPE
	else if (type == FLOAT_TYPE) xf =  ((FF1_Type) fptr)(p1);
#endif
	else ret =  ((LF1_Type) fptr)(p1);
	break;
	
      case 2:
	if (type == VOID_TYPE)  ((VF2_Type) fptr)(p1, p2);
#ifdef FLOAT_TYPE
	else if (type == FLOAT_TYPE) xf = ((FF2_Type) fptr)(p1, p2);
#endif
	else ret = ((LF2_Type) fptr)(p1, p2);
	break;
	
      case 3:
	if (type == VOID_TYPE) ((VF3_Type) fptr)(p1, p2, p3);
#ifdef FLOAT_TYPE
	else if (type == FLOAT_TYPE) xf = ((FF3_Type) fptr)(p1, p2, p3);
#endif
	else ret = ((LF3_Type) fptr)(p1, p2, p3);
	break;
	
      case 4:
	if (type == VOID_TYPE) ((VF4_Type) fptr)(p1, p2, p3, p4);
#ifdef FLOAT_TYPE
	else if (type == FLOAT_TYPE) xf = ((FF4_Type) fptr)(p1, p2, p3, p4);
#endif
	else ret = ((LF4_Type) fptr)(p1, p2, p3, p4);
	break;
	
      case 5:
	if (type == VOID_TYPE) ((VF5_Type) fptr)(p1, p2, p3, p4, p5);
#ifdef FLOAT_TYPE
	else if (type == FLOAT_TYPE) xf = ((FF5_Type) fptr)(p1, p2, p3, p4, p5);
#endif
	else ret = ((LF5_Type) fptr)(p1, p2, p3, p4, p5);
	break;
	
      case 6:
	if (type == VOID_TYPE) ((VF6_Type) fptr)(p1, p2, p3, p4, p5, p6);
#ifdef FLOAT_TYPE
	else if (type == FLOAT_TYPE) xf = ((FF6_Type) fptr)(p1, p2, p3, p4, p5, p6);
#endif
	else ret = ((LF6_Type) fptr)(p1, p2, p3, p4, p5, p6);
	break;
	
      case 7:
	if (type == VOID_TYPE) ((VF7_Type) fptr)(p1, p2, p3, p4, p5, p6, p7);
#ifdef FLOAT_TYPE
	else if (type == FLOAT_TYPE) xf = ((FF7_Type) fptr)(p1, p2, p3, p4, p5, p6, p7);
#endif
	else ret = ((LF7_Type) fptr)(p1, p2, p3, p4, p5, p6, p7);
	break;
	
      default:
	SLang_doerror("Function requires too many parameters");
	SLang_Error = UNKNOWN_ERROR;
	break;
     }
   
   switch(type)
     {
      case STRING_TYPE:
	if (NULL == (char *) ret)
	  {
	     if (!SLang_Error) SLang_Error = INTRINSIC_ERROR;
	  }
	else SLang_push_string((char *) ret); break;
	
      case INT_TYPE:
	SLang_push_integer ((int) ret); break;
	
      case VOID_TYPE: break;
#ifdef FLOAT_TYPE
      case FLOAT_TYPE: SLang_push_float (xf); break;
#endif
      default: SLang_Error = TYPE_MISMATCH;
     }
   
   /* I free afterword because functions that return char * may point to this
    space. */
   switch (argc)
     {
      case 7: if (free_p7 == 1) SLFREE(p7);
      case 6: if (free_p6 == 1) SLFREE(p6);
      case 5: if (free_p5 == 1) SLFREE(p5);
      case 4: if (free_p4 == 1) SLFREE(p4);
      case 3: if (free_p3 == 1) SLFREE(p3);
      case 2: if (free_p2 == 1) SLFREE(p2);
      case 1: if (free_p1 == 1) SLFREE(p1);
     }
#ifdef SLANG_NOOP
   pop_noop ();
#endif
}


static void lang_do_loops(unsigned char type, SLBlock_Type *block)
{
   register int i, ctrl = 0;
   int ctrl1;
   int first, last, one = 0;
   register SLBlock_Type *obj1, *obj2, *obj3;
   
   obj1 = block->b.blk;
   
   switch (type)
     {
      case SLANG_WHILE:
      case SLANG_DOWHILE:
	
	/* we need 2 blocks: first is the control, the second is code */
	block++;
	if (block->main_type != SLANG_BLOCK)
	  {
	     SLang_doerror("Block needed for while.");
	     return;
	  }
	obj2 = block->b.blk;
	
	if (type == SLANG_WHILE)
	  {
	     while(!SLang_Error)
	       {
		  inner_interp(obj1);
		  if (Lang_Break) break;
		  if (SLang_pop_integer(&ctrl1)) return;
		  if (!ctrl1) break;
		  inner_interp(obj2);
		  if (Lang_Break) break;
		  Lang_Break_Condition = Lang_Continue = 0;
	       }
	  }
	else while(!SLang_Error)
	  {
	     Lang_Break_Condition = Lang_Continue = 0;
	     inner_interp(obj1);
	     if (Lang_Break) break;
	     inner_interp(obj2);
	     if (SLang_pop_integer(&ctrl1)) return;
	     if (!ctrl1) break;
	  }
	break;
	
      case SLANG_CFOR:
	
	/* we need 4 blocks: first 3 control, the last is code */
	inner_interp(obj1);
	
	block++;
	if (block->main_type != SLANG_BLOCK) goto cfor_err;
	obj1 = block->b.blk;
	
	block++;
	if (block->main_type != SLANG_BLOCK) goto cfor_err;
	obj2 = block->b.blk;
	
	block++;
	if (block->main_type != SLANG_BLOCK) goto cfor_err;
	obj3 = block->b.blk;
	
	while(!SLang_Error)
	  {
	     inner_interp(obj1);       /* test */
	     if (SLang_pop_integer(&ctrl1)) return;
	     if (!ctrl1) break;
	     inner_interp(obj3);       /* code */
	     if (Lang_Break) break;
	     inner_interp(obj2);       /* bump */
	     Lang_Break_Condition = Lang_Continue = 0;
	  }
	break;
	
	cfor_err:
	SLang_doerror("Block needed for for.");
	return;
	
	
      case SLANG_FOR:  /* 3 elements: first, last, step */
	if (SLang_pop_integer(&ctrl1)) return;
	if (SLang_pop_integer(&last)) return;
	if (SLang_pop_integer(&first)) return;
	ctrl = ctrl1;
	if (ctrl >= 0)
	  {
	     for (i = first; i <= last; i += ctrl)
	       {
		  if (SLang_Error) return;
		  SLang_push_integer(i);
		  inner_interp(obj1);
		  if (Lang_Break) break;
		  Lang_Break_Condition = Lang_Continue = 0;
	       }
	  }
	else
	  {
	     for (i = first; i >= last; i += ctrl)
	       {
		  if (SLang_Error) return;
		  SLang_push_integer(i);
		  inner_interp(obj1);
		  if (Lang_Break) break;
		  Lang_Break_Condition = Lang_Continue = 0;
	       }
	  }
	
	break;
	
      case SLANG_LOOP:
	if (SLang_pop_integer(&ctrl1)) return;
	ctrl = ctrl1;
      case SLANG_FOREVER:
	if (type == SLANG_FOREVER) one = 1;
	while (one || (ctrl-- > 0))
	  {
	     if (SLang_Error) break;
	     inner_interp(obj1);
	     if (Lang_Break) break;
	     Lang_Break_Condition = Lang_Continue = 0;
	  }
	break;
	
      default:  SLang_doerror("Unknown loop type.");
     }
   Lang_Break = Lang_Continue = 0;
   Lang_Break_Condition = Lang_Return;
}

#if 0
static void lang_do_ifs(register SLBlock_Type *addr)
{
   register unsigned char stype;
   int test;
   
   stype = addr->sub_type;
   if (SLang_pop_integer(&test)) return;
   if (stype == SLANG_IF)
     {
	if (!test) return;
     }
   else if (stype == SLANG_IFNOT)
     {
	if (test) return;
     }
   else if (test) addr--;   /* SLANG_ELSE */
   
   if (addr->main_type != SLANG_BLOCK)
     {
	SLang_doerror("Block needed.");
	return;
     }
   inner_interp(addr->b.blk);
}
#endif

static void lang_do_else (unsigned char type, SLBlock_Type *addr, SLBlock_Type *addr_max)
{
   int test, status;
   char *str = NULL;
   SLang_Object_Type cobj;
   
   if (type == SLANG_SWITCH)
     {
	if (SLang_pop_non_object (&cobj)) return;
	if (cobj.sub_type == STRING_TYPE)
	  str = cobj.v.s_val;
     }
   
   while (addr <= addr_max)
     {
	if (type == SLANG_SWITCH)
	  {
	     if (str == NULL) SLang_push(&cobj); else SLang_push_string(str);
	  }
	
	status = inner_interp(addr->b.blk);
	if (SLang_Error || Lang_Break_Condition) return;
	if (type == SLANG_SWITCH)
	  {
	     if (status) break;
	  }
	
	else if (SLang_pop_integer(&test)) return;
	if (((type == SLANG_ANDELSE) && (test == 0))
	    || ((type == SLANG_ORELSE) && test))
	  {
	     break;
	  }
	addr++;
     }
   if (type != SLANG_SWITCH) SLang_push_integer(test);
   else if (str != NULL) SLFREE(str);
   return;
}

static void lang_dump(char *s)
{
   while (*s)
     {
	if (*s == '\n') fputc ('\r', stderr);
	fputc (*s, stderr);
	s++;
     }
}

void (*SLang_Dump_Routine)(char *) = lang_dump;

static void do_traceback(SLang_Name_Type *, int);
static SLBlock_Type *Exit_Block_Ptr;

static SLBlock_Type *Global_User_Block[5];
static SLBlock_Type **User_Block_Ptr = Global_User_Block;

void SLexecute_function(SLang_Name_Type *entry1)
{
   register int i;
   register SLang_Object_Type *frame, *lvf;
   register int n_locals;
   register SLang_Name_Type *entry = entry1;
   SLBlock_Type *val;
   static char buf[96];
   int trace_max, j;
   static int trace = 0;
   SLBlock_Type *exit_block_save;
   SLBlock_Type **user_block_save;
   SLBlock_Type *user_blocks[5];
   char *fname;
   
   n_locals = entry->sub_type;
   
   
   exit_block_save = Exit_Block_Ptr;
   user_block_save = User_Block_Ptr;
   User_Block_Ptr = user_blocks;
   *(user_blocks) = NULL;
   *(user_blocks + 1) = NULL;
   *(user_blocks + 2) = NULL;
   *(user_blocks + 3) = NULL;
   *(user_blocks + 4) = NULL;
   
   Exit_Block_Ptr = NULL;
   
   fname = entry->name + 1;
   
   /* need loaded?  */
   if (n_locals == 255)
     {
	if (!SLang_load_file((char *) entry->addr)) goto the_return;
	n_locals = entry->sub_type;
	if (n_locals == 255)
	  {
	     _SLdo_error ("%s: Function did not autoload!", fname);
             goto the_return;
	  }
     }
   
   /* let the lang error propagate through since it will do no harm
    and allow us to restore stack. */
   val = (SLBlock_Type *) entry->addr;
   /* set new stack frame */
   lvf = frame = Local_Variable_Frame;
   i = n_locals;
   if ((lvf + i) > Local_Variable_Stack + MAX_LOCAL_STACK)
     {
	_SLdo_error("%s: Local Variable Stack Overflow!", fname);
	goto the_return;
     }
   while(i--)
     {
	lvf++;
	lvf->main_type = 0;
     }
   Local_Variable_Frame = lvf;
   
   if (SLang_Enter_Function != NULL) (*SLang_Enter_Function)(fname);
   if (SLang_Trace)
     {
	if ((*SLang_Trace_Function == *entry->name)
	    && !strcmp(SLang_Trace_Function, entry->name)) trace = 1;
	
	trace_max = (trace > 50) ? 50 : trace - 1;
	if (trace)
	  {
	     for (j = 0; j < trace_max; j++) buf[j] = ' ';
	     sprintf(buf + trace_max, ">>%s\n", fname);
	     (*SLang_Dump_Routine)(buf);
	     trace++;
	  }
	
	inner_interp(val);
	Lang_Break_Condition = Lang_Return = Lang_Break = 0;
	if (Exit_Block_Ptr != NULL) inner_interp(Exit_Block_Ptr);
	
	if (trace)
	  {
	     sprintf(buf + trace_max, "<<%s\n", fname);
	     (*SLang_Dump_Routine)(buf);
	     trace--;
	     if (trace == 1) trace = 0;
	  }
     }
   else
     {
	inner_interp(val);
	Lang_Break_Condition = Lang_Return = Lang_Break = 0;
	if (Exit_Block_Ptr != NULL) inner_interp(Exit_Block_Ptr);
     }
   
   
   if (SLang_Exit_Function != NULL) (*SLang_Exit_Function)(fname);
   
   if (SLang_Error && SLang_Traceback)
     {
	do_traceback(entry, n_locals);
     }
   /* free local variables.... */
   lvf = Local_Variable_Frame;
   while(lvf > frame)
     {
	SLang_free_object (lvf);
	lvf--;
     }
   Local_Variable_Frame = lvf;
   
   the_return:
   Lang_Break_Condition = Lang_Return = Lang_Break = 0;
   Exit_Block_Ptr = exit_block_save;
   User_Block_Ptr = user_block_save;
#ifdef SLANG_NOOP
   pop_noop ();
#endif
}



static void do_traceback(SLang_Name_Type *nt, int locals)
{
   char buf[80];
   char *s;
   int i;
   SLang_Object_Type *objp;
   unsigned short stype;
   
   sprintf(buf, "S-Lang Traceback: %s\n",nt->name + 1);
   (*SLang_Dump_Routine)(buf);
   if (!locals) return;
   (*SLang_Dump_Routine)("  Local Variables:\n");
   
   for (i = 0; i < locals; i++)
     {
	objp = Local_Variable_Frame - i;
	stype = objp->sub_type;
	
	sprintf(buf, "\t$%d: ", i);
	(*SLang_Dump_Routine)(buf);
	
	s = SLstringize_object (objp);
	
	if (s == NULL) (*SLang_Dump_Routine)("??");
	else
	  {
	     if (STRING_TYPE == stype) (*SLang_Dump_Routine) ("\"");
	     (*SLang_Dump_Routine)(s);
	     if (STRING_TYPE == stype) (*SLang_Dump_Routine) ("\"");
	     SLFREE (s);
	  }
	(*SLang_Dump_Routine)("\n");
     }
}


static void call_funptr(SLang_Name_Type *optr)
{
   SLBlock_Type objs[2];
   
   if (optr == NULL)
     {
	SLang_doerror("Object Ptr is Nil!");
	return;
     }
   
   objs[0].b.n_blk = optr;
   objs[0].sub_type = optr->sub_type; objs[0].main_type = optr->main_type;
   objs[1].main_type = 0;
   inner_interp(objs);
}


#ifdef SLANG_STATS
static unsigned long stat_counts[256];
#endif

void (*SLang_Interrupt)(void);

static int Last_Error;
void (*SLang_User_Clear_Error)(void);
void SLang_clear_error (void)
{
   if (Last_Error <= 0)
     {
	Last_Error = 0;
	return;
     }
   Last_Error--;
   if (SLang_User_Clear_Error != NULL) (*SLang_User_Clear_Error)();
}

static void do_binary (int op)
{
   SLang_Object_Type obja, objb;
   SLang_Class_Type *cl;
   SL_OOBinary_Type *bt;
   unsigned char b_sub_type, a_sub_type;
   char *err = "Operation undefined for type.";
   
   if (SLang_pop (&objb)) return;
   b_sub_type = objb.sub_type;
   
   if (SLang_pop (&obja)) goto free_objb;
   a_sub_type = obja.sub_type;
   
   /* See if obja has a registered binary operation */
   cl = SLang_Registered_Types [a_sub_type];
   if ((cl == NULL) || (NULL == (bt = cl->binary_ops)))
     {
	SLang_doerror (err);
	goto free_obja;
     }
   
   /* The class is registered, now look for binary operation between the types. */
   while (bt != NULL)
     {
	if (bt->sub_type == b_sub_type)
	  {
	     VOID_STAR p1, p2;
	     
	     if (a_sub_type < STRING_TYPE) p1 = (VOID_STAR) &obja.v.s_val;
	     else if (a_sub_type >= ARRAY_TYPE) p1 = (VOID_STAR) obja.v.uobj->obj;
	     else p1 = (VOID_STAR) obja.v.s_val;
	     
	     if (b_sub_type < STRING_TYPE) p2 = (VOID_STAR) &objb.v.s_val;
	     else if (b_sub_type >= ARRAY_TYPE) p2 = (VOID_STAR) objb.v.uobj->obj;
	     else p2 = (VOID_STAR) objb.v.s_val;
	     
	     if (0 == (*(bt->binary_function)) (op, a_sub_type, b_sub_type, p1, p2))
	       SLang_doerror (err);
	     
	     goto free_obja;
	  }
	bt = bt->next;
     }
   
   SLang_Error = TYPE_MISMATCH;
   
   free_obja:
   SLang_free_object (&obja);
   
   free_objb:
   SLang_free_object (&objb);
}

static void do_unary (int op)
{
   SLang_Object_Type obj;
   SLang_Class_Type *cl;
   int (*f)_PROTO((int, unsigned char, VOID_STAR));
   unsigned char sub_type;
   char *err = "Operation undefined for type.";
   
   if (SLang_pop (&obj)) return;
   
   sub_type = obj.sub_type;
   
   /* See if obj has a registered unary operation */
   cl = SLang_Registered_Types [sub_type];
   
   if ((cl != NULL) && (NULL != (f = cl->unary_function)))
     {
	VOID_STAR p1;
	
	if (sub_type < STRING_TYPE) p1 = (VOID_STAR) &obj.v.s_val;
	else if (sub_type >= ARRAY_TYPE) p1 = (VOID_STAR) obj.v.uobj->obj;
	else p1 = (VOID_STAR) obj.v.s_val;
	
	if (0 == (*f)(op, sub_type, p1)) SLang_doerror (err);
     }
   else SLang_Error = TYPE_MISMATCH;
   
   SLang_free_object (&obj);
}


/* inner interpreter */
static int inner_interp(SLBlock_Type *addr1)
{
   register int bc = 0;
   register SLang_Object_Type *val;
   register SLBlock_Type *addr;
   SLang_Object_Type obj1, obj2, *objp;
   /* register unsigned short type; */
   /* register unsigned char stype; */
   int x, y, z;
   SLBlock_Type *block = NULL;
   SLBlock_Type *err_block = NULL;
   int save_err, slerr;
   
   /* for systems that have no real interrupt facility (e.g. go32 on dos) */
   if (SLang_Interrupt != NULL) (*SLang_Interrupt)();
   addr = addr1;
   if (addr == NULL)
     {
	SLang_Error = UNKNOWN_ERROR;
     }
   
   while (SLang_Error == 0)
     {
	if (bc)
	  {
	     if (SLang_Error) break;
	     if (Lang_Return || Lang_Break)
	       {
		  Lang_Break = 1;
		  return(1);
	       }
	     if (Lang_Continue) return(1);
	  }
	
#ifdef SLANG_STATS
	stat_counts[(unsigned char) (type & 0xFF)] += 1;
#endif
	switch (addr->main_type)
	  {
	   case 0:
	     goto end_of_switch;
#ifdef SLANG_NOOP
	   case SLANG_NOOP_DIRECTIVE:
	     obj1.main_type = SLANG_NOOP;
	     obj1.sub_type = 0;
	     SLang_push (&obj1);
	     break;
#endif
	   case SLANG_LVARIABLE:
	     /* make val point to local stack */
	     val =  (Local_Variable_Frame - addr->b.i_blk);
	     SLang_push_variable (val);
	     break;
	     
	   case SLANG_BINARY:
	     do_binary (addr->sub_type);
	     break;
	     
	   case SLANG_LOGICAL:
	     
	     if (SLang_pop_integer(&y)) return 0;
	     if (SLang_pop_integer(&x)) return 0;
	     z = 0;
	     switch (addr->sub_type)
	       {
		case SLANG_OR: if (x || y) z = 1; break;
		case SLANG_AND: if (x && y) z = 1; break;
		case SLANG_BAND: z = x & y; break;
		case SLANG_BXOR: z = x ^ y; break;
		case SLANG_MOD: z = x % y; break;
		case SLANG_BOR: z = x | y; break;
		case SLANG_SHL: z = x << y; break;
		case SLANG_SHR: z = x >> y; break;
		default: SLang_Error = INTERNAL_ERROR;
		  return(0);
	       }
	     SLang_push_integer(z);
	     break;
	     
	   case SLANG_INTRINSIC:
	     lang_do_intrinsic(addr->b.n_blk);
	     if (SLang_Error && SLang_Traceback)
	       {
		  do_traceback(addr->b.n_blk, 0);
	       }
	     break;
	     
	   case SLANG_FUNCTION:
	     
	     SLexecute_function(addr->b.n_blk);
	     bc = Lang_Break_Condition;
	     break;
	     
	   case SLANG_DATA:
	     SLang_push_string (addr->b.s_blk);
	     break;
	     
	   case SLANG_LITERAL:        /* a constant */
	     obj1.main_type = addr->main_type;
	     obj1.sub_type = addr->sub_type;
#ifdef FLOAT_TYPE
	     /* The value is a pointer to the float */
	     if (obj1.sub_type == FLOAT_TYPE)
	       {
		  obj1.v.f_val = *addr->b.f_blk;
	       }
	     else
#endif
	       obj1.v.l_val = addr->b.l_blk;
	     SLang_push(&obj1);
	     break;
	     
	   case SLANG_BLOCK:
	     switch (addr->sub_type)
	       {
		case ERROR_BLOCK:
		  err_block = addr;
		  break;
		  
		case EXIT_BLOCK:
		  Exit_Block_Ptr = addr->b.blk;
		  break;
		  
		case USER_BLOCK0:
		case USER_BLOCK1:
		case USER_BLOCK2:
		case USER_BLOCK3:
		case USER_BLOCK4:
		  User_Block_Ptr[addr->sub_type - USER_BLOCK0] = addr->b.blk;
		  break;
		  
		case SLANG_LOOP:
		case SLANG_WHILE:
		case SLANG_FOR:
		case SLANG_FOREVER:
		case SLANG_CFOR:
		case SLANG_DOWHILE:
		  if (block == NULL) block =  addr;
		  lang_do_loops(addr->sub_type, block);
		  block = NULL;
		  break;
		  
		  
		case SLANG_IFNOT:
		    {
		       int test;
		       if (SLang_pop_integer(&test)) break;
		       if (test) break;
		       inner_interp(addr->b.blk);
		       block = NULL;
		       break;
		    }
		  
		case SLANG_IF:
		    {
		       int test;
		       if (SLang_pop_integer(&test)) break;
		       if (!test) break;
		       inner_interp(addr->b.blk);
		       block = NULL;
		       break;
		    }
		  
		case SLANG_ELSE:
		    {
		       int test;
		       if (block == NULL)
			 {
			    SLang_doerror("Block needed for ELSE");
			    break;
			 }
		       
		       if (SLang_pop_integer(&test)) break;
		       if (!test) block = addr;
		       inner_interp(block->b.blk);
		       block = NULL;
		       break;
		    }
		  
		case SLANG_ANDELSE:
		case SLANG_ORELSE:
		case SLANG_SWITCH:
		  if (block == NULL) block = addr;
		  lang_do_else (addr->sub_type, block, addr);
		  block = NULL;
		  break;
		  
		default:
		  if (block == NULL) block =  addr;
		  break;
	       }
	     bc = Lang_Break_Condition;
	     break;
	     
	   case SLANG_EQS:
	     lang_do_eqs(addr);
	     break;
	     
	   case SLANG_UNARY:
	     do_unary (addr->sub_type);
	     break;
	     
	   case SLANG_LUNARY:
	     if (SLang_pop_integer (&x)) return 0;
	     
	     /* There are only to in this category */
	     if (addr->sub_type == SLANG_NOT) x = !x;
	     else x = ~x;
	     SLang_push_integer (x);
	     break;
	     
	   case SLANG_GVARIABLE:
	     SLang_push_variable((SLang_Object_Type *) addr->b.n_blk->addr);
	     break;
	     
	   case SLANG_IVARIABLE:
	   case SLANG_RVARIABLE:
	     
	     switch (addr->sub_type)
	       {
		case STRING_TYPE:
		  SLang_push_string((char *) addr->b.n_blk->addr);
		  break;
		case INT_TYPE:
		  SLang_push_integer(*(int *) addr->b.n_blk->addr);
		  break;
		case INTP_TYPE:
		  SLang_push_integer(**(int **) addr->b.n_blk->addr);
		  break;
#ifdef FLOAT_TYPE
		case FLOAT_TYPE:
		  SLang_push_float(*(float64 *) addr->b.n_blk->addr);
		  break;
#endif
		default:
		  if (addr->sub_type >= ARRAY_TYPE)
		    {
		       SLang_push_user_object ((SLuser_Object_Type *) addr->b.n_blk->addr);
		       break;
		    }
		  else SLang_doerror("Unsupported Type!");
	       }
	     
	     break;
	     
	   case SLANG_RETURN:
	     Lang_Break_Condition = Lang_Return = Lang_Break = 1; return(1);
	   case SLANG_BREAK:
	     Lang_Break_Condition = Lang_Break = 1; return(1);
	   case SLANG_CONTINUE:
	     Lang_Break_Condition = Lang_Continue = 1; return(1);
	     
	   case SLANG_EXCH: if (SLang_pop(&obj1) || SLang_pop(&obj2)) return(1);
	     /* Memory leak here if one of the pops failed and one object was
	      * a user object.
	      */
	     SLang_push(&obj1); SLang_push(&obj2);
	     break;
	     
	   case SLANG_LABEL:
	     if (SLang_pop_integer(&z) || !z) return(0);
	     break;
	     
	   case SLANG_LOBJPTR:
	     objp = (Local_Variable_Frame - addr->b.i_blk);
	     if (objp->main_type == 0)
	       {
		  SLang_doerror("Local variable pointer not initialized.");
		  break;
	       }
	     
	     obj1.v.n_val = objp->v.n_val;
	     obj1.sub_type = SLANG_OBJ_TYPE; obj1.main_type = SLANG_DATA;
	     SLang_push(&obj1);
	     break;
	     
	   case SLANG_GOBJPTR:
	     obj1.v.n_val = addr->b.n_blk;
	     obj1.main_type = SLANG_DATA; obj1.sub_type = SLANG_OBJ_TYPE;
	     SLang_push(&obj1);
	     break;
	     
	   case SLANG_X_USER0:
	   case SLANG_X_USER1:
	   case SLANG_X_USER2:
	   case SLANG_X_USER3:
	   case SLANG_X_USER4:
	     if (User_Block_Ptr[addr->main_type - SLANG_X_USER0] != NULL)
	       {
		  inner_interp(User_Block_Ptr[addr->main_type - SLANG_X_USER0]);
	       }
	     else SLang_doerror("No block for SLANG_X_USER");
	     bc = Lang_Break_Condition;
	     break;
	     
	   case SLANG_X_ERROR:
	     if (err_block != NULL)
	       {
		  inner_interp(err_block->b.blk);
		  if (SLang_Error) err_block = NULL;
	       }
	     else SLang_doerror("No ERROR_BLOCK");
	     bc = Lang_Break_Condition;
	     break;
	     
	     /* default : SLang_doerror("Run time error."); */
	  }
	
	addr++;
     }
   
   end_of_switch:
   
   if ((SLang_Error) && (err_block != NULL) &&
       ((SLang_Error == USER_BREAK) || (SLang_Error == INTRINSIC_ERROR)))
     {
	save_err = Last_Error++;
        slerr = SLang_Error;
	SLang_Error = 0;
	inner_interp(err_block->b.blk);
	if (Last_Error <= save_err)
	  {
	     /* Caught error and cleared it */
	     Last_Error = save_err;
	     if (Lang_Break_Condition == 0) inner_interp(addr);
	  }
	else
	  {
	     Last_Error = save_err;
	     SLang_Error = slerr;
	  }
     }
   
   return(1);
}

/* Hash value of current item to search in table */
static unsigned char Hash;

static unsigned char compute_hash (unsigned char *s)
{
   register unsigned char *ss = s;
   register unsigned int h = 0;
   
   while (*ss) h += (unsigned int) *ss++ + (h << 2);
   
   
   if (0 == (Hash = (unsigned char) h))
     {
	Hash = (unsigned char) (h >> 8);
	if (!Hash) 
	  {
	     Hash = *s;
	     if (Hash == 0)
	       Hash = 1;
	  }
     }
   
   return(Hash);
}

static SLang_Name_Type *SLang_locate_name_in_table(char *name, SLang_Name_Type *table, SLang_Name_Type *t0, int max)
{
   register SLang_Name_Type *t = t0, *tmax = table + max;
   register char h = Hash, h1;
   
   /* while(t != tmax) && (nm = t->name, (h1 = *nm) != 0)) */
   while(t != tmax)
     {
	h1 = *t->name;
	/* h is never 0 */
	if ((h1 == h) && !strcmp(t->name + 1,name))
	  {
#ifdef SLANG_STATS
	     t->n++;
#endif
	     return(t);
	  }
	else if (h1 == 0) break;
	t++;
     }
   if (t == tmax) return(NULL);
   return(t);
}

void SLang_trace_fun(char *f)
{
   SLang_Trace = 1;
   compute_hash((unsigned char *) f);
   *SLang_Trace_Function = Hash;
   strcpy((char *) SLang_Trace_Function + 1, f);
}

#ifdef SLANG_STATS
int SLang_dump_stats(char *file)
{
   SLang_Name_Type *t = Lang_Intrinsic_Name_Table;
   int i;
   FILE *fp;
   if ((fp = fopen(file, "w")) == NULL) return(0);
   while (*t->name != 0)
     {
	fprintf(fp, "%3d\t%3d\t%s\n", t->n, (int) (unsigned char) *t->name, t->name + 1);
	t++;
     }
   for (i = 0; i < 256; i++) fprintf(fp, "Count %d: %lu\n", i, stat_counts[i]);
   
   fclose(fp);
   return(1);
}
#endif

/* before calling this routine, make sure that Hash is up to date */
static SLang_Name_Type *SLang_locate_global_name(char *name)
{
   SLName_Table *nt;
   SLang_Name_Type *t;
   int ofs;
   
   nt = SLName_Table_Root;
   while (nt != NULL)
     {
	t = nt->table;
	
	if ((ofs = nt->ofs[Hash]) != -1)
	  {
	     t = SLang_locate_name_in_table(name, t, t + ofs, nt->n);
	     if ((t != NULL) && (*t->name != 0)) return(t);
	  }
	
	nt = nt->next;
     }
   ofs = SLang_Name_Table_Ofs [Hash];
   if (ofs == -1) ofs = SLang_Name_Table_Ofs [0];
   return SLang_locate_name_in_table(name, SLang_Name_Table, SLang_Name_Table + ofs, SLANG_MAX_SYMBOLS);
}



SLang_Name_Type *SLang_locate_name(char *name)
{
   SLang_Name_Type *t;
   
   if (*name == 0) return NULL;
   
   (void) compute_hash((unsigned char *) name);
   
   t = Lang_Local_Variable_Table;
   
   if (t != NULL)
     {
	t = SLang_locate_name_in_table(name, t, t, Local_Variable_Number);
	/* MAX_LOCAL_VARIABLES */
     }
   
   if ((t == NULL) || (*t->name == 0)) t = SLang_locate_global_name(name);
   return(t);
}


/* check syntax.  Allowed chars are: $!_?AB..Zab..z0-9 */
static int lang_check_name(char *name)
{
   register char *p, ch;
   
   p = name;
   while ((ch = *p++) != 0)
     {
	if ((ch >= 'a') && (ch <= 'z')) continue;
	if ((ch >= 'A') && (ch <= 'Z')) continue;
	if ((ch >= '0') && (ch <= '9')) continue;
	if ((ch == '_') || (ch == '$') || (ch == '!') || (ch == '?')) continue;
	_SLdo_error ("%s: bad name syntax.", name);
	return(0);
     }
   
   p--;
   if ((int) (p - name) > SLANG_MAX_NAME_LEN)
     {
	_SLdo_error ("%s: Name too long.", name);
	return(0);
     }
   return (1);
}



void SLadd_name(char *name, long addr, unsigned char main_type, unsigned char sub_type)
{
   SLang_Name_Type *entry;
   unsigned char mtype;
   int ofs, this_ofs;
   
   if (!lang_check_name(name)) return;
   if (NULL == (entry = SLang_locate_name(name)))
     {
	_SLdo_error ("%s: table size exceeded.", name);
	return;  /* table full */
     }
   
   mtype = entry->main_type;
   
   if ((mtype == SLANG_INTRINSIC) || (mtype == SLANG_IVARIABLE)
       || (mtype == SLANG_RVARIABLE))
     {
	
	/* Allow application to change what the binding of a given object
	 * is but do not allow a user function to have same name as something
	 * intrinsic.  It must be the same base type though. */
	if (main_type != mtype)
	  {
	     SLang_Error = DUPLICATE_DEFINITION;
	     SLang_doerror (name);
	     return;
	  }
     }
   
   if (*entry->name != 0)
     {
	/* 255 denotes that the function needs autoloaded. */
	if (mtype == SLANG_FUNCTION)
	  {
	     if (entry->sub_type != 255)
	       {
		  if (lang_free_branch((SLBlock_Type *) entry->addr))
		    SLFREE(entry->addr);
	       }
	     else SLFREE(entry->addr);
	  }
     }
   else
     {
	strcpy(entry->name + 1, name);
	*entry->name = (char) Hash;
	ofs = SLang_Name_Table_Ofs [Hash];
	this_ofs = (int) (entry - SLang_Name_Table);
	if (ofs == -1)		       /* unused */
	  {
	     SLang_Name_Table_Ofs [Hash] = this_ofs;
	     SLang_Name_Table_Ofs [0] = this_ofs;
	  }
     }
   
   entry->addr = (long) addr;
   entry->sub_type = sub_type;
   entry->main_type = main_type;
}

void SLang_autoload(char *name, char *file)
{
   long f;
   SLang_Name_Type *nt;

   (void) compute_hash((unsigned char *) name);
   
   nt = SLang_locate_global_name (name);
   if ((nt != NULL) && (*nt->name != 0))
     {
	/* Function already defined but not autoloaded? */
   	if ((nt->main_type == SLANG_FUNCTION)
	    && (nt->sub_type != 255))
	  return;
     }
   
   f = (long) SLmake_string(file);
   
   SLadd_name (name, f, SLANG_FUNCTION, 255);
}


static void lang_define_function(char *name)
{
   long addr;
   
   /* terminate function */
   Lang_Object_Ptr->main_type = 0;
   
   if (Lang_Function_Body + 1 == Lang_Object_Ptr)
     {
	if (Lang_Function_Body -> main_type == SLANG_RETURN)
	  {
	     SLFREE (Lang_Function_Body);
	     Lang_Function_Body = SLShort_Blocks;
	  }
     }
   
   addr = (long) Lang_Function_Body;
   
   if (name != NULL)
     {
	SLadd_name(name, addr, SLANG_FUNCTION, Local_Variable_Number);
     }
   
   
   if (SLang_Error) return;
   Lang_Defining_Function = 0;
   if (Lang_Local_Variable_Table != NULL) SLFREE(Lang_Local_Variable_Table);
   Lang_Local_Variable_Table = NULL;
   Local_Variable_Number = 0;
   
   Lang_Object_Ptr = Lang_Interp_Stack_Ptr;   /* restore pointer */
}

/* call inner interpreter or return for more */
static void lang_try_now(void)
{
   SLBlock_Type *old_stack, *old_stack_ptr, *old_int_stack_ptr;
   SLBlock_Type new_stack[SLANG_MAX_TOP_STACK];
   int i;
   
   if (Lang_Defining_Function || Lang_Defining_Block)
     {
	Lang_Object_Ptr++;
	return;
     }
   
   /* This is the entry point into the inner interpreter.  As a result, it
    * is also the exit point of the inner interpreter.  So it is necessary to
    * clean up if there was an error.
    */
   
   (Lang_Object_Ptr + 1)->main_type = 0;  /* so next command stops after this */
   
   /* now before entering the inner interpreter, we make a new stack so that
    we are able to be reentrant */
   
   for (i = 1; i < 4; i++)
     {
	new_stack[i].main_type = 0;
	new_stack[i].b.blk = NULL;
     }
   
   /* remember these values */
   old_int_stack_ptr = Lang_Interp_Stack_Ptr;
   old_stack_ptr = Lang_Object_Ptr;
   old_stack = Lang_Interp_Stack;
   
   /* new values for reentrancy */
   Lang_Interp_Stack_Ptr = Lang_Object_Ptr = Lang_Interp_Stack = new_stack;
   
   /* now do it */
   inner_interp(old_stack);
   
   /* we are back so restore old pointers */
   Lang_Interp_Stack_Ptr = old_int_stack_ptr;
   Lang_Object_Ptr = old_stack_ptr;
   Lang_Interp_Stack = old_stack;
   
   /* now free blocks from the current interp_stack.  There can only
    be blocks since they are only objects not evaluated immediately */
   
   while (Lang_Object_Ptr != Lang_Interp_Stack)
     {
	/* note that top object is not freed since it was not malloced */
	Lang_Object_Ptr--;
	if (lang_free_branch(Lang_Object_Ptr->b.blk))
	  SLFREE (Lang_Object_Ptr->b.blk);
     }
   
   /* now free up the callocd stack.
    SLFREE(new_stack); */
}


#define eqs(a,b) ((*(a) == *(b)) && !strcmp(a,b))
int SLang_execute_function(char *name)
{
   unsigned char type;
   SLang_Name_Type *entry;
   if ((NULL == (entry = SLang_locate_name(name))) || (*entry->name == 0)) return(0);
   type = entry->main_type;
   if (type == SLANG_FUNCTION) SLexecute_function(entry);
   else if (type == SLANG_INTRINSIC)
     lang_do_intrinsic(entry);
   else return(0);
   
   if (SLang_Error) SLang_doerror (name);
   
   return(1);
}

/* return S-Lang function or NULL */
SLang_Name_Type *SLang_get_function (char *name)
{
   SLang_Name_Type *entry;
   
   if ((NULL == (entry = SLang_locate_name(name))) || (*entry->name == 0))
     return NULL;
   if (entry->main_type == SLANG_FUNCTION)
     {
	return entry;
     }
   return NULL;
}

/* Look for name ONLY in local or global slang tables */
static SLang_Name_Type *SLang_locate_slang_name (char *name)
{
   SLang_Name_Type *entry;
   int ofs;
   
   compute_hash ((unsigned char *) name);
   /* try local table first */
   entry = Lang_Local_Variable_Table;
   if (entry != NULL)
     {
	entry = SLang_locate_name_in_table(name, entry, entry, Local_Variable_Number);
     }
   if ((entry == NULL) || (*entry->name == 0))
     {
	ofs = SLang_Name_Table_Ofs [Hash];
	if (ofs == -1) ofs = SLang_Name_Table_Ofs [0];
	entry = SLang_locate_name_in_table(name, SLang_Name_Table, SLang_Name_Table + ofs, SLANG_MAX_SYMBOLS);
     }
   return entry;
}


static int lang_exec(char *name, int all)
{
   SLang_Name_Type *entry;
   unsigned char main_type;
   int ptr_type = 0;
   int i = 0;
   
   if (all &&
       (eqs(name, "EXECUTE_ERROR_BLOCK")
	|| ((*name == 'X') &&
	    !strncmp ("X_USER_BLOCK", name, 12) &&
	    ((i = name[12]) < '5') && (i >= '0')
	    && (name[13] == 0))))
     {
	if (*name == 'X')
	  {
	     Lang_Object_Ptr->main_type = SLANG_X_USER0 + (i - '0');
	  }
	else Lang_Object_Ptr->main_type = SLANG_X_ERROR;
	Lang_Object_Ptr->b.blk = NULL;
	lang_try_now ();
	return 1;
     }
   
   if (*name == '&')
     {
	name++;
	ptr_type = 1;
     }
   
   if (all) entry = SLang_locate_name(name);
   else entry = SLang_locate_slang_name (name);
   if ((entry == NULL) || (*entry->name == 0)) return(0);
   
   
   main_type = entry->main_type;
   if (ptr_type)
     {
	Lang_Object_Ptr->main_type = ((main_type == SLANG_LVARIABLE)
				      ? SLANG_LOBJPTR : SLANG_GOBJPTR);
     }
   else
     {
	Lang_Object_Ptr->main_type = main_type;
	Lang_Object_Ptr->sub_type = entry->sub_type;
     }
   
   if (main_type == SLANG_LVARIABLE)
     {
	Lang_Object_Ptr->b.i_blk = (int) entry->addr;
     }
   else
     {
	Lang_Object_Ptr->b.n_blk = entry;
     }
   
   lang_try_now();
   return(1);
}



static int lang_try_binary(char *t)
{
   int ssub;
   unsigned char sub, mtype;
   ssub = 0;
   
   if (0 == (ssub = _SLeqs_name(t, SL_Binary_Ops))) return(0);
   
   if (ssub < 0)
     {
	ssub = -ssub;
	mtype = SLANG_BINARY;
     }
   else mtype = SLANG_LOGICAL;
   sub = (unsigned char) ssub;
   
   Lang_Object_Ptr->b.blk = NULL;         /* not used */
   
   Lang_Object_Ptr->sub_type = sub;
   Lang_Object_Ptr->main_type = mtype;
   
   lang_try_now();
   return(1);
}

static int lang_try_unary(char *t)
{
   unsigned char ssub;
   unsigned char mtype = SLANG_LUNARY;
   
   if (eqs(t, "not")) ssub = SLANG_NOT;
   else if (eqs(t, "~")) ssub = SLANG_BNOT;
   else
     {
	mtype = SLANG_UNARY;
	if (eqs(t, "chs")) ssub = SLANG_CHS;
	else if (eqs(t, "sign")) ssub = SLANG_SIGN;
	else if (eqs(t, "abs")) ssub = SLANG_ABS;
	else if (eqs(t, "sqr")) ssub = SLANG_SQR;
	else if (eqs(t, "mul2")) ssub = SLANG_MUL2;
	else return(0);
     }
   
   Lang_Object_Ptr->main_type = mtype;
   Lang_Object_Ptr->sub_type = ssub;
   Lang_Object_Ptr->b.blk = NULL;         /* not used */
   
   lang_try_now();
   return(1);
}

static void lang_begin_function(void)
{
   if (Lang_Defining_Function || Lang_Defining_Block)
     {
	SLang_doerror ("Function nesting illegal.");
	return;
     }
   
   Lang_Defining_Function = 1;
   
   /* make initial size for 3 things */
   Lang_FBody_Size = 3;
   if (NULL == (Lang_Function_Body = (SLBlock_Type *)
		SLCALLOC(Lang_FBody_Size, sizeof(SLBlock_Type))))
     {
	SLang_doerror("Calloc error defining function.");
	return;
     }
   /* function definitions should be done only at top level so it should be
    safe to do this: */
   Lang_Interp_Stack_Ptr = Lang_Object_Ptr;
   Lang_Object_Ptr = Lang_FBody_Ptr = Lang_Function_Body;
   return;
}


static void lang_end_block(void)
{
   SLBlock_Type *node, *branch;
   unsigned char mtype;
   Lang_Block_Depth--;
   
   /* terminate the block */
   Lang_Object_Ptr->main_type = 0;
   branch = Lang_Block_Body;
   
   if (Lang_Object_Ptr == Lang_Block_Body + 1)
     {
	mtype = (Lang_Object_Ptr - 1)->main_type;
	if ((mtype == SLANG_BREAK) || (mtype == SLANG_CONTINUE) || (mtype == SLANG_RETURN))
	  {
	     SLFREE (branch);
	     branch = SLShort_Blocks + (int) (mtype - SLANG_RETURN);
	  }
     }
   
   
   if (Lang_Block_Depth == -1)         /* done */
     {
	if (Lang_Defining_Function)
	  {
	     node = Lang_FBody_Ptr++;
	  }
	else node = Lang_Interp_Stack_Ptr;   /* on small stack */
     }
   else                                /* pop previous block */
     {
	Lang_BBody_Size = Lang_Block_Stack[Lang_Block_Depth].size;
	Lang_Block_Body = Lang_Block_Stack[Lang_Block_Depth].body;
	node = Lang_Block_Stack[Lang_Block_Depth].ptr;
     }
   
   node->main_type = SLANG_BLOCK;
   node->sub_type = 0;
   node->b.blk = branch;
   Lang_Object_Ptr = node + 1;
   Lang_Defining_Block--;
}

static void lang_begin_block(void)
{
   if (Lang_Block_Depth == SLANG_MAX_BLOCKS - 1)
     {
	SLang_doerror("Block Nesting too deep.");
	SLang_Error = UNKNOWN_ERROR;
	return;
     }
   /* push the current block onto the stack */
   if (Lang_Block_Depth > -1)
     {
	Lang_Block_Stack[Lang_Block_Depth].size = Lang_BBody_Size;
	Lang_Block_Stack[Lang_Block_Depth].body = Lang_Block_Body;
	Lang_Block_Stack[Lang_Block_Depth].ptr = Lang_Object_Ptr;
     }
   
   /* otherwise this is first block so save function pointer */
   else if (Lang_Defining_Function) Lang_FBody_Ptr = Lang_Object_Ptr;
   else Lang_Interp_Stack_Ptr = Lang_Object_Ptr;
   
   Lang_BBody_Size = 5;    /* 40 bytes */
   if (NULL == (Lang_Block_Body = (SLBlock_Type *) SLCALLOC(Lang_BBody_Size,
							    sizeof(SLBlock_Type))))
     {
	SLang_Error = SL_MALLOC_ERROR;
	/* SLang_doerror("Malloc error defining block."); */
	return;
     }
   Lang_Block_Depth++;
   Lang_Defining_Block++;
   Lang_Object_Ptr = Lang_Block_Body;
   return;
}


/* see if token is a directive, and add it to current block/function */
static SLang_Name2_Type Lang_Directives[] =
{
     {"!if", SLANG_IFNOT},
     {"if", SLANG_IF},
     {"else", SLANG_ELSE},
     {"forever", SLANG_FOREVER},
     {"while", SLANG_WHILE},
     {"for", SLANG_CFOR},
     {"_for", SLANG_FOR},
     {"loop", SLANG_LOOP},
     {"switch", SLANG_SWITCH},
     {"do_while", SLANG_DOWHILE},
     {"andelse", SLANG_ANDELSE},
     {"orelse", SLANG_ORELSE},
     {"ERROR_BLOCK", ERROR_BLOCK},
     {"EXIT_BLOCK", EXIT_BLOCK},
     {"USER_BLOCK0", USER_BLOCK0},
     {"USER_BLOCK1", USER_BLOCK1},
     {"USER_BLOCK2", USER_BLOCK2},
     {"USER_BLOCK3", USER_BLOCK3},
     {"USER_BLOCK4", USER_BLOCK4},
     {(char *) NULL, (int) NULL}
};


static int try_directive(char *t, int *flag)
{
   unsigned char sub = 0;
   unsigned short mtype;
   SLBlock_Type *lop;
   int flag_save;
   
   Lang_Object_Ptr->b.blk = 0;         /* not used */
   
   if (((sub = (unsigned char) _SLeqs_name(t, Lang_Directives)) != 0)
       && *flag)
     {
	lop = Lang_Object_Ptr - 1;
	
	if (lop->main_type != SLANG_BLOCK) SLang_doerror("Internal Error with block!");
	else
	  {
	     lop->sub_type = sub;

	     /* Lang_Object_Ptr = lop; */
#if 1
	     Lang_Object_Ptr->main_type = 0;
	     if (Lang_Defining_Function || Lang_Defining_Block)
	       return 1;
#endif
	  }
     }
   
   /* rest valid only if flag is zero */
   else if (*flag) return(0);
   else
     {
	if (Lang_Defining_Block && eqs(t, "continue")) mtype = SLANG_CONTINUE;
	else if (Lang_Defining_Block && eqs(t, "break")) mtype = SLANG_BREAK;
	else if (Lang_Defining_Function && eqs(t, "return")) mtype = SLANG_RETURN;
	/* why is exch here? */
	else if (eqs(t, "exch")) mtype = SLANG_EXCH;
	else return(0);
	*flag = 1;
	Lang_Object_Ptr->main_type = mtype; Lang_Object_Ptr->sub_type = 0;
     }
   
   
   flag_save = *flag; *flag = 0;
   lang_try_now();
   *flag = flag_save;
   
   return(1);
}

static SLang_Object_Type *lang_make_object(void)
{
   SLang_Object_Type *obj;
   
   obj = (SLang_Object_Type *) SLMALLOC(sizeof(SLang_Object_Type));
   if (NULL == obj)
     {
	SLang_Error = SL_MALLOC_ERROR; /* SLang_doerror("Lang: malloc error."); */
	return(0);
     }
   obj->main_type = 0;
   obj->sub_type = 0;
   obj->v.l_val = 0;
   return obj;
}

static int interp_variable_eqs(char *name)
{
   SLang_Name_Type *v;
   SLBlock_Type obj;
   unsigned char mtype;
   unsigned char stype;
   char ch;
   long value;
   int offset;
   int eq, pe, me, pp, mm;
   
   eq = SLANG_GEQS - SLANG_GEQS;
   pe = SLANG_GPEQS - SLANG_GEQS;
   me = SLANG_GMEQS - SLANG_GEQS;
   pp = SLANG_GPP - SLANG_GEQS;
   mm = SLANG_GMM - SLANG_GEQS;
   
   /* Name must be prefixed by one of:  =, ++, --, +=, -=
    all of which have ascii codes less than or equal to 61 ('=') */
   
   offset = -1;
   ch = *name++;
   switch (ch)
     {
      case '=': offset = eq; break;
      case '+':
	ch = *name++;
	if (ch == '+') offset = pp; else if (ch == '=') offset = pe;
	break;
      case '-':
	ch = *name++;
	if (ch == '-') offset = mm; else if (ch == '=') offset = me;
	break;
     }
   
   if (offset == -1) return 0;
   
   v = SLang_locate_name(name);
   if ((v == NULL) || *(v->name) == 0)
     {
	SLang_Error = UNDEFINED_NAME;
	SLang_doerror (name);
	return(1);
     }
   
   mtype = v->main_type;
   if (mtype == SLANG_RVARIABLE)
     {
	SLang_Error = READONLY_ERROR;
	return(1);
     }
   
   if ((mtype != SLANG_GVARIABLE) && (mtype != SLANG_LVARIABLE)
       && (mtype != SLANG_IVARIABLE))
     {
	SLang_Error = DUPLICATE_DEFINITION;
	SLang_doerror (name);
	return(1);
     }
   
   /* its value is location of object in name table unless it is local */
   value = (long) v;
   
   if (mtype == SLANG_IVARIABLE)
     {
	if (v->sub_type == STRING_TYPE)
	  {
	     SLang_Error = READONLY_ERROR;
	     return(1);
	  }
	
	stype = SLANG_IEQS;
     }
   
   else if (mtype == SLANG_GVARIABLE) stype = SLANG_GEQS;
   else
     {
	stype = SLANG_LEQS;
	value = (int) v->addr;
     }
   
   stype += offset;
   
   if (Lang_Defining_Function || Lang_Defining_Block)
     {
	Lang_Object_Ptr->main_type = SLANG_EQS;
	Lang_Object_Ptr->sub_type = stype;
	if (mtype == SLANG_LVARIABLE)
	  Lang_Object_Ptr->b.i_blk = (int) value;
	else
	  Lang_Object_Ptr->b.l_blk = value;
	Lang_Object_Ptr++;
	return (1);
     }
   
   /* create an object with the required properties for next call */
   obj.main_type = SLANG_EQS;
   obj.sub_type = stype;
   
   if (mtype == SLANG_LVARIABLE)
     obj.b.i_blk = (int) value;
   else obj.b.l_blk = value;
   
   lang_do_eqs(&obj);
   return(1);
}


#ifndef HAVE_STDLIB_H
/* Oh dear.  Where is the prototype for atof?  If not in stdlib, then
 * I do not know where.
 */
# ifdef FLOAT_TYPE
extern double atof ();
# endif
#endif

/* a literal */
static int interp_push_number(char *t)
{
   int i = 0;
   unsigned char stype;
   long value = 0;
#ifdef FLOAT_TYPE
   float64 x = 0.0;
#endif
   
   stype = SLang_guess_type(t);
   if (stype == STRING_TYPE) return(0);
   if (stype == INT_TYPE)
     {
	i = SLatoi((unsigned char *) t);
	value = (long) i;
     }
   
#ifdef FLOAT_TYPE
   else if (stype == FLOAT_TYPE)
     {
	x = atof(t);
     }
#endif
   
   if (!Lang_Defining_Block && !Lang_Defining_Function)
     {
#ifdef FLOAT_TYPE
	if (stype == INT_TYPE)
	  {
#endif
	     SLang_push_integer(i);
#ifdef FLOAT_TYPE
	  }
	else SLang_push_float(x);
#endif
	return(1);
     }
   /* a literal */
   
#ifdef FLOAT_TYPE
   if (stype == FLOAT_TYPE)
     {
	if (NULL == (Lang_Object_Ptr->b.f_blk = (float64 *) SLMALLOC(sizeof(float64))))
	  {
	     SLang_Error = SL_MALLOC_ERROR;
	     return 1;
	  }
	*Lang_Object_Ptr->b.f_blk = x;
     }
   else
#endif
     if (stype == INT_TYPE)
       Lang_Object_Ptr->b.i_blk = (int) value;
   else
     Lang_Object_Ptr->b.l_blk = value;
   
   Lang_Object_Ptr->main_type = SLANG_LITERAL;
   Lang_Object_Ptr->sub_type = stype;
   
   Lang_Object_Ptr++;
   return(1);
}

/* only supports non negative integers, use 'chs' to make negative number */

static void lang_check_space(void)
{
   unsigned int n;
   SLBlock_Type *p;
   
   if (Lang_Interp_Stack_Ptr - Lang_Interp_Stack >= 9)
     {
	SLang_doerror("Interpret stack overflow.");
	return;
     }
   
   if (Lang_Defining_Block)
     {
	n = (unsigned int) (Lang_Object_Ptr - Lang_Block_Body);
	if (n + 1 < Lang_BBody_Size) return;   /* extra for terminator */
	p = Lang_Block_Body;
     }
   else if (Lang_Defining_Function)
     {
	n = (unsigned int) (Lang_Object_Ptr - Lang_Function_Body);
	if (n + 1 < Lang_FBody_Size) return;
	p = Lang_Function_Body;
     }
   else return;
   
   /* enlarge the space by 2 objects */
   n += 2;
   if (NULL == (p = (SLBlock_Type *) SLREALLOC(p, n * sizeof(SLBlock_Type))))
     {
	SLang_Error = SL_MALLOC_ERROR;
	return;
     }
   
   if (Lang_Defining_Block)
     {
	Lang_BBody_Size = n;
	n = (unsigned int) (Lang_Object_Ptr - Lang_Block_Body);
	Lang_Block_Body = p;
	Lang_Object_Ptr = p + n;
     }
   else
     {
	Lang_FBody_Size = n;
	n = (unsigned int) (Lang_Object_Ptr - Lang_Function_Body);
	Lang_Function_Body = p;
	Lang_Object_Ptr = p + n;
     }
}

static int Lang_Defining_Variables = 0;

/* returns positive number if name is a function or negative number if it
 is a variable.  If it is intrinsic, it returns magnitude of 1, else 2 */
int SLang_is_defined(char *name)
{
   SLang_Name_Type *t;
   
   (void) compute_hash((unsigned char *) name);
   t = SLang_locate_global_name(name);
   
   if ((t == NULL) || (*t->name == 0)) return 0;
   
   switch (t->main_type)
     {
      case SLANG_FUNCTION: return(2);
      case SLANG_INTRINSIC: return(1);
      case SLANG_GVARIABLE: return (-2);
      default:
	return(-1);
     }
}




char *SLang_find_name(char *name)
{
   SLang_Name_Type *n;
   
   compute_hash((unsigned char *) name);
   
   n = SLang_locate_global_name(name);
   if ((n != NULL) && (*n->name != 0))
     {
	return(n->name);
     }
   return(NULL);
}

int SLang_add_global_variable (char *name)
{
   long value;
   /* Note the importance of checking if it is already defined or not.  For example,
    * suppose X is defined as an intrinsic variable.  Then S-Lang code like:
    * !if (is_defined("X")) { variable X; 
    }
    * will not result in a global variable X.  On the other hand, this would
    * not be an issue if 'variable' statements always were not processed
    * immediately.  That is, as it is now, 'if (0) {variable ZZZZ;}' will result
    * in the variable ZZZZ being defined because of the immediate processing.
    * The current solution is to do: if (0) { eval("variable ZZZZ;"); 
    }
    */
   if (!SLang_is_defined(name))
     {
	if (0 == (value = (long) lang_make_object())) return 0;
	SLadd_name(name, value, SLANG_GVARIABLE, 0);
     }
   return 1;
}

void SLadd_variable(char *name)
{
   SLang_Name_Type *table;
   
   if (!lang_check_name(name)) return;
   
   if (Lang_Defining_Function)	       /* local variable */
     {
	compute_hash((unsigned char *) name);
	table = Lang_Local_Variable_Table;
	if (!Local_Variable_Number)
	  {
	     table = (SLang_Name_Type *) SLCALLOC(MAX_LOCAL_VARIABLES, sizeof(SLang_Name_Type));
	     if (NULL == table)
	       {
		  SLang_Error = SL_MALLOC_ERROR;
		  return;
	       }
	     Lang_Local_Variable_Table = table;
	  }
	strcpy(table[Local_Variable_Number].name + 1, name);
	*table[Local_Variable_Number].name = (char) Hash;
	table[Local_Variable_Number].main_type = SLANG_LVARIABLE;
	table[Local_Variable_Number].addr = (long) Local_Variable_Number;
        Local_Variable_Number++;
     }
   else SLang_add_global_variable (name);
}

static void interp_push_string(char *t)
{
   int len;
   
   /* strings come in with the quotes attached-- knock em off */
   if (*t == '"')
     {
	len = strlen(t) - 1;
	if (*(t + len) == '"') *(t + len) = 0;
	t++;
     }
   
   if (!Lang_Defining_Block && !Lang_Defining_Function)
     {
	SLang_push_string(t);
	return;
     }
   
   if (NULL == (Lang_Object_Ptr->b.s_blk = SLmake_string(t))) return;
   
   Lang_Object_Ptr->main_type = SLANG_DATA;
   Lang_Object_Ptr->sub_type = STRING_TYPE;
   Lang_Object_Ptr++;
}

/* if an error occurs, discard current object, block, function, etc... */
void SLang_restart(int localv)
{
   int save = SLang_Error;
   SLang_Error = UNKNOWN_ERROR;
   
   SLcompile_ptr = SLcompile;
   Lang_Break = Lang_Continue = Lang_Return = 0;
   while(Lang_Defining_Block)
     {
	lang_end_block();
     }
   
   /* I need to free blocks on the interp stack even when not defining a
    * function.  This is not done here--- future work.
    */
   if (Lang_Defining_Function)
     {
	if (Lang_Function_Body != NULL)
	  {
	     lang_define_function(NULL);
	     if (lang_free_branch(Lang_Function_Body)) SLFREE(Lang_Function_Body);
	  }
	if (Local_Variable_Number)
	  {
	     SLFREE(Lang_Local_Variable_Table);
	     Local_Variable_Number = 0;
	     Lang_Local_Variable_Table = NULL;
	  }
	Lang_Defining_Function = 0;
     }
   
   SLang_Error = save;
   
   if (SLang_Error == STACK_OVERFLOW)
     {
	/* This loop guarantees that the stack is properly cleaned. */
	while (SLStack_Pointer != SLRun_Stack)
	  {
	     SLdo_pop ();
	  }
     }
   
   
   Lang_Interp_Stack = Lang_Object_Ptr = Lang_Interp_Stack_Ptr = Lang_Interp_Stack_Static;
   /* This should be handled automatically */
   
   if (localv) Local_Variable_Frame = Local_Variable_Stack;
   Lang_Defining_Variables = 0;
}

#ifdef SL_BYTE_COMPILING

static int try_byte_compiled(register unsigned char *s)
{
   SLName_Table *nt;
   SLang_Name_Type *entry;
   register int ofs;
   int n;
   
   if ((*s++ != '#')
       || ((n = (int) (*s++ - '0')) < 0))
     {
	SLang_doerror ("Illegal name.");
	return 1;
     }
   if (n == 0)
     {
	try_directive ((char *) s, &n);	       /* note that n is a dummy now */
	return 1;
     }
   if (n == 1)
     {
	lang_try_binary((char *) s);
	return 1;
     }
   if (n == 2)
     {
	/* global or local, try it. */
	if (Lang_Defining_Function == -1) return 0;
	return lang_exec ((char *) s, 0);
     }
   
   n -= 3;
   /* 3 digit base 26 number */
   ofs = (*s++ - 'A');
   ofs = 26 * ofs + (*s++ - 'A');
   ofs = 26 * ofs + (*s++ - 'A');
   
   nt = SLName_Table_Root;
   while (n--)
     {
	nt = nt->next;	       /* find the correct table */
	if (nt == NULL)
	  {
	     SLang_doerror("Illegal name.");
	     return 1;
	  }
     }
   
   entry = &(nt->table[ofs]);
   
   /* table = Lang_Local_Variable_Table; */
   Lang_Object_Ptr->main_type = entry->main_type;
   Lang_Object_Ptr->sub_type = entry->sub_type;
   Lang_Object_Ptr->b.n_blk = entry;
   lang_try_now();
   return 1;
}
#endif

int SLPreprocess_Only = 0;

char *SLbyte_compile_name(char *name)
{
   static char code[36];
   SLang_Name_Type *t;
   SLName_Table *nt;
   int ofs, n;
   
   if (SLPreprocess_Only || (*name == 0)) return name;
   
   if (_SLeqs_name(name, Lang_Directives))
     {
	*code = '@'; code[1] = '#';  code[2] = '0';
	strcpy (code + 3, name);
	return code;
     }
   if (_SLeqs_name(name, SL_Binary_Ops))
     {
	*code = '@'; code[1] = '#';  code[2] = '1';
	strcpy (code + 3, name);
	return code;
     }
   
   (void) compute_hash((unsigned char *) name);
   
   /* see if it is in local table */
   t = Lang_Local_Variable_Table;
   if (t != NULL)
     {
	t = SLang_locate_name_in_table(name, t, t, Local_Variable_Number);
     }
   
   if ((t == NULL) || (t->name == 0))
     {
	/* It must be global.  Check intrinsics first */
	nt = SLName_Table_Root;
	n = 3;
	while (nt != NULL)
	  {
	     t = nt->table;
	     
	     if ((ofs = nt->ofs[Hash]) != -1)
	       {
		  t = SLang_locate_name_in_table(name, t, t + ofs, nt->n);
		  if ((t != NULL) && (*t->name != 0))
		    {
		       ofs = (int) (t - nt->table);
		       
		       *code = '@'; *(code + 1) = '#';
		       *(code + 2) = n + '0';
		       *(code + 5) = (ofs % 26) + 'A';
		       ofs = ofs / 26;
		       *(code + 4) = (ofs % 26) + 'A';
		       ofs = ofs / 26;
		       *(code + 3) = (ofs % 26) + 'A';
		       *(code + 6) = 0;
		       return code;
		    }
	       }
	     
	     nt = nt->next;
	     n++;
	  }
	
	/* Now try global */
	t = SLang_locate_slang_name (name);
	if ((t == NULL) || (*t->name == 0)) return name;
     }
   
   *code = '@';
   code [1] = '#';
   code [2] = '2';
   strcpy (code + 3, name);
   return code;
}


void SLcompile(char *t)
{
   static int flag = 0;
   int d = 0;
   char ch = *t;
   
   if (ch == 0) return;
   lang_check_space();                 /* make sure there is space for this */
   
   
   if (!SLang_Error
#ifdef SL_BYTE_COMPILING
       && (ch != '@')
#endif
       && (ch != '"'))
     {
	if (ch == '{')
	  {
	     lang_begin_block();
	     d = 1;
	  }
	else
	  {
	     /* The purpose of this convoluted mess is to flag errors
	      * such as  '{block} statement'  where 'statement' is not
	      * somthing like 'if', '!if', 'while', ...  That is, something
	      which is not supposed to follow a block. */
	     d = try_directive(t, &flag);
	     if ((!flag && d) || (flag && !d)) SLang_Error = SYNTAX_ERROR;
	  }
	flag = 0;
     }
   
#ifdef SL_BYTE_COMPILING
   if (ch == '@')
     {
	flag = 0; d = 0;
	if (0 == try_byte_compiled((unsigned char *) (t + 1)))
	  {
	     /* failure ONLY for slang functions/variables. */
	     t += 3;
	     ch = *t;
	  }
     }
#endif
   
   if ((ch == '@') || SLang_Error || d);  /* null... */
   else if (Lang_Defining_Variables)
     {
	if (ch == ']') Lang_Defining_Variables = 0;
	else SLadd_variable(t);
     }
   else if (Lang_Defining_Function == -1) lang_define_function(t);
#ifdef SLANG_NOOP
   else if ((ch == '_') && (t[1] == '_') && !strcmp (t, "__noop__"))
     {
	Lang_Object_Ptr->main_type = SLANG_NOOP_DIRECTIVE;
	Lang_Object_Ptr->sub_type = 0;
	Lang_Object_Ptr->b.blk = NULL;         /* not used */
	lang_try_now();
     }
#endif
   else if (ch == '"') interp_push_string(t);
   else if ((ch == ':') && (Lang_Defining_Block))
     {
	Lang_Object_Ptr->main_type = SLANG_LABEL;
	Lang_Object_Ptr->b.blk = NULL;
	Lang_Object_Ptr++;
     }
   
   else if ((ch == ')') && (Lang_Defining_Function == 1))
     {
	if (Lang_Defining_Block) SLang_doerror("Function nesting illegal.");
	else Lang_Defining_Function = -1;
     }
   
   else if (ch == '{')
     {
	lang_begin_block();
	flag = 0;
     }
   
   else if ((ch == '}') && Lang_Defining_Block)
     {
	lang_end_block();
	flag = 1;
     }
   
   else if (ch == '(')	lang_begin_function();
   
   else if (ch == '[') Lang_Defining_Variables = 1;
   else if (lang_try_binary(t));
   else if (lang_try_unary(t));
   
   /* note that order here is important */
   else if ((ch <= '9') && interp_push_number(t));
   else if ((ch <= '=') && interp_variable_eqs(t));
   else if (lang_exec(t, 1));
   else
     {
	SLang_Error = UNDEFINED_NAME;
	SLang_doerror (t);
     }
   
   
   if (SLang_Error)
     {
	SLang_restart(0);
	flag = 0;
     }
}






int SLstack_depth()
{
#ifdef SLANG_NOOP
   int n = 0;
   SLang_Object_Type *p = SLStack_Pointer;
   
   while (p > SLRun_Stack)
     {
	p--;
	if (p->main_type != SLANG_NOOP) n++;
     }
   return n;
#else
   return (int) (SLStack_Pointer - SLRun_Stack);
#endif
}






/* #define STRCHR(x, y) ((y >= 'a') && (y <= 'z') ? NULL : ((y) == 32) || strchr(x, y)) */


/* The minus sign ones can be overloaded. */
SLang_Name2_Type SL_Binary_Ops[] =
{
     {"+", -SLANG_PLUS},
     {"-", -SLANG_MINUS},
     {"*", -SLANG_TIMES},
     {"/", -SLANG_DIVIDE},
     {"<", -SLANG_LT},
     {"<=", -SLANG_LE},
     {"==", -SLANG_EQ},
     {">", -SLANG_GT},
     {">=", -SLANG_GE},
     {"!=", -SLANG_NE},
     {"and", SLANG_AND},
     {"or", SLANG_OR},
     {"mod", SLANG_MOD},
     {"&", SLANG_BAND},
     {"shl", SLANG_SHL},
     {"shr", SLANG_SHR},
     {"xor", SLANG_BXOR},
     {"|", SLANG_BOR},
     {(char *) NULL, (int) NULL}
};

static char Really_Stupid_Hash[256];

void SLstupid_hash()
{
   register unsigned char *p;
   register SLang_Name2_Type *d;
   
   d = SL_Binary_Ops;
   while ((p = (unsigned char *) (d->name)) != NULL)
     {
	Really_Stupid_Hash[*(p + 1)] = 1;
	d++;
     }
   d = Lang_Directives;
   while ((p = (unsigned char *) (d->name)) != NULL)
     {
	Really_Stupid_Hash[*(p + 1)] = 1;
	d++;
     }
}




int _SLeqs_name(char *t, SLang_Name2_Type *d_parm)
{
   register char *p;
   register char ch;
   register SLang_Name2_Type *d;
   
   ch = *t++;
   if (Really_Stupid_Hash[(unsigned char) *t] == 0) return(0);
   d = d_parm;
   while ((p = d->name) != NULL)
     {
	if ((ch == *p) && !strcmp(t, p + 1)) return(d->type);
	d++;
     }
   return(0);
}



void (*SLcompile_ptr)(char *) = SLcompile;

int SLang_add_table(SLang_Name_Type *table, char *table_name)
{
   register int i;
   SLang_Name_Type *t;
   SLName_Table *nt;
   int *ofs;
   unsigned char h;
   char *name;
   static int init = 0;
   
   if (init == 0)
     {
#ifdef MSWINDOWS	
	SLRun_Stack = (SLang_Object_Type *) SLCALLOC (SLANG_MAX_STACK_LEN,
						      sizeof (SLang_Object_Type));
	if (SLRun_Stack == NULL)
	  {
	     return 0;
	  }
	SLStack_Pointer = SLRun_Stack;
	SLStack_Pointer_Max = SLRun_Stack + SLANG_MAX_STACK_LEN;
#endif
	
	init = 1;
	
	for (i = 1; i < 256; i++) SLang_Name_Table_Ofs[i] = -1;
	SLang_Name_Table_Ofs[0] = 0;
	
	SLShort_Blocks[0].main_type = SLANG_RETURN;
	SLShort_Blocks[1].main_type = SLANG_BREAK;
	SLShort_Blocks[2].main_type = SLANG_CONTINUE;
     }
   
   if ((nt = (SLName_Table *) SLMALLOC(sizeof(SLName_Table))) == NULL) return(0);
   nt->table = table;
   nt->next = SLName_Table_Root;
   strcpy(nt->name, table_name);
   SLName_Table_Root = nt;
   ofs = nt->ofs;
   for (i = 0; i < 256; i++) ofs[i] = -1;
   
   /* compute hash for table */
   
   t = table;
   while (((name = t->name) != NULL) && (*name != 0))
     {
	h = compute_hash((unsigned char *) (name + 1));
	*name = (char) h;
	if (ofs[h] == -1)
	  {
	     ofs[h] = (int) (t - table);
	  }
	t++;
     }
   nt->n = (int) (t - table);
   return(1);
}

extern char *SLang_extract_list_element(char *, int *, int *);

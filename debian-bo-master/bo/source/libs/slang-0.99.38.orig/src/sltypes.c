/* Basic type operations for S-Lang */
/* Copyright (c) 1992, 1996 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */



#include "config.h"

#include <stdio.h>

#include "slang.h"
#include "_slang.h"
#include "slarray.h"

#ifdef FLOAT_TYPE
#include <math.h>
#endif


#ifdef FLOAT_TYPE
/* doing it this way means that I can handle things like 2.0 + 3 and
 * 3 + 2.0 at the same time.
 */
static int float_bin_op (int op, unsigned char sta, unsigned char stb, 
			 float64 *ap, float64 *bp)
{
   float64 c, a, b;
   int ic;
   
   if (sta == FLOAT_TYPE) a = *ap;
   else a = (float64) *(int *) ap;
   
   if (stb == FLOAT_TYPE) b = *bp;
   else b = (float64) *(int *) bp;

   switch (op)
     {
      case SLANG_PLUS:
	c = a + b; goto push_float;
      case SLANG_MINUS: c = a - b; goto push_float;
      case SLANG_TIMES: c = a * b; goto push_float;
      case SLANG_DIVIDE:
	if (b == 0.0) 
	  {
	     SLang_Error = DIVIDE_ERROR;
	     return 1;
	  }
	c = a / b; goto push_float;
	
      case SLANG_EQ: ic = (a == b); goto push_int;
      case SLANG_NE: ic = (a != b); goto push_int;
      case SLANG_GT: ic = (a > b); goto push_int;
      case SLANG_GE: ic = (a >= b); goto push_int;
      case SLANG_LT: ic = (a < b); goto push_int;
      case SLANG_LE: ic = (a <= b); goto push_int;
      default:
	return 1;
     }
   
   push_float:
   SLang_push_float (c);
   return 1;
   
   push_int:
   SLang_push_integer (ic);
   return 1;
}	

#endif

static int int_bin_op (int op, unsigned char sta, unsigned char stb, 
		       int *ap, int *bp)
{
   int c, a = *ap, b = *bp;
   (void) sta;
   (void) stb;
   switch (op)
     {
      case SLANG_PLUS:
	c = a + b; break;
      case SLANG_MINUS: c = a - b; break;
      case SLANG_TIMES: c = a * b; break;
      case SLANG_DIVIDE:
	if (b == 0) 
	  {
	     SLang_Error = DIVIDE_ERROR;
	     return 1;
	  }
	c = a / b; break;

      case SLANG_EQ: c = (a == b); break;
      case SLANG_NE: c = (a != b); break;
      case SLANG_GT: c = (a > b); break;
      case SLANG_GE: c = (a >= b); break;
      case SLANG_LT: c = (a < b); break;
      case SLANG_LE: c = (a <= b); 
	break;
      default:
	return 0;
     }
   SLang_push_integer (c);
   return 1;
}	

static int unary_op_function (int op, unsigned char type, VOID_STAR xp)
{
   int x;
#ifdef FLOAT_TYPE 
   float64 xf;
   
   if (type == FLOAT_TYPE) 
     {
	xf = *(float64 *) xp;
	switch (op)
	  {
	   case SLANG_CHS: xf = -xf; break;
	   case SLANG_SQR: xf = xf * xf; break;
	   case SLANG_MUL2: xf = xf * 2.0; break;
	   case SLANG_ABS: xf = (float64) fabs((double) xf); break;
	   case SLANG_SIGN: 
	     if (xf > 0.0) x = 1;
	     else if (xf < 0.0) x = -1;
	     else x = 0;
	     SLang_push_integer (x);
	     return 1;
	     
	   default:
	     return 0;
	  }
	SLang_push_float (xf);
     }
   else
     {
#else
	(void) type;
#endif
	x = *(int *) xp;
	switch (op)
	  {
	   case SLANG_CHS: x = -x; break;
	   case SLANG_SQR: x = x * x; break;
	   case SLANG_MUL2: x = x * 2; break;
	   case SLANG_ABS: x = abs(x); break;
	   case SLANG_SIGN: 
	     if (x > 0) x = 1; else if (x < 0) x = -1;
	     break;
	   default:
	     return 0;
	  }
	SLang_push_integer (x);
#ifdef FLOAT_TYPE
     }
#endif
   return 1;
}

int SLregister_types (void)
{
   if ((!SLang_register_class (ARRAY_TYPE, (FVOID_STAR) SLarray_free_array, NULL))
       || !SLang_register_class (INT_TYPE, NULL, NULL)
#ifdef FLOAT_TYPE
       || !SLang_register_class (FLOAT_TYPE, NULL, NULL)
#endif
       )
     return -1;
   

   if (!SLang_add_binary_op (INT_TYPE, INT_TYPE, (FVOID_STAR) int_bin_op)
       || !SLang_add_unary_op (INT_TYPE, (FVOID_STAR) unary_op_function)
#ifdef FLOAT_TYPE
       || !SLang_add_binary_op (FLOAT_TYPE, FLOAT_TYPE, (FVOID_STAR) float_bin_op)
       || !SLang_add_binary_op (FLOAT_TYPE, INT_TYPE, (FVOID_STAR) float_bin_op)
       || !SLang_add_unary_op (FLOAT_TYPE, (FVOID_STAR) unary_op_function)
#endif
       )
     return -1;
   
   return 0;
}

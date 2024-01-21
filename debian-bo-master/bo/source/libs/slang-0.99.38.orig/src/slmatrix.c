/* Floating point matrix manipulation routines for S-Lang */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */

#include "config.h"

#include <stdio.h>
#ifndef FLOAT_TYPE
#define FLOAT_TYPE
#endif
#include "slang.h"
#include "_slang.h"
#include "slarray.h"

static SLuser_Object_Type *SLpop_float_matrix (void)
{
   SLuser_Object_Type *a;
   
   int sa = 0;  /* No strings */
   if (NULL == (a = SLang_pop_array (&sa))) return NULL;
   
   if (((SLArray_Type *) a->obj) -> type == FLOAT_TYPE) return a;
   SLang_Error = TYPE_MISMATCH;
   SLang_free_user_object (a);
   return NULL;
}

   
/* multiply 2 matrices (assumed float) producing a third */
static void SLmatrix_multiply (void)
{
   SLArray_Type *a, *b, *c;
   SLuser_Object_Type *uc = NULL, *ua = NULL, *ub = NULL;
   float64 *aa, *bb, *cc, sum;
   int dim;
   unsigned int imax, jmax, kmax;
   unsigned int i, j, k, ofs_a, ofs_b;
   
   if ((NULL == (ub = SLpop_float_matrix ()))
       || (NULL == (ua = SLpop_float_matrix ())))
     {
	goto free_and_return;
     }
   
   a = (SLArray_Type *) ua->obj;
   b = (SLArray_Type *) ub->obj;
     
   /* Now is a is n*m, then b must be m*x.  Result it n*x */
   
   imax = a->x;
   jmax = a->y;
   
   if ((b->x != jmax) || (a->dim > 2) || (b->dim > 2))
     {
	SLang_Error = TYPE_MISMATCH;
	goto free_and_return;
     }
   
   kmax = b->y;
   
   /* Now result will be  imax by kmax 2d array */
   if (kmax == 1) dim = 1; else dim = 2;
   
   if (NULL == (uc = SLcreate_array(NULL, dim, imax, kmax, 1, 'f', 0)))
     {
	SLang_doerror("Unable to create array.");
	goto free_and_return;
     }
   c = (SLArray_Type *) uc->obj;
   
   /* Finally!! */
   cc = c->buf.f_ptr;
   bb = b->buf.f_ptr;
   aa = a->buf.f_ptr;
   
   for (i = 0; i < imax; i++)
     {
	for (k = 0; k < kmax; k++)
	  {
	     sum = 0.0;
	     ofs_a = i * jmax;
	     ofs_b = k;
	     for (j = 0; j < jmax; j++)
	       {
		  sum += *(aa + ofs_a) * *(bb + ofs_b);
		  ofs_a++;
		  ofs_b += kmax;
	       }
	     
	     /* cc[i][k] */
	     *(cc + (int) (i * kmax + k)) = sum;
	  }
     }
   
   SLang_push_user_object (uc);
   
   free_and_return:
   if (ua != NULL) SLang_free_user_object (ua);
   if (ub != NULL) SLang_free_user_object (ub);
}

static void SLmatrix_addition (void)
{
   SLArray_Type *a, *b, *c;
   SLuser_Object_Type *ua = NULL, *ub, *uc;
   float64 *aa, *bb, *cc;
   unsigned int imax, jmax, kmax, jmaxkmax;
   unsigned int i, j, k, ofs;
   
   
   if ((NULL == (ub = SLpop_float_matrix ()))
       || (NULL == (ua = SLpop_float_matrix ())))
     {
	goto free_and_return;
     }
   
   a = (SLArray_Type *) ua->obj;
   b = (SLArray_Type *) ub->obj;
     
   /* for the addition to make sence, they must be same type. */
   imax = a->x; jmax = a->y; kmax = a->z;
   
   if ((b->dim != a->dim) || (b->x != imax) 
       || (b->y != jmax) || (b->z != kmax))
     {
	SLang_Error = TYPE_MISMATCH;
	goto free_and_return;
     }
   
   if (NULL == (uc = SLcreate_array(NULL, a->dim, imax, jmax, 1, 'f', 0)))
     {
	SLang_doerror("Unable to create array.");
	goto free_and_return;
     }
   c = (SLArray_Type *) uc->obj;
   
   /* Finally!! */
   cc = c->buf.f_ptr;
   bb = b->buf.f_ptr;
   aa = a->buf.f_ptr;
   
   
   /* Probably more efficent if we work in this order */
   jmaxkmax = jmax * kmax;
   for (k = 0; k < kmax; k++)
     {
	for (j = 0; j < jmax; j++)
	  {
	     ofs = j * kmax + k;
	     for (i = 0; i < imax; i++)
	       {
		  *(cc + ofs) = *(aa + ofs) + *(bb + ofs);
		  ofs += jmaxkmax;
	       }
	  }
     }
   
   SLang_push_user_object (uc);
   
   free_and_return:
   if (ua != NULL) SLang_free_user_object (ua);
   if (ub != NULL) SLang_free_user_object (ub);
}

static SLang_Name_Type slmatrix_table[] =
{
   MAKE_INTRINSIC(".matrix_multiply", SLmatrix_multiply, VOID_TYPE, 0),
   MAKE_INTRINSIC(".matrix_add", SLmatrix_addition, VOID_TYPE, 0),
   SLANG_END_TABLE
};

int init_SLmatrix()
{
   return SLang_add_table(slmatrix_table, "_Matrix");
}



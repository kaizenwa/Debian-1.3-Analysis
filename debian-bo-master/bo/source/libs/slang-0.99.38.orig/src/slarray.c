/* Array manipulation routines for S-Lang */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */



#include "config.h"

#include <stdio.h>
#include <string.h>

#include "slang.h"
#include "_slang.h"
#include "slarray.h"

/* This is the callback routine to destroy the array. */
void SLarray_free_array (long *obj)
{
   unsigned char **s_ptr, **s_ptr_max;
   SLuser_Object_Type **u_ptr, **u_ptr_max;
   unsigned char type;
   unsigned int n;
   SLArray_Type *at;
   
   at = (SLArray_Type *) obj;   
   type = at->type;

   if (at->buf.s_ptr != NULL) switch (type)
     {
#ifdef FLOAT_TYPE
      case FLOAT_TYPE:
#endif
      case INT_TYPE:
      case CHAR_TYPE:
	/* For the above, we do not have to worry about free any elements
	 * since the elements are not pointers.
	 */
	break;
	
	/* The ones below here are pointers and need freed */
      case STRING_TYPE:
	/* Strings were ALL malloced. */
	
      default:
	n = (unsigned int) at->x;
	n = n * at->y * at->z;
	
	if ((type == ARRAY_TYPE) || (type >= 100))
	  {
	     u_ptr = at->buf.u_ptr;
	     u_ptr_max = u_ptr + n;
	     
	     while (u_ptr < u_ptr_max)
	       {
		  if (*u_ptr != NULL) SLang_free_user_object (*u_ptr);
		  u_ptr++;
	       }
	  }
	else if (type == STRING_TYPE)
	  {
	     s_ptr = at->buf.s_ptr;
	     s_ptr_max = s_ptr + n;
	     while (s_ptr < s_ptr_max)
	       {
		  if (*s_ptr != NULL) SLFREE (*s_ptr);
		  s_ptr++;
	       }
	  }
     }
   
   s_ptr = at->buf.s_ptr;
   if ((s_ptr != NULL) && (at->flags == 0)) SLFREE (s_ptr);
   SLFREE (obj);
}

	
/* if ptr == NULL then malloc space.  Otherwise assume space is at ptr */
SLuser_Object_Type *SLcreate_array(long *ptr, int dim, int d0, int d1, int d2,
				   unsigned char t, unsigned char flags)
{
   unsigned long n, size;
   unsigned char type;
   SLArray_Type *at;
   SLuser_Object_Type *uat;
   
   switch (t)
     {
      case 'i': type = INT_TYPE; size = sizeof(int); break;
      case 's': type = STRING_TYPE; size = sizeof(char *); break;
      case 'c': type = CHAR_TYPE; size = sizeof(char); break;
#ifdef FLOAT_TYPE
      case 'f': type = FLOAT_TYPE; size = sizeof(float64); break;
#endif
      default:
	/* I need to have applications register their types! */
	if (t >= 100)
	  {
	     type = t;
	     size = sizeof (SLuser_Object_Type *);
	  }
	else return NULL;
     }
   
   /* This must be since indices go from 0 to d - 1 */
   if (d1 < 1) d1 = 1;
   if (d2 < 1) d2 = 1;
   
   n = d0;
   n = n * d1;
   n = n * d2;
   
   if (NULL == (at = (SLArray_Type *) SLMALLOC (sizeof(SLArray_Type))))
     {
	return NULL;
     }
   
   if (NULL == (uat = SLang_create_user_object (ARRAY_TYPE)))
     {
	SLFREE (at);
	return NULL;
     }
   

   if (ptr == NULL)
     {
	if (size == 1) ptr = (long *) SLMALLOC(n); else ptr = (long *) SLCALLOC(n, size);
	if (ptr == NULL) return NULL;
     }
   else 
     {
	if (flags == 0) flags = SLANG_IVARIABLE;
     }
   
   
   at->buf.s_ptr = (unsigned char **) ptr;
   at->dim = dim;
   at->x = d0; at->y = d1; at->z = d2;
   at->type = type;
   at->flags = flags;
   
   uat->obj = (long *) at;
   return uat;
}



void SLang_create_array(void)
{
   int dim, d0, d1, d2, t;
   SLuser_Object_Type *uat;

   if (SLang_pop_integer(&dim)) return;

   if ((dim > 3) || (dim <= 0))
     {
	SLang_Error = SL_INVALID_PARM;
	SLang_doerror("Array size not supported.");
	return;
     }

   d1 = d0 = d2 = 1;
   switch (dim)
     {
      case 3: SLang_pop_integer(&d2);
      case 2: SLang_pop_integer(&d1);
      case 1: SLang_pop_integer(&d0);
     }
   
   if ((d0 <= 0)
       || (d1 <= 0)
       || (d2 <= 0))
     {
	SLang_Error = SL_INVALID_PARM;
	return;
     }
   
   if (SLang_pop_integer(&t)) return;

   uat = SLcreate_array(NULL, dim, d0, d1, d2, t, 0);
   
   if (uat == NULL)
     {
	SLang_doerror("Unable to create array.");
	return;
     }
   
   SLang_push_user_object (uat);
}



/* returns USER OBJECT array.  If *stype is non-zero, a string is accepted 
 * as an array.
 * If an actual array is popped, *stype will be zero upon return.  However,
 * if *stype is such that a string is permitted, *stype will be 1 if the
 * string that is returned (through the cast) should be freed.  In any
 * case, it will be non-zero for a string. 
 */
SLuser_Object_Type *SLang_pop_array(int *sflag)
{
   SLang_Object_Type obj;
   unsigned char stype;
   
   if (SLang_pop(&obj)) return(NULL);

   stype = obj.sub_type;
   if (stype != ARRAY_TYPE)
     {
	if ((*sflag == 0) || (stype != STRING_TYPE))
	  {
	     if (stype >= ARRAY_TYPE) 
	       SLang_free_user_object (obj.v.uobj);
	     
	     SLang_Error = TYPE_MISMATCH;
	     return(NULL);
	  }
	if (obj.main_type == SLANG_DATA) *sflag = 1; else *sflag = -1;
	return (SLuser_Object_Type *) obj.v.s_val;
     }
   
   *sflag = 0;
   
   return obj.v.uobj;
}

static char *Bound_err = "Array dims out of bounds";
static unsigned int compute_array_offset(SLArray_Type *at)
{
   int elem[3], el, x[3], d, dim;
   unsigned int off;

   if (SLang_Error) return (0);
   dim = at->dim;
   x[0] = at->x; x[1] = at->y; x[2] = at->z;
   elem[0] = elem[1] = elem[2] = 0;
   d = dim;

   while (d--)
     {
	if (SLang_pop_integer(&el)) return(0);
	if ((el >= x[d]) || (el < 0))
	  {
	     SLang_doerror(Bound_err);
	     return(0);
	  }
	elem[d] = el;
     }

   off = 0;
   d = 0;

   off = (elem[0] * x[1] + elem[1]) * x[2] + elem[2];
   
   return(off);
}


static void str_get_elem(unsigned char *s, int dat)
{
   int n, nmax, ch;
   if (SLang_pop_integer(&n)) goto done;
   nmax = strlen((char *) s);
   if ((nmax < n) || (n < 0))
     {
	SLang_Error = SL_INVALID_PARM;
	SLang_doerror(Bound_err);
	goto done;
     }
   ch = s[n];
   SLang_push_integer(ch);

   done:
   if (dat == 1) SLFREE(s);
}

   

static void array_putelem (int roll)
{
   SLArray_Type *at;
   SLuser_Object_Type *uat, **uop, *uo;
   unsigned int off;
   int sdat, i;
   unsigned char *str, *newstr, **sp;
   
#ifdef FLOAT_TYPE
   float64 f;
   int ix;
   int convert;
#endif

   sdat = 0;			       /* string NOT accepted. */
   if (NULL == (uat = SLang_pop_array(&sdat))) return;

   at = (SLArray_Type *) uat->obj;
   
   if (at->flags == SLANG_RVARIABLE)
     {
	SLang_Error = READONLY_ERROR;
	goto cleanup_and_return;
     }

   if (roll)
     {
	int roll_amount = at->dim + 1;
	SLroll_stack (&roll_amount);
     }
   

   off = compute_array_offset(at);
   
   if (SLang_Error) goto cleanup_and_return;

   switch(at->type)
     {
      case INT_TYPE:
	if (SLang_pop_integer(&i)) goto cleanup_and_return;
	*(off + at->buf.i_ptr) = i;
	break;

      case STRING_TYPE:
	if (SLpop_string((char **) &str)) 
	  goto cleanup_and_return;
	
	newstr = (unsigned char *) SLmake_string ((char *) str);
	SLFREE(str);
	
	sp = (off + at->buf.s_ptr);
      
	if (NULL != *sp) SLFREE(*sp);
	*sp = newstr;
	break;
	
      case CHAR_TYPE: 
	if (SLang_pop_integer(&i)) goto cleanup_and_return;
	*(off + at->buf.c_ptr) = (unsigned char) i;
	break;

#ifdef FLOAT_TYPE
      case FLOAT_TYPE: 
	if (SLang_pop_float(&f, &convert, &ix)) goto cleanup_and_return;
	(void) convert;  (void) ix;
	*(off + at->buf.f_ptr) = f;
	break;
#endif
      default:
	if (at->type >= ARRAY_TYPE)
	  {
	     if (NULL == (uo = SLang_pop_user_object (at->type)))
	       goto cleanup_and_return;
	     
	     uop = at->buf.u_ptr + off;
	     if (*uop != NULL) SLang_free_user_object (*uop);
	     *uop = uo;
	     break;
	  }
	SLang_doerror("Corrupted Array.");
     }
   
   /* Everthing else has to flow through here too */
   
   cleanup_and_return:
   SLang_free_user_object (uat);
}

void SLarray_putelem (void)
{
   array_putelem (0);
}

void SLarray_putelem_r (void)
{
   array_putelem (1);
}

static void array_push_element(SLArray_Type *at, unsigned int off)
{
   char *err_str = "Array Element uninitialized.";
   unsigned char *s;
   
   switch(at->type)
     {
      case INT_TYPE:  SLang_push_integer (*(at->buf.i_ptr + off)); break;
      case CHAR_TYPE: SLang_push_integer ((int) *(at->buf.c_ptr + off)); break;
      case STRING_TYPE: 
	if (NULL == (s = *(at->buf.s_ptr + off)))
	  {
	     SLang_doerror (err_str);
	  }
	else SLang_push_string((char *) s);
	break;
#ifdef FLOAT_TYPE
	case FLOAT_TYPE: SLang_push_float(*(at->buf.f_ptr + off)); break;
#endif
      default:
	if (at->type >= ARRAY_TYPE) 
	  {
	     SLuser_Object_Type **uop;
	     uop = at->buf.u_ptr + off;
	     if (*uop == NULL)
	       SLang_doerror(err_str);
	     else SLang_push_user_object (*uop);
	  }
	else SLang_doerror("Internal Error in array element.");
     }
}

void SLarray_getelem (void)
{
   SLArray_Type *at;
   SLuser_Object_Type *uob;
   unsigned int off;

   int sdat = 1;		       /* allow STRINGS */
   if (NULL == (uob = SLang_pop_array(&sdat))) return;
   if (sdat) 
     {
	str_get_elem((unsigned char *) uob, sdat);
	return;
     }
   
   at = (SLArray_Type *) uob->obj;
   off = compute_array_offset(at);
   if (SLang_Error == 0) array_push_element(at, off);
   SLang_free_user_object (uob);
}

void SLcopy_array (void)
{
   SLArray_Type *a, *b;
   SLuser_Object_Type *ua, *ub;
   int sa = 0, sb = 0;
   unsigned int size;
   
   /* STRINGS not accepted. */
   if ((NULL == (ub = SLang_pop_array (&sb)))
       || (NULL == (ua = SLang_pop_array (&sa))))
     {
	if (ub != NULL) SLang_free_user_object (ub);
	return;
     }
   
   a = (SLArray_Type *) ua->obj;
   b = (SLArray_Type *) ub->obj;
   
   /* array a must be writable and a and b must be identical */
   if (a->flags == SLANG_RVARIABLE)
     {
     	SLang_Error = READONLY_ERROR;
	goto free_and_return;
     }
   if ((a->dim != b->dim)
       || (a->x != b->x)
       || (a->y != b->y)
       || (a->z != b->z)
       || (a->type != b->type))
     {
	SLang_Error = TYPE_MISMATCH;
	goto free_and_return;
     }
   
   switch (a->type)
     {
      case INT_TYPE: size = sizeof(int); break;
      case CHAR_TYPE: size = sizeof(char); break;
#ifdef FLOAT_TYPE
      case FLOAT_TYPE: size = sizeof(float64); break;
#endif
      case STRING_TYPE: 
	/* size = sizeof(char *); break; */
      default:
	SLang_doerror ("Copy not supported for this array type.");
	goto free_and_return;
     }
   /* I might want to loosen the restriction about the actual dimensions and
    * simply demand that the size be the same */
   size = size * a->x * a->y * a->z;
   SLMEMCPY ((char *)a->buf.s_ptr, (char *)b->buf.s_ptr, size);
   
   free_and_return:
   SLang_free_user_object (ua);
   SLang_free_user_object (ub);
}
   
   
void SLarray_info (void)
{   
   SLuser_Object_Type *u;
   SLArray_Type *at;
   int sa = 0;			       /* string NOT allowed */
   int dim;
   
   if (NULL == (u = SLang_pop_array (&sa))) return;
   
   at = (SLArray_Type *) u->obj;
   SLang_push_integer (at->type);
   dim = at->dim;
   SLang_push_integer (at->x);
   if (dim >= 2) SLang_push_integer (at->y);
   if (dim >= 3) SLang_push_integer (at->z);
   SLang_push_integer (dim);
   
   SLang_free_user_object (u);
}

   
   
   
   
SLuser_Object_Type *SLang_add_array(char *name, long* addr, 
				    int dim, int d0, int d1, int d2, 
				    unsigned char t, unsigned char flags)
{
   SLuser_Object_Type *uo;
   
   if (NULL != (uo = SLcreate_array (addr, dim, d0, d1, d2, t, flags)))
     {
	/* By default, create_array creates SLANG_DATA array.  Since this is 
	 * intrinsic, we over ride it here.
	 */
	uo->main_type = SLANG_IVARIABLE; uo->sub_type = ARRAY_TYPE;
	SLadd_name(name, (long) uo, SLANG_IVARIABLE, ARRAY_TYPE);
     }
   return uo;
}


void SLarray_sort(char *f)
{
   SLArray_Type *at_str, *at_int;
   unsigned char mtype;
   SLang_Name_Type *entry;
   SLuser_Object_Type *uat_str, *uat_int;
   int sdat;
   int l, j, ir, i, n, cmp;
   unsigned int *ra, rra;
   
   
   if ((NULL == (entry = SLang_locate_name(f))) || (*entry->name == 0))
     {
	SLang_doerror("Sort function undefined.");
	return;
     }
   
   mtype = entry->main_type;
   if (mtype != SLANG_FUNCTION)
     {
	SLang_doerror("Invalid sort function.");
	return;
     }
   
   sdat = 0;			       /* no STRINGs */
   if (NULL == (uat_str = SLang_pop_array(&sdat))) return;
   
   at_str = (SLArray_Type *) uat_str->obj;
   
   if (at_str->flags == SLANG_RVARIABLE)
     {
	SLang_Error = READONLY_ERROR;
	goto return_err;
     }
   
   n = at_str->x;
   
   if (at_str->dim != 1)
     {
	SLang_doerror("Sort requires 1 dim arrays.");
	goto return_err;
     }
   
   if (NULL == (uat_int = SLcreate_array(NULL, 1, n, 1, 1, 'i', 0)))
     {
	SLang_doerror("Error Creating index array.");
	goto return_err;
     }
   
   at_int = (SLArray_Type *) uat_int->obj;

   ra = (unsigned int *) at_int->buf.i_ptr;
   ra--;
   for (i = 1; i <= n; i++) ra[i] = i;
   
   /* heap sort from adapted from numerical recipes */
   
   l = 1 + n / 2;
   ir = n;
   
   while (1)
     {
	if (l > 1) rra = ra[--l];
	else
	  {
	     rra = ra[ir];
	     ra[ir] = ra[1];
	     if (--ir <= 1)
	       {
		  ra[1] = rra;
		  for (i = 1; i <= n; i++) ra[i] -= 1;
		  SLang_push_user_object (uat_int);
		  /* Break to free the other user object. */
		  break;
	       }
	  }
	i = l;
	j = 2 * l;
	while(j <= ir)
	  {
	     if (j < ir)
	       {
		  array_push_element(at_str, ra[j] - 1);
		  array_push_element(at_str, ra[j + 1] - 1);
		  SLexecute_function(entry);
		  if (SLang_pop_integer(&cmp)) 
		    {
		       /* Guess we will not be needing this. */
		       SLang_free_user_object (uat_int);
		       goto return_err;
		    }
		  if (cmp) j++;
	       }
	     array_push_element(at_str, rra - 1);
	     array_push_element(at_str, ra[j] - 1);
	     SLexecute_function(entry);
	     if (SLang_pop_integer(&cmp))
	       {
		  /* Guess we will not be needing this. */
		  SLang_free_user_object (uat_int);
		  goto return_err;
	       }
	     
	     if (cmp) 
	       {
		  ra[i] = ra[j];
		  i = j;
		  j += j;
	       }
	     else j = ir + 1;
	  }
	ra[i] = rra;
     }

   /* No matter what, we have call free_user_object.  */
   return_err:
   SLang_free_user_object (uat_str);
}


void SLinit_char_array (void)
{
   int sdat;
   SLArray_Type *at;
   SLuser_Object_Type *uat;
   unsigned char *s;
   unsigned int n, ndim;
   
   if (SLpop_string((char **) &s)) return;
   sdat = 0;			       /* No STRINGS */
   if (NULL == (uat = SLang_pop_array(&sdat))) goto free_and_return;
   at = (SLArray_Type *) uat->obj;
   if (at->type != CHAR_TYPE)
     {
	SLang_doerror("Operation requires character array.");
	goto free_and_return;
     }
   n = (unsigned int) strlen((char *)s);
   ndim = at->x * at->y * at->z;
   if (n > ndim)
     {
	SLang_doerror("String too big to init Array.");
	goto free_and_return;
     }
   
   strncpy((char *) at->buf.c_ptr, (char *) s, ndim);
   
   free_and_return:
   
   if (uat != NULL)
     SLang_free_user_object (uat);
   
   SLFREE(s);
}

   

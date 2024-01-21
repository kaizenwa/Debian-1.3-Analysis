/* User defined objects */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */
#include "config.h"

#include <stdio.h>

#include "slang.h"
#include "_slang.h"

SLang_Class_Type *SLang_Registered_Types[256];

static void default_destroy (long *unused)
{
   (void) unused;
}


int SLang_register_class (unsigned char sub_type, FVOID_STAR dest, FVOID_STAR string)
{
   SLang_Class_Type *cl;
   
   cl = SLang_Registered_Types[sub_type];
   
   if (cl != NULL) return 0;
   
   cl = (SLang_Class_Type *) SLMALLOC (sizeof (SLang_Class_Type));
   if (cl == NULL) return 0;
   
   SLMEMSET ((char *) cl, 0, sizeof (SLang_Class_Type));
   
   if (dest == NULL) dest = (FVOID_STAR) default_destroy;
   
   cl->destroy = (void (*)(VOID_STAR)) dest;
   cl->string = (char *(*)(VOID_STAR)) string;
   
   SLang_Registered_Types[sub_type] = cl;
   return 1;
}


static void add_binary_op (SLang_Class_Type *cla, 
			   SL_OOBinary_Type *ba, unsigned char stb,
			   FVOID_STAR fa)
{
   SL_OOBinary_Type *tmp;

   ba->sub_type = stb;
   ba->binary_function = (int (*)_PROTO((int, unsigned char, unsigned char, 
					   VOID_STAR, VOID_STAR))) fa;
   ba->next = NULL;

   tmp = cla->binary_ops;
   if (tmp == NULL) cla->binary_ops = ba;
   else 
     {
	while (tmp->next != NULL) tmp = tmp->next;
	tmp->next = ba;
     }
}

int SLang_add_binary_op (unsigned char sta, unsigned char stb, FVOID_STAR f)
{
   SLang_Class_Type *cla, *clb;
   SL_OOBinary_Type *ba = NULL, *bb = NULL;
   
   cla = SLang_Registered_Types [sta];
   clb = SLang_Registered_Types [stb];

   if ((cla == NULL) || (clb == NULL)) return 0;
   
   if ((NULL == (ba = (SL_OOBinary_Type *) SLMALLOC (sizeof(SL_OOBinary_Type))))
       || (NULL == (bb = (SL_OOBinary_Type *) SLMALLOC (sizeof(SL_OOBinary_Type)))))
     {
	goto malloc_error_return;
     }
   
   add_binary_op (cla, ba, stb, f);
   add_binary_op (clb, bb, sta, f);
   return 1;
   
   /* Malloc errors go here */
   malloc_error_return:
   if (ba != NULL) SLFREE (ba);  
   if (bb != NULL) SLFREE (bb);
   return 0;
}

int SLang_add_unary_op (unsigned char st, FVOID_STAR f)
{
   SLang_Class_Type *cl;
   
   cl = SLang_Registered_Types[st];
   if (cl == NULL) return 0;
   
   cl->unary_function = (int (*)_PROTO((int, unsigned char, VOID_STAR))) f;
   
   return 1;
}

int SLang_add_copy_operation (unsigned char st, FVOID_STAR f)
{
   SLang_Class_Type *cl;
   
   cl = SLang_Registered_Types[st];
   if (cl == NULL) return 0;
   
   cl->copy_function = (int (*)_PROTO((unsigned char, VOID_STAR))) f;
   
   return 1;
}


SLuser_Object_Type *SLang_create_user_object (unsigned char t)
{
   SLuser_Object_Type *u;
   
   if (t < ARRAY_TYPE) return NULL;
   /* See if it is registered. */
   if (SLang_Registered_Types[t] == NULL)
     {
	SLang_doerror ("Type not registered.");
	return NULL;
     }
   
   
   if (NULL == (u = (SLuser_Object_Type *) SLMALLOC (sizeof (SLuser_Object_Type))))
     {
	SLang_Error = SL_MALLOC_ERROR;
	return NULL;
     }
   u->sub_type = t; u->main_type = SLANG_DATA;
   u->count = 0;
   return u;
}

   
void SLang_free_user_object (SLuser_Object_Type *obj)
{
   unsigned char type;
   
   if (obj->main_type == SLANG_DATA)
     {
	obj->count -= 1;
	if (obj->count == 0)
	  {
	     type = obj->sub_type;
	     (*(SLang_Registered_Types[type]->destroy)) ((VOID_STAR) obj->obj);
	     SLFREE (obj);
	  }
     }
}

void SLang_free_intrinsic_user_object (SLuser_Object_Type *obj)
{
   obj->main_type = SLANG_DATA;
   SLang_free_user_object (obj);
}

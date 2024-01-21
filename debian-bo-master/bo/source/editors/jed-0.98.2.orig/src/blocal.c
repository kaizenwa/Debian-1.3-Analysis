/* -*- mode: C; mode: fold; -*- */
#include <stdio.h>
#include <string.h>

#include "config.h"
#include "jed-feat.h"

#if JED_HAS_BUFFER_LOCAL_VARS

#include <slang.h>

#include "jdmacros.h"
#include "buffer.h"
#include "misc.h"

#define BLOCAL_STRING	1
#define BLOCAL_INTEGER  2

static Jed_BLocal_Type *find_blocal_var (char *name, int err) /*{{{*/
{
   unsigned int len;
   Jed_BLocal_Table_Type *table;
   Jed_BLocal_Type *lv, *lv_max;
   
   len = strlen (name);
   table = CBuf->blocal_table;
   
   while (table != NULL)
     {
	lv = table->local_vars;
	lv_max = lv + table->num;
	
	while (lv < lv_max)
	  {
	     if (((unsigned int) *lv->name == len)
		 && (!strcmp (name, lv->name + 1)))
	       return lv;
	     lv++;
	  }
	table = table->next;
     }
   
   if (err) msg_error ("buffer local variable does not exist.");

   return NULL;
}

/*}}}*/

void jed_make_blocal_var (void) /*{{{*/
{
   int type;
   char *name;
   Jed_BLocal_Table_Type *table;
   Jed_BLocal_Type *lv;
   
   if (SLang_pop_integer (&type))
     return;
   
   if (SLpop_string (&name))
     return;
   
   if (type == 's') type = BLOCAL_STRING;
   else if (type == 'i') type = BLOCAL_INTEGER;
   else
     {
	msg_error ("Unsupported type for buffer local variable");
	goto return_error;
     }

   if (NULL != find_blocal_var (name, 0))
     goto return_error;		       /* already exists */
   
   table = CBuf->blocal_table;
   
   if ((table == NULL)
       || (table->num == MAX_BLOCAL_VARS_PER_TABLE))
     {
	table = (Jed_BLocal_Table_Type *) SLMALLOC (sizeof (Jed_BLocal_Table_Type));
	if (table == NULL) 
	  {
	     SLang_Error = SL_MALLOC_ERROR;
	     goto return_error;
	  }
	
	SLMEMSET ((char *) table, 0, sizeof (Jed_BLocal_Table_Type));
	table->next = CBuf->blocal_table;
	CBuf->blocal_table = table;
     }
   
   lv = table->local_vars + table->num;
   
   strncpy (lv->name + 1, name, 30);
   lv->name[30 + 1] = 0;
   *lv->name = (char) strlen (lv->name + 1);

   lv->type = type;

   table->num += 1;
   
   /* drop */
   
   return_error:
   
   SLFREE (name);
}

/*}}}*/

void jed_delete_blocal_vars (Jed_BLocal_Table_Type *table) /*{{{*/
{
   Jed_BLocal_Type *lv, *lv_max;
   Jed_BLocal_Table_Type *next;

   while (table != NULL)
     {
	lv = table->local_vars;
	lv_max = lv + table->num;
	
	while (lv < lv_max)
	  {
	     if (lv->type == BLOCAL_STRING)
	       {
		  if (lv->v.s != NULL) SLFREE (lv->v.s);
	       }
	     lv++;
	  }

	next = table->next;
	SLFREE (table);
	table = next;
     }
}

/*}}}*/

void jed_set_blocal_var (void) /*{{{*/
{
   char *name;
   Jed_BLocal_Type *lv;
   
   if (SLpop_string (&name))
     return;
   
   lv = find_blocal_var (name, 1);
   if (lv == NULL)
     {
	goto return_error;
     }
   
   if (lv->type == BLOCAL_STRING)
     {
	char *val;
	
	if (SLpop_string (&val))
	  goto return_error;
	
	if (lv->v.s != NULL) SLFREE (lv->v.s);
	lv->v.s = val;
     }
   else
     {
	if (SLang_pop_integer (&lv->v.i))
	  goto return_error;
     }
   
   
   /* drop */
   return_error:
   SLFREE (name);
}

/*}}}*/

   
void jed_get_blocal_var (char *name) /*{{{*/
{
   Jed_BLocal_Type *lv; 
   
   lv = find_blocal_var (name, 1);
   if (lv == NULL)
     return;
   
   if (lv->type == BLOCAL_STRING)
     {
	char *s;
	
	s = lv->v.s;
	if (s == NULL) s = "";
	SLang_push_string (s);
     }
   else
     {
	SLang_push_integer (lv->v.i);
     }
}

/*}}}*/

#endif				       /* JED_HAS_BUFFER_LOCAL_VARS */

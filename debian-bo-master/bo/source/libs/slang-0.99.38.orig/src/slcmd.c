/* cmd line facility for slang */
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

#include "slang.h"
#include "_slang.h"

#ifndef HAVE_STDLIB_H
/* Oh dear.  Where is the prototype for atof?  If not in stdlib, then
 * I do not know where.  Not in math.h onsome systems either.
 */
extern double atof ();
#endif


static SLcmd_Cmd_Type *SLcmd_find_command (char *s, SLcmd_Cmd_Type *cmd)
{
   char *cmdstr;
   register char chs = *s++, ch;
   while (1)
     {
	cmdstr = cmd->cmd;
	ch = *cmdstr++;
	if (ch == 0) break;
	if ((ch == chs) && !strcmp (s, cmdstr)) return cmd;
	cmd++;
     }
   return NULL;
}

int SLcmd_execute_string (char *str, SLcmd_Cmd_Table_Type *table)
{
   char *s, *b = NULL, *arg_type, *last_str, *cmd_name;
   SLcmd_Cmd_Type *cmd;
   char buf[256];
   int token_present;
   int i, ret = -1;
   unsigned int len;
   int argc;
   
   if ((0 == SLang_extract_token (&str, buf, 0))
       || (*buf == '%'))
     return 0;
   if (SLang_Error) return -1;
   
   if (((len = strlen (buf)) >= 32)
       || (NULL == (cmd = SLcmd_find_command (buf, table->table))))
     {
	_SLdo_error ("%s: invalid command.", buf);
	return -1;
     }

   if (NULL == (cmd_name = SLmake_string (buf)))
     return -1;
   
   argc = 0;
   table->string_args[argc++] = cmd_name;
   
   arg_type = cmd->arg_type;
   
   while (*arg_type)
     {
	int guess_type = 0;
	
	last_str = str;
	token_present = SLang_extract_token (&str, buf, 0);
	if (SLang_Error) goto error;
	if (token_present)
	  {
	     b = buf;
	     len = strlen (b);
	     if ((*b == '"') && (len > 1))
	       {
		  b++;
		  len -= 2;
		  b[len] = 0;
		  guess_type = STRING_TYPE;
	       }
	     else guess_type = SLang_guess_type (buf);
	  }
	
	switch (*arg_type++)
	  {
	     /* variable argument number */
	   case 'v':
	     if (token_present == 0) break;
	   case 'V':
	     if (token_present == 0)
	       {
		  _SLdo_error ("%s: Expecting argument", cmd_name);
		  goto error;
	       }
	     
	     while (*last_str == ' ') last_str++;
	     len = strlen (last_str);
	     str = last_str + len;
	     
	     s = SLmake_nstring (last_str, len);
	     if (s == NULL) goto error;
	     table->arg_type[argc] = STRING_TYPE;
	     table->string_args[argc++] = s;
	     break;
	     
	   case 's':
	     if (token_present == 0) break;
	   case 'S':
	     if (token_present == 0)
	       {
		  _SLdo_error ("%s: Expecting string argument", cmd_name);
		  goto error;
	       }
	     
	     s = SLmake_nstring (b, len);

	     if (s == NULL) goto error;
	     table->arg_type[argc] = STRING_TYPE;
	     table->string_args[argc++] = s;
	     break;
	     
	     /* integer argument */
	   case 'i':
	     if (token_present == 0) break;
	   case 'I':
	     if ((token_present == 0) || (INT_TYPE != guess_type))
	       {
		  _SLdo_error ("%s: Expecting integer argument", cmd_name);
		  SLang_Error = TYPE_MISMATCH;
		  goto error;
	       }

	     table->arg_type[argc] = INT_TYPE;
	     table->int_args[argc++] = SLatoi((unsigned char *) buf);
	     break;

	     /* floating point arg */
#ifdef FLOAT_TYPE
	   case 'f':
	     if (token_present == 0) break;
	   case 'F':
	     if ((token_present == 0) || (STRING_TYPE == guess_type))
	       {
		  _SLdo_error ("%s: Expecting float argument", cmd_name);
		  SLang_Error = TYPE_MISMATCH;
		  goto error;
	       }
	     table->arg_type[argc] = FLOAT_TYPE;
	     table->float_args[argc++] = atof(buf);
	     break;
#endif
	     /* Generic type */
	   case 'g':
	     if (token_present == 0) break;
	   case 'G':
	     if (token_present == 0)
	       {
		  _SLdo_error ("%s: Expecting argument", cmd_name);
		  SLang_Error = TYPE_MISMATCH;
		  goto error;
	       }
	     
	     switch (guess_type)
	       {
		case INT_TYPE:
		  table->arg_type[argc] = INT_TYPE;
		  table->int_args[argc++] = SLatoi((unsigned char *) buf);
		  break;
		  
		case STRING_TYPE:
		  s = SLmake_nstring (b, len);
		  if (s == NULL) goto error;
		  
		  table->arg_type[argc] = STRING_TYPE;
		  table->string_args[argc++] = s;
		  break;
#ifdef FLOAT_TYPE
		case FLOAT_TYPE:
		  table->arg_type[argc] = FLOAT_TYPE;
		  table->float_args[argc++] = atof(buf);
#endif
	       }
	     break;
	  }
     }
   
   /*                 call function */
   ret = (*cmd->cmdfun)(argc, table);
   
   error:
   for (i = 0; i < argc; i++)
     {
	if (NULL != table->string_args[i])
	  {
	     SLFREE (table->string_args[i]);
	     table->string_args[i] = NULL;
	  }
     }
   return ret;
   
}

   


   

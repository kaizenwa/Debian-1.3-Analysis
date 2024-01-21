/*
 *  Project   : tin - a Usenet reader
 *  Module    : vms.c
 *  Author    : Andrew Greer
 *  Created   : 19-06-95
 *  Updated   : 19-06-95
 *  Notes     :
 *  Copyright : (c) Copyright 1991-95 by Iain Lea & Andrew Greer
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include "config.h"
#include "slrnfeat.h"

#ifdef VMS

#include <stdio.h>
#include <ctype.h>
#include <descrip.h>
#include <iodef.h>
#include <ssdef.h>
#include <uaidef.h>
#include <string.h>
#include <stdlib.h>
#include <file.h>

#include "vms.h"

char *slrn_vms_getlogin (void)
{
   return getenv ("USER");
}

static struct dsc$descriptor *c$dsc(char *c$_str)
{
   static struct dsc$descriptor c$_tmpdesc;
   
   c$_tmpdesc.dsc$w_length = strlen(c$_str);
   c$_tmpdesc.dsc$b_dtype  = DSC$K_DTYPE_T;
   c$_tmpdesc.dsc$b_class  = DSC$K_CLASS_S;
   c$_tmpdesc.dsc$a_pointer= c$_str;
   return(&c$_tmpdesc);
}

char *slrn_vms_get_uaf_fullname (void)
{
   static char uaf_owner[40];
   char loc_username[13];
   int i, pos;
   
   struct item_list 
     {
	short bl, ic;
	char *ba;
	short *rl;
     }
   getuai_itmlist[] = 
     {
	  {
	     sizeof(uaf_owner),
	       UAI$_OWNER,
	       &uaf_owner[0],
	       0
	  },
	  { 0, 0, 0, 0}
     };
   
   strcpy(loc_username, getenv("USER"));
   for (i = strlen(loc_username); i < 12; ++i)
     loc_username[i] = ' ';
   loc_username[i] = '\0';
   
   sys$getuai(0,0,c$dsc(loc_username),getuai_itmlist,0,0,0);
   
   pos=1;
   if (uaf_owner[pos]=='|')
     pos += 3;
   while (uaf_owner[pos] == ' ')
     pos++;
   uaf_owner[uaf_owner[0] + 1] = '\0';
   return(uaf_owner + pos);
}

/* Converts "TOD_MCQUILLIN" to "Tod McQuillin" */
char *slrn_vms_fix_fullname(char *p)
{
   int cc = 0;
   char *q = p;
   
   while (*q) 
     {
	if (cc > 0) 
	  {
	     if (cc > 1 && *(q-1) == 'c' && *(q-2) == 'M') 
	       {
		  if (islower(*q))
		    *q = toupper(*q);
	       }
	     else
	       if (isupper(*q))
		 *q = tolower(*q);
	  }
	else
	  if (cc == 0)
	    if (islower(*q))
	      *q = toupper(*q);
	if (*q == '_' || *q == ' ') 
	  {
	     *q = ' ';
	     cc = 0;
	  }
	else
	  cc++;
	q++;
     }
   return (p);
}
#if 0
FILE *
popen (
       char *command,
       char *mode)
{
   return ((FILE *) 0);
}


void
pclose (FILE *pipe)
{
   return;
}

void tzset(void)
{
   return;
}
#endif

#endif /* VMS */

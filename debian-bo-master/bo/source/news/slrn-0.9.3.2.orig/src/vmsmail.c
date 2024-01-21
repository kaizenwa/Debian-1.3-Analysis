/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "slrnfeat.h"

#include <stdio.h>
#include <ssdef.h>
#include <string.h>


#include "vms.h"
#include "vmsmail.h"

extern int mail$send_add_bodypart ();
extern int mail$send_begin ();
extern int mail$send_add_attribute ();
extern int mail$send_add_address ();
extern int mail$send_message ();
extern int mail$send_end ();


static void fill_struct(Mail_Type *m, int act, char *s)
{
   m->code = act;
   m->buflen = strlen(s);
   m->addr = (long) s;
   m->junk = m->ret = 0;
}


/* to might be a comma separated list--- parse it too */
int vms_send_mail(char *to, char *subj, char *file)
{
   Mail_Type mt0, mt;
   int context = 0;
   char *p;
   
   mt0.code = mt0.buflen = mt0.addr = mt0.ret = mt0.junk = 0;
   
   if (SS$_NORMAL != mail$send_begin(&context, &mt0, &mt0))
     {
	return(0);
     }
#if 0
   fill_struct(&mt, MAIL$_SEND_TO_LINE, to);
   if (SS$_NORMAL != mail$send_add_attribute(&context, &mt, &mt0))
     {
	return(0);
     }
   
   fill_struct(&mt, MAIL$_SEND_USERNAME, to);
   if (SS$_NORMAL != mail$send_add_address(&context, &mt, &mt0))
     {
	return(0);
     }
#endif
   while (1)
     {
	while (*to && ((*to <= ' ') || (*to == ','))) to++;
	if (*to == 0) break;
	p = to;
	while ((*p > ' ') && (*p != ',')) p++;
	
        mt.code = MAIL$_SEND_TO_LINE;
	mt.buflen = p - to;
	mt.ret = mt.junk = 0;
	mt.addr = (long) to;
	
	if (SS$_NORMAL != mail$send_add_attribute(&context, &mt, &mt0))
	  {
	     return(0);
	  }
	
	mt.code = MAIL$_SEND_USERNAME;
	mt.buflen = p - to;
	mt.ret = mt.junk = 0;
	mt.addr = (long) to;
	
	if (SS$_NORMAL != mail$send_add_address(&context, &mt, &mt0))
	  {
	     return(0);
	  }
	to = p;
     }
   
   fill_struct(&mt, MAIL$_SEND_SUBJECT, subj);
   if (SS$_NORMAL != mail$send_add_attribute(&context, &mt, &mt0))
     {
	return(0);
     }
   
   fill_struct(&mt, MAIL$_SEND_FILENAME, file);
   if (SS$_NORMAL != mail$send_add_bodypart(&context, &mt, &mt0))
     {
	return(0);
     }
   
   if (SS$_NORMAL != mail$send_message(&context, &mt0, &mt0))
     {
	return(0);
     }
   
   if (SS$_NORMAL != mail$send_end(&context, &mt0, &mt0))
     {
	return(0);
     }
   return(1);
}

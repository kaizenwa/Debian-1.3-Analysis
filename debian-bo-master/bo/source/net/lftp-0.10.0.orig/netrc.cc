/*
 * lftp and utils
 *
 * Copyright (c) 1996-1997 by Alexander V. Lukyanov (lav@yars.free.net)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "netrc.h"
#include "xalloca.h"
#include "xmalloc.h"

NetRC::Entry::Entry(const char *h,const char *u,const char *p,const char *a)
{
   host=xstrdup(h);
   user=xstrdup(u);
   pass=xstrdup(p);
   acct=xstrdup(a);
}

NetRC::Entry::~Entry()
{
   free(host);
   free(user);
   free(pass);
   free(acct);
}

NetRC::Entry *NetRC::LookupHost(const char *h)
{
   char str[256];
   char chost[256]="";
   char cuser[256]="";
   char cpass[256]="";
   char cacct[256]="";
   char *home=getenv("HOME")?:".";
   char *netrc=(char*)alloca(strlen(home)+8);
   sprintf(netrc,"%s/.netrc",home);
   FILE *f=fopen(netrc,"r");

   if(f==NULL)
      return NULL;

   while(fscanf(f,"%255s",str)==1)
   {
      if(!strcmp(str,"default"))
	 goto assign_host;
      if(!strcmp(str,"machine"))
      {
	 if(!strcmp(chost,h))
	    break;
	 if(fscanf(f,"%255s",str)!=1)
	    break;
      assign_host:
	 strcpy(chost,str);
	 cuser[0]=0;
	 cpass[0]=0;
	 cacct[0]=0;
	 continue;
      }
      if(!strcmp(str,"login"))
      {
	 if(fscanf(f,"%255s",str)!=1)
	    break;
	 strcpy(cuser,str);
	 continue;
      }
      if(!strcmp(str,"password"))
      {
	 if(fscanf(f,"%255s",str)!=1)
	    break;
	 strcpy(cpass,str);
	 continue;
      }
      if(!strcmp(str,"account"))
      {
	 if(fscanf(f,"%255s",str)!=1)
	    break;
	 strcpy(cacct,str);
	 continue;
      }
   }
   fclose(f);
   if(!strcmp(chost,h) || !strcmp(chost,"default"))
      return new Entry(h,cuser[0]?cuser:0,cpass[0]?cpass:0,cacct[0]?cacct:0);
   return 0;
}

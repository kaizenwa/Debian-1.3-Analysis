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
#include <fcntl.h>
#include "mgetJob.h"
#include "lftp.h"

void mgetJob::ShowRunStatus(StatusLine *s)
{
   if(rg)
   {
      s->Show("Getting file list [%s]",session->CurrentStatus());
      return;
   }
   GetJob::ShowRunStatus(s);
}
void mgetJob::PrintStatus(int v)
{
   if(rg)
   {
      SessionJob::PrintStatus(v);
      printf("\tGetting file list [%s]\n",session->CurrentStatus());
      return;
   }
   GetJob::PrintStatus(v);
}

mgetJob::mgetJob(Ftp *session,ArgV *args) : GetJob(session,0,args)
{
   rg=0;

   this->args=args;

   args->rewind();
   int opt;
   while((opt=args->getopt("+cd"))!=EOF)
   {
      switch(opt)
      {
      case('c'):
	 flags|=CONTINUE;
	 break;
      case('d'):
	 flags|=MAKE_DIRS;
	 break;
      case('?'):
      print_usage:
	 printf("Usage: %s [-c] [-d] pattern ...\n",args->getarg(0));
	 return;
      }
   }
   args->back();
   char *p=args->getnext();
   if(!p)
      goto print_usage;

   rg=new RemoteGlob(session,p,Ftp::LIST);
   rg->SetSlashFilter(0);
   rg->Do();
}

int mgetJob::Do()
{
   int m=STALL;

   if(!rg)
      return GetJob::Do();

   if(!rg->Done())
      return m;

   m=MOVED;

   char **files=rg->GetResult();
   if(!files)
      fprintf(stderr,"%s: no such files\n",rg->pattern);
   else
   for(char **i=files; *i; i++)
   {
      char *nl=strrchr(*i,'/');
      char *local_name;
      if(!nl)
	 local_name=*i;
      else
      {
	 if(flags&MAKE_DIRS)
	 {
	    *nl=0;
	    create_directories(*i);
	    *nl='/';
	    local_name=*i;
	 }
	 else
	    local_name=nl+1;
      }
      FDStream *local_stream=new FileStream(local_name,O_WRONLY|O_CREAT
				       |(flags&CONTINUE?0:O_TRUNC));
      AddToDo(new GetRec(*i,local_stream));
   }

   delete rg;
   rg=0;

   char *p=args->getnext();
   if(!p)
   {
      delete args;
      args=0;
      return m;
   }

   rg=new RemoteGlob(session,p,Ftp::LIST);
   rg->SetSlashFilter(0);
   rg->Do();

   return m;
}

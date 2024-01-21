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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <glob.h>
#include "mputJob.h"
#include "mkrmJob.h"

mputJob::mputJob(Ftp *session,ArgV *args) : PutJob(session,args)
{
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

   ArgV *mkdir_args=0;
   glob_t pglob;
   unsigned i;

   for(;;)
   {
      glob(p,0,0,&pglob);
      if(pglob.gl_pathc==0)
	 fprintf(stderr,"%s: no such files\n",p);
      for(i=0; i<pglob.gl_pathc; i++)
      {
	 char *local_name=pglob.gl_pathv[i];

	 struct stat st;
	 if(stat(local_name,&st)!=-1 && !S_ISREG(st.st_mode))
	    continue;	// put only regular files

	 char *slash=strrchr(local_name,'/');
	 char *remote_name;
	 if(!slash)
	    remote_name=local_name;
	 else
	 {
	    if(flags&MAKE_DIRS)
	    {
	       *slash=0;
	       if(!mkdir_args)
	       {
		  mkdir_args=new ArgV;
	          mkdir_args->Append("mkdir");
	       }
	       int j;
	       for(j=1; j<mkdir_args->count(); j++)
		  if(!strcmp(local_name,mkdir_args->getarg(j)));
		     break;
	       if(j==mkdir_args->count()) // don't try to create dir twice
		  mkdir_args->Append(local_name);
	       *slash='/';
	       remote_name=local_name;
	    }
	    else
	       remote_name=slash+1;
	 }
	 FDStream *local_stream=new FileStream(local_name,O_RDONLY);
	 AddToDo(new GetRec(remote_name,local_stream));
      }
      globfree(&pglob);
      p=args->getnext();
      if(!p)
	 break;
   }
   if(mkdir_args)
   {
      mkrmJob *m=new mkrmJob(Clone(),mkdir_args);
      m->BeQuiet();
      waiting=m;
      waiting->parent=this;
      waiting->cmdline=mkdir_args->Combine();
      // don't delete mkdir_args; -- mkrmJob does it
   }
}

int mputJob::Do()
{
   if(waiting)
   {
      if(waiting->Done())
      {
	 delete waiting;
	 waiting=0;
      }
      else
	 return STALL;
   }
   return PutJob::Do();
}

void  mputJob::PrintStatus(int v)
{
   if(waiting)
   {
      printf("\tCreating remote directories\n");
      waiting->PrintStatus(v);
      return;
   }
   PutJob::PrintStatus(v);
}

void  mputJob::ShowRunStatus(StatusLine *s)
{
   if(waiting)
      waiting->ShowRunStatus(s);
   else
      PutJob::ShowRunStatus(s);
}

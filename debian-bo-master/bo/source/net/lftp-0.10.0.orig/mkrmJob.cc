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

#include <errno.h>
#include "mkrmJob.h"

mkrmJob::mkrmJob(Ftp *s,ArgV *a) : SessionJob(s)
{
   quiet=false;
   failed=file_count=0;

   args=a;
   args->rewind();
   char *op=args->getarg(0);
   first=curr=args->getnext();
   if(curr==0)
   {
      fprintf(stderr,"Usage: %s files...\n",op);
      return;
   }

   if(!strcmp(op,"rm"))
      mode=Ftp::REMOVE;
   else if(!strcmp(op,"rmdir"))
      mode=Ftp::REMOVE_DIR;
   else if(!strcmp(op,"mkdir"))
      mode=Ftp::MAKE_DIR;
   else
      abort();
}

mkrmJob::~mkrmJob()
{
   delete args;
   args=0;
}

int mkrmJob::Do()
{
   if(Done())
      return STALL;
   if(session->IsClosed())
      session->Open(curr,mode);
   int res=session->Read(0,0);
   if(res==Ftp::SEE_ERRNO && (errno==EAGAIN || errno==EMFILE || errno==ENFILE))
      return STALL;
   if(res<0)
   {
      failed++;
      if(!quiet)
	 fprintf(stderr,"%s: %s\n",args->getarg(0),session->StrError(res));
   }
   file_count++;
   session->Close();
   curr=args->getnext();
   return MOVED;
}

void  mkrmJob::PrintStatus(int v)
{
   SessionJob::PrintStatus(v);
   if(Done())
      return;
   printf("\t`%s' [%s]\n",curr,session->CurrentStatus());
}

void  mkrmJob::ShowRunStatus(StatusLine *s)
{
   if(Done())
      s->Show("");
   else
      s->Show("%s `%s' [%s]",args->getarg(0),curr,session->CurrentStatus());
}

void  mkrmJob::SayFinal()
{
   if(failed==file_count)
      return;
   char *file=(mode==Ftp::REMOVE?"fil":"directori");
   char *crdel=(mode==Ftp::MAKE_DIR?"creat":"remov");
   char *op=args->getarg(0);
   if(file_count==1)
      printf("%s ok, `%s' %sed\n",op,first,crdel);
   else if(failed)
      printf("%s failed for %d of %d %ses\n",op,failed,file_count,file);
   else
      printf("%s ok, %d %ses %sed\n",op,file_count,file,crdel);
}

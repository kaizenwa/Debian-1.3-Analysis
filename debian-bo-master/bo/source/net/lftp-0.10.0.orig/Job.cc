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
#include "Job.h"

Job *Job::chain;

Job::Job()
{
   next=chain;
   chain=this;
   cmdline=0;
   parent=0;
   waiting=0;
   jobno=-1;
}

void  Job::AllocJobno()
{
   jobno=0;
   for(Job *scan=chain; scan; scan=scan->next)
      if(scan!=this && scan->jobno>=jobno)
	 jobno=scan->jobno+1;
}

Job::~Job()
{
   // first, kill children (hm, that's sadistic)
   {
      for(Job *scan=chain; scan; )
      {
	 if(scan->parent==this)
	 {
	    delete scan;
	    scan=chain;
	 }
	 else
	    scan=scan->next;
      }
   }
   // now, delete the job from the list
   {
      for(Job **scan=&chain; *scan; scan=&(*scan)->next)
      {
	 if(*scan==this)
	 {
	    *scan=next;
	    break;
	 }
      }
   }

   if(cmdline)
      free(cmdline);
}

Job *Job::FindJob(int n)
{
   for(Job *scan=chain; scan; scan=scan->next)
   {
      if(scan->jobno==n)
	 return scan;
   }
   return 0;
}

void Job::Kill(int n)
{
   Job *j=FindJob(n);
   if(j && (j->parent==this || j->parent==0))
      delete j;
}

void Job::KillAll()
{
   for(Job **scan=&chain; *scan; )
   {
      if((*scan)->jobno>=0)
	 delete *scan;
      else
	 scan=&(*scan)->next;
   }
}

int   Job::NumberOfJobs()
{
   int count=0;
   for(Job *scan=chain; scan; scan=scan->next)
      if(!scan->Done())
	 count++;
   return count;
}

static int jobno_compare(const void *a,const void *b)
{
   Job *ja=*(Job*const*)a;
   Job *jb=*(Job*const*)b;
   return ja->jobno-jb->jobno;
}

void  Job::SortJobs()
{
   int count=0;
   Job *scan;
   for(scan=chain; scan; scan=scan->next)
      count++;

   if(count==0)
      return;

   Job **arr=(Job**)alloca(count*sizeof(*arr));
   count=0;
   for(scan=chain; scan; scan=scan->next)
      arr[count++]=scan;

   qsort(arr,count,sizeof(*arr),jobno_compare);

   chain=0;
   while(count--)
   {
      arr[count]->next=chain;
      chain=arr[count];
   }
}

void  Job::ListJobs(int verbose,int indent)
{
   if(indent==0)
      SortJobs();

   for(Job *scan=chain; scan; scan=scan->next)
   {
      if(!scan->Done() && scan->parent==this)
      {
	 printf("%*s",indent,"");
	 if(scan->jobno>=0)
	    printf("[%d] ",scan->jobno);
	 printf("%s\n",scan->cmdline?:"?");
	 scan->PrintStatus(verbose);
	 scan->ListJobs(verbose,indent+1);
      }
   }
}

void  Job::ListDoneJobs()
{
   SortJobs();

   FILE *f=stdout;
   for(Job *scan=chain; scan; scan=scan->next)
   {
      if(scan->jobno>=0 && (scan->parent==this || scan->parent==0)
         && scan->Done())
      {
	 fprintf(f,"[%d] Done (%s)\n",scan->jobno,scan->cmdline?:"?");
	 scan->PrintStatus(0);
      }
   }
}

void  Job::BuryDoneJobs()
{
   for(Job *scan=chain; scan; )
   {
      if((scan->parent==this || scan->parent==0) && scan->jobno>=0
		  && scan->Done())
      {
	 delete scan;
      	 scan=chain;
      }
      else
	 scan=scan->next;
   }
}

Ftp *SessionPool::pool[pool_size];

void SessionPool::Reuse(Ftp *f)
{
   f->Close();
   int i;
   for(i=0; i<pool_size; i++)
   {
      if(pool[i]==0)
      {
	 pool[i]=f;
	 return;
      }
   }
   for(i=0; i<pool_size; i++)
   {
      if(!pool[i]->IsConnected())
      {
	 delete pool[i];
	 pool[i]=f;
	 return;
      }
   }
   delete f;
}

Ftp *SessionPool::GetBetter(Ftp *f)
{
   if(f->IsOpen())
      return f;

   int i,level;
   for(level=0; level<2; level++)
   {
      for(i=0; i<pool_size; i++)
      {
	 if(pool[i]!=0 && !f->IsConnected() && pool[i]->IsConnected()
	 && pool[i]->SameLocationAs(f,level))
	 {
	    Ftp *tmp=pool[i];
	    pool[i]=f;
	    tmp->CopyOptions(f);
	    return tmp;
	 }
      }
   }
   return f;
}

Ftp *SessionJob::Clone()
{
   GetBetter();
   Ftp *tmp=session;
   session=GetBetter(new Ftp(session));
   return tmp;
}

SessionJob::SessionJob(Ftp *f)
{
   session=f;
}

SessionJob::~SessionJob()
{
   if(session)
      Reuse(session);
   session=0;
}

void SessionJob::PrintStatus(int v)
{
   if(v<2)
      return;
   printf("\t%s@%s:%d %s\n",session->GetUser(),session->GetHostName(),
      session->GetPort(),session->GetCwd());
}

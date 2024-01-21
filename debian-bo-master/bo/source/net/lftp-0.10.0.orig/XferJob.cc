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
#include <errno.h>
#include <unistd.h>
#include <assert.h>

#include "xmalloc.h"

#include "XferJob.h"
#include "rglob.h"

int   GetJob::Done()
{
   return todo==0 && filter_wait==0 && global==0;
}

void  GetJob::ShowRunStatus(StatusLine *s)
{
   if(!print_run_status)
      return;

   if(Done())
   {
      s->Show("");
      return;
   }

   if(todo)
   {
      int w=s->GetWidth()-10;
      char *n=todo->local->name;
      if((int)strlen(n)>w)
	 n=n+strlen(n)-w;
      s->Show("`%s' at %lu [%s]",n,offset,session->CurrentStatus());
   }
}

void  GetJob::SayFinal()
{
   if(file_count==failed || !print_run_status)
      return;
   if(failed>0)
      printf("Transfer of %d of %d files failed\n",failed,file_count);
   else if(file_count>1)
      printf("%d files tranferred\n",file_count);
   if(end_time>start_time)
      printf("Average transfer rate %g bytes/s\n",(float)bytes_transferred/(end_time-start_time));
}

void  GetJob::PrintStatus(int verbose)
{
   SessionJob::PrintStatus(verbose);
   if(Done())
   {
      if(file_count==0)
	 return;
      if(failed==file_count)
      {
	 printf("\tNo files transferred successfully :(\n");
	 return;
      }
      if(failed>0)
	 printf("\tTransfer of %d of %d files failed\n",failed,file_count);
      else if(file_count>1)
	 printf("\t%d files tranferred\n",file_count);
      if(end_time>start_time)
	 printf("\tAverage transfer rate %g bytes/s\n",(float)bytes_transferred/(end_time-start_time));
      return;
   }
   if(todo==0)
   {
      printf("\tWaiting for filter to terminate\n");
      return;
   }
   printf("\t`%s' at %ld [%s]\n",todo->local->name,offset,session->CurrentStatus());
}

int strip_cr(char *buffer,int size)
{
   for(int i=0; i<size; i++)
   {
      if(buffer[i]=='\r')
      {
	 memmove(buffer+i,buffer+i+1,size-i-1);
	 i--;
	 size--;
      }
   }
   return size;
}

int   GetJob::Do()
{
   int m=STALL;
   int fd;

   if(filter_wait)
   {
      if(filter_wait->Done())
      {
	 delete filter_wait;
	 filter_wait=0;
   	 m=MOVED;
      }
      else
	 return m;
   }

   // check if we have smth to do
   if(todo==0)
   {
      if(global)
      {
	 filter_wait=global;
	 global=0;
	 m=MOVED;
      }
      return m;
   }

   // now deal with next todo

   // if we don't have mtime of the file, get it
   if(file_time==(time_t)-2)
   {
      if(session->IsClosed())
      {
	 m=MOVED;
	 GetBetter();
	 session->Open(todo->remote_name,Ftp::DATE);
      }
      char buf[256];
      int res=session->Read(buf,sizeof(buf));
      if(res==Ftp::SEE_ERRNO &&
	    (errno==EINTR || errno==EAGAIN || errno==EMFILE || errno==ENFILE))
      	 return m;
      m=MOVED;
      if(res<0)
	 file_time=(time_t)-1;
      else
	 file_time=session->ConvertFtpDate(buf);
      session->Close();
   }

   // now we can get to data...
   if(in_buffer==0 && flags&GOT_EOF)
   {
      if(file_time>=0)
	 todo->local->setmtime(file_time);
      NextToDo();
      m=MOVED;
      return m;
   }
   if(!(flags&GOT_EOF))
   {
      if(session->IsClosed())
      {
	 if((fd=todo->local->getfd())==-1)
	    goto handle_local_error;
	 if(flags&CONTINUE)
	 {
	    offset=todo->local->getsize_and_seek_end();
	    if(offset<0)
	       offset=0;
	 }
	 m=MOVED;
	 GetBetter();
	 session->Open(todo->remote_name,
	       flags&LIST?Ftp::LONG_LIST:Ftp::RETRIEVE,offset);
      }
      if(in_buffer==buffer_size)
      {
	 session->Suspend();
	 goto try_write;
      }
      int res=session->Read(buffer+in_buffer,buffer_size-in_buffer);
      if(res==Ftp::SEE_ERRNO &&
	    (errno==EINTR || errno==EAGAIN || errno==EMFILE || errno==ENFILE))
	 return m;
      m=MOVED;
      if(res<0)
      {
	 fprintf(stderr,"%s\n",session->StrError(res));
	 failed++;
	 todo->local->remove_if_empty();
	 NextToDo();
      }
      if(res==0)
      {
	 // EOF
	 flags|=GOT_EOF;
	 session->Close();
	 return m;
      }
      if(flags&LIST)
	 res=strip_cr(buffer+in_buffer,res);
      in_buffer+=res;
      bytes_transferred+=res;
      offset+=res;
   }

try_write:
   if(in_buffer>0)
   {
      // try to write the buffer contents
      fd=todo->local->getfd();
      if(fd==-1)
      {
      handle_local_error:
	 if(!todo->local->error())
	 {
	    block+=TimeOut(1000);
	    return m;
	 }
	 fprintf(stderr,"%s\n",todo->local->error_text);
	 if(todo->local==global)
	 {
	    while(todo)
	    {
	       failed++;
	       NextToDo();
	    }
	 }
	 else
	 {
	    failed++;
	    NextToDo();
	 }
	 m=MOVED;
	 return m;
      }
      struct pollfd pfd={fd,POLLOUT};
      int res=poll(&pfd,1,0);
      if(res==1 && pfd.revents&(POLLOUT|POLLNVAL))
      {
      	 res=write(fd,buffer,in_buffer);
	 if(res==-1)
	 {
	    perror(todo->local->name);
	    goto broken_pipe;
	 }
	 if(res==0)
	 {
	    fprintf(stderr,"cannot write -- disk full?\n");
	    goto broken_pipe;
	 }
	 in_buffer-=res;
	 memmove(buffer,buffer+res,in_buffer);
	 if(res!=0)
	    session->Resume();
	 block+=PollVec(fd,POLLOUT);
      }
      else if(res==1 && pfd.revents&POLLHUP)
      {
      broken_pipe:
	 failed++;
	 NextToDo();
      }
   }
   return m;
}

void GetJob::NextToDo()
{
   if(!todo)
      return;
   if(todo->local!=global)
      filter_wait=todo->local;
   GetRec *tmp=todo->next;
   delete todo;
   todo=tmp;
   session->Close();
   flags&=~GOT_EOF;

   file_time=(time_t)(flags&NO_TIME?-1:-2);
   remote_size=-1;

   file_count++;
   offset=0;
   if(!todo)
      time(&end_time);
   remote_size=-1;
}

GetJob::~GetJob()
{
   while(todo)
      NextToDo();

   if(global)
      delete global;
};

void  GetJob::AddToDo(GetRec *r)
{
   if(r->local->usesfd(1))
      print_run_status=false;
   append=&(*append=r)->next;
}

GetJob::GetJob(Ftp *new_session,FDStream *new_global,ArgV *args) : FtpJob(new_session)
{
   append=&todo;

   global=new_global;
   in_buffer=0;
   file_time=(time_t)(flags&NO_TIME?-1:-2);
   remote_size=-1;
   flags=0;
   file_count=0;
   failed=0;
   todo=0;
   filter_wait=0;
   offset=0;
   print_run_status=true;

   time(&start_time);
   end_time=start_time;
   bytes_transferred=0;

   char *op=args->getarg(0);

   if(!strcmp(op,"mget") || !strcmp(op,"mput"))
      return;

   if(!strcmp(op,"ls"))
   {
      flags|=LIST;
      char *ls_arg=args->Combine(1);
      AddToDo(new GetRec(ls_arg,global?:new FDStream(1,"<stdout>")));
      free(ls_arg);
      file_time=-1;
      if(todo->local->usesfd(1))
	 print_run_status=false;
      return;
   }

   if(!strcmp(op,"reget"))
      flags|=CONTINUE;
   else if(!strcmp(op,"put"))
      flags|=PUT;
   else if(!strcmp(op,"cat") || !strcmp(op,"zcat"))
   {
      if(!global)
	 global=new FDStream(1,"<stdout>");
   }
   else if(!strcmp(op,"more") || !strcmp(op,"less")
	|| !strcmp(op,"zmore"))
   {
      if(!global)
      {
	 char *pager=getenv("PAGER");
	 if(pager==NULL)
	    pager="more";
	 global=new OutputFilter(pager);
      }
   }
   char *for_each=0;
   if(!strcmp(op,"zcat") || !strcmp(op,"zmore"))
      for_each="zcat";

   args->rewind();
   int opt;
   while((opt=args->getopt("+c"))!=EOF)
   {
      switch(opt)
      {
      case('c'):
	 flags|=CONTINUE;
	 break;
      case('?'):
      print_usage:
	 {
	    char *local="local";
	    if(flags&PUT)
	       local="remote";
	    printf("Usage: %s [-c] file [-o %s] ...\n",op,local);
	 }
	 return;
      }
   }

   args->back();
   for(;;)
   {
      char *remote=args->getnext();
      if(remote==0)
	 break;
      char *opt=args->getnext();
      char *local=0;
      FDStream *local_stream=0;
      if(flags&PUT)
	 local_stream=new FileStream(remote,O_RDONLY);
      if(opt && !strcmp(opt,"-o"))
      {
	 local=args->getnext();
	 if(local==0)
	 {
	    fprintf(stderr,"Warning: -o requires parameter, option ignored\n");
	    break;
	 }
	 if(!local_stream)
	 {
	    if(!strcmp(local,"-"))
	    {
	       // to stdout
	       local_stream=new FDStream(1,"<stdout>");
	    }
	    else
	    {
	       local_stream=new FileStream(local,O_WRONLY|O_CREAT
		  |(flags&CONTINUE?0:O_TRUNC));
	    }
      	 }
      }
      else /* ! -o */
      {
	 if(opt)
	    args->back();
	 if(for_each && !local_stream)
	    local_stream=new OutputFilter(for_each,global);
	 else if(global && !local_stream)
	    local_stream=global;
	 else
	 {
	    local=strrchr(remote,'/');
	    if(local)
	       local++;
	    else
	       local=remote;
	    if(!local_stream)
	       local_stream=new FileStream(local,O_WRONLY|O_CREAT|
					     (flags&CONTINUE?0:O_TRUNC));
	 }
      }
      if(flags&PUT)
	 AddToDo(new GetRec(local,local_stream));
      else
	 AddToDo(new GetRec(remote,local_stream));
   }
   if(todo==0)
      goto print_usage;
}

int   PutJob::Do()
{
   int m=STALL;
   int fd;
   int res;

   if(filter_wait)
   {
      if(filter_wait->Done())
      {
	 delete filter_wait;
	 filter_wait=0;
   	 m=MOVED;
      }
      else
	 return m;
   }

   // check if we have smth to do
   if(todo==0)
      return m;

   // now deal with next todo
   if(flags&CONTINUE && remote_size==-1 && !session->GetFlag(Ftp::NOREST_MODE))
   {
      if(session->IsClosed())
      {
	 m=MOVED;
	 GetBetter();
	 session->Open(todo->remote_name,Ftp::SIZE);
      }
      char buf[256];
      int res=session->Read(buf,sizeof(buf));
      if(res==Ftp::SEE_ERRNO &&
	    (errno==EINTR || errno==EAGAIN || errno==EMFILE || errno==ENFILE))
      	 return m;
      m=MOVED;
      if(res<0)
	 remote_size=0;
      else
	 remote_size=atol(buf);
      session->Close();
   }

   // now we can get to data...
   if(in_buffer==0 && flags&GOT_EOF)
   {
      res=session->StoreStatus();
remote_error:
      if(res==Ftp::SEE_ERRNO &&
	    (errno==EINTR || errno==EAGAIN || errno==EMFILE || errno==ENFILE))
	 return m;
      if(res==Ftp::IN_PROGRESS)
	 return m;
      m=MOVED;
      if(res==Ftp::STORE_FAILED)
      {
	 // try to restart store
	 in_buffer=0;	// flush buffer
	 remote_size=-1;
	 session->Close();
	 return m;
      }
      if(res!=Ftp::OK)
      {
	 fprintf(stderr,"%s\n",session->StrError(res));
	 failed++;
	 NextToDo();
      }
      NextToDo();
      m=MOVED;
      return m;
   }
   if(!(flags&GOT_EOF))
   {
      if(in_buffer==buffer_size)
	 goto try_write;

      fd=todo->local->getfd();
      if(fd==-1)
      {
	 if(!todo->local->error())
	 {
	    block+=TimeOut(1000);
	    return m;
	 }
	 fprintf(stderr,"%s\n",todo->local->error_text);
	 failed++;
	 NextToDo();
	 m=MOVED;
	 return m;
      }
      if(remote_size>=0)
      {
	 assert(in_buffer==0);
	 res=lseek(fd,remote_size,SEEK_SET);
	 if(res==-1)
	 {
	    perror(todo->local->name);
	    failed++;
	    NextToDo();
	    return MOVED;
	 }
	 offset=remote_size;
	 remote_size=-2;   // to not set it again
      }
      block+=PollVec(fd,POLLIN);
      struct pollfd pfd={fd,POLLIN};
      int res=poll(&pfd,1,0);
      if(res==1 && pfd.revents&(POLLIN|POLLNVAL))
      {
	 res=read(fd,buffer+in_buffer,buffer_size-in_buffer);
	 if(res==-1)
	 {
	    perror(todo->local->name);
	    failed++;
	    NextToDo();
	    return MOVED;
	 }
	 if(res==0)
	 {
	    // EOF
	    flags|=GOT_EOF;
	 }
	 in_buffer+=res;
      }
   }

try_write:
   if(session->IsClosed())
   {
      m=MOVED;
      GetBetter();
      session->Open(todo->remote_name,Ftp::STORE,offset);
   }
   if(in_buffer==0)
   {
      if(flags&GOT_EOF)
	 session->SendEOT();
      else
	 session->Suspend();
      return MOVED;
   }
   res=session->Write(buffer,in_buffer);
   if(res==Ftp::SEE_ERRNO &&
	 (errno==EINTR || errno==EAGAIN || errno==EMFILE || errno==ENFILE))
      return m;
   m=MOVED;
   if(res<0)
      goto remote_error;

   in_buffer-=res;
   if(in_buffer==0 && flags&GOT_EOF)
      session->SendEOT();
   else
      memmove(buffer,buffer+res,in_buffer);

   bytes_transferred+=res;
   offset+=res;

   return m;
}

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
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>
#include <utime.h>
#include <sys/stat.h>

#include "Filter.h"
#include "xmalloc.h"

FDStream::FDStream(int new_fd,const char *new_name)
{
   fd=new_fd;
   name=xstrdup(new_name);
   error_text=0;
}
FDStream::FDStream()
{
   fd=-1;
   name=0;
   error_text=0;
}
void FDStream::MakeErrorText()
{
   if(errno==ENFILE || errno==EMFILE || errno==EAGAIN || errno==EWOULDBLOCK)
      return;  // not a serious error - can be retried
   char *syserr=strerror(errno);
   error_text=(char*)xmalloc(strlen(name)+strlen(syserr)+3);
   sprintf(error_text,"%s: %s",name,syserr);
}
FDStream::~FDStream()
{
   // don't close fd
   xfree(name);
   xfree(error_text);
};

long FDStream::getsize_and_seek_end()
{
   int fd=getfd();
   if(fd==-1)
      return -1;
   long size=lseek(fd,0,SEEK_END);
   if(size<0)
      size=0;
   return size;
}

int OutputFilter::getfd()
{
   if(fd!=-1 || error())
      return fd;

   if(second && second_fd==-1)
   {
      second_fd=second->getfd();
      if(second_fd==-1)
      {
	 if(second->error())
	    error_text=xstrdup("chain output error");
	 return -1;
      }
   }

   int	 p[2];
   char  s[256];
   pid_t pid;

   if(pipe(p)==-1)
   {
      if(errno==EMFILE || errno==ENFILE)
	 return -1;
      sprintf(s,"pipe() failed: %s",strerror(errno));
      error_text=xstrdup(s);
      return -1;
   }

   fflush(stderr);
   switch(pid=fork())
   {
   case(0): /* child */
      close(p[1]);
      dup2(p[0],0);
      close(p[0]);
      if(second_fd!=-1)
      {
	 dup2(second_fd,1);
	 close(second_fd);
	 int fl;
	 fcntl(1,F_GETFL,&fl);
	 fcntl(1,F_SETFL,fl&~O_NONBLOCK);
      }
      if(oldcwd)
	 chdir(oldcwd);
      execl("/bin/sh","sh","-c",name,NULL);
      fprintf(stderr,"execl(/bin/sh) failed: %s\n",strerror(errno));
      fflush(stderr);
      _exit(1);
   case(-1): /* error */
      close(p[0]);
      close(p[1]);
      return -1;
   }
   /* parent */
   close(p[0]);
   fd=p[1];

   fcntl(fd,F_SETFD,FD_CLOEXEC);
   fcntl(fd,F_SETFL,O_NONBLOCK);

   free(oldcwd); oldcwd=0;

   w=new ProcWait(pid);

   return fd;
}

void OutputFilter::Init()
{
   w=0;
   second=0;
   second_fd=-1;
   oldcwd=(char *)xmalloc(1024);
   if(getcwd(oldcwd,1024)==0)
   {
      free(oldcwd);
      oldcwd=0;
   }
}

OutputFilter::OutputFilter(const char *filter,int new_second_fd)
   : FDStream(-1,filter)
{
   Init();
   second_fd=new_second_fd;
}

OutputFilter::OutputFilter(const char *filter,FDStream *new_second)
   : FDStream(-1,filter)
{
   Init();
   second=new_second;
}

OutputFilter::~OutputFilter()
{
   close(fd);
   fd=-1;

   if(w)
      w->SetFlag(w->AUTODESTROY,1);

   free(oldcwd);
}

bool OutputFilter::Done()
{
   if(w==0)
      return true;
   if(fd!=-1)
   {
      close(fd);
      fd=-1;
   }
   w->Do();
   if(w->GetState()!=w->RUNNING)
      return true;
   return false;
}

void FileStream::setmtime(time_t t)
{
   struct utimbuf ut;
   ut.actime=ut.modtime=t;
   utime(full_name,&ut);
}
FileStream::FileStream(const char *fname,int new_mode) : FDStream(-1,fname)
{
   mode=new_mode;
   full_name=0;
   if(name[0]=='/')
      full_name=name;
   else
   {
      char cwd[1024];
      if(getcwd(cwd,sizeof(cwd)))
      {
	 full_name=(char*)xmalloc(strlen(cwd)+1+strlen(name)+1);
	 sprintf(full_name,"%s/%s",cwd,name);
      }
   }
}
FileStream::~FileStream()
{
   if(full_name!=name)
      free(full_name);
   close(fd);
   fd=-1;
}
void FileStream::remove_if_empty()
{
   if(!full_name)
      return;
   struct stat st;
   int res=stat(full_name,&st);
   if(res!=-1 && st.st_size==0)
      remove(full_name);
}

int   FileStream::getfd()
{
   if(fd!=-1 || error())
      return fd;
   fd=open(full_name,mode|O_NONBLOCK,0644);
   if(fd==-1)
   {
      MakeErrorText();
      return -1;
   }
   fcntl(fd,F_SETFD,FD_CLOEXEC);
   return fd;
}

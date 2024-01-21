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

#include <unistd.h>
#include <errno.h>

#include "SysCmdJob.h"
#include "xmalloc.h"

SysCmdJob::SysCmdJob(const char *c)
{
   w=0;
   cmd=xstrdup(c);
}

SysCmdJob::~SysCmdJob()
{
   if(w)
   {
      w->SetFlag(w->AUTODESTROY,1);
      w=0;
   }
}

int SysCmdJob::Do()
{
   if(w)
      return STALL;

   pid_t pid;
   fflush(stderr);
   switch(pid=fork())
   {
   case(0): /* child */
      execl("/bin/sh","sh","-c",cmd,NULL);
      fprintf(stderr,"execl(/bin/sh) failed: %s\n",strerror(errno));
      fflush(stderr);
      _exit(1);
   case(-1): /* error */
      block+=TimeOut(1000);   // wait a second
      return STALL;
   }
   /* parent */
   w=new ProcWait(pid);
   return MOVED;
}

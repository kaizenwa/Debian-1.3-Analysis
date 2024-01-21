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
#include <stdio.h>
#include "mvJob.h"

mvJob::mvJob(Ftp *session,const char *from,const char *to) : SessionJob(session)
{
   failed=0;
   GetBetter();
   session->Rename(from,to);
}

int mvJob::Do()
{
   if(Done())
      return STALL;

   int res=session->Done();
   if(res==Ftp::IN_PROGRESS)
      return STALL;
   if(res==Ftp::OK)
   {
      session->Close();
      return MOVED;
   }
   if(res==Ftp::SEE_ERRNO && (errno==EAGAIN || errno==EINTR || errno==EMFILE
      || errno==ENFILE))
   {
      block+=TimeOut(1000);
      return STALL;
   }
   fprintf(stderr,"%s\n",session->StrError(res));
   failed=1;
   session->Close();
   return MOVED;
}

void  mvJob::PrintStatus(int v)
{
   SessionJob::PrintStatus(v);
   if(Done())
      return;
   printf("\t[%s]\n",session->CurrentStatus());
}

void  mvJob::ShowRunStatus(StatusLine *s)
{
   if(Done())
      s->Show("");
   else
      s->Show("[%s]",session->CurrentStatus());
}

void  mvJob::SayFinal()
{
   if(failed)
      return;
   printf("rename successful\n");
}

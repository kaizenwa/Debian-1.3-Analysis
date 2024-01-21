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
#include <unistd.h>
#include <string.h>
#include "SiteJob.h"

int   SiteJob::Done()
{
   return session->IsClosed() && in_buf==0;
}

int   SiteJob::Do()
{
   if(Done())
      return STALL;

   int res;
   if(in_buf>0)
   {
      // try write
      int fd=output->getfd();
      if(fd==-1)
      {
	 if(output->error())
	 {
	    fprintf(stderr,"%s\n",output->error_text);
	 error:
	    session->Close();
	    in_buf=0;
	    delete output;
	    output=0;
	    return MOVED;
	 }
      }
      res=write(fd,buf,in_buf);
      if(res==0)
	 goto error;
      if(res==-1)
      {
	 if(errno!=EAGAIN)
	 {
	    perror(output->name);
	    goto error;
	 }
      }
      else
      {
	 memmove(buf,buf+res,in_buf-=res);
      	 return MOVED;
      }
   }

   if(in_buf==sizeof(buf))
   {
      session->Suspend();
      return STALL;
   }

   // try read
   res=session->Read(buf+in_buf,sizeof(buf)-in_buf);
   if(res==Ftp::SEE_ERRNO &&
	 (errno==EINTR || errno==EAGAIN || errno==EMFILE || errno==ENFILE))
      return STALL;
   if(res<0)
   {
      fprintf(stderr,"%s\n",session->StrError(res));
      output->remove_if_empty();
      goto error;
   }
   if(res==0)
   {
      // EOF
      session->Close();
      return MOVED;
   }
   in_buf+=res;
   return MOVED;
}

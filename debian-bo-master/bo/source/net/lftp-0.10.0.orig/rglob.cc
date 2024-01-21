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

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>

#include "xalloca.h"
#include "ftpclass.h"
#include "rglob.h"
#include "xmalloc.h"

static
int   count_slashes(const char *name)
{
   int n=0;
   for( ; *name; name++)
      n+=(*name=='/');
   return n;
}

RemoteGlob::RemoteGlob(Ftp *session,char *n_pattern,Ftp::open_mode n_mode)
{
   list=0;
   list_size=0;
   list_alloc=0;
   pattern=n_pattern;
   mode=n_mode;
   f=session;
   inbuf=0;
   flags=0;
   extra_slashes=0;
   f->Open(pattern,mode);
}
RemoteGlob::~RemoteGlob()
{
   for(int i=0; i<list_size; i++)
      free(list[i]);
   free(list);
   if(f)
      f->Close();
}

int   RemoteGlob::Do()
{
   int	 num_of_slashes=count_slashes(pattern);
   int	 res;
   char	 *nl;

   if(flags&ALL_DONE)
   {
      block+=NoWait();
      return STALL;
   }

   res=f->Read(buf+inbuf,sizeof(buf)-inbuf);
   if((res==Ftp::SEE_ERRNO && errno!=EAGAIN && errno!=EINTR)
   || (res<0 && res!=Ftp::SEE_ERRNO))
   {
      if(res!=Ftp::NO_FILE)
	 fprintf(stderr,"glob: %s\n",f->StrError(res));
      flags|=ALL_DONE;
      return MOVED;
   }
   if(res<0)
      return STALL;

   if(res==0)
   {
      // EOF
      f->Close();
      f=0;
      flags|=ALL_DONE;
      return MOVED;
   }
   inbuf+=res;

   while((nl=(char*)memchr(buf,'\n',inbuf)))
   {
      *nl=0;
      if(nl[-1]=='\r')
	 nl[-1]=0;
      if((flags&RESTRICT_SLASHES) && count_slashes(buf)>num_of_slashes+extra_slashes)
      	 goto next;
      // insert new file name into list
      if(list_size>=list_alloc-1)
      {
	 if(list_alloc==0)
	    list_alloc=32;
	 list=(char**)xrealloc(list,(list_alloc*=2)*sizeof(*list));
      }
      list[list_size++]=xstrdup(buf);
      list[list_size]=0;
   next:
      memmove(buf,nl+1,inbuf-=(nl-buf+1));
   }
   return MOVED;
}

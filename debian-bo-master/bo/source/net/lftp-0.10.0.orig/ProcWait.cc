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
#include <sys/wait.h>
#include <errno.h>
#include "ProcWait.h"

int ProcWait::Do()
{
   int m=STALL;
   if(status!=RUNNING)
   {
   final:
      if(flags&AUTODESTROY)
	 return WANTDIE;
      block+=NoWait();
      return m;
   }

   int info;
   int res=waitpid(pid,&info,WNOHANG);
   if(res==-1)
   {
      saved_errno=errno;
      status=ERROR;
      m=MOVED;
      goto final;
   }
   if(res==pid)
   {
      status=TERMINATED;
      term_info=info;
      m=MOVED;
      goto final;
   }
   block+=TimeOut(200);
   return m;
}

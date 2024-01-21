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

#ifndef PROCWAIT_H
#define PROCWAIT_H

#include <sys/types.h>
#include "SMTask.h"

class ProcWait : public SMTask
{
public:
   enum	State
   {
      TERMINATED,
      RUNNING,
      ERROR
   };
   enum
   {
      AUTODESTROY=1
   };

private:
   pid_t pid;
   State status;
   int	 term_info;
   int	 saved_errno;
   int	 flags;

public:
   int	 Do();
   int	 GetState() { return status; }
   int	 GetErrno() { return saved_errno; }
   int	 GetInfo() { return term_info; }

   void	 SetFlag(int flag,int val)
   {
      if(val)
	 flags|=flag;
      else
	 flags&=~flag;
   }

   void Auto() { flags|=AUTODESTROY; }

   ProcWait(pid_t p)
   {
      flags=0;
      pid=p;
      status=RUNNING;
      term_info=-1;
      saved_errno=0;
   }
};

#endif /* PROCWAIT_H */

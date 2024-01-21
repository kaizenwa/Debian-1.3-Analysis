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

#ifndef XFERJOB_H
#define XFERJOB_H

#include "Job.h"
#include "ProcWait.h"
#include "Filter.h"
#include "ArgV.h"
#include "xmalloc.h"

class GetRec
{
   friend class GetJob;
   friend class PutJob;

   char *remote_name;
   FDStream *local;

   GetRec *next;
public:
   GetRec(char *rem,FDStream *loc,GetRec *n=0)
   {
      remote_name=xstrdup(rem);
      local=loc;
      next=n;
   }
   ~GetRec()
   {
      free(remote_name);
      remote_name=0;
   }
};

class GetJob : public FtpJob
{
protected:
   FDStream *global;
   FDStream *filter_wait;
   bool	 print_run_status;

   GetRec **append;

   GetRec *todo;
   void	 NextToDo();

   static const int buffer_size=0x1000;
   char	 buffer[buffer_size];
   int	 in_buffer;

   time_t   file_time;
   long	 remote_size;

   int	 file_count;
   int	 failed;

   time_t   start_time;
   time_t   end_time;

   long	    offset;

   long	    bytes_transferred;

   int	 flags;
   enum flag_mask
   {
      GOT_EOF=1,
      CONTINUE=2,
      LIST=4,
      NO_TIME=8,
      PUT=16,
      LAST_MASK=PUT
   };

   void AddToDo(GetRec *);

public:
   int	 Do();
   int	 Done();
   int	 ExitCode() { return failed; }

   void DontSetTime() { flags|=NO_TIME; }
   void SetTime(time_t t) { file_time=t; }

   void	 ShowRunStatus(StatusLine *);
   void	 PrintStatus(int);
   void	 SayFinal();

   GetJob(Ftp *s,FDStream *global,ArgV *args);
   virtual ~GetJob();
};

class PutJob : public GetJob
{
public:
   PutJob(Ftp *s,ArgV *args) : GetJob(s,0,args) {}

   int	 Do();
};

#endif /* XFERJOB_H */

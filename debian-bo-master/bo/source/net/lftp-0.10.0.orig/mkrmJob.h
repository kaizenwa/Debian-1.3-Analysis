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

#ifndef MKRMJOB_H
#define MKRMJOB_H

#include "Job.h"
#include "StatusLine.h"
#include "ArgV.h"
#include <stdio.h>

class mkrmJob : public SessionJob
{
   ArgV	 *args;
   char	 *curr,*first;
   Ftp::open_mode mode;
   int	 failed,file_count;
   bool	 quiet;

public:
   int	 Do();
   int	 Done() { return curr==0; }
   int	 ExitCode() { return failed; }

   void	 PrintStatus(int);
   void	 ShowRunStatus(StatusLine *);
   void	 SayFinal();

   mkrmJob(Ftp *session,ArgV *a);
   ~mkrmJob();

   void	 BeQuiet() { quiet=true; }
};

#endif  MKRMJOB_H

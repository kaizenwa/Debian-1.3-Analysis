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

#ifndef JOB_H
#define JOB_H

#include "SMTask.h"
#include "ftpclass.h"
#include "StatusLine.h"

class Job : public SMTask
{
   void SortJobs();

   Job	 *next;

   static Job *chain;

protected:
   void SetParent(Job *j) { parent=j; }

public:
   int	 jobno;
   Job	 *parent;
   Job	 *waiting;

   void	 AllocJobno();

   virtual void	  PrintStatus(int) {};
   virtual void	  ShowRunStatus(StatusLine *) {}
   virtual void	  SayFinal() {}; // final phrase of fg job
   virtual int	  Done()=0;
   virtual int	  ExitCode() { return 0; }
   virtual int	  Do()=0;

   char	 *cmdline;
   void ListJobs(int verbose_level,int indent=0);
   void ListDoneJobs();
   void BuryDoneJobs();

   static int NumberOfJobs();
   static Job *FindJob(int n);
   static bool Running(int n)
   {
      Job *j=FindJob(n);
      return j && !j->Done();
   }
   void Kill(int n);
   void KillAll();

   Job();
   virtual ~Job();
};

class SessionPool
{
   static const pool_size=64;
   static Ftp *pool[pool_size];

public:
   static Ftp *GetBetter(Ftp *);
   static void Reuse(Ftp *);
};

class SessionJob : public Job
{
   static Ftp *GetBetter(Ftp *s) { return SessionPool::GetBetter(s); }
   static void Reuse(Ftp *s) { SessionPool::Reuse(s); }

protected:
   Ftp *Clone();

   void	 GetBetter() { session=GetBetter(session); }

   SessionJob(Ftp *);
   ~SessionJob();

public:
   void PrintStatus(int v);
   Ftp *session;
};

class FtpJob : public SessionJob
{
public:
   FtpJob(Ftp *s) : SessionJob(s) {}
};

#endif /* JOB_H */

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

#ifndef POLLVEC_H
#define POLLVEC_H

#include <poll.h>

class SMTask;

class Waiting
{
public:
   enum	Type { POLLFD,TIMEOUT,ASYNCWAIT,NOWAIT };

private:
   friend class PollVec;
   Type	    wait_type;
   struct pollfd pfd;
   int	    timeout;
   Waiting  *next;

   Waiting  *DupChain();
};

class PollVec
{
   Waiting  *chain;
public:
   PollVec(int fd,int events);
   PollVec(int timeout);
   PollVec(SMTask *);
   PollVec(Waiting::Type);
   PollVec() { chain=0; }

   void	 Empty();
   void	 Merge(const PollVec&);
   void	 Block() const;
   void	 operator+=(const PollVec& p) { Merge(p); }

   ~PollVec() { Empty(); }
};

class TimeOut : public PollVec
{
public:
   TimeOut(int t) : PollVec(t) {}
};

class NoWait : public PollVec
{
public:
   NoWait() : PollVec(Waiting::NOWAIT) {}
};

#endif /* POLLVEC_H */

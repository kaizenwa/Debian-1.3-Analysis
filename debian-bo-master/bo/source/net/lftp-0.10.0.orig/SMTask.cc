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
#include <assert.h>
#include "SMTask.h"

SMTask	 *SMTask::chain=0;
SMTask	 *SMTask::sched_scan=0;
PollVec	 SMTask::sched_total;

SMTask::SMTask()
{
   // insert in the chain
   if(sched_scan)
   {
      // insert it so that it would be scanned next
      next=sched_scan->next;
      sched_scan->next=this;
   }
   else
   {
      next=chain;
      chain=this;
   }
   suspended=false;
   running=false;
}

void  SMTask::Suspend() { suspended=1; }
void  SMTask::Resume()  { suspended=0; }

SMTask::~SMTask()
{
   // remove from the chain
   SMTask **scan=&chain;
   while(*scan)
   {
      if(*scan==this)
      {
	 assert(this!=sched_scan);

	 *scan=next;
	 break;
      }
      scan=&((*scan)->next);
   }
}

void SMTask::Schedule()
{
   bool repeat=false;
   sched_scan=chain;
   while(sched_scan)
   {
      if(sched_scan->running)
      {
	 sched_scan=sched_scan->next;
	 continue;
      }
      sched_scan->block.Empty();
      SMTask *tmp=sched_scan;
      tmp->running=true;
      int res=tmp->Do();
      tmp->running=false;
      if(sched_scan)
	 sched_scan=sched_scan->next;
      if(res==WANTDIE)
	 delete tmp;
      else if(res==MOVED)
	 repeat=true;
   }
   sched_total.Empty();
   if(repeat)
   {
      sched_total+=NoWait();
      return;
   }

   for(sched_scan=chain; sched_scan; sched_scan=sched_scan->next)
      if(sched_scan->suspended==0)
	 sched_total.Merge(sched_scan->block);
   return;
}

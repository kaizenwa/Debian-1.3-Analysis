------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . T A S K _ T I M E R                    --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.3 $                             --
--                                                                          --
--   Copyright (C) 1991,1992,1993,1994,1995,1996 Florida State University   --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Alpha/VMS version.

with Ada.Calendar;
--  used for Time

with Ada.Calendar.Conv;
--  use for To_Duration

with Ada.Real_Time;
--  used for Time
--           Time_Span
--           To_Duration

with Ada.Real_Time.Delays;
--  used for To_Duration

with System.Tasking;
--  used for Task_ID

with System.Task_Primitives.Operations;
--  used for Sleep_Until
--           Write_Lock
--           Unlock
--           Self

with System.Time_Operations;
--  used for Clock;

with System.Tasking.Utilities;
--  Used for Make_Independent

with Unchecked_Conversion;

package body System.Task_Timer is

   function Clock return Duration
     renames System.Time_Operations.Clock;

   Max_Clock_Val : constant Duration := Clock + Max_Sensible_Delay;
   --  This constant represent the infinite time so that delaying
   --  until this time means sleeping forever (We do not expect
   --  one to use a delay of more than Max_Sensible_Delay).

   Timer_Server_ID : Tasking.Task_ID;

   Timer_Attention : Boolean := False;
   pragma Atomic (Timer_Attention);

   -------------------
   -- Signal_Object --
   -------------------

   protected body Signal_Object is

      entry Wait when Open is
      begin
         Open := False;
      end Wait;

      procedure Signal is
      begin
         Open := True;
      end Signal;

   end Signal_Object;

   task Timer_Server is
      pragma Interrupt_Priority (System.Any_Priority'Last);
   end Timer_Server;

   Q_Head : Q_Link := null;

   -----------
   -- Timer --
   -----------

   protected body Timer is

      ------------------------
      -- Timer.Time_Enqueue --
      ------------------------

      --  Allocate a queue element for the wakeup time T and put it in the
      --  queue in wakeup time order. Return the allocated queue element
      --  in N.

      procedure Time_Enqueue
        (T : Duration;
         D : access Delay_Block)
      is
         Q_Ptr : Q_Link := Q_Head;
         N     : Q_Link renames D;

      begin
         N.T := T;

         --  If the new element becomes head of the queue, notify Timer Service

         if Q_Head = null then
            N.Next := null;
            N.Previous := null;
            Q_Head := N;
            --  Signal the timer server to wake up
            Task_Primitives.Operations.Write_Lock (Timer_Server_ID);
            Timer_Attention := True;
            Task_Primitives.Operations.Wakeup (Timer_Server_ID);
            Task_Primitives.Operations.Unlock (Timer_Server_ID);

         elsif N.T < Q_Head.T then
            N.Next := Q_Head;
            N.Previous := null;
            Q_Head.Previous := N;
            Q_Head := N;
            --  Signal the timer server to wake up
            Task_Primitives.Operations.Write_Lock (Timer_Server_ID);
            Timer_Attention := True;
            Task_Primitives.Operations.Wakeup (Timer_Server_ID);
            Task_Primitives.Operations.Unlock (Timer_Server_ID);

         else
            --  Place in the middle

            while Q_Ptr.Next /= null loop
               if Q_Ptr.Next.T >= N.T then
                  N.Next := Q_Ptr.Next;
                  N.Previous := Q_Ptr;
                  Q_Ptr.Next.Previous := N;
                  Q_Ptr.Next := N;
                  exit;
               end if;
               Q_Ptr := Q_Ptr.Next;
            end loop;

            if Q_Ptr.Next = null then
               --  Place at the end
               N.Next := null;
               N.Previous := Q_Ptr;
               Q_Ptr.Next := N;
            end if;
         end if;
      end Time_Enqueue;

      -------------------
      -- Timer.Service --
      -------------------

      --  Service all of the wakeup requests on the queue whose wakeup time
      --  is less than the current time. Return the next wakeup time
      --  after that (the wakeup time of the head of the queue if any;
      --  a time far in the future if not).

      procedure Service (T : out Duration) is
         Q_Ptr : Q_Link := Q_Head;
         Now   : constant Duration := Clock;

      begin
         Timer_Attention := False;
         while Q_Ptr /= null and then Q_Ptr.T < Now loop
            --  Wake up the waiting task
            Q_Ptr.S_O.Signal;
            Dequeue (Q_Ptr);
            --  Remove the entry
            Q_Ptr := Q_Ptr.Next;
         end loop;
         if Q_Head = null then
            T := Max_Clock_Val;
         else
            T := Q_Head.T;
         end if;
      end Service;

      -------------
      -- Dequeue --
      -------------

      procedure Dequeue (D : access Delay_Block) is
         Q_Ptr : Q_Link renames D;

      begin
         --  Case of head entry
         if Q_Head = Q_Ptr then
            Q_Head := Q_Ptr.Next;
            if Q_Head /= null then
               Q_Head.Previous := null;
            end if;
         --  Case of tail entry
         elsif Q_Ptr.Next = null then
            if Q_Ptr.Previous /= null then
               Q_Ptr.Previous.Next := null;
            end if;
         else
            Q_Ptr.Previous.Next := Q_Ptr.Next;
            Q_Ptr.Next.Previous := Q_Ptr.Previous;
         end if;
         Q_Ptr.Next := null;
         Q_Ptr.Previous := null;
      end Dequeue;

      -----------------
      -- Timer.Empty --
      -----------------

      function Empty return Boolean is
      begin
         return Q_Head = null;
      end Empty;


      --  ??? The following entries used to all be called Enqueue; the
      --      compiler does not seem to be able to handle overloading
      --      in requeue statements.

      -----------------------
      -- Enqueue_Time_Span --
      -----------------------

      entry Enqueue_Time_Span
        (T : in Ada.Real_Time.Time_Span;
         D : access Delay_Block)
      when True is
         N : Q_Link renames D;
      begin
         Time_Enqueue (Clock + Duration'Min
           (Ada.Real_Time.To_Duration (T), Max_Sensible_Delay), D);
         requeue N.S_O.Wait with abort;
      end Enqueue_Time_Span;

      entry Enqueue_Duration
         (T : in Duration;
          D : access Delay_Block)
      when True is
         N : Q_Link renames D;
      begin
         Time_Enqueue
           (Clock + Duration'Min (T, Max_Sensible_Delay), D);
         requeue N.S_O.Wait with abort;
      end Enqueue_Duration;

      entry Enqueue_Real_Time
        (T : in Ada.Real_Time.Time;
         D : access Delay_Block)
      when True is
         N : Q_Link renames D;
      begin
         Time_Enqueue (Ada.Real_Time.Delays.To_Duration (T), D);
         requeue N.S_O.Wait with abort;
      end Enqueue_Real_Time;

      entry Enqueue_Calendar_Time
        (T : in Ada.Calendar.Time;
         D : access Delay_Block)
      when True is
         N : Q_Link renames D;
      begin
         Time_Enqueue (Ada.Calendar.Conv.To_Absolute_Duration (T), D);
         requeue N.S_O.Wait with abort;
      end Enqueue_Calendar_Time;

   end Timer;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Delay_Block) is
   begin
      Timer.Dequeue (Object'Access);
   end Finalize;

   ------------------
   -- Timer_Server --
   ------------------

   task body Timer_Server is
      Next_Wakeup_Time : Duration := Max_Clock_Val;

   begin
      Timer_Server_ID := Task_Primitives.Operations.Self;

      System.Tasking.Utilities.Make_Independent;

      loop
         Task_Primitives.Operations.Write_Lock (Timer_Server_ID);
         if not Timer_Attention then
            Task_Primitives.Operations.Sleep_Until
              (Timer_Server_ID, Next_Wakeup_Time);
         end if;
         Task_Primitives.Operations.Unlock (Timer_Server_ID);

         Timer.Service (Next_Wakeup_Time);
         --  Timer.Service returns the Next_Wakeup_Time.
         --  The Next_Wakeup_Time is either an infinity (no delay request)
         --  or the wakeup time of the queue head. This value is used for
         --  an actual delay in this server.

      end loop;
   end Timer_Server;

end System.Task_Timer;

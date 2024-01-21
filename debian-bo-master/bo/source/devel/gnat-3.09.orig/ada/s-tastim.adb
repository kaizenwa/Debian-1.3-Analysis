------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . T A S K _ T I M E R                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.13 $                            --
--                  (version not enabling use of nanosleep)                 --
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

--  Note: there are two versions of this package, one enabling use of
--  nanosleep, and one that does not enable the use of nanosleep. The
--  file name for the nanosleep version is 9ntastim.adb. The file name
--  for the non-nanosleep version is s-tastim.adb.

with Ada.Calendar.Conv;
--  Used for, Time_To_Stimespec

with System.Compiler_Exceptions;
--  Used for, Current_Exception

with Ada.Real_Time.Conv;
--  Used for, Time_Span_To_Stimespec
--            Time_To_Stimespec

with System.Task_Primitives;
--  Used for, Condition_Variable
--            Lock, Unlock
--            Write_Lock
--            Cond_Signal
--            Initialize_Lock
--            Initialize_Cond
--            Cond_Timed_wait

with System.Tasking.Utilities;
--  Used for, Make_Independent

with System.Task_Clock;

with System.Task_Clock.Machine_Specifics;
--  Used for, Machine_Specifics.Clock
--             Stimespec_Ticks;

with System.Tasking.Protected_Objects;

with System.Tasking;

with Unchecked_Conversion;

with Unchecked_Deallocation;

package body System.Task_Timer is

   -------------------
   -- Signal_Object --
   -------------------

   use System.Tasking.Protected_Objects;
   use System.Tasking;

   use System.Task_Clock;
   --  Included use clause for operators

   function Clock return Stimespec
     renames System.Task_Clock.Machine_Specifics.Clock;

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

   Timer_Condition :  Task_Primitives.Condition_Variable;
   Timer_Lock      :  Task_Primitives.Lock;

   Stimespec_Day : constant Stimespec := System.Task_Clock.Time_Of (86400, 0);
   Stimespec_Large : Stimespec := Clock + Stimespec_Day;
   --  This value is used to make Timer.Server to sleep until some entry
   --  comes into the timer queue.

   function To_Access is new
     Unchecked_Conversion (System.Address, Protection_Access);

   Q_Head : Q_Link := null;

   -----------
   -- Timer --
   -----------

   protected body Timer is

      ------------------------
      -- Timer.Time_nqueue --
      ------------------------

      --  Allocate a queue element for the wakeup time T and put it in the
      --  queue in wakeup time order. Return the allocated queue element
      --  in N.

      procedure Time_Enqueue
        (T : in System.Task_Clock.Stimespec;
         D : access Delay_Block)
      is
         Q_Ptr : Q_Link := Q_Head;
         Error : Boolean;
         N     : Q_Link renames D;

      begin
         N.T := T;

         --  If the new element becomes head of the queue, notify Timer Service

         if Q_Head = null then
            N.Next := null;
            N.Previous := null;
            Q_Head := N;
            Task_Primitives.Write_Lock (Timer_Lock, Error);
            Task_Primitives.Cond_Signal (Timer_Condition);

            --  Signal the timer server to wake up

            Task_Primitives.Unlock (Timer_Lock);

         elsif N.T < Q_Head.T then
            N.Next := Q_Head;
            N.Previous := null;
            Q_Head.Previous := N;
            Q_Head := N;
            Task_Primitives.Write_Lock (Timer_Lock, Error);
            Task_Primitives.Cond_Signal (Timer_Condition);

            --  Signal the timer server to wake up

            Task_Primitives.Unlock (Timer_Lock);

         else
            --  Place in the middle

            while Q_Ptr.Next /= null loop
               if Q_Ptr.Next.T > N.T then
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

      --  Service all of the wakeup requeues on the queue whose wakeup time
      --  is less than the current time. Return the next wakeup time
      --  after that (the wakeup time of the head of the queue if any;
      --  a time far in the future if not).

      procedure Service (T : out System.Task_Clock.Stimespec) is
         Q_Ptr : Q_Link := Q_Head;

      begin
         while Q_Ptr /= null loop

            if Q_Ptr.T < Clock then

               --  Wake up the waiting task

               Q_Ptr.S_O.Signal;

               Dequeue (Q_Ptr);
               --  Remove the entry

            end if;

            Q_Ptr := Q_Ptr.Next;
         end loop;

         if Q_Head = null then
            T := Stimespec_Large;
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

      --  For the following Enqueue_XXX entries we add
      --  Task_Clock.Machine_Specifics.Stimespec_Ticks to Time value before
      --  queuing it onto the timer queue. This is need to guaranteed at
      --  least the requested amount of waiting regradless of the Clock
      --  granularity mismatch between the system's clock and the clock
      --  used in Task_Clock.Machine_Specific.Clock.

      -----------------------
      -- Enqueue_Time_Span --
      -----------------------

      entry Enqueue_Time_Span
        (T : in Ada.Real_Time.Time_Span;
         D : access Delay_Block)
      when True is
         N : Q_Link renames D;

      begin
         Time_Enqueue (Clock + Duration_To_Stimespec
           (Duration'Min (Stimespec_To_Duration
             (Ada.Real_Time.Conv.Time_Span_To_Stimespec (T)),
              Max_Sensible_Delay)) +
            Task_Clock.Machine_Specifics.Stimespec_Ticks, D);
         requeue N.S_O.Wait with abort;
      end Enqueue_Time_Span;

      entry Enqueue_Duration
         (T : in Duration;
         D : access Delay_Block)
      when True is
         N : Q_Link renames D;

      begin
         Time_Enqueue (Clock + System.Task_Clock.Duration_To_Stimespec
           (Duration'Min (T, Max_Sensible_Delay)) +
           Task_Clock.Machine_Specifics.Stimespec_Ticks, D);
         requeue N.S_O.Wait with abort;
      end Enqueue_Duration;

      entry Enqueue_Real_Time
        (T : in Ada.Real_Time.Time;
         D : access Delay_Block)
      when True is
         N : Q_Link renames D;

      begin
         Time_Enqueue (Ada.Real_Time.Conv.Time_To_Stimespec (T) +
           Task_Clock.Machine_Specifics.Stimespec_Ticks, D);
         requeue N.S_O.Wait with abort;
      end Enqueue_Real_Time;

      entry Enqueue_Calendar_Time
        (T : in Ada.Calendar.Time;
         D : access Delay_Block)
      when True is
         N : Q_Link renames D;

      begin
         Time_Enqueue (Ada.Calendar.Conv.Time_To_Stimespec (T) +
           Task_Clock.Machine_Specifics.Stimespec_Ticks, D);
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

   -------------------
   -- Timer_Service --
   -------------------

   Next_Wakeup_Time : System.Task_Clock.Stimespec := Stimespec_Large;

   procedure Temp_Init;
   procedure Temp_Wait;
   --  These procedures contain processing that should be local to
   --  Timer_Server---GNAT workaround. ???

   procedure Temp_Init is
   begin
      Tasking.Utilities.Make_Independent;
      Task_Primitives.Initialize_Lock (System.Priority'Last, Timer_Lock);
      Task_Primitives.Initialize_Cond (Timer_Condition);
   end Temp_Init;
   procedure Temp_Wait is
      Result           : Boolean;
      Error            : Boolean;
   begin
      Task_Primitives.Write_Lock (Timer_Lock, Error);
      Task_Primitives.Cond_Timed_Wait
        (Timer_Condition, Timer_Lock, Next_Wakeup_Time, Result);
      Task_Primitives.Unlock (Timer_Lock);
   end Temp_Wait;

   task Timer_Server is
      pragma Priority (System.Priority'Last);
   end Timer_Server;

   task body Timer_Server is
   begin
      Temp_Init;
      loop
         Temp_Wait;
         if Timer.Empty and then Next_Wakeup_Time < Clock then
         --  In the case where current time passes Stimespec_Large
            Stimespec_Large := Stimespec_Large + Stimespec_Day;
            Next_Wakeup_Time := Stimespec_Large;
         else
            Timer.Service (Next_Wakeup_Time);
         end if;
      end loop;
   end Timer_Server;

end System.Task_Timer;

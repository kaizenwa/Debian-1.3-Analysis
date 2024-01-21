------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T A S K _ P R I M I T I V E S .            --
--                           O P E R A T I O N S                            --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.1 $                            --
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

--  .... This is a preliminary draft.
--  Lines containing .... contain notes about details
--  that still need attention. -- Ted Baker

--  This is an OS/2 version of this package.

with Interfaces.C;
--  used for size_t

with Interfaces.OS2Lib.Threads;
with Interfaces.OS2Lib.Synchronization;
with Interfaces.OS2Lib.Errors;

with System.Parameters;
--  used for Size_Type

with System.Tasking;
--  used for Task_ID

with System.Error_Reporting;
--  used for Shutdown

with System.Parameters;
--  used for Size_Type

with System.Storage_Elements; use System.Storage_Elements;

with Unchecked_Conversion;
with Unchecked_Deallocation;

package body System.Task_Primitives.Operations is

   package IC renames Interfaces.C;
   use Interfaces.OS2Lib;
   use Interfaces.OS2Lib.Errors;
   use Interfaces.OS2Lib.Threads;
   use Interfaces.OS2Lib.Synchronization;
   use System.Tasking;
   use System.Error_Reporting;

   ------------------
   --  Local Types --
   ------------------

   type Microseconds is new IC.long;

   subtype Fractional_Second is Microseconds range
     0 .. 10#1#E6; --  less than or equal to one second

   type time_t is new IC.int;

   type struct_timeval is record
      tv_sec : time_t;
      tv_usec : Fractional_Second;
   end record;

   type timezone is record
      tz_minuteswest : IC.int;   -- of GMT
      tz_dsttime     : IC.int;   -- type of dst correction to apply
   end record;

   ------------------
   --  Local Data  --
   ------------------

   Clock_Delay_Correction : constant := 0.0;
   --  to allow for early wakeup due to truncation of time values
   --  .... set this to an appropriate value for OS/2

   --  The OS/2 DosAllocThreadLocalMemory API is used to allocate our TCB_Ptr.

   --  This API reserves a small range of virtual addresses that is backed
   --  by different physical memory for each running thread. In this case we
   --  create a pointer at a fixed address that points to the TCB_Ptr for the
   --  running thread. So all threads will be able to query and update their
   --  own TCB_Ptr without destroying the TCB_Ptr of other threads.

   type Thread_Local_Data is record
      Self_ID : Task_ID; --  ID of the current thread
      --  ... room for expansion here, if we decide to make access to
      --  jump-buffer and exception stack more efficient in future
   end record;
   type Access_Thread_Local_Data is access all Thread_Local_Data;
   Thread_Local_Data_Ptr : aliased Access_Thread_Local_Data;
   --  Pointer to Thread Local Data

   type PPTLD is access all Access_Thread_Local_Data;
   type PPVOID is access all PVOID;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_PPVOID is new Unchecked_Conversion (PPTLD, PPVOID);
   function To_Address is new Unchecked_Conversion (Task_ID, System.Address);
   function To_PFNTHREAD is
     new Unchecked_Conversion (System.Address, PFNTHREAD);

   function To_Duration (TV : struct_timeval) return Duration;
   function To_MS (D : Duration) return IC.int;
   function To_MS (D : Duration) return ULONG;
   function To_Timeval (D : Duration) return struct_timeval;

   --  OS/2 has the gettimeofday() function in the EMX library

   function gettimeofday
     (tp : access struct_timeval;
      tzp : access timezone)
      return IC.int;
   pragma Import (C, gettimeofday);

   function To_Duration (TV : struct_timeval) return Duration is
   begin
      return Duration (TV.tv_sec) + Duration (TV.tv_usec) / 10#1#E6;
   end To_Duration;

   function To_Timeval (D : Duration) return struct_timeval is
      S : time_t;
      F : Duration;
   begin
      S := time_t (D);
      F := D - Duration (S);

      --  If F has negative value due to a round-up, adjust for positive F
      --  value.
      if F < 0.0 then S := S - 1; F := F + 1.0; end if;
      return struct_timeval' (tv_sec => S,
        tv_usec => Microseconds (F * 10#1#E6));
   end To_Timeval;

   function To_MS (D : Duration) return ULONG is
   begin
      return ULONG (D * 10#1#E3);
   end To_MS;

   function To_MS (D : Duration) return IC.int is
   begin
      return IC.int (D * 10#1#E3);
   end To_MS;

   -------------------
   -- Abort_Handler --
   -------------------

   --  Apparently, OS/2 does not support per-thread asynchronous signals,
   --  so we have no support for ATC or asynchronous task abort.

   ----------
   -- Self --
   ----------

   function Self return Task_ID is
   begin
      return Thread_Local_Data_Ptr.Self_ID;
   end Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : access Lock)
   is
   begin
      if DosCreateMutexSem
        (ICS.Null_Ptr, L.Mutex'Unchecked_Access, 0, False32) /= NO_ERROR then
         raise Storage_Error;
      end if;
      L.Priority := Prio;
   end Initialize_Lock;

   procedure Initialize_Lock (L : access RTS_Lock) is
   begin
      if DosCreateMutexSem
        (ICS.Null_Ptr, L.Mutex'Unchecked_Access, 0, False32) /= NO_ERROR then
         raise Storage_Error;
      end if;
      --  Is this the right priority, or do we want System.Priority'Last??????
      L.Priority := System.Any_Priority'Last;
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : access Lock) is
   begin
      Must_Not_Fail (DosCloseMutexSem (L.Mutex));
   end Finalize_Lock;

   procedure Finalize_Lock (L : access RTS_Lock) is
   begin
      Must_Not_Fail (DosCloseMutexSem (L.Mutex));
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
      Self_ID : constant Task_ID := Thread_Local_Data_Ptr.Self_ID;
   begin
      L.Owner_Priority := Self_ID.LL.Active_Priority;
      if L.Priority < L.Owner_Priority then
         Ceiling_Violation := True;
         return;
      end if;
      Must_Not_Fail (DosRequestMutexSem (L.Mutex, SEM_INDEFINITE_WAIT));
      Ceiling_Violation := False;
      if L.Priority > L.Owner_Priority then
         Self_ID.LL.Active_Priority := L.Priority;
      end if;
   end Write_Lock;

   procedure Write_Lock (L : access RTS_Lock) is
      Self_ID : constant Task_ID := Thread_Local_Data_Ptr.Self_ID;
   begin
      L.Owner_Priority := Self_ID.LL.Active_Priority;
      Must_Not_Fail (DosRequestMutexSem (L.Mutex, SEM_INDEFINITE_WAIT));
      if L.Priority > L.Owner_Priority then
         Set_Priority (Self_ID, L.Priority);
      end if;
   end Write_Lock;

   procedure Write_Lock (T : Task_ID) is
   begin
      T.LL.L.Owner_Priority := T.LL.Active_Priority;
      Must_Not_Fail (DosRequestMutexSem (T.LL.L.Mutex, SEM_INDEFINITE_WAIT));
      if T.LL.L.Priority > T.LL.L.Owner_Priority then
         Set_Priority (T, T.LL.L.Priority);
      end if;
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock (L : access Lock; Ceiling_Violation : out Boolean)
      renames Write_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : access Lock) is
      Self_ID : constant Task_ID := Thread_Local_Data_Ptr.Self_ID;
   begin
      if L.Owner_Priority /= L.Priority then
         Set_Priority (Self_ID, L.Owner_Priority);
      end if;
      Must_Not_Fail (DosReleaseMutexSem (L.Mutex));
   end Unlock;

   procedure Unlock (L : access RTS_Lock) is
      Self_ID : constant Task_ID := Thread_Local_Data_Ptr.Self_ID;
   begin
      if L.Owner_Priority /= L.Priority then
         Set_Priority (Self_ID, L.Owner_Priority);
      end if;
      Must_Not_Fail (DosReleaseMutexSem (L.Mutex));
   end Unlock;

   procedure Unlock (T : Task_ID) is
   begin
      if T.LL.L.Owner_Priority /= T.LL.L.Priority then
         T.LL.Active_Priority := T.LL.L.Owner_Priority;
      end if;
      Must_Not_Fail (DosReleaseMutexSem (T.LL.L.Mutex));
   end Unlock;

   -----------
   -- Sleep --
   -----------

   procedure Sleep (Self_ID : Task_ID) is
      Count : aliased ULONG; -- Unused
   begin
      --  Must reset Cond BEFORE L is unlocked.
      Must_Not_Fail (DosResetEventSem (Self_ID.LL.CV, Count'Unchecked_Access));
      Unlock (Self_ID);
      --  No problem if we are interrupted here.
      --  If the condition is signaled, DosWaitEventSem will simply not block.
      Must_Not_Fail (DosWaitEventSem (Self_ID.LL.CV, SEM_INDEFINITE_WAIT));
      --  Since L was previously accquired, lock operation should not fail.
      Write_Lock (Self_ID);
   end Sleep;

   ---------------
   -- Sleep_For --
   ---------------

   procedure Sleep_For (Self_ID : Task_ID; Rel_Time : Duration) is
      Count : aliased ULONG; -- Unused
   begin
      --  Must reset Cond BEFORE L is unlocked.
      Must_Not_Fail (DosResetEventSem (Self_ID.LL.CV, Count'Unchecked_Access));
      Unlock (Self_ID);
      if Rel_Time > 0.0 then
         Must_Not_Fail (DosWaitEventSem (Self_ID.LL.CV,
--  .... check all L and CV ref's to see whether we need 'access.
           To_MS (Rel_Time + Clock_Delay_Correction)));
         --  No problem if we are interrupted here.
         --  If the condition is signaled,
         --  DosWaitEventSem will simply not block.
         --  .... How about early return?  If that is possible, we need to
         --  loop, possibly checking the clock.
         --  .... How about overflow on To_MS? Can it happen?
      else
         --  Must reset Cond BEFORE L is unlocked.
         Must_Not_Fail (DosResetEventSem
           (Self_ID.LL.CV, Count'Unchecked_Access));
         Unlock (Self_ID);
         --  No problem if we are interrupted here.
         --  If the condition is signaled,
         --  DosWaitEventSem will simply not block.
         Must_Not_Fail (DosWaitEventSem
           (Self_ID.LL.CV, SEM_INDEFINITE_WAIT));
         --  Since L was previously accquired,
         --  lock operation should not fail.
      end if;
      Write_Lock (Self_ID);
   end Sleep_For;

   -----------------
   -- Sleep_Until --
   -----------------

   procedure Sleep_Until (Self_ID : Task_ID; Abs_Time : Duration) is
      Rel_Time : Duration;
   begin
      --  Change Abs_time to a relative delay.
      --  Be careful not to reintroduce the race condition that gave birth
      --  to delay until.
      Must_Not_Fail (DosEnterCritSec);
      Rel_Time := Abs_Time - Clock + Clock_Delay_Correction;
      Must_Not_Fail (DosExitCritSec);
      Sleep_For (Self_ID, Rel_Time);
   end Sleep_Until;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_ID) is
   begin
      Must_Not_Fail (DosPostEventSem (T.LL.CV));
   end Wakeup;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (T : Task_ID; Prio : System.Any_Priority) is
   begin
      Must_Not_Fail
        (DosSetPriority (Scope   => PRTYS_THREAD,
                         Class   => PRTYC_NOCHANGE,
                         Delta_P => IC.long (Prio - T.LL.Active_Priority),
                         PorTid  => T.LL.Thread));
      T.LL.Active_Priority := Prio;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_ID) return System.Any_Priority is
   begin
      return System.Any_Priority (T.LL.Active_Priority);
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_ID) is
   begin
      Thread_Local_Data_Ptr.Self_ID := Self_ID;
      --  For OS/2, we can set Self_ID.LL.Thread in
      --  Create_Task, since the thread is created suspended.
      --  That is, there is no danger of the thread racing ahead
      --  and trying to reference Self_ID.LL.Thread before it
      --  has been initialized.
      --  .... Do we need to do anything with signals for OS/2?
   end Enter_Task;

   ----------------------
   --  Initialize_TCB  --
   ----------------------

   procedure Initialize_TCB (Self_ID : Task_ID; Succeeded : out Boolean) is
   begin
      Unlock (Self_ID);
      if DosCreateMutexSem
        (ICS.Null_Ptr, Self_ID.LL.L.Mutex'Unchecked_Access, 0, False32)
          = NO_ERROR then
         --  Is this the right priority,
         --  or do we want System.Priority'Last??????
         Self_ID.LL.L.Priority := System.Any_Priority'Last;
         if DosCreateEventSem (ICS.Null_Ptr,
              Self_ID.LL.CV'Unchecked_Access, 0, True32)
           /= NO_ERROR then
            Must_Not_Fail (DosCloseMutexSem (Self_ID.LL.L.Mutex));
            Succeeded := False;
         else Succeeded := True;
         end if;
      else Succeeded := False;
      end if;
      --  assumes any failure must be due to insufficient resources
   end Initialize_TCB;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (T          : Task_ID;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean)
   is
      Result  : APIRET;
      Success : Boolean;
      Adjusted_Stack_Size : IC.size_t;
      use System.Parameters;
   begin
      if Stack_Size = Unspecified_Size then
         Adjusted_Stack_Size :=
           IC.size_t (2 * Default_Stack_Size);
      --  .... If OS/@ has a way to make a thread's stack size extensible,
      --  arrange to make the stack extensible.
      --  if Stack_Size = Unspecified_Size then
      --  .... Round up the stack size, if it is below the minimum size
      --  required for OS/2 threads.
      else
         if Stack_Size < Parameters.Minimum_Stack_Size then
            Adjusted_Stack_Size :=
              IC.size_t (Stack_Size + Minimum_Stack_Size);
         else
            Adjusted_Stack_Size := IC.size_t (Stack_Size);
         end if;
      end if;

      --  .... Add some head-space to the stack size, if there is a known
      --  amount needed by OS/2.

      Initialize_TCB (T, Success);
      if Success then
         --  create the thread, in blocked mode
         Result := DosCreateThread
                   (F_ptid   => T.LL.Thread'Unchecked_Access,
                    pfn      => To_PFNTHREAD (Wrapper),
                    param    => To_Address (T),
                    flag     => 1, -- Block_child + No_commit_stack,
                    cbStack  => ULONG (Adjusted_Stack_Size));
         Succeeded := Result /= NO_ERROR;
         --  set the new thread's priority
         --  (child has inherited priority from parent)
         Must_Not_Fail (DosSetPriority
           (Scope   => PRTYS_THREAD,
            Class   => PRTYC_NOCHANGE,
            Delta_P => IC.long (Priority - Get_Priority (Self)),
            PorTid  => T.LL.Thread));
         --  start the thread executing
         Must_Not_Fail (DosResumeThread (T.LL.Thread));
      else Succeeded := False;
      end if;
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_ID) is
   begin
      Must_Not_Fail (DosCloseMutexSem (T.LL.L.Mutex));
      Must_Not_Fail (DosCloseEventSem (T.LL.CV));
      --  Do not deallocate TCB here.
      --  GNARL layer is responsible for that.
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   procedure Exit_Task is
   begin
      DosExit (EXIT_THREAD, 0);
      --  Do not finalize TCB here.
      --  GNARL layer is responsible for that.
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_ID) is
   begin
      null;
      --  .... verify this
      --  OS/2 apparently has no per-thread signal capability,
      --  so we can't support ATC.
   end Abort_Task;

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
      TV     : aliased struct_timeval;
      Result : IC.int;
      use type IC.int;
   begin
      Result := gettimeofday (TV'Access, null);
      pragma Assert (Result = 0
        or else Shutdown ("GNULLI failure---gettimeofday"));
      return To_Duration (TV);
   exception
   when others =>
      pragma Assert (Shutdown ("exception in Clock"));
      return 0.0; --  to avert warning for no return
   end Clock;

   ---------------
   -- Delay_For --
   ---------------

   --  ???? We need some more thought here about the correct treatment of
   --  early wakeup due to a signal.  If the signal is for task abortion we
   --  need to check for it and proceed accordingly.
   --  This may require checking all the points where these are called.

   procedure Delay_For (Rel_Time : Duration) is
   begin
      if Rel_Time > 0.0 then
         DosSleep (To_MS (Rel_Time + Clock_Delay_Correction));
         --  .... Can this return early?
         --  If that is possible, we need to loop, checking the clock.
      else
         --  .... What is the correct OS/2 way to yield
         --  to equal-priority threads?
         DosSleep (0);
      end if;
   end Delay_For;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Abs_Time : Duration) is
      Rel_Time : Duration;
   begin
      --  Change Abs_time to a relative delay.
      --  Be careful not to reintroduce the race condition that gave birth
      --  to delay until.
      Must_Not_Fail (DosEnterCritSec);
      Rel_Time := Abs_Time - Clock + Clock_Delay_Correction;
      Must_Not_Fail (DosExitCritSec);
      Delay_For (Rel_Time);
   end Delay_Until;

   procedure Initialize (Environment_Task : Task_ID) is
      Succeeded : Boolean;
   begin
      --  Initialize pointer to task local data.
      --  This is done once, for all tasks.
      Must_Not_Fail (DosAllocThreadLocalMemory
        (1, To_PPVOID (Thread_Local_Data_Ptr'Access)));
      --  Set ID of environment task.
      Environment_Task.LL.Thread := 1; --  By definition
      --  Initialize TCB for this task.
      --  This includes all the normal task-external initialization.
      Initialize_TCB (Environment_Task, Succeeded);
      pragma Assert (Succeeded
        or else Shutdown ("GNULLI failure---Initialize_TCB"));
      --  Consider raising Storage_Error, if propagation can be tolerated????
      --  Do normal task-internal initialization,
      --  which depends on an initialized TCB.
      Enter_Task (Environment_Task);
      --  Insert here any other special
      --  initialization needed for the environment task.
   end Initialize;

--  Please, put no initialization code in the body!
--  All global initializations for this package belong
--  in procedure Initialize (above).

end System.Task_Primitives.Operations;

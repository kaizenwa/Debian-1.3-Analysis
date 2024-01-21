------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--              S Y S T E M . T A S K I N G . U T I L I T I E S             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.29 $                            --
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

--  This package provides RTS Internal Declarations.
--  These declarations are not part of the GNARLI

with System.Task_Primitives;  use System.Task_Primitives;

with System.Compiler_Exceptions;
--  Used for, Tasking_Error_Id

with System.Tasking.Abortion;
--  Used for, Undefer_Abortion,
--            Abort_To_Level

with System.Tasking.Queuing; use System.Tasking.Queuing;
--  Used for, Queuing.Dequeue_Head

with System.Tasking.Entry_Calls;
--  Used for, Lock_Server
--            Unlock_Server

with System.Tasking.Initialization;
--  Used for, Remove_From_All_Tasks_List
--            All_Tasks_L
--            All_Tasks_List

with System.Interrupts;
--  Used for, Detach_Interrupt_Entries

package body System.Tasking.Utilities is

   package Abortion renames System.Tasking.Abortion;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Make_Passive
     (T : Task_ID);
   --  Record that task T is passive.

   procedure Close_Entries (Target : Task_ID);
   --  Close entries, purge entry queues (called by Task_Stages.Complete)
   --  T.Stage must be Completing before this is called.

   ------------------------------------
   -- Vulnerable_Complete_Activation --
   ------------------------------------

   --  WARNING : Only call this procedure with abortion deferred.
   --  That's why the name has "Vulnerable" in it.

   procedure Vulnerable_Complete_Activation
     (T         : Task_ID;
      Completed : Boolean)
   is
      Activator : Task_ID;
      Error     : Boolean;

   begin
      Activator := T.Activator;

      --  Note: Activator should only be null for the environment task.

      if Activator /= Null_Task then

         --  Decrement the count of tasks to be activated by the
         --  activator and wake it up so it can check to see if
         --  all tasks have been activated. Note that the locks
         --  of the activator and created task are locked here.
         --  This is necessary because C.Stage and
         --  T.Activation_Count have to be synchronized. This is
         --  also done in Activate_Tasks and Init_Abortion. So
         --  long as the activator lock is always locked first,
         --  this cannot lead to deadlock.

         Write_Lock (Activator.L, Error);
         Write_Lock (T.L, Error);

         --  After the activation task priority should be the same as
         --  its base priority.

         T.Current_Priority := T.Base_Priority;
         Set_Own_Priority (T.Current_Priority);

         if T.Stage = Can_Activate then
            T.Stage := Active;
            Activator.Activation_Count := Activator.Activation_Count - 1;

            if Activator.Activation_Count = 0 then
               Cond_Signal (Activator.Cond);
            end if;

            --  The activator raises a Tasking_Error if any one of the task
            --  it is activating is completed before the activation is
            --  done. However, if the reason for the task completion is
            --  an abortion, we do not raise an exception. ARM (9.2(5)).

            if Completed and then not T.Aborting then
               Activator.Exception_To_Raise :=
                 System.Compiler_Exceptions.Tasking_Error_Id;
            end if;
         end if;

         Unlock (T.L);
         Unlock (Activator.L);
      end if;

   end Vulnerable_Complete_Activation;

   --  PO related routines

   ---------------------
   -- Check_Exception --
   ---------------------

   procedure Check_Exception  (T : Task_ID := Self) is
      use Ada.Exceptions;

      Ex : Exception_Id := T.Exception_To_Raise;

      procedure Internal_Raise (X : Exception_Id);
      pragma Import (C, Internal_Raise, "__gnat_raise_with_msg");

   begin
      if Ex /= Null_Id then
         T.Exception_To_Raise := Null_Id;
         Internal_Raise (Ex);
      end if;
   end Check_Exception;

   -------------------
   -- Close_Entries --
   -------------------

   procedure Close_Entries (Target : Task_ID) is
      T                : Task_ID := Target;
      Temp_Call        : Entry_Call_Link;
      Null_Call        : Entry_Call_Link := null;
      Temp_Caller      : Task_ID;
      Temp_Called_Task : Task_ID;
      Error            : Boolean;

   begin
      --  If there are Interrupt Entries attached, detach them.

      if Target.Interrupt_Entry then
         Interrupts.Detach_Interrupt_Entries (Target);
      end if;

      --  Purging pending callers that are in the middle of rendezvous

      Temp_Call := T.Call;

      while Temp_Call /= null loop
         Temp_Call.Exception_To_Raise :=
           System.Compiler_Exceptions.Tasking_Error_Id;

         Temp_Caller := Temp_Call.Self;

         --  All forms of accept make sure that the acceptor is not
         --  begin completed before accepting further calls, so that we
         --  can be sure that no further calls are made after the the
         --  current calls are purged.

         Write_Lock (Temp_Caller.L, Error);
         Temp_Call.Done := True;
         Unlock (Temp_Caller.L);

         --  Cancel the call.

         Abort_To_Level (Temp_Caller, Temp_Call.Level - 1);

         Temp_Call := Temp_Call.Acceptor_Prev_Call;
      end loop;

      --  Purging entry queues

      Write_Lock (T.L, Error);

      for J in 1 .. T.Entry_Num loop
         Dequeue_Head (T.Entry_Queues (J), Temp_Call);
         while Temp_Call /= Null_Call loop
            Temp_Caller := Temp_Call.Self;
            Temp_Call.Exception_To_Raise :=
              System.Compiler_Exceptions.Tasking_Error_Id;
            Abort_To_Level (Temp_Caller, Temp_Call.Level - 1);
            Dequeue_Head (T.Entry_Queues (J), Temp_Call);
         end loop;
      end loop;

      Unlock (T.L);

      --  If the target task is making any entry calls, then this
      --  is being called due to abortion.  In that case, the entry
      --  calls will either be cancelled as the abort signal is handled or,
      --  if the task is being aborted by an abort statement, they will be
      --  cancelled immediately in Complete_On_Sync_Point.

   end Close_Entries;

   ----------------------------
   -- Complete_On_Sync_Point --
   ----------------------------

   procedure Complete_on_Sync_Point (T : Task_ID) is
      Target    : Task_ID := T;
      Call      : Entry_Call_Link;
      Error     : Boolean;
      No_Server : Boolean;

   begin
      Write_Lock (Target.L, Error);

      --  If the target is waiting to accept an entry call, complete it.

      if Target.Accepting /= Not_Accepting then
         Unlock (Target.L);
         Complete (T);
      else
         Unlock (Target.L);
      end if;

      --  Abort all pending entry calls in LIFO order until a non-abortable
      --  one is found.

      for Level in reverse
        ATC_Level_Index'First .. Target.ATC_Nesting_Level
      loop
         Call := Target.Entry_Calls (Level)'Access;
         System.Tasking.Entry_Calls.Lock_Server (Call, No_Server);

         if not No_Server then
            if Call.Abortable then
               System.Tasking.Queuing.Dequeue_Call (Call);
               System.Tasking.Entry_Calls.Unlock_And_Update_Server (Call);
            else
               System.Tasking.Entry_Calls.Unlock_Server (Call);
               exit;
            end if;
         end if;
      end loop;

   end Complete_on_Sync_Point;

   --------------------
   -- Reset_Priority --
   --------------------

   procedure Reset_Priority
     (Acceptor_Prev_Priority : Rendezvous_Priority;
       Acceptor              : Task_ID)
   is
      Acceptor_ATCB : Task_ID := Acceptor;

   begin
      if Acceptor_Prev_Priority /= Priority_Not_Boosted then
         Acceptor_ATCB.Current_Priority := Acceptor_Prev_Priority;
         Set_Priority
           (Acceptor_ATCB.LL_TCB'Access, Acceptor_ATCB.Current_Priority);
      end if;
   end Reset_Priority;

   ---------------------------
   -- Terminate_Alternative --
   ---------------------------

   --  WARNING : Only call this procedure with abortion deferred. This
   --  procedure needs to have abortion deferred while it has the current
   --  task's lock locked. Since it is called from two procedures which
   --  also need abortion deferred, it is left controlled on entry to
   --  this procedure.

   procedure Terminate_Alternative is
      T     : Task_ID := Self;
      Error : Boolean;

   begin
      Make_Passive (T);

      --  Note that abortion is deferred here (see WARNING above)

      Write_Lock (T.L, Error);

      T.Terminate_Alternative := true;

      while T.Accepting /= Not_Accepting
        and then T.Stage /= Complete
        and then T.Pending_ATC_Level >= T.ATC_Nesting_Level
      loop
         Cond_Wait (T.Cond, T.L);
      end loop;

      if T.Stage = Complete then
         Unlock (T.L);

         if T.Pending_ATC_Level < T.ATC_Nesting_Level then
            Abortion.Undefer_Abortion;
            pragma Assert
              (Runtime_Assert_Shutdown ("Continuing after being aborted!"));
         end if;

         Abort_To_Level (T, 0);
         Abortion.Undefer_Abortion;
         pragma Assert
           (Runtime_Assert_Shutdown ("Continuing after being aborted!"));
      end if;

      T.Terminate_Alternative := False;

      Unlock (T.L);

   end Terminate_Alternative;

   --------------
   -- Complete --
   --------------

   procedure Complete (Target : Task_ID) is
      T      : Task_ID := Target;
      Caller : Task_ID := Self;
      Task1  : Task_ID;
      Task2  : Task_ID;
      Error  : Boolean;

   begin
      Make_Passive (T);
      Write_Lock (T.L, Error);

      if T.Stage < Completing then
         T.Stage := Completing;
         T.Accepting := Not_Accepting;
         T.Awaited_Dependent_Count := 0;
         Unlock (T.L);
         Close_Entries (T);
         T.Stage := Complete;

         --  Wake up all the pending calls on Aborter_Link list

         Task1 := T.Aborter_Link;
         T.Aborter_Link := Null_Task;

         while (Task1 /= Null_Task) loop
            Task2 := Task1;
            Task1 := Task1.Aborter_Link;
            Task2.Aborter_Link := Null_Task;
            Cond_Signal (Task2.Cond);
         end loop;

      else
         --  Some other task is completing this task. So just wait until
         --  the completion is done. A list of such waiting tasks is
         --  maintained by Aborter_Link in ATCB.

         while T.Stage < Complete loop
            if T.Aborter_Link /= Null_Task then
               Caller.Aborter_Link := T.Aborter_Link;
            end if;

            T.Aborter_Link := Caller;
            Cond_Wait (Caller.Cond, T.L);
         end loop;

         Unlock (T.L);
      end if;
   end Complete;

   --  Task_Stage related routines

   ----------------------
   -- Make_Independent --
   ----------------------

   procedure Make_Independent is
      T      : Task_ID := Self;
      P      : Task_ID;
      Result : Boolean;
      Error  : Boolean;

   begin
      Write_Lock (T.L, Error);
      P := T.Parent;
      Unlock (T.L);

      Write_Lock (P.L, Error);
      Write_Lock (T.L, Error);

      T.Master_of_Task := Master_ID (0);

      if P.Awake_Count > 1 then
         P.Awake_Count := P.Awake_Count - 1;
      end if;

      Unlock (T.L);
      Unlock (P.L);

      System.Tasking.Initialization.Remove_From_All_Tasks_List (T, Result);
      pragma Assert (
        Result or else Runtime_Assert_Shutdown (
          "Failed to delete an entry from All_Tasks_List"));

   end Make_Independent;

   --  Task Abortion related routines

   --------------------
   -- Abort_To_Level --
   --------------------

   procedure Abort_To_Level
     (Target : Task_ID;
      L      : ATC_Level)
   is
      T      : Task_ID := Target;
      Error  : Boolean;

   begin
      Write_Lock (T.L, Error);

      --  If the task is suspended on a condition variable, it will
      --  be in an abort-deferred region, and will not be awakened
      --  by abortion. Such an abort deferral is just to protect
      --  the low-level operations, and not to enforce Ada semantics.
      --  Wake the task up and let it decide if it wants to
      --  complete the aborted construct immediately.  This is done
      --  unconditionally, since a Cond_Signal is not persistent, and
      --  is needed even if the task has been aborted before.

      Cond_Signal (T.Cond);

      if T.Pending_ATC_Level > L then
         T.Pending_ATC_Level := L;
         T.Pending_Action := True;

         if not T.Aborting then
            T.Aborting := True;

            --  If this task is aborting itself, it should unlock itself
            --  before calling abort, as it is unlikely to have the
            --  opportunity to do so afterwords. On the other hand, if
            --  another task is being aborted, we want to make sure it is
            --  not terminated, since there is no need to abort a terminated
            --  task, and it may be illegal if it has stopped executing.
            --  In this case, the Abort_Task must take place under the
            --  protection of the mutex, so we know that Stage/=Terminated.

            if Target =  Self then
               Unlock (T.L);
               Abort_Task (T.LL_TCB'Access);
               return;

            elsif T.Stage /= Terminated then
               Abort_Task (T.LL_TCB'Access);
            end if;

         end if;
      end if;

      Unlock (T.L);

   end Abort_To_Level;

   -------------------
   -- Abort_Handler --
   -------------------

   procedure Abort_Handler
     (Context : Task_Primitives.Pre_Call_State)
   is
      T : Task_ID := Self;

   begin
      if T.Deferral_Level = 0
        and then T.Pending_ATC_Level < T.ATC_Nesting_Level
      then

         --  ???  This is implementation dependent.  Some implementations
         --       might not allow an exception to be propagated out of a
         --       handler, and others might leave the signal or interrupt
         --       that invoked this handler masked after the exceptional
         --       return to the application code.

         --       GNAT exceptions are originally implemented using
         --       setjmp()/longjmp().  On most UNIX systems, this will
         --       allow transfer out of a signal handler, which is
         --       usually the only mechanism available for implementing
         --       asynchronous handlers of this kind.  However, some
         --       systems do not restore the signal mask, leaving the
         --       abortion signal masked.
         --       Possible solutions:
         --
         --       1. Change the PC saved in the system-dependent Context
         --          parameter to point to code that raises the exception.
         --          Normal return from this handler will then raise
         --          the exception after the mask and other system state has
         --          been restored.
         --       2. Use siglongjmp()/sigsetjmp() to implement exceptions.
         --       3. Unmask the signal in the Abortion exception handler
         --          (in the RTS).

         raise Standard'Abort_Signal;

      end if;
   end Abort_Handler;

   ----------------------
   -- Abort_Dependents --
   ----------------------

   --  Process abortion of child tasks.

   --  Abortion should be dererred when calling this routine.
   --  No mutexes should be locked when calling this routine.

   procedure Abort_Dependents (Abortee : Task_ID) is
      Temp_T : Task_ID;
      Temp_P : Task_ID;
      A      : Task_ID := Abortee;
      Error  : Boolean;

   begin
      Write_Lock (System.Tasking.Initialization.All_Tasks_L, Error);
      Temp_T := System.Tasking.Initialization.All_Tasks_List;

      while Temp_T /= Null_Task loop
         Temp_P := Temp_T.Parent;

         while Temp_P /= Null_Task loop
            exit when Temp_P = A;
            Temp_P := Temp_P.Parent;
         end loop;

         if Temp_P = A then
            Temp_T.Accepting := Not_Accepting;

            --  Send cancel signal.
            Complete_on_Sync_Point (Temp_T);
            Abort_To_Level (Temp_T, 0);
         end if;

         Temp_T := Temp_T.All_Tasks_Link;
      end loop;

      Unlock (System.Tasking.Initialization.All_Tasks_L);

   end Abort_Dependents;

   ------------------
   -- Make_Passive --
   ------------------

   --  If T is the last dependent of some master in task P to become passive,
   --  then release P. A special case of this is when T has no dependents
   --  and is completed. In this case, T itself should be released.

   --  If the parent is made passive, this is repeated recursively, with C
   --  being the previous parent and P being the next parent up.

   --  Note that we have to hold the locks of both P and C (locked in that
   --  order) so that the Awake_Count of C and the Awaited_Dependent_Count of
   --  P will be synchronized. Otherwise, an attempt by P to terminate can
   --  preempt this routine after C's Awake_Count has been decremented to zero
   --  but before C has checked the Awaited_Dependent_Count of P. P would not
   --  count C in its Awaited_Dependent_Count since it is not awake, but it
   --  might count other awake dependents. When C gained control again, it
   --  would decrement P's Awaited_Dependent_Count to indicate that it is
   --  passive, even though it was never counted as active. This would cause
   --  P to wake up before all of its dependents are passive.

   --  Note : Any task with an interrupt entry should never become passive.
   --  Support for this feature needs to be added here.

   procedure Make_Passive (T : Task_ID) is
      P : Task_ID;
      --  Task whose Awaited_Dependent_Count may be decremented.

      C : Task_ID;
      --  Task whose awake-count gets decremented.

      H : Task_ID;
      --  Highest task that is ready to terminate dependents.

      Error     : Boolean;

   begin
      Utilities.Vulnerable_Complete_Activation (T, Completed => False);

      Write_Lock (T.L, Error);

      if T.Stage >= Passive then
         Unlock (T.L);
         return;
      else
         T.Stage := Passive;
         Unlock (T.L);
      end if;

      H := Null_Task;
      P := T.Parent;
      C := T;

      while C /= Null_Task loop

         if P /= Null_Task then
            Write_Lock (P.L, Error);
            Write_Lock (C.L, Error);

            C.Awake_Count := C.Awake_Count - 1;

            if C.Awake_Count /= 0 then

               --  C is not passive; we cannot make anything above this point
               --  passive.

               Unlock (C.L);
               Unlock (P.L);
               exit;
            end if;

            if P.Awaited_Dependent_Count /= 0 then

               --  We have hit a non-task master; we will not be able to make
               --  anything above this point passive.

               P.Awake_Count := P.Awake_Count - 1;

               if C.Master_of_Task = P.Master_Within then
                  P.Awaited_Dependent_Count := P.Awaited_Dependent_Count - 1;

                  if P.Awaited_Dependent_Count = 0 then
                     H := P;
                  end if;
               end if;

               Unlock (C.L);
               Unlock (P.L);
               exit;
            end if;

            if C.Stage = Complete then

               --  C is both passive (Awake_Count = 0) and complete; wake it
               --  up to await termination of its dependents. It will not be
               --  complete if it is waiting on a terminate alternative. Such
               --  a task is not ready to wait for its dependents to terminate,
               --  though one of its ancestors may be.

               H := C;
            end if;

            Unlock (C.L);
            Unlock (P.L);
            C := P;
            P := C.Parent;

         else
            Write_Lock (C.L, Error);
            C.Awake_Count := C.Awake_Count - 1;

            if C.Awake_Count /= 0 then

               --  C is not passive; we cannot make anything above
               --  this point passive.

               Unlock (C.L);
               exit;
            end if;

            if C.Stage = Complete then

               --  C is both passive (Awake_Count = 0) and complete; wake it
               --  up to await termination of its dependents. It will not be
               --  complete if it is waiting on a terminate alternative. Such
               --  a task is not ready to wait for its dependents to terminate,
               --  though one of its ancestors may be.

               H := C;
            end if;

            Unlock (C.L);
            C := P;
         end if;

      end loop;

      if H /= Null_Task then
         Cond_Signal (H.Cond);
      end if;

   end Make_Passive;

   -----------------------------
   -- Runtime_Assert_Shutdown --
   -----------------------------

   --  If this never returns, why is it a function???

   function Runtime_Assert_Shutdown (Msg : in String) return Boolean is
   begin
      LL_Assert (False, Msg);

      --  This call should never return

      return False;
   end Runtime_Assert_Shutdown;


end System.Tasking.Utilities;

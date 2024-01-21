------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--              S Y S T E M . T A S K I N G . U T I L I T I E S             --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.37 $                            --
--                                                                          --
--            Copyright (C) 1991-1997, Florida State University             --
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

with System.Error_Reporting;
--  used for Shutdown

with System.Interrupts;
--  Used for Detach_Interrupt_Entries

with System.Standard_Library;
--  used for Tasking_Error_Def;
--           Exception_Data

with System.Task_Primitives.Operations;
--  used for Write_Lock
--           Set_Priority
--           Wakeup
--           Unlock
--           Sleep
--           Abort_Task

with System.Tasking.Entry_Calls;
--  used for Lock_Server
--           Unlock_Server
--           Unlock_And_Update_Server

with System.Tasking.Initialization;
--  Used for Remove_From_All_Tasks_List
--           Defer_Abort
--           Undefer_Abort
--           All_Tasks_L
--           All_Tasks_List

with System.Tasking.Queuing;
--  used_for Dequeue_Call
--           Dequeue_Head

with Unchecked_Conversion;

package body System.Tasking.Utilities is

   use System.Task_Primitives.Operations;
   use System.Error_Reporting;

   type Exception_Data_Access
     is access all System.Standard_Library.Exception_Data;

   function To_Exception_ID is new
     Unchecked_Conversion (Exception_Data_Access, Ada.Exceptions.Exception_ID);

   procedure Defer_Abort (Self_ID : Task_ID) renames
     System.Tasking.Initialization.Defer_Abort;

   procedure Undefer_Abort (Self_ID : Task_ID) renames
     System.Tasking.Initialization.Undefer_Abort;

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

         Write_Lock (Activator);
         Write_Lock (T);

         if T.Stage = Can_Activate then

            T.Stage := Active;
            Activator.Activation_Count := Activator.Activation_Count - 1;

            if Activator.Activation_Count = 0 then
               Wakeup (Activator);
            end if;

            --  The activator raises a Tasking_Error if any one of the task
            --  it is activating is completed before the activation is
            --  done. However, if the reason for the task completion is
            --  an abortion, we do not raise an exception. ARM (9.2(5)).
            if Completed and then not T.Aborting then
               Activator.Exception_To_Raise :=
                 To_Exception_ID
                   (System.Standard_Library.Tasking_Error_Def'Access);
            end if;

         end if;
         Unlock (T);
         Unlock (Activator);

         Write_Lock (T);

         --  After the activation task priority should be the same as
         --  its base priority.

         if T.Stage = Active then
            Set_Priority (T, T.Base_Priority);
         end if;

         Unlock (T);

      end if;

   end Vulnerable_Complete_Activation;

   --  PO related routines

   ---------------------
   -- Check_Exception --
   ---------------------

   procedure Check_Exception (T : Task_ID) is
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

   begin

      --  Purging pending callers that are in the middle of rendezvous

      Temp_Call := T.Call;

      while Temp_Call /= null loop
         Temp_Call.Exception_To_Raise :=
           To_Exception_ID
             (System.Standard_Library.Tasking_Error_Def'Access);

         Temp_Caller := Temp_Call.Self;

         --  All forms of accept make sure that the acceptor is not
         --  begin completed before accepting further calls, so that we
         --  can be sure that no further calls are made after the the
         --  current calls are purged.

         Write_Lock (Temp_Caller);
         Temp_Call.Done := True;
         Unlock (Temp_Caller);

         --  Cancel the call.

         Abort_To_Level (Temp_Caller, Temp_Call.Level - 1);


         Temp_Call := Temp_Call.Acceptor_Prev_Call;
      end loop;

      --  Purging entry queues

      for J in 1 .. T.Entry_Num loop
         Queuing.Dequeue_Head (T.Entry_Queues (J), Temp_Call);
         while Temp_Call /= Null_Call loop
            Temp_Caller := Temp_Call.Self;
            Temp_Call.Exception_To_Raise :=
              To_Exception_ID
                (System.Standard_Library.Tasking_Error_Def'Access);
            Abort_To_Level (Temp_Caller, Temp_Call.Level - 1);
            Queuing.Dequeue_Head (T.Entry_Queues (J), Temp_Call);
         end loop;
      end loop;

      --  If the target task is making any entry calls, then this
      --  is being called due to abortion.  In that case, the entry
      --  calls will either be cancelled as the abort signal is handled or,
      --  if the task is being aborted by an abort statement, they will be
      --  cancelled immediately in Complete_On_Sync_Point.

   end Close_Entries;

   ----------------------------
   -- Complete_On_Sync_Point --
   ----------------------------

   procedure Complete_on_Sync_Point (Self_ID : Task_ID; T : Task_ID) is
      Target    : Task_ID := T;
      Call      : Entry_Call_Link;
      No_Server : Boolean;

   begin
      pragma Assert (Self_ID = Self or else
        Shutdown ("Only the Self can execute this!"));

      Write_Lock (Target);

      --  If the target is waiting to accept an entry call, complete it.

      if Target.Accepting /= Not_Accepting then
         Unlock (Target);
         Complete (Self_ID, T);
      else
         Unlock (Target);
      end if;

      --  Abort all pending entry calls in LIFO order until a non-abortable
      --  one is found.

      for Level in reverse
        ATC_Level_Index'First .. Target.ATC_Nesting_Level
      loop
         Call := Target.Entry_Calls (Level)'Access;
         Entry_Calls.Lock_Server (Call, No_Server);
         if not No_Server then
            if Call.Abortable = True then
               Queuing.Dequeue_Call (Call);
               Entry_Calls.Unlock_And_Update_Server (Call);
            else
               Entry_Calls.Unlock_Server (Call);
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
         Set_Priority
           (Acceptor_ATCB, Acceptor_Prev_Priority);
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

   procedure Terminate_Alternative (Self_ID : Task_ID) is
   begin
      pragma Assert (Self_ID = Self or else
        Shutdown ("Only the Self can execute this!"));

      Make_Passive (Self_ID);

      --  Note that abortion is deferred here (see WARNING above)

      Write_Lock (Self_ID);

      Self_ID.Terminate_Alternative := true;

      while Self_ID.Accepting /= Not_Accepting
        and then Self_ID.Stage /= Complete
        and then Self_ID.Pending_ATC_Level >= Self_ID.ATC_Nesting_Level
      loop
         Sleep (Self_ID);
      end loop;

      if Self_ID.Pending_ATC_Level = 0 and then
        Self_ID.Stage < Complete then
         Unlock (Self_ID);
         Complete (Self_ID, Self_ID);
         Write_Lock (Self_ID);
      end if;

      if Self_ID.Stage = Complete then
         Unlock (Self_ID);

         if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level then
            Undefer_Abort (Self_ID);
            pragma Assert (Shutdown ("Continuing after being aborted!"));
         end if;

         Abort_To_Level (Self_ID, 0);
         Undefer_Abort (Self_ID);
         pragma Assert (Shutdown ("Continuing after being aborted!"));
      end if;

      Self_ID.Terminate_Alternative := False;

      Unlock (Self_ID);

   end Terminate_Alternative;

   --------------
   -- Complete --
   --------------

   procedure Complete (Self_ID : Task_ID; Target : Task_ID) is
      T      : Task_ID := Target;
      Task1  : Task_ID;
      Task2  : Task_ID;

   begin
      pragma Assert (Self_ID = Self or else
        Shutdown ("Only the Self can execute this!"));

      Make_Passive (T);
      Write_Lock (T);

      if T.Stage < Complete then
         T.Accepting := Not_Accepting;
         T.Awaited_Dependent_Count := 0;
         Close_Entries (T);
         T.Stage := Complete;
      end if;

      Unlock (T);
   end Complete;

   --  Task_Stage related routines

   ----------------------
   -- Make_Independent --
   ----------------------

   procedure Make_Independent is
      T      : Task_ID := Self;
      P      : Task_ID;
      Result : Boolean;

   begin
      Write_Lock (T);
      P := T.Parent;
      Unlock (T);

      Write_Lock (P);
      Write_Lock (T);

      T.Master_of_Task := Master_ID (0);

      if P.Awake_Count > 1 then
         P.Awake_Count := P.Awake_Count - 1;
      end if;

      Unlock (T);
      Unlock (P);

      Initialization.Remove_From_All_Tasks_List (T, Result);
      pragma Assert (Result
        or else Shutdown ("Failed to delete an entry from All_Tasks_List"));

   end Make_Independent;

   --------------------------
   -- Terminate_Dependents --
   --------------------------

   --  WARNING : Only call this procedure with abortion deferred.
   --  This procedure needs to have abortion deferred while it has
   --  the current task's lock locked. This is indicated by the commented
   --  abortion control calls. Since it is called from two procedures which
   --  also need abortion deferred, it is left controlled on entry to
   --  this procedure.
   --
   --  This relies that all dependents are passive.
   --  That is, they may be :

   --  1) held in COMPLETE_TASK;
   --  2) aborted, with forced-call to COMPLETE_TASK pending;
   --  3) held in terminate-alternative of SELECT.

   procedure Terminate_Dependents
     (Self_ID : Task_ID;
      ML      : Master_ID)
   is
      C        : Task_ID;
      Previous : Task_ID;
      Temp     : Task_ID;

   begin
      Write_Lock (Initialization.All_Tasks_L'Access);

      --  Abortion is deferred already (see WARNING above)

      Write_Lock (Self_ID);

      --  Count the number of active dependents that must terminate before
      --  proceeding. If Terminating_Dependent_Count is not zero, then the
      --  dependents have already been counted. This can occur when a thread
      --  executing this routine is canceled and the cancellation takes effect
      --  when Sleep is called to wait for Terminating_Dependent_Count to
      --  go to zero. In this case we just skip the count and continue waiting
      --  for the count to go to zero.

      if Self_ID.Terminating_Dependent_Count = 0 then
         C := Initialization.All_Tasks_List;

         while C /= null loop

            --  The check for C.Stage=ATCB.Terminated and the increment of
            --  T.Terminating_Dependent_Count must be synchronized here and in
            --  Complete_Task using T.L and C.L. So long as the parent T
            --  is locked before the dependent C, this should not lead to
            --  deadlock.

            if C /= Self_ID then
               Write_Lock (C);

               if C.Parent = Self_ID
                 and then C.Master_of_Task >= ML
                 and then C.Stage /= Terminated
               then
                  Self_ID.Terminating_Dependent_Count :=
                    Self_ID.Terminating_Dependent_Count + 1;
               end if;

               Unlock (C);
            end if;

            C := C.All_Tasks_Link;
         end loop;
      end if;

      Unlock (Self_ID);

      C := Initialization.All_Tasks_List;

      while C /= null loop
         if C.Parent = Self_ID and then C.Master_of_Task >= ML then
            Complete (Self_ID, C);
            Wakeup (C);
         end if;

         C := C.All_Tasks_Link;
      end loop;

      Unlock (Initialization.All_Tasks_L'Access);

      Write_Lock (Self_ID);

      while Self_ID.Terminating_Dependent_Count /= 0 loop
         Sleep (Self_ID);
      end loop;

      Unlock (Self_ID);

      --  We don't wake up for abortion here, since we are already
      --  terminating just as fast as we can so there is no point.

      Write_Lock (Initialization.All_Tasks_L'Access);
      C := Initialization.All_Tasks_List;
      Previous := null;

      while C /= null loop
         if C.Parent = Self_ID and then C.Master_of_Task >= ML then
            if Previous /= null then
               Previous.All_Tasks_Link := C.All_Tasks_Link;
            else
               Initialization.All_Tasks_List :=
                 C.All_Tasks_Link;
            end if;

            Temp := C;
            C := C.All_Tasks_Link;

            --  If there are Interrupt Entries attached, detach them.

            if Temp.Interrupt_Entry then
               Interrupts.Detach_Interrupt_Entries (Temp);
            end if;

            --  Deallocate task's ATCB. It is out of All_Tasks_List and
            --  no further reference to its ATCB should be made.
            --  If we let the terminating task to deallocate its own
            --  structure we may deallocate the ATCB and still access
            --  its ID while that ID is being allocated to other task.

            Finalize_TCB (Temp);

         else
            Previous := C;
            C := C.All_Tasks_Link;
         end if;
      end loop;

      Unlock (Initialization.All_Tasks_L'Access);
   end Terminate_Dependents;

   ------------------------------
   -- Vulnerable_Complete_Task --
   ------------------------------

   --  WARNING : Only call this procedure with abortion deferred.
   --  That's why the name has "Vulnerable" in it.

   --  This procedure needs to have abortion deferred while it has the current
   --  task's lock locked.

   --  This procedure should be called to complete the current task. This
   --  should be done for:
   --    normal termination via completion;
   --    termination via unhandled exception;
   --    terminate alternative;
   --    abortion.

   procedure Vulnerable_Complete_Task (Self_ID : Task_ID) is
      P : Task_ID;
      C : Task_ID;

   begin
      pragma Assert (Self_ID = Self or else
        Shutdown ("Only the Self can execute this!"));

      --  Self.Stage can be safely checked for Can_Activate here without
      --  protection, since Self does not get to run until Stage is
      --  Can_Activate, and Vulnerable_Complete_Activation will check to
      --  see if it has moved beyond Complete_Activation under the
      --  protection of the mutex before decrementing the activator's
      --  Activation_Count.

      if Self_ID.Stage = Can_Activate then
         Vulnerable_Complete_Activation (Self_ID, Completed => True);
      end if;

      --  Note that abortion is deferred (see WARNING above)

      Complete (Self_ID, Self_ID);
      if Self_ID.Stage = Created then
         Self_ID.Stage := Terminated;
      end if;

      Write_Lock (Self_ID);

      --  If the task has been awakened due to abortion, this should
      --  cause the dependents to abort themselves and cause the awake
      --  count to go to zero.

      if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
        and then Self_ID.Awake_Count /= 0
      then
         Unlock (Self_ID);
         Abort_Dependents (Self_ID, Self_ID);
         Write_Lock (Self_ID);
      end if;

      --  At this point we want to complete tasks created by Self and not yet
      --  activated, and also mark those tasks as terminated.

      Unlock (Self_ID);
      Write_Lock (Initialization.All_Tasks_L'Access);

      C := Initialization.All_Tasks_List;

      while C /= null loop

         if C.Parent = Self_ID and then C.Stage = Created then
            Complete (Self_ID, C);
            C.Stage := Terminated;
         end if;

         C := C.All_Tasks_Link;
      end loop;

      Unlock (Initialization.All_Tasks_L'Access);
      Write_Lock (Self_ID);

      while Self_ID.Awake_Count /= 0 loop
         Sleep (Self_ID);

         if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
           and then Self_ID.Awake_Count /= 0
         then
            --  The task may have been awakened to perform abortion.

            Unlock (Self_ID);
            Abort_Dependents (Self_ID, Self_ID);
            Write_Lock (Self_ID);
         end if;
      end loop;

      Self_ID.Pending_Action := False;
      Unlock (Self_ID);
      Terminate_Dependents (Self_ID, Master_ID'First);

   end Vulnerable_Complete_Task;

   --  Task Abortion related routines

   --------------------
   -- Abort_To_Level --
   --------------------

   procedure Abort_To_Level (Target : Task_ID;  L : ATC_Level) is
      T      : Task_ID := Target;

   begin
      Write_Lock (T);

      --  If the task is suspended on a condition variable, it will
      --  be in an abort-deferred region, and will not be awakened
      --  by abortion. Such an abort deferral is just to protect
      --  the low-level operations, and not to enforce Ada semantics.
      --  Wake the task up and let it decide if it wants to
      --  complete the aborted construct immediately.  This is done
      --  unconditionally, since a Cond_Signal is not persistent, and
      --  is needed even if the task has been aborted before.

      Wakeup (T);

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
               Unlock (T);
               Abort_Task (T);
               return;

            elsif T.Stage /= Terminated then
               Abort_Task (T);
            end if;

         end if;
      end if;

      Unlock (T);

   end Abort_To_Level;

   -----------------
   -- Abort_Tasks --
   -----------------

   --  Called to initiate abortion, however, the actual abortion
   --  is done by abortee by means of Abort_Handler

   procedure Abort_Tasks (Tasks : Task_List) is
      Self_ID : constant Task_ID := Self;
      Abortee : Task_ID;

   begin
      Defer_Abort (Self_ID);

      --  Begin non-abortable section

      for J in Tasks'Range loop

         Abortee := Tasks (J);

         if Abortee.Stage = Created then
            Complete (Self_ID, Abortee);
            Abortee.Stage := Terminated;
            --  Task aborted before activation is safe to complete
            --  Mark This task to be terminated.
         else
            Abortee.Accepting := Not_Accepting;
            Complete_on_Sync_Point (Self_ID, Abortee);
            Abort_To_Level (Abortee, 0);
            --  Process abortion of child tasks
            Abort_Dependents (Self_ID, Abortee);
         end if;

      end loop;

      --  End non-abortable section

      Undefer_Abort (Self_ID);
   end Abort_Tasks;

   ----------------------
   -- Abort_Dependents --
   ----------------------

   --  Process abortion of child tasks.

   --  Abortion should be dererred when calling this routine.
   --  No mutexes should be locked when calling this routine.

   procedure Abort_Dependents (Self_ID : Task_ID; Abortee : Task_ID) is
      Temp_T                : Task_ID;
      Temp_P                : Task_ID;
      A                     : Task_ID := Abortee;

   begin
      pragma Assert (Self_ID = Self or else
        Shutdown ("Only the Self can execute this!"));

      Write_Lock (Initialization.All_Tasks_L'Access);
      Temp_T := Initialization.All_Tasks_List;

      while Temp_T /= Null_Task loop
         Temp_P := Temp_T.Parent;

         while Temp_P /= Null_Task loop
            exit when Temp_P = A;
            Temp_P := Temp_P.Parent;
         end loop;

         if Temp_P = A then
            Temp_T.Accepting := Not_Accepting;

            --  Send cancel signal.
            Complete_on_Sync_Point (Self_ID, Temp_T);
            Abort_To_Level (Temp_T, 0);
         end if;

         Temp_T := Temp_T.All_Tasks_Link;
      end loop;

      Unlock (Initialization.All_Tasks_L'Access);

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

      Activator : Task_ID;

   begin
      Vulnerable_Complete_Activation (T, Completed => False);

      Write_Lock (T);

      if T.Stage >= Passive then
         Unlock (T);
         return;
      else
         T.Stage := Passive;
         Unlock (T);
      end if;

      H := Null_Task;
      P := T.Parent;
      C := T;

      while C /= Null_Task loop

         if P /= Null_Task then
            Write_Lock (P);
            Write_Lock (C);

            C.Awake_Count := C.Awake_Count - 1;

            if C.Awake_Count /= 0 then

               --  C is not passive; we cannot make anything above this point
               --  passive.

               Unlock (C);
               Unlock (P);
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

               Unlock (C);
               Unlock (P);
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

            Unlock (C);
            Unlock (P);
            C := P;
            P := C.Parent;

         else
            Write_Lock (C);
            C.Awake_Count := C.Awake_Count - 1;

            if C.Awake_Count /= 0 then

               --  C is not passive; we cannot make anything above
               --  this point passive.

               Unlock (C);
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

            Unlock (C);
            C := P;
         end if;

      end loop;

      if H /= Null_Task then
         Write_Lock (H);
         Wakeup (H);
         Unlock (H);
      end if;

   end Make_Passive;

end System.Tasking.Utilities;

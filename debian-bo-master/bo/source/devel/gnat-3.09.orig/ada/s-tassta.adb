------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T A S K I N G . S T A G E S                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.63 $                            --
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

with System.Parameters; use System.Parameters;

--  The following two packages are not part of the GNARL proper.  They
--  provide access to a compiler-specific per-task data area.

with System.Tasking_Soft_Links;
--  Used for, Abort_Defer, Abort_Undefer
--  These are procedure pointers to non-tasking routines that use
--  task specific data.  In the absence of tasking, these routines
--  refer to global data.  In the presense of tasking, they must be
--  replaced with pointers to task-specific versions.

with System.Task_Specific_Data;
--  Used for, Create_TSD, Destroy_TSD
--  This package provides initialization routines for task specific data.
--  The GNARL must call these to be sure that all non-tasking
--  Ada constructs will work.

with System.Tasking.Utilities;
--  Used for, Utilities.ATCB_To_Address
--            Utilities.Task_Error
--            Utilities.Vulnerable_Complete_Activation
--            Utilities.Abort_To_Level
--            Utilities.Abort_Dependents
--            Utilities.Check_Exceptions
--            Utilities.Runtime_Assert_Shutdown

with System.Tasking.Initialization;
--  Used for, Remove_From_All_Tasks_List
--            All_Tasks_List
--            All_Tasks_L
--            Defer_Abortion,
--            Undefer_Abortion,
--            Change_Base_Priority
--            ATCB_Init
--            Finalize_Attributes_Link
--            Initialize_Attributes_Link

pragma Elaborate_All (System.Tasking.Initialization);
--  This insures that tasking is initialized if any tasks are created.

with System.Task_Memory;
--  Used for, Task_Memory.Low_Level_New,
--            Task_Memory.Unsafe_Low_Level_New,
--            Task_Memory.Low_Level_Free

with System.Task_Primitives; use System.Task_Primitives;

with System.Finalization_Implementation;
--  Used for System.Finalization_Implementation.Finalize_Global_List

with Unchecked_Conversion;
with Unchecked_Deallocation;

package body System.Tasking.Stages is

--   Global_Task_Lock : Lock;
   --  This is a global lock; it is used to execute in mutual exclusion
   --  from all other tasks.  It is only used by Task_Lock and
   --  Task_Unlock.

   procedure Defer_Abortion renames
     System.Tasking.Initialization.Defer_Abortion;

   procedure Undefer_Abortion renames
     System.Tasking.Initialization.Undefer_Abortion;

   -----------------------------
   -- Other Local Subprograms --
   -----------------------------

   procedure Task_Wrapper (Arg : System.Address);
   --  This is the procedure that is called by the GNULL from the
   --  new context when a task is created.  It waits for activation
   --  and then calls the task body procedure.  When the task body
   --  procedure completes, it terminates the task.

   procedure Terminate_Dependents (ML : Master_ID := Master_ID'First);
   --  Terminate all dependent tasks of given master level

   procedure Vulnerable_Complete_Task;
   --  Complete the calling task.  This procedure must be called with
   --  abortion deferred.

   -----------------------------
   -- Finalization management --
   -----------------------------

   procedure Finalize_Global_Tasks is

      package TSL renames System.Tasking_Soft_Links;

      NT_Sec_Stack_Addr : Address := TSL.Get_Sec_Stack_Addr.all;
      NT_Exc_Stack_Addr : Address := TSL.Get_Exc_Stack_Addr.all;
      NT_GNAT_Exception : Address := TSL.Get_GNAT_Exception.all;
      NT_Jmpbuf_Address : Address := TSL.Get_Jmpbuf_Address.all;

   begin

      --  All tasks must be complete before shutting down tasking services.

      Complete_Task;

      --  Finalization of global objects must be done when all tasks are
      --  completed and before the tasking services are shutdown

      System.Finalization_Implementation.Finalize_Global_List;

      TSL.Abort_Defer        := TSL.Abort_Defer_NT'Access;
      TSL.Abort_Undefer      := TSL.Abort_Undefer_NT'Access;
      TSL.Lock_Task          := TSL.Task_Lock_NT'Access;
      TSL.Unlock_Task        := TSL.Task_Unlock_NT'Access;
      TSL.Get_Jmpbuf_Address := TSL.Get_Jmpbuf_Address_NT'Access;
      TSL.Set_Jmpbuf_Address := TSL.Set_Jmpbuf_Address_NT'Access;
      TSL.Get_Gnat_Exception := TSL.Get_GNAT_Exception_NT'Access;
      TSL.Set_Gnat_Exception := TSL.Set_GNAT_Exception_NT'Access;
      TSL.Get_Sec_Stack_Addr := TSL.Get_Sec_Stack_Addr_NT'Access;
      TSL.Set_Sec_Stack_Addr := TSL.Set_Sec_Stack_Addr_NT'Access;
      TSL.Get_Exc_Stack_Addr := TSL.Get_Exc_Stack_Addr_NT'Access;
      TSL.Set_Exc_Stack_Addr := TSL.Set_Exc_Stack_Addr_NT'Access;

      TSL.Set_Sec_Stack_Addr (NT_Sec_Stack_Addr);
      TSL.Set_Exc_Stack_Addr (NT_Exc_Stack_Addr);
      TSL.Set_GNAT_Exception (NT_GNAT_Exception);
      TSL.Set_Jmpbuf_Address (NT_Jmpbuf_Address);

      Finalize_Lock (System.Tasking.Initialization.All_Tasks_L);
--    Finalize_Lock (Global_Task_Lock);

   end Finalize_Global_Tasks;

   ------------------
   -- Task_Wrapper --
   ------------------

   procedure Task_Wrapper (Arg : System.Address) is
      function Address_To_Task_ID is new
        Unchecked_Conversion (System.Address, Task_ID);

      T : Task_ID := Address_To_Task_ID (Arg);

   begin
      Undefer_Abortion;

      --  Call the task body procedure.

      T.Task_Entry_Point (T.Task_Arg);
      --  Return here after task finalization

      Defer_Abortion;

      --  This call won't return. Therefor no need for Undefer_Abortion

      Stages.Leave_Task;

   exception

      --  Only the call to user code (T.Task_Entry_Point) should raise an
      --  exception.  An "at end" handler in the generated code should have
      --  completed the the task, and the exception should not be propagated
      --  further.  Terminate the task as though it had returned.

      when Standard'Abort_Signal =>
         Defer_Abortion;
         Stages.Leave_Task;

      when others =>
         Defer_Abortion;
         Stages.Leave_Task;

   end Task_Wrapper;

   -----------------
   -- Create_Task --
   -----------------

   --  Note: in this version of Create_Task, the Task_Info parameter is ignored

   procedure Create_Task
     (Priority      : Integer;
      Size          : Size_Type;
      Task_Info     : System.Task_Info.Task_Info_Type;
      Num_Entries   : Task_Entry_Index;
      Master        : Master_ID;
      State         : Task_Procedure_Access;
      Discriminants : System.Address;
      Elaborated    : Access_Boolean;
      Chain         : in out Activation_Chain;
      Created_Task  : out Task_ID)
   is
      T, P, S : Task_ID;
      Init    : System.Tasking.Initialization.ATCB_Init;
      Error   : Boolean;

   begin
      S := Self;

      if Priority = Unspecified_Priority then
         Init.Base_Priority := S.Base_Priority;
      else
         Init.Base_Priority := Priority;
      end if;

      --  At first set the Active (Current) Priority same as the Base_Priority.

      Init.Current_Priority := Init.Base_Priority;

      --  Find parent of new task, P, via master level number.

      P := S;
      if P /= null then
         while P.Master_of_Task >= Master loop
            P := P.Parent;
            exit when P = null;
         end loop;
      end if;

      Defer_Abortion;

      Init.Entry_Num := Num_Entries;
      Init.Task_Arg := Discriminants;
      Init.Parent := P;
      Init.Task_Entry_Point := State;
      Init.Task_Info := Task_Info;

      if Size = Unspecified_Size then
         Init.Stack_Size := Default_Stack_Size;
      elsif Size < Minimum_Stack_Size then
         Init.Stack_Size := Minimum_Stack_Size;
      else
         Init.Stack_Size := Size;
      end if;

      Init.Activator := S;
      Init.Master_of_Task := Master;
      Init.Elaborated := Elaborated;

      begin
         T := System.Tasking.Initialization.New_ATCB (Init);
      exception
         when others =>
            Undefer_Abortion;
            raise Storage_Error;
      end;

      if P /= null then
         Write_Lock (P.L, Error);

         if P /= S
           and then P.Awaited_Dependent_Count /= 0
           and then Master = P.Master_Within
         then
            P.Awaited_Dependent_Count := P.Awaited_Dependent_Count + 1;
         end if;

         P.Awake_Count := P.Awake_Count + 1;
         Unlock (P.L);
      end if;

      --  Create TSD as early as possible in the creation of a task, since the
      --  operation of code within the task may depend on task specific data.

      Task_Specific_Data.Create_TSD (T.Compiler_Data);

      T.Activation_Link := Task_ID (Chain);
      Chain := Activation_Chain (T);

      T.Aborter_Link := null;

      System.Tasking.Initialization.Initialize_Attributes_Link.all (T);

      Created_Task := T;

      Undefer_Abortion;

   end Create_Task;

   --------------------
   -- Activate_Tasks --
   --------------------

   procedure Activate_Tasks (Chain_Access : Activation_Chain_Access) is
      This_Task      : Task_ID;
      C              : Task_ID;
      All_Elaborated : Boolean := True;
      LL_Entry_Point : Task_Primitives.LL_Task_Procedure_Access;
      Error          : Boolean;

   begin
      This_Task := Self;

      C := Task_ID (Chain_Access.all);
      while (C /= null) and All_Elaborated loop
         if C.Elaborated /= null and then not C.Elaborated.all then
            All_Elaborated := False;
         end if;

         C := C.Activation_Link;
      end loop;

      --  Check that all task bodies have been elaborated.

      if not All_Elaborated then
         raise Program_Error;
      end if;

      Defer_Abortion;

      Write_Lock (This_Task.L, Error);
      This_Task.Activation_Count := 0;

      --  Wake up all the tasks so that they can activate themselves.

      LL_Entry_Point := Task_Wrapper'Access;

      C := Task_ID (Chain_Access.all);
      while C /= null loop

         Write_Lock (C.L, Error);

         --  Note that the locks of the activator and created task are locked
         --  here.  This is necessary because C.Stage and
         --  This_Task.Activation_Count have to be synchronized.  This is also
         --  done in Complete_Activation and Init_Abortion.  So long as the
         --  activator lock is always locked first, this cannot lead to
         --  deadlock.

         if C.Stage = Created then

            --  Create the task
            --  Actual creation of LL_Task is deferred until the activation
            --  time

            if C.Current_Priority < This_Task.Current_Priority then
               C.Current_Priority := This_Task.Current_Priority;
            end if;

            --  Ask for 4 extra bytes of stack space so that the ATCB
            --  pointer can be stored below the stack limit, plus extra
            --  space for the frame of Task_Wrapper.  This is so the use
            --  gets the amount of stack requested exclusive of the needs
            --  of the runtime.

            --  why 4????

            Create_LL_Task (
              System.Any_Priority (C.Current_Priority),
              Task_Primitives.Task_Storage_Size (
              Integer (C.Stack_Size) +
              Integer (Task_Primitives.Task_Wrapper_Frame) + 4),
              C.Task_Info,
              LL_Entry_Point,
              Utilities.ATCB_To_Address (C),
              C.LL_TCB'Access);

            C.Stage := Can_Activate;
            This_Task.Activation_Count := This_Task.Activation_Count + 1;

         end if;

         Unlock (C.L);

         C := C.Activation_Link;
      end loop;

      while This_Task.Activation_Count > 0 loop
         if This_Task.Pending_Action then
            if This_Task.Pending_Priority_Change then
               System.Tasking.Initialization.Change_Base_Priority (This_Task);
            end if;

            exit when
               This_Task.Pending_ATC_Level < This_Task.ATC_Nesting_Level;
            This_Task.Pending_Action := False;
         end if;
         Cond_Wait (This_Task.Cond, This_Task.L);
      end loop;

      Unlock (This_Task.L);

      --  After the activation, tasks should be removed from the Chain

      Chain_Access.all := null;

      Undefer_Abortion;
      Utilities.Check_Exception;
   end Activate_Tasks;

   -------------------------------
   -- Expunge_Unactivated_Tasks --
   -------------------------------

   procedure Expunge_Unactivated_Tasks (Chain : in out Activation_Chain) is
      This_Task : Task_ID := Self;
      C         : Task_ID;
      Temp      : Task_ID;
      Result    : Boolean;

   begin
      Defer_Abortion;

      C := Task_ID (Chain);

      while C /= null loop

         pragma Assert (
           C.Stage <= Created or else
             Utilities.Runtime_Assert_Shutdown (
               "Trying to expunge task which went beyond CREATED stage"));

         Temp := C;
         C := C.Activation_Link;

         --  Take care of decrementing parent's Await_Count and
         --  Awaited_Dependent_Count.

         Utilities.Complete (Temp);

         System.Tasking.Initialization.Remove_From_All_Tasks_List
           (Temp, Result);

         pragma Assert (
           Result or else Utilities.Runtime_Assert_Shutdown (
             "Mismatch between All_Tasks_List and Chain to be expunged"));

         --  Task is out of Chain and All_Tasks_List. It is now safe to
         --  free the storage for ATCB.

         System.Tasking.Initialization.Free_ATCB (Temp);

      end loop;

      Chain := null;

      Undefer_Abortion;

   end Expunge_Unactivated_Tasks;

   --------------------
   -- Current_Master --
   --------------------

   function Current_Master return Master_ID is
      T : Task_ID := Self;
   begin
      return T.Master_Within;
   end Current_Master;

   ------------------------------
   -- Vulnerable_Complete_Task --
   ------------------------------

   --  WARNING : Only call this procedure with abortion deferred.
   --  That's why the name has "Vulnerable" in it.

   --  This procedure needs to have abortion deferred while it has the current
   --  task's lock locked.

   --  This procedure should be called to complete the current task.  This
   --  should be done for:
   --    normal termination via completion;
   --    termination via unhandled exception;
   --    terminate alternative;
   --    abortion.

   procedure Vulnerable_Complete_Task is
      P, T  : Task_ID := Self;
      C     : Task_ID;
      Error : Boolean;

   begin
      --  T.Stage can be safely checked for Can_Activate here without
      --  protection, since T does not get to run until Stage is Can_Activate,
      --  and Vulnerable_Complete_Activation will check to see if it has moved
      --  beyond Complete_Activation under the protection of the mutex
      --  before decrementing the activator's Activation_Count.

      if T.Stage = Can_Activate then
         Utilities.Vulnerable_Complete_Activation (T, Completed => True);
      end if;

      --  Note that abortion is deferred (see WARNING above)

      Utilities.Complete (T);
      if T.Stage = Created then
         T.Stage := Terminated;
      end if;

      Write_Lock (T.L, Error);

      --  If the task has been awakened due to abortion, this should
      --  cause the dependents to abort themselves and cause the awake
      --  count to go to zero.

      if T.Pending_ATC_Level < T.ATC_Nesting_Level
        and then T.Awake_Count /= 0
      then
         Unlock (T.L);
         Utilities.Abort_Dependents (T);
         Write_Lock (T.L, Error);
      end if;

      --  At this point we want to complete tasks created by T and not yet
      --  activated, and also mark those tasks as terminated.

      Unlock (T.L);
      Write_Lock (System.Tasking.Initialization.All_Tasks_L, Error);

      C := System.Tasking.Initialization.All_Tasks_List;

      while C /= null loop

         if C.Parent = T and then C.Stage = Created then
            Utilities.Complete (C);
            C.Stage := Terminated;
         end if;

         C := C.All_Tasks_Link;
      end loop;

      Unlock (System.Tasking.Initialization.All_Tasks_L);
      Write_Lock (T.L, Error);

      while T.Awake_Count /= 0 loop
         Cond_Wait (T.Cond, T.L);

         if T.Pending_ATC_Level < T.ATC_Nesting_Level
           and then T.Awake_Count /= 0
         then
            --  The task may have been awakened to perform abortion.

            Unlock (T.L);
            Utilities.Abort_Dependents (T);
            Write_Lock (T.L, Error);
         end if;
      end loop;

      T.Pending_Action := False;
      Unlock (T.L);
      Terminate_Dependents;

   end Vulnerable_Complete_Task;

   ----------------
   -- Leave_Task --
   ----------------

   procedure Leave_Task is
      P, T                    : Task_ID := Self;
      Saved_Pending_ATC_Level : ATC_Level_Base;
      Error                   : Boolean;

   begin
      Saved_Pending_ATC_Level := T.Pending_ATC_Level;

      --  We are about to lose our ATCB. Save special fields for final cleanup.

      P := T.Parent;

      if P /= null then
         Write_Lock (P.L, Error);
         Write_Lock (T.L, Error);

         --  If T has a parent, then setting T.Stage to Terminated and
         --  incrementing/decrementing P.Terminating_Dependent_Count
         --  have to be synchronized here and in Terminate_Dependents.
         --  This is done by locking the parent and dependent locks.  So
         --  long as the parent lock is always locked first, this should not
         --  cause deadlock.

         T.Stage := Terminated;

         if P.Terminating_Dependent_Count > 0
           and then T.Master_of_Task = P.Master_Within
         then
            P.Terminating_Dependent_Count := P.Terminating_Dependent_Count - 1;

            if P.Terminating_Dependent_Count = 0 then
               Cond_Signal (P.Cond);
            end if;
         end if;

         System.Tasking.Initialization.Finalize_Attributes_Link.all (T);

         --  Destroying the TSD is the last thing done to a TCB, since the
         --  correct operation of compiled code may depend on it.

         Task_Specific_Data.Destroy_TSD (T.Compiler_Data);

         Unlock (T.L);
         Unlock (P.L);

         --  WARNING - Once this lock is unlocked, it should be assumed that
         --  the ATCB has been deallocated. It should not be accessed again.

      else
         Write_Lock (T.L, Error);
         T.Stage := Terminated;

         System.Tasking.Initialization.Finalize_Attributes_Link.all (T);

         Task_Specific_Data.Destroy_TSD (T.Compiler_Data);
         --  This should be the last thing done to a TCB, since the correct
         --  operation of compiled code may depend on it.

         Unlock (T.L);
      end if;

      Exit_LL_Task;

   end Leave_Task;

   -------------------
   -- Complete_Task --
   -------------------

   procedure Complete_Task is
   begin
      Defer_Abortion;
      Vulnerable_Complete_Task;
      Undefer_Abortion;
   end Complete_Task;

   -------------------------
   -- Complete_Activation --
   -------------------------

   procedure Complete_Activation is
   begin
      Defer_Abortion;

      Utilities.Vulnerable_Complete_Activation
        (Self, Completed => False);

      Undefer_Abortion;
   end Complete_Activation;

   --------------------------
   -- Terminate_Dependents --
   --------------------------

   --  WARNING : Only call this procedure with abortion deferred.
   --  This procedure needs to have abortion deferred while it has
   --  the current task's lock locked.  This is indicated by the commented
   --  abortion control calls.  Since it is called from two procedures which
   --  also need abortion deferred, it is left controlled on entry to
   --  this procedure.
   --
   --  This relies that all dependents are passive.
   --  That is, they may be :

   --  1) held in COMPLETE_TASK;
   --  2) aborted, with forced-call to COMPLETE_TASK pending;
   --  3) held in terminate-alternative of SELECT.

   procedure Terminate_Dependents (ML : Master_ID := Master_ID'First) is
      T        : Task_ID := Self;
      C        : Task_ID;
      Previous : Task_ID;
      Temp     : Task_ID;
      Error    : Boolean;

   begin
      Write_Lock (System.Tasking.Initialization.All_Tasks_L, Error);

      --  Abortion is deferred already (see WARNING above)

      Write_Lock (T.L, Error);

      --  Count the number of active dependents that must terminate before
      --  proceeding.  If Terminating_Dependent_Count is not zero, then the
      --  dependents have already been counted.  This can occur when a thread
      --  executing this routine is canceled and the cancellation takes effect
      --  when Cond_Wait is called to wait for Terminating_Dependent_Count to
      --  go to zero.  In this case we just skip the count and continue waiting
      --  for the count to go to zero.

      if T.Terminating_Dependent_Count = 0 then
         C := System.Tasking.Initialization.All_Tasks_List;

         while C /= null loop

            --  The check for C.Stage=ATCB.Terminated and the increment of
            --  T.Terminating_Dependent_Count must be synchronized here and in
            --  Complete_Task using T.L and C.L.  So long as the parent T
            --  is locked before the dependent C, this should not lead to
            --  deadlock.

            if C /= T then
               Write_Lock (C.L, Error);

               if C.Parent = T
                 and then C.Master_of_Task >= ML
                 and then C.Stage /= Terminated
               then
                  T.Terminating_Dependent_Count :=
                    T.Terminating_Dependent_Count + 1;
               end if;

               Unlock (C.L);
            end if;

            C := C.All_Tasks_Link;
         end loop;
      end if;

      Unlock (T.L);

      C := System.Tasking.Initialization.All_Tasks_List;

      while C /= null loop
         if C.Parent = T and then C.Master_of_Task >= ML then
            Utilities.Complete (C);
            Cond_Signal (C.Cond);
         end if;

         C := C.All_Tasks_Link;
      end loop;

      Unlock (System.Tasking.Initialization.All_Tasks_L);

      Write_Lock (T.L, Error);

      while T.Terminating_Dependent_Count /= 0 loop
         Cond_Wait (T.Cond, T.L);
      end loop;

      Unlock (T.L);

      --  We don't wake up for abortion here, since we are already
      --  terminating just as fast as we can so there is no point.

      Write_Lock (System.Tasking.Initialization.All_Tasks_L, Error);
      C := System.Tasking.Initialization.All_Tasks_List;
      Previous := null;

      while C /= null loop
         if C.Parent = T
           and then C.Master_of_Task >= ML
         then
            if Previous /= null then
               Previous.All_Tasks_Link := C.All_Tasks_Link;
            else
               System.Tasking.Initialization.All_Tasks_List :=
                 C.All_Tasks_Link;
            end if;

            Temp := C;
            C := C.All_Tasks_Link;
            System.Tasking.Initialization.Free_ATCB (Temp);

            --  It is OK to free the ATCB provided that the dependent task
            --  does not access its ATCB in Complete_Task after signaling its
            --  parent's (this task) condition variable and unlocking its lock.

         else
            Previous := C;
            C := C.All_Tasks_Link;
         end if;
      end loop;

      Unlock (System.Tasking.Initialization.All_Tasks_L);
   end Terminate_Dependents;

   ------------------
   -- Enter_Master --
   ------------------

   procedure Enter_Master is
      T : Task_ID := Self;

   begin
      T.Master_Within :=
        System.Tasking.Initialization.Increment_Master (T.Master_Within);
   end Enter_Master;

   ---------------------
   -- Complete_Master --
   ---------------------

   procedure Complete_Master is
      T     : Task_ID := Self;
      C     : Task_ID;
      CM    : Master_ID := T.Master_Within;
      Error : Boolean;

   begin
      Defer_Abortion;

      Write_Lock (System.Tasking.Initialization.All_Tasks_L, Error);

      --  Cancel threads of dependent tasks that have not yet started
      --  activation.

      C := System.Tasking.Initialization.All_Tasks_List;

      while C /= null loop
         if C.Parent = T and then C.Master_of_Task = CM then
            Write_Lock (C.L, Error);

            --  The only way that a dependent should not have been activated
            --  at this point is if the master was aborted before it could
            --  call Activate_Tasks.  Abort such dependents.

            if C.Stage = Created then
               Unlock (C.L);
               Utilities.Complete (C);
               C.Stage := Terminated;
               --  Task is not yet activated. So, just complete and
               --  Mark it as Terminated.
            else
               Unlock (C.L);
            end if;

         end if;

         C := C.All_Tasks_Link;
      end loop;

      --  Note that Awaited_Dependent_Count must be zero at this point.  It is
      --  initialized to zero, this is the only code that can increment it
      --  when it is zero, and it will be zero again on exit from this routine.

      Write_Lock (T.L, Error);
      C := System.Tasking.Initialization.All_Tasks_List;

      while C /= null loop
         if C.Parent = T and then C.Master_of_Task = CM then
            Write_Lock (C.L, Error);

            if C.Awake_Count /= 0 then
               T.Awaited_Dependent_Count := T.Awaited_Dependent_Count + 1;
            end if;

            Unlock (C.L);
         end if;

         C := C.All_Tasks_Link;
      end loop;

      Unlock (T.L);
      --  Unlock T.L here to avoid improper lock nesting; All_Tasks_L
      --  is always the outer lock to avoid deadlock, so we have to
      --  unlock T.L before unlocking All_Tasks_L.

      Unlock (System.Tasking.Initialization.All_Tasks_L);

      Write_Lock (T.L, Error);

      --  If the task has been awakened due to abortion, this should
      --  cause the dependents to abort themselves and cause
      --  Awaited_Dependent_Count count to go to zero.

      if T.Pending_ATC_Level < T.ATC_Nesting_Level
        and then T.Awaited_Dependent_Count /= 0
      then
         Unlock (T.L);
         Utilities.Abort_Dependents (T);
         Write_Lock (T.L, Error);
      end if;

      if T.Awaited_Dependent_Count /= 0 then
         T.Stage := Await_Dependents;
      end if;

      while T.Awaited_Dependent_Count /= 0 loop
         Cond_Wait (T.Cond, T.L);

         if T.Pending_ATC_Level < T.ATC_Nesting_Level
           and then T.Awaited_Dependent_Count /= 0
         then
            --  The task may have been awakened to perform abortion.

            Unlock (T.L);
            Utilities.Abort_Dependents (T);
            Write_Lock (T.L, Error);
         end if;

      end loop;

      Unlock (T.L);

      if T.Pending_ATC_Level < T.ATC_Nesting_Level then
         Undefer_Abortion;
         return;

         --  Abort_Signal should be raised upon exit from at_end handler
      end if;

      Terminate_Dependents (CM);

      T.Stage := Active;

      --  Make next master level up active.  This needs to be done before
      --  decrementing the master level number, so that tasks finding
      --  themselves dependent on the current master level do not think that
      --  this master has been terminated (i.e. Stage=Await_Dependents and
      --  Awaited_Dependent_Count=0).  This should be safe; the only thing that
      --  can affect the stage of a task after it has become active is either
      --  the task itself or abortion, which is deferred here.

      T.Master_Within := System.Tasking.Initialization.Decrement_Master (CM);

      --  Should not need protection; can only change if T executes an
      --  Enter_Master or a Complete_Master.  T is only one task, and cannot
      --  execute these while executing this.

      Undefer_Abortion;

   end Complete_Master;

   ----------------
   -- Terminated --
   ----------------

   function Terminated (T : Task_ID) return Boolean is
   begin
      --  Does not need protection; access is assumed to be atomic.
      --  Why is this assumption made, is pragma Atomic applied properly???

      return T.Stage = Terminated;
   end Terminated;

end System.Tasking.Stages;

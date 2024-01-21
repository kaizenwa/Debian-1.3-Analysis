------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T A S K I N G . S T A G E S                --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.67 $                            --
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

with System.Error_Reporting;
--  used for Shutdown;

with System.Parameters;
--  used for Size_Type

with System.Standard_Library;
--  used for Tasking_Error_Def;
--           Exception_Data

with System.Task_Info;
--  used for Task_Info_Type

with System.Task_Primitives.Operations;
--  used for Finalize_Lock
--           Enter_Task
--           Write_Lock
--           Unlock
--           Sleep
--           Wakeup
--           Get_Priority

with System.Task_Specific_Data;
--  Used for Create_TSD
--           Destroy_TSD
--  This package provides initialization routines for task specific data.
--  The GNARL must call these to be sure that all non-tasking
--  Ada constructs will work.

with System.Tasking_Soft_Links;
--  These are procedure pointers to non-tasking routines that use
--  task specific data. In the absence of tasking, these routines
--  refer to global data. In the presense of tasking, they must be
--  replaced with pointers to task-specific versions.

with System.Tasking.Initialization;
--  Used for Remove_From_All_Tasks_List
--           All_Tasks_List
--           All_Tasks_L
--           Defer_Abort
--           Undefer_Abort
--           Change_Base_Priority
--           ATCB_Init
--           New_ATCB
--           Increment_Master
--           Decrement_Master
--           Finalize_Attributes_Link
--           Initialize_Attributes_Link

pragma Elaborate (System.Tasking.Initialization);
--  This insures that tasking is initialized if any tasks are created.

with System.Tasking.Utilities;
--  Used for Complete
--           Vulnerable_Complete_Activation
--           Vulnerable_Complete_Task
--           Abort_Dependents
--           Terminate_Dependents
--           Check_Exceptions

with Unchecked_Conversion;

with System.Finalization_Implementation;
--  Used for System.Finalization_Implementation.Finalize_Global_List

package body System.Tasking.Stages is

   use System.Task_Primitives.Operations;
   use System.Error_Reporting;

   type Exception_Data_Access
     is access all System.Standard_Library.Exception_Data;

   function To_Exception_ID is new
     Unchecked_Conversion (Exception_Data_Access, Ada.Exceptions.Exception_ID);

   procedure Defer_Abort (Self_ID : Task_ID) renames
     Initialization.Defer_Abort;

   procedure Undefer_Abort (Self_ID : Task_ID) renames
     Initialization.Undefer_Abort;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Task_Wrapper (Self_ID : Task_ID);
   --  This is the procedure that is called by the GNULL from the
   --  new context when a task is created. It waits for activation
   --  and then calls the task body procedure. When the task body
   --  procedure completes, it terminates the task.

   procedure Leave_Task (Self_ID : Task_ID);
   --  This needs comments ???

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

      Finalize_Lock (Initialization.All_Tasks_L'Access);

   end Finalize_Global_Tasks;


   ------------------
   -- Task_Wrapper --
   ------------------

   procedure Task_Wrapper (Self_ID : Task_ID) is
      ID : Task_ID := Self_ID;
      --  Do not delete this variable.
      --  In some targets, we need this variable to implement a fast Self.

   begin

      Enter_Task (ID);

      --  We are separating the following portion of the code in order to
      --  place the exception handlers in a different block.
      --  In this way we do not call Set_Jmpbuf_Address (which needs Self)
      --  before we do thr_setspecific in Enter_Task;

      begin

         Undefer_Abort (ID);

         --  Call the task body procedure.

         ID.Task_Entry_Point (ID.Task_Arg);

         --  Return here after task finalization

         Defer_Abort (ID);
         Stages.Leave_Task (ID);

         --  This call won't return. Therefore no need for Undefer_Abortion

         System.Task_Primitives.Operations.Exit_Task;

      exception

         --  Only the call to user code (T.Task_Entry_Point) should raise an
         --  exception. An "at end" handler in the generated code should have
         --  completed the task, and the exception should not be propagated
         --  further. Terminate the task as though it had returned.

         when Standard'Abort_Signal =>
            Defer_Abort (ID);
            Stages.Leave_Task (ID);
            System.Task_Primitives.Operations.Exit_Task;

         when others =>
            Defer_Abort (ID);
            Stages.Leave_Task (ID);
            System.Task_Primitives.Operations.Exit_Task;
      end;

   end Task_Wrapper;

   -----------------
   -- Create_Task --
   -----------------

   --  Note: in this version of Create_Task, the Task_Info parameter is ignored

   procedure Create_Task
     (Priority      : Integer;
      Size          : System.Parameters.Size_Type;
      Task_Info     : System.Task_Info.Task_Info_Type;
      Num_Entries   : Task_Entry_Index;
      Master        : Master_ID;
      State         : Task_Procedure_Access;
      Discriminants : System.Address;
      Elaborated    : Access_Boolean;
      Chain         : in out Activation_Chain;
      Created_Task  : out Task_ID)
   is
      T, P          : Task_ID;
      Init          : Initialization.ATCB_Init;
      Self_ID       : constant Task_ID := Self;

   begin

      if Priority = Unspecified_Priority then
         Init.Base_Priority := Self_ID.Base_Priority;
      else
         Init.Base_Priority := System.Any_Priority (Priority);
      end if;

      --  Find parent of new task, P, via master level number.

      P := Self_ID;
      if P /= null then
         while P.Master_of_Task >= Master loop
            P := P.Parent;
            exit when P = null;
         end loop;
      end if;

      Defer_Abort (Self_ID);


      Init.Entry_Num := Num_Entries;
      Init.Task_Arg := Discriminants;
      Init.Parent := P;
      Init.Task_Entry_Point := State;
      Init.Task_Info := Task_Info;
      Init.Stack_Size := Size;
      Init.Activator := Self_ID;
      Init.Master_of_Task := Master;
      Init.Elaborated := Elaborated;

      begin
         T := Initialization.New_ATCB (Self_ID, Init);
      exception
         when others =>
            Undefer_Abort (Self_ID);
            raise Storage_Error;
      end;

      if P /= null then
         Write_Lock (P);

         if P /= Self_ID
           and then P.Awaited_Dependent_Count /= 0
           and then Master = P.Master_Within
         then
            P.Awaited_Dependent_Count := P.Awaited_Dependent_Count + 1;
         end if;

         P.Awake_Count := P.Awake_Count + 1;
         Unlock (P);
      end if;

      --  Create TSD as early as possible in the creation of a task, since it
      --  may be used by the operation of Ada code within the task.

      Task_Specific_Data.Create_TSD (T.Compiler_Data);

      T.Activation_Link := Task_ID (Chain);
      Chain := Activation_Chain (T);

      Initialization.Initialize_Attributes_Link.all (T);

      Created_Task := T;

      Undefer_Abort (Self_ID);

   end Create_Task;

   --------------------
   -- Activate_Tasks --
   --------------------

   procedure Activate_Tasks (Chain_Access : Activation_Chain_Access) is
      Self_ID        : constant Task_ID := Self;
      C              : Task_ID;
      All_Elaborated : Boolean := True;
      Activate_Prio  : System.Any_Priority;
      Success        : Boolean;

   begin
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

      Defer_Abort (Self_ID);

      Write_Lock (Self_ID);
      Self_ID.Activation_Count := 0;

      --  Wake up all the tasks so that they can activate themselves.

      C := Task_ID (Chain_Access.all);
      while C /= null loop

         Write_Lock (C);

         --  Note that the locks of the activator and created task are locked
         --  here. This is necessary because C.Stage and Self.Activation_Count
         --  have to be synchronized. This is also done in Complete_Activation
         --  and Init_Abortion. So long as the activator lock is always locked
         --  first, this cannot lead to deadlock.

         if C.Stage = Created then

            --  Creation of the thread of control was deferred until
            --  activation. So create it now.

            if C.Base_Priority < Get_Priority (Self_ID) then
               Activate_Prio := Get_Priority (Self_ID);
            else
               Activate_Prio := C.Base_Priority;
            end if;

            C.Stage := Can_Activate;
            Self_ID.Activation_Count := Self_ID.Activation_Count + 1;

            System.Task_Primitives.Operations.Create_Task
              (C, Task_Wrapper'Address, C.Stack_Size, Activate_Prio, Success);

            if not Success then
               Self_ID.Activation_Count := Self_ID.Activation_Count - 1;
               Self_ID.Exception_To_Raise := To_Exception_ID
                 (System.Standard_Library.Tasking_Error_Def'Access);
            end if;

         end if;

         Unlock (C);

         C := C.Activation_Link;
      end loop;

      while Self_ID.Activation_Count > 0 loop
         if Self_ID.Pending_Action then
            if Self_ID.Pending_Priority_Change then
               Initialization.Change_Base_Priority (Self_ID);
            end if;

            exit when
               Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;
            Self_ID.Pending_Action := False;
         end if;
         Sleep (Self_ID);
      end loop;

      Unlock (Self_ID);

      --  After the activation, tasks should be removed from the Chain

      Chain_Access.all := null;

      Undefer_Abort (Self_ID);
      Utilities.Check_Exception (Self_ID);
   end Activate_Tasks;

   -------------------------------
   -- Expunge_Unactivated_Tasks --
   -------------------------------

   procedure Expunge_Unactivated_Tasks (Chain : in out Activation_Chain) is
      Self_ID : constant Task_ID := Self;
      C       : Task_ID;
      Temp    : Task_ID;
      Result  : Boolean;

   begin
      Defer_Abort (Self_ID);

      C := Task_ID (Chain);

      while C /= null loop

         pragma Assert (
           C.Stage <= Created or else
             Shutdown (
               "Trying to expunge task which went beyond CREATED stage"));

         Temp := C;
         C := C.Activation_Link;

         --  Now take care of decrementing parent's Await_Count and
         --  Awaited_Dependent_Count.

         Utilities.Complete (Self_ID, Temp);

         Initialization.Remove_From_All_Tasks_List
           (Temp, Result);
         pragma Assert (
           Result or else
            Shutdown (
             "Mismatch between All_Tasks_List and Chain to be expunged"));

         --  Task is out of Chain and All_Tasks_List. It is now safe to
         --  free the storage for the ATCB.

         System.Task_Primitives.Operations.Finalize_TCB (Temp);

      end loop;

      Chain := null;

      Undefer_Abort (Self_ID);

   end Expunge_Unactivated_Tasks;

   --------------------
   -- Current_Master --
   --------------------

   function Current_Master return Master_ID is
      Self_ID : constant Task_ID := Self;

   begin
      return Self_ID.Master_Within;
   end Current_Master;

   ----------------
   -- Leave_Task --
   ----------------

   procedure Leave_Task (Self_ID : Task_ID) is
      P                       : Task_ID;
      Saved_Pending_ATC_Level : ATC_Level_Base;

   begin
      pragma Assert (Self_ID = Self or else
        Shutdown ("Only the Self can execute this!"));

      Saved_Pending_ATC_Level := Self_ID.Pending_ATC_Level;

      --  We are about to lose our ATCB. Save special fields for final cleanup.

      P := Self_ID.Parent;

      if P /= null then
         Write_Lock (P);
         Write_Lock (Self_ID);

         --  If Self has a parent, then setting Self.Stage to Terminated and
         --  incrementing/decrementing P.Terminating_Dependent_Count
         --  have to be synchronized here and in Terminate_Dependents.
         --  This is done by locking the parent and dependent locks. So
         --  long as the parent lock is always locked first, this should not
         --  cause deadlock.

         Self_ID.Stage := Terminated;

         if P.Terminating_Dependent_Count > 0
           and then Self_ID.Master_of_Task = P.Master_Within
         then
            P.Terminating_Dependent_Count := P.Terminating_Dependent_Count - 1;

            if P.Terminating_Dependent_Count = 0 then
               Wakeup (P);
            end if;
         end if;

         Initialization.Finalize_Attributes_Link.all (Self_ID);

         Task_Specific_Data.Destroy_TSD (Self_ID.Compiler_Data);
         --  This should be the last thing done to a TCB, since the correct
         --  operation of compiled code may depend on it.

         Unlock (Self_ID);
         Unlock (P);

         --  WARNING - Once this lock is unlocked, it should be assumed that
         --  the ATCB has been deallocated. It should not be accessed again.

      else
         Write_Lock (Self_ID);
         Self_ID.Stage := Terminated;

         Initialization.Finalize_Attributes_Link.all (Self_ID);

         Task_Specific_Data.Destroy_TSD (Self_ID.Compiler_Data);
         --  This should be the last thing done to a TCB, since the correct
         --  operation of compiled code may depend on it.

         Unlock (Self_ID);
      end if;

   end Leave_Task;

   -------------------
   -- Complete_Task --
   -------------------

   procedure Complete_Task is
      Self_ID : constant Task_ID := Self;

   begin
      Defer_Abort (Self_ID);
      Utilities.Vulnerable_Complete_Task (Self_ID);
      Undefer_Abort (Self_ID);
   end Complete_Task;

   -------------------------
   -- Complete_Activation --
   -------------------------

   procedure Complete_Activation is
      Self_ID : constant Task_ID := Self;

   begin
      Defer_Abort (Self_ID);
      Utilities.Vulnerable_Complete_Activation
        (Self_ID, Completed => False);
      Undefer_Abort (Self_ID);
   end Complete_Activation;

   ------------------
   -- Enter_Master --
   ------------------

   procedure Enter_Master is
      Self_ID : constant Task_ID := Self;

   begin
      Self_ID.Master_Within :=
        Initialization.Increment_Master (Self_ID.Master_Within);
   end Enter_Master;

   ---------------------
   -- Complete_Master --
   ---------------------

   procedure Complete_Master is
      Self_ID : constant Task_ID := Self;
      C       : Task_ID;
      CM      : Master_ID := Self_ID.Master_Within;

   begin
      Defer_Abort (Self_ID);

      Write_Lock (Initialization.All_Tasks_L'Access);

      --  Cancel threads of dependent tasks that have not yet started
      --  activation.

      C := Initialization.All_Tasks_List;

      while C /= null loop
         if C.Parent = Self_ID and then C.Master_of_Task = CM and then
           C.Activator = Self_ID then
            Write_Lock (C);

            --  The only way that a dependent should not have been activated
            --  at this point is if the master was aborted before it could
            --  call Activate_Tasks. Abort such dependents.
            --  However, we exclude the case where the activator is
            --  different from the parent. This is because the activation of
            --  task using an allocator may have the same parent and the CM
            --  but should not be considered as being as terminated.

            if C.Stage = Created then
               Unlock (C);
               Utilities.Complete (Self_ID, C);

               --  Task is not yet activated. So, just complete and
               --  Mark it as Terminated.

               C.Stage := Terminated;

            else
               Unlock (C);
            end if;

         end if;

         C := C.All_Tasks_Link;
      end loop;

      --  Note that Awaited_Dependent_Count must be zero at this point. It is
      --  initialized to zero, this is the only code that can increment it
      --  when it is zero, and it will be zero again on exit from this routine.

      Write_Lock (Self_ID);
      C := Initialization.All_Tasks_List;

      while C /= null loop
         if C.Parent = Self_ID and then C.Master_of_Task = CM then
            Write_Lock (C);

            if C.Awake_Count /= 0 then
               Self_ID.Awaited_Dependent_Count :=
                 Self_ID.Awaited_Dependent_Count + 1;
            end if;

            Unlock (C);
         end if;

         C := C.All_Tasks_Link;
      end loop;

      --  Unlock Self.L here to avoid improper lock nesting; All_Tasks_L
      --  is always the outer lock to avoid deadlock, so we have to
      --  unlock Self.L before unlocking All_Tasks_L.

      Unlock (Self_ID);
      Unlock (Initialization.All_Tasks_L'Access);
      Write_Lock (Self_ID);

      --  If the task has been awakened due to abortion, this should
      --  cause the dependents to abort themselves and cause
      --  Awaited_Dependent_Count count to go to zero.

      if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
        and then Self_ID.Awaited_Dependent_Count /= 0
      then
         Unlock (Self_ID);
         Utilities.Abort_Dependents (Self_ID, Self_ID);
         Write_Lock (Self_ID);
      end if;

      if Self_ID.Awaited_Dependent_Count /= 0 then
         Self_ID.Stage := Await_Dependents;
      end if;

      while Self_ID.Awaited_Dependent_Count /= 0 loop
         Sleep (Self_ID);

         if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
           and then Self_ID.Awaited_Dependent_Count /= 0
         then
            --  The task may have been awakened to perform abortion.

            Unlock (Self_ID);
            Utilities.Abort_Dependents (Self_ID, Self_ID);
            Write_Lock (Self_ID);
         end if;

      end loop;

      Unlock (Self_ID);

      if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level then
         Undefer_Abort (Self_ID);
         return;

         --  Abort_Signal should be raised upon exit from at_end handler
      end if;

      Utilities.Terminate_Dependents (Self_ID, CM);

      Self_ID.Stage := Active;

      --  Make next master level up active. This needs to be done before
      --  decrementing the master level number, so that tasks finding
      --  themselves dependent on the current master level do not think that
      --  this master has been terminated (i.e. Stage=Await_Dependents and
      --  Awaited_Dependent_Count=0). This should be safe; the only thing that
      --  can affect the stage of a task after it has become active is either
      --  the task itself or abortion, which is deferred here.

      Self_ID.Master_Within :=
        Initialization.Decrement_Master (CM);

      --  Should not need protection; can only change if T executes an
      --  Enter_Master or a Complete_Master. T is only one task, and cannot
      --  execute these while executing this.

      Undefer_Abort (Self_ID);

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

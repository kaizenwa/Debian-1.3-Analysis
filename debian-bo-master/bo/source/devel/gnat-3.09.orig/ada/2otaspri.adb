------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . T A S K _ P R I M I T I V E S               --
--                                                                          --
--                                  B o d y                                 --
--                               (OS/2 Version)                             --
--                                                                          --
--                             $Revision: 1.16 $                            --
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

with Interfaces.C.Strings;    use Interfaces.C.Strings;

with Interfaces.OS2Lib;                 use Interfaces.OS2Lib;
with Interfaces.OS2Lib.Errors;          use Interfaces.OS2Lib.Errors;
with Interfaces.OS2Lib.Threads;         use Interfaces.OS2Lib.Threads;
with Interfaces.OS2Lib.Synchronization; use Interfaces.OS2Lib.Synchronization;

with System.Task_Clock.Machine_Specifics;

with System.Address_To_Access_Conversions;

with System.Storage_Elements; use System.Storage_Elements;
with System.Io;               use System.Io;

package body System.Task_Primitives is

   package Address_TCB_Ptr_Ptr_Conversion is
     new Address_To_Access_Conversions (TCB_Ptr);

   package Address_TCB_Ptr_Conversion is
      new Address_To_Access_Conversions (Task_Control_Block);

   package Address_Boolean_Conversion is
     new Address_To_Access_Conversions (Boolean);

   subtype TCB_Ptr_Ptr is Address_TCB_Ptr_Ptr_Conversion.Object_Pointer;

   --  The OS/2 DosAllocThreadLocalMemory API is used to allocate our TCB_Ptr.

   --  This API reserves a small range of virtual addresses that is backed
   --  by different physical memory for each running thread. In this case we
   --  create a pointer at a fixed address that points to the TCB_Ptr for the
   --  running thread. So all threads will be able to query and update their
   --  own TCB_Ptr without destroying the TCB_Ptr of other threads.

   Thread_Local_Data_Ptr : TCB_Ptr_Ptr;
   --  Pointer to Thread Local Data, which contains TCB_Ptr of any thread.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Booster (Info : PVOID);
   --  Needs comments ???

   procedure Initialize_Thread_Local_Data
     (Thread_Local_Data_Ptr : out TCB_Ptr_Ptr);
   --  Initialize the Thread_Local_Data_Ptr to point to the TCB

   -------------------------
   -- Initialize_LL_Tasks --
   -------------------------

   procedure Initialize_LL_Tasks (T : TCB_Ptr) is
   begin
      T.all := (LL_Entry_Point  => null,
                LL_Arg          => Null_Address,
                Thread          => 1,            --  By definition
                Active_Priority => Default_Priority,
                Aborted         => False);

      --  Set our copy of the TCB_Ptr. This data is local to this
      --  thread (the main thread), see explanation above.

      Thread_Local_Data_Ptr.all := T;
   end Initialize_LL_Tasks;

   ----------------------------------
   -- Initialize_Thread_Local_Data --
   ----------------------------------

   procedure Initialize_Thread_Local_Data
     (Thread_Local_Data_Ptr : out TCB_Ptr_Ptr)
   is
      use Address_TCB_Ptr_Ptr_Conversion;

      --  TLD is an abbreviation of Thread Local Data

      TLD_Ptr     : aliased PULONG;
      TLD_Address : System.Address;

   begin
      Must_Not_Fail (DosAllocThreadLocalMemory (1, TLD_Ptr'Access));

      --  TLD_Address will contain the address of the thread local memory

      TLD_Address := TLD_Ptr.all'Address;

      --  The address is converted to a pointer to the TLD and stored
      --  in Thread_Local_Data_Ptr

      Thread_Local_Data_Ptr := To_Pointer (TLD_Address);
   end Initialize_Thread_Local_Data;

   ----------
   -- Self --
   ----------

   function Self return TCB_Ptr is
   begin
      return Thread_Local_Data_Ptr.all;
   end Self;

   -------------
   -- Booster --
   -------------

   procedure Booster (Info : PVOID) is

      use Address_TCB_Ptr_Conversion;

      My_TCB_Ptr : TCB_Ptr renames Thread_Local_Data_Ptr.all;

   begin
      --  Set our copy of the TCB_Ptr. This data is local to this
      --  thread (the main thread), see explanation above.

      My_TCB_Ptr := To_Pointer (Info).all'Access;

      --  Here we go!

      My_TCB_Ptr.LL_Entry_Point (My_TCB_Ptr.LL_Arg);

   end Booster;

   --------------------
   -- Create_LL_Task --
   --------------------

   procedure Create_LL_Task
     (Priority       : System.Priority;
      Stack_Size     : Task_Storage_Size;
      Task_Info      : System.Task_Info.Task_Info_Type;
      LL_Entry_Point : LL_Task_Procedure_Access;
      Arg            : Address;
      T              : TCB_Ptr)
   is
      use Interfaces.C;
      use Address_TCB_Ptr_Conversion;

      Result : Interfaces.OS2Lib.APIRET;
      Id     : aliased TID;
      Junk1  : PVOID; -- TBSL ???
      Junk2  : ULONG; -- TBSL ???

   begin
      --  Step 1: Create the thread in blocked mode

      Junk1  := Address_TCB_Ptr_Conversion.To_Address (T.all'Access);
      Junk2  := ULONG (Stack_Size);
      Result := Interfaces.OS2Lib.Threads.DosCreateThread
                   (F_ptid   => Id'Unchecked_Access,
                    pfn      => LL_Task_Procedure_Access'(Booster'Access),
                    param    => Junk1,
                    flag     => Block_child + Commit_stack,
                    cbStack  => Junk2);
      if Result /= NO_ERROR then
         raise Storage_error;
      end if;

      --  Step 2: set its TCB

      T.all := (LL_Entry_Point => LL_Entry_Point,
                LL_Arg          => Arg,
                Thread          => Id,
                Active_Priority => Priority,
                Aborted         => False);

      --  Step 3: set its priority (child has inherited priority from parent)

      Must_Not_Fail
        (DosSetPriority (Scope   => PRTYS_THREAD,
                         Class   => PRTYC_NOCHANGE,
                         Delta_P => long (Priority - Get_Own_Priority),
                         PorTid  => Id));

      --  Step 4: Now, start it for good:

      Must_Not_Fail (DosResumeThread (Id));

   end Create_LL_Task;

   ------------------
   -- Exit_LL_Task --
   ------------------

   procedure Exit_LL_Task is
   begin
      DosExit (EXIT_THREAD, 0);
   end Exit_LL_Task;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   procedure Initialize_Lock (Prio : Integer; L : in out Lock) is
   begin
      if DosCreateMutexSem (Null_Ptr, L.Mutex'Unchecked_Access, 0, False32)
                                            /= NO_ERROR
      then
         raise Storage_Error;
      end if;

      L.Priority := Prio;
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : in out Lock) is
   begin
      Must_Not_Fail (DosCloseMutexSem (L.Mutex));
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock (L : in out Lock; Ceiling_Violation : out Boolean) is
   begin
      L.Owner_Priority := Get_Own_Priority;

      if L.Priority < L.Owner_Priority then
         Ceiling_Violation := True;
         return;
      end if;

      Must_Not_Fail (DosRequestMutexSem (L.Mutex, SEM_INDEFINITE_WAIT));

      Ceiling_Violation := False;

      if L.Priority > L.Owner_Priority then
         Set_Own_Priority (L.Priority);
      end if;
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   --  Not worth worrying about distinguishing read and write locks until
   --  OS/2 supports multi-processing, since no advantage would be gained.

   procedure Read_Lock (L : in out Lock; Ceiling_Violation : out Boolean)
      renames Write_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : in out Lock) is
   begin
      if L.Owner_Priority /= L.Priority then
         Set_Own_Priority (L.Owner_Priority);
      end if;

      Must_Not_Fail (DosReleaseMutexSem (L.Mutex));
   end Unlock;

   -----------------------
   -- Initalialize_Cond --
   -----------------------

   procedure Initialize_Cond (Cond : in out Condition_Variable) is
      Temporary : aliased HEV;
      --  This temporary is needed for two reasons:
      --  1) Since DosCreateSem operates on an PHEV, not HEV, it is not
      --     derived and thus not available on type Condition_variable.
      --  2) Moreover we cannot have an aliased view of Cond, required
      --     for 'Access.

   begin
      Must_Not_Fail
        (DosCreateEventSem (Null_Ptr, Temporary'Unchecked_Access, 0, True32));
      Cond := Condition_Variable (Temporary);
   end Initialize_Cond;

   -------------------
   -- Finalize_Cond --
   -------------------

   --  No such problem here, DosCloseEventSem has been derived.
   --  What does such refer to in above comment???

   procedure Finalize_Cond (Cond : in out Condition_Variable) is
   begin
      Must_Not_Fail (DosCloseEventSem (Cond));
   end Finalize_Cond;

   ---------------
   -- Cond_Wait --
   ---------------

   --  Pre-assertion: Cond is posted
   --                 L is locked.

   --  Post-assertion: Cond is posted
   --                  L is locked.

   procedure Cond_Wait
     (Cond : in out Condition_Variable;
      L    : in out Lock)
   is
      Count : aliased ULONG; -- Unused
      Error : Boolean;
   begin
      --  Must reset Cond BEFORE L is unlocked.

      Must_Not_Fail (DosResetEventSem (Cond, Count'Unchecked_Access));
      Unlock (L);

      --  No problem if we are interrupted here: if the condition is signaled,
      --  DosWaitEventSem will simply not block

      Must_Not_Fail (DosWaitEventSem (Cond, SEM_INDEFINITE_WAIT));

      --  Since L was previously accquired, Error cannot be false:

      Write_Lock (L, Error);
   end Cond_Wait;

   ---------------------
   -- Cond_Timed_Wait --
   ---------------------

   --  Pre-assertion: Cond is posted
   --                 L is locked.

   --  Post-assertion: Cond is posted
   --                  L is locked.

   procedure Cond_Timed_Wait
     (Cond      : in out Condition_Variable;
      L         : in out Lock;
      Abs_Time  : System.Task_Clock.Stimespec;
      Timed_Out : out Boolean)
   is
      use System.Task_Clock;
      use System.Task_Clock.Machine_Specifics;

      Count    : aliased ULONG; -- Unused
      Time_Out : ULONG;
      Error    : Boolean;
      Rel_Time : Stimespec;

   begin
      --  Change Abs_time to a relative delay.

      --  Be careful not to reintroduce the race condition that gave birth
      --  to delay until.

      Must_Not_Fail (DosEnterCritSec);
      Rel_Time := Abs_Time - Clock;
      Must_Not_Fail (DosExitCritSec);

      --  Must reset Cond BEFORE L is unlocked.

      Must_Not_Fail (DosResetEventSem (Cond, Count'Unchecked_Access));
      Unlock (L);

      --  No problem if we are interrupted here: if the condition is signaled,
      --  DosWaitEventSem will simply not block

      if Rel_Time <= Stimespec_Zero then
         Timed_Out := True;
      else
         Time_Out := ULONG (Stimespec_Seconds  (Rel_Time)) * 1000 +
                     ULONG (Stimespec_NSeconds (Rel_Time) / 1E6);
         Timed_Out :=  DosWaitEventSem (Cond, Time_Out) = ERROR_TIMEOUT;
      end if;

      --  Since L was previously accquired, Error cannot be false

      Write_Lock (L, Error);

      --  Ensure post-condition

      if Timed_Out then
         Sem_Must_Not_Fail (DosPostEventSem (Cond));
      end if;
   end Cond_Timed_Wait;

   -----------------
   -- Cond_Signal --
   -----------------

   procedure Cond_Signal (Cond : in out Condition_Variable) is
   begin
      Sem_Must_Not_Fail (DosPostEventSem (Cond));
   end Cond_Signal;

   ------------------
   -- Set_Priority --
   ------------------

   --  Note: Currently, we have only 32 priorities, all in Regular Class.
   --  Priority level 31 is the only value for Interrupt_Priority. (see
   --  package System). A better choice (for OS/2) would be to have 32
   --  priorities in Regular class for subtype Priority and 32 priorities
   --  in Time-critical class for Interrupt_Priority ???

   procedure Set_Priority (T : TCB_Ptr; Prio : Integer) is
      use Interfaces.C;

   begin
      Must_Not_Fail
        (DosSetPriority (Scope   => PRTYS_THREAD,
                         Class   => PRTYC_NOCHANGE,
                         Delta_P => long (Prio - T.Active_Priority),
                         PorTid  => T.Thread));
      T.Active_Priority := Prio;
   end Set_Priority;

   ----------------------
   -- Set_Own_Priority --
   ----------------------

   procedure Set_Own_Priority (Prio : Integer) is
   begin
      Set_Priority (Self, Prio);
   end Set_Own_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : TCB_Ptr) return Integer is
   begin
      return T.Active_Priority;
   end Get_Priority;

   ----------------------
   -- Get_Own_Priority --
   ----------------------

   function Get_Own_Priority return Integer is
   begin
      return Get_Priority (Self);
   end Get_Own_Priority;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : TCB_Ptr) is
   begin
      T.Aborted := True;
   end Abort_Task;

   ----------------
   -- Test_Abort --
   ----------------

   Current_Abort_Handler : Abort_Handler_Pointer;

   procedure Test_Abort is
   begin
      if Self.Aborted then
         Current_Abort_Handler (0);   -- Parameter not used
      end if;
   end Test_Abort;

   ---------------------------
   -- Install_Abort_Handler --
   ---------------------------

   procedure Install_Abort_Handler (Handler : Abort_Handler_Pointer) is
   begin
      Current_Abort_Handler := Handler;
   end Install_Abort_Handler;

   ---------------------------
   -- Install_Error_Handler --
   ---------------------------

   procedure Install_Error_Handler (Handler : Address) is
   begin
      null;
   end Install_Error_Handler;

   -----------------
   -- Signal_Task --
   -----------------

   procedure Signal_Task (T : TCB_Ptr; I : Interrupt_ID) is
   begin
      raise Program_Error;
   end Signal_Task;

   ---------------------
   -- Wait_For_Signal --
   ---------------------

   procedure Wait_for_Signal (I : Interrupt_ID) is
   begin
      raise PROGRAM_ERROR;
   end Wait_for_Signal;

   ---------------------
   -- Reserved_Signal --
   ---------------------

   function Reserved_Signal (I : Interrupt_ID) return Boolean is
   begin
      return False;
   end Reserved_Signal;

   ------------------
   -- Test_And_Set --
   ------------------

   Test_And_Set_Mutex : Lock;
   --  Lock used by Test_And_Set procedure

   -------------------------
   -- Initialize_TAS_Cell --
   -------------------------

   procedure Initialize_TAS_Cell (Cell : out TAS_Cell) is
   begin
      Cell.Value := False;
   end Initialize_TAS_Cell;

   -----------------------
   -- Finalize_TAS_Cell --
   -----------------------

   procedure Finalize_TAS_Cell   (Cell : in out TAS_Cell) is
   begin
      null;
   end Finalize_TAS_Cell;

   -----------
   -- Clear --
   -----------

   --  This was not atomic with respect to another Test_and_Set in the
   --  original code.  Need it be???

   procedure Clear (Cell : in out TAS_Cell) is
   begin
      Cell.Value := False;
   end Clear;

   ------------
   -- Is_Set --
   ------------

   --  This was not atomic with respect to another Test_and_Set in the
   --  original code.  Need it be???

   function Is_Set (Cell : in TAS_Cell) return Boolean is
   begin
      return Cell.Value;
   end Is_Set;

   ------------------
   -- Test_And_Set --
   ------------------

   procedure Test_And_Set (Cell : in out TAS_Cell; Result : out Boolean) is
      Error : Boolean;
   begin
      Write_Lock (Test_And_Set_Mutex, Error);

      if Cell.Value then
         Result := False;
      else
         Result :=  True;
         Cell.Value := True;
      end if;

      Unlock (Test_And_Set_Mutex);
   end Test_And_Set;

   ---------------
   -- LL_Assert --
   ---------------

   procedure LL_Assert (B : Boolean; M : String) is
   begin
      if not B then
         Put ("Failed assertion: ");
         Put (M);
         Put ('.');
         New_Line;
         pragma Assert (False);
      end if;
   end LL_Assert;

begin
   Initialize_Lock (System.Priority'Last, Test_And_Set_Mutex);
   Initialize_Thread_Local_Data (Thread_Local_Data_Ptr);
end System.Task_Primitives;

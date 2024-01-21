------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . T A S K _ P R I M I T I V E S               --
--                                                                          --
--                                  B o d y                                 --
--                              (Win32 Version)                             --
--                                                                          --
--                             $Revision: 1.1 $                             --
--                                                                          --
--          Copyright (c) 1993,1994,1995 NYU, All Rights Reserved           --
--                                                                          --
--  GNARL is free software; you can redistribute it and/or modify it  under --
--  terms  of  the  GNU  Library General Public License as published by the --
--  Free Software Foundation; either version 2,  or (at  your  option)  any --
--  later  version.   GNARL is distributed in the hope that it will be use- --
--  ful, but but WITHOUT ANY WARRANTY; without even the implied warranty of --
--  MERCHANTABILITY  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Gen- --
--  eral Library Public License for more details.  You should have received --
--  a  copy of the GNU Library General Public License along with GNARL; see --
--  file COPYING. If not, write to the Free Software Foundation,  675  Mass --
--  Ave, Cambridge, MA 02139, USA.                                          --
--                                                                          --
--  Calls that need to be results-tested:                                   --
--                                                                          --
--        Function               Good                Failed                 --
--    SetThreadPriority            1                    0                   --
--    TlsSetValue                  1                    0                   --
--    ResumeThread           previous resume count   0xffffffff             --
--    CloseHandle                  1                    0                   --
--    WaitForSingleObject      waking event          WAIT_FAILED            --
--    ReleaseMutex                 1                    0                   --
--    ResetEvent                   1                    0                   --
--    SetEvent                     1                    0                   --
--                                                                          --
------------------------------------------------------------------------------

with System.Task_Clock.Machine_Specifics;
with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
with System.Io;               use System.Io;

with Win_Task;                 use Win_Task;
with Win_Task.Errors;          use Win_Task.Errors;
with Win_Task.Threads;         use Win_Task.Threads;
with Win_Task.Synchronization; use Win_Task.Synchronization;

with Unchecked_Conversion;

package body System.Task_Primitives is

   --  The main thread handle (must be a illegal value);
   TlsIndex          : DWORD;
   --  The thread local storage index

   function To_TCB_Ptr is new
      Unchecked_Conversion (PVOID, TCB_Ptr);

   function From_TCB_Ptr is new
      Unchecked_Conversion (TCB_Ptr, PVOID);

   -------------------------
   -- Initialize_LL_Tasks --
   -------------------------

   procedure Initialize_LL_Tasks (T : TCB_Ptr) is
   begin

      T.all := (LL_Entry_Point  => null,
                LL_Arg          => Null_Address,
                Thread          => GetCurrentThread,
                Active_Priority => Default_Priority,
                Aborted         => False);

      TlsIndex := TlsAlloc;
      Must_Not_Fail_Bool
         (TlsSetValue (TlsIndex, From_TCB_Ptr (T.all'Access)));

   end Initialize_LL_Tasks;

   ----------
   -- Self --
   ----------

   --  When a task is created, the body of the Windows NT thread is the
   --  procedure Booster, which in turn calls the actual task body.
   --  Booster has a thread local variable where the TCB pointer is stored.

   function Self return TCB_Ptr is

   begin

      return To_TCB_Ptr (TlsGetValue (TlsIndex));

   end Self;

   -------------
   -- Booster --
   -------------

   function Booster (Info : PVOID) return DWORD;
   pragma Convention (Stdcall, Booster);
   --  See description above for Self function

   function Booster (Info : PVOID) return DWORD is

      pTCB  : TCB_Ptr;

   begin

      pTCB := To_TCB_Ptr (Info);

      Must_Not_Fail_Bool (TlsSetValue (TlsIndex, Info));

      --  Here we go!

      pTCB.LL_Entry_Point (pTCB.LL_Arg);

      return 0;

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

      ThreadAttributes  : aliased SECURITY_ATTRIBUTES :=
         (SECURITY_ATTRIBUTES'Size / System.Storage_Unit, Null_Void, False);

      hTask             : HANDLE;
      TaskId            : aliased DWORD;
      pTaskParameter    : PVOID;
      dwStackSize       : DWORD;
      result            : DWORD;
      Entry_Point       : PTHREAD_START_ROUTINE;

   begin

      pTaskParameter := From_TCB_Ptr (T.all'Access);

      dwStackSize    := DWORD (Stack_Size);
      Entry_Point    := Booster'Access;

      hTask := CreateThread (
         ThreadAttributes'Unchecked_Access,
            dwStackSize,
               PTHREAD_START_ROUTINE (Entry_Point),
                  pTaskParameter,
                     DWORD (Create_Suspended),
                        TaskId'Unchecked_Access);

--    hTask := BeginThreadEx (
--       ThreadAttributes'Unchecked_Access,
--          dwStackSize,
--             PTHREAD_START_ROUTINE (Entry_Point),
--                pTaskParameter,
--                   DWORD (Create_Suspended),
--                      TaskId'Unchecked_Access);

      --  Step 1: Create the thread in blocked mode

      if hTask = 0 then
         raise Storage_error;
      end if;

      --  Step 2: set its TCB

      T.all := (LL_Entry_Point  => LL_Entry_Point,
                LL_Arg          => Arg,
                Thread          => hTask,
                Active_Priority => Priority,
                Aborted         => False);

      --  Step 3: set its priority (child has inherited priority from parent)

--  FIXME: the priority and priority class should be bumped
--   Must_Not_Fail_Bool (SetThreadPriority (hTask, int (Priority)));

      --  Step 4: Now, start it for good:

      result := ResumeThread (hTask);
--  FIXME: should test this result   Must_Not_Fail (ResumeThread (hTask));

   end Create_LL_Task;

   ------------------
   -- Exit_LL_Task --
   ------------------

   procedure Exit_LL_Task is
   begin
      ExitThread (0);
--    EndThreadEx (0);
   end Exit_LL_Task;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   procedure Initialize_Lock (Prio : Integer; L : in out Lock) is
      MutexAttributes   : aliased SECURITY_ATTRIBUTES :=
         (SECURITY_ATTRIBUTES'Size / System.Storage_Unit, Null_Void, False);
   begin
      L.Mutex := CreateMutex (
         MutexAttributes'Unchecked_Access, False, Null_Ptr);

      if L.Mutex = 0 then
         raise Storage_Error;
      end if;

      L.Priority := Prio;
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : in out Lock) is
   begin
      Must_Not_Fail_Bool (CloseHandle (L.Mutex));
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

      Must_Not_Fail (WaitForSingleObject (L.Mutex, Wait_Infinite));
--      Must_Not_Fail (WaitForSingleObjectEx (L.Mutex, Wait_Infinite, False));

      Ceiling_Violation := False;

      if L.Priority > L.Owner_Priority then
--  LabTek : temporary fix to prevent constraint error
         declare
            Limited_Priority : Priority;
         begin
            if L.Priority > Standard'Max_Priority then
               Limited_Priority := Standard'Max_Priority;
            else
               Limited_Priority := L.Priority;
            end if;
--            Set_Own_Priority (L.Priority);
            Set_Own_Priority (Limited_Priority);
         end;
      end if;
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   --  Not worth worrying about distinguishing read and write locks until
   --  Windows NT supports multi-processing, since no advantage would be
   --  gained.

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
      Must_Not_Fail_Bool (ReleaseMutex (L.Mutex));
   end Unlock;

   -----------------------
   -- Initalialize_Cond --
   -----------------------

   procedure Initialize_Cond (Cond : in out Condition_Variable) is

      hEvent            : HANDLE;
      EventAttributes   : PSECURITY_ATTRIBUTES;

   begin
      hEvent := CreateEvent (EventAttributes, True, False, Null_Ptr);
      pragma Assert (hEvent /= 0);
      Cond := Condition_Variable (hEvent);
   end Initialize_Cond;

   -------------------
   -- Finalize_Cond --
   -------------------

   --  No such problem here, DosCloseEventSem has been derived.
   --  What does such refer to in above comment???

   procedure Finalize_Cond (Cond : in out Condition_Variable) is
   begin
      Must_Not_Fail_Bool (CloseHandle (HANDLE (Cond)));
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
      Error : Boolean;
   begin
      --  Must reset Cond BEFORE L is unlocked.

      Must_Not_Fail_Bool (ResetEvent (HANDLE (Cond)));
      Unlock (L);

      --  No problem if we are interrupted here: if the condition is signaled,
      --  WaitForSingleObject will simply not block

      Must_Not_Fail (WaitForSingleObject (HANDLE (Cond), Wait_Infinite));

      --  Since L was previously accquired, Error cannot be false:

      Write_Lock (L, Error);

   end Cond_Wait;

   Timer_CriticalSection : aliased CRITICAL_SECTION;

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

      Time_Out : DWORD;
      Error    : Boolean;
      Rel_Time : Stimespec;

   begin
      --  Change Abs_time to a relative delay.

      --  Be careful not to reintroduce the race condition that gave birth
      --  to delay until.

      EnterCriticalSection (Timer_CriticalSection'Unchecked_Access);
      Rel_Time := Abs_Time - Clock;
      LeaveCriticalSection (Timer_CriticalSection'Unchecked_Access);

      --  Must reset Cond BEFORE L is unlocked.

      Must_Not_Fail_Bool (ResetEvent (HANDLE (Cond)));
      Unlock (L);

      --  No problem if we are interrupted here: if the condition is signaled,
      --  WaitForSingleObject will simply not block

      if Rel_Time <= Stimespec_Zero then
         Timed_Out := True;
      else
         Time_Out := DWORD (Stimespec_Seconds  (Rel_Time)) * 1000 +
               DWORD (Stimespec_NSeconds (Rel_Time) / 1E6);
--         Win_Task.Sleep (Time_Out);
         Timed_Out :=
            WaitForSingleObject (HANDLE (Cond), Time_Out) = WAIT_TIMEOUT;

      end if;

      --  Since L was previously accquired, Error cannot be false

      Write_Lock (L, Error);

      --  Ensure post-condition

      if Timed_Out then
         Must_Not_Fail_Bool (SetEvent (HANDLE (Cond)));
      end if;
   end Cond_Timed_Wait;

   -----------------
   -- Cond_Signal --
   -----------------

   procedure Cond_Signal (Cond : in out Condition_Variable) is
   begin
      Must_Not_Fail_Bool (SetEvent (HANDLE (Cond)));
   end Cond_Signal;

   ------------------
   -- Set_Priority --
   ------------------

   --  Note: Currently, we have only 32 priorities, all in Regular Class.
   --  Priority level 31 is the only value for Interrupt_Priority. (see
   --  package System). A better choice (for Windows NT) would be to have 32
   --  priorities in Regular class for subtype Priority and 32 priorities
   --  in Time-critical class for Interrupt_Priority ???

   procedure Set_Priority (T : TCB_Ptr; Prio : Integer) is
      use Interfaces.C;
      New_Priority   : int;
   begin

      case Priority (Prio) is
         when Priority'First .. 4 =>
            New_Priority := Thread_Priority_Idle;
         when 5 .. 9 =>
            New_Priority := Thread_Priority_Lowest;
         when 10 .. Default_Priority - 1 =>
            New_Priority := Thread_Priority_Below_Normal;
         when Default_Priority =>
            New_Priority := Thread_Priority_Normal;
         when Default_Priority + 1 .. 20 =>
            New_Priority := Thread_Priority_Above_Normal;
         when 21 .. 25 =>
            New_Priority := Thread_Priority_Highest;
         when 26 .. Priority'Last =>
            New_Priority := Thread_Priority_Time_Critical;
      end case;
      
--  FIXME: the priority and priority class should be bumped
--      Must_Not_Fail_Bool (SetThreadPriority (T.Thread, New_Priority));

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
      raise Program_Error;
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
   InitializeCriticalSection (Timer_CriticalSection'Unchecked_Access);

end System.Task_Primitives;

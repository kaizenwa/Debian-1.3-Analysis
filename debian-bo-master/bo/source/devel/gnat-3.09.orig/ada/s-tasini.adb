------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--         S Y S T E M . T A S K I N G . I N I T I A L I Z A T I O N        --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.10 $                            --
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

--  This package provides overall initialization of the tasking portion
--  of the RTS.  This package must be elaborated before any tasking
--  features are used.  It also contains initialization for
--  Ada Task Control Block (ATCB) records.

with System.Task_Primitives; use System.Task_Primitives;

with Ada.Exceptions;
--  Used for, Null_Id

with System.Compiler_Exceptions;
--  Used for, Notify_Exception

with System.Tasking_Soft_Links;
--  Used for, Abort_Defer, Abort_Undefer, Get_TSD_Address
--  These are procedure pointers to non-tasking routines that use
--  task specific data.  In the absence of tasking, these routines
--  refer to global data.  In the presense of tasking, they must be
--  replaced with pointers to task-specific versions.

with System.Task_Memory;
--  Used for, Task_Memory.Low_Level_New,
--            Task_Memory.Unsafe_Low_Level_New,
--            Task_Memory.Low_Level_Free

with System.Task_Specific_Data;
--  Used for, Create_TSD, Destroy_TSD
--  This package provides initialization routines for task specific data.
--  The GNARL must call these to be sure that all non-tasking
--  Ada constructs will work.

pragma Elaborate_All (System.Task_Primitives);
pragma Elaborate_All (System.Task_Memory);

pragma Elaborate_All (System.Tasking_Soft_Links);
--  This must be elaborated first, to prevent its initialization of
--  the global procedure pointers from overwriting the pointers installed
--  by Stages.

with Unchecked_Deallocation;

package body System.Tasking.Initialization is

   Global_Task_Lock : Lock;
   --  This is a global lock; it is used to execute in mutual exclusion
   --  from all other tasks.  It is only used by Task_Lock and
   --  Task_Unlock.

   -----------------------------------------------------------------
   -- Tasking versions of services needed by non-tasking programs --
   -----------------------------------------------------------------

   procedure Task_Lock;
   --  Locks out other tasks. Preceding a section of code by Task_Lock and
   --  following it by Task_Unlock creates a critical region. This is used
   --  for ensuring that a region of non-tasking code (such as code used to
   --  allocate memory) is tasking safe. Note that it is valid for calls to
   --  Task_Lock/Task_Unlock to be nested, and this must work properly, i.e.
   --  only the corresponding outer level Task_Unlock will actually unlock.

   procedure Task_Unlock;
   --  Releases lock previously set by call to Task_Lock. In the nested case,
   --  all nested locks must be released before other tasks competing for the
   --  tasking lock are released.


   function  Get_Jmpbuf_Address return  Address;
   procedure Set_Jmpbuf_Address (Addr : Address);

   function  Get_GNAT_Exception return  Address;
   procedure Set_GNAT_Exception (Addr : Address);

   function  Get_Sec_Stack_Addr return  Address;
   procedure Set_Sec_Stack_Addr (Addr : Address);

   function  Get_Exc_Stack_Addr return Address;
   procedure Set_Exc_Stack_Addr (Addr : Address);

   function  Get_Message_Length return Natural;
   procedure Set_Message_Length (Len : Natural);

   function  Get_Message_Addr return Address;
   procedure Set_Message_Addr (Addr : Address);


   ----------------------------
   -- Tasking Initialization --
   ----------------------------

   procedure Init_RTS (Main_Task_Priority : System.Priority);
   --  This procedure initializes the GNARL.  This includes creating
   --  data structures to make the initial thread into the environment
   --  task, setting up handlers for ATC and errors, and
   --  installing tasking versions of certain operations used by the
   --  compiler.  Init_RTS is called during elaboration.

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

   --------------------------
   -- Change_Base_Priority --
   --------------------------

   procedure Change_Base_Priority (T : Task_ID) is

   begin
      --  check for ceiling violations ???
      T.Pending_Priority_Change := False;
      T.Base_Priority := T.New_Base_Priority;
      T.Current_Priority := T.Base_Priority;
      Set_Priority (T.LL_TCB'Access, T.Current_Priority);
   end Change_Base_Priority;

   ----------------------
   -- Decrement_Master --
   ----------------------

   function Decrement_Master (M : Master_ID) return Master_ID is
   begin
      return M - 1;
   end Decrement_Master;

   --------------------
   -- Defer_Abortion --
   --------------------

   procedure Defer_Abortion is
      T : Task_ID := Self;

   begin
      T.Deferral_Level := T.Deferral_Level + 1;
   end Defer_Abortion;

   ---------------
   -- Free_ATCB --
   ---------------

   procedure Free_ATCB (T : in out Task_ID) is
      procedure Free is new Unchecked_Deallocation (
        Ada_Task_Control_Block, Task_ID);

   begin
      Finalize_Lock (T.L);
      Finalize_Cond (T.Cond);
      Free (T);
   end Free_ATCB;

   ----------------------
   -- Increment_Master --
   ----------------------

   function Increment_Master (M : Master_ID) return Master_ID is
   begin
      return M + 1;
   end Increment_Master;

   ---------------------
   -- Initialize_ATCB --
   ---------------------

   procedure Initialize_ATCB
     (T     : Task_ID;
      Init  : ATCB_Init)
   is
      Error : Boolean;
   begin
      --  Initialize all fields of the TCB

      Initialize_Lock (System.Any_Priority'Last, T.L);
      Initialize_Cond (T.Cond);
      T.Activation_Count := 0;
      T.Awake_Count := 1;                       --  Counting this task.
      T.Awaited_Dependent_Count := 0;
      T.Terminating_Dependent_Count := 0;
      T.Pending_Action := False;
      T.Pending_ATC_Level := ATC_Level_Infinity;
      T.ATC_Nesting_Level := 1;                 --  1 deep; 0 = abnormal.
      T.Deferral_Level := 1;                    --  Start out deferred.
      T.Stage := Created;
      T.Global_Task_Lock_Nesting := 0;
      T.Exception_To_Raise := Ada.Exceptions.Null_Id;
      T.Accepting := Not_Accepting;
      T.Aborting := False;
      T.Call := null;
      T.Elaborated := Init.Elaborated;
      T.Parent := Init.Parent;
      T.Task_Entry_Point := Init.Task_Entry_Point;
      T.Task_Arg := Init.Task_Arg;
      T.Stack_Size := Init.Stack_Size;
      T.Task_Info := Init.Task_Info;
      T.Current_Priority := Init.Current_Priority;
      T.Base_Priority := Init.Base_Priority;
      T.Pending_Priority_Change := False;
      T.Activator := Init.Activator;
      T.Master_of_Task := Init.Master_of_Task;
      T.Master_Within := Increment_Master (Init.Master_of_Task);
      T.Terminate_Alternative := false;

      for J in 1 .. T.Entry_Num loop
         T.Entry_Queues (J).Head := null;
         T.Entry_Queues (J).Tail := null;
      end loop;

      for L in T.Entry_Calls'Range loop
         T.Entry_Calls (L).Next := null;
         T.Entry_Calls (L).Self := T;
         T.Entry_Calls (L).Level := L;
      end loop;

      --  Link the task into the list of all tasks.

      if T.Parent /= null then
         Defer_Abortion;
         Write_Lock (All_Tasks_L, Error);
      end if;

      T.All_Tasks_Link := All_Tasks_List;
      All_Tasks_List := T;

      if T.Parent /= null then
         Unlock (All_Tasks_L);
         Undefer_Abortion;
      end if;
   end Initialize_ATCB;

   -----------------
   -- Init_Master --
   -----------------

   procedure Init_Master (M : out Master_ID) is
   begin
      M := 0;
   end Init_Master;

   --------------
   -- Init_RTS --
   --------------

   procedure Init_RTS (Main_Task_Priority : System.Priority) is
      T    : Task_ID;
      Init : ATCB_Init;

      package TSL renames System.Tasking_Soft_Links;

      NT_Sec_Stack_Addr : Address := TSL.Get_Sec_Stack_Addr.all;
      NT_Exc_Stack_Addr : Address := TSL.Get_Exc_Stack_Addr.all;
      NT_GNAT_Exception : Address := TSL.Get_GNAT_Exception.all;
      NT_Jmpbuf_Address : Address := TSL.Get_Jmpbuf_Address.all;
      NT_Message_Length : Natural := TSL.Get_Message_Length.all;
      NT_Message_Addr   : Address := TSL.Get_Message_Addr.all;

   begin
      All_Tasks_List := null;
      Init.Entry_Num := 0;
      Init.Parent := null;

      Init.Task_Entry_Point := null;

      Init.Stack_Size := 0;
      Init.Activator := null;
      Init_Master (Init.Master_of_Task);
      Init.Elaborated := null;
      if Main_Task_Priority = Unspecified_Priority then
         Init.Base_Priority := Default_Priority;
         Init.Current_Priority := Default_Priority;
      else
         Init.Base_Priority := Main_Task_Priority;
         Init.Current_Priority := Main_Task_Priority;
      end if;

      T := Unsafe_New_ATCB (Init);
      Initialize_ATCB (T, Init);
      T.LL_TCB.LL_Arg := T.all'Address;

      Initialize_LL_Tasks (T.LL_TCB'Access);
      T.Stage := Active;

      Task_Specific_Data.Create_TSD (T.Compiler_Data);
      --  This needs to be done as early as possible in the creation
      --  of a task, since the operation of Ada code within the task may
      --  depend on task specific data.


      --  The allocation of the initial task ATCB is different from
      --  that of subsequent ATCBs, which are allocated with ATCB.New_ATCB.
      --  New_ATCB performs all of the functions of Unsafe_New_ATCB
      --  and Initialize_ATCB.  However, it uses GNULLI operations, which
      --  should not be called until after Initialize_LL_Tasks.  Since
      --  Initialize_LL_Tasks needs the initial ATCB, New_ATCB was broken
      --  down into two parts, the first of which allocates the ATCB without
      --  calling any GNULLI operations.

      Set_Own_Priority (T.Current_Priority);

      Initialize_Lock (Any_Priority'Last, All_Tasks_L);
      --  Initialize the lock used to synchronize chain of all ATCBs.

      Initialize_Lock (Any_Priority'Last, Global_Task_Lock);
      --  Initialize the lock used to implement mutual exclusion between
      --  all tasks.

      --  This is not according the the GNULLI, which specifies
      --  access procedure (Context: Pre_Call_State) for the handler.
      --  This may be a mistake in the interface. ???

      TSL.Abort_Defer        := Defer_Abortion'Access;
      TSL.Abort_Undefer      := Undefer_Abortion'Access;
      TSL.Lock_Task          := Task_Lock'Access;
      TSL.Unlock_Task        := Task_Unlock'Access;
      TSL.Get_Jmpbuf_Address := Get_Jmpbuf_Address'Access;
      TSL.Set_Jmpbuf_Address := Set_Jmpbuf_Address'Access;
      TSL.Get_Gnat_Exception := Get_GNAT_Exception'Access;
      TSL.Set_Gnat_Exception := Set_GNAT_Exception'Access;
      TSL.Get_Sec_Stack_Addr := Get_Sec_Stack_Addr'Access;
      TSL.Set_Sec_Stack_Addr := Set_Sec_Stack_Addr'Access;
      TSL.Get_Exc_Stack_Addr := Get_Exc_Stack_Addr'Access;
      TSL.Set_Exc_Stack_Addr := Set_Exc_Stack_Addr'Access;
      TSL.Get_Message_Length := Get_Message_Length'Access;
      TSL.Set_Message_Length := Set_Message_Length'Access;
      TSL.Get_Message_Addr   := Get_Message_Addr'Access;
      TSL.Set_Message_Addr   := Set_Message_Addr'Access;

      TSL.Set_Sec_Stack_Addr (NT_Sec_Stack_Addr);
      TSL.Set_Exc_Stack_Addr (NT_Exc_Stack_Addr);
      TSL.Set_GNAT_Exception (NT_GNAT_Exception);
      TSL.Set_Jmpbuf_Address (NT_Jmpbuf_Address);
      TSL.Set_Message_Length (NT_Message_Length);
      TSL.Set_Message_Addr   (NT_Message_Addr);

      Install_Abort_Handler (Abort_Handler'Access);

      --  Install handlers for asynchronous error signals.

      --  This is not according the the GNULLI, which specifies
      --  access procedure(...) for the handler.
      --  This may be a mistake in the interface. ???

      Install_Error_Handler (
        System.Compiler_Exceptions.Notify_Exception'Address);

      --  Set up the soft links to tasking services used in the absence of
      --  tasking.  These replace tasking-free defaults.

      --  Abortion is deferred in a new ATCB, so we need to undefer abortion
      --  at this stage to make the environment task abortable.

      Undefer_Abortion;

   end Init_RTS;

   --------------
   -- New_ATCB --
   --------------

   function New_ATCB (Init : ATCB_Init) return Task_ID is
      T : Task_ID;

   begin
      T := new Ada_Task_Control_Block (Init.Entry_Num);
      Initialize_ATCB (T, Init);
      return T;
   end New_ATCB;

   --------------------------------
   -- Remove_From_All_Tasks_List --
   --------------------------------

   procedure Remove_From_All_Tasks_List (
      Source : Task_ID;
      Result : out Boolean)
   is
      C        : Task_ID;
      Previous : Task_ID;
      Error    : Boolean;

   begin
      Write_Lock (All_Tasks_L, Error);

      Result := False;

      Previous := Null_Task;
      C := All_Tasks_List;

      while C /= Null_Task loop
         if C = Source then
            Result := True;

            if Previous = Null_Task then
               All_Tasks_List :=
                 All_Tasks_List.All_Tasks_Link;
            else
               Previous.All_Tasks_Link := C.All_Tasks_Link;
            end if;

            exit;

         end if;

         Previous := C;
         C := C.All_Tasks_Link;

      end loop;

      Unlock (All_Tasks_L);

   end Remove_From_All_Tasks_List;

   -----------------------------
   -- Runtime_Assert_Shutdown --
   -----------------------------

   function Runtime_Assert_Shutdown (Msg : in String) return boolean is
   begin
      LL_Assert (false, Msg);
      --  This call should never return
      return false;
   end Runtime_Assert_Shutdown;

   ---------------
   -- Task_Lock --
   ---------------

   procedure Task_Lock is
      T     : Task_ID := Self;
      Error : Boolean;
   begin
      T.Global_Task_Lock_Nesting := T.Global_Task_Lock_Nesting + 1;
      if T.Global_Task_Lock_Nesting = 1 then
         Write_Lock (Global_Task_Lock, Error);
      end if;
   end Task_Lock;

   -----------------
   -- Task_Unlock --
   -----------------

   procedure Task_Unlock is
      T     : Task_ID := Self;
   begin
      pragma Assert (
        T.Global_Task_Lock_Nesting > 0 or else
          Runtime_Assert_Shutdown (
            "Unlock_Task_T: Improper lock nesting"));

      T.Global_Task_Lock_Nesting := T.Global_Task_Lock_Nesting - 1;
      if T.Global_Task_Lock_Nesting = 0 then
         Unlock (Global_Task_Lock);
      end if;
   end Task_Unlock;


   function  Get_Jmpbuf_Address return  Address is
   begin
      return Self.Compiler_Data.Jmpbuf_Address;
   end Get_Jmpbuf_Address;

   procedure Set_Jmpbuf_Address (Addr : Address) is
   begin
      Self.Compiler_Data.Jmpbuf_Address := Addr;
   end Set_Jmpbuf_Address;

   function  Get_GNAT_Exception return  Address is
   begin
      return Self.Compiler_Data.GNAT_Exception;
   end Get_GNAT_Exception;

   procedure Set_GNAT_Exception (Addr : Address) is
   begin
      Self.Compiler_Data.GNAT_Exception := Addr;
   end Set_GNAT_Exception;

   function  Get_Sec_Stack_Addr return  Address is
   begin
      return Self.Compiler_Data.Sec_Stack_Addr;
   end Get_Sec_Stack_Addr;

   procedure Set_Sec_Stack_Addr (Addr : Address) is
   begin
      Self.Compiler_Data.Sec_Stack_Addr := Addr;
   end Set_Sec_Stack_Addr;

   function  Get_Exc_Stack_Addr return Address is
   begin
      return Self.Compiler_Data.Exc_Stack_Addr;
   end Get_Exc_Stack_Addr;

   procedure Set_Exc_Stack_Addr (Addr : Address) is
   begin
      Self.Compiler_Data.Exc_Stack_Addr := Addr;
   end Set_Exc_Stack_Addr;

   function  Get_Message_Length return Natural is
   begin
      return Self.Compiler_Data.Message_Length;
   end Get_Message_Length;

   procedure Set_Message_Length (Len : Natural) is
   begin
      Self.Compiler_Data.Message_Length := Len;
   end Set_Message_Length;

   function  Get_Message_Addr return Address is
   begin
      return Self.Compiler_Data.Message_Addr;
   end Get_Message_Addr;

   procedure Set_Message_Addr (Addr : Address) is
   begin
      Self.Compiler_Data.Message_Addr := Addr;
   end Set_Message_Addr;

   ----------------------
   -- Undefer_Abortion --
   ----------------------

   --  Precondition : Self does not hold any locks!

   --  Undefer_Abortion is called on any abortion completion point (aka.
   --  synchronization point). It performs the following actions if they
   --  are pending: (1) change the base priority, (2) abort the task.
   --  The priority change has to occur before abortion. Otherwise, it would
   --  take effect no earlier than the next abortion completion point.
   --  This version of Undefer_Abortion redefers abortion if abortion is
   --  in progress.  There has been some discussion of having
   --  the raise statement defer abortion to prevent abortion of
   --  handlers performing required completion.  This would make
   --  the explicit deferral unnecessary. ???

   procedure Undefer_Abortion is
      T : Task_ID := Self;
      Error : Boolean;

   begin
      T.Deferral_Level := T.Deferral_Level - 1;

      if T.Deferral_Level = ATC_Level'First and then T.Pending_Action then
         Write_Lock (T.L, Error);
         T.Pending_Action := False;

         if T.Pending_Priority_Change then
            Change_Base_Priority (T);
         end if;

         Unlock (T.L);

         if T.Pending_ATC_Level < T.ATC_Nesting_Level then
            raise Standard'Abort_Signal;
         end if;
      end if;

   end Undefer_Abortion;

   ---------------------
   -- Unsafe_New_ATCB --
   ---------------------

   function Unsafe_New_ATCB
     (Init : ATCB_Init)
      return Task_ID
   is
   begin
      return new Ada_Task_Control_Block (Init.Entry_Num);
   end Unsafe_New_ATCB;

   ------------------------
   -- Soft-Link Dummies  --
   ------------------------

   --  These are dummies for subprograms that are only needed by certain
   --  optional runtime system packages.  If they are needed, the soft
   --  links will be redirected to the real subprogram by elaboration of
   --  the subprogram body where the real subprogram is declared.

   procedure Finalize_Attributes (T : Task_ID) is
   --  see System.Tasking.Task_Attributes
   begin null;
   end Finalize_Attributes;

   procedure Initialize_Attributes (T : Task_ID) is
   --  see System.Tasking.Task_Attributes
   begin null;
   end Initialize_Attributes;

   -----------------------------------
   -- Tasking System Initialization --
   -----------------------------------

begin
   declare
      Main_Priority : Priority;
      pragma Import (C, Main_Priority, "__main_priority");

   begin
      Init_RTS (Main_Priority);
   end;
end System.Tasking.Initialization;

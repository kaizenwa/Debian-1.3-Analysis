------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . T A S K _ P R I M I T I V E S               --
--                                                                          --
--                                  S p e c                                 --
--                            (Windows NT version)                          --
--                                                                          --
--                             $Revision: 1.1 $                             --
--                                                                          --
--           Copyright (c) 1993,1994,1995 NYU, All Rights Reserved          --
--                                                                          --
--  GNARL is free software; you can redistribute it and/or modify it  under --
--  terms  of  the  GNU  Library General Public License as published by the --
--  Free Software Foundation; either version 2, or (at  your  option)  any  --
--  later  version.   GNARL is distributed in the hope that it will be use- --
--  ful, but but WITHOUT ANY WARRANTY; without even the implied warranty of --
--  MERCHANTABILITY  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Gen- --
--  eral Library Public License for more details.  You should have received --
--  a  copy of the GNU Library General Public License along with GNARL; see --
--  file COPYING. If not, write to the Free Software Foundation,  675  Mass --
--  Ave, Cambridge, MA 02139, USA.                                          --
--                                                                          --
------------------------------------------------------------------------------

with System.Task_Clock;
with System.Task_Info;
with Win_Task.Threads;
with Win_Task.Synchronization;

package System.Task_Primitives is
--  Cannot be preelaborated, because body requires initialization
--  of Test_And_Set lock
--  pragma Preelaborate (Task_Primitives);

   type LL_Task_Procedure_Access is access procedure (Arg : System.Address);
--   type LL_Task_Procedure_Access is private;

   type Pre_Call_State     is private;
   type Task_Storage_Size  is range 0 .. Integer'Last;
   type Machine_Exceptions is range 0 .. Integer'Last;
   type Interrupt_ID       is range 0 .. Integer'Last;
   type Interrupt_Info     is new String;
   type Error_information  is new String;

   Task_Wrapper_Frame : constant Integer := 72;

   -----------
   -- Tasks --
   -----------

   type Task_Control_Block is record
      LL_Entry_Point  : LL_Task_Procedure_Access;
      LL_Arg          : Address;
      Active_Priority : Priority;
      Thread          : Win_Task.HANDLE;
      Aborted         : Boolean := False;
   end record;

   type TCB_Ptr is access all Task_Control_Block;

   procedure Initialize_LL_Tasks (T : TCB_Ptr);

   procedure Create_LL_Task
      (Priority       : System.Priority;
       Stack_Size     : Task_Storage_Size;
       Task_Info      : System.Task_Info.Task_Info_Type;
       LL_Entry_Point : LL_Task_Procedure_Access;
       Arg            : Address;
       T              : TCB_Ptr);

   procedure Exit_LL_Task;

   function Self return TCB_Ptr;

   -----------
   -- Locks --
   -----------

   type Lock is private;

   procedure Initialize_Lock (Prio : Integer; L : in out Lock);

   procedure Finalize_Lock (L : in out Lock);

   procedure Write_Lock (L : in out Lock; Ceiling_Violation : out Boolean);

   procedure Read_Lock (L : in out Lock; Ceiling_Violation : out Boolean);

   procedure Unlock (L : in out Lock);

   -------------------------
   -- Condition Variables --
   -------------------------

   type Condition_Variable is private;

   procedure Initialize_Cond (Cond : in out Condition_Variable);

   procedure Finalize_Cond (Cond : in out Condition_Variable);

   procedure Cond_Wait (Cond : in out Condition_Variable; L : in out Lock);

   procedure Cond_Timed_Wait
     (Cond      : in out Condition_Variable;
      L         : in out Lock;
      Abs_Time  : System.Task_Clock.Stimespec;
      Timed_Out : out Boolean);

   procedure Cond_Signal (Cond : in out Condition_Variable);

   ----------------
   -- Priorities --
   ----------------

   procedure Set_Priority (T : TCB_Ptr; Prio : Integer);

   procedure Set_Own_Priority (Prio : Integer);

   function Get_Priority (T : TCB_Ptr) return Integer;

   function Get_Own_Priority return Integer;

   -----------------------------
   -- Signals, Errors, Aborts --
   -----------------------------

   procedure Abort_Task (T : TCB_Ptr);

   procedure Test_Abort;

   type Abort_Handler_Pointer is access procedure (Context : Pre_Call_State);

   procedure Install_Abort_Handler (Handler : Abort_Handler_Pointer);

   procedure Install_Error_Handler (Handler : Address);

   procedure Signal_Task (T : TCB_Ptr; I : Interrupt_ID);

   procedure Wait_for_Signal (I : Interrupt_ID);

   function Reserved_Signal (I : Interrupt_ID) return Boolean;

   procedure LL_Assert (B : Boolean; M : String);

   --------------------------
   -- Test and Set Support --
   --------------------------
   type TAS_Cell is private;
   procedure Initialize_TAS_Cell (Cell :    out TAS_Cell);
   pragma Inline (Initialize_TAS_Cell);
   procedure Finalize_TAS_Cell   (Cell : in out TAS_Cell);
   pragma Inline (Finalize_TAS_Cell);
   procedure Clear        (Cell : in out TAS_Cell);
   pragma Inline (Clear);
   procedure Test_And_Set (Cell : in out TAS_Cell; Result : out Boolean);
   pragma Inline (Test_And_Set);
   function  Is_Set       (Cell : in     TAS_Cell) return Boolean;
   pragma Inline (Is_Set);

private

--   type LL_Task_Procedure_Access is access procedure (Arg : System.Address);

   type Pre_Call_State is new Integer;
   --  Unused for Windows NT

   type Lock is
      record
         Mutex          : aliased Win_Task.HANDLE;
         Priority       : Integer;
         Owner_Priority : Integer;
      end record;

   type Condition_Variable is new Win_Task.HANDLE;
   type TAS_Cell is record
      Value : aliased Boolean := False;
   end record;

end System.Task_Primitives;

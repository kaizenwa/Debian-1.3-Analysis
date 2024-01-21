------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                           (No Tasking version)                           --
--                                                                          --
--                             $Revision: 1.5 $                             --
--                                                                          --
--          Copyright (C) 1991-1997 Free Software Foundation, Inc.          --
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

with Interfaces.C;
--  used for int
--           size_t
--           unsigned

with System.Error_Reporting;
--  used for Shutdown

with System.Interrupt_Management;
--  used for Keep_Unmasked
--           Abort_Task_Interrupt
--           Interrupt_ID

with System.OS_Interface;
--  used for various type, constant, and operations

with System.Tasking;
--  used for Ada_Task_Control_Block
--           Task_ID

package body System.Task_Primitives.Operations is

   use System.Tasking;
   use Interfaces.C;
   use System.Error_Reporting;
   use System.OS_Interface;
   use System.Parameters;

   ----------
   -- Self --
   ----------

   function Self return Task_ID is
   begin
      return Null_Task;
   end Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : access Lock)
   is
   begin
      null;
   end Initialize_Lock;

   procedure Initialize_Lock (L : access RTS_Lock) is
   begin
      null;
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : access Lock) is
   begin
      null;
   end Finalize_Lock;

   procedure Finalize_Lock (L : access RTS_Lock) is
   begin
      null;
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
   begin
      Ceiling_Violation := False;
   end Write_Lock;

   procedure Write_Lock (L : access RTS_Lock) is
   begin
      null;
   end Write_Lock;

   procedure Write_Lock (T : Task_ID) is
   begin
      null;
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
   begin
      Ceiling_Violation := False;
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : access Lock) is
   begin
      null;
   end Unlock;

   procedure Unlock (L : access RTS_Lock) is
   begin
      null;
   end Unlock;

   procedure Unlock (T : Task_ID) is
   begin
      null;
   end Unlock;

   -------------
   --  Sleep  --
   -------------

   procedure Sleep (Self_ID : Task_ID) is
   begin
      null;
   end Sleep;

   ---------------
   -- Sleep_For --
   ---------------

   procedure Sleep_For (Self_ID : Task_ID; Rel_Time : Duration) is
   begin
      null;
   end Sleep_For;

   -----------------
   -- Sleep_Until --
   -----------------

   procedure Sleep_Until (Self_ID : Task_ID; Abs_Time : Duration) is
   begin
      null;
   end Sleep_Until;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_ID) is
   begin
      null;
   end Wakeup;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (T : Task_ID; Prio : System.Any_Priority) is
   begin
      null;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_ID) return System.Any_Priority is
   begin
      return 0;
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_ID) is
   begin
      null;
   end Enter_Task;

   ----------------------
   --  Initialize_TCB  --
   ----------------------

   procedure Initialize_TCB (Self_ID : Task_ID; Succeeded : out Boolean) is
   begin
      Succeeded := False;
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
   begin
      Succeeded := False;
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_ID) is
   begin
      null;
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   procedure Exit_Task is
   begin
      null;
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_ID) is
   begin
      null;
   end Abort_Task;

   ------------
   --  Yield --
   ------------

   procedure Yield is
   begin
      null;
   end Yield;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_ID) is
   begin
      null;
   end Initialize;

   No_Tasking : exception;

begin

   Trace ("Tasking not implemented on this configuration");
   raise No_Tasking;
end System.Task_Primitives.Operations;

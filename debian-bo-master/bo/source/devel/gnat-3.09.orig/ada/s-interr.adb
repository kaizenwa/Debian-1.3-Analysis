------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  B o d y                                 --
--                           (No tasking version)                           --
--                                                                          --
--                             $Revision: 1.7 $                             --
--                                                                          --
--          Copyright (C) 1991-1997, Free Software Foundation, Inc.         --
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

--  This implemetation is the dummy version for use if there is no tasking

with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Ada.Interrupts; use Ada.Interrupts;

with System.IO; use System.IO;

package body System.Interrupts is

   procedure Unimplemented;
   --  This procedure writes out a message to the effect that tasking is
   --  not implemented, and then raises Program_Error.

   --------------------
   -- Attach_Handler --
   --------------------

   procedure Attach_Handler
     (New_Handler : Ada.Interrupts.Parameterless_Handler;
      Interrupt   : Ada.Interrupts.Interrupt_ID;
      Static      : Boolean := False)
   is
   begin
      Unimplemented;
   end Attach_Handler;

   -----------------------------
   -- Bind_Interrupt_To_Entry --
   -----------------------------

   procedure Bind_Interrupt_To_Entry
     (T       : System.Tasking.Task_ID;
      E       : System.Tasking.Task_Entry_Index;
      Int_Ref : System.Address)
   is
   begin
      Unimplemented;
   end Bind_Interrupt_To_Entry;

   ---------------------
   -- Block_Interrupt --
   ---------------------

   procedure Block_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID) is
   begin
      Unimplemented;
   end Block_Interrupt;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler
     (Interrupt : Ada.Interrupts.Interrupt_ID)
      return      Ada.Interrupts.Parameterless_Handler
   is
   begin
      Unimplemented;
      return null;
   end Current_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   procedure Detach_Handler
     (Interrupt : Ada.Interrupts.Interrupt_ID;
      Static    : Boolean := False)
   is
   begin
      Unimplemented;
   end Detach_Handler;

   ------------------------------
   -- Detach_Interrupt_Entries --
   ------------------------------

   procedure Detach_Interrupt_Entries (T : Tasking.Task_ID) is
   begin
      Unimplemented;
   end Detach_Interrupt_Entries;

   ----------------------
   -- Exchange_Handler --
   ----------------------

   pragma Warnings (Off); -- because we do not assign Old_Handler

   procedure Exchange_Handler
     (Old_Handler : out Ada.Interrupts.Parameterless_Handler;
      New_Handler : Ada.Interrupts.Parameterless_Handler;
      Interrupt   : Ada.Interrupts.Interrupt_ID;
      Static      : Boolean := False)
   is
   begin
      Unimplemented;
   end Exchange_Handler;

   pragma Warnings (On);

   -----------------
   -- Is_Attached --
   -----------------

   function Is_Attached
     (Interrupt : Ada.Interrupts.Interrupt_ID)
      return      Boolean
   is
   begin
      Unimplemented;
      return false;
   end Is_Attached;

   ----------------
   -- Is_Blocked --
   ----------------

   function Is_Blocked
     (Interrupt : Ada.Interrupts.Interrupt_ID)
      return      Boolean
   is
   begin
      Unimplemented;
      return False;
   end Is_Blocked;

   -----------------
   -- Is_Reserved --
   -----------------

   function Is_Reserved
     (Interrupt : Ada.Interrupts.Interrupt_ID)
      return      Boolean
   is
   begin
      Unimplemented;
      return False;
   end Is_Reserved;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Interrupt : Ada.Interrupts.Interrupt_ID)
      return      System.Address
   is
   begin
      Unimplemented;
      return System.Null_Address;
   end Reference;

   --------------------------------
   -- Register_Interrupt_Handler --
   --------------------------------

   procedure Register_Interrupt_Handler (Handler_Addr : System.Address) is
   begin
      Unimplemented;
   end Register_Interrupt_Handler;

   -----------------------
   -- Unblock_Interrupt --
   -----------------------

   procedure Unblock_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID) is
   begin
      Unimplemented;
   end Unblock_Interrupt;

   -------------------
   -- Unimplemented --
   -------------------

   procedure Unimplemented is
   begin
      Put_Line ("tasking is not implemented in this version of GNARL.");
      raise Program_Error;
   end Unimplemented;

end System.Interrupts;

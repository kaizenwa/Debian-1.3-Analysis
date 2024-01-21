------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.2 $                            --
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

--  This is an OS/2 version of this package.

--  This version is a stub, for systems that
--  do not support interrupts (or signals).

with System.Error_Reporting;
--  used for Shutdown

package body System.Interrupts is

   use System.Error_Reporting;
   use System.Tasking;

   procedure Unimplemented (Feature : String);
   --  This procedure writes out a message to the effect that the
   --  feature specified by the Feature string is unimplemented,
   --  and halts the program.

   -------------------
   -- Unimplemented --
   -------------------

   procedure Unimplemented (Feature : String) is
   begin
      if Shutdown (Feature &
        " is unimplemented in this version of GNARL.") then
         null;
      end if;
   end Unimplemented;

   ----------------------------------
   --  Register_Interrupt_Handler  --
   ----------------------------------

   procedure Register_Interrupt_Handler
     (Handler : Parameterless_Handler) is
   begin
      Unimplemented ("signal handling");
   end Register_Interrupt_Handler;

   -----------------
   -- Is_Reserved --
   -----------------

   function Is_Reserved (Interrupt : Interrupt_ID) return Boolean is
   begin
      Unimplemented ("signal handling");
      return True;
   end Is_Reserved;

   -----------------
   -- Is_Attached --
   -----------------

   function Is_Attached (Interrupt : Interrupt_ID) return Boolean is
   begin
      Unimplemented ("signal handling");
      return True;
   end Is_Attached;

   ----------------
   -- Is_Blocked --
   ----------------

   function Is_Blocked (Interrupt : Interrupt_ID) return boolean is
   begin
      Unimplemented ("signal handling");
      return True;
   end Is_Blocked;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (Interrupt : Interrupt_ID)
     return Parameterless_Handler is
   begin
      Unimplemented ("signal handling");
      return null;
   end Current_Handler;

   --------------------
   -- Attach_Handler --
   --------------------

   --  Calling this procedure with New_Handler = null and Static = true
   --  means we want to Detach the current handler regardless of
   --  the previous handler's binding status (ie. do not care if
   --  it is a dynamic or static handler).
   --  This option is needed so that during the finalization of a PO, we can
   --  detach handlers attached through pragma Attach_Handler.

   procedure Attach_Handler
     (New_Handler : in Parameterless_Handler;
      Interrupt   : in Interrupt_ID;
      Static      : in Boolean := False) is
   begin
      Unimplemented ("signal handling");
   end Attach_Handler;

   ----------------------
   -- Exchange_Handler --
   ----------------------

   --  Calling this procedure with New_Handler = null and Static = true
   --  means we want to Detach the current handler regardless of
   --  the previous handler's binding status (ie. do not care if
   --  it is a dynamic or static handler).
   --  This option is needed so that during the finalization of a PO, we can
   --  detach handlers attached through pragma Attach_Handler.

   procedure Exchange_Handler
     (Old_Handler : out Parameterless_Handler;
      New_Handler : in Parameterless_Handler;
      Interrupt   : in Interrupt_ID;
      Static      : in Boolean := False)  is
   begin
      Unimplemented ("signal handling");
   end Exchange_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   --  Calling this procedure with Static = true
   --  means we want to Detach the current handler regardless of
   --  the previous handler's binding status (ie. do not care if
   --  it is a dynamic or static handler).
   --  This option is needed so that during the finalization of a PO, we can
   --  detach handlers attached through pragma Attach_Handler.

   procedure Detach_Handler
     (Interrupt : in Interrupt_ID;
      Static    : in Boolean := False) is
   begin
      Unimplemented ("signal handling");
   end Detach_Handler;

   ---------------
   -- Reference --
   ---------------

   function Reference (Interrupt : Interrupt_ID) return System.Address is
   begin
      Unimplemented ("signal handling");
      return Interrupt'Address;
   end Reference;

   -----------------------------
   -- Bind_Interrupt_To_Entry --
   -----------------------------

   --  This procedure raises a Program_Error if it tries to
   --  bind an interrupt to which an Entry or a Procedure is
   --  already bound.

   procedure Bind_Interrupt_To_Entry
     (T       : Task_ID;
      E       : Task_Entry_Index;
      Int_Ref : System.Address) is
   begin
      Unimplemented ("signal handling");
   end Bind_Interrupt_To_Entry;

   ------------------------------
   -- Detach_Interrupt_Entries --
   ------------------------------

   procedure Detach_Interrupt_Entries (T : Task_ID) is
   begin
      Unimplemented ("signal handling");
   end Detach_Interrupt_Entries;

   ---------------------
   -- Block_Interrupt --
   ---------------------

   procedure Block_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Unimplemented ("signal handling");
   end Block_Interrupt;

   -----------------------
   -- Unblock_Interrupt --
   -----------------------

   procedure Unblock_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Unimplemented ("signal handling");
   end Unblock_Interrupt;

end System.Interrupts;

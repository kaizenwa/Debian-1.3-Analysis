------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  S p e c                                 --
--                           (Compiler Interface)                           --
--                                                                          --
--                             $Revision: 1.8 $                             --
--                                                                          --
--          Copyright (C) 1992-1997, Free Software Foundation, Inc.         --
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

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

with System.Tasking;
with Ada.Interrupts;

package System.Interrupts is

   --  Routines needed for Ada.Interrupts

   --  Attempt to attach a Handler to an Interrupt to which an Entry is
   --  already bound will raise a Program_Error.

   function Is_Reserved
     (Interrupt : Ada.Interrupts.Interrupt_ID)
      return      Boolean;

   function Is_Attached
     (Interrupt : Ada.Interrupts.Interrupt_ID)
      return      Boolean;

   function Current_Handler
     (Interrupt : Ada.Interrupts.Interrupt_ID)
      return      Ada.Interrupts.Parameterless_Handler;

   procedure Attach_Handler
     (New_Handler : Ada.Interrupts.Parameterless_Handler;
      Interrupt   : Ada.Interrupts.Interrupt_ID;
      Static      : Boolean := False);
   --  Calling this procedure with New_Handler = null and Static = True
   --  means that we want to Detach the current handler regardless of
   --  the previous handler's binding status (ie. do not care if
   --  it is a dynamic or static handler).

   procedure Exchange_Handler
     (Old_Handler : out Ada.Interrupts.Parameterless_Handler;
      New_Handler : Ada.Interrupts.Parameterless_Handler;
      Interrupt   : Ada.Interrupts.Interrupt_ID;
      Static      : Boolean := False);
   --  Calling this procedure with New_Handler = null and Static = True
   --  means that we want to Detach the current handler regardless of
   --  the previous handler's binding status (ie. do not care if
   --  it is a dynamic or static handler).

   procedure Detach_Handler
     (Interrupt : Ada.Interrupts.Interrupt_ID;
      Static    : boolean := False);
   --  Calling this procedure with Static = True means that we want to Detach
   --  the current handler regardless of the previous handler's binding status
   --  (i.e. we do not care if it is a dynamic or static handler).

   function Reference
     (Interrupt : Ada.Interrupts.Interrupt_ID)
      return      System.Address;

   --  Routines needed to implement pragmas Interrupt_Handler, Attach_Handler.

   procedure Register_Interrupt_Handler (Handler_Addr : System.Address);
   --  This routine should be called by the compiler to allow the
   --  handler be used as an Interrupt Handler.

   --  Routines needed for Interrupt Entries

   --  Attempt to bind an Entry to an Interrupt to which a Handler is
   --  already attached will raise a Program_Error.

   procedure Bind_Interrupt_To_Entry
     (T       : System.Tasking.Task_ID;
      E       : System.Tasking.Task_Entry_Index;
      Int_Ref : System.Address);

   procedure Detach_Interrupt_Entries (T : System.Tasking.Task_ID);
   --  This procedure detaches all the Interrupt Entries bound to a task.

   --  Routines needed for POSIX dot5 POSIX_Signals.

   procedure Block_Interrupt   (Interrupt : Ada.Interrupts.Interrupt_ID);

   procedure Unblock_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID);

   function Is_Blocked
     (Interrupt : Ada.Interrupts.Interrupt_ID)
      return      Boolean;

end System.Interrupts;

------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  S p e c                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.11 $                             --
--                                                                          --
--    Copyright (C) 1991,92,93,94,95,1996 Free Software Foundation, Inc.    --
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

--  This package encapsulates the implementation of interrupt or signal
--  handlers.  It is logically an extension of the body of Ada.Interrupts.
--  It is made a child of System to allow visibility of various
--  runtime system internal data and operations.

--  See System.Interrupt_Management for core interrupt/signal interfaces.

--  These two packages are separated in order to allow
--  System.Interrupt_Management to be used without requiring the whole
--  tasking implementation to be linked and elaborated.

with System.Tasking;
--  used for Task_ID

with System.Interrupt_Management;
--  used for Interrupt_ID

package System.Interrupts is

   pragma Elaborate_Body;

   type Ada_Interrupt_ID is new System.Interrupt_Management.Interrupt_ID;
   --  avoid inheritance by Ada.Interrupts.Interrupt_ID of unwanted operations

   type Interrupt_ID is new System.Interrupt_Management.Interrupt_ID;

   type Parameterless_Handler is access protected procedure;

   --  Attempt to attach a Handler to an Interrupt to which an Entry is
   --  already bound will raise a Program_Error.

   function Is_Reserved (Interrupt : Interrupt_ID) return Boolean;

   function Is_Entry_Attached (Interrupt : Interrupt_ID) return Boolean;

   function Is_Handler_Attached (Interrupt : Interrupt_ID) return Boolean;

   function Current_Handler
     (Interrupt : Interrupt_ID)
      return Parameterless_Handler;

   --  Calling the following procedures with New_Handler = null
   --  and Static = true means that we want to modify the current handler
   --  regardless of the previous handler's binding status.
   --  (i.e. we do not care whether it is a dynamic or static handler)

   procedure Attach_Handler
     (New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID;
      Static      : boolean := false);

   procedure Exchange_Handler
     (Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID;
      Static      : boolean := false);

   procedure Detach_Handler
     (Interrupt : Interrupt_ID;
      Static    : boolean := false);

   function Reference (Interrupt : Interrupt_ID)
     return System.Address;

   --  Routines needed to implement pragma Interrupt_Handler
   --  and Attach_Handler

   procedure Register_Interrupt_Handler
     (Handler_Addr : System.Address);
   --  This rotine should be called by the compiler to allow the
   --  handler be used as an Interrupt Handler. That means call this
   --  procedure for each pragma Interrup_Handler providing the
   --  address of the handler (not including the pointer to the
   --  actual PO. In this way this routine is called only once for
   --  each type definition of PO.

   --  Routines needed for Interrupt Entries
   --  Attempt to bind an Entry to an Interrupt to which a Handler is
   --  already attached will raise a Program_Error.

   procedure Bind_Interrupt_To_Entry
     (T       : System.Tasking.Task_ID;
      E       : System.Tasking.Task_Entry_Index;
      Int_Ref : System.Address);

   procedure Detach_Interrupt_Entries (T : System.Tasking.Task_ID);
   --  This procedure detaches all the Interrupt Entries bound to a task.

   --  Routines needed for POSIX dot5 POSIX_Signals

   procedure Block_Interrupt (Interrupt : Interrupt_ID);
   --  Block the Interrupt on the process level

   procedure Unblock_Interrupt (Interrupt : Interrupt_ID);

   function Unblocked_By (Interrupt : Interrupt_ID)
     return System.Tasking.Task_ID;
   --  It returns the ID of the last Task which Unblocked this Interrupt.
   --  It returns Null_Task if no tasks have ever requested the
   --  Unblocking operation or the Interrupt is currently Blocked.

   function Is_Blocked (Interrupt : Interrupt_ID) return Boolean;

   procedure Ignore_Interrupt (Interrupt : Interrupt_ID);
   --  Set the sigacion for the interrupt to SIG_IGN.

   procedure Unignore_Interrupt (Interrupt : Interrupt_ID);

   function Is_Ignored (Interrupt : Interrupt_ID) return Boolean;

   --  Note : Direct calls to sigaction, sigprocmask, thr_sigsetmask or any
   --  other low-level interface that changes the signal action or signal mask
   --  needs a careful thought.
   --  One may acheive the effect of system calls first making RTS blocked
   --  (by calling Block_Interrupt) for the signal under consideration.
   --  This will make all the tasks in RTS blocked for the Interrupt.

end System.Interrupts;

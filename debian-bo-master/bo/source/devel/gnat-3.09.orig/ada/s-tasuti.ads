------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--              S Y S T E M . T A S K I N G . U T I L I T I E S             --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.23 $                            --
--                                                                          --
--      Copyright (C) 1991,1992,1993,1994,1995 Florida State University     --
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

--  This package provides RTS Internal Declarations.
--  These declarations are not part of the GNARLI

with Unchecked_Conversion;

package System.Tasking.Utilities is

   --  Entry queue related types
   --  ??? Not currently used.  Should it be?

   type Server_Kind is (Task_Server, PO_Server);

   type Server_Record (Kind : Server_Kind := Task_Server) is record
      case Kind is
         when Task_Server =>
            Called_Task            : Task_ID;
            Acceptor_Prev_Call     : Entry_Call_Link;

            Acceptor_Prev_Priority : Rendezvous_Priority;
            --  For a task servicing a task entry call,
            --  information about the most recent prior call being serviced.
            --   Not used for protected entry calls;
            --  this function should be performed by GNULLI ceiling locking.

         when PO_Server =>
            Called_PO : Protection_Access;

      end case;
   end record;

   function ATCB_To_Address is new
     Unchecked_Conversion (Task_ID, System.Address);

   procedure Vulnerable_Complete_Activation
     (T : Task_ID;
      Completed : Boolean);
   --  Completes the activation by signaling its children.
   --  Completed indicates a call when the task has completed.
   --  Does not defer abortion (unlike Complete_Activation).

   procedure Check_Exception (T : Task_ID := Self);
   pragma Inline (Check_Exception);
   --  Raises an exception pending on Self.
   --  Used to delay exceptions until abortion is undeferred.

   procedure Complete_on_Sync_Point (T : Task_ID);
   --  If a task is suspended on an accept, select, or entry call
   --  (but not yet *in* rendezvous) then complete the task.

   procedure Reset_Priority
     (Acceptor_Prev_Priority : Rendezvous_Priority;
      Acceptor               : Task_ID);
   pragma Inline (Reset_Priority);
   --  Reset the priority of a task completing an accept statement to
   --  the value it had before the call.

   procedure Terminate_Alternative;
   --  Called when terminate alternative is selected.
   --  Waits for the parent to terminate the task
   --  or a caller to select an accept alternative.
   --  Assumes that abortion is deferred when called.

   procedure Complete (Target : Task_ID);
   --  Complete task and act on pending abortion.

   ---------------------------------
   -- Task_Stage Related routines --
   ---------------------------------

   procedure Make_Independent;
   --  Remove a task from the master hierarchy.  This includes setting the
   --  master ID to zero (no master) and removing the task from the
   --  All_Tasks_List (which is used to search for masters and dependents).
   --  No master will wait on the termination of an independent task;
   --  such tasks may still be running when the program terminates, at which
   --  point they will be killed by the underlying operating system.
   --  This is a dangerous operation, and should only be used on tasks
   --  that require no finalization.  Independent tasks are intended only
   --  for internal use by the GNARL, to prevent such internal tasks from
   --  preventing a user task from terminating.

   ------------------------------------
   -- Task Abortion related routines --
   ------------------------------------

   procedure Abort_To_Level (Target : Task_ID; L : ATC_Level);
   --  Abort a task to a specified ATC level.

   procedure Abort_Handler (Context : System.Task_Primitives.Pre_Call_State);
   --  Handler to be installed at initialization; it is invoked by a task
   --  when it is the target of an Abort_Task low-level operation.

   procedure Abort_Dependents (Abortee : Task_ID);
   --  Propagate the abortion of a parent into its children.

   -----------------------------------------------
   --  RTS routine to be used for pragma assert --
   -----------------------------------------------

   function Runtime_Assert_Shutdown (Msg : in String) return boolean;
   --  This function is used to shut down the runtime when there is
   --  an assertion error to be raise through "pragma Assert"
   --  Usage should be either
   --    pragma Assert (Runtime_Assert_Shutdown ("..."));
   --    --  unconditional shutdown
   --  or
   --    pragma Assert
   --      (ASSERT_CONDITION or else Runtime_Assert_Shutdown ("..."));
   --    --  conditional shutdown. Shut down the runtime only when the
   --    --  ASSERT_CONDITION fails.

end System.Tasking.Utilities;

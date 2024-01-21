------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              S Y S T E M . T A S K I N G . A B O R T I O N               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.19 $                             --
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

with System.Tasking.Utilities;
--  Used for, Utilities.All_Tasks_L,
--            Utilities.All_Tasks_List
--            Utilities.Abort_To_Level,
--            Utilities.Abort_Dependents
--            Utilities.Runtime_Assert_Shutdown

with System.Tasking.Initialization;
--  Used for, Defer_Abortion
--            Undefer_Abortion
--            Change_Base_Priority

with System.Task_Primitives; use System.Task_Primitives;

package body System.Tasking.Abortion is

   procedure Abort_Self (T : Task_ID);
   --  Initiate abortion of self.  Out-of-line actions taken by
   --  Undefer_Abortion[_Self] to process a pending abort.

   --------------------------
   -- Change_Base_Priority --
   --------------------------

   procedure Change_Base_Priority (T : Task_ID) renames
     System.Tasking.Initialization.Change_Base_Priority;

   --------------------
   -- Defer_Abortion --
   --------------------

   procedure Defer_Abortion is
      T : Task_ID := Self;
   begin
      T.Deferral_Level := T.Deferral_Level + 1;
   end Defer_Abortion;


   -------------------------
   -- Defer_Abortion_Self --
   -------------------------

   procedure Defer_Abortion_Self (T : Task_ID := Self) is
   begin
      pragma Assert (T = Self or else
        Utilities.Runtime_Assert_Shutdown (
          "Only the Self can execute this!"));

      T.Deferral_Level := T.Deferral_Level + 1;
   end Defer_Abortion_Self;


   ----------------------
   -- Undefer_Abortion --
   ----------------------

   procedure Undefer_Abortion is
      T : Task_ID := Self;
   begin
      T.Deferral_Level := T.Deferral_Level - 1;
      if T.Deferral_Level = ATC_Level'First and then T.Pending_Action then
         Abort_Self (T);
      end if;
   end Undefer_Abortion;

   ---------------------------
   -- Undefer_Abortion_Self --
   ---------------------------

   procedure Undefer_Abortion_Self (T : Task_ID := Self) is
   begin
      pragma Assert (T = Self or else
        Utilities.Runtime_Assert_Shutdown (
          "Only the Self can execute this!"));

      T.Deferral_Level := T.Deferral_Level - 1;
      if T.Deferral_Level = ATC_Level'First and then T.Pending_Action then
         Abort_Self (T);
      end if;
   end Undefer_Abortion_Self;


   -----------------
   -- Abort_Tasks --
   -----------------

   --  Called to initiate abortion, however, the actual abortion
   --  is done by abortee by means of Abort_Handler

   procedure Abort_Tasks (Tasks : Task_List) is
      Abortee               : Task_ID;
   begin
      Defer_Abortion;

      --  Begin non-abortable section

      for J in Tasks'Range loop

         Abortee := Tasks (J);

         if Abortee.Stage = Created then
            Utilities.Complete (Abortee);
            Abortee.Stage := Terminated;
            --  Task aborted before activation is safe to complete
            --  Mark This task to be terminated.
         else
            Abortee.Accepting := Not_Accepting;
            Utilities.Complete_on_Sync_Point (Abortee);
            Utilities.Abort_To_Level (Abortee, 0);
            --  Process abortion of child tasks
            Utilities.Abort_Dependents (Abortee);
         end if;

      end loop;

      --  End non-abortable section

      Undefer_Abortion;
   end Abort_Tasks;

   ----------------
   -- Abort_Self --
   ----------------

   procedure Abort_Self (T : Task_ID) is
      Error : Boolean;
   begin

      Write_Lock (T.L, Error);

      --  We do not want to clear this field if the abortion is pending
      --  for abrting the whole task. This field should not matter being
      --  turned on if the whole task is going to be aborted.

      if T.Pending_ATC_Level /= ATC_Level_Base'First then
         T.Pending_Action := False;
      end if;

      if T.Pending_Priority_Change then
         Change_Base_Priority (T);
      end if;

      Unlock (T.L);

      if T.Pending_ATC_Level < T.ATC_Nesting_Level then
         raise Standard'Abort_Signal;
      end if;

   end Abort_Self;

end System.Tasking.Abortion;

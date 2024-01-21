------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--             S Y S T E M . T A S K I N G . E N T R Y _ C A L L S          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.7 $                             --
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

--  This package provides internal RTS calls implementing operations
--  that apply to general entry calls, that is, calls to either
--  protected or task entries.
--  These declarations are not part of the GNARLI

with System.Task_Primitives;  use System.Task_Primitives;

with System.Tasking.Queuing; use System.Tasking.Queuing;
--  Used for, Queuing.Dequeue,
--            Queuing.Enqueue

with System.Tasking.Protected_Objects;
--  Used for, Unlock,
--            Service_Entries

with System.Tasking.Utilities;
--  Used for, Runtime_Assert_Shutdown

with System.Tasking.Abortion;
--  Used for, Change_Base_Priority

package body System.Tasking.Entry_Calls is

   -------------------
   -- Internal_Lock --
   -------------------

   procedure Internal_Lock
     (Object : access Protection;
      Ceiling_Violation : out Boolean) is
   begin
      Write_Lock (Object.L, Ceiling_Violation);
   end Internal_Lock;

   -----------------------------
   -- Internal_Lock_Read_Only --
   -----------------------------

   procedure Internal_Lock_Read_Only
     (Object : access Protection;
      Ceiling_Violation : out Boolean) is
   begin
      Read_Lock (Object.L, Ceiling_Violation);
   end Internal_Lock_Read_Only;

   -----------------
   -- Lock_Server --
   -----------------

   procedure Lock_Server
     (Entry_Call : Entry_Call_Link;
      No_Server  : out Boolean)
   is
      Test_Task         : Task_ID;
      Test_PO           : Protection_Access;
      Ceiling_Violation : Boolean;

   begin
      Test_Task := Entry_Call.Called_Task;
      --  This must be atomic.

      loop
         if Test_Task = Null_Task then

            Test_PO := Entry_Call.Called_PO;
            --  This must be atomic.

            Test_Task := Entry_Call.Called_Task;
            --  Check the task again, just in case a transition between
            --  task and protected entry calls is taking place.

            if Test_PO = Null_PO and then Test_Task = Null_Task then
               No_Server := True;
               return;
            end if;

            Internal_Lock (Test_PO, Ceiling_Violation);

            --  ??? The following code allows Lock_Server to be called
            --      when cancelling a call, to allow for the possibility
            --      that the priority of the caller has been raised
            --      beyond that of the protected entry call by
            --      Ada.Dynamic_Priorities.Set_Priority.  This test could be
            --      eliminated for other cases (i.e. when not cancelling
            --      a call), resulting in slightly improved performance.

            --  If the current task has a higher priority than the ceiling
            --  of the protected object, temporarily lower it.  It will
            --  be reset in Unlock.

            if Ceiling_Violation then
               declare
                  Current_Task : Task_ID := Self;
                  Old_Base_Priority : System.Any_Priority;
               begin
                  Write_Lock (Current_Task.L, Ceiling_Violation);
                  Old_Base_Priority := Current_Task.Base_Priority;
                  Current_Task.New_Base_Priority := Test_PO.Ceiling;
                  System.Tasking.Abortion.Change_Base_Priority (Current_Task);
                  Unlock (Current_Task.L);
                  Internal_Lock (Test_PO, Ceiling_Violation);
                  Test_PO.Old_Base_Priority := Old_Base_Priority;
                  Test_PO.Pending_Action := True;
               end;
            end if;

            exit when Test_PO = Entry_Call.Called_PO;
            System.Tasking.Protected_Objects.Unlock (Test_PO);
         else
            Write_Lock (Test_Task.L, Ceiling_Violation);
            exit when Test_Task = Entry_Call.Called_Task;
            Unlock (Test_Task.L);
         end if;

         Test_Task := Entry_Call.Called_Task;
         --  This must be atomic.

      end loop;

      No_Server := False;

   end Lock_Server;

   -------------------
   -- Unlock_Server --
   -------------------

   procedure Unlock_Server (Entry_Call : Entry_Call_Link) is
   begin
      if Entry_Call.Called_Task /= Null_Task then
         Unlock (Entry_Call.Called_Task.L);
      else
         System.Tasking.Protected_Objects.Unlock (Entry_Call.Called_PO);
      end if;
   end Unlock_Server;

   ------------------------------
   -- Unlock_And_Update_Server --
   ------------------------------

   procedure Unlock_And_Update_Server (Entry_Call : Entry_Call_Link) is
   begin
      if Entry_Call.Called_Task /= Null_Task then
         Unlock (Entry_Call.Called_Task.L);
      else
         System.Tasking.Protected_Objects.Service_Entries
           (Entry_Call.Called_PO);
         System.Tasking.Protected_Objects.Unlock (Entry_Call.Called_PO);
      end if;
   end Unlock_And_Update_Server;

   -------------------------
   -- Wait_For_Completion--
   -------------------------

   procedure Wait_For_Completion (Entry_Call : Entry_Call_Link) is
      Caller    : Task_ID := Entry_Call.Self;
      Cancelled : Boolean := False;
      Error     : Boolean;
      No_Server : Boolean;
   begin
      Write_Lock (Caller.L, Error);

      --  If this is a conditional call, it should be cancelled when it
      --  becomes abortable.  This is checked in the loop below, but
      --  only when Caller.Pending_Action is True.  For conditional
      --  calls, enable this check the first time through the loop.

      --  We do the same thing for Asynchronous_Call. Executing the following
      --  loop will clear the Pending_Action field if there is no
      --  Pending_Action. We want the call made from Cancel_Task_Entry_Call
      --  to check the abortion level so that we make sure that the Cancelled
      --  field reflect the status of an Asynchronous_Call properly.
      --  This problem came up when the triggered statement and the abortable
      --  part depend on entries of the same task. When a cancellation is
      --  delivered, undefer_abortion in the call made from abortable part
      --  sets the Pending_Action bit to false. However, the call is actually
      --  made to cancel the Asynchronous Call so that we need to check its
      --  status here again. Otherwise we may end up waiting for a cancelled
      --  call forever.

      if Entry_Call.Mode = Conditional_Call or else
        Entry_Call.Mode = Asynchronous_Call then
         Caller.Pending_Action := True;
      end if;

      while not Entry_Call.Done and then not Cancelled loop
         if Caller.Pending_Action then
            Caller.Pending_Action := False;
            if Caller.Pending_Priority_Change then
               Abortion.Change_Base_Priority (Caller);

               --  Requeue the entry call at the new priority. This only
               --  needs to be done if the caller is blocked waiting
               --  for the call (D.5(16)).

               Unlock (Caller.L);

               Lock_Server (Entry_Call, No_Server);
               Write_Lock (Caller.L, Error);

               Requeue_Call_With_New_Prio (Entry_Call,
                 Caller.Current_Priority);

               Unlock (Caller.L);
               Unlock_Server (Entry_Call);

               Write_Lock (Caller.L, Error);

            end if;

            if Entry_Call.Mode = Conditional_Call
              or else Caller.Pending_ATC_Level < Caller.ATC_Nesting_Level
            then
               Unlock (Caller.L);
               Lock_Server (Entry_Call, No_Server);

               if Entry_Call.Abortable then
                  if Onqueue (Entry_Call) then
                     Dequeue_Call (Entry_Call);
                  end if;
                  Cancelled := True;
                  Unlock_And_Update_Server (Entry_Call);
               else
                  Unlock_Server (Entry_Call);
               end if;

               Write_Lock (Caller.L, Error);
            end if;

         else
            Cond_Wait (Caller.Cond, Caller.L);
         end if;
      end loop;

      Caller.ATC_Nesting_Level := Caller.ATC_Nesting_Level - 1;

      --  If we have reached the desired ATC nesting level, reset the
      --  requested level to effective infinity, to allow further calls.

      if Caller.Pending_ATC_Level = Caller.ATC_Nesting_Level then
         Caller.Pending_ATC_Level := ATC_Level_Infinity;
         Caller.Aborting := False;
      end if;

      --  If there is a pending abortion, the above loop may have
      --  reset the Pending_Action flag.  This flag must be regenerated
      --  here so that Undefer_Abortion will complete the abortion.

      if Caller.Pending_ATC_Level < Caller.ATC_Nesting_Level then
         Caller.Pending_Action := True;
      end if;

      Caller.Exception_To_Raise := Entry_Call.Exception_To_Raise;

      Unlock (Caller.L);
   end Wait_For_Completion;

   --------------------------
   -- Wait_Until_Abortable --
   --------------------------

   procedure Wait_Until_Abortable
     (Caller : Task_ID;
      Call   : Entry_Call_Link)
   is
      Abortable : Boolean := False;
      Error     : Boolean;
      No_Server : Boolean;

   begin
      pragma Assert (Caller.ATC_Nesting_Level > ATC_Level_Base'First
        or else System.Tasking.Utilities.Runtime_Assert_Shutdown
          ("Attempt to wait for a nonexistent call to be abortable."));
      pragma Assert (Call.Mode = Asynchronous_Call
        or else System.Tasking.Utilities.Runtime_Assert_Shutdown
          ("Attempt to wait for a non-asynchronous call to be abortable"));

      Write_Lock (Caller.L, Error);

      if Call.Mode = Conditional_Call then
         Caller.Pending_Action := True;
      end if;

      while not Call.Done and then not Abortable loop
         if Caller.Pending_Action then
            Unlock (Caller.L);
            Lock_Server (Call, No_Server);

            pragma Assert (not No_Server
              or else System.Tasking.Utilities.Runtime_Assert_Shutdown (
                "Entry call has no target"));

            if Call.Abortable then
               Abortable := True;
            end if;
            Unlock_Server (Call);
            Write_Lock (Caller.L, Error);
         else
            Cond_Wait (Caller.Cond, Caller.L);
         end if;
      end loop;

      Unlock (Caller.L);
   end Wait_Until_Abortable;

end System.Tasking.Entry_Calls;

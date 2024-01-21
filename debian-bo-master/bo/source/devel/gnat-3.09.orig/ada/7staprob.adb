-----------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--      S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S     --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.50 $                            --
--                                                                          --
--            Copyright (C) 1991-1997, Florida State University             --
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

with Ada.Exceptions;
--  Used for Exception_ID
--           Null_Id

with System.Error_Reporting;
--  used for Shutdown

with System.Task_Primitives.Operations;
--  used for Initialize_Lock
--           Write_Lock
--           Unlock
--           Get_Priority
--           Wakeup

with System.Tasking.Entry_Calls;
--  used for Wait_For_Completion
--           Wait_Until_Abortable

with System.Tasking.Initialization;
--  Used for Defer_Abortion,
--           Undefer_Abortion,
--           Change_Base_Priority

pragma Elaborate (System.Tasking.Initialization);
--  This insures that tasking is initialized if any protected objects are
--  created.

with System.Tasking.Queuing;
--  used for Enqueue
--           Broadcast_Program_Error
--           Select_Protected_Entry_Call
--           Onqueue
--           Count_Waiting

with System.Tasking.Utilities;
--  used for Abort_To_Level
--           Check_Exception

package body System.Tasking.Protected_Objects is

   use System.Task_Primitives.Operations;
   use System.Error_Reporting;

   procedure Defer_Abort (Self_ID : Task_ID)
     renames System.Tasking.Initialization.Defer_Abort;

   procedure Undefer_Abort (Self_ID : Task_ID)
     renames System.Tasking.Initialization.Undefer_Abort;

   procedure Change_Base_Priority (Self_ID : Task_ID)
     renames System.Tasking.Initialization.Change_Base_Priority;

   procedure Do_Or_Queue
     (Object     : access Protection;
      Entry_Call : Entry_Call_Link);
   --  This procedure either executes or queues an entry call, depending
   --  on the status of the corresponding barrier. It assumes that abortion
   --  is deferred and that the specified object is locked.

   pragma Inline (Do_Or_Queue);

   --------------
   -- Enqueued --
   --------------

   function Enqueued (Block : Communication_Block) return Boolean is
   begin
      return Block.Enqueued;
   end Enqueued;

   ---------------
   -- Cancelled --
   ---------------

   function Cancelled (Block : Communication_Block) return Boolean is
   begin
      return Block.Cancelled;
   end Cancelled;

   ---------------------------
   -- Initialize_Protection --
   ---------------------------

   procedure Initialize_Protection
     (Object            : access Protection;
      Ceiling_Priority  : Integer;
      Compiler_Info     : System.Address;
      Entry_Bodies      : access Protected_Entry_Body_Array)
   is
      Init_Priority     : Integer := Ceiling_Priority;
      First_Entry_Index : Protected_Entry_Index := 1;
      Last_Entry_Index  : Protected_Entry_Index := Object.Num_Entries;

   begin
      if Init_Priority = Unspecified_Priority then
         Init_Priority  := System.Priority'Last;
      end if;

      Initialize_Lock (Init_Priority, Object.L'Access);
      Object.Ceiling := System.Any_Priority (Init_Priority);
      Object.Compiler_Info := Compiler_Info;
      Object.Pending_Action := False;
      Object.Call_In_Progress := null;
      Object.Entry_Bodies := Entry_Bodies;

      for E in Object.Entry_Queues'Range loop
         Object.Entry_Queues (E).Head := null;
         Object.Entry_Queues (E).Tail := null;
      end loop;
   end Initialize_Protection;

   ----------
   -- Lock --
   ----------

   procedure Lock (Object : access Protection) is
      Ceiling_Violation : Boolean;

   begin
      Write_Lock (Object.L'Access, Ceiling_Violation);
      if Ceiling_Violation then
         raise Program_Error;
      end if;
   end Lock;

   --------------------
   -- Lock_Read_Only --
   --------------------

   procedure Lock_Read_Only (Object : access Protection) is
      Ceiling_Violation : Boolean;

   begin
      Read_Lock (Object.L'Access, Ceiling_Violation);
      if Ceiling_Violation then
         raise Program_Error;
      end if;
   end Lock_Read_Only;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Object : access Protection) is
      Caller : Task_ID := Self;

   begin
      if Object.Pending_Action then
         Object.Pending_Action := False;
         Write_Lock (Caller);
         Caller.New_Base_Priority := Object.Old_Base_Priority;
         Change_Base_Priority (Caller);
         Unlock (Caller);
      end if;
      Unlock (Object.L'Access);
   end Unlock;

   -----------------
   -- Do_Or_Queue --
   -----------------

   procedure Do_Or_Queue
     (Object     : access Protection;
      Entry_Call : Entry_Call_Link)
   is
      E : Protected_Entry_Index := Protected_Entry_Index (Entry_Call.E);

   begin

      --  When the Action procedure for an entry body returns, it is either
      --  completed (having called [Exceptional_]Complete_Entry_Body) or it
      --  is queued, having executed a requeue statement.

      if Object.Entry_Bodies (E).Barrier (Object.Compiler_Info, E) then

         if Entry_Call.Abortable /= Never then
            Entry_Call.Abortable := False;
            --  Not abortable while in progress.
         end if;

         Object.Call_In_Progress := Entry_Call;
         Object.Entry_Bodies (E).Action (
           Object.Compiler_Info, Entry_Call.Uninterpreted_Data, E);
      elsif Entry_Call.Mode /= Conditional_Call then
         Entry_Call.Has_Been_Abortable := True;
         Queuing.Enqueue (Object.Entry_Queues (E), Entry_Call);
      end if;

   exception
      when others =>
         Queuing.Broadcast_Program_Error (Object, Entry_Call);
   end Do_Or_Queue;

   ---------------------
   -- Service_Entries --
   ---------------------

   procedure Service_Entries (Object : access Protection) is
      Entry_Call : Entry_Call_Link;
      E          : Protected_Entry_Index;

   begin
      loop
         Queuing.Select_Protected_Entry_Call (Object, Entry_Call);
         if Entry_Call /= null then
            E := Protected_Entry_Index (Entry_Call.E);
            if Entry_Call.Abortable /= Never then
               Entry_Call.Abortable := False;
               --  Not abortable while in progress.
            end if;

            Object.Call_In_Progress := Entry_Call;
            Object.Entry_Bodies (E).Action (
              Object.Compiler_Info, Entry_Call.Uninterpreted_Data, E);
         else
            exit;
         end if;
      end loop;
   end Service_Entries;


   --------------------------
   -- Protected_Entry_Call --
   --------------------------

   procedure Protected_Entry_Call
     (Object              : access Protection;
      E                   : Protected_Entry_Index;
      Uninterpreted_Data  : System.Address;
      Mode                : Call_Modes;
      Block               : out Communication_Block)
   is
      Caller              : Task_ID  := Self;
      Level               : ATC_Level;
      Entry_Call          : Entry_Call_Link;
      Initially_Abortable : Boolean;

   begin
      Defer_Abort (Caller);
      Lock (Object);

      Block.Self := Caller;

      pragma Assert (Caller.ATC_Nesting_Level < ATC_Level'Last or else
        Shutdown ("Attempt to add task entry call past nesting level max."));

      Caller.ATC_Nesting_Level := Caller.ATC_Nesting_Level + 1;

      Level := Caller.ATC_Nesting_Level;
      Entry_Call := Caller.Entry_Calls (Level)'Access;

      --  The caller's lock is not needed here. The call record does not
      --  need protection, since other tasks only access these records
      --  when they are queued, which this one is not.

      Entry_Call.Next := null;
      Entry_Call.Mode := Mode;

      --  If this is a call made inside of an abort deferred region,
      --  the call should be never abortable.
      if Caller.Deferral_Level > 1 then
         Entry_Call.Abortable := Never;
      else
         Entry_Call.Abortable := True;
      end if;

      Entry_Call.Done := False;
      Entry_Call.Has_Been_Abortable := False;
      Entry_Call.E := Entry_Index (E);
      Entry_Call.Prio := Get_Priority (Caller);
      Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
      Entry_Call.Called_PO := Object;
      Entry_Call.Called_Task := Null_Task;
      Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;

      Do_Or_Queue (Object, Entry_Call);
      Initially_Abortable := Entry_Call.Abortable = True;
      Service_Entries (Object);

      --  Indicate whether the call has been cancelled or not.
      --  A call cannot be in progress at this point, since the caller
      --  (this task) cannot be executing it, and we haven't given up
      --  the object lock yet, so no other task can be executing it.
      --  Therefore a call that is not on a queue but not complete must
      --  have been cancelled.  Similarly, no other task can be looking
      --  at the entry call record at this point, so we can check
      --  Entry_Call.Done without locking the caller's mutex.

      --  We have to consider the call is in acceptor's open_entry.
      --  The Abortable flag is off while the call is being served.
      --  It is sufficient to add the third condition in the test below
      --  to see if a call is cancelled or not

      Block.Cancelled :=
        not Entry_Call.Done
          and then not Queuing.Onqueue (Entry_Call)
          and then Entry_Call.Abortable = True;

      --  Call is enqueued if it is not done and not cancelled and
      --  on a queue somewhere.

      Block.Enqueued :=
        not (Entry_Call.Done or Block.Cancelled)
          and then Queuing.Onqueue (Entry_Call);

      --  Set the Enqueued flag

      --  Try to avoid waiting for completed or cancelled calls.

      if not (Entry_Call.Done or else Block.Cancelled) then

         Unlock (Object);

         case Mode is
            when Simple_Call | Conditional_Call =>
               Entry_Calls.Wait_For_Completion (Entry_Call);
            when Asynchronous_Call =>

               --  If the call was never enqueued, it is complete or
               --  cancelled at this point.  The compiler-generated code
               --  avoids calling Cancel_Protected_Entry_Call in this case,
               --  so we need to pop the entry call from the call stack
               --  at this point.

               --  ??? This complicates the interface, making it illegal
               --      to call Cancel_Protected_Entry_Call in this case,
               --      but mandatory to call it in other cases.  Consider
               --      making it mandatory in all cases.

               if not Block.Enqueued then
                  Caller.ATC_Nesting_Level := Caller.ATC_Nesting_Level - 1;

               else

                  --  If the call was not queued abortably, we need to wait
                  --  until it is before proceeding with the abortable part.
                  --  Wait_Until_Abortable can be called unconditionally here,
                  --  but it is expensive.

                  if not Initially_Abortable then
                     Entry_Calls.Wait_Until_Abortable
                       (Caller, Entry_Call);
                  end if;
               end if;
         end case;

      else
         Caller.ATC_Nesting_Level := Caller.ATC_Nesting_Level - 1;
         Unlock (Object);
      end if;

      Caller.Exception_To_Raise := Entry_Call.Exception_To_Raise;
      Undefer_Abort (Caller);
      Utilities.Check_Exception (Caller);

   end Protected_Entry_Call;

   ---------------------------------
   -- Cancel_Protected_Entry_Call --
   ---------------------------------

   procedure Cancel_Protected_Entry_Call (Block : in out Communication_Block)
   is
      Caller : Task_ID := Block.Self;
      Call   : Entry_Call_Link;

   begin
      pragma Assert (Caller.ATC_Nesting_Level > ATC_Level_Base'First or else
        Shutdown ("Attempt to cancel nonexistent task entry call."));

      Call := Caller.Entry_Calls (Caller.ATC_Nesting_Level)'Access;

      pragma Assert (Call.Mode = Asynchronous_Call or else
        Shutdown (
          "Attempt to perform ATC on non-asynchronous protected entry call"));

      pragma Assert (Call.Called_Task = Null_Task or else
        Shutdown (
          "Attempt to use Cancel_Protected_Entry_Call on task entry call."));

      Defer_Abort (Caller);

      Utilities.Abort_To_Level (Caller, Call.Level - 1);
      Entry_Calls.Wait_For_Completion (Call);

      --  This allows the triggered statements to be skipped.
      --  We can check Call.Done here without locking the caller's mutex,
      --  since the call must be over after returning from Wait_For_Completion.
      --  No other task can access the call record at this point.

      Block.Cancelled := not Call.Done;

      Undefer_Abort (Caller);
      Utilities.Check_Exception (Caller);

   end Cancel_Protected_Entry_Call;

   -------------------------
   -- Complete_Entry_Body --
   -------------------------

   procedure Complete_Entry_Body (Object : access Protection) is

   begin
      Exceptional_Complete_Entry_Body
        (Object, Ada.Exceptions.Null_Id);
   end Complete_Entry_Body;

   -------------------------------------
   -- Exceptional_Complete_Entry_Body --
   -------------------------------------

   procedure Exceptional_Complete_Entry_Body
     (Object : access Protection;
      Ex     : Ada.Exceptions.Exception_Id)
   is
      Caller : Task_ID := Object.Call_In_Progress.Self;

   begin
      Object.Call_In_Progress.Exception_To_Raise := Ex;

      Write_Lock (Caller);
      Object.Call_In_Progress.Done := True;

      if Object.Call_In_Progress.Mode = Asynchronous_Call then

         Unlock (Caller);

         --  If the asynchronous call has never been queued abortably, the
         --  abortable part will have been skipped; there is no need to abort
         --  it.

         if Object.Call_In_Progress.Has_Been_Abortable then
            Utilities.Abort_To_Level (
              Caller, Object.Call_In_Progress.Level - 1);
         end if;

      else
         Wakeup (Caller);
         Unlock (Caller);
      end if;
   end Exceptional_Complete_Entry_Body;

   -----------------------------
   -- Requeue_Protected_Entry --
   -----------------------------

   procedure Requeue_Protected_Entry
     (Object     : access Protection;
      New_Object : access Protection;
      E          : Protected_Entry_Index;
      With_Abort : Boolean)
   is
      Entry_Call        : Entry_Call_Link := Object.Call_In_Progress;
      Caller            : Task_ID         := Entry_Call.Self;
      Call_Cancelled    : Boolean := False;

   begin
      --  We have to check if the requeue is internal one.
      --  If it is an internal one, no need to lock.
      if (Object /= New_Object) then
         Lock (New_Object);
      end if;

      if Entry_Call.Abortable /= Never then
         if With_Abort then
            Entry_Call.Abortable := True;
         else
            Entry_Call.Abortable := False;
         end if;
      end if;

      Entry_Call.Has_Been_Abortable :=
        With_Abort or Entry_Call.Has_Been_Abortable;
      Entry_Call.E := Entry_Index (E);
      Entry_Call.Called_PO := New_Object;

      if Object = New_Object
        and then (not With_Abort or else Entry_Call.Mode /= Conditional_Call)
      then
         Queuing.Enqueue (New_Object.Entry_Queues (E), Entry_Call);
      else
         Do_Or_Queue (New_Object, Entry_Call);
      end if;

      if (Object /= New_Object) then
         Object.Call_In_Progress := null;
         Service_Entries (New_Object);
         Unlock (New_Object);
      end if;

      Write_Lock (Caller);
      Caller.Pending_Action := True;

      Wakeup (Caller);
      --  If this is a conditional entry call, and has just become
      --  abortable, the caller should be awakened to cancel the call.

      Unlock (Caller);
   end Requeue_Protected_Entry;

   -------------------------------------
   -- Requeue_Task_To_Protected_Entry --
   -------------------------------------

   procedure Requeue_Task_To_Protected_Entry
     (New_Object : access Protection;
      E          : Protected_Entry_Index;
      With_Abort : Boolean)
   is
      Old_Acceptor : Task_ID := Self;
      Entry_Call   : Entry_Call_Link;

   begin
      Lock (New_Object);

      Write_Lock (Old_Acceptor);
      Entry_Call := Old_Acceptor.Call;
      Old_Acceptor.Call := null;
      Entry_Call.Called_PO := New_Object;
      Entry_Call.Called_Task := Null_Task;
      Unlock (Old_Acceptor);

      if Entry_Call.Abortable /= Never then
         if With_Abort then
            Entry_Call.Abortable := True;
         else
            Entry_Call.Abortable := False;
         end if;
      end if;

      Entry_Call.Has_Been_Abortable :=
        With_Abort or Entry_Call.Has_Been_Abortable;
      Entry_Call.E := Entry_Index (E);

      Do_Or_Queue (New_Object, Entry_Call);
      Service_Entries (New_Object);

      Unlock (New_Object);
   end Requeue_Task_To_Protected_Entry;

   ---------------------
   -- Protected_Count --
   ---------------------

   function Protected_Count
     (Object : Protection;
      E      : Protected_Entry_Index)
      return   Natural
   is
   begin
      return Queuing.Count_Waiting (Object.Entry_Queues (E));
   end Protected_Count;

   ----------------------------
   -- Protected_Entry_Caller --
   ----------------------------

   function Protected_Entry_Caller (Object : Protection) return Task_ID is
   begin
      return Object.Call_In_Progress.Self;
   end Protected_Entry_Caller;

end System.Tasking.Protected_Objects;

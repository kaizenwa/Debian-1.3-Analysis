------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--            S Y S T E M . T A S K I N G . R E N D E Z V O U S             --
--                                                                          --
--                                 B o d y                                  --
--                         (Version for new GNARL)                          --
--                                                                          --
--                            $Revision: 1.57 $                             --
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
--  used for Get_Priority
--           Set_Priority
--           Write_Lock
--           Unlock
--           Sleep
--           Wakeup

with System.Tasking.Entry_Calls;
--  Used for Wait_For_Completion
--           Wait_Until_Abortable

with System.Tasking.Initialization;
--  used for Defer_Abort
--           Undefer_Abort
--           Change_Base_Priority

with System.Tasking.Queuing;
--  used for Enqueue
--           Dequeue_Head
--           Select_Task_Entry_Call
--           Count_Waiting

with System.Tasking.Utilities;
--  Used for Abort_To_Level
--           Reset_Priority
--           Terminate_Alternative
--           Check_Exception

with Unchecked_Conversion;

package body System.Tasking.Rendezvous is

   use System.Task_Primitives.Operations;
   use System.Error_Reporting;

   procedure Change_Base_Priority (Self_ID : Task_ID) renames
     System.Tasking.Initialization.Change_Base_Priority;

   procedure Defer_Abort (Self_ID : Task_ID) renames
     System.Tasking.Initialization.Defer_Abort;

   procedure Undefer_Abort (Self_ID : Task_ID) renames
     System.Tasking.Initialization.Undefer_Abort;

   type Select_Treatment is (
     Accept_Alternative_Selected,
     Accept_Alternative_Completed,
     Else_Selected,
     Terminate_Selected,
     Accept_Alternative_Open,
     No_Alternative_Open);

   Default_Treatment : constant array (Select_Modes) of Select_Treatment :=
     (Simple_Mode         => No_Alternative_Open,
      Else_Mode           => Else_Selected,
      Terminate_Mode      => Terminate_Selected,
      Delay_Mode          => No_Alternative_Open);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Boost_Priority
     (Call     : Entry_Call_Link;
      Acceptor : Task_ID);
   pragma Inline (Boost_Priority);

   procedure Call_Synchronous
     (Acceptor              : Task_ID;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Mode                  : Call_Modes;
      Rendezvous_Successful : out Boolean);
   pragma Inline (Call_Synchronous);
   --  This call is used to make a simple or conditional entry call.

   procedure Do_Or_Queue
     (Self_ID    : Task_ID;
      Entry_Call : in out Entry_Call_Link);
   --  Either initiate the entry call, such that the accepting task is
   --  free to execute the rendezvous, queue the call on the acceptor's
   --  queue, or cancel the call.  Conditional calls that cannot be
   --  accepted immediately are cancelled.

   procedure Adjust_For_Terminate_Alternative_Call
     (Self_ID  : Task_ID;
      Acceptor : Task_ID);
   --  Called by caller to wake up the acceptor if it is waiting on
   --  terminate_alternative.

   procedure Setup_For_Rendezvous_With_Body
     (Entry_Call : Entry_Call_Link; Acceptor : Task_ID);
   --  When a rendezvous selected (ready for rendezvous) we need to save
   --  privious caller and adjust the priority. Also we need to make
   --  this call not Abortable (Cancellable) since the rendezvous has
   --  already been started.
   pragma Inline (Setup_For_Rendezvous_With_Body);

   procedure Await_Abortion (Acceptor : Task_ID);

   function Is_Entry_Open (T : Task_ID; E : Task_Entry_Index) return Boolean;
   pragma Inline (Is_Entry_Open);

   procedure Wait_For_Call (Self_ID : Task_ID);
   pragma Inline (Wait_For_Call);
   --  An accepting task goes into Sleep by calling this routine
   --  waiting for a call from the caller or waiting for an abortion.
   --  Make sure Self_ID is locked before calling this routine.

   --------------------
   -- Boost_Priority --
   --------------------

   procedure Boost_Priority (Call : Entry_Call_Link; Acceptor : Task_ID) is
      Caller        : Task_ID := Call.Self;
      Caller_Prio   : System.Any_Priority := Get_Priority (Caller);
      Acceptor_Prio : System.Any_Priority := Get_Priority (Acceptor);
   begin

      if Caller_Prio > Acceptor_Prio then
         Call.Acceptor_Prev_Priority := Acceptor_Prio;
         Set_Priority (Acceptor, Caller_Prio);
      else
         Call.Acceptor_Prev_Priority := Priority_Not_Boosted;
      end if;
   end Boost_Priority;

   -----------------
   -- Do_Or_Queue --
   -----------------

   procedure Do_Or_Queue
     (Self_ID       : Task_ID;
      Entry_Call : in out Entry_Call_Link)
   is
      E        : Task_Entry_Index := Task_Entry_Index (Entry_Call.E);
      Acceptor : Task_ID          := Entry_Call.Called_Task;

   begin
      if Acceptor.Accepting = Not_Accepting then
         if Callable (Acceptor) then
            if Entry_Call.Mode /= Conditional_Call
              or else Entry_Call.Abortable = False
            then
               Queuing.Enqueue (Acceptor.Entry_Queues (E), Entry_Call);
            end if;
         else
            --  If the acceptor is not callable, cancel the call
            --  and raise Tasking_Error.  The call is not cancelled
            --  for an asynchronous call, since Cancel_Task_Entry_Call
            --  will do the decrement in that case.

            --  ??? It would be better if all entry call cancellation
            --      and the raising of Tasking_Error could be isolated
            --      to Wait_For_Completion.
            if Entry_Call.Mode /= Asynchronous_Call then
               Entry_Call.Self.ATC_Nesting_Level :=
                 Entry_Call.Self.ATC_Nesting_Level - 1;
            end if;
            Unlock (Acceptor);
            Undefer_Abort (Self_ID);
            raise Tasking_Error;
         end if;
      else
         --  ??? This should have a special case for Trivial_Accept, so that
         --      we don't have the loop setup overhead.

         for J in Acceptor.Open_Accepts'Range loop
            if Entry_Call.E = Entry_Index (Acceptor.Open_Accepts (J).S) then

               --  Do rendezvous

               Acceptor.Accepting := Not_Accepting;
               Acceptor.Chosen_Index := J;

               --  Not abortable while in progress.

               if Entry_Call.Abortable /= Never then
                  Entry_Call.Abortable := False;
               end if;

               if Acceptor.Open_Accepts (J).Null_Body then

                  --  Normally, this would have to be protected by
                  --  the caller's mutex.  However, in this case we
                  --  know that the acceptor is accepting, which means
                  --  that it has yet to remove a call from its queue,
                  --  and it will need to lock its own mutex to do that,
                  --  which we hold.  It won't look at Entry_Call.Done
                  --  until it has the call, so it should be safe to
                  --  set it here.

                  Entry_Call.Done := True;

                  Wakeup (Acceptor);
               else
                  Setup_For_Rendezvous_With_Body (Entry_Call, Acceptor);
                  Wakeup (Acceptor);
               end if;

               exit;
            end if;

         end loop;

         --  If the acceptor was ready to accept this call,
         --  Acceptor.Accepting will have been set to Not_Accepting
         --  in the above loop.  Otherwise, the acceptor is accepting,
         --  but not this entry.  Try to queue the call.

         if Acceptor.Accepting /= Not_Accepting and then
           (Entry_Call.Mode /= Conditional_Call or else
              Entry_Call.Abortable = False) then
            Queuing.Enqueue (Acceptor.Entry_Queues (E), Entry_Call);
         end if;

      end if;
   end Do_Or_Queue;

   -------------------------------------------
   -- Adjust_For_Terminate_Alternative_Call --
   -------------------------------------------

   procedure Adjust_For_Terminate_Alternative_Call
     (Self_ID  : Task_ID;
      Acceptor : Task_ID)
   is
      P     : Task_ID;

   begin

      --  This call is made with the Acceptor locked.

      Acceptor.Stage := Active;

      --  Need to set this flag off in order not to make subsequent calls
      --  to be treated to calls to Select With Terminate Alternative.

      Acceptor.Terminate_Alternative := False;

      Acceptor.Awake_Count := Acceptor.Awake_Count + 1;

      --  At this point, T.Awake_Count and P.Awaited_Dependent_Count could
      --  be out of synchronization. However, we know that
      --  P.Awaited_Dependent_Count cannot be zero, and cannot go to zero,
      --  since some other dependent must have just called us. There should
      --  therefore be no danger of the parent terminating before we
      --  increment P.Awaited_Dependent_Count below.

      if Acceptor.Awake_Count = 1 then
         Unlock (Acceptor);

         if Acceptor.Pending_ATC_Level < Acceptor.ATC_Nesting_Level then
            Undefer_Abort (Self_ID);
            pragma Assert (Shutdown ("Continuing after being aborted!"));
         end if;

         P := Acceptor.Parent;
         Write_Lock (P);

         if P.Awake_Count /= 0 then
            P.Awake_Count := P.Awake_Count + 1;

         else
            Unlock (P);
            Utilities.Abort_To_Level (Acceptor, 0);
            Undefer_Abort (Self_ID);
            pragma Assert (Shutdown ("Continuing after being aborted!"));
         end if;

         --  Conservative checks which should only matter when an interrupt
         --  entry was chosen. In this case, the current task completes if
         --  the parent has already been signaled that all children have
         --  terminated.

         if Acceptor.Master_of_Task = P.Master_Within then
            if P.Awaited_Dependent_Count /= 0 then
               P.Awaited_Dependent_Count := P.Awaited_Dependent_Count + 1;

            elsif P.Stage = Await_Dependents then
               Unlock (P);
               Utilities.Abort_To_Level (Acceptor, 0);
               Undefer_Abort (Self_ID);
               pragma Assert (Shutdown ("Continuing after being aborted!"));
            end if;
         end if;

         Unlock (P);

      else
         Unlock (Acceptor);

         if Acceptor.Pending_ATC_Level < Acceptor.ATC_Nesting_Level then
            Undefer_Abort (Self_ID);
            pragma Assert (Shutdown ("Continuing after being aborted!"));
         end if;
      end if;

      Write_Lock (Acceptor);

   end Adjust_For_Terminate_Alternative_Call;

   ------------------------------------
   -- Setup_For_Rendezvous_With_Body --
   ------------------------------------

   procedure Setup_For_Rendezvous_With_Body
     (Entry_Call : Entry_Call_Link; Acceptor : Task_ID) is
   begin
      Entry_Call.Acceptor_Prev_Call := Acceptor.Call;
      Acceptor.Call := Entry_Call;
      if Entry_Call.Abortable /= Never then
         Entry_Call.Abortable := False;
      end if;
      Boost_Priority (Entry_Call, Acceptor);
   end Setup_For_Rendezvous_With_Body;

   ----------------------
   -- Call_Synchronous --
   ----------------------

   procedure Call_Synchronous
     (Acceptor              : Task_ID;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Mode                  : Call_Modes;
      Rendezvous_Successful : out Boolean)
   is
      Caller     : constant Task_ID := Self;
      Level      : ATC_Level;
      Entry_Call : Entry_Call_Link;

   begin
      pragma Assert (Mode /= Asynchronous_Call
        or else Shutdown ("Asynchronous call being treated synchronously."));

      Defer_Abort (Caller);
      Caller.ATC_Nesting_Level := Caller.ATC_Nesting_Level + 1;
      Level := Caller.ATC_Nesting_Level;

      Entry_Call := Caller.Entry_Calls (Level)'Access;

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
      Entry_Call.E := Entry_Index (E);
      Entry_Call.Prio := Get_Priority (Caller);
      Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
      Entry_Call.Called_Task := Acceptor;
      Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;

      --  Note: the caller will undefer abortion on return (see WARNING above)

      Write_Lock (Acceptor);

      if Acceptor.Terminate_Alternative and then
        Is_Entry_Open (Acceptor, E) then
         Adjust_For_Terminate_Alternative_Call (Caller, Acceptor);
      end if;

      Do_Or_Queue (Caller, Entry_Call);
      Unlock (Acceptor);
      System.Tasking.Entry_Calls.Wait_For_Completion (Entry_Call);
      Rendezvous_Successful := Entry_Call.Done;
      Undefer_Abort (Caller);

      pragma Assert (
        Caller.Pending_ATC_Level >= Caller.ATC_Nesting_Level
        or else Shutdown ("Continuing after aborting self!"));

      Utilities.Check_Exception (Caller);
   end Call_Synchronous;

   -----------------
   -- Call_Simple --
   -----------------

   procedure Call_Simple
     (Acceptor           : Task_ID;
      E                  : Task_Entry_Index;
      Uninterpreted_Data : System.Address)
   is
      Rendezvous_Successful : Boolean;

   begin
      Call_Synchronous
        (Acceptor, E, Uninterpreted_Data, Simple_Call, Rendezvous_Successful);
   end Call_Simple;

   ----------------------------
   -- Cancel_Task_Entry_Call --
   ----------------------------

   procedure Cancel_Task_Entry_Call (Cancelled : out Boolean) is
      Caller : constant Task_ID := Self;
      Call   : Entry_Call_Link;

   begin
      pragma Assert (Caller.ATC_Nesting_Level > ATC_Level_Base'First
        or else Shutdown ("Attempt to cancel nonexistent task entry call"));

      Call := Caller.Entry_Calls (Caller.ATC_Nesting_Level)'Access;

      pragma Assert (Call.Mode = Asynchronous_Call
        or else Shutdown ("Attempt to do ATC on non-async task entry call"));

      pragma Assert (Call.Called_PO = Null_PO
        or else Shutdown
         ("Attempt to use Cancel_Task_Entry_Call on protected entry call."));

      Defer_Abort (Caller);

      Utilities.Abort_To_Level (Caller, Call.Level - 1);
      Entry_Calls.Wait_For_Completion (Call);

      --  Allow the triggered statements to be skipped

      Cancelled := not Call.Done;

      Undefer_Abort (Caller);
      Utilities.Check_Exception (Caller);
   end Cancel_Task_Entry_Call;

   ------------------------
   -- Requeue_Task_Entry --
   ------------------------

   procedure Requeue_Task_Entry
     (Acceptor   : Task_ID;
      E          : Task_Entry_Index;
      With_Abort : Boolean)
   is
      Old_Acceptor : constant Task_ID := Self;
      Caller       : Task_ID;
      Entry_Call   : Entry_Call_Link;

   begin
      Defer_Abort (Old_Acceptor);
      Write_Lock (Old_Acceptor);
      Entry_Call := Old_Acceptor.Call;
      Caller := Entry_Call.Self;

      Old_Acceptor.Call := Entry_Call.Acceptor_Prev_Call;

      --  Don't permit this call to be aborted until we have switched to
      --  the new acceptor.  Otherwise, we may queue a cancelled call below.

      if Entry_Call.Abortable /= Never then
         Entry_Call.Abortable := False;
      end if;

      Unlock (Old_Acceptor);

      Entry_Call.E := Entry_Index (E);

      Write_Lock (Acceptor);
      Entry_Call.Called_Task := Acceptor;
      if Entry_Call.Abortable /= Never then
         if With_Abort then
            Entry_Call.Abortable := True;
         else
            Entry_Call.Abortable := False;
         end if;
      end if;
      Entry_Call.Has_Been_Abortable :=
        With_Abort or Entry_Call.Has_Been_Abortable;
      Do_Or_Queue (Old_Acceptor, Entry_Call);
      Unlock (Acceptor);

      Write_Lock (Caller);
      Caller.Pending_Action := True;

      --  If this is a conditional entry call, and has just become
      --  abortable, the caller should be awakened to cancel the call.

      Wakeup (Caller);

      Unlock (Caller);
      Undefer_Abort (Old_Acceptor);
   end Requeue_Task_Entry;

   -------------------------------------
   -- Requeue_Protected_To_Task_Entry --
   -------------------------------------

   procedure Requeue_Protected_To_Task_Entry
     (Object     : Protection_Access;
      Acceptor   : Task_ID;
      E          : Task_Entry_Index;
      With_Abort : Boolean)
   is
      Self_ID  : Task_ID := Self;
      Entry_Call : Entry_Call_Link := Object.Call_In_Progress;
      Caller     : Task_ID         := Entry_Call.Self;

   begin
      Defer_Abort (Self_ID);
      Entry_Call.E := Entry_Index (E);
      Object.Call_In_Progress := null;

      Write_Lock (Acceptor);
      Entry_Call.Called_Task := Acceptor;
      Entry_Call.Called_PO := Null_PO;
      if Entry_Call.Abortable /= Never then
         if With_Abort then
            Entry_Call.Abortable := True;
         else
            Entry_Call.Abortable := False;
         end if;
      end if;
      Entry_Call.Has_Been_Abortable :=
        With_Abort or Entry_Call.Has_Been_Abortable;
      Do_Or_Queue (Self_ID, Entry_Call);
      Unlock (Acceptor);

      Write_Lock (Caller);
      Entry_Call.E := Entry_Index (E);

      --  If this is a conditional entry call, and has just become
      --  abortable, the caller should be awakened to cancel the call.

      Caller.Pending_Action := True;
      Wakeup (Caller);

      Unlock (Caller);
      Undefer_Abort (Self_ID);
   end Requeue_Protected_To_Task_Entry;

   ---------------------
   -- Task_Entry_Call --
   ---------------------

   procedure Task_Entry_Call
     (Acceptor              : Task_ID;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Mode                  : Call_Modes;
      Rendezvous_Successful : out Boolean)
   is
      Caller                : constant Task_ID := Self;
      Entry_Call            : Entry_Call_Link;
      Initially_Abortable   : Boolean;

   begin
      --  Simple or conditional call

      if Mode = Simple_Call or else Mode = Conditional_Call then
         Call_Synchronous
           (Acceptor, E, Uninterpreted_Data, Mode, Rendezvous_Successful);

      --  Asynchronous call

      else
         --  Abortion must already be deferred by the compiler-generated
         --  code.  Without this, an abortion that occurs between the time
         --  that this call is made and the time that the abortable part's
         --  cleanup handler is set up might miss the cleanup handler and
         --  leave the call pending.

         Caller.ATC_Nesting_Level := Caller.ATC_Nesting_Level + 1;

         Entry_Call := Caller.Entry_Calls (Caller.ATC_Nesting_Level)'Access;

         Entry_Call.Next := null;
         Entry_Call.Mode := Mode;
         Entry_Call.Abortable := True;
         Entry_Call.Done := False;
         Entry_Call.E := Entry_Index (E);
         Entry_Call.Prio := Get_Priority (Caller);
         Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
         Entry_Call.Called_Task := Acceptor;
         Entry_Call.Called_PO := Null_PO;
         Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;

         Write_Lock (Acceptor);

         if Acceptor.Terminate_Alternative and then
           Is_Entry_Open (Acceptor, E) then
            Adjust_For_Terminate_Alternative_Call (Caller, Acceptor);
         end if;

         Do_Or_Queue (Caller, Entry_Call);

         Initially_Abortable := Entry_Call.Abortable = True;

         Unlock (Acceptor);

         --  If the call was not queued abortably, we need to wait until
         --  it is before proceeding with the abortable part.
         --  Wait_Until_Abortable can be called unconditionally here,
         --  but it is expensive.

         if not Initially_Abortable then
            Entry_Calls.Wait_Until_Abortable (Caller, Entry_Call);
         end if;

         --  Note: following assignment needs to be atomic.

         Rendezvous_Successful := Entry_Call.Done;

      end if;
   end Task_Entry_Call;

   -----------------
   -- Accept_Call --
   -----------------

   procedure Accept_Call
     (E                  : Task_Entry_Index;
      Uninterpreted_Data : out System.Address)
   is
      Acceptor     : constant Task_ID := Self;
      Caller       : Task_ID := null;
      Open_Accepts : aliased Accept_List (1 .. 1);
      Entry_Call   : Entry_Call_Link;

   begin
      Defer_Abort (Acceptor);
      Write_Lock (Acceptor);

      --  If someone completed this task, this task should not try to
      --  access its pending entry calls or queues in this case, as they
      --  are being emptied. Wait for abortion to kill us.

      if Acceptor.Stage >= Complete then
         Await_Abortion (Acceptor);
      end if;

      Queuing.Dequeue_Head (Acceptor.Entry_Queues (E), Entry_Call);

      if Entry_Call /= null then
         Caller := Entry_Call.Self;
         Setup_For_Rendezvous_With_Body (Entry_Call, Acceptor);
         Uninterpreted_Data := Entry_Call.Uninterpreted_Data;

      else
         --  Wait for a caller

         Open_Accepts (1).Null_Body := False;
         Open_Accepts (1).S := E;
         Acceptor.Open_Accepts := Open_Accepts'Unchecked_Access;

         Acceptor.Accepting := Simple_Accept;

         --  Wait for normal call
         Wait_For_Call (Acceptor);

         if Acceptor.Pending_ATC_Level >= Acceptor.ATC_Nesting_Level then
            Caller := Acceptor.Call.Self;
            Uninterpreted_Data :=
              Caller.Entry_Calls (Caller.ATC_Nesting_Level).Uninterpreted_Data;
         end if;

         --  If this task has been aborted, skip the Uninterpreted_Data load
         --  (Caller will not be reliable) and fall through to
         --  Undefer_Abortion which will allow the task to be killed.
      end if;

      --  Acceptor.Call should already be updated by the Caller

      Unlock (Acceptor);
      Undefer_Abort (Acceptor);

      --  Start rendezvous

   end Accept_Call;

   --------------------
   -- Accept_Trivial --
   --------------------

   procedure Accept_Trivial (E : Task_Entry_Index) is
      Acceptor      : constant Task_ID := Self;
      Caller        : Task_ID := null;
      Open_Accepts  : aliased Accept_List (1 .. 1);
      Entry_Call    : Entry_Call_Link;
      Prev_Priority : Rendezvous_Priority := Priority_Not_Boosted;

   begin
      Defer_Abort (Acceptor);
      Write_Lock (Acceptor);

      --  If someone completed this task, this task should not try to
      --  access its pending entry calls or queues in this case, as they
      --  are being emptied. Wait for abortion to kill us.

      if Acceptor.Stage >= Complete then
         Await_Abortion (Acceptor);
      end if;

      Queuing.Dequeue_Head (Acceptor.Entry_Queues (E), Entry_Call);

      if Entry_Call = null then

         --  Need to wait for call

         Open_Accepts (1).Null_Body := False;
         Open_Accepts (1).S := E;
         Acceptor.Open_Accepts := Open_Accepts'Unchecked_Access;

         Acceptor.Accepting := Trivial_Accept;

         --  Wait for normal entry call
         Wait_For_Call (Acceptor);

         if Acceptor.Pending_ATC_Level < Acceptor.ATC_Nesting_Level then
            Unlock (Acceptor);
            Undefer_Abort (Acceptor);
            pragma Assert (Shutdown ("Continuing after being aborted!"));

         else
            Entry_Call := Acceptor.Call;
            Acceptor.Call := Entry_Call.Acceptor_Prev_Call;
         end if;

      else

         --  No longer abortable

         Entry_Call.Abortable := False;
      end if;

      Unlock (Acceptor);
      Caller := Entry_Call.Self;
      Write_Lock (Caller);

      --  Done with mutex locked to make sure that signal is not lost.

      Entry_Call.Done := True;

      Prev_Priority := Entry_Call.Acceptor_Prev_Priority;

      if Entry_Call.Mode = Asynchronous_Call then
         Unlock (Caller);

         Utilities.Abort_To_Level (Caller, Entry_Call.Level - 1);
      else
         Wakeup (Caller);
         Unlock (Caller);
      end if;

      Utilities.Reset_Priority (Prev_Priority, Acceptor);

      Undefer_Abort (Acceptor);
   end Accept_Trivial;

   -------------------------------------
   -- Exceptional_Complete_Rendezvous --
   -------------------------------------

   procedure Exceptional_Complete_Rendezvous
     (Ex : Ada.Exceptions.Exception_ID)
   is
      Acceptor      : constant Task_ID := Self;
      Caller        : Task_ID;
      Call          : Entry_Call_Link;
      Prev_Priority : Rendezvous_Priority;

      use Ada.Exceptions;

   begin
      Defer_Abort (Acceptor);
      Call := Acceptor.Call;
      Acceptor.Call := Call.Acceptor_Prev_Call;
      Prev_Priority := Call.Acceptor_Prev_Priority;
      Call.Exception_To_Raise := Ex;
      Caller := Call.Self;
      Write_Lock (Caller);

      --  Done with mutex locked to make sure that signal is not lost.

      Call.Done := True;

      if Call.Mode = Asynchronous_Call then
         Unlock (Caller);
         Utilities.Abort_To_Level (Caller, Call.Level - 1);
      else
         Wakeup (Caller);
         Unlock (Caller);
      end if;

      Utilities.Reset_Priority (Prev_Priority, Acceptor);

      Acceptor.Exception_To_Raise := Ex;

      if Ex /= Ada.Exceptions.Null_Id then
         Duplex_Message : declare
            Len : Natural :=  Acceptor.Compiler_Data.Message_Length;
            type Thin_Ptr is access String (Positive);

            function To_Ptr is
              new Unchecked_Conversion (Address, Thin_Ptr);

         begin
            Caller.Compiler_Data.GNAT_Exception :=
              Acceptor.Compiler_Data.GNAT_Exception;

            Caller.Compiler_Data.Message_Length := Len;

            To_Ptr (Caller.Compiler_Data.Message_Addr) (1 .. Len) :=
              To_Ptr (Acceptor.Compiler_Data.Message_Addr) (1 .. Len);
         end Duplex_Message;
      end if;

      Undefer_Abort (Acceptor);
      Utilities.Check_Exception (Acceptor);
   end Exceptional_Complete_Rendezvous;

   -------------------------
   -- Complete_Rendezvous --
   -------------------------

   procedure Complete_Rendezvous is
   begin
      Exceptional_Complete_Rendezvous (Ada.Exceptions.Null_Id);
   end Complete_Rendezvous;

   --------------------
   -- Selective_Wait --
   --------------------

   procedure Selective_Wait
     (Open_Accepts       : Accept_List_Access;
      Select_Mode        : Select_Modes;
      Uninterpreted_Data : out System.Address;
      Index              : out Select_Index)
   is
      Acceptor         : constant Task_ID := Self;
      Treatment        : Select_Treatment;
      Entry_Call       : Entry_Call_Link;
      Caller           : Task_ID;
      Selection        : Select_Index;
      Open_Alternative : Boolean;

   begin
      Defer_Abort (Acceptor);
      Write_Lock (Acceptor);

      --  If someone completed this task, this task should not try to
      --  access its pending entry calls or queues in this case, as they
      --  are being emptied. Wait for abortion to kill us.

      if Acceptor.Stage >= Complete then
         Await_Abortion (Acceptor);
      end if;

      Queuing.Select_Task_Entry_Call
        (Acceptor, Open_Accepts, Entry_Call, Selection, Open_Alternative);

      --  Determine the kind and disposition of the select.

      Treatment := Default_Treatment (Select_Mode);
      Acceptor.Chosen_Index := No_Rendezvous;

      if Open_Alternative then
         if Entry_Call /= null then
            if Open_Accepts (Selection).Null_Body then
               Treatment := Accept_Alternative_Completed;
            else
               Setup_For_Rendezvous_With_Body (Entry_Call, Acceptor);
               Treatment := Accept_Alternative_Selected;
            end if;

            Acceptor.Chosen_Index := Selection;

         elsif Treatment = No_Alternative_Open then
            Treatment := Accept_Alternative_Open;
         end if;
      end if;

      --  Handle the select according to the disposition selected above.

      case Treatment is

      when Accept_Alternative_Selected =>

         --  Ready to rendezvous already

         Uninterpreted_Data := Acceptor.Call.Uninterpreted_Data;

         --  In this case the accept body is not Null_Body. Defer abortion
         --  until it gets into the accept body.
         Defer_Abort (Acceptor);
         Unlock (Acceptor);

      when Accept_Alternative_Completed =>

         --  Rendezvous is over

         Unlock (Acceptor);
         Caller := Entry_Call.Self;
         Write_Lock (Caller);
         Entry_Call.Done := True;

         if Entry_Call.Mode = Asynchronous_Call then
            Unlock (Caller);
            Utilities.Abort_To_Level (Caller, Entry_Call.Level - 1);
         else
            Wakeup (Caller);
            Unlock (Caller);
         end if;

      when Accept_Alternative_Open =>

         --  Wait for caller.

         Acceptor.Open_Accepts := Open_Accepts;

         Acceptor.Accepting := Select_Wait;

         Wait_For_Call (Acceptor);

         --  Acceptor.Call should already be updated by the Caller if
         --  not aborted. It might also be ready to do rendezvous even if
         --  this wakes up due to an abortion.
         --  Therefore, if the call is not empty we need to do the rendezvous
         --  if the accept body is not Null_Body.
         if Acceptor.Call /= null and then
           not Open_Accepts (Acceptor.Chosen_Index).Null_Body then
            Uninterpreted_Data := Acceptor.Call.Uninterpreted_Data;
            Defer_Abort (Acceptor);
         end if;

         Unlock (Acceptor);

      when Else_Selected =>
         Acceptor.Accepting := Not_Accepting;
         Unlock (Acceptor);

      when Terminate_Selected =>

         --  Terminate alternative is open

         Acceptor.Open_Accepts := Open_Accepts;

         Acceptor.Accepting := Select_Wait;

         --  We need to check if a signal is pending on an open interrupt
         --  entry. Otherwise this task would become passive (since terminate
         --  alternative is open) and, if none of the siblings are active
         --  any more, the task could not wake up any more, even though a
         --  signal might be pending on an open interrupt entry.

         Unlock (Acceptor);
         Utilities.Terminate_Alternative (Acceptor);

         --  Wait for normal entry call or termination

         --  consider letting Terminate_Alternative assume mutex L
         --  is already locked, and return with it locked, so
         --  this code could be simplified???

         --  No return here if Acceptor completes, otherwise
         --  Acceptor.Call should already be updated by the Caller

         Write_Lock (Acceptor);

         Index := Acceptor.Chosen_Index;

         if not Open_Accepts (Acceptor.Chosen_Index).Null_Body then
            Uninterpreted_Data := Acceptor.Call.Uninterpreted_Data;
            Defer_Abort (Acceptor);
         end if;

         Unlock (Acceptor);

         Undefer_Abort (Acceptor);
         return;

      when No_Alternative_Open =>

         --  In this case, Index will be No_Rendezvous on return, which
         --  should cause a Program_Error if it is not a Delay_Mode.

         --  If delay altenative exists (Delay_Mode) we should suspend
         --  until the delay expires.

         if Select_Mode = Delay_Mode then
            while not Acceptor.Pending_Action loop
               Sleep (Acceptor);
            end loop;
            Unlock (Acceptor);
         else
            Unlock (Acceptor);
            Undefer_Abort (Acceptor);
            raise Program_Error;
         end if;

      end case;

      --  Caller has been chosen

      --  Acceptor.Call should already be updated by the Caller

      --  Acceptor.Chosen_Index should either be updated by the Caller
      --  or by Test_Selective_Wait

      Index := Acceptor.Chosen_Index;
      Undefer_Abort (Acceptor);

      --  Start rendezvous, if not already completed.

   end Selective_Wait;

   ----------------
   -- Task_Count --
   ----------------

   function Task_Count (E : Task_Entry_Index) return Natural is
      Self_ID : constant Task_ID := Self;
      Return_Count : Natural;

   begin
      Write_Lock (Self_ID);
      Return_Count := Queuing.Count_Waiting (Self_ID.Entry_Queues (E));
      Unlock (Self_ID);
      return Return_Count;
   end Task_Count;

   --------------
   -- Callable --
   --------------

   function Callable (T : Task_ID) return Boolean is
   begin
      return T.Stage < Complete and then
        T.Pending_ATC_Level > ATC_Level_Base'First;
   end Callable;

   --------------------
   -- Await_Abortion --
   --------------------

   procedure Await_Abortion (Acceptor : Task_ID) is
   begin
      loop
         if Acceptor.Pending_Action then
            if Acceptor.Pending_Priority_Change then
               Change_Base_Priority (Acceptor);
            end if;

            exit when
               Acceptor.Pending_ATC_Level < Acceptor.ATC_Nesting_Level;
            Acceptor.Pending_Action := False;
         end if;

         Sleep (Acceptor);
      end loop;

      Unlock (Acceptor);
      Undefer_Abort (Acceptor);
      pragma Assert (Shutdown ("Continuing execution after being aborted"));
   end Await_Abortion;

   -------------------
   -- Is_Entry_Open --
   -------------------

   function Is_Entry_Open (T : Task_ID; E : Task_Entry_Index) return Boolean is
   begin
      for J in T.Open_Accepts'Range loop
         if E = T.Open_Accepts (J).S then
            return True;
         end if;
      end loop;
      return False;
   end Is_Entry_Open;

   -----------------------
   -- Task_Entry_Caller --
   -----------------------

   function Task_Entry_Caller (D : Task_Entry_Nesting_Depth) return Task_ID is
      Entry_Call : Entry_Call_Link;

   begin
      Entry_Call := Self.Call;
      for Depth in 1 .. D loop
         Entry_Call := Entry_Call.Acceptor_Prev_Call;
         pragma Assert (Entry_Call /= null);
      end loop;
      return Entry_Call.Self;
   end Task_Entry_Caller;

   -------------------
   -- Wait_For_Call --
   -------------------

   procedure Wait_For_Call (Self_ID : Task_ID) is
   begin
      --  Wait for normal call and a pending action.
      while Self_ID.Accepting /= Not_Accepting loop
         if Self_ID.Pending_Action then
            if Self_ID.Pending_Priority_Change then
               Change_Base_Priority (Self_ID);
            end if;

            if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level then
               Self_ID.Accepting := Not_Accepting;
               exit;
            end if;

            Self_ID.Pending_Action := False;
         end if;
         Sleep (Self_ID);
      end loop;
   end Wait_For_Call;

end System.Tasking.Rendezvous;

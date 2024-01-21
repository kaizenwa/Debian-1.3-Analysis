------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--            S Y S T E M . T A S K I N G . R E N D E Z V O U S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.53 $                             --
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

with System.Task_Primitives; use System.Task_Primitives;

with System.Tasking.Abortion;
--  Used for, Abortion.Defer_Abortion_Self,
--            Abortion.Undefer_Abortion_Self,
--            Abortion.Change_Base_Priority

with System.Tasking.Queuing; use System.Tasking.Queuing;
--  Used for, Queuing.Enqueue,
--            Queuing.Dequeue,
--            Queuing.Dequeue_Head,
--            Queuing.Count_Waiting,
--            Queuing.Select_Task_Entry_Call

with System.Tasking.Utilities;
--  Used for, Utilities.Abort_To_Level
--            Utilities.Reset_Priority
--            Utilities.Terminate_Alternative
--            Utilities.Runtime_Assert_Shutdown
--            Utilities.Wait_For_Completion;

with System.Tasking.Entry_Calls;
--  Used for, Wait_For_Completion
--            Wait_Until_Abortable

with Ada.Exceptions;
--  Used for, Exception_Id
--            Raise_Exception

with Unchecked_Conversion;

package body System.Tasking.Rendezvous is

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
     (Entry_Call : in out Entry_Call_Link);
   --  Either initiate the entry call, such that the accepting task is
   --  free to execute the rendezvous, queue the call on the acceptor's
   --  queue, or cancel the call.  Conditional calls that cannot be
   --  accepted immediately are cancelled.

   procedure Adjust_For_Terminate_Alternative_Call (Acceptor : Task_ID);
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

   --------------------
   -- Boost_Priority --
   --------------------

   procedure Boost_Priority
     (Call     : Entry_Call_Link;
      Acceptor : Task_ID)
   is
      Caller : Task_ID := Call.Self;

   begin
      if Get_Priority (Caller.LL_TCB'Access) >
         Get_Priority (Acceptor.LL_TCB'Access)
      then
         Call.Acceptor_Prev_Priority := Acceptor.Current_Priority;
         Acceptor.Current_Priority := Caller.Current_Priority;
         Set_Priority (Acceptor.LL_TCB'Access, Acceptor.Current_Priority);
      else
         Call.Acceptor_Prev_Priority := Priority_Not_Boosted;
      end if;
   end Boost_Priority;

   -----------------
   -- Do_Or_Queue --
   -----------------

   procedure Do_Or_Queue
     (Entry_Call : in out Entry_Call_Link)
   is
      E        : Task_Entry_Index := Task_Entry_Index (Entry_Call.E);
      Acceptor : Task_ID          := Entry_Call.Called_Task;
   begin

      if Acceptor.Accepting = Not_Accepting then
         if Callable (Acceptor) then
            if Entry_Call.Mode /= Conditional_Call
              or else not Entry_Call.Abortable
            then
               Enqueue (Acceptor.Entry_Queues (E), Entry_Call);
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

            Unlock (Acceptor.L);
            Abortion.Undefer_Abortion_Self;
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

               Entry_Call.Abortable := False;

               --  Not abortable while in progress.

               if Acceptor.Open_Accepts (J).Null_Body then

                  Entry_Call.Done := True;

                  --  Normally, this would have to be protected by
                  --  the caller's mutex.  However, in this case we
                  --  know that the acceptor is accepting, which means
                  --  that it has yet to remove a call from its queue,
                  --  and it will need to lock its own mutex to do that,
                  --  which we hold.  It won't look at Entry_Call.Done
                  --  until it has the call, so it should be safe to
                  --  set it here.

                  Cond_Signal (Acceptor.Cond);
               else
                  Setup_For_Rendezvous_With_Body (Entry_Call, Acceptor);
                  Cond_Signal (Acceptor.Cond);
               end if;
               exit;
            end if;

         end loop;

         --  If the acceptor was ready to accept this call,
         --  Acceptor.Accepting will have been set to Not_Accepting
         --  in the above loop.  Otherwise, the acceptor is accepting,
         --  but not this entry.  Try to queue the call.

         if Acceptor.Accepting /= Not_Accepting
           and then (Entry_Call.Mode /= Conditional_Call
                       or else not Entry_Call.Abortable)
         then
            Enqueue (Acceptor.Entry_Queues (E), Entry_Call);
         end if;

      end if;
   end Do_Or_Queue;

   -------------------------------------------
   -- Adjust_For_Terminate_Alternative_Call --
   -------------------------------------------

   procedure Adjust_For_Terminate_Alternative_Call (Acceptor : Task_ID) is
      P     : Task_ID;
      Error : Boolean;
   begin
--    Write_Lock (Acceptor.L, Error); ???
--
--    if Acceptor.Terminate_Alternative then
         Acceptor.Stage := Active;
         Acceptor.Awake_Count := Acceptor.Awake_Count + 1;

         --  At this point, T.Awake_Count and P.Awaited_Dependent_Count could
         --  be out of synchronization. However, we know that
         --  P.Awaited_Dependent_Count cannot be zero, and cannot go to zero,
         --  since some other dependent must have just called us. There should
         --  therefore be no danger of the parent terminating before we
         --  increment P.Awaited_Dependent_Count below.

         if Acceptor.Awake_Count = 1 then
            Unlock (Acceptor.L);

            if Acceptor.Pending_ATC_Level < Acceptor.ATC_Nesting_Level then
               Abortion.Undefer_Abortion;
               pragma Assert (
                 Utilities.Runtime_Assert_Shutdown (
                   "Continuing after being aborted!"));
            end if;

            P := Acceptor.Parent;
            Write_Lock (P.L, Error);

            if P.Awake_Count /= 0 then
               P.Awake_Count := P.Awake_Count + 1;

            else
               Unlock (P.L);
               Utilities.Abort_To_Level (Acceptor, 0);
               Abortion.Undefer_Abortion;
               pragma Assert (
                 Utilities.Runtime_Assert_Shutdown (
                   "Continuing after being aborted!"));
            end if;

            --  Conservative checks which should only matter when an interrupt
            --  entry was chosen. In this case, the current task completes if
            --  the parent has already been signaled that all children have
            --  terminated.

            if Acceptor.Master_of_Task = P.Master_Within then
               if P.Awaited_Dependent_Count /= 0 then
                  P.Awaited_Dependent_Count := P.Awaited_Dependent_Count + 1;

               elsif P.Stage = Await_Dependents then
                  Unlock (P.L);
                  Utilities.Abort_To_Level (Acceptor, 0);
                  Abortion.Undefer_Abortion;
                  pragma Assert (
                    Utilities.Runtime_Assert_Shutdown (
                      "Continuing after being aborted!"));
               end if;
            end if;

            Unlock (P.L);

         else
            Unlock (Acceptor.L);

            if Acceptor.Pending_ATC_Level <
              Acceptor.ATC_Nesting_Level then
               Abortion.Undefer_Abortion;
               pragma Assert (
                 Utilities.Runtime_Assert_Shutdown (
                   "Continuing after being aborted!"));
            end if;
         end if;

         Write_Lock (Acceptor.L, Error);

         Acceptor.Terminate_Alternative := False;
         --  Need to set this flag off in order not to make subsequent calls
         --  to be treated to calls to Select With Terminate Alternative.

--    end if;  ???
--    Unlock (Acceptor.L);

   end Adjust_For_Terminate_Alternative_Call;

   ------------------------------------
   -- Setup_For_Rendezvous_With_Body --
   ------------------------------------

   procedure Setup_For_Rendezvous_With_Body
     (Entry_Call : Entry_Call_Link; Acceptor : Task_ID) is
   begin
      Entry_Call.Acceptor_Prev_Call := Acceptor.Call;
      Acceptor.Call := Entry_Call;
      Entry_Call.Abortable := False;
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
      Error      : Boolean;

   begin
      pragma Assert (Mode /= Asynchronous_Call
        or else Utilities.Runtime_Assert_Shutdown (
          "Asynchronous call being treated synchronously."));

      Abortion.Defer_Abortion_Self (Caller);
      Caller.ATC_Nesting_Level := Caller.ATC_Nesting_Level + 1;
      Level := Caller.ATC_Nesting_Level;

      Entry_Call := Caller.Entry_Calls (Level)'Access;

      Entry_Call.Next := null;
      Entry_Call.Mode := Mode;
      Entry_Call.Abortable := True;
      Entry_Call.Done := False;
      Entry_Call.E := Entry_Index (E);
      Entry_Call.Prio := Caller.Current_Priority;
      Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
      Entry_Call.Called_Task := Acceptor;
      Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;

      --  Note: the caller will undefer abortion on return (see WARNING above)

      Write_Lock (Acceptor.L, Error);

      if Acceptor.Terminate_Alternative
        and then Is_Entry_Open (Acceptor, E) then
         Adjust_For_Terminate_Alternative_Call (Acceptor);
      end if;

      Do_Or_Queue (Entry_Call);
      Unlock (Acceptor.L);
      System.Tasking.Entry_Calls.Wait_For_Completion (Entry_Call);
      Rendezvous_Successful := Entry_Call.Done;
      Abortion.Undefer_Abortion_Self (Caller);

      pragma Assert (
        Caller.Pending_ATC_Level >= Caller.ATC_Nesting_Level or else
          Utilities.Runtime_Assert_Shutdown (
            "Continuing after aborting self!"));

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
      Caller : Task_ID := Self;
      Call   : Entry_Call_Link;

   begin
      pragma Assert (Caller.ATC_Nesting_Level > ATC_Level_Base'First or else
        Utilities.Runtime_Assert_Shutdown (
          "Attempt to cancel nonexistent task entry call."));

      Call := Caller.Entry_Calls (Caller.ATC_Nesting_Level)'Access;

      pragma Assert (Call.Mode = Asynchronous_Call or else
        Utilities.Runtime_Assert_Shutdown (
          "Attempt to perform ATC on non-asynchronous task entry call"));

      pragma Assert (Call.Called_PO = Null_PO or else
        Utilities.Runtime_Assert_Shutdown (
          "Attempt to use Cancel_Task_Entry_Call on protected entry call."));

      Abortion.Defer_Abortion_Self (Caller);

      Utilities.Abort_To_Level (Caller, Call.Level - 1);
      System.Tasking.Entry_Calls.Wait_For_Completion (Call);

      Cancelled := not Call.Done;
      --  Allow the triggered statements to be skipped.

      Abortion.Undefer_Abortion_Self (Caller);
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
      Old_Acceptor : Task_ID := Self;
      Caller       : Task_ID;
      Entry_Call   : Entry_Call_Link;
      Error        : Boolean;

   begin
      Abortion.Defer_Abortion_Self (Old_Acceptor);
      Write_Lock (Old_Acceptor.L, Error);
      Entry_Call := Old_Acceptor.Call;
      Caller := Entry_Call.Self;

      Old_Acceptor.Call := Entry_Call.Acceptor_Prev_Call;

      Entry_Call.Abortable := False;

      --  Don't permit this call to be aborted until we have switched to
      --  the new acceptor.  Otherwise, we may queue a cancelled call below.

      Unlock (Old_Acceptor.L);

      Entry_Call.E := Entry_Index (E);

      Write_Lock (Acceptor.L, Error);
      Entry_Call.Called_Task := Acceptor;
      Entry_Call.Abortable := With_Abort;
      Entry_Call.Has_Been_Abortable :=
        With_Abort or Entry_Call.Has_Been_Abortable;
      Do_Or_Queue (Entry_Call);
      Unlock (Acceptor.L);

      Write_Lock (Caller.L, Error);
      Caller.Pending_Action := True;

      Cond_Signal (Caller.Cond);

      --  If this is a conditional entry call, and has just become
      --  abortable, the caller should be awakened to cancel the call.

      Unlock (Caller.L);
      Abortion.Undefer_Abortion_Self (Old_Acceptor);
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
      Entry_Call : Entry_Call_Link := Object.Call_In_Progress;
      Caller     : Task_ID         := Entry_Call.Self;
      Error      : Boolean;

   begin
      Abortion.Defer_Abortion;
      Entry_Call.E := Entry_Index (E);
      Object.Call_In_Progress := null;

      Write_Lock (Acceptor.L, Error);
      Entry_Call.Called_Task := Acceptor;
      Entry_Call.Called_PO := Null_PO;
      Entry_Call.Abortable := With_Abort;
      Entry_Call.Has_Been_Abortable :=
        With_Abort or Entry_Call.Has_Been_Abortable;
      Do_Or_Queue (Entry_Call);
      Unlock (Acceptor.L);

      Write_Lock (Caller.L, Error);
      Entry_Call.E := Entry_Index (E);

      Caller.Pending_Action := True;
      Cond_Signal (Caller.Cond);

      --  If this is a conditional entry call, and has just become
      --  abortable, the caller should be awakened to cancel the call.

      Unlock (Caller.L);
      Abortion.Undefer_Abortion;
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
      Error                 : Boolean;
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
         Entry_Call.Prio := Caller.Current_Priority;
         Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
         Entry_Call.Called_Task := Acceptor;
         Entry_Call.Called_PO := Null_PO;
         Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;

         Write_Lock (Acceptor.L, Error);

         if Acceptor.Terminate_Alternative
           and then Is_Entry_Open (Acceptor, E) then
            Adjust_For_Terminate_Alternative_Call (Acceptor);
         end if;

         Do_Or_Queue (Entry_Call);

         Initially_Abortable := Entry_Call.Abortable;

         Unlock (Acceptor.L);

         --  If the call was not queued abortably, we need to wait until
         --  it is before proceeding with the abortable part.
         --  Wait_Until_Abortable can be called unconditionally here,
         --  but it is expensive.

         if not Initially_Abortable then
            System.Tasking.Entry_Calls.Wait_Until_Abortable
              (Caller, Entry_Call);
         end if;

         Rendezvous_Successful := Entry_Call.Done;

         --  Note: following assignment needs to be atomic.

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
      Error        : Boolean;

   begin
      Abortion.Defer_Abortion_Self (Acceptor);
      Write_Lock (Acceptor.L, Error);

      --  If someone is completing this task, it must be because they plan
      --  to abort it. This task should not try to access its pending entry
      --  calls or queues in this case, as they are being emptied. Wait for
      --  abortion to kill us.

      if Acceptor.Stage >= Completing then
         Await_Abortion (Acceptor);
      end if;

      Dequeue_Head (Acceptor.Entry_Queues (E), Entry_Call);

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

         while Acceptor.Accepting /= Not_Accepting loop
            if Acceptor.Pending_Action then
               if Acceptor.Pending_Priority_Change then
                  Abortion.Change_Base_Priority (Acceptor);
               end if;

               if Acceptor.Pending_ATC_Level < Acceptor.ATC_Nesting_Level then
                  Acceptor.Accepting := Not_Accepting;
                  exit;
               end if;

               Acceptor.Pending_Action := False;
            end if;

            Cond_Wait (Acceptor.Cond, Acceptor.L);
         end loop;

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

      Unlock (Acceptor.L);
      Abortion.Undefer_Abortion_Self (Acceptor);

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
      Error         : Boolean;

   begin
      Abortion.Defer_Abortion_Self (Acceptor);
      Write_Lock (Acceptor.L, Error);

      --  If someone is completing this task, it must be because they plan
      --  to abort it. This task should not try to access its pending entry
      --  calls or queues in this case, as they are being emptied. Wait for
      --  abortion to kill us.

      if Acceptor.Stage >= Completing then
         Await_Abortion (Acceptor);
      end if;

      Dequeue_Head (Acceptor.Entry_Queues (E), Entry_Call);

      if Entry_Call = null then

         --  Need to wait for call

         Open_Accepts (1).Null_Body := True;
         Open_Accepts (1).S := E;
         Acceptor.Open_Accepts := Open_Accepts'Unchecked_Access;

         Acceptor.Accepting := Trivial_Accept;

         --  Wait for normal entry call

         while Acceptor.Accepting /= Not_Accepting loop
            if Acceptor.Pending_Action then
               if Acceptor.Pending_Priority_Change then
                  Abortion.Change_Base_Priority (Acceptor);
               end if;

               if Acceptor.Pending_ATC_Level < Acceptor.ATC_Nesting_Level then
                  Acceptor.Accepting := Not_Accepting;
                  exit;
               end if;

               Acceptor.Pending_Action := False;
            end if;
            Cond_Wait (Acceptor.Cond, Acceptor.L);
         end loop;

         Unlock (Acceptor.L);

      else
         --  No longer abortable.

         Entry_Call.Abortable := False;

         Unlock (Acceptor.L);
         Caller := Entry_Call.Self;
         Write_Lock (Caller.L, Error);

         Entry_Call.Done := True;

         --  Done with mutex locked to make sure that signal is not lost.

         Unlock (Caller.L);

         if Entry_Call.Mode = Asynchronous_Call then
            Utilities.Abort_To_Level (Caller, Entry_Call.Level - 1);
         else
            Cond_Signal (Caller.Cond);
         end if;

      end if;

      Abortion.Undefer_Abortion_Self (Acceptor);

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
      Error         : Boolean;

      use Ada.Exceptions;

   begin
      Abortion.Defer_Abortion_Self (Acceptor);
      Call := Acceptor.Call;
      Acceptor.Call := Call.Acceptor_Prev_Call;
      Prev_Priority := Call.Acceptor_Prev_Priority;
      Call.Exception_To_Raise := Ex;
      Caller := Call.Self;
      Write_Lock (Caller.L, Error);

      Call.Done := True;
      --  Done with mutex locked to make sure that signal is not lost.

      Unlock (Caller.L);

      if Call.Mode = Asynchronous_Call then
         Utilities.Abort_To_Level (Caller, Call.Level - 1);
      else
         Cond_Signal (Caller.Cond);
      end if;

      Utilities.Reset_Priority (Prev_Priority, Acceptor);

      Acceptor.Exception_To_Raise := Ex;

      if Ex /= Null_Id then
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

      Abortion.Undefer_Abortion_Self (Acceptor);
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
      Error            : Boolean;
      Entry_Call       : Entry_Call_Link;
      Caller           : Task_ID;
      Selection        : Select_Index;
      Open_Alternative : Boolean;

   begin
      Abortion.Defer_Abortion_Self (Acceptor);
      Write_Lock (Acceptor.L, Error);

      --  If someone is completing this task, it must be because they plan
      --  to abort it. This task should not try to access its pending entry
      --  calls or queues in this case, as they are being emptied. Wait for
      --  abortion to kill us.

      if Acceptor.Stage >= Completing then
         Await_Abortion (Acceptor);
      end if;

      Select_Task_Entry_Call
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
         Abortion.Defer_Abortion_Self (Acceptor);
         Unlock (Acceptor.L);

      when Accept_Alternative_Completed =>

         --  Rendezvous is over

         Unlock (Acceptor.L);
         Caller := Entry_Call.Self;
         Write_Lock (Caller.L, Error);
         Entry_Call.Done := True;
         Unlock (Caller.L);
         if Entry_Call.Mode = Asynchronous_Call then
            Utilities.Abort_To_Level (Caller, Entry_Call.Level - 1);
         else
            Cond_Signal (Caller.Cond);
         end if;

      when Accept_Alternative_Open =>

         --  Wait for caller.

         Acceptor.Open_Accepts := Open_Accepts;

         Acceptor.Accepting := Select_Wait;

         while Acceptor.Accepting /= Not_Accepting
         loop
            if Acceptor.Pending_Action then
               if Acceptor.Pending_Priority_Change then
                  Abortion.Change_Base_Priority (Acceptor);
               end if;

               if Acceptor.Pending_ATC_Level <
                 Acceptor.ATC_Nesting_Level then
                  Acceptor.Accepting := Not_Accepting;
                  exit;
               end if;

               Acceptor.Pending_Action := False;
            end if;
            Cond_Wait (Acceptor.Cond, Acceptor.L);
         end loop;

         --  Acceptor.Call should already be updated by the Caller if
         --  not aborted. It might also be ready to do rendezvous even if
         --  this wakes up due to an abortion.
         --  Therefore, if the call is not empty we need to do the rendezvous
         --  if the accept body is not Null_Body.

         if Acceptor.Call /= null and then
          not Open_Accepts (Acceptor.Chosen_Index).Null_Body then
            Uninterpreted_Data := Acceptor.Call.Uninterpreted_Data;
            Abortion.Defer_Abortion_Self (Acceptor);
         end if;

         Unlock (Acceptor.L);

      when Else_Selected =>
         Acceptor.Accepting := Not_Accepting;
         Unlock (Acceptor.L);

      when Terminate_Selected =>

         --  Terminate alternative is open

         Acceptor.Open_Accepts := Open_Accepts;

         Acceptor.Accepting := Select_Wait;

         --  We need to check if a signal is pending on an open interrupt
         --  entry. Otherwise this task would become passive (since terminate
         --  alternative is open) and, if none of the siblings are active
         --  any more, the task could not wake up any more, even though a
         --  signal might be pending on an open interrupt entry.

         Unlock (Acceptor.L);
         Utilities.Terminate_Alternative;

         --  Wait for normal entry call or termination

         --  consider letting Terminate_Alternative assume mutex L
         --  is already locked, and return with it locked, so
         --  this code could be simplified???

         --  No return here if Acceptor completes, otherwise
         --  Acceptor.Call should already be updated by the Caller

         Write_Lock (Acceptor.L, Error);

         Index := Acceptor.Chosen_Index;
         if not Open_Accepts (Acceptor.Chosen_Index).Null_Body then
            Uninterpreted_Data := Acceptor.Call.Uninterpreted_Data;
            Abortion.Defer_Abortion_Self (Acceptor);
         end if;
         Unlock (Acceptor.L);

         Abortion.Undefer_Abortion_Self (Acceptor);
         return;

      when No_Alternative_Open =>

         --  In this case, Index will be No_Rendezvous on return, which
         --  should cause a Program_Error if it is not a Delay_Mode.

         --  If delay altenative exists (Delay_Mode) we should suspend
         --  until the delay expires.

         if Select_Mode = Delay_Mode then
            while not Acceptor.Pending_Action loop
               Cond_Wait (Acceptor.Cond, Acceptor.L);
            end loop;
            Unlock (Acceptor.L);
         else
            Unlock (Acceptor.L);
            Abortion.Undefer_Abortion_Self (Acceptor);
            raise Program_Error;
         end if;

      end case;

      --  Caller has been chosen

      --  Acceptor.Call should already be updated by the Caller

      --  Acceptor.Chosen_Index should either be updated by the Caller
      --  or by Test_Selective_Wait

      Index := Acceptor.Chosen_Index;
      Abortion.Undefer_Abortion_Self (Acceptor);

      --  Start rendezvous, if not already completed.

   end Selective_Wait;

   ----------------
   -- Task_Count --
   ----------------

   function Task_Count (E : Task_Entry_Index) return Natural is
      T            : constant Task_ID := Self;
      Return_Count : Natural;
      Error        : Boolean;

   begin
      Write_Lock (T.L, Error);
      Return_Count := Count_Waiting (T.Entry_Queues (E));
      Unlock (T.L);
      return Return_Count;
   end Task_Count;

   --------------
   -- Callable --
   --------------

   function Callable (T : Task_ID) return Boolean is
   begin
      return     T.Stage < Complete
        and then T.Pending_ATC_Level > ATC_Level_Base'First;
   end Callable;

   --------------------
   -- Await_Abortion --
   --------------------

   procedure Await_Abortion (Acceptor : Task_ID) is
   begin
      loop
         if Acceptor.Pending_Action then
            if Acceptor.Pending_Priority_Change then
               Abortion.Change_Base_Priority (Acceptor);
            end if;

            exit when
               Acceptor.Pending_ATC_Level < Acceptor.ATC_Nesting_Level;
            Acceptor.Pending_Action := False;
         end if;

         Cond_Wait (Acceptor.Cond, Acceptor.L);
      end loop;

      Unlock (Acceptor.L);
      Abortion.Undefer_Abortion_Self (Acceptor);
      pragma Assert (
        Utilities.Runtime_Assert_Shutdown (
          "Continuing execution after being aborted."));
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

end System.Tasking.Rendezvous;

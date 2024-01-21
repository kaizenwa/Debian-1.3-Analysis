------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T A S K I N G . Q U E U I N G              --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                        (Priority Queueing Version)                       --
--                                                                          --
--                             $Revision: 1.6 $                             --
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

--  This version of the body implements priority queueing, and is stored
--  under the alternative name s-tasqup in the library. The binder takes
--  care of susbtituting the reference in the list of object files as needed.

--  Note: the initial version of this file was a copy of version 1.16 of
--  the file s-tasque.adb (for a long time, the default was priority queueing
--  and the file reflected this fact).

pragma Source_File_Name
  (System.Tasking.Queuing, Body_File_Name => "s-tasqup.adb");

with Ada.Exceptions;
--  used for Exception_ID

with System.Error_Reporting;
--  used for Shutdown

with System.Standard_Library;
--  used for Program_Error_Def;
--           Exception_Data

with System.Task_Primitives.Operations;
--  used for Write_Lock
--           Unlock

with System.Tasking.Utilities;
--  used for Abort_To_Level

with Unchecked_Conversion;

package body System.Tasking.Queuing is

   use System.Task_Primitives.Operations;
   use System.Error_Reporting;

   --  Entry Queues implemented as doubly linked list, priority ordered

   type Exception_Data_Access
     is access all System.Standard_Library.Exception_Data;

   function To_Exception_ID is new
     Unchecked_Conversion (Exception_Data_Access, Ada.Exceptions.Exception_ID);

   procedure Send_Program_Error (Entry_Call : Entry_Call_Link);
   --  Raise Program_Error in the caller of the specified entry
   --  call.

   ------------------------
   -- Send_Program_Error --
   ------------------------

   procedure Send_Program_Error (Entry_Call : Entry_Call_Link) is
      Current_Task  : Task_ID;

   begin
      Current_Task := Entry_Call.Self;
      Entry_Call.Exception_To_Raise :=
        To_Exception_ID (System.Standard_Library.Program_Error_Def'Access);
      Write_Lock (Current_Task);
      Entry_Call.Done := True;
      Unlock (Current_Task);
      Utilities.Abort_To_Level
        (Current_Task, Entry_Call.Level - 1);
   end Send_Program_Error;

   -----------------------------
   -- Broadcast_Program_Error --
   -----------------------------

   procedure Broadcast_Program_Error
     (Object       : access Protection;
      Pending_Call : Entry_Call_Link)
   is
      Entry_Call   : Entry_Call_Link;

   begin
      if Pending_Call /= null then
         Send_Program_Error (Pending_Call);
      end if;

      for E in Object.Entry_Queues'Range loop
         Dequeue_Head (Object.Entry_Queues (E), Entry_Call);

         while Entry_Call /= null loop
            pragma Assert (Entry_Call.Mode /= Conditional_Call or else
              Shutdown ("Conditional call found on entry queue."));
            Send_Program_Error (Entry_Call);
            Dequeue_Head (Object.Entry_Queues (E), Entry_Call);
         end loop;
      end loop;
   end Broadcast_Program_Error;

   -------------
   -- Enqueue --
   -------------

   --  Enqueue call priority ordered, FIFO at same priority level

   procedure Enqueue (E : in out Entry_Queue; Call : Entry_Call_Link) is
      Temp : Entry_Call_Link := E.Head;
   begin
      if Temp = null then
         Call.Prev := Call;
         Call.Next := Call;
         E.Head := Call;
         E.Tail := Call;
      else
         loop  --  find the entry that the new guy should precede
            exit when Call.Prio > Temp.Prio;
            Temp := Temp.Next;
            if Temp = E.Head then
               Temp := null;
               exit;
            end if;
         end loop;

         if Temp = null then -- insert at tail
            Call.Prev := E.Tail;
            Call.Next := E.Head;
            E.Tail := Call;
         else
            Call.Prev := Temp.Prev;
            Call.Next := Temp;

            if Temp = E.Head then -- insert at head
               E.Head := Call;
            end if;
         end if;

         Call.Prev.Next := Call;
         Call.Next.Prev := Call;

      end if;
   end Enqueue;

   -------------
   -- Dequeue --
   -------------

   --  Dequeue call from entry_queue E

   procedure Dequeue (E : in out Entry_Queue; Call : Entry_Call_Link) is
      Prev : Entry_Call_Link;

   begin
      --  If empty queue, simply return

      if E.Head = null then
         return;
      end if;

      Call.Prev.Next := Call.Next;
      Call.Next.Prev := Call.Prev;

      if E.Head = Call then
         if E.Tail = Call then
            E.Head := null; --  case of one element
            E.Tail := null;
         else
            E.Head := Call.Next;
         end if;
      elsif E.Tail = Call then
         E.Tail := Call.Prev;
      end if;

      --  Successfully dequeued

      Call.Prev := null;
      Call.Next := null;

   end Dequeue;

   ----------
   -- Head --
   ----------

   --  Return the head of entry_queue E

   function Head (E : in Entry_Queue) return Entry_Call_Link is
   begin
      return E.Head;
   end Head;

   ------------------
   -- Dequeue_Head --
   ------------------

   --  Remove and return the head of entry_queue E

   procedure Dequeue_Head
     (E    : in out Entry_Queue;
      Call : out Entry_Call_Link)
   is
      Temp : Entry_Call_Link;

   begin
      --  If empty queue, return null pointer

      if E.Head = null then
         Call := null;
         return;
      end if;

      Temp := E.Head;

      if E.Head = E.Tail then
         E.Head := null; --  case of one element
         E.Tail := null;
      else
         E.Head := Temp.Next;
         Temp.Prev.Next := Temp.Next;
         Temp.Next.Prev := Temp.Prev;
      end if;

      --  Successfully dequeued

      Temp.Prev := null;
      Temp.Next := null;
      Call := Temp;
   end Dequeue_Head;

   -------------
   -- Onqueue --
   -------------

   --  Return True if Call is on any entry_queue at all

   function Onqueue (Call : Entry_Call_Link) return Boolean is
   begin
      --  Utilize the fact that every queue is circular, so if Call
      --  is on any queue at all, Call.Next must NOT be null.

      return Call.Next /= null;
   end Onqueue;

   -------------------
   -- Count_Waiting --
   -------------------

   --  Return number of calls on the waiting queue of E

   function Count_Waiting (E : in Entry_Queue) return Natural is
      Count : Natural;
      Temp : Entry_Call_Link;

   begin
      Count := 0;

      if E.Head /= null then
         Temp := E.Head;

         loop
            Count := Count + 1;
            exit when E.Tail = Temp;
            Temp := Temp.Next;
         end loop;
      end if;

      return Count;
   end Count_Waiting;

   ----------------------------
   -- Select_Task_Entry_Call --
   ----------------------------

   --  Select an entry for rendezvous

   procedure Select_Task_Entry_Call
     (Acceptor         : Task_ID;
      Open_Accepts     : Accept_List_Access;
      Call             : out Entry_Call_Link;
      Selection        : out Select_Index;
      Open_Alternative : out Boolean)
   is
      Entry_Call  : Entry_Call_Link;
      Temp_Call   : Entry_Call_Link;
      Entry_Index : Task_Entry_Index;
      Temp_Entry  : Task_Entry_Index;

   begin
      Open_Alternative := False;
      Entry_Call := null;

      for J in Open_Accepts'Range loop
         Temp_Entry := Open_Accepts (J).S;
         if Temp_Entry /= Null_Task_Entry then
            Open_Alternative := True;
            Temp_Call := Head (Acceptor.Entry_Queues (Temp_Entry));
            if Temp_Call /= null and then
              (Entry_Call = null or else
               Entry_Call.Prio < Temp_Call.Prio)
            then
               Entry_Call := Head (Acceptor.Entry_Queues (Temp_Entry));
               Entry_Index := Temp_Entry;
               Selection := J;
            end if;
         end if;
      end loop;

      if Entry_Call = null then
         Selection := No_Rendezvous;
      else
         Dequeue_Head (Acceptor.Entry_Queues (Entry_Index), Entry_Call);
         --  Guard is open
      end if;

      Call := Entry_Call;
   end Select_Task_Entry_Call;

   ---------------------------------
   -- Select_Protected_Entry_Call --
   ---------------------------------

   --  Select an entry of a protected object

   procedure Select_Protected_Entry_Call
     (Object : access Protection;
      Call   : out Entry_Call_Link)
   is
      Entry_Call  : Entry_Call_Link;
      Temp_Call   : Entry_Call_Link;
      Entry_Index : Protected_Entry_Index;

   begin
      Entry_Call := null;

      begin

         for J in Object.Entry_Queues'Range loop
            Temp_Call := Head (Object.Entry_Queues (J));

            if Temp_Call /= null and then
              Object.Entry_Bodies (J).Barrier (Object.Compiler_Info, J)
            then
               if (Entry_Call = null or else
                 Entry_Call.Prio < Temp_Call.Prio)
               then
                  Entry_Call := Temp_Call;
                  Entry_Index := J;
               end if;
            end if;
         end loop;

      exception
         when others =>
            Broadcast_Program_Error (Object, null);
      end;

      --  If a call was selected, dequeue it and return it for service.

      if Entry_Call /= null then
         Dequeue_Head (Object.Entry_Queues (Entry_Index), Entry_Call);
      end if;

      Call := Entry_Call;
   end Select_Protected_Entry_Call;

   ------------------
   -- Enqueue_Call --
   ------------------

   procedure Enqueue_Call (Entry_Call : Entry_Call_Link) is
   begin
      if Entry_Call.Called_Task /= Null_Task then
         Enqueue
           (Entry_Call.Called_Task.Entry_Queues
              (Task_Entry_Index (Entry_Call.E)),
           Entry_Call);
      else
         Enqueue
           (Entry_Call.Called_PO.Entry_Queues
             (Protected_Entry_Index (Entry_Call.E)),
           Entry_Call);
      end if;
   end Enqueue_Call;

   ------------------
   -- Dequeue_Call --
   ------------------

   procedure Dequeue_Call (Entry_Call : Entry_Call_Link) is
   begin
      if Entry_Call.Called_Task /= Null_Task then
         Dequeue
           (Entry_Call.Called_Task.Entry_Queues
             (Task_Entry_Index (Entry_Call.E)),
           Entry_Call);
      else
         Dequeue
           (Entry_Call.Called_PO.Entry_Queues
             (Protected_Entry_Index (Entry_Call.E)),
           Entry_Call);
      end if;
   end Dequeue_Call;

   --------------------------------
   -- Requeue_Call_With_New_Prio --
   --------------------------------

   procedure Requeue_Call_With_New_Prio
     (Entry_Call : Entry_Call_Link; Prio : System.Any_Priority)
   is
   begin
      if Onqueue (Entry_Call) then
         Dequeue_Call (Entry_Call);
         Entry_Call.Prio := Prio;
         Enqueue_Call (Entry_Call);
      end if;
   end Requeue_Call_With_New_Prio;

end System.Tasking.Queuing;

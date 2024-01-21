------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--             S Y S T E M . T A S K I N G . R E N D E Z V O U S            --
--                                                                          --
--                                  S p e c                                 --
--                           (Compiler Interface)                           --
--                                                                          --
--                             $Revision: 1.14 $                            --
--                                                                          --
--    Copyright (C) 1991, 92, 93, 94, 1995 Free Software Foundation, Inc.   --
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

with Ada.Exceptions;
--  Used for, Exception_Id

package System.Tasking.Rendezvous is
   --  This interface is described in the document
   --  Gnu Ada Runtime Library Interface (GNARLI).

   procedure Task_Entry_Call
     (Acceptor              : Task_ID;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Mode                  : Call_Modes;
      Rendezvous_Successful : out Boolean);
   --  General entry call

   procedure Call_Simple
     (Acceptor           : Task_ID;
      E                  : Task_Entry_Index;
      Uninterpreted_Data : System.Address);
   --  Simple entry call

   procedure Cancel_Task_Entry_Call (Cancelled : out Boolean);
   --  Cancel pending task entry call

   procedure Requeue_Task_Entry
     (Acceptor   : Task_ID;
      E          : Task_Entry_Index;
      With_Abort : Boolean);

   procedure Requeue_Protected_To_Task_Entry
     (Object     : Protection_Access;
      Acceptor   : Task_ID;
      E          : Task_Entry_Index;
      With_Abort : Boolean);

   procedure Selective_Wait
     (Open_Accepts       : Accept_List_Access;
      Select_Mode        : Select_Modes;
      Uninterpreted_Data : out System.Address;
      Index              : out Select_Index);
   --  Selective wait

   procedure Accept_Call
     (E                  : Task_Entry_Index;
      Uninterpreted_Data : out System.Address);
   --  Accept an entry call

   procedure Accept_Trivial (E : Task_Entry_Index);
   --  Accept an entry call that has no parameters and no body

   function Task_Count (E : Task_Entry_Index) return Natural;
   --  Return number of tasks waiting on the entry E (of current task)

   function Callable (T : Task_ID) return Boolean;
   --  Return T'CALLABLE

   type Task_Entry_Nesting_Depth is new Task_Entry_Index
     range 0 .. Max_Task_Entry;

   function Task_Entry_Caller (D : Task_Entry_Nesting_Depth) return Task_ID;
   --  Return E'Caller.  This will only work if called from within an
   --  accept statement that is handling E, as required by the
   --  LRM (C.7.1(14)).

   procedure Complete_Rendezvous;
   --  Called by acceptor to wake up caller

   procedure Exceptional_Complete_Rendezvous
     (Ex : Ada.Exceptions.Exception_ID);
   --  Called by acceptor to mark the end of the current rendezvous and
   --  propagate an exception to the caller.

end System.Tasking.Rendezvous;

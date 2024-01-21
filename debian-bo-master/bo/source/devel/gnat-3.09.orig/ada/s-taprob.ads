------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--      S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S     --
--                                                                          --
--                                  S p e c                                 --
--                           (Compiler Interface)                           --
--                                                                          --
--                             $Revision: 1.18 $                            --
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

package System.Tasking.Protected_Objects is
   --  This interface is described in the document
   --  Gnu Ada Runtime Library Interface (GNARLI).

   pragma Elaborate_Body (System.Tasking.Protected_Objects);

   procedure Initialize_Protection
     (Object            : access Protection;
      Ceiling_Priority  : Integer;
      Compiler_Info     : System.Address;
      Entry_Bodies      : access Protected_Entry_Body_Array);
   --  Initialize the Object parameter so that it can be used by the runtime
   --  to keep track of the runtime state of a protected object.

   procedure Lock (Object : access Protection);
   --  Lock a protected object for write access.  Upon return, the caller
   --  owns the lock to this object, and no other call to Lock or
   --  Lock_Read_Only with the same argument will return until the
   --  corresponding call to Unlock has been made by the caller.

   procedure Lock_Read_Only (Object : access Protection);
   --  Lock a protected object for read access.  Upon return, the caller
   --  owns the lock for read access, and no other calls to Lock
   --  with the same argument will return until the corresponding call
   --  to Unlock has been made by the caller.  Other cals to Lock_Read_Only
   --  may (but need not) return before the call to Unlock, and the
   --  corresponding callers will also own the lock for read access.

   procedure Unlock (Object : access Protection);
   --  Relinquish ownership of the lock for the object represented by
   --  the Object parameter.  If this ownership was for write access, or
   --  if it was for read access where there are no other read access
   --  locks outstanding, one (or more, in the case of Lock_Read_Only)
   --  of the tasks waiting on this lock (if any) will be given the
   --  lock and allowed to return from the Lock or Lock_Read_Only call.

   procedure Protected_Entry_Call
     (Object    : access Protection;
      E         : Protected_Entry_Index;
      Uninterpreted_Data : System.Address;
      Mode      : Call_Modes;
      Block     : out Communication_Block);
   --  Make a protected entry call to the specified object.
   --  Pend a protected entry call on the protected object represented
   --  by Object.  A pended call is not queued; it may be executed immediately
   --  or queued, depending on the state of the entry barrier.
   --  E---The index representing the entry to be called.
   --  Uninterpreted_Data---This will be returned by Next_Entry_Call
   --   when this call is serviced.  It can be used by the compiler
   --   to pass information between the caller and the server, in particular
   --   entry parameters.
   --  Mode---The kind of call to be pended.
   --  Block---Information passed between one runtime call and another
   --   by the compiler.

   procedure Service_Entries (Object : access Protection);
   --  Service all entry queues of the specified object, executing the
   --  corresponding bodies of any queued entry calls that are waiting
   --  on True barriers.  This is used when the state of a protected
   --  object may have changed, in particular after the execution of
   --  the statement sequence of a protected procedure.
   --  Note that servicing an entry may change the value of one or more
   --  barriers, so this this routine keeps checking barriers until all of
   --  them are closed.
   --  This must be called with abortion deferred and with the corresponding
   --  object locked.

   pragma Inline (Service_Entries);
   --  !!! To try to be fairer to the callback interface.

   procedure Cancel_Protected_Entry_Call (Block : in out Communication_Block);
   --  Attempt to cancel the most recent protected entry call.  If the call is
   --  not queued abortably, wait until it is or until it has completed.
   --  If the call is actually cancelled, the called object will be
   --  locked on return from this call. Get_Cancelled (Block) can be
   --  used to determine if the cancellation took place; there
   --  may be entries needing service in this case.
   --  Block passes information between this and other runtime calls.

   procedure Complete_Entry_Body (Object : access Protection);
   --  Called from within an entry body procedure, indicates that the
   --  corresponding entry call has been serviced.

   procedure Exceptional_Complete_Entry_Body
     (Object : access Protection;
      Ex     : Ada.Exceptions.Exception_Id);
   --  Perform all of the functions of Complete_Entry_Body.  In addition,
   --  report in Ex the exception whose propagation terminated the entry
   --  body to the runtime system.

   function Enqueued (Block : Communication_Block) return Boolean;
   --  Returns True if the Protected_Entry_Call which returned the
   --  specified Block object was queued; False otherwise.

   function Cancelled (Block : Communication_Block) return Boolean;
   --  Returns True if the Protected_Entry_Call which returned the
   --  specified Block object was cancelled, False otherwise.

   procedure Requeue_Protected_Entry
     (Object     : access Protection;
      New_Object : access Protection;
      E          : Protected_Entry_Index;
      With_Abort : Boolean);
   --  If Object = New_Object, queue the protected entry call on Object
   --   currently being serviced on the queue corresponding to the entry
   --   represented by E.
   --  If Object /= New_Object, transfer the call to New_Object.E,
   --   executing or queuing it as appropriate.
   --  With_Abort---True if the call is to be queued abortably, false
   --   otherwise.

   procedure Requeue_Task_To_Protected_Entry
     (New_Object : access Protection;
      E          : Protected_Entry_Index;
      With_Abort : Boolean);
   --  Transfer task entry call currently being serviced to entry E
   --   on New_Object.
   --  With_Abort---True if the call is to be queued abortably, false
   --   otherwise.

   function Protected_Count
     (Object : Protection;
      E      : Protected_Entry_Index)
      return   Natural;
   --  Return the number of entry calls to E on Object.

   function Protected_Entry_Caller (Object : Protection) return Task_ID;
   --  Return value of E'Caller, where E is the protected entry currently
   --  being handled.  This will only work if called from within an
   --  entry body, as required by the LRM (C.7.1(14)).

end System.Tasking.Protected_Objects;

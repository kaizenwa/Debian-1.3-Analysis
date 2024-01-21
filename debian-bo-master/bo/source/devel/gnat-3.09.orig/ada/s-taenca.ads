------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--             S Y S T E M . T A S K I N G . E N T R Y _ C A L L S          --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.4 $                             --
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

with Unchecked_Conversion;

package System.Tasking.Entry_Calls is

   procedure Internal_Lock
     (Object : access Protection;
      Ceiling_Violation : out Boolean);
   --  This version of lock is used internally to lock a protected
   --  object. It returns a Ceiling_Violation flag instead of raising
   --  program error, avoiding the need for exception handlers in the
   --  runtime to clean up after a ceiling violation.

   procedure Internal_Lock_Read_Only
     (Object : access Protection;
      Ceiling_Violation : out Boolean);
   --  This version of lock is used internally to lock a protected
   --  object for read access.
   --  It returns a Ceiling_Violation flag instead of raising
   --  program error, avoiding the need for exception handlers in the
   --  runtime to clean up after a ceiling violation.

   procedure Lock_Server
      (Entry_Call : Entry_Call_Link;
       No_Server  : out Boolean);
   --  This locks the server targeted by Entry_Call, returning
   --  No_Server=True if no server is currently targeted.
   --  This may be a task or a protected object, depending on the
   --  target of the original call or any subsequent requeues.
   --  It will fail if there is no such target.
   --  This routine is needed because the field specifying the server
   --  for this call must be protected by the server's mutex.  If it were
   --  protected by the caller's mutex, accessing the server's queues would
   --  require locking the caller to get the server, locking the server,
   --  and then accessing the queues.  This involves holding two ATCB
   --  locks at once, something which we can guarantee that it will always
   --  be done in the same order, or locking a protected object while we
   --  hold an ATCB lock, something which is not permitted.  Since
   --  the server cannot be obtained reliably, it must be obtained unreliably
   --  and then checked again once it has been locked.

   procedure Unlock_Server (Entry_Call : Entry_Call_Link);
   --  Unlock the server targeted by Entry_Call.  The server must
   --  be locked before calling this.

   procedure Unlock_And_Update_Server (Entry_Call : Entry_Call_Link);
   --  Similar to Unlock_Server, but services entry calls if the
   --  server is a protected object.

   procedure Wait_For_Completion (Entry_Call : Entry_Call_Link);
   --  This procedure suspends the calling task until the specified entry
   --  call has either been completed or cancelled.  It performs other
   --  operations required of suspended tasks, such as performing
   --  dynamic priority changes.  On exit, the call will not be queued.
   --  This waits for calls on task or protected entries.
   --  Abortion must be deferred when calling this procedure.

   procedure Wait_Until_Abortable
     (Caller : Task_ID;
      Call   : Entry_Call_Link);
   --  This procedure suspends the calling task until the specified entry
   --  call is queued abortably or completes.
   --  Abortion must be deferred when calling this procedure.

end System.Tasking.Entry_Calls;

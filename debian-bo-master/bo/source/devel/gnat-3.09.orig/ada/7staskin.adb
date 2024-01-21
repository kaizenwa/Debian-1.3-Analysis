------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                        S Y S T E M . T A S K I N G                       --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.17 $                            --
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

with System.Task_Primitives.Operations;
--  used for Self
--           Finalize_Lock

with System.Standard_Library;
--  used for Program_Error_Def;
--           Exception_Data

with Unchecked_Conversion;

package body System.Tasking is

   type Exception_Data_Access
     is access all System.Standard_Library.Exception_Data;

   function To_Exception_ID is new
     Unchecked_Conversion (Exception_Data_Access, Ada.Exceptions.Exception_ID);

   ----------
   -- Self --
   ----------

   --  This is an INLINE_ONLY version of Self for use in the RTS.

   function Self return Task_ID renames
     System.Task_Primitives.Operations.Self;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Protection) is
      Entry_Call : Entry_Call_Link;
      Caller     : Task_ID;
      Error      : boolean;

   begin

      System.Task_Primitives.Operations.Write_Lock (Object.L'Access, Error);

      --  Send program_error to all tasks still queued on this object.

      for E in Object.Entry_Queues'Range loop
         Entry_Call := Object.Entry_Queues (E).Head;

         while Entry_Call /= null loop
            Caller := Entry_Call.Self;

            Entry_Call.Exception_To_Raise :=
              To_Exception_ID
                (System.Standard_Library.Program_Error_Def'Access);

            Entry_Call.Done := True;

            System.Task_Primitives.Operations.Write_Lock (Caller);
            System.Task_Primitives.Operations.Wakeup (Caller);

            if Caller.Pending_ATC_Level > Entry_Call.Level - 1 then
               Caller.Pending_ATC_Level := Entry_Call.Level - 1;
               Caller.Pending_Action := True;
            end if;

            System.Task_Primitives.Operations.Unlock (Caller);

            exit when Entry_Call = Object.Entry_Queues (E).Tail;

            Entry_Call := Entry_Call.Next;
         end loop;
      end loop;

      System.Task_Primitives.Operations.Unlock (Object.L'Access);

      System.Task_Primitives.Operations.Finalize_Lock (Object.L'Access);
   end Finalize;

end System.Tasking;

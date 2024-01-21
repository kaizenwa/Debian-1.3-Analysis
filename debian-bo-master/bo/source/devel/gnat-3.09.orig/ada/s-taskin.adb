------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                        S Y S T E M . T A S K I N G                       --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.18 $                            --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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

with System.Task_Primitives;
--  Used for,  Task_Primitives.TCB_Ptr,
--             Task_Primitives.Self

with System.Storage_Elements;
--  Used for,  Storage_Elements.Storage_Offset,
--             Storage_Elements."-"
--             Storage_Elements.Storage_Count

with Unchecked_Conversion;
with System.Compiler_Exceptions;

package body System.Tasking is

   function "-"
     (A    : System.Address;
      B    : System.Address)
      return Storage_Elements.Storage_Offset
   renames Storage_Elements."-";

   function "-"
     (A    : System.Address;
      I    : Storage_Elements.Storage_Offset)
      return System.Address
   renames Storage_Elements."-";

   function Address_To_Task_ID is new
     Unchecked_Conversion (System.Address, Task_ID);

   function TCB_Ptr_To_Address is new
     Unchecked_Conversion (Task_Primitives.TCB_Ptr, System.Address);

   ----------
   -- Self --
   ----------

   --  This is an INLINE_ONLY version of Self for use in the RTS.

   Dummy_ATCB_Record : Ada_Task_Control_Block (0);
   --  This record is declared ONLY so we use the 'Position attribute on
   --  the LL_TCB field for efficient implementation of Self. It of course
   --  is wasteful in storage to do this. An alternative would be to add a
   --  representation clause that fixed the location of this field.

   function Self return Task_ID is
      S : Task_Primitives.TCB_Ptr := Task_Primitives.Self;

   begin
      return Address_To_Task_ID
        (TCB_Ptr_To_Address (S) -
          Storage_Elements.Storage_Offset'(Dummy_ATCB_Record.LL_TCB'Position));
   end Self;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Protection) is
      Entry_Call : Entry_Call_Link;
      Caller     : Task_ID;
      Error      : Boolean;

   begin

      System.Task_Primitives.Write_Lock (Object.L, Error);

      --  Send program_error to all tasks still queued on this object.

      for E in Object.Entry_Queues'Range loop
         Entry_Call := Object.Entry_Queues (E).Head;

         while Entry_Call /= null loop
            Caller := Entry_Call.Self;

            Entry_Call.Exception_To_Raise :=
              System.Compiler_Exceptions.Program_Error_Id;
            Entry_Call.Done := True;

            System.Task_Primitives.Write_Lock (Caller.L, Error);
            System.Task_Primitives.Cond_Signal (Caller.Cond);

            if Caller.Pending_ATC_Level > Entry_Call.Level - 1 then
               Caller.Pending_ATC_Level := Entry_Call.Level - 1;
               Caller.Pending_Action := True;
            end if;

            System.Task_Primitives.Unlock (Caller.L);

            exit when Entry_Call = Object.Entry_Queues (E).Tail;

            Entry_Call := Entry_Call.Next;
         end loop;
      end loop;

      System.Task_Primitives.Unlock (Object.L);

      System.Task_Primitives.Finalize_Lock (Object.L);

   end Finalize;

end System.Tasking;

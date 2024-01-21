------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--              A D A . T A S K _ I D E N T I F I C A T I O N               --
--                                                                          --
--                                 B o d y                                  --
--                         (Version for new GNARL)                          --
--                                                                          --
--                            $Revision: 1.11 $                             --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with System.Address_Image;
--  used for the function itself

with System.Tasking;
--  used for Task_List
--           Self

with System.Tasking.Abortion;
--  used for Abort_Tasks

with System.Tasking.Stages;
--  used for Terminated

with System.Tasking.Rendezvous;
--  used for Callable

with Unchecked_Conversion;

package body Ada.Task_Identification is

   ---------
   -- "=" --
   ---------

   function  "=" (Left, Right : Task_Id) return Boolean is
   begin
      return System.Tasking."=" (Convert_Ids (Left), Convert_Ids (Right));
   end "=";

   -----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : in out Task_Id) is
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      else
         System.Tasking.Abortion.Abort_Tasks
           (System.Tasking.Task_List'(1 => Convert_Ids (T)));
      end if;
   end Abort_Task;

   ------------------
   -- Current_Task --
   ------------------

   function Current_Task return Task_Id is
   begin
      return Convert_Ids (System.Tasking.Self);
   end Current_Task;

   -----------
   -- Image --
   -----------

   function Image (T : Task_Id) return String is

      function To_Address is new
        Unchecked_Conversion (Task_Id, System.Address);

   begin
      if T = Null_Task_Id then
         return "";
      else
         return System.Address_Image (To_Address (T));
      end if;
   end Image;

   -----------------
   -- Is_Callable --
   -----------------

   function Is_Callable (T : Task_Id) return Boolean is
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      else
         return System.Tasking.Rendezvous.Callable (Convert_Ids (T));
      end if;
   end Is_Callable;

   -------------------
   -- Is_Terminated --
   -------------------

   function Is_Terminated (T : Task_Id) return Boolean is
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      else
         return System.Tasking.Stages.Terminated (Convert_Ids (T));
      end if;
   end Is_Terminated;

end Ada.Task_Identification;

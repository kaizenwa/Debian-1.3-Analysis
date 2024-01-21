------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                    S Y S T E M . T A S K _ M E M O R Y                   --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.12 $                             --
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

with System.Task_Primitives;
--  Used for, Lock
--            Unlock
--            Initialize_Lock
--            Write_Lock

pragma Elaborate (System.Task_Primitives);
package body System.Task_Memory is

   --  malloc() and free() are not currently thread-safe, though they should
   --  be. In the meantime, these protected versions are provided.

   Memory_Mutex : Task_Primitives.Lock;

   --------------------
   -- Low_Level_Free --
   --------------------

   procedure Low_Level_Free (A : System.Address) is

      Error : Boolean;

      procedure free (Addr : System.Address);
      pragma Import (C, free, "free");

   begin
      Task_Primitives.Write_Lock (Memory_Mutex, Error);
      free (A);
      Task_Primitives.Unlock (Memory_Mutex);
   end Low_Level_Free;

   -------------------
   -- Low_Level_New --
   -------------------

   function Low_Level_New
     (Size : Storage_Elements.Storage_Count)
      return System.Address
   is
      Temp : System.Address;
      Error : Boolean;

      function malloc
        (Size : in Storage_Elements.Storage_Count)
         return System.Address;
      pragma Import (C, malloc, "malloc");

   begin
      Task_Primitives.Write_Lock (Memory_Mutex, Error);
      Temp := malloc (Size);
      Task_Primitives.Unlock (Memory_Mutex);
      return Temp;
   end Low_Level_New;

   --------------------------
   -- Unsafe_Low_Level_New --
   --------------------------

   function Unsafe_Low_Level_New
     (Size : Storage_Elements.Storage_Count)
      return System.Address
   is
      function malloc
        (Size : in Storage_Elements.Storage_Count)
         return System.Address;
      pragma Import (C, malloc, "malloc");

   begin
      return malloc (Size);
   end Unsafe_Low_Level_New;

begin

   Task_Primitives.Initialize_Lock (Any_Priority'Last, Memory_Mutex);
   --  Initialize the lock used to synchronize low-level memory allocation.

end System.Task_Memory;

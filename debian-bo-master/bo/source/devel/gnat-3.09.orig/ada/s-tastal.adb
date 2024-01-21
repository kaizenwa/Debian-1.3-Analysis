-----------------------------------------------------------------------------
--                                                                         --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                         --
--       S Y S T E M . T A S K _ S T O R A G E _ A L L O C A T I O N       --
--                                                                         --
--                                 B o d y                                 --
--                                                                         --
--                            $Revision: 1.6 $                             --
--                                                                         --
--        Copyright (C) 1991,1992,1993,1994 Florida State University        --
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

with System.Storage_Elements;
--  Used for, Storage_Count

with System.Task_Memory;
--  Used for, Low_Level_New
--            Low_Level_Free

package body System.Task_Storage_Allocation is

   --------------------
   -- Allocate_Block --
   --------------------

   --  Note: the Alignment parameter is ignored here, since Low_Level_New
   --  is guaranteed to return a block of the maximum possible alignment.

   procedure Allocate_Block
     (Storage_Address : out System.Address;
      Storage_Size    : Storage_Elements.Storage_Count;
      Alignment       : in Storage_Elements.Storage_Count)
   is
   begin
      Storage_Address := Task_Memory.Low_Level_New (Storage_Size);
   end Allocate_Block;

   ----------------------
   -- Deallocate_Block --
   ----------------------

   procedure Deallocate_Block (Storage_Address : System.Address) is
   begin
      Task_Memory.Low_Level_Free (Storage_Address);
   end Deallocate_Block;

   ---------------------
   -- Maximum_Storage --
   ---------------------

   --  Returns zero, indicating no fixed limit, since there is no fixed
   --  (determinable) limit on the memory available on a POSIX system.

   function Maximum_Storage return Storage_Elements.Storage_Count is
   begin
      return 0;
   end Maximum_Storage;

end System.Task_Storage_Allocation;

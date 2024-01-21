------------------------------------------------------------------------------
--                                                                          --
--                 Gnu Ada Runtime Library (Gnarl) Components               --
--                                                                          --
--                 S Y S T E M . T A S K _ P R I M I T I V E S              --
--                                                                          --
--                                  S p e c                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.1 $                            --
--                                                                          --
--    Copyright (C) 1991,92,93,94,95,1996 Free Software Foundation, Inc.    --
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

with System.OS_Interface;
with Interfaces.C;

package System.Task_Primitives is
   pragma Preelaborate;

   type Lock is new System.OS_Interface.mutex_t;

   type RTS_Lock is new System.OS_Interface.mutex_t;

   type Task_Body_Access is access procedure;

   type Private_Data is record
      Thread      : aliased System.OS_Interface.thread_t;
      CV          : aliased System.OS_Interface.cond_t;
      L           : aliased RTS_Lock;
      Current_Priority : Interfaces.C.int;
      Stack_Size : Interfaces.c.size_t;
   end record;

end System.Task_Primitives;

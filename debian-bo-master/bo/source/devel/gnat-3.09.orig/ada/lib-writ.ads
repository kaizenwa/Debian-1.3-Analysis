------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . W R I T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $                              --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routines for writing the library information

package Lib.Writ is

   function Increment_Def_Func_Count return Nat;
   --  Increment Def_Func_Count field for current unit, and return the
   --  incremented value.

   function Increment_Serial_Number return Nat;
   --  Increment Serial_Number field for current unit, and return the
   --  incremented value.

   procedure Store_Linker_Option_String (S : String_Id);
   --  This procedure is called to register the string from a pragma
   --  Linker_Option. The argument is the Id of the string to register.

   procedure Write_Library_Info;
   --  This procedure writes the library information for the current main unit

private
   pragma Inline (Increment_Serial_Number);

end Lib.Writ;

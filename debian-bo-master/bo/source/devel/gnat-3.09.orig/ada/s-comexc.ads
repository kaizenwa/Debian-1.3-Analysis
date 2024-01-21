------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--            S Y S T E M . C O M P I L E R _ E X C E P T I O N S           --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.11 $                            --
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
--  Uses, Task_Primitives.Machine_Exceptions
--        Task_Primitives.Error_Information

with Ada.Exceptions;
--  Used for, Exception_Id
--            Raise_Exception

with System.Standard_Library;
--  Used for Exception_Data
--           Program_Error_Def
--           Tasking_Error_Def
--           Constraint_Error_Def
--           Storage_Error_Def

with Unchecked_Conversion;

package System.Compiler_Exceptions is

   type Exception_Access is access all System.Standard_Library.Exception_Data;

   function Access_To_Exception_Id is new
     Unchecked_Conversion (Exception_Access, Ada.Exceptions.Exception_Id);

   Tasking_Error_Id : constant Ada.Exceptions.Exception_Id :=
     Access_To_Exception_Id (System.Standard_Library.Tasking_Error_Def'Access);

   Program_Error_Id : constant Ada.Exceptions.Exception_Id :=
     Access_To_Exception_Id (System.Standard_Library.Program_Error_Def'Access);

   Constraint_Error_Id : constant Ada.Exceptions.Exception_Id :=
     Access_To_Exception_Id (
       System.Standard_Library.Constraint_Error_Def'Access);

   Storage_Error_Id : constant Ada.Exceptions.Exception_Id :=
     Access_To_Exception_Id (System.Standard_Library.Storage_Error_Def'Access);

   type Pre_Call_State is private;

   procedure Notify_Exception
     (Which              : System.Task_Primitives.Machine_Exceptions;
      Info               : System.Task_Primitives.Error_Information;
      Modified_Registers : Pre_Call_State);

   function Current_Exception return Ada.Exceptions.Exception_ID;

private

   type tmp is record
      d : integer;
   end record;

   type Pre_Call_State is access tmp;
end System.Compiler_Exceptions;

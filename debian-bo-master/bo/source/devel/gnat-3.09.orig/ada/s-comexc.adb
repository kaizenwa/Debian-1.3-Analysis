-----------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--            S Y S T E M . C O M P I L E R _ E X C E P T I O N S           --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.14 $                            --
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

with System.Error_Reporting;
--  Used for,  Error_Reporting.Unimplemented_Operation
--             Error_Reporting.Assert

with System.Task_Primitives;
--  Used for,  Task_Primitives.Machine_Exceptions;
--             Task_Primitives.Error_Information;

with System.Compiler_Exceptions.Machine_Specifics;

with System.Task_Specific_Data;
--  Used for,  Get_GNAT_Exception

with Unchecked_Conversion;

package body System.Compiler_Exceptions is

   procedure Unimplemented renames
      System.Error_Reporting.Unimplemented_Operation;

   ----------------------
   -- Notify_Exception --
   ----------------------

   procedure Notify_Exception
     (Which              : Task_Primitives.Machine_Exceptions;
      Info               : Task_Primitives.Error_Information;
      Modified_Registers : Pre_Call_State)
   is
   begin
      Ada.Exceptions.Raise_Exception (
        Compiler_Exceptions.Machine_Specifics.Identify_Exception (
          Which, Info, Modified_Registers));
   end Notify_Exception;

   -----------------------
   -- Current_Exception --
   -----------------------

   function Current_Exception return Ada.Exceptions.Exception_ID is
      function Address_To_Exception_Id is new
        Unchecked_Conversion (Address, Ada.Exceptions.Exception_Id);
   begin
      return Address_To_Exception_Id (
        System.Task_Specific_Data.Get_GNAT_Exception);
   end Current_Exception;

end System.Compiler_Exceptions;

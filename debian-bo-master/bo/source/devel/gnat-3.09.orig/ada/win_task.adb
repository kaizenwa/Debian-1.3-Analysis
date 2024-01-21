------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                              W I N _ T A S K                             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.1 $                             --
--                                                                          --
--             Copyright (c) 1993,1994 NYU, All Rights Reserved             --
--                                                                          --
--  GNARL is free software; you can redistribute it and/or modify it  under --
--  terms  of  the  GNU  Library General Public License as published by the --
--  Free Software Foundation; either version 2,  or (at  your  option)  any --
--  later  version.   GNARL is distributed in the hope that it will be use- --
--  ful, but but WITHOUT ANY WARRANTY; without even the implied warranty of --
--  MERCHANTABILITY  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Gen- --
--  eral Library Public License for more details.  You should have received --
--  a  copy of the GNU Library General Public License along with GNARL; see --
--  file COPYING. If not, write to the Free Software Foundation,  675  Mass --
--  Ave, Cambridge, MA 02139, USA.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Win_Task.Errors; use Win_Task.Errors;

package body Win_Task is

   procedure Must_Not_Fail (Return_Code : DWORD) is
   begin
      pragma Assert (Return_Code = NO_ERROR);
      null;
   end Must_Not_Fail;

   procedure Must_Not_Fail_Bool (Return_Code : BOOL) is
   begin
      --  Windows NT treats a value of True to be successful
      pragma Assert (Return_Code);
      null;
   end Must_Not_Fail_Bool;

end Win_Task;

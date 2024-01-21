------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . L O A D                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This child package contains the function used to load a separately
--  compiled unit, as well as the routine used to initialize the unit
--  table and load the main source file.
--

package Lib.Load is

   procedure Initialize;
   --  Called at the start of compiling a new main source unit to initialize
   --  the library processing for the new main source. Establishes and
   --  initializes the units table entry for the new main unit (leaving
   --  the Unit_File_Name entry of Main_Unit set to No_File if there are no
   --  more files. Otherwise the main source file has been opened and read
   --  and then closed on return.

   procedure Initialize_Version (U : Unit_Number_Type);
   --  This is called once the source file corresponding to unit U has been
   --  fully scanned. At that point the checksum is computed, and can be used
   --  to initialize the version number.

   function Load_Unit
     (Uname     : Unit_Name_Type;
      Required  : Boolean;
      Enode     : Node_Id;
      Corr_Body : Unit_Number_Type := No_Unit)
      return      Unit_Number_Type;
   --  This function loads and parses the unit specified by Unit_Name_Type
   --  (or returns the unit number for the previously constructed units table
   --  entry if this is not the first call for this unit). Required indicates
   --  the behavior on a file not found condition, as further described below,
   --  and Enode is the node in the calling program to which error messages
   --  are to be attached.
   --
   --  If the corresponding file is found, the value returned by Load is the
   --  unit number that indexes the corresponding entry in the units table. If
   --  a serious enough parser error occurs to prevent subsequent semantic
   --  analysis, then the Fatal_Error flag of the returned entry is set and
   --  in addition, the fatal error flag of the calling unit is also set.
   --
   --  If the corresponding file is not found, then the behavior depends on
   --  the setting of Required. If Required is False, then No_Unit is returned
   --  and no error messages are issued. If Required is True, then a fatal
   --  error message is posted, and Unrecoverable_Error raised to abandon the
   --  compilation.
   --
   --  A special case arises in the call from Rtsfind, where Enode is set to
   --  Empty. In this case Required is False, and the caller in any case treats
   --  any error as fatal.
   --
   --  The Corr_Body argument is normally defaulted. It is set only in the
   --  case of loading the corresponding spec when the main unit is a body.
   --  In this case, Corr_Body is the unit number of this corresponding
   --  body. This is used to set the Serial_Ref_Unit field of the unit
   --  table entry. It is also used to deal with the special processing
   --  required by RM 10.1.4(4). See description in lib.ads.

   procedure Make_Instance_Unit (N : Node_Id);
   --  When a compilation unit is an instantiation, it contains both the
   --  declaration and the body of the instance, each of which can have its
   --  own elaboration routine. The file itself corresponds to the declaration.
   --  We create an additional entry for the body, so that the binder can
   --  generate the proper elaboration calls to both. The argument N is the
   --  compilation unit node created for the body.

   procedure Version_Update (U : Node_Id; From : Node_Id);
   --  This routine is called when unit U is found to be semantically
   --  dependent on unit From. It updates the version of U to register
   --  dependence on the version of From. The arguments are compilation
   --  unit nodes for the relevant library nodes.

end Lib.Load;

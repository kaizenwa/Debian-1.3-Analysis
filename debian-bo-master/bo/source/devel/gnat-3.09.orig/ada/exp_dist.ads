------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D I S T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.10 $                             --
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

--  This package contains utility routines used for the generation of the
--  stubs relevant to the distribution annex.

with Types; use Types;

package Exp_Dist is

   procedure Add_RAST_Features (Vis_Decl : Node_Id);
   --  Build and add bodies for dereference and 'Access subprograms for a
   --  Remote_Access_To_Subprogram type. Vis_Decl is the declaration node of
   --  the RAS type.

   procedure Add_Racw_RW (N : Node_Id);
   --  Add procedures and representation clauses to override read and write
   --  attribute for the RACW type whose declaration node is N.

   function Build_Calling_Stubs_Bodies_Cunit
     (RCI_Cunit : Node_Id) return Node_Id;
   --  Builds the calling stubs package body and returns the corresponding
   --  compilation unit node. RCI_Cunit is the compilation unit node of a RCI
   --  package declaration.

   function Build_Receiving_Stubs_Bodies_Cunit
     (RCI_Cunit : Node_Id)
      return      Node_Id;
   --  Builds and returns the receiving stubs package body compilation unit.

   function Build_Passive_Partition_Stub
     (Comp      : Node_Id;
      Is_Client : Boolean := False)
      return      Node_Id;
   --  Build stub for client of shared passive package. Comp is the
   --  analyzed compilation unit for a package declaration.

   procedure Init_Names;
   --  Initializes the external names used in the name table.

   function Is_ACWLP_Type (E : Entity_Id) return Boolean;
   --  Returns true if the entity is an access class-wide limited private type.

   function Is_RCI_Pkg_Spec_Or_Body (Cunit : Node_Id) return Boolean;
   --  Determines if a compilation unit is the specification or the
   --  body of a remote call interface package.

end Exp_Dist;

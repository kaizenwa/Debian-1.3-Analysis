------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 X R E F                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.9 $                              --
--                                                                          --
--        Copyright (C) 1992,1993,1994 Free Software Foundation, Inc.       --
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

--  The Xref package contains the routines for gathering all definition/use
--  information (entities and references to these entities). It creats a cross-
--  reference list as well as the REQs (required interfaces) for the withed
--  specs.

with Types; use Types;

package Xref is

   procedure Initialize;
   --  Initialize output of cross-reference information. Must be called by
   --  the main driver once before processing any source input files.

   procedure Finalize;
   --  Finalize Xref output. Must be called by the main driver once after
   --  processing any source input files if there were no errors detected
   --  during processing of any of the files. If there were any errors
   --  detected, then this call should not be made.

   procedure Gather_Xref_Info (Top : Node_Id);
   --  Main procedure to build the entity tables and the REQs. Looks for new
   --  entities and new references and adds them to appropriate entity table.
   --  This call has no effect if the appropriate xref switches are not set.

end Xref;

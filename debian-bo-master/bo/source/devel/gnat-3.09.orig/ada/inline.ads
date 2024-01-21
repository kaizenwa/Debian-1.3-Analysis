------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               I N L I N E                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.4 $                              --
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

--  This module handles two kinds of inlining activity:

--  a) Instantiation of generic bodies. This is done unconditionally, after
--  analysis and expansion of the main unit.

--  b) Compilation of unit bodies that contain the bodies of inlined sub-
--  programs. This is done only if inlining is enabled (-gnatn). Full inlining
--  requires that a) an b) be mutually recursive, because each step may
--  generate another generic expansion and further inlined calls. For now each
--  of them uses a workpile algorithm, but they are called independently from
--  Frontend, and thus are not mutually recursive.

with Alloc;
with Opt;    use Opt;
with Snames; use Snames;
with Table;
with Types;  use Types;

package Inline is

   --------------------------------
   -- Generic Body Instantiation --
   --------------------------------

   --  The bodies of generic instantiations are built after semantic analysis
   --  of the main unit is complete. Generic instantiations are saved in a
   --  global data structure, and the bodies constructed by means of a separate
   --  analysis and expansion step.

   --  See full description in body of Sem_Ch12 for details

   type Pending_Body_Info is record
      Inst_Node : Node_Id;
      --  Node for instantiation that requires the body

      Act_Decl  : Node_Id;
      --  Declaration for package or subprogram spec for instantiation
   end record;

   package Pending_Instantiations is new Table (
     Table_Component_Type => Pending_Body_Info,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Pending_Instantiations_Initial,
     Table_Increment      => Alloc.Pending_Instantiations_Increment,
     Table_Name           => "Sem.Pending_Instantiations");

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Initialize internal tables

   procedure Lock;
   --  Lock internal tables before calling backend

   procedure Instantiate_Bodies;
   --  This procedure is called after semantic analysis is complete, to
   --  instantiate the bodies of generic instantiations that appear in the
   --  compilation unit.

   procedure Add_Inlined_Body (N : Node_Id; E : Entity_Id);
   --  N is a procedure or function call, and E is the called entity, which
   --  is an inlined subprogram. Add E's enclosing unit to Inlined_Bodies
   --  so that body of E can be subsequently retrieved and analyzed.

   procedure Analyze_Inlined_Bodies;
   --  At end of compilation, analyze the bodies of all units that contain
   --  inlined subprograms that are actually called.

end Inline;

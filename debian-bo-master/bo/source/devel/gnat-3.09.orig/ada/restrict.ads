------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             R E S T R I C T                              --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package deals with the implementation of the Restrictions pragma

with Types; use Types;
with Uintp; use Uintp;
with Uname; use Uname;

package Restrict is

   Any_Restrictions : Boolean := False;
   --  Set to True if any restrictions have been processed (used to optimize
   --  testing of restrictions to speed up normal case with no restrictions).

   --  The following enumeration type defines the set of restriction
   --  identifiers that are implemented in GNAT. To add a new restriction
   --  identifier, add an entry with the name to be used in the pragma.

   type Restriction_Id is (
     Immediate_Reclamation,                   -- (RM H.4(10))
     No_Abort_Statements,                     -- (RM D.7(5), H.4(3))
     No_Access_Subprograms,                   -- (RM H.4(17))
     No_Allocators,                           -- (RM H.4(7))
     No_Asynchronous_Control,                 -- (RM D.9(10))
     No_Delay,                                -- (RM H.4(21))
     No_Dispatch,                             -- (RM H.4(19))
     No_Dynamic_Priorities,                   -- (RM D.9(9))
     No_Exceptions,                           -- (RM H.4(12))
     No_Fixed_Point,                          -- (RM H.4(15))
     No_Floating_Point,                       -- (RM H.4(14))
     No_Implementation_Attributes,            -- GNAT
     No_Implementation_Pragmas,               -- GNAT
     No_IO,                                   -- (RM H.4(20))
     No_Implicit_Heap_Allocations,            -- (RM D.8(8), H.4(3))
     No_Local_Allocators,                     -- (RM H.4(8))
     No_Nested_Finalization,                  -- (RM D.7(4))
     No_Protected_Types,                      -- (RM H.4(5))
     No_Recursion,                            -- (RM H.4(22))
     No_Reentrancy,                           -- (RM H.4(23))
     No_Task_Allocators,                      -- (RM D.7(7))
     No_Task_Hierarchy,                       -- (RM D.7(3), H.4(3))
     No_Terminate_Alternatives,               -- (RM D.7(6))
     No_Unchecked_Access,                     -- (RM H.4(18))
     No_Unchecked_Conversion,                 -- (RM H.4(16))
     No_Unchecked_Deallocation,               -- (RM H.4(9))
     Not_A_Restriction_Id);

   --  The following enumeration type defines the set of restriction
   --  parameter identifiers that are implemented in GNAT. To add a new
   --  restriction parameter identifier, add an entry with the name to be
   --  used in the pragma.

   --  Note: the GNAT implementation currently only accomodates restriction
   --  parameter identifiers whose expression value is a non-negative
   --  integer. This is true for all language defined parameters.

   type Restriction_Parameter_Id is (
     Max_Asynchronous_Select_Nesting,         -- (RM D.9(18), H.4(3))
     Max_Protected_Entries,                   -- (RM D.9(14))
     Max_Select_Alternatives,                 -- (RM D.9(12))
     Max_Storage_At_Blocking,                 -- (RM D.9(17))
     Max_Task_Entries,                        -- (RM D.9(13), H.4(3))
     Max_Tasks,                               -- (RM D.9(19), H.4(3))
     Not_A_Restriction_Parameter_Id);

   --  The following array indicates the setting of restriction identifiers.
   --  All values are initially False, and are set True if a Restrictions
   --  pragma specifies the corresponding restriction identifier.

   type Restrictions_Type is
     array (Restriction_Id) of Boolean;

   Restrictions_Default :
     constant Restrictions_Type := (others => False);

   Restrictions :
     Restrictions_Type := Restrictions_Default;

   --  The following array indicates the setting of restriction parameter
   --  identifier values. All values are initially set to -1 indicating that
   --  the parameter is not set, and are set to the appropriate non-negative
   --  value if a Restrictions pragma specifies the corresponding restriction
   --  parameter identifier with an appropriate value.

   type Restriction_Parameters_Type is
     array (Restriction_Parameter_Id) of Uint;

   Restriction_Parameters_Default :
     constant Restriction_Parameters_Type := (others => Uint_Minus_1);

   Restriction_Parameters :
     Restriction_Parameters_Type := Restriction_Parameters_Default;

   --  This array defines the mapping between restriction identifiers and
   --  predefined language files containing units for which the identifier
   --  forbids semantic dependence.

   type Unit_Entry is record
      Res_Id : Restriction_Id;
      Filenm : String (1 .. 6);
   end record;

   type Unit_Array_Type is array (Positive range <>) of Unit_Entry;

   Unit_Array : constant Unit_Array_Type := (
     (No_Asynchronous_Control,    "astaco"),
     (No_Delay,                   "calend"),
     (No_Dynamic_Priorities,      "dynpri"),
     (No_IO,                      "direio"),
     (No_IO,                      "sequio"),
     (No_IO,                      "ststio"),
     (No_IO,                      "textio"),
     (No_IO,                      "witeio"),
     (No_Unchecked_Conversion,    "unccon"),
     (No_Unchecked_Deallocation,  "uncdea"));

   -----------------
   -- Subprograms --
   -----------------

   procedure Check_Restricted_Unit (U : Unit_Name_Type; Loc : Source_Ptr);
   --  Checks if loading of unit U is prohibited by the setting of some
   --  restriction (e.g. No_IO restricts the loading of unit Ada.Text_IO).
   --  If a restriction exists post error message at the given location.

   procedure Check_Restriction (R : Restriction_Id; N : Node_Id);
   --  Checks that the given restriction is not set, and if it is set, posts
   --  an appropriate message is posted on the given node.

   procedure Check_Restriction
     (R : Restriction_Parameter_Id;
      N : Node_Id);
   --  Checks that the given restriction parameter identifier is not set to
   --  zero. If it is set to zero, then the node N is replaced by a node
   --  that raises Storage_Error, and a warning is issued.

   procedure Check_Restriction
     (R : Restriction_Parameter_Id;
      V : Uint;
      N : Node_Id);
   --  Checks that the count in V does not exceed the maximum value of the
   --  restriction parameter value corresponding to the given restriction
   --  parameter identifier (if it has been set). If the count in V exceeds
   --  the maximum, then post an error message on node N.

   function Get_Restriction_Id
     (N    : Name_Id)
      return Restriction_Id;
   --  Given an identifier name, determines if it is a valid restriction
   --  identifier, and if so returns the corresponding Restriction_Id
   --  value, otherwise returns Not_A_Restriction_Id.

   function Get_Restriction_Parameter_Id
     (N    : Name_Id)
      return Restriction_Parameter_Id;
   --  Given an identifier name, determines if it is a valid restriction
   --  parameter identifier, and if so returns the corresponding
   --  Restriction_Parameter_Id value, otherwise returns
   --  Not_A_Restriction_Parameter_Id.

end Restrict;

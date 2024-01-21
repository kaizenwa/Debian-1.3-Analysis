------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ D I S T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.45 $                             --
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

--  This package contains utility routines used for validation of the
--  use of the categorization pragmas relevant to the distribution annexe.

with Types; use Types;

package Sem_Dist is

   procedure Append_System_RPC (N : Node_Id);
   --  Given N, a N_Compilation_Unit node whose unit is package spec, if
   --  unit is RCI or remote types then append System.RPC to Context_Items.
   --  System.RPC is used in remote access to subprogram type declaration
   --  processing, check Process_Remote_AST_Declaration for details.

   procedure Append_System_PI (N : Node_Id; L : Node_Id);
   --  Given N, a N_Compilation_Unit node thatis withing a unit whose
   --  N_Compilation_Unit node is L, where L is an RCI or remote types
   --  unit, append System.Partition_Interface to its context items.
   --  System.Partition_Interface is used in processing remote access
   --  to subprogram type (in the call to Get_Local_Partition_Id.)

   procedure CW_Remote_Extension_Add_Receiver (N : Node_Id);
   --  In case N, the (library) package body node or its corresponding spec
   --  has classwide type remote extensions (check comments in following
   --  function Is_Class_Wide_Type_Remote_Extension for definition) then
   --  generates receiver subprogram for this type (extension). Receiver
   --  is used to handle incoming remote dispatching calls.

   function Enclosing_Lib_Unit_Entity return Entity_Id;
   --  Returns the entity of enclosing N_Compilation_Unit Node which is the
   --  root of the current scope (which must not be Standard_Standard, and
   --  the caller is responsible for ensuring this condition).

   function Enclosing_Lib_Unit_Node (N : Node_Id) return Node_Id;
   --  Returns the enclosing N_Compilation_Unit Node that is the root
   --  of a subtree containing N.

   procedure Generate_Stubs_Files (N : Node_Id);
   --  Create the stubs files for a remote call interface package specification
   --  or body. If the corresponding unit is a package declaration requiring
   --  a body, then only the client stubs package body and the server stubs
   --  package specification are generated. If it is a package body or a
   --  package declaration which  does not require a body, the server stubs
   --  package body is also generated.

   function In_Preelaborated_Unit return Boolean;
   --  Determines if the current scope is within a preelaborated compilation
   --  unit, that is one to which one of the pragmas Preelaborate, Pure,
   --  Shared_Passive, Remote_Types, or inside a unit other than a package
   --  body with pragma Remote_Call_Interface.

   function In_Pure_Unit return Boolean;
   pragma Inline (In_Pure_Unit);
   --  Determines if the current scope is within pure compilation unit,
   --  that is, one to which the pragmas Pure is applied.

   function In_Remote_Call_Interface_Unit return Boolean;
   --  Determines if the current scope is within a Remote Call Interface
   --  compilation unit.

   function In_Remote_Types_Unit return Boolean;
   pragma Inline (In_Remote_Types_Unit);
   --  Determines if current scope is within a Remote Types compilation unit

   function In_Shared_Passive_Unit return Boolean;
   pragma Inline (In_Shared_Passive_Unit);
   --  Determines if current scope is within a Shared Passive compilation unit.

   function In_Subprogram_Task_Protected_Unit return Boolean;
   --  Determines if the current scope is within a subprogram, task
   --  or protected unit. Used to validate if the library unit is Pure
   --  (RM 10.2.1(16)).

   function In_Subprogram_Unit return Boolean;
   --  Determines if the current scope is within a subprogram compilation
   --  unit (inside a subprogram declaration, subprogram body, or generic
   --  subprogram declaration). The test is for appearing anywhere within
   --  such a construct (that is it does not need to be directly within).

   function Is_Class_Wide_Type_Remote_Extension (N : Node_Id) return Boolean;
   --  Return True if N is an extension of a root abstract-tagged-limited-
   --  private-type and the root abstract-type is the designated type of
   --  an RCI remote access-to-limited-class-wide-type.

   function Is_Remote_Access_To_Class_Wide_Type (E : Entity_Id) return Boolean;
   --  Return True if E is a remote access-to-class-wide-limited_private type

   function Is_Remote_Access_To_Subprogram_Type (E : Entity_Id) return Boolean;
   --  Return True if E is a remote access to subprogram type.

   procedure Process_Partition_Id (N : Node_Id);
   --  Replace attribute reference with call to runtime function. The result
   --  is converted to the context type, because the attribute yields a
   --  universal integer value.

   procedure Process_Remote_AST_Attribute (N : Node_Id; New_Type : Entity_Id);
   --  Given N, an access attribute reference node whose prefix is a
   --  remote subprogram, rewrite N with a call to a conversion function
   --  whose return type is New_Type.

   procedure Process_Remote_AST_Declaration (N : Node_Id);
   --  Given N, an access to subprogram type declaration node in RCI or
   --  remote types unit, build a new record (fat pointer) type declaration
   --  using the old Defining_Identifier of N and a link to the old
   --  declaration node N whose Defining_Identifier is changed.
   --  We also construct declarations of two subprograms in the unit
   --  specification which handle remote access to subprogram type
   --  (fat pointer) dereference and the unit receiver that handles
   --  remote calls (from remote access to subprogram type values.)

   function Remote_AST_E_Dereference (P : Node_Id) return Boolean;
   --  If the prefix of an explicit dereference is a record type that
   --  represent the fat pointer for an Remote access to subprogram, in
   --  the context of a call, rewrite the enclosing call node into a
   --  remote call, the first actual of which is the fat pointer. Return
   --  true if the context is correct and the transformation took place.

   function Remote_AST_I_Dereference (P : Node_Id) return Boolean;
   --  If P is a record type that represents the fat pointer for a remote
   --  access to subprogram, and P is the prefix of a call, insert an
   --  explicit dereference and perform the transformation described for
   --  the previous function.

   procedure Process_Remote_Access_Subprogram_Type (N : Node_Id);
   --  Given N, a package declaration node, if it is an RCI unit with
   --  declaration of remote access to subprogram type then define its
   --  Read, Write and enumerate the list of remote subprograms.
   --
   --  The mechanism of such types works well in the case of dereferencing
   --  a pointer (an access value) of such type initialized by 'Access
   --  locally. For pointers passed across partition boundaries, this
   --  procedure will construct the mechanism (Read, Write and enumeration
   --  of remote subprograms) to complete the full functionality.
   --
   --  Write will translate an access value to an index (of integer type)
   --  which represents the corresponding enumeration of the designated
   --  remote subprogram.
   --
   --  Read will do the reverse translation, from index to a local pointer.
   --  When dereferenced, since all pointers point to either subprograms in
   --  receiver partition or subprograms in caller partitions (stub
   --  subprogram), the expected result is correct.

   function Should_Declare_Partition_ID (L : List_Id) return Boolean;
   --  Given a non-empty list L of the Visible_Declarations of the package
   --  specification of a libbrary, unit, determines whether the package is
   --  System.RPC or is categorized as pure. This is used to limit the
   --  library package units that declare Predefined_Partition_Id to those
   --  that really need to.

   procedure Set_Categorization_From_Pragmas (N : Node_Id);
   --  Since validation of categorization dependency is done during analyze
   --  so categorization flags from following pragmas should be set before
   --  validation begin. N is the N_Compilation_Unit node.

   procedure Validate_Access_Type_Declaration (T : Entity_Id; N : Node_Id);
   --  Validate all constraints against declaration of access types in
   --  categorized library units. Usually this is a violation in Pure unit,
   --  Shared_Passive unit. N is the declaration node.

   procedure Validate_Categorization_Dependency (N : Node_Id; E : Entity_Id);
   --  There are restrictions on lib unit that semantically depends on other
   --  units (RM E.2(5), 10.2.1(11). This procedure checks the restrictions
   --  on categorizations. N is the current unit node, and E is the current
   --  library unit entity.

   procedure Validate_Non_Static_Call (N : Node_Id);
   --  Non-static calls are not allowed during the elaboration of a
   --  preelaborated unit. A call from inside a subprogram is however
   --  always fine (RM 10.2.1(7)). This procedure validates this rule.
   --  N is the call node.

   procedure Validate_Null_Statement_Sequence (N : Node_Id);
   --  Given N, a package body node, check that the handled statement sequence
   --  contains no statements other than null_statement. This from
   --  RM 10.2.1(6).

   procedure Validate_Object_Declaration
     (N   : Node_Id;
      Id  : Entity_Id;
      E   : Node_Id;
      Odf : Node_Id;
      T   : Entity_Id);
   --  Validate all the constraints in a preelaborable lib unit against
   --  an object declaration, including the creation of task object, etc.

   procedure Validate_RCI_Access_Object_Type_Declaration (T : Entity_Id);
   --  Check validity of declaration if RCI unit. It should not contain
   --  the declaration of an access-to-object type unless it is a
   --  general access type that designates a class-wide limited
   --  private type. There are also constraints about the primitive
   --  subprograms of the class-wide type. RM E.2.3(14).

   procedure Validate_RCI_Declarations (P : Entity_Id);
   --  Apply semantic checks given in  E2.3(10-14).

   procedure Validate_RCI_Subprogram_Declaration (N : Node_Id);
   --  Check for RCI unit subprogram declarations with respect to
   --  in-lined subprogram and subprogram with access parameter or
   --  limited type parameter without Read and Write.

   procedure Validate_Remote_Access_To_Class_Wide_Type (N : Node_Id);
   --  Checks that Storage_Pool and Storage_Size attribute references are
   --  not applied to remote access-to-class-wide types. And the expected
   --  type for an allocator shall not be a remote access-to-class-wide
   --  type. And a remote access-to-class-wide type shall not be an actual
   --  parameter for a generic formal access type. RM E.2.3(22).

   procedure Validate_RT_RAT_Component (N : Node_Id);
   --  Given N, the package library unit declaration node, we should check
   --  against RM:9.95 E.2.2(8): the full view of a type declared in the
   --  visible part of a Remote Types unit has a part that is of a non-remote
   --  access type which has no read/write.

   procedure Validate_Remote_Type_Type_Conversion (N : Node_Id);
   --  Check for remote-type type conversion constraints. First, a value of
   --  a remote access-to-subprogram type can be converted only to another
   --  type conformant remote access-to-subprogram type. Secondly, a value
   --  of a remote access-to-class-wide type can be converted only to another
   --  remote access-to-class-wide type (RM E.2.3(17,20)).

   procedure Validate_SP_Access_Object_Type_Decl (T : Entity_Id);
   --  Check validity of declaration if shared passive unit. It should not
   --  contain the declaration of an access-to-object type whose designated
   --  type is a class-wide type ,task type or protected type. E.2.1(7).
   --  T is the entity of the declared type.

   procedure Validate_Static_Object_Name (N : Node_Id);
   --  In the elaboration code of a preelaborated library unit, check
   --  that we do not have the evaluation of a primary that is a name of
   --  an object, unless the name is a static expression (RM 10.2.1(8)).
   --  Non-static constant and variable are the targets, generic parameters
   --  are not included because the generic declaration and body are
   --  preelaborable.

end Sem_Dist;

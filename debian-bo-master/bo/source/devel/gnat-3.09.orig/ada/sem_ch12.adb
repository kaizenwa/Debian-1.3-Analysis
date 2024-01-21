------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 2                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.468 $                            --
--                                                                          --
--          Copyright (C) 1992-1997, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Ch7;  use Exp_Ch7;
with Features; use Features;
with Freeze;   use Freeze;
with Gnat.Htable;
with Hostparm;
with Inline;   use Inline;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch10; use Sem_Ch10;
with Sem_Ch13; use Sem_Ch13;
with Sem_Dist; use Sem_Dist;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinfo.CN; use Sinfo.CN;
with Sinput;   use Sinput;
with Sinput.L; use Sinput.L;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Uname;    use Uname;
with Table;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Sem_Ch12 is

   use Atree.Unchecked_Access;
   --  This package performs untyped traversals of the tree, therefore it
   --  needs direct access to the fields of a node.

   ----------------------------------------------------------
   -- Implementation of Generic Analysis and Instantiation --
   -----------------------------------------------------------

   --  GNAT implements generics by macro expansion. No attempt is made to
   --  share generic instantiations (for now). Analysis of a generic definition
   --  does not perform any expansion action, but the expander must be called
   --  on the tree for each instantiation, because the expansion may of course
   --  depend on the generic actuals. All of this is best achieved as follows:
   --
   --  a) Semantic analysis of a generic unit is performed on a copy of the
   --  tree for the generic unit. All tree modifications that follow analysis
   --  do not affect the original tree. Links are kept between the original
   --  tree and the copy, in order to recognize non-local references within
   --  the generic, and propagate them to each instance (recall that name
   --  resolution is done on the generic declaration: generics are not really
   --  macros!). This is summarized in the following diagram:
   --
   --              .-----------.               .----------.
   --              |  semantic |<--------------|  generic |
   --              |    copy   |               |    unit  |
   --              |           |==============>|          |
   --              |___________|    global     |__________|
   --                             references     |   |  |
   --                                            |   |  |
   --                                          .-----|--|.
   --                                          |  .-----|---.
   --                                          |  |  .----------.
   --                                          |  |  |  generic |
   --                                          |__|  |          |
   --                                             |__| instance |
   --                                                |__________|
   --
   --  b) Each instantiation copies the original tree, and inserts into it a
   --  series of declarations that describe the mapping between generic formals
   --  and actuals. For example, a generic In OUT parameter is an object
   --  renaming of the corresponing actual, etc. Generic IN parameters are
   --  constant declarations.
   --
   --  c) In order to give the right visibility for these renamings, we use
   --  a different scheme for package and subprogram instantiations. For
   --  packages, the list of renamings is inserted into the package
   --  specification, before the visible declarations of the package. The
   --  renamings are analyzed before any of the text of the instance, and are
   --  thus visible at the right place. Furthermore, outside of the instance,
   --  the generic parameters are visible and denote their corresponding
   --  actuals.

   --  For subprograms, we create a container package to hold the renamings
   --  and the subprogram instance itself. Analysis of the package makes the
   --  renaming declarations visible to the subprogram. After analyzing the
   --  package, the defining entity for the subprogram is touched-up so that
   --  it appears declared in the current scope, and not inside the container
   --  package.

   --  If the instantiation is a compilation unit, the container package is
   --  given the same name as the subprogram instance. This ensures that
   --  the elaboration procedure called by the binder, using the compilation
   --  unit name, calls in fact the elaboration procedure for the package.

   --  Not surprisingly, private types complicate this approach. By saving in
   --  the original generic object the non-local references, we guarantee that
   --  the proper entities are referenced at the point of instantiation.
   --  However, for private types, this by itself does not insure that the
   --  proper VIEW of the entity is used (the full type may be visible at the
   --  point of generic definition, but not at instantiation, or vice-versa).
   --  In  order to reference the proper view, we special-case any reference
   --  to private types in the generic object, by saving both views, one in
   --  the generic and one in the semantic copy. At time of instantiation, we
   --  check whether the two views are consistent, and exchange declarations if
   --  necessary, in order to restore the correct visibility. Similarly, if
   --  the instance view is private when the generic view was not, we perform
   --  the exchange. After completing the instantiation, we restore the
   --  current visibility. The flag Has_Private_View marks identifiers in the
   --  the generic unit that require checking.

   --  Visibility within nested generic units requires special handling.
   --  Consider the following scheme:
   --
   --  type Global is ...         --  outside of generic unit.
   --  generic ...
   --  package Outer is
   --     ...
   --     type Semi_Global is ... --  global to inner.
   --
   --     generic ...                                         -- 1
   --     procedure inner (X1 : Global;  X2 : Semi_Global);
   --
   --     procedure in2 is new inner (...);                   -- 4
   --  end Outer;

   --  package New_Outer is new Outer (...);                  -- 2
   --  procedure New_Inner is new New_Outer.Inner (...);      -- 3

   --  The semantic analysis of Outer captures all occurrences of Global.
   --  The semantic analysis of Inner (at 1) captures both occurrences of
   --  Global and Semi_Global.

   --  At point 2 (instantiation of Outer), we also produce a generic copy
   --  of Inner, even though Inner is, at that point, not being instantiated.
   --  (This is just part of the semantic analysis of New_Outer).
   --  Critically, references to Global within Inner must be preserved, while
   --  references to Semi_Global should not preserved, because they must now
   --  resolve to an entity within New_Outer. To distinguish between these, we
   --  use a global variable, Current_Instantiated_Parent, which is set when
   --  performing a generic copy during instantiation (at 2). This variable is
   --  used when performing a generic copy that is not an instantiation, but
   --  that is nested within one, as the occurrence of 1 within 2. The analysis
   --  of a nested generic only preserves references that are global to the
   --  enclosing Current_Instantiated_Parent. We use the Scope_Depth value to
   --  determine whether a reference is external to the given parent.

   --  The instantiation at point 3 requires no special treatment. The method
   --  works as well for further nestings of generic units, but of course the
   --  variable Current_Instantiated_Parent must be stacked because nested
   --  instantiations can occur, e.g. the occurrence of 4 within 2.

   --  The instantiation of package and subprogram bodies is handled in a
   --  similar manner, except that it is delayed until after semantic
   --  analysis is complete. In this fashion complex cross-dependencies
   --  between several package declarations and bodies containing generics
   --  can be compiled which otherwise would diagnose spurious circularities.

   --  For example, it is possible to compile two packages A and B that
   --  have the following structure:

   --    package A is                         package B is
   --       generic ...                          generic ...
   --       package G_A is                       package G_B is

   --    with B;                              with A;
   --    package body A is                    package body B is
   --       package N_B is new G_B (..)          package N_A is new G_A (..)

   --  The table Pending_Instantiations in package Inline is used to keep
   --  track of body instantiations that are delayed in this manner. Inline
   --  handles the actual calls to do the body instantiations. This activity
   --  is part of Inline, since the processing occurs at the same point, and
   --  for essentially the same reason, as the handling of inlined routines.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Abandon_Instantiation (N : Node_Id);
   --  Posts an error message "instantiation abandoned" at the indicated
   --  node and then raises the exception Instantiation_Error to do it.

   procedure Analyze_Formal_Array_Type
     (T   : in out Entity_Id;
      Def : Node_Id);
   --  A formal array type is treated like an array type declaration, and
   --  invokes Array_Type_Declaration (sem_ch3) whose first parameter is
   --  in-out, because in the case of an anonymous type the entity is
   --  actually created in the procedure.

   --  The following procedures treat other kinds of formal parameters.

   procedure Analyze_Formal_Derived_Type
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id);

   --  All the following need comments???

   procedure Analyze_Formal_Decimal_Fixed_Point_Type
                                                (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Discrete_Type       (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Floating_Type       (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Signed_Integer_Type (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Modular_Type        (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Ordinary_Fixed_Point_Type
                                                (T : Entity_Id; Def : Node_Id);

   procedure Analyze_Formal_Private_Type
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id);
   --  This needs comments???

   procedure Analyze_Generic_Formal_Part (N : Node_Id);

   procedure Analyze_Generic_Access_Type (T : Entity_Id; Def : Node_Id);
   --  This needs comments ???

   function Analyze_Associations
     (Formals : List_Id;
      Actuals : List_Id;
      F_Copy  : List_Id)
      return    List_Id;
   --  At instantiation time, build the list of associations between formals
   --  and actuals. Each association becomes a renaming declaration for the
   --  formal entity. F_Copy is the analyzed list of formals in the generic
   --  copy. It is used to apply legality checks to the actuals.

   procedure Analyze_Subprogram_Instantiation
     (N : Node_Id;
      K : Entity_Kind);

   procedure Build_Instance_Compilation_Unit_Nodes
     (N        : Node_Id;
      Act_Body : Node_Id;
      Act_Decl : Node_Id);
   --  This procedure is used in the case where the generic instance of a
   --  subprogram body or package body is a library unit. In this case, the
   --  original library unit node for the generic instantiation must be
   --  replaced by the resulting generic body, and a link made to a new
   --  compilation unit node for the generic declaration. The argument N is
   --  the original generic instantiation. Act_Body and Act_Decl are the body
   --  and declaration of the instance (either package body and declaration
   --  nodes or subprogram body and declaration nodes depending on the case).
   --  On return, the node N has been rewritten with the actual body.

   procedure Check_Formal_Packages (P_Id : Entity_Id);
   --  Apply the following to all formal packages in generic associations.

   procedure Check_Formal_Package_Instance
     (Form_Pkg : Entity_Id;
      Act_Pkg  : Entity_Id);
   --  Verify that the actuals of the actual instance match the actuals of
   --  the template for a formal package that is not declared with a box.

   procedure Check_Premature_Instantiation
      (Inst_Node : Node_Id;
       Gen_Decl  : Node_Id);
   --  Detect simple cases of premature instantiation, until we implement full
   --  ABE checks for instances. If generic and instantiation are in the same
   --  declarative part, and the body of the generic has not been seen, insert
   --  Raise node. This is only called if it is known that a body is present.

   procedure Check_Private_View (N : Node_Id);
   --  Check whether the type of a generic entity has a different view between
   --  the point of generic analysis and the point of instantiation. If the
   --  view has changed, then at the point of instantiation we restore the
   --  correct view to perform semantic analysis of the instance, and reset
   --  the current view after instantiation.

   procedure Check_Generic_Actuals
     (Instance      : Entity_Id;
      Is_Formal_Box : Boolean);
   --  Similar to previous one. Check the actuals in the instantiation,
   --  whose views can change between the point of instantiation and the point
   --  of instantiation of the body. In addition, mark the generic renamings
   --  as generic actuals, so that they are not compatible with other actuals.
   --  Recurse on an actual that is a formal package whose declaration has
   --  a box.

   procedure Check_Generic_Child_Unit
     (Gen_Id           : Node_Id;
      Parent_Installed : in out Boolean);
   --  If the name of the generic unit in an instantiation is a selected
   --  component, then the prefix may be an instance and the selector may
   --  designate a child unit. Retrieve the parent generic and search for
   --  the child unit that must be declared within. Similarly, if this is
   --  the name of a generic child unit within an instantiation of its own
   --  parent, retrieve the parent generic.

   function Contains_Instance_Of
     (Inner : Entity_Id;
      Outer : Entity_Id;
      N     : Node_Id)
      return  Boolean;
   --  Inner is instantiated within the generic Outer. Check whether Inner
   --  directly or indirectly contains an instance of Outer. Each generic
   --  unit holds a list of the entities instantiated within. This procedure
   --  determines whether the set of such lists contains a cycle, i.e. an
   --  illegal circular instantiation.

   function Denotes_Formal_Package (Pack : Entity_Id) return Boolean;
   --  Returns True if E is a formal package of an enclosing generic, or
   --  the actual for such a formal in an enclosing instantiation. Used in
   --  Restore_Private_Views, to keep the formals of such a package visible
   --  on exit from an inner instantiation.

   function Find_Actual_Type
     (Typ       : Entity_Id;
      Gen_Scope : Entity_Id)
      return      Entity_Id;
   --  When validating the actual types of a child instance, check whether
   --  the formal is a formal type of the parent unit, and retrieve the current
   --  actual for it. ??? Need to document formals ???

   function Get_Package_Instantiation_Node (A : Entity_Id) return Node_Id;
   --  Given the entity of a unit that is an instantiation, retrieve the
   --  original instance node. This is used when loading the instantiations
   --  of the ancestors of a child generic that is being instantiated.

   procedure Set_Instance_Of (A : Entity_Id; B : Entity_Id);
   --  Associate analyzed generic parameter with corresponding
   --  instance. Used for semantic checks at instantiation time.

   function Has_Been_Exchanged (E : Entity_Id) return Boolean;
   --  Traverse the Exchanged_Views list to see if a type was private
   --  and has already been flipped during this phase of instantiation.

   procedure Hide_Current_Scope;
   --  When compiling a generic child unit, the parent context must be
   --  present, but the instance and all entities that may be generated
   --  must be inserted in the current scope. We leave the current scope
   --  on the stack, but make its entities invisible to avoid visibility
   --  problems. This is reversed at the end of instantiations. This is
   --  not done for the instantiation of the bodies, which only require the
   --  instances of the generic parents to be in scope.

   procedure Install_Body
     (Act_Body : Node_Id;
      N        : Node_Id;
      Gen_Body : Node_Id;
      Gen_Decl : Node_Id);
   --  If the instantiation happens textually before the body of the generic,
   --  the instantiation of the body must be analyzed after the generic body,
   --  and not at the point of instantiation. Such early instantiations can
   --  happen if the generic and the instance appear in  a package declaration
   --  because the generic body can only appear in the corresponding package
   --  body. Early instantiations can also appear if generic, instance and
   --  body are all in the declarative part of a subprogram or entry. Entities
   --  of packages that are early instantiations are delayed, and their freeze
   --  node appears after the generic body.

   procedure Install_Parent (P : Entity_Id; In_Body : Boolean := False);
   --  When compiling an instance of a child unit the parent (which is
   --  itself an instance) is an enclosing scope that must be made
   --  immediately visible.

   procedure Remove_Parent (In_Body : Boolean := False);
   --  Reverse effect after instantiation of child is complete.

   --  The functions Instantiate_XXX perform various legality checks and build
   --  the declarations for instantiated generic parameters.
   --  Need to describe what the parameters are ???

   function Instantiate_Object
      (Formal          : Node_Id;
       Actual          : Node_Id;
       Analyzed_Formal : Node_Id)
      return List_Id;

   function Instantiate_Type
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id)
      return Node_Id;

   function Instantiate_Formal_Subprogram
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id)
      return Node_Id;

   function Instantiate_Formal_Package
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id)
      return List_Id;
   --  If the formal package is declared with a box, special visibility rules
   --  apply to its formals: they are in the visible part of the package. This
   --  is true in the declarative region of the formal package, that is to say
   --  in the enclosing generic or instantiation. For an instantiation, the
   --  parameters of the formal package are made visible in an explicit step.
   --  Furthermore, if the actual is a visible use_clause, these formals must
   --  be made potentially use_visible as well. On exit from the enclosing
   --  instantiation, the reverse must be done.

   --  For a formal package declared without a box, there are conformance rules
   --  that apply to the actuals in the generic declaration and the actuals of
   --  the actual package in the enclosing instantiation. The simplest way to
   --  apply these rules is to repeat the instantiation of the formal package
   --  in the context of the enclosing instance, and compare the generic
   --  associations of this instantiation with those of the actual package.

   function Is_In_Main_Unit (N : Node_Id) return Boolean;
   --  Test if given node is in the main unit

   procedure Load_Parent_Of_Generic (N : Entity_Id; Spec : Node_Id);
   --  If the generic appears in a separate non-generic library unit,
   --  load the corresponding body to retrieve the body of the generic.

   procedure Inherit_Context (Gen_Decl : Node_Id; Inst : Node_Id);
   --  If a generic is a compilation unit, its instantiation has semantic
   --  dependences on the context units of the generic. Eventually these
   --  dependences will be reflected in actual ali files for generic units.
   --  In the meantime, the simplest is to attach the with clauses of the
   --  generic compilation to the compilation that contains the instance.

   function Associated_Node (N : Node_Id) return Node_Id;
   --  Nodes in a generic unit that have an entity field are linked to the
   --  corresponding nodes in the semantic copy, so that non-local references
   --  in the copy can be marked in the original generic nodes. The link
   --  overlaps the Entity field of the node, and must be reset correctly
   --  after collecting global references.

   procedure Move_Freeze_Nodes
     (Out_Of : Entity_Id;
      After  : Node_Id;
      L      : List_Id);
   --  Freeze nodes can be generated in the analysis of a generic unit, but
   --  will not be seen by the back-end. It is necessary to move those nodes
   --  to the enclosing scope if they freeze an outer entity. We place them
   --  at the end of the enclosing generic package, which is semantically
   --  neutral.

   procedure Set_Associated_Node
     (Gen_Node  : Node_Id;
      Copy_Node : Node_Id);
   --  Establish the link between an identifier in the generic unit, and the
   --  corresponding node in the semantic copy.

   -------------------------------------------
   -- Data structures for generic renamings --
   -------------------------------------------

   --  The map Generic_Renamings associates generic entities with their
   --  corresponding actuals. Currently used to validate type instances.
   --  It will eventually be used for all generic parameters to eliminate
   --  the need for overload resolution in the instance.

   type Assoc_Ptr is new Int;

   Assoc_Null : constant Assoc_Ptr := -1;

   type Assoc is record
      Gen_Id         : Entity_Id;
      Act_Id         : Entity_Id;
      Next_In_Htable : Assoc_Ptr;
   end record;

   package Generic_Renamings is new Table
     (Table_Component_Type => Assoc,
      Table_Index_Type     => Assoc_Ptr,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Generic_Renamings");

   --  Variable to hold enclosing instantiation.

   Current_Instantiated_Parent : Assoc := (Empty, Empty, Assoc_Null);

   --  Hash table for associations

   Htable_Size : constant := 37;
   type Htable_Range is range 0 .. Htable_Size - 1;

   procedure Set_Next_Assoc (E : Assoc_Ptr; Next : Assoc_Ptr);
   function  Next_Assoc     (E : Assoc_Ptr) return Assoc_Ptr;
   function Get_Gen_Id      (E : Assoc_Ptr) return Name_Id;
   function Hash            (F : Name_Id)   return Htable_Range;

   package Generic_Renamings_Htable is new GNAT.Htable.Static_Htable (
      Header_Num => Htable_Range,
      Element    => Assoc,
      Elmt_Ptr   => Assoc_Ptr,
      Null_Ptr   => Assoc_Null,
      Set_Next   => Set_Next_Assoc,
      Next       => Next_Assoc,
      Key        => Name_Id,
      Get_Key    => Get_Gen_Id,
      Hash       => Hash,
      Equal      => "=");

   Exchanged_Views : Elist_Id;
   --  This list holds the private views that have been exchanged during
   --  instantiation to restore the visibility of the generic declaration.
   --  (see comments above). After instantiation, the current visibility is
   --  reestablished by means of a traversal of this list.

   procedure Restore_Private_Views
     (Pack_Id    : Entity_Id;
      Is_Package : Boolean := True);
   --  Restore the private views of external types, and unmark the generic
   --  renamings of actuals, so that they become comptible subtypes again.
   --  For subprograms, Pack_Id is the package constructed to hold the
   --  renamings.

   procedure Switch_View (T : Entity_Id);
   --  Switch the partial and full views of a type and its private
   --  dependents (i.e. its subtypes and derived types).

   ------------------------------------
   -- Structures for Error Reporting --
   ------------------------------------

   Instantiation_Node : Node_Id;
   --  Used by subprograms that validate instantiation of formal parameters
   --  where there might be no actual on which to place the error message.
   --  Also used to locate the instantiation node for generic subunits.

   Instantiation_Error : exception;
   --  When there is a semantic error in the generic parameter matching,
   --  there is no point in continuing the instantiation, because the
   --  number of cascaded errors is unpredictable. This exception aborts
   --  the instantiation process altogether.

   S_Adjustment : Sloc_Adjustment;
   --  Offset created for each node in an instantiation, in order to keep
   --  track of the source position of the instantiation in each of its nodes.
   --  A subsequent semantic error or warning on a construct of the instance
   --  points to both places: the original generic node, and the point of
   --  instantiation. See Sinput and Sinput.L for additional details.

   ------------------------------------------------------------
   -- Data structure for keeping track when inside a Generic --
   ------------------------------------------------------------

   --  The following table is used to save values of the Inside_A_Generic
   --  flag (see spec of Sem) when they are saved by Start_Generic.

   package Generic_Flags is new Table (
     Table_Component_Type => Boolean,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 32,
     Table_Increment      => 200,
     Table_Name           => "Generic_Flags");

   --------------------
   -- Set_Next_Assoc --
   --------------------

   procedure Set_Next_Assoc (E : Assoc_Ptr; Next : Assoc_Ptr) is
   begin
      Generic_Renamings.Table (E).Next_In_Htable := Next;
   end Set_Next_Assoc;

   ----------------
   -- Next_Assoc --
   ----------------

   function Next_Assoc (E : Assoc_Ptr) return Assoc_Ptr is
   begin
      return Generic_Renamings.Table (E).Next_In_Htable;
   end Next_Assoc;

   ----------------
   -- Get_Gen_Id --
   ----------------

   function Get_Gen_Id (E : Assoc_Ptr) return Name_Id is
   begin
      return Chars (Generic_Renamings.Table (E).Gen_Id);
   end Get_Gen_Id;

   ----------
   -- Hash --
   ----------

   function Hash (F : Name_Id) return Htable_Range is
   begin
      return Htable_Range (F mod Htable_Size);
   end Hash;

   ---------------------------
   -- Abandon_Instantiation --
   ---------------------------

   procedure Abandon_Instantiation (N : Node_Id) is
   begin
      Error_Msg_N ("instantiation abandoned!", N);
      raise Instantiation_Error;
   end Abandon_Instantiation;

   -----------------
   -- End_Generic --
   -----------------

   procedure End_Generic is
   begin
      --  ??? I am sure more things could be factored out in this
      --  routine. Should probably be done at a later stage.

      Inside_A_Generic := Generic_Flags.Table (Generic_Flags.Last);
      Generic_Flags.Decrement_Last;

      Expander_Mode_Restore;
   end End_Generic;

   -------------------
   -- Start_Generic --
   -------------------

   procedure Start_Generic is
   begin
      --  ??? I am sure more things could be factored out in this
      --  routine. Should probably be done at a later stage.

      Generic_Flags.Increment_Last;
      Generic_Flags.Table (Generic_Flags.Last) := Inside_A_Generic;
      Inside_A_Generic := True;

      Expander_Mode_Save_And_Set (False);
   end Start_Generic;

   ------------------------------------------
   -- Analyze_Generic_Package_Declaration  --
   ------------------------------------------

   procedure Analyze_Generic_Package_Declaration (N : Node_Id) is
      Id          : Entity_Id;
      New_N       : Node_Id;
      Save_Parent : Node_Id;

   begin
      --  Create copy of generic unit, and save for instantiation.
      --  If the unit is a child unit, do not copy the specifications
      --  for the parent, which are not part of the generic tree.

      Save_Parent := Parent_Spec (N);
      Set_Parent_Spec (N, Empty);

      New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
      Set_Parent_Spec (New_N, Save_Parent);
      Rewrite_Substitute_Tree (N, New_N);
      Id := Defining_Entity (N);

      --  Expansion is not applied to generic units.

      Start_Generic;

      Enter_Name (Id);
      Set_Ekind (Id, E_Generic_Package);
      Set_Etype (Id, Standard_Void_Type);
      New_Scope (Id);
      Enter_Generic_Scope (Id);
      Set_Inner_Instances (Id, New_Elmt_List);

      Set_Categorization_From_Pragmas (N);

      --  Entities declared in Pure unit should be set Is_Pure
      --  Since 'Partition_Id cannot be applied to such an entity

      Set_Is_Pure (Id, Is_Pure (Current_Scope));

      Analyze_Generic_Formal_Part (N);

      --  After processing the generic formals, analysis proceeds
      --  as for a non-generic package.

      Analyze (Specification (N));

      Validate_Categorization_Dependency (N, Id);

      Save_Global_References (Original_Node (N));

      End_Generic;

      End_Package_Scope (Id);
      Exit_Generic_Scope (Id);

      if Nkind (Parent (N)) /= N_Compilation_Unit then
         Move_Freeze_Nodes (Id, N, Visible_Declarations (Specification (N)));
         Move_Freeze_Nodes (Id, N, Private_Declarations (Specification (N)));
      end if;

   end Analyze_Generic_Package_Declaration;

   --------------------------------------------
   -- Analyze_Generic_Subprogram_Declaration --
   --------------------------------------------

   procedure Analyze_Generic_Subprogram_Declaration (N : Node_Id) is
      Spec        : Node_Id;
      Id          : Entity_Id;
      Formals     : List_Id;
      New_N       : Node_Id;
      Save_Parent : Node_Id;

   begin
      --  Create copy of generic unit,and save for instantiation.
      --  If the unit is a child unit, do not copy the specifications
      --  for the parent, which are not part of the generic tree.

      Save_Parent := Parent_Spec (N);
      Set_Parent_Spec (N, Empty);

      New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
      Set_Parent_Spec (New_N, Save_Parent);
      Rewrite_Substitute_Tree (N, New_N);

      Spec := Specification (N);
      Id := Defining_Entity (Spec);

      if Nkind (Id) = N_Defining_Operator_Symbol then
         Error_Msg_N
           ("operator symbol not allowed for generic subprogram", Id);
      end if;

      Start_Generic;

      Enter_Name (Id);

      New_Scope (Id);
      Set_Inner_Instances (Id, New_Elmt_List);

      --  Entities declared in Pure unit should be set Is_Pure
      --  Since 'Partition_Id cannot be applied to such an entity

      Set_Is_Pure (Id, Is_Pure (Current_Scope));

      Analyze_Generic_Formal_Part (N);

      Formals := Parameter_Specifications (Spec);

      if Present (Formals) then
         Process_Formals (Id, Formals, Spec);
      end if;

      if Nkind (Spec) = N_Function_Specification then
         Set_Ekind (Id, E_Generic_Function);
         Find_Type (Subtype_Mark (Spec));
         Set_Etype (Id, Entity (Subtype_Mark (Spec)));
      else
         Set_Ekind (Id, E_Generic_Procedure);
         Set_Etype (Id, Standard_Void_Type);
      end if;

      Set_Categorization_From_Pragmas (N);
      Validate_Categorization_Dependency (N, Id);

      Save_Global_References (Original_Node (N));

      End_Generic;
      End_Scope;

   end Analyze_Generic_Subprogram_Declaration;

   ---------------------------------
   -- Analyze_Generic_Formal_Part --
   ---------------------------------

   procedure Analyze_Generic_Formal_Part (N : Node_Id) is
      Gen_Parm_Decl : Node_Id;

   begin
      --  The generic formals are processed in the scope of the generic
      --  unit, where they are immediately visible. The scope is installed
      --  by the caller.

      Gen_Parm_Decl := First (Generic_Formal_Declarations (N));

      while Present (Gen_Parm_Decl) loop
         Analyze (Gen_Parm_Decl);
         Gen_Parm_Decl := Next (Gen_Parm_Decl);
      end loop;
   end Analyze_Generic_Formal_Part;

   ---------------------
   -- Is_In_Main_Unit --
   ---------------------

   function Is_In_Main_Unit (N : Node_Id) return Boolean is
      Unum : constant Unit_Number_Type := Get_Sloc_Unit_Number (Sloc (N));

      Current_Unit : Node_Id;

   begin
      if Unum = Main_Unit then
         return True;

      --  If the current unit is a subunit then it is either the main unit
      --  or is being compiled as part of the main unit.

      elsif Nkind (N) = N_Compilation_Unit then
         return Nkind (Unit (N)) = N_Subunit;
      end if;

      Current_Unit := Parent (N);
      while Present (Current_Unit)
        and then Nkind (Current_Unit) /= N_Compilation_Unit
      loop
         Current_Unit := Parent (Current_Unit);
      end loop;

      --  The instantiation node is in the main unit, or else the current
      --  node (perhaps as the result of nested instantiations) is in the
      --  main unit, or in the declaration of the main unit, which in this
      --  last case must be a body.

      return Unum = Main_Unit
        or else Current_Unit = Cunit (Main_Unit)
        or else Current_Unit = Library_Unit (Cunit (Main_Unit))
        or else (Present (Library_Unit (Current_Unit))
                  and then Is_In_Main_Unit (Library_Unit (Current_Unit)));
   end Is_In_Main_Unit;

   -----------------------------------
   -- Analyze_Package_Instantiation --
   -----------------------------------

   procedure Analyze_Package_Instantiation (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Actuals : constant List_Id    := Generic_Associations (N);
      Gen_Id  : constant Node_Id    := Name (N);

      Act_Decl      : Node_Id;
      Act_Decl_Name : Node_Id;
      Act_Decl_Id   : Entity_Id;
      Act_Spec      : Node_Id;
      Act_Tree      : Node_Id;

      Gen_Decl : Node_Id;
      Gen_Unit : Entity_Id;

      Is_Actual_Pack   : Boolean := Is_Internal (Defining_Entity (N));
      Parent_Installed : Boolean := False;
      Renaming_List    : List_Id;
      Unit_Renaming    : Node_Id;
      Needs_Body       : Boolean;

      Save_Instantiated_Parent : Assoc;
      Save_Exchanged_Views     : Elist_Id;

   begin
      --  Very first thing: apply the special kludge for Text_IO processing
      --  in case we are instantiating one of the children of [Wide_]Text_IO.

      Text_IO_Kludge (Name (N));

      --  Make node global for error reporting.

      Instantiation_Node := N;

      if Nkind (N) = N_Package_Instantiation then
         Act_Decl_Id := New_Copy (Defining_Entity (N));

         if Nkind (Defining_Unit_Name (N)) = N_Defining_Program_Unit_Name then
            Act_Decl_Name := Make_Defining_Program_Unit_Name (Loc,
               Name => New_Copy_Tree (Name (Defining_Unit_Name (N))),
               Defining_Identifier => Act_Decl_Id);
         else
            Act_Decl_Name :=  Act_Decl_Id;
         end if;

      else
         --  Instantiation of a formal package.

         Act_Decl_Id   := Defining_Identifier (N);
         Act_Decl_Name := Act_Decl_Id;
      end if;

      Check_Generic_Child_Unit (Gen_Id, Parent_Installed);
      Gen_Unit := Entity (Gen_Id);

      --  Verify that it is the name of a generic package

      if Etype (Gen_Unit) = Any_Type then
         return;

      elsif Ekind (Gen_Unit) /= E_Generic_Package then
         Error_Msg_N
            ("expect name of generic package in instantiation", Gen_Id);
         return;
      end if;

      --  If renaming, indicate this is an instantiation of renamed unit

      if Present (Renamed_Object (Gen_Unit))
        and then Ekind (Renamed_Object (Gen_Unit)) = E_Generic_Package
      then
         Gen_Unit := Renamed_Object (Gen_Unit);
         Set_Entity (Gen_Id, Gen_Unit);
      end if;

      --  Verify that there are no circular instantiations. We check whether
      --  the unit contains an instance of the current scope or the enclosing
      --  scope (in case one of the instances appears in a subunit). Longer
      --  circularities involving subunits are too pathological to consider.

      if In_Open_Scopes (Gen_Unit) then
         Error_Msg_NE ("instantiation of & within itself", N, Gen_Unit);

      elsif Contains_Instance_Of (Gen_Unit, Current_Scope, Gen_Id) then
         Error_Msg_Node_2 := Current_Scope;
         Error_Msg_NE
           ("circular Instantiation: & instantiated in &!", N, Gen_Unit);

      elsif Current_Scope /= Standard_Standard
        and then Contains_Instance_Of (Gen_Unit, Scope (Current_Scope), Gen_Id)
      then
         Error_Msg_Node_2 := Scope (Current_Scope);
         Error_Msg_NE
           ("circular Instantiation: & instantiated in &!", N, Gen_Unit);

      else
         Gen_Decl := Get_Declaration_Node (Gen_Unit);

         --  Initialize renamings map, for error checking, and the list
         --  that holds private entities whose views have changed between
         --  generic definition and instantiation. If this is the instance
         --  created to validate an actual package, the instantiation
         --  environment is that of the enclosing instance.

         Save_Exchanged_Views := Exchanged_Views;
         Exchanged_Views := New_Elmt_List;
         Generic_Renamings.Set_Last (0);
         Generic_Renamings_Htable.Reset;

         Create_Instantiation_Source (N, Gen_Unit, S_Adjustment);

         --  Copy original generic tree, to produce text for instantiation.

         Save_Instantiated_Parent := Current_Instantiated_Parent;
         Current_Instantiated_Parent := (Gen_Unit, Act_Decl_Id, Assoc_Null);

         Act_Tree :=
           Copy_Generic_Node
             (Original_Node (Gen_Decl), Empty, Instantiating => True);

         Act_Spec := Specification (Act_Tree);

         --  If this is the instance created to validate an actual package,
         --  only the formals matter, do not examine the package spec itself.

         if Is_Actual_Pack then
            Set_Visible_Declarations (Act_Spec, New_List);
            Set_Private_Declarations (Act_Spec, New_List);
         end if;

         Renaming_List :=
           Analyze_Associations
             (Generic_Formal_Declarations (Act_Tree),
              Actuals,
              Generic_Formal_Declarations (Gen_Decl));

         Set_Defining_Unit_Name (Act_Spec, Act_Decl_Name);
         Set_Is_Generic_Instance (Act_Decl_Id);

         Set_Generic_Parent (Act_Spec, Gen_Unit);

         --  References to the generic in its own declaration or its body
         --  are references to the instance. Add a renaming declaration for
         --  the generic unit itself. This declaration, as well as the renaming
         --  declarations for the generic formals, must remain private to the
         --  unit: the formals, because this is the language semantics, and
         --  the unit because its use is an artifact of the implementation.

         Unit_Renaming :=
           Make_Package_Renaming_Declaration (Loc,
             Defining_Unit_Name =>
               Make_Defining_Identifier (Loc, Chars (Gen_Unit)),
             Name => New_Reference_To (Act_Decl_Id, Loc));

         Append (Unit_Renaming, Renaming_List);

         --  The renaming declarations are the first local declarations of
         --  the new unit.

         if Is_Non_Empty_List (Visible_Declarations (Act_Spec)) then
            Insert_List_Before
              (First (Visible_Declarations (Act_Spec)), Renaming_List);
         else
            Set_Visible_Declarations (Act_Spec, Renaming_List);
         end if;

         Act_Decl :=
           Make_Package_Declaration (Loc,
             Specification => Act_Spec);

         --  Save the instantiation node, for subsequent instantiation
         --  of the body, if there is one and we are generating code for
         --  the current unit. Mark the unit as having a body, to avoid
         --  a premature error message.

         Needs_Body :=
           (Unit_Requires_Body (Gen_Unit)
               or else (Scope (Gen_Unit) /= Standard_Standard
                          and then not Is_Child_Unit (Gen_Unit)
                          and then Unit_Requires_Body (Scope (Gen_Unit)))
               or else Present (Corresponding_Body (Gen_Decl)))
             and then Is_In_Main_Unit (N)
             and then not Is_Actual_Pack
             and then (Expander_Active or Xref_Analyze);

         if Needs_Body then

            --  We detect simple cases of ABE here. More complex cases, eg.
            --  with late bodies that are not required, will need more complex
            --  processing.

            if Unit_Requires_Body (Gen_Unit) then
               Check_Premature_Instantiation (N, Gen_Decl);
            end if;

            --  Here is a defence against a ludicrous number of instantiations
            --  caused by a circular set of instantiation attempts.

            if Pending_Instantiations.Last >
                 Hostparm.Max_Instantiations
            then
               Error_Msg_N ("too many instantiations", N);
               raise Unrecoverable_Error;
            end if;

            --  If OK, then make entry in table.

            --  Indicate that the enclosing scopes contain an instantiation,
            --  and that cleanup actions should be delayed until after the
            --  instance body is expanded.

            declare
               Enclosing_Master : Entity_Id := Current_Scope;

            begin
               while Enclosing_Master /= Standard_Standard
                 and then Ekind (Enclosing_Master) = E_Package
               loop
                  Enclosing_Master := Scope (Enclosing_Master);
               end loop;

               Set_Has_Pending_Instantiations (Enclosing_Master);
            end;

            Pending_Instantiations.Increment_Last;
            Pending_Instantiations.Table (Pending_Instantiations.Last) :=
                                                                (N, Act_Decl);
         end if;

         Set_Categorization_From_Pragmas (Act_Decl);

         if Parent_Installed then
            Hide_Current_Scope;
         end if;

         if Nkind (Parent (N)) /= N_Compilation_Unit then
            Mark_Rewrite_Insertion (Act_Decl);
            Insert_Before (N, Act_Decl);
            Analyze (Act_Decl);

         else
            --  Place declaration on current node so context is complete
            --  for analysis (including nested instantiations).

            Set_Unit (Parent (N), Act_Decl);
            Set_Parent_Spec (Act_Decl, Parent_Spec (N));
            Analyze (Act_Decl);
            Set_Unit (Parent (N), N);
            Set_Body_Required (Parent (N), False);
         end if;

         Current_Instantiated_Parent := Save_Instantiated_Parent;
         Set_First_Private_Entity (Defining_Unit_Name (Unit_Renaming),
           First_Private_Entity (Act_Decl_Id));

         if not Needs_Body
           and then Nkind (Parent (N)) = N_Compilation_Unit
         then

            --  If this is the main unit, note that the main entity is now
            --  the one created for the instance, because all scope info
            --  is attached to it.

            if Main_Unit_Entity = Defining_Entity (N) then
               Main_Unit_Entity := Act_Decl_Id;
            end if;

            Rewrite_Substitute_Tree (N, Act_Decl);
         end if;

         if Present (Corresponding_Body (Gen_Decl))
           or else Unit_Requires_Body (Gen_Unit)
         then
            Set_Has_Completion (Act_Decl_Id);
         end if;

         Check_Formal_Packages (Act_Decl_Id);

         Restore_Private_Views (Act_Decl_Id);
         Exchanged_Views := Save_Exchanged_Views;
         Inherit_Context (Gen_Decl, N);

         if Parent_Installed then
            Remove_Parent;
         end if;

      end if;

      Validate_Categorization_Dependency (N, Act_Decl_Id);

   exception
      when Instantiation_Error =>
         null;

   end Analyze_Package_Instantiation;

   ------------------------
   -- Hide_Current_Scope --
   ------------------------

   procedure Hide_Current_Scope is
      E : Entity_Id;

   begin
      E := First_Entity (Current_Scope);

      while Present (E) loop
         Set_Is_Immediately_Visible (E, False);
         E := Next_Entity (E);
      end loop;
   end Hide_Current_Scope;

   ------------------------------
   -- Instantiate_Package_Body --
   ------------------------------

   procedure Instantiate_Package_Body
     (Inst_Node : Node_Id;
      Act_Decl  : Node_Id)
   is
      Loc         : constant Source_Ptr := Sloc (Inst_Node);
      Gen_Id      : constant Node_Id    := Name (Inst_Node);
      Gen_Unit    : constant Entity_Id  := Entity (Name (Inst_Node));
      Gen_Decl    : constant Node_Id    := Get_Declaration_Node (Gen_Unit);
      Act_Spec    : constant Node_Id    := Specification (Act_Decl);
      Act_Decl_Id : constant Entity_Id  := Defining_Entity (Act_Spec);

      Act_Body_Name : Node_Id;
      Gen_Body      : Node_Id;
      Gen_Body_Id   : Node_Id;
      Act_Body      : Node_Id;
      Act_Body_Id   : Entity_Id;

      Save_Instantiated_Parent : Assoc;
      Save_Exchanged_Views     : Elist_Id;
      Parent_Installed         : Boolean := False;

   begin
      Instantiation_Node := Inst_Node;
      Gen_Body_Id := Corresponding_Body (Gen_Decl);

      if No (Gen_Body_Id) then
         Load_Parent_Of_Generic (Inst_Node, Specification (Gen_Decl));
         Gen_Body_Id := Corresponding_Body (Gen_Decl);
      end if;

      if Present (Gen_Body_Id) then
         Save_Instantiated_Parent := Current_Instantiated_Parent;
         Current_Instantiated_Parent := (Gen_Unit, Act_Decl_Id, Assoc_Null);
         Save_Exchanged_Views := Exchanged_Views;
         Exchanged_Views := New_Elmt_List;
         Gen_Body := Get_Declaration_Node (Gen_Body_Id);
         Create_Instantiation_Source
          (Inst_Node, Gen_Body_Id, S_Adjustment);

         Act_Body :=
           Copy_Generic_Node
             (Original_Node (Gen_Body), Empty, Instantiating => True);

         --  Build new name (possible qualified) for body declaration.

         Act_Body_Id := New_Copy (Act_Decl_Id);

         if Nkind (Defining_Unit_Name (Act_Spec))
           = N_Defining_Program_Unit_Name
         then
            Act_Body_Name :=
              Make_Defining_Program_Unit_Name (Loc,
                Name => New_Copy_Tree (Name (Defining_Unit_Name (Act_Spec))),
                Defining_Identifier => Act_Body_Id);
         else
            Act_Body_Name :=  Act_Body_Id;
         end if;

         Set_Defining_Unit_Name (Act_Body, Act_Body_Name);

         Set_Corresponding_Spec (Act_Body, Act_Decl_Id);
         Check_Generic_Actuals (Act_Decl_Id, False);

         --  If it is a child unit, make the parent instance (which is an
         --  instance of the parent of the generic) visible. The parent
         --  instance is the prefix of the name of the generic unit.

         if Ekind (Scope (Gen_Unit)) = E_Generic_Package
           and then Nkind (Gen_Id) = N_Expanded_Name
         then
            Install_Parent (Entity (Prefix (Gen_Id)), In_Body => True);
            Parent_Installed := True;

         elsif Is_Child_Unit (Gen_Unit) then
            Install_Private_Declarations (Scope (Gen_Unit));
         end if;

         --  If the instantiation is a library unit, and this is the main
         --  unit, then build the resulting compilation unit nodes for the
         --  instance. If this is a compilation unit but it is not the main
         --  unit, then it is the body of a unit in the context, that is being
         --  compiled because it is encloses some inlined unit or another
         --  generic unit being instantiated. In that case, this body is not
         --  part of the current compilation, and is not attached to the tree,
         --  but its parent must be set for analysis.

         if Nkind (Parent (Inst_Node)) = N_Compilation_Unit then

            if Parent (Inst_Node) = Cunit (Main_Unit) then
               Build_Instance_Compilation_Unit_Nodes
                 (Inst_Node, Act_Body, Act_Decl);
               Analyze (Inst_Node);

               --  If the instance is a child unit itself, then set the
               --  scope of the expanded body to be the parent of the
               --  instantiation (ensuring that the fully qualified name
               --  will be generated for the elaboration subprogram).

               if Nkind (Defining_Unit_Name (Act_Spec)) =
                                              N_Defining_Program_Unit_Name
               then
                  Set_Scope
                    (Defining_Entity (Inst_Node), Scope (Act_Decl_Id));
               end if;

            else
               Set_Parent (Act_Body, Parent (Inst_Node));
               Analyze (Act_Body);
            end if;

         --  If this is an early instantiation, i.e. appears textually
         --  before the corresponding body and must be elaborated first,
         --  indicate that the body instance is to be delayed.

         else
            Install_Body (Act_Body, Inst_Node, Gen_Body, Gen_Decl);
            Analyze (Act_Body);
         end if;

         Inherit_Context (Gen_Body, Inst_Node);
         Current_Instantiated_Parent := Save_Instantiated_Parent;
         Restore_Private_Views (Act_Decl_Id);
         Exchanged_Views := Save_Exchanged_Views;

      --  If we have no body, and the unit requires a body, then complain.
      --  This complaint is suppressed if we have detected other errors
      --  (since a common reason for missing the body is that it had errors).

      elsif Unit_Requires_Body (Gen_Unit) then
         if Errors_Detected = 0 then
            Error_Msg_NE
              ("cannot find body of generic package &", Inst_Node, Gen_Unit);
         end if;

      --  Case of package that does not need a body

      else
         --  If the instantiation of the declaration is a library unit,
         --  rewrite the original package instantiation as a package
         --  declaration in the compilation unit node.

         if Nkind (Parent (Inst_Node)) = N_Compilation_Unit then
            Set_Parent_Spec (Act_Decl, Parent_Spec (Inst_Node));
            Rewrite_Substitute_Tree (Inst_Node, Act_Decl);

         --  If the instantiation is not a library unit, then append the
         --  declaration to the list of implicitly generated entities.
         --  unless it is already a list member which means that it was
         --  already processed

         elsif not Is_List_Member (Act_Decl) then
            Mark_Rewrite_Insertion (Act_Decl);
            Insert_Before (Inst_Node, Act_Decl);
         end if;
      end if;

      if Parent_Installed then
         Remove_Parent (In_Body => True);
      end if;
   end Instantiate_Package_Body;

   ---------------------------------
   -- Instantiate_Subprogram_Body --
   ---------------------------------

   procedure Instantiate_Subprogram_Body
     (Inst_Node : Node_Id;
      Act_Decl  : Node_Id)
   is
      Loc           : constant Source_Ptr := Sloc (Inst_Node);
      Decls         : List_Id;
      Gen_Id        : constant Node_Id   := Name (Inst_Node);
      Gen_Unit      : constant Entity_Id := Entity (Name (Inst_Node));
      Gen_Decl      : constant Node_Id   := Get_Declaration_Node (Gen_Unit);
      Anon_Id       : constant Entity_Id :=
                        Defining_Unit_Name (Specification (Act_Decl));
      Gen_Body      : Node_Id;
      Gen_Body_Id   : Node_Id;
      Act_Body      : Node_Id;
      Act_Body_Id   : Entity_Id;
      Pack_Id       : Entity_Id := Defining_Unit_Name (Parent (Act_Decl));
      Pack_Body     : Node_Id;
      Prev_Formal   : Entity_Id;
      Unit_Renaming : Node_Id;

      Save_Instantiated_Parent : Assoc;
      Save_Exchanged_Views     : Elist_Id;
      Parent_Installed         : Boolean := False;

   begin
      Instantiation_Node := Inst_Node;
      Gen_Body_Id := Corresponding_Body (Gen_Decl);

      if No (Gen_Body_Id) then
         Load_Parent_Of_Generic (Inst_Node, Specification (Gen_Decl));
         Gen_Body_Id := Corresponding_Body (Gen_Decl);
      end if;

      if Present (Gen_Body_Id) then
         Gen_Body := Get_Declaration_Node (Gen_Body_Id);

         if Nkind (Gen_Body) = N_Subprogram_Body_Stub then

            --  Either body is not present, or context is non-expanding, as
            --  when compiling a subunit. Mark the instance as completed.

            Set_Has_Completion (Anon_Id);
            return;
         end if;

         Save_Exchanged_Views := Exchanged_Views;
         Exchanged_Views := New_Elmt_List;
         Save_Instantiated_Parent := Current_Instantiated_Parent;
         Current_Instantiated_Parent := (Gen_Unit, Anon_Id, Assoc_Null);
         Create_Instantiation_Source (Inst_Node, Gen_Body_Id, S_Adjustment);

         Act_Body :=
           Copy_Generic_Node
             (Original_Node (Gen_Body), Empty, Instantiating => True);
         Act_Body_Id := Defining_Entity (Act_Body);
         Set_Chars (Act_Body_Id, Chars (Anon_Id));
         Set_Corresponding_Spec (Act_Body, Anon_Id);
         Set_Has_Completion (Anon_Id);
         Check_Generic_Actuals (Pack_Id, False);

         --  If it is a child unit, make the parent instance (which is an
         --  instance of the parent of the generic) visible. The parent
         --  instance is the prefix of the name of the generic unit.

         if Ekind (Scope (Gen_Unit)) = E_Generic_Package
           and then Nkind (Gen_Id) = N_Expanded_Name
         then
            Install_Parent (Entity (Prefix (Gen_Id)), In_Body => True);
            Parent_Installed := True;
         end if;

         --  Inside its body, a reference to the generic unit is a reference
         --  to the instance. The corresponding renaming is the first
         --  declaration in the body.

         Unit_Renaming :=
           Make_Subprogram_Renaming_Declaration (Loc,
             Specification =>
               Copy_Generic_Node (
                 Specification (Original_Node (Gen_Body)),
                 Empty,
                 Instantiating => True),
             Name => New_Occurrence_Of (Anon_Id, Loc));

         --  If there is a formal subprogram with the same name as the
         --  unit itself, do not add this renaming declaration. This is
         --  a temporary fix for one ACVC test. ???

         Prev_Formal := First_Entity (Pack_Id);

         while Present (Prev_Formal) loop
            if Chars (Prev_Formal) = Chars (Gen_Unit)
              and then Is_Overloadable (Prev_Formal)
            then
               exit;
            end if;

            Prev_Formal := Next_Entity (Prev_Formal);
         end loop;

         if Present (Prev_Formal) then
            Decls :=  New_List (Act_Body);
         else
            Decls :=  New_List (Unit_Renaming, Act_Body);
         end if;

         --  The subprogram body is placed in the body of a dummy package
         --  body, whose spec contains the subprogram declaration as well
         --  as the renaming declarations for the generic parameters.

         Pack_Body := Make_Package_Body (Loc,
           Defining_Unit_Name => New_Copy (Pack_Id),
           Declarations       => Decls);

         Set_Corresponding_Spec (Pack_Body, Pack_Id);

         --  If the instantiation is a library unit, then build resulting
         --  compilation unit nodes for the instance. The declaration of
         --  the enclosing package is the grandparent of the subprogram
         --  declaration. First replace the instantiation node as the unit
         --  of the corresponding compilation.

         if Nkind (Parent (Inst_Node)) = N_Compilation_Unit then
            Set_Unit (Parent (Inst_Node), Inst_Node);
            Build_Instance_Compilation_Unit_Nodes
              (Inst_Node, Pack_Body, Parent (Parent (Act_Decl)));
            Analyze (Inst_Node);

         else
            Insert_Before (Inst_Node, Pack_Body);
            Mark_Rewrite_Insertion (Pack_Body);
            Analyze (Pack_Body);

            --  Instance bodies are delayed  so that Gigi can elaborate
            --  them after all other declarations. Create freeze node
            --  and insert it at end of current list of declarations.

            declare
               F_Node : Node_Id;

            begin
               Ensure_Freeze_Node (Pack_Id);
               F_Node := Freeze_Node (Pack_Id);
               Insert_After (Last (List_Containing (Inst_Node)), F_Node);
            end;
         end if;

         Inherit_Context (Gen_Body, Inst_Node);

         Restore_Private_Views (Pack_Id, False);
         Exchanged_Views := Save_Exchanged_Views;

         if Parent_Installed then
            Remove_Parent (In_Body => True);
         end if;

      --  Body not found. Error was emitted already.

      else
         null;
      end if;

   end Instantiate_Subprogram_Body;

   ------------------
   -- Install_Body --
   ------------------

   procedure Install_Body
     (Act_Body : Node_Id;
      N        : Node_Id;
      Gen_Body : Node_Id;
      Gen_Decl : Node_Id)
   is
      Act_Id    : Entity_Id := Corresponding_Spec (Act_Body);
      Act_Unit  : constant Node_Id :=
                    Unit (Cunit (Get_Sloc_Unit_Number (Sloc (N))));
      F_Node    : Node_Id;
      Gen_Id    : Entity_Id := Corresponding_Spec (Gen_Body);
      Gen_Unit  : constant Node_Id :=
                    Unit (Cunit (Get_Sloc_Unit_Number (Sloc (Gen_Decl))));
      Orig_Body : Node_Id := Gen_Body;
      Body_Unit : Node_Id;

      Must_Delay : Boolean;

      function Enclosing_Subp (Id : Entity_Id) return Entity_Id;
      --  Find subprogram (if any) that encloses instance and/or generic body.

      function True_Sloc (N : Node_Id) return Source_Ptr;
      --  If the instance is nested inside a generic unit, the Sloc of the
      --  instance indicates the place of the original definition, not the
      --  point of the current enclosing instance. Pending a better usage of
      --  Slocs to indicate instantiation places, we determine the place of
      --  origin of a node by finding the maximum sloc of any ancestor node.

      function Enclosing_Subp (Id : Entity_Id) return Entity_Id is
         Scop : Entity_Id := Scope (Id);

      begin
         while Scop /= Standard_Standard
           and then not Is_Overloadable (Scop)
         loop
            Scop := Scope (Scop);
         end loop;

         return Scop;
      end Enclosing_Subp;

      function True_Sloc (N : Node_Id) return Source_Ptr is
         Res : Source_Ptr;
         N1  : Node_Id;

      begin
         Res := Sloc (N);
         N1 := N;
         while Present (N1) and then N1 /= Act_Unit loop
            if Sloc (N1) > Res then
               Res := Sloc (N1);
            end if;

            N1 := Parent (N1);
         end loop;

         return Res;
      end True_Sloc;

   --  Start of processing for Install_Body

   begin
      --  if the body is a subunit, the freeze point is the corresponding
      --  stub in the current compilation, not the subunit itself.

      if Nkind (Parent (Gen_Body)) = N_Subunit then
         Orig_Body :=  Corresponding_Stub (Parent (Gen_Body));
      else
         Orig_Body := Gen_Body;
      end if;

      Body_Unit := Unit (Cunit (Get_Sloc_Unit_Number (Sloc (Orig_Body))));

      --  If the instantiation and the generic definition appear in the
      --  same package declaration, this is an early instantiation.
      --  If they appear in the same declarative part, it is an early
      --  instantiation only if the generic body appears textually later,
      --  and the generic body is also in the main unit.

      --  If instance is nested within a subprogram, and the generic body is
      --  not, the instance is delayed because the enclosing body is. If
      --  instance and body are within the same scope, or the same sub-
      --  program body, indicate explicitly that the instance is delayed.

      Must_Delay :=
         (Gen_Unit = Act_Unit
           and then ((Nkind (Gen_Unit) = N_Package_Declaration)
                    or else Nkind (Gen_Unit) = N_Generic_Package_Declaration
                    or else (Gen_Unit = Body_Unit
                               and then True_Sloc (N) < Sloc (Orig_Body)))
           and then Is_In_Main_Unit (Gen_Unit)

           and then (Scope (Act_Id) = Scope (Gen_Id)
           or else Enclosing_Subp (Act_Id) = Enclosing_Subp (Gen_Id)));

      --  The instance body must be delayed. Place the freeze node either
      --  at the end of the current declarative part, or at the place of
      --  the generic body, in  the case of an early instantiation.

      Ensure_Freeze_Node (Act_Id);
      F_Node := Freeze_Node (Act_Id);

      if Must_Delay then
         Insert_After (Orig_Body, F_Node);
      else
         Insert_After (Last (List_Containing (N)), F_Node);
      end if;

      Insert_Before (N, Act_Body);
      Mark_Rewrite_Insertion (Act_Body);
   end Install_Body;

   --------------------
   -- Install_Parent --
   --------------------

   procedure Install_Parent (P : Entity_Id; In_Body : Boolean := False) is
      S : Entity_Id := Current_Scope;
      E         : Entity_Id;
      Spec      : Node_Id;
      Inst_Par  : Entity_Id;
      First_Par : Entity_Id;
      Inst_Node : Node_Id;

      procedure Install_Spec (Par : Entity_Id);
      --  The child unit is within the declarative part of the parent, so
      --  the declarations within the parent are immediately visible.

      procedure Install_Spec (Par : Entity_Id) is
         Spec : constant Node_Id
                  := Specification (Get_Declaration_Node (Par));

      begin
         New_Scope (Par);
         Set_Is_Immediately_Visible   (Par);
         Install_Visible_Declarations (Par);
         Install_Private_Declarations (Par);
         Set_Use (Visible_Declarations (Spec));
         Set_Use (Private_Declarations (Spec));
      end Install_Spec;

   begin
      --  We need to install the parent instance to compile the instantiation
      --  of the child, but the child instance must appear in the current
      --  scope. Given that we cannot place the parent above the current
      --  scope in the scope stack, we duplicate the current scope and unstack
      --  both after the instantiation is complete.

      --  If the parent is itself the instantiation of a child unit, we must
      --  also stack the instantiation of its parent, and so on. Each such
      --  ancestor is the prefix of the name in a prior instantiation.

      --  If this is a nested instance, the parent unit itself resolves to
      --  a renaming of the parent instance, whose declaration we need.

      Inst_Par := P;

      if Present (Renamed_Entity (Inst_Par)) then
         Inst_Par := Renamed_Entity (Inst_Par);
      end if;

      First_Par := Inst_Par;

      while Is_Child_Unit
        (Generic_Parent (Specification (Get_Declaration_Node (Inst_Par))))
      loop
         --  Load grandparent instance as well.

         Inst_Node := Get_Package_Instantiation_Node (Inst_Par);

         if Nkind (Name (Inst_Node)) = N_Expanded_Name then
            Inst_Par := Entity (Prefix (Name (Inst_Node)));

            if Present (Renamed_Entity (Inst_Par)) then
               Inst_Par := Renamed_Entity (Inst_Par);
            end if;

            if Present (Generic_Parent
              (Specification (Get_Declaration_Node (Inst_Par))))
            then
               Install_Spec (Inst_Par);

            else
               --  Parent is not the name of an instantiation.

               exit;
            end if;

         else
            --  Previous error.

            exit;
         end if;
      end loop;

      Install_Spec (First_Par);

      if not In_Body then
         New_Scope (S);
      end if;
   end Install_Parent;

   -------------------
   -- Remove_Parent --
   -------------------

   procedure Remove_Parent (In_Body : Boolean := False) is
      S : constant Entity_Id := Current_Scope;
      E : Entity_Id;
      P : Entity_Id;

   begin
      --  After child instantiation is complete, remove from scope stack
      --  the extra copy of the current scope, and then remove parent
      --  instances.

      if not In_Body then
         Pop_Scope;
      end if;

      while Current_Scope /= S loop
         P := Current_Scope;
         End_Package_Scope (Current_Scope);

         if In_Open_Scopes (P) then
            E := First_Entity (P);

            while Present (E) loop
               Set_Is_Immediately_Visible (E, True);
               E := Next_Entity (E);
            end loop;
         end if;
      end loop;

      if not In_Body then
         E := First_Entity (S);

         while Present (E) loop
            Set_Is_Immediately_Visible (E, True);
            E := Next_Entity (E);
         end loop;
      end if;

   end Remove_Parent;

   -------------------------------------
   -- Analyze_Procedure_Instantiation --
   -------------------------------------

   procedure Analyze_Procedure_Instantiation (N : Node_Id) is
   begin
      Analyze_Subprogram_Instantiation (N, E_Procedure);
   end Analyze_Procedure_Instantiation;

   ------------------------------------
   -- Analyze_Function_Instantiation --
   ------------------------------------

   procedure Analyze_Function_Instantiation (N : Node_Id) is
   begin
      Analyze_Subprogram_Instantiation (N, E_Function);
   end Analyze_Function_Instantiation;

   --------------------------------------
   -- Analyze_Subprogram_Instantiation --
   --------------------------------------

   procedure Analyze_Subprogram_Instantiation
     (N : Node_Id;
      K : Entity_Kind)
   is
      Loc              : constant Source_Ptr := Sloc (N);
      Actuals          : constant List_Id    := Generic_Associations (N);
      Gen_Id           : constant Node_Id    := Name (N);

      Act_Decl_Id      : Entity_Id;
      Anon_Id          : Entity_Id := Make_Defining_Identifier
                                        (Loc, New_Internal_Name ('S'));
      Act_Decl         : Node_Id;
      Act_Spec         : Node_Id;
      Act_Tree         : Node_Id;

      Gen_Unit         : Entity_Id;
      Gen_Decl         : Node_Id;
      Pack_Id          : Entity_Id;
      Parent_Installed : Boolean := False;
      Renaming_List    : List_Id;
      Spec             : Node_Id;

      Save_Instantiated_Parent : Assoc;
      Save_Exchanged_Views : Elist_Id;

      procedure Analyze_Instance_And_Renamings;
      --  The instance must be analyzed in a context that includes the
      --  mappings of generic parameters into actuals. We create a package
      --  declaration for this purpose, and a subprogram with an internal
      --  name within the package. The subprogram instance is simply an
      --  alias for the internal subprogram, declared in the current scope.

      procedure Analyze_Instance_And_Renamings is
         Pack_Decl : Node_Id;

      begin
         if Nkind (Parent (N)) = N_Compilation_Unit then

            --  The container package has the same name as the instantiation,
            --  to insure that the binder calls the elaboration procedure
            --  with the right name.

            Pack_Id :=
              Make_Defining_Identifier (Loc,
                Chars => Chars (Defining_Entity (N)));

         else
            Pack_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
         end if;

         Pack_Decl := Make_Package_Declaration (Loc,
           Specification => Make_Package_Specification (Loc,
             Defining_Unit_Name  => Pack_Id,
             Visible_Declarations => Renaming_List));

         if Nkind (Parent (N)) /= N_Compilation_Unit then
            Mark_Rewrite_Insertion (Pack_Decl);
            Insert_Before (N, Pack_Decl);
            Set_Has_Completion (Pack_Id);

         else
            --  Place declaration on current node so context is complete
            --  for analysis (including nested instantiations), and for
            --  use in a context_clause (see Analyze_With_Clause).

            Set_Unit (Parent (N), Pack_Decl);
            Set_Parent_Spec (Pack_Decl, Parent_Spec (N));
         end if;

         Analyze (Pack_Decl);
         Check_Formal_Packages (Pack_Id);

         --  Body of the enclosing package is supplied when instantiating
         --  the subprogram body, after semantic  analysis is completed.

         if Nkind (Parent (N)) = N_Compilation_Unit then

            --  Remove package itself from visibility, so it does not
            --  conflict with subprogram.

            Set_Name_Entity_Id (Chars (Pack_Id), Homonym (Pack_Id));

            --  Set name and  scope of internal subprogram so that the
            --  proper external name will be generated.

            Set_Chars (Anon_Id, Chars (Defining_Entity (N)));
            Set_Scope (Anon_Id, Standard_Standard);
         end if;

         Set_Is_Generic_Instance (Anon_Id);
         Act_Decl_Id := New_Copy (Anon_Id);
         Set_Parent (Act_Decl_Id, Parent (Anon_Id));
         Set_Chars  (Act_Decl_Id, Chars (Defining_Entity (N)));

         New_Overloaded_Entity (Act_Decl_Id);

         --  The instance is not a freezing point for the new subprogram.

         Set_Is_Frozen (Act_Decl_Id, False);

         if Nkind (Defining_Entity (N)) = N_Defining_Operator_Symbol then
            Valid_Operator_Definition (Act_Decl_Id);
         end if;

         Set_Alias  (Act_Decl_Id, Anon_Id);
         Set_Parent (Act_Decl_Id, Parent (Anon_Id));
         Set_Has_Completion (Act_Decl_Id);

         if Nkind (Parent (N)) = N_Compilation_Unit then
            Set_Body_Required (Parent (N), False);
         end if;

      end Analyze_Instance_And_Renamings;

   --  Start of processing for Analyze_Subprogram_Instantiation

   begin
      --  Very first thing: apply the special kludge for Text_IO processing
      --  in case we are instantiating one of the children of [Wide_]Text_IO.
      --  Of course such an instantiation is bogus (these are packages, not
      --  subprograms), but we get a better error message if we do this.

      Text_IO_Kludge (Name (N));

      --  Make node global for error reporting.

      Instantiation_Node := N;

      Check_Generic_Child_Unit (Gen_Id, Parent_Installed);
      Gen_Unit := Entity (Gen_Id);

      if Nkind (Gen_Id) = N_Identifier
        and then Chars (Gen_Unit) = Chars (Defining_Entity (N))
      then
         Error_Msg_NE
           ("& is hidden within declaration of instance", N, Gen_Unit);
      end if;

      --  If renaming, indicate that this is instantiation of renamed unit

      if Present (Renamed_Object (Gen_Unit))
        and then (Ekind (Renamed_Object (Gen_Unit)) = E_Generic_Procedure
        or else  Ekind (Renamed_Object (Gen_Unit)) = E_Generic_Function)
      then
         Gen_Unit := Renamed_Object (Gen_Unit);
         Set_Entity (Gen_Id, Gen_Unit);
      end if;

      if Etype (Gen_Unit) = Any_Type then return; end if;

      --  Verify that it is a generic subprogram of the right kind, and that
      --  it does not lead to a circular instantiation.

      if Ekind (Gen_Unit) /= E_Generic_Procedure
        and then Ekind (Gen_Unit) /= E_Generic_Function
      then
         Error_Msg_N ("expect generic subprogram in instantiation",  Gen_Id);

      elsif In_Open_Scopes (Gen_Unit) then
         Error_Msg_NE ("instantiation of & within itself", N, Gen_Unit);

      elsif Contains_Instance_Of (Gen_Unit, Current_Scope, Gen_Id) then
         Error_Msg_Node_2 := Current_Scope;
         Error_Msg_NE
           ("circular Instantiation: & instantiated in &!", N, Gen_Unit);

      elsif K = E_Procedure
        and then Ekind (Gen_Unit) /= E_Generic_Procedure
      then
         Error_Msg_N
            ("expect name of generic procedure in instantiation", Gen_Id);

      elsif K = E_Function
        and then Ekind (Gen_Unit) /= E_Generic_Function
      then
         Error_Msg_N
            ("expect name of generic function in instantiation", Gen_Id);

      else
         Gen_Decl := Get_Declaration_Node (Gen_Unit);
         Spec     := Specification (Gen_Decl);

         --  Initialize renamings map, for error checking.

         Save_Exchanged_Views := Exchanged_Views;
         Exchanged_Views := New_Elmt_List;
         Generic_Renamings.Set_Last (0);
         Generic_Renamings_Htable.Reset;

         Save_Instantiated_Parent := Current_Instantiated_Parent;
         Current_Instantiated_Parent := (Gen_Unit, Act_Decl_Id, Assoc_Null);
         Create_Instantiation_Source (N, Gen_Unit, S_Adjustment);

         --  Copy original generic tree, to produce text for instantiation.

         Act_Tree :=
           Copy_Generic_Node
             (Original_Node (Gen_Decl), Empty, Instantiating => True);

         Act_Spec := Specification (Act_Tree);
         Renaming_List :=
           Analyze_Associations
             (Generic_Formal_Declarations (Act_Tree),
              Actuals,
              Generic_Formal_Declarations (Gen_Decl));

         Set_Defining_Unit_Name (Act_Spec, Anon_Id);
         Set_Generic_Parent (Act_Spec, Gen_Unit);
         Act_Decl :=
           Make_Subprogram_Declaration (Loc,
             Specification => Act_Spec);

         Set_Categorization_From_Pragmas (Act_Decl);

         if Parent_Installed then
            Hide_Current_Scope;
         end if;

         Append (Act_Decl, Renaming_List);
         Analyze_Instance_And_Renamings;

         --  If the generic is marked Import (Intrinsic), then so is the
         --  instance. This indicates that there is no body to instantiate.
         --  Other pragmas might also be inherited ???

         if Is_Intrinsic_Subprogram (Gen_Unit) then
            Set_Is_Intrinsic_Subprogram (Anon_Id);
            Set_Is_Intrinsic_Subprogram (Act_Decl_Id);

            if Chars (Gen_Unit) = Name_Unchecked_Conversion then
               Validate_Unchecked_Conversion (N, Act_Decl_Id);
            end if;
         end if;

         Current_Instantiated_Parent := Save_Instantiated_Parent;

         if not Is_Intrinsic_Subprogram (Act_Decl_Id) then
            Inherit_Context (Gen_Decl, N);
            Restore_Private_Views (Pack_Id, False);

            --  If the context requires a full instantiation, mark node for
            --  subsequent construction of the body.

            if Is_In_Main_Unit (N)
              and then (Expander_Active or Xref_Analyze)
            then
               Pending_Instantiations.Increment_Last;
               Pending_Instantiations.Table (Pending_Instantiations.Last) :=
                 (N, Act_Decl);
               Check_Premature_Instantiation (N, Gen_Decl);

               --  The wrapper package is always delayed, because it does
               --  not constitute a freeze point, but to insure that the
               --  freeze node is placed properly, it is created directly
               --  when instantiating the body (otherwise the freeze node
               --  might appear to early for nested instantiations).
            end if;
         end if;

         Exchanged_Views := Save_Exchanged_Views;

         --  Subject to change, pending on if other pragmas are inherited ???

         Validate_Categorization_Dependency (N, Act_Decl_Id);

         if Parent_Installed then
            Remove_Parent;
         end if;

         Generic_Renamings.Set_Last (0);
         Generic_Renamings_Htable.Reset;
      end if;

   exception
      when Instantiation_Error =>
         null;

   end Analyze_Subprogram_Instantiation;

   ----------------------------
   -- Load_Parent_Of_Generic --
   ----------------------------

   procedure Load_Parent_Of_Generic (N : Entity_Id; Spec : Node_Id) is
      Comp_Unit   : constant Node_Id :=
                      Cunit (Get_Sloc_Unit_Number (Sloc (Spec)));
      True_Parent : Node_Id;
      Inst_Node   : Node_Id;

   begin
      if Get_Sloc_Unit_Number (Sloc (N)) /=
         Get_Sloc_Unit_Number (Sloc (Spec))
        or else Nkind (Unit (Comp_Unit)) = N_Package_Declaration
      then
         --  Find body of parent of spec, and analyze it. A special case
         --  arises when the parent is an instantiation, that is to say when
         --  we are currently instantiating a nested generic. In that case,
         --  there is no separate file for the body of the enclosing instance.
         --  Instead, the enclosing body must be instantiated as if it were
         --  a pending instantiation, in order to produce the body for the
         --  nested generic we require now.

         True_Parent := Parent (Spec);
         Inst_Node   := Empty;

         while Present (True_Parent)
           and then Nkind (True_Parent) /= N_Compilation_Unit
         loop
            if Nkind (True_Parent) = N_Package_Declaration
              and then
                Nkind (Original_Node (True_Parent)) = N_Package_Instantiation
            then
               --  Parent is a compilation unit that is an instantiation.
               --  Instantiation node has been replaced with package decl.

               Inst_Node := Original_Node (True_Parent);
               exit;

            elsif Nkind (True_Parent) = N_Package_Declaration
              and then Present (Generic_Parent (Specification (True_Parent)))
            then
               --  Parent is an instantiation within another specification.
               --  Declaration for instance has been inserted before original
               --  instantiation node. A direct link would be preferable?

               Inst_Node := Next (True_Parent);

               while Nkind (Inst_Node) /= N_Package_Instantiation loop
                  Inst_Node := Next (Inst_Node);
               end loop;

               exit;
            else
               True_Parent := Parent (True_Parent);
            end if;
         end loop;

         if Present (Inst_Node) then

            if Nkind (Parent (True_Parent)) = N_Compilation_Unit then

               --  Instantiation node and declaration of instantiated package
               --  were exchanged when only the declaration was needed.
               --  Restore instantiation node before proceeding with body.

               Set_Unit (Parent (True_Parent), Inst_Node);
            end if;

            --  Now complete instantiation of enclosing body.

            Instantiate_Package_Body (Inst_Node, True_Parent);

         else
            Load_Needed_Body (Comp_Unit);
         end if;
      end if;
   end Load_Parent_Of_Generic;

   ---------------------
   -- Inherit_Context --
   ---------------------

   procedure Inherit_Context (Gen_Decl : Node_Id; Inst : Node_Id) is
      Current_Context : List_Id;
      Current_Unit    : Node_Id;
      Item            : Node_Id;
      New_I           : Node_Id;

   begin
      if Nkind (Parent (Gen_Decl)) = N_Compilation_Unit then

         --  The inherited context is attached to the enclosing compilation
         --  unit. This is either the main unit, or the declaration for the
         --  main unit (in case the instantation appears within the package
         --  declaration and the main unit is its body).

         Current_Unit := Parent (Inst);

         while Present (Current_Unit)
           and then Nkind (Current_Unit) /= N_Compilation_Unit
         loop
            Current_Unit := Parent (Current_Unit);
         end loop;

         Current_Context := Context_Items (Current_Unit);

         Item := First (Context_Items (Parent (Gen_Decl)));
         while Present (Item) loop

            if Nkind (Item) = N_With_Clause then
               New_I := New_Copy (Item);
               Set_Implicit_With (New_I, True);
               Append (New_I, Current_Context);
            end if;

            Item := Next (Item);
         end loop;
      end if;
   end Inherit_Context;

   --------------------------
   -- Analyze_Associations --
   --------------------------

   function Analyze_Associations
     (Formals : List_Id;
      Actuals : List_Id;
      F_Copy  : List_Id)
      return    List_Id
   is
      Actual          : Node_Id;
      Actual_Types    : List_Id := New_List;
      Assoc           : List_Id := New_List;
      Formal          : Node_Id;
      Next_Formal     : Node_Id;
      Temp_Formal     : Node_Id;
      Analyzed_Formal : Node_Id;
      Match           : Node_Id;
      Named           : Node_Id;
      First_Named     : Node_Id := Empty;
      Found_Assoc     : Node_Id;
      Is_Named_Assoc  : Boolean;
      Num_Matched     : Int := 0;
      Num_Actuals     : Int := 0;

      function Matching_Actual (F : Entity_Id) return Node_Id;
      --  Find actual that corresponds to a given a formal parameter. If the
      --  actuals are positional, return the next one, if any. If the actuals
      --  are named, scan the parameter associations to find the right one.

      procedure Set_Analyzed_Formal;
      --  Find the node in the generic copy that corresponds to a given formal.
      --  The semantic information on this node is used to perform legality
      --  checks on the actuals. Because semantic analysis can introduce some
      --  anonymous entities or modify the declaration node itself, the
      --  correspondence between the two lists is not one-one. In addition to
      --  anonymous types, the presence a formal equality will introduce an
      --  implicit declaration for the corresponding inequality.

      ---------------------
      -- Matching_Actual --
      ---------------------

      function Matching_Actual (F : Entity_Id) return Node_Id is
         Found : Node_Id;

      begin
         Is_Named_Assoc := False;

         --  End of list of purely positional parameters

         if No (Actual) then
            Found := Empty;

         --  Case of positional parameter corresponding to current formal

         elsif No (Selector_Name (Actual)) then
            Found := Explicit_Generic_Actual_Parameter (Actual);
            Found_Assoc := Actual;
            Num_Matched := Num_Matched + 1;
            Actual := Next (Actual);

         --  Otherwise scan list of named actuals to find the one with the
         --  desired name. All remaining actuals have explicit names.

         else
            Is_Named_Assoc := True;
            Found := Empty;

            while Present (Actual) loop
               if Chars (Selector_Name (Actual)) = Chars (F) then
                  Found := Explicit_Generic_Actual_Parameter (Actual);
                  Found_Assoc := Actual;
                  Num_Matched := Num_Matched + 1;
                  exit;
               end if;

               Actual := Next (Actual);
            end loop;

            --  Reset for subsequent searches.

            Actual := First_Named;
         end if;

         return Found;
      end Matching_Actual;

      -------------------------
      -- Set_Analyzed_Formal --
      -------------------------

      procedure Set_Analyzed_Formal is
      begin
         while Present (Analyzed_Formal) loop

            case Nkind (Formal) is

               when N_Formal_Subprogram_Declaration =>
                  exit when Nkind (Analyzed_Formal)
                    = N_Formal_Subprogram_Declaration
                    and then Chars
                        (Defining_Unit_Name (Specification (Formal)))
                    = Chars
                        (Defining_Unit_Name (Specification (Analyzed_Formal)));

               when N_Formal_Package_Declaration =>
                  exit when
                    Nkind (Analyzed_Formal) = N_Formal_Package_Declaration
                    or else
                    Nkind (Analyzed_Formal) = N_Generic_Package_Declaration;

               when N_Use_Package_Clause | N_Use_Type_Clause => exit;

               when others =>
                  exit when
                    Nkind (Analyzed_Formal) /= N_Formal_Subprogram_Declaration
                      and then Nkind (Analyzed_Formal)
                        /= N_Subprogram_Declaration
                      and then Nkind (Analyzed_Formal) /= N_Freeze_Entity
                      and then Chars (Defining_Identifier (Formal)) =
                               Chars (Defining_Identifier (Analyzed_Formal));
            end case;

            Analyzed_Formal := Next (Analyzed_Formal);
         end loop;

      end Set_Analyzed_Formal;

   --  Start of processing for Analyze_Associations

   begin
      --  If named associations are present, save the first named association
      --  (it may of course be Empty) to facilitate subsequent name search.

      if Present (Actuals) then
         First_Named := First (Actuals);

         while Present (First_Named)
           and then No (Selector_Name (First_Named))
         loop
            Num_Actuals := Num_Actuals + 1;
            First_Named := Next (First_Named);
         end loop;
      end if;

      Named := First_Named;
      while Present (Named) loop
         if No (Selector_Name (Named)) then
            Error_Msg_N ("invalid positional actual after named one", Named);
            Abandon_Instantiation (Named);
         end if;

         Num_Actuals := Num_Actuals + 1;
         Named := Next (Named);
      end loop;

      if Present (Formals) then
         Formal := First_Non_Pragma (Formals);
         Analyzed_Formal := First_Non_Pragma (F_Copy);

         if Present (Actuals) then
            Actual := First (Actuals);

         --  All formals should have default values

         else
            Actual := Empty;
         end if;

         while Present (Formal) loop
            Set_Analyzed_Formal;
            Next_Formal := Next_Non_Pragma (Formal);

            case Nkind (Formal) is
               when N_Formal_Object_Declaration =>
                  Match := Matching_Actual (Defining_Identifier (Formal));
                  Append_List
                    (Instantiate_Object (Formal, Match, Analyzed_Formal),
                     Assoc);

               when N_Formal_Type_Declaration =>
                  Match := Matching_Actual (Defining_Identifier (Formal));
                  if No (Match) then
                     Error_Msg_NE ("missing actual for instantiation of &",
                        Instantiation_Node, Defining_Identifier (Formal));
                     Abandon_Instantiation (Instantiation_Node);

                  else
                     Analyze (Match);
                     Append_To (Assoc,
                       Instantiate_Type (Formal, Match, Analyzed_Formal));
                     Append (Match, Actual_Types);

                     --  Even though the internal type appears as a subtype
                     --  of the actual, it inherits all operations and they
                     --  are immediately visible. This is equivalent to a use
                     --  type clause on  the actual.

                     Append_To (Assoc,
                       Make_Use_Type_Clause (Sloc (Match),
                         Subtype_Marks => New_List (New_Occurrence_Of
                           (Base_Type (Entity (Match)), Sloc (Match)))));
                  end if;

                  --  A remote access-to-class-wide type must not be an
                  --  actual parameter for a generic formal (RM E.2.3(22))

                  Validate_Remote_Access_To_Class_Wide_Type (Match);

               when N_Formal_Subprogram_Declaration =>
                  Match := Matching_Actual
                             (Defining_Unit_Name (Specification (Formal)));

                  --  If the formal subprogram has the same name as
                  --  another formal subprogram of the generic, then
                  --  a named association is illegal (12.3(9)).

                  if Present (Match) and then Is_Named_Assoc then
                     Temp_Formal := First (Formals);
                     while Present (Temp_Formal) loop
                        if Nkind (Temp_Formal) =
                             N_Formal_Subprogram_Declaration
                          and then
                            Temp_Formal /= Formal
                          and then
                            Chars (Selector_Name (Found_Assoc)) =
                              Chars (Defining_Unit_Name
                                       (Specification (Temp_Formal)))
                        then
                           Error_Msg_N
                             ("name not allowed for overloaded formal",
                              Found_Assoc);
                           Abandon_Instantiation (Instantiation_Node);
                        end if;

                        Temp_Formal := Next (Temp_Formal);
                     end loop;
                  end if;

                  Append_To (Assoc,
                    Instantiate_Formal_Subprogram
                      (Formal, Match, Analyzed_Formal));

               when N_Formal_Package_Declaration =>
                  Match := Matching_Actual (Defining_Identifier (Formal));

                  if No (Match) then
                     Error_Msg_NE
                       ("missing actual for instantiation of&",
                        Instantiation_Node,
                        Defining_Identifier (Formal));

                     Abandon_Instantiation (Instantiation_Node);

                  else
                     Analyze (Match);
                     Append_List
                       (Instantiate_Formal_Package
                         (Formal, Match, Analyzed_Formal),
                        Assoc);
                  end if;

               --  For use type and use package appearing in the context
               --  clause, we have already copied them, so we can just
               --  move them where they belong (we mustn't recopy them
               --  since this would mess up the Sloc values).

               when N_Use_Package_Clause |
                    N_Use_Type_Clause    =>
                  Remove (Formal);
                  Append (Formal, Assoc);

               when others => pragma Assert (False); null;

            end case;

            Formal := Next_Formal;
            Analyzed_Formal := Next_Non_Pragma (Analyzed_Formal);
         end loop;

         if Num_Actuals > Num_Matched then
            Error_Msg_N
              ("unmatched actuals in instantiation", Instantiation_Node);
         end if;

      elsif Present (Actuals) then
         Error_Msg_N
           ("too many actuals in generic instantiation", Instantiation_Node);
      end if;

      Match := First (Actual_Types);

      while Present (Match) loop
         Freeze_Before (Instantiation_Node, Entity (Match));
         Match := Next (Match);
      end loop;

      return Assoc;
   end Analyze_Associations;

   -------------------------------
   -- Analyze_Formal_Array_Type --
   -------------------------------

   procedure Analyze_Formal_Array_Type
     (T   : in out Entity_Id;
      Def : Node_Id)
   is
      DSS : Node_Id;

   begin
      --  Treated like a non-generic array declaration, with
      --  additional semantic checks.

      Enter_Name (T);

      if Nkind (Def) = N_Constrained_Array_Definition then
         DSS := First (Discrete_Subtype_Definitions (Def));

         while Present (DSS) loop
            if Nkind (DSS) = N_Subtype_Indication
              or else Nkind (DSS) = N_Range
              or else Nkind (DSS) = N_Attribute_Reference
            then
               Error_Msg_N ("only a subtype mark is allowed in a formal", Def);
            end if;

            DSS := Next (DSS);
         end loop;
      end if;

      Array_Type_Declaration (T, Def);
      Set_Is_Generic_Type (Base_Type (T));

      if Is_Incomplete_Or_Private_Type (Component_Type (T))
        and then No (Full_View (Component_Type (T)))
        and then not Is_Generic_Type (Component_Type (T))
      then
         Error_Msg_N ("premature usage of incomplete type", Def);

      elsif Is_Internal (Component_Type (T)) then
         Error_Msg_N
           ("only a subtype mark is allowed in a formal", Def);
      end if;

   end Analyze_Formal_Array_Type;

   ---------------------------------------------
   -- Analyze_Formal_Decimal_Fixed_Point_Type --
   ---------------------------------------------

   --  As for other generic types, we create a valid type representation
   --  with legal but arbitrary attributes, whose values are never considered
   --  static. For all scalar types we introduce an anonymous base type, with
   --  the same attributes. We choose the corresponding integer type to be
   --  Standard_Integer.

   procedure Analyze_Formal_Decimal_Fixed_Point_Type
     (T   : Entity_Id;
      Def : Node_Id)
   is
      Loc       : constant Source_Ptr := Sloc (Def);
      Base      : constant Entity_Id :=
                    New_Internal_Entity
                      (E_Decimal_Fixed_Point_Type,
                       Current_Scope, Sloc (Def), 'G');
      Int_Base  : constant Entity_Id := Standard_Integer;
      Delta_Val : constant Ureal := Ureal_1;
      Digs_Val  : constant Uint  := Uint_6;

   begin
      Note_Feature (Generic_Formal_Decimal_Types, Loc);

      Enter_Name (T);

      Set_Etype          (Base, Base);
      Set_Size_Info      (Base,                (Int_Base));
      Set_RM_Size        (Base, RM_Size        (Int_Base));
      Set_First_Rep_Item (Base, First_Rep_Item (Int_Base));
      Set_Digits_Value   (Base, Digs_Val);
      Set_Delta_Value    (Base, Delta_Val);
      Set_Small_Value    (Base, Delta_Val);
      Set_Scalar_Range   (Base,
        Make_Range (Loc,
          Low_Bound  => Make_Real_Literal (Loc, Ureal_1),
          High_Bound => Make_Real_Literal (Loc, Ureal_1)));

      Set_Is_Generic_Type  (Base);

      Set_Ekind          (T, E_Decimal_Fixed_Point_Subtype);
      Set_Etype          (T, Base);
      Set_Size_Info      (T,                (Int_Base));
      Set_RM_Size        (T, RM_Size        (Int_Base));
      Set_First_Rep_Item (T, First_Rep_Item (Int_Base));
      Set_Digits_Value   (T, Digs_Val);
      Set_Delta_Value    (T, Delta_Val);
      Set_Small_Value    (T, Delta_Val);
      Set_Scalar_Range   (T, Scalar_Range (Base));

   end Analyze_Formal_Decimal_Fixed_Point_Type;

   ---------------------------------
   -- Analyze_Formal_Derived_Type --
   ---------------------------------

   procedure Analyze_Formal_Derived_Type
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id)
   is
      Loc      : constant Source_Ptr := Sloc (Def);
      New_N    : Node_Id;
      Unk_Disc : Boolean := Unknown_Discriminants_Present (N);

   begin
      Note_Feature (Generic_Formal_Derived_Types, Loc);
      Set_Is_Generic_Type (T);

      if Private_Present (Def) then
         New_N :=
           Make_Private_Extension_Declaration (Loc,
             Defining_Identifier           => T,
             Discriminant_Specifications   => Discriminant_Specifications (N),
             Unknown_Discriminants_Present => Unk_Disc,
             Subtype_Indication            => Subtype_Mark (Def));

         Set_Abstract_Present (New_N, Abstract_Present (Def));

      else
         New_N :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => T,
             Discriminant_Specifications =>
               Discriminant_Specifications (Parent (T)),
              Type_Definition =>
                Make_Derived_Type_Definition (Loc,
                  Subtype_Indication => Subtype_Mark (Def)));

         Set_Abstract_Present
           (Type_Definition (New_N), Abstract_Present (Def));
      end if;

      Rewrite_Substitute_Tree (N, New_N);
      Analyze (N);

      if Unk_Disc then
         if not Is_Composite_Type (T) then
            Error_Msg_N
              ("unknown discriminants not allowed for elementary types", N);
         else
            Set_Has_Unknown_Discriminants (T);
            Set_Is_Constrained (T, False);
         end if;
      end if;

   end Analyze_Formal_Derived_Type;

   ----------------------------------
   -- Analyze_Formal_Discrete_Type --
   ----------------------------------

   --  The operations defined for a discrete types are those of an
   --  enumeration type. The size is set to an arbitrary value, for use
   --  in analyzing the generic unit.

   procedure Analyze_Formal_Discrete_Type (T : Entity_Id; Def : Node_Id) is
      Loc : constant Source_Ptr := Sloc (Def);
      Lo  : Node_Id;
      Hi  : Node_Id;

   begin
      Enter_Name (T);

      Set_Ekind   (T, E_Enumeration_Type);
      Set_Etype   (T, T);
      Set_Esize   (T, Uint_0);
      Set_RM_Size (T, Uint_0);

      --  For semantic analysis, the bounds of the type must be set to some
      --  non-static value. The simplest is to create attribute nodes for
      --  those bounds, that refer to the type itself. These bounds are never
      --  analyzed but serve as place-holders.

      Lo :=
        Make_Attribute_Reference (Loc,
          Attribute_Name => Name_First,
          Prefix => New_Reference_To (T, Loc));
      Set_Etype (Lo, T);

      Hi :=
        Make_Attribute_Reference (Loc,
          Attribute_Name => Name_Last,
          Prefix => New_Reference_To (T, Loc));
      Set_Etype (Hi, T);

      Set_Scalar_Range (T,
        Make_Range (Loc,
          Low_Bound => Lo,
          High_Bound => Hi));

   end Analyze_Formal_Discrete_Type;

   ----------------------------------
   -- Analyze_Formal_Floating_Type --
   ---------------------------------

   procedure Analyze_Formal_Floating_Type (T : Entity_Id; Def : Node_Id) is
      --  the various semantic attributes are taken from the predefined type
      --  Float, just so that all of them are initialized. Their values are
      --  never used because no constant folding or expansion takes place in
      --  the generic itself.

      Base : constant Entity_Id :=
        New_Internal_Entity
          (E_Floating_Point_Type, Current_Scope, Sloc (Def), 'G');

   begin
      Enter_Name (T);
      Set_Ekind        (T, E_Floating_Point_Subtype);
      Set_Etype        (T, Base);
      Set_Size_Info    (T,              (Standard_Float));
      Set_Digits_Value (T, Digits_Value (Standard_Float));
      Set_Scalar_Range (T, Scalar_Range (Standard_Float));

      Set_Is_Generic_Type (Base);
      Set_Etype           (Base, Base);
      Set_Size_Info       (Base,              (Standard_Float));
      Set_Digits_Value    (Base, Digits_Value (Standard_Float));
      Set_Scalar_Range    (Base, Scalar_Range (Standard_Float));
   end Analyze_Formal_Floating_Type;

   ---------------------------------
   -- Analyze_Formal_Modular_Type --
   ---------------------------------

   procedure Analyze_Formal_Modular_Type (T : Entity_Id; Def : Node_Id) is
   begin

      --  Apart from their entity kind, generic modular types are treated
      --  like signed integer types, and have the same attributes.

      Analyze_Formal_Signed_Integer_Type (T, Def);
      Set_Ekind (T, E_Modular_Integer_Subtype);
      Set_Ekind (Etype (T), E_Modular_Integer_Type);

   end Analyze_Formal_Modular_Type;

   ---------------------------------------
   -- Analyze_Formal_Object_Declaration --
   ---------------------------------------

   procedure Analyze_Formal_Object_Declaration (N : Node_Id) is
      E  : constant Node_Id := Expression (N);
      Id : Node_Id := Defining_Identifier (N);
      K  : Entity_Kind;
      T  : Node_Id;

   begin
      Enter_Name (Id);

      --  Determine the mode of the formal object

      if Out_Present (N) then
         K := E_Generic_In_Out_Parameter;

         if not In_Present (N) then
            Error_Msg_N ("formal generic objects cannot have mode OUT", N);
         end if;

      else
         K := E_Generic_In_Parameter;
      end if;

      Find_Type (Subtype_Mark (N));
      T  := Entity (Subtype_Mark (N));

      if Ekind (T) = E_Incomplete_Type then
         Error_Msg_N ("premature usage of incomplete type", Subtype_Mark (N));
      end if;

      if K = E_Generic_In_Parameter then
         if Is_Limited_Type (T) then
            Error_Msg_N
             ("generic formal of mode IN must not be of limited type", N);
         end if;

         if Is_Abstract (T) then
            Error_Msg_N
             ("generic formal of mode IN must not be of abstract type", N);
         end if;

         if Present (E) then
            Analyze_Default_Expression (E, T);
         end if;

         Set_Ekind (Id, K);
         Set_Etype (Id, T);

      --  Case of generic IN OUT parameter.

      else
         --  If the formal has an unconstrained type, construct its
         --  actual subtype, as is done for subprogram formals. In this
         --  fashion, all its uses can refer to specific bounds.

         Set_Ekind (Id, K);
         Set_Etype (Id, T);

         if (Is_Array_Type (T)
              and then not Is_Constrained (T))
           or else
            (Ekind (T) = E_Record_Type
              and then Has_Discriminants (T))
         then
            declare
               Non_Freezing_Ref : constant Node_Id :=
                                    New_Reference_To (Id, Sloc (Id));
               Decl : Node_Id;

            begin
               --  Make sure that the actual subtype doesn't generate
               --  bogus freezing.

               Set_Must_Not_Freeze (Non_Freezing_Ref);
               Decl := Build_Actual_Subtype (T, Non_Freezing_Ref);
               Insert_Before_And_Analyze (N, Decl);
               Set_Actual_Subtype (Id, Defining_Identifier (Decl));
            end;
         else
            Set_Actual_Subtype (Id, T);
         end if;

         if Present (E) then
            Error_Msg_N
             ("initialization not allowed for `IN OUT` formals", N);
         end if;
      end if;

   end Analyze_Formal_Object_Declaration;

   ----------------------------------------------
   -- Analyze_Formal_Ordinary_Fixed_Point_Type --
   ----------------------------------------------

   procedure Analyze_Formal_Ordinary_Fixed_Point_Type
     (T   : Entity_Id;
      Def : Node_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Def);
      Base : constant Entity_Id :=
        New_Internal_Entity
          (E_Ordinary_Fixed_Point_Type, Current_Scope, Sloc (Def), 'G');
   begin
      --  The semantic attributes are set for completeness only, their
      --  values will never be used, because all properties of the type are
      --  non-static.

      Enter_Name (T);
      Set_Ekind            (T, E_Ordinary_Fixed_Point_Subtype);
      Set_Etype            (T, Base);
      Set_Size_Info        (T,         (Standard_Integer));
      Set_RM_Size          (T, RM_Size (Standard_Integer));
      Set_Small_Value      (T, Ureal_1);
      Set_Delta_Value      (T, Ureal_1);
      Set_Scalar_Range     (T,
        Make_Range (Loc,
          Low_Bound  => Make_Real_Literal (Loc, Ureal_1),
          High_Bound => Make_Real_Literal (Loc, Ureal_1)));

      Set_Is_Generic_Type (Base);
      Set_Etype           (Base, Base);
      Set_Size_Info       (Base,         (Standard_Integer));
      Set_RM_Size         (Base, RM_Size (Standard_Integer));
      Set_Small_Value     (Base, Ureal_1);
      Set_Delta_Value     (Base, Ureal_1);
      Set_Scalar_Range    (Base, Scalar_Range (T));
   end Analyze_Formal_Ordinary_Fixed_Point_Type;

   ----------------------------
   -- Analyze_Formal_Package --
   ----------------------------

   procedure Analyze_Formal_Package (N : Node_Id) is
      Formal           : Entity_Id := Defining_Identifier (N);
      Gen_Id           : constant Node_Id   := Name (N);
      Gen_Decl         : Node_Id;
      Gen_Unit         : Entity_Id;
      New_N            : Node_Id;
      Parent_Installed : Boolean := False;

      Save_Exchanged_Views     : Elist_Id;
      Save_Instantiated_Parent : Assoc;

   begin
      Note_Feature (Generic_Formal_Packages, Sloc (N));

      Text_IO_Kludge (Gen_Id);

      Check_Generic_Child_Unit (Gen_Id, Parent_Installed);
      Gen_Unit := Entity (Gen_Id);

      if Ekind (Gen_Unit) /= E_Generic_Package then
         Error_Msg_N ("expect generic package name", Gen_Id);
         return;
      end if;

      --  Check for a formal package that is a package renaming.

      if Present (Renamed_Object (Gen_Unit)) then
         Gen_Unit := Renamed_Object (Gen_Unit);
      end if;

      --  The formal package is treated like a regular instance, but only
      --  the specification needs to be instantiated, to make entities visible.


      if not Box_Present (N) then
         Analyze_Package_Instantiation (N);

      else
         --  If there are no generic associations, the generic parameters
         --  appear as local entities and are instantiated like them. We copy
         --  the generic package declaration as if it were an instantiation,
         --  and analyze it like a regular package, except that we treat the
         --  formals as additional visible components.

         Save_Exchanged_Views := Exchanged_Views;
         Exchanged_Views := New_Elmt_List;

         Gen_Decl := Get_Declaration_Node (Gen_Unit);
         New_N :=
           Copy_Generic_Node
             (Original_Node (Gen_Decl), Empty, Instantiating => True);
         Set_Defining_Unit_Name (Specification (New_N), Formal);
         Rewrite_Substitute_Tree (N, New_N);

         Formal := Defining_Unit_Name (Specification (N));

         Enter_Name (Formal);
         Set_Ekind  (Formal, E_Generic_Package);
         Set_Etype  (Formal, Standard_Void_Type);
         Set_Inner_Instances (Formal, New_Elmt_List);
         New_Scope  (Formal);

         Save_Instantiated_Parent    := Current_Instantiated_Parent;
         Current_Instantiated_Parent := (Gen_Unit, Formal, Assoc_Null);

         Analyze_Generic_Formal_Part (N);
         Analyze (Specification (N));
         End_Package_Scope (Formal);
         Exchanged_Views := Save_Exchanged_Views;
         Current_Instantiated_Parent := Save_Instantiated_Parent;

         --  Inside the generic unit, the formal package is a regular
         --  package, but no body is needed for it. Note that after
         --  instantiation, the defining_unit_name we need is in the
         --  new tree and not in the original. (see Package_Instantiation).
         --  A generic formal package is an instance, and can be used as
         --  an actual for an inner instance. Mark its generic parent.

         Set_Ekind (Formal, E_Package);
         Set_Generic_Parent (Specification (N), Gen_Unit);
         Set_Has_Completion (Formal, True);
      end if;

      if Parent_Installed then
         Remove_Parent;
      end if;

   end Analyze_Formal_Package;

   ---------------------------------
   -- Analyze_Formal_Private_Type --
   ---------------------------------

   procedure Analyze_Formal_Private_Type
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id)
   is
   begin
      New_Private_Type (N, T, Def);

      --  Set the size to an arbitrary but legal value.

      Set_Esize (T, Esize (Standard_Integer));
   end Analyze_Formal_Private_Type;

   ----------------------------------------
   -- Analyze_Formal_Signed_Integer_Type --
   ----------------------------------------

   procedure Analyze_Formal_Signed_Integer_Type
     (T   : Entity_Id;
      Def : Node_Id)
   is
      Base : constant Entity_Id :=
        New_Internal_Entity
          (E_Signed_Integer_Type, Current_Scope, Sloc (Def), 'G');

   begin
      Enter_Name (T);

      Set_Ekind        (T, E_Signed_Integer_Subtype);
      Set_Etype        (T, Base);
      Set_Size_Info    (T,       (Standard_Integer));
      Set_RM_Size      (T, Esize (Standard_Integer));
      Set_Scalar_Range (T, Scalar_Range (Standard_Integer));

      Set_Is_Generic_Type (Base);
      Set_Size_Info       (Base,       (Standard_Integer));
      Set_RM_Size         (Base, Esize (Standard_Integer));
      Set_Etype           (Base, Base);
      Set_Scalar_Range    (Base, Scalar_Range (Standard_Integer));
   end Analyze_Formal_Signed_Integer_Type;

   -------------------------------
   -- Analyze_Formal_Subprogram --
   -------------------------------

   procedure Analyze_Formal_Subprogram (N : Node_Id) is
      Spec : constant Node_Id   := Specification (N);
      Def  : constant Node_Id   := Default_Name (N);
      Nam  : constant Entity_Id := Defining_Unit_Name (Spec);
      Subp : Entity_Id;

   begin
      if Nkind (Nam) = N_Defining_Program_Unit_Name then
         Error_Msg_N ("name of formal subprogram must be a direct name", Nam);
         return;
      end if;

      Analyze_Subprogram_Declaration (N);
      Set_Has_Completion (Nam);

      --  Default name is resolved at the point of instantiation

      if Box_Present (N) then
         null;

      --  Else default is bound at the point of generic declaration

      elsif Present (Def) then
         if Nkind (Def) = N_Operator_Symbol then
            Find_Direct_Name (Def);

         elsif Nkind (Def) /= N_Attribute_Reference then
            Analyze (Def);

         else
            --  For an attribute reference, analyze the prefix. Whether the
            --  attribute is legal will be determined at instantiation time.

            Analyze (Prefix (Def));
            return;
         end if;

         --  Default name may be overloaded, in which case the interpretation
         --  with the correct profile must be  selected, as for a renaming.

         if Etype (Def) = Any_Type then
            return;

         elsif Nkind (Def) = N_Selected_Component then
            Subp := Entity (Selector_Name (Def));

            if Ekind (Subp) /= E_Entry then
               Error_Msg_N ("expect valid subprogram name as default", Def);
               return;
            end if;

         elsif Nkind (Def) = N_Indexed_Component then

            if  Nkind (Prefix (Def)) /= N_Selected_Component then
               Error_Msg_N ("expect valid subprogram name as default", Def);
               return;

            else
               Subp := Entity (Selector_Name (Prefix (Def)));

               if Ekind (Subp) /= E_Entry_Family then
                  Error_Msg_N ("expect valid subprogram name as default", Def);
                  return;
               end if;
            end if;

         elsif Nkind (Def) = N_Character_Literal then

            --  Needs some type checks: subprogram should be parameterless???

            Resolve (Def, (Etype (Nam)));

         elsif (not Is_Entity_Name (Def)
           or else not Is_Overloadable (Entity (Def)))
         then
            Error_Msg_N ("expect valid subprogram name as default", Def);
            return;

         elsif not Is_Overloaded (Def) then
            Subp := Entity (Def);

            if Subp = Nam then
               Error_Msg_N ("premature usage of formal subprogram", Def);

            elsif not Entity_Matches_Spec (Subp, Nam) then
               Error_Msg_N ("no visible entity matches specification", Def);
            end if;

         else
            declare
               I    : Interp_Index;
               I1   : Interp_Index;
               It   : Interp;
               It1  : Interp;

            begin
               Subp := Any_Id;
               Get_First_Interp (Def, I, It);

               while Present (It.Nam) loop

                  if Entity_Matches_Spec (It.Nam, Nam) then
                     if Subp /= Any_Id then
                        It1 := Disambiguate (Def, I1, I, Etype (Subp));

                        if It1 = No_Interp then
                           Error_Msg_N ("ambiguous default subprogram", Def);
                        else
                           Subp := It1.Nam;
                        end if;

                        exit;

                     else
                        I1  := I;
                        Subp := It.Nam;
                     end if;
                  end if;

                  Get_Next_Interp (I, It);
               end loop;
            end;

            if Subp /= Any_Id then
               Set_Entity (Def, Subp);

               if Subp = Nam then
                  Error_Msg_N ("premature usage of formal subprogram", Def);

               elsif Ekind (Subp) /= E_Operator then
                  Check_Mode_Conformant (Subp, Nam);
               end if;

            else
               Error_Msg_N ("no visible subprogram matches specification", N);
            end if;
         end if;
      end if;
   end Analyze_Formal_Subprogram;

   -------------------------------------
   -- Analyze_Formal_Type_Declaration --
   -------------------------------------

   procedure Analyze_Formal_Type_Declaration (N : Node_Id) is
      Def : constant Node_Id := Formal_Type_Definition (N);
      T   : Entity_Id;

   begin
      T := Defining_Identifier (N);

      if Present (Discriminant_Specifications (N))
        and then Nkind (Def) /= N_Formal_Private_Type_Definition
      then
         Error_Msg_N
           ("discriminants not allowed for this formal type",
            Defining_Identifier (First (Discriminant_Specifications (N))));
      end if;

      --  Enter the new name, and branch to specific routine.

      case Nkind (Def) is
         when N_Formal_Private_Type_Definition
                        => Analyze_Formal_Private_Type (N, T, Def);

         when N_Formal_Derived_Type_Definition
                        => Analyze_Formal_Derived_Type (N, T, Def);

         when N_Formal_Discrete_Type_Definition
                        => Analyze_Formal_Discrete_Type (T, Def);

         when N_Formal_Signed_Integer_Type_Definition
                        => Analyze_Formal_Signed_Integer_Type (T, Def);

         when N_Formal_Modular_Type_Definition
                        => Analyze_Formal_Modular_Type (T, Def);

         when N_Formal_Floating_Point_Definition
                        => Analyze_Formal_Floating_Type (T, Def);

         when N_Formal_Ordinary_Fixed_Point_Definition
                        => Analyze_Formal_Ordinary_Fixed_Point_Type (T, Def);

         when N_Formal_Decimal_Fixed_Point_Definition
                        => Analyze_Formal_Decimal_Fixed_Point_Type (T, Def);

         when N_Array_Type_Definition
                        => Analyze_Formal_Array_Type (T, Def);

         when N_Access_To_Object_Definition |
              N_Access_Function_Definition  |
              N_Access_Procedure_Definition
                        => Analyze_Generic_Access_Type (T, Def);

         when others =>
            pragma Assert (False); null;

      end case;

      Set_Is_Generic_Type (T);

   end Analyze_Formal_Type_Declaration;

   ---------------------------------
   -- Analyze_Generic_Access_Type --
   ---------------------------------

   procedure Analyze_Generic_Access_Type (T : Entity_Id; Def : Node_Id) is
   begin
      Enter_Name (T);

      if Nkind (Def) = N_Access_To_Object_Definition then
         Access_Type_Declaration (T, Def);

         if Is_Incomplete_Or_Private_Type (Designated_Type (T))
           and then No (Full_View (Designated_Type (T)))
           and then not Is_Generic_Type (Designated_Type (T))
         then
            Error_Msg_N ("premature usage of incomplete type", Def);

         elsif Is_Internal (Designated_Type (T)) then
            Error_Msg_N
              ("only a subtype mark is allowed in a formal", Def);
         end if;

      else
         Access_Subprogram_Declaration (T, Def);
      end if;
   end Analyze_Generic_Access_Type;

   ---------------------
   -- Associated_Node --
   ---------------------

   function Associated_Node (N : Node_Id) return Node_Id is
      Assoc : Node_Id := Node4 (N);
      --  ??? what is Node4 being used for here?

   begin
      if Nkind (Assoc) /= Nkind (N) then
         return Assoc;
      else
         --  If the node is part of an inner generic, it may itself have been
         --  remapped into a further generic copy. Node4 is otherwise used for
         --  the entity of the node, and will be of a different node kind, or
         --  else N has been rewritten as a literal or function call.

         while Present (Node4 (Assoc))
           and then Nkind (Node4 (Assoc)) = Nkind (Assoc)
         loop
            Assoc := Node4 (Assoc);
         end loop;

         --  Follow and additional link in case the final node was rewritten.
         --  This can only happen with nested generic units.

         if (Nkind (Assoc) = N_Identifier or else Nkind (Assoc) in N_Op)
           and then Present (Node4 (Assoc))
           and then (Nkind (Node4 (Assoc)) = N_Function_Call
                       or else Nkind (Node4 (Assoc)) = N_Explicit_Dereference
                       or else Nkind (Node4 (Assoc)) = N_Integer_Literal
                       or else Nkind (Node4 (Assoc)) = N_Real_Literal
                       or else Nkind (Node4 (Assoc)) = N_String_Literal)
         then
            Assoc := Node4 (Assoc);
         end if;

         return Assoc;
      end if;
   end Associated_Node;

   -------------------------------------------
   -- Build_Instance_Compilation_Unit_Nodes --
   -------------------------------------------

   procedure Build_Instance_Compilation_Unit_Nodes
     (N        : Node_Id;
      Act_Body : Node_Id;
      Act_Decl : Node_Id)
   is
      Decl_Cunit : Node_Id;
      Body_Cunit : Node_Id;
      Citem      : Node_Id;

   begin
      --  A new compilation unit node is built for the instance declaration

      Decl_Cunit := New_Node (N_Compilation_Unit, Sloc (N));
      Set_Context_Items (Decl_Cunit, Empty_List);
      Set_Unit          (Decl_Cunit, Act_Decl);
      Set_Parent_Spec   (Act_Decl, Parent_Spec (N));
      Set_Body_Required (Decl_Cunit, True);

      --  We use the original instantiation compilation unit as the resulting
      --  compilation unit of the instance, since this is the main unit.

      Rewrite_Substitute_Tree (N, Act_Body);
      Body_Cunit := Parent (N);

      --  The two compilation unit nodes are linked by the Library_Unit field

      Set_Library_Unit  (Decl_Cunit, Body_Cunit);
      Set_Library_Unit  (Body_Cunit, Decl_Cunit);

      --  The context clause items on the instantiation, which are now
      --  attached to the body compilation unit (since the body overwrote
      --  the orginal instantiation node), semantically belong on the spec,
      --  so copy them there. It's harmless to leave them on the body as well.
      --  In fact one could argue that they belong in both places.

      Citem := First (Context_Items (Body_Cunit));
      while Present (Citem) loop
         Append (New_Copy (Citem), Context_Items (Decl_Cunit));
         Citem := Next (Citem);
      end loop;

      --  Make entry in Units table, so that binder can generate call to
      --  elaboration procedure for body, if any.

      Make_Instance_Unit (Body_Cunit);
      Main_Unit_Entity := Defining_Entity (Act_Decl);

   end Build_Instance_Compilation_Unit_Nodes;

   ---------------------------
   -- Check_Generic_Actuals --
   ---------------------------

   --  The visibility of the actuals may be different between the
   --  point of generic instantiation and the instantiation of the body.

   procedure Check_Generic_Actuals
     (Instance      : Entity_Id;
      Is_Formal_Box : Boolean)
   is
      E      : Entity_Id;
      Astype : Entity_Id;

   begin
      E := First_Entity (Instance);

      while Present (E) loop
         if Is_Type (E)
           and then Nkind (Parent (E)) = N_Subtype_Declaration
           and then Scope (Etype (E)) /= Instance
           and then Is_Entity_Name (Subtype_Indication (Parent (E)))
         then
            Check_Private_View (Subtype_Indication (Parent (E)));
            Set_Is_Generic_Actual_Type (E, True);
            Set_Is_Private (E, False);

            --  We constructed the generic actual type as a subtype of
            --  the supplied type. This means that it normally would not
            --  inherit subtype specific attributes of the actual, which
            --  is wrong for the generic case.

            Astype := Ancestor_Subtype (E);

            Set_Size_Info      (E,                (Astype));
            Set_First_Rep_Item (E, First_Rep_Item (Astype));

            if Is_Discrete_Or_Fixed_Point_Type (E) then
               Set_RM_Size (E, RM_Size (Astype));

            --  In  nested instances, the base type of an access actual
            --  may itself be private, and need to be exchanged.

            elsif Is_Access_Type (E)
              and then Is_Private_Type (Etype (E))
            then
               Check_Private_View
                 (New_Occurrence_Of (Etype (E), Sloc (Instance)));
            end if;

         elsif Ekind (E) = E_Package then

            --  If this is the renaming for the current instance, we're done.
            --  Otherwise it is a formal package. If the corresponding formal
            --  was declared with a box, the (instantiations of the) generic
            --  formal part are also visible. Otherwise, ignore the entity
            --  created to validate the actuals.

            if Renamed_Object (E) = Instance then
               exit;

            elsif Nkind (Parent (E)) /= N_Package_Renaming_Declaration then
               null;

            elsif Box_Present (Parent (Associated_Formal_Package (E))) then
               Check_Generic_Actuals (Renamed_Object (E), True);
               Set_Is_Private (E, False);
            end if;

         else
            Set_Is_Private (E, not Is_Formal_Box);
         end if;

         E := Next_Entity (E);
      end loop;

   end Check_Generic_Actuals;

   -----------------------------------
   -- Check_Premature_Instantiation --
   -----------------------------------

   procedure Check_Premature_Instantiation
      (Inst_Node : Node_Id;
       Gen_Decl  : Node_Id)
   is
      Loc : constant Source_Ptr := Sloc (Inst_Node);
      Err : Node_Id;
      Dec : Node_Id;

   begin
      if Parent (Inst_Node) = Parent (Gen_Decl)
        and then No (Corresponding_Body (Gen_Decl))
      then
         Err := Make_Raise_Statement (Loc,
           Name =>
             New_Occurrence_Of (Standard_Program_Error, Loc));
         Insert_Before (Inst_Node, Err);
         Analyze (Err);

         --  If the early instantiation appears within a package declaration,
         --  the body is in the corresponding package body. Program_Error is
         --  certain, and there is no proper placement for the instance body.

         Dec := Get_Declaration_Node (Current_Scope);

         if Nkind  (Dec) = N_Package_Declaration
           and then Nkind (Parent (Dec)) = N_Compilation_Unit
           and then Parent (Inst_Node) = Specification (Dec)
         then
            Error_Msg_N ("instantiation before generic body seen", Inst_Node);

         else
            Error_Msg_N ("instantiation before generic body seen?", Inst_Node);
            Error_Msg_NE ("& will be raised at runtime?!",
              Inst_Node, Standard_Program_Error);
         end if;
      end if;
   end Check_Premature_Instantiation;

   ------------------------
   -- Check_Private_View --
   ------------------------

   procedure Check_Private_View (N : Node_Id) is
      T : constant Entity_Id := Etype (N);

   begin

      if Present (T) then
         if Is_Private_Type (T)
           and then not Has_Private_View (N)
           and then Present (Full_View (T))
         then
            --  In the generic, the full type was visible. Save the
            --  private entity, for subsequent exchange.

            Switch_View (T);

         elsif Has_Private_View (N)
           and then not Is_Private_Type (T)
           and then not Has_Been_Exchanged (T)
           and then Etype (Associated_Node (N)) /= T
         then
            --  Only the private declaration was visible in the generic.

            Append_Elmt (T, Exchanged_Views);
            Exchange_Declarations (Etype (Associated_Node (N)));

         elsif Is_Access_Type (T)
           and then Is_Private_Type (Designated_Type (T))
           and then Present (Full_View (Designated_Type (T)))
         then
            Switch_View (Designated_Type (T));

         elsif Is_Array_Type (T)
           and then Is_Private_Type (Component_Type (T))
           and then Present (Full_View (Component_Type (T)))
         then
            Switch_View (Component_Type (T));

         end if;
      end if;
   end Check_Private_View;


   -----------------
   -- Switch_View --
   -----------------

   procedure Switch_View (T : Entity_Id) is
      Priv_Elmt : Elmt_Id := No_Elmt;
      Priv_Sub  : Entity_Id;
      BT        : Entity_Id := Base_Type (T);

   begin
      Priv_Elmt := First_Elmt (Private_Dependents (BT));

      Append_Elmt (Full_View (BT), Exchanged_Views);
      Exchange_Declarations (BT);

      while Present (Priv_Elmt) loop
         Priv_Sub := (Node (Priv_Elmt));

         --  We avoid flipping the subtype if the Etype of its full
         --  view is private because this would result in a malformed
         --  subtype. This occurs when the Etype of the subtype full
         --  view is the full view of the base type (and since the
         --  base types were just switched, the subtype is pointing
         --  to the wrong view). This is currently the case for
         --  tagged record types, access types (maybe more?) and
         --  needs to be resolved. ???

         if Present (Full_View (Priv_Sub))
           and then not Is_Private_Type (Etype (Full_View (Priv_Sub)))
         then
            Append_Elmt (Full_View (Priv_Sub), Exchanged_Views);
            Exchange_Declarations (Priv_Sub);
         end if;

         Priv_Elmt := Next_Elmt (Priv_Elmt);
      end loop;
   end Switch_View;

   ------------------------
   -- Has_Been_Exchanged --
   ------------------------

   function Has_Been_Exchanged (E : Entity_Id) return boolean is
      Next : Elmt_Id := First_Elmt (Exchanged_Views);

   begin

      while Present (Next) loop
         if Full_View (Node (Next)) = E then
            return TRUE;
         end if;
         Next := Next_Elmt (Next);
      end loop;

      return FALSE;
   end Has_Been_Exchanged;


   -----------------------
   -- Copy_Generic_Node --
   -----------------------

   function Copy_Generic_Node
     (N             : Node_Id;
      Parent_Id     : Node_Id;
      Instantiating : Boolean)
      return          Node_Id
   is
      New_N : Node_Id;

      function Copy_Generic_Descendant (D : Union_Id) return Union_Id;
      --  Check the given value of one of the Fields referenced by the
      --  current node to determine whether to copy it recursively. The
      --  field may hold a Node_Id, a List_Id, or an Elist_Id, or a plain
      --  value (Sloc, Uint, Char) in which case it need not be copied.

      function Copy_Generic_List
        (L         : List_Id;
         Parent_Id : Node_Id)
         return List_Id;
      --  Apply Copy_Node recursively to the members of a node list.

      function Copy_Generic_Elist (E : Elist_Id) return Elist_Id;
      --  Make copy of element list.

      -----------------------------
      -- Copy_Generic_Descendant --
      -----------------------------

      function Copy_Generic_Descendant (D : Union_Id) return Union_Id is
      begin
         if D = Union_Id (Empty) then
            return D;

         elsif D in Node_Range then
            return Union_Id
              (Copy_Generic_Node (Node_Id (D), New_N, Instantiating));

         elsif D in List_Range then
            return Union_Id (Copy_Generic_List (List_Id (D), New_N));

         elsif D in Elist_Range then
            return Union_Id (Copy_Generic_Elist (Elist_Id (D)));

         --  Nothing else is copyable (e.g. Uint values), return as is

         else
            return D;
         end if;
      end Copy_Generic_Descendant;

      -----------------------
      -- Copy_Generic_List --
      -----------------------

      function Copy_Generic_List
        (L         : List_Id;
         Parent_Id : Node_Id)
         return      List_Id
      is
         N     : Node_Id;
         New_L : List_Id;

      begin
         if Present (L) then
            New_L := New_List;
            Set_Parent (New_L, Parent_Id);
            N := First (L);

            while Present (N) loop
               Append (Copy_Generic_Node (N, Empty, Instantiating), New_L);
               N := Next (N);
            end loop;

            return New_L;

         else
            return No_List;
         end if;
      end Copy_Generic_List;

      ------------------------
      -- Copy_Generic_Elist --
      ------------------------

      function Copy_Generic_Elist (E : Elist_Id) return Elist_Id is
         M : Elmt_Id;
         L : Elist_Id;

      begin
         if Present (E) then
            L := New_Elmt_List;
            M := First_Elmt (E);

            while Present (M) loop
               Append_Elmt
                 (Copy_Generic_Node (Node (M), Empty, Instantiating), L);
               M := Next_Elmt (M);
            end loop;

            return L;

         else
            return No_Elist;
         end if;
      end Copy_Generic_Elist;

   --  Start of processing for Copy_Generic_Node

   begin
      if N = Empty then
         return N;
      end if;

      New_N := New_Copy (N);

      if Instantiating then
         Adjust_Instantiation_Sloc (New_N, S_Adjustment);
      end if;

      if not Is_List_Member (N) then
         Set_Parent (New_N, Parent_Id);
      end if;

      --  If defining identifier, then all fields have been copied already

      if Nkind (New_N) in N_Entity then
         null;

      --  Special casing for identifiers and other entity names and operators

      elsif    (Nkind (New_N) = N_Identifier
        or else Nkind (New_N) = N_Character_Literal
        or else Nkind (New_N) = N_Expanded_Name
        or else Nkind (New_N) = N_Operator_Symbol
        or else Nkind (New_N) in N_Op)
      then
         if not Instantiating then

            --  Link both nodes in order to assign subsequently the
            --  entity of the copy to the original node, in case this
            --  is a global reference.

            Set_Associated_Node (N, New_N);

            --  If we are within an instantiation, this is a nested generic
            --  that has already been analyzed at the point of definition. We
            --  must preserve references that were global to the enclosing
            --  parent at that point. Other occurrences, whether global or
            --  local to the current generic, must be resolved anew, so we
            --  reset the entity in the generic copy. A global reference has
            --  a smaller depth than the parent, or else the same depth in
            --  case both are distinct compilation units.

            --  It is also possible for Current_Instantiated_Parent to be
            --  defined, and for this not to be a nested generic, namely
            --  if the unit is loaded through Rtsfind. In that case, the
            --  entity of New_N is only a link to the associated node, and
            --  not a defining occurrence.

            if No (Current_Instantiated_Parent.Gen_Id)
              or else No (Entity (New_N))
              or else
                not (Nkind (Entity (New_N)) = N_Defining_Identifier
                       or else
                     Nkind (Entity (New_N)) = N_Defining_Character_Literal
                       or else
                     Nkind (Entity (New_N)) = N_Defining_Operator_Symbol)
              or else No (Scope (Entity (New_N)))
              or else Scope (Entity (New_N)) =
                                  Current_Instantiated_Parent.Gen_Id
              or else (Scope_Depth (Scope (Entity (New_N))) >
                             Scope_Depth (Current_Instantiated_Parent.Gen_Id)
                         and then
                       Get_Sloc_Unit_Number (Sloc (Entity (New_N))) =
                       Get_Sloc_Unit_Number
                         (Sloc (Current_Instantiated_Parent.Gen_Id)))
            then
               Set_Associated_Node (New_N, Empty);
            end if;

         --  Case of instantiating identifier or some other name or operator

         else
            --  If the associated node is still defined, the entity in
            --  it is global, and must be copied to the instance.

            if Present (Associated_Node (N)) then
               if Nkind (Associated_Node (N)) = Nkind (N) then
                  Set_Entity (New_N, Entity (Associated_Node (N)));
                  Check_Private_View (N);

               elsif Nkind (Associated_Node (N)) = N_Function_Call then
                  Set_Entity (New_N, Entity (Name (Associated_Node (N))));

               else
                  Set_Entity (New_N, Empty);
               end if;
            end if;
         end if;

         --  For expanded name, we must copy the Prefix and Selector_Name

         if Nkind (N) = N_Expanded_Name then

            Set_Prefix
              (New_N, Copy_Generic_Node (Prefix (N), New_N, Instantiating));

            Set_Selector_Name (New_N,
              Copy_Generic_Node (Selector_Name (N), New_N, Instantiating));

         --  For operators, we must copy the right operand

         elsif Nkind (N) in N_Op then

            Set_Right_Opnd (New_N,
              Copy_Generic_Node (Right_Opnd (N), New_N, Instantiating));

            --  And for binary operators, the left operand as well

            if Nkind (N) in N_Binary_Op then
               Set_Left_Opnd (New_N,
                 Copy_Generic_Node (Left_Opnd (N), New_N, Instantiating));
            end if;
         end if;

      --  Special casing for stubs

      elsif Nkind (N) in N_Body_Stub then

         --  In any case, we must copy the specification or defining
         --  identifier as appropriate.

         if Nkind (N) = N_Subprogram_Body_Stub then
            Set_Specification (New_N,
              Copy_Generic_Node (Specification (N), New_N, Instantiating));

         else
            Set_Defining_Identifier (New_N,
              Copy_Generic_Node
                (Defining_Identifier (N), New_N, Instantiating));
         end if;

         --  If we are not instantiating, then this is where we load and
         --  analyze subunits, i.e. at the point where the stub occurs. A
         --  more permissivle system might defer this analysis to the point
         --  of instantiation, but this seems to complicated for now.

         if not Instantiating then
            declare
               Context      : List_Id;
               Subunit_Name : constant Unit_Name_Type := Get_Unit_Name (N);
               Subunit      : Node_Id;
               New_Subunit  : Node_Id;
               Unum         : Unit_Number_Type;
               New_Body     : Node_Id;
               Lib          : Node_Id;

            begin
               Unum := Load_Unit (Subunit_Name, False, N);

               --  If the proper body is not found, a warning message will
               --  be emitted when analyzing the stub, or later at the the
               --  point of instantiation. Here we just leave the stub as is.

               if Unum = No_Unit then
                  Subunits_Missing := True;
                  goto Subunit_Not_Found;
               end if;

               Subunit := Cunit (Unum);

               --  We must create a generic copy of the subunit, in order
               --  to perform semantic analysis on it, and we must replace
               --  the stub in the original generic unit with the subunit,
               --  in order to preserve non-local references within.

               --  Only the proper body needs to be copied. Library_Unit and
               --  context clause are simply inherited by the generic copy.
               --  Note that the copy (which may be recursive if there are
               --  nested subunits) must be done first, before attaching it
               --  to the enclosing generic.

               New_Body :=
                 Copy_Generic_Node
                   (Proper_Body (Unit (Subunit)),
                    Empty, Instantiating => False);

               --  Now place the original proper body in the original
               --  generic unit.

               Rewrite_Substitute_Tree (N, Proper_Body (Unit (Subunit)));
               Set_Was_Originally_Stub (N);

               --  Finally replace the body of the subunit with its copy,
               --  and make this new subunit into the library unit of the
               --  generic copy, which does not have stubs any longer.

               Set_Proper_Body (Unit (Subunit), New_Body);
               Set_Library_Unit (New_N, Subunit);
               Inherit_Context (Unit (Subunit), N);

            end;

         --  If we are instantiating, this must be an error case, since
         --  otherwise we would have replaced the stub node by the proper
         --  body that corresponds. So just ignore it in the copy (i.e.
         --  we have copied it, and that is good enough).

         else
            null;
         end if;

         <<Subunit_Not_Found>> null;

      --  If the node is a compilation unit, it is the subunit of a stub,
      --  which has been loaded already (see code below). In this case,
      --  the library unit field of N points to the parent unit (which
      --  is a compilation unit) and need not (and cannot!) be copied.

      --  When the proper body of the stub is analyzed, thie library_unit
      --  link is used to establish the proper context (see sem_ch10).

      --  The other fields of a compilation unit are copied as usual

      elsif Nkind (N) = N_Compilation_Unit then

         --  This code can only be executed when not instantiating, because
         --  in the copy made for an instantiation, the compilation unit
         --  node has disappeared at the point that a stub is replaced by
         --  its proper body.

         pragma Assert (not Instantiating);

         Set_Context_Items (New_N,
           Copy_Generic_List (Context_Items (N), New_N));

         Set_Unit (New_N,
           Copy_Generic_Node (Unit (N), New_N, False));

         Set_First_Inlined_Subprogram (New_N,
           Copy_Generic_Node
             (First_Inlined_Subprogram (N), New_N, False));

         Set_Pragmas_After (New_N,
           Copy_Generic_List (Pragmas_After (N), New_N));

      --  For an assignment node, the assignment is known to be semantically
      --  legal if we are instantiating the template. This avoids incorrect
      --  diagnostics in generated code.

      elsif Nkind (N) = N_Assignment_Statement then

         --  Copy name and expression fields in usual manner

         Set_Name (New_N,
           Copy_Generic_Node (Name (N), New_N, Instantiating));

         Set_Expression (New_N,
           Copy_Generic_Node (Expression (N), New_N, Instantiating));

         if Instantiating then
            Set_Assignment_OK (Name (New_N), True);
         end if;

      --  For a proper body, we must catch the case of a proper body that
      --  replaces a stub. This represents the point at which a separate
      --  compilation unit, and hence template file, may be referenced, so
      --  we must make a new source instantiation entry for the template
      --  of the subunit, and ensure that all nodes in the subunit are
      --  adjusted using this new source instantiation entry.

      elsif Nkind (N) in N_Proper_Body then

         declare
            Save_Adjustment : constant Sloc_Adjustment := S_Adjustment;

         begin
            if Instantiating and then Was_Originally_Stub (N) then
               Create_Instantiation_Source
                 (Instantiation_Node, Defining_Entity (N), S_Adjustment);
            end if;

            --  Now copy the fields of the proper body, using the new
            --  adjustment factor if one was needed as per test above.

            Set_Field1 (New_N, Copy_Generic_Descendant (Field1 (N)));
            Set_Field2 (New_N, Copy_Generic_Descendant (Field2 (N)));
            Set_Field3 (New_N, Copy_Generic_Descendant (Field3 (N)));
            Set_Field4 (New_N, Copy_Generic_Descendant (Field4 (N)));
            Set_Field5 (New_N, Copy_Generic_Descendant (Field5 (N)));

            --  Restore the original adjustment factor in case changed

            S_Adjustment := Save_Adjustment;
         end;

      --  For the remaining nodes, copy recursively their descendants.

      else
         Set_Field1 (New_N, Copy_Generic_Descendant (Field1 (N)));
         Set_Field2 (New_N, Copy_Generic_Descendant (Field2 (N)));
         Set_Field3 (New_N, Copy_Generic_Descendant (Field3 (N)));
         Set_Field4 (New_N, Copy_Generic_Descendant (Field4 (N)));
         Set_Field5 (New_N, Copy_Generic_Descendant (Field5 (N)));
      end if;

      return New_N;
   end Copy_Generic_Node;

   ------------------------------
   -- Check_Generic_Child_Unit --
   ------------------------------

   procedure Check_Generic_Child_Unit
     (Gen_Id           : Node_Id;
      Parent_Installed : in out Boolean)
   is
      Loc      : constant Source_Ptr := Sloc (Gen_Id);
      Gen_Par  : Entity_Id := Empty;
      Inst_Par : Entity_Id;
      E        : Entity_Id;
      S        : Node_Id;

      function Find_Generic_Child
        (Scop : Entity_Id;
         Id   : Node_Id)
         return Entity_Id;
      --  Search generic parent for possible child unit.

      function Find_Generic_Child
        (Scop : Entity_Id;
         Id   : Node_Id)
         return Entity_Id
      is
         E : Entity_Id;

      begin
         E := First_Entity (Scop);

         while Present (E) loop
            if Chars (E) = Chars (Id)
              and then Is_Child_Unit (E)
            then
               return E;
            end if;

            E := Next_Entity (E);
         end loop;

         return Empty;
      end Find_Generic_Child;

   begin
      --  If the name of the generic is given by a selected component, it
      --  may be the name of a generic child unit, and the prefix the name
      --  of an instance of the parent, in which case the child unit must be
      --  visible. If this instance is not in scope, it must be placed there
      --  and removed after instantiation, because what is being instantiated
      --  is not the original child, but the corresponding child present in
      --  the instance of the parent.
      --  If the child is instantiated within the parent, it can be given by
      --  a simple name. In this case the instance is already in scope, but
      --  the child generic must be recovered from the generic parent as well.

      if Nkind (Gen_Id) = N_Selected_Component then
         S := Selector_Name (Gen_Id);
         Analyze (Prefix (Gen_Id));
         Inst_Par := Entity (Prefix (Gen_Id));

         if Ekind (Inst_Par) = E_Package
           and then Present (Renamed_Object (Inst_Par))
         then
            Inst_Par := Renamed_Object (Inst_Par);
         end if;

         if Ekind (Inst_Par) = E_Package then
            if Nkind (Parent (Inst_Par)) = N_Package_Specification then
               Gen_Par := Generic_Parent (Parent (Inst_Par));

            elsif Nkind (Parent (Inst_Par)) = N_Defining_Program_Unit_Name
              and then
                Nkind (Parent (Parent (Inst_Par))) = N_Package_Specification
            then
               Gen_Par := Generic_Parent (Parent (Parent (Inst_Par)));
            end if;
         end if;

         if Present (Gen_Par) then

            --  The prefix denotes an instantiation. The entity itself
            --  may be a nested generic, or a child unit.

            E := Find_Generic_Child (Gen_Par, S);

            if Present (E) then
               Change_Selected_Component_To_Expanded_Name (Gen_Id);
               Set_Entity (Gen_Id, E);
               Set_Etype (Gen_Id, Etype (E));
               Set_Entity (S, E);
               Set_Etype (S, Etype (E));

               --  A common mistake is to replicate the naming scheme of
               --  a hierarchy by instantiating a generic child directly,
               --  rather than the implicit child in a parent instance:
               --
               --  generic .. package Gpar is ..
               --  generic .. package Gpar.Child is ..
               --  package Par is new Gpar ();

               --  with Gpar.Child;
               --  package Par.Child is new Gpar.Child ();
               --                           rather than Par.Child
               --
               --  In this case the instantiation is within Par, which is
               --  an instance, but Gpar does not denote Par because we are
               --  not IN the instance of Gpar, so this is illegal. The test
               --  below recognizes this particular case.

               if Is_Child_Unit (E)
                 and then not Comes_From_Source (Entity (Prefix (Gen_Id)))
                 and then (not In_Instance
                             or else Nkind (Parent (Parent (Gen_Id)))
                               = N_Compilation_Unit)
               then
                  Error_Msg_N
                    ("prefix of generic child unit must be instance of parent",
                      Gen_Id);
               end if;

               if not In_Open_Scopes (Inst_Par) then
                  Install_Parent (Inst_Par);
                  Parent_Installed := True;
               end if;
            else
               --  If the generic parent does not contain an entity that
               --  corresponds to the selector, the instance doesn't either.
               --  Analyzing the node will yield the appropriate error message.
               --  If the entity is not a child unit, then it is an inner
               --  generic in the parent.

               Analyze (Gen_Id);
            end if;

         else
            Analyze (Gen_Id);
         end if;

      elsif Ekind (Current_Scope) = E_Package
        and then Nkind (Parent (Current_Scope)) = N_Package_Specification
        and then Present (Generic_Parent (Parent (Current_Scope)))
      then
         E := Find_Generic_Child
                (Generic_Parent (Parent (Current_Scope)), Gen_Id);

         if Present (E) then
            Rewrite_Substitute_Tree (Gen_Id,
              Make_Expanded_Name (Loc,
                Chars         => Chars (E),
                Prefix        => New_Occurrence_Of (Current_Scope, Loc),
                Selector_Name => New_Occurrence_Of (E, Loc)));

            Set_Entity (Gen_Id, E);
            Set_Etype  (Gen_Id, Etype (E));
            Parent_Installed := False;      -- Already in scope.

         else
            Analyze (Gen_Id);
         end if;

      else
         Analyze (Gen_Id);
      end if;
   end Check_Generic_Child_Unit;

   --------------------------
   -- Contains_Instance_Of --
   --------------------------

   function Contains_Instance_Of
     (Inner : Entity_Id;
      Outer : Entity_Id;
      N     : Node_Id)
      return  Boolean
   is
      Elmt : Elmt_Id;

   begin
      --  Within a generic subprogram body, the scope is not generic, to
      --  allow for recursive subprograms. Use the declaration to determine
      --  whether this is a generic unit.

      if Ekind (Outer) /= E_Generic_Package
        and then (not Is_Subprogram (Outer)
                   or else Nkind (Get_Declaration_Node (Outer)) /=
                                        N_Generic_Subprogram_Declaration)
      then
         return False;

      else
         Elmt := First_Elmt (Inner_Instances (Inner));

         while Present (Elmt) loop

            if Node (Elmt) = Outer then
               Error_Msg_Node_2 := Inner;
               Error_Msg_NE
                 ("circular Instantiation: & instantiated in &!", N, Outer);
               return True;

            elsif Contains_Instance_Of (Node (Elmt), Outer, N) then
               Error_Msg_Node_2 := Inner;
               Error_Msg_NE
                 ("circular Instantiation: & instantiated in &!",
                   N, Node (Elmt));
               return True;
            end if;

            Elmt := Next_Elmt (Elmt);
         end loop;

         --  Indicate that Inner is being instantiated within Outer.

         Append_Elmt (Inner, Inner_Instances (Outer));
         return False;
      end if;
   end Contains_Instance_Of;

   ---------------------
   -- Get_Instance_Of --
   ---------------------

   function Get_Instance_Of (A : Entity_Id) return Entity_Id is
      Res : Assoc_Ptr := Generic_Renamings_Htable.Get (Chars (A));
   begin
      if Res /= Assoc_Null then
         return Generic_Renamings.Table (Res).Act_Id;
      else

         --  On exit, entity is not instantiated: not a generic parameter,
         --  or else parameter of an inner generic unit.

         return A;
      end if;
   end Get_Instance_Of;

   ----------------------------
   -- Denotes_Formal_Package --
   ----------------------------

   function Denotes_Formal_Package (Pack : Entity_Id) return Boolean is
      Par  : constant Entity_Id := Current_Instantiated_Parent.Act_Id;
      Scop : Entity_Id := Scope (Pack);
      E    : Entity_Id;

   begin
      if not In_Open_Scopes (Scop) then
         return False;

      elsif Ekind (Scop) = E_Generic_Package
        or else Nkind (Get_Declaration_Node (Scop))
          = N_Generic_Subprogram_Declaration
      then
         return True;

      elsif Nkind (Parent (Pack)) = N_Formal_Package_Declaration then
         return True;

      elsif No (Par) then
         return False;

      else
         --  Check whether this package is associated with a formal
         --  package of the enclosing instantiation. Iterate over the list
         --  of renamings.

         E := First_Entity (Par);

         while Present (E) loop

            if Ekind (E) /= E_Package
              or else Nkind (Parent (E)) /= N_Package_Renaming_Declaration
            then
               null;
            elsif Renamed_Object (E) = Par then
               return False;

            elsif Renamed_Object (E) = Pack then
               return True;
            end if;

            E := Next_Entity (E);
         end loop;

         return False;
      end if;
   end Denotes_Formal_Package;

   ------------------------------------
   -- Get_Package_Instantiation_Node --
   ------------------------------------

   function Get_Package_Instantiation_Node (A : Entity_Id) return Node_Id is
      Decl : Node_Id := Get_Declaration_Node (A);
      Inst : Node_Id;

   begin
      --  If the instantiation is a compilation unit, the instantiation node
      --  has been replaced with the package declaration for the instance.
      --  Otherwise the instantiation node appears after the declaration.
      --  If the entity is a formal package, the declaration may have been
      --  rewritten as a generic declaration (in the case of a formal with a
      --  box) or left as a formal package declaration if it has actuals, and
      --  is found with a forward search.

      if Nkind (Parent (Decl)) = N_Compilation_Unit then
         return Original_Node (Decl);

      elsif Nkind (Decl) = N_Generic_Package_Declaration
        and then Nkind (Original_Node (Decl)) = N_Formal_Package_Declaration
      then
         return Original_Node (Decl);

      else
         Inst := Next (Decl);

         while Nkind (Inst) /= N_Package_Instantiation
           and then Nkind (Inst) /= N_Formal_Package_Declaration
         loop
            Inst := Next (Inst);
         end loop;

         return Inst;
      end if;
   end Get_Package_Instantiation_Node;

   ------------------------
   -- Instantiate_Object --
   ------------------------

   function Instantiate_Object
     (Formal : Node_Id;
      Actual : Node_Id;
      Analyzed_Formal : Node_Id)
      return   List_Id
   is
      Formal_Id : constant Entity_Id  := Defining_Identifier (Formal);
      Type_Id   : constant Node_Id    := Subtype_Mark (Formal);
      Loc       : constant Source_Ptr := Sloc (Actual);
      Ftyp      : Entity_Id;
      Decl_Node : Node_Id;
      Subt_Decl : Node_Id := Empty;
      List      : List_Id := New_List;

   begin
      if Get_Instance_Of (Formal_Id) /= Formal_Id then
         Error_Msg_N ("duplicate instantiation of generic parameter", Actual);
      end if;

      if Out_Present (Formal) then

         --  An IN OUT generic actual must be a name. The instantiation
         --  is a renaming declaration. The actual is the name being renamed.
         --  We use the actual directly, rather than a copy, because it is not
         --  used further in the list of actuals, and because a copy or a use
         --  of relocate_node is incorrect if the instance is nested within
         --  a generic. This may lead to problems with ASIS.
         --  To be revisited ???

         if No (Actual) then
            Error_Msg_NE
              ("missing actual for instantiation of &",
               Instantiation_Node, Formal_Id);
            Abandon_Instantiation (Instantiation_Node);
         end if;

         Decl_Node :=
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => New_Copy (Formal_Id),
             Subtype_Mark        => New_Copy (Type_Id),
             Name                => Actual);

         --  The analysis of the actual may produce insert_action nodes, so
         --  the declaration must have a context in which to attach them.

         Append (Decl_Node, List);
         Analyze (Actual);

         --  This check is performed here because Analyze_Object_Renaming
         --  will not check it when Comes_From_Source is False. Note
         --  though that the check for the actual being the name of an
         --  object will be performed in Analyze_Object_Renaming.

         if Is_Object_Reference (Actual)
           and then Is_Dependent_Component_Of_Mutable_Object (Actual)
         then
            Error_Msg_N
              ("illegal discriminant-dependent component for in out parameter",
               Actual);
         end if;

         --  The actual has to be resolved in order to check that it is
         --  a variable (due to cases such as F(1), where F returns
         --  access to an array, and for overloaded prefixes).

         Ftyp :=
           Get_Instance_Of (Etype (Defining_Identifier (Analyzed_Formal)));

         if Is_Private_Type (Ftyp)
           and then not Is_Private_Type (Etype (Actual))
           and then (Base_Type (Full_View (Ftyp)) = Base_Type (Etype (Actual))
                      or else Base_Type (Etype (Actual)) = Ftyp)
         then

            --  If the actual has the type of the full view of the formal,
            --  or else a non-private subtype of the formal, then
            --  the visibility of the formal type has changed. Add to the
            --  actuals a subtype declaration that will force the exchange
            --  of views in the body of the instance as well.

            Subt_Decl :=
              Make_Subtype_Declaration (Loc,
                 Defining_Identifier =>
                   Make_Defining_Identifier (Loc, New_Internal_Name ('P')),
                 Subtype_Indication  => New_Occurrence_Of (Ftyp, Loc));

            Prepend (Subt_Decl, List);

            Append_Elmt (Full_View (Ftyp), Exchanged_Views);
            Exchange_Declarations (Ftyp);
         end if;

         Resolve (Actual, Ftyp);

         if not Is_Variable (Actual) or else Paren_Count (Actual) > 0 then
            Error_Msg_NE
              ("actual for& must be a variable", Actual, Formal_Id);
         end if;

         Note_Possible_Modification (Actual);

      else
         --  The instantiation of a generic formal in-parameter
         --  is a constant declaration. The actual is the expression for
         --  that declaration.

         if Present (Actual) then

            Decl_Node := Make_Object_Declaration (Loc,
              Defining_Identifier => New_Copy (Formal_Id),
              Constant_Present => True,
              Object_Definition => New_Copy (Type_Id),
              Expression => Actual);

            Append (Decl_Node, List);
            Analyze (Actual);

            Freeze_Before (Instantiation_Node, Etype (Expression (Decl_Node)));

         elsif Present (Expression (Formal)) then

            --  Use default to construct declaration.

            Decl_Node := Make_Object_Declaration (Loc,
              Defining_Identifier => New_Copy (Formal_Id),
              Constant_Present => True,
              Object_Definition => New_Copy (Type_Id),
              Expression => New_Copy_Tree (Expression (Formal)));

            Append (Decl_Node, List);
            Set_Analyzed (Expression (Decl_Node), False);

         else
            Error_Msg_NE
              ("missing actual for instantiation of &",
               Instantiation_Node, Formal_Id);
            Abandon_Instantiation (Instantiation_Node);
         end if;

      end if;

      return List;
   end Instantiate_Object;

   --------------------------------
   -- Instantiate_Formal_Package --
   --------------------------------

   function Instantiate_Formal_Package
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id)
      return   List_Id
   is
      Act_Pkg     : Entity_Id;
      Formal_Pack : Entity_Id;
      Decls       : List_Id;
      Loc         : constant Source_Ptr := Sloc (Actual);
      Nod         : Node_Id;
      Parent_Spec : Node_Id;

      function Formal_Entity
        (F       : Node_Id;
         Act_Ent : Entity_Id)
         return    Entity_Id;
      --  Returns the entity associated with the given formal F. In the
      --  case where F is a formal package, this function will iterate
      --  through all of F's formals and enter map associations from the
      --  actuals occurring in the formal package's corresponding actual
      --  package (obtained via Act_Ent) to the formal package's formal
      --  parameters. This function is called recursively for arbitrary
      --  levels of formal packages.

      -------------------
      -- Formal_Entity --
      -------------------

      function Formal_Entity
        (F       : Node_Id;
         Act_Ent : Entity_Id)
         return    Entity_Id
      is
         Orig_Node : Node_Id := F;

      begin
         case Nkind (F) is
            when N_Formal_Object_Declaration =>
               return Defining_Identifier (F);

            when N_Formal_Type_Declaration =>
               return Defining_Identifier (F);

            when N_Formal_Subprogram_Declaration =>
               return Defining_Unit_Name (Specification (F));

            when N_Formal_Package_Declaration |
                 N_Generic_Package_Declaration =>

               if Nkind (F) = N_Generic_Package_Declaration then
                  Orig_Node := Original_Node (F);
               end if;

               declare
                  Actual_Ent  : Entity_Id := First_Entity (Act_Ent);
                  Formal_Node : Node_Id;
                  Formal_Ent  : Entity_Id;

                  Gen_Decl : Node_Id :=
                               Get_Declaration_Node
                                 (Entity (Name (Orig_Node)));
                  Formals  : List_Id :=
                               Generic_Formal_Declarations (Gen_Decl);

               begin
                  if Present (Formals) then
                     Formal_Node := First_Non_Pragma (Formals);
                  end if;

                  --  As for the loop further below, this loop is making
                  --  a probably invalid assumption about the correspondence
                  --  between formals and actuals and eventually needs to
                  --  corrected to account for cases where the formals are
                  --  not synchronized and in one-to-one correspondence
                  --  with actuals. ???

                  while Present (Actual_Ent)
                    and then Present (Formal_Node)
                    and then Actual_Ent /= First_Private_Entity (Act_Ent)
                  loop
                     --  ???  Are the following calls also needed here:
                     --
                     --  Set_Is_Private (Actual_Ent, False);
                     --  Set_Is_Potentially_Use_Visible
                     --    (Actual_Ent, In_Use (Act_Ent));

                     Formal_Ent := Formal_Entity (Formal_Node, Actual_Ent);
                     if Present (Formal_Ent) then
                        Set_Instance_Of (Formal_Ent, Actual_Ent);
                     end if;
                     Formal_Node := Next_Non_Pragma (Formal_Node);

                     Actual_Ent := Next_Entity (Actual_Ent);
                  end loop;
               end;

               return Defining_Identifier (Orig_Node);

            when N_Use_Package_Clause =>
               return Empty;

            when N_Use_Type_Clause =>
               return Empty;

            --  For now we return empty for all other encountered forms
            --  of declaration because there are some cases of nonformal
            --  sorts of declaration that can show up (e.g., when array
            --  formals are present). Since it's not clear what kinds
            --  can appear among the formals, we won't raise failure
            --  for now. ???

            when others =>
               return Empty;

            --  ??? when others => pragma Assert (False); return Empty;

         end case;
      end Formal_Entity;

   --  Start of processing for Instantiate_Formal_Package

   begin
      Analyze (Actual);

      if not Is_Entity_Name (Actual)
        or else  Ekind (Entity (Actual)) /= E_Package
      then
         Error_Msg_N
           ("expect package instance to instantiate formal", Actual);
         Abandon_Instantiation (Actual);
         raise Program_Error;

      else
         Act_Pkg := Entity (Actual);

         --  The actual may be a renamed package, or an outer generic
         --  formal package whose instantiation is converted into a renaming.

         if Present (Renamed_Object (Act_Pkg)) then
            Act_Pkg := Renamed_Object (Act_Pkg);
         end if;

         if Nkind (Analyzed_Formal) = N_Formal_Package_Declaration then
            Formal_Pack := Get_Instance_Of (Entity (Name (Analyzed_Formal)));
         else
            Formal_Pack :=
              Generic_Parent (Specification (Analyzed_Formal));
         end if;

         if Nkind (Parent (Act_Pkg)) = N_Defining_Program_Unit_Name then
            Parent_Spec := Specification (Get_Declaration_Node (Act_Pkg));
         else
            Parent_Spec := Parent (Act_Pkg);
         end if;

         if Generic_Parent (Parent_Spec) /= Formal_Pack then
            Error_Msg_N
              ("expect package instance to instantiate formal", Actual);
            Abandon_Instantiation (Actual);
         end if;

         Set_Instance_Of (Defining_Identifier (Formal), Act_Pkg);

         Nod :=
           Make_Package_Renaming_Declaration (Loc,
             Defining_Unit_Name => New_Copy (Defining_Identifier (Formal)),
             Name               => New_Reference_To (Act_Pkg, Loc));

         Set_Associated_Formal_Package (Defining_Unit_Name (Nod),
           Defining_Identifier (Formal));
         Decls := New_List (Nod);

         --  If the formal F has a box, then the generic declarations are
         --  visible in the generic G. In an instance of G, the corresponding
         --  entities in the actual for F (which are the actuals for the
         --  instantiation of the generic that F denotes) must also be made
         --  visible for analysis of the current instance. On exit from the
         --  current instance, those entities are made private again. If the
         --  actual is currently in use, these entities are also use-visible.

         --  The loop through the actual entities also steps through the
         --  formal entities and enters associations from formals to
         --  actuals into the renaming map. This is necessary to properly
         --  handle checking of actual parameter associations for later
         --  formals that depend on actuals declared in the formal package.
         --
         --  This processing needs to be reviewed at some point because
         --  it is probably not entirely correct as written. For example
         --  there may not be a strict one-to-one correspondence between
         --  actuals and formals and this loop is currently assuming that
         --  there is. ???

         if Box_Present (Formal) then
            declare
               Actual_Ent  : Entity_Id := First_Entity (Act_Pkg);
               Formal_Node : Node_Id;
               Formal_Ent  : Entity_Id;
               Gen_Decl    : Node_Id := Get_Declaration_Node (Formal_Pack);
               Formals     : List_Id := Generic_Formal_Declarations (Gen_Decl);

            begin
               if Present (Formals) then
                  Formal_Node := First_Non_Pragma (Formals);
               end if;

               while Present (Actual_Ent)
                 and then Actual_Ent /= First_Private_Entity (Act_Pkg)
               loop
                  Set_Is_Private (Actual_Ent, False);
                  Set_Is_Potentially_Use_Visible
                    (Actual_Ent, In_Use (Act_Pkg));

                  if Present (Formal_Node) then
                     Formal_Ent := Formal_Entity (Formal_Node, Actual_Ent);

                     if Present (Formal_Ent) then
                        Set_Instance_Of (Formal_Ent, Actual_Ent);
                     end if;

                     Formal_Node := Next_Non_Pragma (Formal_Node);
                  end if;

                  Actual_Ent := Next_Entity (Actual_Ent);
               end loop;
            end;

         else
            --  If the formal is not declared with a box, reanalyze it as
            --  an instantiation, to verify the matching rules of 12.7. The
            --  actual checks are performed after the generic associations
            --  been analyzed.

            declare
               I_Pack : constant Entity_Id :=
                          Make_Defining_Identifier (Sloc (Actual),
                            Chars => New_Internal_Name  ('P'));
               Decl   : Node_Id;

            begin
               Set_Is_Internal (I_Pack);

               Append (
                 Make_Package_Instantiation (Sloc (Actual),
                   Defining_Unit_Name => I_Pack,
                   Name => New_Occurrence_Of (Formal_Pack, Sloc (Actual)),
                   Generic_Associations =>
                     Generic_Associations (Formal)),
              Decls);
            end;
         end if;

         return Decls;
      end if;

   end Instantiate_Formal_Package;

   ---------------------------
   -- Check_Formal_Packages --
   ---------------------------

   procedure Check_Formal_Packages (P_Id : Entity_Id) is
      E        : Entity_Id;
      Formal_P : Entity_Id;

   begin
      E := First_Entity (P_Id);

      --  Iterate through the declarations in the instance, looking for
      --  package renaming declarations that denote instances of formal
      --  packages. Stop when we find the renaming of the current package
      --  itself. The declaration for a formal package without a box is
      --  followed by an internal entity that repeats the instantiation.

      while Present (E) loop
         if Ekind (E) = E_Package then
            if Renamed_Object (E) = P_Id then
               exit;

            elsif Nkind (Parent (E)) /= N_Package_Renaming_Declaration then
               null;

            elsif not Box_Present (Parent (Associated_Formal_Package (E))) then
               Formal_P := Next_Entity (E);
               Check_Formal_Package_Instance (Formal_P, E);
            end if;
         end if;

         E := Next_Entity (E);
      end loop;
   end Check_Formal_Packages;

   -----------------------------------
   -- Check_Formal_Package_Instance --
   -----------------------------------

   --  If the formal has specific parameters, they must match those of the
   --  actual. both of them are instances, and the renaming declarations
   --  for their formal parameters appear in the same order in both.
   --  The analyzed formal has been analyzed in the context of the current
   --  instance.

   procedure Check_Formal_Package_Instance
     (Form_Pkg : Entity_Id;
      Act_Pkg  : Entity_Id)
   is
      E1 : Entity_Id := First_Entity (Act_Pkg);
      E2 : Entity_Id := First_Entity (Form_Pkg);
      Expr1       : Node_Id;
      Expr2       : Node_Id;

      procedure Check_Mismatch (B : Boolean);

      --  Common error routine for mismatch between the parameters of
      --  the actual instance and those of the formal package.

      procedure Check_Mismatch (B : Boolean) is
      begin
         if B then
            Error_Msg_NE (
              "actual for & in actual instance does not match formal",
              Parent (Act_Pkg), E1);
         end if;
      end Check_Mismatch;

   --  Start of processing for Check_Formal_Package_Instance

   begin
      while Present (E1)
        and then Present (E2)
      loop
         exit when Ekind (E1) = E_Package
           and then Renamed_Entity (E1) = Renamed_Entity (Act_Pkg);

         if Is_Type (E1) then

            --  Subtypes must statically match. E1 and E2 are the
            --  local entities that are subtypes of the actuals.

            Check_Mismatch
              (not Is_Type (E2)
                or else Etype (E1) /= Etype (E2)
                or else not Subtypes_Statically_Match (E1, E2));

         elsif Ekind (E1) = E_Constant then

            --  IN parameters must denote the same static value, or
            --  the same constant, or the literal null.

            Expr1 := Expression (Parent (E1));

            if Ekind (E2) /= E_Constant then
               Check_Mismatch (True);
            else
               Expr2 := Expression (Parent (E2));
            end if;

            if Is_Static_Expression (Expr1) then
               if not Is_Static_Expression (Expr2) then
                  Check_Mismatch (True);

               elsif Is_Integer_Type (Etype (E1)) then

                  declare
                     V1 : Uint := Expr_Value (Expr1);
                     V2 : Uint := Expr_Value (Expr2);
                  begin
                     Check_Mismatch (V1 /= V2);
                  end;

               elsif Is_Real_Type (Etype (E1)) then

                  declare
                     V1 : Ureal := Expr_Value_R (Expr1);
                     V2 : Ureal := Expr_Value_R (Expr2);
                  begin
                     Check_Mismatch (V1 /= V2);
                  end;

               elsif Is_String_Type (Etype (E1))
                 and then Nkind (Expr1) = N_String_Literal
               then

                  if Nkind (Expr2) /= N_String_Literal then
                     Check_Mismatch (True);
                  else
                     Check_Mismatch
                       (String_Equal (Strval (Expr1), Strval (Expr2)));
                  end if;
               end if;

            elsif Is_Entity_Name (Expr1) then
               if Is_Entity_Name (Expr2) then
                  if Entity (Expr1) = Entity (Expr2) then
                     null;

                  elsif Ekind (Entity (Expr2)) = E_Constant
                     and then Is_Entity_Name (Constant_Value (Entity (Expr2)))
                     and then
                      Entity (Constant_Value (Entity (Expr2))) = Entity (Expr1)
                  then
                     null;
                  else
                     Check_Mismatch (True);
                  end if;
               else
                  Check_Mismatch (True);
               end if;

            elsif Nkind (Expr1) = N_Null then
               Check_Mismatch (Nkind (Expr1) /= N_Null);

            else
               Check_Mismatch (True);
            end if;

         elsif Ekind (E1) = E_Variable
           or else Ekind (E1) = E_Package
         then
            Check_Mismatch
              (Ekind (E1) /= Ekind (E2)
                or else Renamed_Object (E1) /= Renamed_Object (E2));

         elsif Is_Overloadable (E1) then

            --  Verify that the names of the  entities match.
            --  What if actual is an attribute ???

            Check_Mismatch
              (Ekind (E2) /= Ekind (E1) or else (Alias (E1)) /= Alias (E2));

         else
            pragma Assert (False);
            null;
         end if;

         E1 := Next_Entity (E1);
         E2 := Next_Entity (E2);
      end loop;
   end Check_Formal_Package_Instance;

   -----------------------------------
   -- Instantiate_Formal_Subprogram --
   -----------------------------------

   function Instantiate_Formal_Subprogram
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id)
      return Node_Id
   is
      Loc        : Source_Ptr := Sloc (Instantiation_Node);
      Formal_Sub : constant Entity_Id :=
                     Defining_Unit_Name (Specification (Formal));
      Analyzed_S : constant Entity_Id :=
                     Defining_Unit_Name (Specification (Analyzed_Formal));
      Decl_Node  : Node_Id;
      Nam        : Node_Id;
      New_Spec   : Node_Id;

      procedure Valid_Actual_Subprogram (Act : Node_Id);
      --  Perform legality check and raise exception on failure.

      procedure Valid_Actual_Subprogram (Act : Node_Id) is
      begin
         if not Is_Entity_Name (Act)
           and then Nkind (Act) /= N_Operator_Symbol
           and then Nkind (Act) /= N_Attribute_Reference
           and then Nkind (Act) /= N_Selected_Component
           and then Nkind (Act) /= N_Indexed_Component
           and then Nkind (Act) /= N_Character_Literal
         then
            if Etype (Act) /= Any_Type then
               Error_Msg_NE
                 ("Expect subprogram name to instantiate &",
                  Instantiation_Node, Formal_Sub);
            end if;

            --  In any case, instantiation cannot continue.

            Abandon_Instantiation (Instantiation_Node);
         end if;
      end Valid_Actual_Subprogram;

   --  Start of processing for Instantiate_Formal_Subprogram

   begin
      New_Spec := New_Copy_Tree (Specification (Formal));

      --  Create new entity for the actual (New_Copy_Tree does not).

      Set_Defining_Unit_Name
        (New_Spec, Make_Defining_Identifier (Loc, Chars (Formal_Sub)));

      --  Find entity of actual. If the actual is an attribute reference, it
      --  cannot be resolved here (its formal is missing) but is handled
      --  instead in Attribute_Renaming. If the actual is overloaded, it is
      --  fully resolved subsequently, when the renaming declaration for the
      --  formal is analyzed.

      if Present (Actual) then
         Loc := Sloc (Actual);
         Set_Sloc (New_Spec, Loc);

         if Nkind (Actual) = N_Operator_Symbol then
            Find_Direct_Name (Actual);

         elsif Nkind (Actual) /= N_Attribute_Reference then
            Analyze (Actual);
         end if;

         Valid_Actual_Subprogram (Actual);
         Nam := Actual;

      elsif Present (Default_Name (Formal)) then

         if Nkind (Default_Name (Formal)) /= N_Attribute_Reference
           and then Nkind (Default_Name (Formal)) /= N_Selected_Component
           and then Nkind (Default_Name (Formal)) /= N_Indexed_Component
           and then Nkind (Default_Name (Formal)) /= N_Character_Literal
           and then Present (Entity (Default_Name (Formal)))
         then
            Nam := New_Occurrence_Of (Entity (Default_Name (Formal)), Loc);
         else
            Nam := New_Copy (Default_Name (Formal));
            Set_Sloc (Nam, Loc);
         end if;

      elsif Box_Present (Formal) then

         --  Actual is resolved at the point of instantiation.

         Nam := Make_Identifier (Loc, Chars (Formal_Sub));

      else
         Error_Msg_NE
           ("missing actual for instantiation of &",
                                 Instantiation_Node, Formal_Sub);
         Abandon_Instantiation (Instantiation_Node);
      end if;

      Decl_Node :=
        Make_Subprogram_Renaming_Declaration (Loc,
          Specification => New_Spec,
          Name => Nam);

      --  The generic instantiation freezes the actual. This can only be
      --  done once the actual is resolved, in the analysis of the renaming
      --  declaration. To indicate that must be done, we set the corresponding
      --  spec of the node to point to the formal subprogram declaration.

      Set_Corresponding_Spec (Decl_Node, Analyzed_Formal);

      --  We cannot analyze the renaming declaration, and thus find the
      --  actual, until the all the actuals are assembled in the instance.
      --  For subsequent checks of other actuals, indicate the node that
      --  will hold the instance of this formal.

      Set_Instance_Of (Analyzed_S, Nam);
      return Decl_Node;
   end Instantiate_Formal_Subprogram;

   ----------------------
   -- Find_Actual_Type --
   ----------------------

   function Find_Actual_Type
     (Typ       : Entity_Id;
      Gen_Scope : Entity_Id)
      return      Entity_Id
   is
      T : Entity_Id;

   begin
      if not Is_Child_Unit (Gen_Scope) then
         return Get_Instance_Of (Typ);

      elsif not Is_Generic_Type (Typ)
        or else Scope (Typ) = Gen_Scope
      then
         return Get_Instance_Of (Typ);

      else
         T := Current_Entity (Typ);
         while Present (T) loop
            if In_Open_Scopes (Scope (T)) then
               return T;
            end if;

            T := Homonym (T);
         end loop;

         return Typ;
      end if;
   end Find_Actual_Type;

   ----------------------
   -- Instantiate_Type --
   ----------------------

   function Instantiate_Type
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id)
      return   Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (Actual);
      Gen_T     : constant Entity_Id  := Defining_Identifier (Formal);
      A_Gen_T   : constant Entity_Id  := Defining_Identifier (Analyzed_Formal);
      Ancestor  : Entity_Id;
      Def       : constant Node_Id    := Formal_Type_Definition (Formal);
      Act_T     : Entity_Id;
      Decl_Node : Node_Id;

      procedure Validate_Array_Type_Instance;
      procedure Validate_Access_Subprogram_Instance;
      procedure Validate_Access_Type_Instance;
      procedure Validate_Derived_Type_Instance;
      procedure Validate_Private_Type_Instance;

      --  These procedures perform validation tests for the named case

      function Subtypes_Match (Gen_T, Act_T : Entity_Id) return Boolean;
      --  Check that base types are the same and that the subtypes match
      --  statically. Used in several of the above.

      --------------------
      -- Subtypes_Match --
      --------------------

      function Subtypes_Match (Gen_T, Act_T : Entity_Id) return Boolean is
         T : constant Entity_Id := Get_Instance_Of (Gen_T);

      begin
         return (Base_Type (T) = Base_Type (Act_T)
--                  and then Is_Constrained (T) = Is_Constrained (Act_T)
--                  commented out by RBKD ???
                  and then Subtypes_Statically_Match (T, Act_T))

           or else (Is_Class_Wide_Type (Gen_T)
                     and then Is_Class_Wide_Type (Act_T)
                     and then
                       Subtypes_Match (
                         Get_Instance_Of (Root_Type (Gen_T)),
                         Root_Type (Act_T)));
      end Subtypes_Match;

      ----------------------------------
      -- Validate_Array_Type_Instance --
      ----------------------------------

      procedure Validate_Array_Type_Instance is
         I1 : Node_Id;
         I2 : Node_Id;
         T2 : Entity_Id;

         function Formal_Dimensions return Int;
         --  Count number of dimensions in array type formal

         function Formal_Dimensions return Int is
            Num   : Int := 0;
            Index : Node_Id;

         begin
            if Nkind (Def) = N_Constrained_Array_Definition then
               Index := First (Discrete_Subtype_Definitions (Def));
            else
               Index := First (Subtype_Marks (Def));
            end if;

            while Present (Index) loop
               Num := Num + 1;
               Index := Next_Index (Index);
            end loop;

            return Num;
         end Formal_Dimensions;

      begin
         if not Is_Array_Type (Act_T) then
            Error_Msg_NE
              ("expect array type in instantiation of &", Actual, Gen_T);
            Abandon_Instantiation (Actual);

         elsif Nkind (Def) = N_Constrained_Array_Definition then
            if not (Is_Constrained (Act_T)) then
               Error_Msg_NE
                 ("expect constrained array in instantiation of &",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         else
            if Is_Constrained (Act_T) then
               Error_Msg_NE
                 ("expect unconstrained array in instantiation of &",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;
         end if;

         if Formal_Dimensions /= Number_Dimensions (Act_T) then
            Error_Msg_NE
              ("dimensions of actual do not match formal &", Actual, Gen_T);
            Abandon_Instantiation (Actual);
         end if;

         I1 := First_Index (A_Gen_T);
         I2 := First_Index (Act_T);

         for I in 1 .. Formal_Dimensions loop

            --  If the indices of the actual were given by a subtype_mark,
            --  the index was transformed into a range attribute. Retrieve
            --  the original type mark for checking.

            if Is_Entity_Name (Original_Node (I2)) then
               T2 := Entity (Original_Node (I2));
            else
               T2 := Etype (I2);
            end if;

            if not Subtypes_Match
              (Find_Actual_Type (Etype (I1), Scope (A_Gen_T)), T2)
            then
               Error_Msg_NE
                 ("index types of actual do not match those of formal &",
                    Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

            I1 := Next_Index (I1);
            I2 := Next_Index (I2);
         end loop;

         if not Subtypes_Match (
            Find_Actual_Type (Component_Type (A_Gen_T), Scope (A_Gen_T)),
            Component_Type (Act_T))
         then
            Error_Msg_NE
              ("component subtype of actual does not match that of formal &",
                 Actual, Gen_T);
            Abandon_Instantiation (Actual);
         end if;

         if Has_Aliased_Components (A_Gen_T)
           and then not Has_Aliased_Components (Act_T)
         then
            Error_Msg_NE
              ("actual must have aliased components to match formal type &",
                 Actual, Gen_T);
         end if;


      end Validate_Array_Type_Instance;

      -----------------------------------
      -- Validate_Access_Type_Instance --
      -----------------------------------

      procedure Validate_Access_Type_Instance is
         Desig_Type : Entity_Id := Get_Instance_Of (Designated_Type (A_Gen_T));
      begin
         if not Is_Access_Type (Act_T) then
            Error_Msg_NE
              ("expect access type in instantiation of &", Actual, Gen_T);
            Abandon_Instantiation (Actual);
         end if;

         if Is_Access_Constant (A_Gen_T) then
            if not Is_Access_Constant (Act_T) then
               Error_Msg_N
                 ("actual type must be access-to-constant type", Actual);
               Abandon_Instantiation (Actual);
            end if;
         else
            if Is_Access_Constant (Act_T) then
               Error_Msg_N
                 ("actual type must be access-to-variable type", Actual);
               Abandon_Instantiation (Actual);
            elsif Ekind (A_Gen_T) = E_General_Access_Type
               and then Ekind (Base_Type (Act_T)) /= E_General_Access_Type
            then
               Error_Msg_N ("actual must be general access type!", Actual);
               Error_Msg_NE ("add ALL to }!", Actual, Act_T);
               Abandon_Instantiation (Actual);
            end if;
         end if;

         if not Subtypes_Match
           (Desig_Type, Designated_Type (Act_T))
         then
            Error_Msg_NE
              ("designated type of actual does not match that of formal &",
                 Actual, Gen_T);
            Abandon_Instantiation (Actual);

         elsif Is_Access_Type (Designated_Type (Act_T))
           and then Is_Constrained (Designated_Type (Designated_Type (Act_T)))
               /= Is_Constrained (Designated_Type (Desig_Type))
         then
            Error_Msg_NE
              ("designated type of actual does not match that of formal &",
                 Actual, Gen_T);
            Abandon_Instantiation (Actual);
         end if;
      end Validate_Access_Type_Instance;

      ----------------------------------
      -- Validate_Subprogram_Instance --
      ----------------------------------

      procedure Validate_Access_Subprogram_Instance is
      begin
         if not Is_Access_Type (Act_T)
           or else Ekind (Designated_Type (Act_T)) /= E_Subprogram_Type
         then
            Error_Msg_NE
              ("expect access type in instantiation of &", Actual, Gen_T);
            Abandon_Instantiation (Actual);
         end if;

         Check_Mode_Conformant
           (Designated_Type (Act_T),
            Designated_Type (A_Gen_T),
            Actual,
            Get_Inst => True);

         if Ekind (Base_Type (Act_T)) = E_Access_Protected_Subprogram_Type then
            if Ekind (A_Gen_T) = E_Access_Subprogram_Type then
               Error_Msg_NE
                 ("protected access type not allowed for formal &",
                  Actual, Gen_T);
            end if;

         elsif Ekind (A_Gen_T) = E_Access_Protected_Subprogram_Type then
            Error_Msg_NE
              ("expect protected access type for formal &",
               Actual, Gen_T);
         end if;
      end Validate_Access_Subprogram_Instance;

      ------------------------------------
      -- Validate_Private_Type_Instance --
      ------------------------------------

      procedure Validate_Private_Type_Instance is
         Formal_Discr : Entity_Id;
         Actual_Discr : Entity_Id;
         Formal_Subt  : Entity_Id;

      begin
         if (Is_Limited_Type (Act_T)
              or else Is_Limited_Composite (Act_T))
           and then not Is_Limited_Type (A_Gen_T)
         then
            Error_Msg_NE
              ("actual for non-limited  & cannot be a limited type", Actual,
               Gen_T);
            Abandon_Instantiation (Actual);

         elsif Is_Indefinite_Subtype (Act_T)
            and then not Is_Indefinite_Subtype (A_Gen_T)
            and then Ada_95
         then
            Error_Msg_NE
              ("actual for & must be a definite subtype", Actual, Gen_T);

         elsif not Is_Tagged_Type (Act_T)
           and then Is_Tagged_Type (A_Gen_T)
         then
            Error_Msg_NE
              ("actual for & must be a tagged type", Actual, Gen_T);

         elsif Has_Discriminants (A_Gen_T) then
            if not Has_Discriminants (Act_T) then
               Error_Msg_NE
                 ("actual for & must have discriminants", Actual, Gen_T);
               Abandon_Instantiation (Actual);

            elsif Is_Constrained (Act_T) then
               Error_Msg_NE
                 ("actual for & must be unconstrained", Actual, Gen_T);
               Abandon_Instantiation (Actual);

            else
               Formal_Discr := First_Discriminant (A_Gen_T);
               Actual_Discr := First_Discriminant (Act_T);

               while Formal_Discr /= Empty loop
                  if Actual_Discr = Empty then
                     Error_Msg_NE
                       ("discriminants on actual do not match formal",
                        Actual, Gen_T);
                     Abandon_Instantiation (Actual);
                  end if;

                  Formal_Subt := Get_Instance_Of (Etype (Formal_Discr));

                  if Base_Type (Formal_Subt)
                    /= Base_Type (Etype (Actual_Discr))
                  then
                     Error_Msg_NE
                       ("types of actual discriminants must match formal",
                        Actual, Gen_T);
                     Abandon_Instantiation (Actual);

                  elsif not Subtypes_Statically_Match
                              (Formal_Subt, Etype (Actual_Discr))
                    and then Ada_95
                  then
                     Error_Msg_NE
                       ("subtypes of actual discriminants must match formal",
                        Actual, Gen_T);
                     Abandon_Instantiation (Actual);
                  end if;

                  Formal_Discr := Next_Discriminant (Formal_Discr);
                  Actual_Discr := Next_Discriminant (Actual_Discr);
               end loop;

               if Actual_Discr /= Empty then
                  Error_Msg_NE
                    ("discriminants on actual do not match formal",
                     Actual, Gen_T);
                  Abandon_Instantiation (Actual);
               end if;
            end if;

         end if;

         Ancestor := Gen_T;
      end Validate_Private_Type_Instance;

      ------------------------------------
      -- Validate_Derived_Type_Instance --
      ------------------------------------

      procedure Validate_Derived_Type_Instance is
         Actual_Discr   : Entity_Id;
         Ancestor_Discr : Entity_Id;

      begin

         --  If the parent type in the generic declaration is itself
         --  a previous formal type, then it is local to the generic
         --  and absent from the analyzed generic definition. In  that
         --  case the ancestor is the instance of the formal (which must
         --  have been instantiatied previously). Otherwise, the analyzed
         --  generic carries the parent type.

         if Is_Entity_Name (Subtype_Mark (Def))
           and then Present (Entity (Subtype_Mark (Def)))
         then
            Ancestor := Get_Instance_Of (Entity (Subtype_Mark (Def)));
         else
            Ancestor :=
              Get_Instance_Of (Root_Type (Get_Instance_Of (A_Gen_T)));
         end if;

         if not Is_Ancestor (Base_Type (Ancestor), Act_T) then
            Error_Msg_NE
               ("expect type derived from & in instantiation",
                Actual, Ancestor);
            Abandon_Instantiation (Actual);
         end if;

         --  It should not be necessary to check for unknown discriminants
         --  on Formal, but for some reason Has_Unknown_Discriminants is
         --  false for A_Gen_T, so Is_Indefinite_Subtype incorrectly
         --  returns False. This needs fixing. ???

         if not Is_Indefinite_Subtype (A_Gen_T)
           and then not Unknown_Discriminants_Present (Formal)
           and then Is_Indefinite_Subtype (Act_T)
         then
            Error_Msg_N
              ("actual subtype must be constrained", Actual);
            Abandon_Instantiation (Actual);
         end if;

         if not Unknown_Discriminants_Present (Formal) then
            if Is_Constrained (Ancestor) then
               if not Is_Constrained (Act_T) then
                  Error_Msg_N
                    ("actual subtype must be constrained", Actual);
                  Abandon_Instantiation (Actual);
               end if;

            --  Ancestor is unconstrained

            elsif Is_Constrained (Act_T) then
               if Ekind (Ancestor) = E_Access_Type
                 or else Is_Composite_Type (Ancestor)
               then
                  Error_Msg_N
                    ("actual subtype must be unconstrained", Actual);
                  Abandon_Instantiation (Actual);
               end if;

            --  A class-wide type is only allowed if the formal has
            --  unknown discriminants.

            elsif Is_Class_Wide_Type (Act_T)
              and then not Has_Unknown_Discriminants (Ancestor)
            then
               Error_Msg_NE
                 ("actual for & cannot be a class-wide type", Actual, Gen_T);
               Abandon_Instantiation (Actual);

            --  Otherwise, the formal and actual shall have the same
            --  number of discriminants and each discriminant of the
            --  actual must correspond to a discriminant of the formal.

            elsif Has_Discriminants (Act_T)
              and then Has_Discriminants (Ancestor)
            then
               Actual_Discr   := First_Discriminant (Act_T);
               Ancestor_Discr := First_Discriminant (Ancestor);

               while Present (Actual_Discr)
                 and then Present (Ancestor_Discr)
               loop
                  if Base_Type (Act_T) /= Base_Type (Ancestor) and then
                    not Present (Corresponding_Discriminant (Actual_Discr))
                  then
                     Error_Msg_NE
                       ("discriminant & does not correspond " &
                        "to ancestor discriminant", Actual, Actual_Discr);
                     Abandon_Instantiation (Actual);
                  end if;

                  Actual_Discr   := Next_Discriminant (Actual_Discr);
                  Ancestor_Discr := Next_Discriminant (Ancestor_Discr);
               end loop;

               if Present (Actual_Discr) or else Present (Ancestor_Discr) then
                  Error_Msg_NE
                    ("actual for & must have same number of discriminants",
                     Actual, Gen_T);
                  Abandon_Instantiation (Actual);
               end if;

            --  This case should be caught by the earlier check for
            --  for constrainedness, but the check here is added for
            --  completeness.

            elsif Has_Discriminants (Act_T) then
               Error_Msg_NE
                 ("actual for & must not have discriminants", Actual, Gen_T);
               Abandon_Instantiation (Actual);

            elsif Has_Discriminants (Ancestor) then
               Error_Msg_NE
                 ("actual for & must have known discriminants", Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

            if not Subtypes_Statically_Compatible (Act_T, Ancestor) then
               Error_Msg_N
                 ("constraint on actual is incompatible with formal", Actual);
               Abandon_Instantiation (Actual);
            end if;
         end if;

      end Validate_Derived_Type_Instance;

   --  Start of processing for Instantiate_Type

   begin
      if Get_Instance_Of (A_Gen_T) /= A_Gen_T then
         Error_Msg_N ("duplicate instantiation of generic type", Actual);
         return Error;

      elsif not Is_Entity_Name (Actual)
        or else not Is_Type (Entity (Actual))
      then
         Error_Msg_NE
           ("expect valid subtype mark to instantiate &", Actual, Gen_T);
         Abandon_Instantiation (Actual);

      else
         Act_T := Entity (Actual);

         if Ekind (Act_T) = E_Incomplete_Type then
            if No (Underlying_Type (Act_T)) then
               Error_Msg_N ("premature use of incomplete type", Actual);
               Abandon_Instantiation (Actual);
            else
               Act_T := Full_View (Act_T);
               Set_Entity (Actual, Act_T);
            end if;

         elsif Is_Private_Type (Act_T)
           and then not Is_Generic_Type (Act_T)
           and then not Is_Derived_Type (Act_T)
           and then No (Full_View (Root_Type (Act_T)))
         then
            Error_Msg_N ("premature use of private type", Actual);

         elsif Has_Private_Component (Act_T) then
            Error_Msg_N
              ("premature use of type with private component", Actual);
         end if;

         Set_Instance_Of (A_Gen_T, Act_T);

         --  If the type is generic, the class-wide type may also be used

         if Is_Tagged_Type (A_Gen_T)
           and then Is_Tagged_Type (Act_T)
           and then not Is_Class_Wide_Type (A_Gen_T)
         then
            Set_Instance_Of (Class_Wide_Type (A_Gen_T),
              Class_Wide_Type (Act_T));
         end if;

         if not Is_Abstract (A_Gen_T)
           and then Is_Abstract (Act_T)
         then
            Error_Msg_N
              ("actual of non-abstract formal cannot be abstract", Actual);
         end if;

         if Is_Scalar_Type (Gen_T) then
            Set_Instance_Of (Etype (A_Gen_T), Etype (Act_T));
         end if;
      end if;

      case Nkind (Def) is
         when N_Formal_Private_Type_Definition =>
            Validate_Private_Type_Instance;

         when N_Formal_Derived_Type_Definition =>
            Validate_Derived_Type_Instance;

         when N_Formal_Discrete_Type_Definition =>
            if not Is_Discrete_Type (Act_T) then
               Error_Msg_NE
                 ("expect discrete type in instantiation of&", Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Formal_Signed_Integer_Type_Definition =>
            if not Is_Signed_Integer_Type (Act_T) then
               Error_Msg_NE
                 ("expect signed integer type in instantiation of&",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Formal_Modular_Type_Definition =>
            if not Is_Modular_Integer_Type (Act_T) then
               Error_Msg_NE
                 ("expect modular type in instantiation of &", Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Formal_Floating_Point_Definition =>
            if not Is_Floating_Point_Type (Act_T) then
               Error_Msg_NE
                 ("expect float type in instantiation of &", Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Formal_Ordinary_Fixed_Point_Definition =>
            if not Is_Ordinary_Fixed_Point_Type (Act_T) then
               Error_Msg_NE
                 ("expect ordinary fixed point type in instantiation of &",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Formal_Decimal_Fixed_Point_Definition =>
            if not Is_Decimal_Fixed_Point_Type (Act_T) then
               Error_Msg_NE
                 ("expect decimal type in instantiation of &",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Array_Type_Definition =>
            Validate_Array_Type_Instance;

         when N_Access_To_Object_Definition =>
            Validate_Access_Type_Instance;

         when N_Access_Function_Definition |
              N_Access_Procedure_Definition =>
            Validate_Access_Subprogram_Instance;

         when others =>
            pragma Assert (False); null;

      end case;

      Decl_Node :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => New_Copy (Gen_T),
          Subtype_Indication  => New_Reference_To (Act_T, Loc));

      if Is_Private_Type (Act_T) then
         Set_Has_Private_View (Subtype_Indication (Decl_Node));
      end if;

      --  Flag actual derived types so their elaboration produces the
      --  appropriate renamings for the primitive operations of the ancestor.
      --  Flag actual for formal private types as well, to determine whether
      --  operations in the private part may override inherited operations.

      if Nkind (Def) = N_Formal_Derived_Type_Definition
        or else Nkind (Def) = N_Formal_Private_Type_Definition
      then
         Set_Generic_Parent_Type (Decl_Node, Ancestor);
      end if;

      return Decl_Node;
   end Instantiate_Type;

   -----------------------
   -- Move_Freeze_Nodes --
   -----------------------

   procedure Move_Freeze_Nodes
     (Out_Of : Entity_Id;
      After  : Node_Id;
      L      : List_Id)
   is
      Decl      : Node_Id;
      Next_Decl : Node_Id;
      Next_Node : Node_Id := After;
      Spec      : Node_Id;

      function Is_Outer_Type (T : Entity_Id) return Boolean;
      --  Check whether entity is declared in a scope external to that
      --  of the generic unit.

      function Is_Outer_Type (T : Entity_Id) return Boolean is
         Scop : Entity_Id := Scope (T);
      begin
         if Scope_Depth (Scop) < Scope_Depth (Out_Of) then
            return True;
         else
            while Scop /= Standard_Standard loop

               if Scop = Out_Of then
                  return False;
               else
                  Scop := Scope (Scop);
               end if;
            end loop;

            return True;
         end if;
      end Is_Outer_Type;

   begin
      if No (L) then
         return;
      end if;

      --  First remove the freeze nodes that may appear before all other
      --  declarations.

      Decl := First (L);

      while Present (Decl)
        and then Nkind (Decl) = N_Freeze_Entity
          and then Is_Outer_Type (Entity (Decl))
      loop
         Decl := Remove_Head (L);
         Insert_After (Next_Node, Decl);
         Set_Analyzed (Decl, False);
         Next_Node := Decl;
         Decl := First (L);
      end loop;

      --  Next scan the list of declarations and remove each freeze node that
      --  appears ahead of the current node.

      while Present (Decl) loop
         while Present (Next (Decl))
           and then Nkind (Next (Decl)) = N_Freeze_Entity
           and then Is_Outer_Type (Entity (Next (Decl)))
         loop
            Next_Decl := Remove_Next (Decl);
            Insert_After (Next_Node, Next_Decl);
            Set_Analyzed (Next_Decl, False);
            Next_Node := Next_Decl;
         end loop;

         --  If the declaration is a nested package or concurrent type, then
         --  recurse. Nested generic packages will have been processed from the
         --  inside out.

         if Nkind (Decl) = N_Package_Specification then
            Spec := Decl;

         elsif Nkind (Decl) = N_Task_Type_Declaration then
            Spec := Task_Definition (Decl);

         elsif Nkind (Decl) = N_Protected_Type_Declaration then
            Spec := Protected_Definition (Decl);

         else
            Spec := Empty;
         end if;

         if Present (Spec) then
            Move_Freeze_Nodes (Out_Of, After, Visible_Declarations (Spec));
            Move_Freeze_Nodes (Out_Of, After, Private_Declarations (Spec));
         end if;

         Decl := Next (Decl);

      end loop;
   end Move_Freeze_Nodes;

   ---------------------------
   -- Restore_Private_Views --
   ---------------------------

   procedure Restore_Private_Views
     (Pack_Id    : Entity_Id;
      Is_Package : Boolean := True)
   is
      M : Elmt_Id;
      E : Entity_Id;

   begin
      M := First_Elmt (Exchanged_Views);
      while Present (M) loop
         Exchange_Declarations (Node (M));
         M := Next_Elmt (M);
      end loop;

      --  Make the generic formal parameters private, and make the formal
      --  types into subtypes of the actuals again.

      E := First_Entity (Pack_Id);

      while Present (E) loop
         Set_Is_Private (E, True);

         if Is_Type (E)
           and then Nkind (Parent (E)) = N_Subtype_Declaration
         then
            Set_Is_Generic_Actual_Type (E, False);

         elsif Ekind (E) = E_Package then

            --  The end of the renaming list is the renaming of the generic
            --  package itself. If the instance is a subprogram, all entities
            --  in the corresponding package are renamings. If this entity is
            --  a formal package, make its own formals private as well. The
            --  actual in this case is itself the renaming of an instantation.
            --  If the entity is not a package renaming, it is the entity
            --  created to validate formal package actuals: ignore.

            --  If the actual is itself a formal package for the enclosing
            --  generic, or the actual for such a formal package, it remains
            --  visible after the current instance, and therefore nothing
            --  needs to be done either.

            if Is_Package
              and then Renamed_Object (E) = Pack_Id
            then
               exit;

            elsif Nkind (Parent (E)) /= N_Package_Renaming_Declaration then
               null;

            elsif Denotes_Formal_Package (Renamed_Object (E)) then
               null;

            else
               declare
                  Act_P : Entity_Id := Renamed_Object (E);
                  Id    : Entity_Id := First_Entity (Act_P);

               begin
                  while Present (Id)
                    and then Id /= First_Private_Entity (Act_P)
                  loop
                     Set_Is_Private (Id, True);
                     Set_Is_Potentially_Use_Visible (Id, In_Use (Act_P));
                     exit when Ekind (Id) = E_Package
                       and then Renamed_Object (Id) = Act_P;

                     Id := Next_Entity (Id);
                  end loop;
               end;
               null;
            end if;
         end if;

         E := Next_Entity (E);
      end loop;
   end Restore_Private_Views;

   ----------------------------
   -- Save_Global_References --
   ----------------------------

   procedure Save_Global_References (N : Node_Id) is
      Gen_Scope : Entity_Id;
      E         : Entity_Id;
      N2        : Node_Id;

      function Is_Global (E : Entity_Id) return Boolean;
      --  Check whether entity is defined outside of generic unit.
      --  Examine the scope of an entity, and the scope of the scope,
      --  etc, until we find either Standard, in which case the entity
      --  is global, or the generic unit itself, which indicates that
      --  the entity is local. If the entity is the generic unit itself,
      --  as in the case of a recursive call, or the enclosing generic unit,
      --  if different from the current scope, then it is local as well,
      --  because it will be replaced at the point of instantiation.

      procedure Reset_Entity (N : Node_Id);
      --  Save semantic information on global entity, so that it is not
      --  resolved again at instantiation time.

      procedure Save_Global_Descendant (D : Union_Id);
      --  Apply Save_Global_References recursively to the descendents of
      --  current node.

      procedure Save_References (N : Node_Id);
      --  This is the recursive procedure that does the work, once the
      --  enclosing generic scope has been established.

      ---------------
      -- Is_Global --
      ---------------

      function Is_Global (E : Entity_Id) return Boolean is
         Se  : Entity_Id := Scope (E);

      begin
         if E = Gen_Scope then
            return False;
         elsif E = Standard_Standard then
            return True;
         else
            while Se /= Gen_Scope loop
               if Se = Standard_Standard then
                  return true;
               else
                  Se := Scope (Se);
               end if;
            end loop;

            return False;
         end if;
      end Is_Global;

      ----------------------------
      -- Save_Global_Descendant --
      ----------------------------

      procedure Save_Global_Descendant (D : Union_Id) is
         N1 : Node_Id;

      begin
         if D in Node_Range then
            if D = Union_Id (Empty) then
               null;

            elsif Nkind (Node_Id (D)) /= N_Compilation_Unit then
               Save_References (Node_Id (D));
            end if;

         elsif D in List_Range then
            if D = Union_Id (No_List)
              or else Is_Empty_List (List_Id (D))
            then
               null;

            else
               N1 := First (List_Id (D));
               while Present (N1) loop
                  Save_References (N1);
                  N1 := Next (N1);
               end loop;
            end if;

         --  Element list or other non-node field, nothing to do

         else
            null;
         end if;
      end Save_Global_Descendant;

      ------------------
      -- Reset_Entity --
      ------------------

      procedure Reset_Entity (N : Node_Id) is

         procedure Set_Global_Type (N : Node_Id; N2 : Node_Id);
         --  The type of N2 is global to the generic unit. Save the
         --  type in the generic node.

         procedure Set_Global_Type (N : Node_Id; N2 : Node_Id) is
            Typ : constant Entity_Id := Etype (N2);

         begin
            Set_Etype (N, Typ);

            --  If not a private type, nothing else to do

            if not Is_Private_Type (Typ) then
               null;

            --  If it is a derivation of a private type in a context where
            --  no full view is needed, nothing to do either.

            elsif No (Full_View (Typ)) and then Typ /= Etype (Typ) then
               null;

            --  Otherwise mark the type for flipping and use the full_view
            --  when available.

            else
               Set_Has_Private_View (N);

               if Present (Full_View (Typ)) then
                  Set_Etype (N2, Full_View (Typ));
               end if;
            end if;
         end Set_Global_Type;

      --  Start of processing for Reset_Entity

      begin
         N2 := Associated_Node (N);
         E := Entity (N2);

         if Present (E) then
            if Is_Global (E) then
               Set_Global_Type (N, N2);

            elsif Nkind (N) = N_Op_Concat
              and then Is_Generic_Type (Etype (N2))
              and then (Etype (Right_Opnd (N2)) = Etype (N2)
                         or else Etype (Right_Opnd (N2)) = Etype (N2))
              and then Is_Intrinsic_Subprogram (E)
            then
               null;

            else
               --  Entity is local. Mark generic node as unresolved.
               --  Note that now it does not have an entity.

               Set_Associated_Node (N, Empty);
               Set_Etype  (N, Empty);
            end if;

         elsif Nkind (Parent (N)) = N_Selected_Component
           and then Nkind (Parent (N2)) = N_Expanded_Name
         then

            if Is_Global (Entity (Parent (N2))) then
               Change_Selected_Component_To_Expanded_Name (Parent (N));
               Set_Associated_Node (Parent (N), Parent (N2));
               Set_Global_Type (Parent (N), Parent (N2));

               Save_Global_Descendant (Field2 (N));
               Save_Global_Descendant (Field3 (N));

               --  If this is a reference to the current generic entity,
               --  replace it with a simple name. This is to avoid anomalies
               --  when the enclosing scope is also a generic unit, in which
               --  case the selected component will not resolve to the current
               --  unit within an instance of the outer one.

            elsif Entity (Parent (N2)) = Current_Scope then
               Rewrite_Substitute_Tree (Parent (N),
                 Make_Identifier (Sloc (N),
                   Chars => Chars (Selector_Name (Parent (N2)))));
            end if;

         else
            --  Entity is local. Reset in generic unit, so that node
            --  is resolved anew at the point of instantiation.

            Set_Associated_Node (N, Empty);
            Set_Etype (N, Empty);
         end if;
      end Reset_Entity;

      ----------------------
      --  Save_References --
      ----------------------

      --  This is the recursive procedure that does the work, once the
      --  enclosing generic scope has been established. We have to treat
      --  specially a number of node rewritings that are required by semantic
      --  processing and which change the kind of nodes in the generic copy:
      --  typically constant-folding, replacing an operator node by a string
      --  literal, or a selected component by an expanded name. In  each of
      --  those cases, the transformation is propagated to the generic unit.

      procedure Save_References (N : Node_Id) is

      begin
         if N = Empty then
            null;

         elsif (Nkind (N) = N_Character_Literal
                 or else Nkind (N) = N_Operator_Symbol)
         then
            if Nkind (N) = Nkind (Associated_Node (N)) then
               Reset_Entity (N);

            elsif Nkind (N) = N_Operator_Symbol
              and then Nkind (Associated_Node (N)) = N_String_Literal
            then
               Change_Operator_Symbol_To_String_Literal (N);
            end if;

         elsif Nkind (N) in N_Op then

            if Nkind (N) = Nkind (Associated_Node (N)) then
               Reset_Entity (N);

            else
               --  Node may be transformed into call to a user-defined operator

               N2 := Associated_Node (N);

               if Nkind (N2) = N_Function_Call then
                  E := Entity (Name (N2));

                  if Present (E)
                    and then Is_Global (E)
                  then
                     Set_Etype (N, Etype (N2));
                  else
                     Set_Associated_Node (N, Empty);
                     Set_Etype (N, Empty);
                  end if;

               elsif Nkind (N2) = N_Integer_Literal
                 or else Nkind (N2) = N_Real_Literal
                 or else Nkind (N2) = N_String_Literal
                 or else (Nkind (N2) = N_Identifier
                           and then
                          Ekind (Entity (N2)) = E_Enumeration_Literal)
               then
                  --  Operation was constant-folded, perform the same
                  --  replacement in generic.

                  Rewrite_Substitute_Tree (N, New_Copy (N2));
                  Set_Analyzed (N, False);
               end if;
            end if;

            --  Complete the check on operands.

            Save_Global_Descendant (Field2 (N));
            Save_Global_Descendant (Field3 (N));

         elsif Nkind (N) = N_Identifier then
            if Nkind (N) = Nkind (Associated_Node (N)) then

               --  If this is a discriminant reference, always save it.
               --  It is used in the instance to find the corresponding
               --  discriminant positionally rather than  by name.

               Set_Original_Discriminant
                 (N, Original_Discriminant (Associated_Node (N)));
               Reset_Entity (N);

            else
               N2 := Associated_Node (N);

               if Nkind (N2) = N_Function_Call then
                  E := Entity (Name (N2));

                  --  Name resolves to a call to parameterless function.
                  --  If original entity is global, mark node as resolved.

                  if Present (E)
                    and then Is_Global (E)
                  then
                     Set_Etype (N, Etype (N2));
                  else
                     Set_Associated_Node (N, Empty);
                     Set_Etype (N, Empty);
                  end if;

               elsif
                 Nkind (N2) = N_Integer_Literal or else
                 Nkind (N2) = N_Real_Literal    or else
                 Nkind (N2) = N_String_Literal
               then
                  --  Name resolves to named number that is constant-folded,
                  --  or to string literal from concatenation.
                  --  Perform the same replacement in generic.

                  Rewrite_Substitute_Tree (N, New_Copy (N2));
                  Set_Analyzed (N, False);

               elsif Nkind (N2) = N_Explicit_Dereference then

                  --  Check whether entity of prefix is global.

                  if Present (Entity (Prefix (N2)))
                    and then Is_Global (Entity (Prefix (N2)))
                  then
                     Rewrite_Substitute_Tree (N,
                       Make_Explicit_Dereference (Sloc (N),
                          Prefix => Make_Identifier (Sloc (N),
                            Chars => Chars (N))));
                     Set_Associated_Node (Prefix (N), Prefix (N2));

                  else
                     Set_Associated_Node (N, Empty);
                     Set_Etype (N, Empty);
                  end if;

               else
                  null;
               end if;
            end if;

         elsif Nkind (N) in N_Entity then
            null;

         else
            Save_Global_Descendant (Field1 (N));
            Save_Global_Descendant (Field2 (N));
            Save_Global_Descendant (Field3 (N));
            Save_Global_Descendant (Field4 (N));
            Save_Global_Descendant (Field5 (N));

         end if;
      end Save_References;

   --  Start of processing for Save_Global_References

   begin
      Gen_Scope := Current_Scope;

      --  If the generic unit is a child unit, references to entities in
      --  the parent are treated as local, because they will be resolved
      --  anew in the context of the instance of the parent.

      while Is_Child_Unit (Gen_Scope)
        and then Ekind (Scope (Gen_Scope)) = E_Generic_Package
      loop
         Gen_Scope := Scope (Gen_Scope);
      end loop;

      Save_References (N);
   end Save_Global_References;

   -------------------------
   -- Set_Associated_Node --
   -------------------------

   procedure Set_Associated_Node
     (Gen_Node  : Node_Id;
      Copy_Node : Node_Id)
   is
   begin
      Set_Node4 (Gen_Node, Copy_Node);
   end Set_Associated_Node;

   ---------------------
   -- Set_Instance_Of --
   ---------------------

   procedure Set_Instance_Of (A : Entity_Id; B : Entity_Id) is
   begin
      Generic_Renamings.Table (Generic_Renamings.Last) := (A, B, Assoc_Null);
      Generic_Renamings_Htable.Set (Generic_Renamings.Last);
      Generic_Renamings.Increment_Last;
   end Set_Instance_Of;

end Sem_Ch12;

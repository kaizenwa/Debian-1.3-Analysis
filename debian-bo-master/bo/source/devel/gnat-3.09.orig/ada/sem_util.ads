------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ U T I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.150 $                            --
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

--  Package containing utility procedures used throughout the semantics

with Einfo;  use Einfo;
with Types;  use Types;
with Uintp;  use Uintp;
with Urealp; use Urealp;

package Sem_Util is

   procedure Apply_Compile_Time_Constraint_Error
     (N   : Node_Id;
      Msg : String;
      Ent : Entity_Id  := Empty;
      Loc : Source_Ptr := No_Location);
   --  N is a subexpression which will raise constraint error when evaluated
   --  at runtime. Msg is a message that explains the reason for raising the
   --  exception. The last character is ? if the message is always a
   --  warning, even in Ada 95, and is not a ? if the message represents an
   --  illegality (because of violation of static expression rules) in Ada 95
   --  (but not in Ada 83). Typically this routine posts all messages at
   --  the Sloc of node N. However, if Loc /= No_Location, Loc is the Sloc
   --  used to output the message. After posting the appropriate messages,
   --  this routine replaces the expression with an N_Raise_Constraint_Error
   --  node. This node is marked as being static if the original node is
   --  static, but sets the flag Raises_Constraint_Error, preventing further
   --  evaluation. If Loc = No_Location, the error message may contain a }
   --  insertion character. This normally references Etype (N), unless the Ent
   --  argument is given explicitly, in which case it is used instead.

   function Build_Actual_Subtype
     (T    : Entity_Id;
      N    : Node_Or_Entity_Id)
      return Node_Id;
   --  Build an anonymous subtype for an entity or expression, using the
   --  bounds of the entity or the discriminants of the enclosing record.
   --  T is the type for which the actual subtype is required, and N is either
   --  a defining identifier, or any subexpression.

   function Build_Actual_Subtype_Of_Component
     (T    : Entity_Id;
      N    : Node_Id)
      return Node_Id;
   --  Determine whether a selected component has a type that depends on
   --  discriminants, and build actual subtype for it if so.

   function Build_Discriminal_Subtype_Of_Component
     (T    : Entity_Id)
      return Node_Id;
   --  Determine whether a record component has a type that depends on
   --  discriminants, and build actual subtype for it if so.

   procedure Check_Fully_Declared (T : Entity_Id; N : Node_Id);
   --  Verify that the full declaration of type T has been seen. If not,
   --  place error message on node N. Used in  object declarations, type
   --  conversions, qualified expressions.

   procedure Check_Unset_Reference (N : Node_Id);
   --  N is the node for an expression which occurs in a reference position,
   --  e.g. as the right side of an assignment. This procedure checks to see
   --  if the node is a reference to a variable entity where the entity has
   --  Not_Assigned set. If so, the Unset_Reference field is set if it is not
   --  the first occurrence. No warning is posted, instead warnings will be
   --  posted later by Check_Unset_Variables. The reason we do things that
   --  way is that if there are no assignments anywhere, we prefer to flag
   --  the entity, rather than a reference to it. Note that for the purposes
   --  of this routine, a type conversion or qualified expression whose
   --  expression is an entity is also processed. The reason that we do not
   --  process these at the point of occurrence is that both these constructs
   --  can occur in non-reference positions (e.g. as out parameters).

   procedure Check_Unset_Variables (E : Entity_Id);
   --  Called at the end of processing a declarative region. The entity E
   --  is the entity for the scope. All entities declared in the region,
   --  as indicated by First_Entity and the entity chain, are checked to
   --  see if they are variables for which warnings need to be posted for
   --  either no assignments, or a use before an assignment.

   procedure Check_VMS (Construct : Node_Id);
   --  Check that this is an OpenVMS port of GNAT, and if so, return
   --  with no effect, otherwise post an error noting this can only be
   --  used with OpenVMS ports. The argument is the construct in question
   --  and is used to post the error message.

   function Compile_Time_Constraint_Error
     (N   : Node_Id;
      Msg : String;
      Ent : Entity_Id  := Empty;
      Loc : Source_Ptr := No_Location)
      return Node_Id;
   --  Works like the procedure of the same except it doesn't modify any
   --  nodes, just generates a warning (or error) message. Returns the
   --  input argument N as the result. The caller is responsible for making
   --  sure that this node or some other appropriate node is indeed replaced
   --  by an N_Raise_Constraint_Error node to generate the constraint error.

   procedure Conditional_Delay (New_Ent, Old_Ent : Entity_Id);
   --  Sets the Has_Delayed_Freeze flag of New if the Delayed_Freeze flag
   --  of Old is set and Old has no yet been Frozen (i.e. Is_Frozen is false);

   function Current_Entity (N : Node_Id) return Entity_Id;
   --  Find the currently visible definition for a given identifier, that is to
   --  say the first entry in the visibility chain for the Chars of N.

   function Current_Entity_In_Scope (N : Node_Id) return Entity_Id;
   --  Find whether there is a previous definition for identifier N in the
   --  current scope. Because declarations for a scope are not necessarily
   --  contiguous (e.g. for packages) the first entry on the visibility chain
   --  for N is not necessarily in the current scope.

   function Current_Scope return Entity_Id;
   --  Get entity representing current scope

   function Defining_Entity (N : Node_Id) return Entity_Id;
   --  Given a declaration N, returns the associated defining entity. If
   --  the declaration has a specification, the entity is obtained from
   --  the specification. If the declaration has a defining unit name,
   --  then the defining entity is obtained from the defining unit name
   --  ignoring any child unit prefixes.

   function Designate_Same_Unit
     (Name1 : Node_Id;
      Name2 : Node_Id)
      return  Boolean;
   --  Return true if Name1 and Name2 designate the same unit name;
   --  each of these names is supposed to be a selected component name,
   --  an expanded name, a defining program unit name or an identifier

   function Enclosing_Generic_Body
     (E    : Entity_Id)
      return Node_Id;
   --  Returns the Node_Id associated with the innermost enclosing
   --  generic body, if any. If none, then returns Empty.

   procedure Ensure_Freeze_Node (E : Entity_Id);
   --  Make sure a freeze node is allocated for entity E. If necessary,
   --  build and initialize a new freeze node and set Has_Delayed_Freeze
   --  true for entity E.

   procedure Enter_Name (Def_Id : Node_Id);
   --  Insert new name in symbol table of current scope with check for
   --  duplications (error message is issued if a conflict is found)

   function Find_Corresponding_Discriminant
     (Id   : Node_Id;
      Typ  : Entity_Id)
      return Entity_Id;
   --  Because discriminants may have different names in a generic unit
   --  and in an instance, they are resolved positionally when possible.
   --  A reference to a discriminant carries the discriminant that it
   --  denotes when analyzed. Subsequent uses of this id on a different
   --  type denote the discriminant at the same position in this new type.

   function First_Actual (Node : Node_Id) return Node_Id;
   --  Node is an N_Function_Call or N_Procedure_Call_Statement node. The
   --  result returned is the first actual parameter in declaration order
   --  (not the order of parameters as they appeared in the source, which
   --  can be quite different as a result of the use of named parameters).
   --  Empty is returned for a call with no parameters. The procedure for
   --  iterating through the actuals in declaration order is to use this
   --  function to find the first actual, and then use Next_Actual to obtain
   --  the next actual in declaration order. Note that the value returned
   --  is always the expression (not the N_Parameter_Association nodes
   --  even if named association is used).

   function Full_Qualified_Name (E : Entity_Id) return String_Id;
   --  Generates the string literal corresponding to the E's full qualified
   --  name in upper case. An Ascii.NUL is appended as the last character

   function Get_Actual_Subtype (N : Node_Id) return Entity_Id;
   --  Given a node for an expression, obtain the actual subtype of the
   --  expression. In the case of a parameter where the formal is an
   --  unconstrained array or discriminated type, this will be the
   --  previously constructed subtype of the actual. Note that this is
   --  not quite the "Actual Subtype" of the RM, since it is always
   --  a constrained type, i.e. it is the subtype of the value of the
   --  actual. The actual subtype is also returned in other cases where
   --  it has already been constructed for an object. Otherwise the
   --  expression type is returned unchanged, except for the case of an
   --  unconstrained array type, where an actual subtype is created, using
   --  Insert_Actions if necessary to insert any associated actions.

   function Get_Default_External_Name (E : Entity_Id) return Node_Id;
   --  This is used to construct the string literal node representing a
   --  default external name, i.e. one that is constructed from the name
   --  of an entity, in this case the entity E. The name is cased according
   --  to the appropriate rules for the operating system in use.

   procedure Get_Index_Bounds (N : Node_Id; L, H : out Node_Id);
   --  This procedure assigns to L and H respectively the values of the
   --  low and high bounds of node N, which must be a range, subtype
   --  indication, or the name of a scalar subtype.

   function Get_Declaration_Node (Unit_Id : Entity_Id) return Node_Id;
   --  Unit_Id is the simple name of a unit, this function returns the
   --  corresponding xxx_Declaration node for the entity, or the subprogram
   --  body node for it. The unit may be a child unit with any number of
   --  ancestors.

   function Get_Name_Entity_Id (Id : Name_Id) return Entity_Id;
   --  An entity value is associated with each name in the name table. The
   --  Get_Name_Entity_Id function fetches the Entity_Id of this entity,
   --  which is the innermost visible entity with the given name. See the
   --  body of Sem_Ch8 for further details on handling of entity visibility.

   function Get_Referenced_Object (N : Node_Id) return Node_Id;
   --  Given a node, return the renamed object if the node represents
   --  a renamed object, otherwise return the node unchanged. The node
   --  may represent an arbitrary expression.

   function Has_Private_Component (Type_Id : Entity_Id) return Boolean;
   --  Check if a type has a (sub)component of a private type that has not
   --  yet received a full declaration.

   function Has_Tagged_Component (Typ : Entity_Id) return Boolean;
   --  Typ must be a composite type (array or record). This function is used
   --  to check if '=' has to be expanded into a bunch component comparaisons.

   function In_Instance (Body_Only : Boolean := False) return Boolean;
   --  Returns True if the current scope is within a generic instance. If
   --  Body_Only is True, then only returns True if within an instance body.

   function Is_Dependent_Component_Of_Mutable_Object
     (Object : Node_Id)
      return   Boolean;
   --  Returns True if Object is the name of a subcomponent that
   --  depends on discriminants of a variable whose nominal subtype
   --  is unconstrained and not indefinite, and the variable is
   --  not aliased.  Otherwise returns False.  The nodes passed
   --  to this function are assumed to denote objects.

   function Is_Aliased_View (Obj : Node_Id) return Boolean;
   --  Determine if Obj is an aliased view, i.e. the name of an
   --  object to which 'Access or 'Unchecked_Access can apply.

   function Is_Entity_Name (N : Node_Id) return Boolean;
   --  Test if the node N is the name of an entity (i.e. is an identifier,
   --  expanded name, or an attribute reference that returns an entity).

   function Is_False (U : Uint) return Boolean;
   --  The argument is a Uint value which is the Boolean'Pos value of a
   --  Boolean operand (i.e. is either 0 for False, or 1 for True). This
   --  function simply tests if it is False (i.e. zero)

   function Is_Fixed_Model_Number (U : Ureal; T : Entity_Id) return Boolean;
   --  Returns True iff the number U is a model number of the fixed-
   --  point type T, i.e. if it is an exact multiple of Small.

   function Is_Fully_Initialized_Type (Typ : Entity_Id) return Boolean;
   --  Typ is a type entity. This function returns true if this type is
   --  fully initialized, meaning that an object of the type is fully
   --  initialized. Note that initialization resulting from the use of
   --  pragma Normalized_Scalars does not count.

   function Is_Library_Level_Entity (E : Entity_Id) return Boolean;
   --  A library-level declaration is one that is accessible from Standard,
   --  i.e. a library unit or an entity declared in a library package.

   function Is_Object_Reference (N : Node_Id) return Boolean;
   --  Determines if the tree referenced by N represents an object. Both
   --  variable and constant objects return True (compare Is_Variable).

   function Is_Selector_Name (N : Node_Id) return Boolean;
   --  Given an N_Identifier node N, determines if it is a Selector_Name.
   --  As described in Sinfo, Selector_Names are special because they
   --  represent use of the N_Identifier node for a true identifer, when
   --  normally such nodes represent a direct name.

   function Is_Three_Byte_Record (Typ : Entity_Id) return Boolean;
   --  This function deals with a rather special case of record types. If
   --  we have a record whose size is set (either by an explicit size clause
   --  or implicitly by virtue of having a complete rep clause), then Gigi
   --  makes a gcc type whose size and alignment is the next power of 2 if
   --  no explicit alignment is set. Normally this is fine, but an exception
   --  occurs if the size is in the range 17-24, and Gigi makes a type whose
   --  size is 32. It is reasonable to have the object size be increased to
   --  the next byte boundary, but it is a surprise in practice to have a
   --  three byte component have its size increased to four bytes.
   --
   --  The function Is_Three_Byte_Record checks for a type affected by this
   --  special situation. There are three situations in which the front end
   --  fixes up things to "hide" the fact that the gcc size is different
   --  from the object size.
   --
   --    1. A reference to the Object_Size of a three byte record returns 24
   --       in the front end so that Gigi does not have the opportunity of
   --       revealing that it thinks the size is 32.
   --
   --    2. If an array has a three byte record as the component type, and
   --       no component size is explicitly set, then the size is set to 24,
   --       and front end bit packing is activated.
   --
   --    3. If a component of an unpacked record to which no component clause
   --       applies has a three byte record type, then the Esize of this
   --       component is set to 24 (but the position is left unset). This
   --       will cause Gigi to set a size of 24 bits for the component.
   --
   --  All this is a bit of a kludge, but it is needed to get around the
   --  (current) fundamental rule that components in gigi cannot be bit
   --  packed if they are BLKmode objects. Any object whose size does not
   --  match its alignment is always BLKmode. In order to allow bit packing
   --  of these three byte record types, Gigi therefore needs to ensure that
   --  the GCC size and alignment match. Since an alignment of 3 is not
   --  possible, Gigi increases both the alignment and size to 32 bits,
   --  and this special processing is all about hiding that fact!

   function Is_True (U : Uint) return Boolean;
   --  The argument is a Uint value which is the Boolean'Pos value of a
   --  Boolean operand (i.e. is either 0 for False, or 1 for True). This
   --  function simply tests if it is True (i.e. non-zero)

   function Is_Variable (N : Node_Id) return Boolean;
   --  Determines if the tree referenced by N represents a variable, i.e.
   --  can appear on the left side of an assignment. There is one situation,
   --  namely formal parameters, in which non-tagged type conversions are
   --  also considered variables, but Is_Variable returns False for such
   --  cases, since it has no knowledge of the context. Note that this is
   --  the point at which Assignment_OK is checked, and True is returned
   --  for any tree thus marked.

   procedure Kill_Size_Check_Code (E : Entity_Id);
   --  Called when an address clause or pragma Import is applied to an
   --  entity. If the entity is a variable or a constant, and size check
   --  code is present, this size check code is killed, since the object
   --  will not be allocated by the program.

   function New_External_Entity
     (Kind         : Entity_Kind;
      Scope_Id     : Entity_Id;
      Sloc_Value   : Source_Ptr;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Nat := 0;
      Prefix       : Character := ' ')
      return         Entity_Id;
   --  This function creates an N_Defining_Identifier node for an internal
   --  created entity, such as an implicit type or subtype, or a record
   --  initialization procedure. The entity name is constructed with a call
   --  to New_External_Name (Related_Id, Suffix, Suffix_Index, Prefix), so
   --  that the generated name may be referenced as a public entry, and the
   --  Is_Public flag is set if needed (using Set_Public_Status).

   function New_Internal_Entity
     (Kind       : Entity_Kind;
      Scope_Id   : Entity_Id;
      Sloc_Value : Source_Ptr;
      Id_Char    : Character)
      return       Entity_Id;
   --  This function is similar to New_External_Entity, except that the
   --  name is constructed by New_Internal_Name (Id_Char). This is used
   --  when the resulting entity does not have to be referenced as a
   --  public entity (and in this case Is_Public is not set).

   function Next_Actual (Actual_Id : Node_Id) return Node_Id;
   --  Find next actual parameter in declaration order. As described for
   --  First_Actual, this is the next actual in the declaration order, not
   --  the call order, so this does not correspond to simply taking the
   --  next entry of the Parameter_Associations list. The argument is an
   --  actual previously returned by a call to First_Actual or Next_Actual.
   --  Note tha the result produced is always an expression, not a parameter
   --  assciation node, even if named notation was used.

   procedure Normalize_Actuals
     (N       : Node_Id;
      S       : Entity_Id;
      Report  : Boolean;
      Success : out Boolean);
   --  Reorders lists of actuals according to names of formals, value returned
   --  in Success indicates sucess of reordering. For more details, see body.
   --  Errors are reported only if Report is set to True.

   procedure Note_Possible_Modification (N : Node_Id);
   --  This routine is called if the sub-expression N maybe the target of
   --  an assignment (e.g. it is the left side of an assignment, used as
   --  an out parameters, or used as prefixes of access attributes). It
   --  sets May_Be_Modified in the associated entity if there is one,
   --  taking into account the rule that in the case of renamed objects,
   --  it is the flag in the renamed object that must be set.

   function Object_Access_Level (Obj : Node_Id) return Uint;
   --  Return the accessibility level of the view of the object Obj.

   function Private_Component (Type_Id : Entity_Id) return Entity_Id;
   --  Returns some private component (if any) of the given Type_Id.
   --  Used to enforce the rules on visibility of operations on composite
   --  types, that depend on the full view of the component type. For a
   --  record type there may be several such components, we just return the
   --  first one.

   function Real_Convert (S : String) return Node_Id;
   --  S is a possibly signed syntactically valid real literal. The result
   --  returned is an N_Real_Literal node representing the literal value.

   function Same_Name (N1, N2 : Node_Id) return Boolean;
   --  Determine if two (possibly expanded) names are the same name

   procedure Set_Current_Entity (E : Entity_Id);
   --  Establish the entity E as the currently visible definition of its
   --  associated name (i.e. the Node_Id associated with its name)

   procedure Set_Entity_With_Style_Check (N : Node_Id; Val : Entity_Id);
   --  This procedure has the same calling sequence as Set_Entity, but
   --  if Style_Check is set, then it calls a style checking routine which
   --  can check identifier spelling style.

   procedure Set_Name_Entity_Id (Id : Name_Id; Val : Entity_Id);
   --  Sets the Entity_Id value associated with the given name, which is the
   --  Id of the innermost visible entity with the given name. See the body
   --  of package Sem_Ch8 for further details on the handling of visibility.

   procedure Set_Next_Actual (Ass1_Id : Node_Id; Ass2_Id : Node_Id);
   --  The arguments may be parameter associations, whose descendants
   --  are the optional formal name and the actual parameter. Positional
   --  parameters are already members of a list, and do not need to be
   --  chained separately. See also First_Actual and Next_Actual.

   procedure Set_Public_Status (Id : Entity_Id);
   --  If an entity (visible or otherwise) is defined in a library
   --  package, or a package that is itself public, then this subprogram
   --  labels the entity public as well.

   procedure Set_Scope_Is_Transient (V : Boolean := True);
   --  Set the flag Is_Transient of the current scope

   procedure Set_Size_Info (T1, T2 : Entity_Id);
   --  Copies the Esize field and Has_Biased_Representation flag from
   --  (sub)type entity T2 to (sub)type entity T1.

   function Scope_Is_Transient  return Boolean;
   --  True if the current scope is transient.

   function Static_Integer (N : Node_Id) return Uint;
   --  This function analyzes the given expression node and then resolves it
   --  as any integer type. If the result is static, then the value of the
   --  universal expression is returned, otherwise an error message is output
   --  and a value of No_Uint is returned.

   function Statically_Different (E1, E2 : Node_Id) return Boolean;
   --  Return True if it can be statically determined that the Expressions
   --  E1 and E2 refer to different objects

   function Subprogram_Access_Level (Subp : Entity_Id) return Uint;
   --  Return the accessibility level of the view denoted by Subp.

   procedure Trace_Scope (N : Node_Id; E : Entity_Id; Msg : String);
   --  Print debugging information on entry to each unit being analyzed.

   procedure Transfer_Entities (From : Entity_Id; To : Entity_Id);
   --  Move a list of entities from one scope to another, and recompute
   --  Is_Public based upon the new scope.

   function Type_Access_Level (Typ : Entity_Id) return Uint;
   --  Return the accessibility level of Typ.

   procedure Unimplemented (N : Node_Id; Feature : String);
   --  An indication that here and there we are not finished yet. Prints out
   --  a message on the given node indicating that the feature described by
   --  the given string is not implemented yet.

   procedure Wrong_Type (Expr : Node_Id; Expected_Type : Entity_Id);
   --  Output error message for incorrectly typed expression. Expr is the
   --  node for the incorrectly typed construct (Etype (Expr) is the type
   --  found), and Expected_Type is the entity for the expected type. Note
   --  that Expr does not have to be a subexpression, anything with an
   --  Etype field may be used.

private
   pragma Inline (Current_Entity);
   pragma Inline (Get_Name_Entity_Id);
   pragma Inline (Is_Entity_Name);
   pragma Inline (Is_False);
   pragma Inline (Is_True);
   pragma Inline (Set_Current_Entity);
   pragma Inline (Set_Name_Entity_Id);
   pragma Inline (Set_Size_Info);

end Sem_Util;

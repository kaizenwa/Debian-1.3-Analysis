------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ R E S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.486 $                            --
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
with Checks;   use Checks;
with Debug;    use Debug;
with Debug_A;  use Debug_A;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Util; use Exp_Util;
with Features; use Features;
with Freeze;   use Freeze;
with Itypes;   use Itypes;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Output;   use Output;
with Restrict; use Restrict;
with Sem;      use Sem;
with Sem_Aggr; use Sem_Aggr;
with Sem_Attr; use Sem_Attr;
with Sem_Ch4;  use Sem_Ch4;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Disp; use Sem_Disp;
with Sem_Dist; use Sem_Dist;
with Sem_Eval; use Sem_Eval;
with Sem_Intr; use Sem_Intr;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Sem_Res is

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  Second pass (top-down) type checking and overload resolution procedures
   --  Typ is the type required by context. These procedures propagate the
   --  type information recursively to the descendants of N. If the node
   --  is not overloaded, its Etype is established in the first pass. If
   --  overloaded,  the Resolve routines set the correct type. For arith.
   --  operators, the Etype is the base type of the context.

   --  Note that Resolve_Attribute is separated off in Sem_Attr

   procedure Check_Discriminant_Use (N : Node_Id);
   --  Enforce the restrictions on the use of discriminants when constraining
   --  a component of a discriminated type (record or concurrent type).

   procedure Check_For_Visible_Operator (N : Node_Id; T : Entity_Id);
   --  Given a node for an operator associated with type T, check that
   --  the operator is visible. Operators all of whose operands are
   --  universal must be checked for visibility during resolution
   --  because their type is not determinable based on their operands.

   function Check_Infinite_Recursion (N : Node_Id) return Boolean;
   --  Given a call node, N, which is known to occur immediately within the
   --  subprogram being called, determines whether it is a detectable case of
   --  an infinite recursion, and if so, outputs appropriate messages, and
   --  inserts a raise Storage_Error. Note that this is formally valid even
   --  if the detection is not 100% right with respect to strange exceptions
   --  etc, since Storage_Error can be raised anytime! Returns True if an
   --  infinite recursion is detected, and False otherwise.

   procedure Check_Parameterless_Call (N : Node_Id; T : Entity_Id);
   --  Several forms of names can denote calls to entities without para-
   --  meters. The context determines whether the name denotes the entity
   --  or a call to it. When it is a call, the node must be rebuilt
   --  accordingly (deprocedured, in A68 terms) and renalyzed to obtain
   --  possible interpretations.
   --
   --  The name may be that of an overloadable construct, or it can be an
   --  explicit dereference of a prefix that denotes an access to subprogram.
   --  In that case, we want to convert the name into a call only if the
   --  context requires the return type of the subprogram.  Finally, a
   --  parameterless protected subprogram appears as a selected component.
   --
   --  The parameter T is the Typ for the corresponding resolve call.

   procedure Resolve_Allocator                 (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Arithmetic_Op             (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Call                      (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Character_Literal         (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Comparison_Op             (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Conditional_Expression    (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Equality_Op               (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Explicit_Dereference      (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Entity_Name               (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Indexed_Component         (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Integer_Literal           (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Logical_Op                (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Membership_Op             (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Null                      (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Operator_Symbol           (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Op_Concat                 (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Op_Expon                  (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Op_Not                    (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Qualified_Expression      (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Range                     (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Real_Literal              (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Reference                 (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Selected_Component        (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Shift                     (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Short_Circuit             (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Slice                     (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_String_Literal            (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Type_Conversion           (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Unary_Op                  (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Unchecked_Expression      (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Unchecked_Type_Conversion (N : Node_Id; Typ : Entity_Id);

   procedure Make_Call_Into_Operator
     (N     : Node_Id;
      Typ   : Entity_Id;
      Op_Id : Entity_Id);
   --  Inverse transformation: if an operator is given in functional notation,
   --  then after resolving the node, transform into an operator node, so
   --  that operands are resolved properly. Recall that predefined operators
   --  do not have a full signature and special resolution rules apply.

   function Operator_Kind
     (Op_Name   : Name_Id;
      Is_Binary : Boolean)
      return      Node_Kind;
   --  Utility to map the name of an operator into the corresponding Node. Used
   --  by other node rewriting procedures.

   procedure Resolve_Actuals (N : Node_Id; Nam : Entity_Id);
   --  Resolve actuals of call, and add default expressions for missing ones.

   procedure Resolve_Entry_Call (N : Node_Id; Typ : Entity_Id);
   --  Called from Resolve_Call, when the prefix denotes an entry or element
   --  of entry family. actuals are resolved as for subprograms, and node
   --  is rebuilt as an entry call. Also called for protected operations. Typ
   --  is the context type, which is used when the operation is a protected
   --  function with no arguments, and the return value is indexed.

   procedure Rewrite_Operator_As_Call (N : Node_Id; Nam : Entity_Id);
   --  If an operator node resolves to a call to a user-defined operator,
   --  rewrite the node as a function call.

   procedure Rewrite_Renamed_Operator (N : Node_Id; Op : Entity_Id);
   --  An operator can rename another,  e.g. in  an instantiation. In that
   --  case, the proper operator node must be constructed.

   procedure Set_Slice_Subtype (N : Node_Id);
   --  Build subtype of array type, with the range specified by the slice.

   function Valid_Conversion (N : Node_Id) return Boolean;
   --  Verify legality rules given in 4.6 (8-23)

   -------------------------
   -- Analyze_And_Resolve --
   -------------------------

   procedure Analyze_And_Resolve (N : Node_Id) is
   begin
      Analyze (N);
      Resolve (N, Etype (N));
   end Analyze_And_Resolve;

   procedure Analyze_And_Resolve (N : Node_Id; Typ : Entity_Id) is
   begin
      Analyze (N);
      Resolve (N, Typ);
   end Analyze_And_Resolve;

   --  Version withs check(s) suppressed

   procedure Analyze_And_Resolve
     (N        : Node_Id;
      Typ      : Entity_Id;
      Suppress : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Record := Scope_Suppress;

         begin
            Scope_Suppress := (others => True);
            Analyze_And_Resolve (N, Typ);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Get_Scope_Suppress (Suppress);

         begin
            Set_Scope_Suppress (Suppress, True);
            Analyze_And_Resolve (N, Typ);
            Set_Scope_Suppress (Suppress, Svg);
         end;
      end if;
   end Analyze_And_Resolve;

   procedure Analyze_And_Resolve
     (N        : Node_Id;
      Suppress : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Record := Scope_Suppress;

         begin
            Scope_Suppress := (others => True);
            Analyze_And_Resolve (N);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Get_Scope_Suppress (Suppress);

         begin
            Set_Scope_Suppress (Suppress, True);
            Analyze_And_Resolve (N);
            Set_Scope_Suppress (Suppress, Svg);
         end;
      end if;
   end Analyze_And_Resolve;

   ----------------------------
   -- Check_Discriminant_Use --
   ----------------------------

   procedure Check_Discriminant_Use (N : Node_Id) is
      PN   : constant Node_Id   := Parent (N);
      Disc : constant Entity_Id := Entity (N);
      P    : Node_Id;
      D    : Node_Id;

   begin
      --  Any use in a default expression is legal.

      if In_Default_Expression then
         null;

      elsif Nkind (PN) = N_Range then

         --  Discriminant cannot be used to constrain a scalar type.

         P := Parent (PN);

         if Nkind (P) = N_Range_Constraint
           and then Nkind (Parent (P)) = N_Subtype_Indication
           and then Nkind (Parent (Parent (P))) = N_Component_Declaration
         then
            Error_Msg_N ("discriminant cannot constrain scalar type", N);

         elsif Nkind (P) = N_Index_Or_Discriminant_Constraint then

            --  The following check catches the unusual case where
            --  a discriminant appears within an index constraint
            --  that is part of a larger expression within a constraint
            --  on a component, e.g. "C : Int range 1 .. F (new A(1 .. D))".
            --  For now we only check case of record components, and
            --  note that a similar check should also apply in the
            --  case of discriminant constraints below.  ???

            --  Note that the check for N_Subtype_Declaration below is to
            --  detect the valid use of discriminants in the constraints of a
            --  subtype declaration when this subtype declaration appears
            --  inside the scope of a record type (which is syntactically
            --  illegal, but which may be created as part of derived type
            --  processing for records). See Sem_Ch3.Build_Derived_Record_Type
            --  for more info.

            if Ekind (Current_Scope) = E_Record_Type
              and then Scope (Disc) = Current_Scope
              and then not
                (Nkind (Parent (P)) = N_Subtype_Indication
                 and then
                  (Nkind (Parent (Parent (P))) = N_Component_Declaration
                   or else Nkind (Parent (Parent (P))) = N_Subtype_Declaration)
                  and then Paren_Count (N) = 0)
            then
               Error_Msg_N
                 ("discriminant must appear alone in component constraint", N);
               return;
            end if;

            --   Detect a common beginner error:
            --   type R (D : Positive := 100) is record
            --     Name: String (1 .. D);
            --   end record;

            --  The default value causes an object of type R to be
            --  allocated with room for Positive'Last characters.

            declare
               SI : Node_Id;
               T  : Entity_Id;
               TB : Node_Id;
               CB : Entity_Id;

               function Large_Storage_Type (T : Entity_Id) return Boolean;
               --  Return True if type T has a large enough range that
               --  any array whose index type covered the whole range of
               --  the type would likely raise Storage_Error.

               function Large_Storage_Type (T : Entity_Id) return Boolean is
               begin
                  return
                    T = Standard_Integer
                      or else
                    T = Standard_Positive
                      or else
                    T = Standard_Natural;
               end Large_Storage_Type;

            begin
               --  Check that the Disc has a large range

               if not Large_Storage_Type (Etype (Disc)) then
                  goto No_Danger;
               end if;

               --  Check that it is the high bound

               if N /= High_Bound (PN)
                 or else not Present (Discriminant_Default_Value (Disc))
               then
                  goto No_Danger;
               end if;

               --  Check the array allows a large range at this bound.
               --  First find the array

               SI := Parent (P);

               if Nkind (SI) /= N_Subtype_Indication then
                  goto No_Danger;
               end if;

               T := Entity (Subtype_Mark (SI));

               if not Is_Array_Type (T) then
                  goto No_Danger;
               end if;

               --  Next, find the dimension

               TB := First_Index (T);
               CB := First (Constraints (P));
               while True
                 and then Present (TB)
                 and then Present (CB)
                 and then CB /= PN
               loop
                  TB := Next_Index (TB);
                  CB := Next (CB);
               end loop;

               if CB /= PN then
                  goto No_Danger;
               end if;

               --  Now, check the dimension has a large range

               if not Large_Storage_Type (Etype (TB)) then
                  goto No_Danger;
               end if;

               --  Warn about the danger

               Error_Msg_N
                 ("creation of object of this type may raise Storage_Error?",
                  N);

               <<No_Danger>>
                  null;

            end;
         end if;

      --  Legal case is in index or discriminant constraint

      elsif Nkind (PN) = N_Index_Or_Discriminant_Constraint
        or else Nkind (PN) = N_Discriminant_Association
      then
         if Paren_Count (N) > 0 then
            Error_Msg_N
              ("discriminant in constraint must appear alone",  N);
         end if;

         return;

      --  Otherwise, context is an expression. It should not be within
      --  (i.e. a subexpression of) a constraint for a component.

      else
         D := PN;
         P := Parent (PN);

         while Nkind (P) /= N_Component_Declaration
           and then Nkind (P) /= N_Subtype_Indication
           and then Nkind (P) /= N_Entry_Declaration
         loop
            D := P;
            P := Parent (P);
            exit when No (P);
         end loop;

         --  If the discriminant is used in an expression that is a bound
         --  of a scalar type, an Itype is created and the bounds are attached
         --  to its range,  not to the original subtype indication. Such use
         --  is of course a double fault.

         if (Nkind (P) = N_Subtype_Indication
              and then
                (Nkind (Parent (P)) = N_Component_Declaration
                  or else Nkind (Parent (P)) = N_Derived_Type_Definition)
              and then D = Constraint (P))

           or else Nkind (P) = N_Entry_Declaration
           or else Nkind (D) = N_Defining_Identifier
         then
            Error_Msg_N
              ("discriminant in constraint must appear alone",  N);
         end if;
      end if;
   end Check_Discriminant_Use;

   --------------------------------
   -- Check_For_Visible_Operator --
   --------------------------------

   procedure Check_For_Visible_Operator (N : Node_Id; T : Entity_Id) is
      Orig_Node : Node_Id := Original_Node (N);

   begin
      if Comes_From_Source (Orig_Node)
        and then not In_Open_Scopes (Scope (T))
        and then not Is_Potentially_Use_Visible (T)
        and then not In_Use (T)
        and then not In_Use (Scope (T))
        and then (not Present (Entity (N))
                   or else Ekind (Entity (N)) /= E_Function)
        and then (Nkind (Orig_Node) /= N_Function_Call
                   or else Nkind (Name (Orig_Node)) /= N_Expanded_Name
                   or else Entity (Prefix (Name (Orig_Node))) /= Scope (T))
        and then not In_Instance
      then
         Error_Msg_NE
           ("operator for} is not directly visible!", N, First_Subtype (T));
         Error_Msg_N ("use clause would make operation legal!", N);
      end if;
   end Check_For_Visible_Operator;

   ------------------------------
   -- Check_Infinite_Recursion --
   ------------------------------

   function Check_Infinite_Recursion (N : Node_Id) return Boolean is
      P : Node_Id;
      C : Node_Id;

   begin
      --  Loop moving up tree, quitting if something tells us we are
      --  definitely not in an infinite recursion situation.

      C := N;
      loop
         P := Parent (C);
         exit when Nkind (P) = N_Subprogram_Body;

         if Nkind (P) = N_Or_Else        or else
            Nkind (P) = N_And_Then       or else
            Nkind (P) = N_If_Statement   or else
            Nkind (P) = N_Case_Statement
         then
            return False;

         elsif Nkind (P) = N_Handled_Sequence_Of_Statements
           and then C /= First (Statements (P))
         then
            return False;

         else
            C := P;
         end if;
      end loop;

      Error_Msg_N ("infinite recursion?", N);
      Error_Msg_N ("\Storage_Error will be raised at runtime?", N);

      Insert_Action (N,
        Make_Raise_Statement (Sloc (N),
          Name => New_Occurrence_Of (Standard_Storage_Error, Sloc (N))));

      return True;
   end Check_Infinite_Recursion;

   ------------------------------
   -- Check_Parameterless_Call --
   ------------------------------

   procedure Check_Parameterless_Call (N : Node_Id; T : Entity_Id) is
      Nam : Node_Id;

   begin
      --  Rewrite as call if overloadable entity that is (or could be, in
      --  the overloaded case) a function call. If we know for sure that
      --  the entity is an enumeration literal, we do not rewrite it.

      if (Is_Entity_Name (N)
            and then Is_Overloadable (Entity (N))
            and then (Ekind (Entity (N)) /= E_Enumeration_Literal
                        or else Is_Overloaded (N)))

      --  Rewrite as call if it is an explicit deference resulting in a
      --  subprogram access type, and the type of the result of the call
      --  corresponds to the type required for the resolution context.

      or else
        (Nkind (N) = N_Explicit_Dereference
          and then Ekind (Etype (N)) = E_Subprogram_Type
          and then Base_Type (Etype (Etype (N))) = Base_Type (T))

      --  Rewrite as call if it is a selected component which is a function,
      --  this is the case of a call to a protected function.

      or else
        (Nkind (N) = N_Selected_Component
          and then (Ekind (Entity (Selector_Name (N))) = E_Function
           or else Ekind (Entity (Selector_Name (N))) = E_Entry))

      --  If one of the above three conditions is met, rewrite as call

      then
         Nam := New_Copy (N);

         --  If overloaded, overload set belongs to new copy.

         Save_Interps (N, Nam);

         --  Change node to parameterless function call (note that the
         --  Parameter_Associations associations field is left set to Empty,
         --  its normal default value since there are no parameters)

         Change_Node (N, N_Function_Call);
         Set_Name (N, Nam);
         Set_Sloc (N, Sloc (Nam));
         Analyze_Call (N);
      end if;
   end Check_Parameterless_Call;

   -----------------------------
   -- Make_Call_Into_Operator --
   -----------------------------

   procedure Make_Call_Into_Operator
     (N     : Node_Id;
      Typ   : Entity_Id;
      Op_Id : Entity_Id)
   is
      Op_Name   : constant Name_Id := Chars (Op_Id);
      Act1      : constant Node_Id := First_Actual (N);
      Act2      : constant Node_Id := Next_Actual (Act1);
      Error     : Boolean := False;
      Is_Binary : constant Boolean := Present (Act2);
      Op_Node   : Node_Id;
      Opnd_Type : Entity_Id;
      Pack      : Entity_Id;

      type Kind_Test is access function (E : Entity_Id) return Boolean;

      function Has_Kind (P : Entity_Id; Test : Kind_Test) return Boolean;

      function Is_Definite_Access_Type (E : Entity_Id) return Boolean;
      --  Not allocator and not Access to subprogram.

      function Is_Definite_Access_Type (E : Entity_Id) return Boolean is
      begin
         return Ekind (Base_Type (E)) = E_Access_Type;
      end Is_Definite_Access_Type;

      function Has_Kind (P : Entity_Id; Test : Kind_Test) return Boolean is
         E : Entity_Id;

         function In_Decl return Boolean;
         --  verify that node is not part of the type declaration for the
         --  candidate type, which would otherwise be invisible.

         function In_Decl return Boolean is
            Decl_Node : constant Node_Id := Parent (E);
            N2        : Node_Id;
         begin
            N2 := N;
            if Etype (E) = Any_Type then
               return True;

            elsif No (Decl_Node) then
               return False;

            else
               while Present (N2)
                 and then Nkind (N2) /= N_Compilation_Unit
               loop
                  if N2 = Decl_Node then
                     return True;
                  else
                     N2 := Parent (N2);
                  end if;
               end loop;

               return False;
            end if;
         end In_Decl;

      begin
         E := First_Entity (P);

         while Present (E) loop

            if Test (E)
              and then not In_Decl
            then
               return True;
            end if;

            E := Next_Entity (E);
         end loop;

         return False;
      end Has_Kind;

   --  Start of processing for Make_Call_Into_Operator

   begin
      Op_Node := New_Node (Operator_Kind (Op_Name, Is_Binary), Sloc (N));

      if Is_Binary then

         --  Move the actuals to the operator node. If the actuals
         --  were given by named associations, they are not list members.
         --  Otherwise they must be removed from the actuals list before
         --  attaching to the new node.

         if Nkind (Parent (Act1)) /= N_Parameter_Association then
            Remove (Act1);
         end if;

         if Nkind (Parent (Act2)) /= N_Parameter_Association then
            Remove (Act2);
         end if;

         Set_Left_Opnd  (Op_Node, Act1);
         Set_Right_Opnd (Op_Node, Act2);

      --  Unary operators

      else
         if Nkind (Parent (Act1)) /= N_Parameter_Association then
            Remove (Act1);
         end if;

         Set_Right_Opnd (Op_Node, Act1);
      end if;

      --  If the operator is denoted by an expanded name, and the prefix is
      --  not Standard, but the operator is a predefined one whose scope is
      --  Standard, then this is an implicit_operator, inserted as an
      --  interpretation by the procedure of the same name. This procedure
      --  overestimates the presence of implicit operators, because it does
      --  not examine the type of the operands. Verify now that the operand
      --  type appears in the given scope. If right operand is universal,
      --  check the other operand. In the case of concatenation, either
      --  argument can be the component type, so check the type of the result.
      --  If both arguments are literals, look for a type of the right kind
      --  defined in the given scope. This elaborate nonsense is brought to
      --  you courtesy of b33302a.

      --  A final wrinkle is the multiplication operator for fixed point
      --  types, which is defined in Standard only, and not in the scope of
      --  the fixed_point type itself.

      if Nkind (Name (N)) = N_Expanded_Name then
         Pack := Entity (Prefix (Name (N)));

         --  If the entity being called is defined in the given package,
         --  it is a renaming of a predefined operator, and known to be
         --  legal.

         if Scope (Entity (Name (N))) = Pack then
            null;

         elsif (Op_Name =  Name_Op_Multiply
              or else Op_Name = Name_Op_Divide)
           and then Is_Fixed_Point_Type (Etype (Left_Opnd  (Op_Node)))
           and then Is_Fixed_Point_Type (Etype (Right_Opnd (Op_Node)))
         then
            if Pack /= Standard_Standard then
               Error := True;
            end if;

         else
            Opnd_Type := Base_Type (Etype (Right_Opnd (Op_Node)));

            if Op_Name = Name_Op_Concat then
               Opnd_Type := Base_Type (Typ);

            elsif Scope (Opnd_Type) = Standard_Standard
              and then Is_Binary
            then
               Opnd_Type := Base_Type (Etype (Left_Opnd (Op_Node)));
            end if;

            if Scope (Opnd_Type) = Standard_Standard then

               --  verify that the scope contains a type that corresponds to
               --  the given literal. Optimize the case where Pack is Standard.

               if Pack /= Standard_Standard then

                  if Opnd_Type = Universal_Integer
                    and then not Has_Kind (Pack, Is_Integer_Type'Access)
                  then
                     Error := True;

                  elsif Opnd_Type = Universal_Real
                    and then not Has_Kind (Pack, Is_Real_Type'Access)
                  then
                     Error := True;

                  elsif Opnd_Type = Any_String
                    and then not Has_Kind (Pack, Is_String_Type'Access)
                  then
                     Error := True;

                  elsif Opnd_Type = Any_Access
                    and then not Has_Kind
                      (Pack, Is_Definite_Access_Type'Access)
                  then
                     Error := True;
                  end if;
               end if;

            elsif Ekind (Opnd_Type) = E_Allocator_Type
               and then not Has_Kind (Pack, Is_Definite_Access_Type'Access)
            then
               Error := True;

            --  If the type is defined elsewhere, and the operator is not
            --  defined in the given scope (by a renaming declaration, e.g.)
            --  then this is an error as well.

            elsif Scope (Opnd_Type) /= Pack
              and then Scope (Op_Id) /= Pack
            then
               Error := True;
            end if;
         end if;

         if Error then
            Error_Msg_Node_2 := Pack;
            Error_Msg_NE
              ("& not declared in&", N, Selector_Name (Name (N)));
            Set_Etype (N, Any_Type);
            return;
         end if;
      end if;

      Set_Chars  (Op_Node, Op_Name);
      Set_Etype  (Op_Node, Etype (N));
      Set_Entity (Op_Node, Op_Id);
      Rewrite_Substitute_Tree (N,  Op_Node);
      Resolve (N, Typ);
   end Make_Call_Into_Operator;

   -------------------
   -- Operator_Kind --
   -------------------

   function Operator_Kind
     (Op_Name   : Name_Id;
      Is_Binary : Boolean)
      return      Node_Kind
   is
      Kind : Node_Kind;

   begin
      if Is_Binary then
         if    Op_Name =  Name_Op_And      then Kind := N_Op_And;
         elsif Op_Name =  Name_Op_Or       then Kind := N_Op_Or;
         elsif Op_Name =  Name_Op_Xor      then Kind := N_Op_Xor;
         elsif Op_Name =  Name_Op_Eq       then Kind := N_Op_Eq;
         elsif Op_Name =  Name_Op_Ne       then Kind := N_Op_Ne;
         elsif Op_Name =  Name_Op_Lt       then Kind := N_Op_Lt;
         elsif Op_Name =  Name_Op_Le       then Kind := N_Op_Le;
         elsif Op_Name =  Name_Op_Gt       then Kind := N_Op_Gt;
         elsif Op_Name =  Name_Op_Ge       then Kind := N_Op_Ge;
         elsif Op_Name =  Name_Op_Add      then Kind := N_Op_Add;
         elsif Op_Name =  Name_Op_Subtract then Kind := N_Op_Subtract;
         elsif Op_Name =  Name_Op_Concat   then Kind := N_Op_Concat;
         elsif Op_Name =  Name_Op_Multiply then Kind := N_Op_Multiply;
         elsif Op_Name =  Name_Op_Divide   then Kind := N_Op_Divide;
         elsif Op_Name =  Name_Op_Mod      then Kind := N_Op_Mod;
         elsif Op_Name =  Name_Op_Rem      then Kind := N_Op_Rem;
         elsif Op_Name =  Name_Op_Expon    then Kind := N_Op_Expon;
         else
            pragma Assert (False); null;
         end if;

      --  Unary operators

      else
         if    Op_Name =  Name_Op_Add      then Kind := N_Op_Plus;
         elsif Op_Name =  Name_Op_Subtract then Kind := N_Op_Minus;
         elsif Op_Name =  Name_Op_Abs      then Kind := N_Op_Abs;
         elsif Op_Name =  Name_Op_Not      then Kind := N_Op_Not;
         else
            pragma Assert (False); null;
         end if;
      end if;

      return Kind;
   end Operator_Kind;

   -----------------------------
   -- Pre_Analyze_And_Resolve --
   -----------------------------

   procedure Pre_Analyze_And_Resolve (N : Node_Id; T : Entity_Id) is
      Save_Full_Analysis : constant Boolean := Full_Analysis;

   begin
      Full_Analysis := False;
      Expander_Mode_Save_And_Set (False);

      --  We suppress all checks for this analysis, since the checks will
      --  be applied properly, and in the right location, when the default
      --  expression is reanalyzed and reexpanded later on.

      Analyze_And_Resolve (N, T, Suppress => All_Checks);

      Expander_Mode_Restore;
      Full_Analysis := Save_Full_Analysis;
   end Pre_Analyze_And_Resolve;

   -------------
   -- Resolve --
   -------------

   procedure Resolve (N : Node_Id; Typ : Entity_Id) is
      I         : Interp_Index;
      I1        : Interp_Index;
      It        : Interp;
      It1       : Interp;
      Found     : Boolean := False;
      Seen      : Entity_Id;
      Ctx_Type  : Entity_Id := Typ;
      Expr_Type : Entity_Id;
      Ambiguous : Boolean := False;

      procedure Patch_Up_Value (N : Node_Id; Typ : Entity_Id);
      --  Try and fix up a literal so that it matches its expected type. New
      --  literals are manufactured (either real or integer) if necessary so
      --  that remaining processing occurs smoothly.

      procedure Resolution_Failed;
      --  Called when attempt at resolving current expression fails

      procedure Patch_Up_Value (N : Node_Id; Typ : Entity_Id) is
      begin
         if Nkind (N) = N_Integer_Literal
           and then Is_Real_Type (Typ)
         then
            Rewrite_Substitute_Tree (N,
              Make_Real_Literal (Sloc (N),
                Realval => UR_From_Uint (Intval (N))));
            Set_Etype (N, Universal_Real);
            Set_Is_Static_Expression (N);

         elsif Nkind (N) = N_Real_Literal
           and then Is_Integer_Type (Typ)
         then
            Rewrite_Substitute_Tree (N,
              Make_Integer_Literal (Sloc (N),
                Intval => UR_To_Uint (Realval (N))));
            Set_Etype (N, Universal_Integer);
            Set_Is_Static_Expression (N);
         elsif Nkind (N) = N_String_Literal
           and then Is_Character_Type (Typ)
         then
            Set_Character_Literal_Name (Char_Code (Character'Pos ('A')));
            Rewrite_Substitute_Tree (N,
              Make_Character_Literal (Sloc (N),
                Chars => Name_Find,
                Char_Literal_Value => Char_Code (Character'Pos ('A'))));
            Set_Etype (N, Any_Character);
            Set_Is_Static_Expression (N);

         elsif Nkind (N) = N_Range then
            Patch_Up_Value (Low_Bound (N), Typ);
            Patch_Up_Value (High_Bound (N), Typ);
         end if;
      end Patch_Up_Value;

      procedure Resolution_Failed is
      begin
         Patch_Up_Value (N, Typ);
         Set_Etype (N, Typ);
         Debug_A_Exit ("resolving  ", N, " (done, resolution failed)");
         Set_Is_Overloaded (N, False);

         --  The caller will return without calling the expander, so we need
         --  to set the analyzed flag. Note that it is fine to set Analyzed
         --  to True even if we are in the middle of a shallow analysis,
         --  (see the spec of sem for more details) since this is an error
         --  situation anyway, and there is no point in repeating the
         --  analysis later (indeed it won't work to repeat it later, since
         --  we haven't got a clear resolution of which entity is being
         --  referenced.)

         Set_Analyzed (N, True);
         return;
      end Resolution_Failed;

   --  Start of processing for Resolve

   begin
      --  If the context is a Remote_Access_To_Subprogram, access attributes
      --  must be resolved with the corresponding fat pointer.

      if Nkind (N) = N_Attribute_Reference
        and then Comes_From_Source (N)
        and then Is_Remote_Call_Interface (Typ)
      then
         declare
            Attr_Id : Attribute_Id := Get_Attribute_Id (Attribute_Name (N));
            Pref    : Node_Id      := Prefix (N);

         begin
            if (Attr_Id = Attribute_Access
                  or else
                Attr_Id = Attribute_Unchecked_Access
                  or else
                Attr_Id = Attribute_Unrestricted_Access)
            then
               Process_Remote_AST_Attribute (N, Typ);
            end if;
         end;
      end if;

      Debug_A_Entry ("resolving  ", N);

      if Any_Restrictions then
         if Is_Fixed_Point_Type (Typ) then
            Check_Restriction (No_Fixed_Point, N);
         elsif Is_Floating_Point_Type (Typ) then
            Check_Restriction (No_Floating_Point, N);
         end if;
      end if;

      --  Return if already analyzed

      if Analyzed (N) then
         Debug_A_Exit ("resolving  ", N, "  (done, already analyzed)");
         return;

      --  Return if type = Any_Type (previous error encountered)

      elsif Etype (N) = Any_Type then
         Debug_A_Exit ("resolving  ", N, "  (done, Etype = Any_Type)");
         return;
      end if;

      Check_Parameterless_Call (N, Typ);

      --  If not overloaded, then we know the type, and all that needs doing
      --  is to check that this type is compatible with the context.

      if not Is_Overloaded (N) then
         Found := Covers (Typ, Etype (N));
         Expr_Type := Etype (N);

      --  In the overloaded case, we must select the interpretation that
      --  is compatible with the context (i.e. the type passed to Resolve)

      else
         Get_First_Interp (N, I, It);

         --  Loop through possible interpretations

         Interp_Loop : while Present (It.Typ) loop

            --  We are only interested in interpretations that are compatible
            --  with the expected type, any other interpretations are ignored

            if Covers (Typ, It.Typ) then

               --  First matching interpretation

               if not Found then
                  Found := True;
                  I1    := I;
                  Seen  := It.Nam;
                  Expr_Type := It.Typ;

               --  Matching intepretation that is not the first, maybe an
               --  error, but there are some cases where other rules are
               --  used to choose between the two possibilities.

               else -- Found = True

                  --  Could be a tag-indeterminate call which resolves
                  --  statically to the operation on the root of the class.
                  --  Keep the interpretation that is closest to the root type.

                  if Is_Overloadable (It.Nam)
                    and then Is_Dispatching_Operation (It.Nam)
                    and then Is_Dispatching_Operation (Seen)
                    and then Root_Type (Find_Dispatching_Type (It.Nam)) =
                             Root_Type (Find_Dispatching_Type (Seen))
                  then
                     declare
                        T1 : Entity_Id := Find_Dispatching_Type (It.Nam);
                        T2 : Entity_Id := Find_Dispatching_Type (Seen);
                        R  : Entity_Id := Root_Type (T1);

                     begin
                        while T1 /= R and then T2 /= R loop
                           T1 := Etype (T1);
                           T2 := Etype (T2);
                        end loop;

                        if T1 = R then
                           Seen := It.Nam;
                        end if;
                     end;

                  --  Case of more than one interpretation. Use preference
                  --  rules, and check operator visibility and hiding.

                  else
                     Error_Msg_Sloc := Sloc (Seen);
                     It1 := Disambiguate (N, I1, I, Typ);

                     if It1 = No_Interp then

                        --  Before we issue an ambiguity complaint, check for
                        --  the case of a subprogram call where at least one
                        --  of the arguments is Any_Type, and if so, suppress
                        --  the message, since it is a cascaded message.

                        if Nkind (N) = N_Function_Call
                          or else Nkind (N) = N_Procedure_Call_Statement
                        then
                           declare
                              A : Node_Id := First_Actual (N);
                              E : Node_Id;

                           begin
                              while Present (A) loop
                                 E := A;

                                 if Nkind (E) = N_Parameter_Association then
                                    E := Explicit_Actual_Parameter (E);
                                 end if;

                                 if Etype (E) = Any_Type then
                                    if Debug_Flag_V then
                                       Write_Str ("Any_Type in call");
                                       Write_Eol;
                                    end if;

                                    exit Interp_Loop;
                                 end if;

                                 A := Next_Actual (A);
                              end loop;
                           end;

                        elsif Nkind (N) in  N_Binary_Op
                          and then (Etype (Left_Opnd (N)) = Any_Type
                                     or else Etype (Right_Opnd (N)) = Any_Type)
                        then
                           exit Interp_Loop;

                        elsif Nkind (N) in  N_Unary_Op
                          and then Etype (Right_Opnd (N)) = Any_Type
                        then
                           exit Interp_Loop;
                        end if;

                        --  Not that special case, so issue message using the
                        --  flag Ambiguous to control printing of the header
                        --  message only at the start of an ambiguous set.

                        if not Ambiguous then
                           Error_Msg_NE
                             ("ambiguous expression (cannot resolve&)!",
                              N, It.Nam);
                           Error_Msg_N
                             ("possible interpretation#!", N);
                           Ambiguous := True;
                        end if;

                        Error_Msg_Sloc := Sloc (It.Nam);
                        Error_Msg_N ("possible interpretation#!", N);

                     --  Disambiguation has succeeded. Skip the remaining
                     --  interpretations.
                     else
                        Seen := It1.Nam;
                        Expr_Type := It1.Typ;

                        while Present (It.Typ) loop
                           Get_Next_Interp (I, It);
                        end loop;
                     end if;
                  end if;
               end if;

               --  We have a matching interpretation, Expr_Type is the
               --  type from this interpretation, and Seen is the entity.

               --  For an operator, just set the entity name. The type will
               --  be set by the specific operator resolution routine.

               if Nkind (N) in N_Op then
                  Set_Entity (N, Seen);

               --  For an explicit dereference, attribute reference, range,
               --  short-circuit form (which is not an operator node),
               --  or a call with a name that is an explicit dereference,
               --  there is nothing to be done at this point.

               elsif     Nkind (N) = N_Explicit_Dereference
                 or else Nkind (N) = N_Attribute_Reference
                 or else Nkind (N) = N_And_Then
                 or else Nkind (N) = N_Indexed_Component
                 or else Nkind (N) = N_Or_Else
                 or else Nkind (N) = N_Range
                 or else Nkind (N) = N_Selected_Component
                 or else Nkind (N) = N_Slice
                 or else Nkind (Name (N)) = N_Explicit_Dereference
               then
                  null;

               --  For procedure or function calls, set the type of the
               --  name, and also the entity pointer for the prefix

               elsif (Nkind (N) = N_Procedure_Call_Statement
                       or else Nkind (N) = N_Function_Call)
                 and then (Is_Entity_Name (Name (N))
                            or else Nkind (Name (N)) = N_Operator_Symbol)
               then
                  Set_Etype  (Name (N), Expr_Type);
                  Set_Entity (Name (N), Seen);

               elsif Nkind (N) = N_Function_Call
                 and then Nkind (Name (N)) = N_Selected_Component
               then
                  Set_Etype (Name (N), Expr_Type);
                  Set_Entity (Selector_Name (Name (N)), Seen);

               --  For all other cases, just set the type of the Name

               else
                  Set_Etype (Name (N), Expr_Type);
               end if;

            --  Here if interpetation is incompatible with context type

            else
               if Debug_Flag_V then
                  Write_Str ("    intepretation incompatible with context");
                  Write_Eol;
               end if;
            end if;

            --  Move to next interpretation

            if Present (It.Typ) then
               Get_Next_Interp (I, It);
            else
               exit;
            end if;
         end loop Interp_Loop;
      end if;

      --  At this stage Found indicates whether or not an acceptable
      --  interpretation exists. If not, then we have an error, except
      --  that if the context is Any_Type as a result of some other error,
      --  then we suppress the error report.

      if not Found then
         if Typ /= Any_Type then

            --  If type we are looking for is Void, then this is the
            --  procedure call case, and the error is simply that what
            --  we gave is not a procedure name (we think of procedure
            --  calls as expressions with types internally, but the user
            --  doesn't think of them this way!)

            if Typ = Standard_Void_Type then
               Error_Msg_N ("expect procedure name in procedure call", N);

            --  Otherwise we do have a subexpression with the wrong type

            else
               --  Check for the case of an allocator which uses an access
               --  type instead of the designated type. This is a common
               --  error and we specialize the message, posting an error
               --  on the operand of the allocator, complaining that we
               --  expected the designated type of the allocator.

               if Nkind (N) = N_Allocator
                 and then Ekind (Typ) in Access_Kind
                 and then Ekind (Etype (N)) in Access_Kind
                 and then Designated_Type (Etype (N)) = Typ
               then
                  Wrong_Type (Expression (N), Designated_Type (Typ));

               --  Normal case of looking for Typ, found Etype (N)

               else
                  Wrong_Type (N, Typ);
               end if;
            end if;
         end if;

         Resolution_Failed;
         return;

      --  Test if we have more than one interpretation for the context

      elsif Ambiguous then
         Resolution_Failed;
         return;

      --  Here we have an acceptable interpretation for the context

      else
         --  A user-defined operator is tranformed into a function call at
         --  this point, so that further processing knows that operators are
         --  really operators (i.e. are predefined operators). User-defined
         --  operators that are intrinsic are just renamings of the predefined
         --  ones, and need not be turned into calls either, but if they rename
         --  a different operator, we must transform the node accordingly.

         if Nkind (N) in N_Op
           and then Present (Entity (N))
           and then Ekind (Entity (N)) /= E_Operator
         then

            if not Is_Intrinsic_Subprogram (Entity (N)) then
               Rewrite_Operator_As_Call (N, Entity (N));

            elsif Present (Alias (Entity (N))) then

               if Chars (Alias (Entity (N))) in  Any_Operator_Name then
                  Rewrite_Renamed_Operator (N, Alias (Entity (N)));
               else
                  Rewrite_Operator_As_Call (N, Alias (Entity (N)));
               end if;
            end if;
         end if;

         --  Propagate type information and normalize tree for various
         --  predefined operations. If the context only imposes a class of
         --  types, rather than a specific type, propagate the actual type
         --  downward.

         if Typ = Any_Integer
           or else Typ = Any_Boolean
           or else Typ = Any_Modular
           or else Typ = Any_Real
           or else Typ = Any_Discrete
         then
            Ctx_Type := Expr_Type;

            --  Any_Fixed is legal in a real context only if a specific
            --  fixed point type is imposed. If Norman Cohen can be
            --  confused by this, it deserves a separate message.

            if Typ = Any_Real
              and then Expr_Type = Any_Fixed
            then
               Error_Msg_N ("Illegal context for mixed mode operation", N);
               Set_Etype (N, Universal_Real);
               Ctx_Type := Universal_Real;
            end if;
         end if;

         case N_Subexpr'(Nkind (N)) is

            when N_Aggregate => Resolve_Aggregate                (N, Ctx_Type);

            when N_Allocator => Resolve_Allocator                (N, Ctx_Type);

            when N_And_Then | N_Or_Else
                             => Resolve_Short_Circuit            (N, Ctx_Type);

            when N_Attribute_Reference
                             => Resolve_Attribute                (N, Ctx_Type);

            when N_Character_Literal
                             => Resolve_Character_Literal        (N, Ctx_Type);

            when N_Concat_Multiple => null;  --  ??? needs explanation

            when N_Conditional_Expression
                             => Resolve_Conditional_Expression   (N, Ctx_Type);

            when N_Expanded_Name
                             => Resolve_Entity_Name              (N, Ctx_Type);

            when N_Extension_Aggregate
                             => Resolve_Extension_Aggregate      (N, Ctx_Type);

            when N_Explicit_Dereference
                             => Resolve_Explicit_Dereference     (N, Ctx_Type);

            when N_Function_Call
                             => Resolve_Call                     (N, Ctx_Type);

            when N_Identifier
                             => Resolve_Entity_Name              (N, Ctx_Type);

            when N_In | N_Not_In
                             => Resolve_Membership_Op            (N, Ctx_Type);

            when N_Indexed_Component
                             => Resolve_Indexed_Component        (N, Ctx_Type);

            when N_Integer_Literal
                             => Resolve_Integer_Literal          (N, Ctx_Type);

            when N_Null      => Resolve_Null                     (N, Ctx_Type);

            when N_Op_And | N_Op_Or | N_Op_Xor
                             => Resolve_Logical_Op               (N, Ctx_Type);

            when N_Op_Eq | N_Op_Ne
                             => Resolve_Equality_Op              (N, Ctx_Type);

            when N_Op_Lt | N_Op_Le | N_Op_Gt | N_Op_Ge
                             => Resolve_Comparison_Op            (N, Ctx_Type);

            when N_Op_Not    => Resolve_Op_Not                   (N, Ctx_Type);

            when N_Op_Add    | N_Op_Subtract | N_Op_Multiply |
                 N_Op_Divide | N_Op_Mod      | N_Op_Rem

                             => Resolve_Arithmetic_Op            (N, Ctx_Type);

            when N_Op_Concat => Resolve_Op_Concat                (N, Ctx_Type);

            when N_Op_Expon  => Resolve_Op_Expon                 (N, Ctx_Type);

            when N_Op_Plus | N_Op_Minus  | N_Op_Abs
                             => Resolve_Unary_Op                 (N, Ctx_Type);

            when N_Op_Shift  => Resolve_Shift                    (N, Ctx_Type);

            when N_Procedure_Call_Statement
                             => Resolve_Call                     (N, Ctx_Type);

            when N_Operator_Symbol
                             => Resolve_Operator_Symbol          (N, Ctx_Type);

            when N_Qualified_Expression
                             => Resolve_Qualified_Expression     (N, Ctx_Type);

            when N_Raise_Constraint_Error
                             => Set_Etype (N, Ctx_Type);

            when N_Range     => Resolve_Range                    (N, Ctx_Type);

            when N_Real_Literal
                             => Resolve_Real_Literal             (N, Ctx_Type);

            when N_Reference => Resolve_Reference                (N, Ctx_Type);

            when N_Selected_Component
                             => Resolve_Selected_Component       (N, Ctx_Type);

            when N_Slice     => Resolve_Slice                    (N, Ctx_Type);

            when N_String_Literal
                             => Resolve_String_Literal           (N, Ctx_Type);

            when N_Type_Conversion
                             => Resolve_Type_Conversion          (N, Ctx_Type);

            when N_Unchecked_Expression =>
               Resolve_Unchecked_Expression                      (N, Ctx_Type);

            when N_Unchecked_Type_Conversion =>
               Resolve_Unchecked_Type_Conversion                 (N, Ctx_Type);

         end case;

         --  If the subexpression was replaced by a non-subexpression, then
         --  all we do is to expand it. The only legitimate case we know of
         --  is converting procedure call statement to entry call statements,
         --  but there may be others, so at least temporarily, we are making
         --  this test general since it was general in previous versions ???

         if Nkind (N) not in N_Subexpr then
            Debug_A_Exit ("resolving  ", N, "  (done)");
            Expand (N);
            return;
         end if;

         --  The expression is definitely NOT overloaded at this point, so
         --  we reset the Is_Overloaded flag to avoid any confusion when
         --  reanalyzing the node.

         Set_Is_Overloaded (N, False);

         --  Freeze expression type, entity if it is a name, and designated
         --  type if it is an allocator (RM 13.14(9,10)).

         --  Now that the resolution of the type of the node is complete,
         --  and we did not detect an error, we can expand this node. We
         --  skip the expand call if we are in a default expression, see
         --  section "Handling of Default Expressions" in Sem spec.

         Debug_A_Exit ("resolving  ", N, "  (done)");

         --  We unconditionally freeze the expression, even if we are in
         --  default expression mode (the Freeze_Expression routine tests
         --  this flag and only freezes static types if it is set).

         Freeze_Expression (N);

         Expand (N);
      end if;

   end Resolve;

   --  Version with check(s) suppressed

   procedure Resolve (N : Node_Id; Typ : Entity_Id; Suppress : Check_Id) is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Record := Scope_Suppress;

         begin
            Scope_Suppress := (others => True);
            Resolve (N, Typ);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Get_Scope_Suppress (Suppress);

         begin
            Set_Scope_Suppress (Suppress, True);
            Resolve (N, Typ);
            Set_Scope_Suppress (Suppress, Svg);
         end;
      end if;
   end Resolve;



   ---------------------
   -- Resolve_Actuals --
   ---------------------

   procedure Resolve_Actuals (N : Node_Id; Nam : Entity_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      A     : Node_Id;
      F     : Entity_Id;
      A_Typ : Entity_Id;
      F_Typ : Entity_Id;
      Prev  : Node_Id := Empty;

      function OK_Variable_For_Out_Formal (AV : Node_Id) return Boolean;
      pragma Inline (OK_Variable_For_Out_Formal);
      --  Used to test if AV is an acceptable formal for an OUT or IN OUT
      --  formal. Note that the Is_Variable function is not quite the right
      --  test because this is a case in which conversions whose expression
      --  is a variable (in the Is_Variable sense) with a non-tagged type
      --  target are considered view conversions and hence variables.

      procedure Insert_Default;
      --  If the actual is missing in a call, insert in the actuals list
      --  an instance of the default expression. The insertion is always
      --  a named association.

      --------------------------------
      -- OK_Variable_For_Out_Formal --
      --------------------------------

      function OK_Variable_For_Out_Formal (AV : Node_Id) return Boolean is
      begin
         Note_Possible_Modification (AV);

         --  We must reject parenthesized variable names.  The check
         --  for Comes_From_Source is present because there are
         --  currently cases where the compiler violates this
         --  rule (e.g., passing a task object to its controlled
         --  Initialize routine).

         if Paren_Count (AV) > 0 and then Comes_From_Source (AV) then
            return False;

         elsif Is_Variable (AV) then
            return True;

         elsif Nkind (AV) = N_Identifier then
            return False;

         elsif Nkind (AV) /= N_Type_Conversion
           and then Nkind (AV) /= N_Unchecked_Type_Conversion
         then
            return False;

         --  Here we have a checked or unchecked type conversion

         else
            --  Since a False value returned from this function will cause
            --  a compilation error, if we have a constraint error to raise
            --  we should accept that as a (formerly) valid variable
            --  and go on, unless it fails the parenthesis count test.

            --  Do we need a Comes_From_Source for UC case test here ???
            --  Unchecked conversion is not a variable in RM ???

            return (Is_Variable (Expression (AV))
                     or else Raises_Constraint_Error (Expression (A)))
              and then Paren_Count (Expression (AV)) = 0;
         end if;
      end OK_Variable_For_Out_Formal;

      --------------------
      -- Insert_Default --
      --------------------

      procedure Insert_Default is
         Actval : Node_Id;
         Assoc  : Node_Id;
         Defun  : constant Entity_Id := Default_Expr_Function (F);

      begin
         --  Note that the default expression is a simple identifier (which is
         --  a function call) or a literal. In either case we can just copy the
         --  node to use as the default expression. If the original expression
         --  was not a literal, a thunk function was created, and the default
         --  expression is simply a call to this function.

         --  If we created a default expression function, then the default
         --  expression that we need is simply a call to this function.

         if Present (Defun) then
            Actval :=
              Make_Function_Call (Loc,
                Name => New_Occurrence_Of (Defun, Loc));

            --  Analyze and resolve the function call. Note that we use the
            --  type of the function for this resolution, which may not be
            --  the same as the type of the formal in the case of a derived
            --  subprogram

            Set_Parent (Actval, N);
            Analyze_And_Resolve (Actval, Etype (Defun));

         --  Otherwise, the default expression must be a simple literal
         --  or identifier (referencing a variable or a constant), and the
         --  default expression is simply a copy of this item.

         --  Note that we do a full New_Copy_Tree, so that any associated
         --  Itypes are properly copied. This may not be needed any more,
         --  but it does no harm as a safety measure! Note also that we
         --  do not reresolve the default expression since it is already
         --  resolved, and it would be wrong to reresolve it in the derived
         --  subprogram case anyway. However, the defaults of a generic
         --  formal may be out of bounds of the corresponding actual (see
         --  cc1311b) and an additional check may be required.

         elsif Present (Default_Value (F)) then
            Actval := New_Copy_Tree (Default_Value (F));

            if Is_Overloadable (Nam)
              and then Present (Alias (Nam))
              and then Is_Scalar_Type (Etype (F))
            then
               Set_Do_Range_Check (Actval);
            end if;

         else
            --  Missing argument in call, nothing to insert.
            return;
         end if;

         --  If the default expression raises constraint error, then just
         --  silently replace it with an N_Raise_Constraint_Error node,
         --  since we already gave the warning on the subprogram spec.

         if Raises_Constraint_Error (Actval) then
            Rewrite_Substitute_Tree (Actval,
              Make_Raise_Constraint_Error (Loc));
            Set_Raises_Constraint_Error (Actval);
            Set_Etype (Actval, Etype (F));
         end if;

         Assoc :=
           Make_Parameter_Association (Loc,
             Explicit_Actual_Parameter => Actval,
             Selector_Name => Make_Identifier (Loc, Chars (F)));

         --  Case of insertion is first named actual

         if No (Prev) or else
            Nkind (Parent (Prev)) /= N_Parameter_Association
         then
            Set_Next_Named_Actual (Assoc, First_Named_Actual (N));
            Set_First_Named_Actual (N, Actval);

            if No (Prev) then
               if not Present (Parameter_Associations (N)) then
                  Set_Parameter_Associations (N, New_List (Assoc));
               else
                  Append (Assoc, Parameter_Associations (N));
               end if;

            else
               Insert_After (Prev, Assoc);
            end if;

         --  Case of insertion is not first named actual

         else
            Set_Next_Named_Actual
              (Assoc, Next_Named_Actual (Parent (Prev)));
            Set_Next_Named_Actual (Parent (Prev), Actval);
            Append (Assoc, Parameter_Associations (N));
         end if;

         Mark_Rewrite_Insertion (Assoc);
         Mark_Rewrite_Insertion (Actval);

         Prev := Actval;
      end Insert_Default;

   --  Start of processing for Resolve_Actuals

   begin
      A := First_Actual (N);
      F := First_Formal (Nam);

      while Present (F) loop

         if Present (A)
           and then (Nkind (Parent (A)) /= N_Parameter_Association
                       or else
                     Chars (Selector_Name (Parent (A))) = Chars (F))
         then
            --  If the formal is Out or In_Out do not resolve and expand the
            --  conversion, because it is subsequently expanded into explicit
            --  temporaries and assignments. However, the object of the
            --  conversion can be resolved.

            if (Ekind (F) = E_In_Out_Parameter
                  or else
                Ekind (F) = E_Out_Parameter)
              and then Nkind (A) = N_Type_Conversion
            then
               if Valid_Conversion (A) then
                  Resolve (Expression (A), Etype (Expression (A)));
               end if;

            else
               Resolve (A, Etype (F));
            end if;

            A_Typ := Etype (A);
            F_Typ := Etype (F);

            if Ekind (F) /= E_In_Parameter
              and then not OK_Variable_For_Out_Formal (A)
            then
               Error_Msg_NE ("actual for& must be a variable", A, F);
            end if;

            if Ekind (F) /= E_Out_Parameter then
               Check_Unset_Reference (A);
            end if;

            --  Apply appropriate range checks for in, out, and in-out
            --  parameters.  Out and in-out parameters also need a separate
            --  check, if there is a type conversion, to make sure the return
            --  value meets the constraints of the variable before the
            --  conversion.

            --  Gigi looks at the check flag and uses the appropriate types.
            --  For now since one flag is used there is an optimization which
            --  might not be done in the In Out case since Gigi does not do
            --  any analysis. More thought required about this ???

            if Ekind (F) = E_In_Parameter
              or else Ekind (F) = E_In_Out_Parameter
            then
               if Is_Scalar_Type (Etype (A)) then
                  Apply_Scalar_Range_Check (A, F_Typ);

               elsif Is_Array_Type (Etype (A)) then
                  Apply_Length_Check (A, F_Typ);

               elsif Is_Record_Type (F_Typ)
                 and then Has_Discriminants (F_Typ)
                 and then Is_Constrained (F_Typ)
                 and then (not Is_Derived_Type (F_Typ)
                             or else Comes_From_Source (Nam))
               then
                  Apply_Discriminant_Check (A, F_Typ);

               else
                  Apply_Range_Check (A, F_Typ);
               end if;
            end if;

            if Ekind (F) = E_Out_Parameter
              or else Ekind (F) = E_In_Out_Parameter
            then

               if Nkind (A) = N_Type_Conversion then
                  if Is_Scalar_Type (A_Typ) then
                     Apply_Scalar_Range_Check
                       (Expression (A), Etype (Expression (A)), A_Typ);
                  else
                     Apply_Range_Check
                       (Expression (A), Etype (Expression (A)), A_Typ);
                  end if;
               else
                  if Is_Scalar_Type (F_Typ) then
                     Apply_Scalar_Range_Check (A, A_Typ, F_Typ);
                  else
                     Apply_Range_Check (A, A_Typ, F_Typ);
                  end if;

               end if;
            end if;

            --  Check that subprgrams don't have improper controlling
            --  arguments (RM 3.9.2 (9))

            if Is_Controlling_Formal (F) then
               Set_Is_Controlling_Actual (A);
            end if;

            if (Is_Class_Wide_Type (A_Typ) or else Is_Dynamically_Tagged (A))
              and then not Is_Class_Wide_Type (F_Typ)
              and then not Is_Controlling_Formal (F)
            then
               Error_Msg_N ("class-wide argument not allowed here!", A);
               if Is_Subprogram (Nam) then
                  Error_Msg_Node_2 := F_Typ;
                  Error_Msg_NE
                    ("& is not a primitive operation of &!", A, Nam);
               end if;

            elsif Is_Access_Type (A_Typ)
              and then Is_Class_Wide_Type (Designated_Type (A_Typ))
              and then not Is_Class_Wide_Type (Designated_Type (F_Typ))
              and then not Is_Controlling_Formal (F)
            then
               Error_Msg_N
                 ("access to class-wide argument not allowed here!", A);
               if Is_Subprogram (Nam) then
                  Error_Msg_Node_2 := Designated_Type (F_Typ);
                  Error_Msg_NE
                    ("& is not a primitive operation of &!", A, Nam);
               end if;
            end if;

            Eval_Actual (A);
            Prev := A;
            A := Next_Actual (A);

         else
            Insert_Default;
         end if;

         F := Next_Formal (F);
      end loop;

   end Resolve_Actuals;

   -----------------------
   -- Resolve_Allocator --
   -----------------------

   procedure Resolve_Allocator (N : Node_Id; Typ : Entity_Id) is
      E : constant Node_Id    := Expression (N);

   begin
      --  Replace general access with specific type

      if Ekind (Etype (N)) = E_Allocator_Type then
         Set_Etype (N, Base_Type (Typ));
      end if;

      if Is_Abstract (Typ) then
         Error_Msg_N ("type of allocator cannot be abstract",  N);
      end if;

      --  For qualified expression, resolve the expression using the
      --  given subtype (nothing to do for type mark, subtype indication)

      if Nkind (E) = N_Qualified_Expression then
         Resolve (Expression (E), Etype (E));
         Check_Unset_Reference (Expression (E));

      --  For a subtype mark or subtype indication, freeze the subtype

      else
         Freeze_Expression (E);

         if Is_Access_Constant (Typ) then
            Error_Msg_N
              ("initialization required for access-to-constant allocator", N);
         end if;
      end if;

      --  Check for allocation from an empty storage pool

      if No_Pool_Assigned (Typ) then
         declare
            Loc : constant Source_Ptr := Sloc (N);

         begin
            Error_Msg_N ("?allocation from empty storage pool!", N);
            Error_Msg_N ("?Storage_Error will be raised at run time!", N);
            Insert_Action (N,
              Make_Raise_Statement (Loc,
                Name => New_Occurrence_Of (Standard_Storage_Error, Loc)));
         end;
      end if;
   end Resolve_Allocator;

   ---------------------------
   -- Resolve_Arithmetic_Op --
   ---------------------------

   --  Used for resolving all arithmetic operators except exponentiation

   procedure Resolve_Arithmetic_Op (N : Node_Id; Typ : Entity_Id) is
      L  : constant Node_Id := Left_Opnd (N);
      R  : constant Node_Id := Right_Opnd (N);
      T  : Entity_Id;
      TL : Entity_Id := Base_Type (Etype (L));
      TR : Entity_Id := Base_Type (Etype (R));

      B_Typ : constant Entity_Id := Base_Type (Typ);
      --  We do the resolution using the base type, because intermediate values
      --  in expressions always are of the base type, not a subtype of it.

      function Is_Integer_Or_Universal (T : Entity_Id) return Boolean;
      --  Return True iff given type is Integer or universal real/integer

      procedure Set_Operand_Type (N : Node_Id);
      --  Set operand type to T if universal

      procedure Set_Mixed_Mode_Operand (N : Node_Id; T : Entity_Id);
      --  Choose type of integer literal in fixed-point operation to conform
      --  to available fixed-point type.

      function Universal_Interpretation (N : Node_Id) return Entity_Id;
      --  Find universal type of operand, if any.

      function Is_Integer_Or_Universal (T : Entity_Id) return Boolean is
      begin
         return Base_Type (T) = Base_Type (Standard_Integer)
           or else T = Universal_Integer
           or else T = Universal_Real;
      end Is_Integer_Or_Universal;

      function Universal_Interpretation (N : Node_Id) return Entity_Id is
         Index : Interp_Index;
         It    : Interp;

      begin
         if not Is_Overloaded (N) then

            if Etype (N) = Universal_Integer
               or else Etype (N) = Universal_Real
            then
               return Etype (N);
            else
               return Empty;
            end if;

         else
            Get_First_Interp (N, Index, It);

            while Present (It.Typ) loop

               if It.Typ = Universal_Integer
                  or else It.Typ = Universal_Real
               then
                  return It.Typ;
               end if;

               Get_Next_Interp (Index, It);
            end loop;

            return Empty;
         end if;
      end Universal_Interpretation;

      procedure Set_Operand_Type (N : Node_Id) is
      begin
         if Etype (N) = Universal_Integer
           or else Etype (N) = Universal_Real
         then
            Set_Etype (N, T);
         end if;
      end Set_Operand_Type;

      procedure Set_Mixed_Mode_Operand (N : Node_Id; T : Entity_Id) is
      begin
         if Etype (N) = Universal_Integer then

            --  A universal integer literal is resolved as standard integer
            --  except in the case of a fixed-point result, where we leave
            --  it as universal (to be handled by Exp_Fixd later on)

            if Is_Fixed_Point_Type (T) then
               Resolve (N, Universal_Integer);
            else
               Resolve (N, Standard_Integer);
            end if;

         elsif Etype (N) = Universal_Real
           and then (T = Base_Type (Standard_Integer)
                      or else T = Universal_Real)
         then
            --  A universal real can appear in a fixed-type context. We resolve
            --  the literal with that context, even though this might raise an
            --  exception prematurely (the other operand may be zero).

            Resolve (N, B_Typ);

         elsif Etype (N) = Base_Type (Standard_Integer)
           and then T = Universal_Real
           and then Is_Overloaded (N)
         then
            --  Integer arg in mixed-mode operation. Resolve with universal
            --  type, in case preference rule must be applied.

            Resolve (N, Universal_Integer);

         elsif Etype (N) = T
           and then B_Typ /= Universal_Fixed
         then
            --  Not a mixed-mode operation. Resolve with context.

            Resolve (N, B_Typ);

         elsif Etype (N) = Any_Fixed then
            --  N may itself be a mixed-mode operation, so use context type.

            Resolve (N, B_Typ);

         else
            Resolve (N, Etype (N));
         end if;
      end Set_Mixed_Mode_Operand;

   --  Start of processing for Resolve_Arithmetic_Op

   begin
      --  Special-case for mixed-mode universal expressions or fixed point
      --  type operation: each argument is resolved separately. The same
      --  treatment is required if one of the operands of a fixed point
      --  operation is universal real, since in this case we don't do a
      --  conversion to a specific fixed-point type (instead the expander
      --  takes care of the case).

      if (B_Typ = Universal_Integer
           or else B_Typ = Universal_Real)
        and then Present (Universal_Interpretation (L))
        and then Present (Universal_Interpretation (R))
      then
         Resolve (L, Universal_Interpretation (L));
         Resolve (R, Universal_Interpretation (R));
         Set_Etype (N, B_Typ);

      elsif (B_Typ = Universal_Real
           or else Etype (N) = Universal_Fixed
           or else Etype (N) = Any_Fixed
           or else (Is_Fixed_Point_Type (B_Typ)
                     and then (Is_Integer_Or_Universal (TL)
                                 or else
                               Is_Integer_Or_Universal (TR))))
        and then (Nkind (N) = N_Op_Multiply or else
                  Nkind (N) = N_Op_Divide)
      then
         if TL = Universal_Integer or else TR = Universal_Integer then
            Check_For_Visible_Operator (N, B_Typ);
         end if;

         Set_Mixed_Mode_Operand (L, TR);
         Set_Mixed_Mode_Operand (R, TL);

         if Etype (N) = Universal_Fixed then
            if B_Typ = Universal_Fixed
              and then Nkind (Parent (N)) /= N_Type_Conversion
              and then Nkind (Parent (N)) /= N_Unchecked_Type_Conversion
            then
               Error_Msg_N
                 ("result type can''t be determined from context", N);
            end if;

            Set_Etype (N, B_Typ);

         elsif Is_Fixed_Point_Type (B_Typ)
           and then (Is_Integer_Or_Universal (Etype (L))
                       or else Nkind (L) = N_Real_Literal
                       or else Nkind (R) = N_Real_Literal
                       or else
                     Is_Integer_Or_Universal (Etype (R)))
         then
            Set_Etype (N, B_Typ);

         elsif Etype (N) = Any_Fixed then

            --  If no previous errors, this is only possible if one operand
            --  is overloaded and the context is universal. Resolve as such.

            Set_Etype (N, B_Typ);
         end if;

      else
         if (TL = Universal_Integer or else TL = Universal_Real)
           and then (TR = Universal_Integer or else TR = Universal_Real)
         then
            Check_For_Visible_Operator (N, B_Typ);
         end if;

         Resolve (L, B_Typ);
         Resolve (R, B_Typ);

         --  If one of the arguments was resolved to a non-universal type.
         --  label the result of the operation itself with the same type.
         --  Do the same for the universal argument, if any.

         T := Intersect_Types (L, R);
         Set_Etype (N, Base_Type (T));
         Set_Operand_Type (L);
         Set_Operand_Type (R);
      end if;

      Eval_Arithmetic_Op (N);

      --  Set overflow checking bit. Much cleverer code needed here eventually
      --  and perhaps the Resolve routines should be separated for the various
      --  arithmetic operations, since they will need different processing. ???

      if Nkind (N) in N_Op then
         if not Overflow_Checks_Suppressed (Etype (N)) then
            Set_Do_Overflow_Check (N);
         end if;
      end if;

      Check_Unset_Reference (L);
      Check_Unset_Reference (R);

   end Resolve_Arithmetic_Op;

   ------------------
   -- Resolve_Call --
   ------------------

   procedure Resolve_Call (N : Node_Id; Typ : Entity_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Subp    : constant Node_Id    := Name (N);
      Nam     : Entity_Id;
      I       : Interp_Index;
      It      : Interp;
      Norm_OK : Boolean;
      Scop    : Entity_Id;

      function Is_Predefined_Op (Nam : Entity_Id) return Boolean;
      --  Utility to check whether the name in the call is a predefined
      --  operator, in which case the call is made into an operator node.

      function Is_Predefined_Op (Nam : Entity_Id) return Boolean is
      begin
         return Is_Intrinsic_Subprogram (Nam)
           and then Chars (Nam) in Any_Operator_Name;
      end Is_Predefined_Op;

   --  Start of processing for Resolve_Call

   begin
      --  The context imposes a unique interpretation with type Typ on
      --  a procedure or function call. Find the entity of the subprogram
      --  that yields the expected type, and propagate the corresponding
      --  formal constraints on the actuals. The caller has established
      --  that an interpretation exists, and emitted an error if not unique.

      --  First deal with the case of a call to an access-to-subprogram,
      --  dereference made explicit in Analyze_Call.

      if Ekind (Etype (Subp)) = E_Subprogram_Type then

         if not Is_Overloaded (Subp) then
            Nam := Etype (Subp);

         else
            --  Find the interpretation whose type (a subprogram type)
            --  has a return type that is compatible with the context.

            Get_First_Interp (Subp,  I, It);
            while Present (It.Typ) loop

               if Covers (Typ, Etype (It.Typ)) then
                  Nam := It.Typ;
                  exit;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end if;

         --  If the prefix is not an entity, then resolve it

         if not Is_Entity_Name (Subp) then
            Resolve (Subp, Nam);
         end if;

      --  If this is a procedure call which is really an entry call, do
      --  the conversion of the procedure call to an entry call. Protected
      --  operations use the same circuitry because the name in the call
      --  can be an arbitrary expression with special resolution rules.

      elsif Nkind (Subp) = N_Selected_Component
        or else Nkind (Subp) = N_Indexed_Component
        or else (Is_Entity_Name (Subp)
                  and then Ekind (Entity (Subp)) = E_Entry)
      then
         Resolve_Entry_Call (N, Typ);
         Validate_Non_Static_Call (N);
         return;

      --  Normal subprogram call with name established in Resolve

      elsif not (Is_Type (Entity (Subp))) then
         Nam := Entity (Subp);
         Set_Entity_With_Style_Check (Subp, Nam);

      --  Otherwise we must have the case of an overloaded call

      else
         pragma Assert (Is_Overloaded (Subp));
         Get_First_Interp (Subp,  I, It);

         while Present (It.Typ) loop
            if Covers (Typ, It.Typ) then
               Nam := It.Nam;
               Set_Entity_With_Style_Check (Subp, Nam);
               exit;
            end if;

            Get_Next_Interp (I, It);
         end loop;

      end if;

      --  Check that a procedure call does not occur in the context
      --  of the entry call statement of a conditional or timed
      --  entry call.  Note that the case of a call to a subprogram
      --  renaming of an entry will also be rejected.  The test
      --  for N not being an N_Entry_Call_Statement is defensive,
      --  covering the possibility that the processing of entry
      --  calls might reach this point due to later modifications
      --  of the code above.

      if Nkind (Parent (N)) = N_Entry_Call_Alternative
        and then Nkind (N) /= N_Entry_Call_Statement
        and then Entry_Call_Statement (Parent (N)) = N
      then
         Error_Msg_N ("entry call required in select statement", N);
      end if;

      --  Freeze the subprogram name if not in default expression.  Note
      --  that we freeze procedure calls as well as function calls.
      --  Procedure calls are not frozen according to the rules (RM
      --  13.14(14)) because it is impossible to have a procedure call to
      --  a non-frozen procedure in pure Ada, but in the code that we
      --  generate in the expander, this rule needs extending because we
      --  can generate procedure calls that need freezing.

      if Is_Entity_Name (Subp) and then not In_Default_Expression then
         Freeze_Expression (Subp);
      end if;

      --  The type of the call is the type returned by the subprogram

      if Is_Predefined_Op (Nam) then
         Set_Etype (N, Typ);

      --  If the subprogram returns an array type, and the context
      --  requires the component type of that array type, the node is
      --  really an indexing of the parameterless call. Resolve as such.

      elsif Needs_No_Actuals (Nam)
        and then
          ((Is_Array_Type (Etype (Nam))
                   and then Covers (Typ, Component_Type (Etype (Nam))))
             or else (Is_Access_Type (Etype (Nam))
                        and then Is_Array_Type (Designated_Type (Etype (Nam)))
                        and then
                          Covers (Typ,
                            Component_Type (Designated_Type (Etype (Nam))))))
      then
         declare
            Index_Node : Node_Id;

         begin
            Validate_Non_Static_Call (N);
            if Component_Type (Etype (Nam)) /= Any_Type then
               Index_Node :=
                 Make_Indexed_Component (Loc,
                   Prefix =>
                     Make_Function_Call (Loc,
                       Name => New_Occurrence_Of (Nam, Loc)),
                   Expressions => Parameter_Associations (N));

               --  Since we are correcting a node classification error made by
               --  the parser, we call Replace_Substitute_Tree rather than
               --  Rewrite_Substitute_Tree.

               Replace_Substitute_Tree (N, Index_Node);
               Set_Etype (Prefix (N), Etype (Nam));
               Set_Etype (N, Typ);
               Resolve_Indexed_Component (N, Typ);
            end if;

            return;
         end;

      else
         Set_Etype (N, Etype (Nam));
      end if;

      --  In the case where the call is to an overloaded subprogram, Analyze
      --  calls Normalize_Actuals once per overloaded subprogram. Therefore in
      --  such a case Normalize_Actuals needs to be called once more to order
      --  the actuals correctly. Otherwise the call will have the ordering
      --  given by the last overloaded subprogram whether this is the correct
      --  one being called or not.

      if Is_Overloaded (Subp) then
         Normalize_Actuals (N, Nam, False, Norm_OK);
         pragma Assert (Norm_OK);
      end if;

      --  In any case, call is fully resolved now. Reset Overload flag, to
      --  prevent subsequent overload resolution if node is analyzed again

      Set_Is_Overloaded (Subp, False);
      Set_Is_Overloaded (N, False);

      --  If we are calling the current subprogram from immediately within
      --  its body, then that is the case where we can sometimes detect
      --  cases of infinite recursion statically.

      Scop := Current_Scope;

      if Nam = Scop and then
        Check_Infinite_Recursion (N)
      then
         --  Here we detected and flagged an infinite recursion, so we do
         --  not need to test the case below for further warnings.

         null;

      --  If call is to immediately containing subprogram, then check for
      --  the case of a possible runtime detectable infinite recursion.

      else
         while Scop /= Standard_Standard loop
            if Nam = Scop then

               --  If the recursive call is to a paramterless procedure, then
               --  even if we can't statically detect infinite recursion, this
               --  is pretty suspicious, and we output a warning. Furthermore,
               --  we will try later to detect some cases here at runtime by
               --  expanding checking code (see Detect_Infinite_Recursion in
               --  package Exp_Ch6).

               if No (Parameter_Associations (N))
                 and then Etype (Nam) = Standard_Void_Type
                 and then not Error_Posted (N)
               then
                  Set_Has_Recursive_Call (Nam);
                  Error_Msg_N ("possible infinite recursion?", N);
                  Error_Msg_N ("Storage_Error may be raised at runtime?", N);
               end if;

               exit;
            end if;

            Scop := Scope (Scop);
         end loop;
      end if;

      --  If subprogram name is a predefined operator, it was given in
      --  functional notation. Replace call node with operator node, so
      --  that actuals can be resolved appropriately.

      if Is_Predefined_Op (Nam) or else Ekind (Nam) = E_Operator then
         Make_Call_Into_Operator (N, Typ, Entity (Name (N)));
         return;

      elsif Present (Alias (Nam))
        and then Is_Predefined_Op (Alias (Nam))
      then
         Resolve_Actuals (N, Nam);
         Make_Call_Into_Operator (N, Typ, Alias (Nam));
         return;
      end if;

      --  Create a transient scope if the resulting type requires it

      if Expander_Active
        and then Is_Type (Etype (Nam))
        and then Requires_Transient_Scope (Etype (Nam))
        and then not (Is_Intrinsic_Subprogram (Nam)
                      and then Present (Parent (Nam))
                      and then Present (Generic_Parent (Parent (Nam)))
                      and then Chars (Generic_Parent (Parent (Nam))) =
                          Name_Unchecked_Conversion)
      then
         Establish_Transient_Scope (N, Sec_Stack => True);
      end if;

      --  Propagate interpretation to actuals, and add default expressions
      --  where needed.

      if Present (First_Formal (Nam)) then
         Resolve_Actuals (N, Nam);

         --  Overloaded literals are rewritten as function calls, for
         --  purpose of resolution.  After resolution, we can replace
         --  the call with the literal itself.

      elsif Ekind (Nam) = E_Enumeration_Literal then
         Copy_Node (Subp, N);
         Resolve_Entity_Name (N, Typ);

         --  Avoid validation, since it is a static function call.

         return;
      end if;

      --  If the subprogram is a primitive operation, check whether or not
      --  it is a correct dispatching call.

      if Is_Overloadable (Nam)
        and then Is_Dispatching_Operation (Nam)
      then
         Check_Dispatching_Call (N);

            --  if the subprogram is abstract, check that the call has a
            --  controlling argument (i.e. is dispatching) or is disptaching on
            --  result

         if Is_Abstract (Nam)
           and then No (Controlling_Argument (N))
           and then not Is_Class_Wide_Type (Typ)
           and then not Is_Tag_Indeterminate (N)
         then
            Error_Msg_N ("call to abstract subprogram must be dispatching", N);
         end if;

      elsif Is_Abstract (Nam) then
         Error_Msg_NE ("cannot call abstract subprogram &!", N, Nam);
      end if;

      if Is_Intrinsic_Subprogram (Nam) then
         Check_Intrinsic_Call (N);
      end if;

      --  If we fall through we definitely have a non-static call

      Validate_Non_Static_Call (N);

   end Resolve_Call;

   -------------------------------
   -- Resolve_Character_Literal --
   -------------------------------

   procedure Resolve_Character_Literal (N : Node_Id; Typ : Entity_Id) is
      B_Typ : constant Entity_Id := Base_Type (Typ);
      C     : Entity_Id;

   begin
      --  Verify that the character does belong to the type of the context

      Set_Etype (N, B_Typ);
      Eval_Character_Literal (N);

      --  Wide_Character literals must always be defined, since the set of
      --  wide character literals is complete, i.e. if a character literal
      --  is accepted by the parser, then it is OK for wide character.

      if Root_Type (B_Typ) = Standard_Wide_Character then
         return;

      --  Always accept character literal for type Any_Character, which
      --  occurs in error situations and in comparisons of literals, both
      --  of which should accept all literals.

      elsif B_Typ = Any_Character then
         return;

      --  For Standard.Character or a type derived from it, check that
      --  the literal is in range

      elsif Root_Type (B_Typ) = Standard_Character then
         if In_Character_Range (Char_Literal_Value (N)) then
            return;
         end if;

      --  If the entity is already set, this has already been resolved in
      --  a generic context, or comes from expansion. Nothing else to do.

      elsif Present (Entity (N)) then
         return;

      --  Otherwise we have a user defined character type, and we can use
      --  the standard visibility mechanisms to locate the referenced entity

      else
         C := Current_Entity (N);

         while Present (C) loop
            if Etype (C) = B_Typ then
               Set_Entity_With_Style_Check (N, C);
               return;
            end if;

            C := Homonym (C);
         end loop;
      end if;

      --  If we fall through, then the literal does not match any of the
      --  entries of the enumeration type. This isn't just a constraint
      --  error situation, it is an illegality (see RM 4.2).

      Error_Msg_NE
        ("character not defined for }", N, First_Subtype (B_Typ));

   end Resolve_Character_Literal;

   ---------------------------
   -- Resolve_Comparison_Op --
   ---------------------------

   --  Context requires a boolean type, and plays no role in resolution.
   --  Processing identical to that for equality operators.

   procedure Resolve_Comparison_Op (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id   := Left_Opnd (N);
      R : constant Node_Id   := Right_Opnd (N);
      T : constant Entity_Id := Find_Unique_Type (L, R);

   begin
      Set_Etype (N, Typ);

      if T /= Any_Type then

         if T = Any_String
           or else T = Any_Composite
         then
            Error_Msg_N ("ambiguous operands for comparison", N);
            Set_Etype (N, Any_Type);
            return;

         else
            if Comes_From_Source (N)
              and then Has_Unchecked_Union (T)
            then
               Error_Msg_N
                ("cannot compare Unchecked_Union values", N);
            end if;

            Resolve (L, T);
            Resolve (R, T);
            Check_Unset_Reference (L);
            Check_Unset_Reference (R);
            Eval_Relational_Op (N);
         end if;
      end if;

   end Resolve_Comparison_Op;

   ------------------------------------
   -- Resolve_Conditional_Expression --
   ------------------------------------

   procedure Resolve_Conditional_Expression (N : Node_Id; Typ : Entity_Id) is
      Condition : constant Node_Id := First (Expressions (N));
      Then_Expr : constant Node_Id := Next (Condition);
      Else_Expr : constant Node_Id := Next (Then_Expr);

   begin
      Resolve (Condition, Standard_Boolean);
      Resolve (Then_Expr, Typ);
      Resolve (Else_Expr, Typ);

      Set_Etype (N, Typ);
      Eval_Conditional_Expression (N);
   end Resolve_Conditional_Expression;

   -----------------------------------------
   -- Resolve_Discrete_Subtype_Indication --
   -----------------------------------------

   procedure Resolve_Discrete_Subtype_Indication
     (N   : Node_Id;
      Typ : Entity_Id)
   is
      R : Node_Id;
      S : Entity_Id;

   begin
      Analyze (Subtype_Mark (N));
      S := Entity (Subtype_Mark (N));

      if Nkind (Constraint (N)) /= N_Range_Constraint then
         Error_Msg_N ("expect range constraint for discrete type", N);
         Set_Etype (N, Any_Type);

      else
         R := Range_Expression (Constraint (N));
         Analyze (R);

         if Base_Type (S) /= Base_Type (Typ) then
            Error_Msg_NE
              ("expect subtype of }", N, First_Subtype (Typ));

            --  Rewrite the constraint as a range of Typ
            --  to allow compilation to proceed further.

            Set_Etype (N, Typ);
            Rewrite_Substitute_Tree (Low_Bound (R),
              Make_Attribute_Reference (Sloc (Low_Bound (R)),
                Prefix =>         New_Occurrence_Of (Typ, Sloc (R)),
                Attribute_Name => Name_First));
            Rewrite_Substitute_Tree (High_Bound (R),
              Make_Attribute_Reference (Sloc (High_Bound (R)),
                Prefix =>         New_Occurrence_Of (Typ, Sloc (R)),
                Attribute_Name => Name_First));

         else
            Resolve (R, Typ);
            Set_Etype (N, Etype (R));

            --  Additionally, we must check that the bounds are compatible
            --  with the given subtype, which might be different from the
            --  type of the context.

            Apply_Range_Check (R, S);

            --  ??? If the above check statically detects a Constraint_Error
            --  it replaces the offending bound(s) of the range R with a
            --  Constraint_Error node. When the itype which uses these bounds
            --  is frozen the resulting call to Duplicate_Subexpr generates
            --  a new temporary for the bounds.

            --  Unfortunately there are other itypes that are also made depend
            --  on these bounds, so when Duplicate_Subexpr is called they get
            --  a forward reference to the newly created temporaries and Gigi
            --  aborts on such forward references. This is probably sign of a
            --  more fundamental problem somewhere else in either the order of
            --  itype freezing or the way certain itypes are constructed.

            --  To get around this problem we call Remove_Side_Effects right
            --  away if either bounds of R are a Constraint_Error.

            declare
               L : Node_Id := Low_Bound (R);
               H : Node_Id := High_Bound (R);

            begin
               if Nkind (L) = N_Raise_Constraint_Error then
                  Remove_Side_Effects (L);
               end if;

               if Nkind (H) = N_Raise_Constraint_Error then
                  Remove_Side_Effects (H);
               end if;
            end;

            Check_Unset_Reference (Low_Bound  (R));
            Check_Unset_Reference (High_Bound (R));
         end if;
      end if;
   end Resolve_Discrete_Subtype_Indication;

   -------------------------
   -- Resolve_Entity_Name --
   -------------------------

   --  Used to resolve identifiers and expanded names

   procedure Resolve_Entity_Name (N : Node_Id; Typ : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      E   : constant Entity_Id := Entity (N);

   begin
      --  Replace named numbers by corresponding literals. Note that this is
      --  the one case where Resolve_Entity_Name must reset the Etype, since
      --  it is currently marked as universal.

      if Ekind (E) = E_Named_Integer then
         Set_Etype (N, Typ);
         Eval_Named_Integer (N);

      elsif Ekind (E) = E_Named_Real then
         Set_Etype (N, Typ);
         Eval_Named_Real (N);

      --  Allow use of subtype only if it is a concurrent type where we are
      --  currently inside the body. This will eventually be expanded
      --  into a call to Self (for tasks) or _object (for protected
      --  objects). Any other use of a subtype is invalid.

      elsif Is_Type (E) then
         if Is_Concurrent_Type (E)
           and then In_Open_Scopes (E)
         then
            null;
         else
            Error_Msg_N
               ("Invalid use of subtype mark in expression or call", N);
         end if;

      --  Check discriminant use if entity is discriminant in current scope.
      --  I don't understand Has_Completion check, needs a comment ???

      elsif Ekind (E) = E_Discriminant
        and then Scope (E) = Current_Scope
        and then not Has_Completion (Current_Scope)
      then
         Check_Discriminant_Use (N);

      --  In all other cases, just do the possible static evaluation

      else
         Eval_Entity_Name (N);
      end if;
   end Resolve_Entity_Name;


   -------------------
   -- Resolve_Entry --
   -------------------

   procedure Resolve_Entry (Entry_Name : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (Entry_Name);
      Nam    : Entity_Id;
      New_N  : Node_Id;
      S      : Entity_Id;
      Tsk    : Entity_Id;
      E_Name : Node_Id;
      Index  : Node_Id;

   begin
      --  Find name of entry being called, and resolve prefix of name
      --  with its own type. The prefix can be overloaded, and the name
      --  and signature of the entry must be taken into account.

      if Nkind (Entry_Name) = N_Indexed_Component then

         --  Case of dealing with entry family within the current tasks

         E_Name := Prefix (Entry_Name);

      else
         E_Name := Entry_Name;
      end if;

      if Is_Entity_Name (E_Name) then
         --  Entry call to an entry (or entry family) in the current task.
         --  This is legal even though the task will deadlock. Rewrite as
         --  call to current task.

         --  This can also be a call to an entry in  an enclosing task.
         --  If this is a single task, we have to retrieve its name,
         --  because the scope of the entry is the task type, not the
         --  object. If the enclosing task is a task type, the identity
         --  of the task is given by its own self variable.

         S := Scope (Entity (E_Name));

         for J in reverse 0 .. Scope_Stack.Last loop

            if Is_Task_Type (Scope_Stack.Table (J).Entity)
              and then not Comes_From_Source (S)
            then
               --  S is an enclosing task or protected object. The concurrent
               --  declaration has been converted into a type declaration, and
               --  the object itself has an object declaration that follows
               --  the type in the same declarative part.

               Tsk := Next_Entity (S);

               while Etype (Tsk) /= S loop
                  Tsk := Next_Entity (Tsk);
               end loop;

               S := Tsk;
               exit;

            elsif S = Scope_Stack.Table (J).Entity then

               --  Call to current task. Will be transformed into call to Self

               exit;

            end if;
         end loop;

         New_N :=
           Make_Selected_Component (Loc,
             Prefix => New_Occurrence_Of (S, Loc),
             Selector_Name =>
               New_Occurrence_Of (Entity (E_Name), Loc));
         Rewrite_Substitute_Tree (E_Name, New_N);
         Analyze (E_Name);

      elsif Nkind (Entry_Name) = N_Selected_Component
        and then Is_Overloaded (Prefix (Entry_Name))
      then
         --  Use the entry name (which must be unique at this point) to
         --  find the prefix that returns the corresponding task type or
         --  protected type.

         declare
            Pref : Node_Id := Prefix (Entry_Name);
            I    : Interp_Index;
            It   : Interp;
            Ent  : Entity_Id :=  Entity (Selector_Name (Entry_Name));

         begin
            Get_First_Interp (Pref, I, It);

            while Present (It.Typ) loop

               if Scope (Ent) = It.Typ then
                  Set_Etype (Pref, It.Typ);
                  exit;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;
      end if;

      if Nkind (Entry_Name) = N_Selected_Component then
         Resolve (Prefix (Entry_Name), Etype (Prefix (Entry_Name)));

      elsif Nkind (Entry_Name) = N_Indexed_Component then
         Nam := Entity (Selector_Name (Prefix (Entry_Name)));
         Resolve (Prefix (Prefix (Entry_Name)),
                   Etype (Prefix (Prefix (Entry_Name))));

         Index :=  First (Expressions (Entry_Name));
         Resolve (Index, Entry_Index_Type (Nam));

         --  Up to this point the expression could have been the actual
         --  in a simple entry call, and be given by a named association.

         if Nkind (Index) = N_Parameter_Association then
            Error_Msg_N ("expect expression for entry index", Index);
         else
            Apply_Range_Check (Index, Entry_Index_Type (Nam));
         end if;

      else
         pragma Assert (False); null;
      end if;

   end Resolve_Entry;

   ------------------------
   -- Resolve_Entry_Call --
   ------------------------

   procedure Resolve_Entry_Call (N : Node_Id; Typ : Entity_Id) is
      Entry_Name  : constant Node_Id    := Name (N);
      Loc         : constant Source_Ptr := Sloc (Entry_Name);
      Actuals     : List_Id;
      First_Named : Node_Id;
      Nam         : Entity_Id;
      Obj         : Node_Id;

   begin
      --  Processing of the name is similar for entry calls and protected
      --  operation calls. Once the entity is determined, we can complete
      --  the resolution of the actuals.

      Resolve_Entry (Entry_Name);

      if Nkind (Entry_Name) = N_Selected_Component then

         --  Simple entry call.

         Nam := Entity (Selector_Name (Entry_Name));
         Obj := Prefix (Entry_Name);

      elsif Nkind (Entry_Name) = N_Indexed_Component then

         --  Call to member of entry family.

         Nam := Entity (Selector_Name (Prefix (Entry_Name)));
         Obj := Prefix (Prefix (Entry_Name));
      end if;

      if Ekind (Nam) = E_Function
        and then Needs_No_Actuals (Nam)
        and then Present (Parameter_Associations (N))
        and then
          (Is_Array_Type (Etype (Nam))
            or else (Is_Access_Type (Etype (Nam))
                      and then Is_Array_Type (Designated_Type (Etype (Nam)))))
      then
         declare
            Index_Node : Node_Id;

         begin
            Index_Node :=
              Make_Indexed_Component (Loc,
                Prefix =>
                  Make_Function_Call (Loc,
                    Name => Relocate_Node (Entry_Name)),
                Expressions => Parameter_Associations (N));

            --  Since we are correcting a node classification error made by
            --  the parser, we call Replace_Substitute_Tree rather than
            --  Rewrite_Substitute_Tree.

            Replace_Substitute_Tree (N, Index_Node);
            Set_Etype (Prefix (N), Etype (Nam));
            Set_Etype (N, Typ);
            Resolve_Indexed_Component (N, Typ);
            return;
         end;
      end if;

      Resolve_Actuals (N, Nam);

      --  Verify that a procedure call cannot masquerade as an entry
      --  call where an entry call is expected.

      if Nkind (Parent (N)) = N_Entry_Call_Alternative
        and then Ekind (Nam) = E_Procedure
        and then N = Entry_Call_Statement (Parent (N))
      then
         Error_Msg_N ("entry call required in select statement", N);
      end if;

      --  After resolution, entry calls and protected procedure calls
      --  are changed into entry calls, for expansion. The structure
      --  of the node does not change, so it can safely be done in place.
      --  Protected function calls must keep their structure because they
      --  are subexpressions.

      if Ekind (Nam) /= E_Function then

         --  A protected operation that is not a function may modify the
         --  corresponding object, and cannot apply to a constant.
         --  If this is an internal call, the prefix is the type itself.

         if Is_Protected_Type (Scope (Nam))
           and then not Is_Variable (Obj)
           and then (not Is_Entity_Name (Obj)
                       or else not Is_Type (Entity (Obj)))
         then
            Error_Msg_N
              ("prefix of protected procedure or entry call must be variable",
               Entry_Name);
         end if;

         Actuals := Parameter_Associations (N);
         First_Named := First_Named_Actual (N);

         --  This is a place where we can use Replace_Substitute_Tree, rather
         --  than Rewrite_Substitute_Tree, since we are correcting a node
         --  classification error made by the parser. But in order to recreate
         --  the original tree in an instance, we use Rewrite instead.

         Rewrite_Substitute_Tree (N,
           Make_Entry_Call_Statement (Loc,
             Name                   => Entry_Name,
             Parameter_Associations => Actuals));

         Set_First_Named_Actual (N, First_Named);
         Set_Analyzed (N, True);
      end if;

   end Resolve_Entry_Call;

   -------------------------
   -- Resolve_Equality_Op --
   -------------------------

   --  Both arguments must have the same type, and the boolean context
   --  does not participate in the resolution. The first pass verifies
   --  that the interpretation is not ambiguous, and the type of the left
   --  argument is correctly set, or is Any_Type in case of ambiguity.
   --  If both arguments are strings or aggregates, allocators, or Null,
   --  they are ambiguous even though they carry a single (universal) type.
   --  Diagnose this case here.

   procedure Resolve_Equality_Op (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id   := Left_Opnd (N);
      R : constant Node_Id   := Right_Opnd (N);
      T : Entity_Id := Find_Unique_Type (L, R);

      function Find_Unique_Access_Type return Entity_Id;
      --  In the case of allocators, make a last-ditch attempt to find a single
      --  access type with the right designated type. This is semantically
      --  dubious, and of no interest to any real code, but c48008a makes it
      --  all worthwhile.

      function Find_Unique_Access_Type return Entity_Id is
         Acc : Entity_Id;
         E   : Entity_Id;
         S   : Entity_Id := Current_Scope;

      begin
         if Ekind (Etype (R)) =  E_Allocator_Type then
            Acc := Designated_Type (Etype (R));

         elsif Ekind (Etype (L)) =  E_Allocator_Type then
            Acc := Designated_Type (Etype (L));

         else
            return Empty;
         end if;

         while S /= Standard_Standard loop
            E := First_Entity (S);

            while Present (E) loop

               if Is_Type (E)
                 and then Is_Access_Type (E)
                 and then Ekind (E) /= E_Allocator_Type
                 and then Designated_Type (E) = Base_Type (Acc)
               then
                  return E;
               end if;

               E := Next_Entity (E);
            end loop;

            S := Scope (S);
         end loop;

         return Empty;
      end Find_Unique_Access_Type;

   --  Start of processing for Resolve_Equality_Op

   begin
      Set_Etype (N, Base_Type (Typ));

      if T /= Any_Type then

         if T = Any_String
           or else T = Any_Composite
         then
            Error_Msg_N ("ambiguous operands for equality", N);
            Set_Etype (N, Any_Type);
            return;

         elsif T = Any_Access
           or else Ekind (T) = E_Allocator_Type
         then
            T := Find_Unique_Access_Type;

            if No (T) then
               Error_Msg_N ("ambiguous operands for equality", N);
               Set_Etype (N, Any_Type);
               return;
            end if;
         end if;

         if Comes_From_Source (N)
           and then Has_Unchecked_Union (T)
         then
            Error_Msg_N
              ("cannot compare Unchecked_Union values", N);
         end if;

         Resolve (L, T);
         Resolve (R, T);
         Check_Unset_Reference (L);
         Check_Unset_Reference (R);
         Eval_Relational_Op (N);
      end if;
   end Resolve_Equality_Op;

   ----------------------------------
   -- Resolve_Explicit_Dereference --
   ----------------------------------

   procedure Resolve_Explicit_Dereference (N : Node_Id; Typ : Entity_Id) is
      P  : Node_Id := Prefix (N);
      I  : Interp_Index;
      It : Interp;

   begin
      --  Now that we know the type, check that this is not a
      --  dereference of an uncompleted type.  Note that this
      --  is not entirely correct, because dereferences of
      --  private types are legal in default expressions.
      --  This consideration also applies to similar checks
      --  for allocators, qualified expressions, and type
      --  conversions.  ???

      Check_Fully_Declared (Typ, N);

      if Is_Overloaded (P) then

         --  Use the context type to select the prefix that has the
         --  correct designated type.

         Get_First_Interp (P, I, It);
         while Present (It.Typ) loop
            exit when Is_Access_Type (It.Typ)
              and then Covers (Typ, Designated_Type (It.Typ));

            Get_Next_Interp (I, It);
         end loop;

         Resolve (P, It.Typ);
         Set_Etype (N, Designated_Type (It.Typ));

      else
         Resolve (P, Etype (P));
      end if;

      if Is_Access_Type (Etype (P)) then
         Apply_Access_Check (N, Etype (P));
      end if;

      --  Note: there is no Eval processing required for an explicit
      --  deference, because the type is known to be an allocators, and
      --  allocator expressions can never be static.

   end Resolve_Explicit_Dereference;

   -------------------------------
   -- Resolve_Indexed_Component --
   -------------------------------

   procedure Resolve_Indexed_Component (N : Node_Id; Typ : Entity_Id) is
      Name       : constant Node_Id := Prefix  (N);
      Expr       : Node_Id;
      Array_Type : Entity_Id;
      Index      : Node_Id;

   begin
      if Is_Overloaded (Name) then

         --  Use the context type to select the prefix that yields the
         --  correct component type.

         declare
            I      : Interp_Index;
            It     : Interp;
            P      : constant Node_Id := Prefix (N);
            P_Type : Entity_Id;
            Found  : Boolean := False;

         begin
            Get_First_Interp (P, I, It);

            while Present (It.Typ) loop

               if (Is_Array_Type (It.Typ)
                     and then Covers (Typ, Component_Type (It.Typ)))
                 or else (Is_Access_Type (It.Typ)
                            and then Is_Array_Type (Designated_Type (It.Typ))
                            and then Covers
                              (Typ, Component_Type (Designated_Type (It.Typ))))
               then
                  if Found then
                     Error_Msg_N ("ambiguous prefix for indexing",  N);
                     Set_Etype (N, Typ);
                     return;
                  else
                     Found := True;
                     Array_Type := It.Typ;
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;

      else
         Array_Type := Etype (Name);
      end if;

      Resolve (Name, Array_Type);

      Array_Type := Etype (Name);

      --  If prefix is access type, dereference to get real array type.
      --  Note: we do not apply an access check because the expander always
      --  introduces an explicit dereference, and the check will happen there.

      if Is_Access_Type (Array_Type) then
         Array_Type := Designated_Type (Array_Type);
      end if;

      --  If name was overloaded, set component type correctly now.

      Set_Etype (N, Component_Type (Array_Type));

      Index := First_Index (Array_Type);
      Expr  := First (Expressions (N));

      while Present (Index) and Present (Expr) loop
         Resolve (Expr, Etype (Index));
         Check_Unset_Reference (Expr);

         if Is_Scalar_Type (Etype (Expr)) then
            Apply_Scalar_Range_Check (Expr, Etype (Index));
         else
            Apply_Range_Check (Expr, Etype (Index));
         end if;

         Index := Next_Index (Index);
         Expr  := Next (Expr);
      end loop;

      Eval_Indexed_Component (N);

   end Resolve_Indexed_Component;

   -----------------------------
   -- Resolve_Integer_Literal --
   -----------------------------

   procedure Resolve_Integer_Literal (N : Node_Id; Typ : Entity_Id) is
   begin
      Set_Etype (N, Typ);
      Eval_Integer_Literal (N);
   end Resolve_Integer_Literal;

   ------------------------
   -- Resolve_Logical_Op --
   ------------------------

   procedure Resolve_Logical_Op (N : Node_Id; Typ : Entity_Id) is
      B_Typ : Entity_Id;

   begin
      --  Predefined operations on  scalar types yield the base type. On
      --  the other hand, logical operations on arrays yield the type of
      --  the arguments (and the context).

      if Is_Array_Type (Typ) then
         B_Typ := Typ;
      else
         B_Typ := Base_Type (Typ);
      end if;

      --  The following test is required because the operands of the operation
      --  may be literals, in which case the resulting type appears to be
      --  compatible with a signed integer type, when in fact it is compatible
      --  only with modular types. If the context itself is universal, the
      --  operation is illegal.

      if not Valid_Boolean_Arg (Typ) then
         Error_Msg_N ("invalid context for logical operation", N);
         Set_Etype (N, Any_Type);
         return;

      elsif Typ = Any_Modular then
         Error_Msg_N
           ("no modular type available in this context", N);
         Set_Etype (N, Any_Type);
         return;
      end if;

      Resolve (Left_Opnd (N), B_Typ);
      Resolve (Right_Opnd (N), B_Typ);

      Check_Unset_Reference (Left_Opnd  (N));
      Check_Unset_Reference (Right_Opnd (N));

      Set_Etype (N, B_Typ);
      Eval_Logical_Op (N);
   end Resolve_Logical_Op;

   ---------------------------
   -- Resolve_Membership_Op --
   ---------------------------

   --  The context can only be a boolean type, and does not determine
   --  the arguments. Arguments should be unambiguous, but the preference
   --  rule for universal types applies.

   procedure Resolve_Membership_Op (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id   := Left_Opnd (N);
      R : constant Node_Id   := Right_Opnd (N);
      T : Entity_Id;

   begin
      if not Is_Overloaded (R)
        and then
          (Etype (R) = Universal_Integer or else
           Etype (R) = Universal_Real)
        and then Is_Overloaded (L)
      then
         T := Etype (R);
      else
         T := Intersect_Types (L, R);
      end if;

      Resolve (L, T);
      Check_Unset_Reference (L);

      if Nkind (R) = N_Range
        and then not Is_Scalar_Type (T)
      then
         Error_Msg_N ("scalar type required for range", R);
      end if;

      if Is_Entity_Name (R) then
         Freeze_Expression (R);
      else
         Resolve (R, T);
         Check_Unset_Reference (R);
      end if;

      Eval_Membership_Op (N);
   end Resolve_Membership_Op;

   ------------------
   -- Resolve_Null --
   ------------------

   procedure Resolve_Null (N : Node_Id; Typ : Entity_Id) is
   begin
      --   The literal NULL takes its type from the context.

      --  Keep the following commented out until usage in run-time files
      --  is sorted out  ???

      --  if Ekind (Typ) = E_Anonymous_Access_Type
      --    and then Comes_From_Source (N)
      --  then
      --     Error_Msg_N ("null cannot be of an anonymous access type", N);
      --  end if;

      Set_Etype (N, Typ);
   end Resolve_Null;

   --------------------
   -- Resolve_Op_Not --
   --------------------

   procedure Resolve_Op_Not (N : Node_Id; Typ : Entity_Id) is
      B_Typ : Entity_Id;

      function Parent_Is_Boolean return Boolean;
      --  This function determines if the parent node is a boolean operator
      --  or operation (comparison op, membership test, or short circuit form)
      --  and the not in question is the left operand of this operation.
      --  Note that if the not is in parens, then false is returned.

      function Parent_Is_Boolean return Boolean is
      begin
         if Paren_Count (N) /= 0 then
            return False;

         else
            case Nkind (Parent (N)) is
               when N_Op_And   |
                    N_Op_Eq    |
                    N_Op_Ge    |
                    N_Op_Gt    |
                    N_Op_Le    |
                    N_Op_Lt    |
                    N_Op_Ne    |
                    N_Op_Or    |
                    N_Op_Xor   |
                    N_In       |
                    N_Not_In   |
                    N_And_Then |
                    N_Or_Else =>

                  return Left_Opnd (Parent (N)) = N;

               when others =>
                  return False;
            end case;
         end if;
      end Parent_Is_Boolean;

   --  Start of processing for Resolve_Op_Not

   begin
      --  Predefined operations on scalar types yield the base type. On
      --  the other hand, logical operations on arrays yield the type of
      --  the arguments (and the context).

      if Is_Array_Type (Typ) then
         B_Typ := Typ;
      else
         B_Typ := Base_Type (Typ);
      end if;

      if not Valid_Boolean_Arg (Typ) then
         Error_Msg_N ("invalid operand type for operator&", N);
         Set_Etype (N, Any_Type);
         return;

      elsif (Typ = Universal_Integer
        or else Typ = Any_Modular)
      then
         if Parent_Is_Boolean then
            Error_Msg_N
              ("operand of not must be enclosed in parentheses",
               Right_Opnd (N));
         else
            Error_Msg_N
              ("no modular type available in this context", N);
         end if;

         Set_Etype (N, Any_Type);
         return;

      else
         if not Is_Boolean_Type (Typ)
           and then Parent_Is_Boolean
         then
            Error_Msg_N ("?not expression should be parenthesized here", N);
         end if;

         Resolve (Right_Opnd (N), B_Typ);
         Check_Unset_Reference (Right_Opnd (N));
         Set_Etype (N, B_Typ);
         Eval_Op_Not (N);
      end if;
   end Resolve_Op_Not;

   -----------------------------
   -- Resolve_Operator_Symbol --
   -----------------------------

   procedure Resolve_Operator_Symbol (N : Node_Id; Typ : Entity_Id) is
   begin
      null;
   end Resolve_Operator_Symbol;

   -----------------------
   -- Resolve_Op_Concat --
   -----------------------

   procedure Resolve_Op_Concat (N : Node_Id; Typ : Entity_Id) is
      Btyp : constant Entity_Id := Base_Type (Typ);

      procedure Resolve_Concatenation_Arg (Arg : Node_Id);
      --  Internal procedure to resolve one argument of concatenation operator.
      --  The argument is either of the array type or of the component type.

      procedure Resolve_Concatenation_Arg (Arg : Node_Id) is
      begin
         if Has_Compatible_Type (Arg, Component_Type (Typ)) then
            Resolve (Arg, Component_Type (Typ));
         else
            Resolve (Arg, Btyp);
         end if;

         Check_Unset_Reference (Arg);
      end Resolve_Concatenation_Arg;

   --  Start of processing for Resolve_Op_Concat

   begin
      Set_Etype (N, Btyp);

      if Is_Limited_Composite (Btyp) then
         Error_Msg_N ("concatenation not available for limited array", N);
      end if;

      Resolve_Concatenation_Arg (Left_Opnd (N));
      Resolve_Concatenation_Arg (Right_Opnd (N));

      if Is_String_Type (Typ) then
         Eval_Concatenation (N);
      end if;
   end Resolve_Op_Concat;

   ----------------------
   -- Resolve_Op_Expon --
   ----------------------

   procedure Resolve_Op_Expon (N : Node_Id; Typ : Entity_Id) is
      B_Typ : constant Entity_Id := Base_Type (Typ);

   begin
      if Etype (Left_Opnd (N)) = Universal_Integer
        or else Etype (Left_Opnd (N)) = Universal_Real
      then
         Check_For_Visible_Operator (N, B_Typ);
      end if;

      --  We do the resolution using the base type, because intermediate values
      --  in expressions always are of the base type, not a subtype of it.

      Resolve (Left_Opnd (N), B_Typ);
      Resolve (Right_Opnd (N), Standard_Integer);

      Check_Unset_Reference (Left_Opnd  (N));
      Check_Unset_Reference (Right_Opnd (N));

      Set_Etype (N, B_Typ);
      Eval_Op_Expon (N);

      --  Set overflow checking bit. Much cleverer code needed here eventually
      --  and perhaps the Resolve routines should be separated for the various
      --  arithmetic operations, since they will need different processing. ???

      if Nkind (N) in N_Op then
         if not Overflow_Checks_Suppressed (Etype (N)) then
            Set_Do_Overflow_Check (N, True);
         end if;
      end if;

   end Resolve_Op_Expon;

   ----------------------------------
   -- Resolve_Qualified_Expression --
   ----------------------------------

   procedure Resolve_Qualified_Expression (N : Node_Id; Typ : Entity_Id) is
      Target_Typ : constant Entity_Id := Entity (Subtype_Mark (N));
      Expr       : constant Node_Id   := Expression (N);

   begin
      Resolve (Expr, Target_Typ);

      --  A qualified expression requires an exact match of the type,
      --  class-wide matching is not allowed.

      if Is_Class_Wide_Type (Target_Typ)
        and then Base_Type (Etype (Expr)) /= Base_Type (Target_Typ)
      then
         Wrong_Type (Expr, Target_Typ);
      end if;

      --  If the target type is unconstrained, then we reset the type of
      --  the result from the type of the expression. For other cases, the
      --  actual subtype of the expression is the target type.

      if Is_Composite_Type (Target_Typ)
        and then not Is_Constrained (Target_Typ)
      then
         Set_Etype (N, Etype (Expr));
      end if;

      Eval_Qualified_Expression (N);
   end Resolve_Qualified_Expression;

   -------------------
   -- Resolve_Range --
   -------------------

   procedure Resolve_Range (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id := Low_Bound (N);
      H : constant Node_Id := High_Bound (N);

   begin
      Set_Etype (N, Typ);
      Resolve (L, Typ);
      Resolve (H, Typ);

      Check_Unset_Reference (L);
      Check_Unset_Reference (H);

      --  We have to check the bounds for being within the base range as
      --  required for a non-static context. Normally this is automatic
      --  and done as part of evaluating expressions, but the N_Range
      --  node is an exception, since in GNAT we consider this node to
      --  be a subexpression, even though in Ada it is not. The circuit
      --  in Sem_Eval could check for this, but that would put the test
      --  on the main evaluation path for expressions.

      Check_Non_Static_Context (L);
      Check_Non_Static_Context (H);

   end Resolve_Range;

   --------------------------
   -- Resolve_Real_Literal --
   --------------------------

   procedure Resolve_Real_Literal (N : Node_Id; Typ : Entity_Id) is
      Actual_Typ : constant Entity_Id := Etype (N);

   begin
      --  Special processing for fixed-point literals to make sure that the
      --  value is an exact multiple of small where this is required. We
      --  skip this for the universal real case, and also for generic types.

      if Is_Fixed_Point_Type (Typ)
        and then Typ /= Universal_Fixed
        and then Typ /= Any_Fixed
        and then not Is_Generic_Type (Typ)
      then
         declare
            Val   : constant Ureal := Realval (N);
            Cintr : constant Ureal := Val / Small_Value (Typ);
            Cint  : constant Uint  := UR_Trunc (Cintr);
            Den   : constant Uint  := Norm_Den (Cintr);
            Stat  : Boolean;

         begin
            --  Case of literal is not an exact multiple of the Small

            if Den /= 1 then

               --  For a source program literal for a decimal fixed-point
               --  type, this is statically illegal (RM 4.9(36)).

               if Is_Decimal_Fixed_Point_Type (Typ)
                 and then Actual_Typ = Universal_Real
                 and then Comes_From_Source (N)
               then
                  Error_Msg_N ("value has extraneous low order digits", N);
               end if;

               --  Replace literal by a value that is the exact representation
               --  of a value of the type, i.e. a multiple of the small value,
               --  by truncation, since Machine_Rounds is false for all GNAT
               --  fixed-point types (RM 4.9(38).

               Stat := Is_Static_Expression (N);
               Rewrite_Substitute_Tree (N,
                 Make_Real_Literal (Sloc (N),
                   Realval => Small_Value (Typ) * Cint));

               Set_Is_Static_Expression (N, Stat);
            end if;

            --  In all cases, set the corresponding integer field

            Set_Corresponding_Integer_Value (N, Cint);
         end;
      end if;

      --  Now replace the actual type by the expected type as usual

      Set_Etype (N, Typ);
      Eval_Real_Literal (N);
   end Resolve_Real_Literal;

   -----------------------
   -- Resolve_Reference --
   -----------------------

   procedure Resolve_Reference (N : Node_Id; Typ : Entity_Id) is
      P : constant Node_Id := Prefix (N);

   begin
      --  Replace general access with specific type

      if Ekind (Etype (N)) = E_Allocator_Type then
         Set_Etype (N, Base_Type (Typ));
      end if;

      --  If we are taking the reference of a volatile entity, then treat
      --  it as a potential modification of this entity. This is much too
      --  conservative, but is neccessary because remove side effects can
      --  result in transformations of normal assignments into reference
      --  sequences that otherwise fail to notice the modification.

      if Is_Entity_Name (P) and then Is_Volatile (Entity (P)) then
         Note_Possible_Modification (P);
      end if;
   end Resolve_Reference;

   --------------------------------
   -- Resolve_Selected_Component --
   --------------------------------

   procedure Resolve_Selected_Component (N : Node_Id; Typ : Entity_Id) is
      Comp : Entity_Id;
      P    : constant Node_Id   := Prefix  (N);
      S    : constant Node_Id   := Selector_Name (N);
      T    : Entity_Id := Etype (P);
      I    : Interp_Index;
      It   : Interp;

   begin
      if Is_Overloaded (P) then

         --  Use the context type to select the prefix that has a selector
         --  of the correct name and type.

         Get_First_Interp (P, I, It);

         Search : while Present (It.Typ) loop
            if Is_Access_Type (It.Typ) then
               T := Designated_Type (It.Typ);
            else
               T := It.Typ;
            end if;

            if Is_Record_Type (T) then
               Comp := First_Entity (T);

               while Present (Comp) loop

                  if Chars (Comp) = Chars (S)
                    and then Covers (Etype (Comp), Typ)
                  then
                     Resolve (P, It.Typ);
                     Set_Etype (N, Typ);
                     exit Search;
                  end if;

                  Comp := Next_Entity (Comp);
               end loop;

            end if;

            Get_Next_Interp (I, It);

         end loop Search;

      else
         --  Resolve prefix with its type.

         Resolve (P, T);
      end if;

      if Is_Access_Type (Etype (P)) then
         Apply_Access_Check (N, Etype (P));
         T := Designated_Type (Etype (P));
      else
         T := Etype (P);
      end if;

      if Has_Discriminants (T)
        and then Present (Original_Record_Component (Entity (S)))
        and then Ekind (Original_Record_Component (Entity (S))) = E_Component
        and then Present (Discriminant_Checking_Func
                           (Original_Record_Component (Entity (S))))
        and then not Discriminant_Checks_Suppressed (T)
      then
         Set_Do_Discriminant_Check (N);
      end if;

      if Ekind (Entity (S)) = E_Void then
         Error_Msg_N ("premature use of component", S);
      end if;

      --  Note: No Eval processing is required, because the prefix is of a
      --  record type, or protected type, and neither can possibly be static.

   end Resolve_Selected_Component;

   -------------------
   -- Resolve_Shift --
   -------------------

   procedure Resolve_Shift (N : Node_Id; Typ : Entity_Id) is
      B_Typ : constant Entity_Id := Base_Type (Typ);
      L     : constant Node_Id   := Left_Opnd  (N);
      R     : constant Node_Id   := Right_Opnd (N);

   begin
      --  We do the resolution using the base type, because intermediate values
      --  in expressions always are of the base type, not a subtype of it.

      Resolve (L, B_Typ);
      Resolve (R, Standard_Natural);

      Check_Unset_Reference (L);
      Check_Unset_Reference (R);

      Set_Etype (N, B_Typ);
      Eval_Shift (N);
   end Resolve_Shift;

   ---------------------------
   -- Resolve_Short_Circuit --
   ---------------------------

   procedure Resolve_Short_Circuit (N : Node_Id; Typ : Entity_Id) is
      B_Typ : constant Entity_Id := Base_Type (Typ);
      L     : constant Node_Id   := Left_Opnd  (N);
      R     : constant Node_Id   := Right_Opnd (N);

   begin
      Resolve (L, B_Typ);
      Resolve (R, B_Typ);

      Check_Unset_Reference (L);
      Check_Unset_Reference (R);

      Set_Etype (N, B_Typ);
      Eval_Short_Circuit (N);
   end Resolve_Short_Circuit;

   -------------------
   -- Resolve_Slice --
   -------------------

   procedure Resolve_Slice (N : Node_Id; Typ : Entity_Id) is
      Name       : constant Node_Id := Prefix (N);
      Drange     : constant Node_Id := Discrete_Range (N);
      Array_Type : Entity_Id;
      Index      : Node_Id;

   begin
      if Is_Overloaded (Name) then

         --  Use the context type to select the prefix that yields the
         --  correct array type.

         declare
            I      : Interp_Index;
            It     : Interp;
            P      : constant Node_Id := Prefix (N);
            P_Type : Entity_Id;
            Found  : Boolean := False;

         begin
            Get_First_Interp (P, I,  It);

            while Present (It.Typ) loop

               if (Is_Array_Type (It.Typ)
                    and then Covers (Typ,  It.Typ))
                 or else (Is_Access_Type (It.Typ)
                           and then Is_Array_Type (Designated_Type (It.Typ))
                           and then Covers (Typ, Designated_Type (It.Typ)))
               then
                  if Found then
                     Error_Msg_N ("ambiguous prefix for slicing",  N);
                     Set_Etype (N, Typ);
                     return;
                  else
                     Found := True;
                     Array_Type := It.Typ;
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;

      else
         Array_Type := Etype (Name);
      end if;

      Resolve (Name, Array_Type);

      if Is_Access_Type (Array_Type) then
         Apply_Access_Check (N, Array_Type);
         Array_Type := Designated_Type (Array_Type);
      end if;

      --  If name was overloaded, set slice type correctly now.
      Set_Etype (N, Array_Type);

      --  If the range is specified by a subtype mark, no resolution
      --  is necessary.

      if not Is_Entity_Name (Drange) then
         Index := First_Index (Array_Type);
         Resolve (Drange, Base_Type (Etype (Index)));

         if Nkind (Drange) = N_Range then
            Apply_Range_Check (Drange, Etype (Index));
         end if;
      end if;

      Set_Slice_Subtype (N);
      Eval_Slice (N);

   end Resolve_Slice;

   -----------------------
   -- Set_Slice_Subtype --
   -----------------------

   --  Build an implicit subtype declaration to represent the type delivered
   --  by the slice. This is an abbreviated version of an array subtype. We
   --  define an index subtype for the slice,  using either the subtype name
   --  or the discrete range of the slice. To be consistent with index usage
   --  elsewhere, we create a list header to hold the single index. This list
   --  is not otherwise attached to the syntax tree.

   procedure Set_Slice_Subtype (N : Node_Id) is
      Loc           : constant Source_Ptr := Sloc (N);
      Index         : Node_Id;
      Index_List    : List_Id := New_List;
      Index_Subtype : Entity_Id;
      Index_Type    : Entity_Id;
      Slice_Subtype : Entity_Id;
      Drange        : constant Node_Id := Discrete_Range (N);

   begin
      if Is_Entity_Name (Drange) then
         Index_Subtype := Entity (Drange);

      else
         --  Since we create and insert an Itype refering to the N_Range node
         --  we must make sure that side-effects are already inserted before
         --  itype definition

         if Nkind (Drange) = N_Range then
            Remove_Side_Effects (Low_Bound (Drange));
            Remove_Side_Effects (High_Bound (Drange));
         end if;

         Index_Type := Base_Type (Etype (Drange));

         Index_Subtype := Create_Itype (Subtype_Kind (Ekind (Index_Type)), N);

         Set_Scalar_Range (Index_Subtype, Drange);
         Set_Etype        (Index_Subtype, Index_Type);
         Set_Size_Info    (Index_Subtype, Index_Type);
      end if;

      Slice_Subtype := Create_Itype (E_Array_Subtype, N);

      Index := New_Occurrence_Of (Index_Subtype, Loc);
      Set_Etype (Index, Index_Subtype);
      Append (Index, Index_List);

      Set_Component_Type (Slice_Subtype, Component_Type (Etype (N)));
      Set_First_Index    (Slice_Subtype, Index);
      Set_Etype          (Slice_Subtype, Base_Type (Etype (N)));
      Set_Is_Constrained (Slice_Subtype, True);
      Set_Esize          (Slice_Subtype, Uint_0);

      Check_Compile_Time_Size (Slice_Subtype);

      --  The Etype of the existing Slice node is reset to this slice
      --  subtype. Its bounds are obtained from its first index.

      Set_Etype (N, Slice_Subtype);

      --  In the packed case, this must be immediately frozen

      --  Couldn't we always freeze here??? and if we did, then the above
      --  call to Check_Compile_Time_Size could be eliminated, which would
      --  be nice, because then that routine could be made private to Freeze.

      if Is_Packed (Slice_Subtype) and not In_Default_Expression then
         Freeze_Itype (Slice_Subtype, N);
      end if;

   end Set_Slice_Subtype;

   ----------------------------
   -- Resolve_String_Literal --
   ----------------------------

   procedure Resolve_String_Literal (N : Node_Id; Typ : Entity_Id) is
      C_Typ      : constant Entity_Id  := Component_Type (Typ);
      R_Typ      : constant Entity_Id  := Root_Type (C_Typ);
      Loc        : constant Source_Ptr := Sloc (N);
      Str        : constant String_Id  := Strval (N);
      Strlen     : constant Nat        := String_Length (Str);
      Subtype_Id : Entity_Id;

   begin
      --  If the resolving type is itself a string literal subtype, we
      --  can just reuse it, since there is no point in creating another.

      if Ekind (Typ) = E_String_Literal_Subtype then
         Subtype_Id := Typ;

      --  Otherwise we must create a string literal subtype. Note that the
      --  whole idea of string literal subtypes is simply to avoid the need
      --  for building a full fledged array subtype for each literal.

      else
         Subtype_Id := Create_Itype (E_String_Literal_Subtype, N);

         Set_Component_Type           (Subtype_Id, Component_Type (Typ));
         Set_String_Literal_Length    (Subtype_Id, UI_From_Int (Strlen));
         Set_Etype                    (Subtype_Id, Base_Type (Typ));

         --  The low bound is set from the low bound of the corresponding
         --  index type. Note that we do not store the high bound in the
         --  string literal subtype, but it can be deduced if necssary
         --  from the length and the low bound.

         Set_String_Literal_Low_Bound
           (Subtype_Id, Type_Low_Bound (Etype (First_Index (Typ))));
      end if;

      Set_Etype (N, Subtype_Id);
      Eval_String_Literal (N);

      if Is_Limited_Composite (Typ)
        or else Is_Private_Composite (Typ)
      then
         Error_Msg_N ("string literal not available for private array", N);
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  The null string is always valid
      --  ??? is this right for weird string types?

      if Strlen = 0 then
         return;

      --  Always accept string literal with component type Any_Character,
      --  which occurs in error situations and in comparisons of literals,
      --  both of which should accept all literals.

      elsif R_Typ = Any_Character then
         return;

      --  If the type is bit-packed, then we always tranform the string
      --  literal into a full fledged aggregate.

      elsif Is_Bit_Packed_Array (Typ) then
         null;

      --  Deal with cases of Wide_String and String

      else
         --  For Standard.Wide_String, or any other type whose component
         --  type is Standard.Wide_Character, we know that all the
         --  characters in the string must be acceptable, since the parser
         --  accepted the characters as valid character literals.

         if R_Typ = Standard_Wide_Character then
            null;

         --  For the case of Standard.String, or any other type whose
         --  component type is Standard.Character, we must make sure that
         --  there are no wide characters in the string, i.e. that it is
         --  entirely composed of characters in range of type String.

         elsif R_Typ = Standard_Character then
            for J in 1 .. Strlen loop
               if not In_Character_Range (Get_String_Char (Str, J)) then

                  --  If we are out of range, post error. This is one of the
                  --  very few places that we place the flag in the middle of
                  --  a token, right under the offending wide character.

                  Error_Msg
                    ("literal out of range of type Character",
                     Source_Ptr (Int (Loc) + J));
                  return;
               end if;
            end loop;

         --  If the root type is not a standard character, then we will convert
         --  the string into an aggregate and will let the aggregate code do
         --  the checking.

         else
            null;

         end if;

         --  See if the component type of the array corresponding to the
         --  string has compile time known bounds. If yes we can directly
         --  check whether the evaluation of the string will raise constraint
         --  error. Otherwise we need to transform the string literal into
         --  the corresponding character aggregate and let the aggregate
         --  code do the checking.

         if R_Typ = Standard_Wide_Character
           or else R_Typ = Standard_Character
         then
            --  Check for the case of full range, where we are definitely OK

            if Component_Type (Typ) = Base_Type (Component_Type (Typ)) then
               return;
            end if;

            --  Here the range is not the complete base type range, so check

            declare
               Comp_Typ_Lo : constant Node_Id :=
                               Type_Low_Bound (Component_Type (Typ));
               Comp_Typ_Hi : constant Node_Id :=
                               Type_High_Bound (Component_Type (Typ));

               Char_Val : Uint;

            begin
               if Compile_Time_Known_Value (Comp_Typ_Lo)
                 and then Compile_Time_Known_Value (Comp_Typ_Hi)
               then
                  for J in 1 .. Strlen loop
                     Char_Val := UI_From_Int (Int (Get_String_Char (Str, J)));

                     if Char_Val < Expr_Value (Comp_Typ_Lo)
                       or else Char_Val > Expr_Value (Comp_Typ_Hi)
                     then
                        Apply_Compile_Time_Constraint_Error
                          (N, "character out of range?",
                           Loc => Source_Ptr (Int (Loc) + J));
                     end if;
                  end loop;

                  return;
               end if;
            end;
         end if;
      end if;

      --  If we got here we meed to transform the string literal into the
      --  equivalent qualified positional array aggregate. This is rather
      --  heavy artillery for this situation, but it is hard work to avoid.

      declare
         Lits : List_Id    := New_List;
         P    : Source_Ptr := Loc + 1;
         C    : Char_Code;

      begin
         --  Build the character literals, we give them source locations
         --  that correspond to the string positions, which is a bit tricky
         --  given the possible presence of wide character escape sequences.

         for J in 1 .. Strlen loop
            C := Get_String_Char (Str, J);
            Set_Character_Literal_Name (C);

            Append_To (Lits,
              Make_Character_Literal (P, Name_Find, C));

            if In_Character_Range (C) then
               P := P + 1;
--  ???     else
--             Skip_Wide (P);
            end if;
         end loop;

         Rewrite_Substitute_Tree (N,
           Make_Qualified_Expression (Loc,
             Subtype_Mark => New_Reference_To (Typ, Loc),
             Expression   =>
               Make_Aggregate (Loc, Expressions => Lits)));

         Analyze_And_Resolve (N, Typ);
      end;

   end Resolve_String_Literal;

   -----------------------------
   -- Resolve_Type_Conversion --
   -----------------------------

   procedure Resolve_Type_Conversion (N : Node_Id; Typ : Entity_Id) is
      Target_Type : constant Entity_Id := Etype (N);
      Conv_OK     : constant Boolean   := Conversion_OK (N);
      Operand     : Node_Id;
      Opnd_Type   : Entity_Id;

   begin
      if not Valid_Conversion (N) then
         return;
      end if;

      Operand := Expression (N);

      if Etype (Operand) = Any_Fixed then

         --  Mixed-mode operation involving a literal. Context must be a fixed
         --  type which is applied to the literal subsequently.

         if Is_Fixed_Point_Type (Typ) then
            Set_Etype (Operand, Universal_Real);
         else
            Error_Msg_N ("invalid context for mixed mode operation", N);
            Set_Etype (Operand, Any_Type);
            return;
         end if;
      end if;

      Opnd_Type := Etype (Operand);
      Resolve (Operand, Opnd_Type);

      if Is_Tagged_Type (Typ) then
         Note_Feature (Tagged_Type_Conversion, Sloc (N));
      end if;

      --  Note: we do the Eval_Type_Conversion call before applying the
      --  required checks for a subtype conversion. This is important,
      --  since both are prepared under certain circumstances to change
      --  the type conversion to a constraint error node, but in the case
      --  of Eval_Type_Conversion this may reflect an illegality in the
      --  static case, and we would miss the illegality (getting only a
      --  warning message), if we applied the type conversion checks first.

      Eval_Type_Conversion (N);

      --  If after evaluation, we still have a type conversion, then we
      --  may need to apply checks required for a subtype conversion.

      --  Skip these type conversion checks if universal fixed operands
      --  operands involved, since range checks are handled separately for
      --  these cases (in the appropriate Expand routines in unit Exp_Fixd).

      if Nkind (N) = N_Type_Conversion
        and then not Is_Generic_Type (Root_Type (Target_Type))
        and then Target_Type /= Universal_Fixed
        and then Opnd_Type /= Universal_Fixed
      then
         Apply_Type_Conversion_Checks (N);
      end if;

   end Resolve_Type_Conversion;

   ----------------------
   -- Resolve_Unary_Op --
   ----------------------

   procedure Resolve_Unary_Op (N : Node_Id; Typ : Entity_Id) is
      B_Typ : Entity_Id := Base_Type (Typ);
      R     : constant Node_Id := Right_Opnd (N);

   begin
      --  Generate warning for expressions like -5 mod 3

      if Paren_Count (N) = 0
        and then Nkind (N) = N_Op_Minus
        and then Nkind (Right_Opnd (N)) = N_Op_Mod
      then
         Error_Msg_N
           ("?unary minus expression should be parenthesized here", N);
      end if;

      if Etype (R) = Universal_Integer
        or else Etype (R) = Universal_Real
      then
         Check_For_Visible_Operator (N, B_Typ);
      end if;

      Set_Etype (N, B_Typ);
      Resolve (R, B_Typ);
      Check_Unset_Reference (R);
      Eval_Unary_Op (N);

      --  Set overflow checking bit. Much cleverer code needed here eventually
      --  and perhaps the Resolve routines should be separated for the various
      --  arithmetic operations, since they will need different processing ???

      if Nkind (N) in N_Op then
         if not Overflow_Checks_Suppressed (Etype (N)) then
            Set_Do_Overflow_Check (N, True);
         end if;
      end if;

   end Resolve_Unary_Op;

   ----------------------------------
   -- Resolve_Unchecked_Expression --
   ----------------------------------

   procedure Resolve_Unchecked_Expression
     (N   : Node_Id;
      Typ : Entity_Id)
   is
   begin
      Resolve (Expression (N), Typ, Suppress => All_Checks);
      Set_Etype (N, Typ);
   end Resolve_Unchecked_Expression;

   ---------------------------------------
   -- Resolve_Unchecked_Type_Conversion --
   ---------------------------------------

   procedure Resolve_Unchecked_Type_Conversion
     (N   : Node_Id;
      Typ : Entity_Id)
   is
      Target_Type : constant Entity_Id := Etype (N);
      Operand     : constant Node_Id   := Expression (N);
      Opnd_Type   : constant Entity_Id := Etype (Operand);

   begin
      --  Resolve operand using its own type.

      Resolve (Operand, Opnd_Type);
      Eval_Unchecked_Conversion (N);

   end Resolve_Unchecked_Type_Conversion;

   ------------------------------
   -- Rewrite_Operator_As_Call --
   ------------------------------

   procedure Rewrite_Operator_As_Call (N : Node_Id; Nam : Entity_Id) is
      Loc     :  Source_Ptr := Sloc (N);
      Actuals :  List_Id := New_List;
      New_N   : Node_Id;

   begin
      if Nkind (N) in  N_Binary_Op then
         Append (Left_Opnd (N), Actuals);
      end if;

      Append (Right_Opnd (N), Actuals);

      New_N :=
        Make_Function_Call (Sloc => Loc,
          Name => New_Occurrence_Of (Nam, Loc),
          Parameter_Associations => Actuals);

      Preserve_Comes_From_Source (New_N, N);
      Preserve_Comes_From_Source (Name (New_N), N);
      Rewrite_Substitute_Tree (N, New_N);
      Set_Etype (N, Etype (Nam));
   end Rewrite_Operator_As_Call;

   ------------------------------
   -- Rewrite_Renamed_Operator --
   ------------------------------

   procedure Rewrite_Renamed_Operator (N : Node_Id; Op : Entity_Id) is
      Nam       : constant Name_Id := Chars (Op);
      Is_Binary : constant Boolean := Nkind (N) in N_Binary_Op;
      Op_Node   : Node_Id;

   begin
      if Chars (N) /= Nam then

         --  Rewrite the operator node using the real operator, not its
         --  renaming.

         Op_Node := New_Node (Operator_Kind (Nam, Is_Binary), Sloc (N));
         Set_Chars      (Op_Node, Nam);
         Set_Etype      (Op_Node, Etype (N));
         Set_Entity     (Op_Node, Op);
         Set_Right_Opnd (Op_Node, Right_Opnd (N));

         if Is_Binary then
            Set_Left_Opnd  (Op_Node, Left_Opnd  (N));
         end if;

         Rewrite_Substitute_Tree (N, Op_Node);
      end if;
   end Rewrite_Renamed_Operator;

   ----------------------
   -- Valid_Conversion --
   ----------------------

   function Valid_Conversion (N : Node_Id) return Boolean is
      Target_Type : Entity_Id := Base_Type (Etype (N));
      Operand     : Node_Id   := Expression (N);
      Opnd_Type   : Entity_Id := Etype (Operand);

      function Conversion_Check
        (Valid : Boolean;
         Msg   : String)
         return  Boolean;
      --  Little routine to post Msg if Valid is False, returns Valid value

      function Valid_Tagged_Conversion
        (Target_Type : Entity_Id;
         Opnd_Type   : Entity_Id)
         return        Boolean;
      --  Specifically test for validity of tagged conversions

      function Conversion_Check
        (Valid : Boolean;
         Msg   : String)
         return  Boolean
      is
      begin
         if not Valid then
            Error_Msg_N (Msg, Operand);
         end if;
         return Valid;
      end Conversion_Check;

      function Valid_Tagged_Conversion
        (Target_Type : Entity_Id;
         Opnd_Type   : Entity_Id)
         return        Boolean
      is
      begin
         --  Upward conversions are allowed (RM 4.6(22)).

         if Covers (Target_Type, Opnd_Type)
           or else Is_Ancestor (Target_Type, Opnd_Type)
         then
            return True;

         --  Downward conversion are allowed if the operand is
         --  is class-wide (RM 4.6(23)).

         elsif Is_Class_Wide_Type (Opnd_Type)
              and then Covers (Opnd_Type, Target_Type)
         then
            return True;

         elsif Covers (Opnd_Type, Target_Type)
           or else Is_Ancestor (Opnd_Type, Target_Type)
         then
            return
              Conversion_Check (False,
                "downward conversion of tagged objects not allowed");
         else
            Error_Msg_NE
              ("invalid tagged conversion, not compatible with}",
               N, First_Subtype (Opnd_Type));
            return False;
         end if;
      end Valid_Tagged_Conversion;

   --  Start of processing for Valid_Conversion

   begin
      --  Definitely valid if Conversion_OK set

      if Conversion_OK (N) then
         return True;
      end if;

      --  Otherwise continue with normal conversion processing

      Check_Parameterless_Call (Operand,  Etype (Operand));

      if Is_Overloaded (Operand) then
         declare
            I   : Interp_Index;
            I1  : Interp_Index;
            It  : Interp;
            It1 : Interp;

         begin
            --  Remove procedure calls, which syntactically cannot appear
            --  in this context, but which cannot be removed by type checking,
            --  because the context does not impose a type.

            Get_First_Interp (Operand, I, It);

            while Present (It.Typ) loop

               if It.Typ = Standard_Void_Type then
                  Remove_Interp (I);
               end if;

               Get_Next_Interp (I, It);
            end loop;

            Get_First_Interp (Operand, I, It);
            I1  := I;
            It1 := It;

            if No (It.Typ) then
               Error_Msg_N ("illegal operand in conversion", Operand);
               return False;
            end if;

            Get_Next_Interp (I, It);

            if Present (It.Typ) then
               It1 :=  Disambiguate (Operand, I1, I, Any_Type);

               if It1 = No_Interp then
                  Error_Msg_N ("ambiguous operand in conversion", Operand);
                  return False;
               end if;
            end if;

            Set_Etype (Operand, It1.Typ);
            Opnd_Type := It1.Typ;
         end;
      end if;

      if Chars (Current_Scope) = Name_Unchecked_Conversion then
         --  This check is dubious, what if there were a user defined
         --  scope whose name was Unchecked_Conversion ???
         return True;

      elsif Is_Numeric_Type (Target_Type)  then
         if Opnd_Type = Universal_Fixed then
            return True;
         else
            return Conversion_Check (Is_Numeric_Type (Opnd_Type),
                             "illegal operand for numeric conversion");
         end if;

      elsif Is_Array_Type (Target_Type) then
         if not Is_Array_Type (Opnd_Type)
           or else Opnd_Type = Any_Composite
           or else Opnd_Type = Any_String
         then
            Error_Msg_N
              ("illegal operand for array conversion", Operand);
            return False;

         elsif Number_Dimensions (Target_Type) /=
           Number_Dimensions (Opnd_Type)
         then
            Error_Msg_N
              ("incompatible number of dimensions for conversion", Operand);
            return False;

         else
            declare
               Target_Index      : Node_Id := First_Index (Target_Type);
               Opnd_Index        : Node_Id := First_Index (Opnd_Type);

               Target_Index_Type : Entity_Id;
               Opnd_Index_Type   : Entity_Id;

               Target_Comp_Type  : Entity_Id := Component_Type (Target_Type);
               Opnd_Comp_Type    : Entity_Id := Component_Type (Opnd_Type);

            begin
               while Present (Target_Index) and then Present (Opnd_Index) loop
                  Target_Index_Type := Etype (Target_Index);
                  Opnd_Index_Type   := Etype (Opnd_Index);

                  if not (Is_Integer_Type (Target_Index_Type)
                          and then Is_Integer_Type (Opnd_Index_Type))
                    and then (Root_Type (Target_Index_Type)
                              /= Root_Type (Opnd_Index_Type))
                  then
                     Error_Msg_N
                       ("incompatible index types for array conversion",
                        Operand);
                     return False;
                  end if;

                  Target_Index := Next_Index (Target_Index);
                  Opnd_Index := Next_Index (Opnd_Index);
               end loop;

               if Base_Type (Target_Comp_Type) /=
                 Base_Type (Opnd_Comp_Type)
               then
                  Error_Msg_N
                    ("incompatible component types for array conversion",
                     Operand);
                  return False;

               elsif
                  Is_Constrained (Target_Comp_Type)
                    /= Is_Constrained (Opnd_Comp_Type)
                  or else not Subtypes_Statically_Match
                                (Target_Comp_Type, Opnd_Comp_Type)
               then
                  Error_Msg_N
                    ("component subtypes must statically match", Operand);
                  return False;

               end if;
            end;
         end if;

         return True;

      elsif Ekind (Target_Type) = E_General_Access_Type
        and then
          Conversion_Check
            (Is_Access_Type (Opnd_Type)
               and then Ekind (Opnd_Type) /=
                 E_Access_Subprogram_Type
               and then Ekind (Opnd_Type) /=
                 E_Access_Protected_Subprogram_Type,
             "must be an access-to-object type")
      then
         if Is_Access_Constant (Opnd_Type)
           and then not Is_Access_Constant (Target_Type)
         then
            Error_Msg_NE ("invalid conversion, not compatible with }",
              N, Opnd_Type);
            Error_Msg_N
              ("access-to-constant operand type not allowed", Operand);
         end if;

         --  Check the static accessibility rule of 4.6(17)

         if Type_Access_Level (Opnd_Type) >
            Type_Access_Level (Target_Type)
         then
            Error_Msg_N
              ("operand type has deeper accessibility level than target",
               Operand);

         --  When the operand is an access discriminant the check is
         --  against the level of the prefix object.

         elsif Ekind (Opnd_Type) = E_Anonymous_Access_Type
           and then Nkind (Operand) = N_Selected_Component
           and then Object_Access_Level (Prefix (Operand)) >
                    Type_Access_Level (Target_Type)
         then
            Error_Msg_N
              ("discriminant has deeper accessibility level than target",
               Operand);

         end if;

         declare
            Target : constant Entity_Id := Designated_Type (Target_Type);
            Opnd   : constant Entity_Id := Designated_Type (Opnd_Type);

         begin
            if Is_Tagged_Type (Target) then
               return Valid_Tagged_Conversion (Target, Opnd);

            else
               if  Base_Type (Target) /= Base_Type (Opnd) then
                  Error_Msg_NE
                   ("invalid conversion, designated type not " &
                    "compatible with }", N, Base_Type (Opnd));
                  return False;

               else
                  return True;
               end if;
            end if;
         end;

      elsif Ekind (Target_Type) = E_Access_Subprogram_Type
        and then Conversion_Check
                   (Ekind (Base_Type (Opnd_Type)) = E_Access_Subprogram_Type,
                    "illegal operand for access subprogram conversion")
      then
         --  Check that the designated types are subtype conformant

         if not Subtype_Conformant (Designated_Type (Opnd_Type),
                                    Designated_Type (Target_Type))
         then
            Error_Msg_N
              ("operand type is not subtype conformant with target type",
               Operand);
         end if;

         --  Check the static accessibility rule of 4.6(20)
         --  (except we are not yet checking the rule for case of
         --  operand type being declared within a generic body ???)

         if Type_Access_Level (Opnd_Type) >
            Type_Access_Level (Target_Type)
         then
            Error_Msg_N
              ("operand type has deeper accessibility level than target",
               Operand);
         end if;

         return True;

      elsif Is_Tagged_Type (Target_Type) then
         return Valid_Tagged_Conversion (Target_Type, Opnd_Type);

      --  Just checking the root types of character types does not seem
      --  to be adequate because of the difference between "character type"
      --  and "a character type"

      elsif Is_Character_Type (Target_Type)
        and then Is_Character_Type (Opnd_Type)
      then
         return True;

      --  Types derived from the same root type are convertible

      elsif Root_Type (Target_Type) = Root_Type (Opnd_Type) then
         return True;

      else
         Error_Msg_NE ("invalid conversion, not compatible with }",
           N, Opnd_Type);

         return False;
      end if;
   end Valid_Conversion;

end Sem_Res;

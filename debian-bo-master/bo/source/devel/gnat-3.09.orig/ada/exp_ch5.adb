------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 5                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.134 $                            --
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
with Einfo;    use Einfo;
with Exp_Ch2;  use Exp_Ch2;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Pakd; use Exp_Pakd;
with Exp_Util; use Exp_Util;
with Itypes;   use Itypes;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sinfo;    use Sinfo;
with Sem;      use Sem;
with Sem_Ch13; use Sem_Ch13;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch5 is

   function Change_Of_Representation (N : Node_Id) return Boolean;
   --  Determine if the right hand side of the assignment N is a type
   --  conversion which requires a change of representation. Called
   --  only for the array and record cases.

   procedure Expand_Assign_Array (N : Node_Id);
   --  N is an assignment which assigns an array value. This routine process
   --  the various special cases and checks required for such assignments,
   --  including change of representation.

   function Expand_Assign_Array_Loop
     (N      : Node_Id;
      Larray : Entity_Id;
      Rarray : Entity_Id;
      L_Type : Entity_Id;
      R_Type : Entity_Id;
      Ndim   : Pos;
      Rev    : Boolean)
      return   Node_Id;
   --  N is an assignment statement which assigns an array value. This routine
   --  expands the assignment into a loop (or nested loops for the case of a
   --  multi-dimensional array) to do the assignment component by component.
   --  Larray and Rarray are the entities of the actual arrays on the left
   --  hand and right hand sides. L_Type and R_Type are the types of these
   --  arrays (which may not be the same, due to either sliding, or to a
   --  change of representation case). Ndim is the number of dimensions and
   --  the parameter Rev indicates if the loops run normally (Rev = False),
   --  or reversed (Rev = True). The value returned is the the constructed
   --  loop statement. Auxiliary declarations are inserted before node N
   --  using the standard Insert_Actions mechanism.

   procedure Expand_Assign_Record (N : Node_Id);
   --  N is an assignment of a non-tagged record value. This routine handles
   --  the special cases and checks required for such assignments, including
   --  change of representation.

   function Make_Tag_Ctrl_Assignment (N : Node_Id) return List_Id;
   --  Generate the necessary code for controlled and Tagged assignment,
   --  that is to say, finalization of the target before, adjustement of
   --  the target after and save and restore of the tag and finalization
   --  pointers which are not 'part of the value' and must not be changed
   --  upon assignment. N is the original Assignment node.

   ------------------------------
   -- Change_Of_Representation --
   ------------------------------

   function Change_Of_Representation (N : Node_Id) return Boolean is
      Rhs : constant Node_Id := Expression (N);

   begin
      return
        Nkind (Rhs) = N_Type_Conversion
          and then
            not Same_Representation (Etype (Rhs), Etype (Expression (Rhs)));
   end Change_Of_Representation;

   -------------------------
   -- Expand_Assign_Array --
   -------------------------

   --  There are two issues here. First, do we let Gigi do a block move, or
   --  do we expand out into a loop? Second, we need to set the two flags
   --  Forwards_OK and Backwards_OK which show whether the block move (or
   --  corresponding loops) can be legitimately done in a forwards (low to
   --  high) or backwards (high to low) manner.

   procedure Expand_Assign_Array (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Lhs     : constant Node_Id    := Name (N);
      Rhs     : constant Node_Id    := Expression (N);

      Act_Lhs : constant Node_Id := Get_Referenced_Object (Lhs);
      Act_Rhs : Node_Id          := Get_Referenced_Object (Rhs);

      L_Type  : constant Entity_Id :=
                  Underlying_Type (Get_Actual_Subtype (Act_Lhs));
      R_Type  : Entity_Id :=
                  Underlying_Type (Get_Actual_Subtype (Act_Rhs));

      L_Slice : constant Boolean := Nkind (Act_Lhs) = N_Slice;
      R_Slice : constant Boolean := Nkind (Act_Rhs) = N_Slice;

      Crep    : constant Boolean    := Change_Of_Representation (N);

      Larray  : Node_Id;
      Rarray  : Node_Id;

      Ndim : constant Pos := Number_Dimensions (L_Type);

      function Has_Address_Clause (Exp : Node_Id) return Boolean;
      --  Test if Exp is a reference to an array whose declaration has
      --  an address clause, or it is is a slice of such an array.

      function Is_Formal_Array (Exp : Node_Id) return Boolean;
      --  Test if Exp is a reference to an array which is either a formal
      --  parameter or a slice of a formal parameter. These are the cases
      --  where hidden aliasing can occur.

      function Is_Non_Local_Array_Variable (Exp : Node_Id) return Boolean;
      --  Determine if Exp is a reference to an array variable which is other
      --  than an object defied in the current scope, or as slice of such an
      --  object. Such objects cannot be aliased to parameters unless
      --  they are themselves parameters.

      function Has_Address_Clause (Exp : Node_Id) return Boolean is
      begin
         return
           (Is_Entity_Name (Exp) and then
                              Present (Address_Clause (Entity (Exp))))
             or else
           (Nkind (Exp) = N_Slice and then Has_Address_Clause (Prefix (Exp)));
      end Has_Address_Clause;

      function Is_Formal_Array (Exp : Node_Id) return Boolean is
      begin
         return
           (Is_Entity_Name (Exp) and then Is_Formal (Entity (Exp)))
             or else
           (Nkind (Exp) = N_Slice and then Is_Formal_Array (Prefix (Exp)));
      end Is_Formal_Array;

      function Is_Non_Local_Array_Variable (Exp : Node_Id) return Boolean is
      begin
         return

           (Is_Entity_Name (Exp) and then
                                    Scope (Entity (Exp)) /= Current_Scope)
             or else
           (Nkind (Exp) = N_Slice
             and then Is_Non_Local_Array_Variable (Prefix (Exp)));
      end Is_Non_Local_Array_Variable;

      Lhs_Formal : constant Boolean := Is_Formal_Array (Act_Lhs);
      Rhs_Formal : constant Boolean := Is_Formal_Array (Act_Rhs);

      Lhs_Non_Local_Var : constant Boolean :=
                            Is_Non_Local_Array_Variable (Act_Lhs);
      Rhs_Non_Local_Var : constant Boolean :=
                            Is_Non_Local_Array_Variable (Act_Rhs);

   --  Start of processing for Expand_Assign_Case

   begin
      --  Deal with length check, note that the length check is done with
      --  respect to the right hand side as given, not a possible underlying
      --  renamed object, since this would generate incorrect extra checks.

      Apply_Length_Check (Rhs, L_Type);

      --  We start by assuming that the move can be done in either
      --  direction, i.e. that the two sides are completely disjoint.

      Set_Forwards_OK  (N, True);
      Set_Backwards_OK (N, True);

      --  First step is to determine if the array assignment must be
      --  expanded into an element-by-element loop. The following
      --  if statement returns if such expansion is not required.

      --  We certainly must use a loop for change of representation
      --  and also we use the operand of the conversion on the right
      --  hand side as the effective right hand side (the component
      --  types must match in this situation).

      if Crep then
         Act_Rhs := Get_Referenced_Object (Expression (Rhs));
         R_Type  := Get_Actual_Subtype (Act_Rhs);

      --  Normally it is only the slice cases that bother us, but in the
      --  case of one dimensional arrays, parameters can be slices that
      --  are passed by reference, so we can have aliasing for assignments
      --  from one parameter to another, or assignments between parameters
      --  and non-local variables.

      elsif Ndim = 1
        and then
           ((Lhs_Formal and Rhs_Formal)
              or else
            (Lhs_Formal and Rhs_Non_Local_Var)
              or else
            (Rhs_Formal and Lhs_Non_Local_Var))

      then
         Set_Forwards_OK  (N, False);
         Set_Backwards_OK (N, False);

      --  Arrays with controlled components are expanded into a loop
      --  to force calls to adjust at the component level.

      elsif Has_Controlled_Component (L_Type) then
         null;

      --  The only remaining cases involve slice assignments. If no slices
      --  are involved, then the assignment can definitely be handled by gigi.

      elsif not L_Slice and not R_Slice then
         return;

      --  Here we have slices. If there is only one slice, then we let the
      --  backend do the assignment, since no overlap is possible, except
      --  that if either operand is packed, then we may not be bit aligned
      --  so for the packed case we do the loop if there is only one slice.

      elsif (not L_Slice or not R_Slice)
        and then not Is_Bit_Packed_Array (L_Type)
        and then not Is_Bit_Packed_Array (R_Type)
      then
         return;
      end if;

      --  Here we need to do the move in a loop

      Slice_Case : declare

         L_Index_Typ : constant Node_Id := Etype (First_Index (L_Type));
         R_Index_Typ : constant Node_Id := Etype (First_Index (R_Type));

         Left_Lo  : constant Node_Id := Type_Low_Bound  (L_Index_Typ);
         Left_Hi  : constant Node_Id := Type_High_Bound (L_Index_Typ);
         Right_Lo : constant Node_Id := Type_Low_Bound  (R_Index_Typ);
         Right_Hi : constant Node_Id := Type_High_Bound (R_Index_Typ);

         Act_L_Array : Node_Id;
         Act_R_Array : Node_Id;

         Cresult : Compare_Result;

      begin
         --  Get the expressions for the arrays. If we are dealing with a
         --  private type, then convert to the underlying type. We can do
         --  direct assignments to an array that is a private type, but
         --  we cannot assign to elements of the array without this extra
         --  unchecked conversion.

         if Nkind (Act_Lhs) = N_Slice then
            Larray := Prefix (Act_Lhs);
         else
            Larray := Act_Lhs;

            if Is_Private_Type (Etype (Larray)) then
               Larray :=
                 Unchecked_Convert_To
                   (Underlying_Type (Etype (Larray)), Larray);
            end if;
         end if;

         if Nkind (Act_Rhs) = N_Slice then
            Rarray := Prefix (Act_Rhs);
         else
            Rarray := Act_Rhs;

            if Is_Private_Type (Etype (Rarray)) then
               Rarray :=
                 Unchecked_Convert_To
                   (Underlying_Type (Etype (Rarray)), Rarray);
            end if;
         end if;

         --  If both sides are slices, we must figure out whether
         --  it is safe to do the move in one direction or the other
         --  It is always safe if there is a change of representation
         --  since obviously two arrays with different representations
         --  cannot possibly overlap.

         if (not Crep) and L_Slice and R_Slice then
            Act_L_Array := Get_Referenced_Object (Prefix (Act_Lhs));
            Act_R_Array := Get_Referenced_Object (Prefix (Act_Rhs));

            --  If both left and right hand arrays are entity names, and
            --  refer to different entities, then we know that the move
            --  is safe (the two storage areas are completely disjoint).

            if Is_Entity_Name (Act_L_Array)
              and then Is_Entity_Name (Act_R_Array)
              and then Entity (Act_L_Array) /= Entity (Act_R_Array)
            then
               null;

            --  Otherwise, we assume the worst, which is that the two
            --  arrays are the same array. There is no need to check if
            --  we know that is the case, because if we don't know it,
            --  we still have to assume it!

            --  Generally if the same array is involved, then we have
            --  an overlapping case. We will have to really assume the
            --  worst (i.e. set neither of the OK flags) unless we can
            --  determine the lower or upper bounds at compile time and
            --  compare them.

            else
               Cresult := Compile_Time_Compare (Left_Lo, Right_Lo);

               if Cresult = Unknown then
                  Cresult := Compile_Time_Compare (Left_Hi, Right_Hi);
               end if;

               case Cresult is
                  when LT | LE | EQ => Set_Backwards_OK (N, False);
                  when GT | GE      => Set_Forwards_OK  (N, False);
                  when NE | Unknown => Set_Backwards_OK (N, False);
                                       Set_Forwards_OK  (N, False);
               end case;
            end if;
         end if;

         --  We now generate code for one of three possible cases:
         --  (these examples are for the one dimensional case, for
         --   the multi-dimensional case, see Expand_Assign_Array_Loop).

         --  Forwards_OK = True

         --    Rnn : right_index := right_index'First;
         --    for Lnn in left-index loop
         --       left (Lnn) := right (Rnn);
         --       Rnn := right_index'Succ (Rnn);
         --    end loop;

         --  Forwards_OK = False, Backwards_OK = True

         --    Rnn : right_index := right_index'Last;
         --    for Lnn in reverse left-index loop
         --       left (Lnn) := right (Rnn);
         --       Rnn := right_index'Pred (Rnn);
         --    end loop;

         --  Note: the above code MUST be analyzed with checks off,
         --  because otherwise the Succ or Pred could overflow. But
         --  in any case this is more efficient!

         --  Forwards_OK = Backwards_OK = False

         --    This only happens if we have the same array on each side. It is
         --    possible to create situations using overlays that violate this,
         --    but we simply do not promise to get this "right" in this case.

         --    if Left_Lo <= Right_Lo then
         --       <code for Forwards_OK = True above>
         --    else
         --       <code for Backwards_OK = True above>
         --    end if;

         --  Before we generate this code, we must ensure that the
         --  left and right side array types are defined. They may
         --  be itypes, and we cannot let them be defined inside the
         --  if, since the first use in the then may not be executed.

         Ensure_Defined (L_Type, N);
         Ensure_Defined (R_Type, N);

         if Forwards_OK (N) or else Backwards_OK (N) then
            Rewrite_Substitute_Tree (N,
              Expand_Assign_Array_Loop
                (N, Larray, Rarray, L_Type, R_Type, Ndim,
                 Rev => not Forwards_OK (N)));

         else
            Rewrite_Substitute_Tree (N,
              Make_If_Statement (Loc,
                Condition =>
                  Make_Op_Le (Loc,
                    Left_Opnd =>  New_Copy_Tree (Left_Lo),
                    Right_Opnd => New_Copy_Tree (Right_Lo)),

                Then_Statements => New_List (
                  Expand_Assign_Array_Loop
                   (N, Larray, Rarray, L_Type, R_Type, Ndim,
                    Rev => False)),

                Else_Statements => New_List (
                  Expand_Assign_Array_Loop
                   (N, Larray, Rarray, L_Type, R_Type, Ndim,
                    Rev => True))));
         end if;

         Analyze (N, Suppress => All_Checks);
      end Slice_Case;
   end Expand_Assign_Array;

   ------------------------------
   -- Expand_Assign_Array_Loop --
   ------------------------------

   --  The following is an example of the loop generated for the case of
   --  a two-dimensional array:

   --    R2b : Tm1X1 := 1;
   --    R4b : Tm1X2 := 1;
   --    for L1b in 1 .. 100 loop
   --       for L3b in 1 .. 100 loop
   --          vm1 (L1b, L3b) := vm2 (R2b, R4b);
   --          R4b := Tm1X2'succ(R4b);
   --       end loop;
   --       R2b := Tm1X1'succ(R2b);
   --    end loop;

   --  Here Rev is False, and Tm1Xn are the subscript types for the right
   --  hand side. The declarations of R2b and R4b are inserted before the
   --  original assignment statement.

   function Expand_Assign_Array_Loop
     (N      : Node_Id;
      Larray : Entity_Id;
      Rarray : Entity_Id;
      L_Type : Entity_Id;
      R_Type : Entity_Id;
      Ndim   : Pos;
      Rev    : Boolean)
      return   Node_Id
   is
      Loc  : constant Source_Ptr := Sloc (N);

      Lnn : array (1 .. Ndim) of Entity_Id;
      Rnn : array (1 .. Ndim) of Entity_Id;
      --  Entities used as subscripts on left and right sides

      L_Index_Type : array (1 .. Ndim) of Entity_Id;
      R_Index_Type : array (1 .. Ndim) of Entity_Id;
      --  Left and right index types

      Assign : Node_Id;

      F_Or_L : Name_Id;
      S_Or_P : Name_Id;

   begin
      if Rev then
         F_Or_L := Name_Last;
         S_Or_P := Name_Pred;
      else
         F_Or_L := Name_First;
         S_Or_P := Name_Succ;
      end if;

      --  Setup index types and subscript entities

      declare
         L_Index : Node_Id;
         R_Index : Node_Id;

      begin
         L_Index := First_Index (L_Type);
         R_Index := First_Index (R_Type);

         for J in 1 .. Ndim loop
            Lnn (J) :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('L'));

            Rnn (J) :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('R'));

            L_Index_Type (J) := Etype (L_Index);
            R_Index_Type (J) := Etype (R_Index);

            L_Index := Next_Index (L_Index);
            R_Index := Next_Index (R_Index);
         end loop;
      end;

      --  Build the declarations for the subscripts used on the right hand
      --  side (the left side subscripts are declared implicitly by the
      --  corresponding loops)

      declare
         Decls : List_Id := New_List;

      begin
         for J in 1 .. Ndim loop
            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Rnn (J),
                Object_Definition =>
                  New_Occurrence_Of (R_Index_Type (J), Loc),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Occurrence_Of (R_Index_Type (J), Loc),
                    Attribute_Name => F_Or_L)));
         end loop;

         Insert_Actions (N, Decls, Suppress => All_Checks);
      end;

      --  Now construct the assignment statement

      declare
         ExprL : List_Id := New_List;
         ExprR : List_Id := New_List;

      begin
         for J in 1 .. Ndim loop
            Append_To (ExprL, New_Occurrence_Of (Lnn (J), Loc));
            Append_To (ExprR, New_Occurrence_Of (Rnn (J), Loc));
         end loop;

         Assign :=
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Indexed_Component (Loc,
                 Prefix => Duplicate_Subexpr (Larray, Name_Req => True),
                 Expressions => ExprL),
             Expression =>
               Make_Indexed_Component (Loc,
                 Prefix => Duplicate_Subexpr (Rarray, Name_Req => True),
                 Expressions => ExprR));
      end;

      --  Now construct the loop from the inside out, with the last subscript
      --  varying most rapidly. Note that Assign is first the raw assignment
      --  statement, and then subsequently the loop that wraps it up.

      for J in reverse 1 .. Ndim loop
         Assign :=
           Make_Loop_Statement (Loc,
             Iteration_Scheme =>
               Make_Iteration_Scheme (Loc,
                 Loop_Parameter_Specification =>
                   Make_Loop_Parameter_Specification (Loc,
                     Defining_Identifier => Lnn (J),
                     Reverse_Present => Rev,
                     Discrete_Subtype_Definition =>
                       New_Reference_To (L_Index_Type (J), Loc))),

             Statements => New_List (
               Assign,

               Make_Assignment_Statement (Loc,
                 Name => New_Occurrence_Of (Rnn (J), Loc),
                 Expression =>
                   Make_Attribute_Reference (Loc,
                     Prefix =>
                       New_Occurrence_Of (R_Index_Type (J), Loc),
                     Attribute_Name => S_Or_P,
                     Expressions => New_List (
                       New_Occurrence_Of (Rnn (J), Loc))))));
      end loop;

      return Assign;
   end Expand_Assign_Array_Loop;

   --------------------------
   -- Expand_Assign_Record --
   --------------------------

   --  The only processing required is in the change of representation
   --  case, where we must expand the assignment to a series of field
   --  by field assignments.

   procedure Expand_Assign_Record (N : Node_Id) is
   begin
      if not Change_Of_Representation (N) then
         return;
      end if;

      --  At this stage we know that the right hand side is a conversion

      declare
         Loc   : constant Source_Ptr := Sloc (N);
         Lhs   : constant Node_Id    := Name (N);
         Rhs   : constant Node_Id    := Expression (Expression (N));
         L_Rec : constant Node_Id    := Name (N);
         R_Rec : constant Node_Id    := Expression (Expression (N));
         R_Typ : constant Entity_Id  := Base_Type (Etype (R_Rec));
         L_Typ : constant Entity_Id  := Etype (Lhs);
         Decl  : constant Node_Id    := Declaration_Node (R_Typ);
         RDef  : constant Node_Id    := Type_Definition (Decl);
         F     : Entity_Id;

         function Make_Component_List_Assign (CL : Node_Id) return List_Id;
         --  Returns a sequence of statements to assign the components that
         --  are referenced in the given component list.

         function Make_Field_Assign (C : Entity_Id) return Node_Id;
         --  Given C, the entity for a discriminant or component, build
         --  an assignment for the corresponding field values.

         function Make_Field_Assigns (CI : List_Id) return List_Id;
         --  Given CI, a component items list, construct series of statements
         --  for fieldwise assignment of the corresponding components.

         --------------------------------
         -- Make_Component_List_Assign --
         --------------------------------

         function Make_Component_List_Assign (CL : Node_Id) return List_Id is
            CI : constant List_Id := Component_Items (CL);
            VP : constant Node_Id := Variant_Part (CL);

            Result : List_Id;
            Alts   : List_Id;
            V      : Node_Id;
            DC     : Node_Id;
            DCH    : List_Id;

         begin
            Result := Make_Field_Assigns (CI);

            if Present (VP) then

               V := First_Non_Pragma (Variants (VP));
               Alts := New_List;
               while Present (V) loop

                  DCH := New_List;
                  DC := First (Discrete_Choices (V));
                  while Present (DC) loop
                     Append_To (DCH, New_Copy_Tree (DC));
                     DC := Next (DC);
                  end loop;

                  Append_To (Alts,
                    Make_Case_Statement_Alternative (Loc,
                      Discrete_Choices => DCH,
                      Statements =>
                        Make_Component_List_Assign (Component_List (V))));
                  V := Next_Non_Pragma (V);
               end loop;

               Append_To (Result,
                 Make_Case_Statement (Loc,
                   Expression =>
                     Make_Selected_Component (Loc,
                       Prefix => Duplicate_Subexpr (Rhs),
                       Selector_Name =>
                         Make_Identifier (Loc, Chars (Name (VP)))),
                   Alternatives => Alts));

            end if;

            return Result;
         end Make_Component_List_Assign;

         -----------------------
         -- Make_Field_Assign --
         -----------------------

         function Make_Field_Assign (C : Entity_Id) return Node_Id is
            A : Node_Id;

         begin
            A :=
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix => Duplicate_Subexpr (Lhs),
                    Selector_Name => Make_Identifier (Loc, Chars (C))),
                Expression =>
                  Make_Selected_Component (Loc,
                    Prefix => Duplicate_Subexpr (Rhs),
                    Selector_Name => Make_Identifier (Loc, Chars (C))));

            --  Set Assignment_OK, so discriminants can be assigned

            Set_Assignment_OK (Name (A), True);
            return A;
         end Make_Field_Assign;

         ------------------------
         -- Make_Field_Assigns --
         ------------------------

         function Make_Field_Assigns (CI : List_Id) return List_Id is
            Item   : Node_Id;
            Result : List_Id;

         begin
            Item := First (CI);
            Result := New_List;

            while Present (Item) loop
               if Nkind (Item) = N_Component_Declaration then
                  Append_To
                    (Result, Make_Field_Assign (Defining_Identifier (Item)));
               end if;

               Item := Next (Item);
            end loop;

            return Result;
         end Make_Field_Assigns;

      --  Start of processing for Expand_Assign_Record

      begin
         --  Note that we use the base type for this processing. This results
         --  in some extra work in the constrained case, but the change of
         --  representation case is so unusual that it is not worth the effort.

         --  First copy the discriminants. This is done unconditionally. It
         --  is required in the unconstrained left side case, and also in the
         --  case where this assignment was constructed during the expansion
         --  of a type conversion (since initialization of discriminants is
         --  suppressed in this case). It is unnecessary but harmless in
         --  other cases.

         if Has_Discriminants (L_Typ) then
            F := First_Discriminant (R_Typ);
            while Present (F) loop
               Insert_Action (N, Make_Field_Assign (F));
               F := Next_Discriminant (F);
            end loop;
         end if;

         if Present (Component_List (RDef)) then
            Insert_Actions
              (N, Make_Component_List_Assign (Component_List (RDef)));
         end if;

         Rewrite_Substitute_Tree (N, Make_Null_Statement (Loc));
      end;
   end Expand_Assign_Record;

   -----------------------------------
   -- Expand_N_Assignment_Statement --
   -----------------------------------

   --  For array types, deal with slice assignments and setting the flags
   --  to indicate if it can be statically determined which direction the
   --  move should go in. Also deal with generating length checks.

   procedure Expand_N_Assignment_Statement (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Lhs  : constant Node_Id    := Name (N);
      Rhs  : constant Node_Id    := Expression (N);
      Typ  : constant Entity_Id  := Underlying_Type (Etype (Lhs));
      Exp  : Node_Id;

      function Is_Ref_To_Bit_Packed_Array (P : Node_Id) return Boolean;
      --  Determine whether the node P is a reference to a bit packed
      --  array (see discussion in main body of procedure for a definition
      --  of this term). If so, then all subscripts in P are evaluated
      --  using Force_Evaluation, and True is returned. Otherwise False
      --  is returned, and P is not affected.

      --------------------------------
      -- Is_Ref_To_Bit_Packed_Array --
      --------------------------------

      function Is_Ref_To_Bit_Packed_Array (P : Node_Id) return Boolean is
         Result : Boolean;
         Expr   : Node_Id;

      begin
         if Nkind (P) = N_Indexed_Component
              or else
            Nkind (P) = N_Selected_Component
         then
            if Is_Bit_Packed_Array (Etype (Prefix (P))) then
               Result := True;
            else
               Result := Is_Ref_To_Bit_Packed_Array (Prefix (P));
            end if;

            if Result and then Nkind (P) = N_Indexed_Component then
               Expr := First (Expressions (P));

               while Present (Expr) loop
                  Force_Evaluation (Expr);
                  Expr := Next (Expr);
               end loop;
            end if;

            return Result;

         else
            return False;
         end if;
      end Is_Ref_To_Bit_Packed_Array;

   --  Start of processing for Expand_N_Assignment_Statement

   begin
      --  Check for a special case where a high level transformation is
      --  required. If we have either of:

      --    P.field := rhs;
      --    P (sub) := rhs;

      --  where P is a reference to a bit packed array, then we have to unwind
      --  the assignment. The exact meaning of being a reference to a bit
      --  packed array is as follows:

      --    An indexed component whose prefix is a bit packed array is a
      --     reference to a bit packed array.

      --    An indexed component or selected component whose prefix is a
      --     reference to a bit packed array is itself a reference ot a
      --     bit packed array.

      --  The required transformation is

      --     Tnn : prefix_type := P;
      --     Tnn.field := rhs;
      --     P := Tnn;

      --  or

      --     Tnn : prefix_type := P;
      --     Tnn (subscr) := rhs;
      --     P := Tnn;

      --  Since P is going to be evaluated more than once, any subscripts
      --  in P must have their evaluation forced.

      if (Nkind (Lhs) = N_Indexed_Component
           or else
          Nkind (Lhs) = N_Selected_Component)
        and then Is_Ref_To_Bit_Packed_Array (Prefix (Lhs))
      then
         declare
            BPAR_Expr : constant Node_Id   := Relocate_Node (Prefix (Lhs));
            BPAR_Typ  : constant Entity_Id := Etype (BPAR_Expr);
            Tnn       : constant Entity_Id :=
                          Make_Defining_Identifier (Loc,
                            Chars => New_Internal_Name ('T'));

         begin
            --  Insert the post assignment first, because we want to copy
            --  the BPAR_Expr tree before it gets analyzed in the context
            --  of the pre assignment. Note that we do not analyze the
            --  post assignment yet (we cannot till we have completed the
            --  analysis of the pre assignment). As usual, the analysis
            --  of this post assignment will happen on its own when we
            --  "run into" it after finishing the current assignment.

            Insert_After (N,
              Make_Assignment_Statement (Loc,
                Name       => New_Copy_Tree (BPAR_Expr),
                Expression => New_Occurrence_Of (Tnn, Loc)));

            --  At this stage BPAR_Expr is a reference to a bit packed
            --  array where the reference was not expanded in the original
            --  tree, since it was on the left side of an assignment. But
            --  in the pre-assignment statement (the object definition),
            --  BPAR_Expr will end up on the right hand side, and must be
            --  reexpanded. To achieve this, we reset the analyzed flag
            --  of all selected and indexed components down to the actual
            --  indexed component for the packed array.

            Exp := BPAR_Expr;
            loop
               Set_Analyzed (Exp, False);

               if Nkind (Exp) = N_Selected_Component
                    or else
                  Nkind (Exp) = N_Indexed_Component
               then
                  Exp := Prefix (Exp);
               else
                  exit;
               end if;
            end loop;

            --  Now we can insert and analyze the pre assignment

            Insert_Before_And_Analyze (N,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Tnn,
                Object_Definition   => New_Occurrence_Of (BPAR_Typ, Loc),
                Expression          => BPAR_Expr));

            --  Now fix up the original assignment and continue processing

            Rewrite_Substitute_Tree (Prefix (Lhs),
              New_Occurrence_Of (Tnn, Loc));
         end;
      end if;

      --  Apply discriminant check if required. If Lhs is an access type
      --  to a designated type with discriminants, we must always check.

      if Has_Discriminants (Etype (Lhs)) then

         --  Skip discriminant check if change of representation. Will be
         --  done when the change of representation is expanded out.

         if not Change_Of_Representation (N) then
            Apply_Discriminant_Check (Rhs, Etype (Lhs), Lhs);
         end if;

      --  In the access type case, we need the same discriminant check,
      --  and also range checks if we have an access to constrained array.

      elsif Is_Access_Type (Etype (Lhs))
        and then Is_Constrained (Designated_Type (Etype (Lhs)))
      then
         if Has_Discriminants (Designated_Type (Etype (Lhs))) then

            --  Skip discriminant check if change of representation. Will be
            --  done when the change of representation is expanded out.

            if not Change_Of_Representation (N) then
               Apply_Discriminant_Check (Rhs, Etype (Lhs));
            end if;

         elsif Is_Array_Type (Designated_Type (Etype (Lhs))) then
            Apply_Range_Check (Rhs, Etype (Lhs));

            if Is_Constrained (Etype (Lhs)) then
               Apply_Length_Check (Rhs, Etype (Lhs));
            end if;

            if Nkind (Rhs) = N_Allocator then
               declare
                  Target_Typ : constant Entity_Id := Etype (Expression (Rhs));
                  C_Es       : Check_Result;

               begin
                  C_Es :=
                    Range_Check
                      (Lhs,
                       Target_Typ,
                       Etype (Designated_Type (Etype (Lhs))));

                  Insert_Range_Checks
                    (C_Es,
                     N,
                     Target_Typ,
                     Sloc (Lhs),
                     Lhs);
               end;
            end if;
         end if;

      --  Apply range check for access type case

      elsif Is_Access_Type (Etype (Lhs))
        and then Nkind (Rhs) = N_Allocator
        and then Nkind (Expression (Rhs)) = N_Qualified_Expression
      then
         Analyze_And_Resolve (Expression (Rhs));
         Apply_Range_Check
           (Expression (Rhs), Designated_Type (Etype (Lhs)));
      end if;

      --  Case of assignment to a bit packed array element

      if Nkind (Lhs) = N_Indexed_Component
        and then Is_Bit_Packed_Array (Etype (Prefix (Lhs)))
      then
         Expand_Bit_Packed_Element_Set (N);
         return;

      --  Case of tagged type assignment

      elsif Is_Tagged_Type (Typ)
        or else (Controlled_Type (Typ) and then not Is_Array_Type (Typ))
      then
         Tagged_Case : declare
            L : List_Id;

         begin
            --  In the controlled case, we need to make sure that function
            --  calls are evaluated before finalizing the target. In all
            --  cases, it makes the expansion easier if the side-effects
            --  are removed first.

            Remove_Side_Effects (Lhs);
            Remove_Side_Effects (Rhs);

            --  Avoid recursion in the mechanism

            Set_Analyzed (N);

            --  In the class-wide case, rewrite the assignment in a dispatch
            --  call to _assign

            if Is_Class_Wide_Type (Typ) then
               L := New_List (
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Reference_To (
                     Find_Prim_Op (Root_Type (Typ), Name_uAssign), Loc),

                   Parameter_Associations => New_List (
                     Duplicate_Subexpr (Lhs),
                     Convert_To (Etype (Lhs), Duplicate_Subexpr (Rhs)))));

            else
               L := Make_Tag_Ctrl_Assignment (N);
            end if;

            --  We can't afford to have destructive Finalization Actions in
            --  the Self assignment case, so if the target and the source are
            --  not obviously different, code is generated to avoid the self
            --  assignment case

            if Statically_Different (Lhs, Rhs)
              or else Chars (Current_Scope) = Name_uAssign
            then
               Rewrite_Substitute_Tree (N,
                  Make_Block_Statement (Loc,
                    Handled_Statement_Sequence =>
                      Make_Handled_Sequence_Of_Statements (Loc,
                        Statements => L)));

            --  Otherwise generate:
            --
            --    if lhs'address /= rhs'address then
            --       <code for controlled and/or tagged assignment>
            --    end if;

            else
               Rewrite_Substitute_Tree (N,
                 Make_If_Statement (Loc,
                   Condition =>
                     Make_Op_Ne (Loc,
                       Left_Opnd =>
                         Make_Attribute_Reference (Loc,
                           Prefix         => Lhs,
                           Attribute_Name => Name_Address),

                        Right_Opnd =>
                         Make_Attribute_Reference (Loc,
                           Prefix         => Rhs,
                           Attribute_Name => Name_Address)),

                   Then_Statements => L));
            end if;

            Analyze (N);
            return;
         end Tagged_Case;

      --  Array types

      elsif Is_Array_Type (Typ) then
         Expand_Assign_Array (N);

      --  Record types

      elsif Is_Record_Type (Typ) then
         Expand_Assign_Record (N);
      end if;

   end Expand_N_Assignment_Statement;

   -----------------------------
   -- Expand_N_Case_Statement --
   -----------------------------

   --  If the last alternative is not an Others choice replace it with an
   --  N_Others_Choice. Note that we do not bother to call Analyze on the
   --  modified case statement, since it's only effect would be to compute
   --  the contents of the Others_Discrete_Choices node laboriously, and of
   --  course we already know the list of choices that corresponds to the
   --  others choice (it's the list we are replacing!)

   procedure Expand_N_Case_Statement (N : Node_Id) is
      Altnode     : constant Node_Id := Last (Alternatives (N));
      Others_Node : Node_Id;

   begin
      if Nkind (First (Discrete_Choices (Altnode))) /= N_Others_Choice then
         Others_Node := Make_Others_Choice (Sloc (Altnode));
         Set_Others_Discrete_Choices
           (Others_Node, Discrete_Choices (Altnode));
         Set_Discrete_Choices (Altnode, New_List (Others_Node));
      end if;
   end Expand_N_Case_Statement;

   ---------------------------
   -- Expand_N_If_Statement --
   ---------------------------

   --  First, we deal with the obvious rewriting for the cases where the
   --  condition of the IF is known at compile time to be True or False.

   --  Second, we remove elsif parts which have non-empty Condition_Actions
   --  and rewrite as independent if statements. For example:

   --     if x then xs
   --     elsif y then ys
   --     ...
   --     end if;

   --  becomes
   --
   --     if x then xs
   --     else
   --        <<condition actions of y>>
   --        if y then ys
   --        ...
   --        end if;
   --     end if;

   --  This rewriting is needed if at least one elsif part has a non-empty
   --  Condition_Actions list. We also do the same processing if there is
   --  a constant condition in an elsif part (in conjunction with the first
   --  processing step mentioned above, for the recursive call made to deal
   --  with the created inner if, this deals with properly optimizing the
   --  cases of constant elsif conditions).

   procedure Expand_N_If_Statement (N : Node_Id) is
      Hed    : Node_Id;
      E      : Node_Id;
      New_If : Node_Id;

   begin
      --  The following loop deals with constant conditions for the IF. We
      --  need a loop because as we eliminate False conditions, we grab the
      --  first elsif condition and use it as the primary condition.

      while Nkind (Condition (N)) = N_Identifier
        and then (Entity (Condition (N)) = Standard_True
                   or else
                  Entity (Condition (N)) = Standard_False)
      loop
         --  If condition is True, we can simply rewrite the if statement
         --  now by replacing it by the series of then statements.

         if Entity (Condition (N)) = Standard_True then
            Hed := Remove_Head (Then_Statements (N));
            Insert_List_After (N, Then_Statements (N));
            Rewrite_Substitute_Tree (N, Hed);
            return;

         --  If condition is False, then we can delete the condition and
         --  the Then statements

         else
            --  If there are no elsif statements, then we simply replace
            --  the entire if statement by the sequence of else statements.

            if No (Elsif_Parts (N)) then

               if No (Else_Statements (N))
                 or else Is_Empty_List (Else_Statements (N))
               then
                  Rewrite_Substitute_Tree (N,
                    Make_Null_Statement (Sloc (N)));

               else
                  Hed := Remove_Head (Else_Statements (N));
                  Insert_List_After (N, Else_Statements (N));
                  Rewrite_Substitute_Tree (N, Hed);
               end if;

               return;

            --  If there are elsif statements, the first of them becomes
            --  the if/then section of the rebuilt if statement This is
            --  the case where we loop to reprocess this copied condition.

            else
               Hed := Remove_Head (Elsif_Parts (N));
               Insert_Actions      (N, Condition_Actions (Hed));
               Set_Condition       (N, Condition (Hed));
               Set_Then_Statements (N, Then_Statements (Hed));

               if Is_Empty_List (Elsif_Parts (N)) then
                  Set_Elsif_Parts (N, No_List);
               end if;
            end if;
         end if;
      end loop;

      --  Loop through elsif parts, dealing with constant conditions and
      --  possible expression actions that are present.

      if Present (Elsif_Parts (N)) then
         E := First (Elsif_Parts (N));
         while Present (E) loop

            --  If there are condition actions, then we rewrite the if
            --  statement as indicated above. We also do the same rewrite
            --  if the condition is True or False. The further processing
            --  of this constant condition is then done by the recursive
            --  call to expand the newly created if statement

            if Present (Condition_Actions (E))
              or else
                (Nkind (Condition (E)) = N_Identifier
                  and then
                    (Entity (Condition (E)) = Standard_True
                      or else
                     Entity (Condition (E)) = Standard_False))
            then
               New_If :=
                 Make_If_Statement (Sloc (E),
                   Condition       => Condition (E),
                   Then_Statements => Then_Statements (E),
                   Elsif_Parts     => No_List,
                   Else_Statements => Else_Statements (N));

               --  Elsif parts for new if come from remaining elsif's of parent

               while Present (Next (E)) loop
                  if No (Elsif_Parts (New_If)) then
                     Set_Elsif_Parts (New_If, New_List);
                  end if;

                  Append (Remove_Next (E), Elsif_Parts (New_If));
               end loop;

               Set_Else_Statements (N, New_List (New_If));

               if Present (Condition_Actions (E)) then
                  Insert_List_Before (New_If, Condition_Actions (E));
               end if;

               Remove (E);

               if Is_Empty_List (Elsif_Parts (N)) then
                  Set_Elsif_Parts (N, No_List);
               end if;

               Analyze (New_If);
               return;

            --  No special processing for that elsif part, move to next

            else
               E := Next (E);
            end if;
         end loop;
      end if;
   end Expand_N_If_Statement;

   -----------------------------
   -- Expand_N_Loop_Statement --
   -----------------------------

   --  1. Deal with loops with a non-standard enumeration type range
   --  2. Deal with while loops where Condition_Actions is set

   procedure Expand_N_Loop_Statement (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Isc  : constant Node_Id    := Iteration_Scheme (N);

   begin
      if No (Isc) then
         return;
      end if;

      --  Handle the case where we have a for loop with the range type being
      --  an enumeration type with non-standard representation. In this case
      --  we expand:

      --    for x in [reverse] a .. b loop
      --       ...
      --    end loop;

      --  to

      --    for xP in [reverse] integer
      --                          range etype'Pos (a) .. etype'Pos (b) loop
      --       declare
      --          x : constant etype := Pos_To_Rep (xP);
      --       begin
      --          ...
      --       end;
      --    end loop;

      if Present (Loop_Parameter_Specification (Isc)) then
         declare
            LPS     : constant Node_Id   := Loop_Parameter_Specification (Isc);
            Loop_Id : constant Entity_Id := Defining_Identifier (LPS);
            Ltype   : constant Entity_Id := Etype (Loop_Id);
            Btype   : constant Entity_Id := Base_Type (Ltype);
            New_Id  : Entity_Id;
            Lo, Hi  : Node_Id;

         begin
            if not Is_Enumeration_Type (Btype)
              or else No (Enum_Pos_To_Rep (Btype))
            then
               return;
            end if;

            New_Id :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Chars (Loop_Id), 'P'));

            Lo := Type_Low_Bound (Ltype);
            Hi := Type_High_Bound (Ltype);

            Rewrite_Substitute_Tree (N,

              Make_Loop_Statement (Loc,
                Identifier => Identifier (N),

                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier => New_Id,
                        Reverse_Present => Reverse_Present (LPS),

                        Discrete_Subtype_Definition =>
                          Make_Subtype_Indication (Loc,

                            Subtype_Mark =>
                              New_Reference_To (Standard_Natural, Loc),

                            Constraint =>
                              Make_Range_Constraint (Loc,
                                Range_Expression =>
                                  Make_Range (Loc,

                                    Low_Bound =>
                                      Make_Attribute_Reference (Loc,
                                        Prefix =>
                                          New_Reference_To (Btype, Loc),

                                        Attribute_Name => Name_Pos,

                                        Expressions => New_List (
                                          Relocate_Node
                                            (Type_Low_Bound (Ltype)))),

                                    High_Bound =>
                                      Make_Attribute_Reference (Loc,
                                        Prefix =>
                                          New_Reference_To (Btype, Loc),

                                        Attribute_Name => Name_Pos,

                                        Expressions => New_List (
                                          Relocate_Node
                                            (Type_High_Bound (Ltype))))))))),

                Statements => New_List (
                  Make_Block_Statement (Loc,
                    Declarations => New_List (
                      Make_Object_Declaration (Loc,
                        Defining_Identifier => Loop_Id,
                        Constant_Present    => True,
                        Object_Definition   => New_Reference_To (Ltype, Loc),
                        Expression          =>
                          Make_Indexed_Component (Loc,
                            Prefix =>
                              New_Reference_To (Enum_Pos_To_Rep (Btype), Loc),
                            Expressions => New_List (
                              New_Reference_To (New_Id, Loc))))),

                    Handled_Statement_Sequence =>
                      Make_Handled_Sequence_Of_Statements (Loc,
                        Statements => Statements (N))))));

            Analyze (N);
         end;

      --  Second case, if we have a while loop with Condition_Actions set,
      --  then we change it into a plain loop:

      --    while C loop
      --       ...
      --    end loop;

      --  changed to:

      --    loop
      --       <<condition actions>>
      --       exit when not C;
      --       ...
      --    end loop

      elsif Present (Isc)
        and then Present (Condition_Actions (Isc))
      then
         declare
            Cond : constant Node_Id    := Condition (Isc);
            ES   : Node_Id;

         begin
            ES :=
              Make_Exit_Statement (Sloc (Condition (Isc)),
                Condition =>
                  Make_Op_Not (Sloc (Condition (Isc)),
                    Right_Opnd => Condition (Isc)));

            Prepend (ES, Statements (N));
            Insert_List_Before (ES, Condition_Actions (Isc));

            Rewrite_Substitute_Tree (N,
              Make_Loop_Statement (Sloc (N),
                Identifier => Identifier (N),
                Statements => Statements (N)));

            Analyze (N);
         end;
      end if;
   end Expand_N_Loop_Statement;

   -------------------------------
   -- Expand_N_Return_Statement --
   -------------------------------

   procedure Expand_N_Return_Statement (N : Node_Id) is
      Loc         : constant Source_Ptr := Sloc (N);
      Exp         : constant Node_Id := Expression (N);
      T           : Entity_Id;
      Utyp        : Entity_Id;
      Scope_Id    : Entity_Id;
      Kind        : Entity_Kind;
      Call        : Node_Id;
      Acc_Stat    : Node_Id;
      Goto_Stat   : Node_Id;
      Lab_Node    : Node_Id;
      Cur_Idx     : Int;
      Return_Type : Entity_Id;

   begin
      for J in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (J).Entity;
         Cur_Idx := J;
         exit when Ekind (Scope_Id) /= E_Block and then
                   Ekind (Scope_Id) /= E_Loop;
      end loop;

      if No (Exp) then
         Kind := Ekind (Scope_Id);

         --  If it is a return from procedures do no extra steps.

         if Kind = E_Procedure or else Kind = E_Generic_Procedure then
            return;
         end if;

         pragma Assert (Is_Entry (Scope_Id));

         --  Look at the enclosing block to see whether the return is from
         --  an accept statement or an entry body.

         for J in reverse 0 .. Cur_Idx loop
            Scope_Id := Scope_Stack.Table (J).Entity;
            exit when Is_Concurrent_Type (Scope_Id);
         end loop;

         --  If it is a return from accept statement it should be expanded
         --  as a call to RTS Complete_Rendezvous and a goto to the end of
         --  the accept body.

         --  (cf : Expand_N_Accept_Statement, Expand_N_Selective_Accept,
         --   Expand_N_Accept_Alternative in exp_ch9.adb)

         if Is_Task_Type (Scope_Id) then

            Call := (Make_Procedure_Call_Statement (Loc,
                      Name => New_Reference_To
                        (RTE (RE_Complete_Rendezvous), Loc)));
            Insert_Before (N, Call);
            --  why not insert actions here???
            Analyze (Call);

            Acc_Stat := Parent (N);
            while Nkind (Acc_Stat) /= N_Accept_Statement loop
               Acc_Stat := Parent (Acc_Stat);
            end loop;

            Lab_Node := Last (Statements
              (Handled_Statement_Sequence (Acc_Stat)));

            Goto_Stat := Make_Goto_Statement (Loc,
              Name => New_Occurrence_Of
                (Entity (Identifier (Lab_Node)), Loc));

            Set_Analyzed (Goto_Stat);

            Rewrite_Substitute_Tree (N, Goto_Stat);
            Analyze (N);

         --  If it is a return from an entry body, put a Complete_Entry_Body
         --  call in front of the return.

         elsif Is_Protected_Type (Scope_Id) then

            Call :=
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To
                  (RTE (RE_Complete_Entry_Body), Loc),
                Parameter_Associations => New_List
                  (Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Reference_To
                        (Object_Ref
                           (Corresponding_Body (Parent (Scope_Id))),
                        Loc),
                    Attribute_Name => Name_Unchecked_Access)));

            Insert_Before (N, Call);
            Analyze (Call);

         end if;

         return;
      end if;

      T    := Etype (Exp);
      Return_Type := Etype (Scope_Id);
      Utyp := Underlying_Type (Return_Type);

      --  Check the result expression of a scalar function against
      --  the subtype of the function by inserting a conversion.
      --  This conversion must eventually be performed for other
      --  classes of types, but for now it's only done for scalars.
      --  ???

      if Is_Scalar_Type (T) then

         Rewrite_Substitute_Tree (Exp, Convert_To (Return_Type, Exp));
         Analyze (Exp);

      end if;

      --  Deal with returning on the secondary stack for complex objects

      if Is_Return_By_Reference_Type (T)
        or else not Requires_Transient_Scope (Return_Type)
      then
         null;

      else
         --  ??? Make sure that no surrounding block will reclaim the
         --  sec-stack on which we are going to put the result.... This may
         --  introduce leaks in the sec stack. Those leaks are usually not
         --  too worrysome since they are reclaimed on return of the
         --  function but nonetheless there is a potential problem here.

         declare
            S : Entity_Id := Current_Scope;

         begin
            while Ekind (S)  = E_Block loop
               Set_Uses_Sec_Stack (S, False);
               S := Enclosing_Dynamic_Scope (S);
            end loop;
         end;

         --  Optimize the case where the result is already on the
         --  secondary-stack except when one type is an unconstrained array
         --  and the other is constrained because the bounds are not
         --  allocated at the same place

         if Requires_Transient_Scope (T)
           and then Is_Constrained (T) = Is_Constrained (Return_Type)
           and then (Nkind (Exp) = N_Function_Call
                        or else Nkind (Original_Node (Exp)) = N_Function_Call)
         then
            Set_By_Ref (N);

         --  For controlled types, do the allocation on the sec-stack
         --  manually in order to call adjust at the right time
         --    type Anon1 is access Return_Type;
         --    for Anon1'Storage_pool use ss_pool;
         --    Anon2 : anon1 := new Return_Type'(expr);
         --    return Anon2.all;

         elsif Controlled_Type (Utyp) then
            declare
               Loc        : constant Source_Ptr := Sloc (N);
               Temp       : constant Entity_Id :=
                              Make_Defining_Identifier (Loc,
                                Chars => New_Internal_Name ('R'));
               Acc_Typ    : constant Entity_Id :=
                              Make_Defining_Identifier (Loc,
                                Chars => New_Internal_Name ('A'));
               Alloc_Node : Node_Id;

            begin
               Set_Ekind (Acc_Typ, E_Access_Type);
               Set_Associated_Storage_Pool (Acc_Typ, RTE (RE_SS_Pool));
               Alloc_Node :=
                 Make_Allocator (Loc,
                   Expression =>
                     Make_Qualified_Expression (Loc,
                       Subtype_Mark => New_Reference_To (Etype (Exp), Loc),
                       Expression => Relocate_Node (Exp)));

               Insert_List_Before_And_Analyze (N, New_List (
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Acc_Typ,
                   Type_Definition     =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication =>
                          New_Reference_To (Return_Type, Loc))),

                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   => New_Reference_To (Acc_Typ, Loc),
                   Expression          => Alloc_Node)));

               Rewrite_Substitute_Tree (Exp,
                 Make_Explicit_Dereference (Loc,
                 Prefix => New_Reference_To (Temp, Loc)));

               Analyze_And_Resolve (Exp, Return_Type);
            end;

         --  use the gigi mechanism to allocate result on the sec-stack

         else
            Set_Storage_Pool      (N, RTE (RE_SS_Pool));
            Set_Procedure_To_Call (N, RTE (RE_SS_Allocate));
         end if;
      end if;
   end Expand_N_Return_Statement;

   ------------------------------
   -- Make_Tag_Ctrl_Assignment --
   ------------------------------

   function Make_Tag_Ctrl_Assignment (N : Node_Id) return List_Id is
      Loc        : constant Source_Ptr := Sloc (N);
      In_uAssign : constant Boolean := Chars (Current_Scope) = Name_uAssign;
      L          : constant Node_Id := Name (N);
      T          : constant Entity_Id := Underlying_Type (Etype (L));

      Ctrl_Act : constant Boolean := Controlled_Type (T)
                                       and then not No_Ctrl_Actions (N);
      Save_Tag : constant Boolean := Is_Tagged_Type (T)
                                       and then not In_uAssign
                                       and then not No_Ctrl_Actions (N);
      Res      : List_Id;
      Tag_Tmp  : Entity_Id;
      Prev_Tmp : Entity_Id;
      Next_Tmp : Entity_Id;
      Ctrl_Ref : Node_Id;

   begin
      Res := New_List;

      --  Finalize the target of the assignment when controlled. (not in
      --  the init_proc since it is an initialization more than an
      --  assignment)

      if Ctrl_Act then
         Append_List_To (Res,
           Make_Final_Call (
             Ref         => Duplicate_Subexpr (L),
             Typ         => Etype (L),
             With_Detach => New_Reference_To (Standard_False, Loc)));
      end if;

      Next_Tmp := Make_Defining_Identifier (Loc, New_Internal_Name ('C'));

      --  Save the Tag in a local variable 'Tag_Tmp'

      if Save_Tag then
         Tag_Tmp := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
         Append_To (Res,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Tag_Tmp,
             Object_Definition => New_Reference_To (RTE (RE_Tag), Loc),
             Expression =>
               Make_Selected_Component (Loc,
                 Prefix        => Duplicate_Subexpr (L),
                 Selector_Name => New_Reference_To (Tag_Component (T), Loc))));
      end if;

      --  Save the Finalization Pointers in local variables Prev_Tmp and
      --  Next_Tmp. For objects with Has_Controlled_Component set, these
      --  pointers are in the Record_Controller

      if Ctrl_Act then
         Ctrl_Ref := Duplicate_Subexpr (L);

         if Has_Controlled_Component (T) then
            Ctrl_Ref :=
              Make_Selected_Component (Loc,
                Prefix => Ctrl_Ref,
                Selector_Name =>
                  New_Reference_To (Controller_Component (T), Loc));
         end if;

         Prev_Tmp := Make_Defining_Identifier (Loc, New_Internal_Name ('B'));

         Append_To (Res,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Prev_Tmp,

             Object_Definition =>
               New_Reference_To (RTE (RE_Finalizable_Ptr), Loc),

             Expression =>
               Make_Selected_Component (Loc,
                 Prefix =>
                   Unchecked_Convert_To (RTE (RE_Finalizable), Ctrl_Ref),
                 Selector_Name => Make_Identifier (Loc, Name_Prev))));

         Next_Tmp := Make_Defining_Identifier (Loc, New_Internal_Name ('C'));

         Append_To (Res,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Next_Tmp,

             Object_Definition =>
               New_Reference_To (RTE (RE_Finalizable_Ptr), Loc),

             Expression =>
               Make_Selected_Component (Loc,
                 Prefix =>
                   Unchecked_Convert_To (RTE (RE_Finalizable),
                     New_Copy_Tree (Ctrl_Ref)),
                 Selector_Name => Make_Identifier (Loc, Name_Next))));
      end if;

      --  Do the Assignment

      Append_To (Res, Relocate_Node (N));

      --  Restore the Tag

      if Save_Tag then
         Append_To (Res,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Selected_Component (Loc,
                 Prefix        => Duplicate_Subexpr (L),
                 Selector_Name => New_Reference_To (Tag_Component (T), Loc)),
             Expression => New_Reference_To (Tag_Tmp, Loc)));
      end if;

      --  Restore the finalization pointers

      if Ctrl_Act then
         Append_To (Res,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Selected_Component (Loc,
                 Prefix =>
                   Unchecked_Convert_To (RTE (RE_Finalizable),
                     New_Copy_Tree (Ctrl_Ref)),
                 Selector_Name => Make_Identifier (Loc, Name_Prev)),
             Expression => New_Reference_To (Prev_Tmp, Loc)));

         Append_To (Res,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Selected_Component (Loc,
                 Prefix =>
                   Unchecked_Convert_To (RTE (RE_Finalizable),
                     New_Copy_Tree (Ctrl_Ref)),
                 Selector_Name => Make_Identifier (Loc, Name_Next)),
             Expression => New_Reference_To (Next_Tmp, Loc)));
      end if;

      --  Adjust the target after the assignment when controlled. (not in
      --  the init_proc since it is an initialization more than an
      --  assignment)

      if Ctrl_Act then
         Append_List_To (Res,
           Make_Adjust_Call (
             Ref         => Duplicate_Subexpr (L),
             Typ         => Etype (L),
             Flist_Ref   => New_Reference_To (RTE (RE_Global_Final_List), Loc),
             With_Attach => Make_Integer_Literal (Loc, Uint_0)));
      end if;

      return Res;
   end Make_Tag_Ctrl_Assignment;

end Exp_Ch5;

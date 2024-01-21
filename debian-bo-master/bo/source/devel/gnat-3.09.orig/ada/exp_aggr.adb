------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A G G R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.79 $                             --
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

with Atree;    use Atree;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Exp_Util; use Exp_Util;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch7;  use Exp_Ch7;
with Hostparm;
with Itypes;   use Itypes;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Aggr is

   --  ??? beginning of temporary code
   --  ??? Should be merged with sem_ch5 identical functionality

   type Case_Bounds is record
     Choice_Lo   : Node_Id;
     Choice_Hi   : Node_Id;
     Choice_Node : Node_Id;
   end record;

   type Case_Table_Type is array (Nat range <>) of Case_Bounds;
   --  Table type used by Check_Case_Choices procedure

   procedure Sort_Case_Table (Case_Table : in out Case_Table_Type);
   --  Sort the Case Table using the Lower Bound of each Choice as the key.
   --  A simple insertion sort is used since the number of choices in a case
   --  statement of variant part will usually be small and probably in near
   --  sorted order.

   ---------------------
   -- Sort_Case_Table --
   ---------------------

   procedure Sort_Case_Table (Case_Table : in out Case_Table_Type) is
      L : Int := Case_Table'First;
      U : Int := Case_Table'Last;
      K : Int;
      J : Int;
      T : Case_Bounds;

   begin
      K := L;

      while K /= U loop
         T := Case_Table (K + 1);
         J := K + 1;

         while J /= L
           and then Expr_Value (Case_Table (J - 1).Choice_Lo) >
                    Expr_Value (T.Choice_Lo)
         loop
            Case_Table (J) := Case_Table (J - 1);
            J := J - 1;
         end loop;

         Case_Table (J) := T;
         K := K + 1;
      end loop;
   end Sort_Case_Table;

   --  ??? end of temporary code

   ------------------------------------------------------
   -- Local subprograms for Record Aggregate Expansion --
   ------------------------------------------------------

   procedure Expand_Record_Aggregate
     (N           : Node_Id;
      Orig_Tag    : Node_Id := Empty;
      Parent_Expr : Node_Id := Empty);
   --  This is the top level procedure for record aggregate expansion.
   --  Expansion for record aggregates needs expand aggregates for tagged
   --  record types. Specifically Expand_Record_Aggregate adds the Tag
   --  field in front of the Component_Association list that was created
   --  during resolution by Resolve_Record_Aggregate.
   --
   --    N is the record aggregate node.
   --    Orig_Tag is the value of the Tag that has to be provided for this
   --      specific aggregate. It carries the tag corresponding to the type
   --      of the outermost aggregate during the recursive expansion
   --    Parent_Expr is the ancestor part of the original extension
   --      aggregate

   procedure Convert_To_Assignments (N : Node_Id; Typ : Entity_Id);
   --  N is an N_Aggregate of a N_Extension_Aggregate. Typ is the type of
   --  the aggregate. Transform the given aggregate into a buch of
   --  assignments component per component.

   function Build_Record_Aggr_Code
     (N      : Node_Id;
      Typ    : Entity_Id;
      Target : Node_Id;
      Flist  : Node_Id := Empty)
      return   List_Id;
   --  N is an N_Aggregate of a N_Extension_Aggregate. Typ is the type of
   --  the aggregate. Target is an expression containing the location on
   --  which the component by component assignments will take
   --  place. Returns the list of assignments plus all other adjustments
   --  needed for tagged and controlled types. Flist is an expression
   --  representing the finalization list on which to attach the controlled
   --  components if any.

   -----------------------------------------------------
   -- Local subprograms for array aggregate expansion --
   -----------------------------------------------------

   procedure Expand_Array_Aggregate (N : Node_Id);
   --  This is the top-level routine to perform array aggregate expansion.
   --  N is the N_Aggregate node to be expanded.

   function Static_Processing_Possible (N : Node_Id) return Boolean;
   --  This function checks if array aggregate N can be processed directly
   --  by Gigi. Iff this is the case True is returned.

   function Contains_Safe_Scalars (N : Node_Id) return Boolean;
   --  Returns true if array aggregate N contains only scalar constants
   --  or variables whose evaluation cannot possibly raise constraint error.

   function Build_Array_Aggr_Code
     (N           : Node_Id;
      Index       : Node_Id;
      Into        : Node_Id;
      Scalar_Comp : Boolean;
      Indices     : List_Id := No_List;
      Flist       : Node_Id := Empty)
      return        List_Id;
   --  This recursive routine returns a list of statements containing the
   --  loops and assignments that are needed for the expansion of the array
   --  aggregate N.
   --
   --    N is the (sub-)aggregate node to be expanded into code.
   --
   --    Index is the index node corresponding to the array sub-aggregate N.
   --
   --    Into is the target expression into which we are copying the aggregate.
   --
   --    Scalar_Comp is True if the component type of the aggregate is scalar.
   --
   --    Indices is the current list of expressions used to index the
   --    object we are writing into.
   --
   --    Flist is an expression representing the finalization list on which
   --    to attach the controlled components if any.

   function Number_Of_Choices (N : Node_Id) return Nat;
   --  Returns the number of discrete choices (not including the others choice
   --  if present) contained in (sub-)aggregate N.

   function Late_Expansion
     (N      : Node_Id;
      Typ    : Entity_Id;
      Target : Node_Id;
      Flist  : Node_Id := Empty)
      return List_Id;
   --  N is a nested (record or array) aggregate that has been marked with
   --  'Delay_Expansion'. Typ is the expected type of the aggregate and
   --  Target is a (duplicable) expression that will hold the result of the
   --  aggregate expansion. Basically this procedure is used to implement
   --  top-down expansions of nested aggregates. This is necessary for
   --  avoiding temporaries at each level as well as for propagating the
   --  right internal finalization list.

   ---------------------------
   -- Build_Array_Aggr_Code --
   ---------------------------

   --  The code that we generate from a one dimensional aggregate is

   --  1. If the sub-aggregate contains discrete choices we

   --     (a) Sort the discrete choices

   --     (b) Otherwise for each discrete choice that specifies a range we
   --         emit a loop. If a range specifies a maximum of three values, or
   --         we are dealing with an expression we emit a sequence of
   --         assignments instead of a loop.

   --     (c) Generate the remaning loops to cover the others choice if any.

   --  2. If the aggregate contains positional elements we

   --     (a) translate the positional elements in a series of assignments.

   --     (b) Generate a final loop to cover the others choice if any.
   --         Note that this final loop has to be a while loop since the case

   --             L : Integer := Integer'Last;
   --             H : Integer := Integer'Last;
   --             A : array (L .. H) := (1, others =>0);

   --         cannot be handled by a for loop. Thus for the following

   --             array (L .. H) := (.. positional elements.., others =>E);

   --         we always generate something like:

   --             I : Index_Type := Index_Of_Last_Positional_Element;
   --             while I < H loop
   --                I := Index_Base'Succ (I)
   --                Tmp (I) := E;
   --             end loop;

   function Build_Array_Aggr_Code
     (N           : Node_Id;
      Index       : Node_Id;
      Into        : Node_Id;
      Scalar_Comp : Boolean;
      Indices     : List_Id := No_List;
      Flist       : Node_Id := Empty)
      return        List_Id
   is
      Loc          : constant Source_Ptr := Sloc (N);
      Index_Base   : constant Entity_Id  := Base_Type (Etype (Index));
      Index_Base_L : constant Node_Id := Type_Low_Bound (Index_Base);
      Index_Base_H : constant Node_Id := Type_High_Bound (Index_Base);

      function Add (Val : Int; To : Node_Id) return Node_Id;
      --  Returns an expression where Val is added to expression To,
      --  unless To+Val is provably out of To's base type range.
      --  To must be an already analyzed expression.

      function Empty_Range (L, H : Node_Id) return Boolean;
      --  Returns True if the range defined by L .. H is certainly empty.

      function Equal (L, H : Node_Id) return Boolean;
      --  Returns True if L = H for sure.

      function Index_Base_Name return Node_Id;
      --  Returns a new reference to the index type name.

      function Gen_Assign (Ind : Node_Id; Expr : Node_Id) return List_Id;
      --  Ind must be a side-effect free expression.
      --  If the input aggregate N to Build_Loop contains no sub-aggregates,
      --  This routine returns the assignment statement
      --
      --     Into (Indices, Ind) := Expr;
      --
      --  Otherwise we call Build_Code recursively.

      function Gen_Loop (L, H : Node_Id; Expr : Node_Id) return List_Id;
      --  Nodes L and H must be side-effect free expressions.
      --  If the input aggregate N to Build_Loop contains no sub-aggregates,
      --  This routine returns the for loop statement
      --
      --     for J in Index_Base'(L) .. Index_Base'(H) loop
      --        Into (Indices, J) := Expr;
      --     end loop;
      --
      --  Otherwise we call Build_Code recursively.
      --  As an optimization if the loop covers 3 or less scalar elements we
      --  generate a sequence of assignments.

      function Gen_While (L, H : Node_Id; Expr : Node_Id) return List_Id;
      --  Nodes L and H must be side-effect free expressions.
      --  If the input aggregate N to Build_Loop contains no sub-aggregates,
      --  This routine returns the while loop statement
      --
      --     I : Index_Base := L;
      --     while I < H loop
      --        I := Index_Base'Succ (I);
      --        Into (Indices, I) := Expr;
      --     end loop;
      --
      --  Otherwise we call Build_Code recursively.

      function Local_Compile_Time_Known_Value (E : Node_Id) return Boolean;
      function Local_Expr_Value               (E : Node_Id) return Uint;
      --  These two Local routines are used to replace the corresponding ones
      --  in sem_eval because while processing the bounds of an aggregate with
      --  discrete choices whose index type is an enumeration, we build static
      --  expressions not recognized by Compile_Time_Known_Value as such since
      --  they have not yet been analyzed and resolved. All the expressions in
      --  questions are things like Index_Base_Name'Val (Const) which we can
      --  easily recognize as being constant.

      ---------
      -- Add --
      ---------

      function Add (Val : Int; To : Node_Id) return Node_Id is
         Expr_Pos : Node_Id;
         Expr     : Node_Id;
         To_Pos   : Node_Id;

         U_To  : Uint;
         U_Val : Uint := UI_From_Int (Val);

      begin
         --  Note: do not try to optimize the case of Val = 0, because
         --  we need to build a new node with the proper Sloc value anyway.

         --  First test if we can do constant folding

         if Local_Compile_Time_Known_Value (To) then
            U_To := Local_Expr_Value (To) + Val;

            --  Determine if our constant is outside the range of the index.
            --  If so return an Empty node. This empty node will be caught
            --  by Empty_Range below.

            if Compile_Time_Known_Value (Index_Base_L)
              and then U_To < Expr_Value (Index_Base_L)
            then
               return Empty;

            elsif Compile_Time_Known_Value (Index_Base_H)
              and then U_To > Expr_Value (Index_Base_H)
            then
               return Empty;
            end if;

            Expr_Pos := Make_Integer_Literal (Loc, U_To);
            Set_Is_Static_Expression (Expr_Pos);

            if not Is_Enumeration_Type (Index_Base) then
               Expr := Expr_Pos;

            --  If we are dealing with enumeration return
            --     Index_Base'Val (Expr_Pos)

            else
               Expr :=
                 Make_Attribute_Reference
                   (Loc,
                    Prefix         => Index_Base_Name,
                    Attribute_Name => Name_Val,
                    Expressions    => New_List (Expr_Pos));
            end if;

            return Expr;
         end if;

         --  If we are here no constant folding possible

         if not Is_Enumeration_Type (Index_Base) then
            Expr :=
              Make_Op_Add (Loc,
                           Left_Opnd  => Duplicate_Subexpr (To),
                           Right_Opnd => Make_Integer_Literal (Loc, U_Val));

         --  If we are dealing with enumeration return
         --    Index_Base'Val (Index_Base'Pos (To) + Val)

         else
            To_Pos :=
              Make_Attribute_Reference
                (Loc,
                 Prefix         => Index_Base_Name,
                 Attribute_Name => Name_Pos,
                 Expressions    => New_List (Duplicate_Subexpr (To)));

            Expr_Pos :=
              Make_Op_Add (Loc,
                           Left_Opnd  => To_Pos,
                           Right_Opnd => Make_Integer_Literal (Loc, U_Val));

            Expr :=
              Make_Attribute_Reference
                (Loc,
                 Prefix         => Index_Base_Name,
                 Attribute_Name => Name_Val,
                 Expressions    => New_List (Expr_Pos));
         end if;

         return Expr;
      end Add;

      -----------------
      -- Empty_Range --
      -----------------

      function Empty_Range (L, H : Node_Id) return Boolean is
         Is_Empty : Boolean := False;
         Low      : Node_Id;
         High     : Node_Id;

      begin
         --  First check if L or H were already detected as overflowing the
         --  index base range type by function Add above. If this is so Add
         --  returns the empty node.

         if No (L) or else No (H) then
            return True;
         end if;

         for J in 1 .. 3 loop
            case J is

               --  L > H    range is empty

               when 1 =>
                  Low  := L;
                  High := H;

               --  B_L > H  range must be empty

               when 2 =>
                  Low  := Index_Base_L;
                  High := H;

               --  L > B_H  range must be empty

               when 3 =>
                  Low  := L;
                  High := Index_Base_H;
            end case;

            if Local_Compile_Time_Known_Value (Low)
              and then Local_Compile_Time_Known_Value (High)
            then
               Is_Empty :=
                 UI_Gt (Local_Expr_Value (Low), Local_Expr_Value (High));
            end if;

            exit when Is_Empty;
         end loop;

         return Is_Empty;
      end Empty_Range;

      -----------
      -- Equal --
      -----------

      function Equal (L, H : Node_Id) return Boolean is
      begin
         if L = H then
            return True;

         elsif Local_Compile_Time_Known_Value (L)
           and then Local_Compile_Time_Known_Value (H)
         then
            return UI_Eq (Local_Expr_Value (L), Local_Expr_Value (H));
         end if;

         return False;
      end Equal;

      ---------------------
      -- Index_Base_Name --
      ---------------------

      function Index_Base_Name return Node_Id is
      begin
         return New_Reference_To (Index_Base, Sloc (N));
      end Index_Base_Name;

      ----------------
      -- Gen_Assign --
      ----------------

      function Gen_Assign (Ind : Node_Id; Expr : Node_Id) return List_Id is
         A : Node_Id;

         New_Indices  : List_Id;
         Indexed_Comp : Node_Id;

      begin
         if No (Indices) then
            New_Indices := New_List;
         else
            New_Indices := New_List_Copy_Tree (Indices);
         end if;

         Append_To (New_Indices, Ind);

         if Present (Next_Index (Index)) then
            return
              Build_Array_Aggr_Code
                (Expr, Next_Index (Index), Into, Scalar_Comp, New_Indices);
         end if;

         --  If we get here then we are at a bottom-level (sub-)aggregate

         Indexed_Comp :=  Checks_Off (
             Make_Indexed_Component (Loc,
               Prefix      => New_Copy_Tree (Into),
               Expressions => New_Indices));

         Set_Assignment_OK (Indexed_Comp);

         if (Nkind (Expr) = N_Aggregate
              or else Nkind (Expr) = N_Extension_Aggregate)
            and then Expansion_Delayed (Expr)
         then
            return Late_Expansion (Expr, Etype (Expr), Indexed_Comp, Flist);
         else
            A :=
              Make_Assignment_Statement (Loc,
                Name       => Indexed_Comp,
                Expression => New_Copy_Tree (Expr));

            Set_Assignment_OK (Name (A));
            Set_No_Ctrl_Actions (A);
            return New_List (A);
         end if;
      end Gen_Assign;

      --------------
      -- Gen_Loop --
      --------------

      function Gen_Loop (L, H : Node_Id; Expr : Node_Id) return List_Id is
         L_I : Node_Id;

         L_Range : Node_Id;
         --  Index_Base'(L) .. Index_Base'(H)

         L_Iteration_Scheme : Node_Id;
         --  L_I in Index_Base'(L) .. Index_Base'(H)

         L_Body : List_Id;
         --  The statements to execute in the loop

         S : List_Id := New_List;
         --  list of statement

      begin
         --  If loop bounds define an empty range return the null statement

         if Empty_Range (L, H) then
            Append_To (S, Make_Null_Statement (Loc));
            return S;

         --  If loop bounds are the same then generate an assignment

         elsif Equal (L, H) then
            return Gen_Assign (New_Copy_Tree (L), Expr);

         --  If H - L <= 2 then generate a sequence of assignments
         --  when we are processing the bottom most aggregate and it contains
         --  scalar components.

         elsif No (Next_Index (Index))
           and then Scalar_Comp
           and then Local_Compile_Time_Known_Value (L)
           and then Local_Compile_Time_Known_Value (H)
           and then Local_Expr_Value (H) - Local_Expr_Value (L) <= 2
         then
            Append_List_To (S, Gen_Assign (New_Copy_Tree (L), Expr));
            Append_List_To (S, Gen_Assign (Add (1, To => L), Expr));

            if Local_Expr_Value (H) - Local_Expr_Value (L) = 2 then
               Append_List_To (S, Gen_Assign (Add (2, To => L), Expr));
            end if;

            return S;
         end if;

         --  Otherwise construct the loop, starting with the loop index L_I

         L_I := Make_Defining_Identifier (Loc, New_Internal_Name ('I'));

         --  contruct "L .. H"

         L_Range :=
           Make_Range
             (Loc,
              Low_Bound  => Make_Qualified_Expression
                              (Loc,
                               Subtype_Mark => Index_Base_Name,
                               Expression   => L),
              High_Bound => Make_Qualified_Expression
                              (Loc,
                               Subtype_Mark => Index_Base_Name,
                               Expression => H));

         --  construct "for L_I in Index_Base range in  L .. H"

         L_Iteration_Scheme :=
           Make_Iteration_Scheme
             (Loc,
              Loop_Parameter_Specification =>
                Make_Loop_Parameter_Specification
                  (Loc,
                   Defining_Identifier         => L_I,
                   Discrete_Subtype_Definition => L_Range));

         --  Construct the statements to execute in the loop body

         L_Body := Gen_Assign (New_Reference_To (L_I, Loc), Expr);

         --  construct the final loop

         Append_To (S, Make_Loop_Statement
                         (Loc,
                          Identifier       => Empty,
                          Iteration_Scheme => L_Iteration_Scheme,
                          Statements       => L_Body));

         return S;
      end Gen_Loop;

      ---------------
      -- Gen_While --
      ---------------

      --  The code built is
      --     W_I : Index_Base := L;
      --     while W_I < H loop
      --        W_I := Index_Base'Succ (W);
      --        L_Body;
      --     end loop;

      function Gen_While (L, H : Node_Id; Expr : Node_Id) return List_Id is

         W_I : Node_Id;

         W_Decl : Node_Id;
         --  W_I : Base_Type := L;

         W_Iteration_Scheme : Node_Id;
         --  while W_I < H

         W_Index_Succ : Node_Id;
         --  Index_Base'Succ (I)

         W_Increment  : Node_Id;
         --  W_I := Index_Base'Succ (W)

         W_Body : List_Id := New_List;
         --  The statements to execute in the loop

         S : List_Id := New_List;
         --  list of statement

      begin
         --  If loop bounds define an empty range or are equal return null

         if Empty_Range (L, H) or else Equal (L, H) then
            Append_To (S, Make_Null_Statement (Loc));
            return S;
         end if;

         --  Build the decl of W_I

         W_I    := Make_Defining_Identifier (Loc, New_Internal_Name ('I'));
         W_Decl :=
           Make_Object_Declaration
             (Loc,
              Defining_Identifier => W_I,
              Object_Definition   => Index_Base_Name,
              Expression          => L);

         --  Theoretically we should do a New_Copy_Tree (L) here, but we know
         --  that in this particular case L is a fresh Expr generated by
         --  Add which we are the only ones to use.

         Append_To (S, W_Decl);

         --  construct " while W_I < H"

         W_Iteration_Scheme :=
           Make_Iteration_Scheme
             (Loc,
              Condition => Make_Op_Lt
                             (Loc,
                              Left_Opnd  => New_Reference_To (W_I, Loc),
                              Right_Opnd => New_Copy_Tree (H)));

         --  Construct the statements to execute in the loop body

         W_Index_Succ :=
           Make_Attribute_Reference
             (Loc,
              Prefix         => Index_Base_Name,
              Attribute_Name => Name_Succ,
              Expressions    => New_List (New_Reference_To (W_I, Loc)));

         W_Increment  :=
           Make_Assignment_Statement
             (Loc,
              Name       => New_Reference_To (W_I, Loc),
              Expression => W_Index_Succ);

         Append_To (W_Body, W_Increment);
         Append_List_To (W_Body,
           Gen_Assign (New_Reference_To (W_I, Loc), Expr));

         --  Construct the final loop

         Append_To (S, Make_Loop_Statement
                         (Loc,
                          Identifier       => Empty,
                          Iteration_Scheme => W_Iteration_Scheme,
                          Statements       => W_Body));

         return S;
      end Gen_While;

      ------------------------------------
      -- Local_Compile_Time_Known_Value --
      ------------------------------------

      function Local_Compile_Time_Known_Value (E : Node_Id) return Boolean is
      begin
         return Compile_Time_Known_Value (E)
           or else
             (Nkind (E) = N_Attribute_Reference
              and then Attribute_Name (E) = Name_Val
              and then Compile_Time_Known_Value (First (Expressions (E))));
      end Local_Compile_Time_Known_Value;

      ----------------------
      -- Local_Expr_Value --
      ----------------------

      function Local_Expr_Value (E : Node_Id) return Uint is
      begin
         if Compile_Time_Known_Value (E) then
            return Expr_Value (E);
         else
            return Expr_Value (First (Expressions (E)));
         end if;
      end Local_Expr_Value;

      --  Build_Array_Aggr_Code Variables

      Assoc  : Node_Id;
      Choice : Node_Id;
      Expr   : Node_Id;

      Others_Expr : Node_Id   := Empty;

      Aggr_L : constant Node_Id := Low_Bound (Aggregate_Bounds (N));
      Aggr_H : constant Node_Id := High_Bound (Aggregate_Bounds (N));
      --  The aggregate bounds of this specific sub-aggregate. Note that if
      --  the code generated by Build_Array_Aggr_Code is executed then these
      --  bounds are OK. Otherwise a Constraint_Error would have been raised.

      Aggr_Low  : constant Node_Id := Duplicate_Subexpr (Aggr_L);
      Aggr_High : constant Node_Id := Duplicate_Subexpr (Aggr_H);
      --  After Duplicate_Subexpr these are side-effect free.

      Low  : Node_Id;
      High : Node_Id;

      Nb_Choices : Nat := 0;
      Table      : Case_Table_Type (1 .. Number_Of_Choices (N));
      --  Used to sort all the different choice values

      Nb_Elements : Int;
      --  Number of elements in the positional aggegate

      New_Code : List_Id := New_List;

   --  Start of processing for Build_Array_Aggr_Code

   begin
      --  STEP 1: Process component associations

      if No (Expressions (N)) then

         --  STEP 1 (a): Sort the discrete choices

         Assoc := First (Component_Associations (N));
         while Present (Assoc) loop

            Choice := First (Choices (Assoc));
            while Present (Choice) loop

               if Nkind (Choice) = N_Others_Choice then
                  Others_Expr := Expression (Assoc);
                  exit;
               end if;

               Get_Index_Bounds (Choice, Low, High);

               Nb_Choices := Nb_Choices + 1;
               Table (Nb_Choices) := (Choice_Lo   => Low,
                                      Choice_Hi   => High,
                                      Choice_Node => Expression (Assoc));

               Choice := Next (Choice);
            end loop;

            Assoc := Next (Assoc);
         end loop;

         --  If there is more than one set of choices these must be static
         --  and we can therefore sort them. Remeber that Nb_Choices does not
         --  account for an others choice.

         if Nb_Choices > 1 then
            Sort_Case_Table (Table);
         end if;

         --  STEP 1 (b):  take care of the whole set of discrete choices.

         for J in 1 .. Nb_Choices loop
            Low  := Table (J).Choice_Lo;
            High := Table (J).Choice_Hi;
            Expr := Table (J).Choice_Node;

            Append_List (Gen_Loop (Low, High, Expr), To => New_Code);
         end loop;

         --  STEP 1 (c): generate the remaning loops to cover others choice

         if Present (Others_Expr) then
            for J in 0 .. Nb_Choices loop

               if J = 0 then
                  Low := Aggr_Low;
               else
                  Low := Add (1, To => Table (J).Choice_Hi);
               end if;

               if J = Nb_Choices then
                  High := Aggr_High;
               else
                  High := Add (-1, To => Table (J + 1).Choice_Lo);
               end if;

               Append_List (Gen_Loop (Low, High, Others_Expr), To => New_Code);
            end loop;
         end if;

      --  STEP 2: Process positional components

      else
         --  STEP 2 (a): Generate the assignments for each positional element
         --  Note that here we have to use Aggr_L rather than Aggr_Low because
         --  Aggr_L is analyzed and Add wants an analyzed expression.

         Expr        := First (Expressions (N));
         Nb_Elements := -1;

         while Present (Expr) loop
            Nb_Elements := Nb_Elements + 1;
            Append_List (Gen_Assign (Add (Nb_Elements, To => Aggr_L), Expr),
                         To => New_Code);
            Expr := Next (Expr);
         end loop;

         --  STEP 2 (b): Generate final loop if an others choice is present
         --  Here Nb_Elements gives the offset of the last positional element.

         if Present (Component_Associations (N)) then
            Assoc := Last (Component_Associations (N));
            Expr  := Expression (Assoc);

            Append_List (Gen_While (Add (Nb_Elements, To => Aggr_L),
                                    Aggr_High,
                                    Expr),
                         To => New_Code);
         end if;
      end if;

      return New_Code;
   end Build_Array_Aggr_Code;

   ----------------------------
   -- Build_Record_Aggr_Code --
   ----------------------------

   function Build_Record_Aggr_Code
     (N           : Node_Id;
      Typ         : Entity_Id;
      Target      : Node_Id;
      Flist       : Node_Id   := Empty)
      return   List_Id
   is
      Loc   : constant Source_Ptr := Sloc (N);
      L     : constant List_Id    := New_List;
      N_Typ : constant Entity_Id  := Etype (N);

      Comp      : Node_Id;
      Instr     : Node_Id;
      Ref       : Node_Id;
      Comp_Type : Entity_Id;
      Selector  : Entity_Id;
      Comp_Expr : Node_Id;
      Comp_Kind : Node_Kind;

      Internal_Final_List : Node_Id := Empty;
      External_Final_List : Node_Id := Flist;
      Flist_Ref           : Node_Id;

      function Get_Constraint_Association (T : Entity_Id) return Node_Id;
      --  Returns the first discriminant association in the constraint
      --  associated with T, if any, otherwise returns Empty.

      function Ancestor_Discriminant_Value (Disc : Entity_Id) return Node_Id;
      --  Returns the value that the given discriminant of an ancestor
      --  type should receive (in the absence of a conflict with the
      --  value provided by an ancestor part of an extension aggregate).

      procedure Check_Ancestor_Discriminants (Anc_Typ : Entity_Id);
      --  Check that each of the discriminant values defined by the
      --  ancestor part of an extension aggregate match the corresponding
      --  values provided by either an association of the aggregate or
      --  by the constraint imposed by a parent type (RM95-4.3.2(8)).

      function Get_Constraint_Association (T : Entity_Id) return Node_Id is
         Typ_Def : constant Node_Id := Type_Definition (Parent (T));
         Indic   : constant Node_Id := Subtype_Indication (Typ_Def);

      begin
         --  ??? Also need to cover case of a type mark denoting a subtype
         --  with constraint.

         if Nkind (Indic) = N_Subtype_Indication
           and then Present (Constraint (Indic))
         then
            return First (Constraints (Constraint (Indic)));
         end if;

         return Empty;
      end Get_Constraint_Association;

      function Ancestor_Discriminant_Value (Disc : Entity_Id) return Node_Id is
         Assoc        : Node_Id;
         Assoc_Elmt   : Elmt_Id;
         Aggr_Comp    : Entity_Id;
         Corresp_Disc : Entity_Id;
         Current_Typ  : Entity_Id := Base_Type (Typ);
         Parent_Typ   : Entity_Id;
         Parent_Disc  : Entity_Id;
         Save_Assoc   : Node_Id := Empty;

      begin
         --  First check any discriminant associations to see if
         --  any of them provide a value for the discriminant.

         if Present (Discriminant_Specifications (Parent (Current_Typ))) then
            Assoc := First (Component_Associations (N));
            while Present (Assoc) loop
               Aggr_Comp := Entity (First (Choices (Assoc)));

               if Ekind (Aggr_Comp) = E_Discriminant then
                  Save_Assoc := Expression (Assoc);

                  Corresp_Disc := Corresponding_Discriminant (Aggr_Comp);
                  while Present (Corresp_Disc) loop
                     --  If found a corresponding discriminant then return
                     --  the value given in the aggregate. (Note: this is
                     --  not correct in the presence of side effects. ???)

                     if Disc = Corresp_Disc then
                        return Duplicate_Subexpr (Expression (Assoc));
                     end if;
                     Corresp_Disc :=
                       Corresponding_Discriminant (Corresp_Disc);
                  end loop;
               end if;

               Assoc := Next (Assoc);
            end loop;
         end if;

         --  No match found in aggregate, so chain up parent types to find
         --  a constraint that defines the value of the discriminant.

         Parent_Typ := Etype (Current_Typ);
         while Current_Typ /= Parent_Typ loop
            if Has_Discriminants (Parent_Typ) then
               Parent_Disc := First_Discriminant (Parent_Typ);

               --  We either get the association from the subtype indication
               --  of the type definition itself, or from the discriminant
               --  constraint associated with the type entity (which is
               --  preferable, but it's not always present ???)

               if Is_Empty_Elmt_List (
                 Discriminant_Constraint (Current_Typ))
               then
                  Assoc := Get_Constraint_Association (Current_Typ);
                  Assoc_Elmt := No_Elmt;
               else
                  Assoc_Elmt :=
                    First_Elmt (Discriminant_Constraint (Current_Typ));
                  Assoc := Node (Assoc_Elmt);
               end if;

               --  Traverse the discriminants of the parent type looking
               --  for one that corresponds.

               while Present (Parent_Disc) and then Present (Assoc) loop
                  Corresp_Disc := Parent_Disc;
                  while Present (Corresp_Disc)
                    and then Disc /= Corresp_Disc
                  loop
                     Corresp_Disc :=
                       Corresponding_Discriminant (Corresp_Disc);
                  end loop;

                  if Disc = Corresp_Disc then
                     if Nkind (Assoc) = N_Discriminant_Association then
                        Assoc := Expression (Assoc);
                     end if;

                     --  If the located association directly denotes
                     --  a discriminant, then use the value of a saved
                     --  association of the aggregate. This is a kludge
                     --  to handle certain cases involving multiple
                     --  discriminants mapped to a single discriminant
                     --  of a descendant. It's not clear how to locate the
                     --  appropriate discriminant value for such cases. ???

                     if Is_Entity_Name (Assoc)
                       and then Ekind (Entity (Assoc)) = E_Discriminant
                     then
                        Assoc := Save_Assoc;
                     end if;

                     return Duplicate_Subexpr (Assoc);
                  end if;

                  Parent_Disc := Next_Discriminant (Parent_Disc);

                  if No (Assoc_Elmt) then
                     Assoc := Next (Assoc);
                  else
                     Assoc_Elmt := Next_Elmt (Assoc_Elmt);
                     if Present (Assoc_Elmt) then
                        Assoc := Node (Assoc_Elmt);
                     else
                        Assoc := Empty;
                     end if;
                  end if;
               end loop;
            end if;

            Current_Typ := Parent_Typ;
            Parent_Typ := Etype (Current_Typ);
         end loop;

         --  In some cases there's no ancestor value to locate (such as
         --  when an ancestor part given by an expression defines the
         --  discriminant value).

         return Empty;
      end Ancestor_Discriminant_Value;

      procedure Check_Ancestor_Discriminants (Anc_Typ : Entity_Id) is
         Discr      : Entity_Id := First_Discriminant (Base_Type (Anc_Typ));
         Disc_Value : Node_Id;
         Cond       : Node_Id;

      begin
         while Present (Discr) loop
            Disc_Value := Ancestor_Discriminant_Value (Discr);

            if Present (Disc_Value) then
               Cond := Make_Op_Ne (Loc,
                 Left_Opnd =>
                   Make_Selected_Component (Loc,
                     Prefix        => New_Copy_Tree (Target),
                     Selector_Name => New_Occurrence_Of (Discr, Loc)),
                 Right_Opnd => Disc_Value);

               Append_To (L, Make_Raise_Constraint_Error (Loc,
                                                          Condition => Cond));
            end if;

            Discr := Next_Discriminant (Discr);
         end loop;
      end Check_Ancestor_Discriminants;

   --  Start of processing for Build_Record_Aggr_Code

   begin
      --  Determine the finalization lists. The external is either the
      --  finalization list of the outer-scope or the one coming from an
      --  outer aggregate. The internal one is the one on which the
      --  controlled components are hooked.

      if Controlled_Type (Typ) and then No (Flist) then
            External_Final_List := Find_Final_List (Current_Scope);
      end if;

      if Has_Controlled_Component (Typ) then

         Internal_Final_List :=
           Find_Final_List (Current_Scope,
              Make_Selected_Component (Loc,
                 Prefix        => New_Copy_Tree (Target),
                 Selector_Name => Make_Identifier (Loc, Name_uController)));

         --  Attach the temp controller to the final list before anything
         --  else in presence of controlled components and init the record
         --  controller:

         --     _init_proc (tmp._controller);
         --     initialize (tmp._controller);
         --     Attach_to_Final_List (tmp._controller, F);

         Ref := Make_Selected_Component (Loc,
                  Prefix        => Target,
                  Selector_Name => Make_Identifier (Loc, Name_uController));
         Set_Assignment_OK (Ref);

         Append_List_To (L,
           Build_Initialization_Call (Loc,
             Id_Ref       => Ref,
             Typ          => RTE (RE_Record_Controller),
             In_Init_Proc => Chars (Current_Scope) = Name_uInit_Proc));

         Append_To (L,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (
               Find_Prim_Op (RTE (RE_Record_Controller), Name_Initialize),
               Loc),
             Parameter_Associations => New_List (New_Copy_Tree (Ref))));

         Append_To (L,
           Make_Attach_Call (
             Obj_Ref     => New_Copy_Tree (Ref),
             Flist_Ref   => External_Final_List,
             With_Attach => Make_Integer_Literal (Loc, Uint_1)));
      end if;

      --  Deal with the ancestor part of extension aggregates
      --  or with the discriminants of the root type

      if Nkind (N) = N_Extension_Aggregate then
         declare
            A : constant Node_Id := Ancestor_Part (N);
            Init_Typ : Entity_Id;

         begin

            --  If the ancestor part is a subtype mark "T", we generate
            --     _init_proc (T(tmp));  if T is constrained and
            --     _init_proc (S(tmp));  where S applies an appropriate
            --                           constraint if T is unconstrained

            if Is_Entity_Name (A) and then Is_Type (Entity (A)) then
               if Is_Constrained (Entity (A)) then
                  Init_Typ := Entity (A);

               --  For an ancestor part given by an unconstrained type
               --  mark, create a subtype constrained by appropriate
               --  corresponding discriminant values coming from either
               --  associations of the aggregate or a constraint on
               --  a parent type. The subtype will be used to generate
               --  the correct default value for the ancestor part.

               elsif Has_Discriminants (Entity (A)) then
                  declare
                     Anc_Typ    : Entity_Id := Entity (A);
                     Discrim    : Entity_Id := First_Discriminant (Anc_Typ);
                     Anc_Constr : List_Id := New_List;
                     Disc_Value : Node_Id;
                     New_Indic  : Node_Id;
                     Subt_Decl  : Node_Id;
                  begin
                     while Present (Discrim) loop
                        Disc_Value := Ancestor_Discriminant_Value (Discrim);
                        Append_To (Anc_Constr, Disc_Value);
                        Discrim := Next_Discriminant (Discrim);
                     end loop;

                     New_Indic :=
                       Make_Subtype_Indication (Loc,
                         Subtype_Mark => New_Occurrence_Of (Anc_Typ, Loc),
                         Constraint   =>
                           Make_Index_Or_Discriminant_Constraint (Loc,
                             Constraints => Anc_Constr));

                     Init_Typ := Create_Itype (Ekind (Anc_Typ), N);

                     Subt_Decl :=
                       Make_Subtype_Declaration (Loc,
                         Defining_Identifier => Init_Typ,
                         Subtype_Indication  => New_Indic);

                     --  Itypes must be analyzed with checks off

                     Analyze (Subt_Decl, Suppress => All_Checks);
                  end;
               end if;

               Ref := Convert_To (Init_Typ, New_Copy_Tree (Target));
               Set_Assignment_OK (Ref);

               Append_List_To (L,
                 Build_Initialization_Call (Loc,
                   Id_Ref => Ref,
                   Typ    => Init_Typ,
                   In_Init_Proc => Chars (Current_Scope) = Name_uInit_Proc));

               if Is_Constrained (Entity (A))
                 and then Has_Discriminants (Entity (A))
               then
                  Check_Ancestor_Discriminants (Entity (A));
               end if;

            --  If the ancestor part is an expression "E", we generate
            --     T(tmp) := E;

            else
               Ref := Convert_To (Etype (A), New_Copy_Tree (Target));
               Set_Assignment_OK (Ref);
               Append_To (L,
                 Make_Unsuppress_Block (Loc,
                   Name_Discriminant_Check,
                   New_List (
                     Make_Assignment_Statement (Loc,
                       Name       => Ref,
                       Expression => A))));

               if Has_Discriminants (Etype (A)) then
                  Check_Ancestor_Discriminants (Etype (A));
               end if;
            end if;
         end;

      else
         --  Generate the discriminant expressions, component by component

         if Has_Discriminants (Typ) then

            --  ??? The discriminants of the object not inherited in the type
            --  of the object should be initialized here

            null;

            --  Generate discriminant init values

            declare
               Discriminant : Entity_Id;
               Discriminant_Value : Node_Id;

            begin
               Discriminant := First_Girder_Discriminant (Typ);

               while Present (Discriminant) loop

                  Comp_Expr :=
                    Make_Selected_Component (Loc,
                      Prefix        => New_Copy_Tree (Target),
                      Selector_Name => New_Occurrence_Of (Discriminant, Loc));

                  Discriminant_Value :=
                    Get_Discriminant_Value (
                      Discriminant,
                      N_Typ,
                      Discriminant_Constraint (N_Typ));

                  Instr :=
                    Make_Assignment_Statement (Loc,
                      Name       => Comp_Expr,
                      Expression => New_Copy_Tree (Discriminant_Value));

                  Set_Assignment_OK (Name (Instr));
                  Set_No_Ctrl_Actions (Instr);
                  Append_To (L, Instr);

                  Discriminant := Next_Girder_Discriminant (Discriminant);
               end loop;
            end;
         end if;
      end if;

      --  Generate the assignments, component by component

      --    tmp.comp1 := Expr1_From_Aggr;
      --    tmp.comp2 := Expr2_From_Aggr;
      --    ....

      Comp := First (Component_Associations (N));
      while Present (Comp) loop
         Selector  := Entity (First (Choices (Comp)));

         if Ekind (Selector) /= E_Discriminant
           or else Nkind (N) = N_Extension_Aggregate
         then
            Comp_Type := Etype (Selector);
            Comp_Kind := Nkind (Expression (Comp));
            Comp_Expr :=
              Make_Selected_Component (Loc,
                Prefix        => New_Copy_Tree (Target),
                Selector_Name => New_Occurrence_Of (Selector, Loc));

            if (Comp_Kind = N_Aggregate
                 or else Comp_Kind = N_Extension_Aggregate)
               and then Expansion_Delayed (Expression (Comp))
            then
               Append_List_To (L,
                 Late_Expansion (Expression (Comp), Comp_Type, Comp_Expr,
                   Internal_Final_List));

            else
               Instr :=
                 Make_Assignment_Statement (Loc,
                   Name       => Comp_Expr,
                   Expression => Expression (Comp));

               Set_Assignment_OK (Name (Instr));
               Set_No_Ctrl_Actions (Instr);
               Append_To (L, Instr);

               --  Adjust the tag if tagged (because of possible
               --  view conversions)
               --    tmp.comp._tag := comp_typ'tag;

               if Is_Tagged_Type (Comp_Type) then
                  Instr :=
                    Make_Assignment_Statement (Loc,
                      Name =>
                        Make_Selected_Component (Loc,
                          Prefix =>  New_Copy_Tree (Comp_Expr),
                          Selector_Name =>
                            New_Reference_To (Tag_Component (Comp_Type), Loc)),

                      Expression =>
                        Unchecked_Convert_To (RTE (RE_Tag),
                          New_Reference_To (
                            Access_Disp_Table (Comp_Type), Loc)));

                  Set_Assignment_OK (Name (Instr));
                  Append_To (L, Instr);
               end if;

               --  Adjust and Attach the component to the temporary
               --  rec controller
               --     Adjust (tmp.comp);
               --     Attach_To_Final_List (tmp.comp, tmp._record_controller.f)

               if Controlled_Type (Comp_Type) then
                  Append_List_To (L,
                    Make_Adjust_Call (
                      Ref         => New_Copy_Tree (Comp_Expr),
                      Typ         => Comp_Type,
                      Flist_Ref   => New_Copy_Tree (Internal_Final_List),
                      With_Attach => Make_Integer_Literal (Loc, Uint_1)));
               end if;
            end if;
         end if;

         Comp := Next (Comp);
      end loop;

      --  Attach the temporary to the final list afterward in the Is_Controlled
      --  case or attach it to its own controller if it is both Is_Controlled
      --  and Has_Controlled_Component

      if Is_Controlled (Typ) then
         if Has_Controlled_Component (Typ) then
            Flist_Ref := New_Copy_Tree (Internal_Final_List);
         else
            Flist_Ref := New_Copy_Tree (External_Final_List);
         end if;

         Ref := New_Copy_Tree (Target);
         Set_Assignment_OK (Ref);

         Append_To (L,
           Make_Attach_Call (
             Obj_Ref     => Ref,
             Flist_Ref   => Flist_Ref,
             With_Attach => Make_Integer_Literal (Loc, Uint_1)));

      end if;

      --  If the type is tagged, the tag needs to be initialized

      if Is_Tagged_Type (Typ) then
         Instr :=
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Selected_Component (Loc,
                 Prefix => New_Copy_Tree (Target),
                 Selector_Name =>
                   New_Reference_To (Tag_Component (Base_Type (Typ)), Loc)),

             Expression =>
               Unchecked_Convert_To (RTE (RE_Tag),
                 New_Reference_To (Access_Disp_Table (Base_Type (Typ)), Loc)));

         Set_Assignment_OK (Name (Instr));
         Append_To (L, Instr);
      end if;

      return L;
   end Build_Record_Aggr_Code;

   ---------------------------
   -- Contains_Safe_Scalars --
   ---------------------------

   function Contains_Safe_Scalars (N : Node_Id) return Boolean is
      Aggr_Typ      : constant Entity_Id := Etype (N);
      Component_Typ : constant Entity_Id := Component_Type (Aggr_Typ);

      function Safe_Scalars (N : Node_Id; Dim : Pos) return Boolean;
      --  Does the actual job of Contains_Safe_Scalars.
      --  N is the aggregate to explore and Dim its number of dimensions.

      function Safe_Scalar_Value (E : Node_Id) return Boolean;
      --  Returns true if E is a scalar constant or variable whose evaluation
      --  cannot possibly raise constraint error, taking into effect that
      --  there may be a range check involved, so the bounds of the scalar
      --  type for constants are also checked.

      -----------------------
      -- Safe_Scalar_Value --
      -----------------------

      function Safe_Scalar_Value (E : Node_Id) return Boolean is
         Expr : constant Node_Id := Get_Referenced_Object (E);
         --  Deal properly with renamings

         Typ  : constant Entity_Id := Etype (Expr);
         Low  : Node_Id;
         High : Node_Id;

      begin
         --  Here we know we are dealing with a scalar type because
         --  Contains_Safe_Scalars immediately returns False if the
         --  component type of the top level aggregate is not a scalar.

         if Nkind (Expr) = N_Identifier and then Component_Typ = Typ then
            return True;

         elsif not Compile_Time_Known_Value (Expr) then
            return False;

         else
            Low  := Type_Low_Bound (Typ);
            High := Type_High_Bound (Typ);

            if not Compile_Time_Known_Value (Low)
              or else not Compile_Time_Known_Value (High)
            then
               return False;
            end if;
         end if;

         return True;
      end Safe_Scalar_Value;

      -----------------
      -- Safe_Scalars --
      -----------------

      function Safe_Scalars (N : Node_Id; Dim : Pos) return Boolean is
         Assoc : Node_Id;
         Expr  : Node_Id;

      begin
         --  Process positional components

         if Present (Expressions (N)) then
            Expr := First (Expressions (N));

            while Present (Expr) loop
               if Dim > 1 then
                  if not Safe_Scalars (Expr, Dim - 1) then
                     return False;
                  end if;

               elsif not Safe_Scalar_Value (Expr) then
                  return False;
               end if;

               Expr := Next (Expr);
            end loop;
         end if;

         --  Process component associations

         if Present (Component_Associations (N)) then
            Assoc := First (Component_Associations (N));
            while Present (Assoc) loop
               Expr := Expression (Assoc);

               if Dim > 1 then
                  if  not Safe_Scalars (Expr, Dim - 1) then
                     return False;
                  end if;

               elsif not Safe_Scalar_Value (Expr) then
                  return False;
               end if;

               Assoc := Next (Assoc);
            end loop;
         end if;

         return True;
      end Safe_Scalars;

   --  Start of processing for Contains_Safe_Scalars

   begin
      return Is_Scalar_Type (Component_Typ)
        and then Safe_Scalars (N, Number_Dimensions (Aggr_Typ));
   end Contains_Safe_Scalars;

   ----------------------------
   -- Convert_To_Assignments --
   ----------------------------

   procedure Convert_To_Assignments (N : Node_Id; Typ : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Temp : Entity_Id;

      Instr         : Node_Id;
      Target_Expr   : Node_Id;
      Parent_Kind   : Node_Kind;

   begin
      --  Don't do anything for internal aggregates, they will be converted
      --  on the way back. For object declaration, we do not need a temporary
      --  we can use the object itself (see Expand_N_Object_Declaration)

      Parent_Kind := Nkind (Parent (N));

      if Parent_Kind = N_Aggregate
        or else Parent_Kind = N_Extension_Aggregate
        or else Parent_Kind = N_Component_Association
        or else Parent_Kind = N_Object_Declaration
      then
         Set_Expansion_Delayed (N);
         return;
      end if;

      if Requires_Transient_Scope (Typ) then
         Establish_Transient_Scope
           (N,
            Sec_Stack =>
              Is_Controlled (Typ) or else Has_Controlled_Component (Typ));
      end if;

      --  If we are in a context of an expanded assignment, we certainly do
      --  not need a temporary so we assign in-place component per
      --  component. Since we are deeper in the tree, we cannot get rid of
      --  the original assignment which is transformed for now in a useless
      --  (but harmless) self assignment.

      if Parent_Kind = N_Assignment_Statement
        and then not Comes_From_Source (Parent (N))
      then
         Target_Expr := New_Copy_Tree (Name (Parent (N)));
         Insert_Actions (N, Build_Record_Aggr_Code (N, Typ, Target_Expr));
         Rewrite_Substitute_Tree (N, New_Copy_Tree (Name (Parent (N))));
         Analyze_And_Resolve (N, Typ);

      else
         --  Create the temporary

         Temp := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

         Instr :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Object_Definition => New_Occurrence_Of (Typ, Loc));

         Set_No_Default_Init (Instr);
         Insert_Action (N, Instr);
         Target_Expr := New_Occurrence_Of (Temp, Loc);

         Insert_Actions (N, Build_Record_Aggr_Code (N, Typ, Target_Expr));
         Rewrite_Substitute_Tree (N, New_Occurrence_Of (Temp, Loc));
         Analyze_And_Resolve (N, Typ);
      end if;
   end Convert_To_Assignments;

   ---------------------------------
   -- Convert_Aggr_In_Object_Decl --
   ---------------------------------

   procedure Convert_Aggr_In_Object_Decl (N : Node_Id) is
      Obj  : constant Entity_Id  := Defining_Identifier (N);
      Aggr : constant Node_Id    := Expression (N);
      Loc  : constant Source_Ptr := Sloc (Aggr);
      Typ  : constant Entity_Id  := Etype (Aggr);
      Occ  : constant Node_Id    := New_Occurrence_Of (Obj, Loc);

   begin
      Set_Assignment_OK (Occ);
      Insert_Actions_After (N, Build_Record_Aggr_Code (Aggr, Typ, Occ));
      Set_No_Default_Init (N);
   end Convert_Aggr_In_Object_Decl;

   ----------------------------
   -- Expand_Array_Aggregate --
   ----------------------------

   --  Array aggregate expansion proceeds as follows:

   --  1. If requested we generate code to perform all the array aggregate
   --     bound checks, specifically

   --         (a) Check that the index range defined by aggregate bounds is
   --             compatible with corresponding index subtype.

   --         (b) If an others choice is present check that no aggregate
   --             index is outside the bounds of the index constraint.

   --         (c) For multidimensional arrays make sure that all subaggregates
   --             corresponding to the same dimension have the same bounds.

   --  2. Check if the aggregate can be statically processed. If this is the
   --     case pass it as is to Gigi. Note that a necessary condition for
   --     static processing is that the aggregate be fully positional.

   --  3. If in place aggregate expansion is possible (i.e. no need to create
   --     a temporary) then mark the aggregate as such and return. Otherwise
   --     create a new temporary and generate the appropriate initialization
   --     code.

   procedure Expand_Array_Aggregate (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Typ  : constant Entity_Id := Etype (N);
      Ctyp : constant Entity_Id := Component_Type (Typ);
      --  Typ is the correct constrained array subtype of the aggregate and
      --  Ctyp is the corresponding component type.

      Aggr_Dimension : constant Pos := Number_Dimensions (Typ);
      --  Number of aggregate index dimensions.

      Aggr_Low  : array (1 .. Aggr_Dimension) of Node_Id;
      Aggr_High : array (1 .. Aggr_Dimension) of Node_Id;
      --  Low and High bounds of the constraint for each aggregate index.

      Aggr_Index_Typ : array (1 .. Aggr_Dimension) of Entity_Id;
      --  The type of each index.

      Others_Present : array (1 .. Aggr_Dimension) of Boolean
        := (others => False);
      --  If Others_Present (I) is True, then there is an others choice
      --  in one of the sub-aggregates of N at dimension I.

      procedure Check_Bounds (Aggr_Bounds : Node_Id; Index_Bounds : Node_Id);
      --  Checks that the bounds of Aggr_Bounds are within the bounds defined
      --  by Index_Bounds.

      procedure Check_Same_Aggr_Bounds (Sub_Aggr : Node_Id; Dim : Pos);
      --  Checks that in a multi-dimensional array aggregate all subaggregates
      --  corresponding to the same dimension have the same bounds.
      --  Sub_Aggr is an array sub-aggregate. Dim is the dimension
      --  corresponding to the sub-aggregate.

      procedure Compute_Others_Present (Sub_Aggr : Node_Id; Dim : Pos);
      --  Computes the values of array Others_Present. Sub_Aggr is the
      --  array sub-aggregate we start the computation from. Dim is the
      --  dimension corresponding to the sub-aggregate.

      procedure Others_Check (Sub_Aggr : Node_Id; Dim : Pos);
      --  Checks that if an others choice is present in any sub-aggregate no
      --  aggregate index is outside the bounds of the index constraint.
      --  Sub_Aggr is an array sub-aggregate. Dim is the dimension
      --  corresponding to the sub-aggregate.

      ------------------
      -- Check_Bounds --
      ------------------

      procedure Check_Bounds (Aggr_Bounds : Node_Id; Index_Bounds : Node_Id) is
         Aggr_Lo : Node_Id;
         Aggr_Hi : Node_Id;

         Ind_Lo  : Node_Id;
         Ind_Hi  : Node_Id;

         Cond    : Node_Id := Empty;

      begin
         Get_Index_Bounds (Aggr_Bounds, Aggr_Lo, Aggr_Hi);
         Get_Index_Bounds (Index_Bounds, Ind_Lo, Ind_Hi);

         --  Generate the following test:
         --
         --    [constraint_error when
         --      Aggr_Lo <= Aggr_Hi and then
         --        (Aggr_Lo < Ind_Lo or else Aggr_Hi > Ind_Hi)]
         --
         --  As an optimization try to see if some tests are trivially vacuos
         --  because we are comparing an expression against itself.

         if Aggr_Lo = Ind_Lo and then Aggr_Hi = Ind_Hi then
            Cond := Empty;

         elsif Aggr_Hi = Ind_Hi then
            Cond :=
              Make_Op_Lt (Loc,
                Left_Opnd  => Duplicate_Subexpr (Aggr_Lo),
                Right_Opnd => Duplicate_Subexpr (Ind_Lo));

         elsif Aggr_Lo = Ind_Lo then
            Cond :=
              Make_Op_Gt (Loc,
                Left_Opnd  => Duplicate_Subexpr (Aggr_Hi),
                Right_Opnd => Duplicate_Subexpr (Ind_Hi));

         else
            Cond :=
              Make_Or_Else (Loc,
                Left_Opnd =>
                  Make_Op_Lt (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Aggr_Lo),
                    Right_Opnd => Duplicate_Subexpr (Ind_Lo)),

                Right_Opnd =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Aggr_Hi),
                    Right_Opnd => Duplicate_Subexpr (Ind_Hi)));
         end if;

         if Present (Cond) then
            Cond :=
              Make_And_Then (Loc,
                Left_Opnd =>
                  Make_Op_Le (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Aggr_Lo),
                    Right_Opnd => Duplicate_Subexpr (Aggr_Hi)),

                Right_Opnd => Cond);

            Set_Analyzed (Left_Opnd  (Left_Opnd (Cond)), False);
            Set_Analyzed (Right_Opnd (Left_Opnd (Cond)), False);
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc, Condition => Cond));
         end if;
      end Check_Bounds;

      ----------------------------
      -- Check_Same_Aggr_Bounds --
      ----------------------------

      procedure Check_Same_Aggr_Bounds (Sub_Aggr : Node_Id; Dim : Pos) is
         Sub_Lo : constant Node_Id := Low_Bound (Aggregate_Bounds (Sub_Aggr));
         Sub_Hi : constant Node_Id := High_Bound (Aggregate_Bounds (Sub_Aggr));
         --  The bounds of this specific sub-aggregate.

         Aggr_Lo : constant Node_Id := Aggr_Low (Dim);
         Aggr_Hi : constant Node_Id := Aggr_High (Dim);
         --  The bounds of the aggregate for this dimension

         Ind_Typ : constant Entity_Id := Aggr_Index_Typ (Dim);
         --  The index type for this dimension.

         Cond : Node_Id := Empty;

         Assoc  : Node_Id;
         Expr   : Node_Id;

      begin
         --  If index checks are on generate the test
         --
         --    [constraint_error when
         --      Aggr_Lo /= Sub_Lo or else Aggr_Hi /= Sub_Hi]
         --
         --  As an optimization try to see if some tests are trivially vacuos
         --  because we are comparing an expression against itself. Also for
         --  the first dimension the test is trivially vacuous because there
         --  is just one aggregate for dimension 1.

         if Index_Checks_Suppressed (Ind_Typ) then
            Cond := Empty;

         elsif Dim = 1
           or else (Aggr_Lo = Sub_Lo and then Aggr_Hi = Sub_Hi)
         then
            Cond := Empty;

         elsif Aggr_Hi = Sub_Hi then
            Cond :=
              Make_Op_Ne (Loc,
                Left_Opnd  => Duplicate_Subexpr (Aggr_Lo),
                Right_Opnd => Duplicate_Subexpr (Sub_Lo));

         elsif Aggr_Lo = Sub_Lo then
            Cond :=
              Make_Op_Ne (Loc,
                Left_Opnd  => Duplicate_Subexpr (Aggr_Hi),
                Right_Opnd => Duplicate_Subexpr (Sub_Hi));

         else
            Cond :=
              Make_Or_Else (Loc,
                Left_Opnd =>
                  Make_Op_Ne (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Aggr_Lo),
                    Right_Opnd => Duplicate_Subexpr (Sub_Lo)),

                Right_Opnd =>
                  Make_Op_Ne (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Aggr_Hi),
                    Right_Opnd => Duplicate_Subexpr (Sub_Hi)));
         end if;

         if Present (Cond) then
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc, Condition => Cond));
         end if;

         --  Now look inside the sub-aggregate to see if there is more work

         if Dim < Aggr_Dimension then

            --  Process positional components

            if Present (Expressions (Sub_Aggr)) then
               Expr := First (Expressions (Sub_Aggr));
               while Present (Expr) loop
                  Check_Same_Aggr_Bounds (Expr, Dim + 1);
                  Expr := Next (Expr);
               end loop;
            end if;

            --  Process component associations

            if Present (Component_Associations (Sub_Aggr)) then
               Assoc := First (Component_Associations (Sub_Aggr));
               while Present (Assoc) loop
                  Expr := Expression (Assoc);
                  Check_Same_Aggr_Bounds (Expr, Dim + 1);
                  Assoc := Next (Assoc);
               end loop;
            end if;
         end if;
      end Check_Same_Aggr_Bounds;

      ----------------------------
      -- Compute_Others_Present --
      ----------------------------

      procedure Compute_Others_Present (Sub_Aggr : Node_Id; Dim : Pos) is
         Assoc  : Node_Id;
         Expr   : Node_Id;

      begin
         if Present (Component_Associations (Sub_Aggr)) then
            Assoc := Last (Component_Associations (Sub_Aggr));
            if Nkind (First (Choices (Assoc))) = N_Others_Choice then
               Others_Present (Dim) := True;
            end if;
         end if;

         --  Now look inside the sub-aggregate to see if there is more work

         if Dim < Aggr_Dimension then

            --  Process positional components

            if Present (Expressions (Sub_Aggr)) then
               Expr := First (Expressions (Sub_Aggr));
               while Present (Expr) loop
                  Compute_Others_Present (Expr, Dim + 1);
                  Expr := Next (Expr);
               end loop;
            end if;

            --  Process component associations

            if Present (Component_Associations (Sub_Aggr)) then
               Assoc := First (Component_Associations (Sub_Aggr));
               while Present (Assoc) loop
                  Expr := Expression (Assoc);
                  Compute_Others_Present (Expr, Dim + 1);
                  Assoc := Next (Assoc);
               end loop;
            end if;
         end if;
      end Compute_Others_Present;

      ------------------
      -- Others_Check --
      ------------------

      procedure Others_Check (Sub_Aggr : Node_Id; Dim : Pos) is
         Aggr_Lo : constant Node_Id := Aggr_Low (Dim);
         Aggr_Hi : constant Node_Id := Aggr_High (Dim);
         --  The bounds of the aggregate for this dimension.

         Ind_Typ : constant Entity_Id := Aggr_Index_Typ (Dim);
         --  The index type for this dimension.

         Need_To_Check : Boolean := False;

         Choices_Lo : Node_Id := Empty;
         Choices_Hi : Node_Id := Empty;
         --  The lowest and highest discrete choices for a named sub-aggregate

         Nb_Choices : Int := -1;
         --  The number of discrete non-others choices in this sub-aggregate

         Nb_Elements : Uint := Uint_0;
         --  The number of elements in a positional aggegate

         Cond : Node_Id := Empty;

         Assoc  : Node_Id;
         Choice : Node_Id;
         Expr   : Node_Id;

      begin
         --  Check if we have an others choice. If we do make sure that this
         --  sub-aggregate contains at least one element in addition to the
         --  others choice.

         if Range_Checks_Suppressed (Ind_Typ) then
            Need_To_Check := False;

         elsif Present (Expressions (Sub_Aggr))
           and then Present (Component_Associations (Sub_Aggr))
         then
            Need_To_Check := True;

         elsif Present (Component_Associations (Sub_Aggr)) then
            Assoc := Last (Component_Associations (Sub_Aggr));

            if Nkind (First (Choices (Assoc))) /= N_Others_Choice then
               Need_To_Check := False;

            else
               --  Count the number of discrete choices. Start with -1
               --  because the others choice does not count.

               Nb_Choices := -1;
               Assoc := First (Component_Associations (Sub_Aggr));
               while Present (Assoc) loop
                  Choice := First (Choices (Assoc));
                  while Present (Choice) loop
                     Nb_Choices := Nb_Choices + 1;
                     Choice := Next (Choice);
                  end loop;

                  Assoc := Next (Assoc);
               end loop;

               --  If there is only an others choice nothing to do

               Need_To_Check := (Nb_Choices > 0);
            end if;

         else
            Need_To_Check := False;
         end if;

         --  If we are dealing with a positional sub-aggregate with an
         --  others choice,  compute the number or positional elements.

         if Need_To_Check and then Present (Expressions (Sub_Aggr)) then
            Expr := First (Expressions (Sub_Aggr));
            Nb_Elements := Uint_0;
            while Present (Expr) loop
               Nb_Elements := Nb_Elements + 1;
               Expr := Next (Expr);
            end loop;

         --  If the aggregate contains discrete choices and an others choice
         --  compute the smallest and largest discrete choice values.

         elsif Need_To_Check then
            Compute_Choices_Lo_And_Choices_Hi : declare
               Table : Case_Table_Type (1 .. Nb_Choices);
               --  Used to sort all the different choice values

               I    : Pos := 1;
               Low  : Node_Id;
               High : Node_Id;

            begin
               Assoc := First (Component_Associations (Sub_Aggr));
               while Present (Assoc) loop
                  Choice := First (Choices (Assoc));
                  while Present (Choice) loop
                     if Nkind (Choice) = N_Others_Choice then
                        exit;
                     end if;

                     Get_Index_Bounds (Choice, Low, High);
                     Table (I).Choice_Lo := Low;
                     Table (I).Choice_Hi := High;

                     I := I + 1;
                     Choice := Next (Choice);
                  end loop;

                  Assoc := Next (Assoc);
               end loop;

               --  Sort the discrete choices

               Sort_Case_Table (Table);

               Choices_Lo := Table (1).Choice_Lo;
               Choices_Hi := Table (Nb_Choices).Choice_Hi;
            end Compute_Choices_Lo_And_Choices_Hi;
         end if;

         --  If no others choice in this sub-aggregate, or the aggregate
         --  comprises only an others choice, nothing to do.

         if not Need_To_Check then
            Cond := Empty;

         --  If we are dealing with an aggregate containing an others
         --  choice and positional components, we generate the following test:
         --
         --    if Ind_Typ'Pos (Aggr_Lo) + (Nb_Elements - 1) >
         --            Ind_Typ'Pos (Aggr_Hi)
         --    then
         --       raise Constraint_Error;
         --    end if;

         elsif Nb_Elements > Uint_0 then
            Cond :=
              Make_Op_Gt (Loc,
                Left_Opnd  =>
                  Make_Op_Add (Loc,
                    Left_Opnd  =>
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Reference_To (Ind_Typ, Loc),
                        Attribute_Name => Name_Pos,
                        Expressions    =>
                          New_List (Duplicate_Subexpr (Aggr_Lo))),
                    Right_Opnd => Make_Integer_Literal (Loc, Nb_Elements - 1)),

                Right_Opnd =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Reference_To (Ind_Typ, Loc),
                    Attribute_Name => Name_Pos,
                    Expressions    => New_List (Duplicate_Subexpr (Aggr_Hi))));

         --  If we are dealing with an aggregate containing an others
         --  choice and discrete choices we generate the following test:
         --
         --    [constraint_error when
         --      Choices_Lo < Aggr_Lo or else Choices_Hi > Aggr_Hi];

         else
            Cond :=
              Make_Or_Else (Loc,
                Left_Opnd =>
                  Make_Op_Lt (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Choices_Lo),
                    Right_Opnd => Duplicate_Subexpr (Aggr_Lo)),

                Right_Opnd =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Choices_Hi),
                    Right_Opnd => Duplicate_Subexpr (Aggr_Hi)));
         end if;

         if Present (Cond) then
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc, Condition => Cond));
         end if;

         --  Now look inside the sub-aggregate to see if there is more work

         if Dim < Aggr_Dimension then

            --  Process positional components

            if Present (Expressions (Sub_Aggr)) then
               Expr := First (Expressions (Sub_Aggr));
               while Present (Expr) loop
                  Others_Check (Expr, Dim + 1);
                  Expr := Next (Expr);
               end loop;
            end if;

            --  Process component associations

            if Present (Component_Associations (Sub_Aggr)) then
               Assoc := First (Component_Associations (Sub_Aggr));
               while Present (Assoc) loop
                  Expr := Expression (Assoc);
                  Others_Check (Expr, Dim + 1);
                  Assoc := Next (Assoc);
               end loop;
            end if;
         end if;
      end Others_Check;

      --  Remaining Expand_Array_Aggregate variables

      Tmp : Entity_Id;
      --  Holds the temporary aggregate value.

      Tmp_Decl : Node_Id;
      --  Holds the declaration of Tmp.

      Aggr_Code   : List_Id;
      Parent_Kind : Node_Kind;

   --  Start of processing for Expand_Array_Aggregate

   begin
      --  Do not touch the special aggregates of attributes used for Asm calls

      if Is_RTE (Ctyp, RE_Asm_Input_Operand)
        or else Is_RTE (Ctyp, RE_Asm_Output_Operand)
      then
         return;
      end if;

      --  If during semantic analysis it has been determined that aggregate N
      --  will raise Constraint_Error at run-time, then the aggregate node
      --  has been replaced with an N_Raise_Constraint_Error node and we
      --  should never get here.

      pragma Assert (not Raises_Constraint_Error (N));

      --  STEP 1: Check (a)

      Index_Compatibility_Check : declare
         Aggr_Index_Range : Node_Id := First_Index (Typ);
         --  The current aggregate index range

         Index_Constraint : Node_Id := First_Index (Etype (Typ));
         --  The corresponding index constraint against which we have to
         --  check the above aggregate index range.

      begin
         Compute_Others_Present (N, 1);

         for J in 1 .. Aggr_Dimension loop
            --  There is no need to emit a check if an others choice is
            --  present for this array aggregate dimension since in this
            --  case one of N's sub-aggregates has taken its bounds from the
            --  context and these bounds must have been checked already. In
            --  addition all sub-aggregates corresponding to the same
            --  dimension must all have the same bounds (checked in (c) below).

            if not Range_Checks_Suppressed (Etype (Index_Constraint))
              and then not Others_Present (J)
            then
               --  We don't use Checks.Apply_Range_Check here because it
               --  emits a spurious check. Namely it checks that the range
               --  defined by the aggregate bounds is non empty. But we know
               --  this already if we get here.

               Check_Bounds (Aggr_Index_Range, Index_Constraint);
            end if;

            --  Save the low and high bounds of the aggregate index as well
            --  as the index type for later use in checks (b) and (c) below.

            Aggr_Low  (J) := Low_Bound (Aggr_Index_Range);
            Aggr_High (J) := High_Bound (Aggr_Index_Range);

            Aggr_Index_Typ (J) := Etype (Index_Constraint);

            Aggr_Index_Range := Next_Index (Aggr_Index_Range);
            Index_Constraint := Next_Index (Index_Constraint);
         end loop;
      end Index_Compatibility_Check;

      --  STEP 1: Check (b)

      Others_Check (N, 1);

      --  STEP 1: Check (c)

      if Aggr_Dimension > 1 then
         Check_Same_Aggr_Bounds (N, 1);
      end if;

      --  STEP 2.

      if Static_Processing_Possible (N) then
         return;
      end if;

      --  Delay expansion for nested aggregates it will be taken care of
      --  when the parent aggregate is expanded

      Parent_Kind := Nkind (Parent (N));

      if Parent_Kind = N_Aggregate
        or else Parent_Kind = N_Extension_Aggregate
        or else Parent_Kind = N_Component_Association
      then
         Set_Expansion_Delayed (N);
         return;
      end if;

      --  STEP 3.

      --  Look if in place aggregate expansion is possible

      --  ??? In place aggregate assignment is coming

      --  If we got here then in place aggregate expansion is impossible.
      --  We need to create a temporary.

      --  Create the declaration but don't initialize it by default

      if Requires_Transient_Scope (Typ) then
         Establish_Transient_Scope
           (N, Sec_Stack => Has_Controlled_Component (Typ));
      end if;

      Tmp := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
      Tmp_Decl :=
        Make_Object_Declaration
          (Loc,
           Defining_Identifier => Tmp,
           Object_Definition   => New_Occurrence_Of (Typ, Loc),
           No_Default_Init     => True);

      Insert_Action (N, Tmp_Decl);

      --  Construct and insert the aggregate code. We can safely suppress
      --  index checks because this code is guaranteed not to raise CE
      --  on index checks. However we should *not* suppress all checks.

      Aggr_Code :=
        Build_Array_Aggr_Code (N,
          Index       => First_Index (Typ),
          Into        => New_Reference_To (Tmp, Loc),
          Scalar_Comp => Is_Scalar_Type (Ctyp));
      Insert_Actions (N, Aggr_Code);

      Rewrite_Substitute_Tree (N, New_Occurrence_Of (Tmp, Loc));
      Analyze_And_Resolve (N, Typ);
   end Expand_Array_Aggregate;

   ------------------------
   -- Expand_N_Aggregate --
   ------------------------

   procedure Expand_N_Aggregate (N : Node_Id) is
   begin
      if Is_Record_Type (Etype (N)) then
         Expand_Record_Aggregate (N);
      else
         Expand_Array_Aggregate (N);
      end if;
   end Expand_N_Aggregate;

   ----------------------------------
   -- Expand_N_Extension_Aggregate --
   ----------------------------------

   --  If the ancestor part is an expression, add a component association for
   --  the parent field. If the type of the ancestor part is not the direct
   --  parent of the expected type,  build recursively the needed ancestors.
   --  If the ancestor part is a subtype_mark, replace aggregate with a decla-
   --  ration for a temporary of the expected type, followed by individual
   --  assignments to the given components.

   procedure Expand_N_Extension_Aggregate (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc  (N);
      A   : constant Node_Id    := Ancestor_Part (N);
      Typ : constant Entity_Id  := Etype (N);

   begin
      --  If the ancestor is a subtype mark, an init_proc must be called
      --  on the resulting object which thus has to be materialized in
      --  the front-end

      if Is_Entity_Name (A) and then Is_Type (Entity (A)) then
         Convert_To_Assignments (N, Typ);

      --  The extension aggregate is transformed into a record aggregate
      --  of the following form (c1 and c2 are inherited components)

      --   (Exp with c3 => a, c4 => b)
      --      ==> (c1 => Exp.c1, c2 => Exp.c2, c1 => a, c2 => b)

      else
         Set_Etype (N, Typ);
         Expand_Record_Aggregate (N,
           Orig_Tag    => New_Occurrence_Of (Access_Disp_Table (Typ), Loc),
           Parent_Expr => A);
      end if;
   end Expand_N_Extension_Aggregate;

   -----------------------------
   -- Expand_Record_Aggregate --
   -----------------------------

   procedure Expand_Record_Aggregate
     (N           : Node_Id;
      Orig_Tag    : Node_Id := Empty;
      Parent_Expr : Node_Id := Empty)
   is
      Loc   : constant Source_Ptr   := Sloc  (N);
      Comps : constant List_Id      := Component_Associations (N);
      Typ   : constant Entity_Id    := Etype (N);
      Base_Typ : constant Entity_Id := Base_Type (Typ);

      function Contains_Discr_Records_Or_Array_Components return Boolean;
      --  Returns true if the record aggregate contains array components
      --  or discriminated record.

      function Has_Delayed_Nested_Aggregate return Boolean;
      --  Checks the presence of a nested aggregate which needs Late_Expansion

      ------------------------------------------------
      -- Contains_Discr_Records_Or_Array_Components --
      ------------------------------------------------

      function Contains_Discr_Records_Or_Array_Components return Boolean is
         Comp     : Entity_Id := First_Component (Typ);
         Comp_Typ : Entity_Id;

      begin
         while Present (Comp) loop
            Comp_Typ := Etype (Comp);

            if Is_Array_Type (Comp_Typ)
              or else (Is_Record_Type (Comp_Typ)
                       and then Has_Discriminants (Comp_Typ))
            then
               return True;
            end if;

            Comp := Next_Component (Comp);
         end loop;

         return False;
      end Contains_Discr_Records_Or_Array_Components;

      ----------------------------------
      -- Has_Delayed_Nested_Aggregate --
      ----------------------------------

      function Has_Delayed_Nested_Aggregate return Boolean is
         C : Node_Id;

      begin
         if No (Comps) then
            return False;
         end if;

         C := First (Comps);
         while Present (C) loop
            if (Nkind (Expression (C)) = N_Aggregate
                 or else Nkind (Expression (C)) = N_Extension_Aggregate)
              and then Expansion_Delayed (Expression (C))
            then
               return True;
            end if;

            C := Next (C);
         end loop;

         return False;
      end  Has_Delayed_Nested_Aggregate;

      --  Remaining Expand_Record_Aggregate variables

      Tag_Value : Node_Id;
      Comp      : Entity_Id;
      New_Comp  : Node_Id;

   --  Start of processing for Expand_Record_Aggregate

   begin
      --  Gigi doesn't handle properly temporaries of variable size
      --  so we generate it in the front-end

      if not Size_Known_At_Compile_Time (Typ) then
         Convert_To_Assignments (N, Typ);

      --  Gigi does not emit any discriminant or length checks for record
      --  or array inner components of a record aggregate. To force the
      --  generation of these checks we convert the record aggregate into
      --  a bunch of assignments. Note that if checks are off no need
      --  to perform this conversion.

      --  ??? This introduces redundant checks. Needs to be fixed
      --  These checks have already been generated in sem_aggr by
      --  Constraint_Check

      elsif (not Discriminant_Checks_Suppressed (Typ)
        or else not Length_Checks_Suppressed (Empty))
        and then Contains_Discr_Records_Or_Array_Components
      then
         Convert_To_Assignments (N, Typ);

      --  Temporaries for controlled aggregates need to be attached to a
      --  final chain in order to be properly finalized, so it has to
      --  be created in the front-end

      elsif Is_Controlled (Typ)
        or else Has_Controlled_Component (Base_Type (Typ))
      then
         Convert_To_Assignments (N, Typ);

      elsif Has_Delayed_Nested_Aggregate then
         Convert_To_Assignments (N, Typ);

      --  If an ancestor is private, some components are not inherited and
      --  we cannot expand into a record aggregate

      elsif Has_Private_Ancestor (Typ) then
         Convert_To_Assignments (N, Typ);

      --  ??? The following was done to compile fxacc00.ads in the ACVCs. Gigi
      --  is not able to handle the aggregate for Late_Request.

      elsif Is_Tagged_Type (Typ) and then Has_Discriminants (Typ) then
         Convert_To_Assignments (N, Typ);

      --  In all other cases we generate a proper aggregate that
      --  can be handled by gigi.

      else
         if not Has_Discriminants (Typ) then

            --  This bizzare if/elsif is to avoid a compiler crash ???

            null;

         elsif Is_Derived_Type (Typ) then

            --  Non-girder discriminants are replaced with girder discriminants

            declare
               First_Comp   : Node_Id;
               Discriminant : Entity_Id;

            begin
               --  Remove all the discriminants

               First_Comp := First (Component_Associations (N));

               while Present (First_Comp) loop
                  Comp := First_Comp;
                  First_Comp := Next (First_Comp);

                  if Ekind (Entity (First (Choices (Comp)))) =
                    E_Discriminant
                  then
                     Remove (Comp);
                  end if;
               end loop;

               --  Insert girder discriminant associations
               --  in the correct order

               First_Comp := Empty;
               Discriminant := First_Girder_Discriminant (Typ);
               while Present (Discriminant) loop
                  New_Comp :=
                    Make_Component_Association (Loc,
                      Choices    =>
                        New_List (New_Occurrence_Of (Discriminant, Loc)),

                      Expression =>
                        New_Copy_Tree (
                          Get_Discriminant_Value (
                              Discriminant,
                              Typ,
                              Discriminant_Constraint (Typ))));

                  if No (First_Comp) then
                     Prepend_To (Component_Associations (N), New_Comp);
                  else
                     Insert_After (First_Comp, New_Comp);
                  end if;

                  First_Comp := New_Comp;
                  Discriminant := Next_Girder_Discriminant (Discriminant);
               end loop;
            end;
         end if;

         if Is_Tagged_Type (Typ) then

            --  The tagged case, _parent and _tag component must be created.

            --  Reset null_present unconditionally. tagged records always have
            --  at least one field (the tag or the parent)

            Set_Null_Record_Present (N, False);

            --  When the current aggregate comes from the expansion of an
            --  extension aggregate, the parent expr is replaced by an
            --  aggregate formed by selected components of this expr

            if Present (Parent_Expr)
              and then Is_Empty_List (Comps)
            then
               Comp := First_Entity (Typ);
               while Present (Comp) loop

                  --  Skip all entities that aren't discriminants or components

                  if Ekind (Comp) /= E_Discriminant
                    and then Ekind (Comp) /= E_Component
                  then
                     null;

                  --  Skip all expander-generated components

                  elsif
                    not Comes_From_Source (Original_Record_Component (Comp))
                  then
                     null;

                  else
                     New_Comp :=
                       Make_Selected_Component (Loc,
                         Prefix =>
                           Unchecked_Convert_To (Typ,
                             Duplicate_Subexpr (Parent_Expr, True)),

                         Selector_Name => New_Occurrence_Of (Comp, Loc));

                     Append_To (Comps,
                       Make_Component_Association (Loc,
                         Choices    =>
                           New_List (New_Occurrence_Of (Comp, Loc)),
                         Expression =>
                           New_Comp));

                     Analyze_And_Resolve (New_Comp, Etype (Comp));
                  end if;

                  Comp := Next_Entity (Comp);
               end loop;
            end if;

            --  Compute the value for the Tag now, if the type is a root it
            --  will be included in the aggregate right away, otherwise it will
            --  be propagated to the parent aggregate

            if Present (Orig_Tag) then
               Tag_Value := Orig_Tag;
            else
               Tag_Value := New_Occurrence_Of (Access_Disp_Table (Typ), Loc);
            end if;

            --  For a derived type, an aggregate for the parent is formed with
            --  all the inherited components.

            if Is_Derived_Type (Typ) then

               declare
                  First_Comp   : Node_Id;
                  Parent_Comps : List_Id;
                  Parent_Aggr  : Node_Id;
                  Parent_Name  : Node_Id;

               begin
                  --  Remove the inherited component association from the
                  --  aggregate and store them in the parent aggregate

                  First_Comp := First (Component_Associations (N));
                  Parent_Comps := New_List;

                  while Present (First_Comp)
                    and then Scope (Original_Record_Component (
                            Entity (First (Choices (First_Comp))))) /= Base_Typ
                  loop
                     Comp := First_Comp;
                     First_Comp := Next (First_Comp);
                     Remove (Comp);
                     Append (Comp, Parent_Comps);
                  end loop;

                  Parent_Aggr := Make_Aggregate (Loc,
                    Component_Associations => Parent_Comps);
                  Set_Etype (Parent_Aggr, Etype (Base_Type (Typ)));

                  --  Find the _parent component

                  Comp := First_Component (Typ);
                  while Chars (Comp) /= Name_uParent loop
                     Comp := Next_Component (Comp);
                  end loop;

                  Parent_Name := New_Occurrence_Of (Comp, Loc);

                  --  Insert the parent aggregate

                  Prepend_To (Component_Associations (N),
                    Make_Component_Association (Loc,
                      Choices    => New_List (Parent_Name),
                      Expression => Parent_Aggr));

                  --  Expand recursively the parent propagating the right Tag

                  Expand_Record_Aggregate (
                    Parent_Aggr, Tag_Value, Parent_Expr);
               end;

            --  For a root type, the tag component is added

            else
               declare
                  Tag_Name  : constant Node_Id :=
                                New_Occurrence_Of (Tag_Component (Typ), Loc);
                  Typ_Tag   : constant Entity_Id := RTE (RE_Tag);
                  Conv_Node : constant Node_Id :=
                                Unchecked_Convert_To (Typ_Tag, Tag_Value);

               begin
                  Set_Etype (Conv_Node, Typ_Tag);
                  Prepend_To (Component_Associations (N),
                    Make_Component_Association (Loc,
                      Choices    => New_List (Tag_Name),
                      Expression => Conv_Node));
               end;
            end if;
         end if;
      end if;
   end Expand_Record_Aggregate;

   -----------------------
   -- Number_Of_Choices --
   -----------------------

   function Number_Of_Choices (N : Node_Id) return Nat is
      Assoc  : Node_Id;
      Choice : Node_Id;

      Nb_Choices : Nat := 0;

   begin
      if Present (Expressions (N)) then
         return 0;
      end if;

      Assoc := First (Component_Associations (N));
      while Present (Assoc) loop

         Choice := First (Choices (Assoc));
         while Present (Choice) loop

            if Nkind (Choice) /= N_Others_Choice then
               Nb_Choices := Nb_Choices + 1;
            end if;

            Choice := Next (Choice);
         end loop;

         Assoc := Next (Assoc);
      end loop;

      return Nb_Choices;
   end Number_Of_Choices;

   --------------------------------
   -- Static_Processing_Possible --
   --------------------------------

   --  Static processing by Gigi is possible only if all the following
   --  conditions are met:

   --    1. N is fully positional;

   --    2. No index type is an enumeration with non-standard representation;

   --    3. N is not a packed array aggregates;

   --    4. N's component type is a scalar type;

   --    5. The size of N's array type must be known at compile time.

   --    6. The array type of N does not follow the Fortan layout convention
   --       or if it does it must be 1 dimensional.

   function Static_Processing_Possible (N : Node_Id) return Boolean is
      Typ : constant Entity_Id := Etype (N);
      --  Typ is the correct constrained array subtype of the aggregate.

      function Static_Check (N : Node_Id; Index : Node_Id) return Boolean;
      --  Recursively checks that N is fully positional and that no Index is
      --  an enumeration with non-standard representation. Returns True if
      --  check succeeds.

      ------------------
      -- Static_Check --
      ------------------

      function Static_Check (N : Node_Id; Index : Node_Id) return Boolean is
         Expr      : Node_Id;
         Index_Typ : constant Entity_Id := Etype (Index);

      begin
         --  Don't pass the aggregate to gigi if index subtype is enumeration
         --  type with holes

         if Is_Enumeration_Type (Index_Typ)
           and then Present (Enum_Pos_To_Rep (Base_Type (Index_Typ)))
         then
            return False;

         --  or if it has component associations

         elsif Present (Component_Associations (N)) then
            return False;
         end if;

         --  Recurse

         Expr := First (Expressions (N));

         if Present (Next_Index (Index)) then
            while Present (Expr) loop
               if not Static_Check (Expr, Next_Index (Index)) then
                  return False;
               end if;

               Expr := Next (Expr);
            end loop;
         end if;

         return True;
      end Static_Check;

   --  Start of processing for Static_Processing_Possible

   begin
      --  Checks 3.

      if Is_Packed (Typ) then
         return False;

      --  Checks 6.

      elsif Convention (Typ) = Convention_Fortran
        and then Number_Dimensions (Typ) > 1
      then
         return False;

      --  Checks 4.

      elsif not Is_Scalar_Type (Component_Type (Typ)) then
         return False;

      --  Checks 5.

      elsif not Size_Known_At_Compile_Time (Typ) then
         return False;

      end if;

      --  Checks 1. & 2.

      return Static_Check (N, First_Index (Typ));
   end Static_Processing_Possible;

   --------------------
   -- Late_Expansion --
   --------------------

   function Late_Expansion
     (N      : Node_Id;
      Typ    : Entity_Id;
      Target : Node_Id;
      Flist  : Node_Id := Empty)
      return   List_Id is

   begin
      if Is_Record_Type (Etype (N)) then
         return Build_Record_Aggr_Code (N, Typ, Target, Flist);
      else
         return
           Build_Array_Aggr_Code
             (N,
              First_Index (Typ),
              Target,
              Is_Scalar_Type (Component_Type (Typ)),
              No_List,
              Flist);
      end if;
   end Late_Expansion;

end Exp_Aggr;

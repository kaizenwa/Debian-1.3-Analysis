------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.356 $                            --
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
with Casing;   use Casing;
with Checks;   use Checks;
with Debug;    use Debug;
with Errout;   use Errout;
with Elists;   use Elists;
with Expander; use Expander;
with Exp_Util; use Exp_Util;
with Hostparm; use Hostparm;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Opt;      use Opt;
with Scans;    use Scans;
with Scn;      use Scn;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Prag; use Sem_Prag;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Style;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;

package body Sem_Util is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Build_Component_Subtype
     (C    : List_Id;
      Loc  : Source_Ptr;
      T    : Entity_Id)
      return Node_Id;
   --  This function builds the subtype for Build_Actual_Subtype_Of_Component
   --  and Build_Discriminal_Subtype_Of_Component. C is a list of constraints,
   --  Loc is the source location, T is the original subtype.

   function Denotes_Discriminant (N : Node_Id) return Boolean;
   --  Check whether bound or discriminant constraint is a discriminant.

   -----------------------------------------
   -- Apply_Compile_Time_Constraint_Error --
   -----------------------------------------

   procedure Apply_Compile_Time_Constraint_Error
     (N   : Node_Id;
      Msg : String;
      Ent : Entity_Id  := Empty;
      Loc : Source_Ptr := No_Location)
   is
      Stat : constant Boolean := Is_Static_Expression (N);
      Typ  : Entity_Id;

   begin
      if No (Ent) then
         Typ := Etype (N);
      else
         Typ := Ent;
      end if;

      if not Present (Compile_Time_Constraint_Error (N, Msg, Typ, Loc)) then
         return;
      end if;

      --  Now we replace the node by an N_Raise_Constraint_Error node
      --  This does not need reanalyzing, so set it as analyzed now.

      Rewrite_Substitute_Tree (N, Make_Raise_Constraint_Error (Sloc (N)));
      Set_Analyzed (N, True);
      Set_Etype (N, Typ);
      Set_Raises_Constraint_Error (N);

      --  If the original expression was marked as static, the result is
      --  still marked as static, but the Raises_Constraint_Error flag is
      --  set so that further static evaluation is not attempted.

      if Stat then
         Set_Is_Static_Expression (N);
      end if;

   end Apply_Compile_Time_Constraint_Error;

   --------------------------
   -- Build_Actual_Subtype --
   --------------------------

   function Build_Actual_Subtype
     (T    : Entity_Id;
      N    : Node_Or_Entity_Id)
      return Node_Id
   is
      Obj : Node_Id;

      Loc         : constant Source_Ptr := Sloc (N);
      Constraints : List_Id;
      Decl        : Node_Id;
      Discr       : Entity_Id;
      Hi          : Node_Id;
      Lo          : Node_Id;
      Subt        : Entity_Id;
      Disc_Type   : Entity_Id;

   begin
      if Nkind (N) = N_Defining_Identifier then
         Obj := New_Reference_To (N, Loc);
      else
         Obj := N;
      end if;

      if Is_Array_Type (T) then
         Constraints := New_List;

         for J in 1 .. Number_Dimensions (T) loop

            --  Build an array subtype declaration with the nominal
            --  subtype and the bounds of the actual. Add the declaration
            --  in front of the local declarations for the subprogram,for
            --  analysis before any reference to the formal in the body.

            Lo :=
              Make_Attribute_Reference (Loc,
                Prefix         => Duplicate_Subexpr (Obj, Name_Req => True),
                Attribute_Name => Name_First,
                Expressions    => New_List (
                  Make_Integer_Literal (Loc, UI_From_Int (J))));

            Hi :=
              Make_Attribute_Reference (Loc,
                Prefix         => Duplicate_Subexpr (Obj, Name_Req => True),
                Attribute_Name => Name_Last,
                Expressions    => New_List (
                  Make_Integer_Literal (Loc, UI_From_Int (J))));

            Append (Make_Range (Loc, Lo, Hi), Constraints);
         end loop;

      --  If the type has unknown discriminants there is no constrained
      --  subtype to build.

      elsif Has_Unknown_Discriminants (T) then
         return T;

      else
         Constraints := New_List;

         if Is_Private_Type (T) and then No (Full_View (T)) then
            Disc_Type := Root_Type (Base_Type (T));
         else
            Disc_Type := T;
         end if;

         Discr := First_Discriminant (Disc_Type);

         while Present (Discr) loop
            Append_To (Constraints,
              Make_Selected_Component (Loc,
                Prefix => Duplicate_Subexpr (Obj),
                Selector_Name => New_Occurrence_Of (Discr, Loc)));
            Discr := Next_Discriminant (Discr);
         end loop;
      end if;

      Subt :=
        Make_Defining_Identifier (Loc,
          Chars => New_Internal_Name ('S'));
      Set_Is_Internal (Subt);

      Decl :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (T,  Loc),
              Constraint  =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => Constraints)));

      Mark_Rewrite_Insertion (Decl);
      return Decl;
   end Build_Actual_Subtype;

   ---------------------------------------
   -- Build_Actual_Subtype_Of_Component --
   ---------------------------------------

   function Build_Actual_Subtype_Of_Component
     (T    : Entity_Id;
      N    : Node_Id)
      return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);
      P   : constant Node_Id    := Prefix (N);
      D   : Elmt_Id;
      Id  : Node_Id;

      Deaccessed_T : Entity_Id;
      --  This is either a copy of T, or if T is an access type, then it is
      --  the directly designated type of this access type.

      function Build_Actual_Array_Constraint return List_Id;
      --  If one or more of the bounds of the component depends on
      --  discriminants, build  actual constraint using the discriminants
      --  of the prefix.

      function Build_Actual_Record_Constraint return List_Id;
      --  Similar to previous one, for discriminated components constrained
      --  by the discriminant of the enclosing object.

      -----------------------------------
      -- Build_Actual_Array_Constraint --
      -----------------------------------

      function Build_Actual_Array_Constraint return List_Id is
         Constraints : List_Id := New_List;
         Indx        : Node_Id;
         Hi          : Node_Id;
         Lo          : Node_Id;
         Old_Hi      : Node_Id;
         Old_Lo      : Node_Id;

      begin
         Indx := First_Index (Deaccessed_T);
         while Present (Indx) loop
            Old_Lo := Type_Low_Bound  (Etype (Indx));
            Old_Hi := Type_High_Bound (Etype (Indx));

            if Denotes_Discriminant (Old_Lo) then
               Lo :=
                 Make_Selected_Component (Loc,
                   Prefix => New_Copy_Tree (P),
                   Selector_Name => New_Occurrence_Of (Entity (Old_Lo), Loc));

            else
               Lo := New_Copy_Tree (Old_Lo);
            end if;

            if Denotes_Discriminant (Old_Hi) then
               Hi :=
                 Make_Selected_Component (Loc,
                   Prefix => New_Copy_Tree (P),
                   Selector_Name => New_Occurrence_Of (Entity (Old_Hi), Loc));

            else
               Hi := New_Copy_Tree (Old_Hi);
            end if;

            Append (Make_Range (Loc, Lo, Hi), Constraints);
            Indx := Next_Index (Indx);
         end loop;

         return Constraints;
      end Build_Actual_Array_Constraint;

      ------------------------------------
      -- Build_Actual_Record_Constraint --
      ------------------------------------

      function Build_Actual_Record_Constraint return List_Id is
         Constraints : List_Id := New_List;
         D           : Elmt_Id;
         D_Val       : Node_Id;

      begin
         D := First_Elmt (Discriminant_Constraint (Deaccessed_T));
         while Present (D) loop

            if Denotes_Discriminant (Node (D)) then
               D_Val :=  Make_Selected_Component (Loc,
                 Prefix => New_Copy_Tree (P),
                Selector_Name => New_Occurrence_Of (Entity (Node (D)), Loc));

            else
               D_Val := New_Copy_Tree (Node (D));
            end if;

            Append (D_Val, Constraints);
            D := Next_Elmt (D);
         end loop;

         return Constraints;
      end Build_Actual_Record_Constraint;

   --  Start of processing for Build_Actual_Subtype_Of_Component

   begin
      Remove_Side_Effects (P);

      if Nkind (N) = N_Explicit_Dereference then
         if Is_Composite_Type (T)
           and then not Is_Constrained (T)
           and then not (Is_Class_Wide_Type (T)
                          and then Is_Constrained (Root_Type (T)))
           and then not Has_Unknown_Discriminants (T)
         then
            return Build_Actual_Subtype (T, N);
         else
            return Empty;
         end if;
      end if;

      if Ekind (T) = E_Access_Subtype then
         Deaccessed_T := Designated_Type (T);
      else
         Deaccessed_T := T;
      end if;

      if Ekind (Deaccessed_T) = E_Array_Subtype then

         Id := First_Index (Deaccessed_T);

         while Present (Id) loop

            if Denotes_Discriminant (Type_Low_Bound  (Etype (Id))) or else
               Denotes_Discriminant (Type_High_Bound (Etype (Id)))
            then
               return
                 Build_Component_Subtype (
                   Build_Actual_Array_Constraint, Loc, Base_Type (T));
            end if;

            Id := Next_Index (Id);
         end loop;

      elsif Is_Composite_Type (Deaccessed_T)
        and then Has_Discriminants (Deaccessed_T)
        and then not Has_Unknown_Discriminants (Deaccessed_T)
      then
         D := First_Elmt (Discriminant_Constraint (Deaccessed_T));
         while Present (D) loop

            if Denotes_Discriminant (Node (D)) then
               return
                  Build_Component_Subtype (
                    Build_Actual_Record_Constraint, Loc, Base_Type (T));
            end if;

            D := Next_Elmt (D);
         end loop;
      end if;

      --  If none of the above, the actual and nominal subtypes are the same.

      return Empty;

   end Build_Actual_Subtype_Of_Component;

   -----------------------------
   -- Build_Component_Subtype --
   -----------------------------

   function Build_Component_Subtype
     (C    : List_Id;
      Loc  : Source_Ptr;
      T    : Entity_Id)
      return Node_Id
   is
      Subt : Entity_Id;
      Decl : Node_Id;

   begin
      Subt :=
        Make_Defining_Identifier (Loc,
          Chars => New_Internal_Name ('S'));
      Set_Is_Internal (Subt);

      Decl :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (Base_Type (T),  Loc),
              Constraint  =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => C)));

      Mark_Rewrite_Insertion (Decl);
      return Decl;
   end Build_Component_Subtype;

   --------------------------------------------
   -- Build_Discriminal_Subtype_Of_Component --
   --------------------------------------------

   function Build_Discriminal_Subtype_Of_Component
     (T    : Entity_Id)
      return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (T);
      D   : Elmt_Id;
      Id  : Node_Id;

      function Build_Discriminal_Array_Constraint return List_Id;
      --  If one or more of the bounds of the component depends on
      --  discriminants, build  actual constraint using the discriminants
      --  of the prefix.

      function Build_Discriminal_Record_Constraint return List_Id;
      --  Similar to previous one, for discriminated components constrained
      --  by the discriminant of the enclosing object.

      ----------------------------------------
      -- Build_Discriminal_Array_Constraint --
      ----------------------------------------

      function Build_Discriminal_Array_Constraint return List_Id is
         Constraints : List_Id := New_List;
         Indx        : Node_Id;
         Hi          : Node_Id;
         Lo          : Node_Id;
         Old_Hi      : Node_Id;
         Old_Lo      : Node_Id;

      begin
         Indx := First_Index (T);
         while Present (Indx) loop
            Old_Lo := Type_Low_Bound  (Etype (Indx));
            Old_Hi := Type_High_Bound (Etype (Indx));

            if Denotes_Discriminant (Old_Lo) then
               Lo := New_Occurrence_Of (Discriminal (Entity (Old_Lo)), Loc);

            else
               Lo := New_Copy_Tree (Old_Lo);
            end if;

            if Denotes_Discriminant (Old_Hi) then
               Hi := New_Occurrence_Of (Discriminal (Entity (Old_Hi)), Loc);

            else
               Hi := New_Copy_Tree (Old_Hi);
            end if;

            Append (Make_Range (Loc, Lo, Hi), Constraints);
            Indx := Next_Index (Indx);
         end loop;

         return Constraints;
      end Build_Discriminal_Array_Constraint;

      -----------------------------------------
      -- Build_Discriminal_Record_Constraint --
      -----------------------------------------

      function Build_Discriminal_Record_Constraint return List_Id is
         Constraints     : List_Id := New_List;
         D     : Elmt_Id;
         D_Val : Node_Id;

      begin
         D := First_Elmt (Discriminant_Constraint (T));
         while Present (D) loop

            if Denotes_Discriminant (Node (D)) then
               D_Val :=
                 New_Occurrence_Of (Discriminal (Entity (Node (D))), Loc);

            else
               D_Val := New_Copy_Tree (Node (D));
            end if;

            Append (D_Val, Constraints);
            D := Next_Elmt (D);
         end loop;

         return Constraints;
      end Build_Discriminal_Record_Constraint;

   --  Start of processing for Build_Discriminal_Subtype_Of_Component

   begin
      if Ekind (T) = E_Array_Subtype then

         Id := First_Index (T);

         while Present (Id) loop

            if Denotes_Discriminant (Type_Low_Bound  (Etype (Id))) or else
               Denotes_Discriminant (Type_High_Bound (Etype (Id)))
            then
               return Build_Component_Subtype
                 (Build_Discriminal_Array_Constraint, Loc, T);
            end if;

            Id := Next_Index (Id);
         end loop;

      elsif Ekind (T) = E_Record_Subtype
        and then Has_Discriminants (T)
        and then not Has_Unknown_Discriminants (T)
      then
         D := First_Elmt (Discriminant_Constraint (T));
         while Present (D) loop

            if Denotes_Discriminant (Node (D)) then
               return Build_Component_Subtype
                 (Build_Discriminal_Record_Constraint, Loc, T);
            end if;

            D := Next_Elmt (D);
         end loop;
      end if;

      --  If none of the above, the actual and nominal subtypes are the same.

      return Empty;

   end Build_Discriminal_Subtype_Of_Component;

   --------------------------
   -- Check_Fully_Declared --
   --------------------------

   procedure Check_Fully_Declared (T : Entity_Id; N : Node_Id) is
   begin
      if Ekind (T) = E_Incomplete_Type then
         Error_Msg_NE
           ("premature usage of incomplete}", N, First_Subtype (T));

      elsif Has_Private_Component (T)
        and then not Is_Generic_Type (Root_Type (T))
      then
         Error_Msg_NE
           ("premature usage of incomplete}", N, First_Subtype (T));
      end if;
   end Check_Fully_Declared;

   ---------------------------
   -- Check_Unset_Reference --
   ---------------------------

   procedure Check_Unset_Reference (N : Node_Id) is
   begin
      if Warning_Mode = Suppress then
         return;
      end if;

      case Nkind (N) is

         when N_Identifier | N_Expanded_Name =>
            declare
               E  : constant Entity_Id := Entity (N);

            begin
               if Ekind (E) = E_Variable
                 and then Not_Assigned (E)
                 and then No (Unset_Reference (E))
               then
                  --  Here we have a potential unset reference. But before we
                  --  get worried about it, we have to make sure that the
                  --  entity declaration is in the same procedure as the
                  --  reference, since if they are in separate procedures,
                  --  then we have no idea about sequential execution.

                  --  The tests in the loop below catch all such cases, but
                  --  do allow the reference to appear in a loop, block, or
                  --  package spec that is nested within the declaring scope.
                  --  As always, it is possible to construct cases where the
                  --  warning is wrong, that is why it is a warning!

                  declare
                     SR : Entity_Id;
                     SE : constant Entity_Id := Scope (E);

                  begin
                     SR := Current_Scope;
                     while SR /= SE loop
                        if SR = Standard_Standard
                          or else Is_Subprogram (SR)
                          or else Is_Concurrent_Body (SR)
                          or else Is_Type (SR)
                        then
                           return;
                        end if;

                        SR := Scope (SR);
                     end loop;

                     if Nkind (N) = N_Identifier then
                        Set_Unset_Reference (E, N);
                     else
                        Set_Unset_Reference (E, Selector_Name (N));
                     end if;
                  end;
               end if;
            end;

         when N_Indexed_Component | N_Selected_Component | N_Slice =>
            Check_Unset_Reference (Prefix (N));
            return;

         when N_Type_Conversion | N_Qualified_Expression =>
            Check_Unset_Reference (Expression (N));

         when others =>
            null;

      end case;
   end Check_Unset_Reference;

   ---------------------------
   -- Check_Unset_Variables --
   ---------------------------

   procedure Check_Unset_Variables (E : Entity_Id) is
      E1 : Entity_Id;

   begin
      --  No messages if warnings are suppressed, or if we have detected
      --  any real errors so far (this last check avoids junk messages
      --  resulting from errors, e.g. a subunit that is not loaded).

      if Warning_Mode = Suppress or else Errors_Detected /= 0 then
         return;
      end if;

      --  Otherwise loop through entities, looking for suspicious stuff

      E1 := First_Entity (E);
      while Present (E1) loop

         if Comes_From_Source (E1)
            and then not Warnings_Off (E1)
            and then (Ekind (E1) = E_Variable
                        or else
                     (Ekind (E1) = E_Out_Parameter
                       and then not Is_Protected_Type (Current_Scope)))
         then
            if Not_Assigned (E1) then
               Error_Msg_N ("& is never assigned a value?", E1);

            elsif Present (Unset_Reference (E1)) then

               --  We go back to the original node to deal with cases where
               --  the original unset reference has been rewritten.

               Error_Msg_N
                 ("& may be referenced before it has a value?",
                  Original_Node (Unset_Reference (E1)));
            end if;
         end if;

         E1 := Next_Entity (E1);
      end loop;
   end Check_Unset_Variables;

   ---------------
   -- Check_VMS --
   ---------------

   procedure Check_VMS (Construct : Node_Id) is
   begin
      if not OpenVMS and not Debug_Flag_M then
         Error_Msg_N
           ("this construct is allowed only in Open'V'M'S", Construct);
      end if;
   end Check_VMS;

   -----------------------------------
   -- Compile_Time_Constraint_Error --
   -----------------------------------

   function Compile_Time_Constraint_Error
     (N    : Node_Id;
      Msg  : String;
      Ent  : Entity_Id  := Empty;
      Loc  : Source_Ptr := No_Location)
      return Node_Id
   is
      Msgc : String (1 .. Msg'Length + 2);
      Msgl : Natural;
      Warn : Boolean;
      P    : Node_Id;
      Msgs : Boolean;
      Typ  : Entity_Id := Ent;

   begin
      if No (Typ) then
         Typ := Etype (N);
      end if;

      --  A static constraint error in an instance body is not a fatal error.
      --  we choose to inhibit the message altogether, because there is no
      --  obvious node (for now) on which to post it. On the other hand the
      --  offending node must be replaced with a constraint_error in any case.

      if In_Instance (Body_Only => True) then
         return N;

      --  No messages are generated if we already posted an error on this node

      elsif not Error_Posted (N) then

         --  Make all such messages unconditional

         Msgc (1 .. Msg'Length) := Msg;
         Msgc (Msg'Length + 1) := '!';
         Msgl := Msg'Length + 1;

         --  Message is a warning, even in Ada 95 case

         if Msg (Msg'Length) = '?' then
            Warn := True;

         --  In Ada 83, all messages are warnings

         elsif Ada_83 and then Comes_From_Source (N) then
            Msgl := Msgl + 1;
            Msgc (Msgl) := '?';
            Warn := True;

         --  Otherwise we have a real error message (Ada 95 static case)

         else
            Warn := False;
         end if;

         --  Should we generate a warning? The answer is not quite yes. The
         --  very annoying exception occurs in the case of a short circuit
         --  operator where the left operand is static and decisive. Climb
         --  parents to see if that is the case we have here.

         Msgs := True;
         P := N;

         loop
            P := Parent (P);
            exit when Nkind (P) not in N_Subexpr;

            if (Nkind (P) = N_And_Then
                and then Compile_Time_Known_Value (Left_Opnd (P))
                and then Is_False (Expr_Value (Left_Opnd (P))))
              or else (Nkind (P) = N_Or_Else
                and then Compile_Time_Known_Value (Left_Opnd (P))
                and then Is_True (Expr_Value (Left_Opnd (P))))
            then
               Msgs := False;
               exit;
            end if;
         end loop;

         if Msgs then
            if Loc = No_Location then
               Error_Msg_NE (Msgc (1 .. Msgl), N, Typ);

            --  If Loc /= No_Location, the error message should not contain
            --  a } insertion character.

            else
               Error_Msg (Msgc (1 .. Msgl), Loc);
            end if;

            if Warn then
               if Inside_Init_Proc then
                  Error_Msg_NE
                    ("\& will be raised for objects of this type!?",
                     N, Standard_Constraint_Error);
               else
                  Error_Msg_NE
                    ("\& will be raised at runtime!?",
                     N, Standard_Constraint_Error);
               end if;
            else
               Error_Msg_NE
                 ("\static expression raises&!",
                  N, Standard_Constraint_Error);
            end if;
         end if;
      end if;

      return N;

   end Compile_Time_Constraint_Error;

   -----------------------
   -- Conditional_Delay --
   -----------------------

   procedure Conditional_Delay (New_Ent, Old_Ent : Entity_Id) is
   begin
      if Has_Delayed_Freeze (Old_Ent) and then not Is_Frozen (Old_Ent) then
         Set_Has_Delayed_Freeze (New_Ent);
      end if;
   end Conditional_Delay;

   --------------------
   -- Current_Entity --
   --------------------

   --  The currently visible definition for a given identifier is the
   --  one most chained at the start of the visibility chain, i.e. the
   --  one that is referenced by the Node_Id value of the name of the
   --  given identifier.

   function Current_Entity (N : Node_Id) return Entity_Id is
   begin
      return Get_Name_Entity_Id (Chars (N));
   end Current_Entity;

   -----------------------------
   -- Current_Entity_In_Scope --
   -----------------------------

   function Current_Entity_In_Scope (N : Node_Id) return Entity_Id is
      E  : Entity_Id;
      CS : constant Entity_Id := Current_Scope;

      Transient_Case : constant Boolean := Scope_Is_Transient;

   begin
      E := Get_Name_Entity_Id (Chars (N));

      while Present (E)
        and then Scope (E) /= CS
        and then (not Transient_Case or else Scope (E) /= Scope (CS))
      loop
         E := Homonym (E);
      end loop;

      return E;
   end Current_Entity_In_Scope;

   -----------------------
   -- Ensure_Freeze_Node --
   -----------------------

   procedure Ensure_Freeze_Node (E : Entity_Id) is
      FN : Node_Id;

   begin
      if No (Freeze_Node (E)) then
         FN := Make_Freeze_Entity (Sloc (E));
         Set_Has_Delayed_Freeze (E);
         Set_Freeze_Node (E, FN);
         Set_TSS_Elist (FN, No_Elist);
         Set_Entity (FN, E);
      end if;
   end Ensure_Freeze_Node;

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope return Entity_Id is
      C : constant Entity_Id := Scope_Stack.Table (Scope_Stack.last).Entity;

   begin
      if Present (C) then
         return C;
      else
         return Standard_Standard;
      end if;
   end Current_Scope;

   ---------------------
   -- Defining_Entity --
   ---------------------

   function Defining_Entity (N : Node_Id) return Entity_Id is
      K : constant Node_Kind := Nkind (N);

   begin
      case K is
         when
           N_Subprogram_Declaration                 |
           N_Abstract_Subprogram_Declaration        |
           N_Subprogram_Body                        |
           N_Package_Declaration                    |
           N_Subprogram_Renaming_Declaration        |
           N_Subprogram_Body_Stub                   |
           N_Generic_Subprogram_Declaration         |
           N_Generic_Package_Declaration            |
           N_Formal_Subprogram_Declaration
         =>
            return Defining_Entity (Specification (N));

         when
           N_Component_Declaration                  |
           N_Defining_Program_Unit_Name             |
           N_Discriminant_Specification             |
           N_Entry_Body                             |
           N_Entry_Declaration                      |
           N_Entry_Index_Specification              |
           N_Exception_Declaration                  |
           N_Exception_Renaming_Declaration         |
           N_Formal_Object_Declaration              |
           N_Formal_Package_Declaration             |
           N_Formal_Type_Declaration                |
           N_Full_Type_Declaration                  |
           N_Implicit_Label_Declaration             |
           N_Incomplete_Type_Declaration            |
           N_Loop_Parameter_Specification           |
           N_Number_Declaration                     |
           N_Object_Declaration                     |
           N_Object_Renaming_Declaration            |
           N_Package_Body_Stub                      |
           N_Parameter_Specification                |
           N_Private_Extension_Declaration          |
           N_Private_Type_Declaration               |
           N_Protected_Body                         |
           N_Protected_Body_Stub                    |
           N_Protected_Type_Declaration             |
           N_Single_Protected_Declaration           |
           N_Single_Task_Declaration                |
           N_Subtype_Declaration                    |
           N_Task_Body                              |
           N_Task_Body_Stub                         |
           N_Task_Type_Declaration
         =>
            return Defining_Identifier (N);

         when
           N_Function_Instantiation                 |
           N_Function_Specification                 |
           N_Generic_Function_Renaming_Declaration  |
           N_Generic_Package_Renaming_Declaration   |
           N_Generic_Procedure_Renaming_Declaration |
           N_Package_Body                           |
           N_Package_Instantiation                  |
           N_Package_Renaming_Declaration           |
           N_Package_Specification                  |
           N_Procedure_Instantiation                |
           N_Procedure_Specification
         =>
            declare
               Nam : constant Node_Id := Defining_Unit_Name (N);

            begin
               if Nkind (Nam) in N_Entity then
                  return Nam;
               else
                  return Defining_Identifier (Nam);
               end if;
            end;

         when others =>
            raise Program_Error;

      end case;
   end Defining_Entity;

   --------------------------
   -- Denotes_Discriminant --
   --------------------------

   function Denotes_Discriminant (N : Node_Id) return Boolean is
   begin
      return Is_Entity_Name (N)
        and then Ekind (Entity (N)) = E_Discriminant;
   end Denotes_Discriminant;

   -------------------------
   -- Designate_Same_Unit --
   -------------------------

   function Designate_Same_Unit
     (Name1 : Node_Id;
      Name2 : Node_Id)
      return  Boolean
   is
      K1 : Node_Kind := Nkind (Name1);
      K2 : Node_Kind := Nkind (Name2);

      function Prefix_Node (N : Node_Id) return Node_Id;
      --  Returns the parent unit name node of a defining program unit name
      --  or the prefix if N is a selected component or an expanded name.

      function Select_Node (N : Node_Id) return Node_Id;
      --  Returns the defining identifier node of a defining program unit
      --  name or  the selector node if N is a selected component or an
      --  expanded name.

      function Prefix_Node (N : Node_Id) return Node_Id is
      begin
         if Nkind (N) = N_Defining_Program_Unit_Name then
            return Name (N);

         else
            return Prefix (N);
         end if;
      end Prefix_Node;

      function Select_Node (N : Node_Id) return Node_Id is
      begin
         if Nkind (N) = N_Defining_Program_Unit_Name then
            return Defining_Identifier (N);

         else
            return Selector_Name (N);
         end if;
      end Select_Node;

   --  Start of processing for Designate_Next_Unit

   begin
      if (K1 = N_Identifier or else
          K1 = N_Defining_Identifier)
        and then
         (K2 = N_Identifier or else
          K2 = N_Defining_Identifier)
      then
         return Chars (Name1) = Chars (Name2);

      elsif
         (K1 = N_Expanded_Name      or else
          K1 = N_Selected_Component or else
          K1 = N_Defining_Program_Unit_Name)
        and then
         (K2 = N_Expanded_Name      or else
          K2 = N_Selected_Component or else
          K2 = N_Defining_Program_Unit_Name)
      then
         return
           (Chars (Select_Node (Name1)) = Chars (Select_Node (Name2)))
             and then
               Designate_Same_Unit (Prefix_Node (Name1), Prefix_Node (Name2));

      else
         return False;
      end if;
   end Designate_Same_Unit;

   ----------------------------
   -- Enclosing_Generic_Body --
   ----------------------------

   function Enclosing_Generic_Body
     (E    : Entity_Id)
      return Node_Id
   is
      P    : Node_Id;
      Spec : Node_Id;

   begin
      P := Parent (E);

      while Present (P) loop
         if Nkind (P) = N_Package_Body
           or else Nkind (P) = N_Subprogram_Body
         then
            Spec := Parent (Corresponding_Spec (P));

            if Present (Parent (Spec))
              and then
                (Nkind (Parent (Spec)) = N_Generic_Package_Declaration
                  or else
                 Nkind (Parent (Spec)) = N_Generic_Subprogram_Declaration)
            then
               return P;
            end if;
         end if;

         P := Parent (P);
      end loop;

      return Empty;
   end Enclosing_Generic_Body;

   ----------------
   -- Enter_Name --
   ----------------

   procedure Enter_Name (Def_Id : Node_Id) is
      C : constant Entity_Id := Current_Entity (Def_Id);
      E : constant Entity_Id := Current_Entity_In_Scope (Def_Id);
      S : constant Entity_Id := Current_Scope;

   begin
      --  Add new name to current scope declarations. Check for duplicate
      --  declaration, which may or may not be a genuine error.

      if Present (E) then

         --  Case of previous entity entered because of a missing declaration
         --  or else a bad subtype indication. Best is to use the new entity,
         --  and make the previous one invisible.

         if Etype (E) = Any_Type then
            Set_Is_Immediately_Visible (E, False);

         --  Case of renaming declaration constructed for package instances.
         --  if there is an explicit declaration with the same identifier,
         --  the renaming is not immediately visible any longer, but remains
         --  visible through selected component notation.

         elsif Nkind (Parent (E)) = N_Package_Renaming_Declaration
           and then not Comes_From_Source (E)
         then
            Set_Is_Immediately_Visible (E, False);

         --  For a fat pointer corresponding to a remote access to subprogram,
         --  we use the same identifier as the RAS type, so that the proper
         --  name appears in the stub. This type is only retrieved through
         --  the RAS type and never by visibility, and is not added to the
         --  visibility list (see below).

         elsif Present (Corresponding_Remote_Type (Def_Id)) then
            null;

         --  Case of an implicit operation or derived literal. The new entity
         --  hides the implicit one,  which is removed from all visibility,
         --  i.e. the entity list of its scope, and homonym chain of its name.

         elsif (Is_Overloadable (E) and then Present (Alias (E)))
           or else Is_Internal (E)
           or else (Ekind (E) = E_Enumeration_Literal
                     and then Is_Derived_Type (Etype (E)))
         then
            declare
               Prev     : Entity_Id;
               Prev_Vis : Entity_Id;

            begin
               --  If E is an implicit declaration, it cannot be the first
               --  entity in the scope.

               Prev := First_Entity (Current_Scope);

               while Next_Entity (Prev) /= E loop
                  Prev := Next_Entity (Prev);
               end loop;

               Set_Next_Entity (Prev, Next_Entity (E));

               if No (Next_Entity (Prev)) then
                  Set_Last_Entity (Current_Scope, Prev);
               end if;

               if E = Current_Entity (E) then
                     Prev_Vis := Empty;
               else
                  Prev_Vis := Current_Entity (E);
                  while Homonym (Prev_Vis) /= E loop
                     Prev_Vis := Homonym (Prev_Vis);
                  end loop;
               end if;

               if Present (Prev_Vis)  then

                  --  Skip E in the visibility chain

                  Set_Homonym (Prev_Vis, Homonym (E));

               else
                  Set_Name_Entity_Id (Chars (E), Homonym (E));
               end if;
            end;

         --  Case of genuine duplicate declaration

         else
            Error_Msg_Sloc := Sloc (E);
            Error_Msg_N ("& conflicts with declaration#", Def_Id);

            --  If entity is in standard, then we are in trouble, because
            --  it means that we have a library package with a duplicated
            --  name. That's hard to recover from, so abort!

            if S = Standard_Standard then
               raise Unrecoverable_Error;

            --  Otherwise we continue with the declaration. Having two
            --  identical declarations should not cause us too much trouble!

            else
               null;
            end if;
         end if;
      end if;

      --  If we fall through, declaration is OK , or OK enough to continue

      --  If Def_Id is a discriminant or a record component we are in the
      --  midst of inheriting components in a derived record definition.
      --  Preserve their Ekind and Etype.

      if Ekind (Def_Id) = E_Discriminant
        or else Ekind (Def_Id) = E_Component
      then
         null;

      --  Otherwise, the kind E_Void insures that premature uses of the entity
      --  will be detected. Any_Type insures that no cascaded errors will occur

      else
         Set_Ekind (Def_Id, E_Void);
         Set_Etype (Def_Id, Any_Type);
      end if;

      --  Inherited discriminants and components in derived record types are
      --  immediately visible. Itypes are not.

      if Ekind (Def_Id) = E_Discriminant
        or else Ekind (Def_Id) = E_Component
        or else (No (Corresponding_Remote_Type (Def_Id))
                 and then not Is_Itype (Def_Id))
      then
         Set_Is_Immediately_Visible (Def_Id);
         Set_Current_Entity         (Def_Id);
      end if;

      Set_Homonym                (Def_Id, C);
      Append_Entity              (Def_Id, S);
      Set_Public_Status          (Def_Id);

   end Enter_Name;

   -------------------------------------
   -- Find_Corresponding_Discriminant --
   -------------------------------------

   function Find_Corresponding_Discriminant
     (Id   : Node_Id;
      Typ  : Entity_Id)
      return Entity_Id
   is
      Par_Disc : Entity_Id;
      Old_Disc : Entity_Id;
      New_Disc : Entity_Id;

   begin
      Par_Disc := Original_Record_Component (Original_Discriminant (Id));
      Old_Disc := First_Discriminant (Scope (Par_Disc));
      New_Disc := First_Discriminant (Typ);

      while Present (Old_Disc) and then Present (New_Disc) loop
         if Old_Disc = Par_Disc  then
            return New_Disc;
         else
            Old_Disc := Next_Discriminant (Old_Disc);
            New_Disc := Next_Discriminant (New_Disc);
         end if;
      end loop;

      --  Should always find it.

      pragma Assert (False);
      raise Program_Error;
   end Find_Corresponding_Discriminant;

   ------------------
   -- First_Actual --
   ------------------

   function First_Actual (Node : Node_Id) return Node_Id is
      N : Node_Id;

   begin
      if No (Parameter_Associations (Node)) then
         return Empty;
      end if;

      N := First (Parameter_Associations (Node));

      if Nkind (N) = N_Parameter_Association then
         return First_Named_Actual (Node);
      else
         return N;
      end if;
   end First_Actual;

   -------------------------
   -- Full_Qualified_Name --
   -------------------------

   function Full_Qualified_Name (E : Entity_Id) return String_Id is

      Res : String_Id;

      function Internal_Full_Qualified_Name (E : Entity_Id) return String_Id;
      --  Compute recursively the qualified name without NUL at the end.

      function Internal_Full_Qualified_Name (E : Entity_Id) return String_Id is
         Ent         : Entity_Id := E;
         Parent_Name : String_Id := No_String;

      begin
         --  Deals properly with child units

         if Nkind (Ent) = N_Defining_Program_Unit_Name then
            Ent := Defining_Identifier (Ent);
         end if;

         --  Compute recursively the qualification. Only "Standard" has no
         --  scope.

         if Present (Scope (Scope (Ent))) then
            Parent_Name := Internal_Full_Qualified_Name (Scope (Ent));
         end if;

         --  Every entity should have a name except some expanded blocks
         --  don't bother about those.

         if Chars (Ent) = No_Name then
            return Parent_Name;
         end if;

         --  Add a period between Name and qualification

         if Parent_Name /= No_String then
            Start_String (Parent_Name);
            Store_String_Char (Get_Char_Code ('.'));

         else
            Start_String;
         end if;

         --  Generates the entity name in upper case

         Get_Name_String (Chars (Ent));
         Set_All_Upper_Case;
         Store_String_Chars (Name_Buffer (1 .. Name_Len));
         return End_String;
      end Internal_Full_Qualified_Name;

   begin
      Res := Internal_Full_Qualified_Name (E);
      Store_String_Char (Get_Char_Code (ASCII.Nul));
      return End_String;
   end Full_Qualified_Name;

   ------------------------
   -- Get_Actual_Subtype --
   ------------------------

   function Get_Actual_Subtype (N : Node_Id) return Entity_Id is
      Typ  : constant Entity_Id := Etype (N);
      Utyp : Entity_Id := Underlying_Type (Typ);
      Decl : Node_Id;
      Atyp : Entity_Id;

   begin
      if not Present (Utyp) then
         Utyp := Typ;
      end if;

      --  If what we have is an identifier that references a subprogram
      --  formal, or a variable or constant object, then we get the actual
      --  subtype from the referenced entity if one has been built.

      if Nkind (N) = N_Identifier
        and then
          (Is_Formal (Entity (N))
            or else Ekind (Entity (N)) = E_Constant
            or else Ekind (Entity (N)) = E_Variable)
        and then Present (Actual_Subtype (Entity (N)))

      then
         return Actual_Subtype (Entity (N));

      --  Actual subtype of unchecked union is always itself. We never need
      --  the "real" actual subtype. If we did, we couldn't get it anyway
      --  because the discriminant is not available. The restrictions on
      --  Unchecked_Union are designed to make sure that this is OK.

      elsif Is_Unchecked_Union (Utyp) then
         return Typ;

      --  Here for the unconstrained case, we must find actual subtype
      --  No actual subtype is available, so we must build it on the fly.

      --  Checking the type, not the underlying type, for constrainedness
      --  seems to be necessary. Maybe all the tests should be on the type???

      elsif (not Is_Constrained (Typ))
           and then (Is_Array_Type (Utyp)
                      or else (Is_Record_Type (Utyp)
                                and then Has_Discriminants (Utyp)))
           and then not Has_Unknown_Discriminants (Utyp)
           and then not (Ekind (Utyp) = E_String_Literal_Subtype)
      then
         --  Nothing to do if in default expression

         if In_Default_Expression then
            return Typ;

         --  Else build the actual subtype

         else
            Decl := Build_Actual_Subtype (Typ, N);
            Atyp := Defining_Identifier (Decl);

            --  If Build_Actual_Subtype generated a new declaration then use it

            if Atyp /= Typ then

               --  The actual subtype is an Itype, so analyze the declaration,
               --  but do not attach it to the tree, to get the type defined.

               Set_Parent (Decl, N);
               Set_Is_Itype (Atyp);
               Analyze (Decl, Suppress => All_Checks);
               Set_Associated_Node_For_Itype (Atyp, N);
               Set_Has_Delayed_Freeze (Atyp, False);
               return Atyp;

            --  Otherwise we did not build a declaration, so return original

            else
               return Typ;
            end if;
         end if;

      --  For all remaining cases, the actual subtype is the same as
      --  the nominal type.

      else
         return Typ;
      end if;
   end Get_Actual_Subtype;

   --------------------------
   -- Get_Declaration_Node --
   --------------------------

   function Get_Declaration_Node (Unit_Id : Entity_Id) return Node_Id is
      N : Node_Id := Parent (Unit_Id);

   begin
      --  Predefined operators do not have a full function declaration.

      if Ekind (Unit_Id) = E_Operator then
         return N;
      end if;

      while Nkind (N) /= N_Abstract_Subprogram_Declaration
        and then Nkind (N) /= N_Formal_Subprogram_Declaration
        and then Nkind (N) /= N_Function_Instantiation
        and then Nkind (N) /= N_Generic_Package_Declaration
        and then Nkind (N) /= N_Generic_Subprogram_Declaration
        and then Nkind (N) /= N_Package_Declaration
        and then Nkind (N) /= N_Package_Body
        and then Nkind (N) /= N_Package_Instantiation
        and then Nkind (N) /= N_Package_Renaming_Declaration
        and then Nkind (N) /= N_Procedure_Instantiation
        and then Nkind (N) /= N_Subprogram_Declaration
        and then Nkind (N) /= N_Subprogram_Body
        and then Nkind (N) /= N_Subprogram_Body_Stub
        and then Nkind (N) /= N_Subprogram_Renaming_Declaration
        and then Nkind (N) /= N_Task_Type_Declaration
        and then Nkind (N) not in N_Generic_Renaming_Declaration
      loop
         N := Parent (N);
         pragma Assert (Present (N));
      end loop;

      return N;
   end Get_Declaration_Node;

   -------------------------------
   -- Get_Default_External_Name --
   -------------------------------

   function Get_Default_External_Name (E : Entity_Id) return Node_Id is
   begin
      Get_Decoded_Name_String (Chars (E));

      if OpenVMS or Debug_Flag_M then
         Set_Casing (All_Upper_Case);
      else
         Set_Casing (All_Lower_Case);
      end if;

      return
        Make_String_Literal (Sloc (E),
          Strval => String_From_Name_Buffer);

   end Get_Default_External_Name;

   ----------------------
   -- Get_Index_Bounds --
   ----------------------

   procedure Get_Index_Bounds (N : Node_Id; L, H : out Node_Id) is
      Kind : constant Node_Kind := Nkind (N);

   begin
      if Kind = N_Range then
         L := Low_Bound (N);
         H := High_Bound (N);

      elsif Kind = N_Subtype_Indication then
         L := Low_Bound  (Range_Expression (Constraint (N)));
         H := High_Bound (Range_Expression (Constraint (N)));

      elsif Is_Entity_Name (N) and then Is_Type (Entity (N)) then
         if Nkind (Scalar_Range (Entity (N))) = N_Subtype_Indication then
            Get_Index_Bounds (Scalar_Range (Entity (N)), L, H);
         else
            L := Low_Bound  (Scalar_Range (Entity (N)));
            H := High_Bound (Scalar_Range (Entity (N)));
         end if;

      else
         --  N is an expression, indicating a range with one value.

         L := N;
         H := N;
      end if;

   end Get_Index_Bounds;

   ------------------------
   -- Get_Name_Entity_Id --
   ------------------------

   function Get_Name_Entity_Id (Id : Name_Id) return Entity_Id is
   begin
      return Entity_Id (Get_Name_Table_Info (Id));
   end Get_Name_Entity_Id;

   ---------------------------
   -- Get_Referenced_Object --
   ---------------------------

   function Get_Referenced_Object (N : Node_Id) return Node_Id is
      R   : Node_Id := N;

   begin
      while Is_Entity_Name (R)
        and then Present (Renamed_Object (Entity (R)))
      loop
         R := Renamed_Object (Entity (R));
      end loop;

      return R;
   end Get_Referenced_Object;

   ---------------------------
   -- Has_Private_Component --
   ---------------------------

   function Has_Private_Component (Type_Id : Entity_Id) return Boolean is
      Btype     : Entity_Id := Base_Type (Type_Id);
      Component : Entity_Id;

   begin
      if Is_Class_Wide_Type (Btype) then
         Btype := Root_Type (Btype);
      end if;

      if Is_Private_Type (Btype) then
         declare
            UT : constant Entity_Id := Underlying_Type (Btype);
         begin
            if No (UT) then
               return not Is_Generic_Type (Btype)
                 and then not Is_Generic_Type (Root_Type (Btype));
            else
               return not Is_Frozen (UT) and then Has_Private_Component (UT);
            end if;
         end;
      elsif Is_Array_Type (Btype) then
         return Has_Private_Component (Component_Type (Btype));

      elsif Is_Record_Type (Btype) then

         Component := First_Component (Btype);
         while Present (Component) loop
            if Has_Private_Component (Etype (Component)) then
               return True;
            end if;

            Component := Next_Component (Component);
         end loop;

         return False;

      elsif Is_Protected_Type (Btype)
        and then Present (Corresponding_Record_Type (Btype))
      then
         return Has_Private_Component (Corresponding_Record_Type (Btype));

      else
         return False;
      end if;
   end Has_Private_Component;

   --------------------------
   -- Has_Tagged_Component --
   --------------------------

   function Has_Tagged_Component (Typ : Entity_Id) return Boolean is
      Comp : Entity_Id;

   begin
      if Is_Private_Type (Typ)
        and then Present (Underlying_Type (Typ))
      then
         return Has_Tagged_Component (Underlying_Type (Typ));

      elsif Is_Array_Type (Typ) then
         return Has_Tagged_Component (Component_Type (Typ));

      elsif Is_Tagged_Type (Typ) then
         return True;

      elsif Is_Record_Type (Typ) then
         Comp := First_Component (Typ);

         while Present (Comp) loop
            if Has_Tagged_Component (Etype (Comp)) then
               return True;
            end if;

            Comp := Next_Component (Typ);
         end loop;

         return False;

      else
         return False;
      end if;
   end Has_Tagged_Component;

   -----------------
   -- In_Instance --
   -----------------

   function In_Instance (Body_Only : Boolean := False) return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if (Ekind (S) = E_Function
              or else Ekind (S) = E_Procedure)
           and then Is_Generic_Instance (S)
         then
            return True;

         elsif Ekind (S) = E_Package
           and then (not Body_Only or else In_Package_Body (S))
           and then Is_Generic_Instance (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance;

   ---------------------
   -- Is_Aliased_View --
   ---------------------

   function Is_Aliased_View (Obj : Node_Id) return Boolean is
      E : Entity_Id;

   begin
      if Is_Entity_Name (Obj) then
         E := Entity (Obj);

         return Is_Aliased (E)
           or else (Present (Renamed_Object (E))
                     and then Is_Aliased_View (Renamed_Object (E)))

           or else ((Is_Formal (E)
                      or else Ekind (E) = E_Generic_In_Out_Parameter
                      or else Ekind (E) = E_Generic_In_Parameter)
                    and then Is_Tagged_Type (Etype (E)))

           or else ((Ekind (E) = E_Task_Type or else
                     Ekind (E) = E_Protected_Type)
                    and then In_Open_Scopes (E))

            --  Current instance of type

           or else (Is_Type (E) and then E = Current_Scope)
           or else (Is_Incomplete_Or_Private_Type (E)
                     and then Full_View (E) = Current_Scope);

      elsif Nkind (Obj) = N_Selected_Component then
         return Is_Aliased (Entity (Selector_Name (Obj)));

      elsif Nkind (Obj) = N_Indexed_Component then
         return Has_Aliased_Components (Etype (Prefix (Obj)))
           or else
             (Is_Access_Type (Etype (Prefix (Obj)))
               and then
              Has_Aliased_Components
                (Designated_Type (Etype (Prefix (Obj)))));

      elsif Nkind (Obj) = N_Unchecked_Type_Conversion
        or else Nkind (Obj) = N_Type_Conversion
      then
         return Is_Tagged_Type (Etype (Obj));

      elsif Nkind (Obj) = N_Explicit_Dereference then
         return True;

      else
         return False;
      end if;
   end Is_Aliased_View;

   ----------------------------------------------
   -- Is_Dependent_Component_Of_Mutable_Object --
   ----------------------------------------------

   function Is_Dependent_Component_Of_Mutable_Object
     (Object : Node_Id)
      return   Boolean
   is
      P           : Node_Id;
      Prefix_Type : Entity_Id;
      P_Aliased   : Boolean := False;
      Comp        : Entity_Id;

      function Is_Declared_Within_Variant (Comp : Entity_Id) return Boolean;
      --  Returns True if and only if Comp is declared within a variant part.

      function Has_Dependent_Constraint (Comp : Entity_Id) return Boolean;
      --  Returns True if and only if Comp has a constrained subtype
      --  that depends on a discriminant.

      function Is_Declared_Within_Variant (Comp : Entity_Id) return Boolean is
         Comp_Decl : constant Node_Id   := Parent (Comp);
         Comp_List : constant Node_Id   := Parent (Comp_Decl);

      begin
         return Nkind (Parent (Comp_List)) = N_Variant;
      end Is_Declared_Within_Variant;

      function Has_Dependent_Constraint (Comp : Entity_Id) return Boolean is
         Comp_Decl  : constant Node_Id   := Parent (Comp);
         Subt_Indic : constant Node_Id   := Subtype_Indication (Comp_Decl);
         Constr     : Node_Id;
         Assn       : Node_Id;
         Expr       : Node_Id;

      begin
         if Nkind (Subt_Indic) = N_Subtype_Indication then
            Constr := Constraint (Subt_Indic);

            if Nkind (Constr) = N_Index_Or_Discriminant_Constraint then
               Assn := First (Constraints (Constr));
               while Present (Assn) loop
                  case Nkind (Assn) is
                     when N_Subtype_Indication =>
                        null;  --  Not yet implemented ???

                     when N_Range =>
                        if Is_Entity_Name (Low_Bound (Assn))
                          and then Ekind (Entity (Low_Bound (Assn)))
                                     = E_Discriminant
                        then
                           return True;
                        elsif Is_Entity_Name (High_Bound (Assn))
                          and then Ekind (Entity (High_Bound (Assn)))
                                     = E_Discriminant
                        then
                           return True;
                        end if;

                     when N_Discriminant_Association =>
                        Expr := Expression (Assn);
                        if Is_Entity_Name (Expr) then
                           if Ekind (Entity (Expr)) = E_Discriminant then
                              return True;
                           end if;
                        end if;

                     when N_Identifier =>
                        if Ekind (Entity (Assn)) = E_Discriminant then
                           return True;
                        end if;

                     when others =>
                        null;

                  end case;

                  Assn := Next (Assn);
               end loop;
            end if;
         end if;

         return False;
      end Has_Dependent_Constraint;

   --  Start of processing for Is_Dependent_Component_Of_Mutable_Object

   begin
      if Is_Variable (Object) then

         if Nkind (Object) = N_Selected_Component then
            P := Prefix (Object);
            Prefix_Type := Etype (P);

            if Is_Entity_Name (P) then

               if Ekind (Entity (P)) = E_Generic_In_Out_Parameter then
                  Prefix_Type := Base_Type (Prefix_Type);
               end if;

               if Is_Aliased (Entity (P)) then
                  P_Aliased := True;
               end if;

            else
               --  Check for prefix being an aliased component ???
               null;
            end if;

            if Is_Access_Type (Prefix_Type)
              or else Nkind (P) = N_Explicit_Dereference
            then
               return False;
            end if;

            Comp :=
              Original_Record_Component (Entity (Selector_Name (Object)));

            if not Is_Constrained (Prefix_Type)
              and then not Is_Indefinite_Subtype (Prefix_Type)
              and then (Is_Declared_Within_Variant (Comp)
                          or else Has_Dependent_Constraint (Comp))
              and then not P_Aliased
            then
               return True;

            else
               return
                 Is_Dependent_Component_Of_Mutable_Object (Prefix (Object));

            end if;

         elsif Nkind (Object) = N_Indexed_Component
           or else Nkind (Object) = N_Slice
         then
            return Is_Dependent_Component_Of_Mutable_Object (Prefix (Object));
         end if;
      end if;

      return False;
   end Is_Dependent_Component_Of_Mutable_Object;

   --------------------
   -- Is_Entity_Name --
   --------------------

   function Is_Entity_Name (N : Node_Id) return Boolean is
      Kind : constant Node_Kind := Nkind (N);

   begin
      --  Identifiers, operator symbols, expanded names are entity names

      return Kind = N_Identifier
        or else Kind = N_Operator_Symbol
        or else Kind = N_Expanded_Name

      --  Attribute references are entity names if they refer to an entity.
      --  Note that we don't do this by testing for the presence of the
      --  Entity field in the N_Attribute_Reference node, since it may not
      --  have been set yet.

        or else (Kind = N_Attribute_Reference
                  and then Is_Entity_Attribute_Name (Attribute_Name (N)));
   end Is_Entity_Name;

   --------------
   -- Is_False --
   --------------

   function Is_False (U : Uint) return Boolean is
   begin
      return (U = 0);
   end Is_False;

   ---------------------------
   -- Is_Fixed_Model_Number --
   ---------------------------

   function Is_Fixed_Model_Number (U : Ureal; T : Entity_Id) return Boolean is
      S : constant Ureal := Small_Value (T);
      M : Urealp.Save_Mark;
      R : Boolean;

   begin
      M := Urealp.Mark;
      R := (U = UR_Trunc (U / S) * S);
      Urealp.Release (M);
      return R;
   end Is_Fixed_Model_Number;

   -------------------------------
   -- Is_Fully_Initialized_Type --
   -------------------------------

   function Is_Fully_Initialized_Type (Typ : Entity_Id) return Boolean is
   begin
      if Is_Scalar_Type (Typ) then
         return False;

      elsif Is_Access_Type (Typ) then
         return True;

      elsif Is_Array_Type (Typ) then
         if Is_Fully_Initialized_Type (Component_Type (Typ)) then
            return True;
         end if;

         --  An interesting case, if we have a constrained type one of whose
         --  bounds is known to be null, then there are no elements to be
         --  initialized, so all the elements are initialized!

         if Is_Constrained (Typ) then
            declare
               Indx     : Node_Id;
               Indx_Typ : Entity_Id;
               Lbd, Hbd : Node_Id;

            begin
               Indx := First_Index (Typ);
               while Present (Indx) loop
                  Indx_Typ := Etype (Indx);
                  Lbd := Type_Low_Bound  (Indx_Typ);
                  Hbd := Type_High_Bound (Indx_Typ);

                  if Compile_Time_Known_Value (Lbd)
                    and then Compile_Time_Known_Value (Hbd)
                  then
                     if Expr_Value (Hbd) < Expr_Value (Lbd) then
                        return True;
                     end if;
                  end if;

                  Indx := Next_Index (Indx);
               end loop;
            end;
         end if;

         return False;

      elsif Is_Record_Type (Typ) then
         declare
            Ent : Entity_Id;

         begin
            Ent := First_Entity (Typ);

            while Present (Ent) loop
               if Ekind (Ent) = E_Component
                 and then (No (Parent (Ent))
                             or else No (Expression (Parent (Ent))))
                 and then not Is_Fully_Initialized_Type (Etype (Ent))
               then
                  return False;
               end if;

               Ent := Next_Entity (Ent);
            end loop;
         end;

         return True;

      elsif Is_Concurrent_Type (Typ) then
         return True;

      elsif Is_Private_Type (Typ) then
         declare
            U : constant Entity_Id := Underlying_Type (Typ);

         begin
            if No (U) then
               return False;
            else
               return Is_Fully_Initialized_Type (U);
            end if;
         end;

      else
         return False;
      end if;
   end Is_Fully_Initialized_Type;

   -----------------------------
   -- Is_Library_Level_Entity --
   -----------------------------

   function Is_Library_Level_Entity (E : Entity_Id) return Boolean is
   begin
      return Enclosing_Dynamic_Scope (E) = Standard_Standard;
   end Is_Library_Level_Entity;

   -------------------------
   -- Is_Object_Reference --
   -------------------------

   function Is_Object_Reference (N : Node_Id) return Boolean is
   begin
      if Is_Entity_Name (N) then
         return Is_Object (Entity (N));

      else
         --  Note: one would think that a function call should be a case
         --  that returns True here, but if this is done, then we get a
         --  compilation error in a-strunb, which has not been figured
         --  out, needs investigation ???

         case Nkind (N) is
            when N_Indexed_Component | N_Slice =>
               return True;

            when N_Selected_Component =>
               return Is_Object_Reference (Selector_Name (N));

            when N_Explicit_Dereference =>
               return True;

            --  An unchecked type conversion is considered to be an object if
            --  the operand is an object (this construction arises only as a
            --  result of expansion activities).

            when N_Unchecked_Type_Conversion =>
               return True;

            when others =>
               return False;
         end case;
      end if;
   end Is_Object_Reference;

   ----------------------
   -- Is_Selector_Name --
   ----------------------

   function Is_Selector_Name (N : Node_Id) return Boolean is

   begin
      if not Is_List_Member (N) then
         declare
            P : constant Node_Id   := Parent (N);
            K : constant Node_Kind := Nkind (P);

         begin
            return
              (K = N_Expanded_Name          or else
               K = N_Generic_Association    or else
               K = N_Parameter_Association  or else
               K = N_Selected_Component)
              and then Selector_Name (P) = N;
         end;

      else
         declare
            L : constant List_Id := List_Containing (N);
            P : constant Node_Id := Parent (L);

         begin
            return (Nkind (P) = N_Discriminant_Association
                     and then Selector_Names (P) = L)
              or else
                   (Nkind (P) = N_Component_Association
                     and then Choices (P) = L);
         end;
      end if;
   end Is_Selector_Name;

   --------------------------
   -- Is_Three_Byte_Record --
   --------------------------

   function Is_Three_Byte_Record (Typ : Entity_Id) return Boolean is
   begin
      return Is_Record_Type (Typ)
        and then Esize (Typ) > 16
        and then Esize (Typ) <= 24;
   end Is_Three_Byte_Record;

   -------------
   -- Is_True --
   -------------

   function Is_True (U : Uint) return Boolean is
   begin
      return (U /= 0);
   end Is_True;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable (N : Node_Id) return Boolean is

      Orig_Node : constant Node_Id := Original_Node (N);
      --  We do the test on the original node, since this is basically a
      --  test of syntactic categories, so it must not be disturbed by
      --  whatever rewriting might have occurred. For example, an aggregate,
      --  which is certainly NOT a variable, could be turned into a variable
      --  by expansion.

      function Is_Variable_Prefix (P : Node_Id) return Boolean;
      --  Prefixes can involve implicit dereferences, in which case we
      --  must test for the case of a reference of a constant access
      --  type, which can never be a variable.

      function Is_Variable_Prefix (P : Node_Id) return Boolean is
      begin
         if Is_Access_Type (Etype (P)) then
            return not Is_Access_Constant (Root_Type (Etype (P)));
         else
            return Is_Variable (P);
         end if;
      end Is_Variable_Prefix;

   --  Start of processing for Is_Variable

   begin
      --  Definitely OK if Assignment_OK is set. Since this is something that
      --  only gets set for expanded nodes, the test is on N, not Orig_Node.

      if Nkind (N) in N_Subexpr and then Assignment_OK (N) then
         return True;

      --  Normally we go to the original node, but there is one exception
      --  where we use the rewritten node, namely when it is an explicit
      --  dereference. The generated code may rewrite a prefix which is an
      --  access type with an explicit dereference. The dereference is a
      --  variable, even though the original node may not be (since it could
      --  be a constant of the access type).

      elsif Nkind (N) = N_Explicit_Dereference
        and then Nkind (Orig_Node) /= N_Explicit_Dereference
        and then Is_Access_Type (Etype (Orig_Node))
      then
         return Is_Variable_Prefix (Original_Node (Prefix (N)));

      --  All remaining checks use the original node

      elsif Is_Entity_Name (Orig_Node) then
         declare
            E : constant Entity_Id := Entity (Orig_Node);
            K : constant Entity_Kind := Ekind (E);

         begin
            return (K = E_Variable
                      and then Nkind (Parent (E)) /= N_Exception_Handler)
              or else  K = E_Component
              or else  K = E_Out_Parameter
              or else  K = E_In_Out_Parameter
              or else  K = E_Generic_In_Out_Parameter

               --  Current instance of type:

              or else (Is_Type (E) and then E = Current_Scope)
              or else (Is_Incomplete_Or_Private_Type (E)
                        and then Full_View (E) = Current_Scope);
         end;

      else
         case Nkind (Orig_Node) is
            when N_Indexed_Component | N_Slice =>
               return Is_Variable_Prefix (Prefix (Orig_Node));

            when N_Selected_Component =>
               return Is_Variable_Prefix (Prefix (Orig_Node))
                 and then Is_Variable (Selector_Name (Orig_Node));

            --  For an explicit dereference, we must check whether the type
            --  is ACCESS CONSTANT, since if it is, then it is not a variable.

            when N_Explicit_Dereference =>
               return Is_Access_Type (Etype (Prefix (Orig_Node)))
                 and then not
                   Is_Access_Constant (Root_Type (Etype (Prefix (Orig_Node))));

            --  The type conversion is the case where we do not deal with the
            --  context dependent special case of an actual parameter. Thus
            --  the type conversion is only considered a variable for the
            --  purposes of this routine if the target type is tagged. However,
            --  a type conversion is considered to be a variable if it does not
            --  come from source (this deals for example with the conversions
            --  of expressions to their actual subtypes).

            when N_Type_Conversion =>
               return Is_Variable (Expression (Orig_Node))
                 and then
                   (not Comes_From_Source (Orig_Node)
                      or else
                        (Is_Tagged_Type (Etype (Subtype_Mark (Orig_Node)))
                          and then
                         Is_Tagged_Type (Etype (Expression (Orig_Node)))));

            --  GNAT allows an unchecked type conversion as a variable. This
            --  only affects the generation of internal expanded code, since
            --  calls to instantiations of Unchecked_Conversion are never
            --  considered variables (since they are function calls).
            --  This is also true for expression actions.

            when N_Unchecked_Type_Conversion =>
               return Is_Variable (Expression (Orig_Node));

            when others =>
               return False;
         end case;
      end if;
   end Is_Variable;

   --------------------------
   -- Kill_Size_Check_Code --
   --------------------------

   procedure Kill_Size_Check_Code (E : Entity_Id) is
   begin
      if (Ekind (E) = E_Constant or else Ekind (E) = E_Variable)
        and then Present (Size_Check_Code (E))
      then
         Remove (Size_Check_Code (E));
      end if;
   end Kill_Size_Check_Code;

   -------------------------
   -- New_External_Entity --
   -------------------------

   function New_External_Entity
     (Kind         : Entity_Kind;
      Scope_Id     : Entity_Id;
      Sloc_Value   : Source_Ptr;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Nat := 0;
      Prefix       : Character := ' ')
      return         Entity_Id
   is
      N : constant Entity_Id :=
            Make_Defining_Identifier (Sloc_Value,
              New_External_Name
                (Chars (Related_Id), Suffix, Suffix_Index, Prefix));

   begin
      Set_Ekind          (N, Kind);
      Set_Is_Internal    (N, True);
      Append_Entity      (N, Scope_Id);
      Set_Public_Status  (N);
      return N;
   end New_External_Entity;

   -------------------------
   -- New_Internal_Entity --
   -------------------------

   function New_Internal_Entity
     (Kind       : Entity_Kind;
      Scope_Id   : Entity_Id;
      Sloc_Value : Source_Ptr;
      Id_Char    : Character)
      return       Entity_Id
   is
      N : constant Entity_Id :=
            Make_Defining_Identifier (Sloc_Value, New_Internal_Name (Id_Char));

   begin
      Set_Ekind          (N, Kind);
      Set_Is_Internal    (N, True);
      Append_Entity      (N, Scope_Id);
      return N;
   end New_Internal_Entity;

   -----------------
   -- Next_Actual --
   -----------------

   function Next_Actual (Actual_Id : Node_Id) return Node_Id is
      N  : Node_Id;

   begin
      --  If we are pointing at a positional parameter, it is a member of
      --  a node list (the list of parameters), and the next parameter
      --  is the next node on the list, unless we hit a parameter
      --  association, in which case we shift to using the chain whose
      --  head is the First_Named_Actual in the parent, and then is
      --  threaded using the Next_Named_Actual of the Parameter_Association.
      --  All this fiddling is because the original node list is in the
      --  textual call order, and what we need is the declaration order.

      if Is_List_Member (Actual_Id) then
         N := Next (Actual_Id);

         if Nkind (N) = N_Parameter_Association then
            return First_Named_Actual (Parent (Actual_Id));
         else
            return N;
         end if;

      else
         return Next_Named_Actual (Parent (Actual_Id));
      end if;
   end Next_Actual;

   -----------------------
   -- Normalize_Actuals --
   -----------------------

   --  Chain actuals according to formals of subprogram. If there are
   --  no named associations, the chain is simply the list of Parameter
   --  Associations, since the order is the same as the declaration order.
   --  If there are named associations, then the First_Named_Actual field
   --  in the N_Procedure_Call_Statement node or N_Function_Call node
   --  points to the Parameter_Association node for the parameter that
   --  comes first in declaration order. The remaining named parameters
   --  are then chained in declaration order using Next_Named_Actual.

   --  This routine also verifies that the number of actuals is compatible
   --  with the number and default values of formals, but performs no type
   --  checking (type checking is done by the caller).

   --  If the matching succeeds, Success is set to True, and the caller
   --  proceeds with type-checking. If the match is unsuccessful, then
   --  Success is set to False, and the caller attempts a different
   --  interpretation, if there is one.

   --  If the flag Report is on, the call is not overloaded, and a failure
   --  to match can be reported here, rather than in the caller.

   procedure Normalize_Actuals
     (N       : Node_Id;
      S       : Entity_Id;
      Report  : Boolean;
      Success : out Boolean)
   is
      Actuals     : constant List_Id := Parameter_Associations (N);
      Actual      : Node_Id   := Empty;
      Formal      : Entity_Id;
      Last        : Node_Id := Empty;
      First_Named : Node_Id := Empty;
      Found       : Boolean;

      Formals_To_Match : Integer := 0;
      Actuals_To_Match : Integer := 0;

      procedure Chain (A : Node_Id);
      --  Add named actual at the proper place in the list, using the
      --  Next_Named_Actual link.

      procedure Chain (A : Node_Id) is
      begin
         if No (Last) then

            --  Call node points to first actual in list.

            Set_First_Named_Actual (N, Explicit_Actual_Parameter (A));

         else
            Set_Next_Named_Actual (Last, Explicit_Actual_Parameter (A));
         end if;

         Last := A;
         Set_Next_Named_Actual (Last, Empty);
      end Chain;

   --  Start of processing for Normalize_Actuals

   begin
      if Is_Access_Type (S) then

         --  The name in the call is a function call that returns an access
         --  to subprogram. The designated type has the list of formals.

         Formal := First_Formal (Designated_Type (S));
      else
         Formal := First_Formal (S);
      end if;

      while Present (Formal) loop
         Formals_To_Match := Formals_To_Match + 1;
         Formal := Next_Formal (Formal);
      end loop;

      --  Find if there is a named association, and verify that no positional
      --  associations appear after named ones.

      if Present (Actuals) then
         Actual := First (Actuals);
      end if;

      while Present (Actual)
        and then Nkind (Actual) /= N_Parameter_Association
      loop
         Actuals_To_Match := Actuals_To_Match + 1;
         Actual := Next (Actual);
      end loop;

      if No (Actual) and Actuals_To_Match = Formals_To_Match then

         --  Most common case: positional notation, no defaults

         Success := True;
         return;

      elsif Actuals_To_Match > Formals_To_Match then

         --  Too many actuals: will not work.

         if Report then
            Error_Msg_N ("too many arguments in call", N);
         end if;

         Success := False;
         return;
      end if;

      First_Named := Actual;

      while Present (Actual) loop
         if Nkind (Actual) /= N_Parameter_Association then
            Error_Msg_N
              ("positional parameters not allowed after named ones", Actual);
            Success := False;
            return;

         else
            Actuals_To_Match := Actuals_To_Match + 1;
         end if;

         Actual := Next (Actual);
      end loop;

      if Present (Actuals) then
         Actual := First (Actuals);
      end if;

      Formal := First_Formal (S);

      while Present (Formal) loop

         --  Match the formals in order. If the corresponding actual
         --  is positional,  nothing to do. Else scan the list of named
         --  actuals to find the one with the right name.

         if Present (Actual)
           and then Nkind (Actual) /= N_Parameter_Association
         then
            Actual := Next (Actual);
            Actuals_To_Match := Actuals_To_Match - 1;
            Formals_To_Match := Formals_To_Match - 1;

         else
            --  For named parameters, search the list of actuals to find
            --  one that matches the next formal name.

            Actual := First_Named;
            Found  := False;

            while Present (Actual) loop
               if Chars (Selector_Name (Actual)) = Chars (Formal) then
                  Found := True;
                  Chain (Actual);
                  Actuals_To_Match := Actuals_To_Match - 1;
                  Formals_To_Match := Formals_To_Match - 1;
                  exit;
               end if;

               Actual := Next (Actual);
            end loop;

            if not Found then
               if Ekind (Formal) /= E_In_Parameter
                 or else No (Default_Value (Formal))
               then
                  if Report then
                     if Comes_From_Source (S)
                       and then Is_Overloadable (S)
                     then
                        Error_Msg_Name_1 := Chars (S);
                        Error_Msg_NE
                          ("missing argument for parameter & in call to %",
                             N, Formal);
                     else
                        Error_Msg_NE
                          ("missing argument for parameter &", N, Formal);
                     end if;
                  end if;

                  Success := False;
                  return;

               else
                  Formals_To_Match := Formals_To_Match - 1;
               end if;
            end if;
         end if;

         Formal := Next_Formal (Formal);
      end loop;

      if  Formals_To_Match = 0 and then Actuals_To_Match = 0 then
         Success := True;
         return;

      else
         if Report then

            --  Find some superfluous named actual that did not get
            --  attached to the list of associations.

            Actual := First (Actuals);

            while Present (Actual) loop

               if Nkind (Actual) = N_Parameter_Association
                 and then Actual /= Last
                 and then No (Next_Named_Actual (Actual))
               then
                  Error_Msg_N ("Unmatched actual in call",  Actual);
                  exit;
               end if;

               Actual := Next (Actual);
            end loop;
         end if;

         Success := False;
         return;
      end if;
   end Normalize_Actuals;

   --------------------------------
   -- Note_Possible_Modification --
   --------------------------------

   procedure Note_Possible_Modification (N : Node_Id) is
      Ent : Entity_Id;
      Exp : Node_Id;

   begin
      Exp := N;

      --  Loop to find referenced entity, if there is one

      loop
         if Is_Entity_Name (Exp) then
            Ent := Entity (Exp);

            if (Ekind (Ent) = E_Variable or else Ekind (Ent) = E_Constant)
              and then Present (Renamed_Object (Ent))
            then
               Exp := Renamed_Object (Ent);

            else
               Set_Not_Assigned (Ent, False);
               return;
            end if;

         elsif     Nkind (Exp) = N_Type_Conversion
           or else Nkind (Exp) = N_Unchecked_Type_Conversion
         then
            Exp := Expression (Exp);

         elsif     Nkind (Exp) = N_Slice
           or else Nkind (Exp) = N_Indexed_Component
           or else Nkind (Exp) = N_Selected_Component
         then
            Exp := Prefix (Exp);

         else
            return;
         end if;
      end loop;
   end Note_Possible_Modification;

   -------------------------
   -- Object_Access_Level --
   -------------------------

   function Object_Access_Level (Obj : Node_Id) return Uint is
      E : Entity_Id;

   --  Returns the static accessibility level of the view denoted
   --  by Obj.  Note that the value returned is the result of a
   --  call to Scope_Depth.  Only scope depths associated with
   --  dynamic scopes can actually be returned.  Since only
   --  relative levels matter for accessibility checking, the fact
   --  that the distance between successive levels of accessibility
   --  is not always one is immaterial (invariant: if level(E2) is
   --  deeper than level(E1), then Scope_Depth(E1) < Scope_Depth(E2)).

   begin
      if Is_Entity_Name (Obj) then
         E := Entity (Obj);

         --  If E is a type then it denotes a current instance.
         --  For this case we add one to the normal accessibility
         --  level of the type to ensure that current instances
         --  are treated as always being deeper than than the level
         --  of any visible named access type (see 3.10.2(21)).

         if Is_Type (E) then
            return Type_Access_Level (E) +  1;

         elsif Present (Renamed_Object (E)) then
            return Object_Access_Level (Renamed_Object (E));

         else
            return Scope_Depth (Enclosing_Dynamic_Scope (E));
         end if;

      elsif Nkind (Obj) = N_Selected_Component then
         if Is_Access_Type (Etype (Prefix (Obj))) then
            return Type_Access_Level (Etype (Prefix (Obj)));
         else
            return Object_Access_Level (Prefix (Obj));
         end if;

      elsif Nkind (Obj) = N_Indexed_Component then
         if Is_Access_Type (Etype (Prefix (Obj))) then
            return Type_Access_Level (Etype (Prefix (Obj)));
         else
            return Object_Access_Level (Prefix (Obj));
         end if;

      elsif Nkind (Obj) = N_Explicit_Dereference then
         return Type_Access_Level (Etype (Prefix (Obj)));

      elsif Nkind (Obj) = N_Type_Conversion then
         return Object_Access_Level (Expression (Obj));

      --  Otherwise return the scope level of Standard.
      --  (If there are cases that fall through
      --  to this point they will be treated as
      --  having global accessibility for now. ???)

      else
         return Scope_Depth (Standard_Standard);
      end if;
   end Object_Access_Level;

   -----------------------
   -- Private_Component --
   -----------------------

   function Private_Component (Type_Id : Entity_Id) return Entity_Id is
      Ancestor  : constant Entity_Id := Base_Type (Type_Id);

      function Trace_Components
        (T     : Entity_Id;
         Check : Boolean)
         return  Entity_Id;
      --  Recursive function that does the work, and checks against circular
      --  definition for each subcomponent type.

      function Trace_Components
         (T     : Entity_Id;
          Check : Boolean) return Entity_Id
       is
         Btype     : constant Entity_Id := Base_Type (T);
         Component : Entity_Id;
         P         : Entity_Id;
         Candidate : Entity_Id := Empty;

      begin
         if Check and then Btype = Ancestor then
            Error_Msg_N ("circular type definition", Type_Id);
            return Any_Type;
         end if;

         if Is_Private_Type (Btype)
           and then not Is_Generic_Type (Btype)
         then
            return Btype;

         elsif Is_Array_Type (Btype) then
            return Trace_Components (Component_Type (Btype), True);

         elsif Is_Record_Type (Btype) then
            Component := First_Entity (Btype);
            while Present (Component) loop
               P := Trace_Components (Etype (Component), True);

               if Present (P) then
                  if P = Any_Type then
                     return P;
                  else
                     Candidate := P;
                  end if;
               end if;

               Component := Next_Entity (Component);
            end loop;

            return Candidate;

         else
            return Empty;
         end if;
      end Trace_Components;

   begin
      return Trace_Components (Type_Id, False);
   end Private_Component;

   ------------------
   -- Real_Convert --
   ------------------

   --  We do the conversion to get the value of the real string by using
   --  the scanner, see Sinput for details on use of the internal source
   --  buffer for scanning internal strings.

   function Real_Convert (S : String) return Node_Id is
      Save_Src : constant Source_Buffer_Ptr := Source;
      Negative : Boolean;

   begin
      Source := Internal_Source_Ptr;
      Scan_Ptr := 1;

      for J in S'Range loop
         Source (Source_Ptr (J)) := S (J);
      end loop;

      Source (S'Length + 1) := EOF;

      if Source (Scan_Ptr) = '-' then
         Negative := True;
         Scan_Ptr := Scan_Ptr + 1;
      else
         Negative := False;
      end if;

      Scan;

      if Negative then
         Set_Realval (Token_Node, UR_Negate (Realval (Token_Node)));
      end if;

      Source := Save_Src;
      return Token_Node;
   end Real_Convert;

   ---------------
   -- Same_Name --
   ---------------

   function Same_Name (N1, N2 : Node_Id) return Boolean is
      K1 : constant Node_Kind := Nkind (N1);
      K2 : constant Node_Kind := Nkind (N2);

   begin
      if (K1 = N_Identifier or else K1 = N_Defining_Identifier)
        and then (K2 = N_Identifier or else K2 = N_Defining_Identifier)
      then
         return Chars (N1) = Chars (N2);

      elsif (K1 = N_Selected_Component or else K1 = N_Expanded_Name)
        and then (K2 = N_Selected_Component or else K2 = N_Expanded_Name)
      then
         return Same_Name (Selector_Name (N1), Selector_Name (N2))
           and then Same_Name (Prefix (N1), Prefix (N2));

      else
         return False;
      end if;
   end Same_Name;

   ------------------------
   -- Scope_Is_Transient --
   ------------------------

   function Scope_Is_Transient  return Boolean is
   begin
      return Scope_Stack.Table (Scope_Stack.Last).Is_Transient;
   end Scope_Is_Transient;

   ------------------------
   -- Set_Current_Entity --
   ------------------------

   --  The given entity is to be set as the currently visible definition
   --  of its associated name (i.e. the Node_Id associated with its name).
   --  All we have to do is to get the name from the identifier, and
   --  then set the associated Node_Id to point to the given entity.

   procedure Set_Current_Entity (E : Entity_Id) is
   begin
      Set_Name_Entity_Id (Chars (E), E);
   end Set_Current_Entity;

   ---------------------------------
   -- Set_Entity_With_Style_Check --
   ---------------------------------

   procedure Set_Entity_With_Style_Check (N : Node_Id; Val : Entity_Id) is
      Val_Actual : Entity_Id;

   begin
      if Style_Check and then Nkind (N) = N_Identifier then
         Val_Actual := Val;

         --  A special situation arises for derived operations, where we want
         --  to do the check against the parent (since the Sloc of the derived
         --  operation points to the derived type declaration itself).

         while not Comes_From_Source (Val_Actual)
           and then Nkind (Val_Actual) in N_Entity
           and then (Ekind (Val_Actual) = E_Enumeration_Literal
                      or else Ekind (Val_Actual) = E_Function
                      or else Ekind (Val_Actual) = E_Generic_Function
                      or else Ekind (Val_Actual) = E_Procedure
                      or else Ekind (Val_Actual) = E_Generic_Procedure)
           and then Present (Alias (Val_Actual))
         loop
            Val_Actual := Alias (Val_Actual);
         end loop;

         --  Renaming declarations for generic actuals do not come from source,
         --  and have a different name from that of the entity they rename, so
         --  there is not style check to perform here.

         if Chars (N) = Chars (Val_Actual) then
            Style.Check_Identifier (N, Val_Actual);
         end if;

      end if;

      Set_Entity (N, Val);
   end Set_Entity_With_Style_Check;

   ------------------------
   -- Set_Name_Entity_Id --
   ------------------------

   procedure Set_Name_Entity_Id (Id : Name_Id; Val : Entity_Id) is
   begin
      Set_Name_Table_Info (Id, Int (Val));
   end Set_Name_Entity_Id;

   ---------------------
   -- Set_Next_Actual --
   ---------------------

   procedure Set_Next_Actual (Ass1_Id : Node_Id; Ass2_Id : Node_Id) is
   begin
      if Nkind (Parent (Ass1_Id)) = N_Parameter_Association then
         Set_First_Named_Actual (Parent (Ass1_Id), Ass2_Id);
      end if;
   end Set_Next_Actual;

   -----------------------
   -- Set_Public_Status --
   -----------------------

   procedure Set_Public_Status (Id : Entity_Id) is
      S : constant Entity_Id := Current_Scope;

   begin
      if S = Standard_Standard
        or else (Is_Public (S)
                  and then (Ekind (S) = E_Package
                             or else Is_Record_Type (S)
                             or else Ekind (S) = E_Void))
      then
         Set_Is_Public (Id);

      --  The bounds of an entry family declaration can generate object
      --  declarations that are visible to the back-end, e.g. in the
      --  the declaration of a composite type that contains tasks.

      elsif Is_Public (S)
        and then Is_Concurrent_Type (S)
        and then not Has_Completion (S)
        and then Nkind (Parent (Id)) = N_Object_Declaration
      then
         Set_Is_Public (Id);
      end if;
   end Set_Public_Status;

   ----------------------------
   -- Set_Scope_Is_Transient --
   ----------------------------

   procedure Set_Scope_Is_Transient (V : Boolean := True) is
   begin
      Scope_Stack.Table (Scope_Stack.Last).Is_Transient := V;
   end Set_Scope_Is_Transient;

   -------------------
   -- Set_Size_Info --
   -------------------

   procedure Set_Size_Info (T1, T2 : Entity_Id) is
   begin
      Set_Esize (T1, Esize (T2));
      Set_Has_Biased_Representation (T1, Has_Biased_Representation (T2));
   end Set_Size_Info;

   --------------------
   -- Static_Integer --
   --------------------

   function Static_Integer (N : Node_Id) return Uint is
   begin
      Analyze_And_Resolve (N, Any_Integer);

      if N = Error
        or else Error_Posted (N)
        or else Etype (N) = Any_Type
      then
         return No_Uint;
      end if;

      if Is_Static_Expression (N) then
         if not Raises_Constraint_Error (N) then
            return Expr_Value (N);
         else
            return No_Uint;
         end if;

      elsif Etype (N) = Any_Type then
         return No_Uint;

      else
         Error_Msg_N ("static integer expression required here", N);
         return No_Uint;
      end if;
   end Static_Integer;

   --------------------------
   -- Statically_Different --
   --------------------------

   function Statically_Different (E1, E2 : Node_Id) return Boolean is
      R1 : constant Node_Id := Get_Referenced_Object (E1);
      R2 : constant Node_Id := Get_Referenced_Object (E2);

   begin
      return     Is_Entity_Name (R1)
        and then Is_Entity_Name (R2)
        and then Entity (R1) /= Entity (R2);
   end Statically_Different;

   -----------------------------
   -- Subprogram_Access_Level --
   -----------------------------

   function Subprogram_Access_Level (Subp : Entity_Id) return Uint is
   begin
      if Present (Alias (Subp)) then
         return Subprogram_Access_Level (Alias (Subp));
      else
         return Scope_Depth (Enclosing_Dynamic_Scope (Subp));
      end if;
   end Subprogram_Access_Level;

   -----------------
   -- Trace_Scope --
   -----------------

   procedure Trace_Scope (N : Node_Id; E : Entity_Id; Msg : String) is
   begin
      if Debug_Flag_W then
         for J in 0 .. Scope_Stack.Last loop
            Write_Str ("  ");
         end loop;

         Write_Str (Msg);
         Write_Name (Chars (E));
         Write_Str ("   line ");
         Write_Int (Int (Get_Line_Number (Sloc (N))));
         Write_Eol;
      end if;
   end Trace_Scope;

   -----------------------
   -- Transfer_Entities --
   -----------------------

   procedure Transfer_Entities (From : Entity_Id; To : Entity_Id) is
      Ent      : Entity_Id := First_Entity (From);

   begin
      if No (Ent) then
         return;
      end if;

      if (Last_Entity (To)) = Empty then
         Set_First_Entity (To, Ent);
      else
         Set_Next_Entity (Last_Entity (To), Ent);
      end if;

      Set_Last_Entity (To, Last_Entity (From));

      while Present (Ent) loop
         Set_Scope (Ent, To);
         Set_Public_Status (Ent);
         Ent := Next_Entity (Ent);
      end loop;

      Set_First_Entity (From, Empty);
      Set_Last_Entity (From, Empty);
   end Transfer_Entities;

   -----------------------
   -- Type_Access_Level --
   -----------------------

   function Type_Access_Level (Typ : Entity_Id) return Uint is
      Btyp : Entity_Id := Base_Type (Typ);

   begin
      --  If the type is an anonymous access type we treat it as being
      --  declared at the library level to ensure that names such as
      --  X.all'access don't fail static accessibility checks.

      if Ekind (Btyp) in Access_Kind then
         if Ekind (Btyp) = E_Anonymous_Access_Type then
            return Scope_Depth (Standard_Standard);
         end if;

         Btyp := Root_Type (Btyp);
      end if;

      return Scope_Depth (Enclosing_Dynamic_Scope (Btyp));
   end Type_Access_Level;

   -------------------
   -- Unimplemented --
   -------------------

   procedure Unimplemented (N : Node_Id; Feature : String) is
      Msg1 : constant String := " not implemented yet";
      Msg2 : String (Feature'First .. Feature'Last + Msg1'Length);

   begin
      --  Note that we don't want to use dynamic concatenation in the compiler
      --  (to limit the number of runtime routines, and hence the possibility
      --  of bootstrap path problems is reduced).

      Msg2 (Feature'Range) := Feature;
      Msg2 (Feature'Last + 1 .. Msg2'Last) := Msg1;
      Error_Msg_N (Msg2, N);
   end Unimplemented;

   ----------------
   -- Wrong_Type --
   ----------------

   procedure Wrong_Type (Expr : Node_Id; Expected_Type : Entity_Id) is
      Found_Type : constant Entity_Id := First_Subtype (Etype (Expr));
      Expec_Type : constant Entity_Id := First_Subtype (Expected_Type);

      function Has_One_Matching_Field return Boolean;
      --  Determines whether Expec_Type is a record type with a single
      --  component or discriminant whose type matches the found type or
      --  is a one dimensional array whose component type matches the
      --  found type.

      function Has_One_Matching_Field return Boolean is
         E : Entity_Id;

      begin
         if Is_Array_Type (Expec_Type)
           and then Number_Dimensions (Expec_Type) = 1
           and then
             Covers (Etype (Component_Type (Expec_Type)), Found_Type)
         then
            return True;

         elsif not Is_Record_Type (Expec_Type) then
            return False;

         else
            E := First_Entity (Expec_Type);

            loop
               if No (E) then
                  return False;

               elsif (Ekind (E) /= E_Discriminant
                       and then Ekind (E) /= E_Component)
                 or else (Chars (E) = Name_uTag
                           or else Chars (E) = Name_uParent)
               then
                  E := Next_Entity (E);

               else
                  exit;
               end if;
            end loop;

            if not Covers (Etype (E), Found_Type) then
               return False;

            elsif Present (Next_Entity (E)) then
               return False;

            else
               return True;
            end if;
         end if;
      end Has_One_Matching_Field;

   --  Start of processing for Wrong_Type

   begin
      --  Don't output message if either type is Any_Type, or if a message
      --  has already been posted for this node. We need to do the latter
      --  check explicitly (it is ordinarily done in Errout), because we
      --  are using ! to force the output of the error messages.

      if Expec_Type = Any_Type
        or else Found_Type = Any_Type
        or else Error_Posted (Expr)
      then
         return;
      end if;

      --  An interesting special check. If the expression is parenthesized
      --  and its type corresponds to the type of the sole component of the
      --  expected record type, or to the component type of the expected one
      --  dimensional array type, then assume we have a bad aggregate attempt.

      if Nkind (Expr) in N_Subexpr
        and then Paren_Count (Expr) /= 0
        and then Has_One_Matching_Field
      then
         Error_Msg_N ("positional aggregate cannot have one component", Expr);

      --  Another special check, if we are looking for a pool specific access
      --  type and we found an anonymous access type, then we probably have
      --  the case of a 'Access attribute being used in a context which needs
      --  a pool specific type, which is never allowed. The one extra check
      --  we make is that the designated types cover.

      elsif Is_Access_Type (Expec_Type)
        and then Ekind (Found_Type) = E_Anonymous_Access_Type
        and then Ekind (Base_Type (Expec_Type)) /= E_General_Access_Type
        and then Covers
          (Designated_Type (Expec_Type), Designated_Type (Found_Type))
      then
         Error_Msg_N ("result must be general access type!", Expr);
         Error_Msg_NE ("add ALL to }!", Expr, Expec_Type);

      --  Normal case of one type found, some other type expected

      else
         --  If the names of the two types are the same, see if some
         --  number of levels of qualification will help. Don't try
         --  more than three levels, and if we get to standard, it's
         --  no use (and probably represents an error in the compiler)
         --  Also do not bother with internal scope names.

         declare
            Expec_Scope : Entity_Id;
            Found_Scope : Entity_Id;

         begin
            Expec_Scope := Expec_Type;
            Found_Scope := Found_Type;

            for Levels in Int range 0 .. 3 loop
               if Chars (Expec_Scope) /= Chars (Found_Scope) then
                  Error_Msg_Qual_Level := Levels;
                  exit;
               end if;

               Expec_Scope := Scope (Expec_Scope);
               Found_Scope := Scope (Found_Scope);

               exit when Expec_Scope = Standard_Standard
                           or else
                         Found_Scope = Standard_Standard
                           or else
                         not Comes_From_Source (Expec_Scope)
                           or else
                         not Comes_From_Source (Found_Scope);
            end loop;
         end;

         Error_Msg_NE ("expected}!", Expr, Expec_Type);
         Error_Msg_NE ("found}!", Expr, Found_Type);

         Error_Msg_Qual_Level := 0;
      end if;
   end Wrong_Type;

end Sem_Util;

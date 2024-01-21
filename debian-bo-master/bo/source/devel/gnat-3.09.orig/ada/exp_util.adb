------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.201 $                            --
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
with Expander; use Expander;
with Exp_Ch7;  use Exp_Ch7;
with Itypes;   use Itypes;
with Lib.Writ; use Lib.Writ;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Exp_Util is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Make_CW_Equivalent_Type
     (T : Entity_Id;
      E : Node_Id)
      return Entity_Id;
   --  T is a class-wide type entity, E is the initial expression node that
   --  constrains T in case such as: " X: T := E" or "new T'(E)"
   --  This function returns the entity of the Equivalent type and inserts
   --  on the fly the necessary declaration such as:
   --    type anon is record
   --       _parent : Root_Type (T); constrained with E discriminants (if any)
   --       Extension : String (1 .. expr to match size of E);
   --    end record;
   --
   --  This record is compatible with any object of the class of T thanks
   --  to the first field and has the same size as E thanks to the second.

   function Make_Literal_Range
     (Loc         : Source_Ptr;
      Literal_Typ : Entity_Id;
      Index_Typ   : Entity_Id)
      return        Node_Id;
   --  Produce a Range node whose bounds are:
   --    Index_Typ'first .. Index_Typ'First + Length (Literal_Typ)
   --  this is used for expanding declarations like X : String := "sdfgdfg";

   function Make_Subtype_From_Expr
     (E       : Node_Id;
      Unc_Typ : Entity_Id)
      return    Node_Id;
   --  Creates an appropriate Subtype Indication for unconstrained object
   --  declarations. Unc_Typ can be an unconstrained array or record, or
   --  a classwide type.

   function New_Class_Wide_Subtype
     (CW_Typ : Entity_Id;
      N      : Node_Id)
      return   Entity_Id;
   --  Create an implicit subtype of CW_Typ attached to node N.

   ---------------------------------
   -- Make_Default_Expr_Functions --
   ---------------------------------

   procedure Make_Default_Expr_Functions
     (N : Node_Id;
      E : Entity_Id)
  is
      Loc     : Source_Ptr;
      Formal  : Entity_Id;
      Default : Node_Id;

   begin
      Formal := First_Formal (E);

      while Present (Formal) loop
         Default := Default_Value (Formal);

         --  If no default expression, then nothing to do

         if No (Default) then
            null;

         --  If default expression is a real literal, integer literal,
         --  character literal, or string literal, then we can copy it
         --  easily, and do not need a function. Same with an identifier
         --  or a Null_Parameter attribute reference.

         --  Another special case, if we are a library level declaration,
         --  then don't create a default expression function, because there
         --  is no place to put it. This is not right  since we really
         --  should analyze this expression in the proper context ???

         elsif Nkind (Default) = N_Real_Literal      or else
               Nkind (Default) = N_Integer_Literal   or else
               Nkind (Default) = N_Character_Literal or else
               Nkind (Default) = N_String_Literal    or else
               Nkind (Default) = N_Null              or else
               Nkind (Default) = N_Identifier        or else
               Nkind (Default) = N_Expanded_Name

           or else
               (Nkind (Default) = N_Attribute_Reference
                 and then
                Attribute_Name (Default) = Name_Null_Parameter)
           or else
             Nkind (Parent (N)) = N_Compilation_Unit
         then
            null;

         --  If we have something more complex, construct a function
         --  specification for the function that will be used to evaluate
         --  the default. The body of this function is not constructed
         --  until we encounter the body of the function currently being
         --  processed (since otherwise we would get premature freezing)

         else
            Loc := Sloc (Formal);

            --  We need a name for the function. This will definitely be
            --  referenced externally, and because of overloading and the
            --  use of the same parameter name in different overloaded
            --  functions, that's not so easy. What we do is to use a
            --  unit-specific serial number, maintained in the library
            --  table entry for the unit.

            Set_Default_Expr_Function
              (Formal,
                Make_Defining_Identifier (Loc,
                  Chars =>
                    New_External_Name (
                      Suffix       => 'D',
                      Suffix_Index => Increment_Def_Func_Count)));

            Insert_Action (N,
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Function_Specification (Loc,
                    Defining_Unit_Name =>
                      Default_Expr_Function (Formal),
                    Subtype_Mark =>
                      New_Occurrence_Of (Etype (Formal), Loc))));

            Set_Is_Inlined (Default_Expr_Function (Formal));
         end if;

         Formal := Next_Formal (Formal);
      end loop;

   end Make_Default_Expr_Functions;

   --------------------------
   -- Append_Freeze_Action --
   --------------------------

   procedure Append_Freeze_Action (T : Entity_Id; N : Node_Id) is
      Fnode : Node_Id := Freeze_Node (T);

   begin
      Ensure_Freeze_Node (T);
      Fnode := Freeze_Node (T);

      if not Present (Actions (Fnode)) then
         Set_Actions (Fnode, New_List);
      end if;

      Append (N, Actions (Fnode));
   end Append_Freeze_Action;

   ---------------------------
   -- Append_Freeze_Actions --
   ---------------------------

   procedure Append_Freeze_Actions (T : Entity_Id; L : List_Id) is
      Fnode : constant Node_Id := Freeze_Node (T);

   begin
      if No (L) then
         return;

      else
         if No (Actions (Fnode)) then
            Set_Actions (Fnode, L);

         else
            Append_List (L, Actions (Fnode));
         end if;

      end if;
   end Append_Freeze_Actions;

   ------------------------
   -- Build_Runtime_Call --
   ------------------------

   function Build_Runtime_Call (Loc : Source_Ptr; RE : RE_Id) return Node_Id is
   begin
      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (RTE (RE), Loc));
   end Build_Runtime_Call;

   --------------------------
   -- Compile_Time_Compare --
   --------------------------

   function Compile_Time_Compare (L, R : Node_Id) return Compare_Result is
      T : constant Entity_Id := Etype (L);

   begin
      --  Case where comparison involves compile time known values

      if Compile_Time_Known_Value (L)
        and then Compile_Time_Known_Value (R)
      then
         --  For the floating-point case, we have to be a little careful, since
         --  at compile time we are dealing with universal exact values, but at
         --  runtime, these will be in non-exact target form. That's why the
         --  returned results are LE and GE below instead of LT and GT.

         if Is_Floating_Point_Type (T) then
            declare
               Lo : constant Ureal := Expr_Value_R (L);
               Hi : constant Ureal := Expr_Value_R (R);

            begin
               if Lo < Hi then
                  return LE;
               elsif Lo = Hi then
                  return EQ;
               else
                  return GE;
               end if;
            end;

         --  For the integer case we know exactly (note that this includes the
         --  fixed-point case, where we know the run time integer values now)

         else
            declare
               Lo : constant Uint := Expr_Value (L);
               Hi : constant Uint := Expr_Value (R);

            begin
               if Lo < Hi then
                  return LT;
               elsif Lo = Hi then
                  return EQ;
               else
                  return GT;
               end if;
            end;
         end if;

      --  For now, say not known if non-static expressions, we will do better
      --  than this later on for some special cases where we can tell.

      else
         return Unknown;
      end if;
   end Compile_Time_Compare;

   -------------------------------
   -- Convert_To_Actual_Subtype --
   -------------------------------

   procedure Convert_To_Actual_Subtype (Exp : Entity_Id) is
      Act_ST : Entity_Id;

   begin
      Act_ST := Get_Actual_Subtype (Exp);

      if Act_ST = Etype (Exp) then
         return;

      else
         Rewrite_Substitute_Tree (Exp,
           Convert_To (Act_ST, Relocate_Node (Exp)));
         Analyze_And_Resolve (Exp, Act_ST);
      end if;
   end Convert_To_Actual_Subtype;

   -----------------------
   -- Duplicate_Subexpr --
   -----------------------

   function Duplicate_Subexpr
     (Exp      : Node_Id;
      Name_Req : Boolean := False)
      return Node_Id
   is
   begin
      Remove_Side_Effects (Exp, Name_Req);
      return New_Copy_Tree (Exp);
   end Duplicate_Subexpr;

   --------------------
   -- Ensure_Defined --
   --------------------

   procedure Ensure_Defined (Typ : Entity_Id; N : Node_Id) is
      IR : Node_Id;

   begin
      if Is_Itype (Typ) then
         IR := Make_Itype_Reference (Sloc (N));
         Set_Itype (IR, Typ);
         Insert_Action (N, IR);
      end if;
   end Ensure_Defined;

   ---------------------
   -- Evolve_And_Then --
   ---------------------

   procedure Evolve_And_Then (Cond : in out Node_Id; Cond1 : Node_Id) is
   begin
      if No (Cond) then
         Cond := Cond1;
      else
         Cond :=
           Make_And_Then (Sloc (Cond1),
             Left_Opnd  => Cond,
             Right_Opnd => Cond1);
      end if;
   end Evolve_And_Then;

   --------------------
   -- Evolve_Or_Else --
   --------------------

   procedure Evolve_Or_Else (Cond : in out Node_Id; Cond1 : Node_Id) is
   begin
      if No (Cond) then
         Cond := Cond1;
      else
         Cond :=
           Make_Or_Else (Sloc (Cond1),
             Left_Opnd  => Cond,
             Right_Opnd => Cond1);
      end if;
   end Evolve_Or_Else;

   ------------------------------
   -- Expand_Subtype_From_Expr --
   ------------------------------

   --  This function is applicable for both static and dynamic allocation of
   --  objects which are constrained by an initial expression. Basically it
   --  transforms an unconstrained subtype indication into a constrained one.
   --  The expression may also be transformed in certain cases in order to
   --  avoid multiple evaulation. In the static allocation case, the general
   --  scheme is :
   --     Val : T := Expr;
   --        is transformed into
   --     Val : Constrained_Subtype_of_T := Maybe_Modified_Expr;
   --
   --  Here are the main cases :
   --
   --  <if Expr is a Slice>
   --    Val : T ([Index_Subtype (Expr)]) := Expr;
   --
   --  <elsif Expr is a String Literal>
   --    Val : T (T'First .. T'First + Length (string literal) - 1) := Expr;
   --
   --  <elsif Expr is Constrained>
   --    Val : Type_Of_Expr := Expr;
   --
   --  <elsif Expr is an entity_name>
   --    Val : T (contraints taken from Expr) := Expr;
   --
   --  <else>
   --    type Axxx is access all T;
   --    Rval : Axxx := Expr'ref;
   --    Val  : T (contraints taken from Rval) := Rval.all;
   --    ??? note: when the Expression is allocated in the secondary stack
   --              we could use it directly instead of copying it by declaring
   --              Val : T (...) renames Rval.all

   procedure Expand_Subtype_From_Expr
     (N             : Node_Id;
      Unc_Type      : Entity_Id;
      Subtype_Indic : Node_Id;
      Exp           : Node_Id)
   is
      Loc           : constant Source_Ptr := Sloc (N);
      Exp_Typ       : constant Entity_Id  := Etype (Exp);

   begin
      if not Expander_Active then
         return;
      end if;

      if Nkind (Exp) = N_Slice then
         declare
            Slice_Type : constant Entity_Id := Etype (First_Index (Exp_Typ));

         begin
            Rewrite_Substitute_Tree (Subtype_Indic,
              Make_Subtype_Indication (Loc,
                Subtype_Mark => New_Reference_To (Unc_Type, Loc),
                Constraint =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List
                      (New_Reference_To (Slice_Type, Loc)))));

            --  This subtype indication may be used later for contraint checks
            --  we better make sure that if a variable was used as a bound of
            --  of the original slice, its value is frozen.

            Force_Evaluation (Low_Bound (Scalar_Range (Slice_Type)));
            Force_Evaluation (High_Bound (Scalar_Range (Slice_Type)));
         end;


      elsif Ekind (Exp_Typ) = E_String_Literal_Subtype then
         Rewrite_Substitute_Tree (Subtype_Indic,
           Make_Subtype_Indication (Loc,
             Subtype_Mark => New_Reference_To (Unc_Type, Loc),
             Constraint =>
               Make_Index_Or_Discriminant_Constraint (Loc,
                 Constraints => New_List (
                   Make_Literal_Range (Loc,
                     Literal_Typ => Exp_Typ,
                     Index_Typ   => Etype (First_Index (Unc_Type)))))));

      elsif Is_Constrained (Exp_Typ)
        and then not Is_Class_Wide_Type (Unc_Type)
      then
         Rewrite_Substitute_Tree (Subtype_Indic,
           New_Reference_To (Exp_Typ, Loc));

      --  nothing needs to be done for private types with unknown discriminants
      --  if the underlying type is not an unconstrained composite type.

      elsif Is_Private_Type (Unc_Type)
        and then Has_Unknown_Discriminants (Unc_Type)
        and then (not Is_Composite_Type (Underlying_Type (Unc_Type))
                    or else Is_Constrained (Underlying_Type (Unc_Type)))
      then
         null;

      else
         Remove_Side_Effects (Exp);
         Rewrite_Substitute_Tree (Subtype_Indic,
           Make_Subtype_From_Expr (Exp, Unc_Type));
      end if;
   end Expand_Subtype_From_Expr;

   ------------------
   -- Find_Prim_Op --
   ------------------

   function Find_Prim_Op (T : Entity_Id; Name : Name_Id) return Entity_Id is
      Prim : Elmt_Id;
      Typ  : Entity_Id := T;

   begin
      if Is_Class_Wide_Type (Typ) then
         Typ := Root_Type (Typ);
      end if;

      Typ := Underlying_Type (Typ);
      Prim := First_Elmt (Primitive_Operations (Typ));
      while Chars (Node (Prim)) /= Name loop
         Prim := Next_Elmt (Prim);
         pragma Assert (Present (Prim));
      end loop;

      return Node (Prim);
   end Find_Prim_Op;

   ----------------------
   -- Force_Evaluation --
   ----------------------

   procedure Force_Evaluation (Exp : Node_Id; Name_Req : Boolean := False) is
   begin
      Remove_Side_Effects (Exp, Name_Req, Variable_Ref => True);
   end Force_Evaluation;

   -------------------
   -- Insert_Action --
   -------------------

   procedure Insert_Action (Assoc_Node : Node_Id; Ins_Action : Node_Id) is
   begin
      if Present (Ins_Action) then
         Insert_Actions (Assoc_Node, New_List (Ins_Action));
      end if;
   end Insert_Action;

   --  Version with check(s) suppressed

   procedure Insert_Action
     (Assoc_Node : Node_Id; Ins_Action : Node_Id; Suppress : Check_Id)
   is
   begin
      Insert_Actions (Assoc_Node, New_List (Ins_Action), Suppress);
   end Insert_Action;

   --------------------
   -- Insert_Actions --
   --------------------

   procedure Insert_Actions (Assoc_Node : Node_Id; Ins_Actions : List_Id) is
      N : Node_Id;
      P : Node_Id;

      Wrapped_Node : Node_Id := Empty;

   begin
      if No (Ins_Actions) or else Is_Empty_List (Ins_Actions) then
         return;
      end if;

      --  Ignore insert of actions from inside default expression in the
      --  special preliminary analyze mode. Any insertions at this point
      --  have no relevance, since we are only doing the analyze to freeze
      --  the types of any static expressions. See section "Handling of
      --  Default Expressions" in the spec of package Sem for further details.

      if In_Default_Expression then
         return;
      end if;

      --  We now intend to climb up the tree to find the right point to
      --  insert the actions. We start at Assoc_Node, unless this node is
      --  a subexpression in which case we start with its parent. We do this
      --  for two reasons. First it speeds things up. Second, if Assoc_Node
      --  is itself one of the special nodes like N_And_Then, then we assume
      --  that an initial request to insert actions for such a node does not
      --  expect the actions to get deposited in the node for later handling
      --  when the node is expanded, since clearly the node is being dealt
      --  with by the caller. Note that in the subexpression case, N is
      --  always the child we came from.

      --  N_Raise_Constraint_Error is an annoying special case, it is a
      --  statement if it has type Standard_Void_Type, and a subexpression
      --  otherwise. Attribute references for procedures are

      if Nkind (Assoc_Node) in N_Subexpr
        and then (Nkind (Assoc_Node) /= N_Raise_Constraint_Error
                   or else Etype (Assoc_Node) /= Standard_Void_Type)
        and then (Nkind (Assoc_Node) /= N_Attribute_Reference
                   or else
                     not Is_Procedure_Attribute_Name
                           (Attribute_Name (Assoc_Node)))
      then
         P := Assoc_Node;             -- ????? does not agree with above!
         N := Parent (Assoc_Node);

      --  Non-subexpression case. Note that N is initially undefined in
      --  this case (N is only guaranteed defined in the subexpr case).

      else
         P := Assoc_Node;
      end if;

      --  Capture root of the transient scope

      if Scope_Is_Transient then
         Wrapped_Node  := Node_To_Be_Wrapped;
      end if;

      loop
         pragma Assert (Present (P));

         case Nkind (P) is

            --  Case of right operand of AND THEN or OR ELSE. Put the actions
            --  in the Actions field of the right operand. They will be moved
            --  out further when the AND THEN or OR ELSE operator is expanded.
            --  Nothing special needs to be done for the left operand since
            --  in that case the actions are executed unconditionally.

            when N_And_Then | N_Or_Else =>
               if N = Right_Opnd (P) then
                  if Present (Actions (P)) then
                     Insert_List_After_And_Analyze
                      (Last (Actions (P)), Ins_Actions);
                  else
                     Set_Actions (P, Ins_Actions);
                     Analyze_List (Actions (P));
                  end if;

                  return;
               end if;

            --  Then or Else operand of conditional expression. Add actions to
            --  Then_Actions or Else_Actions field as appropriate. The actions
            --  will be moved further out when the conditional is expanded.

            when N_Conditional_Expression =>
               declare
                  ThenX : constant Node_Id := Next (First (Expressions (P)));
                  ElseX : constant Node_Id := Next (ThenX);

               begin
                  --  Actions belong to the then expression, temporarily
                  --  place them as Then_Actions of the conditional expr.
                  --  They will be moved to the proper place later when
                  --  the conditional expression is expanded.

                  if N = ThenX then
                     if Present (Then_Actions (P)) then
                        Insert_List_After_And_Analyze
                          (Last (Then_Actions (P)), Ins_Actions);
                     else
                        Set_Then_Actions (P, Ins_Actions);
                        Analyze_List (Then_Actions (P));
                     end if;

                     return;

                  --  Actions belong to the else expression, temporarily
                  --  place them as Else_Actions of the conditional expr.
                  --  They will be moved to the proper place later when
                  --  the conditional expression is expanded.

                  elsif N = ElseX then
                     if Present (Else_Actions (P)) then
                        Insert_List_After_And_Analyze
                          (Last (Else_Actions (P)), Ins_Actions);
                     else
                        Set_Else_Actions (P, Ins_Actions);
                        Analyze_List (Else_Actions (P));
                     end if;

                     return;

                  --  Actions belong to the condition. In this case they are
                  --  unconditionally executed, and so we can continue the
                  --  search for the proper insert point.

                  else
                     null;
                  end if;
               end;

            --  Case of appearing in the condition of a while expression or
            --  elsif. We insert the actions into the Condition_Actions field.
            --  They will be moved further out when the while loop or elsif
            --  is analyzed.

            when N_Iteration_Scheme |
                 N_Elsif_Part
            =>
               if N = Condition (P) then
                  if Present (Condition_Actions (P)) then
                     Insert_List_After_And_Analyze
                       (Last (Condition_Actions (P)), Ins_Actions);
                  else
                     Set_Condition_Actions (P, Ins_Actions);
                     Analyze_List (Condition_Actions (P));
                  end if;

                  return;
               end if;

            --  Statements, declarations, pragmas, representation clauses.

            when
               --  Statements

               N_Accept_Statement                       |
               N_Procedure_Call_Statement               |
               N_Statement                              |

               --  Pragmas

               N_Pragma                                 |

               --  Representation_Clause

               N_At_Clause                              |
               N_Attribute_Definition_Clause            |
               N_Enumeration_Representation_Clause      |
               N_Record_Representation_Clause           |

               --  Declarations

               N_Abstract_Subprogram_Declaration        |
               N_Entry_Body                             |
               N_Exception_Declaration                  |
               N_Exception_Renaming_Declaration         |
               N_Formal_Object_Declaration              |
               N_Formal_Subprogram_Declaration          |
               N_Formal_Type_Declaration                |
               N_Full_Type_Declaration                  |
               N_Function_Instantiation                 |
               N_Generic_Function_Renaming_Declaration  |
               N_Generic_Package_Declaration            |
               N_Generic_Package_Renaming_Declaration   |
               N_Generic_Procedure_Renaming_Declaration |
               N_Generic_Subprogram_Declaration         |
               N_Implicit_Label_Declaration             |
               N_Incomplete_Type_Declaration            |
               N_Number_Declaration                     |
               N_Object_Declaration                     |
               N_Object_Renaming_Declaration            |
               N_Package_Body                           |
               N_Package_Body_Stub                      |
               N_Package_Declaration                    |
               N_Package_Instantiation                  |
               N_Package_Renaming_Declaration           |
               N_Private_Extension_Declaration          |
               N_Private_Type_Declaration               |
               N_Procedure_Instantiation                |
               N_Protected_Body_Stub                    |
               N_Protected_Type_Declaration             |
               N_Single_Task_Declaration                |
               N_Subprogram_Body                        |
               N_Subprogram_Body_Stub                   |
               N_Subprogram_Declaration                 |
               N_Subprogram_Renaming_Declaration        |
               N_Subtype_Declaration                    |
               N_Task_Body                              |
               N_Task_Body_Stub                         |
               N_Task_Type_Declaration                  |

               --  Freeze entity behaves like a declaration or statement

               N_Freeze_Entity
            =>
               --  Do not insert here if the item is not a list member (this
               --  happens for example with a triggering statement, and the
               --  proper approach is to insert before the entire select).

               if not Is_List_Member (P) then
                  null;

               --  Do not insert if parent of P is an N_Component_Association
               --  node (i.e. we are in the context of an N_Aggregate node.
               --  In this case we want to insert before the entire aggregate.

               elsif Nkind (Parent (P)) = N_Component_Association then
                  null;

               --  Do not insert if the parent of P is either an N_Variant
               --  node or an N_Record_Definition node, meaning in either
               --  case that P is a member of a component list, and that
               --  therefore the actions should be inserted outside the
               --  complete record declaration.

               elsif Nkind (Parent (P)) = N_Variant
                 or else Nkind (Parent (P)) = N_Record_Definition
               then
                  null;

               --  Otherwise we can go ahead and do the insertion

               elsif  P = Wrapped_Node then
                  Store_Before_Actions_In_Scope (Ins_Actions);
                  return;

               else
                  Insert_List_Before_And_Analyze (P, Ins_Actions);
                  return;
               end if;

            --  A special case, N_Raise_Constraint_Error can act either
            --  as a statement or a subexpression. We tell the difference
            --  by looking at the Etype. It is set to Standard_Void_Type
            --  in the statement case.

            when
               N_Raise_Constraint_Error =>
                  if Etype (P) = Standard_Void_Type then
                     if  P = Wrapped_Node then
                        Store_Before_Actions_In_Scope (Ins_Actions);
                     else
                        Insert_List_Before_And_Analyze (P, Ins_Actions);
                     end if;

                     return;

                  --  In the subexpression case, keep climbing

                  else
                     null;
                  end if;

            --  Another special case, an attribute denoting a procedure call

            when
               N_Attribute_Reference =>
                  if Is_Procedure_Attribute_Name (Attribute_Name (P)) then
                     if P = Wrapped_Node then
                        Store_Before_Actions_In_Scope (Ins_Actions);
                     else
                        Insert_List_Before_And_Analyze (P, Ins_Actions);
                     end if;

                     return;

                  --  In the subexpression case, keep climbing

                  else
                     null;
                  end if;

            --  For all other node types, keep climbing tree

            when
               N_Abortable_Part                         |
               N_Accept_Alternative                     |
               N_Access_Definition                      |
               N_Access_Function_Definition             |
               N_Access_Procedure_Definition            |
               N_Access_To_Object_Definition            |
               N_Aggregate                              |
               N_Allocator                              |
               N_Case_Statement_Alternative             |
               N_Character_Literal                      |
               N_Compilation_Unit                       |
               N_Component_Association                  |
               N_Component_Clause                       |
               N_Component_Declaration                  |
               N_Component_List                         |
               N_Concat_Multiple                        |
               N_Constrained_Array_Definition           |
               N_Decimal_Fixed_Point_Definition         |
               N_Defining_Character_Literal             |
               N_Defining_Identifier                    |
               N_Defining_Operator_Symbol               |
               N_Defining_Program_Unit_Name             |
               N_Delay_Alternative                      |
               N_Delta_Constraint                       |
               N_Derived_Type_Definition                |
               N_Designator                             |
               N_Digits_Constraint                      |
               N_Discriminant_Association               |
               N_Discriminant_Specification             |
               N_Empty                                  |
               N_Entry_Body_Formal_Part                 |
               N_Entry_Call_Alternative                 |
               N_Entry_Declaration                      |
               N_Entry_Index_Specification              |
               N_Enumeration_Type_Definition            |
               N_Error                                  |
               N_Exception_Handler                      |
               N_Expanded_Name                          |
               N_Explicit_Dereference                   |
               N_Extension_Aggregate                    |
               N_Floating_Point_Definition              |
               N_Formal_Decimal_Fixed_Point_Definition  |
               N_Formal_Derived_Type_Definition         |
               N_Formal_Discrete_Type_Definition        |
               N_Formal_Floating_Point_Definition       |
               N_Formal_Modular_Type_Definition         |
               N_Formal_Ordinary_Fixed_Point_Definition |
               N_Formal_Package_Declaration             |
               N_Formal_Private_Type_Definition         |
               N_Formal_Signed_Integer_Type_Definition  |
               N_Function_Call                          |
               N_Function_Specification                 |
               N_Generic_Association                    |
               N_Handled_Sequence_Of_Statements         |
               N_Identifier                             |
               N_In                                     |
               N_Index_Or_Discriminant_Constraint       |
               N_Indexed_Component                      |
               N_Integer_Literal                        |
               N_Interpretation                         |
               N_Itype_Reference                        |
               N_Label                                  |
               N_Loop_Parameter_Specification           |
               N_Mod_Clause                             |
               N_Modular_Type_Definition                |
               N_Not_In                                 |
               N_Null                                   |
               N_Op_Abs                                 |
               N_Op_Add                                 |
               N_Op_And                                 |
               N_Op_Concat                              |
               N_Op_Divide                              |
               N_Op_Eq                                  |
               N_Op_Expon                               |
               N_Op_Ge                                  |
               N_Op_Gt                                  |
               N_Op_Le                                  |
               N_Op_Lt                                  |
               N_Op_Minus                               |
               N_Op_Mod                                 |
               N_Op_Multiply                            |
               N_Op_Ne                                  |
               N_Op_Not                                 |
               N_Op_Or                                  |
               N_Op_Plus                                |
               N_Op_Rem                                 |
               N_Op_Rotate_Left                         |
               N_Op_Rotate_Right                        |
               N_Op_Shift_Left                          |
               N_Op_Shift_Right                         |
               N_Op_Shift_Right_Arithmetic              |
               N_Op_Subtract                            |
               N_Op_Xor                                 |
               N_Operator_Symbol                        |
               N_Ordinary_Fixed_Point_Definition        |
               N_Others_Choice                          |
               N_Package_Specification                  |
               N_Parameter_Association                  |
               N_Parameter_Specification                |
               N_Pragma_Argument_Association            |
               N_Procedure_Specification                |
               N_Protected_Body                         |
               N_Protected_Definition                   |
               N_Qualified_Expression                   |
               N_Range                                  |
               N_Range_Constraint                       |
               N_Real_Literal                           |
               N_Real_Range_Specification               |
               N_Record_Definition                      |
               N_Reference                              |
               N_Selected_Component                     |
               N_Signed_Integer_Type_Definition         |
               N_Single_Protected_Declaration           |
               N_Slice                                  |
               N_String_Literal                         |
               N_Subtype_Indication                     |
               N_Subunit                                |
               N_Task_Definition                        |
               N_Terminate_Alternative                  |
               N_Triggering_Alternative                 |
               N_Type_Conversion                        |
               N_Unchecked_Expression                   |
               N_Unchecked_Type_Conversion              |
               N_Unconstrained_Array_Definition         |
               N_Unused_At_End                          |
               N_Unused_At_Start                        |
               N_Use_Package_Clause                     |
               N_Use_Type_Clause                        |
               N_Variant                                |
               N_Variant_Part                           |
               N_Validate_Unchecked_Conversion          |
               N_With_Clause
            =>
               null;

         end case;


         --  Make sure that inserted actions stay in the transient scope

         if P = Wrapped_Node then
            Store_Before_Actions_In_Scope (Ins_Actions);
            return;
         end if;

         --  If we fall through above tests, keep climbing tree

         N := P;

         if Nkind (Parent (N)) = N_Subunit then

            --  This is the proper body corresponding to a stub. Insertion
            --  must be done at the point of the stub, which is in the decla-
            --  tive part of the parent unit.

            P := Corresponding_Stub (Parent (N));

         else
            P := Parent (N);
         end if;
      end loop;

   end Insert_Actions;

   --  Version with check(s) suppressed

   procedure Insert_Actions
     (Assoc_Node : Node_Id; Ins_Actions : List_Id; Suppress : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Record := Scope_Suppress;

         begin
            Scope_Suppress := (others => True);
            Insert_Actions (Assoc_Node, Ins_Actions);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Get_Scope_Suppress (Suppress);

         begin
            Set_Scope_Suppress (Suppress, True);
            Insert_Actions (Assoc_Node, Ins_Actions);
            Set_Scope_Suppress (Suppress, Svg);
         end;
      end if;
   end Insert_Actions;

   --------------------------
   -- Insert_Actions_After --
   --------------------------

   procedure Insert_Actions_After
     (Assoc_Node  : Node_Id;
      Ins_Actions : List_Id)
   is
   begin
      if Scope_Is_Transient
        and then Assoc_Node = Node_To_Be_Wrapped
      then
         Store_After_Actions_In_Scope (Ins_Actions);
      else
         Insert_List_After_And_Analyze (Assoc_Node, Ins_Actions);
      end if;
   end Insert_Actions_After;

   ----------------------
   -- Inside_Init_Proc --
   ----------------------

   function Inside_Init_Proc return Boolean is
      S : Entity_Id;

   begin
      S := Current_Scope;
      while S /= Standard_Standard loop
         if Chars (S) = Name_uInit_Proc then
            return True;
         else
            S := Scope (S);
         end if;
      end loop;

      return False;
   end Inside_Init_Proc;

   ------------------------
   -- Known_Non_Negative --
   ------------------------

   function Known_Non_Negative (Opnd : Node_Id) return Boolean is
   begin
      if Is_OK_Static_Expression (Opnd)
        and then Expr_Value (Opnd) >= 0
      then
         return True;

      else
         declare
            Lo : constant Node_Id := Type_Low_Bound (Etype (Opnd));

         begin
            return
              Is_OK_Static_Expression (Lo) and then Expr_Value (Lo) >= 0;
         end;
      end if;
   end Known_Non_Negative;

   --------------------------------
   -- Make_Constraints_From_Expr --
   --------------------------------

   --  1. if Expr is an uncontrained array expression, creates
   --    Unc_Type(Expr'first(1)..Expr'Last(1),..., Expr'first(n)..Expr'last(n))

   --  2. if Expr is a unconstrained discriminated type expression, creates
   --    Unc_Type(Expr.Discr1, ... , Expr.Discr_n)

   --  3. if Expr is class-wide, creates an implicit class wide subtype

   function Make_Subtype_From_Expr
     (E       : Node_Id;
      Unc_Typ : Entity_Id)
      return    Node_Id
   is
      Loc         : constant Source_Ptr := Sloc (E);
      List_Constr : List_Id := New_List;
      D           : Entity_Id;

      Full_Subtyp  : Entity_Id;
      Priv_Subtyp  : Entity_Id;
      Utyp         : Entity_Id;
      Full_Exp     : Node_Id;

   begin
      if Is_Private_Type (Unc_Typ)
        and then Has_Unknown_Discriminants (Unc_Typ)
      then

         --  Prepare the subtype completion

         Utyp        := Underlying_Type (Unc_Typ);
         Full_Subtyp := Make_Defining_Identifier (Loc,
                          New_Internal_Name ('C'));
         Full_Exp    := Unchecked_Convert_To (Utyp, Duplicate_Subexpr (E));
         Set_Parent (Full_Exp, Parent (E));

         Priv_Subtyp :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

         Insert_Action (E,
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Full_Subtyp,
             Subtype_Indication  => Make_Subtype_From_Expr (Full_Exp, Utyp)));

         --  Define the dummy private subtype

         Set_Ekind          (Priv_Subtyp, Subtype_Kind (Ekind (Unc_Typ)));
         Set_Etype          (Priv_Subtyp, Unc_Typ);
         Set_Scope          (Priv_Subtyp, Full_Subtyp);
         Set_Is_Constrained (Priv_Subtyp);
         Set_Is_Tagged_Type (Priv_Subtyp, Is_Tagged_Type (Unc_Typ));
         Set_Is_Itype       (Priv_Subtyp);
         Set_Associated_Node_For_Itype (Priv_Subtyp, E);

         if Is_Tagged_Type  (Priv_Subtyp) then
            Set_Class_Wide_Type
              (Base_Type (Priv_Subtyp), Class_Wide_Type (Unc_Typ));
            Set_Primitive_Operations (Priv_Subtyp,
              Primitive_Operations (Unc_Typ));
         end if;

         Set_Full_View (Priv_Subtyp, Full_Subtyp);

         return New_Reference_To (Priv_Subtyp, Loc);

      elsif Is_Array_Type (Unc_Typ) then
         for J in 1 .. Number_Dimensions (Unc_Typ) loop
            Append_To (List_Constr,
              Make_Range (Loc,
                Low_Bound =>
                  Make_Attribute_Reference (Loc,
                    Prefix => Duplicate_Subexpr (E),
                    Attribute_Name => Name_First,
                    Expressions => New_List (
                      Make_Integer_Literal (Loc, Intval => UI_From_Int (J)))),
                High_Bound =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => Duplicate_Subexpr (E),
                    Attribute_Name => Name_Last,
                    Expressions    => New_List (
                      Make_Integer_Literal (Loc,
                        Intval => UI_From_Int (J))))));
         end loop;

      elsif Is_Class_Wide_Type (Unc_Typ) then
         declare
            CW_Subtype : Entity_Id;
            EQ_Typ     : Entity_Id := Empty;

         begin
            if Expander_Active then
               EQ_Typ := Make_CW_Equivalent_Type (Unc_Typ, E);
            end if;

            CW_Subtype := New_Class_Wide_Subtype (Unc_Typ, E);
            Set_Equivalent_Type (CW_Subtype, EQ_Typ);

            return New_Occurrence_Of (CW_Subtype, Loc);
         end;

      else
         D := First_Discriminant (Unc_Typ);
         while (Present (D)) loop

            Append_To (List_Constr,
              Make_Selected_Component (Loc,
                Prefix        => Duplicate_Subexpr (E),
                Selector_Name => New_Reference_To (D, Loc)));

            D := Next_Discriminant (D);
         end loop;
      end if;

      return
        Make_Subtype_Indication (Loc,
          Subtype_Mark => New_Reference_To (Unc_Typ, Loc),
          Constraint   =>
            Make_Index_Or_Discriminant_Constraint (Loc,
              Constraints => List_Constr));
   end Make_Subtype_From_Expr;

   -----------------------------
   -- Make_CW_Equivalent_Type --
   -----------------------------

   --  Create a record type used as an equivalent of any member
   --  of the class which takes its size from exp.

   --  Generate the following code:

   --   type Equiv_T is record
   --     _parent :  T (List of discriminant constaints taken from Exp);
   --     Ext__50 : Storage_Array (1 .. (Exp'size - Typ'size) / Storage_Unit);
   --   end Equiv_T;

   function Make_CW_Equivalent_Type
     (T : Entity_Id;
      E : Node_Id)
      return Entity_Id
   is
      Loc         : constant Source_Ptr := Sloc (E);
      Root_Typ    : constant Entity_Id := Root_Type (T);
      Equiv_Type  : Entity_Id;
      Range_Type  : Entity_Id;
      Str_Type    : Entity_Id;
      List_Def    : List_Id := Empty_List;
      Constr_Root : Entity_Id;
      Sizexpr     : Node_Id;

   begin
      if not Has_Discriminants (Root_Typ) then
         Constr_Root := Root_Typ;
      else
         Constr_Root :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

         --  subtype cstr__n is T (List of discr constraints taken from Exp)

         Append_To (List_Def,
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Constr_Root,
               Subtype_Indication =>
                 Make_Subtype_From_Expr (E, Root_Typ)));
      end if;

      --  subtype rg__xx is Storage_Offset range
      --                           (Expr'size - typ'size) / Storage_Unit

      Range_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('G'));

      Sizexpr :=
        Make_Op_Subtract (Loc,
          Left_Opnd =>
            Make_Attribute_Reference (Loc,
              Prefix         => Duplicate_Subexpr (E),
              Attribute_Name => Name_Size),
          Right_Opnd =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (Constr_Root, Loc),
              Attribute_Name => Name_Size));

      Set_Paren_Count (Sizexpr, 1);

      Append_To (List_Def,
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Range_Type,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Offset), Loc),
              Constraint => Make_Range_Constraint (Loc,
                Range_Expression =>
                  Make_Range (Loc,
                    Low_Bound => Make_Integer_Literal (Loc, Uint_1),
                    High_Bound =>
                      Make_Op_Divide (Loc,
                        Left_Opnd => Sizexpr,
                        Right_Opnd => Make_Integer_Literal (Loc,
                          Intval =>
                            UI_From_Int (System_Storage_Unit))))))));

      --  subtype str__nn is Storage_Array (rg__x);

      Str_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      Append_To (List_Def,
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Str_Type,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Array), Loc),
              Constraint =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints =>
                    New_List (New_Reference_To (Range_Type, Loc))))));

      --  type Equiv_T is record
      --    _parent : Tnn;
      --    E : Str_Type;
      --  end Equiv_T;

      Equiv_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

      --  Avoid the generation of an init procedure

      Set_Is_Frozen (Equiv_Type);

      Set_Ekind (Equiv_Type, E_Record_Type);
      Set_Parent_Subtype (Equiv_Type, Constr_Root);

      Append_To (List_Def,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Equiv_Type,
          Type_Definition =>
            Make_Record_Definition (Loc,
              Component_List => Make_Component_List (Loc,
                Component_Items => New_List (
                  Make_Component_Declaration (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc, Name_uParent),
                    Subtype_Indication => New_Reference_To (Constr_Root, Loc)),
                  Make_Component_Declaration (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc,
                        Chars => New_Internal_Name ('C')),
                    Subtype_Indication => New_Reference_To (Str_Type, Loc))),
                Variant_Part => Empty))));

      Insert_Actions (E, List_Def);
      return Equiv_Type;
   end Make_CW_Equivalent_Type;

   ------------------------
   -- Make_Literal_Range --
   ------------------------

   function Make_Literal_Range
     (Loc         : Source_Ptr;
      Literal_Typ : Entity_Id;
      Index_Typ   : Entity_Id)
      return        Node_Id
   is
   begin
         return
           Make_Range (Loc,
             Low_Bound =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Index_Typ, Loc),
                 Attribute_Name => Name_First),
             High_Bound =>
               Make_Op_Subtract (Loc,
                  Left_Opnd =>
                    Make_Op_Add (Loc,
                      Left_Opnd =>
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Occurrence_Of (Index_Typ, Loc),
                          Attribute_Name => Name_First),
                      Right_Opnd => Make_Integer_Literal (Loc,
                        String_Literal_Length (Literal_Typ))),
                  Right_Opnd => Make_Integer_Literal (Loc, Uint_1)));
   end Make_Literal_Range;

   ----------------------------
   -- New_Class_Wide_Subtype --
   ----------------------------

   function New_Class_Wide_Subtype
     (CW_Typ : Entity_Id;
      N      : Node_Id)
      return   Entity_Id
   is
      Res      : Entity_Id := Create_Itype (E_Void, N);
      Res_Name : constant Name_Id := Chars (Res);

   begin
      Set_Public_Status (Res);
      Copy_Node (CW_Typ, Res);
      Set_Chars (Res, Res_Name);
      Set_Ekind (Res, E_Class_Wide_Subtype);
      Set_Next_Entity (Res, Empty);
      Set_Etype (Res, Base_Type (CW_Typ));
      Set_Freeze_Node (Res, Empty);
      return (Res);
   end New_Class_Wide_Subtype;

   -------------------------
   -- Remove_Side_Effects --
   -------------------------

   procedure Remove_Side_Effects
     (Exp          : Node_Id;
      Name_Req     : Boolean := False;
      Variable_Ref : Boolean := False)
   is
      Loc          : constant Source_Ptr := Sloc (Exp);
      Exp_Type     : constant Entity_Id  := Etype (Exp);
      Kind         : constant Node_Kind  := Nkind (Exp);
      Svg_Suppress : constant Suppress_Record := Scope_Suppress;
      Def_Id       : Entity_Id;
      Ref_Type     : Entity_Id;
      Res          : Node_Id;
      Ptr_Typ_Decl : Node_Id;
      New_Exp      : Node_Id;
      E            : Node_Id;

      function No_Range_Checks (L : List_Id) return Boolean;
      --  Determines if any of the expressions in L has a range check

      function Side_Effect_Free (N : Node_Id) return Boolean;
      --  Determines if the tree N represents an expession that is known
      --  not to have side effects, and for which no processing is required.

      function Side_Effect_Free (L : List_Id) return Boolean;
      --  Determines if all elements of the list L are side effect free

      function No_Range_Checks (L : List_Id) return Boolean is
         N : Node_Id;

      begin
         N := First (L);
         while Present (N) loop
            if Do_Range_Check (N) then
               return False;
            else
               N := Next (N);
            end if;
         end loop;

         return True;
      end No_Range_Checks;

      ----------------------
      -- Side_Effect_Free --
      ----------------------

      function Side_Effect_Free (N : Node_Id) return Boolean is
         K : constant Node_Kind := Nkind (N);

      begin
         --  Note on checks that could raise Constraint_Error. Strictly,
         --  if we take advantage of 11.6, these checks do not count as
         --  side effects. However, we would just as soon consider that
         --  they are side effects, since the backend CSE does not work
         --  very well on expressions which can raise Constraint_Error.

         --  An attribute reference is side effect free if it does not have
         --  an overflow check, and its expressions are side effect free.

         if K = N_Attribute_Reference then
            return not Do_Overflow_Check (N)
              and then Side_Effect_Free (Expressions (N));

         --  An entity is side effect free unless it is a function call, or
         --  a reference to a volatile variable and Name_Req is False. If
         --  Name_Req is True then we can't help returning a name which
         --  effectively allows multiple references in any case.

         elsif Is_Entity_Name (N)
           and then (Ekind (Entity (N)) /= E_Function)
           and then (not Is_Volatile (Entity (N)) or else Name_Req)
         then
            --  If the Variable_Ref flag is set, any variable reference is
            --  is considered a side-effect

            if Variable_Ref then
               return not Is_Variable (N);
            else
               return True;
            end if;

         --  A value known at compile time is always side effect free

         elsif Compile_Time_Known_Value (N) then
            return True;

         --  Literals are always side-effect free

         elsif (K = N_Integer_Literal
                 or else K = N_Real_Literal
                 or else K = N_Character_Literal
                 or else K = N_String_Literal
                 or else K = N_Null)
           and then not Raises_Constraint_Error (N)
         then
            return True;

         --  A type conversion is side effect free if there are no checks,
         --  and the expression to be converted is side effect free.

         elsif K = N_Type_Conversion then
            return not Do_Overflow_Check (N)
              and then not Do_Range_Check (Expression (N))
              and then Side_Effect_Free (Expression (N));

         --  An unchecked type conversion or type qualification is side
         --  effect free if its argument is side effect free.

         elsif K = N_Qualified_Expression
           or else K = N_Unchecked_Type_Conversion
         then
            return Side_Effect_Free (Expression (N));

         --  A unary operator is side effect free if there is no overflow
         --  check, and the operand is side effect free.

         elsif K in N_Unary_Op then
            return not Do_Overflow_Check (N)
              and then Side_Effect_Free (Right_Opnd (N));

         --  A binary operator is side effect free if there is no overflow
         --  check, and both operands are side effect free.

         elsif K in N_Binary_Op then
            return not Do_Overflow_Check (N)
              and then Side_Effect_Free (Left_Opnd  (N))
              and then Side_Effect_Free (Right_Opnd (N));

         --  An explicit dereference or selected component is side effect
         --  free if its prefix is side effect free and there is no access
         --  check. (??? for now, we do not check for access checks since
         --  the expanded code generate piles of those and is not able yet
         --  to generate them with checks off)

         elsif K = N_Explicit_Dereference
           or else K = N_Selected_Component
         then
            return Side_Effect_Free (Prefix (N));
--              and then not Do_Access_Check (N);

         --  An indexed component can be copied if the prefix is copyable
         --  and all the indexing expressions are copyable and there is
         --  no access check and no range checks.

         elsif K = N_Indexed_Component then
            return not Do_Access_Check (N)
              and then Side_Effect_Free (Prefix (N))
              and then Side_Effect_Free (Expressions (N))
              and then No_Range_Checks (Expressions (N));

         elsif K = N_Unchecked_Expression then
            return Side_Effect_Free (Expression (N));

         --  We consider that anything else has side effects. This is a bit
         --  crude, but we are pretty close for most common cases, and we
         --  are certainly correct (i.e. we never return True when the
         --  answer should be False).

         else
            return False;
         end if;
      end Side_Effect_Free;

      function Side_Effect_Free (L : List_Id) return Boolean is
         N : Node_Id;

      begin
         if L = No_List or else L = Error_List then
            return True;

         else
            N := First (L);

            while Present (N) loop
               if not Side_Effect_Free (N) then
                  return False;
               else
                  N := Next (N);
               end if;
            end loop;

            return True;
         end if;
      end Side_Effect_Free;

   --  Start of processing for Remove_Side_Effects

   begin
      --  If we are side effect free already or expansion is disabled,
      --  there is nothing to do.

      if Side_Effect_Free (Exp) or else not Expander_Active then
         return;
      end if;

      --  All the must not have any checks

      Scope_Suppress := (others => True);

      --  If the expression has the form v.all then we can just capture
      --  the pointer, and then do an explicit dereference on the result.

      if Nkind (Exp) = N_Explicit_Dereference then
         Def_Id :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
         Res :=
           Make_Explicit_Dereference (Loc, New_Reference_To (Def_Id, Loc));

         Insert_Action (Exp,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   =>
               New_Reference_To (Etype (Prefix (Exp)), Loc),
             Constant_Present    => True,
             Expression          => Relocate_Node (Prefix (Exp))));

      --  For expressions that denote objects, we can use a renaming scheme
      --  We skip using this if we have a volatile variable and we do not
      --  have Nam_Req set true (see comments above for Side_Effect_Free).

      elsif Is_Object_Reference (Exp)
        and then not Variable_Ref
        and then (Name_Req
                   or else not Is_Entity_Name (Exp)
                   or else not Is_Volatile (Entity (Exp)))
      then
         Def_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
         Res := New_Reference_To (Def_Id, Loc);

         Insert_Action (Exp,
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Subtype_Mark        => New_Reference_To (Exp_Type, Loc),
             Name                => Relocate_Node (Exp)));

      --  If it is a scalar type, just make a copy.  Likewise if this is
      --  an unchecked conversion that Gigi can't handle.

      elsif Is_Elementary_Type (Exp_Type)
        or else (Nkind (Exp) = N_Unchecked_Type_Conversion
                  and then not Safe_Unchecked_Type_Conversion (Exp))
      then
         Def_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
         Set_Etype (Def_Id, Exp_Type);
         Res := New_Reference_To (Def_Id, Loc);

         E :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   => New_Reference_To (Exp_Type, Loc),
             Constant_Present    => True,
             Expression          => Relocate_Node (Exp));

         Set_Assignment_OK (E);
         Insert_Action (Exp, E);

      --  Otherwise we generate a reference to the value

      else
         Ref_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

         Ptr_Typ_Decl :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Ref_Type,
             Type_Definition =>
               Make_Access_To_Object_Definition (Loc,
                 All_Present => True,
                 Subtype_Indication =>
                   New_Reference_To (Exp_Type, Loc)));

         E := Exp;
         Insert_Action (Exp, Ptr_Typ_Decl);

         Def_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
         Set_Etype (Def_Id, Exp_Type);

         Res :=
           Make_Explicit_Dereference (Loc,
             Prefix => New_Reference_To (Def_Id, Loc));

         if Nkind (E) = N_Explicit_Dereference then
            New_Exp := Relocate_Node (Prefix (E));
         else
            New_Exp := Make_Reference (Loc, Relocate_Node (E));
         end if;

         Insert_Action (Exp,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   => New_Reference_To (Ref_Type, Loc),
             Expression          => New_Exp));
      end if;

      --  Preserve the Assignment_OK flag in all copies, since at least
      --  one copy may be used in a context where this flag must be set
      --  (otherwise why would the flag be set in the first place).

      Set_Assignment_OK (Res, Assignment_OK (Exp));

      --  Finally rewrite the original expressoin and we are done

      Rewrite_Substitute_Tree (Exp, Res);
      Analyze_And_Resolve (Exp, Exp_Type);
      Scope_Suppress := Svg_Suppress;
   end Remove_Side_Effects;

   ------------------------------------
   -- Safe_Unchecked_Type_Conversion --
   ------------------------------------

   --  Note: this function knows quite a bit about the exact requirements
   --  of Gigi with respect to unchecked type conversions, and its code
   --  must be coordinated with any changes in Gigi in this area.

   function Safe_Unchecked_Type_Conversion (Exp : Node_Id) return Boolean is
      Otyp : constant Entity_Id := Etype (Exp);
      Ityp : constant Entity_Id := Etype (Expression (Exp));

   begin
      --  If the size of the input or output type is known at compile time,
      --  there is never a problem.  Note that unconstrained records are
      --  considered to be of known size, but we can't consider them that way
      --  here, because we are talking about the actual size of the object.

      if Size_Known_At_Compile_Time (Otyp)
        and then not (Is_Record_Type (Otyp) and then not Is_Constrained (Otyp))
      then
         return True;
      elsif Size_Known_At_Compile_Time (Ityp)
        and then not (Is_Record_Type (Ityp) and then not Is_Constrained (Ityp))
      then
         return True;

      --  If the base types of the input and output are the same, we know the
      --  alignment is the same.

      elsif Implementation_Base_Type (Ityp) =
            Implementation_Base_Type (Otyp)
      then
         return True;

      --  If either type is tagged, then we know the alignment is OK so
      --  Gigi will be able to use pointer punning.

      elsif Is_Tagged_Type (Otyp) or else Is_Tagged_Type (Ityp) then
         return True;

      --  Conversions to and from packed array types are always ignored and
      --  hence are safe.

      elsif Is_Packed_Array_Type (Otyp)
        or else Is_Packed_Array_Type (Ityp)
      then
         return True;

      --  Likewise for conversions between concurrent types and their
      --  corresponding record types.

      elsif (Present (Implementation_Type (Ityp))
             and then Present (Implementation_Type (Otyp))
             and then Is_Concurrent_Type (Implementation_Type (Ityp))
             and then Corresponding_Record_Type (Implementation_Type (Ityp)) =
                      Implementation_Type (Otyp))
        or else (Present (Implementation_Type (Ityp))
             and then Present (Implementation_Type (Otyp))
             and then Is_Concurrent_Type (Implementation_Type (Otyp))
             and then Corresponding_Record_Type (Implementation_Type (Otyp)) =
                      Implementation_Type (Ityp))
      then
         return True;

      --  The only other cases known to be safe is if the input type's
      --  alignment is known to be at least the maximum alignment for the
      --  target or if both alignments are known and the output type's
      --  alignment is no stricter than the input's.

      elsif Present (Alignment_Clause (Ityp))
        and then Expr_Value (Expression (Alignment_Clause (Ityp))) <
                 Maximum_Alignment
      then
         return True;

      elsif Present (Alignment_Clause (Ityp))
        and then Present (Alignment_Clause (Otyp))
        and then Expr_Value (Expression (Alignment_Clause (Ityp))) <=
                 Expr_Value (Expression (Alignment_Clause (Otyp)))
      then
         return True;

      --   Otherwise, Gigi cannot handle this and we must make a temporary.

      else
         return False;
      end if;

   end Safe_Unchecked_Type_Conversion;

   ----------------------------
   -- Wrap_Cleanup_Procedure --
   ----------------------------

   procedure Wrap_Cleanup_Procedure (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Stseq : constant Node_Id    := Handled_Statement_Sequence (N);
      Stmts : constant List_Id    := Statements (Stseq);

   begin
      Prepend_To (Stmts, Build_Runtime_Call (Loc, RE_Abort_Defer));
      Append_To  (Stmts, Build_Runtime_Call (Loc, RE_Abort_Undefer));
   end Wrap_Cleanup_Procedure;

end Exp_Util;

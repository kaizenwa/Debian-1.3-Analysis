------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 5                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.210 $                            --
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
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Ch7;  use Exp_Ch7;
with Itypes;   use Itypes;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Case; use Sem_Case;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Disp; use Sem_Disp;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Snames;   use Snames;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Sem_Ch5 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Elsif_Parts      (L : List_Id);
   procedure Analyze_Iteration_Scheme (N : Node_Id);

   ------------------------
   -- Analyze_Statements --
   ------------------------

   procedure Analyze_Statements (L : List_Id) is
      S : Node_Id;

   begin
      --  The labels declared in the statement list are reachable from
      --  statements in the list.

      S := First (L);

      while Present (S) loop
         if Nkind (S) = N_Label then
            Analyze (Identifier (S));

            --  If we found a label mark it as reachable.

            if Ekind (Entity (Identifier (S))) = E_Label then
               Set_Reachable (Entity (Identifier (S)));

            --  If we failed to find a label, it means the implicit
            --  declaration of the label was hidden.  A for-loop
            --  parameter can do this to a label with the same
            --  name inside the loop, since the implicit label
            --  declaration is in the innermost enclosing body
            --  or block statement.

            else
               Error_Msg_Sloc := Sloc (Entity (Identifier (S)));
               Error_Msg_N
                 ("implicit label declaration for & is hidden#",
                  Identifier (S));
            end if;
         end if;

         S := Next (S);
      end loop;

      --  Perform semantic analysis on all statements

      S := First (L);

      while Present (S) loop

         if Nkind (S) /= N_Label then
            Analyze (S);
         end if;

         S := Next (S);
      end loop;

      --  Make labels unreachable. Visibility is not sufficient, because
      --  labels in one if-branch for example are not reachable from the
      --  other branch, even though their declarations are in the enclosing
      --  declarative part.

      S := First (L);

      while Present (S) loop
         if Nkind (S) = N_Label then
            Set_Reachable (Entity (Identifier (S)), False);
         end if;

         S := Next (S);
      end loop;
   end Analyze_Statements;

   ------------------------
   -- Analyze_Assignment --
   ------------------------

   procedure Analyze_Assignment (N : Node_Id) is
      Lhs    : constant Node_Id := Name (N);
      Rhs    : constant Node_Id := Expression (N);
      T1, T2 : Entity_Id;
      Decl   : Node_Id;

      procedure Diagnose_Non_Variable_Lhs (N : Node_Id);
      --  N is the node for the left hand side of an assignment, and it
      --  is not a variable. This routine issues an appropriate diagnostic.

      -------------------------------
      -- Diagnose_Non_Variable_Lhs --
      -------------------------------

      procedure Diagnose_Non_Variable_Lhs (N : Node_Id) is
      begin
         --  Not worth posting another error if left hand side already
         --  flagged as being illegal in some respect

         if Error_Posted (N) then
            return;

         --  Some special bad cases of entity names

         elsif Is_Entity_Name (N) then

            if Ekind (Entity (N)) = E_In_Parameter then
               Error_Msg_N
                 ("assignment to IN mode parameter not allowed", N);
               return;

            --  Private declarations in a protected object are turned into
            --  constants when compiling a protected function.

            elsif Present (Scope (Entity (N)))
              and then Is_Protected_Type (Scope (Entity (N)))
              and then
                (Ekind (Current_Scope) = E_Function
                  or else
                 Ekind (Enclosing_Dynamic_Scope (Current_Scope)) = E_Function)
            then
               Error_Msg_N
                 ("protected function cannot modify protected object", N);
               return;

            elsif Ekind (Entity (N)) = E_Loop_Parameter then
               Error_Msg_N
                 ("assignment to loop parameter not allowed", N);
               return;

            end if;

         --  For indexed components, or selected components, test prefix

         elsif Nkind (N) = N_Indexed_Component
           or else Nkind (N) = N_Selected_Component
         then
            Diagnose_Non_Variable_Lhs (Prefix (N));
            return;
         end if;

         --  If we fall through, we have no special message to issue!

         Error_Msg_N ("left hand side of assignment must be a variable", N);

      end Diagnose_Non_Variable_Lhs;

   --  Start of processing for Analyze_Assignment

   begin
      Analyze (Lhs);
      Analyze (Rhs);
      T1 := Etype (Lhs);

      --  In the most general case, both Lhs and Rhs can be overloaded, and we
      --  must compute the intersection of the possible types on each side.

      if Is_Overloaded (Lhs) then
         declare
            I  : Interp_Index;
            It : Interp;

         begin
            T1 := Any_Type;
            Get_First_Interp (Lhs, I, It);

            while Present (It.Typ) loop
               if Has_Compatible_Type (Rhs, It.Typ) then
                  if T1 /= Any_Type then
                     Error_Msg_N
                       ("ambiguous left-hand side in assignment", Lhs);
                     exit;
                  else
                     T1 := It.Typ;
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;

         if T1 = Any_Type then
            Error_Msg_N
              ("no valid types for left-hand side for assignment", Lhs);
            return;
         end if;
      end if;

      Resolve (Lhs, T1);
      Note_Possible_Modification (Lhs);

      if not Is_Variable (Lhs) then
         Diagnose_Non_Variable_Lhs (Lhs);
         return;

      elsif Is_Limited_Type (T1)
        and then not Assignment_OK (Lhs)
        and then not Assignment_OK (Original_Node (Lhs))
      then
         Error_Msg_N
           ("left hand of assignment must not be limited type", Lhs);
         return;
      end if;

      --  If the nominal subtype of the left-hand side is unconstrained,
      --  use the actual subtype, or construct it if not available.

      if Is_Entity_Name (Lhs)
        and then (Ekind (Entity (Lhs)) = E_Out_Parameter
                   or else Ekind (Entity (Lhs)) = E_In_Out_Parameter
                   or else Ekind (Entity (Lhs)) = E_Generic_In_Out_Parameter)
      then
         T1 := Get_Actual_Subtype (Lhs);

      elsif Nkind (Lhs) = N_Selected_Component
        or else Nkind (Lhs) = N_Explicit_Dereference
      then
         Decl := Build_Actual_Subtype_Of_Component (T1, Lhs);

         if Present (Decl) then
            Insert_Before (N, Decl);
            Mark_Rewrite_Insertion (Decl);
            Analyze (Decl);
            T1 := Defining_Identifier (Decl);
            Set_Etype (Lhs, T1);
         end if;

      elsif Nkind (Lhs) = N_Slice then

         --  Use constrained subtype created for slice.

         T1 := Etype (Lhs);
      end if;

      Resolve (Rhs, T1);
      T2 := Etype (Rhs);
      Check_Unset_Reference (Rhs);

      if Covers (T1, T2) then
         null;
      else
         Wrong_Type (Rhs, Etype (Lhs));
         return;
      end if;

      if T1 = Any_Type or else T2 = Any_Type then
         return;
      end if;

      if (Is_Class_Wide_Type (T2) or else Is_Dynamically_Tagged (Rhs))
        and then not Is_Class_Wide_Type (T1)
      then
         Error_Msg_N ("dynamically tagged expression not allowed!", Rhs);

      elsif Is_Class_Wide_Type (T1)
        and then not Is_Class_Wide_Type (T2)
        and then not Is_Tag_Indeterminate (Rhs)
        and then not Is_Dynamically_Tagged (Rhs)
      then
         Error_Msg_N ("dynamically tagged expression required!", Rhs);
      end if;

      --  Tag propagation is done only in semantics mode only. If expansion
      --  is on, the rhs tag indeterminate function call has been expanded
      --  and tag propagation would have happened too late, so the
      --  propagation take place in expand_call instead.

      if not Expander_Active
        and then Is_Class_Wide_Type (T1)
        and then Is_Tag_Indeterminate (Rhs)
      then
         Propagate_Tag (Lhs, Rhs);
      end if;

      if Is_Scalar_Type (T1) then
         Apply_Scalar_Range_Check (Rhs, Etype (Lhs));

      elsif Is_Array_Type (T1) then

         --  Assignment verifies that the length of the Lsh and Rhs are equal,
         --  but of course the indices do not have to match.

         Apply_Length_Check (Rhs, Etype (Lhs));

      else
         --  Discriminant checks are applied in the course of expansion.
         null;
      end if;

            --  ??? a real accessibility check is needed when
   end Analyze_Assignment;

   -----------------------------
   -- Analyze_Block_Statement --
   -----------------------------

   procedure Analyze_Block_Statement (N : Node_Id) is
      Decls : constant List_Id := Declarations (N);
      Id    : Node_Id;

   begin
      Id := Identifier (N);

      if Present (Id) then
         Analyze (Id);
         Id := Entity (Id);
         Set_Ekind (Id, E_Block);
      else
         Id := New_Internal_Entity (E_Block, Current_Scope, Sloc (N), 'B');
      end if;

      Set_Etype (Id, Standard_Void_Type);
      Set_Block_Node (Id, N);
      New_Scope (Id);

      if Present (Decls) then
         Analyze_Declarations (Decls);
         Check_Completion;
      end if;

      Analyze (Handled_Statement_Sequence (N));

      if Present (Exception_Handlers (Handled_Statement_Sequence (N))) then
         declare
            S : Entity_Id := Scope (Id);

         begin
            --  Indicate that enclosing scopes contain a block with handlers.
            --  Only non-generic scopes need to be marked.

            loop
               Set_Has_Nested_Block_With_Handler (S);
               exit when Is_Overloadable (S)
                 or else Ekind (S) = E_Package
                 or else Ekind (S) = E_Generic_Function
                 or else Ekind (S) = E_Generic_Package
                 or else Ekind (S) = E_Generic_Procedure;
               S := Scope (S);
            end loop;
         end;
      end if;

      End_Scope;
   end Analyze_Block_Statement;

   ----------------------------
   -- Analyze_Case_Statement --
   ----------------------------

   procedure Analyze_Case_Statement (N : Node_Id) is

      procedure Non_Static_Choice_Error (Choice : Node_Id);
      --  Error routine invoked by the generic instantiation below when
      --  the case statment has a non static choice.

      procedure Process_Statements (Alternative : Node_Id);
      --  Analyzes all the statements associated to a case alternative.
      --  Needed by the generic instantiation below.

      package Case_Choices_Processing is new
        Generic_Choices_Processing
          (Get_Alternatives          => Alternatives,
           Get_Choices               => Discrete_Choices,
           Process_Empty_Choice      => No_OP,
           Process_Non_Static_Choice => Non_Static_Choice_Error,
           Process_Associated_Node   => Process_Statements);
      use Case_Choices_Processing;
      --  Instantiation of the generic choice processing package.

      -----------------------------
      -- Non_Static_Choice_Error --
      -----------------------------

      procedure Non_Static_Choice_Error (Choice : Node_Id) is
      begin
         Error_Msg_N ("choice given in case statement is not static", Choice);
      end Non_Static_Choice_Error;

      ------------------------
      -- Process_Statements --
      ------------------------

      procedure Process_Statements (Alternative : Node_Id) is
      begin
         Analyze_Statements (Statements (Alternative));
      end Process_Statements;

      --  Variables local to Analyze_Case_Statement.

      Exp       : Node_Id;
      Exp_Type  : Entity_Id;
      Exp_Btype : Entity_Id;

      Case_Table     : Choice_Table_Type (1 .. Number_Of_Choices (N));
      Last_Choice    : Nat;
      Dont_Care      : Boolean;
      Others_Present : Boolean;

   --  Start of processing for Analyze_Case_Statement

   begin
      Exp := Expression (N);
      Analyze_And_Resolve (Exp, Any_Discrete);
      Check_Unset_Reference (Exp);
      Exp_Type  := Etype (Exp);
      Exp_Btype := Base_Type (Exp_Type);

      --  The expression must be of a discrete type which must be determinable
      --  independently of the context in which the expression occurs, but
      --  using the fact that the expression must be of a discrete type.
      --  Moreover, the type this expression must not be a character literal
      --  (which is always ambiguous) or a generic formal type.

      --  If error already reported by Resolve, nothing more to do

      if Exp_Btype = Any_Discrete then
         return;

      elsif Exp_Btype = Any_Character then
         Error_Msg_N ("character literal as case expression is ambiguous",
                      Exp);
         return;

      elsif Is_Generic_Type (Exp_Btype)
        or else Is_Generic_Type (Root_Type (Exp_Btype))
      then
         Error_Msg_N ("case expression cannot be of a generic type", Exp);
         return;

      end if;

      --  If the case expression is a formal object of mode in out,
      --  then treat it as having a nonstatic subtype by forcing
      --  use of the base type (which has to get passed to
      --  Check_Case_Choices below).  Also use base type when
      --  the case expression is parenthesized.

      if Paren_Count (Exp) > 0
        or else (Is_Entity_Name (Exp)
                  and then Ekind (Entity (Exp)) = E_Generic_In_Out_Parameter)
      then
         Exp_Type := Exp_Btype;
      end if;

      --  Call the instantiated Analyze_Choices which does the rest of the work

      Analyze_Choices
        (N, Exp_Type, Case_Table, Last_Choice, Dont_Care, Others_Present);

      if Exp_Type = Universal_Integer and then not Others_Present then
         Error_Msg_N ("case on universal integer requires OTHERS choice", Exp);
      end if;
   end Analyze_Case_Statement;

   -------------------------
   -- Analyze_Elsif_Parts --
   -------------------------

   procedure Analyze_Elsif_Parts (L : List_Id) is
      N    : constant Node_Id := Parent (L);
      Cond : constant Node_Id := Condition (N);
      E    : Node_Id;

   begin
      E := First (L);
      while Present (E) loop
         declare
            Cond : constant Node_Id := Condition (E);

         begin
            Analyze_And_Resolve (Cond, Any_Boolean);
            Check_Unset_Reference (Cond);
         end;

         Analyze_Statements (Then_Statements (E));
         E := Next (E);
      end loop;
   end Analyze_Elsif_Parts;

   ----------------------------
   -- Analyze_Exit_Statement --
   ----------------------------

   --  If the exit includes a name, it must be the name of a currently open
   --  loop. Otherwise there must be an innermost open loop on the stack,
   --  to which the statement implicitly refers.

   procedure Analyze_Exit_Statement (N : Node_Id) is
      Target   : constant Node_Id := Name (N);
      Cond     : constant Node_Id := Condition (N);
      Scope_Id : Entity_Id;
      U_Name   : Entity_Id;
      Kind     : Entity_Kind;

   begin
      if Present (Target) then
         Analyze (Target);
         U_Name := Entity (Target);

         if not In_Open_Scopes (U_Name) or else Ekind (U_Name) /= E_Loop then
            Error_Msg_N ("invalid loop name in exit statement", N);
            return;
         else
            Set_Has_Exit (U_Name);
         end if;
      end if;

      for J in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (J).Entity;
         Kind := Ekind (Scope_Id);

         if Kind = E_Loop  and (No (Target) or Scope_Id = U_Name) then
            Set_Has_Exit (Scope_Id);
            exit;

         elsif Kind = E_Block or else Kind = E_Loop then
            null;

         else
            Error_Msg_N
              ("cannot exit from program unit or accept statement", N);
            exit;
         end if;
      end loop;

      --  Verify that if present the condition is a Boolean expression.

      if Present (Cond) then
         Analyze_And_Resolve (Cond, Any_Boolean);
         Check_Unset_Reference (Cond);
      end if;
   end Analyze_Exit_Statement;

   ----------------------------
   -- Analyze_Goto_Statement --
   ----------------------------

   procedure Analyze_Goto_Statement (N : Node_Id) is
      Label       : constant Node_Id := Name (N);
      Scope_Id    : Entity_Id;
      Label_Scope : Entity_Id;

   begin
      Analyze (Label);

      if Entity (Label) = Any_Id then
         return;

      elsif Ekind (Entity (Label)) /= E_Label then
         Error_Msg_N ("target of goto statement must be a label", Label);
         return;

      elsif not Reachable (Entity (Label)) then
         Error_Msg_N ("target of goto statement is not reachable", Label);
         return;
      end if;

      Label_Scope := Enclosing_Scope (Entity (Label));

      for J in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (J).Entity;
         exit when (Label_Scope = Scope_Id)
           or else (Ekind (Scope_Id) /= E_Block
                     and then Ekind (Scope_Id) /= E_Loop);
      end loop;

      if Scope_Id /= Label_Scope then
         Error_Msg_N
           ("cannot exit from program unit or accept statement", N);
      end if;
   end Analyze_Goto_Statement;

   --------------------------
   -- Analyze_If_Statement --
   --------------------------

   procedure Analyze_If_Statement (N : Node_Id) is
      Cond : constant Node_Id := Condition (N);

   begin
      Analyze_And_Resolve (Cond, Any_Boolean);
      Check_Unset_Reference (Cond);
      Analyze_Statements (Then_Statements (N));

      if Present (Elsif_Parts (N)) then
         Analyze_Elsif_Parts (Elsif_Parts (N));
      end if;

      if Present (Else_Statements (N)) then
         Analyze_Statements (Else_Statements (N));
      end if;
   end Analyze_If_Statement;

   ----------------------------------------
   -- Analyze_Implicit_Label_Declaration --
   ----------------------------------------

   --  An implicit label declaration is generated in the innermost
   --  enclosing declarative part. This is done for labels as well as
   --  block and loop names.

   procedure Analyze_Implicit_Label_Declaration (N : Node_Id) is
      Id : Node_Id := Defining_Identifier (N);

   begin
      Enter_Name (Id);
      Set_Ekind           (Id, E_Label);
      Set_Etype           (Id, Standard_Void_Type);
      Set_Enclosing_Scope (Id, Current_Scope);
   end Analyze_Implicit_Label_Declaration;

   ------------------------------
   -- Analyze_Iteration_Scheme --
   ------------------------------

   procedure Analyze_Iteration_Scheme (N : Node_Id) is
   begin
      --  For an infinite loop, there is no iteration scheme

      if No (N) then
         return;

      else
         declare
            Cond : constant Node_Id := Condition (N);

         begin
            --  For WHILE loop, verify that the condition is a Boolean
            --  expression and resolve and check it.

            if Present (Cond) then
               Analyze_And_Resolve (Cond, Any_Boolean);
               Check_Unset_Reference (Cond);

            --  Else we have a FOR loop

            else
               declare
                  L  : constant Node_Id := Loop_Parameter_Specification (N);
                  Id : constant Node_Id := Defining_Identifier (L);
                  D  : constant Node_Id := Discrete_Subtype_Definition (L);

               begin
                  Enter_Name (Id);

                  Analyze (D);

                  if not Is_Discrete_Type (Etype (D)) then
                     Wrong_Type (D, Any_Discrete);
                     Set_Etype (D, Any_Type);
                  end if;

                  Make_Index (D, L);
                  Set_Ekind (Id, E_Loop_Parameter);
                  Set_Etype (Id, Etype (D));
               end;
            end if;
         end;
      end if;
   end Analyze_Iteration_Scheme;

   ----------------------------
   -- Analyze_Loop_Statement --
   ----------------------------

   procedure Analyze_Loop_Statement (N : Node_Id) is
      Id  : constant Node_Id := Identifier (N);
      Ent : Entity_Id;

   begin
      if Present (Id) then

         --  Make name visible, e.g. for use in exit statements

         Analyze (Id);
         Ent := Entity (Id);

         --  If we found a label, mark its type. If not, ignore it, since it
         --  means we have a conflicting declaration, which would already have
         --  been diagnosed at declaration time.

         if Ekind (Ent) = E_Label then
            Set_Ekind (Ent, E_Loop);
         end if;

      else
         Ent := New_Internal_Entity (E_Loop, Current_Scope, Sloc (N), 'L');
         Set_Etype (Ent,  Standard_Void_Type);
         Set_Parent (Ent, N);
      end if;

      New_Scope (Ent);
      Analyze_Iteration_Scheme (Iteration_Scheme (N));

      Analyze_Statements (Statements (N));
      End_Scope;
   end Analyze_Loop_Statement;

   ----------------------------
   -- Analyze_Null_Statement --
   ----------------------------

   --  Note: the semantics of the null statement is implemented by a single
   --  null statement, too bad everything isn't as simple as this!

   procedure Analyze_Null_Statement (N : Node_Id) is
   begin
      null;
   end Analyze_Null_Statement;

end Sem_Ch5;

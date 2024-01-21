------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 6                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.348 $                            --
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
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Util; use Exp_Util;
with Exp_Ch7;  use Exp_Ch7;
with Freeze;   use Freeze;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch4;  use Sem_Ch4;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch12; use Sem_Ch12;
with Sem_Disp; use Sem_Disp;
with Sem_Dist; use Sem_Dist;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Prag; use Sem_Prag;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Sinput;   use Sinput;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinfo.CN; use Sinfo.CN;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Style;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Sem_Ch6 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Generic_Subprogram_Body (N : Node_Id; Gen_Id : Entity_Id);
   --  Analyze a generic subprogram body

   type Conformance_Type is
     (Type_Conformant, Mode_Conformant, Subtype_Conformant, Fully_Conformant);

   procedure Check_Conformance
     (New_Id   : Entity_Id;
      Old_Id   : Entity_Id;
      Ctype    : Conformance_Type;
      Errmsg   : Boolean;
      Conforms : out Boolean;
      Err_Loc  : Node_Id := Empty;
      Get_Inst : Boolean := False);
   --  Given two entities, this procedure checks that the profiles associated
   --  with these entities meet the conformance criterion given by the third
   --  parameter. If they conform, Conforms is set True and control returns
   --  to the caller. If they do not conform, Conforms is set to False, and
   --  in addition, if Errmsg is True on the call, proper messages are output
   --  to complain about the conformance failure. If Err_Loc is non_Empty
   --  the error messages are placed on Err_Loc, if Err_Loc is empty, then
   --  error messages are placed on the appropriate part of the construct
   --  denoted by New_Id. If Get_Inst is true, then this is a mode conformance
   --  against a formal access-to-subprogram type so Get_Instance_Of must
   --  be called.

   function Is_Non_Overriding_Operation
     (Prev_E : Entity_Id;
      New_E  : Entity_Id)
      return Boolean;
   --  Enforce the rule given in 12.3(18): a private operation in an instance
   --  overrides an inherited operation only if the corresponding operation
   --  was overriding in the generic. This can happen for primitive operations
   --  of types derived (in the generic unit) from formal private or formal
   --  derived types.

   procedure Check_Returns
     (HSS  : Node_Id;
      Mode : Character;
      Err  : out Boolean);
   --  Called to check for missing return statements in a function body,
   --  or for returns present in a procedure body which has No_Return set.
   --  L is the handled statement sequence for the subprogram body. This
   --  procedure checks all flow paths to make sure they either have a
   --  return (Mode = 'F') or do not have a return (Mode = 'P'). The flag
   --  Err is set if there are any control paths not explicitly terminated
   --  by a return in the function case, and is True otherwise.

   function Conforming_Types
     (T1       : Entity_Id;
      T2       : Entity_Id;
      Ctype    : Conformance_Type;
      Get_Inst : Boolean := False)
      return     Boolean;
   --  Check that two formal parameter types conform, checking both
   --  for equality of base types, and where required statically
   --  matching subtypes, depending on the setting of Ctype.

   procedure Enter_Overloaded_Entity (S : Entity_Id);
   --  This procedure makes S, a new overloaded entity, into the first
   --  visible entity with that name.

   procedure Install_Entity (E : Entity_Id);
   --  Make single entity visible. Used for generic formals as well.

   procedure Install_Formals (Id : Entity_Id);
   --  On entry to a subprogram body, make the formals visible. Note
   --  that simply placing the subprogram on the scope stack is not
   --  sufficient: the formals must become the current entities for
   --  their names.

   procedure Make_Inequality_Operator (S : Entity_Id);
   --  Create the declaration for an inequality operator that is implicitly
   --  created by a user-defined equality operator that yields a boolean.

   procedure May_Need_Actuals (Fun : Entity_Id);
   --  Flag functions that can be called without parameters, i.e. those that
   --  have no parameters, or those for which defaults exist for all parameters

   ---------------------------------------------
   -- Analyze_Abstract_Subprogram_Declaration --
   ---------------------------------------------

   procedure Analyze_Abstract_Subprogram_Declaration (N : Node_Id) is
      Designator : constant Entity_Id := Analyze_Spec (Specification (N));
      ELU        : constant Entity_Id := Current_Scope;
      Pure_Flag  : constant Boolean   := Is_Pure (ELU);
      RCI_Flag   : constant Boolean   := Is_Remote_Call_Interface (ELU);
      RT_Flag    : constant Boolean   := Is_Remote_Types (ELU);

   begin
      New_Overloaded_Entity (Designator);
      Set_Is_Abstract (Designator);
      Check_Delayed_Subprogram (Designator);

      --  Entities declared in Pure unit should be set Is_Pure
      --  Since 'Partition_ID cannot be applied to such an entity
      --  Subprogram declared in RCI unit should be set
      --  Is_Remote_Call_Interface, used to verify remote call.

      Set_Is_Pure (Designator, Pure_Flag);
      Set_Is_Remote_Call_Interface (Designator, RCI_Flag);
      Set_Is_Remote_Types (Designator, RT_Flag);

   end Analyze_Abstract_Subprogram_Declaration;

   ----------------------------
   -- Analyze_Function_Call  --
   ----------------------------

   procedure Analyze_Function_Call (N : Node_Id) is
      P      : constant Node_Id := Name (N);
      L      : constant List_Id := Parameter_Associations (N);
      Actual : Node_Id;

   begin
      Analyze (P);

      --  If error analyzing name, then set Any_Type as result type and return

      if Etype (P) = Any_Type then
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  Otherwise analyze the parameters

      if Present (L) then
         Actual := First (L);

         while Present (Actual) loop
            Analyze (Actual);
            Actual := Next (Actual);
         end loop;
      end if;

      Analyze_Call (N);

   end Analyze_Function_Call;

   -------------------------------------
   -- Analyze_Generic_Subprogram_Body --
   -------------------------------------

   procedure Analyze_Generic_Subprogram_Body
     (N      : Node_Id;
      Gen_Id : Entity_Id)
   is
      Gen_Decl : constant Node_Id := Get_Declaration_Node (Gen_Id);
      Spec     : Node_Id;
      Kind     : constant Entity_Kind := Ekind (Gen_Id);
      Nam      : Entity_Id;
      New_N    : Node_Id;

   begin
      --  Copy body and disable expansion while analyzing the generic

      New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
      Rewrite_Substitute_Tree (N, New_N);

      Start_Generic;

      Spec := Specification (N);

      --  Within the body of the generic, the subprogram is callable, and
      --  behaves like the corresponding non-generic unit.

      Nam := Defining_Entity (Spec);

      if Kind = E_Generic_Procedure
        and then Nkind (Spec) /= N_Procedure_Specification
      then
         Error_Msg_N ("invalid body for generic procedure ", Nam);
         return;

      elsif Kind = E_Generic_Function
        and then Nkind (Spec) /= N_Function_Specification
      then
         Error_Msg_N ("invalid body for generic function ", Nam);
         return;
      end if;

      Set_Corresponding_Body (Gen_Decl, Nam);

      if Has_Completion (Gen_Id) then
         Error_Msg_N ("duplicate generic body", N);
         return;
      else
         Set_Has_Completion (Gen_Id);
      end if;

      if Nkind (N) = N_Subprogram_Body_Stub then
         return;
      end if;

      Set_Corresponding_Spec (N, Gen_Id);

      --  Make generic parameters immediately visible in the body. They are
      --  needed to process the formals declarations. Then make the formals
      --  visible in a separate step.

      New_Scope (Gen_Id);

      declare
         E         : Entity_Id;
         First_Ent : Entity_Id;

      begin
         First_Ent := First_Entity (Gen_Id);
         E := First_Ent;
         while Present (E) and then not Is_Formal (E) loop
            Install_Entity (E);
            E := Next_Entity (E);
         end loop;

         Set_Use (Generic_Formal_Declarations (Gen_Decl));

         --  Now generic formals are visible, and the specification can be
         --  analyzed, for subsequent conformance check.

         Nam := Analyze_Spec (Spec);

         if Present (E) then

            --  E is the first formal parameter, which must be the first
            --  entity in the subprogram body.

            Set_First_Entity (Gen_Id, E);

            --  Now make formal parameters visible

            while Present (E) loop
               Install_Entity (E);
               E := Next_Formal (E);
            end loop;
         end if;

         --  Visible generic entity is callable within its own body.

         Set_Ekind (Gen_Id, Ekind (Nam));
         Set_Convention (Nam, Convention (Gen_Id));
         Check_Fully_Conformant (Nam, Gen_Id, Nam);

         --  If this is a compilation unit, it must be made visible
         --  explicitly, because the compilation of the declaration,
         --  unlike other library unit declarations, does not. If it
         --  is not a unit, the following is redundant but harmless.

         Set_Is_Immediately_Visible (Gen_Id);

         Set_Actual_Subtypes (N, Current_Scope);
         Analyze_Declarations (Declarations (N));
         Check_Completion;
         Analyze (Handled_Statement_Sequence (N));

         Save_Global_References (Original_Node (N));

         --  Prior to exiting the scope, include generic formals again
         --  (if any are present) in the set of local entities.

         if Present (First_Ent) then
            Set_First_Entity (Gen_Id, First_Ent);
         end if;

      end;

      End_Scope;

      --  Outside of its body, unit is generic again.

      Set_Ekind (Gen_Id, Kind);
      End_Generic;

   end Analyze_Generic_Subprogram_Body;

   -----------------------------
   -- Analyze_Operator_Symbol --
   -----------------------------

   --  An operator symbol such as "+" or "and" may appear in context where
   --  the literal denotes an entity name, such as  "+"(x, y) or in a
   --  context when it is just a string, as in  (conjunction = "or"). In
   --  these cases the parser generates this node, and the semantics does
   --  the disambiguation. Other such case are actuals in an instantiation,
   --  the generic unit in an instantiation, and pragma arguments.

   procedure Analyze_Operator_Symbol (N : Node_Id) is
      Par : constant Node_Id := Parent (N);

   begin
      if        (Nkind (Par) = N_Function_Call and then N = Name (Par))
        or else  Nkind (Par) = N_Function_Instantiation
        or else (Nkind (Par) = N_Indexed_Component and then N = Prefix (Par))
        or else (Nkind (Par) = N_Pragma_Argument_Association
                   and then not Is_Pragma_String_Literal (Par))
        or else  Nkind (Par) = N_Subprogram_Renaming_Declaration
        or else  Nkind (Par) = N_Attribute_Reference
      then
         Find_Direct_Name (N);

      else
         Change_Operator_Symbol_To_String_Literal (N);
         Analyze (N);
      end if;
   end Analyze_Operator_Symbol;

   -----------------------------------
   -- Analyze_Parameter_Association --
   -----------------------------------

   procedure Analyze_Parameter_Association (N : Node_Id) is
   begin
      Analyze (Explicit_Actual_Parameter (N));
   end Analyze_Parameter_Association;

   ----------------------------
   -- Analyze_Procedure_Call --
   ----------------------------

   procedure Analyze_Procedure_Call (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      P       : constant Node_Id    := Name (N);
      Actuals : constant List_Id    := Parameter_Associations (N);
      Actual  : Node_Id;
      New_N   : Node_Id;

      procedure Analyze_Call_And_Resolve;
      --  Do Analyze and Resolve calls for procedure call

      procedure Analyze_Call_And_Resolve is
      begin
         Analyze_Call (N);
         Resolve (N, Standard_Void_Type);
      end Analyze_Call_And_Resolve;

   --  Start of processing for Analyze_Procedure_Call

   begin
      --  The syntactic construct: PREFIX ACTUAL_PARAMETER_PART can denote
      --  a procedure call or an entry call. The prefix may denote an access
      --  to subprogram type, in which case an implicit dereference applies.
      --  If the prefix is an indexed component (without implicit defererence)
      --  then the construct denotes a call to a member of an entire family.
      --  If the prefix is a simple name, it may still denote a call to a
      --  parameterless member of an entry family. Resolution of these various
      --  interpretations is delicate.

      Analyze (P);

      --  If error analyzing prefix, then set Any_Type as result and return

      if Etype (P) = Any_Type then
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  Otherwise analyze the parameters

      if Present (Actuals) then
         Actual := First (Actuals);

         while Present (Actual) loop
            Analyze (Actual);
            Actual := Next (Actual);
         end loop;
      end if;

      --  Special processing for Elab_Spec and Elab_Body calls

      if Nkind (P) = N_Attribute_Reference
        and then (Attribute_Name (P) = Name_Elab_Spec
                   or else Attribute_Name (P) = Name_Elab_Body)
      then
         if Present (Actuals) then
            Error_Msg_N
              ("no parameters allowed for this call", First (Actuals));
            return;
         end if;

         Set_Etype (N, Standard_Void_Type);
         Set_Analyzed (N);

      elsif Is_Entity_Name (P)
        and then Is_Record_Type (Etype (Entity (P)))
        and then Remote_AST_I_Dereference (P)
      then
         return;

      elsif Is_Entity_Name (P)
        and then Ekind (Entity (P)) /= E_Entry_Family
      then
         Analyze_Call_And_Resolve;

      --  If the prefix is the simple name of an entry family, this is
      --  a parameterless call from within the task body itself.

      elsif Is_Entity_Name (P)
        and then Nkind (P) = N_Identifier
        and then Ekind (Entity (P)) = E_Entry_Family
        and then Present (Actuals)
        and then No (Next (First (Actuals)))
      then
         --  Can be call to parameterless entry family. What appears to be
         --  the sole argument is in fact the entry index. Rewrite prefix
         --  of node accordingly. Source representation is unchanged by this
         --  transformation.

         New_N :=
           Make_Indexed_Component (Loc,
             Prefix =>
               Make_Selected_Component (Loc,
                 Prefix => New_Occurrence_Of (Scope (Entity (P)), Loc),
                 Selector_Name => New_Occurrence_Of (Entity (P), Loc)),
             Expressions => Actuals);
         Set_Name (N, New_N);
         Set_Etype (New_N, Standard_Void_Type);
         Set_Parameter_Associations (N, No_List);
         Analyze_Call_And_Resolve;

      elsif Nkind (P) = N_Explicit_Dereference then
         if Ekind (Etype (P)) = E_Subprogram_Type then
            Analyze_Call_And_Resolve;
         else
            Error_Msg_N ("expect access to procedure in call", P);
         end if;

      --  The name can be a selected component or an indexed component
      --  that yields an access to subprogram. Such a prefix is legal if
      --  the call has parameter associations.

      elsif Is_Access_Type (Etype (P))
        and then Ekind (Designated_Type (Etype (P))) = E_Subprogram_Type
      then
         if Present (Actuals) then
            Analyze_Call_And_Resolve;
         else
            Error_Msg_N ("missing explicit dereference in call ", N);
         end if;

      --  If not an access to subprogram, then the prefix must resolve to
      --  the name of an entry, entry family, or protected operation.

      --  For the case of a simple entry call, P is a selected component
      --  where the prefix is the task and the selector name is the entry.
      --  A call to a protected procedure will have the same syntax.

      elsif Nkind (P) = N_Selected_Component
        and then (Ekind (Entity (Selector_Name (P))) = E_Entry
                    or else
                  Ekind (Entity (Selector_Name (P))) = E_Procedure)
      then
         Analyze_Call_And_Resolve;

      elsif Nkind (P) = N_Selected_Component
        and then Ekind (Entity (Selector_Name (P))) = E_Entry_Family
        and then Present (Actuals)
        and then No (Next (First (Actuals)))
      then
         --  Can be call to parameterless entry family. What appears to be
         --  the sole argument is in fact the entry index. Rewrite prefix
         --  of node accordingly. Source representation is unchanged by this
         --  transformation.

         New_N :=
           Make_Indexed_Component (Loc,
             Prefix => New_Copy (P),
             Expressions => Actuals);
         Set_Name (N, New_N);
         Set_Etype (New_N, Standard_Void_Type);
         Set_Parameter_Associations (N, No_List);
         Analyze_Call_And_Resolve;

      --  For the case of a reference to an element of an entry family, P is
      --  an indexed component whose prefix is a selected component (task and
      --  entry family), and whose index is the entry family index.

      elsif Nkind (P) = N_Indexed_Component
        and then Nkind (Prefix (P)) = N_Selected_Component
        and then Ekind (Entity (Selector_Name (Prefix (P)))) = E_Entry_Family
      then
         Analyze_Call_And_Resolve;

      --  If the prefix is the name of an entry family, it is a call from
      --  within the task body itself.

      elsif Nkind (P) = N_Indexed_Component
        and then Nkind (Prefix (P)) = N_Identifier
        and then Ekind (Entity (Prefix (P))) = E_Entry_Family
      then
         New_N :=
           Make_Selected_Component (Loc,
             Prefix => New_Occurrence_Of (Scope (Entity (Prefix (P))), Loc),
             Selector_Name => New_Occurrence_Of (Entity (Prefix (P)), Loc));
         Rewrite_Substitute_Tree (Prefix (P), New_N);
         Analyze (P);
         Analyze_Call_And_Resolve;

      --  Anything else is an error.

      else
         Error_Msg_N ("Invalid procedure or entry call", N);
      end if;
   end Analyze_Procedure_Call;

   ------------------------------
   -- Analyze_Return_Statement --
   ------------------------------

   procedure Analyze_Return_Statement (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      Expr     : Node_Id;
      Scope_Id : Entity_Id;
      Kind     : Entity_Kind;
      R_Type   : Entity_Id;

   begin
      --  Find subprogram or accept statement enclosing the return statement

      for J in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (J).Entity;
         exit when Ekind (Scope_Id) /= E_Block and then
                   Ekind (Scope_Id) /= E_Loop;
      end loop;

      Kind := Ekind (Scope_Id);
      Expr := Expression (N);

      if Kind /= E_Function
        and then Kind /= E_Generic_Function
        and then Kind /= E_Procedure
        and then Kind /= E_Generic_Procedure
        and then Kind /= E_Entry
        and then Kind /= E_Entry_Family
      then
         Error_Msg_N ("illegal context for return statement", N);

      elsif Present (Expr) then
         if Kind = E_Function or else Kind = E_Generic_Function then
            Set_Return_Present (Scope_Id);
            R_Type := Etype (Scope_Id);
            Set_Return_Type (N, R_Type);
            Analyze_And_Resolve (Expr, R_Type);
            Apply_Constraint_Check (Expr, R_Type);

            --  ??? a real accessibility check is needed when
            --  returning by reference. For now just check the most obvious
            --  cases

            if Is_Return_By_Reference_Type (Etype (Scope_Id))
              and then Is_Entity_Name (Expr)
            then
               if Scope (Entity (Expr)) = Scope_Id
                 or else Scope (Scope (Entity (Expr))) = Scope_Id
                 or else Scope (Scope (Scope (Entity (Expr)))) = Scope_Id
               then
                  Rewrite_Substitute_Tree (N,
                    Make_Raise_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (Standard_Program_Error, Loc)));

                  Analyze (N);

                  Error_Msg_N
                    ("cannot return a local value by reference?", N);
                  Error_Msg_NE
                    ("& will be raised at runtime?!",
                     N, Standard_Program_Error);
               end if;
            end if;

         elsif Kind = E_Procedure or else Kind = E_Generic_Procedure then
            Error_Msg_N ("procedure cannot return value (use function)", N);

         else
            Error_Msg_N ("accept statement cannot return value", N);
         end if;

      --  No expression present

      else
         if Kind = E_Function or Kind = E_Generic_Function then
            Error_Msg_N ("missing expression in return from function", N);
         end if;

         if (Ekind (Scope_Id) = E_Procedure
              or else Ekind (Scope_Id) = E_Generic_Procedure)
           and then  No_Return (Scope_Id)
         then
            Error_Msg_N
              ("RETURN statement not allowed (No_Return)", N);
         end if;
      end if;
   end Analyze_Return_Statement;

   ------------------
   -- Analyze_Spec --
   ------------------

   function Analyze_Spec (N : Node_Id) return Entity_Id is
      Designator : constant Entity_Id := Defining_Entity (N);
      Formals    : constant List_Id   := Parameter_Specifications (N);
      Typ        : Entity_Id;

   begin
      if Nkind (N) = N_Function_Specification then
         Set_Ekind (Designator, E_Function);
         Set_Mechanism (Designator, Default_Mechanism);
         Find_Type (Subtype_Mark (N));
         Typ := Entity (Subtype_Mark (N));
         Set_Etype (Designator, Typ);

         if (Ekind (Typ) = E_Incomplete_Type
              or else (Is_Class_Wide_Type (Typ)
                        and then Ekind (Root_Type (Typ)) = E_Incomplete_Type))
         then
            Error_Msg_N ("invalid use of incomplete type", Subtype_Mark (N));
         end if;

      else
         Set_Ekind (Designator, E_Procedure);
         Set_Etype (Designator, Standard_Void_Type);
      end if;

      if Present (Formals) then
         Set_Scope (Designator, Current_Scope);
         New_Scope (Designator);
         Process_Formals (Designator, Formals, N);
         End_Scope;
      end if;

      if Nkind (N) = N_Function_Specification then
         if Nkind (Designator) = N_Defining_Operator_Symbol then
            Valid_Operator_Definition (Designator);
         end if;

         May_Need_Actuals (Designator);

         if Is_Abstract (Etype (Designator))
           and then Nkind (Parent (N)) /= N_Abstract_Subprogram_Declaration
         then
            Error_Msg_N
              ("function that returns abstract type must be abstract", N);
         end if;
      end if;

      return Designator;
   end Analyze_Spec;

   -----------------------------
   -- Analyze_Subprogram_Body --
   -----------------------------

   --  This procedure is called for regular subprogram bodies, generic bodies,
   --  and for subprogram stubs of both kinds. In the case of stubs, only the
   --  specification matters, and is used to create a proper declaration for
   --  the subprogram, or to perform conformance checks.

   procedure Analyze_Subprogram_Body (N : Node_Id) is
      Body_Spec : constant Node_Id    := Specification (N);
      Body_Id   : constant Entity_Id  := Defining_Entity (Body_Spec);
      Gen_Id    : constant Entity_Id  := Current_Entity_In_Scope (Body_Id);

      HSS         : Node_Id;
      Subp        : Entity_Id;
      Spec_Id     : Entity_Id;
      Last_Formal : Entity_Id;
      Conformant  : Boolean;
      Missing_Ret : Boolean;

   begin
      if Debug_Flag_C then
         Write_Str ("====  Compiling subprogram body ");
         Write_Name (Chars (Body_Id));
         Write_Str (" from ");
         Write_Location (Sloc (N));
         Write_Eol;
      end if;

      Trace_Scope (N, Body_Id, " Analyze subprogram");

      --  Generic subprograms are handled separately. They always have
      --  a generic specification. Determine whether current scope has
      --  a previous declaration.

      --  If the subprogram body is defined within an instance of the
      --  same name, the instance appears as a package renaming, and
      --  will be hidden within the subprogram.

      if Present (Gen_Id)
        and then not Is_Overloadable (Gen_Id)
        and then (Nkind (Parent (Gen_Id)) /= N_Package_Renaming_Declaration
                   or else Comes_From_Source (Gen_Id))
      then
         if Ekind (Gen_Id) = E_Generic_Procedure
           or else Ekind (Gen_Id) = E_Generic_Function
         then
            Analyze_Generic_Subprogram_Body (N, Gen_Id);
            return;

         else
            --  Previous entity conflicts with subprogram name.
            --  Attempting to enter name will post error.

            Enter_Name (Body_Id);
            return;
         end if;

      --  Non-generic case, find the subprogram declaration, if one was
      --  seen, or enter new overloaded entity in the current scope.
      --  If the current_entity is the body_id itself, the unit is being
      --  analyzed as part of the context of one of its subunits. No need
      --  to redo the analysis.

      elsif Gen_Id = Body_Id
        and then Has_Completion (Body_Id)
      then
         return;

      else
         Subp := Analyze_Spec (Body_Spec);

         if Nkind (N) = N_Subprogram_Body_Stub
           or else No (Corresponding_Spec (N))
         then
            Spec_Id := Find_Corresponding_Spec (N);

            --  A subprogram body should cause freezing of its own
            --  declaration, but if there was no previous explicit
            --  declaration, then the subprogram will get frozen too
            --  late (there may be code within the body that depends
            --  on the subprogram having been frozen, such as uses of
            --  extra formals), so we force it to be frozen here.
            --  Same holds if the body and the spec are compilation units.

            if No (Spec_Id) then
               Freeze_Before (N, Subp);

            elsif Nkind (Parent (N)) = N_Compilation_Unit then
               Freeze_Before (N, Spec_Id);
            end if;
         else
            Spec_Id := Corresponding_Spec (N);
         end if;
      end if;

      if No (Spec_Id)
        and then Comes_From_Source (N)
        and then Is_Protected_Type (Current_Scope)
      then
         --  Fully private operation in the body of the protected type. We
         --  must create a declaration for the subprogram, in order to attach
         --  the protected subprogram that will be used in internal calls.

         declare
            Loc      : constant Source_Ptr := Sloc (N);
            Decl     : Node_Id;
            Plist    : List_Id;
            Formal   : Entity_Id;
            New_Spec : Node_Id;

         begin
            Plist := New_List;
            Formal := First_Formal (Subp);

            while Present (Formal) loop
               Append
                 (Make_Parameter_Specification (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Sloc (Formal),
                       Chars => Chars (Formal)),
                   In_Present  => In_Present (Parent (Formal)),
                   Out_Present => Out_Present (Parent (Formal)),
                   Parameter_Type =>
                     New_Reference_To (Etype (Formal), Loc)),
                 Plist);

               Formal := Next_Formal (Formal);
            end loop;

            if Nkind (Body_Spec) = N_Procedure_Specification then
               New_Spec :=
                 Make_Procedure_Specification (Loc,
                    Defining_Unit_Name =>
                      Make_Defining_Identifier (Sloc (Subp),
                        Chars => Chars (Subp)),
                    Parameter_Specifications => Plist);
            else
               New_Spec :=
                 Make_Function_Specification (Loc,
                    Defining_Unit_Name =>
                      Make_Defining_Identifier (Sloc (Subp),
                        Chars => Chars (Subp)),
                    Parameter_Specifications => Plist,
                    Subtype_Mark => New_Occurrence_Of (Etype (Subp), Loc));
            end if;

            Decl :=
              Make_Subprogram_Declaration (Loc,
                Specification => New_Spec);
            Insert_Before (N, Decl);
            Analyze (Decl);
            Spec_Id := Defining_Unit_Name (New_Spec);
            Set_Has_Completion (Spec_Id);
         end;
      end if;

      --  Place subprogram on scope stack, and make formals visible. If there
      --  is a spec, the visible entity remains that of the spec. The defining
      --  entity for the body is entered in the chain of entities in that case,
      --  to insure that it is instantiated if it appears in  a generic unit.

      if Present (Spec_Id) then
         if Is_Abstract (Spec_Id) then
            Error_Msg_N ("an abstract subprogram cannot have a body", N);
            return;
         else
            Set_Convention (Subp, Convention (Spec_Id));
            Set_Has_Completion (Spec_Id);

            if Is_Protected_Type (Scope (Spec_Id)) then
               Set_Privals_Chain (Spec_Id, New_Elmt_List);
            end if;

            Check_Conformance
              (Subp, Spec_Id, Fully_Conformant, True, Conformant, Subp);

            --  If the body is not fully conformant, we have to decide if we
            --  should analyze it or not. If it has a really messed up profile
            --  then we probably should not analyze it, since we will get too
            --  many bogus messages.

            --  Our decision is to go ahead in the non-fully conformant case
            --  only if it is at least mode conformant with the spec. Note
            --  that the call to Check_Fully_Conformant has issued the proper
            --  error messages to complain about the lack of conformance.

            if not Conformant
              and then not Mode_Conformant (Subp, Spec_Id)
            then
               return;
            end if;
         end if;

         if Nkind (N) /= N_Subprogram_Body_Stub then
            Set_Corresponding_Spec (N, Spec_Id);
            Install_Formals (Spec_Id);
            Last_Formal := Last_Entity (Spec_Id);
            New_Scope (Spec_Id);
         end if;

         Set_Corresponding_Body (Get_Declaration_Node (Spec_Id), Subp);

      else
         if Style_Check and then Comes_From_Source (Body_Id) then
            Style.Body_With_No_Spec (N);
         end if;

         New_Overloaded_Entity (Subp);

         if Nkind (N) /= N_Subprogram_Body_Stub then
            Set_Acts_As_Spec (N);
            Install_Formals (Subp);
            New_Scope (Subp);
         end if;

      end if;

      Set_Has_Completion (Subp);

      if Nkind (N) = N_Subprogram_Body_Stub then
         return;
      end if;

      --  Here we have a real body, not a stub

      HSS := Handled_Statement_Sequence (N);
      Set_Actual_Subtypes (N, Current_Scope);
      Analyze_Declarations (Declarations (N));
      Check_Completion;
      Analyze (HSS);
      End_Scope;

      --  If we have a separate spec, then the analysis of the declarations
      --  caused the entities in the body to be chained to the spec id, but
      --  we want them chained to the body id. Only the formal parameters
      --  end up chained to the spec id in this case.

      if Present (Spec_Id) then
         if Present (Last_Formal) then
            Set_Next_Entity
              (Last_Entity (Subp), Next_Entity (Last_Formal));
            Set_Next_Entity (Last_Formal, Empty);
            Set_Last_Entity (Subp, Last_Entity (Spec_Id));
            Set_Last_Entity (Spec_Id, Last_Formal);

         else
            Set_First_Entity (Subp, First_Entity (Spec_Id));
            Set_Last_Entity  (Subp, Last_Entity (Spec_Id));
            Set_First_Entity (Spec_Id, Empty);
            Set_Last_Entity  (Spec_Id, Empty);
         end if;
      end if;

      --  If function, check return statements

      if Ekind (Body_Id) = E_Function
        or else Ekind (Body_Id) = E_Generic_Function
      then
         if (Present (Spec_Id) and then Return_Present (Spec_Id))
           or else (No (Spec_Id) and then Return_Present (Subp))
         then
            Check_Returns (HSS, 'F', Missing_Ret);

            if Missing_Ret then
               if Present (Spec_Id) then
                  Set_Has_Missing_Return (Spec_Id);
               else
                  Set_Has_Missing_Return (Body_Id);
               end if;
            end if;

         else
            Error_Msg_N ("missing RETURN statement in function body", N);
         end if;

      --  If procedure with No_Return, check returns

      elsif (Ekind (Body_Id) = E_Procedure
               or else Ekind (Body_Id) = E_Generic_Procedure)
        and then Present (Spec_Id)
        and then No_Return (Spec_Id)
      then
         Check_Returns (HSS, 'P', Missing_Ret);
      end if;

      --  Don't worry about checking for variables that are never modified
      --  if the first statement of the body is a raise statement, since
      --  we assume this is some kind of stub.

      if Nkind (First (Statements (HSS))) = N_Raise_Statement then
         return;
      end if;

      --  Check for variables that are never modified

      declare
         E1, E2 : Entity_Id;

      begin
         --  If there is a separate spec, then transfer any Not_Assigned flags
         --  from out parameters to the corresponding entities in the body.
         --  The reason we do that is we want to post error flags on the body
         --  entities, not the spec entities.

         if Present (Spec_Id) then
            E1 := First_Entity (Spec_Id);

            while Present (E1) loop
               if Ekind (E1) = E_Out_Parameter then
                  E2 := First_Entity (Subp);

                  while Chars (E1) /= Chars (E2) loop
                     E2 := Next_Entity (E2);
                  end loop;

                  Set_Not_Assigned (E2, Not_Assigned (E1));
               end if;

               E1 := Next_Entity (E1);
            end loop;
         end if;

         --  Now flag unset variables in the body

         Check_Unset_Variables (Subp);
      end;

   end Analyze_Subprogram_Body;

   ---------------------------------
   -- Is_Non_Overriding_Operation --
   ---------------------------------

   function Is_Non_Overriding_Operation
     (Prev_E : Entity_Id;
      New_E  : Entity_Id)
      return Boolean
   is
      Formal : Entity_Id;
      F_Typ  : Entity_Id;
      G_Typ  : Entity_Id := Empty;

   begin
      if Ekind (Current_Scope) = E_Package
        and then Is_Generic_Instance (Current_Scope)
        and then In_Private_Part (Current_Scope)
        and then Comes_From_Source (New_E)
      then

         --  We examine the formals of the inherited operation, to determine
         --  whether their type is derived from (the instance of) a generic
         --  type.
         --  Missing case: access parameters, dipatching return type ???

         Formal := First_Formal (Prev_E);

         while Present (Formal) loop
            F_Typ := Etype (Formal);

            if Is_Derived_Type (F_Typ)
              and then Nkind (Parent (F_Typ)) = N_Full_Type_Declaration
            then
               G_Typ :=
                 Entity
                   (Subtype_Indication (Type_Definition (Parent (F_Typ))));

               if Nkind (Parent (G_Typ)) = N_Subtype_Declaration
                 and then Present (Generic_Parent_Type (Parent (G_Typ)))
               then
                  G_Typ := Generic_Parent_Type (Parent (G_Typ));
                  exit;
               end if;
            end if;

            Formal := Next_Formal (Formal);
         end loop;

         if No (G_Typ) then
            return False;
         end if;

         --  If the generic type is a private type, then the original
         --  operation was not overriding in the generic, because there was
         --  no primitive operation to override. If the generic type is a
         --  formal derived type, check whether it has a primitive operation
         --  that could have been overriden in the generic.

         if Nkind (Parent (G_Typ)) = N_Formal_Type_Declaration then
            if Nkind (Formal_Type_Definition (Parent (G_Typ))) =
              N_Formal_Private_Type_Definition
            then
               return True;

            elsif Nkind (Formal_Type_Definition (Parent (G_Typ))) =
              N_Formal_Derived_Type_Definition
            then
               return False;   -- for now. Refinement ???

            else
               return False;
            end if;
         else
            return False;
         end if;
      else
         return False;
      end if;
   end Is_Non_Overriding_Operation;

   -------------------
   -- Check_Returns --
   -------------------

   procedure Check_Returns
     (HSS  : Node_Id;
      Mode : Character;
      Err  : out Boolean)
   is
      Handler : Node_Id;

      procedure Check_Statement_Sequence (L : List_Id);
      --  Internal recursive procedure to check a list of statements for proper
      --  termination by a return statement (or a transfer of control or a
      --  compound statement that is itself internally properly terminated).

      ------------------------------
      -- Check_Statement_Sequence --
      ------------------------------

      procedure Check_Statement_Sequence (L : List_Id) is
         Last_Stm : Node_Id;
         Kind     : Node_Kind;
         No_Warn  : Boolean;

      begin
         No_Warn := False;

         --  Get last real statement, not counting pragmas, and also not
         --  counting calls to SS_Release (can happen after Raise_Exception)

         Last_Stm := Last (L);

         while Nkind (Last_Stm) = N_Pragma
           or else
             (Nkind (Last_Stm) = N_Procedure_Call_Statement
                and then
              Nkind (Name (Last_Stm)) = N_Identifier
                and then
              Is_RTE (Entity (Name (Last_Stm)), RE_SS_Release))
         loop
            Last_Stm := Prev (Last_Stm);
         end loop;

         Kind := Nkind (Last_Stm);

         --  Transfer of control, OK. Note that in the No_Return procedure
         --  case, we already diagnosed any explicit return statements, so
         --  we can treat them as OK in this context.

         if Kind = N_Goto_Statement
           or else Kind = N_Return_Statement
           or else Kind = N_Raise_Statement
         then
            return;

         --  Check cases of explicit non-indirect procedure calls

         elsif Kind = N_Procedure_Call_Statement
           and then Is_Entity_Name (Name (Last_Stm))
         then
            --  Call of procedure to which No_Return applies is OK

            if No_Return (Entity (Name (Last_Stm))) then
               return;

            --  Call of procedure named Raise_Exception is treated specially.
            --  We suppress the warning in this case since it is likely that
            --  the programmer really does not expect to deal with the case
            --  of Null_Occurrence, and thus would find a warning about a
            --  return curious. We are talking here about Raise_Exception
            --  defined in Ada.Exceptions, but it is not terrible to do the
            --  kill of the warnings for a user routine of this name!

            elsif Chars (Entity (Name (Last_Stm))) = Name_Raise_Exception then
               No_Warn := True;
            end if;

         --  If statement, need to look inside if there is an else and check
         --  each constituent statement sequence for proper termination.

         elsif Kind = N_If_Statement
           and then Present (Else_Statements (Last_Stm))
         then
            Check_Statement_Sequence (Then_Statements (Last_Stm));
            Check_Statement_Sequence (Else_Statements (Last_Stm));

            if Present (Elsif_Parts (Last_Stm)) then
               declare
                  Elsif_Part : Node_Id := First (Elsif_Parts (Last_Stm));

               begin
                  while Present (Elsif_Part) loop
                     Check_Statement_Sequence (Then_Statements (Elsif_Part));
                     Elsif_Part := Next (Elsif_Part);
                  end loop;
               end;
            end if;

            return;

         --  Case statement, check each case for proper termination

         elsif Kind = N_Case_Statement then
            declare
               Case_Alt : Node_Id;

            begin
               Case_Alt := First_Non_Pragma (Alternatives (Last_Stm));
               while Present (Case_Alt) loop
                  Check_Statement_Sequence (Statements (Case_Alt));
                  Case_Alt := Next_Non_Pragma (Case_Alt);
               end loop;
            end;

            return;

         --  Block statement, check its handled sequence of statements

         elsif Kind = N_Block_Statement then
            declare
               Err1 : Boolean;

            begin
               Check_Returns
                 (Handled_Statement_Sequence (Last_Stm), Mode, Err1);

               if Err1 then
                  Err := True;
               end if;

               return;
            end;

         --  Loop statement. If there is an iteration scheme, we can definitely
         --  fall out of the loop. Similarly if there is an exit statement, we
         --  can fall out. In either case we need a following return.

         elsif Kind = N_Loop_Statement then
            if Present (Iteration_Scheme (Last_Stm))
              or else Has_Exit (Entity (Identifier (Last_Stm)))
            then
               null;

            --  A loop with no exit statement or iteration scheme if either
            --  an inifite loop, or it has some other exit (raise/return).
            --  In either case, no warning is required.

            else
               return;
            end if;

         --  Timed entry call, check entry call and delay alternatives

         --  Note: in expanded code, the timed entry call has been converted
         --  to a set of expanded statements on which the check will work
         --  correctly in any case.

         elsif Kind = N_Timed_Entry_Call then
            declare
               ECA : constant Node_Id := Entry_Call_Alternative (Last_Stm);
               DCA : constant Node_Id := Delay_Alternative      (Last_Stm);

            begin
               --  If statement sequence of entry call alternative is missing,
               --  then we can definitely fall through, and we post the error
               --  message on the entry call alternative itself.

               if No (Statements (ECA)) then
                  Last_Stm := ECA;

               --  If statement sequence of delay alternative is missing, then
               --  we can definitely fall through, and we post the error
               --  message on the delay alternative itself.

               --  Note: if both ECA and DCA are missing the return, then we
               --  post only one message, should be enough to fix the bugs.
               --  If not we will get a message next time on the DCA when the
               --  ECA is fixed!

               elsif No (Statements (DCA)) then
                  Last_Stm := DCA;

               --  Else check both statement sequences

               else
                  Check_Statement_Sequence (Statements (ECA));
                  Check_Statement_Sequence (Statements (DCA));
                  return;
               end if;
            end;

         --  Conditional entry call, check entry call and else part

         --  Note: in expanded code, the conditional entry call has been
         --  converted to a set of expanded statements on which the check
         --  will work correctly in any case.

         elsif Kind = N_Conditional_Entry_Call then
            declare
               ECA : constant Node_Id := Entry_Call_Alternative (Last_Stm);

            begin
               --  If statement sequence of entry call alternative is missing,
               --  then we can definitely fall through, and we post the error
               --  message on the entry call alternative itself.

               if No (Statements (ECA)) then
                  Last_Stm := ECA;

               --  Else check statement sequence and else part

               else
                  Check_Statement_Sequence (Statements (ECA));
                  Check_Statement_Sequence (Else_Statements (Last_Stm));
                  return;
               end if;
            end;
         end if;

         --  If we fall through, issue appropriate message

         if Mode = 'F' then

            if not No_Warn then
               Error_Msg_N
                 ("?RETURN statement missing following this statement!",
                  Last_Stm);
               Error_Msg_N
                 ("\?Program_Error may be raised at runtime",
                  Last_Stm);
            end if;

            --  Note: we set Err even though we have not issued a warning
            --  because we still have a case of a missing return. This is
            --  an extremely marginal case, probably will never be noticed
            --  but we might as well get it right.

            Err := True;

         else
            Error_Msg_N
              ("implied return after this statement not allowed (No_Return)",
               Last_Stm);
         end if;
      end Check_Statement_Sequence;

   --  Start of processing for Check_Returns

   begin
      Err := False;
      Check_Statement_Sequence (Statements (HSS));

      if Present (Exception_Handlers (HSS)) then
         Handler := First_Non_Pragma (Exception_Handlers (HSS));
         while Present (Handler) loop
            Check_Statement_Sequence (Statements (Handler));
            Handler := Next_Non_Pragma (Handler);
         end loop;
      end if;
   end Check_Returns;

   ------------------------------------
   -- Analyze_Subprogram_Declaration --
   ------------------------------------

   procedure Analyze_Subprogram_Declaration (N : Node_Id) is
      Designator : constant Entity_Id := Analyze_Spec (Specification (N));
      ELU        : constant Entity_Id := Current_Scope;
      Pure_Flag  : Boolean;
      RCI_Flag   : Boolean;
      RT_Flag    : Boolean;

   --  Start of processing for Analyze_Subprogram_Declaration

   begin
      --  Check for RCI unit subprogram declarations against in-lined
      --  subprograms and subprograms having access parameter or limited
      --  parameter without Read and Write (RM E.2.3(12-13)).

      Validate_RCI_Subprogram_Declaration (N);

      Trace_Scope
        (N,
         Defining_Entity (N),
         " Analyze subprogram spec. ");

      if Debug_Flag_C then
         Write_Str ("====  Compiling subprogram spec ");
         Write_Name (Chars (Designator));
         Write_Str (" from ");
         Write_Location (Sloc (N));
         Write_Eol;
      end if;

      New_Overloaded_Entity (Designator);
      Check_Delayed_Subprogram (Designator);
      Set_Suppress_Elaboration_Checks (Designator,
        Elaboration_Checks_Suppressed (Designator));

      --  Entities declared in Pure unit should be set Is_Pure
      --  Since 'Partition_ID cannot be applied to such an entity
      --  Subprogram declared in RCI unit should be set
      --  Is_Remote_Call_Interface, used to verify remote call.

      if ELU /= Standard_Standard then
         Pure_Flag := Is_Pure (ELU);
         Set_Is_Pure (Designator, Pure_Flag);
         RCI_Flag := Is_Remote_Call_Interface (ELU);
         Set_Is_Remote_Call_Interface (Designator, RCI_Flag);
         RT_Flag := Is_Remote_Types (ELU);
         Set_Is_Remote_Types (Designator, RT_Flag);
      end if;

      --  For a compilation unit, set body required. This flag will only be
      --  reset if a valid Import or Interface pragma is processed later on.

      if Nkind (Parent (N)) = N_Compilation_Unit then
         Set_Body_Required (Parent (N), True);
      end if;

   end Analyze_Subprogram_Declaration;

   -----------------------
   -- Check_Conformance --
   -----------------------

   procedure Check_Conformance
     (New_Id   : Entity_Id;
      Old_Id   : Entity_Id;
      Ctype    : Conformance_Type;
      Errmsg   : Boolean;
      Conforms : out Boolean;
      Err_Loc  : Node_Id := Empty;
      Get_Inst : Boolean := False)
   is
      Old_Type   : constant Entity_Id := Etype (Old_Id);
      New_Type   : constant Entity_Id := Etype (New_Id);
      Old_Formal : Entity_Id;
      New_Formal : Entity_Id;

      procedure Conformance_Error (Msg : String; N : Node_Id := New_Id);
      --  Post error message for conformance error on given node.
      --  Two messages are output. The first points to the previous
      --  declaration with a general "no conformance" message.
      --  The second is the detailed reason, supplied as Msg. The
      --  parameter N provide information for a possible & insertion
      --  in the message, and also provides the location for posting
      --  the message in the absence of a specified Err_Loc location.

      -----------------------
      -- Conformance_Error --
      -----------------------

      procedure Conformance_Error (Msg : String; N : Node_Id := New_Id) is
         Enode : Node_Id;

      begin
         Conforms := False;

         if Errmsg then
            if No (Err_Loc) then
               Enode := N;
            else
               Enode := Err_Loc;
            end if;

            Error_Msg_Sloc := Sloc (Old_Id);

            case Ctype is
               when Type_Conformant =>
                  Error_Msg_N
                    ("not type conformant with declaration#!", Enode);

               when Mode_Conformant =>
                  Error_Msg_N
                    ("not mode conformant with declaration#!", Enode);

               when Subtype_Conformant =>
                  Error_Msg_N
                    ("not subtype conformant with declaration#!", Enode);

               when Fully_Conformant =>
                  Error_Msg_N
                    ("not fully conformant with declaration#!", Enode);
            end case;

            Error_Msg_NE (Msg, Enode, N);
         end if;
      end Conformance_Error;


   --  Start of processing for Check_Conformance

   begin
      Conforms := True;

      --  We need a special case for operators, since they don't
      --  appear explicitly.

      if Ctype = Type_Conformant then
         if Ekind (New_Id) = E_Operator
           and then Operator_Matches_Spec (New_Id, Old_Id)
         then
            return;
         end if;
      end if;

      --  If both are functions/operators, check return types conform

      if Old_Type /= Standard_Void_Type
        and then New_Type /= Standard_Void_Type
      then
         if not Conforming_Types (Old_Type, New_Type, Ctype, Get_Inst) then
            Conformance_Error ("return type does not match!", New_Id);
            return;
         end if;

      --  If either is a function/operator and the other isn't, error

      elsif Old_Type /= Standard_Void_Type
        or else New_Type /= Standard_Void_Type
      then
         Conformance_Error ("functions can only match functions!", New_Id);
         return;
      end if;

      --  In subtype conformant case, conventions must match (RM 6.3.1(16))

      if Ctype >= Subtype_Conformant then
         if Convention (Old_Id) /= Convention (New_Id) then
            Conformance_Error ("calling conventions do not match!");
            return;
         end if;
      end if;

      --  Deal with parameters

      --  Note: we use the entity information, rather than going directly
      --  to the specification in the tree. This is not only simpler, but
      --  absolutely necessary for some cases of conformance tests between
      --  operators, where the declaration tree simply does not exist!

      Old_Formal := First_Formal (Old_Id);
      New_Formal := First_Formal (New_Id);

      while Present (Old_Formal) and then Present (New_Formal) loop

         --  Types must always match

         if not Conforming_Types
                  (Etype (Old_Formal), Etype (New_Formal), Ctype, Get_Inst)
         then
            Conformance_Error ("type of & does not match!", New_Formal);
            return;
         end if;

         --  For mode conformance, mode must match

         if Ctype >= Mode_Conformant
           and then Parameter_Mode (Old_Formal) /= Parameter_Mode (New_Formal)
         then
            Conformance_Error ("mode of & does not match!", New_Formal);
            return;
         end if;

         --  Full conformance checks

         if Ctype = Fully_Conformant then

            --  Names must match

            if Chars (Old_Formal) /= Chars (New_Formal) then
               Conformance_Error ("name & does not match!", New_Formal);
               return;

            --  And default expressions for in parameters

            elsif Parameter_Mode (Old_Formal) = E_In_Parameter then
               declare
                  NewD : constant Boolean :=
                           Present (Default_Value (New_Formal));
                  OldD : constant Boolean :=
                           Present (Default_Value (Old_Formal));
               begin
                  if NewD or OldD then

                     --  The old default value has been analyzed and expanded,
                     --  because the current full declaration will have frozen
                     --  everything before. The new default values have not
                     --  been expanded, so expand now to check conformance.

                     if NewD then
                        New_Scope (New_Id);
                        Analyze_Default_Expression
                         (Default_Value (New_Formal), Etype (New_Formal));
                        End_Scope;
                     end if;

                     if not (NewD and OldD)
                       or else not Fully_Conformant_Expressions
                                    (Default_Value (Old_Formal),
                                     Default_Value (New_Formal))
                     then
                        Conformance_Error
                          ("default expression for & does not match!",
                           New_Formal);
                        return;
                     end if;
                  end if;
               end;
            end if;
         end if;

         --  A couple of special checks for Ada 83 mode. These checks are
         --  skipped if either entity is an operator in package Standard.
         --  or if either old or new instance is not from the source program.

         if Ada_83
           and then Sloc (Old_Id) > Standard_Location
           and then Sloc (New_Id) > Standard_Location
           and then Comes_From_Source (Old_Id)
           and then Comes_From_Source (New_Id)
         then
            declare
               Old_Param : constant Node_Id := Declaration_Node (Old_Formal);
               New_Param : constant Node_Id := Declaration_Node (New_Formal);

            begin
               --  Explicit IN must be present or absent in both cases. This
               --  test is required only in the full conformance case.

               if In_Present (Old_Param) /= In_Present (New_Param)
                 and then Ctype = Fully_Conformant
               then
                  Conformance_Error
                    ("(Ada 83) IN must appear in both declarations",
                     New_Formal);
                  return;
               end if;

               --  Grouping (use of comma in param lists) must be the same
               --  This is where we catch a misconformance like:

               --    A,B : Integer
               --    A : Integer; B : Integer

               --  which are represented identically in the tree except
               --  for the setting of the flags More_Ids and Prev_Ids.

               if More_Ids (Old_Param) /= More_Ids (New_Param)
                 or else Prev_Ids (Old_Param) /= Prev_Ids (New_Param)
               then
                  Conformance_Error
                    ("grouping of & does not match!", New_Formal);
                  return;
               end if;
            end;
         end if;

         Old_Formal := Next_Formal (Old_Formal);
         New_Formal := Next_Formal (New_Formal);
      end loop;

      if Present (Old_Formal) then
         Conformance_Error ("too few parameters!");
         return;

      elsif Present (New_Formal) then
         Conformance_Error ("too many parameters!", New_Formal);
         return;
      end if;

   end Check_Conformance;

   ------------------------------
   -- Check_Delayed_Subprogram --
   ------------------------------

   procedure Check_Delayed_Subprogram (Designator : Entity_Id) is
      F : Entity_Id;

      procedure Possible_Freeze (T : Entity_Id);
      --  T is the type of either a formal parameter or of the return type.
      --  If T is not yet frozen and needs a delayed freeze, then the
      --  subprogram itself must be delayed.

      procedure Possible_Freeze (T : Entity_Id) is
      begin
         if Has_Delayed_Freeze (T)
           and then not Is_Frozen (T)
         then
            Set_Has_Delayed_Freeze (Designator);

         elsif Is_Access_Type (T)
           and then Has_Delayed_Freeze (Designated_Type (T))
           and then not Is_Frozen (Designated_Type (T))
         then
            Set_Has_Delayed_Freeze (Designator);
         end if;
      end Possible_Freeze;

   --  Start of processing for Check_Delayed_Subprogram

   begin
      --  Never need to freeze abstract subprogram

      if Is_Abstract (Designator) then
         null;
      else

         --  Need delayed freeze if return type itself needs a delayed
         --  freeze and is not yet frozen.

         Possible_Freeze (Etype (Designator));
         Possible_Freeze (Base_Type (Etype (Designator))); -- needed ???

         --  Need delayed freeze if any of the formal types themselves need
         --  a delayed freeze and are not yet frozen.

         F := First_Formal (Designator);
         while Present (F) loop
            Possible_Freeze (Etype (F));
            Possible_Freeze (Base_Type (Etype (F))); -- needed ???
            F := Next_Formal (F);
         end loop;
      end if;

      --  Mark functions that return by reference. Note that it cannot be
      --  done for delayed_freeze subprograms because the underlying
      --  returned type may not be known yet (for private types)

      if not Has_Delayed_Freeze (Designator)
        and then Expander_Active
      then
         declare
            Typ  : constant Entity_Id := Etype (Designator);
            Utyp : constant Entity_Id := Underlying_Type (Typ);

         begin
            if Is_Return_By_Reference_Type (Typ) then
               Set_Returns_By_Ref (Designator);

            elsif Present (Utyp) and then Controlled_Type (Utyp) then
               Set_Returns_By_Ref (Designator);
            end if;
         end;
      end if;
   end Check_Delayed_Subprogram;

   ------------------------------------
   -- Check_Discriminant_Conformance --
   ------------------------------------

   procedure Check_Discriminant_Conformance
     (N        : Node_Id;
      Prev     : Entity_Id;
      Prev_Loc : Node_Id)
   is
      Old_Discr      : Entity_Id := First_Discriminant (Prev);
      New_Discr      : Node_Id   := First (Discriminant_Specifications (N));
      New_Discr_Id   : Entity_Id;
      New_Discr_Type : Entity_Id;

      procedure Conformance_Error (Msg : String; N : Node_Id);
      --  Post error message for conformance error on given node.
      --  Two messages are output. The first points to the previous
      --  declaration with a general "no conformance" message.
      --  The second is the detailed reason, supplied as Msg. The
      --  parameter N provide information for a possible & insertion
      --  in the message.

      -----------------------
      -- Conformance_Error --
      -----------------------

      procedure Conformance_Error (Msg : String; N : Node_Id) is
      begin
         Error_Msg_Sloc := Sloc (Prev_Loc);
         Error_Msg_N ("not fully conformant with declaration#!", N);
         Error_Msg_NE (Msg, N, N);
      end Conformance_Error;

   --  Start of processing for Check_Discriminant_Conformance

   begin
      while Present (Old_Discr) and then Present (New_Discr) loop

         New_Discr_Id := Defining_Identifier (New_Discr);

         --  The subtype mark of the discriminant on the full type
         --  has not been analyzed so we do it here. For an access
         --  discriminant a new type is created.

         if Nkind (Discriminant_Type (New_Discr)) = N_Access_Definition then
            New_Discr_Type :=
              Access_Definition (N, Discriminant_Type (New_Discr));

         else
            Analyze (Discriminant_Type (New_Discr));
            New_Discr_Type := Etype (Discriminant_Type (New_Discr));
         end if;

         if not Conforming_Types
                  (Etype (Old_Discr), New_Discr_Type, Fully_Conformant)
         then
            Conformance_Error ("type of & does not match!", New_Discr_Id);
            return;
         end if;

         --  Names must match

         if Chars (Old_Discr) /= Chars (Defining_Identifier (New_Discr)) then
            Conformance_Error ("name & does not match!", New_Discr_Id);
            return;
         end if;

         --  Default expressions must match

         declare
            NewD : constant Boolean :=
                     Present (Expression (New_Discr));
            OldD : constant Boolean :=
                     Present (Expression (Parent (Old_Discr)));

         begin
            if NewD or OldD then

               --  The old default value has been analyzed and expanded,
               --  because the current full declaration will have frozen
               --  everything before. The new default values have not
               --  been expanded, so expand now to check conformance.

               if NewD then
                  Analyze_Default_Expression
                    (Expression (New_Discr), New_Discr_Type);
               end if;

               if not (NewD and OldD)
                 or else not Fully_Conformant_Expressions
                              (Expression (Parent (Old_Discr)),
                               Expression (New_Discr))

               then
                  Conformance_Error
                    ("default expression for & does not match!",
                     New_Discr_Id);
                  return;
               end if;
            end if;
         end;

         --  In Ada 83 case, grouping must match: (A,B : X) /= (A : X; B : X)

         if Ada_83 then
            declare
               Old_Disc : constant Node_Id := Declaration_Node (Old_Discr);

            begin
               --  Grouping (use of comma in param lists) must be the same
               --  This is where we catch a misconformance like:

               --    A,B : Integer
               --    A : Integer; B : Integer

               --  which are represented identically in the tree except
               --  for the setting of the flags More_Ids and Prev_Ids.

               if More_Ids (Old_Disc) /= More_Ids (New_Discr)
                 or else Prev_Ids (Old_Disc) /= Prev_Ids (New_Discr)
               then
                  Conformance_Error
                    ("grouping of & does not match!", New_Discr_Id);
                  return;
               end if;
            end;
         end if;

         Old_Discr := Next_Discriminant (Old_Discr);
         New_Discr := Next (New_Discr);
      end loop;

      if Present (Old_Discr) then
         Conformance_Error ("too few discriminants!", Defining_Identifier (N));
         return;

      elsif Present (New_Discr) then
         Conformance_Error
           ("too many discriminants!", Defining_Identifier (New_Discr));
         return;
      end if;
   end Check_Discriminant_Conformance;

   ----------------------------
   -- Check_Fully_Conformant --
   ----------------------------

   procedure Check_Fully_Conformant
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty)
   is
      Result : Boolean;

   begin
      Check_Conformance
        (New_Id, Old_Id, Fully_Conformant, True, Result, Err_Loc);
   end Check_Fully_Conformant;

   ---------------------------
   -- Check_Mode_Conformant --
   ---------------------------

   procedure Check_Mode_Conformant
     (New_Id   : Entity_Id;
      Old_Id   : Entity_Id;
      Err_Loc  : Node_Id := Empty;
      Get_Inst : Boolean := False)
   is
      Result : Boolean;

   begin
      Check_Conformance
        (New_Id, Old_Id, Mode_Conformant, True, Result, Err_Loc, Get_Inst);
   end Check_Mode_Conformant;

   ------------------------------
   -- Check_Subtype_Conformant --
   ------------------------------

   procedure Check_Subtype_Conformant
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty)
   is
      Result : Boolean;

   begin
      Check_Conformance
        (New_Id, Old_Id, Subtype_Conformant, True, Result, Err_Loc);
   end Check_Subtype_Conformant;

   ---------------------------
   -- Check_Type_Conformant --
   ---------------------------

   procedure Check_Type_Conformant
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty)
   is
      Result : Boolean;

   begin
      Check_Conformance
        (New_Id, Old_Id, Type_Conformant, True, Result, Err_Loc);
   end Check_Type_Conformant;

   ----------------------
   -- Conforming_Types --
   ----------------------

   function Conforming_Types
     (T1       : Entity_Id;
      T2       : Entity_Id;
      Ctype    : Conformance_Type;
      Get_Inst : Boolean := False)
      return     Boolean
   is
      Type_1 : Entity_Id := T1;
      Type_2 : Entity_Id := T2;

      function Base_Types_Match (T1, T2 : Entity_Id) return Boolean;
      --  If neither T1 nor T2 are generic actual types, then verify
      --  that the base types are equal. Otherwise T1 and T2 must be
      --  on the same subtype chain. The whole purpose of this procedure
      --  is to prevent spurious ambiguities in an instantiation that may
      --  arise if two distinct generic types are instantiated with the
      --  same actual.

      ----------------------
      -- Base_Types_Match --
      ----------------------

      function Base_Types_Match (T1, T2 : Entity_Id) return Boolean is
      begin
         if T1 = T2 then
            return True;

         elsif Base_Type (T1) = Base_Type (T2) then

            --  The following is too permissive. A more precise test must
            --  check that the generic actual is an ancestor subtype of the
            --  other ???.

            return not Is_Generic_Actual_Type (T1)
              or else not Is_Generic_Actual_Type (T2);

         else
            return False;
         end if;
      end Base_Types_Match;

   begin
      --  The context is an instance association for a formal
      --  access-to-subprogram type; the formal parameter types
      --  require mapping because they may denote other formal
      --  parameters of the generic unit.

      if Get_Inst then
         Type_1 := Get_Instance_Of (T1);
         Type_2 := Get_Instance_Of (T2);
      end if;

      --  First see if base types match

      if Base_Types_Match (Type_1, Type_2) then
         return Ctype <= Mode_Conformant
           or else Subtypes_Statically_Match (Type_1, Type_2);

      elsif Is_Incomplete_Or_Private_Type (Type_1)
        and then Present (Full_View (Type_1))
        and then Base_Types_Match (Full_View (Type_1), Type_2)
      then
         return Ctype <= Mode_Conformant
           or else Subtypes_Statically_Match (Full_View (Type_1), Type_2);
      end if;

      --  Test anonymous access type case. For this case, static subtype
      --  matching is required for mode conformance (RM 6.3.1(15))

      if Ekind (Type_1) = E_Anonymous_Access_Type
        and then Ekind (Type_2) = E_Anonymous_Access_Type
      then
         declare
            Desig_1 : Entity_Id;
            Desig_2 : Entity_Id;

         begin
            Desig_1 := Directly_Designated_Type (Type_1);

            if Is_Incomplete_Or_Private_Type (Desig_1)
              and then Present (Full_View (Desig_1))
            then
               Desig_1 := Full_View (Desig_1);
            end if;

            Desig_2 := Directly_Designated_Type (Type_2);

            if Is_Incomplete_Or_Private_Type (Desig_2)
              and then Present (Full_View (Desig_2))
            then
               Desig_2 := Full_View (Desig_2);
            end if;

            --  The context is an instance association for a formal
            --  access-to-subprogram type; formal access parameter
            --  designated types require mapping because they may
            --  denote other formal parameters of the generic unit.

            if Get_Inst then
               Desig_1 := Get_Instance_Of (Desig_1);
               Desig_2 := Get_Instance_Of (Desig_2);
            end if;

            return Base_Type (Desig_1) = Base_Type (Desig_2)
              and then (Ctype = Type_Conformant
                          or else
                        Subtypes_Statically_Match (Desig_1, Desig_2));
         end;

      --  Otherwise definitely no match

      else
         return False;
      end if;

   end Conforming_Types;

   --------------------------
   -- Create_Extra_Formals --
   --------------------------

   procedure Create_Extra_Formals (E : Entity_Id) is
      Formal      : Entity_Id;
      Last_Formal : Entity_Id;
      Last_Extra  : Entity_Id;
      Formal_Type : Entity_Id;
      P_Formal    : Entity_Id := Empty;

      function Add_Extra_Formal (Typ : Entity_Id) return Entity_Id;
      --  Add an extra formal, associated with the current Formal. The
      --  extra formal is added to the list of extra formals, and also
      --  returned as the result. These formals are always of mode IN.

      function Add_Extra_Formal (Typ : Entity_Id) return Entity_Id is
         EF : constant Entity_Id :=
                Make_Defining_Identifier (Sloc (Formal),
                  Chars => New_External_Name (Chars (Formal), 'F'));

      begin
         --  We never generate extra formals if expansion is not active
         --  because we don't need them unless we are generating code.

         if not Expander_Active then
            return Empty;
         end if;

         --  A little optimization. Never generate an extra formal for
         --  the _init operand of an initialization procedure, since it
         --  could never be used.

         if Chars (Formal) = Name_uInit then
            return Empty;
         end if;

         Set_Ekind          (EF, E_In_Parameter);
         Set_Actual_Subtype (EF, Typ);
         Set_Etype          (EF, Typ);
         Set_Scope          (EF, Scope (Formal));
         Set_Mechanism      (EF, Default_Mechanism);

         Set_Extra_Formal (Last_Extra, EF);
         Last_Extra := EF;
         return EF;
      end Add_Extra_Formal;

   --  Start of processing for Create_Extra_Formals

   begin
      --  If this is a derived subprogram then the subtypes of the
      --  parent subprogram's formal parameters will be used to
      --  to determine the need for extra formals.

      if Is_Overloadable (E) and then Present (Alias (E)) then
         P_Formal := First_Formal (Alias (E));
      end if;

      Last_Extra := Empty;
      Formal := First_Formal (E);
      while Present (Formal) loop
         Last_Extra := Formal;
         Formal := Next_Formal (Formal);
      end loop;

      --  If Extra_formals where already created, don't do it again
      --  This situation may arise for subprogram types created as part
      --  of dispatching calls (see Expand_Dispatch_Call)

      if Present (Last_Extra) and then
        Present (Extra_Formal (Last_Extra))
      then
         return;
      end if;

      Formal := First_Formal (E);

      while Present (Formal) loop
         --  Create extra formal for supporting the attribute
         --  Constrained. The case of a private type view
         --  without discriminants also requires the extra
         --  formal if the underlying type has defaulted
         --  discriminants.

         if Ekind (Formal) /= E_In_Parameter then
            if Present (P_Formal) then
               Formal_Type := Etype (P_Formal);
            else
               Formal_Type := Etype (Formal);
            end if;

            if not Has_Discriminants (Formal_Type)
              and then Ekind (Formal_Type) in Private_Kind
              and then Present (Underlying_Type (Formal_Type))
            then
               Formal_Type := Underlying_Type (Formal_Type);
            end if;

            if Has_Discriminants (Formal_Type)
              and then
                ((not Is_Constrained (Formal_Type)
                   and then not Is_Indefinite_Subtype (Formal_Type))
                 or else Present (Extra_Formal (Formal)))
            then
               Set_Extra_Formal_Constrained
                 (Formal, Add_Extra_Formal (Standard_Boolean));
            end if;
         end if;

         --  Create extra formal for supporting accessibility checking

         if Ekind (Etype (Formal)) = E_Anonymous_Access_Type
           and then not Suppress_Accessibility_Checks (E)
           and then
             (not Present (P_Formal)
               or else Present (Extra_Formal_Accessibility (P_Formal)))
         then

            --  Temporary kludge: for now we avoid creating the extra
            --  formal for access parameters of protected operations
            --  because of problem with the case of internal protected
            --  calls. ???

            if Nkind (Parent (Parent (Parent (E)))) /= N_Protected_Definition
              and then Nkind (Parent (Parent (Parent (E)))) /= N_Protected_Body
            then
               Set_Extra_Formal_Accessibility
                 (Formal, Add_Extra_Formal (Standard_Natural));
            end if;
         end if;

         if Present (P_Formal) then
            P_Formal := Next_Formal (P_Formal);
         end if;

         Last_Formal := Formal;
         Formal := Next_Formal (Formal);
      end loop;
   end Create_Extra_Formals;

   -----------------------------
   -- Enter_Overloaded_Entity --
   -----------------------------

   procedure Enter_Overloaded_Entity (S : Entity_Id) is
      E : Entity_Id;

   begin
      E := Current_Entity_In_Scope (S);

      if Present (E) then
         Set_Has_Homonym (E);
         Set_Has_Homonym (S);
      end if;

      E := Current_Entity (S);
      Set_Is_Immediately_Visible (S);
      Set_Current_Entity (S);
      Set_Scope (S, Current_Scope);
      Set_Homonym (S, E);

      Append_Entity (S, Current_Scope);
      Set_Public_Status (S);

      if Debug_Flag_E then
         Write_Str ("New overloaded entity chain: ");
         Write_Name (Chars (S));
         E := S;

         while Present (E) loop
            Write_Str (" "); Write_Int (Int (E));
            E := Homonym (E);
         end loop;

         Write_Eol;
      end if;

   end Enter_Overloaded_Entity;

   -----------------------------
   -- Find_Corresponding_Spec --
   -----------------------------

   function Find_Corresponding_Spec (N : Node_Id) return Entity_Id is
      Spec       : constant Node_Id   := Specification (N);
      Designator : constant Entity_Id := Defining_Entity (Spec);

      E : Entity_Id;

   begin
      E := Current_Entity (Designator);

      while Present (E) loop

         --  We are looking for a matching spec. It must have the same scope,
         --  and the same name, and either be type conformant, or be the case
         --  of a library procedure spec and its body (which belong to one
         --  another regardless of whether they are type conformant or not).

         if Scope (E) = Current_Scope
           and then (Current_Scope = Standard_Standard
                       or else (Ekind (E) = Ekind (Designator)
                                 and then
                                Type_Conformant (E, Designator)))
         then
            if not Has_Completion (E) then

               if Nkind (N) /= N_Subprogram_Body_Stub then
                  Set_Corresponding_Spec (N, E);
               end if;

               Set_Has_Completion (E);
               return E;

            --  If body already exists, this is an error unless the
            --  previous declaration is the implicit declaration of
            --  a derived subprogram, or this is a spurious overloading
            --  in an instance.

            elsif No (Alias (E))
              and then not Is_Intrinsic_Subprogram (E)
              and then not In_Instance
            then
               Error_Msg_N ("duplicate subprogram body", N);
            end if;
         end if;

         E := Homonym (E);
      end loop;

      --  On exit, we know that no previous declaration of subprogram exists

      return Empty;
   end Find_Corresponding_Spec;

   ----------------------
   -- Fully_Conformant --
   ----------------------

   function Fully_Conformant (New_Id, Old_Id : Entity_Id) return Boolean is
      Result : Boolean;

   begin
      Check_Conformance (New_Id, Old_Id, Fully_Conformant, False, Result);
      return Result;
   end Fully_Conformant;

   ----------------------------------
   -- Fully_Conformant_Expressions --
   ----------------------------------

   function Fully_Conformant_Expressions
     (Given_E1 : Node_Id;
      Given_E2 : Node_Id)
      return     Boolean
   is
      E1 : constant Node_Id := Original_Node (Given_E1);
      E2 : constant Node_Id := Original_Node (Given_E2);
      --  We always test conformance on original nodes, since it is possible
      --  for analysis and/or expansion to make things look as though they
      --  conform when they do not, e.g. by converting 1+2 into 3.

      function FCE (Given_E1, Given_E2 : Node_Id) return Boolean
        renames Fully_Conformant_Expressions;

      function FCL (L1, L2 : List_Id) return Boolean;
      --  Compare elements of two lists for conformance. Elements have to
      --  be conformant, and actuals inserted as default parameters do not
      --  match explicit actuals with the same value.

      function FCL (L1, L2 : List_Id) return Boolean is
         N1, N2 : Node_Id;

      begin
         if L1 = No_List then
            N1 := Empty;
         else
            N1 := First (L1);
         end if;

         if L2 = No_List then
            N2 := Empty;
         else
            N2 := First (L2);
         end if;

         while Present (N1) and then Present (N2) loop
            if not FCE (N1, N2)
              or else Is_Rewrite_Insertion (N1) /= Is_Rewrite_Insertion (N2)
            then
               return False;
            end if;

            N1 := Next (N1);
            N2 := Next (N2);
         end loop;

         return No (N1) and then No (N2);
      end FCL;

   --  Start of processing for Fully_Conformant_Expressions

   begin
      --  What about "+"(a,b) conforming with a+b ???

      --  Non-conformant if paren count does not match. Note: if some idiot
      --  complains that we don't do this right for more than 3 levels of
      --  parentheses, they will be treated with the respect they deserve :-)

      if Paren_Count (E1) /= Paren_Count (E2) then
         return False;

      --  If same entities are referenced, then they are conformant
      --  even if they have different forms (RM 8.3.1(19-20)).

      elsif Is_Entity_Name (E1) and then Is_Entity_Name (E2) then
         return Entity (E1) = Entity (E2);

      --  Otherwise we must have the same syntactic entity

      elsif Nkind (E1) /= Nkind (E2) then
         return False;

      --  At this point, we specialize by node type

      else
         case Nkind (E1) is

            when N_Aggregate =>
               return
                 FCL (Expressions (E1), Expressions (E2))
                   and then FCL (Component_Associations (E1),
                                 Component_Associations (E2));

            when N_Allocator =>
               if Nkind (Expression (E1)) = N_Qualified_Expression
                    or else
                  Nkind (Expression (E2)) = N_Qualified_Expression
               then
                  return FCE (Expression (E1), Expression (E2));

               --  Check that the subtype marks and any constraints
               --  are conformant

               else
                  declare
                     Indic1 : constant Node_Id := Expression (E1);
                     Indic2 : constant Node_Id := Expression (E2);
                     Elt1   : Node_Id;
                     Elt2   : Node_Id;

                  begin
                     if Nkind (Indic1) /= N_Subtype_Indication then
                        return
                          Nkind (Indic2) /= N_Subtype_Indication
                            and then Entity (Indic1) = Entity (Indic2);

                     elsif Nkind (Indic2) /= N_Subtype_Indication then
                        return
                          Nkind (Indic1) /= N_Subtype_Indication
                            and then Entity (Indic1) = Entity (Indic2);

                     else
                        if Entity (Subtype_Mark (Indic1)) /=
                          Entity (Subtype_Mark (Indic2))
                        then
                           return False;
                        end if;

                        Elt1 := First (Constraints (Constraint (Indic1)));
                        Elt2 := First (Constraints (Constraint (Indic2)));

                        while Present (Elt1) and then Present (Elt2) loop
                           if not FCE (Elt1, Elt2) then
                              return False;
                           end if;

                           Elt1 := Next (Elt1);
                           Elt2 := Next (Elt2);
                        end loop;

                        return True;
                     end if;
                  end;
               end if;

            when N_Attribute_Reference =>
               return
                 Attribute_Name (E1) = Attribute_Name (E2)
                   and then FCL (Expressions (E1), Expressions (E2));

            when N_Binary_Op =>
               return
                 Entity (E1) = Entity (E2)
                   and then FCE (Left_Opnd  (E1), Left_Opnd  (E2))
                   and then FCE (Right_Opnd (E1), Right_Opnd (E2));

            when N_And_Then | N_Or_Else | N_In | N_Not_In =>
               return
                 FCE (Left_Opnd  (E1), Left_Opnd  (E2))
                   and then
                 FCE (Right_Opnd (E1), Right_Opnd (E2));

            when N_Character_Literal =>
               return
                 Char_Literal_Value (E1) = Char_Literal_Value (E2);

            when N_Component_Association =>
               return
                 FCL (Choices (E1), Choices (E2))
                   and then FCE (Expression (E1), Expression (E2));

            when N_Concat_Multiple =>
               return
                 FCL (Expressions (E1), Expressions (E2));

            when N_Conditional_Expression =>
               return
                 FCL (Expressions (E1), Expressions (E2));

            when N_Explicit_Dereference =>
               return
                 FCE (Prefix (E1), Prefix (E2));

            when N_Extension_Aggregate =>
               return
                 FCL (Expressions (E1), Expressions (E2))
                   and then Null_Record_Present (E1) =
                            Null_Record_Present (E2)
                   and then FCL (Component_Associations (E1),
                               Component_Associations (E2));

            when N_Function_Call =>
               return
                 FCE (Name (E1), Name (E2))
                   and then FCL (Parameter_Associations (E1),
                                 Parameter_Associations (E2));

            when N_Indexed_Component =>
               return
                 FCE (Prefix (E1), Prefix (E2))
                   and then FCL (Expressions (E1), Expressions (E2));

            when N_Integer_Literal =>
               return (Intval (E1) = Intval (E2));

            when N_Null =>
               return True;

            when N_Operator_Symbol =>
               return
                 Chars (E1) = Chars (E2);

            when N_Others_Choice =>
               return True;

            when N_Parameter_Association =>
               return

                 Chars (Selector_Name (E1))  = Chars (Selector_Name (E2))
                   and then FCE (Explicit_Actual_Parameter (E1),
                                 Explicit_Actual_Parameter (E2));

            when N_Qualified_Expression =>
               return
                 FCE (Subtype_Mark (E1), Subtype_Mark (E2))
                   and then FCE (Expression (E1), Expression (E2));

            when N_Range =>
               return
                 FCE (Low_Bound (E1), Low_Bound (E2))
                   and then FCE (High_Bound (E1), High_Bound (E2));

            when N_Real_Literal =>
               return (Realval (E1) = Realval (E2));

            when N_Selected_Component =>
               return
                 FCE (Prefix (E1), Prefix (E2))
                   and then FCE (Selector_Name (E1), Selector_Name (E2));

            when N_Slice =>
               return
                 FCE (Prefix (E1), Prefix (E2))
                   and then FCE (Discrete_Range (E1), Discrete_Range (E2));

            when N_String_Literal =>
               declare
                  S1 : constant String_Id := Strval (E1);
                  S2 : constant String_Id := Strval (E2);
                  L1 : constant Nat       := String_Length (S1);
                  L2 : constant Nat       := String_Length (S2);

               begin
                  if L1 /= L2 then
                     return False;

                  else
                     for J in 1 .. L1 loop
                        if Get_String_Char (S1, J) /=
                           Get_String_Char (S2, J)
                        then
                           return False;
                        end if;
                     end loop;

                     return True;
                  end if;
               end;

            when N_Type_Conversion =>
               return
                 FCE (Subtype_Mark (E1), Subtype_Mark (E2))
                   and then FCE (Expression (E1), Expression (E2));

            when N_Unary_Op =>
               return
                 Entity (E1) = Entity (E2)
                   and then FCE (Right_Opnd (E1), Right_Opnd (E2));

            when N_Unchecked_Type_Conversion =>
               return
                 FCE (Subtype_Mark (E1), Subtype_Mark (E2))
                   and then FCE (Expression (E1), Expression (E2));

            --  All other node types cannot appear in this context. Strictly
            --  we should do a pragma Assert (False). Instead we just ignore
            --  the nodes. This means that if anyone makes a mistake in the
            --  expander and mucks an expression tree irretrievably, the
            --  result will be a failure to detect a (probably very obscure)
            --  case of non-conformance, which is better than bombing on some
            --  case where two expressions do in fact conform.

            when others =>
               return True;

         end case;
      end if;
   end Fully_Conformant_Expressions;

   --------------------
   -- Install_Entity --
   --------------------

   procedure Install_Entity (E : Entity_Id) is
      Prev : constant Entity_Id := Current_Entity (E);

   begin
      Set_Is_Immediately_Visible (E);
      Set_Current_Entity (E);
      Set_Homonym (E, Prev);
   end Install_Entity;

   ---------------------
   -- Install_Formals --
   ---------------------

   procedure Install_Formals (Id : Entity_Id) is
      F : Entity_Id;

   begin
      F := First_Formal (Id);

      while Present (F) loop
         Install_Entity (F);
         F := Next_Formal (F);
      end loop;
   end Install_Formals;

   ------------------------------
   -- Make_Inequality_Operator --
   ------------------------------

   --  S is the defining identifier of an equality operator. We build a
   --  subprogram declaration with the rignt signature. This operation is
   --  intrinsic, because it is always expanded as the negation of the
   --  call to the equality function.

   procedure Make_Inequality_Operator (S : Entity_Id) is
      Loc     : constant Source_Ptr := Sloc (S);
      Decl    : Node_Id;
      Formals : List_Id;
      Op_Name : Entity_Id;
      Typ     : constant Entity_Id := Etype (First_Formal (S));

      A : Entity_Id;
      B : Entity_Id;

   begin
      --  Check that equality was properly defined.

      if  No (Next_Formal (First_Formal (S))) then
         return;
      end if;

      A := Make_Defining_Identifier (Loc, Chars (First_Formal (S)));
      B := Make_Defining_Identifier (Loc,
             Chars (Next_Formal (First_Formal (S))));

      Op_Name := Make_Defining_Operator_Symbol (Loc, Name_Op_Ne);

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => A,
          Parameter_Type =>
            New_Reference_To (Etype (First_Formal (S)), Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => B,
          Parameter_Type =>
            New_Reference_To (Etype (Next_Formal (First_Formal (S))), Loc)));

      Decl :=
        Make_Subprogram_Declaration (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name => Op_Name,
              Parameter_Specifications => Formals,
              Subtype_Mark => New_Reference_To (Standard_Boolean, Loc)));

      --  Insert inequality right after equality if it is explicit or after
      --  the derived type when implicit. These entities are created only
      --  for visibility purposes, and eventually replaced in the course of
      --  expansion, so they do not need to be attached to the tree and seen
      --  by the back-end. Keeping them internal also avoids spurious freezing
      --  problems. The parent field is set simply to make analysis safe.

      if No (Alias (S)) then
         Set_Parent (Decl, Parent (Get_Declaration_Node (S)));
      else
         Set_Parent (Decl, Parent (Parent (Etype (First_Formal (S)))));
      end if;

      Mark_Rewrite_Insertion (Decl);
      Analyze (Decl);
      Set_Has_Completion (Op_Name);
      Set_Is_Intrinsic_Subprogram (Op_Name);
      Set_Corresponding_Equality (Op_Name, S);

   end Make_Inequality_Operator;

   ----------------------
   -- May_Need_Actuals --
   ----------------------

   procedure May_Need_Actuals (Fun : Entity_Id) is
      F : Entity_Id;
      B : Boolean;

   begin
      F := First_Formal (Fun);
      B := True;

      while Present (F) loop
         if No (Default_Value (F)) then
            B := False;
            exit;
         end if;

         F := Next_Formal (F);
      end loop;

      Set_Needs_No_Actuals (Fun, B);
   end May_Need_Actuals;

   ---------------------
   -- Mode_Conformant --
   ---------------------

   function Mode_Conformant (New_Id, Old_Id : Entity_Id) return Boolean is
      Result : Boolean;

   begin
      Check_Conformance (New_Id, Old_Id, Mode_Conformant, False, Result);
      return Result;
   end Mode_Conformant;

   ---------------------------
   -- New_Overloaded_Entity --
   ---------------------------

   procedure New_Overloaded_Entity
     (S            : Entity_Id;
      Derived_Type : Entity_Id := Empty)
   is
      E        : Entity_Id := Current_Entity_In_Scope (S);
      Prev_Vis : Entity_Id := Empty;

      procedure Maybe_Primitive_Operation (Overriding : Boolean := False);
      --  If the subprogram being analyzed is a primitive operation of
      --  the type of one of its formals, set the corresponding flag.

      procedure Maybe_Primitive_Operation (Overriding : Boolean := False) is
         Formal : Entity_Id;
         F_Typ  : Entity_Id;

      begin
         if not Comes_From_Source (S) then
            null;

         elsif (Ekind (Current_Scope) = E_Package
              and then not In_Package_Body (Current_Scope))
           or else Overriding
         then

            if Ekind (S) = E_Function
              and then Scope (Base_Type (Etype (S))) = Current_Scope
            then
               Set_Has_Primitive_Operations (Base_Type (Etype (S)));
               return;
            end if;

            Formal := First_Formal (S);

            while Present (Formal) loop
               if Ekind (Etype (Formal)) = E_Anonymous_Access_Type then
                  F_Typ := Designated_Type (Etype (Formal));
               else
                  F_Typ := Etype (Formal);
               end if;

               if Scope (Base_Type (F_Typ)) = Current_Scope then
                  Set_Has_Primitive_Operations (Base_Type (F_Typ));
                  return;
               end if;

               Formal := Next_Formal (Formal);
            end loop;

         end if;
      end Maybe_Primitive_Operation;

   begin
      if No (E) then
         Enter_Overloaded_Entity (S);
         Check_Dispatching_Operation (S, Empty);
         Maybe_Primitive_Operation;

      elsif not Is_Overloadable (E) then

         --  Check for spurious conflict produced by a subprogram that has the
         --  same name as that of the enclosing generic package. The conflict
         --  occurs within an instance, between the subprogram and the renaming
         --  declaration for the package. After the subprogram, the package
         --  renaming declaration becomes hidden.

         if Ekind (E) = E_Package
           and then Present (Renamed_Object (E))
           and then Renamed_Object (E) = Current_Scope
           and then Nkind (Parent (Renamed_Object (E))) =
             N_Package_Specification
           and then Present (Generic_Parent (Parent (Renamed_Object (E))))
         then
            Set_Is_Private (E);
            Set_Is_Immediately_Visible (E, False);
            Enter_Overloaded_Entity (S);
            Set_Homonym (S, Homonym (E));
            Check_Dispatching_Operation (S, Empty);

         --  If the subprogram is implicit it is hidden by the previous
         --  declaration.

         elsif Present (Alias (S))
           and then not Comes_From_Source (S)
         then
            return;

         else
            Error_Msg_N ("duplicate identifier:&", S);
            return;
         end if;

      else
         --  E exists and is overloadable. Determine whether S is the body
         --  of E, a new overloaded entity with a different signature, or
         --  an error altogether.

         while Present (E) loop
            if Scope (E) /= Current_Scope then
               null;

            elsif Type_Conformant (E, S) then

               --  If the old and new entities have the same profile and
               --  one is not the body of the other, then this is an error,
               --  unless one of them is implicitly declared.

               --  There are some cases when both can be implicit, for example
               --  when both a literal and a function that overrides it are
               --  inherited in a derivation. Ada83 had a special rule for
               --  this. In Ada95, the later implicit operation hides the
               --  former, and the literal is always the former.

               if Present (Alias (S))
                 and then
                   (Ekind (E) = E_Entry
                     or else No (Alias (E))
                     or else Ekind (E) /= E_Enumeration_Literal)
               then
                  --  When an derived operation is overloaded it may be
                  --  due to the fact that the full view of a private extension
                  --  re-inherits. It has to be dealt with.

                  if (Ekind (Current_Scope) = E_Package
                       or else Ekind (Current_Scope) = E_Generic_Package)
                    and then In_Private_Part (Current_Scope)
                  then
                     Check_Operation_From_Private_View (S, E);
                  end if;

                  --  In any case the implicit operation remains hidden by
                  --  the existing declaration.

                  return;

                  --  Within an instance, the renaming declarations for
                  --  actual subprograms may become ambiguous, but they do
                  --  not hide each other.

               elsif Ekind (E) /= E_Entry
                 and then not Comes_From_Source (E)
                 and then not Is_Generic_Instance (E)
                 and then (Present (Alias (E))
                            or else Is_Intrinsic_Subprogram (E))
                 and then (not In_Instance
                            or else No (Parent (E))
                            or else Nkind (Get_Declaration_Node (E)) /=
                               N_Subprogram_Renaming_Declaration)
               then
                  --  A subprogram child unit is not allowed to override
                  --  an inherited subprogram (10.1.1(20)).

                  if Is_Child_Unit (S) then
                     Error_Msg_N
                       ("child unit overrides inherited subprogram in parent",
                        S);
                     return;
                  end if;

                  if Is_Non_Overriding_Operation (E, S) then
                     Enter_Overloaded_Entity (S);
                     return;
                  end if;

                  --  E is a derived operation or an internal operator which
                  --  is being overridden. Remove E from further visibility.
                  --  Furthermore, if E is a dispatching operation, it must be
                  --  replaced in the list of primitive operations of its type

                  declare
                     Prev : Entity_Id;

                  begin
                     Prev := First_Entity (Current_Scope);

                     while Next_Entity (Prev) /= E loop
                        Prev := Next_Entity (Prev);
                     end loop;

                     --  E must be removed both from the entity_list of the
                     --  current scope, and from the visibility chain

                     if Debug_Flag_E then
                        Write_Str ("Override implicit operation ");
                        Write_Int (Int (E));
                        Write_Eol;
                     end if;

                     --  If E is a predefined concatenation, it stands for four
                     --  different operations. As a result, a single explicit
                     --  declaration does not hide it. In a possible ambiguous
                     --  situation, Disambiguate chooses the user-defined op,
                     --  so it is correct to retain the previous internal one.

                     if Chars (E) /= Name_Op_Concat then

                        --  For nondispatching derived operations that
                        --  are overridden by a subprogram declared in
                        --  the private part of a package, we retain the
                        --  derived subprogram but mark it as not
                        --  immediately visible. If the derived operation
                        --  was declared in the visible part then this
                        --  ensures that it will still be visible outside
                        --  the package with the proper signature (calls
                        --  from outside must also be directed to this
                        --  version rather than the overriding one, unlike
                        --  the dispatching case). Calls from inside the
                        --  package will still resolve to the overriding
                        --  subprogram since the derived one is marked
                        --  as not visible within the package.

                        --  Note that something similar needs to occur for
                        --  dispatching operations since there as well the
                        --  outside view of the operation should have the
                        --  signature of the derived operation, but that
                        --  case will require more work because dispatching
                        --  operations are handled quite differently in terms
                        --  of their inheritance and overriding. ???

                        if In_Private_Part (Current_Scope)
                          and then not Is_Dispatching_Operation (E)
                        then
                           Set_Is_Immediately_Visible (E, False);

                        else

                           --  Find predecessor of E in Homonym chain.

                           if E = Current_Entity (E) then
                              Prev_Vis := Empty;
                           else
                              Prev_Vis := Current_Entity (E);
                              while Homonym (Prev_Vis) /= E loop
                                 Prev_Vis := Homonym (Prev_Vis);
                              end loop;
                           end if;

                           if Prev_Vis /= Empty then

                              --  Skip E in the visibility chain

                              Set_Homonym (Prev_Vis, Homonym (E));

                           else
                              Set_Name_Entity_Id (Chars (E), Homonym (E));
                           end if;

                           Set_Next_Entity (Prev, Next_Entity (E));

                           if No (Next_Entity (Prev)) then
                              Set_Last_Entity (Current_Scope, Prev);
                           end if;

                        end if;
                     end if;

                     Enter_Overloaded_Entity (S);

                     if Is_Dispatching_Operation (E) then
                        Check_Dispatching_Operation (S, E);
                     else
                        Check_Dispatching_Operation (S, Empty);
                     end if;

                     Maybe_Primitive_Operation (Overriding => True);
                     goto Check_Inequality;
                  end;

               --  Apparent redeclarations in instances can occur when two
               --  formal types get the same actual type. The subprograms in
               --  in the instance are legal,  even if not callable from the
               --  outside. Calls from within are disambiguated elsewhere.

               elsif In_Instance then
                  null;

               --  Here we have a real error (identical profile)

               else
                  Error_Msg_Sloc := Sloc (E);
                  Error_Msg_N ("& conflicts with declaration#", S);
                  return;
               end if;

            else
               null;
            end if;

            Prev_Vis := E;
            E := Homonym (E);
         end loop;

         --  On exit, we know that S is a new entity

         Enter_Overloaded_Entity (S);
         Maybe_Primitive_Operation;

         --  If S is a derived operation for an untagged type then
         --  by definition it's not a dispatching operation (even
         --  if the parent operation was dispatching), so we don't
         --  call Check_Dispatching_Operation in that case.

         if not Present (Derived_Type)
           or else Is_Tagged_Type (Derived_Type)
         then
            Check_Dispatching_Operation (S, Empty);
         end if;
      end if;

      --  If this is a  user-defined equality operator that is not
      --  a derived subprogram, create the corresponding inequality.
      --  If the operation is dispatching, the expansion is done
      --  elsewhere,  and we do not create an explicit inequality
      --  operation.

      <<Check_Inequality>>
         if Chars (S) = Name_Op_Eq
           and then Etype (S) = Standard_Boolean
           and then Present (Parent (S))
           and then not Is_Dispatching_Operation (S)
         then
            Make_Inequality_Operator (S);
         end if;

   end New_Overloaded_Entity;

   ---------------------
   -- Process_Formals --
   ---------------------

   procedure Process_Formals
     (S           : Entity_Id;
      T           : List_Id;
      Related_Nod : Node_Id)
   is
      Param_Spec  : Node_Id;
      Formal      : Entity_Id;
      Formal_Type : Entity_Id;
      Default     : Node_Id;


   begin
      --  In order to prevent premature use of the formals in the same formal
      --  part, the Ekind is left undefined until all default expressions are
      --  analyzed. The Ekind is established in a separate loop at the end.

      Param_Spec := First (T);

      while Present (Param_Spec) loop

         Formal := Defining_Identifier (Param_Spec);
         Enter_Name (Formal);

         --  Case of ordinary parameters

         if Nkind (Parameter_Type (Param_Spec)) /= N_Access_Definition then
            Find_Type (Parameter_Type (Param_Spec));
            Formal_Type := Entity (Parameter_Type (Param_Spec));

            if Ekind (Formal_Type) = E_Incomplete_Type
              or else (Is_Class_Wide_Type (Formal_Type)
                        and then Ekind (Root_Type (Formal_Type)) =
                                                         E_Incomplete_Type)
            then
               if Nkind (Parent (T)) /= N_Access_Function_Definition
                 and then Nkind (Parent (T)) /= N_Access_Procedure_Definition
               then
                  Error_Msg_N ("invalid use of incomplete type", Param_Spec);
               end if;

            elsif Ekind (Formal_Type) = E_Void then
               Error_Msg_NE ("premature use of&",
                 Parameter_Type (Param_Spec), Formal_Type);
            end if;

         --  An access formal type

         else
            Formal_Type :=
              Access_Definition (Related_Nod, Parameter_Type (Param_Spec));
         end if;

         Set_Etype (Formal, Formal_Type);

         Default :=  Expression (Param_Spec);

         if Present (Default) then
            if Out_Present (Param_Spec) then
               Error_Msg_N
                 ("default initialization only allowed for IN parameters",
                  Param_Spec);
            end if;

            --  Do the special preanalysis of the expression (see section on
            --  "Handling of Default Expressions" in the spec of package Sem).

            Analyze_Default_Expression (Default, Formal_Type);

            --  Check that the designated type of an access parameter's
            --  default is not a class-wide type unless the parameter's
            --  designated type is also class-wide.

            if Ekind (Formal_Type) = E_Anonymous_Access_Type
              and then Is_Class_Wide_Type (Designated_Type (Etype (Default)))
              and then not Is_Class_Wide_Type (Designated_Type (Formal_Type))
            then
               Wrong_Type (Default, Formal_Type);
            end if;
         end if;

         Param_Spec := Next (Param_Spec);
      end loop;

      --  Now set the kind (mode) of each formal

      Param_Spec := First (T);

      while Present (Param_Spec) loop
         Formal := Defining_Identifier (Param_Spec);
         Set_Formal_Mode (Formal);

         if Ekind (Formal) = E_In_Parameter then
            Set_Default_Value (Formal, Expression (Param_Spec));
            if Present (Expression (Param_Spec)) then
               Default :=  Expression (Param_Spec);

               if Is_Scalar_Type (Etype (Default)) then
                  if Nkind
                       (Parameter_Type (Param_Spec)) /= N_Access_Definition
                  then
                     Formal_Type := Entity (Parameter_Type (Param_Spec));

                  else
                     Formal_Type := Access_Definition
                       (Related_Nod, Parameter_Type (Param_Spec));
                  end if;

                  Apply_Scalar_Range_Check (Default, Formal_Type);
               end if;

            end if;
         end if;

         Param_Spec := Next (Param_Spec);
      end loop;

   end Process_Formals;

   -------------------------
   -- Set_Actual_Subtypes --
   -------------------------

   --  Note: for now we only set actual subtypes for in parameters, where
   --  the actual subtype is sure not to change. We could actually also
   --  set it for arrays in all cases, since the actual subtype cannot
   --  change for an array, even if the parameter is in out or out ???

   procedure Set_Actual_Subtypes (N : Node_Id; Subp : Entity_Id) is
      Loc         : constant Source_Ptr := Sloc (N);
      Decl        : Node_Id;
      Formal      : Entity_Id;
      T           : Entity_Id;

   begin
      Formal := First_Formal (Subp);

      --  Expansion does not apply to initialization procedures, where
      --  discriminants are handled specially.

      if Chars (Formal) = Name_uInit then
         return;
      end if;

      while Present (Formal) loop
         T := Etype (Formal);

         --  Generate actual subtypes for unconstrained arrays and
         --  unconstrained discriminated records.

         if (not Is_Constrained (T))
           and then Ekind (Formal) = E_In_Parameter
           and then (Is_Array_Type (T)
                      or else (Is_Record_Type (T)
                                and then Has_Discriminants (T)))
           and then not Has_Unknown_Discriminants (T)
         then
            Decl := Build_Actual_Subtype (T, Formal);

            if Nkind (N) = N_Accept_Statement then
               if Present (Handled_Statement_Sequence (N)) then
                  Prepend (Decl, Statements (Handled_Statement_Sequence (N)));
                  Mark_Rewrite_Insertion (Decl);
               else
                  --  If the accept statement has no body, there will be
                  --  no reference to the actuals, so no need to compute
                  --  actual subtypes.

                  return;
               end if;

            else
               Prepend (Decl, Declarations (N));
               Mark_Rewrite_Insertion (Decl);
            end if;

            Analyze (Decl);
            Set_Actual_Subtype (Formal, Defining_Identifier (Decl));
         end if;

         Formal := Next_Formal (Formal);
      end loop;
   end Set_Actual_Subtypes;

   ---------------------
   -- Set_Formal_Mode --
   ---------------------

   procedure Set_Formal_Mode (Formal_Id : Entity_Id) is
      Spec : constant Node_Id := Parent (Formal_Id);

   begin
      if Out_Present (Spec) then

         if Ekind (Scope (Formal_Id)) = E_Function
           or else Ekind (Scope (Formal_Id)) = E_Generic_Function
         then
            Error_Msg_N ("functions can only have IN parameters", Spec);
            Set_Ekind (Formal_Id, E_In_Parameter);

         elsif In_Present (Spec) then
            Set_Ekind (Formal_Id, E_In_Out_Parameter);

         else
            Set_Ekind (Formal_Id, E_Out_Parameter);
            Set_Not_Assigned (Formal_Id);
         end if;

      else
         Set_Ekind (Formal_Id, E_In_Parameter);
      end if;

      Set_Mechanism (Formal_Id, Default_Mechanism);
   end Set_Formal_Mode;

   ------------------------
   -- Subtype_Conformant --
   ------------------------

   function Subtype_Conformant (New_Id, Old_Id : Entity_Id) return Boolean is
      Result : Boolean;

   begin
      Check_Conformance (New_Id, Old_Id, Subtype_Conformant, False, Result);
      return Result;
   end Subtype_Conformant;

   ---------------------
   -- Type_Conformant --
   ---------------------

   function Type_Conformant (New_Id, Old_Id : Entity_Id) return Boolean is
      Result : Boolean;

   begin
      Check_Conformance (New_Id, Old_Id, Type_Conformant, False, Result);
      return Result;
   end Type_Conformant;

   -------------------------------
   -- Valid_Operator_Definition --
   -------------------------------

   procedure Valid_Operator_Definition (Designator : Entity_Id) is
      N    : Integer := 0;
      F    : Entity_Id;
      Id   : constant Name_Id := Chars (Designator);
      N_OK : Boolean;

   begin
      F := First_Formal (Designator);

      while Present (F) loop
         N := N + 1;

         if Present (Default_Value (F)) then
            Error_Msg_N
              ("default values not allowed for operator parameters",
               Parent (F));
         end if;

         F := Next_Formal (F);
      end loop;

      --  Verify that user-defined operators have proper number of arguments
      --  First case of operators which can only be unary

      if Id = Name_Op_Not
        or else Id = Name_Op_Abs
      then
         N_OK := (N = 1);

      --  Case of operators which can be unary or binary

      elsif Id = Name_Op_Add
        or Id = Name_Op_Subtract
      then
         N_OK := (N in 1 .. 2);

      --  All other operators can only be binary

      else
         N_OK := (N = 2);
      end if;

      if not N_OK then
         Error_Msg_N
           ("incorrect number of arguments for operator", Designator);
      end if;

      if Id = Name_Op_Ne
        and then Etype (Designator) = Standard_Boolean
        and then (Comes_From_Source (Designator)
                    or else
                  Present (Generic_Parent (Parent (Designator))))
      then
         Error_Msg_N
            ("explicit definition of inequality not allowed", Designator);
      end if;
   end Valid_Operator_Definition;

end Sem_Ch6;

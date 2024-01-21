------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 9                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.177                             --
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
with Errout;   use Errout;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch9;
with Elists;   use Elists;
with Features; use Features;
with Itypes;   use Itypes;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch4;  use Sem_Ch4;
with Sem_Ch5;  use Sem_Ch5;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Dist; use Sem_Dist;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;

package body Sem_Ch9 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Max_Entries (Def : Node_Id; R : Restriction_Parameter_Id);
   --  Given either a protected definition or a task definition in Def, check
   --  the corresponding restriction parameter identifier R, and if it is set,
   --  count the entries (checking the static requirement), and compare with
   --  the given maximum.

   function Find_Concurrent_Spec (Body_Id : Entity_Id) return Entity_Id;
   --  Find entity in corresponding task or protected declaration. Use full
   --  view if first declaration was for an incomplete type.

   procedure Install_Declarations (Spec : Entity_Id);
   --  Utility to make visible in corresponding body the entities defined
   --  in task, protected type declaration, or entry declaration.

   -----------------------------
   -- Analyze_Abort_Statement --
   -----------------------------

   procedure Analyze_Abort_Statement (N : Node_Id) is
      T_Name : Node_Id;

   begin
      Tasking_Used := True;
      T_Name := First (Names (N));
      while Present (T_Name) loop
         Analyze (T_Name);

         if not Is_Task_Type (Etype (T_Name)) then
            Error_Msg_N ("expect task name for ABORT", T_Name);
            return;
         else
            Resolve (T_Name,  Etype (T_Name));
         end if;

         T_Name := Next (T_Name);
      end loop;

      Check_Restriction (No_Abort_Statements, N);
   end Analyze_Abort_Statement;

   ----------------------------
   -- Analyze_Abortable_Part --
   ----------------------------

   --  Is this ever called? I guess not, since we implement ATC ???

   procedure Analyze_Abortable_Part (N : Node_Id) is
   begin
      Unimplemented (N, "abortable part");
   end Analyze_Abortable_Part;

   --------------------------------
   -- Analyze_Accept_Alternative --
   --------------------------------

   procedure Analyze_Accept_Alternative (N : Node_Id) is
   begin
      Tasking_Used := True;

      if Present (Pragmas_Before (N)) then
         Analyze_List (Pragmas_Before (N));
      end if;

      Analyze (Accept_Statement (N));

      if Present (Condition (N)) then
         Analyze_And_Resolve (Condition (N), Any_Boolean);
      end if;

      if Is_Non_Empty_List (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;
   end Analyze_Accept_Alternative;

   ------------------------------
   -- Analyze_Accept_Statement --
   ------------------------------

   procedure Analyze_Accept_Statement (N : Node_Id) is
      Nam       : constant Entity_Id := Entry_Direct_Name (N);
      Formals   : constant List_Id   := Parameter_Specifications (N);
      Index     : constant Node_Id   := Entry_Index (N);
      Stats     : constant Node_Id   := Handled_Statement_Sequence (N);
      Ityp      : Entity_Id;
      Entry_Nam : Entity_Id;
      E         : Entity_Id;
      Kind      : Entity_Kind;
      Task_Nam  : Entity_Id;

   begin
      Tasking_Used := True;

      --  Entry name is initialized to Any_Id. It should get reset to the
      --  matching entry entity. An error is signalled if it is not reset.

      Entry_Nam := Any_Id;

      for J in reverse 0 .. Scope_Stack.Last loop
         Task_Nam := Scope_Stack.Table (J).Entity;
         exit when Ekind (Etype (Task_Nam)) = E_Task_Type;
         Kind :=  Ekind (Task_Nam);

         if Kind /= E_Block and then Kind /= E_Loop
           and then not Is_Entry (Task_Nam)
         then
            Error_Msg_N ("enclosing body of accept must be a task", N);
            return;
         end if;
      end loop;

      if Ekind (Etype (Task_Nam)) /= E_Task_Type then
         Error_Msg_N ("invalid context for accept statement",  N);
         return;
      end if;

      --  In order to process the parameters, we create a defining
      --  identifier that can be used as the name of the scope. The
      --  name of the accept statement itself is not a defining identifier.

      if Present (Index) then
         Ityp := New_Internal_Entity
           (E_Entry_Family, Current_Scope, Sloc (N), 'E');
      else
         Ityp := New_Internal_Entity
           (E_Entry, Current_Scope, Sloc (N), 'E');
      end if;

      Set_Etype          (Ityp, Standard_Void_Type);
      Set_Accept_Address (Ityp, New_Elmt_List);

      if Present (Formals) then
         New_Scope (Ityp);
         Process_Formals (Ityp, Formals, N);
         Create_Extra_Formals (Ityp);
         End_Scope;
      end if;

      --  We set the default expressions processed flag because we don't
      --  need default expression functions. This is really more like a
      --  body entity than a spec entity anyway.

      Set_Default_Expressions_Processed (Ityp);

      E := First_Entity (Etype (Task_Nam));

      while Present (E) loop
         if Chars (E) = Chars (Nam)
           and then (Ekind (E) = Ekind (Ityp))
           and then Type_Conformant (Ityp, E)
         then
            Entry_Nam := E;
            exit;
         end if;

         E := Next_Entity (E);
      end loop;

      if Entry_Nam = Any_Id then
         Error_Msg_N ("no entry declaration matches accept statement",  N);
         return;
      else
         Set_Entity (Nam, Entry_Nam);
      end if;

      --  Verify that the entry is not hidden by a procedure declared in
      --  the current block (pathological but possible).

      if Current_Scope /= Task_Nam then
         declare
            E1 : Entity_Id;

         begin
            E1 := First_Entity (Current_Scope);

            while Present (E1) loop

               if Ekind (E1) = E_Procedure
                 and then Type_Conformant (E1, Entry_Nam)
               then
                  Error_Msg_N ("entry name is not visible", N);
               end if;

               E1 := Next_Entity (E1);
            end loop;
         end;
      end if;

      Set_Convention (Ityp, Convention (Entry_Nam));
      Check_Fully_Conformant (Ityp, Entry_Nam, N);

      for J in reverse 0 .. Scope_Stack.Last loop
         exit when Task_Nam = Scope_Stack.Table (J).Entity;

         if Entry_Nam = Scope_Stack.Table (J).Entity then
            Error_Msg_N ("duplicate accept statement for same entry", N);
         end if;

      end loop;

      declare
         P : Node_Id := N;
      begin
         loop
            P := Parent (P);
            case Nkind (P) is
               when N_Task_Body | N_Compilation_Unit =>
                  exit;
               when N_Asynchronous_Select =>
                  Error_Msg_N ("accept statements are not allowed within" &
                               " an asynchronous select inner" &
                               " to the enclosing task body", N);
                  exit;
               when others =>
                  null;
            end case;
         end loop;
      end;

      if Ekind (E) = E_Entry_Family then
         if No (Index) then
            Error_Msg_N ("missing entry index in accept for entry family", N);
         else
            Analyze_And_Resolve (Index, Entry_Index_Type (E));
            Apply_Range_Check (Index, Entry_Index_Type (E));
         end if;

      elsif Present (Index) then
         Error_Msg_N ("invalid entry index in accept for simple entry", N);
      end if;

      --  If statements are present, they must be analyzed in the context
      --  of the entry, so that references to formals are correctly resolved.
      --  We also have to add the declarations that are required by the
      --  expansion of the accept statement in this case if expansion active.

      --  In the case of a select alternative of a selective accept,
      --  the expander references the address declaration even if there
      --  is no statement list.

      Exp_Ch9.Expand_Accept_Declarations (N, Entry_Nam);

      --  If label declarations present, analyze them. They are declared
      --  in the enclosing task, but their enclosing scope is the entry itself,
      --  so that goto's to the label are recognized as local to the accept.

      if Present (Declarations (N)) then

         declare
            Decl : Node_Id;
            Id   : Entity_Id;

         begin
            Decl := First (Declarations (N));

            while Present (Decl) loop
               Analyze (Decl);

               if Nkind (Decl) = N_Implicit_Label_Declaration then
                  Id := Defining_Identifier (Decl);
                  Set_Enclosing_Scope (Id, Entry_Nam);
               else
                  pragma Assert (False); null;
               end if;

               Decl := Next (Decl);
            end loop;
         end;
      end if;

      if Present (Stats) then
         New_Scope (Entry_Nam);
         Install_Declarations (Entry_Nam);

         Set_Actual_Subtypes (N, Current_Scope);
         Analyze (Stats);
         End_Scope;
      end if;

   end Analyze_Accept_Statement;

   ---------------------------------
   -- Analyze_Asynchronous_Select --
   ---------------------------------

   procedure Analyze_Asynchronous_Select (N : Node_Id) is
   begin
      Tasking_Used := True;
      Check_Restriction (Max_Asynchronous_Select_Nesting, N);

      Analyze (Triggering_Alternative (N));

      Analyze_Statements (Statements (Abortable_Part (N)));
   end Analyze_Asynchronous_Select;

   ------------------------------------
   -- Analyze_Conditional_Entry_Call --
   ------------------------------------

   procedure Analyze_Conditional_Entry_Call (N : Node_Id) is
   begin
      Tasking_Used := True;
      Analyze (Entry_Call_Alternative (N));
      Analyze_Statements (Else_Statements (N));
   end Analyze_Conditional_Entry_Call;

   --------------------------------
   -- Analyze_Delay_Alternative  --
   --------------------------------

   procedure Analyze_Delay_Alternative (N : Node_Id) is
   begin
      Tasking_Used := True;
      Check_Restriction (No_Delay, N);

      if Present (Pragmas_Before (N)) then
         Analyze_List (Pragmas_Before (N));
      end if;

      if Nkind (Parent (N)) = N_Selective_Accept then
         Analyze (Expression (Delay_Statement (N)));
      else
         Analyze (Delay_Statement (N));
      end if;

      if Present (Condition (N)) then
         Analyze_And_Resolve (Condition (N), Any_Boolean);
      end if;

      if Is_Non_Empty_List (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;
   end Analyze_Delay_Alternative;

   ----------------------------
   -- Analyze_Delay_Relative --
   ----------------------------

   procedure Analyze_Delay_Relative (N : Node_Id) is
      E : constant Node_Id := Expression (N);

   begin
      Tasking_Used := True;
      Check_Restriction (No_Delay, N);
      Analyze_And_Resolve (E,  Standard_Duration);
   end Analyze_Delay_Relative;

   -------------------------
   -- Analyze_Delay_Until --
   -------------------------

   procedure Analyze_Delay_Until (N : Node_Id) is
      E : constant Node_Id := Expression (N);

   begin
      Tasking_Used := True;
      Check_Restriction (No_Delay, N);
      Analyze (E);

      if Etype (E) /= Etype (RTE (RO_CA_Time)) and then
         Etype (E) /= Etype (RTE (RO_RT_Time))
      then
         Error_Msg_N ("expect Time types for `DELAY UNTIL`", E);
      end if;
   end Analyze_Delay_Until;

   ------------------------
   -- Analyze_Entry_Body --
   ------------------------

   procedure Analyze_Entry_Body (N : Node_Id) is
      Id         : constant Entity_Id := Defining_Identifier (N);
      Decls      : constant List_Id   := Declarations (N);
      Stats      : constant Node_Id   := Handled_Statement_Sequence (N);
      Formals    : constant Node_Id   := Entry_Body_Formal_Part (N);
      Cond       : constant Node_Id   := Condition (Formals);
      P_Type     : constant Entity_Id := Current_Scope;
      Entry_Name : Entity_Id;
      E          : Entity_Id;

   begin
      Tasking_Used := True;

      --  Entry_Name is initialized to Any_Id. It should get reset to the
      --  matching entry entity. An error is signalled if it is not reset

      Entry_Name := Any_Id;

      Analyze (Formals);

      if Present (Entry_Index_Specification (Formals)) then
         Set_Ekind (Id, E_Entry_Family);
      else
         Set_Ekind (Id, E_Entry);
      end if;

      Set_Etype          (Id, Standard_Void_Type);
      Set_Accept_Address (Id, New_Elmt_List);

      E := First_Entity (P_Type);
      while Present (E) loop
         if Chars (E) = Chars (Id)
           and then (Ekind (E) = Ekind (Id))
           and then Type_Conformant (Id, E)
         then
            Entry_Name := E;
            Set_Convention (Id, Convention (E));
            Check_Fully_Conformant (Id, E, N);
            exit;
         end if;

         E := Next_Entity (E);
      end loop;

      if Entry_Name = Any_Id then
         Error_Msg_N ("no entry declaration matches entry body",  N);

      elsif Has_Completion (Entry_Name) then
         Error_Msg_N ("duplicate entry body", N);
         return;

      else
         Set_Has_Completion (Entry_Name);
      end if;

      Exp_Ch9.Expand_Entry_Barrier (N, Entry_Name);
      New_Scope (Entry_Name);

      Exp_Ch9.Expand_Entry_Body_Declarations (N);
      Install_Declarations (Entry_Name);
      Set_Actual_Subtypes (N, Current_Scope);

      --  The entity for the protected subprogram corresponding to the entry
      --  has been created. We retain the name of this entity in the entry
      --  body, for use when the corresponding subprogram body is created.
      --  Note that entry bodies have to corresponding_spec, and there is no
      --  easy link back in the tree between the entry body and the entity for
      --  the entry itself.

      Set_Protected_Body_Subprogram (Id,
        Protected_Body_Subprogram (Entry_Name));

      if Present (Decls) then
         Analyze_Declarations (Decls);
      end if;

      if Present (Stats) then
         Analyze (Stats);
      end if;

      End_Scope;

      --  If this is an entry family, remove the loop created to provide
      --  a scope for the entry index.

      if Ekind (Id) = E_Entry_Family
        and then Present (Entry_Index_Specification (Formals))
      then
         End_Scope;
      end if;

   end Analyze_Entry_Body;

   ------------------------------------
   -- Analyze_Entry_Body_Formal_Part --
   ------------------------------------

   procedure Analyze_Entry_Body_Formal_Part (N : Node_Id) is
      Id      : constant Entity_Id := Defining_Identifier (Parent (N));
      Index   : constant Node_Id   := Entry_Index_Specification (N);
      Formals : constant List_Id   := Parameter_Specifications (N);

   begin
      Tasking_Used := True;

      if Present (Index) then
         Analyze (Index);
      end if;

      if Present (Formals) then
         Set_Scope (Id, Current_Scope);
         New_Scope (Id);
         Process_Formals (Id, Formals, Parent (N));
         End_Scope;
      end if;

   end Analyze_Entry_Body_Formal_Part;

   ------------------------------------
   -- Analyze_Entry_Call_Alternative --
   ------------------------------------

   procedure Analyze_Entry_Call_Alternative (N : Node_Id) is
   begin
      Tasking_Used := True;

      if Present (Pragmas_Before (N)) then
         Analyze_List (Pragmas_Before (N));
      end if;

      Analyze (Entry_Call_Statement (N));

      if Is_Non_Empty_List (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;
   end Analyze_Entry_Call_Alternative;

   -------------------------------
   -- Analyze_Entry_Declaration --
   -------------------------------

   procedure Analyze_Entry_Declaration (N : Node_Id) is
      Id       : Entity_Id := Defining_Identifier (N);
      D_Sdef   : Node_Id   := Discrete_Subtype_Definition (N);
      Formals  : List_Id   := Parameter_Specifications (N);
      Task_Ent : Entity_Id := Current_Scope;

   begin
      Tasking_Used := True;

      if No (D_Sdef) then
         Set_Ekind (Id, E_Entry);
      else
         Enter_Name (Id);
         Set_Ekind (Id, E_Entry_Family);
         Analyze (D_Sdef);
         Make_Index (D_Sdef, N, Id);
      end if;

      Set_Etype          (Id, Standard_Void_Type);
      Set_Convention     (Id, Convention_Entry);
      Set_Accept_Address (Id, New_Elmt_List);

      if Present (Formals) then
         Set_Scope (Id, Current_Scope);
         New_Scope (Id);
         Process_Formals (Id, Formals, N);
         Create_Extra_Formals (Id);
         End_Scope;
      end if;

      if Ekind (Id) = E_Entry then
         New_Overloaded_Entity (Id);
      end if;

   end Analyze_Entry_Declaration;

   ---------------------------------------
   -- Analyze_Entry_Index_Specification --
   ---------------------------------------

   --  The defining_Identifier of the entry index specification is local
   --  to the entry body, but must be available in the entry barrier,
   --  which is evaluated outside of the entry body. The index is eventually
   --  renamed as a run-time object, so is visibility is strictly a front-end
   --  concern. In order to make it available to the barrier, we create
   --  an additional scope, as for a loop, whose only declaration is the
   --  index name. This loop is not attached to the tree and does not appear
   --  as an entity local to the protected type, so its existence need only
   --  be knwown to routines that process entry families.

   procedure Analyze_Entry_Index_Specification (N : Node_Id) is
      Iden    : constant Node_Id := Defining_Identifier (N);
      Def     : constant Node_Id := Discrete_Subtype_Definition (N);
      Loop_Id : Entity_Id :=
                  Make_Defining_Identifier (Sloc (N),
                    Chars => New_Internal_Name ('L'));

   begin
      Tasking_Used := True;
      Analyze (Def);
      Make_Index (Def, N);
      Set_Ekind (Loop_Id, E_Loop);
      Set_Scope (Loop_Id, Current_Scope);
      New_Scope (Loop_Id);
      Enter_Name (Iden);
      Set_Ekind (Iden, E_Entry_Index_Parameter);
      Set_Etype (Iden, Etype (Def));
   end Analyze_Entry_Index_Specification;

   ----------------------------
   -- Analyze_Protected_Body --
   ----------------------------

   procedure Analyze_Protected_Body (N : Node_Id) is
      Body_Id   : constant Entity_Id := Defining_Identifier (N);
      Spec_Id   : Entity_Id;
      Last_E    : Entity_Id;

   begin
      Tasking_Used := True;
      Set_Ekind (Body_Id, E_Protected_Body);
      Spec_Id := Find_Concurrent_Spec (Body_Id);

      if Present (Spec_Id)
        and then Ekind (Spec_Id) = E_Protected_Type
      then
         null;

      elsif Present (Spec_Id)
        and then Ekind (Etype (Spec_Id)) = E_Protected_Type
        and then not Comes_From_Source (Etype (Spec_Id))
      then
         null;

      else
         Error_Msg_N ("missing specification for protected body", Body_Id);
         return;
      end if;

      --  The declarations are always attached to the type

      if Ekind (Spec_Id) /= E_Protected_Type then
         Spec_Id := Etype (Spec_Id);
      end if;

      New_Scope (Spec_Id);
      Set_Corresponding_Spec (N, Spec_Id);
      Set_Corresponding_Body (Parent (Spec_Id), Body_Id);
      Set_Has_Completion (Spec_Id);
      Install_Declarations (Spec_Id);

      Exp_Ch9.Expand_Protected_Body_Declarations (N, Spec_Id);

      Last_E := Last_Entity (Spec_Id);

      Analyze_Declarations (Declarations (N));

      --  For visibility purposes, all entities in the body are private.
      --  Set First_Private_Entity accordingly, if there was no private
      --  part in the protected declaration.

      if No (First_Private_Entity (Spec_Id)) then
         if Present (Last_E) then
            Set_First_Private_Entity (Spec_Id, Next_Entity (Last_E));
         else
            Set_First_Private_Entity (Spec_Id, First_Entity (Spec_Id));
         end if;
      end if;

      Check_Completion (Body_Id);
      End_Scope;
   end Analyze_Protected_Body;

   ----------------------------------
   -- Analyze_Protected_Definition --
   ----------------------------------

   procedure Analyze_Protected_Definition (N : Node_Id) is
      E : Entity_Id;
      L : Entity_Id;

   begin
      Tasking_Used := True;
      Analyze_Declarations (Visible_Declarations (N));

      if Present (Private_Declarations (N))
        and then not Is_Empty_List (Private_Declarations (N))
      then
         L := Last_Entity (Current_Scope);
         Analyze_Declarations (Private_Declarations (N));

         if Present (L) then
            Set_First_Private_Entity (Current_Scope, Next_Entity (L));

         else
            Set_First_Private_Entity (Current_Scope,
              First_Entity (Current_Scope));
         end if;
      end if;

      E := First_Entity (Current_Scope);

      while Present (E) loop

         if Ekind (E) = E_Function
           or else Ekind (E) = E_Procedure
         then
            Set_Convention (E, Convention_Protected);
         end if;

         E := Next_Entity (E);
      end loop;

      Check_Max_Entries (N, Max_Protected_Entries);
   end Analyze_Protected_Definition;

   ----------------------------
   -- Analyze_Protected_Type --
   ----------------------------

   procedure Analyze_Protected_Type (N : Node_Id) is
      E : Entity_Id;
      T : Entity_Id;

   begin
      Tasking_Used := True;
      Check_Restriction (No_Protected_Types, N);

      T := Find_Type_Name (N);

      if Ekind (T) = E_Incomplete_Type then
         T := Full_View (T);
      end if;

      Set_Ekind              (T, E_Protected_Type);
      Set_Esize              (T, Uint_0);
      Set_Etype              (T, T);
      Set_Is_First_Subtype   (T, True);
      Set_Has_Delayed_Freeze (T, True);
      Set_Girder_Constraint  (T, No_Elist);
      New_Scope (T);

      --  All protected types are controlled (because of the Protection
      --  component if nothing else).

      Set_Has_Controlled_Component (T, True);

      if Present (Discriminant_Specifications (N)) then
         if Has_Discriminants (T) then

            --  Install discriminants. Also, verify conformance of
            --  discriminants of previous and current view.  ???

            Install_Declarations (T);
         else
            Process_Discriminants (N);
         end if;
      end if;

      Analyze (Protected_Definition (N));

      --  The Ekind of components is E_Void during analysis to detect
      --  illegal uses. Now it can be set correctly.

      E := First_Entity (Current_Scope);

      while Present (E) loop
         if Ekind (E) = E_Void then
            Set_Ekind (E, E_Component);
         end if;

         E := Next_Entity (E);
      end loop;

      End_Scope;
   end Analyze_Protected_Type;

   ---------------------
   -- Analyze_Requeue --
   ---------------------

   procedure Analyze_Requeue (N : Node_Id) is
      Entry_Name : Node_Id := Name (N);
      Entry_Id   : Entity_Id;
      Found      : Boolean;
      I          : Interp_Index;
      It         : Interp;
      Enclosing  : Entity_Id;

   begin
      Tasking_Used := True;

      for J in reverse 0 .. Scope_Stack.Last loop
         Enclosing := Scope_Stack.Table (J).Entity;
         exit when Is_Entry (Enclosing);

         if Ekind (Enclosing) /= E_Block
           and then Ekind (Enclosing) /= E_Loop
         then
            Error_Msg_N ("requeue must appear within accept or entry body", N);
            return;
         end if;
      end loop;

      Analyze (Entry_Name);

      if Etype (Entry_Name) = Any_Type then
         return;
      end if;

      if Nkind (Entry_Name) = N_Selected_Component then
         Entry_Name := Selector_Name (Entry_Name);
      end if;

      --  Overloaded case, find right interpretation

      if Is_Overloaded (Entry_Name) then
         Get_First_Interp (Entry_Name, I, It);
         Found := False;

         while Present (It.Nam) loop

            if No (First_Formal (It.Nam))
              or else Subtype_Conformant (Enclosing, It.Nam)
            then
               if not Found then
                  Found := True;
                  Entry_Id := It.Nam;
               else
                  Error_Msg_N ("ambiguous entry name in requeue", N);
                  return;
               end if;
            end if;

            Get_Next_Interp (I, It);
         end loop;

         if not Found then
            Error_Msg_N ("no entry matches context",  N);
            return;
         else
            Set_Entity (Entry_Name, Entry_Id);
         end if;

      --  Non-overloaded cases

      --  For the case of a reference to an element of an entry family,
      --  the Entry_Name is an indexed component.

      elsif Nkind (Entry_Name) = N_Indexed_Component then

         --  Requeue to an entry out of the body

         if Nkind (Prefix (Entry_Name)) = N_Selected_Component then
            Entry_Id := Entity (Selector_Name (Prefix (Entry_Name)));

         --  Requeue from within the body itself

         elsif Nkind (Prefix (Entry_Name)) = N_Identifier then
            Entry_Id := Entity (Prefix (Entry_Name));

         else
            Error_Msg_N ("invalid entry_name specified",  N);
            return;
         end if;

      --  If we had a requeue of the form REQUEUE A (B), then the parser
      --  accepted it (because it could have been a requeue on an entry
      --  index. If A turns out not to be an entry family, then the analysis
      --  of A (B) turned it into a function call.

      elsif Nkind (Entry_Name) = N_Function_Call then
         Error_Msg_N
           ("arguments not allowed in requeue statement",
            First (Parameter_Associations (Entry_Name)));
         return;

      --  Normal case of no entry family, no argument

      else
         Entry_Id := Entity (Entry_Name);
      end if;

      --  Resolve entry, and check that it is subtype conformant with the
      --  enclosing construct if this construct has formals (RM 9.5.4(5)).

      if not Is_Entry (Entry_Id) then
         Error_Msg_N ("expect entry name in requeue statement", Name (N));
      else
         Resolve_Entry (Name (N));

         if Present (First_Formal (Entry_Id)) then
            Check_Subtype_Conformant (Enclosing, Entry_Id, Name (N));
         end if;
      end if;

   end Analyze_Requeue;

   ------------------------------
   -- Analyze_Selective_Accept --
   ------------------------------

   procedure Analyze_Selective_Accept (N : Node_Id) is
      Alts : constant List_Id := Select_Alternatives (N);
      Alt  : Node_Id;

      Accept_Present    : Boolean := False;
      Terminate_Present : Boolean := False;
      Delay_Present     : Boolean := False;
      Alt_Count         : Uint    := Uint_0;

   begin
      Tasking_Used := True;

      Alt := First (Alts);
      while Present (Alt) loop
         Alt_Count := Alt_Count + 1;
         Analyze (Alt);

         if Nkind (Alt) = N_Delay_Alternative then
            Delay_Present := True;

         elsif Nkind (Alt) = N_Terminate_Alternative then
            if Terminate_Present then
               Error_Msg_N ("Only one terminate alternative allowed", N);
            else
               Terminate_Present := True;
               Check_Restriction (No_Terminate_Alternatives, N);
            end if;

         else
            Accept_Present := True;
         end if;

         Alt := Next (Alt);
      end loop;

      Check_Restriction (Max_Select_Alternatives, Alt_Count, N);

      if Terminate_Present and Delay_Present then
         Error_Msg_N ("at most one of terminate or delay alternative", N);

      elsif not Accept_Present then
         Error_Msg_N
           ("select must contain at least one accept alternative", N);
      end if;

      if Present (Else_Statements (N)) then
         if Terminate_Present or Delay_Present then
            Error_Msg_N ("else part not allowed with other alternatives", N);
         end if;

         Analyze_Statements (Else_Statements (N));
      end if;
   end Analyze_Selective_Accept;

   ------------------------------
   -- Analyze_Single_Protected --
   ------------------------------

   procedure Analyze_Single_Protected (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Id     : constant Node_Id    := Defining_Identifier (N);
      T      : Entity_Id;
      T_Decl : Node_Id;
      O_Decl : Node_Id;

   begin
      Tasking_Used := True;

      --  The node is rewritten as a protected type declaration,
      --  in exact analogy with what is done with single tasks.

      T :=
        Make_Defining_Identifier (Sloc (Id),
          New_External_Name (Chars (Id), 'T'));

      T_Decl :=
        Make_Protected_Type_Declaration (Loc,
         Defining_Identifier => T,
         Protected_Definition => Relocate_Node (Protected_Definition (N)));

      O_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => New_Copy (Id),
          Object_Definition   => Make_Identifier (Loc,  Chars (T)));

      Rewrite_Substitute_Tree (N, T_Decl);
      Insert_After (N, O_Decl);
      Mark_Rewrite_Insertion (O_Decl);

      --  Instead of calling Analyze on the new node,  call directly
      --  the proper analysis procedure. Otherwise the node would be
      --  expanded twice, with disastrous result.

      Analyze_Protected_Type (N);

   end Analyze_Single_Protected;

   -------------------------
   -- Analyze_Single_Task --
   -------------------------

   procedure Analyze_Single_Task (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Id     : constant Node_Id    := Defining_Identifier (N);
      T      : Entity_Id;
      T_Decl : Node_Id;
      O_Decl : Node_Id;

   begin
      Tasking_Used := True;

      --  The node is rewritten as a task type declaration,  followed
      --  by an object declaration of that anonymous task type.

      T :=
        Make_Defining_Identifier (Sloc (Id),
          New_External_Name (Chars (Id), 'T'));

      T_Decl :=
        Make_Task_Type_Declaration (Loc,
          Defining_Identifier => T,
          Task_Definition     => Relocate_Node (Task_Definition (N)));

      O_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => New_Copy (Id),
          Object_Definition   => Make_Identifier (Loc, Chars (T)));

      Rewrite_Substitute_Tree (N, T_Decl);
      Insert_After (N, O_Decl);
      Mark_Rewrite_Insertion (O_Decl);

      --  Instead of calling Analyze on the new node,  call directly
      --  the proper analysis procedure. Otherwise the node would be
      --  expanded twice, with disastrous result.

      Analyze_Task_Type (N);

   end Analyze_Single_Task;

   -----------------------
   -- Analyze_Task_Body --
   -----------------------

   procedure Analyze_Task_Body (N : Node_Id) is
      Body_Id : constant Entity_Id := Defining_Identifier (N);
      Spec_Id : Entity_Id;
      Last_E  : Entity_Id;

   begin
      Tasking_Used := True;
      Set_Ekind (Body_Id, E_Task_Body);
      Spec_Id := Find_Concurrent_Spec (Body_Id);

      --  The spec is either a task type declaration, or a single task
      --  declaration for which we have created an anonymous type.

      if Present (Spec_Id)
        and then Ekind (Spec_Id) = E_Task_Type
      then
         null;

      elsif Present (Spec_Id)
        and then Ekind (Etype (Spec_Id)) = E_Task_Type
        and then not Comes_From_Source (Etype (Spec_Id))
      then
         null;

      else
         Error_Msg_N ("missing specification for task body", Body_Id);
         return;
      end if;

      --  Deal with case of body of single task (anonymous type was created)

      if Ekind (Spec_Id) = E_Variable then
         Spec_Id := Etype (Spec_Id);
      end if;

      New_Scope (Spec_Id);
      Set_Corresponding_Spec (N, Spec_Id);
      Set_Corresponding_Body (Parent (Spec_Id), Body_Id);
      Set_Has_Completion (Spec_Id);
      Install_Declarations (Spec_Id);
      Last_E := Last_Entity (Spec_Id);

      Analyze_Declarations (Declarations (N));

      --  For visibility purposes, all entities in the body are private.
      --  Set First_Private_Entity accordingly, if there was no private
      --  part in the protected declaration.

      if No (First_Private_Entity (Spec_Id)) then
         if Present (Last_E) then
            Set_First_Private_Entity (Spec_Id, Next_Entity (Last_E));
         else
            Set_First_Private_Entity (Spec_Id, First_Entity (Spec_Id));
         end if;
      end if;

      Analyze (Handled_Statement_Sequence (N));
      Check_Completion (Body_Id);
      Check_Unset_Variables (Body_Id);
      End_Scope;
   end Analyze_Task_Body;

   -----------------------------
   -- Analyze_Task_Definition --
   -----------------------------

   procedure Analyze_Task_Definition (N : Node_Id) is
      L : Entity_Id;

   begin
      Tasking_Used := True;

      if Present (Visible_Declarations (N)) then
         Analyze_Declarations (Visible_Declarations (N));
      end if;

      if Present (Private_Declarations (N)) then
         L := Last_Entity (Current_Scope);
         Analyze_Declarations (Private_Declarations (N));

         if Present (L) then
            Set_First_Private_Entity
              (Current_Scope, Next_Entity (L));
         else
            Set_First_Private_Entity
              (Current_Scope, First_Entity (Current_Scope));
         end if;
      end if;

      Check_Max_Entries (N, Max_Task_Entries);
   end Analyze_Task_Definition;

   -----------------------
   -- Analyze_Task_Type --
   -----------------------

   procedure Analyze_Task_Type (N : Node_Id) is
      T : Entity_Id;

   begin
      Tasking_Used := True;
      Check_Restriction (Max_Tasks, N);
      T := Find_Type_Name (N);

      if Ekind (T) = E_Incomplete_Type then
         T := Full_View (T);
      end if;

      Set_Ekind              (T, E_Task_Type);
      Set_Is_First_Subtype   (T, True);
      Set_Has_Task           (T, True);
      Set_Esize              (T, Uint_0);
      Set_Etype              (T, T);
      Set_Has_Delayed_Freeze (T, True);
      Set_Girder_Constraint (T, No_Elist);
      New_Scope (T);

      if Present (Discriminant_Specifications (N)) then
         Note_Feature (Task_Discriminants, Sloc (N));

         if Ada_83 and then Comes_From_Source (N) then
            Error_Msg_N ("(Ada 83) task discriminant not allowed!", N);
         end if;

         if Has_Discriminants (T) then

            --  Install discriminants. Also, verify conformance of
            --  discriminants of previous and current view.  ???

            Install_Declarations (T);
         else
            Process_Discriminants (N);
         end if;
      end if;

      if Present (Task_Definition (N)) then
         Analyze_Task_Definition (Task_Definition (N));
      end if;

      if not Is_Library_Level_Entity (T) then
         Check_Restriction (No_Task_Hierarchy, N);
      end if;

      End_Scope;
   end Analyze_Task_Type;

   -----------------------------------
   -- Analyze_Terminate_Alternative --
   -----------------------------------

   procedure Analyze_Terminate_Alternative (N : Node_Id) is
   begin
      Tasking_Used := True;

      if Present (Pragmas_Before (N)) then
         Analyze_List (Pragmas_Before (N));
      end if;

      if Present (Condition (N)) then
         Analyze_And_Resolve (Condition (N), Any_Boolean);
      end if;
   end Analyze_Terminate_Alternative;

   ------------------------------
   -- Analyze_Timed_Entry_Call --
   ------------------------------

   procedure Analyze_Timed_Entry_Call (N : Node_Id) is
   begin
      Tasking_Used := True;
      Analyze (Entry_Call_Alternative (N));
      Analyze (Delay_Alternative (N));
   end Analyze_Timed_Entry_Call;

   ------------------------------------
   -- Analyze_Triggering_Alternative --
   ------------------------------------

   procedure Analyze_Triggering_Alternative (N : Node_Id) is
      Trigger : Node_Id := Triggering_Statement (N);
   begin
      Tasking_Used := True;

      if Present (Pragmas_Before (N)) then
         Analyze_List (Pragmas_Before (N));
      end if;

      Analyze (Trigger);
      if Comes_From_Source (Trigger)
        and then Nkind (Trigger) /= N_Delay_Until_Statement
        and then Nkind (Trigger) /= N_Delay_Relative_Statement
        and then Nkind (Trigger) /= N_Entry_Call_Statement
      then
         Error_Msg_N
          ("triggering statement must be delay or entry call", Trigger);
      end if;

      if Is_Non_Empty_List (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;
   end Analyze_Triggering_Alternative;

   -----------------------
   -- Check_Max_Entries --
   -----------------------

   procedure Check_Max_Entries (Def : Node_Id; R : Restriction_Parameter_Id) is
      Ecount : Uint;

      procedure Count (L : List_Id);
      --  Count entries in given declaration list

      procedure Count (L : List_Id) is
         D : Node_Id;

      begin
         if No (L) then
            return;
         end if;

         D := First (L);
         while Present (D) loop
            if Nkind (D) = N_Entry_Declaration then
               declare
                  DSD : constant Node_Id :=
                          Discrete_Subtype_Definition (D);

               begin
                  if No (DSD) then
                     Ecount := Ecount + 1;

                  elsif Is_OK_Static_Subtype (Etype (DSD)) then
                     declare
                        Lo : constant Uint :=
                               Expr_Value
                                 (Type_Low_Bound (Etype (DSD)));
                        Hi : constant Uint :=
                               Expr_Value
                                 (Type_High_Bound (Etype (DSD)));

                     begin
                        if Hi >= Lo then
                           Ecount := Ecount + Hi - Lo + 1;
                        end if;
                     end;

                  else

                     Error_Msg_N
                       ("static subtype required by Restriction pragma", DSD);
                  end if;
               end;
            end if;

            D := Next (D);
         end loop;
      end Count;

   --  Start of processing for Check_Max_Entries

   begin
      if Restriction_Parameters (R) >= 0 then
         Ecount := Uint_0;
         Count (Visible_Declarations (Def));
         Count (Private_Declarations (Def));
         Check_Restriction (R, Ecount, Def);
      end if;
   end Check_Max_Entries;

   --------------------------
   -- Find_Concurrent_Spec --
   --------------------------

   function Find_Concurrent_Spec (Body_Id : Entity_Id) return Entity_Id is
      Spec_Id : Entity_Id := Current_Entity_In_Scope (Body_Id);

   begin
      --  The type may have been given by an incomplete type declaration.
      --  Find full view now.

      if Present (Spec_Id) and then Ekind (Spec_Id) = E_Incomplete_Type then
         Spec_Id := Full_View (Spec_Id);
      end if;

      return Spec_Id;
   end Find_Concurrent_Spec;

   --------------------------
   -- Install_Declarations --
   --------------------------

   procedure Install_Declarations (Spec : Entity_Id) is
      E    : Entity_Id;
      Prev : Entity_Id;

   begin
      E := First_Entity (Spec);

      while Present (E) loop
         Prev := Current_Entity (E);
         Set_Current_Entity (E);
         Set_Is_Immediately_Visible (E);
         Set_Homonym (E, Prev);
         E := Next_Entity (E);
      end loop;
   end Install_Declarations;

begin
   null;
end Sem_Ch9;

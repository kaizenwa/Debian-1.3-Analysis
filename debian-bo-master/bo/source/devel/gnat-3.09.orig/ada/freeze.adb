------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               F R E E Z E                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.122 $                            --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Pakd; use Exp_Pakd;
with Exp_Util; use Exp_Util;
with Itypes;   use Itypes;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Freeze is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Build_Renamed_Body
     (Decl  : Node_Id;
      New_S : Entity_Id;
      After : in out Node_Id);
   --  Rewrite renaming declaration as a subprogram body, whose single
   --  statement is a call to the renamed entity. New_S is the entity that
   --  appears in the renaming declaration. If this is a Renaming_As_Body,
   --  then Decl is the original subprogram declaration that is completed
   --  by the renaming, otherwise it is the renaming declaration itself.

   procedure Check_Strict_Alignment (E : Entity_Id);
   --  E is a base type.  If E is tagged or has a component that is aliased
   --  or tagged or contains something this is aliased or tagged, set
   --  Strict_Alignment.

   procedure Check_Unsigned_Type (E : Entity_Id);
   pragma Inline (Check_Unsigned_Type);
   --  If E is a fixed-point or discrete type, then all the necessary work
   --  to freeze it is completed except for possible setting of the flag
   --  Is_Unsigned_Type, which is done by this procedure. The call has no
   --  effect if the entity E is not a discrete or fixed-point type.

   procedure Freeze_And_Append
     (Ent    : Entity_Id;
      Loc    : Source_Ptr;
      Result : in out List_Id);
   --  Freezes Ent using Freeze_Entity, and appends the resulting list of
   --  nodes to Result, modifying Result from No_List if necessary.

   procedure Freeze_Enumeration_Type (Typ : Entity_Id);
   --  Freeze enumeration type. The Esize field is set as processing
   --  proceeds (i.e. set by default when the type is declared and then
   --  adjusted by rep clauses. What this procedure does is to make sure
   --  that if a foreign convention is specified, and no specific size
   --  is given, then the size must be at least Integer'Size.

   procedure Freeze_Fixed_Point_Type (Typ : Entity_Id);
   --  Freeze fixed point type. For fixed-point types, we have to defer
   --  setting the size and bounds till the freeze point, since they are
   --  potentially affected by the presence of size and small clauses.

   procedure Freeze_Subprogram (E : Entity_Id);
   --  Perform freezing actions for a subprogram (create extra formals,
   --  and set proper default mechanism values). Note that this routine
   --  is not called for internal subprograms, for which neither of these
   --  actions is needed (or desirable, we do not want for example to have
   --  these extra formals present in initialization procedures, where they
   --  would serve no purpose). In this call E is either a subprogram or
   --  a subprogram type (i.e. an access to a subprogram).

   procedure Process_Default_Expressions
     (E     : Entity_Id;
      After : in out Node_Id);
   --  This procedure is called for each subprogram to complete processing
   --  of default expressions at the point where all types are known to be
   --  frozen. In cases where no default expression function is required,
   --  all that is done is to do a full analyze of the default expression,
   --  to make sure that all error processing is done. For the case where
   --  a default expression function was created, the body is built.

   procedure Set_Component_Alignment_If_Not_Set (Typ : Entity_Id);
   --  Typ is a record or array type that is being frozen. This routine
   --  sets the default component alignment from the scope stack values
   --  if the alignment is otherwise not specified.

   ------------------------
   -- Build_Renamed_Body --
   ------------------------

   procedure Build_Renamed_Body
     (Decl  : Node_Id;
      New_S : Entity_Id;
      After : in out Node_Id)
   is
      N          : constant Node_Id := Get_Declaration_Node (New_S);
      Loc        : constant Source_Ptr := Sloc (N);
      Nam        : constant Node_Id := Name (N);
      Old_S      : Entity_Id;
      Spec       : constant Node_Id := New_Copy_Tree (Specification (Decl));
      Actuals    : List_Id := No_List;
      Call_Node  : Node_Id;
      Call_Name  : Node_Id;
      Body_Node  : Node_Id;
      Formal     : Entity_Id;
      O_Formal   : Entity_Id;
      Param_Spec : Node_Id;

   begin
      --  Determine the entity being renamed, which is the target of the
      --  call statement. If the name is an explicit dereference, this is
      --  a renaming of a subprogram type rather than a subprogram. The
      --  name itself is fully analyzed.

      if Nkind (Nam) = N_Selected_Component then
         Old_S := Entity (Selector_Name (Nam));

      elsif Nkind (Nam) = N_Explicit_Dereference then
         Old_S := Etype (Nam);

      elsif Nkind (Nam) = N_Indexed_Component then

         if Is_Entity_Name (Prefix (Nam)) then
            Old_S := Entity (Prefix (Nam));
         else
            Old_S := Entity (Selector_Name (Prefix (Nam)));
         end if;

      elsif Nkind (Nam) = N_Character_Literal then
         Old_S := Etype (New_S);

      else
         Old_S := Entity (Nam);
      end if;

      Call_Name := New_Copy (Name (N));
      Formal := First_Formal (Defining_Entity (Decl));

      if Present (Formal) then
         Actuals := New_List;

         while Present (Formal) loop
            Append (New_Reference_To (Formal, Loc), Actuals);
            Formal := Next_Formal (Formal);
         end loop;
      end if;

      --  If the renamed entity is an entry, inherit its profile. For
      --  other renamings as bodies, both profiles must be subtype
      --  conformant, so it is not necessary to replace the profile given
      --  in the declaration.

      if Is_Entry (Old_S) then
         Formal := First_Formal (Defining_Entity (Decl));

         if Present (Formal) then
            O_Formal := First_Formal (Old_S);
            Param_Spec := First (Parameter_Specifications (Spec));

            while Present (Formal) loop

               if Nkind (Parameter_Type (Param_Spec)) /=
                                                    N_Access_Definition
               then
                  Set_Entity (Parameter_Type (Param_Spec), Etype (O_Formal));
               end if;

               Formal := Next_Formal (Formal);
               O_Formal := Next_Formal (O_Formal);
               Param_Spec := Next (Param_Spec);
            end loop;
         end if;
      end if;

      --  If the renamed entity is a function, the generated body contains a
      --  return statement. Otherwise, build a procedure call. If the entity is
      --  an entry, subsequent analysis of the call will transform it into the
      --  proper entry or protected operation call. If the renamed entity is
      --  a character literal, return it directly.

      if Ekind (Old_S) = E_Function
        or else Ekind (Old_S) = E_Operator
        or else (Ekind (Old_S) = E_Subprogram_Type
                  and then Etype (Old_S) /= Standard_Void_Type)
      then
         Call_Node := Make_Return_Statement (Loc,
            Expression =>
              Make_Function_Call (Loc,
                Name => Call_Name,
                Parameter_Associations => Actuals));

      elsif Nkind (Nam) = N_Character_Literal then
         Call_Node := Make_Return_Statement (Loc,
            Expression => Call_Name);

      else
         Call_Node := Make_Procedure_Call_Statement (Loc,
           Name => Call_Name,
           Parameter_Associations => Actuals);
      end if;

      Set_Defining_Unit_Name (Spec,
        Make_Defining_Identifier (Loc, Chars => Chars (New_S)));

      Body_Node := Make_Subprogram_Body (Loc,
        Specification => Spec,
        Declarations => New_List,
        Handled_Statement_Sequence =>
          Make_Handled_Sequence_Of_Statements (Loc,
            Statements => New_List (Call_Node)));

      Insert_After (After, Body_Node);

      if Nkind (Decl) /= N_Subprogram_Declaration then
         Rewrite_Substitute_Tree (N,
           Make_Subprogram_Declaration (Loc,
             Specification => Specification (N)));
      end if;

      Analyze (Body_Node);
      After := Body_Node;
   end Build_Renamed_Body;

   -----------------------------
   -- Check_Compile_Time_Size --
   -----------------------------

   procedure Check_Compile_Time_Size (T : Entity_Id) is

      procedure Set_Small_Size (S : Uint);
      --  Sets the compile time known size (32 bits or less) in the Esize
      --  field, checking for a size clause that was given which attempts
      --  to give a smaller size.

      function Size_Known (T : Entity_Id) return Boolean;
      --  Recursive function that does all the work.
      --  Is this right??? isn't recursive case already handled???
      --  certainly yes for normal call, but what about bogus sem_res call???

      ----------------
      -- Size_Known --
      ----------------

      function Size_Known (T : Entity_Id) return Boolean is
         Index : Entity_Id;
         Comp  : Entity_Id;
         Low   : Node_Id;
         High  : Node_Id;

      begin
         if Size_Known_At_Compile_Time (T) then
            return True;

         elsif Is_Scalar_Type (T)
           or else Is_Task_Type (T)
         then
            return not Is_Generic_Type (T);

         elsif Is_Array_Type (T) then
            if not Is_Constrained (T) then
               return False;

            elsif not Size_Known (Component_Type (T)) then
               return False;
            end if;

            --  Check for all indexes static, and also compute possible
            --  size (in case it is less than 32 and may be packable).

            declare
               Esiz : Uint := Component_Size (T);
               Dim  : Uint;

            begin
               Index := First_Index (T);

               while Present (Index) loop
                  if Nkind (Index) = N_Range then
                     Get_Index_Bounds (Index, Low, High);
                  else
                     Low  := Type_Low_Bound (Etype (Index));
                     High := Type_High_Bound (Etype (Index));
                  end if;

                  if not Compile_Time_Known_Value (Low)
                    or else not Compile_Time_Known_Value (High)
                    or else Etype (Index) = Any_Type
                  then
                     return False;

                  else
                     Dim := Expr_Value (High) - Expr_Value (Low) + 1;

                     if Dim >= 0 then
                        Esiz := Esiz * Dim;
                     else
                        Esiz := Uint_0;
                     end if;
                  end if;

                  Index := Next_Index (Index);
               end loop;

               Set_Small_Size (Esiz);
               return True;
            end;

         elsif Is_Access_Type (T) then
            return True;

         elsif Is_Private_Type (T)
           and then not Is_Generic_Type (T)
           and then Present (Underlying_Type (T))
         then
            return Size_Known (Underlying_Type (T));

         elsif Is_Record_Type (T) then
            if Is_Class_Wide_Type (T) then
               return False;

            elsif T /= Base_Type (T) then
               return Size_Known_At_Compile_Time (Base_Type (T));

            else
               declare
                  Packed_Size_Known : Boolean := Is_Packed (T);
                  Packed_Size       : Uint    := Uint_0;

               begin
                  --  Test for variant part present

                  if Has_Discriminants (T)
                    and then Present (Parent (T))
                    and then Nkind (Parent (T)) = N_Full_Type_Declaration
                    and then Nkind (Type_Definition (Parent (T)))
                      = N_Record_Definition
                    and then not Null_Present (Type_Definition (Parent (T)))
                    and then Present (Variant_Part
                       (Component_List (Type_Definition (Parent (T)))))
                  then
                     --  If variant part is present, and type is unconstrained,
                     --  then we must have defaulted discriminants, or a size
                     --  clause must be present for the type, or else the size
                     --  is definitely not known at compile time.

                     if not Is_Constrained (T)
                       and then
                         No (Discriminant_Default_Value
                              (First_Discriminant (T)))
                       and then Esize (T) = Uint_0
                     then
                        return False;
                     else
                        Packed_Size_Known := False;
                     end if;
                  end if;


                  Comp := First_Component (T);

                  while Present (Comp) loop
                     if not Is_Type (Comp) then
                        if not Size_Known (Etype (Comp)) then
                           return False;

                        elsif Packed_Size_Known then
                           if Esize (Etype (Comp)) /= 0 then
                              Packed_Size :=
                                Packed_Size + Get_RM_Size (Etype (Comp));
                           else
                              Packed_Size_Known := False;
                           end if;
                        end if;
                     end if;

                     Comp := Next_Component (Comp);
                  end loop;

                  if Packed_Size_Known then
                     Set_Small_Size (Packed_Size);
                  end if;

                  return True;
               end;
            end if;

         else
            return False;
         end if;
      end Size_Known;

      --------------------
      -- Set_Small_Size --
      --------------------

      procedure Set_Small_Size (S : Uint) is
      begin
         if S > 32 then
            return;

         elsif Has_Size_Clause (T) then
            if Esize (T) < S then
               Error_Msg_Uint_1 := S;
               Error_Msg_NE
                 ("size for & is too small, minimum is ^",
                  Size_Clause (T), T);
            end if;

         elsif Esize (T) = Uint_0 then
            Set_Esize (T, S);
         end if;
      end Set_Small_Size;

   --  Start of processing for Check_Compile_Time_Size

   begin
      Set_Size_Known_At_Compile_Time (T, Size_Known (T));
   end Check_Compile_Time_Size;

   ----------------------------
   -- Check_Strict_Alignment --
   ----------------------------

   procedure Check_Strict_Alignment (E : Entity_Id) is

      Comp  : Entity_Id;

   begin
      if Is_Tagged_Type (E) or else Is_Concurrent_Type (E) then
         Set_Strict_Alignment (E);

      elsif Is_Array_Type (E) then
         Set_Strict_Alignment (E, Strict_Alignment (Component_Type (E)));

      elsif Is_Record_Type (E) then
         if Is_Limited_Record (E) then
            Set_Strict_Alignment (E);
            return;
         end if;

         Comp := First_Component (E);

         while Present (Comp) loop
            if not Is_Type (Comp)
              and then (Strict_Alignment (Etype (Comp))
                        or else Is_Aliased (Comp))
            then
               Set_Strict_Alignment (E);
               return;
            end if;

            Comp := Next_Component (Comp);
         end loop;
      end if;

   end Check_Strict_Alignment;

   -------------------------
   -- Check_Unsigned_Type --
   -------------------------

   procedure Check_Unsigned_Type (E : Entity_Id) is
      Ancestor : Entity_Id;
      Lo_Bound : Node_Id;
      Btyp     : Entity_Id;

   begin
      if not Is_Discrete_Or_Fixed_Point_Type (E) then
         return;
      end if;

      --  The situation that is non trivial is something like

      --     subtype x1 is integer range -10 .. +10;
      --     subtype x2 is x1 range 0 .. V1;
      --     subtype x3 is x2 range V2 .. V3;
      --     subtype x4 is x3 range V4 .. V5;

      --  where Vn are variables. Here the base type is signed, but we still
      --  know that x4 is unsigned because of the lower bound of x2.

      --  The only way to deal with this is to look up the ancestor chain

      Ancestor := E;
      loop
         if Ancestor = Any_Type or else Etype (Ancestor) = Any_Type then
            return;
         end if;

         Lo_Bound := Type_Low_Bound (Ancestor);

         if Compile_Time_Known_Value (Lo_Bound) then

            if Expr_Rep_Value (Lo_Bound) >= 0 then
               Set_Is_Unsigned_Type (E, True);
            end if;

            return;

         else
            Ancestor := Ancestor_Subtype (Ancestor);

            --  If no ancestor had a static lower bound, go to base type

            if No (Ancestor) then

               --  Note: the reason we still check for a compile time known
               --  value for the base type is that at least in the case of
               --  generic formals, we can have bounds that fail this test,
               --  and there may be other cases in error situations.

               Btyp := Base_Type (E);

               if Btyp = Any_Type or else Etype (Btyp) = Any_Type then
                  return;
               end if;

               Lo_Bound := Type_Low_Bound (Base_Type (E));

               if Compile_Time_Known_Value (Lo_Bound)
                 and then Expr_Rep_Value (Lo_Bound) >= 0
               then
                  Set_Is_Unsigned_Type (E, True);
               end if;

               return;

            end if;
         end if;
      end loop;
   end Check_Unsigned_Type;

   ----------------
   -- Freeze_All --
   ----------------

   --  Note: the easy coding for this procedure would be to just build a
   --  single list of freeze nodes and then insert them and analyze them
   --  all at once. This won't work, because the analysis of earlier freeze
   --  nodes may recursively freeze types which would otherwise appear later
   --  on in the freeze list. So we must analyze and expand the freeze nodes
   --  as they are generated.

   procedure Freeze_All (From : Entity_Id; After : in out Node_Id) is
      Loc   : constant Source_Ptr := Sloc (Last_Entity (Current_Scope));
      E     : Entity_Id;
      Decl  : Node_Id;

      procedure Freeze_All_Ent (From : Entity_Id; After : in out Node_Id);
      --  This is the internal recursive routine that does freezing of
      --  entities (but NOT the analysis of default expressions, which
      --  should not be recursive, we don't want to analyze those till
      --  we are sure that ALL the types are frozen).

      procedure Freeze_All_Ent
        (From  : Entity_Id;
         After : in out Node_Id)
      is
         E     : Entity_Id;
         Flist : List_Id;
         Lastn : Node_Id;

      begin
         E := From;
         while Present (E) loop

            --  If the entity is an inner package which is not a package
            --  renaming, then its entities must be frozen at this point.
            --  Note that such entities do NOT get frozen at the end of
            --  the nested package itself (only library packages freeze).

            --  Same is true for task declarations, where anonymous records
            --  created for entry parameters must be frozen.

            if Ekind (E) = E_Package
              and then No (Renamed_Object (E))
              and then not Is_Child_Unit (E)
            then
               New_Scope (E);
               Install_Visible_Declarations (E);
               Install_Private_Declarations (E);

               Freeze_All (First_Entity (E), After);

               End_Package_Scope (E);

            elsif Ekind (E) in Task_Kind
              and then
                (Nkind (Parent (E)) = N_Task_Type_Declaration
                  or else
                 Nkind (Parent (E)) = N_Single_Task_Declaration)
            then
               New_Scope (E);
               Freeze_All (First_Entity (E), After);
               End_Scope;

            elsif Ekind (E) = E_Record_Type
              or else  Ekind (E) = E_Record_Subtype
            then
               Freeze_All (First_Entity (E), After);

            end if;

            if not Is_Frozen (E) then
               Flist := Freeze_Entity (E, Loc);

               if Is_Non_Empty_List (Flist) then
                  Lastn := Next (After);
                  Insert_List_After_And_Analyze (After, Flist);

                  if Present (Lastn) then
                     After := Prev (Lastn);
                  else
                     After := Last (List_Containing (After));
                  end if;
               end if;
            end if;

            E := Next_Entity (E);
         end loop;
      end Freeze_All_Ent;

   --  Start of processing for Freeze_All

   begin
      Freeze_All_Ent (From, After);

      --  Now that all types are frozen, we can deal with default expressions
      --  that require us to build a default expression functions. This is the
      --  point at which such functions are constructed (after all types that
      --  might be used in such expressions have been frozen).

      --  Loop through entities

      E := From;
      while Present (E) loop

         if Is_Subprogram (E) then

            if not Default_Expressions_Processed (E) then
               Process_Default_Expressions (E, After);
            end if;

            if not Has_Completion (E) then
               Decl := Get_Declaration_Node (E);

               if Nkind (Decl) = N_Subprogram_Renaming_Declaration then
                  Build_Renamed_Body (Decl, E, After);

               elsif Nkind (Decl) = N_Subprogram_Declaration
                 and then Present (Corresponding_Body (Decl))
                 and then
                   Nkind (Get_Declaration_Node (Corresponding_Body (Decl)))
                   = N_Subprogram_Renaming_Declaration
               then
                  Build_Renamed_Body (Decl, Corresponding_Body (Decl), After);
               end if;
            end if;

         elsif Ekind (E) in Task_Kind
           and then
             (Nkind (Parent (E)) = N_Task_Type_Declaration
               or else
              Nkind (Parent (E)) = N_Single_Task_Declaration)
         then
            declare
               Ent : Entity_Id;

            begin
               Ent := First_Entity (E);

               while Present (Ent) loop

                  if Is_Entry (Ent)
                    and then not Default_Expressions_Processed (Ent)
                  then
                     Process_Default_Expressions (Ent, After);
                  end if;

                  Ent := Next_Entity (Ent);
               end loop;
            end;
         end if;

         E := Next_Entity (E);
      end loop;

   end Freeze_All;

   -----------------------
   -- Freeze_And_Append --
   -----------------------

   procedure Freeze_And_Append
     (Ent    : Entity_Id;
      Loc    : Source_Ptr;
      Result : in out List_Id)
   is
      L : constant List_Id := Freeze_Entity (Ent, Loc);

   begin
      if Is_Non_Empty_List (L) then
         if Result = No_List then
            Result := L;
         else
            Append_List (L, Result);
         end if;
      end if;
   end Freeze_And_Append;

   -------------------
   -- Freeze_Before --
   -------------------

   procedure Freeze_Before (N : Node_Id; T : Entity_Id) is
      Freeze_Nodes : constant List_Id := Freeze_Entity (T, Sloc (N));
      F            : Node_Id;

   begin
      if Is_Non_Empty_List (Freeze_Nodes) then
         F := First (Freeze_Nodes);

         if Present (F) then
            Insert_Actions (N, Freeze_Nodes);
         end if;
      end if;
   end Freeze_Before;

   -------------------
   -- Freeze_Entity --
   -------------------

   function Freeze_Entity (E : Entity_Id; Loc : Source_Ptr) return List_Id is
      Comp        : Entity_Id;
      F_Node      : Node_Id;
      Result      : List_Id;
      Indx        : Node_Id;
      Formal      : Entity_Id;
      Last_Formal : Entity_Id := Empty;

   begin
      --  Do not freeze if already frozen since we only need one freeze node.

      if Is_Frozen (E) then
         return No_List;

      --  It is improper to freeze an external entity within a generic
      --  because its freeze node will appear in a non-valid context.
      --  ??? We should probably freeze the entity at that point and insert
      --  the freeze node in a proper place but this proper place is not
      --  easy to find, and the proper scope is not easy to restore. For
      --  now, just wait to get out of the generic to freeze ???

      elsif Inside_A_Generic and then External_Ref_In_Generic (E) then
         return No_List;
      end if;

      --  Here to freeze the entity

      Result := No_List;
      Set_Is_Frozen (E);

      --  Case of entity being frozen is other than a type

      if not Is_Type (E) then

         --  If entity is exported or imported and does not have an external
         --  name, now is the time to provide the appropriate default name.

         if (Is_Imported (E) or else Is_Exported (E))
           and then No (Interface_Name (E))
         then
            Set_Interface_Name (E, Get_Default_External_Name (E));
         end if;

         --  For a subprogram, freeze all parameter types and also the return
         --  type (RM 13.14(13)). However skip this for internal subprograms.
         --  This is also the point where any extra formal parameters are
         --  created since we now know whether the subprogram will use
         --  a foreign convention.

         if Is_Subprogram (E) then
            if not Is_Internal (E) then
               Formal := First_Formal (E);
               while Present (Formal) loop
                  Freeze_And_Append (Etype (Formal), Loc, Result);
                  Last_Formal := Formal;
                  Formal := Next_Formal (Formal);
               end loop;

               Freeze_And_Append (Etype (E), Loc, Result);
            end if;

            --  Must freeze its parent first if it is a derived subprogram

            if Present (Alias (E)) then
               Freeze_And_Append (Alias (E), Loc, Result);
            end if;

            if not Is_Internal (E) then
               Freeze_Subprogram (E);
            end if;

         --  If entity has a type, freeze it first (RM 13.14(10))

         elsif Present (Etype (E)) then
            Freeze_And_Append (Etype (E), Loc, Result);
         end if;

      --  Case of a type or subtype being frozen

      else
         --  For a subtype, freeze the base type of the entity before
         --  freezing the entity itself, (RM 13.14(14)).

         if E /= Base_Type (E) then
            Freeze_And_Append (Base_Type (E), Loc, Result);

         --  For a derived type, freeze its parent type first (RM 13.14(14))

         elsif Is_Derived_Type (E) then
            Freeze_And_Append (Etype (E), Loc, Result);
         end if;

         --  For array type, freeze index types and component type first
         --  before freezing the array (RM 13.14(14)).

         if Is_Array_Type (E) then
            declare
               Ctyp  : constant Entity_Id := Component_Type (E);

               Non_Standard_Enum : Boolean := False;
               --  Set true if any of the index types is an enumeration
               --  type with a non-standard representation.

            begin
               Freeze_And_Append (Ctyp, Loc, Result);

               Indx := First_Index (E);
               while Present (Indx) loop
                  Freeze_And_Append (Etype (Indx), Loc, Result);

                  if Is_Enumeration_Type (Etype (Indx))
                    and then Has_Non_Standard_Rep (Etype (Indx))
                  then
                     Non_Standard_Enum := True;
                  end if;

                  Indx := Next_Index (Indx);
               end loop;

               --  For base type, propagate flags for component type

               if Ekind (E) = E_Array_Type then
                  if Is_Controlled (Component_Type (E))
                    or else Has_Controlled_Component (Ctyp)
                  then
                     Set_Has_Controlled_Component (E);
                  end if;

                  if Has_Unchecked_Union (Component_Type (E)) then
                     Set_Has_Unchecked_Union (E);
                  end if;
               end if;

               --  If the component size is not set, and the array is not
               --  packed, and we have the special case of a three byte
               --  record as the component type, then set the component size
               --  to 24. See routine in Sem_Util for details of this fix.

               if Component_Size (E) = 0
                  and then not Is_Packed (E)
                  and then Is_Three_Byte_Record (Component_Type (E))
               then
                  Set_Component_Size (E, Uint_24);
               end if;

               --  If packing was requested or if the component size was set
               --  explicitly, then see if bit packing is required. This
               --  processing is only done for base types, since all the
               --  representation aspects involved are type-related. This
               --  is not just an optimization, if we start processing the
               --  subtypes, they intefere with the settings on the base
               --  type (this is because Is_Packed has a slightly different
               --  meaning before and after freezing).

               if E = Base_Type (E) then
                  declare
                     Csiz : Uint;
                     Esiz : Uint;

                  begin
                     if Is_Packed (E)
                       or else Has_Pragma_Pack (E)
                     then
                        Csiz := Get_RM_Size (Ctyp);

                     elsif Component_Size (E) /= Uint_0 then
                        Csiz := Component_Size (E);

                     else
                        Esiz := Esize (Ctyp);

                        --  We can set the component size if it is less than
                        --  16, rounding it up to the next storage unit size.

                        if Esiz = 0 then
                           Csiz := Uint_0;
                        elsif Esiz <= 8 then
                           Csiz := Uint_8;
                        elsif Esiz <= 16 then
                           Csiz := Uint_16;
                        else
                           Csiz := Uint_0;
                        end if;
                     end if;

                     if 1 <= Csiz and then Csiz <= 32 then

                        --  We set the component size for all cases 1-32

                        Set_Component_Size (Base_Type (E), Csiz);

                        --  But actual packing is not needed for 8,16,32

                        if Csiz /= 8
                             and then
                           Csiz /= 16
                             and then
                           Csiz /= 32
                        then
                           Set_Has_Non_Standard_Rep (Base_Type (E));
                           Set_Is_Bit_Packed_Array  (Base_Type (E));
                           Set_Is_Packed            (Base_Type (E));

                        --  Here the array was requested to be packed, but
                        --  the packing request had no effect, so Is_Packed
                        --  is reset.

                        --  Note: semantically this means that we lose track
                        --  of the fact that a derived type inherited a pack
                        --  pragma that was non-effective, but that seems fine.
                        --  We regard a Pack pragma as a request to set a
                        --  representation characteristic, and this request
                        --  may be ignored.

                        else
                           Set_Is_Packed (Base_Type (E), False);
                        end if;

                     else
                        Set_Is_Packed (Base_Type (E), False);
                     end if;
                  end;
               end if;

               --  If any of the index types was an enumeration type with
               --  a non-standard rep clause, then we indicate that the
               --  array type is always packed (even if it is not bit packed).

               if Non_Standard_Enum then
                  Set_Has_Non_Standard_Rep (Base_Type (E));
                  Set_Is_Packed            (Base_Type (E));
               end if;
            end;

            Set_Component_Alignment_If_Not_Set (E);

            --  If the array is packed, we must create the packed array
            --  type to be used to actually implement the type. This is
            --  only needed if expansion is active, and is only needed for
            --  real array types (not for string literal types, since they
            --  are present only for the front end).

            if Expander_Active
              and then Is_Packed (E)
              and then Ekind (E) /= E_String_Literal_Subtype
            then
               Create_Packed_Array_Type (E);
            end if;

         --  For a class wide type, the corresponding specific type is
         --  frozen as well (RM 13.14(14))

         elsif Is_Class_Wide_Type (E) then
            Freeze_And_Append (Root_Type (E), Loc, Result);

         --  For record (sub)type, freeze the all component types (RM
         --  13.14(14). We test for E_Record_(sub)Type here, rather than
         --  using Is_Record_Type, because we don't want to attempt the
         --  freeze for the case of a private type with record extension
         --  (we will do that later when the full type is frozen).

         elsif Ekind (E) = E_Record_Type
           or else  Ekind (E) = E_Record_Subtype
         then
            --  Freeze components and embedded subtypes

            Comp := First_Entity (E);

            while Present (Comp) loop

               if not Is_Type (Comp) then
                  Freeze_And_Append (Etype (Comp), Loc, Result);
               end if;

               --  If the component is an access type with an allocator
               --  as default value, the designated type will be frozen
               --  by the corresponding expression in init_proc. In  order
               --  to place the freeze node for the designated type before
               --  that for the current record type, freeze it now.

               if Is_Access_Type (Etype (Comp))
                 and then Present (Parent (Comp))
                 and then Present (Expression (Parent (Comp)))
                 and then Nkind (Expression (Parent (Comp))) = N_Allocator
               then
                  Freeze_And_Append
                    (Designated_Type (Etype (Comp)), Loc, Result);
               end if;

               --  Deal with case of three byte component. See description
               --  of routine in Sem_Util for details of what is going on!

               if Esize (Comp) = 0
                 and then Is_Three_Byte_Record (Etype (Comp))
                 and then not Is_Packed (E)
               then
                  Set_Esize (Comp, Uint_24);
               end if;

               --  Check for error of component clause given for variable
               --  sized type. We have to delay this test till this point,
               --  since the component type has to be frozen for us to know
               --  if it is variable length.

               if Ekind (Comp) = E_Component
                 or else Ekind (Comp) = E_Discriminant
               then
                  declare
                     CC : constant Node_Id := Component_Clause (Comp);

                  begin
                     if Present (CC)
                       and then not Size_Known_At_Compile_Time
                                     (Underlying_Type (Etype (Comp)))
                     then
                        Error_Msg_N
                          ("component clause not allowed for variable " &
                           "length component", CC);
                     end if;
                  end;
               end if;

               Comp := Next_Entity (Comp);
            end loop;

            --  Check for controlled components and unchecked unions

            if  Ekind (E) = E_Record_Type then
               Comp := First_Component (E);

               while Present (Comp) loop
                  if Has_Controlled_Component (Etype (Comp))
                    or else (Chars (Comp) /= Name_uParent
                              and then Is_Controlled (Etype (Comp)))
                  then
                     Set_Has_Controlled_Component (E);
                     exit;
                  end if;

                  if Has_Unchecked_Union (Etype (Comp)) then
                     Set_Has_Unchecked_Union (E);
                  end if;

                  Comp := Next_Component (Comp);
               end loop;
            end if;

            Set_Component_Alignment_If_Not_Set (E);

         --  For a concurrent type, freeze corresponding record type. This
         --  does not correpond to any specific rule in the RM, but the
         --  record type is essentially part of the concurrent type.
         --  Freeze as well all local entities. This includes record types
         --  created for entry parameter blocks, and whatever local entities
         --  may appear in the private part.

         elsif Is_Concurrent_Type (E) then
            if Present (Corresponding_Record_Type (E)) then
               Freeze_And_Append
                 (Corresponding_Record_Type (E), Loc, Result);
            end if;

            Comp := First_Entity (E);

            while Present (Comp) loop
               if Is_Type (Comp) then
                  Freeze_And_Append (Comp, Loc, Result);
               else
                  Freeze_And_Append (Etype (Comp), Loc, Result);
               end if;
               Comp := Next_Entity (Comp);
            end loop;

         --  For enumeration type, freeze type of literal table and table
         --  itself before we freeze the enumeration type if one exists.
         --  Again, this does not correspond to any specific rule in the RM,
         --  but the table is an essentially part of the enumeration type.

         elsif Is_Enumeration_Type (E) then
            if Present (Lit_Name_Table (E)) then
               Freeze_And_Append (Lit_Name_Table (E), Loc, Result);
            end if;

         --  Private types are required to point to the same freeze node
         --  as their corresponding full views. The freeze node itself
         --  has to point to the partial view of the entity (because
         --  from the partial view, we can retrieve the full view, but
         --  not the reverse). However, in order to freeze correctly,
         --  we need to freeze the full view. If we are freezing at the
         --  end of a scope (or within the scope of the private type),
         --  the partial and full views will have been swapped, the
         --  full view appears first in the entity chain and the swapping
         --  mechanism enusres that the pointers are properly set (on
         --  scope exit).

         --  If we encounter the partial view before the full view
         --  (e.g. when freezing from another scope), we freeze the
         --  full view, and then set the pointers appropriately since
         --  we cannot rely on swapping to fix things up (subtypes in an
         --  outer scope might not get swapped).

         elsif Is_Incomplete_Or_Private_Type (E)
           and then not Is_Generic_Type (E)
         then
            --  Case of full view present

            if Present (Full_View (E)) then

               --  If full view has already been frozen, then no
               --  further processing is required

               if Is_Frozen (Full_View (E)) then

                  Set_Has_Delayed_Freeze (E, False);
                  Set_Freeze_Node (E, Empty);

                  return Result;

               --  Otherwise freeze full view and patch the pointers

               else
                  Freeze_And_Append (Full_View (E), Loc, Result);

                  if Has_Delayed_Freeze (E) then
                     F_Node := Freeze_Node (Full_View (E));

                     if Present (F_Node) then
                        Set_Freeze_Node (E, F_Node);
                        Set_Entity (F_Node, E);
                     else
                        --  {Incomplete,Private}_Subtypes
                        --  with Full_Views constrained by discriminants
                        Set_Has_Delayed_Freeze (E, False);
                        Set_Freeze_Node (E, Empty);
                     end if;
                  end if;

                  return Result;
               end if;

            --  Case of no full view present, freeze the partial view!

            else
               null;
            end if;

         --  For a subprogram, freeze types of all formals, the return
         --  type was already frozen, since it is the Etype of the function.

         elsif Ekind (E) = E_Subprogram_Type then
            Formal := First_Formal (E);
            while Present (Formal) loop
               Freeze_And_Append (Etype (Formal), Loc, Result);
               Formal := Next_Formal (Formal);
            end loop;

            Freeze_Subprogram (E);

         --  For access to a protected subprogram, freeze the equivalent
         --  type (however this is not set if we are not generating code)
         --  or if this is an anonymous type used just for resolution).

         elsif Ekind (E) = E_Access_Protected_Subprogram_Type
           and then Operating_Mode = Generate_Code
           and then Present (Equivalent_Type (E))
         then
            Freeze_And_Append (Equivalent_Type (E), Loc, Result);
         end if;

         --  Generic types are never seen by the back-end, and are also not
         --  processed by the expander (since the expander is turned off for
         --  generic processing), so we never need freeze nodes for them.

         if Is_Generic_Type (E) then
            return Result;
         end if;

         --  Some special processing for non-generic types to complete
         --  representation details not known till the freeze point.

         if Is_Fixed_Point_Type (E) then
            Freeze_Fixed_Point_Type (E);
         elsif Is_Enumeration_Type (E) then
            Freeze_Enumeration_Type (E);
         end if;

         --  If the current entity is an array or record subtype and has
         --  discriminants used to constrain it, it must not freeze, because
         --  Freeze_Entity nodes force Gigi to process the frozen type.

         if Is_Composite_Type (E) then

            if Is_Array_Type (E) then

               declare
                  Index : Node_Id := First_Index (E);
                  Expr1 : Node_Id;
                  Expr2 : Node_Id;

               begin
                  while Present (Index) loop
                     if Etype (Index) /= Any_Type then
                        Get_Index_Bounds (Index, Expr1, Expr2);

                        for J in 1 .. 2 loop
                           if Nkind (Expr1) = N_Identifier
                             and then Ekind (Entity (Expr1)) = E_Discriminant
                           then
                              Set_Has_Delayed_Freeze (E, False);
                              Set_Freeze_Node (E, Empty);
                              return Result;
                           end if;

                           Expr1 := Expr2;
                        end loop;
                     end if;

                     Index := Next_Index (Index);
                  end loop;
               end;

            elsif Has_Discriminants (E)
              and Is_Constrained (E)
            then

               declare
                  Constraint : Elmt_Id;
                  Expr       : Node_Id;
               begin
                  Constraint := First_Elmt (Discriminant_Constraint (E));

                  while Present (Constraint) loop

                     Expr := Node (Constraint);
                     if Nkind (Expr) = N_Identifier
                       and then Ekind (Entity (Expr)) = E_Discriminant
                     then
                        Set_Has_Delayed_Freeze (E, False);
                        Set_Freeze_Node (E, Empty);
                        return Result;
                     end if;

                     Constraint := Next_Elmt (Constraint);
                  end loop;
               end;

            end if;
         end if;

         --  Now that all types from which E may depend are frozen, see
         --  if the size is known at compile time, if it must be unsigned,
         --  or if strict alignent is required

         Check_Compile_Time_Size (E);
         Check_Unsigned_Type (E);

         if Base_Type (E) = E then
            Check_Strict_Alignment (E);
         end if;

         --  Do not allow a size clause for a type which does not have a size
         --  that is known at compile time

         if Has_Size_Clause (E)
           and then not Size_Known_At_Compile_Time (E)
         then
            Error_Msg_N
              ("size clause not allowed for variable length type",
               Size_Clause (E));
         end if;

         --  For access types, set the size.  This is normally system address
         --  size, except for fat pointers (unconstrained array access types),
         --  where the size is two times the address size, to accomodate the
         --  two pointers that are required for a fat pointer (data and
         --  template). Note that E_Access_Protected_Subprogram_Type is not
         --  an access type for this purpose since it is not a pointer but is
         --  equivalent to a record. For access subtypes, copy the size from
         --  the base type since Gigi represents them the same way.

         if Esize (E) /= 0 then
            null;

         elsif Ekind (E) = E_Access_Protected_Subprogram_Type then
            null;

         elsif Ekind (E) = E_Access_Subtype then
            Set_Size_Info (E, Base_Type (E));

         elsif Is_Access_Type (E) then
            if Is_Array_Type (Designated_Type (E))
              and then not Is_Constrained (Designated_Type (E))
              and then not Has_Completion_In_Body
                                (Directly_Designated_Type (E))
              and then not Debug_Flag_6
            then
               Set_Esize (E, 2 * System_Address_Size);
            else
               Set_Esize (E, System_Address_Size);
            end if;
         end if;

         --  For record first subtype, check if there are any fixed-point
         --  fields with component clauses, where we must check the size.
         --  This is not done till the freeze point, since for fixed-point
         --  types, we do not know the size until the type is frozen

         if Is_Record_Type (E) and then Is_First_Subtype (E) then
            declare
               Comp : Node_Id;
               Junk : Boolean;

            begin
               Comp := First_Component (E);
               while Present (Comp) loop
                  if Present (Component_Clause (Comp))
                    and then Is_Fixed_Point_Type (Etype (Comp))
                  then
                     Check_Size
                       (Component_Clause (Comp),
                        Etype (Comp),
                        Esize (Comp),
                        Junk);
                  end if;

                  Comp := Next_Component (Comp);
               end loop;
            end;
         end if;
      end if;

      --  Here is where we logically freeze the current entity. If it has a
      --  freeze node, then this is the point at which the freeze node is
      --  linked into the result list.

      if Has_Delayed_Freeze (E) then

         --  If a freeze node is already allocated, use it, otherwise allocate
         --  a new one. The preallocation happens in the case of anonymous base
         --  types, where we preallocate so that we can set First_Subtype_Link.
         --  Note that we reset the Sloc to the current freeze location.

         if Present (Freeze_Node (E)) then
            F_Node := Freeze_Node (E);
            Set_Sloc (F_Node, Loc);

         else
            F_Node := New_Node (N_Freeze_Entity, Loc);
            Set_Freeze_Node (E, F_Node);
            Set_TSS_Elist (F_Node, No_Elist);
            Set_Actions (F_Node, No_List);
         end if;

         Set_Entity (F_Node, E);

         if Result = No_List then
            Result := New_List (F_Node);
         else
            Append (F_Node, Result);
         end if;

      end if;

      --  When a type is frozen, the first subtype of the type is frozen as
      --  well (RM 13.14(15)). This has to be done after freezing the type,
      --  since obviously the first subtype depends on its own base type.

      if Is_Type (E) then
         Freeze_And_Append (First_Subtype (E), Loc, Result);

         --  If we just froze a tagged non-class wide record, then freeze the
         --  corresponding class-wide type. This must be done after the tagged
         --  type itself is frozen, because the class-wide type refers to the
         --  tagged type which generates the class.

         if Is_Tagged_Type (E)
           and then not Is_Class_Wide_Type (E)
           and then Present (Class_Wide_Type (E))
         then
            Freeze_And_Append (Class_Wide_Type (E), Loc, Result);
         end if;
      end if;

      return Result;

   end Freeze_Entity;

   -----------------------
   -- Freeze_Expression --
   -----------------------

   procedure Freeze_Expression (N : Node_Id) is
      In_Def_Exp : constant Boolean := In_Default_Expression;
      Typ        : Entity_Id;
      Nam        : Entity_Id;
      Desig_Typ  : Entity_Id;
      P          : Node_Id;
      Parent_P   : Node_Id;

      Freeze_Outside : Boolean := False;
      --  This flag is set true if the entity must be frozen outside the
      --  current subprogram. This happens in the case of expander generated
      --  subprograms (_Init_Proc, _Input, _Output, _Read, _Write) which do
      --  not freeze all entities like other bodies, but which nevertheless
      --  may reference entities that have to be frozen before the body and
      --  obviously cannot be frozen inside the body.

      function In_Exp_Body (N : Node_Id) return Boolean;
      --  Given an N_Handled_Sequence_Of_Statements node N, determines whether
      --  it is the handled statement sequence of an expander generated
      --  subprogram (init proc, or stream subprogram). If so, it returns
      --  True, otherwise False.

      function In_Exp_Body (N : Node_Id) return Boolean is
         P : Node_Id;

      begin
         if Nkind (N) = N_Subprogram_Body then
            P := N;
         else
            P := Parent (N);
         end if;

         if Nkind (P) /= N_Subprogram_Body then
            return False;

         else
            P := Defining_Unit_Name (Specification (P));

            if Nkind (P) = N_Defining_Identifier
              and then (Chars (P) = Name_uInit_Proc or else
                        Chars (P) = Name_uInput     or else
                        Chars (P) = Name_uOutput    or else
                        Chars (P) = Name_uRead      or else
                        Chars (P) = Name_uWrite)
            then
               return True;
            else
               return False;
            end if;
         end if;

      end In_Exp_Body;

   --  Start of processing for Freeze_Expression

   begin
      --  Immediate return if freezing is inhibited. This flag is set by
      --  the analyzer to stop freezing on generated expressions that would
      --  cause freezing if they were in the source program, but which are
      --  not supposed to freeze, since they are created.

      if Must_Not_Freeze (N) then
         return;
      end if;

      --  If expression is non-static, then it does not freeze in a default
      --  expression, see section "Handling of Default Expressions" in the
      --  spec of package Sem for further details. Note that we have to
      --  make sure that we actually have a real expression (if we have
      --  a subtype indication, we can't test Is_Static_Expression!)

      if In_Def_Exp
        and then Nkind (N) in N_Subexpr
        and then not Is_Static_Expression (N)
      then
         return;
      end if;

      --  Freeze type of expression if not frozen already

      if Nkind (N) in N_Has_Etype
        and then not Is_Frozen (Etype (N))
      then
         Typ := Etype (N);
      else
         Typ := Empty;
      end if;

      --  For entity name, freeze entity if not frozen already. A special
      --  exception occurs for an identifier that did not come from source.
      --  We don't let such identifiers freeze a non-internal entity, i.e.
      --  an entity that did come from source, since such an identifier was
      --  generated by the expander, and cannot have any semantic effect on
      --  the freezing semantics. For example, this stops the parameter of
      --  an initialization procedure from freezing the variable.

      if Is_Entity_Name (N)
        and then not Is_Frozen (Entity (N))
        and then (Nkind (N) /= N_Identifier
                   or else Comes_From_Source (N)
                   or else not Comes_From_Source (Entity (N)))
      then
         Nam := Entity (N);

      else
         Nam := Empty;
      end if;

      --  For an allocator freeze designated type if not frozen already

      Desig_Typ := Empty;
      case Nkind (N) is

         when N_Allocator =>
            Desig_Typ := Designated_Type (Etype (N));

         when N_Selected_Component |
            N_Indexed_Component    |
            N_Slice                =>

            if Is_Access_Type (Etype (Prefix (N))) then
               Desig_Typ := Designated_Type (Etype (Prefix (N)));
            end if;

         when others =>
            null;

      end case;

      if Desig_Typ /= Empty
        and then Is_Frozen (Desig_Typ)
      then
         Desig_Typ := Empty;
      end if;

      --  All done if nothing needs freezing

      if No (Typ)
        and then No (Nam)
        and then No (Desig_Typ)
      then
         return;
      end if;

      --  Loop for looking at the right place to insert the freeze nodes
      --  exiting from the loop when it is appropriate to insert the freeze
      --  node before the current node P.

      --  Also checks some special exceptions to the freezing rules. These
      --  cases result in a direct return, bypassing the freeze action.

      P := N;
      loop
         Parent_P := Parent (P);

         --  If we don't have a parent, then we are not in a well-formed
         --  tree. This is an unusual case, but there are some legitimate
         --  situations in which this occurs, notably when the expressions
         --  in the range of a type declaration are resolved. We simply
         --  ignore the freeze request in this case. Is this right ???

         if No (Parent_P) then
            return;
         end if;

         --  See if we have got to an appropriate point in the tree

         case Nkind (Parent_P) is

            --  A special test for the exception of (RM 13.14(8)) for the
            --  case of per-object expressions (RM 3.8(18)) occurring in a
            --  component definition or a discrete subtype definition. Note
            --  that we test for a component declaration which includes both
            --  cases we are interested in, and furthermore the tree does not
            --  have explicit nodes for either of these two constructs.

            when N_Component_Declaration =>

               --  The case we want to test for here is an identifier that is
               --  a per-object expression, this is either a discriminant that
               --  appears in a context other than the component declaration
               --  or it is a reference to the type of the enclosing construct.

               --  For either of these cases, we skip the freezing

               if not In_Default_Expression
                 and then Nkind (N) = N_Identifier
                 and then (Present (Entity (N)))
               then
                  --  We recognize the discriminant case by just looking for
                  --  a reference to a discriminant. It can only be one for
                  --  the enclosing construct. Skip freezing in this case.

                  if Ekind (Entity (N)) = E_Discriminant then
                     return;

                  --  For the case of a reference to the enclosing record,
                  --  (or task or protected type), we look for a type that
                  --  matches the current scope.

                  elsif Entity (N) = Current_Scope then
                     return;
                  end if;
               end if;

            --  If we have an enumeration literal that appears as the
            --  choice in the aggregate of an enumeration representation
            --  clause, then freezing does not occur (RM 13.14(9)).

            when N_Enumeration_Representation_Clause =>

               --  The case we are looking for is an enumeration literal

               if (Nkind (N) = N_Identifier or Nkind (N) = N_Character_Literal)
                 and then Is_Enumeration_Type (Etype (N))
               then
                  --  If enumeration literal appears directly as the choice,
                  --  do not freeze (this is the normal non-overloade case)

                  if Nkind (Parent (N)) = N_Component_Association
                    and then First (Choices (Parent (N))) = N
                  then
                     return;

                  --  If enumeration literal appears as the name of a
                  --  function which is the choice, then also do not freeze.
                  --  This happens in the overloaded literal case, where the
                  --  enumeration literal is temporarily changed to a function
                  --  call for overloading analysis purposes.

                  elsif Nkind (Parent (N)) = N_Function_Call
                     and then
                       Nkind (Parent (Parent (N))) = N_Component_Association
                     and then
                       First (Choices (Parent (Parent (N)))) = Parent (N)
                  then
                     return;
                  end if;
               end if;

            --  Normally if the parent is a handled sequence of statements,
            --  then the current node must be a statement, and that is an
            --  appropriate place to insert a freeze node.

            when N_Handled_Sequence_Of_Statements =>

               --  The exception occurs when the sequence of statements is
               --  for an initialization procedure, in this case we want to
               --  freeze outside this body, not inside it, and we skip
               --  past the subprogram body that we are inside.

               if In_Exp_Body (Parent_P) then
                  Parent_P := Parent (Parent_P);
                  Freeze_Outside := True;

               --  If not that special case, we have found the insertion point

               else
                  exit;
               end if;

            --  If parent is a body or a spec or a block, the the current
            --  node is a statement or declaration and we can insert the
            --  freeze node before it.

            when N_Package_Specification |
                 N_Package_Body          |
                 N_Subprogram_Body       |
                 N_Task_Body             |
                 N_Protected_Body        |
                 N_Entry_Body            |
                 N_Block_Statement       => exit;

            --  The expander is allowed to define types in any statements list,
            --  so any of the following parent nodes also mark a freezing point
            --  if the actual node is in a list of statements or declarations.

            when N_Exception_Handler          |
                 N_If_Statement               |
                 N_Elsif_Part                 |
                 N_Case_Statement_Alternative |
                 N_Selective_Accept           |
                 N_Accept_Alternative         |
                 N_Delay_Alternative          |
                 N_Conditional_Entry_Call     |
                 N_Entry_Call_Alternative     |
                 N_Triggering_Alternative     |
                 N_Abortable_Part             |
                 N_Freeze_Entity              =>

               exit when Is_List_Member (P);


            --  Note: The N_Loop_Statement is a special case. A type that
            --  appears in the source can never be frozen in a loop (this
            --  occurs only because of a loop expanded by the expander),
            --  so we keep on going. Otherwise we terminate the search.

            when N_Loop_Statement =>
               exit when not Comes_From_Source (Etype (N));

            --  For all other cases, keep looking at parents

            when others =>
               null;
         end case;

         --  We fall through the case if we did not yet find the proper
         --  place in the free for inserting the freeze node, so climb!

         P := Parent_P;
      end loop;

      --  If the expression appears in a record or an initialization
      --  procedure, the freeze nodes are collected and attached to
      --  the current scope, to be inserted an analyzed on exit from
      --  the scope, to insure that generated entities appear in the
      --  correct scope. If the expression is a default for a discriminant
      --  specification, the scope is still void. The expression can also
      --  appear in the discriminant part of a private or concurrent type.

      --  The other case requiring this special handling is if we are in
      --  a default expression, since in that case we are about to freeze
      --  a static type, and the freeze scope needs to be the outer scope,
      --  not the scope of the subprogram with the default parameter.

      if In_Def_Exp
        or else Freeze_Outside
        or else (Is_Type (Current_Scope)
                  and then (not Is_Concurrent_Type (Current_Scope)
                             or else not Has_Completion (Current_Scope)))
        or else Ekind (Current_Scope) = E_Void
      then
         declare
            Loc          : constant Source_Ptr := Sloc (Current_Scope);
            Freeze_Nodes : List_Id := No_List;

         begin
            if Present (Desig_Typ) then
               Freeze_And_Append (Desig_Typ, Loc, Freeze_Nodes);
            end if;

            if Present (Typ) then
               Freeze_And_Append (Typ, Loc, Freeze_Nodes);
            end if;

            if Present (Nam) then
               Freeze_And_Append (Nam, Loc, Freeze_Nodes);
            end if;

            if Is_Non_Empty_List (Freeze_Nodes) then

               if No (Scope_Stack.Table
                 (Scope_Stack.Last).Pending_Freeze_Nodes)
               then
                  Scope_Stack.Table (Scope_Stack.Last).Pending_Freeze_Nodes :=
                     Freeze_Nodes;
               else
                  Append_List (Freeze_Nodes, Scope_Stack.Table
                                   (Scope_Stack.Last).Pending_Freeze_Nodes);
               end if;
            end if;
         end;

         return;
      end if;

      --  Now we have the right place to do the freezing. First, a special
      --  adjustment, if we are in default expression analysis mode, these
      --  freeze actions must not be thrown away (normally all inserted
      --  actions are thrown away in this mode. However, the freeze actions
      --  are from static expressions and one of the important reasons we
      --  are doing this special analysis is to get these freeze actions.
      --  Therefore we turn off the In_Default_Expression mode to propagate
      --  these freeze actions. This also means they get properly analyzed
      --  and expanded.

      In_Default_Expression := False;

      --  Freeze the designated type of an allocator (RM 13.14(12))

      if Present (Desig_Typ) then
         Freeze_Before (P, Desig_Typ);
      end if;

      --  Freeze type of expression (RM 13.14(9)). Note that we took care of
      --  the enumeration representation clause exception in the loop above.

      if Present (Typ) then
         Freeze_Before (P, Typ);
      end if;

      --  Freeze name if one is present (RM 13.14(10))

      if Present (Nam) then
         Freeze_Before (P, Nam);
      end if;

      In_Default_Expression := In_Def_Exp;
   end Freeze_Expression;

   -----------------------------
   -- Freeze_Enumeration_Type --
   -----------------------------

   procedure Freeze_Enumeration_Type (Typ : Entity_Id) is
   begin
      if Has_Foreign_Convention (Typ)
        and then not Has_Size_Clause (Typ)
        and then Esize (Typ) < Standard_Integer_Size
      then
         Set_Esize (Typ, UI_From_Int (Standard_Integer_Size));
      end if;
   end Freeze_Enumeration_Type;

   -----------------------------
   -- Freeze_Fixed_Point_Type --
   -----------------------------

   --  Certain fixed-point types and subtypes, including implicit base
   --  types and declared first subtypes, have not yet set up a range.
   --  This is because the range cannot be set until the Small and Size
   --  values are known, and these are not known till the type is frozen.

   --  To signal this case, Scalar_Range contains an unanalyzed syntactic
   --  range whose bounds are unanalyzed real literals. This routine will
   --  recognize this case, and transform this range node into a properly
   --  typed range with properly analyzed and resolved values.

   procedure Freeze_Fixed_Point_Type (Typ : Entity_Id) is
      Rng   : constant Node_Id    := Scalar_Range (Typ);
      Lo    : constant Node_Id    := Low_Bound (Rng);
      Hi    : constant Node_Id    := High_Bound (Rng);
      Btyp  : constant Entity_Id  := Base_Type (Typ);
      Brng  : constant Node_Id    := Scalar_Range (Btyp);
      BLo   : constant Node_Id    := Low_Bound (Brng);
      BHi   : constant Node_Id    := High_Bound (Brng);
      Small : constant Ureal      := Small_Value (Typ);
      Loval : Ureal;
      Hival : Ureal;

      Actual_Size : Nat;

      function Fsize (Lov, Hiv : Ureal) return Nat;
      --  Returns size of type with given bounds. Also leaves these
      --  bounds set as the current bounds of the Typ.

      function Fsize (Lov, Hiv : Ureal) return Nat is
      begin
         Set_Realval (Lo, Lov);
         Set_Realval (Hi, Hiv);
         return Minimum_Size (Typ);
      end Fsize;

   --  Start of processing for Freeze_Fixed_Point_Type;

   begin
      --  Immediate return if the range is already analyzed. This means
      --  that the range is already set, and does not need to be computed
      --  by this routine.

      if Analyzed (Rng) then
         return;
      end if;

      --  Immediate return if either of the bounds raises Constraint_Error

      if Raises_Constraint_Error (Lo)
        or else Raises_Constraint_Error (Hi)
      then
         return;
      end if;

      Loval := Realval (Lo);
      Hival := Realval (Hi);

      --  Ordinary fixed-point case

      if Is_Ordinary_Fixed_Point_Type (Typ) then

         --  For the ordinary fixed-point case, we are allowed to fudge the
         --  end-points up or down by small. Generally we prefer to fudge
         --  up, i.e. widen the bounds for non-model numbers so that the
         --  end points are included. However there are cases in which this
         --  cannot be done, and indeed cases in which we may need to narrow
         --  the bounds. The following circuit makes the decision.

         --  Note: our terminology here is that Incl_EP means that the
         --  bounds are widened by Small if necessary to include the end
         --  points, and Excl_EP means that the bounds are narrowed by
         --  Small to exclude the end-points if this reduces the size.

         --  Note that in the Incl case, all we care about is including the
         --  end-points. In the Excl case, we want to narrow the bounds as
         --  much as permitted by the RM, to give the smallest possible size.

         Fudge : declare
            Loval_Incl_EP : Ureal;
            Hival_Incl_EP : Ureal;

            Loval_Excl_EP : Ureal;
            Hival_Excl_EP : Ureal;

            Size_Incl_EP  : Nat;
            Size_Excl_EP  : Nat;

            Model_Num     : Ureal;
            First_Subt    : Entity_Id;
            Actual_Lo     : Ureal;
            Actual_Hi     : Ureal;

         begin
            --  First step. Base types are required to be symmetrical. Right
            --  now, the base type range is a copy of the first subtype range.
            --  This will be corrected before we are done, but right away we
            --  need to deal with the case where both bounds are non-negative.
            --  In this case, we set the low bound to the negative of the high
            --  bound, to make sure that the size is computed to include the
            --  required sign. Note that we do not need to worry about the
            --  case of both bounds negative, because the sign will be dealt
            --  with anyway. Furthermore we can't just go making such a bound
            --  symmetrical, since in a twos-complement system, there is an
            --  extra negative value which could not be accomodated on the
            --  positive side.

            if Typ = Btyp
              and then not UR_Is_Negative (Loval)
              and then Hival > Loval
            then
               Loval := -Hival;
               Set_Realval (Lo, Loval);
            end if;

            --  Compute the fudged bounds. If the number is a model number,
            --  then we do nothing to include it, but we are allowed to
            --  backoff to the next adjacent model number when we exclude
            --  it. If it is not a model number then we straddle the two
            --  values with the model numbers on either side.

            Model_Num := UR_Trunc (Loval / Small) * Small;

            if Loval = Model_Num then
               Loval_Incl_EP := Model_Num;
               Loval_Excl_EP := Model_Num + Small;

            else
               Loval_Excl_EP := Model_Num;
               Loval_Incl_EP := Model_Num - Small;
            end if;

            Model_Num := UR_Trunc (Hival / Small) * Small;

            if Hival = Model_Num then
               Hival_Incl_EP := Model_Num;
               Hival_Excl_EP := Model_Num - Small;

            else
               Hival_Excl_EP := Model_Num;
               Hival_Incl_EP := Model_Num + Small;
            end if;

            --  One further adjustment is needed. In the case of subtypes,
            --  we cannot go outside the range of the base type, or we get
            --  peculiarities, and the base type range is already set. This
            --  only applies to the Incl values, since clearly the Excl
            --  values are already as restricted as they are allowed to be.

            if Typ /= Btyp then
               Loval_Incl_EP := UR_Max (Loval_Incl_EP, Realval (BLo));
               Hival_Incl_EP := UR_Min (Hival_Incl_EP, Realval (BHi));
            end if;

            Size_Incl_EP := Fsize (Loval_Incl_EP, Hival_Incl_EP);

            --  Similarly, get the size with the bounds fudged to exclude the
            --  end-points (i.e. narrowed by Small). We do not need to worry
            --  about the base type range in this case.

            Loval_Excl_EP := Loval + Small;
            Hival_Excl_EP := Hival - Small;
            Size_Excl_EP  := Fsize (Loval_Excl_EP, Hival_Excl_EP);

            --  No need to exclude end-points if it does not reduce size

            if Fsize (Loval_Incl_EP, Hival_Excl_EP) = Size_Excl_EP then
               Loval_Excl_EP := Loval_Incl_EP;
            end if;

            if Fsize (Loval_Excl_EP, Hival_Incl_EP) = Size_Excl_EP then
               Hival_Excl_EP := Hival_Incl_EP;
            end if;

            --  Now we set the actual size to be used. We want to use the
            --  bounds fudged up to include the end-points but only if this
            --  can be done without violating a specifically given size
            --  size clause or causing an unacceptable increase in size.

            --  Case of size clause given

            if Has_Size_Clause (Typ) then

               --  Use the inclusive size only if it is consistent with
               --  the explicitly specified size.

               if Size_Incl_EP <= Esize (Typ) then
                  Actual_Lo   := Loval_Incl_EP;
                  Actual_Hi   := Hival_Incl_EP;
                  Actual_Size := Size_Incl_EP;

               --  If the inclusive size is too large, we try excluding
               --  the end-points (will be caught later if does not work).

               else
                  Actual_Lo   := Loval_Excl_EP;
                  Actual_Hi   := Hival_Excl_EP;
                  Actual_Size := Size_Excl_EP;
               end if;

            --  Case of size clause not given

            else
               --  If we have a base type whose corresponding first subtype
               --  has an explicit size that is large enough to include our
               --  end-points, then do so. There is no point in working hard
               --  to get a base type whose size is smaller than the specified
               --  size of the first subtype.

               First_Subt := First_Subtype (Typ);

               if Has_Size_Clause (First_Subt)
                 and then Size_Incl_EP <= Esize (First_Subt)
               then
                  Actual_Size := Size_Incl_EP;
                  Actual_Lo   := Loval_Incl_EP;
                  Actual_Hi   := Hival_Incl_EP;

               --  If excluding the end-points makes the size smaller and
               --  results in a size of 8,16,32,64, then we take the smaller
               --  size. For the 64 case, this is compulsory. For the other
               --  cases, it seems reasonable. We like to include end points
               --  if we can, but not at the expense of moving to the next
               --  natural boundary of size.

               elsif Size_Incl_EP /= Size_Excl_EP
                 and then
                    (Size_Excl_EP = 8  or else
                     Size_Excl_EP = 16 or else
                     Size_Excl_EP = 32 or else
                     Size_Excl_EP = 64)
               then
                  Actual_Size := Size_Excl_EP;
                  Actual_Lo   := Loval_Excl_EP;
                  Actual_Hi   := Hival_Excl_EP;

               --  Otherwise we can definitely include the end points

               else
                  Actual_Size := Size_Incl_EP;
                  Actual_Lo   := Loval_Incl_EP;
                  Actual_Hi   := Hival_Incl_EP;
               end if;
            end if;

            Set_Realval (Lo, Actual_Lo);
            Set_Realval (Hi, Actual_Hi);
         end Fudge;

      --  For the decimal case, none of this fudging is required, since there
      --  are no end-point problems in the decimal case (the end-points are
      --  always included).

      else
         Actual_Size := Fsize (Loval, Hival);
      end if;

      --  At this stage, the actual size has been calculated and the proper
      --  required bounds are stored in the low and high bounds.

      if Actual_Size > 64 then
         Error_Msg_Uint_1 := UI_From_Int (Actual_Size);
         Error_Msg_N
           ("size required (^) for type& too large, maximum is 64", Typ);
         Actual_Size := 64;
      end if;

      --  Check size against explicit given size

      if Has_Size_Clause (Typ) then
         if Actual_Size > Esize (Typ) then
            Error_Msg_Uint_1 := Esize (Typ);
            Error_Msg_Uint_2 := UI_From_Int (Actual_Size);
            Error_Msg_NE
              ("size given (^) for type& too small, minimum is ^",
               Size_Clause (Typ), Typ);

         else
            Actual_Size := UI_To_Int (Esize (Typ));
         end if;

      --  Increase size to next natural boundary if no size clause given

      else
         if Actual_Size <= 8 then
            Actual_Size := 8;
         elsif Actual_Size <= 16 then
            Actual_Size := 16;
         elsif Actual_Size <= 32 then
            Actual_Size := 32;
         else
            Actual_Size := 64;
         end if;

         Set_Esize (Typ, Actual_Size);
      end if;

      --  If we have a base type, then expand the bounds so that they
      --  extend to the full width of the allocated size in bits, to
      --  avoid junk range checks on intermediate computations. Don't
      --  however do this for decimal types, since here we want the
      --  range to exactly reflect the digits value.

      if Is_Ordinary_Fixed_Point_Type (Typ)
        and then Base_Type (Typ) = Typ
      then
         Set_Realval (Lo, -(Small * (Uint_2 ** (Actual_Size - 1))));
         Set_Realval (Hi,  (Small * (Uint_2 ** (Actual_Size - 1) - 1)));
      end if;

      --  Final step is to reanalyze the bounds using the proper type
      --  and set the Corresponding_Integer_Value fields of the literals.

      Set_Etype (Lo, Empty);
      Set_Analyzed (Lo, False);
      Analyze (Lo);

      --  Resolve with universal fixed if the base type, and the base
      --  type if it is a subtype. Note we can't resolve the base type
      --  with itself, that would be a reference before definition.

      if Typ = Btyp then
         Resolve (Lo, Universal_Fixed);
      else
         Resolve (Lo, Btyp);
      end if;

      --  Set corresponding integer value for bound

      Set_Corresponding_Integer_Value
        (Lo, UR_To_Uint (Realval (Lo) / Small));

      --  Similar processing for high bound

      Set_Etype (Hi, Empty);
      Set_Analyzed (Hi, False);
      Analyze (Hi);

      if Typ = Btyp then
         Resolve (Hi, Universal_Fixed);
      else
         Resolve (Hi, Btyp);
      end if;

      Set_Corresponding_Integer_Value
        (Hi, UR_To_Uint (Realval (Hi) / Small));

      --  Set type of range to correspond to bounds

      Set_Etype (Rng, Etype (Lo));

      --  Set Esize to calculated size and also set RM_Size

      Set_Esize (Typ, Actual_Size);

      --  Set RM_Size if not already set. If already set, check value

      declare
         Minsiz : constant Uint := UI_From_Int (Minimum_Size (Typ));

      begin
         if RM_Size (Typ) /= Uint_0 then
            if RM_Size (Typ) < Minsiz then
               Error_Msg_Uint_1 := RM_Size (Typ);
               Error_Msg_Uint_2 := Minsiz;
               Error_Msg_NE
                 ("size given (^) for type& too small, minimum is ^",
                  Size_Clause (Typ), Typ);
            end if;

         else
            Set_RM_Size (Typ, Minsiz);
         end if;
      end;

   end Freeze_Fixed_Point_Type;

   ------------------
   -- Freeze_Itype --
   ------------------

   procedure Freeze_Itype (T : Entity_Id; N : Node_Id) is
      L : List_Id;

   begin
      Set_Has_Delayed_Freeze (T);
      L := Freeze_Entity (T, Sloc (N));

      if Is_Non_Empty_List (L) then
         Insert_Actions (N, L);
      end if;
   end Freeze_Itype;

   -----------------------
   -- Freeze_Subprogram --
   -----------------------

   procedure Freeze_Subprogram (E : Entity_Id) is
      Retype : Entity_Id;

   begin
      --  For non-foreign convention subprograms, this is where we create
      --  the extra formals (for accessibility level and constrained bit
      --  information). We delay this till the freeze point precisely so
      --  that we know the convention!

      if not Has_Foreign_Convention (E) then
         Create_Extra_Formals (E);
         Set_Mechanisms (E);


      --  For foreign conventions, do not permit return of an unconstrained
      --  array. There are probably more error checks that should be made!

      --  Note: we *do* allow a return by descriptor, though here again
      --  there is probably more to be done ???

      else
         Set_Mechanisms (E);

         if Ekind (E) = E_Function then
            Retype := Implementation_Type (Etype (E));

            if Is_Array_Type (Retype)
              and then not Is_Constrained (Retype)
              and then Mechanism (E) not in Descriptor_Codes
            then
               Error_Msg_NE
                ("convention for& does not permit returning " &
                  "unconstrained array type", E, E);
               return;
            end if;
         end if;
      end if;

   end Freeze_Subprogram;

   ---------------------------------
   -- Process_Default_Expressions --
   ---------------------------------

   procedure Process_Default_Expressions
     (E     : Entity_Id;
      After : in out Node_Id)
   is
      Loc    : constant Source_Ptr := Sloc (E);
      Defun  : Entity_Id;
      Dbody  : Node_Id;
      Formal : Node_Id;
      Dcopy  : Node_Id;

   begin
      Set_Default_Expressions_Processed (E);
      Formal := First_Formal (E);

      while Present (Formal) loop
         if Present (Default_Value (Formal)) then

            --  We work with a copy of the default expression because we
            --  do not want to disturb the original, since this would mess
            --  up the conformance checking.

            Dcopy := New_Copy_Tree (Default_Value (Formal));
            Defun := Default_Expr_Function (Formal);

            --  Case where no default expression function is required

            if No (Defun) then

               --  If there is no default function, we must still do a full
               --  analyze call on the default value, to ensure that all
               --  error checks are performed, e.g. those associated with
               --  static evaluation. Note that this branch will always be
               --  taken if the analyzer is turned off (but we still need the
               --  error checks).

               --  Note: the setting of parent here is to meet the requirement
               --  that we can only analyze the expression while attached to
               --  the tree. Really the requirement is that the parent chain
               --  be set, we don't actually need to be in the tree.

               Set_Parent (Dcopy, Declaration_Node (Formal));
               Analyze (Dcopy);

               --  Default expressions are resolved with their own type if the
               --  context is generic, to avoid anomalies with private types.

               if Ekind (Scope (E)) = E_Generic_Package then
                  Resolve (Dcopy, Etype (Dcopy));
               else
                  Resolve (Dcopy, Etype (Formal));
               end if;

               --  If that resolved expression will raise constraint error,
               --  then flag the default value as raising constraint error.
               --  This allows a proper error message on the calls.

               if Raises_Constraint_Error (Dcopy) then
                  Set_Raises_Constraint_Error (Default_Value (Formal));
               end if;

            --  Else construct the body of the function

            else
               Dbody :=
                 Make_Subprogram_Body (Loc,
                   Specification =>
                     Make_Function_Specification (Loc,
                       Defining_Unit_Name =>
                         Make_Defining_Identifier (Loc, Chars (Defun)),
                       Subtype_Mark =>
                         New_Occurrence_Of (Etype (Formal), Loc)),

                   Declarations => Empty_List,

                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (
                         Make_Return_Statement (Loc,
                           Expression =>
                             New_Copy_Tree (Dcopy)))));

               Insert_After (After, Dbody);
               Analyze (Dbody);
               After := Dbody;
            end if;
         end if;

         Formal := Next_Formal (Formal);
      end loop;

   end Process_Default_Expressions;

   ----------------------------------------
   -- Set_Component_Alignment_If_Not_Set --
   ----------------------------------------

   procedure Set_Component_Alignment_If_Not_Set (Typ : Entity_Id) is
   begin
      --  Ignore if not base type, subtypes don't need anything

      if Typ /= Base_Type (Typ) then
         return;
      end if;

      --  Do not override existing representation

      if Is_Packed (Typ) then
         return;

      elsif Has_Specified_Layout (Typ) then
         return;

      elsif Component_Alignment (Typ) /= Calign_Default then
         return;

      else
         Set_Component_Alignment
           (Typ, Scope_Stack.Table
                  (Scope_Stack.Last).Component_Alignment_Default);
      end if;
   end Set_Component_Alignment_If_Not_Set;

end Freeze;

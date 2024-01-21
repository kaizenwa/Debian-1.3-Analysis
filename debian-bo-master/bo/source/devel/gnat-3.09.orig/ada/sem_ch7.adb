------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M . C H 7                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.242 $                            --
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

--  This package contains the routines to process package specifications and
--  bodies. The most important semantic aspects of package processing are the
--  handling of private and full declarations, and the construction of
--  dispatch tables for tagged types.

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Features; use Features;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch12; use Sem_Ch12;
with Sem_Ch13; use Sem_Ch13;
with Sem_Dist; use Sem_Dist;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Sem_Ch7 is

   -----------------------------------
   -- Handling private declarations --
   -----------------------------------

   --  The principle that each entity has a single defining occurrence clashes
   --  with the presence of two separate definitions for private types: the
   --  first is the private type declaration, and the second is the full type
   --  declaration. It is important that all references to the type point to
   --  the same defining occurence, namely the first one. To enforce the two
   --  separate views of the entity, the corresponding information is swapped
   --  between the two declarations. Outside of the package, the defining
   --  occurence only contains the private declaration information, while in
   --  the private part and the body of the package the defining occurrence
   --  contains the full declaration. To simplify the swap, the defining
   --  occurrence that currently holds the private declaration points to the
   --  full declaration. During semantic processing the defining occurence also
   --  points to a list of private dependents, that is to say access types or
   --  composite types whose designated types or component types are subtypes
   --  or derived types of the private type in question. After the full decla-
   --  ration has been seen, the private dependents are updated to indicate
   --  that they have full definitions.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Install_Composite_Operations (P : Entity_Id);
   --  Composite types declared in the current scope may depend on
   --  types that were private at the point of declaration, and whose
   --  full view is now in  scope. Indicate that the corresponding
   --  operations on the composite type are available.

   function Is_Private_Base_Type (E : Entity_Id) return Boolean;
   --  True for a private type that is not a subtype.

   procedure Preserve_Full_Attributes (Priv, Full : Entity_Id);
   --  Copy to the private declaration the attributes of the full view
   --  that need to be available for the partial view also.

   procedure Declare_Inherited_Private_Subprograms (Id : Entity_Id);
   --  Called upon entering the private part of a public child package
   --  and the body of a nested package, to potentially declare certain
   --  inherited subprograms that were inherited by types in the visible
   --  part, but whose declaration was deferred because the parent
   --  operation was private and not visible at that point. These
   --  subprograms are located by traversing the visible part declarations
   --  looking for nonprivate type extensions and then examining each of
   --  the primitive operations of such types to find those that were
   --  inherited but declared with a special internal name. Each such
   --  operation is now declared as an operation with a normal name (using
   --  the name of the parent operation) and replaces the previous implicit
   --  operation in the primitive operations list of the type.

   --------------------------
   -- Analyze_Package_Body --
   --------------------------

   procedure Analyze_Package_Body (N : Node_Id) is
      Loc              : constant Source_Ptr := Sloc (N);
      Body_Id          : Entity_Id  := Defining_Entity (N);
      Spec_Id          : Entity_Id;
      Last_Spec_Entity : Entity_Id;
      New_N            : Node_Id;
      Pack_Decl        : Node_Id;

   begin
      --  Find corresponding package specification, and establish the
      --  current scope. The visible defining entity for the package is the
      --  defining occurrence in the spec. On exit from the package body, all
      --  body declarations are attached to the defining entity for the body,
      --  but the later is never used for name resolution. In this fashion
      --  there is only one visible entity that denotes the package.

      if Debug_Flag_C then
         Write_Str ("====  Compiling package body ");
         Write_Name (Chars (Defining_Entity (N)));
         Write_Str (" from ");
         Write_Location (Loc);
         Write_Eol;
      end if;

      if Present (Corresponding_Spec (N)) then

         --  Body is body of package instantiation. Corresponding spec
         --  has already been set.

         Spec_Id := Corresponding_Spec (N);
         Pack_Decl := Get_Declaration_Node (Spec_Id);

      else
         Spec_Id := Current_Entity_In_Scope (Defining_Entity (N));

         if Present (Spec_Id)
           and then (Ekind (Spec_Id) = E_Package
                      or else Ekind (Spec_Id) = E_Generic_Package)
         then
            Pack_Decl := Get_Declaration_Node (Spec_Id);

            if Present (Corresponding_Body (Pack_Decl)) then
               Error_Msg_N ("redefinition of package body", N);
               return;
            end if;

         else
            Error_Msg_N ("missing specification for package body", N);
            return;
         end if;

         if (Ekind (Spec_Id) = E_Package
              or else Ekind (Spec_Id) = E_Generic_Package)
           and then
             (Scope (Spec_Id) = Standard_Standard
               or else Is_Child_Unit (Spec_Id))
           and then not Unit_Requires_Body (Spec_Id)
         then
            if Ada_83 then
               Error_Msg_N
                 ("optional package body (not allowed in Ada 95)?", N);
            else
               Error_Msg_N
                 ("spec of this package does not allow a body", N);
            end if;
         end if;
      end if;

      --  If the package is generic,  disable expansion, and perform
      --  analysis on copy. Unannotated body is used in instantiations.

      if Ekind (Spec_Id) = E_Generic_Package then
         Set_Use (Generic_Formal_Declarations (Pack_Decl));

         if Is_Child_Unit (Spec_Id) then
            Note_Feature (Generic_Unit_Child, Loc);
         end if;

         New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
         Rewrite_Substitute_Tree (N, New_N);
         Start_Generic;
      end if;

      --  The Body_Id is that of the copied node in the generic case, the
      --  current node otherwise. Note that N was rewritten above, so we
      --  must be sure to get the latest Body_Id value.

      Body_Id := Defining_Entity (N);
      Set_Ekind (Body_Id, E_Package_Body);

      --  Defining name for the package body is not a visible entity: Only
      --  the defining name for the declaration is visible.

      Set_Ekind (Body_Id, Ekind (Spec_Id));
      Set_Etype (Body_Id, Standard_Void_Type);
      Set_Scope (Body_Id, Current_Scope);
      Set_Corresponding_Spec (N, Spec_Id);
      Set_Corresponding_Body (Pack_Decl, Body_Id);

      --  Indicate that we are currently compiling the body of the package.

      Set_In_Package_Body (Spec_Id);
      Set_Has_Completion (Spec_Id);
      Last_Spec_Entity := Last_Entity (Spec_Id);

      New_Scope (Spec_Id);

      Set_Categorization_From_Pragmas (N);

      Install_Visible_Declarations (Spec_Id);
      Install_Private_Declarations (Spec_Id);
      Install_Composite_Operations (Spec_Id);

      Set_Use (Visible_Declarations (Specification (Pack_Decl)));
      Set_Use (Private_Declarations (Specification (Pack_Decl)));

      --  This is a nested package, so it may be necessary to declare
      --  certain inherited subprograms that are not yet visible because
      --  the parent type's subprograms are now visible.

      if Ekind (Scope (Spec_Id)) = E_Package then
         Declare_Inherited_Private_Subprograms (Spec_Id);
      end if;

      if Present (Declarations (N)) then
         Analyze_Declarations (Declarations (N));
      end if;

      if Present (Handled_Statement_Sequence (N)) then
         Analyze (Handled_Statement_Sequence (N));

         --  Check that elaboration code in a package body has no statements
         --  other than null statements and labels (RM 10.2.1(6)).

         Validate_Null_Statement_Sequence (N);
      end if;

      Validate_Categorization_Dependency (N, Spec_Id);

      Check_Completion (Body_Id);

      if Ekind (Spec_Id) /= E_Package then

         --  For a generic package, collect global references and mark
         --  them on the original body so that they are not resolved
         --  again at the point of instantiation.

         Save_Global_References (Original_Node (N));
         End_Generic;

         if Nkind (Parent (N)) = N_Compilation_Unit then
            End_Package_Scope (Spec_Id);
         else
            --  Local declarations of local packages are not subsequently
            --  visible. Declarations of child units, on the other hand,
            --  should not be unlinked because they may be withed later.

            End_Scope;
         end if;
      end if;

      --  Chain the body declarations to the defining occurrence in the package

      if Present (Last_Spec_Entity) then
         Set_First_Entity (Body_Id, Next_Entity (Last_Spec_Entity));
         Set_Next_Entity (Last_Spec_Entity, Empty);

      else
         Set_First_Entity (Body_Id, First_Entity (Spec_Id));
         Set_First_Entity (Spec_Id, Empty);
      end if;

      if Ekind (Spec_Id) = E_Package then
         End_Package_Scope (Spec_Id);
      end if;

      --  All entities declared in body are not visible.

      declare
         E : Entity_Id;

      begin
         E := First_Entity (Body_Id);

         while Present (E) loop
            Set_Is_Private (E);
            Set_Is_Immediately_Visible (E, False);
            Set_Is_Potentially_Use_Visible (E, False);
            E := Next_Entity (E);
         end loop;
      end;

      Set_In_Package_Body (Spec_Id, False);
      Check_Unset_Variables (Body_Id);
   end Analyze_Package_Body;

   ---------------------------------
   -- Analyze_Package_Declaration --
   ---------------------------------

   procedure Analyze_Package_Declaration (N : Node_Id) is
      Id : constant Node_Id := Defining_Entity (N);
      PF : Boolean;

   begin
      Enter_Name (Id);
      Set_Ekind (Id, E_Package);
      Set_Etype (Id, Standard_Void_Type);
      New_Scope (Id);

      --  Entities declared in Pure unit should be set Is_Pure
      --  Since 'Partition_Id cannot be applied to such an entity

      PF := Is_Pure (Enclosing_Lib_Unit_Entity);
      Set_Is_Pure (Id, PF);

      Set_Categorization_From_Pragmas (N);

      if Debug_Flag_C then
         Write_Str ("====  Compiling package spec ");
         Write_Name (Chars (Id));
         Write_Str (" from ");
         Write_Location (Sloc (N));
         Write_Eol;
      end if;

      Analyze (Specification (N));

      Validate_Categorization_Dependency (N, Id);

      End_Package_Scope (Id);

      --  For a compilation unit, indicate whether it needs a body.

      if Nkind (Parent (N)) = N_Compilation_Unit then
         Set_Body_Required (Parent (N), Unit_Requires_Body (Id));
         Validate_RT_RAT_Component (N);
      end if;

      --  Clear Not_Assigned on all variables in the package spec, because
      --  at this stage some client, or the body, or a child package, may
      --  modify variables in the declaration. Note that we wait till now
      --  to reset these flags, because during analysis of the declaration,
      --  the flags correctly indicated the status up to that point.

      declare
         E : Entity_Id;

      begin
         E := First_Entity (Id);
         while Present (E) loop
            if Ekind (E) = E_Variable then
               Set_Not_Assigned (E, False);
            end if;

            E := Next_Entity (E);
         end loop;
      end;
   end Analyze_Package_Declaration;

   -------------------------------------------
   -- Declare_Inherited_Private_Subprograms --
   -------------------------------------------

   procedure Declare_Inherited_Private_Subprograms (Id : Entity_Id) is
      E : Entity_Id;

   begin
      E := First_Entity (Id);

      while Present (E) loop

         --  If the entity is a nonprivate type extension whose parent
         --  type is declared in an open scope, then the type may have
         --  inherited operations that now need to be made visible.

         if Is_Tagged_Type (E)
           and then Is_Derived_Type (E)
           and then not Is_Private_Type (E)
           and then In_Open_Scopes (Scope (Etype (E)))
           and then E = Base_Type (E)
         then
            declare
               Op_List    : constant Elist_Id := Primitive_Operations (E);
               Op_Elmt    : Elmt_Id := First_Elmt (Op_List);
               Prim_Op    : Entity_Id;
               New_Op     : Entity_Id;
               Formal     : Entity_Id;
               New_Formal : Entity_Id;
               Dummy_Elmt : Elmt_Id;

            begin
               while Present (Op_Elmt) loop
                  Prim_Op := Node (Op_Elmt);

                  --  If the primitive operation is an implicit operation
                  --  with an internal name whose parent operation has
                  --  a normal name, then we now need to declare the
                  --  operation (i.e., make it visible).  (Question:
                  --  can this be done more simply, say by just replacing
                  --  the name of the earlier operation, reentering it
                  --  in the symbol table (how???), and marking it as
                  --  private???)

                  if Present (Alias (Prim_Op))
                    and then not Comes_From_Source (Prim_Op)
                    and then Is_Internal_Name (Chars (Prim_Op))
                    and then not Is_Internal_Name (Chars (Alias (Prim_Op)))
                  then
                     Derive_Subprogram
                       (New_Op, Alias (Prim_Op), E, Etype (E));

                     --  Substitute the new operation for the old one
                     --  in the type's primitive operations list.

                     Replace_Elmt (Op_Elmt, New_Op);
                  end if;

                  Op_Elmt := Next_Elmt (Op_Elmt);
               end loop;
            end;
         end if;
         E := Next_Entity (E);
      end loop;
   end Declare_Inherited_Private_Subprograms;

   -----------------------------------
   -- Analyze_Package_Specification --
   -----------------------------------

   procedure Analyze_Package_Specification (N : Node_Id) is
      Loc          : constant Source_Ptr := Sloc (N);
      Id           : constant Entity_Id  := Defining_Entity (N);
      Vis_Decls    : constant List_Id    := Visible_Declarations (N);
      Priv_Decls   : constant List_Id    := Private_Declarations (N);
      E            : Entity_Id;
      L            : Entity_Id;
      Public_Child : Boolean := False;

      function Is_Public_Child (Child, Unit : Entity_Id) return Boolean;
      --  Child and Unit are entities of compilation units. True if Child
      --  is a public child of Parent as defined in 10.1.1

      function Is_Public_Child (Child, Unit : Entity_Id) return Boolean is
      begin
         if not Is_Private_Descendant (Child) then
            return True;
         else
            if Child = Unit then
               return not Private_Present (
                 Parent (Get_Declaration_Node (Child)));
            else
               return Is_Public_Child (Scope (Child), Unit);
            end if;
         end if;
      end Is_Public_Child;

   --  Start of processing for Analyze_Package_Specification

   begin
      if Present (Vis_Decls) then
         Analyze_Declarations (Vis_Decls);
      end if;

      --  Verify that incomplete types have received full declarations.

      E := First_Entity (Id);

      while Present (E) loop
         if Ekind (E) = E_Incomplete_Type
           and then No (Full_View (E))
         then
            Error_Msg_N ("no declaration in visible part for incomplete}", E);
         end if;

         E := Next_Entity (E);
      end loop;

      if Is_Remote_Call_Interface (Id)
         and then Nkind (Parent (Parent (N))) = N_Compilation_Unit
      then
         Validate_RCI_Declarations (Id);
      end if;

      --  If package is a public child unit, then make the private
      --  declarations of the parent visible.

      if Present (Parent_Spec (Parent (N))) then
         Note_Feature (Child_Units, Loc);

         declare
            Par       : Entity_Id := Id;
            Pack_Decl : Node_Id;

         begin
            while Scope (Par) /= Standard_Standard
              and then Is_Public_Child (Id, Par)
            loop
               Public_Child := True;
               Par := Scope (Par);
               Install_Private_Declarations (Par);
               Pack_Decl := Get_Declaration_Node (Par);
               Set_Use (Private_Declarations (Specification (Pack_Decl)));
            end loop;
         end;
      end if;

      --  Analyze private part if present. The flag In_Private_Part is
      --  reset in End_Package_Scope.

      L := Last_Entity (Id);

      if Present (Priv_Decls) then
         L := Last_Entity (Id);
         Set_In_Private_Part (Id);

         --  Upon entering a public child's private part, it may be
         --  necessary to declare subprograms that were derived in
         --  the package visible part but not yet made visible.

         if Public_Child then
            Declare_Inherited_Private_Subprograms (Id);
         end if;

         Analyze_Declarations (Priv_Decls);

         --  The first private entity is the immediate follower of the last
         --  visible entity, if there was one.

         if Present (L) then
            Set_First_Private_Entity (Id, Next_Entity (L));
         else
            Set_First_Private_Entity (Id, First_Entity (Id));
         end if;

      --  There may be inherited private subprograms that need to be
      --  declared, even in the absence of an explicit private part.
      --  If there are any public declarations in the package and
      --  the package is a public child unit, then an implicit private
      --  part is assumed.

      elsif Present (L) and then Public_Child then
         Set_In_Private_Part (Id);
         Declare_Inherited_Private_Subprograms (Id);
         Set_First_Private_Entity (Id, Next_Entity (L));
      end if;

      --  Check rule of 3.6(11), which in general requires
      --  waiting till all full types have been seen.

      E := First_Entity (Id);
      while Present (E) loop
         if Ekind (E) = E_Record_Type or else Ekind (E) = E_Array_Type then
            Check_Aliased_Component_Types (E);
         end if;

         E := Next_Entity (E);
      end loop;
   end Analyze_Package_Specification;

   --------------------------------------
   -- Analyze_Private_Type_Declaration --
   --------------------------------------

   procedure Analyze_Private_Type_Declaration (N : Node_Id) is
      PF : constant Boolean := Is_Pure (Enclosing_Lib_Unit_Entity);
      Id : Entity_Id := Defining_Identifier (N);

   begin
      --  Entities declared in Pure unit should be set Is_Pure
      --  Since 'Partition_Id cannot be applied to such an entity

      Set_Is_Pure (Id, PF);

      if (Ekind (Current_Scope) /= E_Package
          and then Ekind (Current_Scope) /= E_Generic_Package)
        or else In_Private_Part (Current_Scope)
      then
         Error_Msg_N ("invalid context for private declaration", N);
      end if;

      New_Private_Type (N, Id, N);
      Set_Depends_On_Private (Id);
      Set_Has_Delayed_Freeze (Id);

   end Analyze_Private_Type_Declaration;

   -----------------------
   -- End_Package_Scope --
   -----------------------

   procedure End_Package_Scope (P : Entity_Id) is
      Id   : Entity_Id;
      Decl : Node_Id := Get_Declaration_Node (P);
      Full : Entity_Id;
      Priv_Elmt : Elmt_Id;
      Priv_Sub  : Entity_Id;

   begin
      Id := First_Entity (P);

      while Present (Id) and then Id /= First_Private_Entity (P) loop
         if Debug_Flag_E then
            Write_Str ("unlinking visible entity ");
            Write_Int (Int (Id));
            Write_Eol;
         end if;

         --  On  exit from the package scope, we must preserve the visibility
         --  established by use clauses in the current scope. Two cases:

         --  a) If the entity is an operator, it may be a primitive operator of
         --  a type for which there is a visible use-type clause.

         --  b) for other entities, their use-visibility is determined by a
         --  visible use clause for the package itself. For a generic instance,
         --  the instantiation of the formals appears in the visible part,
         --  but the formals are private and remain so.

         if Ekind (Id) = E_Function
           and then  Is_Operator_Symbol_Name (Chars (Id))
           and then not Is_Private (Id)
         then
            Set_Is_Potentially_Use_Visible (Id,
              In_Use (P)
              or else In_Use (Etype (Id))
              or else In_Use (Etype (First_Formal (Id)))
              or else (Present (Next_Formal (First_Formal (Id)))
               and then In_Use (Etype (Next_Formal (First_Formal (Id))))));
         else
            Set_Is_Potentially_Use_Visible (Id,
              In_Use (P) and not Is_Private (Id));
         end if;

         --  Local entities are not immediately visible outside of the package,
         --  but child units, which appear as local entities, may be in the
         --  context and their visibility status should not be affected by
         --  exit from the parent.

         if not Is_Child_Unit (Id) then
            Set_Is_Immediately_Visible (Id, False);
         end if;

         if Is_Tagged_Type (Id) and then Ekind (Id) = E_Record_Type then
            Check_Abstract_Overriding (Id);
         end if;

         if (Ekind (Id) = E_Private_Type
               or else Ekind (Id) = E_Limited_Private_Type)
           and then No (Full_View (Id))
           and then not Is_Generic_Type (Id)
           and then not Is_Derived_Type (Id)
         then
            Error_Msg_N ("missing full declaration for private type", Id);

         elsif Ekind (Id) = E_Record_Type_With_Private
           and then not Is_Generic_Type (Id)
           and then No (Full_View (Id))
         then
            Error_Msg_N ("missing full declaration for private extension", Id);

         elsif Ekind (Id) = E_Constant
           and then No (Constant_Value (Id))
           and then No (Full_View (Id))
         then
            Error_Msg_N ("missing full declaration for deferred constant", Id);
         end if;

         Id := Next_Entity (Id);
      end loop;

      --  If the specification was installed as the parent of a public child
      --  unit, the private declarations were not installed, and there is
      --  nothing to do.

      if not In_Private_Part (P) then
         Pop_Scope;
         return;
      else
         Set_In_Private_Part (P, False);
      end if;

      --  Make private entities invisible and exchange full and private
      --  declarations for private types.

      while Present (Id) loop
         if Debug_Flag_E then
            Write_Str ("unlinking private entity ");
            Write_Int (Int (Id));
            Write_Eol;
         end if;

         if not Is_Child_Unit (Id) then
            Set_Is_Immediately_Visible (Id, False);
         end if;

         if Is_Private_Base_Type (Id)
           and then Present (Full_View (Id))
         then
            Full := Full_View (Id);

            --  If the partial view is not declared in the visible part
            --  of the package (as is the case when it is a type derived
            --  from some other private type in the private part if the
            --  current package), no exchange takes place.

            if No (Parent (Id))
              or else List_Containing (Parent (Id))
                /= Visible_Declarations (Specification (Decl))
            then
               goto Next_Id;
            end if;

            --  The entry in the private part points to the full declaration,
            --  which is currently visible. Exchange them so only the private
            --  type declaration remains accessible, and link private and
            --  full declaration in the opposite direction. Before the actual
            --  exchange, we copy back attributes of the full view that
            --  must be available to the partial view too.

            Preserve_Full_Attributes (Id, Full);

            Set_Is_Potentially_Use_Visible (Id, In_Use (P));

            if  Is_Indefinite_Subtype (Full)
              and then not Is_Indefinite_Subtype (Id)
            then
               Error_Msg_N
                 ("full view of type must be definite subtype", Full);
            end if;

            Priv_Elmt := First_Elmt (Private_Dependents (Id));
            Exchange_Declarations (Id);

            --  Swap out the subtypes and derived types of Id that were
            --  compiled in this scope, or installed previously by
            --  Install_Private_Declarations.
            --  Before we do the swap, we verify the presence of the
            --  Full_View field which may be empty due to a swap by
            --  a previous call to End_Package_Scope (e.g. from the
            --  freezing mechanism).

            while Present (Priv_Elmt) loop
               Priv_Sub := Node (Priv_Elmt);

               if Present (Full_View (Priv_Sub)) then
                  Preserve_Full_Attributes (Priv_Sub, Full_View (Priv_Sub));
                  Replace_Elmt (Priv_Elmt, Full_View (Priv_Sub));
                  Exchange_Declarations (Priv_Sub);
               end if;

               Priv_Elmt := Next_Elmt (Priv_Elmt);
            end loop;

         elsif Ekind (Id) = E_Incomplete_Type
           and then No (Full_View (Id))
         then
         --  Mark Taft amendment types

            Set_Has_Completion_In_Body (Id);

         elsif not Is_Child_Unit (Id) then
            Set_Is_Private (Id);
            Set_Is_Potentially_Use_Visible (Id, False);
         end if;

         <<Next_Id>>
            Id  := Next_Entity (Id);
      end loop;

      Pop_Scope;
   end End_Package_Scope;

   ---------------------------
   -- Exchange_Declarations --
   ---------------------------

   procedure Exchange_Declarations (Id : Entity_Id) is
      Full_Id : constant Entity_Id := Full_View (Id);
      H1      : constant Entity_Id := Homonym (Id);
      Next1   : constant Entity_Id := Next_Entity (Id);
      H2      : Entity_Id;
      Next2   : Entity_Id;

   begin
      --  If missing full declaration for type, nothing to exchange

      if No (Full_Id) then
         return;
      end if;

      --  Otherwise complete the exchange, and preserve semantic links

      Next2 := Next_Entity (Full_Id);
      H2    := Homonym (Full_Id);

      --  Reset full declaration pointer to reflect the switched entities
      --  and readjust the next entity chains.

      Exchange_Entities (Id, Full_Id);

      Set_Next_Entity (Id, Next1);
      Set_Homonym     (Id, H1);

      Set_Full_View   (Full_Id, Id);
      Set_Next_Entity (Full_Id, Next2);
      Set_Homonym     (Full_Id, H2);
   end Exchange_Declarations;

   ----------------------------
   -- Install_Package_Entity --
   ----------------------------

   procedure Install_Package_Entity (Id : Entity_Id) is
   begin
      if not Is_Internal (Id) then
         if Debug_Flag_E then
            Write_Str ("Install: ");
            Write_Name (Chars (Id));
            Write_Eol;
         end if;

         if not Is_Child_Unit (Id) then
            Set_Is_Immediately_Visible (Id);
         end if;

      end if;
   end Install_Package_Entity;

   ----------------------------------
   -- Install_Composite_Operations --
   ----------------------------------

   procedure Install_Composite_Operations (P : Entity_Id) is
      Id : Entity_Id;

   begin
      Id := First_Entity (P);

      while Present (Id) loop

         if Is_Type (Id)
           and then (Is_Limited_Composite (Id)
                      or else Is_Private_Composite (Id))
           and then No (Private_Component (Id))
         then
            Set_Is_Limited_Composite (Id, False);
            Set_Is_Private_Composite (Id, False);
         end if;

         Id := Next_Entity (Id);
      end loop;
   end Install_Composite_Operations;

   ----------------------------------
   -- Install_Private_Declarations --
   ----------------------------------

   procedure Install_Private_Declarations (P : Entity_Id) is
      Id        : Entity_Id;
      Priv_Elmt : Elmt_Id;
      Priv      : Entity_Id;

   begin
      --  First exchange declarations for private types, so that the
      --  full declaration is visible. For each private type, we check
      --  its Private_Dependents list and also exchange any subtypes of
      --  or derived types from it. Finally, if this is a Taft amendment
      --  type, the incomplete declaration is irrelevant, and we want to
      --  link the eventual full declaration with the original private
      --  one so we also skip the exchange.

      Id := First_Entity (P);
      while Present (Id) and then Id /= First_Private_Entity (P) loop
         if Is_Private_Base_Type (Id)
           and then Comes_From_Source (Full_View (Id))
           and then Present (Full_View (Id))
           and then Scope (Full_View (Id)) = Scope (Id)
           and then Ekind (Full_View (Id)) /= E_Incomplete_Type
         then
            Priv_Elmt := First_Elmt (Private_Dependents (Id));

            --  If there is a use-type clause on the private type, set the
            --  full view accordingly.

            Set_In_Use (Full_View (Id), In_Use (Id));
            Exchange_Declarations (Id);
            Set_Is_Immediately_Visible (Id);

            while Present (Priv_Elmt) loop
               Priv := Node (Priv_Elmt);

               --  Before the exchange, verify that the presence of the
               --  Full_View field. It will be empty if the entity
               --  has already been installed due to a previous call.

               if Present (Full_View (Priv)) then

                  --  For each subtype that is swapped, we also swap the
                  --  reference to it in Private_Dependents, to allow access
                  --  to it when we swap them out in End_Package_Scope.

                  Replace_Elmt (Priv_Elmt, Full_View (Priv));
                  Exchange_Declarations (Priv);
                  Set_Is_Immediately_Visible
                    (Priv, In_Open_Scopes (Scope (Priv)));
                  Set_Is_Potentially_Use_Visible
                    (Priv, Is_Potentially_Use_Visible (Node (Priv_Elmt)));
               end if;

               Priv_Elmt := Next_Elmt (Priv_Elmt);
            end loop;
         end if;

         Id := Next_Entity (Id);
      end loop;

      --  Next make other declarations in the private part visible as well.

      Id := First_Private_Entity (P);

      while Present (Id) loop
         Install_Package_Entity (Id);
         Id := Next_Entity (Id);
      end loop;

      --  Indicate that the private part is currently visible, so it can be
      --  properly reset on exit.

      Set_In_Private_Part (P);
   end Install_Private_Declarations;

   ----------------------------------
   -- Install_Visible_Declarations --
   ----------------------------------

   procedure Install_Visible_Declarations (P : Entity_Id) is
      Id : Entity_Id;

   begin
      Id := First_Entity (P);

      while Present (Id) and then Id /= First_Private_Entity (P) loop
         Install_Package_Entity (Id);
         Id := Next_Entity (Id);
      end loop;
   end Install_Visible_Declarations;

   ----------------------
   -- Is_Fully_Visible --
   ----------------------

   --  The full declaration of a private type is visible in the private
   --  part of the package declaration, and in the package body, at which
   --  point the full declaration must have been given.

   function Is_Fully_Visible (Type_Id : Entity_Id) return Boolean is
      S : constant Entity_Id := Scope (Type_Id);

   begin
      if Is_Generic_Type (Type_Id) then
         return False;

      elsif In_Private_Part (S) then
         return Present (Full_View (Type_Id));

      else
         return In_Package_Body (S);
      end if;
   end Is_Fully_Visible;

   --------------------------
   -- Is_Private_Base_Type --
   --------------------------

   function Is_Private_Base_Type (E : Entity_Id) return Boolean is
   begin
      return Ekind (E) = E_Private_Type
        or else Ekind (E) = E_Limited_Private_Type
        or else Ekind (E) = E_Record_Type_With_Private;
   end Is_Private_Base_Type;

   ----------------------------
   -- May_Need_Implicit_Body --
   ----------------------------

   procedure May_Need_Implicit_Body (E : Entity_Id) is
      P     : constant Node_Id := Get_Declaration_Node (E);
      S     : constant Node_Id := Parent (P);
      B     : Node_Id;
      Decls : List_Id;

   begin
      if not Has_Completion (E)
        and then Nkind (P) = N_Package_Declaration
        and then Present (Activation_Chain_Entity (P))
      then
         B :=
           Make_Package_Body (Sloc (E),
             Defining_Unit_Name => Make_Defining_Identifier (Sloc (E),
               Chars => Chars (E)),
             Declarations  => New_List);

         if Nkind (S) = N_Package_Specification then
            if Present (Private_Declarations (S)) then
               Decls := Private_Declarations (S);
            else
               Decls := Visible_Declarations (S);
            end if;
         else
            Decls := Declarations (S);
         end if;

         Append (B, Decls);
         Analyze (B);
      end if;
   end May_Need_Implicit_Body;

   ----------------------
   -- New_Private_Type --
   ----------------------

   procedure New_Private_Type (N : Node_Id; Id : Entity_Id; Def : Node_Id) is
   begin
      Enter_Name (Id);

      if Limited_Present (Def) then
         Set_Ekind (Id, E_Limited_Private_Type);
      else
         Set_Ekind (Id, E_Private_Type);
      end if;

      Set_Etype (Id, Id);
      Set_Has_Delayed_Freeze (Id);
      Set_Is_First_Subtype (Id);

      Set_Is_Constrained (Id,
        No (Discriminant_Specifications (N))
          and then not Unknown_Discriminants_Present (N));

      Set_Discriminant_Constraint (Id, No_Elist);
      Set_Girder_Constraint (Id, No_Elist);

      if Present (Discriminant_Specifications (N)) then
         New_Scope (Id);
         Process_Discriminants (N);
         End_Scope;

      elsif Unknown_Discriminants_Present (N) then
         Set_Has_Unknown_Discriminants (Id);
      end if;

      Set_Private_Dependents (Id, New_Elmt_List);

      if Tagged_Present (Def) then
         Set_Is_Tagged_Type       (Id, True);
         Set_Ekind                (Id, E_Record_Type_With_Private);
         Make_Class_Wide_Type     (Id);
         Set_Primitive_Operations (Id, New_Elmt_List);
         Set_Is_Abstract          (Id, Abstract_Present (Def));
         Set_Is_Limited_Record    (Id, Limited_Present (Def));
         Set_Has_Delayed_Freeze   (Id, True);

      elsif Abstract_Present (Def) then
         Error_Msg_N ("only a tagged type can be abstract", N);
      end if;
   end New_Private_Type;

   ------------------------------
   -- Preserve_Full_Attributes --
   ------------------------------

   procedure Preserve_Full_Attributes (Priv, Full : Entity_Id) is
      Priv_Is_Base_Type : constant Boolean := Priv = Base_Type (Priv);

   begin
      Set_Size_Info                   (Priv,                          (Full));
      Set_Size_Known_At_Compile_Time  (Priv, Size_Known_At_Compile_Time
                                                                      (Full));

      if Priv_Is_Base_Type then
         Set_Is_Controlled            (Priv, Is_Controlled (Base_Type (Full)));
         Set_Has_Task                 (Priv, Has_Task      (Base_Type (Full)));
         Set_Has_Controlled_Component (Priv, Has_Controlled_Component
                                                           (Base_Type (Full)));
      end if;

      Set_Freeze_Node              (Priv, Freeze_Node                 (Full));

      if Is_Tagged_Type (Priv) then
         if Priv_Is_Base_Type then
            Set_Access_Disp_Table     (Priv, Access_Disp_Table
                                                          (Base_Type (Full)));
         end if;

         Set_First_Entity             (Priv, First_Entity             (Full));
         Set_Last_Entity              (Priv, Last_Entity              (Full));
      end if;
   end Preserve_Full_Attributes;

   ------------------------
   -- Unit_Requires_Body --
   ------------------------

   function Unit_Requires_Body (P : Entity_Id) return Boolean is
      E : Entity_Id;

   begin
      --  Body required if library package with pragma Elaborate_Body

      if (Ekind (P) = E_Package or else Ekind (P) = E_Generic_Package)
        and then
          (Scope (P) = Standard_Standard or else Is_Child_Unit (P))
        and then
          Elaborate_Body_Present (Cunit (Get_Sloc_Unit_Number (Sloc (P))))
      then
         return True;

      elsif Ekind (P) = E_Package
        and then Nkind (Parent (P)) = N_Package_Specification
        and then Present (Generic_Parent (Parent (P)))
      then
         declare
            G_P : Entity_Id := Generic_Parent (Parent (P));

         begin

            if (Scope (G_P) = Standard_Standard
                or else Is_Child_Unit (G_P))
              and then
                Elaborate_Body_Present
                  (Cunit (Get_Sloc_Unit_Number (Sloc (G_P))))
            then
               return True;
            end if;
         end;
      end if;

      --  Otherwise search entity chain for entity requiring completion

      E := First_Entity (P);
      while Present (E) loop

         if (Is_Overloadable (E)
               and then Ekind (E) /= E_Enumeration_Literal
               and then Ekind (E) /= E_Operator
               and then not Is_Abstract (E)
               and then not Has_Completion (E))

           or else
             (Ekind (E) = E_Package and then E /= P
               and then not Has_Completion (E)
               and then Unit_Requires_Body (E))

           or else
             (Ekind (E) = E_Incomplete_Type and then No (Full_View (E)))

           or else
            ((Ekind (E) = E_Task_Type or else
              Ekind (E) = E_Protected_Type)
               and then not Has_Completion (E))

           or else
             (Ekind (E) = E_Generic_Package and then E /= P
               and then not Has_Completion (E)
               and then not Is_Child_Unit (E)
               and then Unit_Requires_Body (E))

           or else
             (Ekind (E) = E_Generic_Function
               and then not Has_Completion (E)
               and then not Is_Child_Unit (E))

           or else
             (Ekind (E) = E_Generic_Procedure
               and then not Has_Completion (E)
               and then not Is_Child_Unit (E))

         then
            return True;
         else
            null;
         end if;

         E := Next_Entity (E);
      end loop;

      return False;
   end Unit_Requires_Body;

end Sem_Ch7;

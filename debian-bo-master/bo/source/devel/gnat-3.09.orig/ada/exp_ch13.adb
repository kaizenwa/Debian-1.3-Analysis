------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C H 1 3                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.51 $                             --
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
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch6;  use Exp_Ch6;
with Exp_TSS;  use Exp_TSS;
with Exp_Util; use Exp_Util;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch13 is

   ------------------------------------------
   -- Expand_N_Attribute_Definition_Clause --
   ------------------------------------------

   --  Expansion action depends on attribute involved

   procedure Expand_N_Attribute_Definition_Clause (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Exp : constant Node_Id    := Expression (N);
      Ent : Entity_Id;
      V   : Node_Id;

   begin
      Ent := Entity (Name (N));

      if Is_Type (Ent) then
         Ent := Underlying_Type (Ent);
      end if;

      case Get_Attribute_Id (Chars (N)) is

         -------------
         -- Address --
         -------------

         when Attribute_Address =>

            --  If there is an initialization which did not come from
            --  the source program, then it is an artifact of our
            --  expansion, and we suppress it. The case we are most
            --  concerned about here is the initialization of a packed
            --  array to all false, which seems inappropriate for a
            --  variable to which an address clause is applied.

            declare
               Decl : constant Node_Id := Declaration_Node (Ent);

            begin
               if Nkind (Decl) = N_Object_Declaration
                  and then Present (Expression (Decl))
                  and then not Comes_From_Source (Expression (Decl))
               then
                  Set_Expression (Decl, Empty);
               end if;
            end;

         ---------------
         -- Alignment --
         ---------------

         when Attribute_Alignment =>

            --  As required by Gigi, we guarantee that the operand is an
            --  integer literal (this simplifies things in Gigi).

            if Nkind (Exp) /= N_Integer_Literal then
               Rewrite_Substitute_Tree
                 (Exp, Make_Integer_Literal (Loc, Expr_Value (Exp)));
            end if;

         ------------------
         -- External_Tag --
         ------------------

         --  For the rep clause "for x'external_tag use y" generate:

         --     xV : constant string := y;
         --     Set_External_Tag (x'tag, xV'Address);
         --     Register_Tag (x'tag);

         --  note that register_tag has been delayed up to now because
         --  the external_tag must be set before resistering.

         when Attribute_External_Tag => External_Tag : declare
            E : Entity_Id;

         begin
            if Is_Entity_Name (Exp) then
               E := Entity (Exp);

            else
               E := Make_Defining_Identifier (Loc,
                      New_External_Name (Chars (Ent), 'A'));

               Insert_Action (N,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => E,
                   Constant_Present    => True,
                   Object_Definition   =>
                     New_Reference_To (Standard_String, Loc),
                   Expression          => Relocate_Node (Exp)));
            end if;

            Insert_Actions (N, New_List (
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Set_External_Tag), Loc),
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_Tag,
                    Prefix         => New_Occurrence_Of (Ent, Loc)),

                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_Address,
                    Prefix         => New_Occurrence_Of (E, Loc)))),

              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Register_Tag), Loc),
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_Tag,
                    Prefix         => New_Occurrence_Of (Ent, Loc))))));
         end External_Tag;

         ------------------
         -- Storage_Size --
         ------------------

         when Attribute_Storage_Size =>

            --  If the type is a task type, then assign the value of the
            --  storage size to the Size variable associated with the task.
            --    task_typeZ := expression

            if Ekind (Ent) = E_Task_Type then
               Insert_Action (N,
                 Make_Assignment_Statement (Loc,
                   Name => New_Reference_To (Storage_Size_Variable (Ent), Loc),
                   Expression =>
                     Convert_To (RTE (RE_Size_Type), Expression (N))));

            --  For Storage_Size for an access type, create a variable to hold
            --  the value of the specified size with name typeV and expand an
            --  assignment statement to initialze this value.

            elsif Is_Access_Type (Ent) then

               V := Make_Defining_Identifier (Loc,
                      New_External_Name (Chars (Ent), 'V'));

               Insert_Action (N,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => V,
                   Object_Definition  =>
                     New_Reference_To (RTE (RE_Storage_Offset), Loc),
                   Expression =>
                     Convert_To (RTE (RE_Storage_Offset), Expression (N))));

               Set_Storage_Size_Variable (Ent, Entity_Id (V));
            end if;

         --  Other attributes require no expansion

         when others =>
            null;

      end case;

   end Expand_N_Attribute_Definition_Clause;

   ----------------------------
   -- Expand_N_Freeze_Entity --
   ----------------------------

   procedure Expand_N_Freeze_Entity (N : Node_Id) is
      E           : constant Entity_Id := Entity (N);
      Other_Scope : Boolean;
      S           : Entity_Id;

   begin
      if not Is_Type (E) and then not Is_Subprogram (E) then
         return;
      end if;

      --  If the entity being frozen is defined in a scope that is not
      --  currently on the stack-scope, we must establish the proper
      --  visibility before freezing the entity, and related subprograms.

      S := Current_Scope;
      while S /= Standard_Standard and then S /= Scope (E) loop
         S := Scope (S);
      end loop;

      Other_Scope := not (S = Scope (E));

      if Other_Scope then
         New_Scope (Scope (E));
         Install_Visible_Declarations (Scope (E));
         Install_Private_Declarations (Scope (E));
      end if;

      --  If type, freeze the type

      if Is_Type (E) then
         Freeze_Type (N);

      --  If subprogram, freeze the subprogram

      elsif Is_Subprogram (E) then
         Freeze_Subprogram (N);

      --  No other entities require any front end freeze actions

      else
         null;
      end if;

      --  Analyze actions generated by freezing.

      if Present (Actions (N)) then
         Analyze_List (Actions (N));
      end if;

      if Other_Scope then
         if Ekind (Current_Scope) = E_Package then
            End_Package_Scope (Scope (E));
         else
            End_Scope;
         end if;
      end if;
   end Expand_N_Freeze_Entity;

   -------------------------------------------
   -- Expand_N_Record_Representation_Clause --
   -------------------------------------------

   --  The only expansion required is for the case of a mod clause present,
   --  which is removed, and translated into an alignment representation
   --  clause inserted immediately after the record rep clause with any
   --  initial pragmas inserted at the start of the component clause list.

   procedure Expand_N_Record_Representation_Clause (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Rectype : constant Entity_Id  := Entity (Identifier (N));
      Mod_Val : Uint;
      Citems  : List_Id;
      Repitem : Node_Id;
      AtM_Nod : Node_Id;

   begin
      if Present (Mod_Clause (N)) then
         Mod_Val := Expr_Value (Expression (Mod_Clause (N)));
         Citems  := Pragmas_Before (Mod_Clause (N));

         if Present (Citems) then
            Append_List_To (Citems, Component_Clauses (N));
            Set_Component_Clauses (N, Citems);
         end if;

         AtM_Nod :=
           Make_Attribute_Definition_Clause (Loc,
             Name       => New_Reference_To (Base_Type (Rectype), Loc),
             Chars      => Name_Alignment,
             Expression => Make_Integer_Literal (Loc, Mod_Val));

         Set_From_At_Mod (AtM_Nod);
         Insert_After (N, AtM_Nod);
         Set_Mod_Clause (N, Empty);
      end if;

      --  If the record representation clause has no components, then
      --  completely remove it.  Note that we also have to remove
      --  ourself from the Rep Item list.

      if Is_Empty_List (Component_Clauses (N)) then
         if First_Rep_Item (Rectype) = N then
            Set_First_Rep_Item (Rectype, Next_Rep_Item (N));
         else
            Repitem := First_Rep_Item (Rectype);
            while Present (Next_Rep_Item (Repitem)) loop
               if Next_Rep_Item (Repitem) = N then
                  Set_Next_Rep_Item (Repitem, Next_Rep_Item (N));
                  exit;
               end if;

               Repitem := Next_Rep_Item (Repitem);
            end loop;
         end if;

         Rewrite_Substitute_Tree (N,
           Make_Null_Statement (Loc));
      end if;
   end Expand_N_Record_Representation_Clause;

end Exp_Ch13;

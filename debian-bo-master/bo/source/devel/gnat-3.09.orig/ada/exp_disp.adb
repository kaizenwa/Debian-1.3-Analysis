------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D I S P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.38 $                             --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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
with Errout;   use Errout;
with Exp_TSS;  use Exp_TSS;
with Exp_Util; use Exp_Util;
with Expander; use Expander;
with Itypes;   use Itypes;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Disp; use Sem_Disp;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Disp is

   Ada_Actions : constant array (DT_Access_Action) of RE_Id :=
      (CW_Membership           => RE_CW_Membership,
       DT_Entry_Size           => RE_DT_Entry_Size,
       DT_Prologue_Size        => RE_DT_Prologue_Size,
       Get_Expanded_Name       => RE_Get_Expanded_Name,
       Get_External_Tag        => RE_Get_External_Tag,
       Get_Prim_Op_Address     => RE_Get_Prim_Op_Address,
       Get_TSD                 => RE_Get_TSD,
       Inherit_DT              => RE_Inherit_DT,
       Inherit_TSD             => RE_Inherit_TSD,
       Register_Tag            => RE_Register_Tag,
       Set_Expanded_Name       => RE_Set_Expanded_Name,
       Set_External_Tag        => RE_Set_External_Tag,
       Set_Prim_Op_Address     => RE_Set_Prim_Op_Address,
       Set_TSD                 => RE_Set_TSD,
       TSD_Entry_Size          => RE_TSD_Entry_Size,
       TSD_Prologue_Size       => RE_TSD_Prologue_Size);

   CPP_Actions : constant array (DT_Access_Action) of RE_Id :=
      (CW_Membership           => RE_CPP_CW_Membership,
       DT_Entry_Size           => RE_CPP_DT_Entry_Size,
       DT_Prologue_Size        => RE_CPP_DT_Prologue_Size,
       Get_Expanded_Name       => RE_CPP_Get_Expanded_Name,
       Get_External_Tag        => RE_CPP_Get_External_Tag,
       Get_Prim_Op_Address     => RE_CPP_Get_Prim_Op_Address,
       Get_TSD                 => RE_CPP_Get_TSD,
       Inherit_DT              => RE_CPP_Inherit_DT,
       Inherit_TSD             => RE_CPP_Inherit_TSD,
       Register_Tag            => RE_CPP_Register_Tag,
       Set_Expanded_Name       => RE_CPP_Set_Expanded_Name,
       Set_External_Tag        => RE_CPP_Set_External_Tag,
       Set_Prim_Op_Address     => RE_CPP_Set_Prim_Op_Address,
       Set_TSD                 => RE_CPP_Set_TSD,
       TSD_Entry_Size          => RE_CPP_TSD_Entry_Size,
       TSD_Prologue_Size       => RE_CPP_TSD_Prologue_Size);

   Action_Is_Proc : constant array (DT_Access_Action) of Boolean :=
      (CW_Membership           => False,
       DT_Entry_Size           => False,
       DT_Prologue_Size        => False,
       Get_Expanded_Name       => False,
       Get_External_Tag        => False,
       Get_Prim_Op_Address     => False,
       Get_TSD                 => False,
       Inherit_DT              => True,
       Inherit_TSD             => True,
       Register_Tag            => True,
       Set_Expanded_Name       => True,
       Set_External_Tag        => True,
       Set_Prim_Op_Address     => True,
       Set_TSD                 => True,
       TSD_Entry_Size          => False,
       TSD_Prologue_Size       => False);

   Action_Nb_Arg : constant array (DT_Access_Action) of Int :=
      (CW_Membership           => 2,
       DT_Entry_Size           => 0,
       DT_Prologue_Size        => 0,
       Get_Expanded_Name       => 1,
       Get_External_Tag        => 1,
       Get_Prim_Op_Address     => 2,
       Get_TSD                 => 1,
       Inherit_DT              => 3,
       Inherit_TSD             => 2,
       Register_Tag            => 1,
       Set_Expanded_Name       => 2,
       Set_External_Tag        => 2,
       Set_Prim_Op_Address     => 3,
       Set_TSD                 => 2,
       TSD_Entry_Size          => 0,
       TSD_Prologue_Size       => 0);

   ---------------------------
   -- Make_DT_Access_Action --
   ---------------------------

   function Make_DT_Access_Action
     (Typ    : Entity_Id;
      Action : DT_Access_Action;
      Args   : List_Id)
      return Node_Id
   is
      Action_Name : Entity_Id;
      Loc         : Source_Ptr;

   begin
      if Is_CPP_Class (Root_Type (Typ)) then
         Action_Name := RTE (CPP_Actions (Action));
      else
         Action_Name := RTE (Ada_Actions (Action));
      end if;

      if No (Args) then

         --  This is a constant

         return New_Reference_To (Action_Name, Sloc (Typ));
      end if;

      pragma Assert (List_Length (Args) = Action_Nb_Arg (Action));

      Loc := Sloc (First (Args));

      if Action_Is_Proc (Action) then
         return
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (Action_Name, Loc),
             Parameter_Associations => Args);

      else
         return
           Make_Function_Call (Loc,
             Name => New_Reference_To (Action_Name, Loc),
             Parameter_Associations => Args);
      end if;
   end Make_DT_Access_Action;

   -------------------------
   -- Set_All_DT_Position --
   -------------------------

   procedure Set_All_DT_Position (Typ : Entity_Id) is
      First_Prim : constant Elmt_Id := First_Elmt (Primitive_Operations (Typ));
      Nb_Prim    : Int;
      Prim       : Entity_Id;
      The_Tag    : constant Entity_Id := Tag_Component (Typ);
      Prim_Elmt  : Elmt_Id;

   begin
      --  C++ Case, check that pragma CPP_Class, CPP_Virtual and CPP_Vtable
      --  give a coherent set of information

      if Is_CPP_Class (Typ) then

         --  Compute the number of primitive operations in the main Vtable

         Prim_Elmt := First_Prim;
         Nb_Prim := 0;
         while Present (Prim_Elmt) loop
            Prim := Node (Prim_Elmt);

            if Present (Alias (Prim)) then
               Nb_Prim := Nb_Prim + 1;
               Set_DTC_Entity (Prim, DTC_Entity (Alias (Prim)));
            end if;

            if No (DTC_Entity (Prim)) then
               Error_Msg_NE
                 ("is a primitive operation of&, pragma CPP_Virtual required",
                  Prim, Typ);

            elsif DTC_Entity (Prim) = The_Tag then
               Nb_Prim := Nb_Prim + 1;
               if DT_Position (Prim) = No_Uint then
                  Set_DT_Position (Prim, UI_From_Int (Nb_Prim));
               end if;
            end if;

            Prim_Elmt := Next_Elmt (Prim_Elmt);
         end loop;

         --  Check that the declared size of the Vtable is bigger or equal
         --  than the number of primitive operations (if bigger it means that
         --  some of the c++ virtual functions were not imported, that is
         --  allowed)

         if DT_Entry_Count (The_Tag) = No_Uint then
            Set_DT_Entry_Count (The_Tag, UI_From_Int (Nb_Prim));

         elsif UI_To_Int (DT_Entry_Count (The_Tag)) < Nb_Prim then
            Error_Msg_N ("not enough room in the Vtable for all virtual"
              & " functions", The_Tag);
         end if;

         --  Check that Positions are not duplicate nor outside the range of
         --  the Vtable

         declare
            Size : constant Int := UI_To_Int (DT_Entry_Count (The_Tag));
            Pos  : Int;
            Prim_Pos_Table : array (1 .. Size) of Entity_Id :=
                                                        (others => Empty);

         begin
            Prim_Elmt := First_Prim;
            while Present (Prim_Elmt) loop
               Prim := Node (Prim_Elmt);

               if DTC_Entity (Prim) = The_Tag then
                  Pos := UI_To_Int (DT_Position (Prim));

                  if Pos not in Prim_Pos_Table'Range then
                     Error_Msg_N
                       ("position not in range of virtual table", Prim);

                  elsif Present (Prim_Pos_Table (Pos)) then
                     Error_Msg_NE ("cannot be at the same position in the"
                       & " vtable than&", Prim, Prim_Pos_Table (Pos));

                  else
                     Prim_Pos_Table (Pos) := Prim;
                  end if;
               end if;

               Prim_Elmt := Next_Elmt (Prim_Elmt);
            end loop;
         end;

      --  For regular Ada tagged types, just set the DT_Position for each
      --  primitive operation.

      else
         Nb_Prim := 0;
         Prim_Elmt := First_Prim;
         while Present (Prim_Elmt) loop
            Nb_Prim := Nb_Prim + 1;
            Prim := Node (Prim_Elmt);
            Set_DTC_Entity (Prim, The_Tag);
            Set_DT_Position (Prim, UI_From_Int (Nb_Prim));
            Prim_Elmt := Next_Elmt (Prim_Elmt);
         end loop;

         Set_DT_Entry_Count (The_Tag, UI_From_Int (Nb_Prim));
      end if;
   end Set_All_DT_Position;

   -------------
   -- Make_DT --
   -------------

   function Make_DT (Typ : Entity_Id) return List_Id is
      Result    : constant List_Id := New_List;
      Elab_Code : constant List_Id := New_List;
      Loc       : constant Source_Ptr := Sloc (Typ);

      Tname       : constant Name_Id := Chars (Typ);
      Name_DT     : constant Name_Id := New_External_Name (Tname, 'T');
      Name_DT_Ptr : constant Name_Id := New_External_Name (Tname, 'P');
      Name_TSD    : constant Name_Id := New_External_Name (Tname, 'B');
      Name_Exname : constant Name_Id := New_External_Name (Tname, 'E');
      Name_No_Reg : constant Name_Id := New_External_Name (Tname, 'F');

      DT     : constant Node_Id := Make_Defining_Identifier (Loc, Name_DT);
      DT_Ptr : constant Node_Id := Make_Defining_Identifier (Loc, Name_DT_Ptr);
      TSD    : constant Node_Id := Make_Defining_Identifier (Loc, Name_TSD);
      Exname : constant Node_Id := Make_Defining_Identifier (Loc, Name_Exname);
      No_Reg : constant Node_Id := Make_Defining_Identifier (Loc, Name_No_Reg);

      I_Depth         : Int;
      Generalized_Tag : Entity_Id;
      Size_Expr_Node  : Node_Id;
      Old_Tag         : Node_Id;
      Old_TSD         : Node_Id;

   begin
      if Is_CPP_Class (Root_Type (Typ)) then
         Generalized_Tag := RTE (RE_Vtable_Ptr);
      else
         Generalized_Tag := RTE (RE_Tag);
      end if;

      --  Dispatch table and related entities are allocated statically

      Set_Ekind (DT, E_Variable);
      Set_Is_Statically_Allocated (DT);

      Set_Ekind (DT_Ptr, E_Variable);
      Set_Is_Statically_Allocated (DT_Ptr);

      Set_Ekind (TSD, E_Variable);
      Set_Is_Statically_Allocated (TSD);

      Set_Ekind (Exname, E_Variable);
      Set_Is_Statically_Allocated (Exname);

      Set_Ekind (No_Reg, E_Variable);
      Set_Is_Statically_Allocated (No_Reg);

      --  Create the storage for the Dispatch_Table object:

      --   DT : Storage_Array (1..DT_Prologue_Size+nb_prim*DT_Entry_Size);
      --   for DT'Alignment use Address'Alignment

      Size_Expr_Node :=
        Make_Op_Add (Loc,
          Left_Opnd  => Make_DT_Access_Action (Typ, DT_Prologue_Size, No_List),
          Right_Opnd =>
            Make_Op_Multiply (Loc,
              Left_Opnd  =>
                Make_DT_Access_Action (Typ, DT_Entry_Size, No_List),
              Right_Opnd =>
                Make_Integer_Literal (Loc,
                  DT_Entry_Count (Tag_Component (Typ)))));

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => DT,
          Aliased_Present     => True,
          Object_Definition   =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Array), Loc),
              Constraint   => Make_Index_Or_Discriminant_Constraint (Loc,
                Constraints => New_List (
                  Make_Range (Loc,
                    Low_Bound  => Make_Integer_Literal (Loc, Uint_1),
                    High_Bound => Size_Expr_Node))))));

      Append_To (Result,
        Make_Attribute_Definition_Clause (Loc,
          Name       => New_Reference_To (DT, Loc),
          Chars      => Name_Alignment,
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (RTE (RE_Integer_Address), Loc),
              Attribute_Name => Name_Alignment)));

      --  Create the pointer to the dispatch table

      --    DT_Ptr : Tag := Tag!(DT'Address);                 Ada case
      --  or
      --    DT_Ptr : Vtable_Ptr := Vtable_Ptr!(DT'Address);   CPP case


      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => DT_Ptr,
          Constant_Present    => True,
          Object_Definition   => New_Reference_To (Generalized_Tag, Loc),
          Expression          =>
            Unchecked_Convert_To (Generalized_Tag,
              Make_Attribute_Reference (Loc,
                Prefix         => New_Reference_To (DT, Loc),
                Attribute_Name => Name_Address))));

      --  Define the boolean that controls registration, in order to avoid
      --  multiple registrations for tagged types defined in multiple-called
      --  scopes

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => No_Reg,
          Object_Definition   => New_Reference_To (Standard_Boolean, Loc),
          Expression          => New_Reference_To (Standard_True, Loc)));

      --  Set Access_Disp_Table field to be the dispatch table pointer

      Set_Access_Disp_Table (Typ, DT_Ptr);

      --  Count ancestors to compute the inheritance depth. For private
      --  extensions, always go to the full view in order to compute the real
      --  inheritance depth.

      declare
         Parent_Type : Entity_Id := Typ;
         P           : Entity_Id;

      begin
         I_Depth := 0;

         loop
            P := Etype (Parent_Type);

            if Is_Private_Type (P) then
               P := Full_View (Base_Type (P));
            end if;

            exit when P = Parent_Type;

            I_Depth := I_Depth + 1;
            Parent_Type := P;
         end loop;
      end;

      --  Create the storage for the type specific data object:
      --   TSD: Storage_Array (1..TSD_Prologue_Size+(1+Idepth)*TSD_Entry_Size);
      --   for TSD'Alignment use Address'Alignment

      Size_Expr_Node :=
        Make_Op_Add (Loc,
          Left_Opnd  =>
            Make_DT_Access_Action (Typ, TSD_Prologue_Size, No_List),
          Right_Opnd =>
            Make_Op_Multiply (Loc,
              Left_Opnd  =>
                Make_DT_Access_Action (Typ, TSD_Entry_Size, No_List),
              Right_Opnd =>
                Make_Op_Add (Loc,
                  Left_Opnd  => Make_Integer_Literal (Loc, Uint_1),
                  Right_Opnd =>
                    Make_Integer_Literal (Loc, UI_From_Int (Int (I_Depth))))));

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => TSD,
          Aliased_Present     => True,
          Object_Definition   =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Array), Loc),
              Constraint   => Make_Index_Or_Discriminant_Constraint (Loc,
                Constraints => New_List (
                  Make_Range (Loc,
                    Low_Bound  => Make_Integer_Literal (Loc, Uint_1),
                    High_Bound => Size_Expr_Node))))));

      Append_To (Result,
        Make_Attribute_Definition_Clause (Loc,
          Name       => New_Reference_To (TSD, Loc),
          Chars      => Name_Alignment,
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (RTE (RE_Integer_Address), Loc),
              Attribute_Name => Name_Alignment)));

      --  Put the Address of the TSD in the Dispatch Table
      --    Set_TSD (DT_Ptr, TSD);

      Append_To (Elab_Code,
        Make_DT_Access_Action (Typ,
          Action => Set_TSD,
          Args   => New_List (
            New_Reference_To (DT_Ptr, Loc),                  -- DTptr
              Make_Attribute_Reference (Loc,                 -- Value
              Prefix          => New_Reference_To (TSD, Loc),
              Attribute_Name  => Name_Address))));

      if Typ = Etype (Typ)
        or else Is_CPP_Class (Etype (Typ))
      then
         Old_Tag :=
           Unchecked_Convert_To (Generalized_Tag,
             Make_Integer_Literal (Loc, Uint_0));

         Old_TSD :=
           Unchecked_Convert_To (RTE (RE_Address),
             Make_Integer_Literal (Loc, Uint_0));
      else
         Old_Tag := New_Reference_To (Access_Disp_Table (Etype (Typ)), Loc);
         Old_TSD :=
           Make_DT_Access_Action (Typ,
             Action => Get_TSD,
             Args   => New_List (
               New_Reference_To (Access_Disp_Table (Etype (Typ)), Loc)));
      end if;

      --  Inherit_DT (parent'tag, DT_Ptr, nb_prim of parent);

      Append_To (Elab_Code,
        Make_DT_Access_Action (Typ,
          Action => Inherit_DT,
          Args   => New_List (
            Node1 => Old_Tag,
            Node2 => New_Reference_To (DT_Ptr, Loc),
            Node3 => Make_Integer_Literal (Loc,
                       DT_Entry_Count (Tag_Component (Etype (Typ)))))));

      --  Inherit_TSD (Get_TSD (parent), DT_Ptr);

      Append_To (Elab_Code,
        Make_DT_Access_Action (Typ,
          Action => Inherit_TSD,
          Args   => New_List (
            Node1 => Old_TSD,
            Node2 => New_Reference_To (DT_Ptr, Loc))));

      --  Exname : constant String := full_qualified_name (typ);

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Exname,
          Constant_Present    => True,
          Object_Definition   => New_Reference_To (Standard_String, Loc),
          Expression => Make_String_Literal (Loc, Full_Qualified_Name (Typ))));

      --  Set_Expanded_Name (DT_Ptr, exname'Address);

      Append_To (Elab_Code,
        Make_DT_Access_Action (Typ,
          Action => Set_Expanded_Name,
          Args   => New_List (
            Node1 => New_Reference_To (DT_Ptr, Loc),
            Node2 =>
              Make_Attribute_Reference (Loc,
                Prefix => New_Reference_To (Exname, Loc),
                Attribute_Name => Name_Address))));

      --  Set_External_Tag (DT_Ptr, exname'Address);
      --  ??? should be the external name not the qualified name

      if not Has_External_Tag_Rep_Clause (Typ) then
         Append_To (Elab_Code,
           Make_DT_Access_Action (Typ,
             Action => Set_External_Tag,
             Args   => New_List (
               Node1 => New_Reference_To (DT_Ptr, Loc),
               Node2 =>
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Reference_To (Exname, Loc),
                   Attribute_Name => Name_Address))));

      --  Register the Tag in the external_tag Htable for pure Ada type only
      --        Register_Tag (Dt_Ptr);

         if Is_RTE (Generalized_Tag, RE_Tag) then
            Append_To (Elab_Code,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Register_Tag), Loc),
                Parameter_Associations =>
                  New_List (New_Reference_To (DT_Ptr, Loc))));
         end if;
      end if;

      --     if No_Reg then
      --        <elab_code>
      --        No_Reg := False;
      --     end if;

      Append_To (Elab_Code,
        Make_Assignment_Statement (Loc,
          Name       => New_Reference_To (No_Reg, Loc),
          Expression => New_Reference_To (Standard_False, Loc)));

      Append_To (Result,
        Make_If_Statement (Loc,
          Condition       => New_Reference_To (No_Reg, Loc),
          Then_Statements => Elab_Code));

      return Result;
   end Make_DT;

   -------------
   -- Fill_DT --
   -------------

   function Fill_DT_Entry
     (Loc  : Source_Ptr;
      Prim : Entity_Id)
      return Node_Id
   is
      Typ    : constant Entity_Id := Scope (DTC_Entity (Prim));
      DT_Ptr : constant Entity_Id := Access_Disp_Table (Typ);

   begin
      return
        Make_DT_Access_Action (Typ,
          Action => Set_Prim_Op_Address,
          Args   => New_List (
            New_Reference_To (DT_Ptr, Loc),                     -- DTptr

            Make_Integer_Literal (Loc, DT_Position (Prim)),     -- Position

            Make_Attribute_Reference (Loc,                      -- Value
              Prefix          => New_Reference_To (Prim, Loc),
              Attribute_Name  => Name_Address)));
   end Fill_DT_Entry;

   --------------------------
   -- Expand_Dispatch_Call --
   --------------------------

   procedure Expand_Dispatch_Call (Call_Node : Node_Id) is
      Call_Typ       : constant Entity_Id := Etype (Call_Node);
      Ctrl_Arg       : constant Node_Id := Controlling_Argument (Call_Node);
      Loc            : constant Source_Ptr := Sloc (Call_Node);
      Param_List     : constant List_Id := Parameter_Associations (Call_Node);
      Subp           : constant Entity_Id  := Entity (Name (Call_Node));

      CW_Typ         : Entity_Id;
      New_Call       : Node_Id;
      New_Call_Name  : Node_Id;
      New_Params     : List_Id := No_List;
      Param          : Node_Id;
      Res_Typ        : Entity_Id;
      Subp_Ptr_Typ   : Entity_Id;
      Subp_Typ       : Entity_Id;
      Typ            : Entity_Id;
      Eq_Prim_Op     : Entity_Id := Empty;

      function New_Value (From : Node_Id) return Node_Id;
      --  From is the original Expression. New_Value is equivalent to
      --  Duplicate_Subexpr with an explicit dereference when From is an
      --  access parameter

      function New_Value (From : Node_Id) return Node_Id is
         Res : constant Node_Id := Duplicate_Subexpr (From);

      begin
         if Is_Access_Type (Etype (From)) then
            return Make_Explicit_Dereference (Sloc (From), Res);
         else
            return Res;
         end if;
      end New_Value;

   begin
      --  Expand_Dispatch is called directly from the semantics, so we need
      --  a check to see whether expansion is active before proceeding

      if not Expander_Active then
         return;
      end if;

      --  Definition of the ClassWide Type and the Tagged type

      if Is_Access_Type (Etype (Ctrl_Arg)) then
         CW_Typ := Designated_Type (Etype (Ctrl_Arg));
      else
         CW_Typ := Etype (Ctrl_Arg);
      end if;

      Typ := Root_Type (CW_Typ);

      if not Is_Limited_Type (Typ) then
         Eq_Prim_Op := Find_Prim_Op (Typ, Name_Op_Eq);
      end if;

      if Is_CPP_Class (Root_Type (Typ)) then

         --  Create a new parameter list with the displaced 'this'

         New_Params := New_List;
         Param := First_Actual (Call_Node);
         while Present (Param) loop

            --  We assume that dispatching through the main dispatch table
            --  (referenced by Tag_Component) doesn't require a displacement
            --  so the expansion below is only done when dispatching on
            --  another vtable pointer, in which case the first argument
            --  is expanded into :

            --     typ!(Displaced_This (Address!(Param)))

            if Param = Ctrl_Arg
              and then DTC_Entity (Subp) /= Tag_Component (Typ)
            then
               Append_To (New_Params,

                 Unchecked_Convert_To (Etype (Param),
                   Make_Function_Call (Loc,
                     Name => New_Reference_To (RTE (RE_Displaced_This), Loc),
                     Parameter_Associations => New_List (

                     --  Current_This

                       Make_Unchecked_Type_Conversion (Loc,
                         Subtype_Mark =>
                           New_Reference_To (RTE (RE_Address), Loc),
                         Expression   => Relocate_Node (Param)),

                     --  Vptr

                       Make_Selected_Component (Loc,
                          Prefix => Duplicate_Subexpr (Ctrl_Arg),
                          Selector_Name =>
                            New_Reference_To (DTC_Entity (Subp), Loc)),

                     --  Position

                       Make_Integer_Literal (Loc, DT_Position (Subp))))));

            else
               Append_To (New_Params, Relocate_Node (Param));
            end if;

            Param := Next_Actual (Param);
         end loop;

      elsif Present (Param_List) then

         --  Generate the Tag checks when appropriate

         New_Params := New_List;

         Param := First_Actual (Call_Node);
         while Present (Param) loop

            --  No tag check with itself

            if Param = Ctrl_Arg then
               Append_To (New_Params, Duplicate_Subexpr (Param));

            --  No tag check for parameter whose type is neither tagged nor
            --  access to tagged (for access parameters)

            elsif No (Find_Controlling_Arg (Param)) then
               Append_To (New_Params, Relocate_Node (Param));

            --  No tag check for function dispatching on result it the
            --  Tag given by the context is this one

            elsif Find_Controlling_Arg (Param) = Ctrl_Arg then
               Append_To (New_Params, Relocate_Node (Param));

            --  "=" is the only dispatching operation allowed to get
            --  operands with incompatible tags (it just returns false)

            elsif Subp = Eq_Prim_Op then
               Append_To (New_Params, Relocate_Node (Param));

            --  No check in presence of suppress flags

            elsif Tag_Checks_Suppressed (Etype (Param))
              or else (Is_Access_Type (Etype (Param))
                         and then Tag_Checks_Suppressed
                                    (Designated_Type (Etype (Param))))
            then
               Append_To (New_Params, Relocate_Node (Param));

            --  Optimization: no tag checks if the parameters are identical

            elsif Is_Entity_Name (Param)
              and then Is_Entity_Name (Ctrl_Arg)
              and then Entity (Param) = Entity (Ctrl_Arg)
            then
               Append_To (New_Params, Relocate_Node (Param));

            --  Now we need to generate the Tag check

            else
               --  Generate code for tag equality check
               --  Perhaps should have Checks.Apply_Tag_Equality_Check???

               Insert_Action (Ctrl_Arg,
                 Make_If_Statement (Loc,
                   Condition =>
                     Make_Op_Ne (Loc,
                       Left_Opnd =>
                         Make_Selected_Component (Loc,
                           Prefix => New_Value (Ctrl_Arg),
                           Selector_Name =>
                             New_Reference_To (Tag_Component (Typ), Loc)),

                       Right_Opnd =>
                         Make_Selected_Component (Loc,
                           Prefix =>
                             Unchecked_Convert_To (Typ, New_Value (Param)),
                           Selector_Name =>
                             New_Reference_To (Tag_Component (Typ), Loc))),

                   Then_Statements =>
                     New_List (New_Constraint_Error (Loc))));

               Append_To (New_Params, Relocate_Node (Param));
            end if;

            Param := Next_Actual (Param);
         end loop;
      end if;

      --  Generate the appropriate subprogram pointer type

      if  Etype (Subp) = Typ then
         Res_Typ := CW_Typ;
      else
         Res_Typ :=  Etype (Subp);
      end if;

      Subp_Typ := Create_Itype (E_Subprogram_Type, Call_Node);
      Subp_Ptr_Typ := Create_Itype (E_Access_Subprogram_Type, Call_Node);
      Set_Etype (Subp_Typ, Res_Typ);
      Set_Esize (Subp_Ptr_Typ, Uint_0);
      Set_Returns_By_Ref (Subp_Typ, Returns_By_Ref (Subp));

      --  Create a new list of parameters which is a copy of the old formal
      --  list including the creation of a new set of matching entities.

      declare
         Old_Formal : Entity_Id := First_Formal (Subp);
         New_Formal : Entity_Id;
         Extra      : Entity_Id;

      begin
         if Present (Old_Formal) then
            New_Formal := New_Copy (Old_Formal);
            Set_First_Entity (Subp_Typ, New_Formal);
            Param := First_Actual (Call_Node);

            loop
               --  Change all the controlling argument types to be class-wide
               --  to avoid a recursion in dispatching

               if Is_Controlling_Actual (Param) then
                  Set_Etype (New_Formal, Etype (Param));
               end if;

               Extra := New_Formal;
               Old_Formal := Next_Formal (Old_Formal);
               exit when No (Old_Formal);

               Set_Next_Entity (New_Formal, New_Copy (Old_Formal));
               New_Formal := Next_Entity (New_Formal);
               Param := Next_Actual (Param);
            end loop;
            Set_Last_Entity (Subp_Typ, Extra);

            --  Copy extra formals

            New_Formal := First_Entity (Subp_Typ);
            while Present (New_Formal) loop
               if Present (Extra_Formal_Constrained (New_Formal)) then
                  Set_Extra_Formal (Extra,
                    New_Copy (Extra_Formal_Constrained (New_Formal)));
                  Extra := Extra_Formal (Extra);
                  Set_Extra_Formal_Constrained (New_Formal, Extra);

               elsif Present (Extra_Formal_Accessibility (New_Formal)) then
                  Set_Extra_Formal (Extra,
                    New_Copy (Extra_Formal_Accessibility (New_Formal)));
                  Extra := Extra_Formal (Extra);
                  Set_Extra_Formal_Accessibility (New_Formal, Extra);
               end if;

               New_Formal := Next_Formal (New_Formal);
            end loop;
         end if;
      end;

      Set_Etype (Subp_Ptr_Typ, Subp_Ptr_Typ);
      Set_Directly_Designated_Type (Subp_Ptr_Typ, Subp_Typ);

      --  Generate:
      --   Subp_Ptr_Typ!(Get_Prim_Op_Address (Ctrl._Tag, pos));

      New_Call_Name :=
        Unchecked_Convert_To (Subp_Ptr_Typ,
          Make_DT_Access_Action (Typ,
            Action => Get_Prim_Op_Address,
            Args => New_List (

            --  Vptr

              Make_Selected_Component (Loc,
                Prefix => Duplicate_Subexpr (Ctrl_Arg),
                Selector_Name => New_Reference_To (DTC_Entity (Subp), Loc)),

            --  Position

              Make_Integer_Literal (Loc, DT_Position (Subp)))));

      if Nkind (Call_Node) = N_Function_Call then
         New_Call :=
           Make_Function_Call (Loc,
             Name => New_Call_Name,
             Parameter_Associations => New_Params);

         --  if this is a dispatching "=", we must first compare the tags so
         --  we generate: x.tag = y.tag and then x = y

         if Subp = Eq_Prim_Op then

            Param := First_Actual (Call_Node);
            New_Call :=
              Make_And_Then (Loc,
                Left_Opnd =>
                     Make_Op_Eq (Loc,
                       Left_Opnd =>
                         Make_Selected_Component (Loc,
                           Prefix => New_Value (Param),
                           Selector_Name =>
                             New_Reference_To (Tag_Component (Typ), Loc)),

                       Right_Opnd =>
                         Make_Selected_Component (Loc,
                           Prefix =>
                             Unchecked_Convert_To (Typ,
                               New_Value (Next_Actual (Param))),
                           Selector_Name =>
                             New_Reference_To (Tag_Component (Typ), Loc))),

                Right_Opnd => New_Call);
         end if;

      else
         New_Call :=
           Make_Procedure_Call_Statement (Loc,
             Name => New_Call_Name,
             Parameter_Associations => New_Params);
      end if;

      Rewrite_Substitute_Tree (Call_Node, New_Call);
      Analyze_And_Resolve (Call_Node, Call_Typ);
   end Expand_Dispatch_Call;

   -----------------------------
   -- Set_Default_Constructor --
   -----------------------------

   procedure Set_Default_Constructor (Typ : Entity_Id) is
      Loc   : Source_Ptr;
      Init  : Entity_Id;
      Param : Entity_Id;
      Decl  : Node_Id;
      E     : Entity_Id;

   begin
      --  Look for the default constructor entity. For now only the
      --  default constructor has the flag Is_Constructor.

      E := Next_Entity (Typ);
      while Present (E)
        and then (Ekind (E) /= E_Function or else not Is_Constructor (E))
      loop
         E := Next_Entity (E);
      end loop;

      --  Create the init procedure

      if Present (E) then
         Loc   := Sloc (E);
         Init  := Make_Defining_Identifier (Loc, Name_uInit_Proc);
         Param := Make_Defining_Identifier (Loc, Name_X);
         Decl  :=
           Make_Subprogram_Declaration (Loc,
             Make_Procedure_Specification (Loc,
               Defining_Unit_Name => Init,
               Parameter_Specifications => New_List (
                 Make_Parameter_Specification (Loc,
                   Defining_Identifier => Param,
                   Parameter_Type      =>
                     Make_Access_Definition (Loc,
                      Subtype_Mark => New_Reference_To (Typ, Loc))))));

         Set_Init_Proc (Typ, Init);
         Set_Is_Imported (Init);
         Set_Interface_Name (Init, Interface_Name (E));
         Set_Convention (Init, Convention_C);
         Set_Is_Public (Init);
         Set_Has_Completion (Init);
      end if;
   end Set_Default_Constructor;

end Exp_Disp;

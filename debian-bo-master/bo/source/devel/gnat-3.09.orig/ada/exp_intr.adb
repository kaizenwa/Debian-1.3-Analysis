------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ I N T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.40 $                             --
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
with Einfo;    use Einfo;
with Exp_Ch4;  use Exp_Ch4;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Code; use Exp_Code;
with Exp_Fixd; use Exp_Fixd;
with Exp_Util; use Exp_Util;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Exp_Intr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Is_Negative (N : Node_Id);
   --  Expand a call to the intrinsic Is_Negative function

   procedure Expand_Exception_Call (N : Node_Id; Ent : RE_Id);
   --  Expand a call to Exception_Information/Message/Name. The first
   --  parameter, N, is the node for the function call, and Ent is the
   --  entity for the corresponding routine in the Ada.Exceptions package.

   procedure Expand_Import_Call (N : Node_Id);
   --  Expand a call to Import_Address/Longest_Integer/Value. The parameter
   --  N is the node for the function call.

   procedure Expand_Shift (N : Node_Id; E : Entity_Id; K : Node_Kind);
   --  Expand an intrinsic shift operation, N and E are from the call to
   --  Expand_Instrinsic_Call (call node and subprogram spec entity) and
   --  K is the kind for the shift node

   procedure Expand_Unc_Conversion (N : Node_Id; E : Entity_Id);
   --  Expand a call to an instantiation of Unchecked_Convertion into a node
   --  N_Unchecked_Type_Conversion.

   procedure Expand_Unc_Deallocation (N : Node_Id; E : Entity_Id);
   --  Expand a call to an instantiation of Unchecked_Deallocation into a node
   --  N_Free_Statement and appropriate context.

   ---------------------------
   -- Expand_Exception_Call --
   ---------------------------

   --  If the function call is not within an exception handler, then the
   --  call is replaced by a null string. Otherwise the appropriate routine
   --  in Ada.Exceptions is called passing the choice parameter specification
   --  from the enclosing handler. If the enclosing handler lacks a choice
   --  parameter, then one is supplied.

   procedure Expand_Exception_Call (N : Node_Id; Ent : RE_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      P   : Node_Id;
      E   : Entity_Id;
      S   : String_Id;

   begin
      --  Climb up parents to see if we are in exception handler

      P := Parent (N);
      loop
         --  Case of not in exception handler

         if No (P) then
            Start_String;
            S := End_String;
            Rewrite_Substitute_Tree (N,
              Make_String_Literal (Loc,
                Strval => S));
            exit;

         --  Case of in exception handler

         elsif Nkind (P) = N_Exception_Handler then
            if No (Choice_Parameter (P)) then

               --  If no choice parameter present, then put one there. Note
               --  that we do not need to put it on the entity chain, since
               --  no one will be referencing it by normal visibility methods.

               E := Make_Defining_Identifier (Loc, New_Internal_Name ('E'));
               Set_Choice_Parameter (P, E);
               Set_Ekind (E, E_Variable);
               Set_Etype (E, RTE (RE_Exception_Occurrence));
            end if;

            Rewrite_Substitute_Tree (N,
              Make_Function_Call (Loc,
                Name => New_Occurrence_Of (RTE (Ent), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Choice_Parameter (P), Loc))));
            exit;

         --  Keep climbing!

         else
            P := Parent (P);
         end if;
      end loop;

      Analyze_And_Resolve (N, Standard_String);
   end Expand_Exception_Call;

   ------------------------
   -- Expand_Import_Call --
   ------------------------

   --  The function call must have a static string as its argument. We create
   --  a dummy variable which uses this string as the external name in an
   --  Import pragma. The result is then obtained as the address of this
   --  dummy variable, converted to the appropriate target type.

   procedure Expand_Import_Call (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Ent : constant Entity_Id  := Entity (Name (N));
      Str : constant Node_Id    := First_Actual (N);
      Dum : Entity_Id;

   begin
      Dum := Make_Defining_Identifier (Loc, New_Internal_Name ('D'));

      Insert_Actions (N, New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier => Dum,
          Object_Definition   =>
            New_Occurrence_Of (Standard_Character, Loc)),

        Make_Pragma (Loc,
          Chars => Name_Import,
          Pragma_Argument_Associations => New_List (
            Make_Pragma_Argument_Association (Loc,
              Expression => Make_Identifier (Loc, Name_Ada)),

            Make_Pragma_Argument_Association (Loc,
              Expression => Make_Identifier (Loc, Chars (Dum))),

            Make_Pragma_Argument_Association (Loc,
              Chars => Name_Link_Name,
              Expression => Relocate_Node (Str))))));

      Rewrite_Substitute_Tree (N,
        Unchecked_Convert_To (Etype (Ent),
          Make_Attribute_Reference (Loc,
            Attribute_Name => Name_Address,
            Prefix => Make_Identifier (Loc, Chars (Dum)))));

      Analyze_And_Resolve (N, Etype (Ent));
   end Expand_Import_Call;

   ---------------------------
   -- Expand_Intrinsic_Call --
   ---------------------------

   procedure Expand_Intrinsic_Call (N : Node_Id; E : Entity_Id) is
      Nam : Name_Id;

   begin
      --  If the intrinsic subprogram is generic, gets its original name.

      if Present (Parent (E))
        and then Present (Generic_Parent (Parent (E)))
      then
         Nam := Chars (Generic_Parent (Parent (E)));
      else
         Nam := Chars (E);
      end if;

      if Nam = Name_Asm then
         Expand_Asm_Call (N);

      elsif Nam = Name_Divide then
         Expand_Decimal_Divide_Call (N);

      elsif Nam = Name_Exception_Information then
         Expand_Exception_Call (N, RE_Exception_Information);

      elsif Nam = Name_Exception_Message then
         Expand_Exception_Call (N, RE_Exception_Message);

      elsif Nam = Name_Exception_Name then
         Expand_Exception_Call (N, RE_Exception_Name_Simple);

      elsif Nam = Name_Import_Address
              or else
            Nam = Name_Import_Largest_Value
              or else
            Nam = Name_Import_Value
      then
         Expand_Import_Call (N);

      elsif Nam = Name_Is_Negative then
         Expand_Is_Negative (N);

      elsif Nam = Name_Rotate_Left then
         Expand_Shift (N, E, N_Op_Rotate_Left);

      elsif Nam = Name_Rotate_Right then
         Expand_Shift (N, E, N_Op_Rotate_Right);

      elsif Nam = Name_Shift_Left then
         Expand_Shift (N, E, N_Op_Shift_Left);

      elsif Nam = Name_Shift_Right then
         Expand_Shift (N, E, N_Op_Shift_Right);

      elsif Nam = Name_Shift_Right_Arithmetic then
         Expand_Shift (N, E, N_Op_Shift_Right_Arithmetic);

      elsif Nam = Name_Unchecked_Conversion then
         Expand_Unc_Conversion (N, E);

      elsif Nam = Name_Unchecked_Deallocation then
         Expand_Unc_Deallocation (N, E);

      else
         --  Only other possibility is a renaming, in which case we expand
         --  the call to the original operation (which must be intrinsic).

         pragma Assert (Present (Alias (E)));
         Expand_Intrinsic_Call (N,  Alias (E));
      end if;

   end Expand_Intrinsic_Call;

   ------------------------
   -- Expand_Is_Negative --
   ------------------------

   procedure Expand_Is_Negative (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Opnd  : constant Node_Id    := Relocate_Node (First_Actual (N));

   begin

      --  We replace the function call by the following expression

      --    if Opnd < 0.0 then
      --       True
      --    else
      --       if Opnd > 0.0 then
      --          False;
      --       else
      --          Float_Unsigned!(Float (Opnd)) /= 0
      --       end if;
      --    end if;

      Rewrite_Substitute_Tree (N,
        Make_Conditional_Expression (Loc,
          Expressions => New_List (
            Make_Op_Lt (Loc,
              Left_Opnd  => Duplicate_Subexpr (Opnd),
              Right_Opnd => Make_Real_Literal (Loc, Ureal_0)),

            New_Occurrence_Of (Standard_True, Loc),

            Make_Conditional_Expression (Loc,
             Expressions => New_List (
               Make_Op_Gt (Loc,
                 Left_Opnd  => Duplicate_Subexpr (Opnd),
                 Right_Opnd => Make_Real_Literal (Loc, Ureal_0)),

               New_Occurrence_Of (Standard_False, Loc),

                Make_Op_Ne (Loc,
                  Left_Opnd =>
                    Unchecked_Convert_To (RTE (RE_Float_Unsigned),
                                          Convert_To (Standard_Float,
                                            Duplicate_Subexpr (Opnd))),
                  Right_Opnd =>
                    Make_Integer_Literal (Loc, Uint_0)))))));

      Analyze_And_Resolve (N, Standard_Boolean);
   end Expand_Is_Negative;

   ------------------
   -- Expand_Shift --
   ------------------

   --  This procedure is used to convert a call to a shift function to the
   --  corresponding operator node. This conversion is not done by the usual
   --  circuit for converting calls to operator functions (e.g. "+"(1,2)) to
   --  operator nodes, because shifts are not predefined operators.

   --  As a result, whenever a shift is used in the source program, it will
   --  remain as a call until converted by this routine to the operator node
   --  form which Gigi is expecting to see.

   --  Note: it is possible for the expander to generate shift operator nodes
   --  directly, which will be analyzed in the normal manner by calling Analyze
   --  and Resolve. Such shift operator nodes will not be seen by Expand_Shift.

   procedure Expand_Shift (N : Node_Id; E : Entity_Id; K : Node_Kind) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      Left  : constant Node_Id    := First_Actual (N);
      Right : constant Node_Id    := Next_Actual (Left);
      Ltyp  : constant Node_Id    := Etype (Left);
      Rtyp  : constant Node_Id    := Etype (Right);
      Snode : Node_Id;

   begin
      Snode := New_Node (K, Loc);
      Set_Left_Opnd  (Snode, Relocate_Node (Left));
      Set_Right_Opnd (Snode, Relocate_Node (Right));
      Set_Chars      (Snode, Chars (E));
      Set_Etype      (Snode, Base_Type (Typ));
      Set_Entity     (Snode, E);

      if Compile_Time_Known_Value (Type_High_Bound (Rtyp))
        and then Expr_Value (Type_High_Bound (Rtyp)) < Esize (Ltyp)
      then
         Set_Shift_Count_OK (Snode, True);
      end if;

      --  Do the rewrite. Note that we don't call Analyze and Resolve on
      --  this node, because it already got analyzed and resolved when
      --  it was a function call!

      Rewrite_Substitute_Tree (N, Snode);
      Set_Analyzed (N);

   end Expand_Shift;

   ---------------------------
   -- Expand_Unc_Conversion --
   ---------------------------

   procedure Expand_Unc_Conversion (N : Node_Id; E : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      Rewrite_Substitute_Tree (N,
        Unchecked_Convert_To (Etype (E),
          Relocate_Node (First_Actual (N))));

      Set_Etype (N, Etype (E));
      Set_Analyzed (N);
      if Nkind (N) = N_Unchecked_Type_Conversion then
         Expand_N_Unchecked_Type_Conversion (N);
      end if;
   end Expand_Unc_Conversion;

   -----------------------------
   -- Expand_Unc_Deallocation --
   -----------------------------

   --  Generate the following Code :
   --    if Arg /= null then
   --
   --     <Finalize_Call> (.., T'Class(Arg.all), ..);  -- for controlled types
   --       Free (Arg);
   --       Arg := Null;
   --    end if;

   --  For now, we do not do anything in response to an attempt to deallocate
   --  a task. Perhaps we should do more later ???

   procedure Expand_Unc_Deallocation (N : Node_Id; E : Entity_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Arg       : constant Node_Id    := First_Actual (N);
      Typ       : constant Entity_Id  := Etype (Arg);
      Stmts     : constant List_Id    := New_List;
      Desig_T   : Entity_Id  := Designated_Type (Typ);
      Free_Node : Node_Id;
      Deref     : Node_Id;

   begin
      if Controlled_Type (Desig_T) then

         Deref := Make_Explicit_Dereference (Loc, Duplicate_Subexpr (Arg));

         --  If the type is tagged, then we must force dispatching on the
         --  finalization call because the designated type may not be the
         --  actual type of the object

         if Is_Tagged_Type (Desig_T)
           and then not Is_Class_Wide_Type (Desig_T)
         then
            Deref := Unchecked_Convert_To (Class_Wide_Type (Desig_T), Deref);
         end if;

         Append_List_To (Stmts,
           Make_Final_Call (
             Ref         => Deref,
             Typ         => Desig_T,
             With_Detach => New_Reference_To (Standard_True, Loc)));
      end if;

      --  For a task type, replace the free call with an assignment of null

      if Is_Task_Type (Desig_T) then
         Rewrite_Substitute_Tree (N,
           Make_Assignment_Statement (Loc,
             Name       => Relocate_Node (Arg),
             Expression => Make_Null (Loc)));
         Analyze (N);
         return;
      end if;

      --  Normal processing for non-task types

      Free_Node :=
        Make_Free_Statement (Loc, Expression => Duplicate_Subexpr (Arg));

      Set_Storage_Pool (Free_Node,
        Associated_Storage_Pool (Underlying_Type (Root_Type (Etype (Arg)))));

      if Present (Storage_Pool (Free_Node)) then

         --  Freeing the secondary stack is meaningless

         if Is_RTE (Storage_Pool (Free_Node), RE_SS_Pool) then
            null;

         else
            Set_Procedure_To_Call (Free_Node, Find_Prim_Op (
              Etype (Storage_Pool (Free_Node)), Name_Deallocate));
         end if;
      end if;

      Append_To (Stmts, Free_Node);

      Append_To (Stmts,
        Make_Assignment_Statement (Loc,
          Name       => Duplicate_Subexpr (Arg),
          Expression => Make_Null (Loc)));

      Rewrite_Substitute_Tree (N,
        Make_If_Statement (Loc,
          Condition =>
            Make_Op_Ne (Loc,
              Left_Opnd  => Duplicate_Subexpr (Arg),
              Right_Opnd => Make_Null (Loc)),

          Then_Statements => Stmts));

      Analyze (N);
   end Expand_Unc_Deallocation;

end Exp_Intr;

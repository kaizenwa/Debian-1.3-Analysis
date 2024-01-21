------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ P R A G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.33 $                             --
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
with Exp_TSS;  use Exp_TSS;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Stand;    use Stand;
with Tbuild;   use Tbuild;

package body Exp_Prag is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Arg1 (N : Node_Id) return Node_Id;
   function Arg2 (N : Node_Id) return Node_Id;
   function Arg3 (N : Node_Id) return Node_Id;
   --  Obtain specified Pragma_Argument_Association

   procedure Expand_Pragma_Abort_Defer         (N : Node_Id);
   procedure Expand_Pragma_Assert              (N : Node_Id);
   procedure Expand_Pragma_Import              (N : Node_Id);
   procedure Expand_Pragma_Inspection_Point    (N : Node_Id);
   procedure Expand_Pragma_Interrupt_Priority  (N : Node_Id);

   --------------
   -- Arg1,2,3 --
   --------------

   function Arg1 (N : Node_Id) return Node_Id is
   begin
      return First (Pragma_Argument_Associations (N));
   end Arg1;

   function Arg2 (N : Node_Id) return Node_Id is
   begin
      return Next (Arg1 (N));
   end Arg2;

   function Arg3 (N : Node_Id) return Node_Id is
   begin
      return Next (Arg2 (N));
   end Arg3;

   ---------------------
   -- Expand_N_Pragma --
   ---------------------

   procedure Expand_N_Pragma (N : Node_Id) is
   begin
      case Get_Pragma_Id (Chars (N)) is

         --  Pragmas requiring special expander action

         when Pragma_Abort_Defer =>
            Expand_Pragma_Abort_Defer (N);

         when Pragma_Assert =>
            Expand_Pragma_Assert (N);

         when Pragma_Inspection_Point =>
            Expand_Pragma_Inspection_Point (N);

         when Pragma_Interrupt_Priority =>
            Expand_Pragma_Interrupt_Priority (N);

         when Pragma_Import =>
            Expand_Pragma_Import (N);

         --  All other pragmas need no expander action

         when others => null;
      end case;

   end Expand_N_Pragma;

   -------------------------------
   -- Expand_Pragma_Abort_Defer --
   -------------------------------

   --  An Abort_Defer pragma appears as the first statement in a handled
   --  statement sequence (right after the begin). It defers aborts for
   --  the entire statement sequence, but not for any declarations or
   --  handlers (if any) associated with this statement sequence.

   --  The transformation is to transform

   --    pragma Abort_Defer;
   --    statements;

   --  into

   --    begin
   --       Abort_Defer;
   --       statements
   --    at end
   --       Abort_Undefer;
   --    end;

   procedure Expand_Pragma_Abort_Defer (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Stm  : Node_Id;
      Stms : List_Id;

   begin
      Stms := New_List (Build_Runtime_Call (Loc, RE_Abort_Defer));

      loop
         Stm := Remove_Next (N);
         exit when No (Stm);
         Append (Stm, Stms);
      end loop;

      Rewrite_Substitute_Tree (N,
        Make_Block_Statement (Loc,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stms,
              Identifier =>
                New_Occurrence_Of (RTE (RE_Abort_Undefer_Direct), Loc))));

      Analyze (N);
   end Expand_Pragma_Abort_Defer;

   --------------------------
   -- Expand_Pragma_Assert --
   --------------------------

   procedure Expand_Pragma_Assert (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      --  If we are not in debug mode then rewrite the pragma with
      --  a null statement and do not even analyze the pragma.

      if not Assertions_Enabled then
         Rewrite_Substitute_Tree (N, Make_Null_Statement (Loc));

      --  If we are in debug mode, then rewrite the pragma with its
      --  corresponding if statement, and then analyze the statement
      --  The expansion transforms:

      --    pragma Assert (condition [,message]);

      --  into

      --    if not condition then
      --       System.Assertions.Raise_Assert_Failure (Str);
      --    end if;

      --  where Str is the message if one is present, or the default of
      --  file:line if no message is given.

      else
         Assert : declare
            Msg   : String_Id;

            procedure Store_String_Int (N : Logical_Line_Number);
            --  Store characters of decimal representation of N in string
            --  currently being constructed by Stringt.Store_String_Char.

            procedure Store_String_Int (N : Logical_Line_Number) is
            begin
               if N > 9 then
                  Store_String_Int (N / 10);
               end if;

               Store_String_Char
                 (Get_Char_Code
                   (Character'Val (N mod 10 + Character'Pos ('0'))));
            end Store_String_Int;

         --  Start of processing for Assert

         begin
            --  First, we need to prepare the character literal

            if Present (Arg2 (N)) then
               Msg := Strval (Expr_Value_S (Expression (Arg2 (N))));

            else
               Start_String;
               Get_Name_String
                 (Reference_Name (Source_Index (Get_Sloc_Unit_Number (Loc))));

               for J in 1 .. Name_Len loop
                  Store_String_Char (Get_Char_Code (Name_Buffer (J)));
               end loop;

               Store_String_Char (Get_Char_Code (':'));
               Store_String_Int (Get_Line_Number (Loc));
               Msg := End_String;
            end if;


            --  Now generate the if statement

            Rewrite_Substitute_Tree (N,
              Make_If_Statement (Loc,
                Condition =>
                  Make_Op_Not (Loc,
                    Right_Opnd => Expression (Arg1 (N))),
                Then_Statements => New_List (
                  Make_Procedure_Call_Statement (Loc,
                    Name =>
                      New_Reference_To (RTE (RE_Raise_Assert_Failure), Loc),
                    Parameter_Associations => New_List (
                      Make_String_Literal (Loc, Msg))))));

            Analyze (N);
         end Assert;
      end if;

   end Expand_Pragma_Assert;

   --------------------------
   -- Expand_Pragma_Import --
   --------------------------

   --  When applied to a variable, the default initialization must not be
   --  done. As it is already done when the pragma is found, we just get rid
   --  of the call the initialization procedure which followed the object
   --  declaration.

   --  We can't use the freezing mechanism for this purpose, since we
   --  have to elaborate the initialization expression when it is first
   --  seen (i.e. this elaboration cannot be deferred to the freeze point).

   procedure Expand_Pragma_Import (N : Node_Id) is
      Def_Id    : constant Entity_Id := Entity (Expression (Arg2 (N)));
      Init      : Entity_Id;
      After_Def : Node_Id;

   begin
      if Ekind (Def_Id) = E_Variable then
         Init := Base_Init_Proc (Etype (Def_Id));
         After_Def := Next (Parent (Def_Id));

         if Present (Init)
           and then Nkind (After_Def) = N_Procedure_Call_Statement
           and then Is_Entity_Name (Name (After_Def))
           and then Entity (Name (After_Def)) = Init
         then
            Remove (After_Def);

         elsif Is_Access_Type (Etype (Def_Id)) then
            Set_Expression (Parent (Def_Id), Empty);
         end if;
      end if;
   end Expand_Pragma_Import;

   ------------------------------------
   -- Expand_Pragma_Inspection_Point --
   ------------------------------------

   --  If no argument is given, then we supply a default argument list that
   --  includes all objects declared at the source level in all subprograms
   --  that enclose the inspection point pragma.

   procedure Expand_Pragma_Inspection_Point (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      A   : List_Id;
      S   : Entity_Id;
      E   : Entity_Id;

   begin
      if No (Pragma_Argument_Associations (N)) then
         A := New_List;
         S := Current_Scope;

         while S /= Standard_Standard loop
            E := First_Entity (S);
            while Present (E) loop
               if Comes_From_Source (E)
                 and then Is_Object (E)
                 and then not Is_Entry_Formal (E)
                 and then Ekind (E) /= E_Component
                 and then Ekind (E) /= E_Discriminant
                 and then Ekind (E) /= E_Generic_In_Parameter
                 and then Ekind (E) /= E_Generic_In_Out_Parameter
               then
                  Append_To (A,
                    Make_Pragma_Argument_Association (Loc,
                      Expression => New_Occurrence_Of (E, Loc)));
               end if;

               E := Next_Entity (E);
            end loop;

            S := Scope (S);
         end loop;

         Set_Pragma_Argument_Associations (N, A);
      end if;
   end Expand_Pragma_Inspection_Point;

   --------------------------------------
   -- Expand_Pragma_Interrupt_Priority --
   --------------------------------------

   --  Supply default argument if none exists (System.Interrupt_Priority'Last)

   procedure Expand_Pragma_Interrupt_Priority (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      if No (Pragma_Argument_Associations (N)) then
         Set_Pragma_Argument_Associations (N, New_List (
           Make_Pragma_Argument_Association (Loc,
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (RTE (RE_Interrupt_Priority), Loc),
                 Attribute_Name => Name_Last))));
      end if;
   end Expand_Pragma_Interrupt_Priority;

end Exp_Prag;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C O D E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
--                                                                          --
--             Copyright (C) 1996 Free Software Foundation, Inc.            --
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
with Errout;   use Errout;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;

package body Exp_Code is

   -----------------------
   -- Local_Subprograms --
   -----------------------

   function Asm_Constraint (Operand_Var : Node_Id) return Node_Id;
   --  Common processing for Asm_Input_Constraint and Asm_Output_Constraint.
   --  Obtains the constraint argument from the global operand variable
   --  Operand_Var, which must be non-Empty.

   function Asm_Operand (Operand_Var : Node_Id) return Node_Id;
   --  Common processing for Asm_Input_Value and Asm_Output_Variable. Obtains
   --  the value/variable argument from Operand_Var, the global operand
   --  variable. Returns Empty if no operand available.

   function Get_String_Node (S : Node_Id) return Node_Id;
   --  Given S, a static expression node of type String, returns the
   --  string literal node. This is needed to deal with the use of constants
   --  for these expressions, which is perfectly permissible.

   procedure Next_Asm_Operand (Operand_Var : in out Node_Id);
   --  Common processing for Next_Asm_Input and Next_Asm_Output, updates
   --  the value of the global operand variable Operand_Var appropriately.

   procedure Setup_Asm_IO_Args (Arg : Node_Id; Operand_Var : out Node_Id);
   --  Common processing for Setup_Asm_Inputs and Setup_Asm_Outputs. Arg
   --  is the actual parameter from the call, and Operand_Var is the global
   --  operand variable to be initialized to the first operand.

   ----------------------
   -- Global Variables --
   ----------------------

   Current_Input_Operand : Node_Id := Empty;
   --  Points to current Asm_Input_Operand attribute reference. Initialized
   --  by Setup_Asm_Inputs, updated by Next_Asm_Input, and referenced by
   --  Asm_Input_Constraint and Asm_Input_Value.

   Current_Output_Operand : Node_Id := Empty;
   --  Points to current Asm_Output_Operand attribute reference. Initialized
   --  by Setup_Asm_Outputs, updated by Next_Asm_Output, and referenced by
   --  Asm_Output_Constraint and Asm_Output_Variable.

   --------------------
   -- Asm_Constraint --
   --------------------

   function Asm_Constraint (Operand_Var : Node_Id) return Node_Id is
   begin
      pragma Assert (Present (Operand_Var));
      return Get_String_Node (First (Expressions (Operand_Var)));
   end Asm_Constraint;

   --------------------------
   -- Asm_Input_Constraint --
   --------------------------

   --  Note: error checking on Asm_Input attribute done in Sem_Attr

   function Asm_Input_Constraint return Node_Id is
   begin
      return Get_String_Node (Asm_Constraint (Current_Input_Operand));
   end Asm_Input_Constraint;

   ---------------------
   -- Asm_Input_Value --
   ---------------------

   --  Note: error checking on Asm_Input attribute done in Sem_Attr

   function Asm_Input_Value return Node_Id is
   begin
      return Asm_Operand (Current_Input_Operand);
   end Asm_Input_Value;

   -----------------
   -- Asm_Operand --
   -----------------

   function Asm_Operand (Operand_Var : Node_Id) return Node_Id is
   begin
      if No (Operand_Var) then
         return Empty;
      else
         return Next (First (Expressions (Operand_Var)));
      end if;
   end Asm_Operand;

   ---------------------------
   -- Asm_Output_Constraint --
   ---------------------------

   --  Note: error checking on Asm_Output attribute done in Sem_Attr

   function Asm_Output_Constraint return Node_Id is
   begin
      return Asm_Constraint (Current_Output_Operand);
   end Asm_Output_Constraint;

   -------------------------
   -- Asm_Output_Variable --
   -------------------------

   --  Note: error checking on Asm_Output attribute done in Sem_Attr

   function Asm_Output_Variable return Node_Id is
   begin
      return Asm_Operand (Current_Output_Operand);
   end Asm_Output_Variable;

   ------------------
   -- Asm_Template --
   ------------------

   function Asm_Template (N : Node_Id) return Node_Id is
      Call : constant Node_Id := Expression (Expression (N));
      Temp : constant Node_Id := First_Actual (Call);

   begin
      if not Is_OK_Static_Expression (Temp) then
         Error_Msg_N ("asm template argument is not static", Temp);
         return Empty;
      else
         return Get_String_Node (Temp);
      end if;
   end Asm_Template;

   ----------------------
   -- Clobber_Get_Next --
   ----------------------

   Clobber_Node : Node_Id;
   --  String literal node for clobber string. Initialized by Clobber_Setup,
   --  and not modified by Clobber_Get_Next. Empty if clobber string was in
   --  error (resulting in no clobber arguments being returned).

   Clobber_Ptr : Nat;
   --  Pointer to current character of string. Initialized to 1 by the call
   --  to Clobber_Setup, and then updated by Clobber_Get_Next.

   function Clobber_Get_Next return Address is
      Str : constant String_Id := Strval (Clobber_Node);
      Len : constant Nat       := String_Length (Str);
      C   : Character;

   begin
      if No (Clobber_Node) then
         return Null_Address;
      end if;

      --  Skip spaces and commas before next register name

      loop
         --  Return null string if no more names

         if Clobber_Ptr > Len then
            return Null_Address;
         end if;

         C := Get_Character (Get_String_Char (Str, Clobber_Ptr));
         exit when C /= ',' and then C /= ' ';
         Clobber_Ptr := Clobber_Ptr + 1;
      end loop;

      --  Acquire next register name

      Name_Len := 0;
      loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := C;
         Clobber_Ptr := Clobber_Ptr + 1;
         exit when Clobber_Ptr > Len;
         C := Get_Character (Get_String_Char (Str, Clobber_Ptr));
         exit when C = ',' or else C = ' ';
      end loop;

      Name_Buffer (Name_Len + 1) := Ascii.NUL;
      return Name_Buffer'Address;

   end Clobber_Get_Next;

   -------------------
   -- Clobber_Setup --
   -------------------

   procedure Clobber_Setup (N : Node_Id) is
      Call : constant Node_Id := Expression (Expression (N));
      Clob : constant Node_Id := Next_Actual (
                                   Next_Actual (
                                     Next_Actual (
                                       First_Actual (Call))));

   begin
      if not Is_OK_Static_Expression (Clob) then
         Error_Msg_N ("asm clobber argument is not static", Clob);
         Clobber_Node := Empty;

      else
         Clobber_Node := Get_String_Node (Clob);
         Clobber_Ptr := 1;
      end if;
   end Clobber_Setup;

   ---------------------
   -- Expand_Asm_Call --
   ---------------------

   procedure Expand_Asm_Call (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      --  If we have the function call case, we are inside a code statement,
      --  and the tree is already in the necessary form for gigi.

      if Nkind (N) = N_Function_Call then
         null;

      --  For the procedure case, we convert the call into a code statement

      else
         pragma Assert (Nkind (N) = N_Procedure_Call_Statement);

         --  Note: strictly we should change the procedure call to a function
         --  call in the qualified expression, but since we are not going to
         --  reanalyze (see below), and the interface subprograms in this
         --  package don't care, we can leave it as a procedure call.

         Rewrite_Substitute_Tree (N,
           Make_Code_Statement (Loc,
             Expression =>
               Make_Qualified_Expression (Loc,
                 Subtype_Mark => New_Occurrence_Of (RTE (RE_Asm_Insn), Loc),
                 Expression => Relocate_Node (N))));

         --  There is no need to reanalyze this node, it is completely analyzed
         --  already, at least sufficiently for the purposes of the abstract
         --  procedural interface defined in this package.

         Set_Analyzed (N);
      end if;
   end Expand_Asm_Call;

   ---------------------
   -- Get_String_Node --
   ---------------------

   function Get_String_Node (S : Node_Id) return Node_Id is
   begin
      if Nkind (S) = N_String_Literal then
         return S;

      else
         pragma Assert (Ekind (Entity (S)) = E_Constant);
         return Get_String_Node (Constant_Value (Entity (S)));
      end if;
   end Get_String_Node;

   ---------------------
   -- Is_Asm_Volatile --
   ---------------------

   function Is_Asm_Volatile (N : Node_Id) return Boolean is
      Call : constant Node_Id := Expression (Expression (N));
      Vol  : constant Node_Id :=
               Next_Actual (
                 Next_Actual (
                   Next_Actual (
                     Next_Actual (
                       First_Actual (Call)))));
   begin
      if not Is_OK_Static_Expression (Vol) then
         Error_Msg_N ("asm volatile argument is not static", Vol);
         return False;

      else
         return Is_True (Expr_Value (Vol));
      end if;
   end Is_Asm_Volatile;

   ----------------------
   -- Next_Asm_Operand --
   ----------------------

   procedure Next_Asm_Operand (Operand_Var : in out Node_Id) is
   begin
      pragma Assert (Present (Operand_Var));

      if Nkind (Parent (Operand_Var)) = N_Aggregate then
         Operand_Var := Next (Operand_Var);

      else
         Operand_Var := Empty;
      end if;
   end Next_Asm_Operand;

   --------------------
   -- Next_Asm_Input --
   --------------------

   procedure Next_Asm_Input is
   begin
      Next_Asm_Operand (Current_Input_Operand);
   end Next_Asm_Input;

   ---------------------
   -- Next_Asm_Output --
   ---------------------

   procedure Next_Asm_Output is
   begin
      Next_Asm_Operand (Current_Output_Operand);
   end Next_Asm_Output;

   -----------------------
   -- Setup_Asm_IO_Args --
   -----------------------

   procedure Setup_Asm_IO_Args (Arg : Node_Id; Operand_Var : out Node_Id) is
   begin
      --  Case of single argument

      if Nkind (Arg) = N_Attribute_Reference then
         Operand_Var := Arg;

      --  Case of list of arguments

      elsif Nkind (Arg) = N_Aggregate then
         if Expressions (Arg) = No_List then
            Operand_Var := Empty;
         else
            Operand_Var := First (Expressions (Arg));
         end if;

      --  Otherwise must be default (no operands) case

      else
         Operand_Var := Empty;
      end if;
   end Setup_Asm_IO_Args;

   ----------------------
   -- Setup_Asm_Inputs --
   ----------------------

   procedure Setup_Asm_Inputs (N : Node_Id) is
      Call : constant Node_Id := Expression (Expression (N));

   begin
      Setup_Asm_IO_Args
        (Next_Actual (Next_Actual (First_Actual (Call))),
         Current_Input_Operand);
   end Setup_Asm_Inputs;

   -----------------------
   -- Setup_Asm_Outputs --
   -----------------------

   procedure Setup_Asm_Outputs (N : Node_Id) is
      Call : constant Node_Id := Expression (Expression (N));

   begin
      Setup_Asm_IO_Args
        (Next_Actual (First_Actual (Call)),
         Current_Output_Operand);
   end Setup_Asm_Outputs;

end Exp_Code;

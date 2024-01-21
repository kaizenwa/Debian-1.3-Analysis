------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E V A L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.231 $                            --
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
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Util; use Exp_Util;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Dist; use Sem_Dist;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;

package body Sem_Eval is

   -----------------------------------------
   -- Handling of Compile Time Evaluation --
   -----------------------------------------

   --  The compile time evaluation of expressions is distributed over several
   --  Eval_xxx procedures. These procedures are called immediatedly after
   --  a subexpression is resolved and is therefore accomplished in a bottom
   --  up fashion. The flags are synthesized using the following approach.

   --    Is_Static_Expression is determined by following the detailed rules
   --    in RM 4.9(4-14). This involves testing the Is_Static_Expression
   --    flag of the operands in many cases.

   --    Raises_Constraint_Error is set if any of the operands have the flag
   --    set or if an attempt to compute the value of the current expression
   --    results in detection of a runtime constraint error.

   --  As described in the spec, the requirement is that Is_Static_Expression
   --  be accurately set, and in addition for nodes for which this flag is set,
   --  Raises_Constraint_Error must also be set. Furthermore a node which has
   --  Is_Static_Expression set, and Raises_Constraint_Error clear, then the
   --  requirement is that the expression value must be precomputed, and the
   --  node is either a literal, or the name of a constant entity whose value
   --  is a static expression.

   --  The general approach is as follows. First compute Is_Static_Expression.
   --  If the node is not static, then the flag is left off in the node and
   --  we are all done. Otherwise for a static node, we test if any of the
   --  operands will raise constraint error, and if so, propagate the flag
   --  Raises_Constraint_Error to the result node and we are done (since the
   --  error was already posted at a lower level).

   --  For the case of a static node whose operands do not raise constraint
   --  error, we attempt to evaluate the node. If this evaluation succeeds,
   --  then the node is replaced by the result of this computation. If the
   --  evaluation raises constraint error, then we rewrite the node with
   --  Apply_Compile_Time_Constraint_Error to raise the exception and also
   --  to post appropriate error messages.

   ----------------
   -- Local Data --
   ----------------

   type Bits is array (Nat range <>) of Boolean;
   --  Used to convert unsigned (modular) values for folding logical ops

   -----------------------
   -- Local Subprograms --
   -----------------------

   function From_Bits (B : Bits; T : Entity_Id) return Uint;
   --  Converts a bit string of length B'Length to a Uint value to be used
   --  for a target of type T, which is a modular type. This procedure
   --  includes the necessary reduction by the modulus in the case of a
   --  non-binary modulus (for a binary modulus, the bit string is the
   --  right length any way so all is well).

   function Get_String_Val (N : Node_Id) return Node_Id;
   --  Given a tree node for a folded string or character value, returns
   --  the corresponding string literal or character literal (one of the
   --  two must be available, or the operand would not have been marked
   --  as foldable in the earlier analysis of the operation).

   procedure Out_Of_Range (N : Node_Id);
   --  This procedure is called if it is determined that node N, which
   --  appears in a non-static context, is a compile time known value
   --  which is outside its range, i.e. the range of Etype. This is used
   --  in contexts where this is an illegality if N is static, and should
   --  generate a warning otherwise.

   procedure Rewrite_In_Raise_CE (N : Node_Id; Exp : Node_Id);
   --  N and Exp are nodes representing an expression, Exp is known
   --  to raise CE. N is rewritten in term of Exp in the optimal way.

   function String_Type_Len (Stype : Entity_Id) return Uint;
   --  Given a string type, determines the length of the index type, or,
   --  if this index type is non-static, the length of the base type of
   --  this index type. Note that if the string type is itself static,
   --  then the index type is static, so the second case applies only
   --  if the string type passed is non-static.

   function Test (Cond : Boolean) return Uint;
   pragma Inline (Test);
   --  This function simply returns the appropriate Boolean'Pos value
   --  corresponding to the value of Cond as a universal integer. It is
   --  used for producing the result of the static evaluation of the
   --  logical operators

   procedure Test_Expression_Is_Foldable
     (N    : Node_Id;
      Op1  : Node_Id;
      Stat : out Boolean;
      Fold : out Boolean);
   --  Tests to see if expression N whose single operand is Op1 is foldable,
   --  i.e. the operand value is known at compile time. If the operation is
   --  foldable, then Fold is True on return, and Stat indicates whether
   --  the result is static (i.e. both operands were static). Note that it
   --  is quite possible for Fold to be True, and Stat to be False, since
   --  there are cases in which we know the value of an operand even though
   --  it is not technically static (e.g. the static lower bound of a range
   --  whose upper bound is non-static).
   --
   --  If Stat is set False on return, then Expression_Is_Foldable makes a
   --  call to Check_Non_Static_Context on the operand. If Fold is False on
   --  return, then all processing is complete, and the caller should
   --  return, since there is nothing else to do.

   procedure Test_Expression_Is_Foldable
     (N    : Node_Id;
      Op1  : Node_Id;
      Op2  : Node_Id;
      Stat : out Boolean;
      Fold : out Boolean);
   --  Same processing, except applies to an expression N with two operands
   --  Op1 and Op2.

   procedure To_Bits (U : Uint; B : out Bits);
   --  Converts a Uint value to a bit string of length B'Length

   ------------------------------
   -- Check_Non_Static_Context --
   ------------------------------

   procedure Check_Non_Static_Context (N : Node_Id) is
      T         : Entity_Id := Etype (N);
      Checks_On : constant Boolean :=
                    not Index_Checks_Suppressed (T)
                      and not Range_Checks_Suppressed (T);

   begin
      --  We need the check only for static expressions not raising CE
      --  We can also ignore cases in which the type is Any_Type

      if not Is_OK_Static_Expression (N)
        or else Etype (N) = Any_Type
      then
         return;

      --  Skip this check for non-scalar expressions

      elsif not Is_Scalar_Type (T) then
         return;

      --  Check is required

      else
         if Is_Out_Of_Range (N, Base_Type (T)) then
            Out_Of_Range (N);

         --  Give warning if outside subtype (where one or both of the
         --  bounds of the subtype is static). This warning is omitted
         --  if the expression appears in a range that could be null
         --  (warnings are handled elsewhere for this case).

         elsif T /= Base_Type (T)
           and then Nkind (Parent (N)) /= N_Range
         then
            if Is_In_Range (N, T) then
               null;

            elsif Is_Out_Of_Range (N, T) then
               Apply_Compile_Time_Constraint_Error
                 (N, "value not in range of}?");
            else
               Set_Do_Range_Check (N, Checks_On);
            end if;
         end if;

      end if;
   end Check_Non_Static_Context;

   ---------------------------------
   -- Check_String_Literal_Length --
   ---------------------------------

   procedure Check_String_Literal_Length (N : Node_Id; Ttype : Entity_Id) is
   begin
      if not Raises_Constraint_Error (N)
        and then Is_Constrained (Ttype)
      then
         if
           UI_From_Int (String_Length (Strval (N))) /= String_Type_Len (Ttype)
         then
            Apply_Compile_Time_Constraint_Error
              (N, "string length wrong for}?", Ttype);
         end if;
      end if;
   end Check_String_Literal_Length;

   ------------------------------
   -- Compile_Time_Known_Value --
   ------------------------------

   function Compile_Time_Known_Value (Op : Node_Id) return Boolean is
      K : constant Node_Kind := Nkind (Op);

   begin
      --  Never known at compile time if bad type or raises constraint error

      if Etype (Op) = Any_Type
        or else Raises_Constraint_Error (Op)
      then
         return False;
      end if;

      --  If we have an entity name, then see if it is the name of a constant
      --  and if so, test the corresponding constant value, or the name of
      --  an enumeration literal, which is always a constant.

      if Is_Entity_Name (Op) then
         declare
            E : constant Entity_Id := Entity (Op);
            V : Node_Id;

         begin
            if Ekind (E) = E_Enumeration_Literal then
               return True;

            elsif Ekind (E) /= E_Constant then
               return False;

            else
               V := Constant_Value (E);
               return Present (V) and then Compile_Time_Known_Value (V);
            end if;
         end;

      --  We have a value, see if it is compile time known

      else
         --  Literals and NULL are known at compile time

         if K = N_Integer_Literal
              or else
            K = N_Character_Literal
              or else
            K = N_Real_Literal
              or else
            K = N_String_Literal
              or else
            K = N_Null
         then
            return True;

         --  Any reference to Null_Parameter is known at compile time. No
         --  other attribute references (that have not already been folded)
         --  are known at compile time.

         elsif K = N_Attribute_Reference then
            return Attribute_Name (Op) = Name_Null_Parameter;

         --  All other types of values are not known at compile time

         else
            return False;
         end if;

      end if;
   end Compile_Time_Known_Value;

   --------------------------------------
   -- Compile_Time_Known_Value_Or_Aggr --
   --------------------------------------

   function Compile_Time_Known_Value_Or_Aggr (Op : Node_Id) return Boolean is
      K : constant Node_Kind := Nkind (Op);

   begin
      --  If we have an entity name, then see if it is the name of a constant
      --  and if so, test the corresponding constant value, or the name of
      --  an enumeration literal, which is always a constant.

      if Is_Entity_Name (Op) then
         declare
            E : constant Entity_Id := Entity (Op);
            V : Node_Id;

         begin
            if Ekind (E) = E_Enumeration_Literal then
               return True;

            elsif Ekind (E) /= E_Constant then
               return False;

            else
               V := Constant_Value (E);
               return Present (V)
                 and then Compile_Time_Known_Value_Or_Aggr (V);
            end if;
         end;

      --  We have a value, see if it is compile time known

      else
         if Compile_Time_Known_Value (Op) then
            return True;

         elsif Nkind (Op) = N_Aggregate then

            if Present (Expressions (Op)) then
               declare
                  Expr : Node_Id;

               begin
                  Expr := First (Expressions (Op));
                  while Present (Expr) loop
                     if not Compile_Time_Known_Value_Or_Aggr (Expr) then
                        return False;
                     end if;

                     Expr := Next (Expr);
                  end loop;
               end;
            end if;


            if Present (Component_Associations (Op)) then
               declare
                  Cass : Node_Id;

               begin
                  Cass := First (Component_Associations (Op));
                  while Present (Cass) loop
                     if not
                       Compile_Time_Known_Value_Or_Aggr (Expression (Cass))
                     then
                        return False;
                     end if;

                     Cass := Next (Cass);
                  end loop;
               end;
            end if;

            return True;

         --  All other types of values are not known at compile time

         else
            return False;
         end if;

      end if;
   end Compile_Time_Known_Value_Or_Aggr;

   -----------------
   -- Eval_Actual --
   -----------------

   --  This is only called for actuals of functions that are not predefined
   --  operators (which have already been rewritten as operators at this
   --  stage), so the call can never be folded, and all that needs doing for
   --  the actual is to do the check for a non-static context.

   procedure Eval_Actual (N : Node_Id) is
   begin
      Check_Non_Static_Context (N);
   end Eval_Actual;

   --------------------
   -- Eval_Allocator --
   --------------------

   --  Allocators are never static, so all we have to do is to do the
   --  check for a non-static context if an expression is present.

   procedure Eval_Allocator (N : Node_Id) is
      Expr : constant Node_Id := Expression (N);

   begin
      if Nkind (Expr) = N_Qualified_Expression then
         Check_Non_Static_Context (Expression (Expr));
      end if;
   end Eval_Allocator;

   ------------------------
   -- Eval_Arithmetic_Op --
   ------------------------

   --  Arithmetic operations are static functions, so the result is static
   --  if both operands are static (RM 4.9(7), 4.9(20)).

   procedure Eval_Arithmetic_Op (N : Node_Id) is
      Left  : constant Node_Id   := Left_Opnd (N);
      Right : constant Node_Id   := Right_Opnd (N);
      Ltype : constant Entity_Id := Etype (Left);
      Rtype : constant Entity_Id := Etype (Right);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Fold for cases where both operands are of integer type

      if Is_Integer_Type (Ltype) and then Is_Integer_Type (Rtype) then
         declare
            Left_Int  : constant Uint := Expr_Value (Left);
            Right_Int : constant Uint := Expr_Value (Right);
            Result    : Uint;

         begin
            case Nkind (N) is

               when N_Op_Add =>
                  Result := Left_Int + Right_Int;

               when N_Op_Subtract =>
                  Result := Left_Int - Right_Int;

               when N_Op_Multiply =>
                  Result := Left_Int * Right_Int;

               when N_Op_Divide =>

                  --  The exception Constraint_Error is raised by integer
                  --  division, rem and mod if the right operand is zero.

                  if Right_Int = 0 then
                     Apply_Compile_Time_Constraint_Error
                       (N, "division by zero");
                     return;
                  else
                     Result := Left_Int / Right_Int;
                  end if;

               when N_Op_Mod =>

                  --  The exception Constraint_Error is raised by integer
                  --  division, rem and mod if the right operand is zero.

                  if Right_Int = 0 then
                     Apply_Compile_Time_Constraint_Error
                       (N, "mod with zero divisor");
                     return;
                  else
                     Result := Left_Int mod Right_Int;
                  end if;

               when N_Op_Rem =>

                  --  The exception Constraint_Error is raised by integer
                  --  division, rem and mod if the right operand is zero.

                  if Right_Int = 0 then
                     Apply_Compile_Time_Constraint_Error
                       (N, "rem with zero divisor");
                     return;
                  else
                     Result := Left_Int rem Right_Int;
                  end if;

               when others =>
                  pragma Assert (False); null;
            end case;

            --  Adjust the result by the modulus if the type is a modular type

            if Is_Modular_Integer_Type (Ltype) then
               Result := Result mod Modulus (Ltype);
            end if;

            Fold_Uint (N, Result);
         end;

      --  Cases where at least one operand is a real. We handle the cases
      --  of both reals, or mixed/real integer cases (the latter happen
      --  only for divide and multiply, and the result is always real).

      elsif Is_Real_Type (Ltype) or else Is_Real_Type (Rtype) then
         declare
            Left_Real  : Ureal;
            Right_Real : Ureal;
            Result     : Ureal;

         begin
            if Is_Real_Type (Ltype) then
               Left_Real := Expr_Value_R (Left);
            else
               Left_Real := UR_From_Uint (Expr_Value (Left));
            end if;

            if Is_Real_Type (Rtype) then
               Right_Real := Expr_Value_R (Right);
            else
               Right_Real := UR_From_Uint (Expr_Value (Right));
            end if;

            if Nkind (N) = N_Op_Add then
               Result := Left_Real + Right_Real;

            elsif Nkind (N) = N_Op_Subtract then
               Result := Left_Real - Right_Real;

            elsif Nkind (N) = N_Op_Multiply then
               Result := Left_Real * Right_Real;

            elsif Nkind (N) = N_Op_Divide then
               if UR_Is_Zero (Right_Real) then
                  Apply_Compile_Time_Constraint_Error
                    (N, "division by zero");
                  return;
               end if;

               Result := Left_Real / Right_Real;

            else
               pragma Assert (False); null;
            end if;

            Fold_Ureal (N, Result);
         end;
      end if;

      Set_Is_Static_Expression (N, Stat);

   end Eval_Arithmetic_Op;

   ----------------------------
   -- Eval_Character_Literal --
   ----------------------------

   --  Nothing to be done!

   procedure Eval_Character_Literal (N : Node_Id) is
   begin
      null;
   end Eval_Character_Literal;

   ------------------------
   -- Eval_Concatenation --
   ------------------------

   --  Concatenation is a a static functions, so the result is static if
   --  both operands are static (RM 4.9(7), 4.9(21)).

   procedure Eval_Concatenation (N : Node_Id) is
      Left  : constant Node_Id := Left_Opnd (N);
      Right : constant Node_Id := Right_Opnd (N);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  Concatenation is never static in Ada 83, so if Ada 83
      --  check operand non-static context

      if Ada_83
        and then Comes_From_Source (N)
      then
         Check_Non_Static_Context (Left);
         Check_Non_Static_Context (Right);
         return;
      end if;

      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Compile time string concatenation.

      --  ??? Note that operands that are aggregates can be marked as
      --  static, so we should attempt at a later stage to fold
      --  concatenations with such aggregates.

      declare
         Left_Str  : constant Node_Id := Get_String_Val (Left);
         Right_Str : constant Node_Id := Get_String_Val (Right);

      begin
         --  Establish new string literal, and store left operand. We make
         --  sure to use the special Start_String that takes an operand if
         --  the left operand is a string literal. Since this is optimized
         --  in the case where that is the most recently created string
         --  literal, we ensure efficient time/space behavior for the
         --  case of a concatenation of a series of string literals.

         if Nkind (Left_Str) = N_String_Literal then
            Start_String (Strval (Left_Str));
         else
            Start_String;
            Store_String_Char (Char_Literal_Value (Left_Str));
         end if;

         --  Now append the characters of the right operand

         if Nkind (Right_Str) = N_String_Literal then
            declare
               S : constant String_Id := Strval (Right_Str);

            begin
               for J in 1 .. String_Length (S) loop
                  Store_String_Char (Get_String_Char (S, J));
               end loop;
            end;
         else
            Store_String_Char (Char_Literal_Value (Right_Str));
         end if;

         Fold_Str (N, End_String);
         Set_Is_Static_Expression (N, Stat);
      end;
   end Eval_Concatenation;

   ---------------------------------
   -- Eval_Conditional_Expression --
   ---------------------------------

   --  This GNAT internal construct can never be statically folded, so the
   --  only required processing is to do the check for non-static context
   --  for the two expression operands.

   procedure Eval_Conditional_Expression (N : Node_Id) is
      Condition : constant Node_Id := First (Expressions (N));
      Then_Expr : constant Node_Id := Next (Condition);
      Else_Expr : constant Node_Id := Next (Then_Expr);

   begin
      Check_Non_Static_Context (Then_Expr);
      Check_Non_Static_Context (Else_Expr);
   end Eval_Conditional_Expression;

   ----------------------
   -- Eval_Entity_Name --
   ----------------------

   --  This procedure is used for identifiers and expanded names other than
   --  named numbers (see Eval_Named_Integer, Eval_Named_Real. These are
   --  static if they denote a static constant (RM 4.9(6)) or if the name
   --  denotes an enumeration literal (RM 4.9(22)).

   procedure Eval_Entity_Name (N : Node_Id) is
      Def_Id : constant Entity_Id := Entity (N);
      Val    : Node_Id;

   begin
      --  Enumeration literals are always considered to be constants
      --  and cannot raise constraint error (RM 4.9(22)).

      if Ekind (Def_Id) = E_Enumeration_Literal then
         Set_Is_Static_Expression (N);
         return;

      --  A name is static if it denotes a static constant (RM 4.9(5)), and
      --  we also copy Raise_Constraint_Error. Notice that even if non-static,
      --  it does not violate 10.2.1(8) here, since this is not a variable.

      elsif Ekind (Def_Id) = E_Constant then
         Val := Constant_Value (Def_Id);

         if Present (Val) then
            Set_Is_Static_Expression
              (N, Is_Static_Expression (Val)
                    and then Is_Static_Subtype (Etype (Def_Id)));
            Set_Raises_Constraint_Error (N, Raises_Constraint_Error (Val));
            return;
         end if;
      end if;

      --  Fall through if the name is not static.

      Validate_Static_Object_Name (N);
   end Eval_Entity_Name;

   ----------------------------
   -- Eval_Indexed_Component --
   ----------------------------

   --  Indexed components are never static, so the only required processing
   --  is to perform the check for non-static context on the index values.

   procedure Eval_Indexed_Component (N : Node_Id) is
      Expr : Node_Id;

   begin
      Expr := First (Expressions (N));
      while Present (Expr) loop
         Check_Non_Static_Context (Expr);
         Expr := Next (Expr);
      end loop;

   end Eval_Indexed_Component;

   --------------------------
   -- Eval_Integer_Literal --
   --------------------------

   --  Numeric literals are static (RM 4.9(1)), and have already been marked
   --  as static by the analyzer. The reason we did it that early is to allow
   --  the possibility of turning off the Is_Static_Expression flag after
   --  analysis, but before resolution, when integer literals are generated
   --  in the expander that do not correspond to static expressions.

   procedure Eval_Integer_Literal (N : Node_Id) is
      T : constant Entity_Id := Etype (N);

   begin
      --  If the literal appears in a non-expression context, then it is
      --  certainly appearing in a non-static context, so check it. This
      --  is actually a redundant check, since Check_Non_Static_Context
      --  would check it, but it seems worth while avoiding the call.

      if Nkind (Parent (N)) not in N_Subexpr then
         Check_Non_Static_Context (N);
      end if;

      --  Modular integer literals must be in their base range

      if Is_Modular_Integer_Type (T)
        and then Is_Out_Of_Range (N, Base_Type (T))
      then
         Out_Of_Range (N);
      end if;
   end Eval_Integer_Literal;

   ---------------------
   -- Eval_Logical_Op --
   ---------------------

   --  Logical operations are static functions, so the result is potentially
   --  static if both operands are potentially static (RM 4.9(7), 4.9(20)).

   procedure Eval_Logical_Op (N : Node_Id) is
      Left  : constant Node_Id := Left_Opnd (N);
      Right : constant Node_Id := Right_Opnd (N);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Compile time evaluation of logical operation

      declare
         Left_Int  : constant Uint := Expr_Value (Left);
         Right_Int : constant Uint := Expr_Value (Right);

      begin
         if Is_Modular_Integer_Type (Etype (N)) then
            declare
               Left_Bits  : Bits (0 .. UI_To_Int (Esize (Etype (N))) - 1);
               Right_Bits : Bits (0 .. UI_To_Int (Esize (Etype (N))) - 1);

            begin
               To_Bits (Left_Int, Left_Bits);
               To_Bits (Right_Int, Right_Bits);

               --  Note: should really be able to use array ops instead of
               --  these loops, but they weren't working at the time ???

               if Nkind (N) = N_Op_And then
                  for J in Left_Bits'Range loop
                     Left_Bits (J) := Left_Bits (J) and Right_Bits (J);
                  end loop;

               elsif Nkind (N) = N_Op_Or then
                  for J in Left_Bits'Range loop
                     Left_Bits (J) := Left_Bits (J) or Right_Bits (J);
                  end loop;

               else
                  pragma Assert (Nkind (N) = N_Op_Xor);

                  for J in Left_Bits'Range loop
                     Left_Bits (J) := Left_Bits (J) xor Right_Bits (J);
                  end loop;
               end if;

               Fold_Uint (N, From_Bits (Left_Bits, Etype (N)));
            end;

         else
            pragma Assert (Is_Boolean_Type (Etype (N)));

            if Nkind (N) = N_Op_And then
               Fold_Uint (N,
                 Test (Is_True (Left_Int) and then Is_True (Right_Int)));

            elsif Nkind (N) = N_Op_Or then
               Fold_Uint (N,
                 Test (Is_True (Left_Int) or else Is_True (Right_Int)));

            else
               pragma Assert (Nkind (N) = N_Op_Xor);
               Fold_Uint (N,
                 Test (Is_True (Left_Int) xor Is_True (Right_Int)));
            end if;
         end if;

         Set_Is_Static_Expression (N, Stat);
      end;
   end Eval_Logical_Op;

   ------------------------
   -- Eval_Membership_Op --
   ------------------------

   --  A membership test is potentially static if the expression is static,
   --  and the range is a potentially static range, or is a subtype mark
   --  denoting a static subtype (RM 4.9(12)).

   procedure Eval_Membership_Op (N : Node_Id) is
      Left   : constant Node_Id := Left_Opnd (N);
      Right  : constant Node_Id := Right_Opnd (N);
      Def_Id : Entity_Id;
      Lo     : Node_Id;
      Hi     : Node_Id;
      Result : Boolean;
      Stat   : Boolean;
      Fold   : Boolean;

   begin
      --  Ignore if error in either operand, except to make sure that
      --  Any_Type is properly propagated to avoid junk cascaded errors.

      if Etype (Left) = Any_Type
        or else Etype (Right) = Any_Type
      then
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  Case of right operand is a subtype name

      if Is_Entity_Name (Right) then
         Def_Id := Entity (Right);

         if (Is_Scalar_Type (Def_Id) or else Is_String_Type (Def_Id))
           and then Is_OK_Static_Subtype (Def_Id)
         then
            Test_Expression_Is_Foldable (N, Left, Stat, Fold);

            if not Fold or else not Stat then
               return;
            end if;
         else
            Check_Non_Static_Context (Left);
            return;
         end if;

         --  Here we deal with the bizarre case of a string type
         --  For now, just never fold, we will worry about this later ???

         if Is_String_Type (Def_Id) then
            Check_Non_Static_Context (Left);
            return;
         end if;

         Lo := Type_Low_Bound (Def_Id);
         Hi := Type_High_Bound (Def_Id);

      --  Case of right operand is a range

      else
         if Is_Static_Range (Right) then
            Test_Expression_Is_Foldable (N, Left, Stat, Fold);

            if not Fold or else not Stat then
               return;

            --  If one bound of range raises CE, then don't try to fold

            elsif not Is_OK_Static_Range (Right) then
               Check_Non_Static_Context (Left);
               return;
            end if;

         else
            Check_Non_Static_Context (Left);
            return;
         end if;

         --  Here we know range is an OK static range

         Lo := Low_Bound (Right);
         Hi := High_Bound (Right);
      end if;

      --  Fold the membership test. We know we have a static range and Lo
      --  and Hi are set to the expressions for the end points of this range.

      if Is_Real_Type (Etype (Right)) then
         declare
            Leftval : constant Ureal := Expr_Value_R (Left);

         begin
            Result := Expr_Value_R (Lo) <= Leftval
                        and then Leftval <= Expr_Value_R (Hi);
         end;

      else
         declare
            Leftval : constant Uint := Expr_Value (Left);

         begin
            Result := Expr_Value (Lo) <= Leftval
                        and then Leftval <= Expr_Value (Hi);
         end;
      end if;

      if Nkind (N) = N_Not_In then
         Result := not Result;
      end if;

      Fold_Uint (N, Test (Result));

   end Eval_Membership_Op;

   ------------------------
   -- Eval_Named_Integer --
   ------------------------

   procedure Eval_Named_Integer (N : Node_Id) is
   begin
      Fold_Uint (N,
        Expr_Value (Expression (Declaration_Node (Entity (N)))));
   end Eval_Named_Integer;

   ---------------------
   -- Eval_Named_Real --
   ---------------------

   procedure Eval_Named_Real (N : Node_Id) is
   begin
      Fold_Ureal (N,
        Expr_Value_R (Expression (Declaration_Node (Entity (N)))));
   end Eval_Named_Real;

   -------------------
   -- Eval_Op_Expon --
   -------------------

   --  Exponentiation is a static functions, so the result is potentially
   --  static if both operands are potentially static (RM 4.9(7), 4.9(20)).

   procedure Eval_Op_Expon (N : Node_Id) is
      Left  : constant Node_Id := Left_Opnd (N);
      Right : constant Node_Id := Right_Opnd (N);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Fold exponentiation operation

      declare
         Right_Int : constant Uint := Expr_Value (Right);

      begin
         --  Integer case

         if Is_Integer_Type (Etype (Left)) then
            declare
               Left_Int : constant Uint := Expr_Value (Left);
               Result   : Uint;

            begin
               --  Exponentiation of an integer raises the exception
               --  Constraint_Error for a negative exponent (RM 4.5.6)

               if Right_Int < 0 then
                  Apply_Compile_Time_Constraint_Error
                    (N, "integer exponent negative");
                  return;

               else
                  Result := Left_Int ** Right_Int;

                  if Is_Modular_Integer_Type (Etype (N)) then
                     Result := Result mod Modulus (Etype (N));
                  end if;

                  Fold_Uint (N, Result);
               end if;
            end;

         --  Real case

         else
            declare
               Left_Real : constant Ureal := Expr_Value_R (Left);

            begin
               --  Cannot have a zero base with a negative exponent

               if Right_Int < 0 and then UR_Is_Zero (Left_Real) then
                  Apply_Compile_Time_Constraint_Error
                    (N, "zero ** negative integer");
                  return;
               else
                  Fold_Ureal (N, Left_Real ** Right_Int);
               end if;
            end;
         end if;

         Set_Is_Static_Expression (N, Stat);
      end;
   end Eval_Op_Expon;

   -----------------
   -- Eval_Op_Not --
   -----------------

   --  The not operation is a  static functions, so the result is potentially
   --  static if the operand is potentially static (RM 4.9(7), 4.9(20)).

   procedure Eval_Op_Not (N : Node_Id) is
      Right : constant Node_Id := Right_Opnd (N);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Fold not operation

      declare
         Rint : constant Uint := Expr_Value (Right);

      begin
         if Is_Modular_Integer_Type (Etype (N)) then
            declare
               Right_Bits : Bits (0 .. UI_To_Int (Esize (Etype (N))) - 1);

            begin
               To_Bits (Rint, Right_Bits);

               for J in Right_Bits'Range loop
                  Right_Bits (J) := not Right_Bits (J);
               end loop;

               Fold_Uint (N, From_Bits (Right_Bits, Etype (N)));
            end;

         else
            pragma Assert (Is_Boolean_Type (Etype (N)));
            Fold_Uint (N, Test (not Is_True (Rint)));
         end if;

         Set_Is_Static_Expression (N, Stat);
      end;
   end Eval_Op_Not;

   -------------------------------
   -- Eval_Qualified_Expression --
   -------------------------------

   --  A qualified expression is potentially static if its subtype mark denotes
   --  a static subtype and its expression is potentially static (RM 4.9 (11)).

   procedure Eval_Qualified_Expression (N : Node_Id) is
      Operand     : constant Node_Id   := Expression (N);
      Target_Type : constant Entity_Id := Entity (Subtype_Mark (N));

      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  Can only fold if target is string or scalar and subtype is static
      --  Also, do not fold if our parent is an allocator (this is because
      --  the qualified expression is really part of the syntactic structure
      --  of an allocator, and we do not want to end up with something that
      --  corresponds to "new 1" where the 1 is the result of folding a
      --  qualified expression).

      if not Is_Static_Subtype (Target_Type)
        or else Nkind (Parent (N)) = N_Allocator
      then
         Check_Non_Static_Context (Operand);
         return;
      end if;

      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Operand, Stat, Fold);

      if not Fold then
         return;

      --  Don't try fold if target type has constraint error bounds

      elsif not Is_OK_Static_Subtype (Target_Type) then
         Set_Raises_Constraint_Error (N);
         return;
      end if;

      --  Fold the result of qualification

      if Is_Discrete_Type (Target_Type) then
         Fold_Uint (N, Expr_Value (Operand));
         Set_Is_Static_Expression (N, Stat);

      elsif Is_Real_Type (Target_Type) then
         Fold_Ureal (N, Expr_Value_R (Operand));
         Set_Is_Static_Expression (N, Stat);

      else
         Fold_Str (N, Strval (Get_String_Val (Operand)));

         if not Stat then
            Set_Is_Static_Expression (N, False);
         else
            Check_String_Literal_Length (N, Target_Type);
         end if;

         return;
      end if;

      if Is_Out_Of_Range (N, Etype (N)) then
         Out_Of_Range (N);
      end if;

   end Eval_Qualified_Expression;

   -----------------------
   -- Eval_Real_Literal --
   -----------------------

   --  Numeric literals are static (RM 4.9(1)), and have already been marked
   --  as static by the analyzer. The reason we did it that early is to allow
   --  the possibility of turning off the Is_Static_Expression flag after
   --  analysis, but before resolution, when integer literals are generated
   --  in the expander that do not correspond to static expressions.

   procedure Eval_Real_Literal (N : Node_Id) is
   begin
      --  If the literal appears in a non-expression context, then it is
      --  certainly appearing in a non-static context, so check it.

      if Nkind (Parent (N)) not in N_Subexpr then
         Check_Non_Static_Context (N);
      end if;

   end Eval_Real_Literal;

   ------------------------
   -- Eval_Relational_Op --
   ------------------------

   --  Relational operations are static functions, so the result is static
   --  if both operands are static (RM 4.9(7), 4.9(20)).

   procedure Eval_Relational_Op (N : Node_Id) is
      Left   : constant Node_Id   := Left_Opnd (N);
      Right  : constant Node_Id   := Right_Opnd (N);
      Typ    : constant Entity_Id := Etype (Left);
      Result : Boolean;
      Stat   : Boolean;
      Fold   : Boolean;

   begin
      --  Can only fold if type is scalar (don't fold string ops)

      if not Is_Scalar_Type (Typ) then
         Check_Non_Static_Context (Left);
         Check_Non_Static_Context (Right);
         return;
      end if;

      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Integer and Enumeration (discrete) type cases

      if Is_Discrete_Type (Typ) then
         declare
            Left_Int  : constant Uint := Expr_Value (Left);
            Right_Int : constant Uint := Expr_Value (Right);

         begin
            case Nkind (N) is
               when N_Op_Eq => Result := Left_Int =  Right_Int;
               when N_Op_Ne => Result := Left_Int /= Right_Int;
               when N_Op_Lt => Result := Left_Int <  Right_Int;
               when N_Op_Le => Result := Left_Int <= Right_Int;
               when N_Op_Gt => Result := Left_Int >  Right_Int;
               when N_Op_Ge => Result := Left_Int >= Right_Int;

               when others => pragma Assert (False); null;
            end case;

            Fold_Uint (N, Test (Result));
         end;

      --  Real type case

      else
         pragma Assert (Is_Real_Type (Typ));

         declare
            Left_Real  : constant Ureal := Expr_Value_R (Left);
            Right_Real : constant Ureal := Expr_Value_R (Right);

         begin
            case Nkind (N) is
               when N_Op_Eq => Result := (Left_Real =  Right_Real);
               when N_Op_Ne => Result := (Left_Real /= Right_Real);
               when N_Op_Lt => Result := (Left_Real <  Right_Real);
               when N_Op_Le => Result := (Left_Real <= Right_Real);
               when N_Op_Gt => Result := (Left_Real >  Right_Real);
               when N_Op_Ge => Result := (Left_Real >= Right_Real);

               when others => pragma Assert (False); null;
            end case;

            Fold_Uint (N, Test (Result));
         end;
      end if;

      Set_Is_Static_Expression (N, Stat);

   end Eval_Relational_Op;

   ----------------
   -- Eval_Shift --
   ----------------

   --  Shift operations are intrinsic operations that can never be static,
   --  so the only processing required is to perform the required check for
   --  a non static context for the two operands.

   --  Actually we could do some compile time evaluation here some time ???

   procedure Eval_Shift (N : Node_Id) is
   begin
      Check_Non_Static_Context (Left_Opnd (N));
      Check_Non_Static_Context (Right_Opnd (N));
   end Eval_Shift;

   ------------------------
   -- Eval_Short_Circuit --
   ------------------------

   --  A short circuit operation is potentially static if both operands
   --  are potentially static (RM 4.9 (13))

   procedure Eval_Short_Circuit (N : Node_Id) is
      Kind     : constant Node_Kind := Nkind (N);
      Left     : constant Node_Id   := Left_Opnd (N);
      Right    : constant Node_Id   := Right_Opnd (N);
      Left_Int : Uint;
      Rstat    : constant Boolean   :=
                   Is_Static_Expression (Left)
                     and then Is_Static_Expression (Right);

   begin
      --  Short circuit operations are never static in Ada 83

      if Ada_83
        and then Comes_From_Source (N)
      then
         Check_Non_Static_Context (Left);
         Check_Non_Static_Context (Right);
         return;
      end if;

      --  Now look at the operands, we can't quite use the normal call to
      --  Test_Expression_Is_Foldable here because short circuit operations
      --  are a special case, they can still be foldable, even if the right
      --  operand raises constraint error.

      --  If either operand is Any_Type, just propagate to result and
      --  do not try to fold, this prevents cascaded errors.

      if Etype (Left) = Any_Type or else Etype (Right) = Any_Type then
         Set_Etype (N, Any_Type);
         return;

      --  If left operand raises constraint error, then replace node N with
      --  the raise constraint error node, and we are obviously not foldable.
      --  Is_Static_Expression is set from the two operands in the normal way,
      --  and we check the right operand if it is in a non-static context.

      elsif Raises_Constraint_Error (Left) then
         if not Rstat then
            Check_Non_Static_Context (Right);
         end if;

         Rewrite_In_Raise_CE (N, Left);
         Set_Is_Static_Expression (N, Rstat);
         return;

      --  If the result is not static, then we won't in any case fold

      elsif not Rstat then
         Check_Non_Static_Context (Left);
         Check_Non_Static_Context (Right);
         return;
      end if;

      --  Here the result is static, note that, unlike the normal processing
      --  in Test_Expression_Is_Foldable, we did *not* check above to see if
      --  the right operand raises constraint error, that's because it is not
      --  significant if the left operand is decisive.

      Set_Is_Static_Expression (N);

      --  It does not matter if the right operand raises constraint error if
      --  it will not be evaluated. So deal specially with the cases where
      --  the right operand is not evaluated. Note that we will fold these
      --  cases even if the right operand is non-static, which is fine, but
      --  of course in these cases the result is not potentially static.

      Left_Int := Expr_Value (Left);

      if (Kind = N_And_Then and then Is_False (Left_Int))
        or else (Kind = N_Or_Else and Is_True (Left_Int))
      then
         Fold_Uint (N, Left_Int);
         return;
      end if;

      --  If first operand not decisive, then it does matter if the right
      --  operand raises constraint error, since it will be evaluated, so
      --  we simply replace the node with the right operand. Note that this
      --  properly propagates Is_Static_Expression and Raises_Constraint_Error
      --  (both are set to True in Right).

      if Raises_Constraint_Error (Right) then
         Rewrite_In_Raise_CE (N, Right);
         Check_Non_Static_Context (Left);
         return;
      end if;

      --  Otherwise the result depends on the right operand

      Fold_Uint (N, Expr_Value (Right));
      return;

   end Eval_Short_Circuit;

   ----------------
   -- Eval_Slice --
   ----------------

   --  Slices can never be static, so the only processing required is to
   --  check for non-static context if an explicit range is given.

   procedure Eval_Slice (N : Node_Id) is
      Drange : constant Node_Id := Discrete_Range (N);

   begin
      if Nkind (Drange) = N_Range then
         Check_Non_Static_Context (Low_Bound (Drange));
         Check_Non_Static_Context (High_Bound (Drange));
      end if;
   end Eval_Slice;

   -------------------------
   -- Eval_String_Literal --
   -------------------------

   procedure Eval_String_Literal (N : Node_Id) is
      T : constant Entity_Id := Etype (N);
      B : constant Entity_Id := Base_Type (T);

   begin
      --  Nothing to do if error type (handles cases like default expressions
      --  or generics where we have not yet fully resolved the type)

      if B = Any_Type or else B = Any_String then
         return;

      --  String literals are static if the subtype is static (RM 4.9(2)), so
      --  reset the static expression flag (it was set unconditionally in
      --  Analyze_String_Literal) if the subtype is non-static. We tell if
      --  the subtype is static by looking at the lower bound.

      elsif not Is_OK_Static_Expression (String_Literal_Low_Bound (T)) then
         Set_Is_Static_Expression (N, False);

      --  Test for illegal Ada 95 cases. A string literal is illegal in
      --  Ada 95 if its bounds are outside the index base type and this
      --  index type is static. This can hapen in only two ways. Either
      --  the string literal is too long, or it is null, and the lower
      --  bound is type'First. In either case it is the upper bound that
      --  is out of range of the index type.

      elsif Ada_95 then
         if String_Literal_Length (T) > String_Type_Len (B) then
            Apply_Compile_Time_Constraint_Error
              (N, "string literal too long for}", B);

         elsif String_Literal_Length (T) = 0
            and then Expr_Value (String_Literal_Low_Bound (T)) =
                     Expr_Value
                       (Type_Low_Bound (Base_Type (Etype (First_Index (B)))))
         then
            Apply_Compile_Time_Constraint_Error
              (N, "null string literal not allowed for}", B);
         end if;
      end if;

   end Eval_String_Literal;

   --------------------------
   -- Eval_Type_Conversion --
   --------------------------

   --  A type conversion is potentially static if its subtype mark is for a
   --  static scalar subtype, and its operand expression is potentially static
   --  (RM 4.9 (10))

   procedure Eval_Type_Conversion (N : Node_Id) is
      Operand     : constant Node_Id   := Expression (N);
      Source_Type : constant Entity_Id := Etype (Operand);
      Target_Type : constant Entity_Id := Etype (N);

      Stat   : Boolean;
      Fold   : Boolean;

      function To_Be_Treated_As_Integer (T : Entity_Id) return Boolean;
      --  Returns true if type T is an integer type, or if it is a
      --  fixed-point type to be treated as an integer (i.e. the flag
      --  Conversion_OK is set on the conversion node).

      function To_Be_Treated_As_Real (T : Entity_Id) return Boolean;
      --  Returns true if type T is a floating-point type, or if it is a
      --  fixed-point type that is not to be treated as an integer (i.e. the
      --  flag Conversion_OK is not set on the conversion node).

      function To_Be_Treated_As_Integer (T : Entity_Id) return Boolean is
      begin
         return
           Is_Integer_Type (T)
             or else (Is_Fixed_Point_Type (T) and then Conversion_OK (N));
      end To_Be_Treated_As_Integer;

      function To_Be_Treated_As_Real (T : Entity_Id) return Boolean is
      begin
         return
           Is_Floating_Point_Type (T)
             or else (Is_Fixed_Point_Type (T) and then not Conversion_OK (N));
      end To_Be_Treated_As_Real;

   --  Start of processing for Eval_Type_Conversion

   begin
      --  Cannot fold if target type is non-static

      if not Is_Static_Subtype (Target_Type) then
         Check_Non_Static_Context (Operand);
         return;
      end if;

      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Operand, Stat, Fold);

      if not Fold then
         return;

      --  Don't try fold if target type has constraint error bounds

      elsif not Is_OK_Static_Subtype (Target_Type) then
         Set_Raises_Constraint_Error (N);
         return;
      end if;

      --  Remaining processing depends on operand types. Note that in the
      --  following type test, fixed-point counts as real unless the flag
      --  Conversion_OK is set, in which case it counts as integer.

      --  Fold conversion, case of string type

      if Is_String_Type (Target_Type) then
         Fold_Str (N, Strval (Get_String_Val (Operand)));

         if not Stat then
            Set_Is_Static_Expression (N, False);
         else
            Check_String_Literal_Length (N, Target_Type);
         end if;

         return;

      --  Fold conversion, case of integer target type

      elsif To_Be_Treated_As_Integer (Target_Type) then
         declare
            Result : Uint;

         begin
            --  Integer to integer conversion

            if To_Be_Treated_As_Integer (Source_Type) then
               Result := Expr_Value (Operand);

            --  Real to integer conversion

            else
               Result := UR_To_Uint (Expr_Value_R (Operand));
            end if;

            --  If fixed-point type (Conversion_OK must be set), then the
            --  result is logically an integer, but we must replace the
            --  conversion with the corresponding real literal, since the
            --  type from a semantic point of view is still fixed-point.

            if Is_Fixed_Point_Type (Target_Type) then
               Fold_Ureal
                 (N, UR_From_Uint (Result) * Small_Value (Target_Type));

            --  Otherwise result is integer literal

            else
               Fold_Uint (N, Result);
            end if;
         end;

      --  Fold conversion, case of real target type

      elsif To_Be_Treated_As_Real (Target_Type) then
         declare
            Result : Ureal;

         begin
            if To_Be_Treated_As_Real (Source_Type) then
               Result := Expr_Value_R (Operand);
            else
               Result := UR_From_Uint (Expr_Value (Operand));
            end if;

            Fold_Ureal (N, Result);
         end;

      --  Enumeration types

      else
         Fold_Uint (N, Expr_Value (Operand));
      end if;

      Set_Is_Static_Expression (N, Stat);

      if Is_Out_Of_Range (N, Etype (N)) then
         Out_Of_Range (N);
      end if;

   end Eval_Type_Conversion;

   -------------------------------
   -- Eval_Unchecked_Conversion --
   -------------------------------

   --  Unchecked conversions can never be static, so the only required
   --  processing is to check for a non-static context for the operand.

   procedure Eval_Unchecked_Conversion (N : Node_Id) is
   begin
      Check_Non_Static_Context (Expression (N));
   end Eval_Unchecked_Conversion;

   -------------------
   -- Eval_Unary_Op --
   -------------------

   --  Predefined unary operators are static functions (RM 4.9(20)) and thus
   --  are potentially static if the operand is potentially static (RM 4.9(7))

   procedure Eval_Unary_Op (N : Node_Id) is
      Right : constant Node_Id := Right_Opnd (N);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Right, Stat, Fold);

      if not Fold then
         return;
      end if;


      --  Fold for integer case

      if Is_Integer_Type (Etype (N)) then
         declare
            Rint   : constant Uint := Expr_Value (Right);
            Result : Uint;

         begin
            --  In the case of modular unary plus and abs there is no need
            --  to adjust the result of the operation since if the original
            --  operand was in bounds the result will be in the bounds of the
            --  modular type. However, in the case of modular unary minus the
            --  result may go out of the bounds of the modular type and needs
            --  adjustment.

            if Nkind (N) = N_Op_Plus then
               Result := Rint;

            elsif Nkind (N) = N_Op_Minus then
               if Is_Modular_Integer_Type (Etype (N)) then
                  Result := (-Rint) mod Modulus (Etype (N));
               else
                  Result := (-Rint);
               end if;

            else
               pragma Assert (Nkind (N) = N_Op_Abs);
               Result := abs Rint;
            end if;

            Fold_Uint (N, Result);
         end;

      --  Fold for real case

      elsif Is_Real_Type (Etype (N)) then
         declare
            Rreal  : constant Ureal := Expr_Value_R (Right);
            Result : Ureal;

         begin
            if Nkind (N) = N_Op_Plus then
               Result := Rreal;

            elsif Nkind (N) = N_Op_Minus then
               Result := UR_Negate (Rreal);

            else
               pragma Assert (Nkind (N) = N_Op_Abs);
               Result := abs Rreal;
            end if;

            Fold_Ureal (N, Result);
         end;
      end if;

      Set_Is_Static_Expression (N, Stat);

   end Eval_Unary_Op;

   --------------------
   -- Expr_Rep_Value --
   --------------------

   function Expr_Rep_Value (N : Node_Id) return Uint is
      Kind   : constant Node_Kind := Nkind (N);
      Ent    : Entity_Id;

   begin
      if Is_Entity_Name (N) then
         Ent := Entity (N);

         --  An enumeration literal that was either in the source or
         --  created as a result of static evaluation.

         if Ekind (Ent) = E_Enumeration_Literal then
            return Enumeration_Rep (Ent);

         --  A user defined static constant

         else
            pragma Assert (Ekind (Ent) = E_Constant);
            return Expr_Rep_Value (Constant_Value (Ent));
         end if;

      --  An integer literal that was either in the source or created
      --  as a result of static evaluation.

      elsif Kind = N_Integer_Literal then
         return Intval (N);

      --  A real literal for a fixed-point type. This must be the fixed-point
      --  case, either the literal is of a fixed-point type, or it is a bound
      --  of a fixed-point type, with type universal real. In either case we
      --  obtain the desired value from Corresponding_Integer_Value.

      elsif Kind = N_Real_Literal then
         pragma Assert (Is_Fixed_Point_Type (Etype (N)));
         return Corresponding_Integer_Value (N);

      else
         pragma Assert (Kind = N_Character_Literal);
         Ent := Entity (N);

         --  Since Character literals of type Standard.Character don't
         --  have any defining character literals built for them, they
         --  do not have their Entity set, so just use their Char
         --  code. Otherwise for user-defined character literals use
         --  their Pos value as usual which is the same as the Rep value.

         if No (Ent) then
            return UI_From_Int (Int (Char_Literal_Value (N)));
         else
            return Enumeration_Rep (Ent);
         end if;
      end if;
   end Expr_Rep_Value;

   ----------------
   -- Expr_Value --
   ----------------

   function Expr_Value (N : Node_Id) return Uint is
      Kind : constant Node_Kind := Nkind (N);
      Ent  : Entity_Id;

   begin
      if Is_Entity_Name (N) then
         Ent := Entity (N);

         --  An enumeration literal that was either in the source or
         --  created as a result of static evaluation.

         if Ekind (Ent) = E_Enumeration_Literal then
            return Enumeration_Pos (Ent);

         --  A user defined static constant

         else
            pragma Assert (Ekind (Ent) = E_Constant);
            return Expr_Value (Constant_Value (Ent));
         end if;

      --  An integer literal that was either in the source or created
      --  as a result of static evaluation.

      elsif Kind = N_Integer_Literal then
         return Intval (N);

      --  A real literal for a fixed-point type. This must be the fixed-point
      --  case, either the literal is of a fixed-point type, or it is a bound
      --  of a fixed-point type, with type universal real. In either case we
      --  obtain the desired value from Corresponding_Integer_Value.

      elsif Kind = N_Real_Literal then
         pragma Assert (Is_Fixed_Point_Type (Etype (N)));
         return Corresponding_Integer_Value (N);

      else
         pragma Assert (Kind = N_Character_Literal);
         Ent := Entity (N);

         --  Since Character literals of type Standard.Character don't
         --  have any defining character literals built for them, they
         --  do not have their Entity set, so just use their Char
         --  code. Otherwise for user-defined character literals use
         --  their Pos value as usual.

         if No (Ent) then
            return UI_From_Int (Int (Char_Literal_Value (N)));
         else
            return Enumeration_Pos (Ent);
         end if;
      end if;

   end Expr_Value;

   ------------------
   -- Expr_Value_E --
   ------------------

   function Expr_Value_E (N : Node_Id) return Entity_Id is
      Ent  : constant Entity_Id := Entity (N);

   begin
      if Ekind (Ent) = E_Enumeration_Literal then
         return Ent;
      else
         pragma Assert (Ekind (Ent) = E_Constant);
         return Expr_Value_E (Constant_Value (Ent));
      end if;
   end Expr_Value_E;

   ------------------
   -- Expr_Value_R --
   ------------------

   function Expr_Value_R (N : Node_Id) return Ureal is
      Kind : constant Node_Kind := Nkind (N);
      Ent  : Entity_Id;

   begin
      if Kind = N_Identifier or else Kind = N_Expanded_Name then
         Ent := Entity (N);
         pragma Assert (Ekind (Ent) = E_Constant);
         return Expr_Value_R (Constant_Value (Ent));

      elsif Kind = N_Integer_Literal then
         return UR_From_Uint (Expr_Value (N));

      else
         pragma Assert (Kind = N_Real_Literal);
         return Realval (N);
      end if;
   end Expr_Value_R;

   ------------------
   -- Expr_Value_S --
   ------------------

   function Expr_Value_S (N : Node_Id) return Node_Id is
   begin
      if Nkind (N) = N_String_Literal then
         return N;
      else
         pragma Assert (Ekind (Entity (N)) = E_Constant);
         return Expr_Value_S (Constant_Value (Entity (N)));
      end if;
   end Expr_Value_S;

   --------------
   -- Fold_Str --
   --------------

   procedure Fold_Str (N : Node_Id; Val : String_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

   begin
      Rewrite_Substitute_Tree (N, Make_String_Literal (Loc, Strval => Val));
      Analyze_And_Resolve (N, Typ);
   end Fold_Str;

   ---------------
   -- Fold_Uint --
   ---------------

   procedure Fold_Uint (N : Node_Id; Val : Uint) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      Lit : Entity_Id;
      Pos : Int;

   begin
      --  For a result of type integer, subsitute an N_Integer_Literal node
      --  for the result of the compile time evaluation of the expression.

      if Is_Integer_Type (Etype (N)) then
         Rewrite_Substitute_Tree (N, Make_Integer_Literal (Loc, Val));

      --  Otherwise we have an enumeration type, and we substitute either
      --  an N_Identifier or N_Character_Literal to represent the enumeration
      --  literal corresponding to the given value, which must always be in
      --  range, because appropriate tests have already been made for this.

      elsif Is_Enumeration_Type (Etype (N)) then
         Pos := UI_To_Int (Val);

         --  In the case where the literal is either of type Wide_Character
         --  or Character or of a type derived from them, there needs to be
         --  some special handling since there is no explicit chain of
         --  literals to search. Instead, an N_Character_Literal node is
         --  created with the appropriate Char_Code and Chars fields.

         if Root_Type (Etype (N)) = Standard_Character
           or else Root_Type (Etype (N)) = Standard_Wide_Character
         then
            Set_Character_Literal_Name (Char_Code (Pos));

            Rewrite_Substitute_Tree (N,
              Make_Character_Literal (Loc,
                Chars => Name_Find,
                Char_Literal_Value => Char_Code (Pos)));

         --  For all other cases, we have a complete table of literals, and
         --  we simply iterate through the chain of literal until the one
         --  with the desired position value is found.
         --

         else
            Lit := First_Literal (Base_Type (Etype (N)));
            for J in 1 .. Pos loop
               Lit := Next_Literal (Lit);
            end loop;

            Rewrite_Substitute_Tree (N, New_Occurrence_Of (Lit, Loc));
         end if;

      --  Anything other than an integer type or enumeration type is wrong

      else
         pragma Assert (False); null;
      end if;

      --  We now have the literal with the right value, both the actual type
      --  and the expected type of this literal are taken from the expression
      --  that was evaluated.

      Analyze (N);
      Set_Etype (N, Typ);
      Resolve (N, Typ);
   end Fold_Uint;

   ----------------
   -- Fold_Ureal --
   ----------------

   procedure Fold_Ureal (N : Node_Id; Val : Ureal) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

   begin
      Rewrite_Substitute_Tree (N, Make_Real_Literal (Loc, Realval => Val));
      Analyze (N);

      --  Both the actual and expected type comes from the original expression

      Set_Etype (N, Typ);
      Resolve (N, Typ);
   end Fold_Ureal;

   ---------------
   -- From_Bits --
   ---------------

   function From_Bits (B : Bits; T : Entity_Id) return Uint is
      V : Uint := Uint_0;

   begin
      for J in 0 .. B'Last loop
         if B (J) then
            V := V + 2 ** J;
         end if;
      end loop;

      if Non_Binary_Modulus (T) then
         V := V mod Modulus (T);
      end if;

      return V;
   end From_Bits;

   --------------------
   -- Get_String_Val --
   --------------------

   function Get_String_Val (N : Node_Id) return Node_Id is
   begin
      if Nkind (N) = N_String_Literal then
         return N;

      elsif Nkind (N) = N_Character_Literal then
         return N;

      else
         pragma Assert (Is_Entity_Name (N));
         return Get_String_Val (Constant_Value (Entity (N)));
      end if;
   end Get_String_Val;

   -----------------
   -- Is_In_Range --
   -----------------

   function Is_In_Range
     (N         : Node_Id;
      Typ       : Entity_Id;
      Fixed_Int : Boolean := False;
      Int_Real  : Boolean := False)
      return      Boolean
   is
      Val  : Uint;
      Valr : Ureal;

   begin
      --  Universal types have no range limits, so always in range.

      if Typ = Universal_Integer or else Typ = Universal_Real then
         return True;

      --  Never in range if not scalar type. Don't know if this can
      --  actually happen, but our spec allows it, so we must check!

      elsif not Is_Scalar_Type (Typ) then
         return False;

      --  Never in range unless we have a compile time known value.

      elsif not Compile_Time_Known_Value (N) then
         return False;

      else
         declare
            Lo       : constant Node_Id := Type_Low_Bound  (Typ);
            Hi       : constant Node_Id := Type_High_Bound (Typ);
            LB_Known : constant Boolean := Compile_Time_Known_Value (Lo);
            UB_Known : constant Boolean := Compile_Time_Known_Value (Hi);

         begin
            --  Fixed point types should be considered as such only in
            --  flag Fixed_Int is set to False.

            if Is_Floating_Point_Type (Typ)
              or else (Is_Fixed_Point_Type (Typ) and then not Fixed_Int)
              or else Int_Real
            then
               Valr := Expr_Value_R (N);

               if LB_Known and then Valr >= Expr_Value_R (Lo)
                 and then UB_Known and then Valr <= Expr_Value_R (Hi)
               then
                  return True;
               else
                  return False;
               end if;

            else
               Val := Expr_Value (N);

               if         LB_Known and then Val >= Expr_Value (Lo)
                 and then UB_Known and then Val <= Expr_Value (Hi)
               then
                  return True;
               else
                  return False;
               end if;
            end if;
         end;
      end if;
   end Is_In_Range;

   -----------------------------
   -- Is_OK_Static_Expression --
   -----------------------------

   function Is_OK_Static_Expression (N : Node_Id) return Boolean is
   begin
      return Is_Static_Expression (N)
        and then not Raises_Constraint_Error (N);
   end Is_OK_Static_Expression;

   ------------------------
   -- Is_OK_Static_Range --
   ------------------------

   --  A static range is a range whose bounds are static expressions, or a
   --  Range_Attribute_Reference equivalent to such a range (RM 4.9(26)).
   --  We have already converted range attribute references, so we get the
   --  "or" part of this rule without needing a special test.

   function Is_OK_Static_Range (N : Node_Id) return Boolean is
   begin
      return Is_OK_Static_Expression (Low_Bound (N))
        and then Is_OK_Static_Expression (High_Bound (N));
   end Is_OK_Static_Range;

   --------------------------
   -- Is_OK_Static_Subtype --
   --------------------------

   --  Determines if Typ is a static subtype as defined in (RM 4.9(26))
   --  where neither bound raises constraint error when evaluated.

   function Is_OK_Static_Subtype (Typ : Entity_Id) return Boolean is
      Base_T   : constant Entity_Id := Base_Type (Typ);
      Anc_Subt : Entity_Id;

   begin
      --  First a quick check on the non static subtype flag. As described
      --  in further detail in Einfo, this flag is not decisive in all cases,
      --  but if it is set, then the subtype is definitely non-static.

      if Is_Non_Static_Subtype (Typ) then
         return False;
      end if;

      Anc_Subt := Ancestor_Subtype (Typ);

      if Anc_Subt = Empty then
         Anc_Subt := Base_T;
      end if;

      if Is_Generic_Type (Root_Type (Base_T))
        or else Is_Generic_Actual_Type (Base_T)
      then
         return False;

      --  String types

      elsif Is_String_Type (Typ) then
         return
           Ekind (Typ) = E_String_Literal_Subtype
             or else
           (Is_OK_Static_Subtype (Component_Type (Typ))
              and then Is_OK_Static_Subtype (Etype (First_Index (Typ))));

      --  Scalar types

      elsif Is_Scalar_Type (Typ) then
         if Base_T = Typ then
            return True;

         else
            --  Scalar_Range (Typ) might be an N_Subtype_Indication, so
            --  use Get_Type_Low,High_Bound.

            return     Is_OK_Static_Subtype (Anc_Subt)
              and then Is_OK_Static_Expression (Type_Low_Bound (Typ))
              and then Is_OK_Static_Expression (Type_High_Bound (Typ));
         end if;

      --  Types other than string and scalar types are never static

      else
         return False;
      end if;
   end Is_OK_Static_Subtype;

   -------------------
   -- Is_Null_Range --
   -------------------

   function Is_Null_Range (Lo : Node_Id; Hi : Node_Id) return Boolean is
      Typ : constant Entity_Id := Etype (Lo);

   begin
      if not Compile_Time_Known_Value (Lo)
        or else not Compile_Time_Known_Value (Hi)
      then
         return False;
      end if;

      if Is_Discrete_Type (Typ) then
         return Expr_Value (Lo) > Expr_Value (Hi);

      else
         pragma Assert (Is_Real_Type (Typ));
         return Expr_Value_R (Lo) > Expr_Value_R (Hi);
      end if;
   end Is_Null_Range;

   ---------------------
   -- Is_Out_Of_Range --
   ---------------------

   function Is_Out_Of_Range
     (N         : Node_Id;
      Typ       : Entity_Id;
      Fixed_Int : Boolean := False;
      Int_Real  : Boolean := False)
      return      Boolean
   is
      Val  : Uint;
      Valr : Ureal;

   begin
      --  Universal types have no range limits, so always in range.

      if Typ = Universal_Integer or else Typ = Universal_Real then
         return False;

      --  Never out of range if not scalar type. Don't know if this can
      --  actually happen, but our spec allows it, so we must check!

      elsif not Is_Scalar_Type (Typ) then
         return False;

      --  Never out of range if this is a generic type, since the bounds
      --  of generic types are junk. Note that if we only checked for
      --  static expressions (instead of compile time known values) below,
      --  we would not need this check, because values of a generic type
      --  can never be static, but they can be known at compile time.

      elsif Is_Generic_Type (Typ) then
         return False;

      --  Never out of range unless we have a compile time known value.

      elsif not Compile_Time_Known_Value (N) then
         return False;

      else
         declare
            Lo       : constant Node_Id := Type_Low_Bound  (Typ);
            Hi       : constant Node_Id := Type_High_Bound (Typ);
            LB_Known : constant Boolean := Compile_Time_Known_Value (Lo);
            UB_Known : constant Boolean := Compile_Time_Known_Value (Hi);

         begin
            --  Real types (note that fixed-point types are not treated
            --  as being of a real type if the flag Fixed_Int is set,
            --  since in that case they are regarded as integer types).

            if Is_Floating_Point_Type (Typ)
              or else (Is_Fixed_Point_Type (Typ) and then not Fixed_Int)
              or else Int_Real
            then
               Valr := Expr_Value_R (N);

               if LB_Known and then Valr < Expr_Value_R (Lo) then
                  return True;

               elsif UB_Known and then Expr_Value_R (Hi) < Valr then
                  return True;

               else
                  return False;
               end if;

            else
               Val := Expr_Value (N);

               if LB_Known and then Val < Expr_Value (Lo) then
                  return True;

               elsif UB_Known and then Expr_Value (Hi) < Val then
                  return True;

               else
                  return False;
               end if;
            end if;
         end;
      end if;
   end Is_Out_Of_Range;

   ---------------------
   -- Is_Static_Range --
   ---------------------

   --  A static range is a range whose bounds are static expressions, or a
   --  Range_Attribute_Reference equivalent to such a range (RM 4.9(26)).
   --  We have already converted range attribute references, so we get the
   --  "or" part of this rule without needing a special test.

   function Is_Static_Range (N : Node_Id) return Boolean is
   begin
      return Is_Static_Expression (Low_Bound (N))
        and then Is_Static_Expression (High_Bound (N));
   end Is_Static_Range;

   -----------------------
   -- Is_Static_Subtype --
   -----------------------

   --  Determines if Typ is a static subtype as defined in (RM 4.9(26)).

   function Is_Static_Subtype (Typ : Entity_Id) return Boolean is
      Base_T   : constant Entity_Id := Base_Type (Typ);
      Anc_Subt : Entity_Id;

   begin
      --  First a quick check on the non static subtype flag. As described
      --  in further detail in Einfo, this flag is not decisive in all cases,
      --  but if it is set, then the subtype is definitely non-static.

      if Is_Non_Static_Subtype (Typ) then
         return False;
      end if;

      Anc_Subt := Ancestor_Subtype (Typ);

      if Anc_Subt = Empty then
         Anc_Subt := Base_T;
      end if;

      if Is_Generic_Type (Root_Type (Base_T))
        or else Is_Generic_Actual_Type (Base_T)
      then
         return False;

      --  String types

      elsif Is_String_Type (Typ) then
         return
           Ekind (Typ) = E_String_Literal_Subtype
             or else
           (Is_Static_Subtype (Component_Type (Typ))
              and then Is_Static_Subtype (Etype (First_Index (Typ))));

      --  Scalar types

      elsif Is_Scalar_Type (Typ) then
         if Base_T = Typ then
            return True;

         else
            return     Is_Static_Subtype (Anc_Subt)
              and then Is_Static_Expression (Type_Low_Bound (Typ))
              and then Is_Static_Expression (Type_High_Bound (Typ));
         end if;

      --  Types other than string and scalar types are never static

      else
         return False;
      end if;
   end Is_Static_Subtype;

   --------------------
   -- In_Subrange_Of --
   --------------------

   function In_Subrange_Of
     (T1        : Entity_Id;
      T2        : Entity_Id;
      Fixed_Int : Boolean := False)
      return      Boolean
   is
      L1 : Node_Id;
      H1 : Node_Id;

      L2 : Node_Id;
      H2 : Node_Id;

   begin
      if T1 = T2 or else Is_Subtype_Of (T1, T2) then
         return True;

      --  Never in range if both types are not scalar. Don't know if this can
      --  actually happen, but just in case.

      elsif not Is_Scalar_Type (T1) or else not Is_Scalar_Type (T1) then
         return False;

      else
         L1 := Type_Low_Bound  (T1);
         H1 := Type_High_Bound (T1);

         L2 := Type_Low_Bound  (T2);
         H2 := Type_High_Bound (T2);

         if not Compile_Time_Known_Value (L2)
           or else not Compile_Time_Known_Value (H2)
         then
            return False;
         end if;

         --  If the bounds of T1 are know at compile time then use these
         --  ones, otherwise use the bounds of the base type (which are of
         --  course always static).

         if not Compile_Time_Known_Value (L1) then
            L1 := Type_Low_Bound (Base_Type (T1));
         end if;

         if not Compile_Time_Known_Value (H1) then
            H1 := Type_High_Bound (Base_Type (T1));
         end if;

         --  Fixed point types should be considered as such only if
         --  flag Fixed_Int is set to False.

         if Is_Floating_Point_Type (T1) or else Is_Floating_Point_Type (T2)
           or else (Is_Fixed_Point_Type (T1) and then not Fixed_Int)
           or else (Is_Fixed_Point_Type (T2) and then not Fixed_Int)
         then
            return
              Expr_Value_R (L2) <= Expr_Value_R (L1)
                and then
              Expr_Value_R (H2) >= Expr_Value_R (H1);

         else
            return
              Expr_Value (L2) <= Expr_Value (L1)
                and then
              Expr_Value (H2) >= Expr_Value (H1);

         end if;
      end if;
   end In_Subrange_Of;

   --------------------
   -- Not_Null_Range --
   --------------------

   function Not_Null_Range (Lo : Node_Id; Hi : Node_Id) return Boolean is
      Typ : constant Entity_Id := Etype (Lo);

   begin
      if not Compile_Time_Known_Value (Lo)
        or else not Compile_Time_Known_Value (Hi)
      then
         return False;
      end if;

      if Is_Discrete_Type (Typ) then
         return Expr_Value (Lo) <= Expr_Value (Hi);

      else
         pragma Assert (Is_Real_Type (Typ));

         return Expr_Value_R (Lo) <= Expr_Value_R (Hi);
      end if;
   end Not_Null_Range;

   ------------------
   -- Out_Of_Range --
   ------------------

   procedure Out_Of_Range (N : Node_Id) is
   begin
      if Is_Static_Expression (N) then
         Apply_Compile_Time_Constraint_Error
           (N, "value not in range of}");
      else
         Apply_Compile_Time_Constraint_Error
           (N, "value not in range of}?");
      end if;
   end Out_Of_Range;

   -------------------------
   -- Rewrite_In_Raise_CE --
   -------------------------

   procedure Rewrite_In_Raise_CE (N : Node_Id; Exp : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      --  If we want to raise CE in the condition of a raise_CE node
      --  we may as well get rid of the condition

      if Present (Parent (N))
        and then Nkind (Parent (N)) = N_Raise_Constraint_Error
      then
         Set_Condition (Parent (N), Empty);

      --  If the expression raising CE is a N_Raise_CE node, we can use
      --  that one. We just preserve the type of the context

      elsif Nkind (Exp) = N_Raise_Constraint_Error then
         Rewrite_Substitute_Tree (N, Exp);
         Set_Etype (N, Typ);

      --  We have to build an explicit raise_ce node

      else
         Rewrite_Substitute_Tree (N, Make_Raise_Constraint_Error (Sloc (Exp)));
         Set_Raises_Constraint_Error (N);
         Set_Etype (N, Typ);
      end if;
   end Rewrite_In_Raise_CE;

   ---------------------
   -- String_Type_Len --
   ---------------------

   function String_Type_Len (Stype : Entity_Id) return Uint is
      NT : constant Entity_Id := Etype (First_Index (Stype));
      T  : Entity_Id;

   begin
      if Is_OK_Static_Subtype (NT) then
         T := NT;
      else
         T := Base_Type (NT);
      end if;

      return Expr_Value (Type_High_Bound (T)) -
             Expr_Value (Type_Low_Bound (T)) + 1;
   end String_Type_Len;

   ------------------------------------
   -- Subtypes_Statically_Compatible --
   ------------------------------------

   function Subtypes_Statically_Compatible
     (T1   : Entity_Id;
      T2   : Entity_Id)
      return Boolean
   is
   begin
      if Is_Scalar_Type (T1) then

         --  Definitely compatible if we match

         if Subtypes_Statically_Match (T1, T2) then
            return True;

         --  If either type has constraint error bounds, then consider that
         --  they match to avoid junk cascaded errors here.

         elsif not Is_OK_Static_Subtype (T1)
           or else not Is_OK_Static_Subtype (T2)
         then
            return True;

         --  Base types must match, but we don't check that (should
         --  we???) but we do at least check that both types are
         --  real, or both types are not real.

         elsif (Is_Real_Type (T1) /= Is_Real_Type (T2)) then
            return False;

         --  Here we check the bounds

         else
            declare
               LB1 : constant Node_Id := Type_Low_Bound  (T1);
               HB1 : constant Node_Id := Type_High_Bound (T1);
               LB2 : constant Node_Id := Type_Low_Bound  (T2);
               HB2 : constant Node_Id := Type_High_Bound (T2);

            begin
               if Is_Real_Type (T1) then
                  return
                    (Expr_Value_R (LB1) > Expr_Value_R (HB1))
                      or else
                    (Expr_Value_R (LB2) <= Expr_Value_R (LB1)
                       and then
                     Expr_Value_R (HB1) <= Expr_Value_R (HB2));

               else
                  return
                    (Expr_Value (LB1) > Expr_Value (HB1))
                      or else
                    (Expr_Value (LB2) <= Expr_Value (LB1)
                       and then
                     Expr_Value (HB1) <= Expr_Value (HB2));
               end if;
            end;
         end if;

      elsif Is_Access_Type (T1) then
         return Subtypes_Statically_Match
                  (Designated_Type (T1),
                   Designated_Type (T2));

      else
         return Subtypes_Statically_Match (T1, T2);
      end if;
   end Subtypes_Statically_Compatible;

   -------------------------------
   -- Subtypes_Statically_Match --
   -------------------------------

   --  Subtypes statically match if they have statically matching constraints
   --  (RM 4.9.1(2)). Constraints statically match if there are none, or if
   --  they are the same identical constraint, or if they are static and the
   --  values match (RM 4.9.1(1)).

   function Subtypes_Statically_Match (T1, T2 : Entity_Id) return Boolean is
   begin
      --  A type always statically matches itself

      if T1 = T2 then
         return True;

      --  Scalar types

      elsif Is_Scalar_Type (T1) then

         --  Base types must be the same

         if Base_Type (T1) /= Base_Type (T2) then
            return False;
         end if;

         --  A constrained numeric subtype never matches an unconstrained
         --  subtype, i.e. both types must be constrained or unconstrained.

         --  To understand the requirement for this test, see RM 4.9.1(1).
         --  As is made clear in RM 3.5.4(11), type Integer, for example
         --  is a constrained subtype with constraint bounds matching the
         --  bounds of its corresponding uncontrained base type. In this
         --  situation, Integer and Integer'Base do not statically match,
         --  even though they have the same bounds.

         --  We only apply this test to types in Standard and types that
         --  appear in user programs. That way, we do not have to be
         --  too careful about setting Is_Constrained right for itypes.

         if Is_Numeric_Type (T1)
           and then (Is_Constrained (T1) /= Is_Constrained (T2))
           and then (Scope (T1) = Standard_Standard
                      or else Comes_From_Source (T1))
           and then (Scope (T2) = Standard_Standard
                      or else Comes_From_Source (T2))
         then
            return False;
         end if;

         --  Otherwise both types have bound that can be compared

         declare
            LB1 : constant Node_Id := Type_Low_Bound  (T1);
            HB1 : constant Node_Id := Type_High_Bound (T1);
            LB2 : constant Node_Id := Type_Low_Bound  (T2);
            HB2 : constant Node_Id := Type_High_Bound (T2);

         begin
            --  If the bounds are the same tree node, then match

            if LB1 = LB2 and then HB1 = HB2 then
               return True;

            --  Otherwise bounds must be static and identical value

            else
               if not Is_Static_Subtype (T1)
                 or else not Is_Static_Subtype (T2)
               then
                  return False;

               --  If either type has constraint error bounds, then say
               --  that they match to avoid junk cascaded errors here.

               elsif not Is_OK_Static_Subtype (T1)
                 or else not Is_OK_Static_Subtype (T2)
               then
                  return True;

               elsif Is_Real_Type (T1) then
                  return
                    (Expr_Value_R (LB1) = Expr_Value_R (LB2))
                      and then
                    (Expr_Value_R (HB1) = Expr_Value_R (HB2));

               else
                  return
                    Expr_Value (LB1) = Expr_Value (LB2)
                      and then
                    Expr_Value (HB1) = Expr_Value (HB2);
               end if;
            end if;
         end;

      --  Type with discriminants

      elsif Has_Discriminants (T1) or else Has_Discriminants (T2) then
         if Has_Discriminants (T1) /= Has_Discriminants (T2) then
            return False;
         end if;

         declare
            DL1 : constant Elist_Id := Discriminant_Constraint (T1);
            DL2 : constant Elist_Id := Discriminant_Constraint (T2);

            DA1 : Elmt_Id := First_Elmt (DL1);
            DA2 : Elmt_Id := First_Elmt (DL2);

         begin
            if DL1 = DL2 then
               return True;

            elsif Is_Constrained (T1) /= Is_Constrained (T2) then
               return False;
            end if;

            while Present (DA1) loop
               declare
                  Expr1 : constant Node_Id := Node (DA1);
                  Expr2 : constant Node_Id := Node (DA2);

               begin
                  if not Is_Static_Expression (Expr1)
                    or else not Is_Static_Expression (Expr2)
                  then
                     return False;

                  --  If either expression raised a constraint error,
                  --  consider the expressions as matching, since this
                  --  helps to prevent cascading errors.

                  elsif Raises_Constraint_Error (Expr1)
                    or else Raises_Constraint_Error (Expr2)
                  then
                     null;

                  elsif Expr_Value (Expr1) /= Expr_Value (Expr2) then
                     return False;
                  end if;
               end;

               DA1 := Next_Elmt (DA1);
               DA2 := Next_Elmt (DA2);
            end loop;
         end;

         return True;

      --  Array type

      elsif Is_Array_Type (T1) then

         --  If either subtype is unconstrained then both must be,
         --  and if both are unconstrained then no further checking
         --  is needed.

         if not Is_Constrained (T1) or else not Is_Constrained (T2) then
            return not (Is_Constrained (T1) or else Is_Constrained (T2));
         end if;

         --  Both subtypes are constrained, so check that the index
         --  subtypes statically match.

         declare
            Index1 : Node_Id := First_Index (T1);
            Index2 : Node_Id := First_Index (T2);

         begin
            while Present (Index1) loop
               if not
                 Subtypes_Statically_Match (Etype (Index1), Etype (Index2))
               then
                  return False;
               end if;

               Index1 := Next_Index (Index1);
               Index2 := Next_Index (Index2);
            end loop;

            return True;
         end;

      elsif Is_Access_Type (T1) then
         return Subtypes_Statically_Match
                  (Designated_Type (T1),
                   Designated_Type (T2));

      --  All other types definitely match

      else
         return True;
      end if;
   end Subtypes_Statically_Match;

   ----------
   -- Test --
   ----------

   function Test (Cond : Boolean) return Uint is
   begin
      if Cond then
         return Uint_1;
      else
         return Uint_0;
      end if;
   end Test;

   ---------------------------------
   -- Test_Expression_Is_Foldable --
   ---------------------------------

   --  One operand case

   procedure Test_Expression_Is_Foldable
     (N    : Node_Id;
      Op1  : Node_Id;
      Stat : out Boolean;
      Fold : out Boolean)
   is
   begin
      Stat := False;

      --  If operand is Any_Type, just propagate to result and do not
      --  try to fold, this prevents cascaded errors.

      if Etype (Op1) = Any_Type then
         Set_Etype (N, Any_Type);
         Fold := False;
         return;

      --  If operand raises constraint error, then replace node N with the
      --  raise constraint error node, and we are obviously not foldable.
      --  Note that this replacement inherits the Is_Static_Expression flag
      --  from the operand.

      elsif Raises_Constraint_Error (Op1) then
         Rewrite_In_Raise_CE (N, Op1);
         Fold := False;
         return;

      --  If the operand is not static, then the result is not static, and
      --  all we have to do is to check the operand since it is now known
      --  to appear in a non-static context.

      elsif not Is_Static_Expression (Op1) then
         Check_Non_Static_Context (Op1);
         Fold := Compile_Time_Known_Value (Op1);
         return;

      --  Here we have the case of an operand whose type is OK, which is
      --  static, and which does not raise constraint error, we can fold.

      else
         Set_Is_Static_Expression (N);
         Fold := True;
         Stat := True;
      end if;
   end Test_Expression_Is_Foldable;

   --  Two operand case

   procedure Test_Expression_Is_Foldable
     (N    : Node_Id;
      Op1  : Node_Id;
      Op2  : Node_Id;
      Stat : out Boolean;
      Fold : out Boolean)
   is
      Rstat : constant Boolean := Is_Static_Expression (Op1)
                                    and then Is_Static_Expression (Op2);

   begin
      Stat := False;

      --  If either operand is Any_Type, just propagate to result and
      --  do not try to fold, this prevents cascaded errors.

      if Etype (Op1) = Any_Type or else Etype (Op2) = Any_Type then
         Set_Etype (N, Any_Type);
         Fold := False;
         return;

      --  If left operand raises constraint error, then replace node N with
      --  the raise constraint error node, and we are obviously not foldable.
      --  Is_Static_Expression is set from the two operands in the normal way,
      --  and we check the right operand if it is in a non-static context.

      elsif Raises_Constraint_Error (Op1) then
         if not Rstat then
            Check_Non_Static_Context (Op2);
         end if;

         Rewrite_In_Raise_CE (N, Op1);
         Set_Is_Static_Expression (N, Rstat);
         Fold := False;
         return;

      --  Similar processing for the case of the right operand. Note that
      --  we don't use this routine for the short-circuit case, so we do
      --  not have to worry about that special case here.

      elsif Raises_Constraint_Error (Op2) then
         if not Rstat then
            Check_Non_Static_Context (Op1);
         end if;

         Rewrite_In_Raise_CE (N, Op2);
         Set_Is_Static_Expression (N, Rstat);
         Fold := False;
         return;

      --  If result is not-static, then check non-static contexts on operands
      --  since one of them may be static and the other one may not be static

      elsif not Rstat then
         Check_Non_Static_Context (Op1);
         Check_Non_Static_Context (Op2);
         Fold := Compile_Time_Known_Value (Op1)
                   and then Compile_Time_Known_Value (Op2);
         return;

      --  Else result is static and foldable. Both operands are static,
      --  and neither raises constraint error, so we can definitely fold.

      else
         Set_Is_Static_Expression (N);
         Fold := True;
         Stat := True;
         return;
      end if;
   end Test_Expression_Is_Foldable;

   --------------
   -- To_Bits --
   --------------

   procedure To_Bits (U : Uint; B : out Bits) is
   begin
      for J in 0 .. B'Last loop
         B (J) := (U / (2 ** J)) mod 2 /= 0;
      end loop;
   end To_Bits;

end Sem_Eval;

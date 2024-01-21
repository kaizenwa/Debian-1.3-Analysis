------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C H E C K S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.116 $                            --
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
with Errout;   use Errout;
with Exp_Ch2;  use Exp_Ch2;
with Exp_Util; use Exp_Util;
with Expander; use Expander;
with Elists;   use Elists;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Checks is

   --  General note: many of these routines are concerned with generating
   --  checking code to make sure that constraint error is raised at runtime.
   --  Clearly this code is only needed if the expander is active, since
   --  otherwise we will not be generating code or going into the runtime
   --  execution anyway.

   --  We therefore disconnect most of these checks if the expander is
   --  inactive. This has the additional benefit that we do not need to
   --  worry about the tree being messed up by previous errors (since errors
   --  turn off expansion anyway).

   --  There are a few exceptions to the above rule. For instance routines
   --  such as Apply_Scalar_Range_Check that do not insert any code can be
   --  safely called even when the Expander is inactive (but Errors_Detected
   --  is 0). The benefit of executing this code when expansion is off, is
   --  the ability to emit constraint error warning for static expressions
   --  even when we are not generating code.

   ----------------------------
   -- Local Subprogram Specs --
   ----------------------------

   procedure Apply_Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean);
   --  This is the subprogram that does all the work for Apply_Length_Check
   --  and Apply_Static_Length_Check. Expr, Target_Typ and Source_Typ are as
   --  described for the above routines. The Do_Static flag indicates that
   --  only a static check is to be done.

   procedure Apply_Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean);
   --  This is the subprogram that does all the work for Apply_Range_Check.
   --  Expr, Target_Typ and Source_Typ are as described for the above
   --  routine. The Do_Static flag indicates that only a static check is
   --  to be done.

   procedure Determine_Range
     (N  : Node_Id;
      OK : out Boolean;
      Lo : out Uint;
      Hi : out Uint);
   --  Given a node N whose Etype is either an integer or fixed-point type,
   --  determines the maximum possible range of the result, assuming that
   --  the execution is non-erroneous. If OK is True on return, then Lo and
   --  Hi are set to a conservative estimate of the range, i.e. the value is
   --  absolutely guaranteed to lie within the returned range.

   function Guard_Access
     (Cond    : Node_Id;
      Loc     : Source_Ptr;
      Ck_Node : Node_Id)
      return    Node_Id;
   --  In the access type case, guard the test with a test to ensure
   --  that the access value is non-null, since the checks do not
   --  not apply to null access values.

   procedure Install_Static_Check (R_Cno : Node_Id; Loc : Source_Ptr);
   --  Called by Apply_{Length,Range}_Checks to rewrite the tree with the
   --  Constraint_Error node.

   function Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id)
      return       Check_Result;
   --  Like Apply_Selected_Length_Checks, except it doesn't modify
   --  anything, just returns a list of nodes as described in the spec of
   --  this package for the Range_Check function.

   function Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id)
      return       Check_Result;
   --  Like Apply_Selected_Range_Checks, except it doesn't modify anything,
   --  just returns a list of nodes as described in the spec of this package
   --  for the Range_Check function.

   ------------------------------
   -- Access_Checks_Suppressed --
   ------------------------------

   function Access_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Access_Checks
        or else (Present (E) and then Suppress_Access_Checks (E));
   end Access_Checks_Suppressed;

   -------------------------------------
   -- Accessibility_Checks_Suppressed --
   -------------------------------------

   function Accessibility_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Accessibility_Checks
        or else (Present (E) and then Suppress_Accessibility_Checks (E));
   end Accessibility_Checks_Suppressed;

   -------------------------
   -- Append_Range_Checks --
   -------------------------

   procedure Append_Range_Checks
     (Checks       : Check_Result;
      Stmts        : List_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr;
      Flag_Node    : Node_Id)
   is
      Internal_Flag_Node   : Node_Id    := Flag_Node;
      Internal_Static_Sloc : Source_Ptr := Static_Sloc;
      Checks_On : constant Boolean :=
                    (not Index_Checks_Suppressed (Suppress_Typ))
                       or else
                    (not Range_Checks_Suppressed (Suppress_Typ));

   begin
      --  For now we just return if Checks_On is false, however this should
      --  be enhanced to check for an always True value in the condition
      --  and to generate a compilation warning???

      if not Checks_On then
         return;
      end if;

      for J in 1 .. 2 loop
         exit when No (Checks (J));

         if Nkind (Checks (J)) = N_Raise_Constraint_Error
           and then Present (Condition (Checks (J)))
         then
            if not Has_Dynamic_Range_Check (Internal_Flag_Node) then
               Append_To (Stmts, Checks (J));
               Set_Has_Dynamic_Range_Check (Internal_Flag_Node);
            end if;

         else
            Append_To
              (Stmts, Make_Raise_Constraint_Error (Internal_Static_Sloc));
         end if;
      end loop;
   end Append_Range_Checks;

   ------------------------
   -- Apply_Access_Check --
   ------------------------

   procedure Apply_Access_Check (N : Node_Id; Typ : Entity_Id) is
   begin
      if not Access_Checks_Suppressed (Typ) then
         Set_Do_Access_Check (N, True);
      end if;
   end Apply_Access_Check;

   -------------------------------
   -- Apply_Accessibility_Check --
   -------------------------------

   procedure Apply_Accessibility_Check (N : Node_Id; Typ : Entity_Id) is
      Loc         : constant Source_Ptr := Sloc (N);
      Param_Ent   : constant Entity_Id  := Param_Entity (N);
      Param_Level : Node_Id;
      Type_Level  : Node_Id;

   begin
      --  Only apply the run-time check if the access parameter
      --  has an associated extra access level parameter and
      --  when the level of the type is less deep than the level
      --  of the access parameter.

      if Present (Param_Ent)
         and then Present (Extra_Formal_Accessibility (Param_Ent))
         and then UI_Gt (Object_Access_Level (N),
                         Type_Access_Level (Typ))
         and then not Accessibility_Checks_Suppressed (Param_Ent)
         and then not Accessibility_Checks_Suppressed (Typ)
      then
         Param_Level :=
           New_Occurrence_Of (Extra_Formal_Accessibility (Param_Ent), Loc);

         Type_Level :=
           Make_Integer_Literal (Loc, Type_Access_Level (Typ));

         --  Raise Program_Error if the accessibility level of the
         --  the access parameter is deeper than the level of the
         --  target access type.

         Insert_Action (N,
           Make_If_Statement (Loc,
             Condition =>
               Make_Op_Gt (Loc,
                 Left_Opnd  => Param_Level,
                 Right_Opnd => Type_Level),
             Then_Statements =>
               New_List (
                 Make_Raise_Statement (Loc,
                   Name => New_Reference_To (Standard_Program_Error, Loc)))));

         Analyze_And_Resolve (N);
      end if;
   end Apply_Accessibility_Check;

   -------------------------------------
   -- Apply_Arithmetic_Overflow_Check --
   -------------------------------------

   --  This routine is called only if the type is an integer type, and
   --  a software arithmetic overflow check must be performed for op
   --  (add, subtract, multiply). The check is performed only if
   --  Software_Overflow_Checking is enabled and Do_Overflow_Check
   --  is set. In this case we expand the operation into a more complex
   --  sequence of tests that ensures that overflow is properly caught.

   procedure Apply_Arithmetic_Overflow_Check (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      Rtyp  : constant Entity_Id  := Root_Type (Typ);
      Siz   : constant Int        := UI_To_Int (Esize (Rtyp));
      Dsiz  : constant Int        := Siz * 2;
      Opnod : Node_Id;
      Ctyp  : Entity_Id;
      Opnd  : Node_Id;
      Cent  : RE_Id;
      Lo    : Uint;
      Hi    : Uint;
      OK    : Boolean;

   begin
      if not Software_Overflow_Checking
        or else not Do_Overflow_Check (N)
        or else not Expander_Active
      then
         return;
      end if;

      --  Nothing to do if the range of the result is known OK

      Determine_Range (N, OK, Lo, Hi);

      if OK
        and then Lo >= Expr_Value (Type_Low_Bound  (Typ))
        and then Hi <= Expr_Value (Type_High_Bound (Typ))
      then
         return;
      end if;

      --  None of the special case optimizations worked, so there is nothing
      --  for it but to generate the full general case code:

      --    x op y

      --  is expanded into

      --    Typ (Checktyp (x) op Checktyp (y));

      --  where Typ is the type of the original expression, and Checktyp is
      --  an integer type of sufficient length to hold the largest possible
      --  result.

      --  In the case where check type exceeds the size of Long_Long_Integer,
      --  we use a different approach, expanding to:

      --    typ (xxx_With_Ovflo_Check (Integer_64 (x), Integer (y)))

      --  where xxx is Add, Multiply or Subtract as appropriate

      --  Find check type if one exists

      if Dsiz <= Standard_Integer_Size then
         Ctyp := Standard_Integer;

      elsif Dsiz <= Standard_Long_Long_Integer_Size then
         Ctyp := Standard_Long_Long_Integer;

      --  No check type exists, use runtime call

      else
         if Nkind (N) = N_Op_Add then
            Cent := RE_Add_With_Ovflo_Check;
         elsif Nkind (N) = N_Op_Multiply then
            Cent := RE_Multiply_With_Ovflo_Check;
         elsif Nkind (N) = N_Op_Subtract then
            Cent := RE_Subtract_With_Ovflo_Check;
         else
            pragma Assert (False); null;
         end if;

         Rewrite_Substitute_Tree (N,
           OK_Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Reference_To (RTE (Cent), Loc),
               Parameter_Associations => New_List (
                 OK_Convert_To (RTE (RE_Integer_64), Left_Opnd  (N)),
                 OK_Convert_To (RTE (RE_Integer_64), Right_Opnd (N))))));

         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  If we fall through, we have the case where we do the arithmetic in
      --  the next higher type and get the check by conversion. In these cases
      --  Ctyp is set to the type to be used as the check type.

      Opnod := Relocate_Node (N);

      Opnd := OK_Convert_To (Ctyp, Left_Opnd (Opnod));

      Analyze (Opnd);
      Set_Etype (Opnd, Ctyp);
      Set_Analyzed (Opnd, True);
      Set_Left_Opnd (Opnod, Opnd);

      Opnd := OK_Convert_To (Ctyp, Right_Opnd (Opnod));

      Analyze (Opnd);
      Set_Etype (Opnd, Ctyp);
      Set_Analyzed (Opnd, True);
      Set_Right_Opnd (Opnod, Opnd);

      --  The type of the operation changes to the base type of the check
      --  type, and we reset the overflow check indication, since clearly
      --  no overflow is possible now that we are using a double length
      --  type. We also set the Analyzed flag to avoid a recursive attempt
      --  to expand the node.

      Set_Etype             (Opnod, Base_Type (Ctyp));
      Set_Do_Overflow_Check (Opnod, False);
      Set_Analyzed          (Opnod, True);

      --  Now build the outer conversion

      Opnd := OK_Convert_To (Typ, Opnod);

      Analyze (Opnd);
      Set_Etype (Opnd, Typ);
      Set_Analyzed (Opnd, True);
      Set_Do_Overflow_Check (Opnd, True);

      Rewrite_Substitute_Tree (N, Opnd);
   end Apply_Arithmetic_Overflow_Check;

   --------------------------------------------
   -- Apply_Arithmetic_Divide_Overflow_Check --
   --------------------------------------------

   --  This routine is called only if the type is an integer type, and
   --  a software arithmetic overflow check must be performed for the
   --  given division operation. The only case to be checked is division
   --  of the largest negative number by minus one. The check is performed
   --  only if Software_Overflow_Checking is enabled and Do_Overflow_Check
   --  is set.

   procedure Apply_Arithmetic_Divide_Overflow_Check (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Base_Type (Etype (N));

   begin
      if not Software_Overflow_Checking
        or else not Do_Overflow_Check (N)
        or else not Expander_Active
        or else not Is_Signed_Integer_Type (Typ)
      then
         return;
      end if;

      Insert_Action (N,
        Make_Raise_Constraint_Error (Loc,
          Condition =>
            Make_And_Then (Loc,
              Left_Opnd =>
                Make_Op_Eq (Loc,
                  Left_Opnd  => Duplicate_Subexpr (Left_Opnd (N)),
                  Right_Opnd =>
                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_First,
                      Prefix => New_Occurrence_Of (Typ, Loc))),

              Right_Opnd =>
                Make_Op_Eq (Loc,
                  Left_Opnd  => Duplicate_Subexpr (Right_Opnd (N)),
                  Right_Opnd => Make_Integer_Literal (Loc, Uint_Minus_1)))));

   end Apply_Arithmetic_Divide_Overflow_Check;

   ----------------------------
   -- Apply_Array_Size_Check --
   ----------------------------

   --  Our criterion here is that the array size must fit in Integer'Last
   --  bits. Really of course this entre check should be in the backend,
   --  and perhaps this is not quite the right value, but it is good
   --  enough to catch the normal cases (and the relevant ACVC tests!)

   --  We only need to deal with the case where at least one index type
   --  is dynamic, because the backend handles the all static case fine.

   procedure Apply_Array_Size_Check (N : Node_Id; Typ : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Ctyp : constant Entity_Id  := Component_Type (Typ);
      Ent  : constant Entity_Id  := Defining_Identifier (N);
      Decl : Node_Id;
      Lo   : Node_Id;
      Hi   : Node_Id;
      Lob  : Uint;
      Hib  : Uint;
      Siz  : Uint;
      Xtyp : Entity_Id;
      Indx : Node_Id;
      Sizx : Node_Id;
      Code : Node_Id;

      Static : Boolean := True;
      --  Set false if any index subtye bound is non-static

      Umark : constant Uintp.Save_Mark := Uintp.Mark;
      --  We can throw away all the Uint computations here, since they are
      --  done only to generate boolean test results.

      function Is_Address_Or_Import (Decl : Node_Id) return Boolean;
      --  Determines if Decl is an address clause or Import/Interface pragma
      --  that references the defining identifier of the current declaration.

      --------------------------
      -- Is_Address_Or_Import --
      --------------------------

      function Is_Address_Or_Import (Decl : Node_Id) return Boolean is
      begin
         if Nkind (Decl) = N_At_Clause then
            return Chars (Identifier (Decl)) = Chars (Ent);

         elsif Nkind (Decl) = N_Attribute_Definition_Clause then
            return
              Chars (Decl) = Name_Address
                and then
              Nkind (Name (Decl)) = N_Identifier
                and then
              Chars (Name (Decl)) = Chars (Ent);

         elsif Nkind (Decl) = N_Pragma then
            if (Chars (Decl) = Name_Import
                 or else
                Chars (Decl) = Name_Interface)
              and then Present (Pragma_Argument_Associations (Decl))
            then
               declare
                  F : constant Node_Id :=
                        First (Pragma_Argument_Associations (Decl));

               begin
                  return
                    Present (F)
                      and then
                    Present (Next (F))
                      and then
                    Nkind (Expression (Next (F))) = N_Identifier
                      and then
                    Chars (Expression (Next (F))) = Chars (Ent);
               end;

            else
               return False;
            end if;

         else
            return False;
         end if;
      end Is_Address_Or_Import;

   --  Start of processing for Apply_Array_Size_Check

   begin
      if not Expander_Active
        or else Storage_Checks_Suppressed (Typ)
      then
         return;
      end if;

      --  It is pointless to insert this check inside an _init_proc, because
      --  that's too late, we have already built the object to be the right
      --  size, and if it's too large, too bad!

      if Inside_Init_Proc then
         return;
      end if;

      --  Look head for pragma interface/import or address clause applying
      --  to this entity. If found, we suppress the check entirely. For now
      --  we only look ahead 20 declarations to stop this becoming too slow
      --  Note that eventually this whole routine gets moved to gigi.

      Decl := N;
      for Ctr in 1 .. 20 loop
         Decl := Next (Decl);
         exit when No (Decl);

         if Is_Address_Or_Import (Decl) then
            return;
         end if;
      end loop;

      --  First step is to calculate the maximum number of elements. For this
      --  calculation, we use the actual size of the subtype if it is static,
      --  and if a bound of a subtype is non-static, we go to the bound of the
      --  base type.

      Siz := Uint_1;
      Indx := First_Index (Typ);
      while Present (Indx) loop
         Xtyp := Etype (Indx);
         Lo := Type_Low_Bound (Xtyp);
         Hi := Type_High_Bound (Xtyp);

         --  If any bound raises constraint error, we will never get this
         --  far, so there is no need to generate any kind of check.

         if Raises_Constraint_Error (Lo)
              or else
            Raises_Constraint_Error (Hi)
         then
            Uintp.Release (Umark);
            return;
         end if;

         --  Otherwise get bounds values

         if Is_Static_Expression (Lo) then
            Lob := Expr_Value (Lo);
         else
            Lob := Expr_Value (Type_Low_Bound (Base_Type (Xtyp)));
            Static := False;
         end if;

         if Is_Static_Expression (Hi) then
            Hib := Expr_Value (Hi);
         else
            Hib := Expr_Value (Type_High_Bound (Base_Type (Xtyp)));
            Static := False;
         end if;

         Siz := Siz *  UI_Max (Hib - Lob + 1, Uint_0);
         Indx := Next_Index (Indx);
      end loop;

      --  If we have all static bounds and Siz is greater than 2 ** 31 - 1
      --  then we know we have a storage error right now, so generate message

      if Static and then Siz >= Uint_2 ** 31 then
         Insert_Action (N,
           Make_Raise_Statement (Loc,
             Name => New_Occurrence_Of (Standard_Storage_Error, Loc)));
         Error_Msg_N ("?Storage_Error will be raised at run-time", N);
         Uintp.Release (Umark);
         return;
      end if;

      --  Case of component size known at compile time

      if Esize (Ctyp) /= 0 then

         --  If array size is definitely in range, then we need no check

         if Siz * Esize (Ctyp) < Uint_2 ** 31 then
            Uintp.Release (Umark);
            return;
         end if;

         --  If bounds are all static, and component size is known at
         --  compile time, then this is a case that gigi can handle fine
         --  (it is only the dynamic case that gigi currently messes up)

         if Static then
            Uintp.Release (Umark);
            return;
         end if;
      end if;

      --  Here if a dynamic check is required

      --  What we do is to build an expression for the size of the array,
      --  which is computed as the 'Size of the array component, times
      --  the size of each dimension.

      Uintp.Release (Umark);

      Sizx :=
        Make_Attribute_Reference (Loc,
          Prefix => New_Occurrence_Of (Ctyp, Loc),
          Attribute_Name => Name_Size);

      for J in 1 .. Number_Dimensions (Typ) loop
         Sizx :=
           Make_Op_Multiply (Loc,
             Left_Opnd  => Sizx,
             Right_Opnd =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Typ, Loc),
                 Attribute_Name => Name_Length,
                 Expressions => New_List (
                   Make_Integer_Literal (Loc, UI_From_Int (J)))));
      end loop;

      Code :=
        Make_If_Statement (Loc,
          Condition =>
            Make_Op_Ge (Loc,
              Left_Opnd  => Sizx,
              Right_Opnd =>
                Make_Integer_Literal (Loc,
                  Intval => Uint_2 ** (Standard_Integer_Size - 1))),

          Then_Statements => New_List (
            Make_Raise_Statement (Loc,
              Name => New_Occurrence_Of (Standard_Storage_Error, Loc))));

      Set_Size_Check_Code (Defining_Identifier (N), Code);
      Insert_Action (N, Code);

   end Apply_Array_Size_Check;

   ----------------------------
   -- Apply_Constraint_Check --
   ----------------------------

   procedure Apply_Constraint_Check
     (N          : Node_Id;
      Typ        : Entity_Id;
      No_Sliding : Boolean := False)
   is
      Desig_Typ : Entity_Id;

   begin
      if Is_Scalar_Type (Typ) then
         Apply_Scalar_Range_Check (N, Typ);

      elsif Is_Array_Type (Typ) then

         if Is_Constrained (Typ) then
            Apply_Length_Check (N, Typ);

            if No_Sliding then
               Apply_Range_Check (N, Typ);
            end if;
         else
            Apply_Range_Check (N, Typ);
         end if;

      elsif (Is_Record_Type (Typ)
          or else Is_Private_Type (Typ))
        and then Has_Discriminants (Base_Type (Typ))
        and then Is_Constrained (Typ)
      then
         Apply_Discriminant_Check (N, Typ);

      elsif Is_Access_Type (Typ) then

         Desig_Typ := Designated_Type (Typ);

         --  No checks necessary if expression statically null

         if Nkind (N) = N_Null then
            null;

         --  No sliding possible on access to arrays

         elsif Is_Array_Type (Desig_Typ) then
            if Is_Constrained (Desig_Typ) then
               Apply_Length_Check (N, Typ);
            end if;

            Apply_Range_Check (N, Typ);

         elsif Has_Discriminants (Base_Type (Desig_Typ))
            and then Is_Constrained (Desig_Typ)
         then
            Apply_Discriminant_Check (N, Typ);
         end if;
      end if;
   end Apply_Constraint_Check;

   ------------------------------
   -- Apply_Discriminant_Check --
   ------------------------------

   procedure Apply_Discriminant_Check
     (N   : Node_Id;
      Typ : Entity_Id;
      Lhs : Node_Id := Empty)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Do_Access : constant Boolean    := Is_Access_Type (Typ);
      Cond      : Node_Id;
      T_Typ     : Entity_Id;

   begin
      if Do_Access then
         T_Typ := Designated_Type (Typ);
      else
         T_Typ := Typ;
      end if;

      --  Nothing to do if discriminant checks are suppressed or else no code
      --  is to be generated

      if not Expander_Active
        or else Discriminant_Checks_Suppressed (T_Typ)
      then
         return;
      end if;

      --  No discriminant checks necessary for access when expression
      --  is statically Null. This is not only an optimization, this is
      --  fundamental because otherwise discriminant checks may be generated
      --  in init procs for types containing an access to a non-frozen yet
      --  record, causing a deadly forward reference.

      --  Also, if the expression is of an access type whose designated
      --  type is incomplete, then the access value must be null and
      --  we suppress the check.


      if Nkind (N) = N_Null then
         return;
      elsif Is_Access_Type (Etype (N))
        and then Ekind (Designated_Type (Etype (N))) = E_Incomplete_Type
      then
         return;
      end if;

      --  If an assignment target is present, then we need to generate
      --  the actual subtype if the target is a parameter or aliased
      --  object with an unconstrained nominal subtype.

      if Present (Lhs)
        and then (Present (Param_Entity (Lhs))
                   or else (not Is_Constrained (T_Typ)
                             and then Is_Aliased_View (Lhs)))
      then
         T_Typ := Get_Actual_Subtype (Lhs);
      end if;

      --  Nothing to do if the type is unconstrained (this is the case
      --  where the actual subtype in the RM sense of N is unconstrained
      --  and no check is required).

      if not Is_Constrained (T_Typ) then
         return;
      end if;

      --  Here we need a discriminant check. First build the expression
      --  for the comparisons of the discriminants:

      --    (n.disc1 /= typ.disc1) or else
      --    (n.disc2 /= typ.disc2) or else
      --     ...
      --    (n.discn /= typ.discn)

      Cond := Build_Discriminant_Checks (N, T_Typ);

      --  If Lhs is set and is a parameter, then the condition is
      --  guarded by: lhs'constrained and then (condition built above)

      if Present (Param_Entity (Lhs)) then
         Cond :=
           Make_And_Then (Loc,
             Left_Opnd =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Param_Entity (Lhs), Loc),
                 Attribute_Name => Name_Constrained),
             Right_Opnd => Cond);
      end if;

      if Do_Access then
         Cond := Guard_Access (Cond, Loc, N);
      end if;

      Insert_Action (N,
        Make_Raise_Constraint_Error (Loc, Condition => Cond));

   end Apply_Discriminant_Check;

   ------------------------
   -- Apply_Length_Check --
   ------------------------

   procedure Apply_Length_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty)
   is
   begin
      Apply_Selected_Length_Checks
        (Ck_Node, Target_Typ, Source_Typ, Do_Static => False);
   end Apply_Length_Check;

   -----------------------
   -- Apply_Range_Check --
   -----------------------

   procedure Apply_Range_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty)
   is
   begin
      Apply_Selected_Range_Checks
        (Ck_Node, Target_Typ, Source_Typ, Do_Static => False);
   end Apply_Range_Check;

   ------------------------------
   -- Apply_Scalar_Range_Check --
   ------------------------------

   --  Note that Apply_Scalar_Range_Check never turns the Do_Range_Check
   --  flag off if it is already set on.

   procedure Apply_Scalar_Range_Check
     (Expr       : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Fixed_Int  : Boolean   := False)
   is
      S_Typ : Entity_Id;

      Checks_On : constant Boolean :=
                    not (Index_Checks_Suppressed (Target_Typ)
                           and then
                         Range_Checks_Suppressed (Target_Typ))
                      or else Do_Range_Check (Expr);

      Int_Real : Boolean;
      --  Set to True if Expr should be regarded as a real value
      --  even though the type of Expr might be discrete.

   begin
      --  Execute this code even if the expander is inactive since the
      --  following does not insert any code (see the comment at the
      --  beginning of this package).

      if not Checks_On
        or else Target_Typ = Any_Type
        or else not Is_Scalar_Type (Target_Typ)
        or else Raises_Constraint_Error (Expr)
      then
         return;
      end if;

      if No (Source_Typ) then
         S_Typ := Etype (Expr);
      else
         S_Typ := Source_Typ;
      end if;

      if not Is_Scalar_Type (S_Typ) or else S_Typ = Any_Type then
         return;
      end if;

      Int_Real :=
        Is_Floating_Point_Type (S_Typ)
          or else (Is_Fixed_Point_Type (S_Typ) and then not Fixed_Int);

      --  Check if we can determine at compile time whether Expr is in the
      --  range of the target type. Note that if S_Typ is within the
      --  bounds of Target_Typ then this must be the case.

      if In_Subrange_Of (S_Typ, Target_Typ, Fixed_Int)
        or else Is_In_Range (Expr, Target_Typ, Fixed_Int, Int_Real)
      then
         null;

      elsif Is_Out_Of_Range (Expr, Target_Typ, Fixed_Int, Int_Real) then
         Apply_Compile_Time_Constraint_Error
           (Expr, "static value out of range of}?", Target_Typ);

      --  ??? We only need a runtime check if the target type is constrained
      --  (the predefined type Float is not for instance).
      --  so the following should really be
      --
      --    elsif Is_Constrained (Target_Typ) then
      --
      --  but it isn't because certain types do not have the Is_Constrained
      --  flag properly set (see 1503-003).

      else
         Set_Do_Range_Check (Expr);
      end if;

   end Apply_Scalar_Range_Check;

   ----------------------------------
   -- Apply_Selected_Length_Checks --
   ----------------------------------

   procedure Apply_Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean)
   is
      Cond     : Node_Id;
      R_Result : Check_Result;
      R_Cno    : Node_Id;

      Loc         : constant Source_Ptr := Sloc (Ck_Node);
      Checks_On   : constant Boolean :=
                      (not Index_Checks_Suppressed (Target_Typ))
                        or else
                      (not Length_Checks_Suppressed (Target_Typ));

   begin
      if not Expander_Active or else not Checks_On then
         return;
      end if;

      R_Result :=
        Selected_Length_Checks (Ck_Node, Target_Typ, Source_Typ, Empty);

      for J in 1 .. 2 loop

         R_Cno := R_Result (J);
         exit when No (R_Cno);

         --  If the item is a conditional raise of constraint error,
         --  then have a look at what check is being performed and
         --  ???

         if Nkind (R_Cno) = N_Raise_Constraint_Error
           and then Present (Condition (R_Cno))
         then
            Cond := Condition (R_Cno);

            if not Has_Dynamic_Length_Check (Ck_Node) then
               Insert_Action (Ck_Node, R_Cno);

               if not Do_Static then
                  Set_Has_Dynamic_Length_Check (Ck_Node);
               end if;

            end if;

            --  Output a warning if the condition is known to be True

            if Is_Entity_Name (Cond)
              and then Entity (Cond) = Standard_True
            then
               Apply_Compile_Time_Constraint_Error
                 (Ck_Node, "wrong length for array of}?", Target_Typ);

            --  If we were only doing a static check, or if checks are not
            --  on, then we want to delete the check, since it is not needed.
            --  We do this by replacing the if statement by a null statement

            elsif Do_Static or else not Checks_On then
               Rewrite_Substitute_Tree (R_Cno, Make_Null_Statement (Loc));
            end if;

         else
            Install_Static_Check (R_Cno, Loc);
         end if;

      end loop;

   end Apply_Selected_Length_Checks;

   ---------------------------------
   -- Apply_Selected_Range_Checks --
   ---------------------------------

   procedure Apply_Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean)
   is
      Cond     : Node_Id;
      R_Result : Check_Result;
      R_Cno    : Node_Id;

      Loc       : constant Source_Ptr := Sloc (Ck_Node);
      Checks_On : constant Boolean :=
                    (not Index_Checks_Suppressed (Target_Typ))
                      or else
                    (not Range_Checks_Suppressed (Target_Typ));

   begin
      if not Expander_Active or else not Checks_On then
         return;
      end if;

      R_Result :=
        Selected_Range_Checks (Ck_Node, Target_Typ, Source_Typ, Empty);

      for J in 1 .. 2 loop

         R_Cno := R_Result (J);
         exit when No (R_Cno);

         --  If the item is a conditional raise of constraint error,
         --  then have a look at what check is being performed and
         --  ???

         if Nkind (R_Cno) = N_Raise_Constraint_Error
           and then Present (Condition (R_Cno))
         then
            Cond := Condition (R_Cno);

            if not Has_Dynamic_Range_Check (Ck_Node) then
               Insert_Action (Ck_Node, R_Cno);

               if not Do_Static then
                  Set_Has_Dynamic_Range_Check (Ck_Node);
               end if;
            end if;

            --  Output a warning if the condition is known to be True

            if Is_Entity_Name (Cond)
              and then Entity (Cond) = Standard_True
            then
               --  Since an N_Range is technically not an expression, we
               --  have to set one of the bounds to C_E and then just flag
               --  the N_Range. The warning message will point to the
               --  lower bound and complain about a range, which seems OK.

               if Nkind (Ck_Node) = N_Range then
                  Apply_Compile_Time_Constraint_Error
                    (Low_Bound (Ck_Node),
                     "static range out of bounds of}?", Target_Typ);
                  Set_Raises_Constraint_Error (Ck_Node);

               else
                  Apply_Compile_Time_Constraint_Error
                    (Ck_Node, "static value out of range of}?", Target_Typ);
               end if;

            --  If we were only doing a static check, or if checks are not
            --  on, then we want to delete the check, since it is not needed.
            --  We do this by replacing the if statement by a null statement

            elsif Do_Static or else not Checks_On then
               Rewrite_Substitute_Tree (R_Cno, Make_Null_Statement (Loc));
            end if;

         else
            Install_Static_Check (R_Cno, Loc);
         end if;

      end loop;

   end Apply_Selected_Range_Checks;

   -------------------------------
   -- Apply_Static_Length_Check --
   -------------------------------

   procedure Apply_Static_Length_Check
     (Expr       : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty)
   is
   begin
      Apply_Selected_Length_Checks
        (Expr, Target_Typ, Source_Typ, Do_Static => True);
   end Apply_Static_Length_Check;

   ---------------------------------------
   -- Apply_Subscript_Conversion_Checks --
   ---------------------------------------

   procedure Apply_Subscript_Conversion_Checks (N : Node_Id) is
      Prefix_Type : constant Entity_Id := Etype (Prefix (N));
      Index       : Entity_Id;
      Expr        : Node_Id;
      Conv_Expr   : Node_Id;

   begin
      if not Expander_Active
        or else Index_Checks_Suppressed (Prefix_Type)
      then
         return;
      end if;

      --  Conversion checks need to be added only for unconstrained arrays
      --  or bit-packed arrays since otherwise the appropriate array bounds
      --  exist to make the index checks in calls to Apply_Range_Check as
      --  the indexes of the indexed component are resolved.

      if not Is_Array_Type (Prefix_Type) or else
        ((Is_Constrained (Prefix_Type)
           or else Ekind (Prefix_Type) = E_Enum_Table_Type)
          and then not Is_Bit_Packed_Array (Prefix_Type))
      then
         return;
      end if;

      Index := First_Index (Get_Actual_Subtype (Prefix (N)));
      Expr  := First (Expressions (N));

      --  For each subscript generate a type conversion to the corresponding
      --  actual subtype for the index.

      while Present (Index) loop
         if not Index_Checks_Suppressed (Etype (Index)) then
            Conv_Expr := Convert_To (Etype (Index), Expr);
            Rewrite_Substitute_Tree (Expr, Conv_Expr);
            Analyze_And_Resolve (Expr, Etype (Index));
         end if;

         Index := Next_Index (Index);
         Expr  := Next (Expr);
      end loop;
   end Apply_Subscript_Conversion_Checks;

   ----------------------------------
   -- Apply_Type_Conversion_Checks --
   ----------------------------------

   procedure Apply_Type_Conversion_Checks (N : Node_Id) is
      Target_Type : constant Entity_Id := Etype (N);
      Target_Base : constant Entity_Id := Base_Type (Target_Type);

      Expr      : constant Node_Id   := Expression (N);
      Expr_Type : constant Entity_Id := Etype (Expr);

   begin
      if Errors_Detected > 0 then
         return;
      end if;

      --  Scalar type conversions of the form Target_Type (Expr) require
      --  two checks:
      --
      --    - First there is an overflow check to insure that Expr is
      --      in the base type of Target_Typ (4.6 (28)),
      --
      --    - After we know Expr fits into the base type, we must perform a
      --      range check to ensure that Expr meets the constraints of the
      --      Target_Type.

      if Is_Scalar_Type (Target_Type) then
         declare
            Conv_OK  : constant Boolean := Conversion_OK (N);
            --  If the Conversion_OK flag on the type conversion is set
            --  and no floating point type is involved in the type conversion
            --  then fixed point values must be read as integral values.

         begin
            --  Overflow check.

            if not Overflow_Checks_Suppressed (Target_Base)
              and then not In_Subrange_Of (Expr_Type, Target_Base, Conv_OK)
            then
               Set_Do_Overflow_Check (N);
            end if;

            if not Range_Checks_Suppressed (Target_Type) then
               Apply_Scalar_Range_Check
                 (Expr, Target_Type, Fixed_Int => Conv_OK);
            end if;
         end;

      --  ??? shouldn't there be other checks here for other types
      --  such as arrays, records etc ....

      else
         null;
      end if;

   end Apply_Type_Conversion_Checks;

   ----------------------------------------------
   -- Apply_Universal_Integer_Attribute_Checks --
   ----------------------------------------------

   procedure Apply_Universal_Integer_Attribute_Checks (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

   begin
      --  Nothing to do if checks are suppressed

      if Range_Checks_Suppressed (Typ)
        and then Overflow_Checks_Suppressed (Typ)
      then
         return;

      --  Nothing to do if the attribute does not come from source. The
      --  internal attributes we generate of this type do not need checks,
      --  and furthermore the attempt to check them causes some circular
      --  elaboration orders when dealing with packed types.

      elsif not Comes_From_Source (N) then
         return;

      --  Otherwise, replace the attribute node with a type conversion
      --  node whose expression is the attribute, retyped to universal
      --  integer, and whose subtype mark is the target type. The call
      --  to analyze this conversion will set range and overflow checks
      --  as required for proper detection of an out of range value.

      else
         Set_Etype    (N, Universal_Integer);
         Set_Analyzed (N, True);

         Rewrite_Substitute_Tree (N,
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Occurrence_Of (Typ, Loc),
             Expression   => Relocate_Node (N)));

         Analyze_And_Resolve (N, Typ);
         return;
      end if;

   end Apply_Universal_Integer_Attribute_Checks;

   -----------------------------
   -- Apply_Zero_Divide_Check --
   -----------------------------

   procedure Apply_Zero_Divide_Check (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      if Expander_Active
        and then Do_Overflow_Check (N)
      then
         Insert_Action (N,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_Op_Eq (Loc,
                 Left_Opnd => Duplicate_Subexpr (Right_Opnd (N)),
                 Right_Opnd => Make_Integer_Literal (Loc, Uint_0))));
      end if;
   end Apply_Zero_Divide_Check;

   -------------------------------
   -- Build_Discriminant_Checks --
   -------------------------------

   function Build_Discriminant_Checks
     (N     : Node_Id;
      T_Typ : Entity_Id)
      return Node_Id
   is
      Loc      : constant Source_Ptr := Sloc (N);
      Cond     : Node_Id;
      Disc     : Elmt_Id;
      Disc_Ent : Entity_Id;
      Dval     : Node_Id;

   begin
      Cond := Empty;
      Disc := First_Elmt (Discriminant_Constraint (T_Typ));

      --  For a fully private type, use the discriminants of the parent
      --  type.

      if Is_Private_Type (T_Typ)
        and then No (Full_View (T_Typ))
      then
         Disc_Ent := First_Discriminant (Etype (Base_Type (T_Typ)));
      else
         Disc_Ent := First_Discriminant (T_Typ);
      end if;

      while Present (Disc) loop

         Dval := Node (Disc);

         if Nkind (Dval) = N_Identifier
           and then Ekind (Entity (Dval)) = E_Discriminant
         then
            Dval := New_Occurrence_Of (Discriminal (Entity (Dval)), Loc);
         else
            Dval := Duplicate_Subexpr (Dval);
         end if;

         Evolve_Or_Else (Cond,
           Make_Op_Ne (Loc,
             Left_Opnd =>
               Make_Selected_Component (Loc,
                 Prefix =>
                   Duplicate_Subexpr (N, Name_Req => True),
                 Selector_Name =>
                   Make_Identifier (Loc, Chars (Disc_Ent))),
             Right_Opnd => Dval));

         Disc := Next_Elmt (Disc);
         Disc_Ent := Next_Discriminant (Disc_Ent);
      end loop;

      return Cond;
   end Build_Discriminant_Checks;

   ---------------------
   -- Determine_Range --
   ---------------------

   procedure Determine_Range
     (N  : Node_Id;
      OK : out Boolean;
      Lo : out Uint;
      Hi : out Uint)
   is
   begin
      if Is_OK_Static_Expression (N) then
         Lo := Expr_Value (N);
         Hi := Lo;
         OK := True;

      else
         declare
            Lo_Left  : Uint;
            Lo_Right : Uint;
            Hi_Left  : Uint;
            Hi_Right : Uint;
            Typ      : Entity_Id;
            Bound    : Node_Id;

         begin
            case Nkind (N) is

               when N_Op_Plus =>
                  Determine_Range (Right_Opnd (N), OK, Lo, Hi);

               when N_Op_Minus =>
                  Determine_Range (Right_Opnd (N), OK, Lo_Right, Hi_Right);

                  if OK then
                     Lo := -Hi_Right;
                     Hi := -Lo_Right;
                  end if;

               when N_Op_Add =>
                  Determine_Range (Left_Opnd  (N), OK, Lo_Left,  Hi_Left);

                  if not OK then
                     return;
                  end if;

                  Determine_Range (Right_Opnd (N), OK, Lo_Right, Hi_Right);

                  if OK then
                     Lo := Lo_Left + Lo_Right;
                     Hi := Hi_Left + Hi_Right;
                  end if;

               when N_Op_Subtract =>
                  Determine_Range (Left_Opnd  (N), OK, Lo_Left,  Hi_Left);

                  if not OK then
                     return;
                  end if;

                  Determine_Range (Right_Opnd (N), OK, Lo_Right, Hi_Right);

                  if OK then
                     Lo := Lo_Left - Hi_Right;
                     Hi := Hi_Left - Lo_Right;
                  end if;

               when N_Op_Divide =>
                  OK := False;

               when N_Op_Multiply =>
                  OK := False;

               when others =>
                  Typ   := Implementation_Type (Etype (N));

                  Bound := Type_Low_Bound (Typ);

                  if Is_OK_Static_Expression (Bound) then
                     Lo := Expr_Value (Bound);
                  else
                     Lo := Expr_Value (Type_Low_Bound (Base_Type (Typ)));
                  end if;

                  Bound := Type_High_Bound (Typ);

                  if Is_OK_Static_Expression (Bound) then
                     Hi := Expr_Value (Bound);
                  else
                     Hi := Expr_Value (Type_Low_Bound (Base_Type (Typ)));
                  end if;

                  OK := True;

            end case;
         end;
      end if;

   end Determine_Range;

   ------------------------------------
   -- Discriminant_Checks_Suppressed --
   ------------------------------------

   function Discriminant_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Discriminant_Checks
        or else (Present (E) and then Suppress_Discriminant_Checks (E));
   end Discriminant_Checks_Suppressed;

   --------------------------------
   -- Division_Checks_Suppressed --
   --------------------------------

   function Division_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Division_Checks
        or else (Present (E) and then Suppress_Division_Checks (E));
   end Division_Checks_Suppressed;

   -----------------------------------
   -- Elaboration_Checks_Suppressed --
   -----------------------------------

   function Elaboration_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Elaboration_Checks
        or else (Present (E) and then Suppress_Elaboration_Checks (E));
   end Elaboration_Checks_Suppressed;

   ------------------
   -- Guard_Access --
   ------------------

   function Guard_Access
     (Cond    : Node_Id;
      Loc     : Source_Ptr;
      Ck_Node : Node_Id)
      return    Node_Id
   is
   begin
      if Nkind (Cond) = N_Or_Else then
         Set_Paren_Count (Cond, 1);
      end if;

      if Nkind (Ck_Node) = N_Allocator then
         return Cond;
      else
         return
           Make_And_Then (Loc,
             Left_Opnd =>
               Make_Op_Ne (Loc,
                 Left_Opnd  => Duplicate_Subexpr (Ck_Node),
                 Right_Opnd => Make_Null (Loc)),
             Right_Opnd => Cond);
      end if;
   end Guard_Access;

   -----------------------------
   -- Index_Checks_Suppressed --
   -----------------------------

   function Index_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Index_Checks
        or else (Present (E) and then Suppress_Index_Checks (E));
   end Index_Checks_Suppressed;

   -------------------------
   -- Insert_Range_Checks --
   -------------------------

   procedure Insert_Range_Checks
     (Checks       : Check_Result;
      Node         : Node_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr := No_Location;
      Flag_Node    : Node_Id    := Empty;
      Do_Before    : Boolean    := False)
   is
      Internal_Flag_Node   : Node_Id    := Flag_Node;
      Internal_Static_Sloc : Source_Ptr := Static_Sloc;

      Check_Node : Node_Id;
      Checks_On  : constant Boolean :=
                     (not Index_Checks_Suppressed (Suppress_Typ))
                       or else
                     (not Range_Checks_Suppressed (Suppress_Typ));
   begin
      --  For now we just return if Checks_On is false, however this should
      --  be enhanced to check for an always True value in the condition
      --  and to generate a compilation warning???

      if not Expander_Active or else not Checks_On then
         return;
      end if;

      if Static_Sloc = No_Location then
         Internal_Static_Sloc := Sloc (Node);
      end if;

      if No (Flag_Node) then
         Internal_Flag_Node := Node;
      end if;

      for J in 1 .. 2 loop
         exit when No (Checks (J));

         if Nkind (Checks (J)) = N_Raise_Constraint_Error
           and then Present (Condition (Checks (J)))
         then
            if not Has_Dynamic_Range_Check (Internal_Flag_Node) then
               Check_Node := Checks (J);
               Mark_Rewrite_Insertion (Check_Node);

               if Do_Before then
                  Insert_Before_And_Analyze (Node, Check_Node);
               else
                  Insert_After_And_Analyze (Node, Check_Node);
               end if;

               Set_Has_Dynamic_Range_Check (Internal_Flag_Node);
            end if;

         else
            Check_Node := Make_Raise_Constraint_Error (Internal_Static_Sloc);
            Mark_Rewrite_Insertion (Check_Node);

            if Do_Before then
               Insert_Before_And_Analyze (Node, Check_Node);
            else
               Insert_After_And_Analyze (Node, Check_Node);
            end if;
         end if;
      end loop;
   end Insert_Range_Checks;

   --------------------------
   -- Install_Static_Check --
   --------------------------

   procedure Install_Static_Check (R_Cno : Node_Id; Loc : Source_Ptr) is
      Stat : constant Boolean   := Is_Static_Expression (R_Cno);
      Typ  : constant Entity_Id := Etype (R_Cno);

   begin
      Rewrite_Substitute_Tree (R_Cno, Make_Raise_Constraint_Error (Loc));
      Set_Analyzed (R_Cno);
      Set_Etype (R_Cno, Typ);
      Set_Raises_Constraint_Error (R_Cno);
      Set_Is_Static_Expression (R_Cno, Stat);
   end Install_Static_Check;

   ------------------------------
   -- Length_Checks_Suppressed --
   ------------------------------

   function Length_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Length_Checks
        or else (Present (E) and then Suppress_Length_Checks (E));
   end Length_Checks_Suppressed;

   --------------------------------
   -- Overflow_Checks_Suppressed --
   --------------------------------

   function Overflow_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Overflow_Checks
        or else (Present (E) and then Suppress_Overflow_Checks (E));
   end Overflow_Checks_Suppressed;

   -----------------
   -- Range_Check --
   -----------------

   function Range_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Warn_Node  : Node_Id   := Empty)
      return       Check_Result
   is
   begin
      return Selected_Range_Checks
        (Ck_Node, Target_Typ, Source_Typ, Warn_Node);
   end Range_Check;

   -----------------------------
   -- Range_Checks_Suppressed --
   -----------------------------

   function Range_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Range_Checks
        or else (Present (E) and then Suppress_Range_Checks (E));
   end Range_Checks_Suppressed;

   ----------------------------
   -- Selected_Length_Checks --
   ----------------------------

   function Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id)
      return       Check_Result
   is
      Loc         : constant Source_Ptr := Sloc (Ck_Node);
      S_Typ       : Entity_Id;
      T_Typ       : Entity_Id;
      Expr_Actual : Node_Id;
      Exptyp      : Entity_Id;
      Cond        : Node_Id := Empty;
      Do_Access   : Boolean := False;
      Wnode       : Node_Id := Warn_Node;
      Ret_Result  : Check_Result := (Empty, Empty);
      Num_Checks  : Natural := 0;

      procedure Add_Check (N : Node_Id);
      --  Adds the action given to Ret_Result if N is non-Empty

      function Get_E_Length (E : Entity_Id; Indx : Nat) return Node_Id;
      function Get_N_Length (N : Node_Id; Indx : Nat) return Node_Id;

      function Length_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat)
         return   Node_Id;
      --  Returns expression to compute:
      --    Typ'Length /= Exptyp'Length

      function Length_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat)
         return Node_Id;
      --  Returns expression to compute:
      --    Typ'Length /= Expr'Length

      ---------------
      -- Add_Check --
      ---------------

      procedure Add_Check (N : Node_Id) is
      begin
         if Present (N) then

            --  For now, ignore attempt to place more than 2 checks ???

            if Num_Checks = 2 then
               return;
            end if;

            pragma Assert (Num_Checks <= 1);
            Num_Checks := Num_Checks + 1;
            Ret_Result (Num_Checks) := N;
         end if;
      end Add_Check;

      ------------------
      -- Get_E_Length --
      ------------------

      function Get_E_Length (E : Entity_Id; Indx : Nat) return Node_Id is
         N  : Node_Id;
         E1 : Entity_Id := E;

      begin
         if Ekind (Scope (E)) = E_Record_Type
           and then Has_Discriminants (Scope (E))
         then
            N := Build_Discriminal_Subtype_Of_Component (E);

            if Present (N) then
               Insert_Action (Ck_Node, N);
               E1 := Defining_Identifier (N);
            end if;
         end if;

         if Ekind (E1) = E_String_Literal_Subtype then
            return
              Make_Integer_Literal (Loc,
                Intval => String_Literal_Length (E1));

         else
            N :=
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Length,
                Prefix =>
                  New_Occurrence_Of (E1, Loc));

            if Indx > 1 then
               Set_Expressions (N, New_List (
                 Make_Integer_Literal (Loc, UI_From_Int (Indx))));
            end if;

            return N;

         end if;
      end Get_E_Length;

      ------------------
      -- Get_N_Length --
      ------------------

      function Get_N_Length (N : Node_Id; Indx : Nat) return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Length,
             Prefix =>
               Duplicate_Subexpr (N, Name_Req => True),
             Expressions => New_List (
               Make_Integer_Literal (Loc, UI_From_Int (Indx))));

      end Get_N_Length;

      -------------------
      -- Length_E_Cond --
      -------------------

      function Length_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat)
         return   Node_Id
      is
      begin
         return
           Make_Op_Ne (Loc,
             Left_Opnd  => Get_E_Length (Typ, Indx),
             Right_Opnd => Get_E_Length (Exptyp, Indx));

      end Length_E_Cond;

      -------------------
      -- Length_N_Cond --
      -------------------

      function Length_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat)
         return Node_Id
      is
      begin
         return
           Make_Op_Ne (Loc,
             Left_Opnd  => Get_E_Length (Typ, Indx),
             Right_Opnd => Get_N_Length (Expr, Indx));

      end Length_N_Cond;

   --  Start of processing for Selected_Length_Checks

   begin
      if not Expander_Active then
         return Ret_Result;
      end if;

      if Target_Typ = Any_Type
        or else Target_Typ = Any_Composite
        or else Raises_Constraint_Error (Ck_Node)
      then
         return Ret_Result;
      end if;

      if No (Wnode) then
         Wnode := Ck_Node;
      end if;

      T_Typ := Target_Typ;

      if No (Source_Typ) then
         S_Typ := Etype (Ck_Node);
      else
         S_Typ := Source_Typ;
      end if;

      if S_Typ = Any_Type or else S_Typ = Any_Composite then
         return Ret_Result;
      end if;

      if Is_Access_Type (T_Typ) and then Is_Access_Type (S_Typ) then
         S_Typ := Designated_Type (S_Typ);
         T_Typ := Designated_Type (T_Typ);
         Do_Access := True;
      end if;

      if Is_Array_Type (T_Typ) and then Is_Array_Type (S_Typ) then
         if Is_Constrained (T_Typ) then

            Expr_Actual := Get_Referenced_Object (Ck_Node);
            Exptyp      := Get_Actual_Subtype (Expr_Actual);

            if Is_Access_Type (Exptyp) then
               Exptyp := Designated_Type (Exptyp);
            end if;

            --  String_Literal case. This needs to be handled specially be-
            --  cause no index types are available for string literals. The
            --  condition is simply:

            --    T_Typ'Length = string-literal-length

            if Nkind (Expr_Actual) = N_String_Literal then
               Cond :=
                 Make_Op_Ne (Loc,
                   Left_Opnd  => Get_E_Length (T_Typ, 1),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc,
                       Intval =>
                         String_Literal_Length (Etype (Expr_Actual))));

            --  General array case. Here we have a usable actual subtype for
            --  the expression, and the condition is built from the two types
            --  (Do_Length):

            --     T_Typ'Length     /= Exptyp'Length     or else
            --     T_Typ'Length (2) /= Exptyp'Length (2) or else
            --     T_Typ'Length (3) /= Exptyp'Length (3) or else
            --     ...

            elsif Is_Constrained (Exptyp) then
               declare
                  L_Index : Node_Id;
                  R_Index : Node_Id;
                  Ndims   : Nat := Number_Dimensions (T_Typ);

                  L_Low  : Node_Id;
                  L_High : Node_Id;
                  R_Low  : Node_Id;
                  R_High : Node_Id;

                  L_Length : Uint;
                  R_Length : Uint;

               begin
                  L_Index := First_Index (T_Typ);
                  R_Index := First_Index (Exptyp);

                  for Indx in 1 .. Ndims loop
                     if not (Nkind (L_Index) = N_Raise_Constraint_Error
                       or else Nkind (R_Index) = N_Raise_Constraint_Error)
                     then
                        Get_Index_Bounds (L_Index, L_Low, L_High);
                        Get_Index_Bounds (R_Index, R_Low, R_High);

                        --  Deal with compile time length check. Note that we
                        --  skip this in the access case, because the access
                        --  value may be null, so we cannot know statically.

                        if not Do_Access
                          and then Compile_Time_Known_Value (L_Low)
                          and then Compile_Time_Known_Value (L_High)
                          and then Compile_Time_Known_Value (R_Low)
                          and then Compile_Time_Known_Value (R_High)
                        then
                           if Expr_Value (L_High) >= Expr_Value (L_Low) then
                              L_Length := Expr_Value (L_High) -
                                          Expr_Value (L_Low) + 1;
                           else
                              L_Length := UI_From_Int (0);
                           end if;

                           if Expr_Value (R_High) >= Expr_Value (R_Low) then
                              R_Length := Expr_Value (R_High) -
                                          Expr_Value (R_Low) + 1;
                           else
                              R_Length := UI_From_Int (0);
                           end if;

                           if L_Length > R_Length then
                              Add_Check
                                (Compile_Time_Constraint_Error
                                  (Wnode, "too few elements for}?", T_Typ));

                           elsif  L_Length < R_Length then
                              Add_Check
                                (Compile_Time_Constraint_Error
                                  (Wnode, "too many elements for}?", T_Typ));
                           end if;

                        --  The comparison for an individual index subtype
                        --  is omitted if the corresponding index subtypes
                        --  statically match, since the result is known to
                        --  be true. Note that this test is worth while even
                        --  though we do static evaluation, because non-static
                        --  subtypes can statically match.

                        elsif not
                          Subtypes_Statically_Match
                            (Etype (L_Index), Etype (R_Index))
                        then
                           Evolve_Or_Else
                             (Cond, Length_E_Cond (Exptyp, T_Typ, Indx));
                        end if;

                        L_Index := Next (L_Index);
                        R_Index := Next (R_Index);
                     end if;
                  end loop;
               end;

            --  Handle cases where we do not get a usable actual subtype that
            --  is constrained. This happens for example in the function call
            --  and explicit dereference cases. In these cases, we have to get
            --  the length or range from the expression itself, making sure we
            --  do not evaluate it more than once.

            --  Here Ck_Node is the original expression, or more properly the
            --  result of applying Duplicate_Expr to the original tree,
            --  forcing the result to be a name.

            else
               declare
                  Ndims   : Nat := Number_Dimensions (T_Typ);

               begin
                  --  Build the condition for the explicit dereference case

                  for Indx in 1 .. Ndims loop
                     Evolve_Or_Else
                       (Cond, Length_N_Cond (Ck_Node, T_Typ, Indx));
                  end loop;
               end;

            end if;

         end if;
      end if;

      --  Construct the test and insert into the tree

      if Present (Cond) then
         if Do_Access then
            Cond := Guard_Access (Cond, Loc, Ck_Node);
         end if;

         Add_Check (Make_Raise_Constraint_Error (Loc, Condition => Cond));
      end if;

      return Ret_Result;

   end Selected_Length_Checks;

   ---------------------------
   -- Selected_Range_Checks --
   ---------------------------

   function Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id)
      return       Check_Result
   is
      Loc         : constant Source_Ptr := Sloc (Ck_Node);
      S_Typ       : Entity_Id;
      T_Typ       : Entity_Id;
      Expr_Actual : Node_Id;
      Exptyp      : Entity_Id;
      Cond        : Node_Id := Empty;
      Do_Access   : Boolean := False;
      Wnode       : Node_Id  := Warn_Node;
      Ret_Result  : Check_Result := (Empty, Empty);
      Num_Checks  : Integer := 0;

      procedure Add_Check (N : Node_Id);
      --  Adds the action given to Ret_Result if N is non-Empty

      function Discrete_Range_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id)
         return Node_Id;
      --  Returns expression to compute:
      --    Low_Bound (Expr) < Typ'First
      --      or else
      --    High_Bound (Expr) > Typ'Last

      function Discrete_Expr_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id)
         return Node_Id;
      --  Returns expression to compute:
      --    Expr < Typ'First
      --      or else
      --    Expr > Typ'Last

      function Get_E_First_Or_Last
        (E    : Entity_Id;
         Indx : Nat;
         Nam  : Name_Id)
         return Node_Id;
      --  Returns expression to compute:
      --    E'First or E'Last

      function Get_N_First  (N : Node_Id; Indx : Nat) return Node_Id;
      function Get_N_Last   (N : Node_Id; Indx : Nat) return Node_Id;
      --  Returns expression to compute:
      --    N'First or N'Last using Duplicate_Subexpr

      function Range_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat)
         return   Node_Id;
      --  Returns expression to compute:
      --    Exptyp'First < Typ'First or else Exptyp'Last > Typ'Last

      function Range_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat)
         return Node_Id;
      --  Return expression to compute:
      --    Expr'First < Typ'First or else Expr'Last > Typ'Last

      ---------------
      -- Add_Check --
      ---------------

      procedure Add_Check (N : Node_Id) is
      begin
         if Present (N) then

            --  For now, ignore attempt to place more than 2 checks ???

            if Num_Checks = 2 then
               return;
            end if;

            pragma Assert (Num_Checks <= 1);
            Num_Checks := Num_Checks + 1;
            Ret_Result (Num_Checks) := N;
         end if;
      end Add_Check;

      -------------------------
      -- Discrete_Range_Cond --
      -------------------------

      function Discrete_Range_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id)
         return Node_Id
      is
         LB : Node_Id := Low_Bound (Expr);
         HB : Node_Id := High_Bound (Expr);

         Left_Opnd  : Node_Id;
         Right_Opnd : Node_Id;

      begin
         if Nkind (LB) = N_Identifier
           and then Ekind (Entity (LB)) = E_Discriminant then
            LB := New_Occurrence_Of (Discriminal (Entity (LB)), Loc);
         end if;

         if Nkind (HB) = N_Identifier
           and then Ekind (Entity (HB)) = E_Discriminant then
            HB := New_Occurrence_Of (Discriminal (Entity (HB)), Loc);
         end if;

         Left_Opnd :=
           Make_Op_Lt (Loc,
             Left_Opnd  =>
               Convert_To
                 (Base_Type (Typ), Duplicate_Subexpr (LB)),

             Right_Opnd =>
               Convert_To
                 (Base_Type (Typ), Get_E_First_Or_Last (Typ, 0, Name_First)));

         if Base_Type (Typ) = Typ then
            return Left_Opnd;

         elsif Compile_Time_Known_Value (High_Bound (Scalar_Range (Typ)))
            and then
               Compile_Time_Known_Value (High_Bound (Scalar_Range
                                                     (Base_Type (Typ))))
         then
            if Is_Floating_Point_Type (Typ) then
               if Expr_Value_R (High_Bound (Scalar_Range (Typ))) =
                  Expr_Value_R (High_Bound (Scalar_Range (Base_Type (Typ))))
               then
                  return Left_Opnd;
               end if;

            else
               if Expr_Value (High_Bound (Scalar_Range (Typ))) =
                  Expr_Value (High_Bound (Scalar_Range (Base_Type (Typ))))
               then
                  return Left_Opnd;
               end if;
            end if;
         end if;

         Right_Opnd :=
           Make_Op_Gt (Loc,
             Left_Opnd  =>
               Convert_To
                 (Base_Type (Typ), Duplicate_Subexpr (HB)),

             Right_Opnd =>
               Convert_To
                 (Base_Type (Typ),
                  Get_E_First_Or_Last (Typ, 0, Name_Last)));

         return Make_Or_Else (Loc, Left_Opnd, Right_Opnd);
      end Discrete_Range_Cond;

      -------------------------
      -- Discrete_Expr_Cond --
      -------------------------

      function Discrete_Expr_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id)
         return Node_Id
      is
      begin
         return
           Make_Or_Else (Loc,
             Left_Opnd =>
               Make_Op_Lt (Loc,
                 Left_Opnd =>
                   Convert_To (Base_Type (Typ), Duplicate_Subexpr (Expr)),
                 Right_Opnd =>
                   Convert_To (Base_Type (Typ),
                               Get_E_First_Or_Last (Typ, 0, Name_First))),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd =>
                   Convert_To (Base_Type (Typ), Duplicate_Subexpr (Expr)),
                 Right_Opnd =>
                   Convert_To
                     (Base_Type (Typ),
                      Get_E_First_Or_Last (Typ, 0, Name_Last))));
      end Discrete_Expr_Cond;

      -------------------------
      -- Get_E_First_Or_Last --
      -------------------------

      function Get_E_First_Or_Last
        (E    : Entity_Id;
         Indx : Nat;
         Nam  : Name_Id)
         return Node_Id
      is
         N  : Node_Id;
         LB : Node_Id;
         HB : Node_Id;

      begin
         if Is_Array_Type (E) then
            N := First_Index (E);

            for J in 2 .. Indx loop
               N := Next_Index (N);
            end loop;

         else
            N := Scalar_Range (E);
         end if;

         if Nkind (N) = N_Subtype_Indication then
            LB := Low_Bound (Range_Expression (Constraint (N)));
            HB := High_Bound (Range_Expression (Constraint (N)));

         elsif Is_Entity_Name (N) then
            LB := Type_Low_Bound  (Etype (N));
            HB := Type_High_Bound (Etype (N));

         else
            LB := Low_Bound  (N);
            HB := High_Bound (N);
         end if;

         if Nkind (LB) = N_Identifier
           and then Ekind (Entity (LB)) = E_Discriminant
         then
            LB := New_Occurrence_Of (Discriminal (Entity (LB)), Loc);
         elsif Nkind (LB) = N_Integer_Literal then
            LB :=  Make_Integer_Literal (Loc, Intval (LB));
         end if;

         if Nkind (HB) = N_Identifier
           and then Ekind (Entity (HB)) = E_Discriminant
         then
            HB := New_Occurrence_Of (Discriminal (Entity (HB)), Loc);
         elsif Nkind (HB) = N_Integer_Literal then
            HB :=  Make_Integer_Literal (Loc, Intval (HB));
         end if;

         if Nam = Name_First then
            return Duplicate_Subexpr (LB);
         else
            return Duplicate_Subexpr (HB);
         end if;

      end Get_E_First_Or_Last;

      -----------------
      -- Get_N_First --
      -----------------

      function Get_N_First (N : Node_Id; Indx : Nat) return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_First,
             Prefix =>
               Duplicate_Subexpr (N, Name_Req => True),
             Expressions => New_List (
               Make_Integer_Literal (Loc, UI_From_Int (Indx))));

      end Get_N_First;

      ----------------
      -- Get_N_Last --
      ----------------

      function Get_N_Last (N : Node_Id; Indx : Nat) return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Last,
             Prefix =>
               Duplicate_Subexpr (N, Name_Req => True),
             Expressions => New_List (
              Make_Integer_Literal (Loc, UI_From_Int (Indx))));

      end Get_N_Last;

      ------------------
      -- Range_E_Cond --
      ------------------

      function Range_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat)
         return   Node_Id
      is
      begin
         return
           Make_Or_Else (Loc,
             Left_Opnd =>
               Make_Op_Lt (Loc,
                 Left_Opnd => Get_E_First_Or_Last (Exptyp, Indx, Name_First),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_First)),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd => Get_E_First_Or_Last (Exptyp, Indx, Name_Last),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_Last)));

      end Range_E_Cond;

      ------------------
      -- Range_N_Cond --
      ------------------

      function Range_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat)
         return Node_Id
      is
      begin
         return
           Make_Or_Else (Loc,
             Left_Opnd =>
               Make_Op_Lt (Loc,
                 Left_Opnd => Get_N_First (Expr, Indx),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_First)),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd => Get_N_Last (Expr, Indx),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_Last)));
      end Range_N_Cond;

   --  Start of processing for Selected_Range_Checks

   begin
      if not Expander_Active then
         return Ret_Result;
      end if;

      if Target_Typ = Any_Type
        or else Target_Typ = Any_Composite
        or else Raises_Constraint_Error (Ck_Node)
      then
         return Ret_Result;
      end if;

      if No (Wnode) then
         Wnode := Ck_Node;
      end if;

      T_Typ := Target_Typ;

      if No (Source_Typ) then
         S_Typ := Etype (Ck_Node);
      else
         S_Typ := Source_Typ;
      end if;

      if S_Typ = Any_Type or else S_Typ = Any_Composite then
         return Ret_Result;
      end if;

      --  The order of evaluating T_Typ before S_Typ seems to be critical
      --  because S_Typ can be derived from Etype (Ck_Node), if it's not passed
      --  in, and since Node can be an N_Range node, it might be invalid.
      --  Should there be an assert check somewhere for taking the Etype of
      --  an N_Range node ???

      if Is_Access_Type (T_Typ) and then Is_Access_Type (S_Typ) then
         S_Typ := Designated_Type (S_Typ);
         T_Typ := Designated_Type (T_Typ);
         Do_Access := True;
      end if;

      --  For an N_Range Node, check for a null range and then if not
      --  null generate a range check action.

      if Nkind (Ck_Node) = N_Range then
         --  There's no point in checking a range against itself.

         if Ck_Node = Scalar_Range (T_Typ) then
            return Ret_Result;
         end if;

         declare
            T_LB       : constant Node_Id := Type_Low_Bound  (T_Typ);
            T_HB       : constant Node_Id := Type_High_Bound (T_Typ);
            LB         : Node_Id := Low_Bound (Ck_Node);
            HB         : Node_Id := High_Bound (Ck_Node);
            Null_Range : Boolean;

            Out_Of_Range_L : Boolean;
            Out_Of_Range_H : Boolean;

         begin
            --  Check for case where everything is static and we can
            --  do the check at compile time. This is skipped if we
            --  have an access type, since the access value may be null.

            --  ??? This code can be improved since you only need to know
            --  that the two respective bounds (LB & T_LB or HB & T_HB)
            --  are known at compile time to emit pertinent messages.

            if Compile_Time_Known_Value (LB)
              and then Compile_Time_Known_Value (HB)
              and then Compile_Time_Known_Value (T_LB)
              and then Compile_Time_Known_Value (T_HB)
              and then not Do_Access
            then
               --  Floating-point case

               if Is_Floating_Point_Type (S_Typ) then
                  Null_Range := Expr_Value_R (HB) < Expr_Value_R (LB);
                  Out_Of_Range_L :=
                    (Expr_Value_R (LB) < Expr_Value_R (T_LB))
                       or else
                    (Expr_Value_R (LB) > Expr_Value_R (T_HB));

                  Out_Of_Range_H :=
                    (Expr_Value_R (HB) > Expr_Value_R (T_HB))
                       or else
                    (Expr_Value_R (HB) < Expr_Value_R (T_LB));

               --  Fixed or discrete type case

               else
                  Null_Range := Expr_Value (HB) < Expr_Value (LB);
                  Out_Of_Range_L :=
                    (Expr_Value (LB) < Expr_Value (T_LB))
                    or else
                    (Expr_Value (LB) > Expr_Value (T_HB));

                  Out_Of_Range_H :=
                    (Expr_Value (HB) > Expr_Value (T_HB))
                    or else
                    (Expr_Value (HB) < Expr_Value (T_LB));
               end if;

               if not Null_Range then
                  if Out_Of_Range_L then
                     if No (Warn_Node) then
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Low_Bound (Ck_Node),
                              "static value out of range of}?", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                            (Wnode,
                             "static range out of bounds of}?", T_Typ));
                     end if;
                  end if;

                  if Out_Of_Range_H then
                     if No (Warn_Node) then
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (High_Bound (Ck_Node),
                              "static value out of range of}?", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Wnode,
                              "static range out of bounds of}?", T_Typ));
                     end if;
                  end if;

               end if;

            else

               declare
                  LB : Node_Id := Low_Bound (Ck_Node);
                  HB : Node_Id := High_Bound (Ck_Node);

               begin

                  --  If either bound is a discriminant and we are within
                  --  the record declaration, it is a use of the discriminant
                  --  in a constraint of a component, and nothing can be
                  --  checked here. The check will be emitted within the
                  --  init_proc. Before then, the discriminal has no real
                  --  meaning.

                  if Nkind (LB) = N_Identifier
                    and then Ekind (Entity (LB)) = E_Discriminant
                  then
                     if Current_Scope = Scope (Entity (LB)) then
                        return Ret_Result;
                     else
                        LB :=
                          New_Occurrence_Of (Discriminal (Entity (LB)), Loc);
                     end if;
                  end if;

                  if Nkind (HB) = N_Identifier
                    and then Ekind (Entity (HB)) = E_Discriminant
                  then
                     if Current_Scope = Scope (Entity (HB)) then
                        return Ret_Result;
                     else
                        HB :=
                          New_Occurrence_Of (Discriminal (Entity (HB)), Loc);
                     end if;
                  end if;

                  Cond := Discrete_Range_Cond (Ck_Node, T_Typ);
                  Set_Paren_Count (Cond, 1);

                  Cond :=
                    Make_And_Then (Loc,
                      Left_Opnd =>
                        Make_Op_Ge (Loc,
                          Left_Opnd  => Duplicate_Subexpr (HB),
                          Right_Opnd => Duplicate_Subexpr (LB)),
                      Right_Opnd => Cond);
               end;

            end if;
         end;

      elsif Is_Scalar_Type (S_Typ) then

         --  This somewhat duplicates what Apply_Scalar_Range_Check does,
         --  except the the above simply sets a flag in the node and lets
         --  gigi generate the check base on the Etype of the expression.
         --  Sometimes, however we want to do a dynamic check against an
         --  arbitrary target type, so we do that here.

         if Ekind (Base_Type (S_Typ)) /= Ekind (Base_Type (T_Typ)) then
            Cond := Discrete_Expr_Cond (Ck_Node, T_Typ);

         --  For literals, we can tell if the constraint error will be
         --  raised at compile time, so we never need a dynamic check, but
         --  if the exception will be raised, then post the usual warning,
         --  and replace the literal with a raise constraint error
         --  expression. As usual, skip this for access types

         elsif Compile_Time_Known_Value (Ck_Node)
           and then not Do_Access
         then
            declare
               LB : constant Node_Id := Type_Low_Bound (T_Typ);
               UB : constant Node_Id := Type_High_Bound (T_Typ);

               Out_Of_Range  : Boolean;
               Static_Bounds : constant Boolean :=
                                 Compile_Time_Known_Value (LB)
                                   and Compile_Time_Known_Value (UB);

            begin
               --  Following range tests should use Sem_Eval routine ???

               if Static_Bounds then
                  if Is_Floating_Point_Type (S_Typ) then
                     Out_Of_Range :=
                       (Expr_Value_R (Ck_Node) < Expr_Value_R (LB))
                         or else
                       (Expr_Value_R (Ck_Node) > Expr_Value_R (UB));

                  else -- fixed or discrete type
                     Out_Of_Range :=
                       Expr_Value (Ck_Node) < Expr_Value (LB)
                         or else
                       Expr_Value (Ck_Node) > Expr_Value (UB);
                  end if;

                  --  Bounds of the type are static and the literal is
                  --  out of range so make a warning message.

                  if Out_Of_Range then
                     if No (Warn_Node) then
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Ck_Node,
                              "static value out of range of}?", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Wnode,
                              "static value out of range of}?", T_Typ));
                     end if;
                  end if;

               else
                  Cond := Discrete_Expr_Cond (Ck_Node, T_Typ);
               end if;
            end;

         --  Here for the case of a non-static expression, we need a runtime
         --  check unless the source type range is guaranteed to be in the
         --  range of the target type.

         else
            if not In_Subrange_Of (S_Typ, T_Typ) then
               Cond := Discrete_Expr_Cond (Ck_Node, T_Typ);
            end if;
         end if;
      end if;

      if Is_Array_Type (T_Typ) and then Is_Array_Type (S_Typ) then
         if Is_Constrained (T_Typ) then

            Expr_Actual := Get_Referenced_Object (Ck_Node);
            Exptyp      := Get_Actual_Subtype (Expr_Actual);

            if Is_Access_Type (Exptyp) then
               Exptyp := Designated_Type (Exptyp);
            end if;

            --  String_Literal case. This needs to be handled specially be-
            --  cause no index types are available for string literals. The
            --  condition is simply:

            --    T_Typ'Length = string-literal-length

            if Nkind (Expr_Actual) = N_String_Literal then

               null;

            --  General array case. Here we have a usable actual subtype for
            --  the expression, and the condition is built from the two types

            --     T_Typ'First     < Exptyp'First     or else
            --     T_Typ'Last      > Exptyp'Last      or else
            --     T_Typ'First(1)  < Exptyp'First(1)  or else
            --     T_Typ'Last(1)   > Exptyp'Last(1)   or else
            --     ...

            elsif Is_Constrained (Exptyp) then
               declare
                  L_Index : Node_Id;
                  R_Index : Node_Id;
                  Ndims   : Nat := Number_Dimensions (T_Typ);

                  L_Low  : Node_Id;
                  L_High : Node_Id;
                  R_Low  : Node_Id;
                  R_High : Node_Id;

               begin
                  L_Index := First_Index (T_Typ);
                  R_Index := First_Index (Exptyp);

                  for Indx in 1 .. Ndims loop
                     if not (Nkind (L_Index) = N_Raise_Constraint_Error
                       or else Nkind (R_Index) = N_Raise_Constraint_Error)
                     then
                        Get_Index_Bounds (L_Index, L_Low, L_High);
                        Get_Index_Bounds (R_Index, R_Low, R_High);

                        --  Deal with compile time length check. Note that we
                        --  skip this in the access case, because the access
                        --  value may be null, so we cannot know statically.

                        if not
                          Subtypes_Statically_Match
                            (Etype (L_Index), Etype (R_Index))
                        then
                           Evolve_Or_Else
                             (Cond, Range_E_Cond (Exptyp, T_Typ, Indx));

                        end if;

                        L_Index := Next (L_Index);
                        R_Index := Next (R_Index);

                     end if;
                  end loop;
               end;

            --  Handle cases where we do not get a usable actual subtype that
            --  is constrained. This happens for example in the function call
            --  and explicit dereference cases. In these cases, we have to get
            --  the length or range from the expression itself, making sure we
            --  do not evaluate it more than once.

            --  Here Ck_Node is the original expression, or more properly the
            --  result of applying Duplicate_Expr to the original tree,
            --  forcing the result to be a name.

            else
               declare
                  Ndims   : Nat := Number_Dimensions (T_Typ);

               begin
                  --  Build the condition for the explicit dereference case

                  for Indx in 1 .. Ndims loop
                     Evolve_Or_Else
                       (Cond, Range_N_Cond (Ck_Node, T_Typ, Indx));
                  end loop;
               end;

            end if;

         else
            --  Generate an Action to check that the bounds of the
            --  source value are within the constraints imposed by the
            --  target type for a conversion to an unconstrained type.
            --  Rule is 4.6(38).

            if Nkind (Parent (Ck_Node)) = N_Type_Conversion then
               declare
                  Opnd_Index : Node_Id;
                  Targ_Index : Node_Id;

               begin
                  Opnd_Index
                    := First_Index (Get_Actual_Subtype (Ck_Node));
                  Targ_Index := First_Index (T_Typ);

                  while Opnd_Index /= Empty loop
                     if Nkind (Opnd_Index) = N_Range then
                        if Is_In_Range
                             (Low_Bound (Opnd_Index), Etype (Targ_Index))
                          and then
                            Is_In_Range
                             (High_Bound (Opnd_Index), Etype (Targ_Index))
                        then
                           null;

                        elsif Is_Out_Of_Range
                                (Low_Bound (Opnd_Index), Etype (Targ_Index))
                          or else
                              Is_Out_Of_Range
                                (High_Bound (Opnd_Index), Etype (Targ_Index))
                        then
                           Add_Check
                             (Compile_Time_Constraint_Error
                               (Wnode, "value out of range of}?", T_Typ));

                        else
                           Evolve_Or_Else
                             (Cond,
                              Discrete_Range_Cond
                                (Opnd_Index, Etype (Targ_Index)));
                        end if;
                     end if;

                     Opnd_Index := Next_Index (Opnd_Index);
                     Targ_Index := Next_Index (Targ_Index);
                  end loop;
               end;
            end if;
         end if;
      end if;

      --  Construct the test and insert into the tree

      if Present (Cond) then
         if Do_Access then
            Cond := Guard_Access (Cond, Loc, Ck_Node);
         end if;

         Add_Check (Make_Raise_Constraint_Error (Loc, Condition => Cond));
      end if;

      return Ret_Result;

   end Selected_Range_Checks;

   -------------------------------
   -- Storage_Checks_Suppressed --
   -------------------------------

   function Storage_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Storage_Checks
        or else (Present (E) and then Suppress_Storage_Checks (E));
   end Storage_Checks_Suppressed;

   ---------------------------
   -- Tag_Checks_Suppressed --
   ---------------------------

   function Tag_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Tag_Checks
        or else (Present (E) and then Suppress_Tag_Checks (E));
   end Tag_Checks_Suppressed;


end Checks;

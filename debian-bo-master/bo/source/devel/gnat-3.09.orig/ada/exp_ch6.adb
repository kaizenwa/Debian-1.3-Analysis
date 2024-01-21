------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 6                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.199 $                            --
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
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Ch2;  use Exp_Ch2;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Disp; use Exp_Disp;
with Exp_Intr; use Exp_Intr;
with Exp_Pakd; use Exp_Pakd;
with Exp_TSS;  use Exp_TSS;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Inline;   use Inline;
with Lib.Writ; use Lib.Writ;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Disp; use Sem_Disp;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch6 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Detect_Infinite_Recursion (N : Node_Id; Spec : Entity_Id);
   --  This procedure is called only if the subprogram body N, whose spec
   --  has the given entity Spec, contains a parameterless recursive call.
   --  It attempts to generate runtime code to detect if this a case of
   --  infinite recursion.
   --
   --  The body is scanned to determine dependencies. If the only external
   --  dependencies are on a small set of scalar variables, then the values
   --  of these variables are captured on entry to the subprogram, and if
   --  the values are not changed for the call, we know immediately that
   --  we have an infinite recursion.

   procedure Expand_Actuals (N : Node_Id; Subp : Entity_Id);
   --  For each actual of an in-out parameter which is a numeric conversion
   --  of the form T(A), where A denotes a variable, we insert the declaration:
   --
   --  Temp : T := T(A);
   --
   --  prior to the call. Then we replace the actual with a reference to Temp,
   --  and append the assignment:
   --
   --  A := T' (Temp);
   --
   --  after the call. Here T' is the actual type of variable A.
   --  For out parameters, the initial declaration has no expression.
   --  If A is not an entity name,  we generate instead:
   --
   --  Var  : T' renames A;
   --  Temp : T := Var;       --  omitting expression for out parameter.
   --  ...
   --  Var := T' (Temp);
   --
   --  For other in-out parameters, we emit the required constraint checks
   --  before and/or after the call.

   procedure Expand_Protected_Subprogram_Call
     (N    : Node_Id;
      Subp : Entity_Id;
      Scop : Entity_Id);
   --  A call to a protected subprogram within the protected object may appear
   --  as a regular call. The list of actuals must be expanded to contain a
   --  reference to the object itself, and the call becomes a call to the
   --  corresponding protected subprogram.

   -------------------------------
   -- Detect_Infinite_Recursion --
   -------------------------------

   procedure Detect_Infinite_Recursion (N : Node_Id; Spec : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Var_List : Elist_Id := New_Elmt_List;
      --  List of globals referenced by body of procedure

      Call_List : Elist_Id := New_Elmt_List;
      --  List of recursive calls in body of procedure

      Shad_List : Elist_Id := New_Elmt_List;
      --  List of entity id's for entities created to capture the
      --  value of referenced globals on entry to the procedure.

      Scop : constant Uint := Scope_Depth (Spec);
      --  This is used to record the scope depth of the current
      --  procedure, so that we can identify global references.

      Forget_It : Boolean := False;
      --  The flag Forget_It is set if we detect a global reference to
      --  than a scalar object, which means that we cannot safely expand
      --  the detection code.

      Max_Vars : constant := 4;
      --  Do not test more than four global variables

      Count_Vars : Natural := 0;
      --  Count variables found so far

      Var  : Entity_Id;
      Elm  : Elmt_Id;
      Ent  : Entity_Id;
      Call : Elmt_Id;
      Decl : Node_Id;
      Test : Node_Id;
      Elm1 : Elmt_Id;
      Elm2 : Elmt_Id;
      Last : Node_Id;

      procedure Visit_Node (Nod : Node_Id);
      --  Visit a single node while traversing tree of body

      procedure Visit_List (Lst : List_Id);
      --  Visit a list while traversing tree of body

      procedure Visit_Field (Nod : Node_Id; Fld : Union_Id);
      --  Given node Nod, visit one of its fields Fld

      procedure Visit_Field (Nod : Node_Id; Fld : Union_Id) is
      begin
         if Fld = Union_Id (Empty) then
            return;

         elsif Fld in Node_Range then
            if Parent (Node_Id (Fld)) = Nod then
               Visit_Node (Node_Id (Fld));
            end if;

         elsif Fld in List_Range then
            if Parent (List_Id (Fld)) = Nod then
               Visit_List (List_Id (Fld));
            end if;
         end if;
      end Visit_Field;

      procedure Visit_List (Lst : List_Id) is
         LN : Node_Id;

      begin
         LN := First (List_Id (Lst));
         while Present (LN) loop
            Visit_Node (LN);
            LN := Next (LN);
         end loop;
      end Visit_List;

      procedure Visit_Node (Nod : Node_Id) is
         use Atree.Unchecked_Access;

      begin
         --  Case of one of the detected recursive calls

         if Nkind (Nod) = N_Procedure_Call_Statement
           and then Has_Recursive_Call (Entity (Name (Nod)))
           and then Entity (Name (Nod)) = Spec
         then
            Append_Elmt (Nod, Call_List);
            return;

         --  A call to a pure function can always be ignored

         elsif Nkind (Nod) = N_Function_Call
           and then Is_Pure (Entity (Name (Nod)))
         then
            return;

         --  Case of an identifier reference

         elsif Nkind (Nod) = N_Identifier then
            Ent := Entity (Nod);

            --  If no entity, then ignore the reference

            --  Not clear why this can happen. To investigate, remove this
            --  test and look at the crash that occurs here in 3401-004 ???

            if No (Ent) then
               null;

            --  Ignore the reference if not to a more global object

            elsif Scope_Depth (Scope (Ent)) >= Scop then
               null;

            --  References to types, exceptions and constants are always OK

            elsif Is_Type (Ent)
              or else Ekind (Ent) = E_Exception
              or else Ekind (Ent) = E_Constant
            then
               null;

            --  If other than a non-volatile scalar variable, we have some
            --  kind of global reference (e.g. to a function) that we cannot
            --  deal with so we forget the attempt.

            elsif Ekind (Ent) /= E_Variable
              or else not Is_Scalar_Type (Etype (Ent))
              or else Is_Volatile (Ent)
            then
               Forget_It := True;
               return;

            --  Otherwise we have a reference to a global scalar

            else
               --  Loop through global entities already detected

               Elm := First_Elmt (Var_List);
               loop
                  --  If not detected before, record this new global reference

                  if No (Elm) then
                     Count_Vars := Count_Vars + 1;

                     if Count_Vars <= Max_Vars then
                        Append_Elmt (Entity (Nod), Var_List);
                     else
                        Forget_It := True;
                     end if;

                     exit;

                  --  If recorded before, ignore

                  elsif Node (Elm) = Entity (Nod) then
                     exit;

                  --  Otherwise keep looking

                  else
                     Elm := Next_Elmt (Elm);
                  end if;
               end loop;
            end if;

         --  For all other node kinds, recursively visit syntactic children

         else
            Visit_Field (Nod, Union_Id (Field1 (Nod)));
            Visit_Field (Nod, Union_Id (Field2 (Nod)));
            Visit_Field (Nod, Union_Id (Field3 (Nod)));
            Visit_Field (Nod, Union_Id (Field4 (Nod)));
            Visit_Field (Nod, Union_Id (Field5 (Nod)));
         end if;
      end Visit_Node;

   --  Start of processing for Detect_Infinite_Recursion

   begin
      Visit_Node (Handled_Statement_Sequence (N));
      Visit_List (Declarations (N));

      if Forget_It then
         return;
      end if;

      --  We must have a call, since Has_Recursive_Call was set

      pragma Assert (not Is_Empty_Elmt_List (Call_List));

      --  Push our current scope for analyzing the declarations and
      --  code that we will insert for the checking.

      New_Scope (Spec);

      --  This loop builds temporary variables for each of the
      --  referenced globals, so that at the end of the loop the
      --  list Shad_List contains these temporaries in one-to-one
      --  correspondence with the elements in Var_List.

      Last := Empty;
      Elm := First_Elmt (Var_List);
      while Present (Elm) loop
         Var := Node (Elm);
         Ent :=
           Make_Defining_Identifier (Loc,
             Chars => New_Internal_Name ('S'));
         Append_Elmt (Ent, Shad_List);

         --  Insert a declaration for this temporary at the start of
         --  the declarations for the procedure. The temporaries are
         --  declared as constant objects initialized to the current
         --  values of the corresponding temporaries.

         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Ent,
             Object_Definition   => New_Occurrence_Of (Etype (Var), Loc),
             Constant_Present    => True,
             Expression          => New_Occurrence_Of (Var, Loc));

         if No (Last) then
            Prepend (Decl, Declarations (N));
         else
            Insert_After (Last, Decl);
         end if;

         Last := Decl;
         Analyze (Decl);
         Elm := Next_Elmt (Elm);
      end loop;

      --  Loop through calls

      Call := First_Elmt (Call_List);
      while Present (Call) loop

         --  Build a predicate expression of the form

         --    True
         --      and then global1 = temp1
         --      and then global2 = temp2
         --      ...

         --  This predicate determines if any of the global values
         --  referenced by the procedure have changed since the
         --  current call, if not an infinite recursion is assured.

         Test := New_Occurrence_Of (Standard_True, Loc);

         Elm1 := First_Elmt (Var_List);
         Elm2 := First_Elmt (Shad_List);
         while Present (Elm1) loop
            Test :=
              Make_And_Then (Loc,
                Left_Opnd  => Test,
                Right_Opnd =>
                  Make_Op_Eq (Loc,
                    Left_Opnd  => New_Occurrence_Of (Node (Elm1), Loc),
                    Right_Opnd => New_Occurrence_Of (Node (Elm2), Loc)));

            Elm1 := Next_Elmt (Elm1);
            Elm2 := Next_Elmt (Elm2);
         end loop;

         --  Now we replace the call with the sequence

         --    if no-changes (see above) then
         --       raise Storage_Error;
         --    else
         --       original-call
         --    end if;

         Rewrite_Substitute_Tree (Node (Call),
           Make_If_Statement (Loc,
             Condition       => Test,
             Then_Statements => New_List (
               Make_Raise_Statement (Loc,
                 Name =>
                   New_Occurrence_Of (Standard_Storage_Error, Loc))),

             Else_Statements => New_List (
               Relocate_Node (Node (Call)))));

         Analyze (Node (Call));

         Call := Next_Elmt (Call);
      end loop;

      --  Remove temporary scope stack entry used for analysis

      Pop_Scope;
   end Detect_Infinite_Recursion;

   --------------------
   -- Expand_Actuals --
   --------------------

   procedure Expand_Actuals (N : Node_Id; Subp : Entity_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Actual    : Node_Id;
      Formal    : Entity_Id;
      N_Node    : Node_Id;
      Post_Call : List_Id;
      E_Formal  : Entity_Id;

      procedure Add_Call_By_Copy_Code;
      --  For In and In-Out parameters, where the parameter must be passed
      --  by copy, this routine generates a temporary variable into which
      --  the actual is copied, and then passes this as the parameter. This
      --  routine also takes care of any constraint checks required for the
      --  type conversion case (on both the way in and the way out). Note
      --  that we only need to do this manual copying for cases other than
      --  simple scalar types, since the case of simple scalars is handled
      --  by the back end.

      function Is_Ref_To_Component_With_Component_Clause
        (Act  : Node_Id)
         return Boolean;
      --  Determines if Act represents a name that is a reference to a
      --  record component to which a component clause applies. Any such
      --  reference is passed by copy-in, copy-out, since the component
      --  clause may disturb the natural alignment required for a call
      --  by reference.

      ---------------------------
      -- Add_Call_By_Copy_Code --
      ---------------------------

      procedure Add_Call_By_Copy_Code is
         Expr   : Node_Id;
         Init   : Node_Id;
         Temp   : Entity_Id;
         Var    : Entity_Id;
         V_Typ  : Entity_Id;
         Crep   : Boolean;

         function Make_Var (Actual : Node_Id) return Entity_Id;
         --  Returns an entity that refers to the given actual parameter,
         --  Actual (not including any type conversion). If Actual is an
         --  entity name, then this entity is returned unchanged, otherwise
         --  a renaming is created to provide an entity for the actual.

         --------------
         -- Make_Var --
         --------------

         function Make_Var (Actual : Node_Id) return Entity_Id is
            Var : Entity_Id;

         begin
            if Is_Entity_Name (Actual) then
               return Entity (Actual);

            else
               Var := Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

               N_Node :=
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier => Var,
                   Subtype_Mark        =>
                     New_Occurrence_Of (Etype (Actual), Loc),
                   Name                => Relocate_Node (Actual));

               Insert_Action (N, N_Node);
               return Var;
            end if;
         end Make_Var;

      --  Start of processing for Add_Call_By_Copy_Code

      begin
         Temp := Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

         if Nkind (Actual) = N_Type_Conversion then
            V_Typ := Etype (Expression (Actual));
            Var   := Make_Var (Expression (Actual));
            Crep  := not Same_Representation
                       (Etype (Formal), Etype (Expression (Actual)));
         else
            V_Typ := Etype (Actual);
            Var   := Make_Var (Actual);
            Crep  := False;
         end if;

         --  Setup initialization for case of in out parameter, or an out
         --  parameter where the formal is an unconstrained array (in the
         --  latter case, we have to pass in an object with bounds).

         if Ekind (Formal) = E_In_Out_Parameter
           or else (Is_Array_Type (Etype (Formal))
                     and then
                    not Is_Constrained (Etype (Formal)))
         then
            if Nkind (Actual) = N_Type_Conversion then
               Init :=
                 Convert_To (Etype (Formal), New_Occurrence_Of (Var, Loc));
            else
               Init := New_Occurrence_Of (Var, Loc);
            end if;

         --  An initialization is created for packed conversions as
         --  actuals for out parameters to enable Make_Object_Declaration
         --  to determine the proper subtype for N_Node. Note that this
         --  is wasteful because the extra copying on the call side is
         --  not required for such out parameters. ???

         elsif Ekind (Formal) = E_Out_Parameter
           and then Nkind (Actual) = N_Type_Conversion
           and then (Is_Bit_Packed_Array (Etype (Formal))
                       or else
                     Is_Bit_Packed_Array (Etype (Expression (Actual))))
         then
            Init :=
              Convert_To (Etype (Formal), New_Occurrence_Of (Var, Loc));

         else
            Init := Empty;
         end if;

         N_Node :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Object_Definition   =>
               New_Occurrence_Of (Etype (Formal), Loc),
             Expression => Init);
         Insert_Action (N, N_Node);

         --  Now, normally the deal here is that we use the defining
         --  identifier created by that object declaration. There is
         --  one exception to this. In the change of representation case
         --  the above declaration will end up looking like:

         --    temp : type := identifier;

         --  And in this case we might as well use the identifier directly
         --  and eliminate the identifier. Note that the analysis of the
         --  declaration was not a waste of time in that case, since it is
         --  what generated the necessary change of representation code.

         if Crep then
            Temp := Entity (Expression (N_Node));
            Rewrite_Substitute_Tree (N_Node, Make_Null_Statement (Loc));
         end if;

         --  If type conversion, use reverse conversion on exit

         if Nkind (Actual) = N_Type_Conversion then
            Expr := Convert_To (V_Typ, New_Occurrence_Of (Temp, Loc));
         else
            Expr := New_Occurrence_Of (Temp, Loc);
         end if;

         Rewrite_Substitute_Tree (Actual, New_Reference_To (Temp, Loc));
         Analyze (Actual);

         Append_To (Post_Call,
           Make_Assignment_Statement (Loc,
             Name       => New_Occurrence_Of (Var, Loc),
             Expression => Expr));
      end Add_Call_By_Copy_Code;

      -----------------------------------------------
      -- Is_Ref_To_Component_With_Component_Clause --
      -----------------------------------------------

      function Is_Ref_To_Component_With_Component_Clause
        (Act  : Node_Id)
         return Boolean
      is
      begin
         if Nkind (Act) = N_Indexed_Component
           or else Nkind (Act) = N_Slice
         then
            return Is_Ref_To_Component_With_Component_Clause (Prefix (Act));

         elsif Nkind (Act) = N_Selected_Component then
            if Present (Component_Clause (Entity (Selector_Name (Act)))) then
               return True;
            else
               return Is_Ref_To_Component_With_Component_Clause (Prefix (Act));
            end if;

         else
            return False;
         end if;
      end Is_Ref_To_Component_With_Component_Clause;

   --  Start of processing for Expand_Actuals

   begin
      Formal := First_Formal (Subp);
      Actual := First_Actual (N);

      Post_Call := New_List;

      while Present (Formal) loop
         E_Formal := Etype (Formal);

         if Is_Scalar_Type (E_Formal) then

            --  ??? not sure why is this necessary
            null;

         --  RM 6.4.1 (11)

         elsif Ekind (Formal) /= E_Out_Parameter then
            Apply_Constraint_Check (Actual, E_Formal);

         --  Out parameter case. Not constraint checks on access type
         --  RM 6.4.1 (13)

         elsif Is_Access_Type (E_Formal) then
            null;

         --  RM 6.4.1 (14)

         elsif Has_Discriminants (Base_Type (E_Formal))
           or else Present (Base_Init_Proc (E_Formal))
         then
            Apply_Constraint_Check (Actual, E_Formal);

         --  RM 6.4.1 (15)

         else
            Apply_Constraint_Check (Actual, Base_Type (E_Formal));
         end if;

         if Ekind (Formal) /= E_In_Parameter then

            if Is_Array_Type (E_Formal)
              and then Nkind (Actual) = N_Type_Conversion
            then
               Apply_Range_Check (Expression (Actual), E_Formal);
            end if;

            --  If argument is a type conversion for a type that is passed
            --  by copy, then we must pass the parameter by copy.

            if Nkind (Actual) = N_Type_Conversion
              and then
                (Is_Numeric_Type (E_Formal)
                  or else Is_Enumeration_Type (E_Formal)
                  or else Is_Bit_Packed_Array (Etype (Formal))
                  or else Is_Bit_Packed_Array (Etype (Expression (Actual)))
                  or else Is_Ref_To_Component_With_Component_Clause
                                                     (Expression (Actual))

                  --  Also pass by copy if change of representation

                  or else not Same_Representation
                               (Etype (Formal),
                                Etype (Expression (Actual))))
            then
               Add_Call_By_Copy_Code;

            elsif Is_Ref_To_Component_With_Component_Clause (Actual) then
               Add_Call_By_Copy_Code;

            --  References to components of bit packed arrays are expanded
            --  at this point, rather than at the point of analysis of the
            --  actuals, to handle the expansion of the assignment to
            --  [in] out parameters.

            elsif Nkind (Actual) = N_Indexed_Component
              and then Is_Bit_Packed_Array (Etype (Prefix (Actual)))
            then
               Add_Call_By_Copy_Code;

            --  It is neccessary to exclude tagged types because of "downward
            --  conversion" errors and a strange assertion error in namet
            --  from gnatf in bug 1215-001.

            elsif Is_Access_Type (E_Formal)
              and then (E_Formal /= Etype (Actual)
                         or else Nkind (Actual) = N_Type_Conversion)
              and then not Is_Tagged_Type (Designated_Type (E_Formal))
            then
               Add_Call_By_Copy_Code;
            end if;

         --  The only processing required for IN parameters is in the packed
         --  array case, where we expand the indexed component (the circuit
         --  in Exp_Ch4 deliberately left indexed components appearing as
         --  actuals untouched, so that the special processing above for
         --  the OUT and IN OUT cases could be performed. We could make the
         --  test in Exp_Ch4 more complex and have it detect the parameter
         --  mode, but it is easier simply to handle all cases here.

         else
            if Nkind (Actual) = N_Indexed_Component
              and then Is_Packed (Etype (Prefix (Actual)))
            then
               Expand_Packed_Element_Reference (Actual);
            end if;
         end if;

         Formal := Next_Formal (Formal);
         Actual := Next_Actual (Actual);
      end loop;

      --  Find right place to put post call stuff if it is present

      if not Is_Empty_List (Post_Call) then

         --  If call is not a list member, it must be the triggering
         --  statement of a triggering alternative, and we can add
         --  the post call stuff to the corresponding statement list.

         if not Is_List_Member (N) then
            declare
               P : constant Node_Id := Parent (N);

            begin
               pragma Assert (Nkind (P) = N_Triggering_Alternative);

               if Present (Statements (P)) then
                  Insert_List_Before_And_Analyze
                    (First (Statements (P)), Post_Call);
               else
                  Set_Statements (P, Post_Call);
               end if;
            end;

         --  Otherwise, normal case where N is in a statement sequence,
         --  just put the post-call stuff after the call statement.

         else
            Insert_List_After (N, Post_Call);
         end if;
      end if;

      --  The call node itself is re-analyzed in Expand_Call.

   end Expand_Actuals;

   -----------------
   -- Expand_Call --
   -----------------

   --  This procedure handles expansion of function calls and procedure call
   --  statements (i.e. it serves as the body for Expand_N_Function_Call and
   --  Expand_N_Procedure_Call_Statement. Processing for calls includes:

   --    Provide values of actuals for all formals in Extra_Formals list
   --    Replace "call" to enumeration literal function by literal itself
   --    Rewrite call to predefined operator as operator
   --    Replace actuals to in-out parameters that are numeric conversions,
   --     with explicit assignment to temporaries before and after the call.
   --    Remove optional actuals if First_Optional_Parameter specified.

   --   Note that the list of actuals has been filled with default expressions
   --   during semantic analysis of the call. Only the extra actuals required
   --   for the 'Constrained attribute and for accessibility checks are added
   --   at this point.

   procedure Expand_Call (N : Node_Id) is
      Loc           : constant Source_Ptr := Sloc (N);
      Subp          : Entity_Id;
      Parent_Subp   : Entity_Id;
      Parent_Formal : Entity_Id;
      Actual        : Node_Id;
      Formal        : Entity_Id;
      Prev          : Node_Id := Empty;
      Scop          : Entity_Id;
      Extra_Actuals : List_Id := No_List;

      procedure Add_Actual_Parameter (Insert_Param : Node_Id);
      --  Adds one entry to the end of the actual parameter list. Used for
      --  default parameters and for extra actuals (for Extra_Formals).
      --  The argument is an N_Parameter_Association node.

      procedure Add_Extra_Actual (Expr : Node_Id; EF : Entity_Id);
      --  Adds an extra actual to the list of extra actuals. Expr
      --  is the expression for the value of the actual, EF is the
      --  entity for the extra formal.

      --------------------------
      -- Add_Actual_Parameter --
      --------------------------

      procedure Add_Actual_Parameter (Insert_Param : Node_Id) is
         Actual_Expr : constant Node_Id :=
                         Explicit_Actual_Parameter (Insert_Param);

      begin
         --  Case of insertion is first named actual

         if No (Prev) or else
            Nkind (Parent (Prev)) /= N_Parameter_Association
         then
            Set_Next_Named_Actual (Insert_Param, First_Named_Actual (N));
            Set_First_Named_Actual (N, Actual_Expr);

            if No (Prev) then
               if not Present (Parameter_Associations (N)) then
                  Set_Parameter_Associations (N, New_List);
                  Append (Insert_Param, Parameter_Associations (N));
               end if;
            else
               Insert_After (Prev, Insert_Param);
            end if;

         --  Case of insertion is not first named actual

         else
            Set_Next_Named_Actual
              (Insert_Param, Next_Named_Actual (Parent (Prev)));
            Set_Next_Named_Actual (Parent (Prev), Actual_Expr);
            Append (Insert_Param, Parameter_Associations (N));
         end if;

         Prev := Actual_Expr;
      end Add_Actual_Parameter;

      ----------------------
      -- Add_Extra_Actual --
      ----------------------

      procedure Add_Extra_Actual (Expr : Node_Id; EF : Entity_Id) is
         Loc : constant Source_Ptr := Sloc (Expr);

      begin
         if Extra_Actuals = No_List then
            Extra_Actuals := New_List;
            Set_Parent (Extra_Actuals, N);
         end if;

         Append_To (Extra_Actuals,
           Make_Parameter_Association (Loc,
             Explicit_Actual_Parameter => Expr,
             Selector_Name =>
               Make_Identifier (Loc, Chars (EF))));

         Analyze_And_Resolve (Expr, Etype (EF));

      end Add_Extra_Actual;

   --  Start of processing for Expand_Call

   begin
      if Nkind (Name (N)) = N_Explicit_Dereference then
         Subp := Etype (Name (N));
         Parent_Subp := Empty;

      --  Case of call to simple entry, where the Name is a selected component
      --  whose prefix is the task, and whose selector name is the entry name

      elsif Nkind (Name (N)) = N_Selected_Component then
         Subp := Entity (Selector_Name (Name (N)));
         Parent_Subp := Empty;

      --  Case of call to member of entry family, where Name is an indexed
      --  component, with the prefix being a selected component giving the
      --  task and entry family name, and the index being the entry index.

      elsif Nkind (Name (N)) = N_Indexed_Component then
         Subp := Entity (Selector_Name (Prefix (Name (N))));
         Parent_Subp := Empty;

      --  Normal case

      else
         Subp := Entity (Name (N));
         Parent_Subp := Alias (Subp);

         if Ekind (Subp) = E_Entry then
            Parent_Subp := Empty;
         end if;
      end if;

      --  First step, compute  extra actuals, corresponding to any
      --  Extra_Formals present. Note that we do not access Extra_Formals
      --  directly, instead we simply  note the presence of the extra
      --  formals as we process the regular formals and collect the
      --  corresponding actuals in Extra_Actuals.

      Formal := First_Formal (Subp);
      Actual := First_Actual (N);

      while Present (Formal) loop

         Prev := Actual;

         --  Create possible extra actual for constrained case. Usually,
         --  the extra actual is of the form actual'constrained, but since
         --  this attribute is only available for unconstrained records,
         --  TRUE is expanded if the type of the formal happens to be
         --  constrained (for instance when this procedure is inherited
         --  from an unconstrained record to a constrained one) or if the
         --  actual has no discriminant (its type is constrained). An
         --  exception to this is the case of a private type without
         --  discriminants. In this case we pass FALSE because the
         --  object has underlying discriminants with defaults.

         if Present (Extra_Formal_Constrained (Formal)) then
            if Ekind (Etype (Prev)) in Private_Kind
              and then not Has_Discriminants (Base_Type (Etype (Prev)))
            then
               Add_Extra_Actual (
                 New_Occurrence_Of (Standard_False, Loc),
                 Extra_Formal_Constrained (Formal));

            elsif Is_Constrained (Etype (Formal))
              or else not Has_Discriminants (Etype (Prev))
            then
               Add_Extra_Actual (
                 New_Occurrence_Of (Standard_True, Loc),
                 Extra_Formal_Constrained (Formal));

            else
               --  If the actual is a type conversion, then the constrained
               --  test applies to the actual, not the target type.

               declare
                  Act_Prev : Node_Id := Prev;

               begin
                  if Nkind (Act_Prev) = N_Type_Conversion then
                     Act_Prev := Expression (Act_Prev);
                  end if;

                  Add_Extra_Actual (
                    Make_Attribute_Reference (Sloc (Prev),
                      Prefix => Duplicate_Subexpr (Act_Prev, Name_Req => True),
                      Attribute_Name => Name_Constrained),
                    Extra_Formal_Constrained (Formal));
               end;
            end if;
         end if;

         --  Create possible extra actual for accessibility level.

         if Present (Extra_Formal_Accessibility (Formal)) then
            if Is_Entity_Name (Prev) then

               --  When passing an access parameter as the actual to another
               --  access parameter we need to pass along the actual's own
               --  associated access level parameter.

               if Ekind (Entity (Prev)) in Formal_Kind
                 and then Ekind (Etype (Prev)) = E_Anonymous_Access_Type
               then
                  declare
                     Parm_Ent : constant Entity_Id := Param_Entity (Prev);

                  begin
                     pragma Assert (Present (Parm_Ent));

                     if Present (Extra_Formal_Accessibility (Parm_Ent)) then
                        Add_Extra_Actual (
                          New_Occurrence_Of
                            (Extra_Formal_Accessibility (Parm_Ent), Loc),
                          Extra_Formal_Accessibility (Formal));

                     --  If the actual access parameter does not have an
                     --  associated extra formal providing its scope level,
                     --  then treat the actual as having library-level
                     --  accessibility.

                     else
                        Add_Extra_Actual (
                          Make_Integer_Literal
                            (Loc, Scope_Depth (Standard_Standard)),
                          Extra_Formal_Accessibility (Formal));
                     end if;
                  end;

               --  The actual is a normal access value, so just pass the
               --  level of the actual's access type.

               else
                  Add_Extra_Actual (
                    Make_Integer_Literal
                      (Loc, Type_Access_Level (Etype (Prev))),
                    Extra_Formal_Accessibility (Formal));
               end if;

            else
               case Nkind (Prev) is

                  when N_Attribute_Reference =>

                     case Get_Attribute_Id (Attribute_Name (Prev)) is

                        --  For X'Access, pass on the level of the prefix X

                        when Attribute_Access =>
                           Add_Extra_Actual (
                             Make_Integer_Literal
                               (Loc, Object_Access_Level (Prefix (Prev))),
                             Extra_Formal_Accessibility (Formal));

                        --  Treat the unchecked attributes as library-level

                        when Attribute_Unchecked_Access |
                           Attribute_Unrestricted_Access =>
                           Add_Extra_Actual (
                             Make_Integer_Literal
                               (Loc, Scope_Depth (Standard_Standard)),
                             Extra_Formal_Accessibility (Formal));

                        --  No other cases of attributes returning access
                        --  values that can be passed to access parameters

                        when others =>
                           pragma Assert (False); null;

                     end case;

                  --  For allocators we pass the level of the execution of
                  --  the called subprogram, which is one greater than the
                  --  current scope level.

                  when N_Allocator =>
                     Add_Extra_Actual (
                       Make_Integer_Literal
                         (Loc, UI_Add (Scope_Depth (Current_Scope), 1)),
                       Extra_Formal_Accessibility (Formal));

                  --  For other cases we simply pass the level of the
                  --  actual's access type.

                  when others =>
                     Add_Extra_Actual (
                       Make_Integer_Literal
                         (Loc, Type_Access_Level (Etype (Prev))),
                       Extra_Formal_Accessibility (Formal));

               end case;
            end if;
         end if;

         Actual := Next_Actual (Actual);
         Formal := Next_Formal (Formal);
      end loop;

      --  If we are expanding a rhs of an assignement we need to check if
      --  tag propagation is needed. This code belongs theorically in Analyze
      --  Assignement but this may be too late and the function call may have
      --  moved away thank's to a duplicate subexpr call.

      if Nkind (N) = N_Function_Call
        and then Nkind (Parent (N)) = N_Assignment_Statement
        and then Is_Entity_Name (Name (N))
        and then Is_Class_Wide_Type (Etype (Name (Parent (N))))
        and then Is_Tag_Indeterminate (N)
      then
         Propagate_Tag (Name (Parent (N)), N);
         return;
      end if;

      --  Deals with Dispatch_Call if we still have a call, before expanding
      --  extra actuals since this will be done on the re-analysis of the
      --  dispatching call. Note that we do not try to shorten the actual
      --  list for a dispatching call, it would not make sense to do so.

      if (Nkind (N) = N_Function_Call
           or else Nkind (N) =  N_Procedure_Call_Statement)
        and then Present (Controlling_Argument (N))
      then
         Expand_Dispatch_Call (N);
         return;

      --  Similarly, do not add extra actuals for an entry call whose entity
      --  is a protected procedure, because it will be rewritten as a
      --  protected procedure call and reanalyzed.

      elsif Nkind (N) = N_Entry_Call_Statement
        and then Ekind (Subp) = E_Procedure
        and then Is_Protected_Type (Scope (Subp))
      then
         null;

      --  During that loop we gathered the extra actuals (the ones that
      --  correspond to Extra_Formals), so now they can be appended.

      else
         while Is_Non_Empty_List (Extra_Actuals) loop
            Add_Actual_Parameter (Remove_Head (Extra_Actuals));
         end loop;
      end if;

      --  Could use a comment at this point ???

      if Nkind (N) /= N_Entry_Call_Statement
        and then No (Controlling_Argument (N))
        and then Present (Parent_Subp)
      then
         while Present (Alias (Parent_Subp)) loop
            Parent_Subp := Alias (Parent_Subp);
         end loop;

         Set_Entity (Name (N), Parent_Subp);

         --  Add an explicit conversion for parameter of the derived type.
         --  This is only done for scalar in-parameters. Others are expanded
         --  in expand_actuals.

         Formal := First_Formal (Subp);
         Parent_Formal := First_Formal (Parent_Subp);
         Actual := First_Actual (N);

         if not Is_Intrinsic_Subprogram (Parent_Subp) then

            while Present (Formal) loop

               if Etype (Formal) /= Etype (Parent_Formal)
                 and then Is_Scalar_Type (Etype (Formal))
                 and then Ekind (Formal) = E_In_Parameter
               then
                  Rewrite_Substitute_Tree (Actual,
                    Convert_To (Etype (Parent_Formal),
                      Relocate_Node (Actual)));

                  Analyze (Actual);
                  Resolve (Actual, Etype (Parent_Formal));
                  Set_Do_Range_Check (Actual, True);

               elsif Is_Access_Type (Etype (Formal))
                 and then Ekind (Formal) /= E_In_Parameter
               then
                  Rewrite_Substitute_Tree (Actual,
                    Convert_To (Etype (Parent_Formal),
                      Relocate_Node (Actual)));

                  Analyze (Actual);
                  Resolve (Actual, Etype (Parent_Formal));
               end if;

               Formal := Next_Formal (Formal);
               Parent_Formal := Next_Formal (Parent_Formal);
               Actual := Next_Actual (Actual);
            end loop;
         end if;

         Subp := Parent_Subp;
      end if;

      --  Some more special cases for cases other than explicit dereference

      if Nkind (Name (N)) /= N_Explicit_Dereference then

         --  Calls to an enumeration literal are replaced by the literal
         --  The only way that this case occurs is when we have a call to
         --  a function that is a renaming of an enumeration literal. The
         --  normal case of a direct reference to an enumeration literal
         --  has already been dealt with by Resolve_Call

         if Ekind (Subp) = E_Enumeration_Literal then
            Rewrite_Substitute_Tree (N, Name (N));
         end if;

      --  Handle case of access to protected subprogram type

      else
         if Ekind (Etype (Prefix (Name (N)))) =
                               E_Access_Protected_Subprogram_Type
         then
            --  If this is a call through an access to protected operation,
            --  the prefix has the form (object'address, operation'access).
            --  Rewrite as a for other protected calls: the object is the
            --  first parameter of the list of actuals.

            declare
               Call : Node_Id;
               Parm : List_Id;
               Nam  : Node_Id;
               Obj  : Node_Id;
               Ptr  : Node_Id := Prefix (Name (N));
               T    : Entity_Id := Equivalent_Type (Etype (Ptr));
               D_T  : Entity_Id := Designated_Type (Etype (Ptr));

            begin
               Obj := Make_Selected_Component (Loc,
                 Prefix => Unchecked_Convert_To (T, Ptr),
                 Selector_Name => New_Occurrence_Of (First_Entity (T), Loc));

               Nam := Make_Selected_Component (Loc,
                 Prefix => Unchecked_Convert_To (T, Ptr),
                 Selector_Name => New_Occurrence_Of (
                   Next_Entity (First_Entity (T)), Loc));

               Nam := Make_Explicit_Dereference (Loc, Nam);

               if Present (Parameter_Associations (N))  then
                  Parm := New_List_Copy (Parameter_Associations (N));
               else
                  Parm := New_List;
               end if;

               Prepend (Obj, Parm);

               if Etype (D_T) = Standard_Void_Type then
                  Call := Make_Procedure_Call_Statement (Loc,
                    Name => Nam,
                    Parameter_Associations => Parm);
               else
                  Call := Make_Function_Call (Loc,
                    Name => Nam,
                    Parameter_Associations => Parm);
               end if;

               Set_Etype (Call, Etype (D_T));

               Rewrite_Substitute_Tree (N, Call);
               Analyze (Nam);
               Analyze (Obj);
               return;
            end;
         end if;
      end if;

      if Ekind (Subp) = E_Procedure
         or else Is_Entry (Subp)
      then
         Expand_Actuals (N, Subp);
      end if;

      --  If this is a call to an intrinsic subprogram, then perform the
      --  appropriate expansion to the corresponding tree node and we
      --  are all done (since after that the call is gone!)

      if Is_Intrinsic_Subprogram (Subp) then
         Expand_Intrinsic_Call (N, Subp);
         return;
      end if;

      if Ekind (Subp) = E_Function
        or else Ekind (Subp) = E_Procedure
      then

         if Is_Inlined (Subp)
           or else Inline_All
         then
            Add_Inlined_Body (N, Subp);
            Set_Is_Inlined (Subp);
         end if;
      end if;

      --  Check for a protected subprogram.  This is either an intra-object
      --  call, or a protected function call. Protected procedure calls are
      --  rewritten as entry calls and handled accordingly.

      Scop := Scope (Subp);

      if Nkind (N) /= N_Entry_Call_Statement
        and then Is_Protected_Type (Scop)
      then
         --  If the call is an internal one,  it is rewritten as a call to
         --  to the corresponding unprotected subprogram.

         Expand_Protected_Subprogram_Call (N, Subp, Scop);
      end if;

      --  Functions returning controlled objects need special attention

      if Controlled_Type (Etype (Subp))
        and then not Is_Return_By_Reference_Type (Etype (Subp))
      then
         Expand_Ctrl_Function_Call (N);
      end if;

      --  Test for First_Optional_Parameter, and if so, truncate parameter
      --  list if there are optional parameters at the the trailing end.
      --  Note we never delete procedures for call via a pointer.

      if (Ekind (Subp) = E_Procedure or else Ekind (Subp) = E_Function)
        and then Present (First_Optional_Parameter (Subp))
      then
         declare
            Last_Keep_Arg : Node_Id;

         begin
            --  Last_Keep_Arg will hold the last actual that should be
            --  retained. If it remains empty at the end, it means that
            --  all parameters are optional.

            Last_Keep_Arg := Empty;

            --  Find first optional parameter, must be present since we
            --  checked the validity of the parameter before setting it.

            Formal := First_Formal (Subp);
            Actual := First_Actual (N);
            while Formal /= First_Optional_Parameter (Subp) loop
               Last_Keep_Arg := Actual;
               Formal := Next_Formal (Formal);
               Actual := Next_Actual (Actual);
            end loop;

            --  Now we have Formal and Actual pointing to the first
            --  potentially droppable argument. We can drop all the
            --  trailing arguments whose actual matches the default.
            --  Note that we know that all remaining formals have
            --  defaults, because we checked that this requirement
            --  was met before setting First_Optional_Parameter.

            --  We use Fully_Conformant_Expressions to check for identity
            --  between formals and actuals, which may miss some cases, but
            --  on the other hand, this is only an optimization (if we fail
            --  to truncate a parameter it does not affect functionality).
            --  So if the default is 3 and the actual is 1+2, we consider
            --  them unequal, which hardly seems worrisome.

            while Present (Formal) loop
               if not Fully_Conformant_Expressions
                    (Actual, Default_Value (Formal))
               then
                  Last_Keep_Arg := Actual;
               end if;

               Formal := Next_Formal (Formal);
               Actual := Next_Actual (Actual);
            end loop;

            --  If no arguments, delete entire list, this is the easy case

            if No (Last_Keep_Arg) then
               while Is_Non_Empty_List (Parameter_Associations (N)) loop
                  Delete_Tree (Remove_Head (Parameter_Associations (N)));
               end loop;

               Set_Parameter_Associations (N, No_List);
               Set_First_Named_Actual (N, Empty);

            --  Case where at the last retained argument is positional. This
            --  is also an easy case, since the retained arguments are already
            --  in the right form, and we don't need to worry about the order
            --  of arguments that get eliminated.

            elsif Is_List_Member (Last_Keep_Arg) then
               while Present (Next (Last_Keep_Arg)) loop
                  Delete_Tree (Remove_Next (Last_Keep_Arg));
               end loop;

               Set_First_Named_Actual (N, Empty);

            --  This is the annoying case where the last retained argument
            --  is a named parameter. Since the original arguments are not
            --  in declaration order, we may have to delete some fairly
            --  random collection of arguments.

            else
               declare
                  Temp   : Node_Id;
                  Passoc : Node_Id;
                  Junk   : Node_Id;

               begin
                  --  First step, remove all the named parameters from the
                  --  list (they are still chained using First_Named_Actual
                  --  and Next_Named_Actual, so we have not lost them!)

                  Temp := First (Parameter_Associations (N));

                  --  Case of all parameters named, remove them all

                  if Nkind (Temp) = N_Parameter_Association then
                     while Is_Non_Empty_List (Parameter_Associations (N)) loop
                        Temp := Remove_Head (Parameter_Associations (N));
                     end loop;

                  --  Case of mixed positional/named, remove named parameters

                  else
                     while Nkind (Next (Temp)) /= N_Parameter_Association loop
                        Temp := Next (Temp);
                     end loop;

                     while Present (Next (Temp)) loop
                        Junk := Remove_Next (Temp);
                     end loop;
                  end if;

                  --  Now we loop through the named parameters, till we get
                  --  to the last one to be retained, adding them to the list.
                  --  Note that the Next_Named_Actual list does not need to be
                  --  touched since we are only reordering them on the actual
                  --  parameter association list.

                  Passoc := Parent (First_Named_Actual (N));
                  loop
                     Append_To
                       (Parameter_Associations (N), Relocate_Node (Passoc));
                     exit when
                       Last_Keep_Arg = Explicit_Actual_Parameter (Passoc);
                     Passoc := Parent (Next_Named_Actual (Passoc));
                  end loop;

                  Set_Next_Named_Actual (Passoc, Empty);

                  loop
                     Temp := Next_Named_Actual (Passoc);
                     exit when No (Temp);
                     Set_Next_Named_Actual
                       (Passoc, Next_Named_Actual (Parent (Temp)));
                     Delete_Tree (Temp);
                  end loop;
               end;
            end if;
         end;
      end if;

   end Expand_Call;

   --------------------------------------
   -- Expand_Protected_Subprogram_Call --
   --------------------------------------

   procedure Expand_Protected_Subprogram_Call
     (N    : Node_Id;
      Subp : Entity_Id;
      Scop : Entity_Id)
   is
      Loc   : constant Source_Ptr := Sloc (N);
      Param : Entity_Id;
      Corr  : Entity_Id;
      Proc  : Entity_Id;
      Rec   : Node_Id;

   begin
      --  If the protected object is not an enclosing scope, this is
      --  an inter-object function call. Inter-object procedure
      --  calls are expanded by Exp_Ch9.Build_Simple_Entry_Call.
      --  The call is intra-object only if the the subprogram being
      --  called is in the protected body being compiled, and if the
      --  protected object in the call is statically the enclosing type.
      --  The object may be an component of some other data structure,
      --  in which case this must be handled as an inter-object call.

      if not In_Open_Scopes (Scop)
        or else not Is_Entity_Name (Name (N))
      then
         if Nkind (Name (N)) = N_Selected_Component then
            Rec := Prefix (Name (N));

         elsif Nkind (Name (N)) = N_Indexed_Component then
            Rec := Prefix (Prefix (Name (N)));

         else
            null;
            pragma Assert (False);
         end if;

         Rewrite_Substitute_Tree (N,
           Build_Protected_Subprogram_Call (N,
             Name => New_Occurrence_Of (Subp, Sloc (N)),
             Rec =>  Convert_Concurrent (Rec, Etype (Rec)),
             External => True));

      else
         Rec := Make_Identifier (Loc, Name_uObject);
         Set_Etype (Rec, Corresponding_Record_Type (Scop));

         --  Find enclosing protected operation, and retrieve its first
         --  parameter, which denotes the enclosing protected object.
         --  If the enclosing operation is an entry, we are immediately
         --  within the protected body, and we can retrieve the object
         --  from the service entries procedure. A barrier function has
         --  has the same signature as an entry. A barrier function is
         --  compiled within the protected object, but unlike protected
         --  operations its never needs locks, so that its protected body
         --  subprogram points to itself.

         Proc := Current_Scope;
         while Present (Proc)
           and then Scope (Proc) /= Scop
         loop
            Proc := Scope (Proc);
         end loop;

         Corr := Protected_Body_Subprogram (Proc);

         if No (Corr) then

            --  Previous error left expansion incomplete.
            --  Nothing to do on this call.

            return;
         end if;

         Param := Defining_Identifier
           (First
             (Parameter_Specifications (Parent (Corr))));

         if Is_Subprogram (Proc)
           and then Proc /= Corr
         then
            --  Protected function or procedure.

            Set_Entity (Rec, Param);

            --  What is this about and why is it necessary???

            Set_Analyzed (Rec);

            --  A protected procedure cannot be called internally from
            --  a protected function.

            if Ekind (Corr) = E_Function
              and then Ekind (Subp) /= E_Function
            then
               Error_Msg_N
                ("within a protected function object is constant", N);
               --  ??? this is wrong, error messages should not be output
               --  ??? in expander, since then they do not appear in -gnatc.
            end if;

         else
            --  Entry or barrier function for entry body.
            --  The first parameter of the entry body procedure is a
            --  pointer to the object. We create a local variable
            --  of the proper type, duplicating what is done to define
            --  _object later on.

            declare
               Decls : List_Id;
               Obj_Ptr : Entity_Id :=  Make_Defining_Identifier
                                         (Loc, New_Internal_Name ('T'));
            begin
               Decls := New_List (
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Obj_Ptr,
                     Type_Definition =>
                        Make_Access_To_Object_Definition (Loc,
                          Subtype_Indication =>
                            New_Reference_To
                         (Corresponding_Record_Type (Scop), Loc))));

               Insert_Actions (N, Decls);
               Insert_Actions (N, Freeze_Entity (Obj_Ptr, Sloc (N)));

               Rec :=
                 Make_Explicit_Dereference (Loc,
                   Unchecked_Convert_To (Obj_Ptr,
                     New_Occurrence_Of (Param, Loc)));
            end;
         end if;

         Rewrite_Substitute_Tree (N,
           Build_Protected_Subprogram_Call (N,
             Name     => Name (N),
             Rec      => Rec,
             External => False));

         --  What is this about, and why is it necessary ???
         --  Even if the setting of analyzed on Rec is legitimate,
         --  it would protect the result of this analyze anyway!

         if not Is_Subprogram (Proc) then
            Analyze (Rec);
         end if;
      end if;

      Analyze (N);
   end Expand_Protected_Subprogram_Call;

   ----------------------------
   -- Expand_N_Function_Call --
   ----------------------------

   procedure Expand_N_Function_Call (N : Node_Id) is
   begin
      Expand_Call (N);
   end Expand_N_Function_Call;

   ---------------------------------------
   -- Expand_N_Procedure_Call_Statement --
   ---------------------------------------

   procedure Expand_N_Procedure_Call_Statement (N : Node_Id) is
   begin
      Expand_Call (N);
   end Expand_N_Procedure_Call_Statement;

   ------------------------------
   -- Expand_N_Subprogram_Body --
   ------------------------------

   --  Add return statement if last statement in body is not a return
   --  statement (this makes things easier on Gigi which does not want
   --  to have to handle a missing return).

   --  Add call to Activate_Tasks if body is a task activator

   --  Deal with possible detection of infinite recursion

   procedure Expand_N_Subprogram_Body (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      H        : constant Node_Id    := Handled_Statement_Sequence (N);
      Spec_Id  : Entity_Id;
      Except_H : Node_Id;
      Scop     : Entity_Id;
      Dec      : Node_Id;
      Next_Op  : Node_Id;
      Hloc     : Source_Ptr;

      procedure Add_Return (S : List_Id);
      --  Append a return statement to the statement sequence S if the last
      --  statement is not already a return or a goto statement. Note that
      --  the latter test is not critical, it does not matter if we add a
      --  few extra returns, since they get eliminated anyway later on.

      procedure Add_Return (S : List_Id) is
         Last_S : constant Node_Id := Last (S);

      begin
         if Nkind (Last_S) /= N_Return_Statement
           and then Nkind (Last_S) /= N_Goto_Statement
           and then Nkind (Last_S) /= N_Raise_Statement
         then
            Append_To (S, Make_Return_Statement (Sloc (Last_S)));
         end if;
      end Add_Return;

   --  Start of processing for Expand_N_Subprogram_Body

   begin
      if Present (Corresponding_Spec (N)) then
         Spec_Id := Corresponding_Spec (N);

      --  If there is no spec, then we did not freeze the spec, so this is
      --  where we must manufacture the default expression functions.

      else
         Spec_Id := Defining_Entity (N);
         Make_Default_Expr_Functions (N, Spec_Id);
      end if;

      Scop := Scope (Spec_Id);

      --  Returns_By_Ref flag is normally set when the subprogram is frozen
      --  but subprograms with no specs are not frozen

      declare
         Typ  : constant Entity_Id := Etype (Spec_Id);
         Utyp : constant Entity_Id := Underlying_Type (Typ);

      begin
         if not Acts_As_Spec (N) then
            null;

         elsif Is_Return_By_Reference_Type (Typ) then
            Set_Returns_By_Ref (Spec_Id);

         elsif Present (Utyp) and then Controlled_Type (Utyp) then
            Set_Returns_By_Ref (Spec_Id);
         end if;
      end;

      --  For a procedure, we add a return for all possible syntactic ends
      --  of the subprogram. Note that reanalysis is not necessary in this
      --  case since it would require a lot of work and accomplish nothing.

      if Ekind (Spec_Id) = E_Procedure
        or else Ekind (Spec_Id) = E_Generic_Procedure
      then
         Add_Return (Statements (H));

         if Present (Exception_Handlers (H)) then
            Except_H := First_Non_Pragma (Exception_Handlers (H));

            while Present (Except_H) loop
               Add_Return (Statements (Except_H));
               Except_H := Next_Non_Pragma (Except_H);
            end loop;
         end if;

      --  For a function, we must deal with the case where there is at
      --  least one missing return. What we do is to wrap the entire body
      --  of the function in a block:

      --    begin
      --      ...
      --    end;

      --  becomes

      --    begin
      --       begin
      --          ...
      --       end;

      --       raise Program_Error;
      --    end;

      --  This approach is necessary because the raise must be signalled
      --  to the caller, not handled by any local handler (RM 6.4(11)).

      elsif Has_Missing_Return (Spec_Id) then
         Hloc := Sloc (H);
         Set_Handled_Statement_Sequence (N,
           Make_Handled_Sequence_Of_Statements (Hloc,
             Statements => New_List (
               Make_Block_Statement (Hloc,
                 Handled_Statement_Sequence => H),
               Make_Raise_Statement (Hloc,
                 Name => New_Occurrence_Of (Standard_Program_Error, Hloc)))));
      end if;

      --  Add discriminal renamings to protected subprograms.
      --  Install new discriminals for expansion of the next
      --  subprogram of this protected type, if any.

      if Is_List_Member (N)
        and then Present (Parent (List_Containing (N)))
        and then Nkind (Parent (List_Containing (N))) = N_Protected_Body
      then
         Add_Discriminal_Declarations
           (Declarations (N), Scop, Name_uObject, Loc);
         Add_Private_Declarations (Declarations (N), Scop, Name_uObject, Loc);

         --  Associate privals and discriminals with the next protected
         --  operation body to be expanded. These are used to expand
         --  references to private data objects and discriminants,
         --  respectively.

         Next_Op := Next_Protected_Operation (N);

         if Present (Next_Op) then
            Dec := Parent (Base_Type (Scop));
            Set_Privals (Dec, Next_Op, Loc);
            Set_Discriminals (Dec, Next_Op, Loc);
         end if;

      end if;

      --  If subprogram contains a parameterless recursive call, then we may
      --  have an infinite recursion, so see if we can generate code to check
      --  for this possibility if storage checks are not suppressed.

      if Ekind (Spec_Id) = E_Procedure
        and then Has_Recursive_Call (Spec_Id)
        and then not Storage_Checks_Suppressed (Spec_Id)
      then
         Detect_Infinite_Recursion (N, Spec_Id);
      end if;
   end Expand_N_Subprogram_Body;

   -------------------------------------
   -- Expand_N_Subprogram_Declaration --
   -------------------------------------

   --  The first task to be performed is the construction of default
   --  expression functions for in parameters with default values. These
   --  are parameterless inlined functions that are used to evaluate
   --  default expressions that are more complicated than simple literals
   --  or identifiers referencing constants and variables.

   --  If the declaration appears within a protected body, it is a private
   --  operation of the protected type. We must create the corresponding
   --  protected subprogram an associated formals. For a normal protected
   --  operation, this is done when expanding the protected type declaration.

   procedure Expand_N_Subprogram_Declaration (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      Subp     : Entity_Id := Defining_Entity (N);
      Scop     : Entity_Id := Scope (Subp);
      Prot_Sub : Entity_Id;
      Prot_Bod : Node_Id;

   begin
      --  Generate Default expr functions only if it is not possible to
      --  generate them at the freezing point, where they really belong
      --  otherwise they will be inserted too soon and will cause all sort
      --  of trouble (e.g. becoming primitive operations of a tagged type etc.)

      if not Has_Delayed_Freeze (Subp) then
         Make_Default_Expr_Functions (N, Defining_Entity (N));
      end if;

      if Is_List_Member (N)
        and then Present (Parent (List_Containing (N)))
        and then Nkind (Parent (List_Containing (N))) = N_Protected_Body
      then
         if No (Protected_Body_Subprogram (Subp)) then
            Prot_Sub :=
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Build_Protected_Sub_Specification
                    (N, Scop, Unprotected => True));

            --  The protected subprogram is declared outside of the protected
            --  body. Given that the body has frozen all entities so far, we
            --  freeze the subprogram explicitly.

            Prot_Bod := Parent (List_Containing (N));
            Insert_Before (Prot_Bod, Prot_Sub);

            New_Scope (Scope (Scop));
            Analyze (Prot_Sub);
            Set_Protected_Body_Subprogram (Subp,
              Defining_Unit_Name (Specification (Prot_Sub)));
            Pop_Scope;
         end if;
      end if;
   end Expand_N_Subprogram_Declaration;

   -----------------------
   -- Freeze_Subprogram --
   -----------------------

   procedure Freeze_Subprogram (N : Node_Id) is
      E : constant Entity_Id := Entity (N);

   begin

      Make_Default_Expr_Functions (N, E);

      --  When a primitive is frozen, enter its name in the corresponding
      --  dispatch table. If the DTC_Entity field is not set this is an
      --  overridden primitive that can be ignored.

      if Is_Dispatching_Operation (E)
        and then not Is_Abstract (E)
        and then Present (DTC_Entity (E))
        and then not Is_CPP_Class (Scope (DTC_Entity (E)))
      then
         Insert_After (N, Fill_DT_Entry (Sloc (N), E));
      end if;

      --  Mark functions that return by reference. Note that it cannot be
      --  part of the normal semantic analysis of the spec since the
      --  underlying returned type may not be known yet (for private types)

      declare
         Typ  : constant Entity_Id := Etype (E);
         Utyp : constant Entity_Id := Underlying_Type (Typ);

      begin
         if Is_Return_By_Reference_Type (Typ) then
            Set_Returns_By_Ref (E);

         elsif Present (Utyp) and then Controlled_Type (Utyp) then
            Set_Returns_By_Ref (E);
         end if;
      end;

   end Freeze_Subprogram;

end Exp_Ch6;

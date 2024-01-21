------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 3                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.282 $                            --
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
with Errout;   use Errout;
with Exp_TSS;  use Exp_TSS;
with Exp_Util; use Exp_Util;
with Features; use Features;
with Freeze;   use Freeze;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Ttypes;   use Ttypes;
with Tbuild;   use Tbuild;
with Urealp;   use Urealp;

package body Sem_Ch13 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Component_Overlap (C1_Ent, C2_Ent : Entity_Id);
   --  Given two entities for record components or discriminants, checks
   --  if they hav overlapping component clauses and issues errors if so.

   function Defined_Before (E1, E2 : Entity_Id) return Boolean;
   --  Determine if entity E1 is defined before E2 (returns True if so)

   function Get_Alignment_Value (Expr : Node_Id) return Uint;
   --  Given the expression fo an alignment value, returns the corresponding
   --  Uint value. If the value is inappropriate, then error messages are
   --  posted as required, and a value of No_Uint is returned.

   procedure New_Stream_Function
     (N    : Node_Id;
      Ent  : Entity_Id;
      Subp : Entity_Id;
      Nam  : Name_Id);
   --  Create a function renaming of a given stream attribute to the
   --  designated subprogram and then in the tagged case, provide this as
   --  a primitive operation, or in the non-tagged case make an appropriate
   --  TSS entry. Used for Input. This is more properly an expansion activity
   --  than just semantics, but the presence of user-defined stream functions
   --  for limited types is a legality check, which is why this takes place
   --  here rather than in exp_ch13, where it was previously.

   procedure New_Stream_Procedure
     (N     : Node_Id;
      Ent   : Entity_Id;
      Subp  : Entity_Id;
      Nam   : Name_Id;
      Out_P : Boolean := False);
   --  Create a procedure renaming of a given stream attribute to the
   --  designated subprogram and then in the tagged case, provide this as
   --  a primitive operation, or in the non-tagged case make an appropriate
   --  TSS entry. Used for Read, Output, Write.

   procedure Check_Constant_Address_Clause (Expr : Node_Id; U_Ent : Entity_Id);
   --  Expr is an expression for an address clause. This procedure checks
   --  that the expression is constant, in the limited sense that it is safe
   --  to evaluate it at the point the object U_Ent is declared, rather than
   --  at the point of the address clause. The condition for this to be true
   --  is that the expression has no variables, no constants declared after
   --  U_Ent, and no calls to non-pure functions. If this condition is not
   --  met, then an appropriate error message is posted.

   -----------------------
   -- Analyze_At_Clause --
   -----------------------

   --  An at clause is replaced by the corresponding Address attribute
   --  definition clause that is the preferred approach in Ada 95.

   procedure Analyze_At_Clause (N : Node_Id) is
   begin
      Rewrite_Substitute_Tree (N,
        Make_Attribute_Definition_Clause (Sloc (N),
          Name  => Identifier (N),
          Chars => Name_Address,
          Expression => Expression (N)));
      Analyze (N);
   end Analyze_At_Clause;

   -----------------------------------------
   -- Analyze_Attribute_Definition_Clause --
   -----------------------------------------

   procedure Analyze_Attribute_Definition_Clause (N : Node_Id) is
      Loc   : constant Source_Ptr   := Sloc (N);
      Nam   : constant Node_Id      := Name (N);
      Attr  : constant Name_Id      := Chars (N);
      Expr  : constant Node_Id      := Expression (N);
      Id    : constant Attribute_Id := Get_Attribute_Id (Attr);
      Ent   : Entity_Id;
      U_Ent : Entity_Id;

      FOnly : Boolean := False;
      --  Reset to True for subtype specific attribute (Alignment, Size)
      --  and for stream attributes, i.e. those cases where in the call
      --  to Rep_Item_Too_Late, FOnly is set True so that only the freezing
      --  rules are checked. Note that the case of stream attributes is not
      --  clear from the RM, but see AI95-00137. Also, the RM seems to
      --  disallow Storage_Size for derived task types, but that is also
      --  clearly unintentional.

   begin
      Analyze (Nam);
      Ent := Entity (Nam);

      if Rep_Item_Too_Early (Ent, N) then
         return;
      end if;

      --  Rep clause applies to full view of incomplete type or private type
      --  if we have one (if not, this is a premature use of the type).
      --  However, certain semantic checks need to be done on the specified
      --  entity (i.e. the private view), so we save it in Ent.

      if Is_Private_Type (Ent)
        and then Is_Derived_Type (Ent)
        and then not Is_Tagged_Type (Ent)
        and then Is_Tagged_Type (Underlying_Type (Ent))
        and then No (Full_View (Ent))
      then
         --  Temporarily kludge the case of a derivation from an untagged
         --  private type whose completion is tagged (until these types
         --  receive their own full views). ???
         U_Ent := Ent;

      elsif Ekind (Ent) = E_Incomplete_Type then
         Ent := Underlying_Type (Ent);
         U_Ent := Ent;
      else
         U_Ent := Underlying_Type (Ent);
      end if;

      --  Complete other routine error checks

      if Etype (Nam) = Any_Type then
         return;

      elsif Scope (Ent) /= Current_Scope then
         Error_Msg_N ("entity must be declared in this scope", Nam);
         return;

      elsif Is_Type (U_Ent)
        and then not Is_First_Subtype (U_Ent)
        and then Id /= Attribute_Object_Size
        and then Id /= Attribute_Value_Size
        and then not From_At_Mod (N)
      then
         Error_Msg_N ("cannot specify attribute for subtype", Nam);
         return;

      end if;

      --  Switch on particular attribute

      case Id is

         -------------
         -- Address --
         -------------

         --  Address attribute definition clause

         when Attribute_Address => Address : begin
            Note_Feature (New_Representation_Clauses, Loc);
            Analyze_And_Resolve (Expr, RTE (RE_Address));

            if Present (Address_Clause (U_Ent)) then
               Error_Msg_N ("address already given for &", Nam);

            --  Case of address clause for subprogram

            elsif Is_Subprogram (U_Ent) then
               if not Is_Imported (U_Ent) then
                  Error_Msg_N
                    ("address clause can only be given " &
                     "for imported subprogram",
                     Nam);
               end if;

               if Has_Homonym (U_Ent) then
                  Error_Msg_N
                    ("address clause cannot be given " &
                     "for overloaded subprogram",
                     Nam);
               end if;

               --  For subprograms, all address clauses are permitted,
               --  and we mark the subprogram as having a deferred freeze
               --  so that Gigi will not elaborate it too soon.

               Set_Has_Delayed_Freeze (U_Ent);
               Set_Address_Clause (U_Ent, N);

            --  Case of address clause for entry

            elsif Ekind (U_Ent) = E_Entry then

               if Nkind (Parent (N)) = N_Task_Body then
                  Error_Msg_N
                    ("entry address must be specified in task spec", Nam);
               end if;

               --  For entries, we require a constant address

               Check_Constant_Address_Clause (Expr, U_Ent);
               Set_Address_Clause (U_Ent, N);

            --  Case of address clause for variable or constant

            elsif
              Ekind (U_Ent) = E_Variable
                or
              Ekind (U_Ent) = E_Constant
            then
               declare
                  Decl : constant Node_Id   := Declaration_Node (U_Ent);
                  Typ  : constant Entity_Id := Etype (U_Ent);
                  Init : Node_Id;

               begin
                  --  We consider an address clause to constitute an implicit
                  --  declaration of the variable as volatile to inhibit all
                  --  optimizations (RM 13.3(19)). We also mark a possible
                  --  modification, since it is likely aliasing is occurring.

                  Note_Possible_Modification (Nam);
                  Set_Is_Volatile (Ent);

                  --  If we have no initialization of any kind, then we can
                  --  safely defer the elaboration of the variable to its
                  --  freezing point, so that the address clause will be
                  --  computed at the proper point.

                  --  The same processing applies to all initialized scalar
                  --  types and all access types

                  if (No (Expression (Decl))
                          and then
                      No (Base_Init_Proc (Typ)))
                    or else
                      (Present (Expression (Decl))
                        and then Is_Scalar_Type (Typ))
                    or else
                      Is_Access_Type (Typ)
                  then
                     Set_Has_Delayed_Freeze (U_Ent);

                  --  Otherwise, we require the address clause to be constant

                  else
                     Check_Constant_Address_Clause (Expr, U_Ent);
                  end if;

                  --  Kill the size check code, since we are not allocating
                  --  the variable, it is somewhere else.

                  Kill_Size_Check_Code (U_Ent);
                  Set_Address_Clause (U_Ent, N);
               end;

            --  Not a valid entity for an address clause

            else
               Error_Msg_N ("address cannot be given for &", Nam);
            end if;
         end Address;

         ---------------
         -- Alignment --
         ---------------

         --  Alignment attribute definition clause

         when Attribute_Alignment => Alignment : declare
            Align : Uint := Get_Alignment_Value (Expr);

         begin
            Note_Feature (New_Representation_Clauses, Loc);
            FOnly := True;

            if not Is_Type (U_Ent)
              and then Ekind (U_Ent) /= E_Variable
              and then Ekind (U_Ent) /= E_Constant
            then
               Error_Msg_N ("alignment cannot be given for &", Nam);

            elsif Has_Alignment_Clause (U_Ent) then
               Error_Msg_Sloc := Sloc (Alignment_Clause (U_Ent));
               Error_Msg_N ("alignment clause previously given#", N);

            elsif Align /= No_Uint then
               Set_Has_Alignment_Clause (U_Ent);
               Set_Has_Gigi_Rep_Item (U_Ent);
            end if;
         end Alignment;

         ---------------
         -- Bit_Order --
         ---------------

         --  Bit_Order attribute definition clause

         when Attribute_Bit_Order => Bit_Order : declare
         begin
            Note_Feature (New_Representation_Clauses, Loc);

            if not Is_Record_Type (U_Ent) then
               Error_Msg_N ("& definition requires record type", Nam);

            else
               Analyze_And_Resolve (Expr, RTE (RE_Bit_Order));

               if Etype (Expr) = Any_Type then
                  return;

               elsif not Is_Static_Expression (Expr) then
                  Error_Msg_N ("& requires static expression", Expr);

               else
                  if (Expr_Value (Expr) = 0) /= Bytes_Big_Endian then
                     Error_Msg_N ("unsupported value for & attribute", Expr);
                  end if;
               end if;
            end if;
         end Bit_Order;

         --------------------
         -- Component_Size --
         --------------------

         --  Component_Size attribute definition clause

         when Attribute_Component_Size => Component_Size_Case : declare
            Csize    : constant Uint := Static_Integer (Expr);
            Btype    : Entity_Id;
            Biased   : Boolean;
            New_Ctyp : Entity_Id;
            Decl     : Node_Id;

         begin
            Note_Feature (New_Representation_Clauses, Loc);

            if not Is_Array_Type (U_Ent) then
               Error_Msg_N ("component size requires array type", Nam);
               return;
            end if;

            Btype := Base_Type (U_Ent);

            if Has_Component_Size_Clause (Btype) then
               Error_Msg_N
                 ("component size clase for& previously given", Nam);

            elsif Csize /= No_Uint then
               Check_Size (Expr, Component_Type (Btype), Csize, Biased);

               --  For the biased case, build a declaration for a subtype
               --  that will be used to represent the biased subtype that
               --  reflects the biased representation of components. We need
               --  this subtype to get proper conversions on referencing
               --  elements of the array.

               if Biased then
                  New_Ctyp :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_External_Name (Chars (U_Ent), 'C', 0, 'T'));

                  Decl :=
                    Make_Subtype_Declaration (Loc,
                      Defining_Identifier => New_Ctyp,
                      Subtype_Indication  =>
                        New_Occurrence_Of (Component_Type (Btype), Loc));

                  Set_Parent (Decl, N);
                  Analyze (Decl, Suppress => All_Checks);

                  Set_Has_Delayed_Freeze        (New_Ctyp, False);
                  Set_Esize                     (New_Ctyp, Csize);
                  Set_RM_Size                   (New_Ctyp, Csize);
                  Set_Has_Biased_Representation (New_Ctyp, True);
                  Set_Is_Itype                  (New_Ctyp, True);
                  Set_Associated_Node_For_Itype (New_Ctyp, U_Ent);

                  Set_Component_Type (Btype, New_Ctyp);
               end if;

               Set_Component_Size            (Btype, Csize);
               Set_Has_Component_Size_Clause (Btype, True);
               Set_Has_Non_Standard_Rep      (Btype, True);
            end if;
         end Component_Size_Case;

         ------------------
         -- External_Tag --
         ------------------

         when Attribute_External_Tag => External_Tag :
         begin
            if not Is_Tagged_Type (U_Ent) then
               Error_Msg_N ("should be a tagged type", Nam);
            end if;

            Analyze_And_Resolve (Expr, Standard_String);

            if not Is_Static_Expression (Expr) then
               Error_Msg_N ("must be a static string", Nam);
            end if;

            Set_Has_External_Tag_Rep_Clause (U_Ent);
         end External_Tag;

         -----------
         -- Input --
         -----------

         when Attribute_Input => Input : declare
            Subp : Entity_Id := Empty;
            I    : Interp_Index;
            It   : Interp;

            function Has_Good_Profile (Subp : Entity_Id) return Boolean;
            --  Return true if the entity is a function with the good
            --  profile for the input attribute.

            function Has_Good_Profile (Subp : Entity_Id) return Boolean is
               F  : Entity_Id;
               Ok : Boolean := False;

            begin
               if Ekind (Subp) = E_Function then
                  F := First_Formal (Subp);

                  if Present (F) and then No (Next_Formal (F)) then
                     if Ekind (Etype (F)) = E_Anonymous_Access_Type
                       and then Designated_Type (Etype (F)) =
                         Class_Wide_Type (RTE (RE_Root_Stream_Type))
                     then
                        Ok := Base_Type (Etype (Subp)) = Base_Type (Ent);
                     end if;
                  end if;
               end if;
               return Ok;
            end Has_Good_Profile;

         --  Start of processing for Input

         begin
            Note_Feature (New_Representation_Clauses, Loc);
            FOnly := True;

            if not Is_Type (U_Ent) then
               Error_Msg_N ("local name must be a subtype", Nam);
               return;

            elsif Present (TSS (Base_Type (U_Ent), Name_uInput)) then
               Error_Msg_Sloc := Sloc (TSS (Base_Type (U_Ent), Name_uInput));
               Error_Msg_N ("input attribute already defined #", Nam);
               return;
            end if;

            Analyze (Expr);

            if Is_Entity_Name (Expr) then
               if not Is_Overloaded (Expr) then
                  if Has_Good_Profile (Entity (Expr)) then
                     Subp := Entity (Expr);
                  end if;

               else
                  Get_First_Interp (Expr, I, It);

                  while Present (It.Nam) loop
                     if Has_Good_Profile (It.Nam) then
                        Subp := It.Nam;
                        exit;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end if;
            end if;

            if Present (Subp) then
               Set_Entity (Expr, Subp);
               Set_Etype (Expr, Etype (Subp));
               New_Stream_Function (N, U_Ent, Subp,  Name_uInput);
            else
               Error_Msg_N ("incorrect expression for input attribute", Expr);
               return;
            end if;
         end Input;

         -------------------
         -- Machine_Radix --
         -------------------

         --  Machine radix attribute definition clause

         when Attribute_Machine_Radix => Machine_Radix : declare
            Radix : constant Uint := Static_Integer (Expr);

         begin
            Note_Feature (New_Representation_Clauses, Loc);

            if not Is_Decimal_Fixed_Point_Type (U_Ent) then
               Error_Msg_N ("decimal fixed-point type expected for &", Nam);

            elsif Has_Machine_Radix_Clause (U_Ent) then
               Error_Msg_Sloc := Sloc (Alignment_Clause (U_Ent));
               Error_Msg_N ("machine radix clause previously given#", N);

            elsif Radix /= No_Uint then
               Set_Has_Machine_Radix_Clause (U_Ent);
               Set_Has_Non_Standard_Rep (Base_Type (U_Ent));

               if Radix = 2 then
                  null;
               elsif Radix = 10 then
                  Set_Machine_Radix_10 (U_Ent);
               else
                  Error_Msg_N ("machine radix value must be 2 or 10", Expr);
               end if;
            end if;
         end Machine_Radix;

         -----------------
         -- Object_Size --
         -----------------

         --  Object_Size attribute definition clause

         when Attribute_Object_Size => Object_Size : declare
            Size   : constant Uint := Static_Integer (Expr);
            Etyp   : Entity_Id;
            Biased : Boolean;

         begin
            if not Is_Discrete_Or_Fixed_Point_Type (U_Ent) then
               Error_Msg_N ("Object_Size cannot be given for &", Nam);

            else
               Check_Size (Expr, U_Ent, Size, Biased);
               Set_Esize (U_Ent, Size);
            end if;
         end Object_Size;

         ------------
         -- Output --
         ------------

         when Attribute_Output => Output : declare
            Subp : Entity_Id := Empty;
            I    : Interp_Index;
            It   : Interp;

            function Has_Good_Profile (Subp : Entity_Id) return Boolean;
            --  return true if the entity is a procedure with the good
            --  profile for the output attribute.

            function Has_Good_Profile (Subp : Entity_Id) return Boolean is
               F  : Entity_Id;
               Ok : Boolean := False;

            begin
               if Ekind (Subp) = E_Procedure then
                  F := First_Formal (Subp);

                  if Present (F) then
                     if Ekind (Etype (F)) = E_Anonymous_Access_Type
                       and then Designated_Type (Etype (F)) =
                         Class_Wide_Type (RTE (RE_Root_Stream_Type))
                     then
                        F := Next_Formal (F);
                        Ok :=  Present (F)
                          and then Parameter_Mode (F) = E_In_Parameter
                          and then Base_Type (Etype (F)) = Base_Type (Ent)
                          and then No (Next_Formal (F));
                     end if;
                  end if;
               end if;
               return Ok;
            end Has_Good_Profile;

         begin
            Note_Feature (New_Representation_Clauses, Loc);
            FOnly := True;

            if not Is_Type (U_Ent) then
               Error_Msg_N ("local name must be a subtype", Nam);
               return;

            elsif Present (TSS (Base_Type (U_Ent), Name_uOutput)) then
               Error_Msg_Sloc := Sloc (TSS (Base_Type (U_Ent), Name_uOutput));
               Error_Msg_N ("output attribute already defined#", Nam);
               return;
            end if;

            Analyze (Expr);

            if Is_Entity_Name (Expr) then
               if not Is_Overloaded (Expr) then
                  if Has_Good_Profile (Entity (Expr)) then
                     Subp := Entity (Expr);
                  end if;

               else
                  Get_First_Interp (Expr, I, It);

                  while Present (It.Nam) loop
                     if Has_Good_Profile (It.Nam) then
                        Subp := It.Nam;
                        exit;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end if;
            end if;

            if Present (Subp) then
               Set_Entity (Expr, Subp);
               Set_Etype (Expr, Etype (Subp));
               New_Stream_Procedure (N, U_Ent, Subp, Name_uOutput);
            else
               Error_Msg_N ("incorrect expression for output attribute", Expr);
               return;
            end if;
         end Output;

         ----------
         -- Read --
         ----------

         when Attribute_Read => Read : declare
            Subp : Entity_Id := Empty;
            I    : Interp_Index;
            It   : Interp;

            function Has_Good_Profile (Subp : Entity_Id) return Boolean;
            --  return true if the entity is a procedure with the good
            --  profile for the read attribute.

            function Has_Good_Profile (Subp : Entity_Id) return Boolean is
               F     : Entity_Id;
               Ok    : Boolean := False;

            begin
               if Ekind (Subp) = E_Procedure then
                  F := First_Formal (Subp);

                  if Present (F) then
                     if Ekind (Etype (F)) = E_Anonymous_Access_Type
                       and then Designated_Type (Etype (F)) =
                        Class_Wide_Type (RTE (RE_Root_Stream_Type))
                     then
                        F := Next_Formal (F);
                        Ok :=  Present (F)
                          and then Parameter_Mode (F) = E_Out_Parameter
                          and then Base_Type (Etype (F)) = Base_Type (Ent)
                          and then No (Next_Formal (F));
                     end if;
                  end if;
               end if;
               return Ok;
            end Has_Good_Profile;

         begin
            Note_Feature (New_Representation_Clauses, Loc);
            FOnly := True;

            if not Is_Type (U_Ent) then
               Error_Msg_N ("local name must be a subtype", Nam);
               return;

            elsif Present (TSS (Base_Type (U_Ent), Name_uRead)) then
               Error_Msg_Sloc := Sloc (TSS (Base_Type (U_Ent), Name_uRead));
               Error_Msg_N ("read attribute already defined#", Nam);
               return;
            end if;

            Analyze (Expr);

            if Is_Entity_Name (Expr) then
               if not Is_Overloaded (Expr) then
                  if Has_Good_Profile (Entity (Expr)) then
                     Subp := Entity (Expr);
                  end if;

               else
                  Get_First_Interp (Expr, I, It);

                  while Present (It.Nam) loop
                     if Has_Good_Profile (It.Nam) then
                        Subp := It.Nam;
                        exit;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end if;
            end if;

            if Present (Subp) then
               Set_Entity (Expr, Subp);
               Set_Etype (Expr, Etype (Subp));
               New_Stream_Procedure (N, U_Ent, Subp, Name_uRead, True);
            else
               Error_Msg_N ("incorrect expression for read attribute", Expr);
               return;
            end if;
         end Read;

         ----------
         -- Size --
         ----------

         --  Size attribute definition clause

         when Attribute_Size => Size : declare
            Size   : constant Uint := Static_Integer (Expr);
            Etyp   : Entity_Id;
            Biased : Boolean;

         begin
            FOnly := True;

            if Has_Size_Clause (U_Ent) then
               Error_Msg_N ("size already given for &", Nam);

            elsif not Is_Type (U_Ent)
              and then Ekind (U_Ent) /= E_Variable
              and then Ekind (U_Ent) /= E_Constant
            then
               Error_Msg_N ("size cannot be given for &", Nam);

            elsif Is_Array_Type (U_Ent)
              and then not Is_Constrained (U_Ent)
            then
               Error_Msg_N
                 ("size cannot be given for unconstrained array", Nam);

            elsif Size /= No_Uint then

               if Is_Type (U_Ent) then
                  Etyp := U_Ent;
               else
                  Etyp := Etype (U_Ent);
               end if;

               --  Check size, note that Gigi is in charge of checking
               --  that the size of an array or record type is OK. Also
               --  we do not check the size in the ordinary fixed-point
               --  case, since it is too early to do so (there may be
               --  a subsequent small clause that affects the size). We
               --  can check the size if a small clause has already been
               --  given.

               if not Is_Ordinary_Fixed_Point_Type (U_Ent)
                 or else Has_Small_Clause (U_Ent)
               then
                  Check_Size (Expr, Etyp, Size, Biased);
                  Set_Has_Biased_Representation (U_Ent, Biased);
               end if;

               Set_Esize (U_Ent, Size);

               if Is_Discrete_Or_Fixed_Point_Type (U_Ent) then
                  Set_RM_Size (U_Ent, Size);
               end if;

               Set_Has_Size_Clause (U_Ent);
            end if;
         end Size;

         -----------
         -- Small --
         -----------

         --  Small attribute definition clause

         when Attribute_Small => Small : declare
            Int_Type      : Entity_Id;
            Implicit_Base : constant Entity_Id := Base_Type (U_Ent);
            Small         : Ureal;
            Size_Min      : Nat;

         begin
            Analyze_And_Resolve (Expr, Any_Real);

            if Etype (Expr) = Any_Type then
               return;

            elsif not Is_Static_Expression (Expr) then
               Error_Msg_N ("small requires static expression", Expr);
               return;

            else
               Small := Expr_Value_R (Expr);
            end if;

            if not Is_Ordinary_Fixed_Point_Type (U_Ent) then
               Error_Msg_N
                 ("small requires an ordinary fixed point type", Nam);

            elsif Has_Small_Clause (U_Ent) then
               Error_Msg_N ("small already given for &", Nam);

            elsif Small > Delta_Value (U_Ent) then
               Error_Msg_N
                 ("small value must not be greater then delta value", Nam);

            else
               Set_Small_Value (U_Ent, Small);
               Set_Small_Value (Implicit_Base, Small);
               Set_Has_Small_Clause (U_Ent);
               Set_Has_Small_Clause (Implicit_Base);
               Set_Has_Non_Standard_Rep (Implicit_Base);
            end if;
         end Small;

         ------------------
         -- Storage_Size --
         ------------------

         --  Storage_Size attribute definition clause

         when Attribute_Storage_Size => Storage_Size : declare
            Btype : constant Entity_Id := Base_Type (U_Ent);

         begin
            if Is_Task_Type (U_Ent) then
               FOnly := True;
            end if;

            if not Is_Access_Type (U_Ent)
              and then Ekind (U_Ent) /= E_Task_Type
            then
               Error_Msg_N ("storage size cannot be given for &", Nam);

            elsif Is_Access_Type (U_Ent) and Is_Derived_Type (U_Ent) then
               Error_Msg_N
                 ("storage size cannot be given for a derived access type",
                  Nam);

            elsif Has_Storage_Size_Clause (Btype) then
               Error_Msg_N ("storage size already given for &", Nam);

            else
               Analyze_And_Resolve (Expr, Any_Integer);

               if Is_Access_Type (U_Ent) then

                  if Present (Associated_Storage_Pool (U_Ent)) then
                     Error_Msg_N ("storage pool already given for &", Nam);
                     return;
                  end if;

                  if Compile_Time_Known_Value (Expr)
                    and then Expr_Value (Expr) = 0
                  then
                     Set_No_Pool_Assigned (Btype);
                  end if;
               end if;

               Set_Has_Storage_Size_Clause (Btype);
            end if;
         end Storage_Size;

         ------------------
         -- Storage_Pool --
         ------------------

         --  Storage_Pool attribute definition clause

         when Attribute_Storage_Pool => Storage_Pool : declare
            Pool : Entity_Id;

         begin
            Note_Feature (New_Representation_Clauses, Loc);
            Note_Feature (User_Defined_Storage_Pools, Loc);

            if Ekind (U_Ent) /= E_Access_Type
              and then Ekind (U_Ent) /= E_General_Access_Type
            then
               Error_Msg_N (
                 "storage pool can only be given for access types", Nam);
               return;

            elsif Is_Derived_Type (U_Ent) then
               Error_Msg_N
                 ("storage pool cannot be given for a derived access type",
                  Nam);

            elsif Has_Storage_Size_Clause (U_Ent) then
               Error_Msg_N ("storage size already given for &", Nam);
               return;

            elsif Present (Associated_Storage_Pool (U_Ent)) then
               Error_Msg_N ("storage pool already given for &", Nam);
               return;
            end if;

            Analyze_And_Resolve
              (Expr, Class_Wide_Type (RTE (RE_Root_Storage_Pool)));

            if Is_Entity_Name (Expr) then
               Pool := Entity (Expr);

               if Present (Etype (Pool))
                 and then Etype (Pool) /= RTE (RE_Stack_Bounded_Pool)
                 and then Etype (Pool) /= RTE (RE_Unbounded_Reclaim_Pool)
               then
                  Set_Associated_Storage_Pool (U_Ent, Pool);
               else
                  Error_Msg_N ("Non sharable GNAT Pool", Expr);
               end if;

            else
               Error_Msg_N ("incorrect reference to a Storage Pool", Expr);
               return;
            end if;
         end Storage_Pool;

         ----------------
         -- Value_Size --
         ----------------

         --  Value_Size attribute definition clause

         when Attribute_Value_Size => Value_Size : declare
            Size   : constant Uint := Static_Integer (Expr);
            Etyp   : Entity_Id;
            Biased : Boolean;

         begin
            if not Is_Discrete_Or_Fixed_Point_Type (U_Ent) then
               Error_Msg_N ("Value_Size cannot be given for &", Nam);

            else
               Check_Size (Expr, U_Ent, Size, Biased);
               Set_Has_Biased_Representation (U_Ent, Biased);
               Set_RM_Size (U_Ent, Size);
            end if;
         end Value_Size;

         -----------
         -- Write --
         -----------

         --  Write attribute definition clause
         --  check for class-wide case will be performed later

         when Attribute_Write => Write : declare
            Subp : Entity_Id := Empty;
            I    : Interp_Index;
            It   : Interp;

            function Has_Good_Profile (Subp : Entity_Id) return Boolean;
            --  return true if the entity is a procedure with the good
            --  profile for the write attribute.

            function Has_Good_Profile (Subp : Entity_Id) return Boolean is
               F     : Entity_Id;
               Ok    : Boolean := False;

            begin
               if Ekind (Subp) = E_Procedure then
                  F := First_Formal (Subp);

                  if Present (F) then
                     if Ekind (Etype (F)) = E_Anonymous_Access_Type
                       and then Designated_Type (Etype (F)) =
                         Class_Wide_Type (RTE (RE_Root_Stream_Type))
                     then
                        F := Next_Formal (F);
                        Ok :=  Present (F)
                          and then Parameter_Mode (F) = E_In_Parameter
                          and then Base_Type (Etype (F)) = Base_Type (Ent)
                          and then No (Next_Formal (F));
                     end if;
                  end if;
               end if;
               return Ok;
            end Has_Good_Profile;

         begin
            Note_Feature (New_Representation_Clauses, Loc);
            FOnly := True;

            if not Is_Type (U_Ent) then
               Error_Msg_N ("local name must be a subtype", Nam);
               return;

            elsif Present (TSS (Base_Type (U_Ent), Name_uWrite)) then
               Error_Msg_Sloc := Sloc (TSS (Base_Type (U_Ent), Name_uWrite));
               Error_Msg_N ("write attribute already defined#", Nam);
               return;
            end if;

            Analyze (Expr);

            if Is_Entity_Name (Expr) then
               if not Is_Overloaded (Expr) then
                  if Has_Good_Profile (Entity (Expr)) then
                     Subp := Entity (Expr);
                  end if;

               else
                  Get_First_Interp (Expr, I, It);

                  while Present (It.Nam) loop
                     if Has_Good_Profile (It.Nam) then
                        Subp := It.Nam;
                        exit;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end if;
            end if;

            if Present (Subp) then
               Set_Entity (Expr, Subp);
               Set_Etype (Expr, Etype (Subp));
               New_Stream_Procedure (N, U_Ent, Subp, Name_uWrite);
            else
               Error_Msg_N ("incorrect expression for write attribute", Expr);
               return;
            end if;
         end Write;

         --  All other attributes cannot be set

         when others =>
            Error_Msg_N
              ("attribute& cannot be set with definition clause", N);

      end case;

      --  The test for the type being frozen must be performed after
      --  any expression the clause has been analyzed since the expression
      --  itself might cause freezing that makes the clause illegal.

      if Rep_Item_Too_Late (Ent, N, FOnly) then
         return;
      end if;
   end Analyze_Attribute_Definition_Clause;

   ----------------------------
   -- Analyze_Code_Statement --
   ----------------------------

   procedure Analyze_Code_Statement (N : Node_Id) is
      HSS   : constant Node_Id   := Parent (N);
      SBody : constant Node_Id   := Parent (HSS);
      Subp  : constant Entity_Id := Current_Scope;
      Stmt  : Node_Id;
      Decl  : Node_Id;
      StmtO : Node_Id;
      DeclO : Node_Id;

   begin
      --  Analyze and check we get right type, note that this implements the
      --  requirement (RM 13.8(1)) that Machine_Code be with'ed, since that
      --  is the only way that Asm_Insn could possibly be visible.

      Analyze_And_Resolve (Expression (N));

      if Etype (Expression (N)) = Any_Type then
         return;
      elsif Etype (Expression (N)) /= RTE (RE_Asm_Insn) then
         Error_Msg_N ("incorrect type for code statement", N);
         return;
      end if;

      --  Make sure we appear in the handled statement sequence of a
      --  subprogram (RM 13.8(3)).

      if Nkind (HSS) /= N_Handled_Sequence_Of_Statements
        or else Nkind (SBody) /= N_Subprogram_Body
      then
         Error_Msg_N
           ("code statement can only appear in body of subprogram", N);
         return;
      end if;

      --  Do remaining checks (RM 13.8(3)) if not already done

      if not Is_Machine_Code_Subprogram (Subp) then
         Set_Is_Machine_Code_Subprogram (Subp);

         --  No exception handlers allowed

         if Present (Exception_Handlers (HSS)) then
            Error_Msg_N
              ("exception handlers not permitted in machine code subprogram",
               First (Exception_Handlers (HSS)));
         end if;

         --  No declarations other than use clauses and pragmas (we allow
         --  certain internally generated declarations as well).

         Decl := First (Declarations (SBody));
         while Present (Decl) loop
            DeclO := Original_Node (Decl);
            if Comes_From_Source (DeclO)
              and then Nkind (DeclO) /= N_Pragma
              and then Nkind (DeclO) /= N_Use_Package_Clause
              and then Nkind (DeclO) /= N_Use_Type_Clause
              and then Nkind (DeclO) /= N_Implicit_Label_Declaration
            then
               Error_Msg_N
                 ("this declaration not allowed in machine code subprogram",
                  DeclO);
            end if;

            Decl := Next (Decl);
         end loop;

         --  No statements other than code statements, pragmas, and labels.
         --  Again we allow certain internally generated statements.

         Stmt := First (Statements (HSS));
         while Present (Stmt) loop
            StmtO := Original_Node (Stmt);
            if Comes_From_Source (StmtO)
              and then Nkind (StmtO) /= N_Pragma
              and then Nkind (StmtO) /= N_Label
              and then Nkind (StmtO) /= N_Code_Statement
            then
               Error_Msg_N
                 ("this statement is not allowed in machine code subprogram",
                  StmtO);
            end if;

            Stmt := Next (Stmt);
         end loop;
      end if;

   end Analyze_Code_Statement;

   -----------------------------------------------
   -- Analyze_Enumeration_Representation_Clause --
   -----------------------------------------------

   procedure Analyze_Enumeration_Representation_Clause (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      Ident    : constant Node_Id    := Identifier (N);
      Aggr     : constant Node_Id    := Array_Aggregate (N);
      Enumtype : Entity_Id;
      Elit     : Entity_Id;
      Expr     : Node_Id;
      Assoc    : Node_Id;
      Choice   : Node_Id;
      Val      : Uint;
      Err      : Boolean := False;

      Lo  : constant Uint := Expr_Value (Type_Low_Bound (Universal_Integer));
      Hi  : constant Uint := Expr_Value (Type_High_Bound (Universal_Integer));
      Min : Uint;
      Max : Uint;

   begin
      --  First some basic error checks

      Find_Type (Ident);
      Enumtype := Entity (Ident);

      if Enumtype = Any_Type
        or else Rep_Item_Too_Early (Enumtype, N)
      then
         return;
      else
         Enumtype := Underlying_Type (Enumtype);
      end if;

      if not Is_Enumeration_Type (Enumtype) then
         Error_Msg_NE
           ("enumeration type required, found}",
            Ident, First_Subtype (Enumtype));
         return;
      end if;

      if Scope (Enumtype) /= Current_Scope then
         Error_Msg_N ("type must be declared in this scope", Ident);
         return;

      elsif not Is_First_Subtype (Enumtype) then
         Error_Msg_N ("cannot give enumeration rep clause for subtype", N);
         return;

      elsif Has_Enumeration_Rep_Clause (Enumtype) then
         Error_Msg_N ("duplicate enumeration rep clause ignored", N);
         return;

      elsif Root_Type (Enumtype) = Standard_Character
        or else Root_Type (Enumtype) = Standard_Wide_Character
        or else Root_Type (Enumtype) = Standard_Boolean
      then
         Error_Msg_N ("enumeration rep clause not allowed for this type", N);

      else
         Set_Has_Enumeration_Rep_Clause (Enumtype);
         Set_Has_Enumeration_Rep_Clause (Base_Type (Enumtype));
      end if;

      --  Now we process the aggregate. Note that we don't use the normal
      --  aggregate code for this purpose, because we don't want any of the
      --  normal expansion activities, and a number of special semantic
      --  rules apply (including the component type being any integer type)

      --  Badent signals that we found some incorrect entries processing
      --  the list. The final checks for completeness and ordering are
      --  skipped in this case.

      Elit := First_Literal (Enumtype);

      --  First the positional entries if any

      if Present (Expressions (Aggr)) then
         Expr := First (Expressions (Aggr));
         while Present (Expr) loop

            if No (Elit) then
               Error_Msg_N ("too many entries in aggregate", Expr);
               return;
            end if;

            Val := Static_Integer (Expr);

            if Val = No_Uint then
               Err := True;

            elsif Val < Lo or else Hi < Val then
               Error_Msg_N ("value outside permitted range", Expr);
               Err := True;
            end if;

            Set_Enumeration_Rep (Elit, Val);
            Set_Enumeration_Rep_Expr (Elit, Expr);
            Expr := Next (Expr);
            Elit := Next (Elit);
         end loop;
      end if;

      --  Now process the named entries if present

      if Present (Component_Associations (Aggr)) then
         Assoc := First (Component_Associations (Aggr));
         while Present (Assoc) loop
            Choice := First (Choices (Assoc));

            if Present (Next (Choice)) then
               Error_Msg_N
                 ("multiple choice not allowed here", Next (Choice));
               Err := True;
            end if;

            if Nkind (Choice) = N_Others_Choice then
               Error_Msg_N ("others choice not allowed here", Choice);
               Err := True;

            elsif Nkind (Choice) = N_Range then
               --  ??? should allow zero/one element range here
               Error_Msg_N ("range not allowed here", Choice);
               Err := True;

            else
               Analyze_And_Resolve (Choice, Enumtype);

               if Is_Entity_Name (Choice)
                 and then Is_Type (Entity (Choice))
               then
                  Error_Msg_N ("subtype name not allowed here", Choice);
                  Err := True;
                  --  ??? should allow static subtype with zero/one entry

               elsif Etype (Choice) = Base_Type (Enumtype) then
                  if not Is_Static_Expression (Choice) then
                     Error_Msg_N
                       ("non-static expression used for choice", Choice);
                     Err := True;
                  else
                     Elit := Expr_Value_E (Choice);

                     if Present (Enumeration_Rep_Expr (Elit)) then
                        Error_Msg_Sloc := Sloc (Enumeration_Rep_Expr (Elit));
                        Error_Msg_NE
                          ("representation for& previously given#",
                           Choice, Elit);
                        Err := True;
                     end if;

                     Set_Enumeration_Rep_Expr (Elit, Choice);

                     Val := Static_Integer (Expression (Assoc));

                     if Val = No_Uint then
                        Err := True;
                     elsif Val < Lo or else Hi < Val then
                        Error_Msg_N ("value outside permitted range", Expr);
                        Err := True;
                     end if;

                     Set_Enumeration_Rep (Elit, Val);
                  end if;
               end if;
            end if;

            Assoc := Next (Assoc);
         end loop;
      end if;

      --  Aggregate is fully processed. Now we check that a full set of
      --  representations was given, and that they are in range and in order.
      --  These checks are only done if no other errors occurred.

      if not Err then
         Min  := No_Uint;
         Max  := No_Uint;

         Elit := First_Literal (Enumtype);
         while Present (Elit) loop
            if No (Enumeration_Rep_Expr (Elit)) then
               Error_Msg_NE ("missing representation for&!", N, Elit);

            else
               Val := Enumeration_Rep (Elit);

               if Min = No_Uint then
                  Min := Val;
               end if;

               if Val /= No_Uint then
                  if Max /= No_Uint and then Val <= Max then
                     Error_Msg_NE
                       ("enumeration value for& not ordered!",
                                       Enumeration_Rep_Expr (Elit), Elit);
                  end if;

                  Max := Val;
               end if;

               --  If there is at least one literal whose representation
               --  is not equal to the Pos value, then note that this
               --  enumeration type has a non-standard representation.

               if Val /= Enumeration_Pos (Elit) then
                  Set_Has_Non_Standard_Rep (Base_Type (Enumtype));
               end if;
            end if;

            Elit := Next (Elit);
         end loop;
      end if;

      declare
         Minsize : Uint := UI_From_Int (Minimum_Size (Enumtype));

      begin
         if Has_Size_Clause (Enumtype) then
            if Esize (Enumtype) >= Minsize then
               null;
            else
               Minsize :=
                 UI_From_Int (Minimum_Size (Enumtype, Biased => True));

               if Esize (Enumtype) < Minsize then
                  Error_Msg_N ("previously given size is too small", N);

               else
                  Set_Has_Biased_Representation (Enumtype);
               end if;
            end if;

         else
            Set_RM_Size    (Enumtype, Minsize);
            Set_Enum_Esize (Enumtype);
         end if;

         Set_RM_Size (Base_Type (Enumtype), RM_Size (Enumtype));
         Set_Esize   (Base_Type (Enumtype), Esize (Enumtype));
      end;

      --  We repeat the too late test in case it froze itself!

      if Rep_Item_Too_Late (Enumtype, N) then
         null;
      end if;

   end Analyze_Enumeration_Representation_Clause;

   ----------------------------
   -- Analyze_Free_Statement --
   ----------------------------

   procedure Analyze_Free_Statement (N : Node_Id) is
   begin
      Analyze (Expression (N));
   end Analyze_Free_Statement;

   ------------------------------------------
   -- Analyze_Record_Representation_Clause --
   ------------------------------------------

   procedure Analyze_Record_Representation_Clause (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Ident   : constant Node_Id    := Identifier (N);
      Rectype : Entity_Id;
      CC      : Node_Id;
      Posit   : Uint;
      Fbit    : Uint;
      Lbit    : Uint;
      Hbit    : Uint := Uint_0;
      Comp    : Entity_Id;
      Ocomp   : Entity_Id;
      Biased  : Boolean;

      Max_Bit_So_Far : Uint;
      --  Records the maximum bit position so far. If all field positoins
      --  are monotonically increasing, then we can skip the circuit for
      --  checking for overlap, since no overlap is possible.

      Overlap_Check_Required : Boolean;
      --  Used to keep track of whether or not an overlap check is required

   begin
      Find_Type (Ident);
      Rectype := Entity (Ident);

      if Rectype = Any_Type
        or else Rep_Item_Too_Early (Rectype, N)
      then
         return;
      else
         Rectype := Underlying_Type (Rectype);
      end if;

      --  First some basic error checks

      if not Is_Record_Type (Rectype) then
         Error_Msg_NE
           ("record type required, found}", Ident, First_Subtype (Rectype));
         return;

      elsif Is_Unchecked_Union (Rectype) then
         Error_Msg_N
           ("record rep clause not allowed for Unchecked_Union", N);

      elsif Scope (Rectype) /= Current_Scope then
         Error_Msg_N ("type must be declared in this scope", N);
         return;

      elsif not Is_First_Subtype (Rectype) then
         Error_Msg_N ("cannot give record rep clause for subtype", N);
         return;

      elsif Has_Record_Rep_Clause (Rectype) then
         Error_Msg_N ("duplicate record rep clause ignored", N);
         return;

      elsif Rep_Item_Too_Late (Rectype, N) then
         return;
      end if;

      if Present (Mod_Clause (N)) then
         declare
            M : constant Node_Id := Mod_Clause (N);
            P : constant List_Id := Pragmas_Before (M);
            V : constant Uint    := Get_Alignment_Value (Expression (M));

         begin
            if Present (P) then
               Analyze_List (P);
            end if;
         end;
      end if;

      --  Clear any existing component clauses for the type (this happens
      --  with derived types, where we are now overriding the original)

      Comp := First_Entity (Rectype);
      while Present (Comp) loop
         if Ekind (Comp) = E_Component
           or else Ekind (Comp) = E_Discriminant
         then
            Set_Component_Clause (Comp, Empty);
         end if;

         Comp := Next_Entity (Comp);
      end loop;

      --  All done if no component clauses

      CC := First (Component_Clauses (N));

      if No (CC) then
         return;
      end if;

      Set_Has_Record_Rep_Clause (Rectype);
      Set_Has_Specified_Layout  (Rectype);

      --  A representation like this applies to the base type as well

      Set_Has_Record_Rep_Clause (Base_Type (Rectype));
      Set_Has_Non_Standard_Rep  (Base_Type (Rectype));
      Set_Has_Specified_Layout  (Base_Type (Rectype));

      Max_Bit_So_Far := Uint_Minus_1;
      Overlap_Check_Required := False;

      --  Process the component clauses

      while Present (CC) loop

         --  If pragma, just analyze it

         if Nkind (CC) = N_Pragma then
            Analyze (CC);

         --  Processing for real component clause

         else
            Posit := Static_Integer (Position  (CC));
            Fbit  := Static_Integer (First_Bit (CC));
            Lbit  := Static_Integer (Last_Bit  (CC));

            if Posit /= No_Uint
              and then Fbit /= No_Uint
              and then Lbit /= No_Uint
            then
               if Posit < 0 then
                  Error_Msg_N
                    ("position cannot be negative", Position (CC));

               elsif Fbit < 0 then
                  Error_Msg_N
                    ("first bit cannot be negative", First_Bit (CC));

               --  Values look OK, so find the corresponding record component

               else
                  Comp := First_Entity (Rectype);
                  while Present (Comp) loop
                     exit when Chars (Comp) = Chars (Component_Name (CC));
                     Comp := Next_Entity (Comp);
                  end loop;

                  if No (Comp) then
                     Error_Msg_N
                       ("component clause is for non-existent field", CC);

                  elsif Present (Component_Clause (Comp)) then
                     Error_Msg_Sloc := Sloc (Component_Clause (Comp));
                     Error_Msg_N
                       ("component clause previously given#", CC);

                  else
                     --  Update Fbit and Lbit to the actual bit number.

                     Fbit :=
                       Fbit + UI_From_Int (System_Storage_Unit) * Posit;
                     Lbit :=
                       Lbit + UI_From_Int (System_Storage_Unit) * Posit;

                     if Fbit <= Max_Bit_So_Far then
                        Overlap_Check_Required := True;
                     else
                        Max_Bit_So_Far := Lbit;
                     end if;

                     if Has_Size_Clause (Rectype)
                       and then Esize (Rectype) <= Lbit
                     then
                        Error_Msg_N
                          ("bit number out of range of specified size",
                           Last_Bit (CC));
                     else
                        Set_Component_Clause    (Comp, CC);
                        Set_Component_First_Bit (Comp, Fbit);
                        Set_Esize               (Comp, 1 + (Lbit - Fbit));

                        if Is_Tagged_Type (Rectype)
                          and then Fbit < System_Address_Size
                        then
                           Error_Msg_NE
                             ("component overlaps tag field of&",
                              CC, Rectype);
                        end if;

                        --  This information is also set in the
                        --  corresponding component of the base type,
                        --  found by accessing the Original_Record_Component
                        --  link if it is present.

                        Ocomp := Original_Record_Component (Comp);

                        if Hbit < Lbit then
                           Hbit := Lbit;
                        end if;

                        Check_Size (Component_Name (CC),
                          Etype (Comp), Esize (Comp), Biased);

                        Set_Has_Biased_Representation (Comp, Biased);

                        if Present (Ocomp) then
                           Set_Component_Clause
                             (Ocomp, CC);
                           Set_Component_First_Bit
                             (Ocomp, Fbit);
                           Set_Esize
                             (Ocomp, 1 + (Lbit - Fbit));
                           Set_Has_Biased_Representation
                             (Ocomp, Has_Biased_Representation (Comp));
                        end if;

                        if Esize (Comp) < 0 then
                           Error_Msg_N ("component size is negative", CC);
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;

         CC := Next (CC);
      end loop;

      --  Now that we have processed all the component clauses, check for
      --  overlap. We have to leave this till last, since the components
      --  can appear in any arbitrary order in the representation clause.
      --  We do not need this check if all specified ranges were monotonic.

      if Overlap_Check_Required then

         Overlap_Check : declare
            C1_Ent, C2_Ent : Entity_Id;
            --  Entities of components being checked for overlap

            Clist : Node_Id;
            --  Component_List node whose Component_Items are being checked

            Citem : Node_Id;
            --  Component declaration for component being checked

         begin
            C1_Ent := First_Entity (Base_Type (Rectype));

            --  Loop through all components in record. For each component check
            --  for overlap with any of the preceding elements on the component
            --  list containing the component, and also, if the component is in
            --  a variant, check against components outside the case structure.
            --  This latter test is repeated recursively up the variant tree.

            Main_Component_Loop : while Present (C1_Ent) loop
               if Ekind (C1_Ent) /= E_Component
                 and then Ekind (C1_Ent) /= E_Discriminant
               then
                  goto Continue_Main_Component_Loop;
               end if;

               --  Skip overlap check if entity has no declaration node. This
               --  happens with discriminants in constrained derived types.
               --  Probably we are missing some checks as a result, but that
               --  does not seem terribly serious ???

               if No (Declaration_Node (C1_Ent)) then
                  goto Continue_Main_Component_Loop;
               end if;

               Clist := Parent (List_Containing (Declaration_Node (C1_Ent)));

               --  Loop through component lists that need checking. Check the
               --  current component list and all lists in variants above us.

               Component_List_Loop : loop

                  --  If at outer level, check discriminants if there are any

                  if Nkind (Clist) = N_Full_Type_Declaration
                    or else Nkind (Clist) = N_Private_Type_Declaration
                  then
                     if Has_Discriminants (Defining_Identifier (Clist)) then
                        C2_Ent :=
                          First_Discriminant (Defining_Identifier (Clist));

                        while Present (C2_Ent) loop
                           exit when C1_Ent = C2_Ent;
                           Check_Component_Overlap (C1_Ent, C2_Ent);
                           C2_Ent := Next_Discriminant (C2_Ent);
                        end loop;
                     end if;

                  --  Otherwise check one component list

                  else
                     Citem := First (Component_Items (Clist));

                     while Present (Citem) loop
                        if Nkind (Citem) = N_Component_Declaration then
                           C2_Ent := Defining_Identifier (Citem);
                           exit when C1_Ent = C2_Ent;
                           Check_Component_Overlap (C1_Ent, C2_Ent);
                        end if;

                        Citem := Next (Citem);
                     end loop;
                  end if;

                  --  Check for variants above us (the parent of the Clist can
                  --  be a variant, in which case its parent is a variant part,
                  --  and the parent of the variant part is a component list
                  --  whose components must all be checked against the current
                  --  component for overlap.

                  if Nkind (Parent (Clist)) = N_Variant then
                     Clist := Parent (Parent (Parent (Clist)));

                  --  Check for possible discriminant part in record, this is
                  --  treated essentially as another level in the recursion.
                  --  For this case we have the parent of the component list
                  --  is the record definition, and its parent is the full
                  --  type declaration which contains the discriminant
                  --  specifications.

                  elsif Nkind (Parent (Clist)) = N_Record_Definition then
                     Clist := Parent (Parent ((Clist)));

                  --  If neither of these two cases, we are at the top of
                  --  the tree

                  else
                     exit Component_List_Loop;
                  end if;
               end loop Component_List_Loop;

               <<Continue_Main_Component_Loop>>
                  C1_Ent := Next_Entity (C1_Ent);

            end loop Main_Component_Loop;

         end Overlap_Check;
      end if;

      --  For records that have component clauses for all components, and
      --  whose size is less than or equal to 32, we need to know the size
      --  in the front end to activate possible packed array processing
      --  where the component type is a record.

      --  At this stage Hbit + 1 represents the first unused bit from all
      --  the component clauses processed, so if the component clauses are
      --  complete, then this is the length of the record.

      --  For records longer than System.Storage_Unit, and for those where
      --  not all components have component clauses, the back end determines
      --  the length (it may for example be appopriate to round up the size
      --  to some convenient boundary, based on alignment considerations etc).

      if Esize (Rectype) = 0 and then Hbit + 1 <= 32 then

         --  Nothing to do if at least one component with no component clause

         Comp := First_Entity (Rectype);
         while Present (Comp) loop
            if Ekind (Comp) = E_Component
              or else Ekind (Comp) = E_Discriminant
            then
               if No (Component_Clause (Comp)) then
                  return;
               end if;
            end if;

            Comp := Next_Entity (Comp);
         end loop;

         --  If we fall out of loop, all components have component clauses

         Set_Esize (Rectype, Hbit + 1);
      end if;

   end Analyze_Record_Representation_Clause;

   -----------------------------
   -- Check_Component_Overlap --
   -----------------------------

   procedure Check_Component_Overlap (C1_Ent, C2_Ent : Entity_Id) is
   begin
      if Present (Component_Clause (C1_Ent))
        and then Present (Component_Clause (C2_Ent))
      then
         declare
            S1 : constant Uint := Component_First_Bit (C1_Ent);
            S2 : constant Uint := Component_First_Bit (C2_Ent);
            E1 : constant Uint := S1 + Esize (C1_Ent);
            E2 : constant Uint := S2 + Esize (C2_Ent);

         begin
            if E2 <= S1 or else E1 <= S2 then
               null;
            else
               Error_Msg_Node_2 :=
                 Component_Name (Component_Clause (C2_Ent));
               Error_Msg_Sloc := Sloc (Error_Msg_Node_2);
               Error_Msg_Node_1 :=
                 Component_Name (Component_Clause (C1_Ent));
               Error_Msg_N
                 ("component& overlaps & #",
                  Component_Name (Component_Clause (C1_Ent)));
            end if;
         end;
      end if;
   end Check_Component_Overlap;

   -----------------------------------
   -- Check_Constant_Address_Clause --
   -----------------------------------

   procedure Check_Constant_Address_Clause
     (Expr  : Node_Id;
      U_Ent : Entity_Id)
   is
      Decl : constant Node_Id := Declaration_Node (U_Ent);

      procedure Check_At_Constant_Address (Nod : Node_Id);
      --  Checks that the given node N represents a name whose 'Address
      --  is constant (in the same sense as OK_Constant_Address_Clause,
      --  i.e. the address value is the same at the point of declaration
      --  of U_Ent and at the time of elaboration of the address clause.

      procedure Check_Expr_Constants (Nod : Node_Id);
      --  Checks that Nod meets the requirements for a constant address
      --  clause in the sense of the enclosing procedure.

      procedure Check_List_Constants (Lst : List_Id);
      --  Check that all elements of list Lst meet the requirements for a
      --  constant address clause in the sense of the enclosing procedure.

      -------------------------------
      -- Check_At_Constant_Address --
      -------------------------------

      procedure Check_At_Constant_Address (Nod : Node_Id) is
      begin
         if Is_Entity_Name (Nod) then
            return;

         elsif Nkind (Nod) = N_Selected_Component then
            Check_At_Constant_Address (Prefix (Nod));

         elsif Nkind (Nod) = N_Indexed_Component then
            Check_At_Constant_Address (Prefix (Nod));
            Check_List_Constants (Expressions (Nod));

         else
            Check_Expr_Constants (Nod);
         end if;
      end Check_At_Constant_Address;

      --------------------------
      -- Check_Expr_Constants --
      --------------------------

      procedure Check_Expr_Constants (Nod : Node_Id) is
      begin
         if Etype (Nod) = Any_Type then
            return;
         end if;

         case Nkind (Nod) is
            when N_Empty | N_Error =>
               return;

            when N_Identifier | N_Expanded_Name =>
               declare
                  Ent       : constant Entity_Id  := Entity (Nod);
                  Loc_Ent   : constant Source_Ptr := Sloc (Ent);
                  Loc_U_Ent : constant Source_Ptr := Sloc (U_Ent);

               begin
                  if Ekind (Ent) = E_Named_Integer
                       or else
                     Ekind (Ent) = E_Named_Real
                       or else
                     Is_Type (Ent)
                  then
                     return;

                  elsif
                     Ekind (Ent) = E_Constant
                       or else
                     Ekind (Ent) = E_In_Parameter
                  then
                     --  This is the case where we must have Ent defined
                     --  before U_Ent. Clearly if they are in different
                     --  units this requirement is met since the unit
                     --  containing Ent is already processed.

                     if Get_Sloc_Unit_Number (Loc_Ent) /=
                        Get_Sloc_Unit_Number (Loc_U_Ent)
                     then
                        return;

                     --  Otherwise location of Ent must be before the
                     --  location of U_Ent, that's what prior defined means.

                     elsif Loc_Ent < Loc_U_Ent then
                        return;

                     else
                        Error_Msg_NE
                          ("invalid address clause for initialized object &!",
                           Nod, U_Ent);
                        Error_Msg_Name_1 := Chars (Ent);
                        Error_Msg_Name_2 := Chars (U_Ent);
                        Error_Msg_N
                          ("\% must be defined before % ('R'M 13.1(22))!",
                           Nod);
                     end if;

                  else
                     Error_Msg_NE
                       ("invalid address clause for initialized object &!",
                        Nod, U_Ent);
                     Error_Msg_Name_1 := Chars (Ent);
                     Error_Msg_N
                       ("\reference to variable% not allowed ('R'M 13.1(22))!",
                        Nod);
                  end if;
               end;

            when N_Integer_Literal   |
                 N_Real_Literal      |
                 N_String_Literal    |
                 N_Character_Literal =>
               return;

            when N_Range =>
               Check_Expr_Constants (Low_Bound (Nod));
               Check_Expr_Constants (High_Bound (Nod));

            when N_Explicit_Dereference =>
               Check_Expr_Constants (Prefix (Nod));

            when N_Indexed_Component =>
               Check_Expr_Constants (Prefix (Nod));
               Check_List_Constants (Expressions (Nod));

            when N_Slice =>
               Check_Expr_Constants (Prefix (Nod));
               Check_Expr_Constants (Discrete_Range (Nod));

            when N_Selected_Component =>
               Check_Expr_Constants (Prefix (Nod));

            when N_Attribute_Reference =>

               if (Attribute_Name (Nod) = Name_Address
                    or else
                   Attribute_Name (Nod) = Name_Access
                    or else
                   Attribute_Name (Nod) = Name_Unchecked_Access
                    or else
                   Attribute_Name (Nod) = Name_Unrestricted_Access)
               then
                  Check_At_Constant_Address (Prefix (Nod));

               else
                  Check_Expr_Constants (Prefix (Nod));
                  Check_List_Constants (Expressions (Nod));
               end if;

            when N_Aggregate =>
               Check_List_Constants (Component_Associations (Nod));
               Check_List_Constants (Expressions (Nod));

            when N_Component_Association =>
               Check_Expr_Constants (Expression (Nod));

            when N_Extension_Aggregate =>
               Check_Expr_Constants (Ancestor_Part (Nod));
               Check_List_Constants (Component_Associations (Nod));
               Check_List_Constants (Expressions (Nod));

            when N_Null =>
               return;

            when N_Binary_Op | N_And_Then | N_Or_Else | N_In | N_Not_In =>
               Check_Expr_Constants (Left_Opnd (Nod));
               Check_Expr_Constants (Right_Opnd (Nod));

            when N_Unary_Op =>
               Check_Expr_Constants (Right_Opnd (Nod));

            when N_Type_Conversion           |
                 N_Qualified_Expression      |
                 N_Allocator                 |
                 N_Unchecked_Type_Conversion =>
               Check_Expr_Constants (Expression (Nod));

            when N_Function_Call =>
               if not Is_Pure (Entity (Name (Nod))) then
                  Error_Msg_NE
                    ("invalid address clause for initialized object &!",
                     Nod, U_Ent);

                  Error_Msg_NE
                    ("\function & is not pure ('R'M 13.1(22))!",
                     Nod, Entity (Name (Nod)));

               else
                  Check_List_Constants (Parameter_Associations (Nod));
               end if;

            when N_Parameter_Association =>
               Check_Expr_Constants (Explicit_Actual_Parameter (Nod));

            when others =>
               Error_Msg_NE
                 ("invalid address clause for initialized object &!",
                  Nod, U_Ent);
               Error_Msg_NE
                 ("\must be constant defined before& ('R'M 13.1(22))!",
                  Nod, U_Ent);
         end case;
      end Check_Expr_Constants;

      --------------------------
      -- Check_List_Constants --
      --------------------------

      procedure Check_List_Constants (Lst : List_Id) is
         Nod1 : Node_Id;

      begin
         if Present (Lst) then
            Nod1 := First (Lst);
            while Present (Nod1) loop
               Check_Expr_Constants (Nod1);
               Nod1 := Next (Nod1);
            end loop;
         end if;
      end Check_List_Constants;

   --  Start of processing for Check_Constant_Address_Clause

   begin
      Check_Expr_Constants (Expr);
   end Check_Constant_Address_Clause;

   ----------------
   -- Check_Size --
   ----------------

   procedure Check_Size
     (N      : Node_Id;
      T      : Entity_Id;
      Siz    : Uint;
      Biased : out Boolean)
   is
      UT : constant Entity_Id := Underlying_Type (T);
      M  : Uint;

   begin
      Biased := False;

      --  Immediate return if size is same as standard size or if composite
      --  item with no size available (i.e. none was given explicitly) or
      --  generic type, or type with previous errors.

      if No (UT)
        or else UT = Any_Type
        or else Is_Generic_Type (UT)
        or else Is_Generic_Type (Root_Type (UT))
        or else (Is_Composite_Type (UT) and then Esize (UT) = 0)
        or else Siz = Esize (UT)
      then
         return;

      --  If the type is a fat pointer, then allow specifying a thin
      --  pointer.

      elsif Is_Access_Type (UT)
         and then Esize (UT) = System_Address_Size * 2
         and then Siz >= System_Address_Size
         and then Siz < System_Address_Size * 2
      then
         null;

      --  If the type is a thin pointer to a non-Taft-amendment type that
      --  is an unconstrained array, allow it to be a fat pointer.

      elsif Is_Access_Type (UT)
         and then not Has_Completion_In_Body (Designated_Type (UT))
         and then Is_Array_Type (Designated_Type (UT))
         and then not Is_Constrained (Designated_Type (UT))
         and then Esize (UT) = System_Address_Size
         and then Siz >= System_Address_Size * 2
      then
         null;

      --  If the type is a fat pointer, allow it to be a thin pointer.

      elsif Is_Access_Type (UT)
         and then Esize (UT) = System_Address_Size * 2
         and then Siz >= System_Address_Size
      then
         null;

      --  For most types, we can be bigger than, but not smaller
      --  than, the standard size.

      elsif Is_Composite_Type (UT)
        or else Is_Floating_Point_Type (UT)
        or else Is_Access_Type (UT)
      then
         if Siz < Esize (UT) then
            if Is_Access_Type (UT) then
               Error_Msg_Uint_1 := UI_From_Int (System_Address_Size);
            else
               Error_Msg_Uint_1 := Esize (UT);
            end if;

            Error_Msg_NE
              ("size for& too small, minimum allowed is ^", N, T);
         end if;

      --  For fixed-point types, don't check minimum if type is not frozen,
      --  since type is not known till then
      --  at freeze time.

      elsif Is_Fixed_Point_Type (UT)
        and then not Is_Frozen (UT)
      then
         null;

      --  Cases for which a minimum check is required

      else
         M := UI_From_Int (Minimum_Size (UT));

         if Siz < M then
            M := UI_From_Int (Minimum_Size (UT, Biased => True));

            if Siz < M then
               Error_Msg_Uint_1 := M;
               Error_Msg_NE
                 ("size for& too small, minimum allowed is ^", N, T);
            else
               Biased := True;
            end if;
         end if;
      end if;
   end Check_Size;

   --------------------
   -- Defined_Before --
   --------------------

   function Defined_Before (E1, E2 : Entity_Id) return Boolean is
      Loc1 : constant Source_Ptr := Sloc (E1);
      Loc2 : constant Source_Ptr := Sloc (E2);

   begin
      --  If E1 is in a different unit from E2, then clearly is is already
      --  defined, since the unit containing E1 is already processed.

      if Get_Sloc_Unit_Number (Loc1) /= Get_Sloc_Unit_Number (Loc2) then
         return True;

      --  Otherwise location of E1 must be less than location of E2

      else
         return Loc1 < Loc2;
      end if;
   end Defined_Before;

   -------------------------
   -- Get_Alignment_Value --
   -------------------------

   function Get_Alignment_Value (Expr : Node_Id) return Uint is
      Align : constant Uint := Static_Integer (Expr);

   begin
      if Align = No_Uint then
         return No_Uint;

      elsif Align <= 0 then
         Error_Msg_N ("alignment value must be positive", Expr);
         return No_Uint;

      else
         for J in Int range 0 .. 64 loop
            declare
               M : constant Uint := Uint_2 ** J;

            begin
               exit when M = Align;

               if M > Align then
                  Error_Msg_N
                    ("alignment value must be power of 2", Expr);
                  return No_Uint;
               end if;
            end;
         end loop;

         return Align;
      end if;
   end Get_Alignment_Value;

   ------------------
   -- Minimum_Size --
   ------------------

   function Minimum_Size
     (T      : Entity_Id;
      Biased : Boolean := False)
      return   Nat
   is
      R_Type   : constant Entity_Id := Root_Type (T);
      Lo, Hi   : Uint;
      LoR, HiR : Ureal;
      LoSet    : Boolean := False;
      HiSet    : Boolean := False;
      B        : Uint;
      S        : Nat;
      Ancest   : Entity_Id;

   begin
      --  If bad type, return 0

      if T = Any_Type then
         return 0;

      --  For generic types, just return zero. There cannot be any legitimate
      --  need to know such a size, but this routine may be called with a
      --  generic type as part of normal processing.

      elsif Is_Generic_Type (T) then
         return 0;

      --  Discrete types

      elsif Is_Discrete_Type (T) then

         --  The following loop is looking for the nearest compile time
         --  known bounds following the ancestor subtype chain. The idea
         --  is to find the most restrictive known bounds information.

         Ancest := T;
         loop
            if Ancest = Any_Type or else Etype (Ancest) = Any_Type then
               return 0;
            end if;

            if not LoSet then
               if Compile_Time_Known_Value (Type_Low_Bound (Ancest)) then
                  Lo := Expr_Rep_Value (Type_Low_Bound (Ancest));
                  LoSet := True;
                  exit when HiSet;
               end if;
            end if;

            if not HiSet then
               if Compile_Time_Known_Value (Type_High_Bound (Ancest)) then
                  Hi := Expr_Rep_Value (Type_High_Bound (Ancest));
                  HiSet := True;
                  exit when LoSet;
               end if;
            end if;

            Ancest := Ancestor_Subtype (Ancest);

            if No (Ancest) then
               Ancest := Base_Type (T);

               if Is_Generic_Type (Ancest) then
                  return 0;
               end if;
            end if;
         end loop;

      --  Fixed-point types. We can't simply use Expr_Value to get the
      --  Corresponding_Integer_Value values of the bounds, since these
      --  do not get set till the type is frozen, and this routine can
      --  be called before the type is frozen. Similarly the test for
      --  bounds being static needs to include the case where we have
      --  unanalyzed real literals for the same reason.

      elsif Is_Fixed_Point_Type (T) then

         --  The following loop is looking for the nearest compile time
         --  known bounds following the ancestor subtype chain. The idea
         --  is to find the most restrictive known bounds information.

         Ancest := T;
         loop
            if Ancest = Any_Type or else Etype (Ancest) = Any_Type then
               return 0;
            end if;

            if not LoSet then
               if Nkind (Type_Low_Bound (Ancest)) = N_Real_Literal
                 or else Compile_Time_Known_Value (Type_Low_Bound (Ancest))
               then
                  LoR := Expr_Value_R (Type_Low_Bound (Ancest));
                  LoSet := True;
                  exit when HiSet;
               end if;
            end if;

            if not HiSet then
               if Nkind (Type_High_Bound (Ancest)) = N_Real_Literal
                 or else Compile_Time_Known_Value (Type_High_Bound (Ancest))
               then
                  HiR := Expr_Value_R (Type_High_Bound (Ancest));
                  HiSet := True;
                  exit when LoSet;
               end if;
            end if;

            Ancest := Ancestor_Subtype (Ancest);

            if No (Ancest) then
               Ancest := Base_Type (T);

               if Is_Generic_Type (Ancest) then
                  return 0;
               end if;
            end if;
         end loop;

         Lo := UR_To_Uint (LoR / Small_Value (T));
         Hi := UR_To_Uint (HiR / Small_Value (T));

      --  No other types allowed

      else
         pragma Assert (False);
         null;
      end if;

      if (Biased and then not Is_Fixed_Point_Type (T))
        or else Has_Biased_Representation (T)
      then
         Hi := Hi - Lo;
         Lo := Uint_0;
      end if;

      --  Signed case

      if Lo < 0 then
         S := 1;
         B := Uint_1;

         --  S = size, B = 2 ** (size - 1) (can accomodate -B .. +(B - 1))

         while Lo < -B or else Hi >= B loop
            B := Uint_2 ** S;
            S := S + 1;
         end loop;

      --  Unsigned case

      else
         S := 0;
         B := Uint_1;

         --  S = size, (can accomodate 0 .. (2**size - 1))

         while Hi >= Uint_2 ** S loop
            S := S + 1;
         end loop;
      end if;

      return S;
   end Minimum_Size;

   --------------------------
   -- New_Stream_Procedure --
   --------------------------

   procedure New_Stream_Procedure
     (N     : Node_Id;
      Ent   : Entity_Id;
      Subp  : Entity_Id;
      Nam   : Name_Id;
      Out_P : Boolean := False)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Subp_Id   : Entity_Id := Make_Defining_Identifier (Loc, Nam);
      Subp_Decl : Node_Id;
      F         : Entity_Id;
      Etyp      : Entity_Id;

   begin
      F        := First_Formal (Subp);
      Etyp     := Etype (Next_Formal (F));

      Subp_Decl :=
        Make_Subprogram_Renaming_Declaration (Loc,
          Specification =>

            Make_Procedure_Specification (Loc,
              Defining_Unit_Name => Subp_Id,
              Parameter_Specifications =>
                New_List (
                  Make_Parameter_Specification (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc, Name_S),
                    Parameter_Type =>
                      Make_Access_Definition (Loc,
                        Subtype_Mark =>
                          New_Reference_To (
                            Designated_Type (Etype (F)), Loc))),

                  Make_Parameter_Specification (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc,
                        Chars => New_Internal_Name ('V')),
                    Out_Present => Out_P,
                    Parameter_Type =>
                      New_Reference_To (Etyp, Loc)))),
        Name => New_Reference_To (Subp, Loc));

      if Is_Tagged_Type (Ent) and then not Is_Limited_Type (Ent) then
         Set_TSS (Base_Type (Ent), Subp_Id);
      else
         Insert_Action (N, Subp_Decl);
         Copy_TSS (Subp_Id, Base_Type (Ent));
      end if;

   end New_Stream_Procedure;

   -------------------------
   -- New_Stream_Function --
   -------------------------

   procedure New_Stream_Function
     (N    : Node_Id;
      Ent  : Entity_Id;
      Subp : Entity_Id;
      Nam  : Name_Id)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Subp_Id   : Entity_Id := Make_Defining_Identifier (Loc, Nam);
      Subp_Decl : Node_Id;
      F         : Entity_Id;
      Etyp      : Entity_Id;

   begin
      F        := First_Formal (Subp);
      Etyp     := Etype (Subp);

      Subp_Decl :=
        Make_Subprogram_Renaming_Declaration (Loc,
          Specification =>

            Make_Function_Specification (Loc,
              Defining_Unit_Name => Subp_Id,
              Parameter_Specifications =>
                New_List (
                  Make_Parameter_Specification (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc, Name_S),
                    Parameter_Type =>
                      Make_Access_Definition (Loc,
                        Subtype_Mark =>
                          New_Reference_To (
                            Designated_Type (Etype (F)), Loc)))),

              Subtype_Mark =>
                New_Reference_To (Etyp, Loc)),

        Name => New_Reference_To (Subp, Loc));

      if Is_Tagged_Type (Ent) and then not Is_Limited_Type (Ent) then
         Set_TSS (Base_Type (Ent), Subp_Id);
      else
         Insert_Action (N, Subp_Decl);
         Copy_TSS (Subp_Id, Base_Type (Ent));
      end if;

   end New_Stream_Function;

   ------------------------
   -- Rep_Item_Too_Early --
   ------------------------

   function Rep_Item_Too_Early
     (T     : Entity_Id;
      N     : Node_Id)
      return  Boolean
   is
   begin
      --  Cannot apply rep items to generic types

      if Is_Type (T)
        and then Is_Generic_Type (Root_Type (T))
      then
         Error_Msg_N
           ("representation item not allowed for generic type", N);
         return True;
      end if;

      --  Otherwise check for incompleted type

      if Is_Incomplete_Or_Private_Type (T)
        and then No (Underlying_Type (T))
      then
         Error_Msg_N
           ("representation item must be after full type declaration", N);
         return True;
      else
         return False;
      end if;
   end Rep_Item_Too_Early;

   -----------------------
   -- Rep_Item_Too_Late --
   -----------------------

   function Rep_Item_Too_Late
     (T     : Entity_Id;
      N     : Node_Id;
      FOnly : Boolean := False)
      return  Boolean
   is
      S           : Entity_Id;
      Parent_Type : Entity_Id;

      procedure Too_Late;
      --  Output the too late message

      procedure Too_Late is
      begin
         Error_Msg_N ("representation item appears too late!", N);
      end Too_Late;

   --  Start of processing for Rep_Item_Too_Late

   begin
      --  First make sure entity is not frozen (RM 13.1(9))

      if Is_Frozen (T) then
         Too_Late;
         S := First_Subtype (T);

         if Present (Freeze_Node (S)) then
            Error_Msg_NE
              ("?no more representation items for }!", Freeze_Node (S), S);
         end if;

         return True;

      --  Check for case of non-tagged derived type whose parent either has
      --  primitive operations, or is a by reference type (RM 13.1(10)).

      elsif Is_Type (T)
        and then not FOnly
        and then Is_Derived_Type (T)
        and then not Is_Tagged_Type (T)
      then
         Parent_Type := Etype (Base_Type (T));

         if Has_Primitive_Operations (Parent_Type) then
            Too_Late;
            Error_Msg_NE
              ("primitive operations already defined for&!", N, Parent_Type);
            return True;

         elsif Is_By_Reference_Type (Parent_Type) then
            Too_Late;
            Error_Msg_NE
              ("parent type & is a by reference type!", N, Parent_Type);
            return True;
         end if;
      end if;

      --  No error, link item into head of chain of rep items for the entity

      Set_Next_Rep_Item (N, First_Rep_Item (T));
      Set_First_Rep_Item (T, N);
      return False;
   end Rep_Item_Too_Late;

   -------------------------
   -- Same_Representation --
   -------------------------

   function Same_Representation (Typ1, Typ2 : Entity_Id) return Boolean is
      T1 : constant Entity_Id := Underlying_Type (Typ1);
      T2 : constant Entity_Id := Underlying_Type (Typ2);

   begin
      --  A quick check, if base types are the same, then we definitely have
      --  the same representation, because the subtype specific representation
      --  attributes (Size and Alignment) do not affect representation from
      --  the point of view of this test.

      if Base_Type (T1) = Base_Type (T2) then
         return True;
      end if;

      --  Tagged types never have differing representations

      if Is_Tagged_Type (T1) then
         return True;
      end if;

      --  Representations are definitely different if conventions differ

      if Convention (T1) /= Convention (T2) then
         return False;
      end if;

      --  Representations are different if component alignments differ

      if (Is_Record_Type (T1) or else Is_Array_Type (T1))
        and then
         (Is_Record_Type (T2) or else Is_Array_Type (T2))
        and then Component_Alignment (T1) /= Component_Alignment (T2)
      then
         return False;
      end if;

      --  Types definitely have same representation if neither has non-standard
      --  representation since default representations are always consistent.
      --  If only one has non-standard representation, and the other does not,
      --  then we consider that they do not have the same representation. They
      --  might, but there is no way of telling early enough.

      if Has_Non_Standard_Rep (T1) then
         if not Has_Non_Standard_Rep (T2) then
            return False;
         end if;
      else
         return not Has_Non_Standard_Rep (T2);
      end if;

      --  Here the two types both have non-standard representation, and we
      --  need to determine if they have the same non-standard representation

      --  For arrays, we simply need to test if the component sizes are the
      --  same. Pragma Pack is reflected in modified component sizes, so this
      --  check also deals with pragma Pack.

      if Is_Array_Type (T1) then
         return Component_Size (T1) = Component_Size (T2);

      --  Tagged types always have the same representation, because it is not
      --  possible to specify different representations for common fields.

      elsif Is_Tagged_Type (T1) then
         return True;

      --  Case of record types

      elsif Is_Record_Type (T1) then

         --  Packed status must conform

         if Is_Packed (T1) /= Is_Packed (T2) then
            return False;

         --  Otherwise we must check components

         else
            Record_Case : declare
               CD1, CD2 : Entity_Id;

               function Same_Rep return Boolean;
               --  CD1 and CD2 are either components or discriminants. This
               --  function tests whether the two have the same representation

               function Same_Rep return Boolean is
               begin
                  if No (Component_Clause (CD1)) then
                     return No (Component_Clause (CD2));

                  else
                     return
                        Present (Component_Clause (CD2))
                          and then
                        Component_First_Bit (CD1) = Component_First_Bit (CD2)
                          and then
                        Esize (CD1) = Esize (CD2);
                  end if;
               end Same_Rep;

            --  Start processing for Record_Case

            begin
               if Has_Discriminants (T1) then
                  CD1 := First_Discriminant (T1);
                  CD2 := First_Discriminant (T2);

                  while Present (CD1) loop
                     if not Same_Rep then
                        return False;
                     else
                        CD1 := Next_Discriminant (CD1);
                        CD2 := Next_Discriminant (CD2);
                     end if;
                  end loop;
               end if;

               CD1 := First_Component (T1);
               CD2 := First_Component (T2);

               while Present (CD1) loop
                  if not Same_Rep then
                     return False;
                  else
                     CD1 := Next_Component (CD1);
                     CD2 := Next_Component (CD2);
                  end if;
               end loop;

               return True;
            end Record_Case;
         end if;

      --  For enumeration types, we must check each literal to see if the
      --  representation is the same. Note that we do not permit enumeration
      --  reprsentation clauses for Character and Wide_Character, so these
      --  cases were already dealt with.

      elsif Is_Enumeration_Type (T1) then

         Enumeration_Case : declare
            L1, L2 : Entity_Id;

         begin
            L1 := First_Literal (T1);
            L2 := First_Literal (T2);

            while Present (L1) loop
               if Enumeration_Rep (L1) /= Enumeration_Rep (L2) then
                  return False;
               else
                  L1 := Next_Literal (L1);
                  L2 := Next_Literal (L2);
               end if;
            end loop;

            return True;

         end Enumeration_Case;

      --  Any other types have the same representation for these purposes

      else
         return True;
      end if;

   end Same_Representation;

   --------------------
   -- Set_Enum_Esize --
   --------------------

   procedure Set_Enum_Esize (T : Entity_Id) is
      Lo : Uint;
      Hi : Uint;
      Sz : Nat;

   begin
      --  Find the minimum standard size (8,16,32,64) that fits

      Lo := Enumeration_Rep (Entity (Type_Low_Bound (T)));
      Hi := Enumeration_Rep (Entity (Type_High_Bound (T)));

      if Lo < 0 then
         if Lo >= -Uint_2**07 and then Hi < Uint_2**07 then
            Sz := 8;

         elsif Lo >= -Uint_2**15 and then Hi < Uint_2**15 then
            Sz := 16;

         elsif Lo >= -Uint_2**31 and then Hi < Uint_2**31 then
            Sz := 32;

         elsif Lo >= -Uint_2**63 and then Hi < Uint_2**63 then
            Sz := 64;

         else
            pragma Assert (False); null;
         end if;

      else
         if Hi < Uint_2**08 then
            Sz := 8;

         elsif Hi < Uint_2**16 then
            Sz := 16;

         elsif Hi < Uint_2**32 then
            Sz := 32;

         elsif Hi < Uint_2**63 then
            Sz := 64;

         else
            pragma Assert (False); null;
         end if;
      end if;

      --  That minimum is the proper size unless we have a foreign convention
      --  and the size required is 32 or less, in which case we bump the size
      --  up to 32. This is required for C and C++ and seems reasonable for
      --  all other foreign conventions.

      if Has_Foreign_Convention (T)
        and then Esize (T) < Standard_Integer_Size
      then
         Set_Esize (T, Standard_Integer_Size);

      else
         Set_Esize (T, Sz);
      end if;

   end Set_Enum_Esize;

   -----------------------------------
   -- Validate_Unchecked_Conversion --
   -----------------------------------

   procedure Validate_Unchecked_Conversion
     (N        : Node_Id;
      Act_Unit : Entity_Id)
   is
      Source : Entity_Id;
      Target : Entity_Id;

   begin
      --  Obtain source and target types. Note that we call Ancestor_Subtype
      --  here because the processing for generic instantiation always makes
      --  subtypes, and we want the original frozen actual types.

      --  If we are dealing with private types, then do the check on their
      --  fully declared counterparts if the full declarations have been
      --  encountered (they don't have to be visible, but they must exist!)

      Source := Ancestor_Subtype (Etype (First_Formal (Act_Unit)));

      if Is_Private_Type (Source)
        and then Present (Underlying_Type (Source))
      then
         Source := Underlying_Type (Source);
      end if;

      Target := Ancestor_Subtype (Etype (Act_Unit));

      if Is_Private_Type (Target)
        and then Present (Underlying_Type (Target))
      then
         Target := Underlying_Type (Target);
      end if;

      --  Source may be unconstrained array, but not target

      if Is_Array_Type (Target)
        and then not Is_Constrained (Target)
      then
         Error_Msg_N
           ("unchecked conversion to unconstrained array not allowed", N);
         return;
      end if;

      --  Check unequal sizes

      declare
         Source_Siz : Uint;
         Target_Siz : Uint;
         Vnode      : Node_Id;

      begin
         --  This validation check, which warns if we have unequal sizes
         --  for unchecked conversion, and thus potentially implementation
         --  dependent semantics, is one of the few occasions on which we
         --  use the official RM size instead of Esize. See description
         --  in Einfo "Handling of Type'Size Values" for details.

         Source_Siz := Get_RM_Size (Source);
         Target_Siz := Get_RM_Size (Target);

         if (Comes_From_Source (Source)
              or else Sloc (Source) <= Standard_Location)
           and then
            (Comes_From_Source (Target)
              or else Sloc (Target) <= Standard_Location)
         then
            --  If both sizes are known by the front end, then we can do
            --  the validation of matching sizes in the front end.

            if Source_Siz /= 0
              and then Target_Siz /= 0
            then
               if Source_Siz /= Target_Siz then
                  Error_Msg_N
                    ("types for unchecked conversion have different sizes?",
                     N);
               end if;

            --  Otherwise we generate the special semantic node that will
            --  cause gigi to do the appropriate check in the back end.
            --  Notice here that go to

            else
               Vnode :=
                 Make_Validate_Unchecked_Conversion (Sloc (N));
               Set_Source_Type (Vnode, Source);
               Set_Target_Type (Vnode, Target);
               Insert_After (N, Vnode);
            end if;
         end if;
      end;
   end Validate_Unchecked_Conversion;

end Sem_Ch13;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ P A K D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.71 $                             --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Exp_Ch4;  use Exp_Ch4;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Itypes;   use Itypes;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;

package body Exp_Pakd is

   ---------------------------
   -- Endian Considerations --
   ---------------------------

   --  As described in the specification, bit numbering in a packed array
   --  is consistent with bit numbering in a record representation clause,
   --  and hence dependent on the endianness of the machine:

   --    For little-endian machines, element zero is at the right hand end
   --    (low order end) of a bit field.

   --    For big-endian machines, element zero is at the left hand end
   --    (high order end) of a bit field.

   --  The shifts that are used to right justify a field therefore differ
   --  in the two cases. For the little-endian case, we can simply use the
   --  bit number (i.e. the element number * element size) as the count for
   --  a right shift. For the big-endian case, we have to subtract the shift
   --  count from an appropriate constant to use in the right shift. We use
   --  rotates instead of shifts (which is necessary in the store case to
   --  preserve other fields), and we expect that the backend will be able
   --  to change the right rotate into a left rotate, avoiding the subtract,
   --  if the architecture provides such an instruction.

   ----------------------------------------------
   -- Entity Tables for Packed Access Routines --
   ----------------------------------------------

   --  For the cases of component size = 3,5-7,9-15,17-31, we call library
   --  routines, and this table is used to obtain the proper routine entity.

   type E_Array is array (Int range 01 .. 31) of RE_Id;

   --  Array of Bits_nn entities. RE_Bit_Not is used as a null entry, for
   --  the cases where a library routine is not used.

   Bits_Id : constant E_Array :=
     (01 => RE_Bits_1,
      02 => RE_Bits_2,
      03 => RE_Bits_03,
      04 => RE_Bits_4,
      05 => RE_Bits_05,
      06 => RE_Bits_06,
      07 => RE_Bits_07,
      08 => RE_Bit_Not,
      09 => RE_Bits_09,
      10 => RE_Bits_10,
      11 => RE_Bits_11,
      12 => RE_Bits_12,
      13 => RE_Bits_13,
      14 => RE_Bits_14,
      15 => RE_Bits_15,
      16 => RE_Bit_Not,
      17 => RE_Bits_17,
      18 => RE_Bits_18,
      19 => RE_Bits_19,
      20 => RE_Bits_20,
      21 => RE_Bits_21,
      22 => RE_Bits_22,
      23 => RE_Bits_23,
      24 => RE_Bits_24,
      25 => RE_Bits_25,
      26 => RE_Bits_26,
      27 => RE_Bits_27,
      28 => RE_Bits_28,
      29 => RE_Bits_29,
      30 => RE_Bits_30,
      31 => RE_Bits_31);

   --  Array of Get_nn entities. RE_Bit_Not is used as a null entry, for
   --  the cases where a library routine is not used.

   Get_Id : constant E_Array :=
     (01 => RE_Bit_Not,
      02 => RE_Bit_Not,
      03 => RE_Get_03,
      04 => RE_Bit_Not,
      05 => RE_Get_05,
      06 => RE_Get_06,
      07 => RE_Get_07,
      08 => RE_Bit_Not,
      09 => RE_Get_09,
      10 => RE_Get_10,
      11 => RE_Get_11,
      12 => RE_Get_12,
      13 => RE_Get_13,
      14 => RE_Get_14,
      15 => RE_Get_15,
      16 => RE_Bit_Not,
      17 => RE_Get_17,
      18 => RE_Get_18,
      19 => RE_Get_19,
      20 => RE_Get_20,
      21 => RE_Get_21,
      22 => RE_Get_22,
      23 => RE_Get_23,
      24 => RE_Get_24,
      25 => RE_Get_25,
      26 => RE_Get_26,
      27 => RE_Get_27,
      28 => RE_Get_28,
      29 => RE_Get_29,
      30 => RE_Get_30,
      31 => RE_Get_31);

   --  Array of Set_nn entities. RE_Bit_Not is used as a null entry, for
   --  the cases where a library routine is not used.

   Set_Id : E_Array :=
     (01 => RE_Bit_Not,
      02 => RE_Bit_Not,
      03 => RE_Set_03,
      04 => RE_Bit_Not,
      05 => RE_Set_05,
      06 => RE_Set_06,
      07 => RE_Set_07,
      08 => RE_Bit_Not,
      09 => RE_Set_09,
      10 => RE_Set_10,
      11 => RE_Set_11,
      12 => RE_Set_12,
      13 => RE_Set_13,
      14 => RE_Set_14,
      15 => RE_Set_15,
      16 => RE_Bit_Not,
      17 => RE_Set_17,
      18 => RE_Set_18,
      19 => RE_Set_19,
      20 => RE_Set_20,
      21 => RE_Set_21,
      22 => RE_Set_22,
      23 => RE_Set_23,
      24 => RE_Set_24,
      25 => RE_Set_25,
      26 => RE_Set_26,
      27 => RE_Set_27,
      28 => RE_Set_28,
      29 => RE_Set_29,
      30 => RE_Set_30,
      31 => RE_Set_31);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Compute_Linear_Subscript
     (Atyp   : Entity_Id;
      N      : Node_Id;
      Subscr : out Node_Id);
   --  Given a constrained array type Atyp, and an indexed component node
   --  N referencing an array object of this type, build an expression of
   --  type Standard.Integer representing the zero-based linear subscript
   --  value. This expression includes any required range checks.

   procedure Convert_To_PAT_Type (Aexp : Node_Id);
   --  Given an expression of a packed array type, builds a corresponding
   --  expression whose type is the implementation type used to represent
   --  the packed array. Aexp is analyzed and resolved on entry and on exit.

   function Make_Shift_Left (N : Node_Id; S : Node_Id) return Node_Id;
   --  Build a left shift node, checking for the case of a shift count of zero

   function Make_Shift_Right (N : Node_Id; S : Node_Id) return Node_Id;
   --  Build a right shift node, checking for the case of a shift count of zero

   procedure Setup_Enumeration_Packed_Array_Reference (N : Node_Id);
   --  This routine is called in the Get and Set case for arrays that are
   --  packed but not bit-packed, meaning that they have at least one
   --  subscript that is of an enumeration type with a non-standard
   --  representation. This routine modifies the given node to properly
   --  reference the corresponding packed array type.

   procedure Setup_Inline_Packed_Array_Reference
     (N      : Node_Id;
      Atyp   : Entity_Id;
      Obj    : in out Node_Id;
      Cmask  : out Uint;
      Shift  : out Node_Id);
   --  This procedure performs common processing on the N_Indexed_Component
   --  parameter given as N, whose prefix is a reference to a packed array.
   --  This is used for the get and set when the component size is 1,2,4
   --  or for other component sizes when the packed array type is a modular
   --  type (i.e. the cases that are handled with inline code).
   --
   --  On entry:
   --
   --    N is the N_Indexed_Component node for the packed array reference
   --
   --    Atyp is the constrained array type (the actual subtype has been
   --    computed if necessary to obtain the constraints, but this is still
   --    the original array type, not the Packed_Array_Type value).
   --
   --    Obj is the object which is to be indexed. It is always of type Atyp.
   --
   --  On return:
   --
   --    Obj is the object containing the desired bit field. It is of type
   --    Unsigned or Long_Long_Unsigned, and is either the entire value,
   --    for the small static case, or the proper selected byte from the
   --    array in the large or dynamic case. This node is analyzed and
   --    resolved on return.
   --
   --    Shift is a node representing the shift count to be used in the
   --    rotate right instruction that positions the field for access.
   --    This node is analyzed and resolved on return.
   --
   --    Cmask is a mask corresponding to the width of the component field.
   --    Its value is 2 ** Csize - 1 (e.g. 2#1111# for component size of 4).
   --
   --  Note: in some cases the call to this routine may generate actions
   --  (for handling multi-use references and the generation of the packed
   --  array type on the fly). Such actions are inserted into the tree
   --  directly using Insert_Action.

   ------------------------------
   -- Compute_Linear_Subcsript --
   ------------------------------

   procedure Compute_Linear_Subscript
     (Atyp   : Entity_Id;
      N      : Node_Id;
      Subscr : out Node_Id)
   is
      Loc    : constant Source_Ptr := Sloc (N);
      Oldsub : Node_Id;
      Newsub : Node_Id;
      Indx   : Node_Id;
      Styp   : Entity_Id;

   begin
      Subscr := Empty;

      --  Loop through dimensions

      Indx   := First_Index (Atyp);
      Oldsub := First (Expressions (N));

      while Present (Indx) loop
         Styp := Etype (Indx);
         Newsub := Relocate_Node (Oldsub);

         --  Get expression for the subscript value. First, if Do_Range_Check
         --  is set on a subscript, then we must do a range check against the
         --  original bounds (not the bounds of the packed array type). We do
         --  this by introducing a subtype conversion.

         if Do_Range_Check (Newsub)
           and then Etype (Newsub) /= Styp
         then
            Newsub := Convert_To (Styp, Newsub);
         end if;

         --  Now evolve the expression for the subscript. First convert
         --  the subscript to be zero based and of an integer type.

         --  If it is of an integer type now, we just subtract:

         --    Integer (subscript - Styp'First)

         if Is_Integer_Type (Styp) then
            Newsub :=
              Convert_To (Standard_Integer,
                Make_Op_Subtract (Loc,
                  Left_Opnd => Newsub,
                Right_Opnd =>
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Occurrence_Of (Styp, Loc),
                    Attribute_Name => Name_First)));

         --  For the enumeration case, we have to use 'Pos to get the value
         --  to work with before subtracting the lower bound.

         --    Integer (Styp'Pos (subscr)) - Integer (Styp'Pos (Styp'First));

         --  This is not quite right for bizarre cases where the size of the
         --  enumeration type is > Integer'Size bits due to rep clause ???

         else
            pragma Assert (Is_Enumeration_Type (Styp));

            Newsub :=
              Make_Op_Subtract (Loc,
                Left_Opnd => Convert_To (Standard_Integer,
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Occurrence_Of (Styp, Loc),
                    Attribute_Name => Name_Pos,
                    Expressions => New_List (Newsub))),

                Right_Opnd =>
                  Convert_To (Standard_Integer,
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Occurrence_Of (Styp, Loc),
                      Attribute_Name => Name_Pos,
                      Expressions => New_List (
                        Make_Attribute_Reference (Loc,
                        Prefix => New_Occurrence_Of (Styp, Loc),
                        Attribute_Name => Name_First)))));
         end if;

         Set_Paren_Count (Newsub, 1);

         --  For the first subscript, we just copy that subscript value

         if No (Subscr) then
            Subscr := Newsub;

         --  Otherwise, we must multiply what we already have by the current
         --  stride and then add in the new value to the evolving subscript.

         else
            Subscr :=
              Make_Op_Add (Loc,
                Left_Opnd =>
                  Make_Op_Multiply (Loc,
                    Left_Opnd  => Subscr,
                    Right_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_Range_Length,
                        Prefix         => New_Occurrence_Of (Styp, Loc))),
                Right_Opnd => Newsub);
         end if;

         --  Move to next subscript

         Indx   := Next_Index (Indx);
         Oldsub := Next (Oldsub);
      end loop;
   end Compute_Linear_Subscript;

   -------------------------
   -- Convert_To_PAT_Type --
   -------------------------

   --  The PAT is always obtained from the actual subtype

   procedure Convert_To_PAT_Type (Aexp : Entity_Id) is
      Act_ST : Entity_Id;

   begin
      Convert_To_Actual_Subtype (Aexp);
      Act_ST := Underlying_Type (Etype (Aexp));
      Create_Packed_Array_Type (Act_ST);

      --  Just replace the etype with the packed array type. This works
      --  because the expression will not be further analyzed, and Gigi
      --  considers the two types equivalent in any case.

      Set_Etype (Aexp, Packed_Array_Type (Act_ST));
   end Convert_To_PAT_Type;

   ------------------------------
   -- Create_Packed_Array_Type --
   ------------------------------

   procedure Create_Packed_Array_Type (Typ : Entity_Id) is
      Loc      : constant Source_Ptr := Sloc (Typ);
      Base     : constant Entity_Id  := Base_Type (Typ);
      Frst     : constant Entity_Id  := First_Subtype (Base);
      Ctyp     : constant Entity_Id  := Component_Type (Typ);
      Csize    : constant Uint       := Component_Size (Typ);

      PB_Type  : Entity_Id;
      Esiz     : Uint;
      Decl     : Node_Id;
      PAT      : Entity_Id;
      Len_Dim  : Node_Id;
      Len_Expr : Node_Id;
      Len_Bits : Uint;
      Bits_U1  : Node_Id;
      PAT_High : Node_Id;
      Btyp     : Entity_Id;
      Lit      : Node_Id;

      procedure Install_PAT;
      --  This procedure is called with Decl set to the declaration for the
      --  packed array type. It creates the type and installs it as required.

      procedure Set_PB_Type;
      --  Sets PB_Type to either Packed_Bytes or Packed_Bytes_Unaligned,
      --  according to the valeu of the component size.

      -----------------
      -- Install_PAT --
      -----------------

      procedure Install_PAT is
      begin
         --  We do not want to put the declaration we have created in the tree
         --  since it is often hard, and sometimes impossible to find a proper
         --  place for it (the impossible case arises for a packed array type
         --  with bounds depending on the discriminant, a declaration cannot
         --  be put inside the record, and the reference to the discriminant
         --  cannot be outside the record).

         --  The solution is to analyze the declaration while temporarily
         --  attached to the tree at an appropriate point, and then we install
         --  the resulting type as an Itype in the packed array type field of
         --  the original type, so that no explicit declaration is required.

         --  Note: the packed type is created in the scope of its parent type.
         --  There are at least some cases where the current scope is deeper,
         --  and so we temporarily reset the scope for the definition. This
         --  is clearly safe, since the first use of the packed array type
         --  will be the implicit reference from the corresponding unpacked
         --  type when it is elaborated.

         if Is_Itype (Typ) then
            Set_Parent (Decl, Associated_Node_For_Itype (Typ));
         else
            Set_Parent (Decl, Declaration_Node (Typ));
         end if;

         New_Scope (Scope (Typ));
         Set_Is_Itype (PAT, True);
         Analyze (Decl, Suppress => All_Checks);
         Pop_Scope;
         Set_Esize (PAT, Esiz);
         Set_Parent (PAT, Empty);
         Set_Packed_Array_Type (Typ, PAT);
         Set_Is_Packed_Array_Type (PAT, True);
         Set_Associated_Node_For_Itype (PAT, Typ);

         --  We definitely do not want to delay freezing for packed array
         --  types. This is of particular importance for the itypes that
         --  are generated for record components depending on discriminants
         --  where there is no place to put the freeze node.

         Set_Has_Delayed_Freeze (PAT, False);
         Set_Has_Delayed_Freeze (Etype (PAT), False);
      end Install_PAT;

      -----------------
      -- Set_PB_Type --
      -----------------

      procedure Set_PB_Type is
      begin
         if Csize <= 2 or else Csize = 4 then
            PB_Type := RTE (RE_Packed_Bytes_Unaligned);
         else
            PB_Type := RTE (RE_Packed_Bytes);
         end if;
      end Set_PB_Type;

   --  Start of processing for Create_Packed_Array_Type

   begin
      --  If we already have a packed array type, nothing to do

      if Present (Packed_Array_Type (Typ)) then
         return;
      end if;

      --  We preset the result type size from the size of the original array
      --  type, since this size clearly belongs to the packed array type. The
      --  size of the conceptual unpacked type is always set to unknown.

      Esiz := Esize (Typ);

      --  For the case where the first subtype is constrained, and we have
      --  already built a modular type when the bounds were appropriate
      --  (static and small enough). The code for this is below. It is
      --  unusual but possible to have subtypes of such a type (happens
      --  for example in the case of aggregates) and in this case we
      --  simply use the same representation as for the first subtype.

      if Present (Packed_Array_Type (Frst))
        and then Is_Integer_Type (Packed_Array_Type (Frst))
      then
         Set_Packed_Array_Type (Typ, Packed_Array_Type (Frst));
         Set_Esize             (Typ, Esize (Packed_Array_Type (Frst)));
         return;

      --  Case of an array where at least one index is of an enumeration
      --  type with a non-standard representation, but the component size
      --  is not appropriate for bit packing. This is the case where we
      --  have Is_Packed set (we would never be in this unit otherwise),
      --  but Is_Bit_Packed_Array is false.

      --  Note that if the component size is appropriate for bit packing,
      --  then the circuit for the computation of the subscript properly
      --  deals with the non-standard enumeration type case by taking the
      --  Pos anyway.

      elsif not Is_Bit_Packed_Array (Typ) then

         --  Here we build a declaration:

         --    type tttP is array (index1, index2, ...) of component_type

         --  where index1, index2, are the index types. These are the same
         --  as the index types of the original array, except for the non-
         --  standard representation enumeration type case, where we have
         --  two subcases. For the unconstrained array case, we use

         --    Natural range <>

         --  For the constrained case, we use

         --    Natural range Enum_Type'Pos (Enum_Type'First) ..
         --                  Enum_Type'Pos (Enum_Type'Last);

         PAT :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (Typ), 'P'));

         Set_Packed_Array_Type (Typ, PAT);

         declare
            Indexes   : List_Id := New_List;
            Indx      : Node_Id;
            Indx_Typ  : Entity_Id;
            Enum_Case : Boolean;

         begin
            Indx := First_Index (Typ);

            while Present (Indx) loop
               Indx_Typ := Etype (Indx);

               Enum_Case := Is_Enumeration_Type (Indx_Typ)
                              and then Has_Non_Standard_Rep (Indx_Typ);

               if not Is_Constrained (Typ) then
                  if Enum_Case then
                     Indx_Typ := Standard_Natural;
                  end if;

                  Append_To (Indexes, New_Occurrence_Of (Indx_Typ, Loc));

               else -- Is_Constrained (Typ)

                  if not Enum_Case then
                     Append_To (Indexes, New_Occurrence_Of (Indx_Typ, Loc));

                  else
                     Append_To (Indexes,
                       Make_Subtype_Indication (Loc,
                         Subtype_Mark =>
                           New_Occurrence_Of (Standard_Natural, Loc),
                         Constraint =>
                           Make_Range_Constraint (Loc,
                             Range_Expression =>
                               Make_Range (Loc,
                                 Low_Bound =>
                                   Make_Attribute_Reference (Loc,
                                     Prefix =>
                                       New_Occurrence_Of (Indx_Typ, Loc),
                                     Attribute_Name => Name_Pos,
                                     Expressions => New_List (
                                       Make_Attribute_Reference (Loc,
                                         Prefix =>
                                           New_Occurrence_Of (Indx_Typ, Loc),
                                         Attribute_Name => Name_First))),

                                 High_Bound =>
                                   Make_Attribute_Reference (Loc,
                                     Prefix =>
                                       New_Occurrence_Of (Indx_Typ, Loc),
                                     Attribute_Name => Name_Pos,
                                     Expressions => New_List (
                                       Make_Attribute_Reference (Loc,
                                         Prefix =>
                                           New_Occurrence_Of (Indx_Typ, Loc),
                                         Attribute_Name => Name_Last)))))));

                  end if;
               end if;

               Indx := Next_Index (Indx);
            end loop;

            if Is_Constrained (Typ) then
               Decl :=
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => PAT,
                   Type_Definition =>
                     Make_Constrained_Array_Definition (Loc,
                       Discrete_Subtype_Definitions => Indexes,
                       Subtype_Indication =>
                         New_Occurrence_Of (Ctyp, Loc)));
            else
               Decl :=
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => PAT,
                   Type_Definition =>
                     Make_Unconstrained_Array_Definition (Loc,
                       Subtype_Marks => Indexes,
                       Subtype_Indication =>
                         New_Occurrence_Of (Ctyp, Loc)));

            end if;
         end;

         Install_PAT;
         return;

      --  Case of bit-packing required for unconstrained array. We simply
      --  use Packed_Bytes or Packed_Bytes_Unaligned as appropriate, and we
      --  do not need to construct a special packed array type.

      elsif not Is_Constrained (Typ) then

         Set_PB_Type;
         Set_Packed_Array_Type (Typ, PB_Type);
         Set_Is_Packed_Array_Type (Packed_Array_Type (Typ), True);
         return;
      end if;

      --  Remaining code is for the case of bit-packing for constrained array

      --  The name of the packed array subtype is

      --    tttPx

      --  where x is the component size in bits and ttt is the name of
      --  the parent packed type.

      PAT :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Chars (Typ), 'P', UI_To_Int (Csize)));

      Set_Packed_Array_Type (Typ, PAT);

      --  Build an expression for the length of the array in bits.
      --  This is the product of the length of each of the dimensions

      for J in 1 .. Number_Dimensions (Typ) loop
         Len_Dim :=
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Length,
             Prefix         => New_Occurrence_Of (Typ, Loc),
             Expressions    => New_List (
               Make_Integer_Literal (Loc, UI_From_Int (J))));

         if J = 1 then
            Len_Expr := Len_Dim;

         else
            Len_Expr :=
              Make_Op_Multiply (Loc,
                Left_Opnd  => Len_Expr,
                Right_Opnd => Len_Dim);
         end if;
      end loop;

      --  Temporarily attach the length expression to the tree and analyze
      --  and resolve it, so that we can test its value. We assume that the
      --  total length fits in type Integer.

      Set_Parent (Len_Expr, Typ);
      Analyze_And_Resolve (Len_Expr, Standard_Integer);

      --  Use a modular type if possible. We can do this if we are we
      --  have static bounds, and the length is small enough.

      if Compile_Time_Known_Value (Len_Expr) then
         Len_Bits := UI_Max (Uint_1, (Expr_Value (Len_Expr) * Csize));

         --  We use the modular type always if the size does not exceed
         --  the maximum length of a binary modular type.

         if Len_Bits <= System_Max_Binary_Modulus_Power then

            --  We can use the modular type, it has the form:

            --    subtype tttPn is btyp
            --      range 0 .. 2 ** (Esize (Typ) * Csize) - 1;

            --  Here Siz is 1, 2 or 4, as computed above, and btyp is either
            --  Unsigned or Long_Long_Unsigned depending on the length.

            if Len_Bits <= Standard_Integer_Size then
               Btyp := RTE (RE_Unsigned);
            else
               Btyp := RTE (RE_Long_Long_Unsigned);
            end if;

            Lit := Make_Integer_Literal (Loc, Intval => 2 ** Len_Bits - 1);
            Set_Print_In_Hex (Lit);

            Decl :=
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => PAT,
                  Subtype_Indication =>
                    Make_Subtype_Indication (Loc,
                      Subtype_Mark => New_Occurrence_Of (Btyp, Loc),

                      Constraint =>
                        Make_Range_Constraint (Loc,
                          Range_Expression =>
                            Make_Range (Loc,
                              Low_Bound =>
                                Make_Integer_Literal (Loc, Intval => Uint_0),
                              High_Bound => Lit))));

            if Esiz = Uint_0 then
               Esiz := Len_Bits;
            end if;

            Install_PAT;
            return;
         end if;
      end if;

      --  Could not use a modular type, for all other cases, we build
      --  a packed array subtype:

      --    subtype tttPn is
      --      System.Packed_Bytes[_Unaligned] (0 .. (Bits + 7) / 8 - 1);

      --  Bits is the length of the array in bits.

      Set_PB_Type;

      Bits_U1 :=
        Make_Op_Add (Loc,
          Left_Opnd =>
            Make_Op_Multiply (Loc,
              Left_Opnd  =>
                Make_Integer_Literal (Loc, Csize),
              Right_Opnd => Len_Expr),

          Right_Opnd =>
            Make_Integer_Literal (Loc, Uint_7));

      Set_Paren_Count (Bits_U1, 1);

      PAT_High :=
        Make_Op_Subtract (Loc,
          Left_Opnd =>
            Make_Op_Divide (Loc,
              Left_Opnd => Bits_U1,
              Right_Opnd => Make_Integer_Literal (Loc, Uint_8)),
          Right_Opnd => Make_Integer_Literal (Loc, Uint_1));

      Decl :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => PAT,
            Subtype_Indication =>
              Make_Subtype_Indication (Loc,
                Subtype_Mark => New_Occurrence_Of (PB_Type, Loc),
                Constraint =>

                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List (
                      Make_Range (Loc,
                        Low_Bound =>
                          Make_Integer_Literal (Loc,
                            Intval => Uint_0),
                        High_Bound => PAT_High)))));

      Install_PAT;

   end Create_Packed_Array_Type;

   -----------------------------------
   -- Expand_Bit_Packed_Element_Set --
   -----------------------------------

   procedure Expand_Bit_Packed_Element_Set (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Lhs : constant Node_Id    := Name (N);

      Rhs : Node_Id := Expression (N);
      --  Initially Rhs is the right hand side value, it will be replaced
      --  later by an appropriate unchecked conversion for the assignment.

      Obj    : Node_Id;
      Atyp   : Entity_Id;
      PAT    : Entity_Id;
      Ctyp   : Entity_Id;
      Csiz   : Int;
      Shift  : Node_Id;
      Cmask  : Uint;

      New_Lhs : Node_Id;
      New_Rhs : Node_Id;

      Rhs_Val_Known : Boolean;
      Rhs_Val       : Uint;
      --  If the value of the right hand side as an integer constant is
      --  known at compile time, Rhs_Val_Known is set True, and Rhs_Val
      --  contains the value. Otherwise Rhs_Val_Known is set False, and
      --  the Rhs_Val is undefined.

   begin
      pragma Assert (Is_Bit_Packed_Array (Etype (Prefix (Lhs))));

      Obj := Relocate_Node (Prefix (Lhs));
      Convert_To_Actual_Subtype (Obj);
      Atyp := Etype (Obj);
      PAT  := Packed_Array_Type (Atyp);
      Ctyp := Component_Type (Atyp);
      Csiz := UI_To_Int (Component_Size (Atyp));

      --  Case of component size 1,2,4 or any component size for the modular
      --  case. These are the cases for which we can inline the code.

      if Csiz = 1 or else Csiz = 2 or else Csiz = 4
        or else (Present (PAT) and then Is_Modular_Integer_Type (PAT))
      then
         Setup_Inline_Packed_Array_Reference (Lhs, Atyp, Obj, Cmask, Shift);

         --  The statement to be generated is:

         --    Obj := Obj and Mask1 or (shift_left (rhs, shift))

         --      where mask1 is obtained by shifting Cmask left Shift bits
         --      and then complementing the result.

         --      the "and Mask1" is omitted if rhs is constant and all 1 bits

         --      the "or ..." is omitted if rhs is constant and all 0 bits

         --      rhs is converted to the appropriate type.

         --  Determine if right side is all 0 bits or all 1 bits

         Rhs_Val_Known := False;

         if Compile_Time_Known_Value (Rhs) then
            Rhs_Val := Expr_Rep_Value (Rhs);
            Rhs_Val_Known := True;

         --  The following test catches the case of an unchecked conversion
         --  of an integer literal. This results from optimizing aggregates
         --  of packed types.

         elsif Nkind (Rhs) = N_Unchecked_Type_Conversion
           and then Compile_Time_Known_Value (Expression (Rhs))
         then
            Rhs_Val := Expr_Rep_Value (Expression (Rhs));
            Rhs_Val_Known := True;
         end if;

         --  Some special checks for the case where the right hand value
         --  is known at compile time. Basically we have to take care of
         --  the implicit conversion to the subtype of the component object.

         if Rhs_Val_Known then

            --  If we have a biased component type then we must manually do
            --  the biasing, since we are taking responsibility in this case
            --  for constructing the exact bit pattern to be used.

            if Has_Biased_Representation (Ctyp) then
               Rhs_Val := Rhs_Val - Expr_Rep_Value (Type_Low_Bound (Ctyp));
            end if;

            --  For a negative value, we manually convert the twos complement
            --  value to a corresponding unsigned value, so that the proper
            --  field width is maintained. If we did not do this, we would
            --  get too many leading sign bits later on.

            if Rhs_Val < 0 then
               Rhs_Val := 2 ** UI_From_Int (Csiz) + Rhs_Val;
            end if;
         end if;

         New_Lhs := Duplicate_Subexpr (Obj, True);
         New_Rhs := Duplicate_Subexpr (Obj);

         --  First we deal with the "and"

         if not Rhs_Val_Known or else Rhs_Val /= Cmask then
            declare
               Mask1 : Node_Id;
               Sval  : Uint;
               Lit   : Node_Id;

            begin
               if Compile_Time_Known_Value (Shift) then
                  Mask1 :=
                    Make_Integer_Literal (Loc,
                      Modulus (Etype (Obj)) - 1 -
                                 (Cmask * (2 ** Expr_Value (Shift))));
                  Set_Print_In_Hex (Mask1);

               else
                  Lit := Make_Integer_Literal (Loc, Cmask);
                  Set_Print_In_Hex (Lit);
                  Mask1 :=
                    Make_Op_Not (Loc,
                      Right_Opnd => Make_Shift_Left (Lit, Shift));
               end if;

               New_Rhs :=
                 Make_Op_And (Loc,
                   Left_Opnd  => New_Rhs,
                   Right_Opnd => Mask1);
            end;
         end if;

         --  Then deal with the "or"

         if not Rhs_Val_Known or else Rhs_Val /= 0 then
            declare
               Or_Rhs : Node_Id;

               procedure Bias_Rhs;
               --  Adjust Rhs by bias if biased representation for components

               procedure Bias_Rhs is
                  Etyp : Entity_Id;

               begin
                  if Has_Biased_Representation (Ctyp) then

                     --  Do the biasing by simply converting to the biased
                     --  subtype, the conversion will generate the bias.

                     Etyp := Etype (Rhs);
                     Rhs := Convert_To (Ctyp, Rhs);

                     --  Set Etype, since it can be referenced before the
                     --  node is completely analyzed.

                     Set_Etype (Rhs, Etyp);
                  end if;
               end Bias_Rhs;

            begin
               if Rhs_Val_Known
                 and then Compile_Time_Known_Value (Shift)
               then
                  Or_Rhs :=
                    Make_Integer_Literal (Loc,
                      Rhs_Val * (2 ** Expr_Value (Shift)));
                  Set_Print_In_Hex (Or_Rhs);

               else
                  --  We have to convert the right hand side to Etype (Obj).
                  --  A special case case arises if what we have now is a Val
                  --  attribute reference whose expression type is Etype (Obj).
                  --  This happens for assignments of fields from the same
                  --  array. In this case we get the required right hand side
                  --  by simply removing the inner attribute reference.

                  if Nkind (Rhs) = N_Attribute_Reference
                    and then Attribute_Name (Rhs) = Name_Val
                    and then Etype (First (Expressions (Rhs))) = Etype (Obj)
                  then
                     Rhs := Relocate_Node (First (Expressions (Rhs)));
                     Bias_Rhs;

                  --  If the value of the right hand side is a known integer
                  --  value, then just replace it by an untyped constant,
                  --  which will be properly retyped when we analyze and
                  --  resolve the expression.

                  elsif Rhs_Val_Known then

                     --  Note that Rhs_Val has already been normalized to
                     --  be an unsigned value with the proper number of bits.

                     Rhs :=
                       Make_Integer_Literal (Loc,
                         Intval => Rhs_Val);

                  --  Otherwise we need an unchecked conversion

                  else
                     Bias_Rhs;

                     --  If we don't have a discrete type then first convert
                     --  to Bits_1, Bits_2 or Bits_4. The reason for this is
                     --  that in the big-endian case, a direct unchecked
                     --  conversion to Etype (Obj) would incorrectly position
                     --  the bits (i.e. left justify them).

                     if not Is_Discrete_Type (Etype (Rhs)) then
                        Rhs :=
                          Unchecked_Convert_To (RTE (Bits_Id (Csiz)), Rhs);
                     end if;

                     --  Now we can do the final unchecked convert

                     Rhs := Unchecked_Convert_To (Etype (Obj), Rhs);
                  end if;

                  Or_Rhs := Make_Shift_Left (Rhs, Shift);
               end if;

               if Nkind (New_Rhs) = N_Op_And then
                  Set_Paren_Count (New_Rhs, 1);
               end if;

               New_Rhs :=
                 Make_Op_Or (Loc,
                   Left_Opnd  => New_Rhs,
                   Right_Opnd => Or_Rhs);
            end;
         end if;

         --  Now do the rewrite

         Rewrite_Substitute_Tree (N,
           Make_Assignment_Statement (Loc,
             Name       => New_Lhs,
             Expression => New_Rhs));

      --  All other component sizes for non-modular case

      else
         --  We generate

         --    Set_nn (Arr'address, Subscr, Bits_nn!(Rhs))

         --  where Subscr is the computed linear subscript.

         declare
            Set_nn  : constant Entity_Id := RTE (Set_Id (Csiz));
            Bits_nn : constant Entity_Id := RTE (Bits_Id (Csiz));
            Subscr  : Node_Id;
            Atyp    : Entity_Id;

         begin
            pragma Assert (3 <= Csiz and then Csiz <= 31
                             and then Csiz /= 8
                             and then Csiz /= 16);

            Obj := Relocate_Node (Prefix (Lhs));
            Convert_To_Actual_Subtype (Obj);
            Atyp := Etype (Obj);
            Compute_Linear_Subscript (Atyp, Lhs, Subscr);

            Rewrite_Substitute_Tree (N,
              Make_Procedure_Call_Statement (Loc,
                  Name => New_Occurrence_Of (Set_nn, Loc),
                  Parameter_Associations => New_List (
                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_Address,
                      Prefix => Obj),
                    Subscr,
                    Unchecked_Convert_To (Bits_nn,
                      Convert_To (Ctyp, Rhs)))));

         end;
      end if;

      Analyze (N, Suppress => All_Checks);

   end Expand_Bit_Packed_Element_Set;

   ------------------------------------
   -- Expand_Packed_Boolean_Operator --
   ------------------------------------

   --  This routine expands "a op b" for the packed cases

   procedure Expand_Packed_Boolean_Operator (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      L   : constant Node_Id    := Relocate_Node (Left_Opnd  (N));
      R   : constant Node_Id    := Relocate_Node (Right_Opnd (N));

      Ltyp : Entity_Id;
      Rtyp : Entity_Id;
      PAT  : Entity_Id;

   begin
      Convert_To_Actual_Subtype (L);
      Convert_To_Actual_Subtype (R);

      Ltyp := Etype (L);
      Rtyp := Etype (R);

      Convert_To_PAT_Type (L);
      Convert_To_PAT_Type (R);

      PAT := Etype (L);

      --  For the modular case, we expand a op b into

      --    rtyp!(pat!(a) op pat!(b))

      --  where rtyp is the Etype of the left operand. Note that we do not
      --  convert to the base type, since this would be unconstrained, and
      --  hence not have a corresponding packed array type set.

      if Is_Modular_Integer_Type (PAT) then
         declare
            P : Node_Id;

         begin
            if Nkind (N) = N_Op_And then
               P := Make_Op_And (Loc, L, R);

            elsif Nkind (N) = N_Op_Or then
               P := Make_Op_Or  (Loc, L, R);

            else -- Nkind (N) = N_Op_Xor
               P := Make_Op_Xor (Loc, L, R);
            end if;

            Rewrite_Substitute_Tree (N, Unchecked_Convert_To (Rtyp, P));
         end;

      --  For the array case, we insert the actions

      --    Result : Ltype;

      --    System.Bitops.Bit_And/Or/Xor
      --     (Left'Address,
      --      Ltype'Length * Ltype'Component_Size;
      --      Right'Address,
      --      Rtype'Length * Rtype'Component_Size
      --      Result'Address);

      --  where Left and Right are the Packed_Bytes[_Unaligned] operands and
      --  the second argument and fourth arguments are the lengths of the
      --  operands in bits. Then we replace the expression by a reference
      --  to Result.

      else
         declare
            Result_Ent : constant Entity_Id :=
                           Make_Defining_Identifier (Loc,
                             Chars => New_Internal_Name ('T'));

            E_Id : RE_Id;

         begin
            if Nkind (N) = N_Op_And then
               E_Id := RE_Bit_And;

            elsif Nkind (N) = N_Op_Or then
               E_Id := RE_Bit_Or;

            else -- Nkind (N) = N_Op_Xor
               E_Id := RE_Bit_Xor;
            end if;

            Insert_Actions (N, New_List (

              Make_Object_Declaration (Loc,
                Defining_Identifier => Result_Ent,
                Object_Definition => New_Occurrence_Of (Ltyp, Loc)),

              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (RTE (E_Id), Loc),
                  Parameter_Associations => New_List (

                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_Address,
                      Prefix => L),

                    Make_Op_Multiply (Loc,
                      Left_Opnd =>
                        Make_Attribute_Reference (Loc,
                          Prefix =>
                            New_Occurrence_Of
                              (Etype (First_Index (Ltyp)), Loc),
                          Attribute_Name => Name_Range_Length),
                      Right_Opnd =>
                        Make_Integer_Literal (Loc, Component_Size (Ltyp))),

                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_Address,
                      Prefix => R),

                    Make_Op_Multiply (Loc,
                      Left_Opnd =>
                        Make_Attribute_Reference (Loc,
                          Prefix =>
                            New_Occurrence_Of
                              (Etype (First_Index (Rtyp)), Loc),
                          Attribute_Name => Name_Range_Length),
                      Right_Opnd =>
                        Make_Integer_Literal (Loc, Component_Size (Rtyp))),

                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_Address,
                      Prefix => New_Occurrence_Of (Result_Ent, Loc))))));

            Rewrite_Substitute_Tree (N,
              New_Occurrence_Of (Result_Ent, Loc));
         end;
      end if;

      Analyze_And_Resolve (N, Typ, Suppress => All_Checks);
   end Expand_Packed_Boolean_Operator;

   -------------------------------------
   -- Expand_Packed_Element_Reference --
   -------------------------------------

   procedure Expand_Packed_Element_Reference (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Obj   : Node_Id;
      Atyp  : Entity_Id;
      PAT   : Entity_Id;
      Ctyp  : Entity_Id;
      Csiz  : Int;
      Shift : Node_Id;
      Cmask : Uint;
      Lit   : Node_Id;

   begin
      --  If not bit packed, we have the enumeration case, which is easily
      --  dealt with (just adjust the subscripts of the indexed component)

      --  Note: this leaves the result as an indexed component, which is
      --  still a variable, so can be used in the assignment case, as is
      --  required in the enumeration case.

      if not Is_Bit_Packed_Array (Etype (Prefix (N))) then
         Setup_Enumeration_Packed_Array_Reference (N);
         return;
      end if;

      --  Remaining processing is for the bit-packed case.

      Obj := Relocate_Node (Prefix (N));
      Convert_To_Actual_Subtype (Obj);
      Atyp := Etype (Obj);
      PAT  := Packed_Array_Type (Atyp);
      Ctyp := Component_Type (Atyp);
      Csiz := UI_To_Int (Component_Size (Atyp));

      --  Case of component size 1,2,4 or any component size for the modular
      --  case. These are the cases for which we can inline the code.

      if Csiz = 1 or else Csiz = 2 or else Csiz = 4
        or else (Present (PAT) and then Is_Modular_Integer_Type (PAT))
      then
         Setup_Inline_Packed_Array_Reference (N, Atyp, Obj, Cmask, Shift);
         Lit := Make_Integer_Literal (Loc, Cmask);
         Set_Print_In_Hex (Lit);

         --  We generate a shift right to position the field, followed by a
         --  masking operation to extract the bit field, and we finally do an
         --  unchecked conversion to convert the result to the required target.

         --  Note that the unchecked conversion automatically deals with the
         --  bias if we are dealing with a biased representation. What will
         --  happen is that we temporarily generate the biased representation,
         --  but almost immediately that will be converted to the original
         --  unbiased component type, and the bias will disappear.

         Rewrite_Substitute_Tree (N,
           Unchecked_Convert_To (Ctyp,
             Make_Op_And (Loc,
               Left_Opnd  => Make_Shift_Right (Obj, Shift),
               Right_Opnd => Lit)));

      --  All other component sizes for non-modular case

      else
         --  We generate

         --    Component_Type!(Get_nn (Arr'address, Subscr))

         --  where Subscr is the computed linear subscript.

         declare
            Get_nn : constant Entity_Id := RTE (Get_Id (Csiz));
            Subscr : Node_Id;

         begin
            pragma Assert (3 <= Csiz and then Csiz <= 31
                             and then Csiz /= 4
                             and then Csiz /= 8
                             and then Csiz /= 16);

            Compute_Linear_Subscript (Atyp, N, Subscr);

            Rewrite_Substitute_Tree (N,
              Unchecked_Convert_To (Ctyp,
                Make_Function_Call (Loc,
                  Name => New_Occurrence_Of (Get_nn, Loc),
                  Parameter_Associations => New_List (
                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_Address,
                      Prefix => Obj),
                    Subscr))));
         end;
      end if;

      Analyze_And_Resolve (N, Ctyp, Suppress => All_Checks);

   end Expand_Packed_Element_Reference;

   ----------------------
   -- Expand_Packed_Eq --
   ----------------------

   --  Handles expansion of "=" on packed array types

   procedure Expand_Packed_Eq (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      L   : constant Node_Id    := Relocate_Node (Left_Opnd  (N));
      R   : constant Node_Id    := Relocate_Node (Right_Opnd (N));

      LLexpr : Node_Id;
      RLexpr : Node_Id;

      Ltyp : Entity_Id;
      Rtyp : Entity_Id;
      PAT  : Entity_Id;

   begin
      Convert_To_Actual_Subtype (L);
      Convert_To_Actual_Subtype (R);
      Ltyp := Underlying_Type (Etype (L));
      Rtyp := Underlying_Type (Etype (R));

      Convert_To_PAT_Type (L);
      Convert_To_PAT_Type (R);
      PAT := Etype (L);

      LLexpr :=
        Make_Op_Multiply (Loc,
          Left_Opnd =>
            Make_Attribute_Reference (Loc,
              Attribute_Name => Name_Length,
              Prefix => New_Occurrence_Of (Ltyp, Loc)),
          Right_Opnd =>
            Make_Integer_Literal (Loc, Component_Size (Ltyp)));

      RLexpr :=
        Make_Op_Multiply (Loc,
          Left_Opnd =>
            Make_Attribute_Reference (Loc,
              Attribute_Name => Name_Length,
              Prefix => New_Occurrence_Of (Rtyp, Loc)),
          Right_Opnd =>
            Make_Integer_Literal (Loc, Component_Size (Rtyp)));

      --  For the modular case, we transform the comparison to:

      --    Ltyp'Length = Rtyp'Length and then PAT!(L) = PAT!(R)

      --  where PAT is the packed array type. This works fine, since in the
      --  modular case we guarantee that the unused bits are always zeroes.
      --  We do have to compare the lengths because we could be comparing
      --  two different subtypes of the same base type.

      if Is_Modular_Integer_Type (PAT) then
         Rewrite_Substitute_Tree (N,
           Make_And_Then (Loc,
             Left_Opnd =>
               Make_Op_Eq (Loc,
                 Left_Opnd  => LLexpr,
                 Right_Opnd => RLexpr),

             Right_Opnd =>
               Make_Op_Eq (Loc,
                 Left_Opnd => L,
                 Right_Opnd => R)));

      --  For the non-modular case, we call a runtime routine

      --    System.Bit_Ops.Bit_Eq
      --      (L'Address, L_Length, R'Address, R_Length)

      --  where PAT is the packed array type, and the lengths are the lengths
      --  in bits of the original packed arrays. This routine takes care of
      --  not comparing the unused bits in the last byte.

      else
         Rewrite_Substitute_Tree (N,
           Make_Function_Call (Loc,
             Name => New_Occurrence_Of (RTE (RE_Bit_Eq), Loc),
             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Address,
                 Prefix => L),

               LLexpr,

               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Address,
                 Prefix => R),

               RLexpr)));
      end if;

      Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
   end Expand_Packed_Eq;

   -----------------------
   -- Expand_Packed_Not --
   -----------------------

   --  Handles expansion of "not" on packed array types

   procedure Expand_Packed_Not (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (N);
      Opnd : constant Node_Id    := Relocate_Node (Right_Opnd (N));

      Rtyp : Entity_Id;
      PAT  : Entity_Id;
      Lit  : Node_Id;

   begin
      Convert_To_Actual_Subtype (Opnd);
      Rtyp := Etype (Opnd);
      Convert_To_PAT_Type (Opnd);
      PAT := Etype (Opnd);

      --  For the case where the packed array type is a modular type,
      --  not A expands simply into:

      --     rtyp!(PAT!(A) xor mask)

      --  where PAT is the packed array type, and mask is a mask of all
      --  one bits of length equal to the size of this packed type and
      --  rtyp is the actual subtype of the operand

      Lit := Make_Integer_Literal (Loc, 2 ** Esize (PAT) - 1);
      Set_Print_In_Hex (Lit);

      if not Is_Array_Type (PAT) then
         Rewrite_Substitute_Tree (N,
           Unchecked_Convert_To (Rtyp,
             Make_Op_Xor (Loc,
               Left_Opnd  => Opnd,
               Right_Opnd => Lit)));

      --  For the array case, we insert the actions

      --    Result : Typ;

      --    System.Bitops.Bit_Not
      --     (Opnd'Address,
      --      Typ'Length * Typ'Component_Size;
      --      Result'Address);

      --  where Opnd is the Packed_Bytes[_Unaligned] operand and the second
      --  argument is the length of the operand in bits. Then we replace the
      --  expression by a reference to Result.

      else
         declare
            Result_Ent : constant Entity_Id :=
                           Make_Defining_Identifier (Loc,
                             Chars => New_Internal_Name ('T'));

         begin
            Insert_Actions (N, New_List (

              Make_Object_Declaration (Loc,
                Defining_Identifier => Result_Ent,
                Object_Definition => New_Occurrence_Of (Rtyp, Loc)),

              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (RTE (RE_Bit_Not), Loc),
                  Parameter_Associations => New_List (

                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_Address,
                      Prefix => Opnd),

                    Make_Op_Multiply (Loc,
                      Left_Opnd =>
                        Make_Attribute_Reference (Loc,
                          Prefix =>
                            New_Occurrence_Of
                              (Etype (First_Index (Rtyp)), Loc),
                          Attribute_Name => Name_Range_Length),
                      Right_Opnd =>
                        Make_Integer_Literal (Loc, Component_Size (Rtyp))),

                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_Address,
                      Prefix => New_Occurrence_Of (Result_Ent, Loc))))));

            Rewrite_Substitute_Tree (N,
              New_Occurrence_Of (Result_Ent, Loc));
         end;
      end if;

      Analyze_And_Resolve (N, Typ, Suppress => All_Checks);

   end Expand_Packed_Not;

   ---------------------
   -- Make_Shift_Left --
   ---------------------

   function Make_Shift_Left (N : Node_Id; S : Node_Id) return Node_Id is
      Nod : Node_Id;

   begin
      if Compile_Time_Known_Value (S) and then Expr_Value (S) = 0 then
         return N;
      else
         Nod :=
           Make_Op_Shift_Left (Sloc (N),
             Left_Opnd  => N,
             Right_Opnd => S);
         Set_Shift_Count_OK (Nod, True);
         return Nod;
      end if;
   end Make_Shift_Left;

   ----------------------
   -- Make_Shift_Right --
   ----------------------

   function Make_Shift_Right (N : Node_Id; S : Node_Id) return Node_Id is
      Nod : Node_Id;

   begin
      if Compile_Time_Known_Value (S) and then Expr_Value (S) = 0 then
         return N;
      else
         Nod :=
           Make_Op_Shift_Right (Sloc (N),
             Left_Opnd  => N,
             Right_Opnd => S);
         Set_Shift_Count_OK (Nod, True);
         return Nod;
      end if;
   end Make_Shift_Right;

   ----------------------------------------------
   -- Setup_Enumeration_Packed_Array_Reference --
   ----------------------------------------------

   --  All we have to do here is to find the subscripts that correspond
   --  to the index positions that have non-standard enumeration types
   --  and insert a Pos attribute to get the proper subscript value.
   --  Finally the prefix must be uncheck converted to the corresponding
   --  packed array type.

   --  Note that the component type is unchanged, so we do not need to
   --  fiddle with the types (Gigi always automatically takes the packed
   --  array type if it is set, as it will be in this case).

   procedure Setup_Enumeration_Packed_Array_Reference (N : Node_Id) is
      Pfx   : constant Node_Id   := Prefix (N);
      PAT   : constant Entity_Id := Packed_Array_Type (Etype (Prefix (N)));
      Typ   : constant Entity_Id := Etype (N);
      Exprs : constant List_Id   := Expressions (N);
      Expr  : Node_Id;

   begin
      pragma Assert (Present (PAT));

      Expr := First (Exprs);
      while Present (Expr) loop
         declare
            Loc      : constant Source_Ptr := Sloc (Expr);
            Expr_Typ : constant Entity_Id := Etype (Expr);

         begin
            if Is_Enumeration_Type (Expr_Typ)
              and then Has_Non_Standard_Rep (Expr_Typ)
            then
               Rewrite_Substitute_Tree (Expr,
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Expr_Typ, Loc),
                   Attribute_Name => Name_Pos,
                   Expressions    => New_List (Relocate_Node (Expr))));

               Analyze_And_Resolve (Expr, Standard_Natural);
            end if;
         end;

         Expr := Next (Expr);
      end loop;

      Rewrite_Substitute_Tree (N,
        Make_Indexed_Component (Sloc (N),
          Prefix      => Unchecked_Convert_To (PAT, Pfx),
          Expressions => Exprs));

      Analyze_And_Resolve (N, Typ);

   end Setup_Enumeration_Packed_Array_Reference;

   -----------------------------------------
   -- Setup_Inline_Packed_Array_Reference --
   -----------------------------------------

   procedure Setup_Inline_Packed_Array_Reference
     (N      : Node_Id;
      Atyp   : Entity_Id;
      Obj    : in out Node_Id;
      Cmask  : out Uint;
      Shift  : out Node_Id)
   is
      Loc    : constant Source_Ptr := Sloc (N);
      Ctyp   : Entity_Id;
      PAT    : Entity_Id;
      Otyp   : Entity_Id;
      Csiz   : Uint;
      Osiz   : Uint;

   begin
      Ctyp := Component_Type (Atyp);
      Csiz := Component_Size (Atyp);

      Convert_To_PAT_Type (Obj);
      PAT  := Etype (Obj);

      Cmask := 2 ** Csiz - 1;

      if Is_Array_Type (PAT) then
         Otyp := Component_Type (PAT);
         Osiz := Esize (Otyp);

      else
         Otyp := PAT;

         --  In the case where the PAT is a modular type, we want the actual
         --  size in bits of the modular value we use. This is neither the
         --  Object_Size nor the Value_Size, either of which may have been
         --  reset to strange values, but rather the minimum size. Note that
         --  since this is a modular type with full range, the issue of
         --  biased representation does not arise.

         Osiz := UI_From_Int (Minimum_Size (Otyp));
      end if;

      Compute_Linear_Subscript (Atyp, N, Shift);

      --  If the component size is not 1, then the subscript must be
      --  multiplied by the component size to get the shift count.

      if Csiz /= 1 then
         Shift :=
           Make_Op_Multiply (Loc,
             Left_Opnd => Make_Integer_Literal (Loc, Csiz),
             Right_Opnd => Shift);
      end if;

      --  If we have the array case, then this shift count must be broken
      --  down into a byte subscript, and a shift within the byte.

      if Is_Array_Type (PAT) then

         declare
            New_Shift : Node_Id;

         begin
            --  We must analyze shift, since we will duplicate it

            Set_Parent (Shift, N);
            Analyze_And_Resolve
              (Shift, Standard_Integer, Suppress => All_Checks);

            --  The shift count within the word is
            --    shift mod Osiz

            New_Shift :=
              Make_Op_Mod (Loc,
                Left_Opnd  => Duplicate_Subexpr (Shift),
                Right_Opnd => Make_Integer_Literal (Loc, Osiz));

            --  The subscript to be used on the PAT array is
            --    shift / Osiz

            Obj :=
              Make_Indexed_Component (Loc,
                Prefix => Obj,
                Expressions => New_List (
                  Make_Op_Divide (Loc,
                    Left_Opnd => Duplicate_Subexpr (Shift),
                    Right_Opnd => Make_Integer_Literal (Loc, Osiz))));

            Shift := New_Shift;
         end;

      --  For the modular integer case, the object to be manipulated is
      --  the entire array, so Obj is unchanged. Note that we will reset
      --  its type to PAT before returning to the caller.

      else
         null;
      end if;

      --  The one remaining step is to modify the shift count for the
      --  big-endian case. Consider the following example in a byte:

      --     xxxxxxxx  bits of byte
      --     vvvvvvvv  bits of value
      --     33221100  little-endian numbering
      --     00112233  big-endian numbering

      --  Here we have the case of 2-bit fields

      --  For the little-endian case, we already have the proper shift
      --  count set, e.g. for element 2, the shift count is 2*2 = 4.

      --  For the big endian case, we have to adjust the shift count,
      --  computing it as (N - F) - shift, where N is the number of bits
      --  in an element of the array used to implement the packed array,
      --  F is the number of bits in a source level array element, and
      --  shift is the count so far computed.

      if Bytes_Big_Endian xor Debug_Flag_8 then
         Shift :=
           Make_Op_Subtract (Loc,
             Left_Opnd  => Make_Integer_Literal (Loc, Osiz - Csiz),
             Right_Opnd => Shift);
      end if;

      Set_Parent (Shift, N);
      Set_Parent (Obj, N);
      Analyze_And_Resolve (Obj,   Otyp,             Suppress => All_Checks);
      Analyze_And_Resolve (Shift, Standard_Integer, Suppress => All_Checks);

      --  Make sure final type of object is the appropriate packed type

      Set_Etype (Obj, Otyp);

   end Setup_Inline_Packed_Array_Reference;

end Exp_Pakd;

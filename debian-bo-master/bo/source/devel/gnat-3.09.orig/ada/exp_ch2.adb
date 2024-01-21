------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 2                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.47 $                             --
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
with Elists;   use Elists;
with Nmake;    use Nmake;
with Sem;      use Sem;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Tbuild;   use Tbuild;
with Snames;   use Snames;

package body Exp_Ch2 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Discriminant (N : Node_Id);
   --  An occurence of a discriminant within a discriminated type is replaced
   --  with the corresponding discriminal, that is to say the formal parameter
   --  of the initialization procedure for the type that is associated with
   --  that particular discriminant. This replacement is not performed for
   --  discriminants of records that appear in constraints of component of the
   --  record, because Gigi uses the discriminant name to retrieve its value.
   --  In the other hand, it has to be performed for default expressions of
   --  components because they are used in the record init procedure.  See
   --  Einfo for more details, and Exp_Ch3, Exp_Ch9 for examples of use.

   procedure Expand_Entry_Index_Parameter (N : Node_Id);
   --  A reference to the identifier in the entry index specification
   --  of a protected entry body is modified to a reference to a constant
   --  definintion equal to the index of the entry family member being
   --  called.  This constant is calculated as part of the elaboration
   --  of the expanded code for the body, and is calculated from the
   --  object-wide entry index returned by Next_Entry_Call.

   procedure Expand_Entry_Parameter (N : Node_Id);
   --  A reference to an entry parameter is modified to be a reference to
   --  the corresponding component of the entry parameter record that is
   --  passed by the runtime to the accept body procedure

   procedure Expand_Formal (N : Node_Id);
   --  A reference to a formal parameter of a protected subprogram is
   --  expanded to the corresponding formal of the unprotected procedure
   --  used to represent the protected subprogram within the protected object.

   procedure Expand_Protected_Private (N : Node_Id);
   --  A reference to a private object of a protected type is expanded
   --  to a component selected from the record used to implement
   --  the protected object.  Such a record is passed to all operations
   --  on a protected object in a parameter named _object.  Such an object
   --  can be a left-hand side only within a protected procedure
   --  (or entry, not yet implemented ???).

   procedure Expand_Renaming (N : Node_Id);
   --  For renamings, just replace the identifier by the corresponding
   --  name expression. Note that this has been evaluated (see routine
   --  Exp_Ch8.Expand_N_Object_Renaming.Evaluate_Name) so this gives
   --  the correct renaming semantics.

   -------------------------
   -- Expand_Discriminant --
   -------------------------

   procedure Expand_Discriminant (N : Node_Id) is
      Scop     : constant Entity_Id := Scope (Entity (N));
      P        : Node_Id := N;
      Parent_P : Node_Id := Parent (P);

   begin
      --  The Incomplete_Or_Private_Kind happens while resolving the
      --  discriminant constraint involved in a derived full type,
      --  such as:

      --    type D is private;
      --    type D(C : ...) is new T(C);

      if Ekind (Scop) = E_Record_Type
        or Ekind (Scop) in Incomplete_Or_Private_Kind
      then

         --  Find the origin by walking up the tree till the component
         --  declaration

         while Present (Parent_P)
           and then Nkind (Parent_P) /= N_Component_Declaration
         loop
            P := Parent_P;
            Parent_P := Parent (P);
         end loop;

         --  If the discriminant reference was part of the default expression
         --  it has to be "discriminalized"

         if Present (Parent_P) and then P = Expression (Parent_P) then
            Set_Entity (N, Discriminal (Entity (N)));
         end if;

      else
         Set_Entity (N, Discriminal (Entity (N)));
      end if;
   end Expand_Discriminant;

   ----------------------------------
   -- Expand_Entry_Index_Parameter --
   ----------------------------------

   procedure Expand_Entry_Index_Parameter (N : Node_Id) is
   begin
      Set_Entity (N, Entry_Index_Constant (Entity (N)));
   end Expand_Entry_Index_Parameter;

   ----------------------------
   -- Expand_Entry_Parameter --
   ----------------------------

   procedure Expand_Entry_Parameter (N : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      Ent_Formal : constant Entity_Id  := Entity (N);
      Ent_Spec   : constant Entity_Id  := Scope (Ent_Formal);
      Parm_Type  : constant Entity_Id  := Entry_Parameters_Type (Ent_Spec);
      Acc_Stack  : constant Elist_Id   := Accept_Address (Ent_Spec);
      Addr_Ent   : constant Entity_Id  := Node (Last_Elmt (Acc_Stack));
      P_Comp_Ref : Entity_Id;

   begin
      --  What we need is a reference to the corresponding component of the
      --  parameter record object. The Accept_Address field of the entry
      --  entity references the address variable that contains the address
      --  of the accept parameters record. We first have to do an unchecked
      --  conversion to turn this into a pointer to the parameter record and
      --  then we select the required parameter field.

      P_Comp_Ref :=
        Make_Selected_Component (Loc,
          Prefix =>
            Unchecked_Convert_To (Parm_Type,
              New_Reference_To (Addr_Ent, Loc)),
          Selector_Name =>
            New_Reference_To (Entry_Component (Ent_Formal), Loc));

      --  For all types of parameters, the constructed parameter record
      --  object contains a pointer to the parameter. Thus we must
      --  dereference them to access them (this will often be redundant,
      --  since the needed deference is implicit, but no harm is done by
      --  making it explicit).

      Rewrite_Substitute_Tree (N,
        Make_Explicit_Dereference (Loc, P_Comp_Ref));

      Analyze (N);
   end Expand_Entry_Parameter;

   -------------------
   -- Expand_Formal --
   -------------------

   procedure Expand_Formal (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      E    : constant Entity_Id  := Entity (N);
      Subp : constant Entity_Id  := Scope (E);

   begin
      if Is_Protected_Type (Scope (Subp))
        and then Chars (Subp) /= Name_uInit_Proc
        and then Present (Protected_Formal (E))
      then
         Set_Entity (N, Protected_Formal (E));
      end if;
   end Expand_Formal;

   ------------------------------
   -- Expand_Protected_Private --
   ------------------------------

   procedure Expand_Protected_Private (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      E    : constant Entity_Id  := Entity (N);
      Op   : constant Node_Id    := Protected_Operation (E);
      Scop : Entity_Id;

   begin
      if Nkind (Op) /= N_Subprogram_Body
        or else Ekind (Defining_Unit_Name (Specification (Op))) /= E_Function
      then
         Set_Ekind (Prival (E), E_Variable);
      else
         Set_Ekind (Prival (E), E_Constant);
      end if;

      --  The type of the reference is the type of the prival, which may
      --  differ from that of the original component if it is an itype.

      Set_Entity  (N, Prival (E));
      Set_Etype   (N,  Etype (Prival (E)));
      Scop := Current_Scope;

      --  Find entity for protected operation, which must be on scope stack.

      while not Is_Protected_Type (Scope (Scop)) loop
         Scop := Scope (Scop);
      end loop;

      Append_Elmt (N, Privals_Chain (Scop));
   end Expand_Protected_Private;

   ---------------------
   -- Expand_Renaming --
   ---------------------

   procedure Expand_Renaming (N : Node_Id) is
      E : constant Entity_Id := Entity (N);
      T : constant Entity_Id := Etype (N);

   begin
      Rewrite_Substitute_Tree (N, New_Copy_Tree (Renamed_Object (E)));

      --  We mark the copy as unanalyzed, so that it is sure to be
      --  reanalyzed at the top level. This is needed in the packed
      --  case since we specifically avoided expanding packed array
      --  references when the renaming declaration was analyzed.

      Set_Analyzed (N, False);
      Analyze_And_Resolve (N, T);
   end Expand_Renaming;

   ----------------------------
   -- Expand_N_Expanded_Name --
   ----------------------------

   --  Performs discriminal and entry parameter replacement as described above

   procedure Expand_N_Expanded_Name (N : Node_Id) is
      E : constant Entity_Id := Entity (N);

   begin
      if Ekind (E) = E_Discriminant then
         Expand_Discriminant (N);

      elsif Is_Entry_Formal (E) then
         Expand_Entry_Parameter (N);

      elsif Ekind (E) = E_Component
        and then Is_Protected_Private (E)
      then
         Expand_Protected_Private (N);

      elsif Ekind (E) = E_Entry_Index_Parameter then
         Expand_Entry_Index_Parameter (N);

      elsif Is_Formal (E) then
         Expand_Formal (N);

      elsif Is_Renaming_Of_Object (E) then
         Expand_Renaming (N);
      end if;
   end Expand_N_Expanded_Name;

   -------------------------
   -- Expand_N_Identifier --
   -------------------------

   procedure Expand_N_Identifier (N : Node_Id) is
      E : constant Entity_Id := Entity (N);

   begin
      if Ekind (E) = E_Discriminant then
         Expand_Discriminant (N);

      elsif Is_Entry_Formal (E) then
         Expand_Entry_Parameter (N);

      elsif Ekind (E) = E_Component
        and then Is_Protected_Private (E)
      then
         Expand_Protected_Private (N);

      elsif Ekind (E) = E_Entry_Index_Parameter then
         Expand_Entry_Index_Parameter (N);

      elsif Is_Formal (E) then
         Expand_Formal (N);

      elsif Is_Renaming_Of_Object (E) then
         Expand_Renaming (N);
      end if;
   end Expand_N_Identifier;

   ------------------
   -- Param_Entity --
   ------------------

   --  This would be trivial, simply a test for an identifier that was a
   --  reference to a formal, if it were not for the fact that a previous
   --  call to Expand_Entry_Parameter will have modified the reference
   --  to the identifier to be of the form

   --    typ!(recobj).rec.all'Constrained

   --  where rec is a selector whose Entry_Formal link points to the formal

   function Param_Entity (N : Node_Id) return Entity_Id is
   begin
      --  Simple reference case

      if Nkind (N) = N_Identifier then
         if Is_Formal (Entity (N)) then
            return Entity (N);
         end if;

      else
         if Nkind (N) = N_Explicit_Dereference then
            declare
               P : constant Node_Id := Prefix (N);
               S : Node_Id;

            begin
               if Nkind (P) = N_Selected_Component then
                  S := Selector_Name (P);

                  if Present (Entry_Formal (Entity (S))) then
                     return Entry_Formal (Entity (S));
                  end if;
               end if;
            end;
         end if;
      end if;

      return (Empty);
   end Param_Entity;

end Exp_Ch2;

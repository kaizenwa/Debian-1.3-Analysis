------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 8                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.12 $                             --
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
with Exp_Util; use Exp_Util;
with Exp_Ch6;  use Exp_Ch6;
with Itypes;   use Itypes;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Tbuild;   use Tbuild;

package body Exp_Ch8 is

   ------------------------------------------
   -- Expand_N_Object_Renaming_Declaration --
   ------------------------------------------

   procedure Expand_N_Object_Renaming_Declaration (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Nam : Node_Id := Name (N);

      procedure Evaluate_Name (Fname : Node_Id);
      --  A recursive procedure used to freeze a name in the sense desribed
      --  above, i.e. any variable references or function calls are removed.
      --  Of course the outer level variable reference must not be removed.
      --  For example in A(J,F(K)), A is left as is, but J and F(K) are
      --  evaluated and removed.

      procedure Evaluate_Name (Fname : Node_Id) is
         K : constant Node_Kind := Nkind (Fname);
         E : Node_Id;

      begin
         --  For an explicit dereference, we simply force the evaluation
         --  of the name expression. The dereference provides a value that
         --  is the address for the renamed object, and it is precisely
         --  this value that we want to preserve.

         if K = N_Explicit_Dereference then
            Force_Evaluation (Prefix (Nam));

         --  For a selected component, we simply evaluate the prefix

         elsif K = N_Selected_Component then
            Force_Evaluation (Prefix (Nam));

         --  For an indexed component, or an attribute reference, we evaluate
         --  the prefix, which is itself a name, recursively, and then force
         --  the evaluation of all the subscripts (or attribute expressions).

         elsif K = N_Indexed_Component
           or else K = N_Attribute_Reference
         then
            Evaluate_Name (Prefix (Fname));

            E := First (Expressions (Fname));
            while Present (E) loop
               Force_Evaluation (E);
               E := Next (E);
            end loop;

         --  For a slice, we evalute the prefix, as for the indexed component
         --  case and then, if there is a range present, either directly or
         --  as the constraint of a discrete subtype indication, we evaluate
         --  the two bounds of this range.

         elsif K = N_Slice then
            Evaluate_Name (Prefix (Fname));

            declare
               DR     : constant Node_Id := Discrete_Range (Fname);
               Constr : Node_Id;
               Rexpr  : Node_Id;

            begin
               if Nkind (DR) = N_Range then
                  Force_Evaluation (Low_Bound (DR));
                  Force_Evaluation (High_Bound (DR));

               elsif Nkind (DR) = N_Subtype_Indication then
                  Constr := Constraint (DR);

                  if Nkind (Constr) = N_Range_Constraint then
                     Rexpr := Range_Expression (Constr);

                     Force_Evaluation (Low_Bound (Rexpr));
                     Force_Evaluation (High_Bound (Rexpr));
                  end if;
               end if;
            end;

         --  For a type conversion, the expression of the conversion must be
         --  the name of an object, and we simply need to evaluate this name.

         elsif K = N_Type_Conversion then
            Evaluate_Name (Expression (Fname));

         --  It is not clear if a function call is valid here??? In any case
         --  clearly the right approach if it is, is to force its evaluation.

         elsif K = N_Function_Call then
            Force_Evaluation (Fname);

         --  The remaining cases are direct name, operator symbol and
         --  character literal. In all these cases, we do nothing, since
         --  we want to reevaluate each time the renamed object is used.

         else
            return;
         end if;
      end Evaluate_Name;

   --  Start of processing for Expand_N_Object_Renaming_Declaration

   begin
      --  The main processing is that we must evaluate the expression for the
      --  renamed object. The implementation of renaming consists of doing a
      --  complete deep tree copy of the renamed object and inserting at the
      --  point of reference of the identifier. This works only if we perform
      --  necessary evaluation of all variables, function calls etc at the
      --  point of the renaming declaration.

      --  Currently we only do this for names denoting a component or a
      --  slice of a packed array or selected components where a component
      --  clause applies to the component. We set the Is_Renaming_Of_Object
      --  flag to indicate that we have performed the evaluation.

      --  There may be other cases where the same transformation would
      --  improve efficiency, but these are the only cases in which we must
      --  do the transformation, because the alternative approach of taking
      --  the address of the object does not work. The reason that we do not
      --  always do the transformation is that certain processing to do with
      --  privals blows up, and perhaps there are other problems.

      if (Nkind (Nam) = N_Indexed_Component
           or else
          Nkind (Nam) = N_Slice)
        and then
          Is_Packed (Etype (Prefix (Nam)))
      then
         Evaluate_Name (Nam);
         Set_Is_Renaming_Of_Object (Defining_Identifier (N));

      elsif Nkind (Nam) = N_Selected_Component
        and then Present (Component_Clause (Entity (Selector_Name (Nam))))
      then
         Evaluate_Name (Nam);
         Set_Is_Renaming_Of_Object (Defining_Identifier (N));
      end if;

   end Expand_N_Object_Renaming_Declaration;

   ----------------------------------------------
   -- Expand_N_Subprogram_Renaming_Declaration --
   ----------------------------------------------

   --  Same processing as for N_Subprogram_Declaration

   procedure Expand_N_Subprogram_Renaming_Declaration (N : Node_Id) is
      Subp     : Entity_Id := Defining_Entity (N);

   begin
      --  Generate Default expr functions only if it is not possible to
      --  generate them at the freezing point, where they really belong
      --  otherwise they will be inserted too soon and will cause all sorts
      --  of trouble (e.g. becoming primitive operations of a tagged type etc).

      if No (Corresponding_Spec (N))
        and then not Has_Delayed_Freeze (Subp)
      then
         Make_Default_Expr_Functions (N, Subp);
      end if;
   end Expand_N_Subprogram_Renaming_Declaration;

end Exp_Ch8;

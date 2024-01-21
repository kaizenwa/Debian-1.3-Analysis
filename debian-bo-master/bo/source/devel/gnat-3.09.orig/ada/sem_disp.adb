------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ D I S P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.80 $                             --
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
with Debug;    use Debug;
with Elists;   use Elists;
with Einfo;    use Einfo;
with Exp_Disp; use Exp_Disp;
with Errout;   use Errout;
with Nlists;   use Nlists;
with Output;   use Output;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Sinfo;    use Sinfo;
with Uintp;    use Uintp;

package body Sem_Disp is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Override_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      Prev_Op     : Entity_Id;
      New_Op      : Entity_Id);
   --  Replace an implicit dispatching operation with an  explicit one.
   --  Prev_Op is an inherited primitive operation which is overridden
   --  by the explicit declaration of New_Op.

   procedure Add_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      New_Op      : Entity_Id);
   --  Add New_Op in the list of primitive operations of Tagged_Type

   function Check_Controlling_Type
     (T    : Entity_Id;
      Subp : Entity_Id)
      return Entity_Id;
      --  T is the type of a formal parameter of subp. Returns the tagged
      --  if the parameter can be a controlling argument, empty otherwise

   ----------------------------
   -- Check_Controlling_Type --
   ----------------------------

   function Check_Controlling_Type
     (T    : Entity_Id;
      Subp : Entity_Id)
      return Entity_Id
   is
      Tagged_Type : Entity_Id := Empty;

   begin
      if Is_Tagged_Type (T) then
         Tagged_Type := Base_Type (T);

      elsif Ekind (T) = E_Anonymous_Access_Type
        and then Is_Tagged_Type (Designated_Type (T))
        and then Ekind (Designated_Type (T)) /= E_Incomplete_Type
      then
         Tagged_Type := Base_Type (Designated_Type (T));
      end if;

      if No (Tagged_Type)
        or else Is_Class_Wide_Type (Tagged_Type)
      then
         return Empty;

      --  The dispatching type and the primitive operation must be defined
      --  in the same scope except for internal operations.

      elsif (Scope (Subp) = Scope (Tagged_Type)
              or else Is_Internal (Subp))
        and then
            (not Is_Generic_Type (Tagged_Type)
              or else not Comes_From_Source (Subp))
      then
         return Tagged_Type;

      else
         return Empty;
      end if;
   end Check_Controlling_Type;

   ---------------------------
   -- Find_Dispatching_Type --
   ---------------------------

   function Find_Dispatching_Type (Subp : Entity_Id) return Entity_Id is
      Formal    : Entity_Id;
      Ctrl_Type : Entity_Id;

   begin
      if Present (DTC_Entity (Subp)) then
         return Scope (DTC_Entity (Subp));

      else
         Formal := First_Formal (Subp);
         while Present (Formal) loop
            Ctrl_Type := Check_Controlling_Type (Etype (Formal), Subp);
            if Present (Ctrl_Type) then
               return Ctrl_Type;
            end if;
            Formal := Next_Formal (Formal);
         end loop;

      --  The subprogram may also be dispatching on result

         if Present (Etype (Subp)) then
            Ctrl_Type := Check_Controlling_Type (Etype (Subp), Subp);
            if Present (Ctrl_Type) then
               return Ctrl_Type;
            end if;
         end if;
      end if;

      return Empty;
   end Find_Dispatching_Type;

   -------------------------------
   -- Check_Controlling_Formals --
   -------------------------------

   procedure Check_Controlling_Formals
     (Typ  : Entity_Id;
      Subp : Entity_Id)
   is
      Formal    : Entity_Id;
      Ctrl_Type : Entity_Id;

   begin
      Formal := First_Formal (Subp);
      while Present (Formal) loop
         Ctrl_Type := Check_Controlling_Type (Etype (Formal), Subp);
         if Present (Ctrl_Type) then
            if Ctrl_Type = Typ then
               Set_Is_Controlling_Formal (Formal);

               --  Check that the parameter's nominal subtype statically
               --  matches the first subtype.

               if Ekind (Etype (Formal)) = E_Anonymous_Access_Type then
                  if not Subtypes_Statically_Match
                           (Typ, Designated_Type (Etype (Formal)))
                  then
                     Error_Msg_N
                       ("parameter subtype does not match controlling type",
                        Formal);
                  end if;

               elsif not Subtypes_Statically_Match (Typ, Etype (Formal)) then
                  Error_Msg_N
                    ("parameter subtype does not match controlling type",
                     Formal);
               end if;

               if Present (Default_Value (Formal)) then
                  if Ekind (Etype (Formal)) = E_Anonymous_Access_Type then
                     Error_Msg_N
                       ("default not allowed for controlling access parameter",
                        Default_Value (Formal));

                  --  Is "tag indeterminate" too RM'y for a message. Is there
                  --  a clearer way of saying this without jargon???

                  elsif not Is_Tag_Indeterminate (Default_Value (Formal)) then
                     Error_Msg_N
                       ("default expression must be tag indeterminate",
                        Default_Value (Formal));
                  end if;
               end if;

            elsif Comes_From_Source (Subp) then
               Error_Msg_N
                 ("operation can be dispatching in only one type", Subp);
            end if;
         end if;

         Formal := Next_Formal (Formal);
      end loop;

      if Present (Etype (Subp)) then
         Ctrl_Type := Check_Controlling_Type (Etype (Subp), Subp);

         if Present (Ctrl_Type) then
            if Ctrl_Type = Typ then
               Set_Has_Controlling_Result (Subp);

               --  Check that the result subtype statically matches
               --  the first subtype.

               if not Subtypes_Statically_Match (Typ, Etype (Subp)) then
                  Error_Msg_N
                    ("result subtype does not match controlling type", Subp);
               end if;

            elsif Comes_From_Source (Subp) then
               Error_Msg_N
                 ("operation can be dispatching in only one type", Subp);
            end if;
         end if;
      end if;
   end Check_Controlling_Formals;

   ---------------------------------
   -- Check_Dispatching_Operation --
   ---------------------------------

   procedure Check_Dispatching_Operation (Subp, Old_Subp : Entity_Id) is
      Tagged_Seen : Entity_Id;

   begin
      if Ekind (Subp) /= E_Procedure and then Ekind (Subp) /= E_Function then
         return;
      end if;

      Set_Is_Dispatching_Operation (Subp, False);
      Tagged_Seen := Find_Dispatching_Type (Subp);

      if No (Tagged_Seen) then
         return;

      --  The subprograms build internally after the freezing point (such as
      --  the Init procedure) are not primitives

      elsif Is_Frozen (Tagged_Seen)
        and then not Comes_From_Source (Subp)
      then
         return;

      --  The operation may be a child unit, whose scope is the defining
      --  package, but which is not a primitive operation of the type.

      elsif Is_Child_Unit (Subp) then
         return;

      --  If the subprogram is not defined in a package spec, the only case
      --  where it can be a dispatching op. if when it overriddes an
      --  operation before the freezing point of the type

      elsif (Ekind (Scope (Subp)) /= E_Package
              and then Ekind (Scope (Subp)) /= E_Generic_Package)
        or else In_Package_Body (Scope (Subp))
      then
         if not Comes_From_Source (Subp)
           or else (Present (Old_Subp) and then not Is_Frozen (Tagged_Seen))
         then
            null;

         --  If the type is already frozen, the overriding is not allowed
         --  except when Old_Subp is not a dispatching operation (which
         --  can occur when Old_Subp was inherited by an untagged type).

         elsif Present (Old_Subp)
           and then Is_Dispatching_Operation (Old_Subp)
         then
            Error_Msg_N ("overriding of& is too late!", Subp);
            Error_Msg_N
            ("subprogram spec should appear immediately after the type!",
             Subp);

         --  If the type is not frozen yet and we are not in the overridding
         --  case it looks suspiciously like an attempt to define a primitive
         --  operation.

         elsif not Is_Frozen (Tagged_Seen) then
            Error_Msg_N
              ("?not dispatching (must be defined in a package spec)", Subp);
            return;

         --  When the type is frozen, it is legitimate to define a new
         --  non-primitive operation.

         else
            return;
         end if;

      --  Now, we are sure that the scope is a package spec. If the subprogram
      --  is declared after the freezing point ot the type that's an error

      elsif Is_Frozen (Tagged_Seen) then
         Error_Msg_N ("this primitive operation is declared too late", Subp);
         Error_Msg_NE
           ("?no primitive operations for& after this line",
            Freeze_Node (Tagged_Seen),
            Tagged_Seen);
         return;
      end if;

      Check_Controlling_Formals (Tagged_Seen, Subp);

      --  Now it should be a correct primitive operation, put it in the list

      if Present (Old_Subp) then
         Check_Subtype_Conformant (Subp, Old_Subp);
         Override_Dispatching_Operation (Tagged_Seen, Old_Subp, Subp);

      else
         Add_Dispatching_Operation (Tagged_Seen, Subp);
      end if;

      Set_Is_Dispatching_Operation (Subp, True);
      Set_DT_Position (Subp, No_Uint);

   end Check_Dispatching_Operation;

   --------------------------------
   --  Add_Dispatching_Operation --
   --------------------------------

   procedure Add_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      New_Op      : Entity_Id)
   is
      List : constant Elist_Id := Primitive_Operations (Tagged_Type);

   begin
         Append_Elmt (New_Op, List);
   end Add_Dispatching_Operation;

   ---------------------------------------
   -- Check_Operation_From_Private_View --
   ---------------------------------------

   procedure Check_Operation_From_Private_View (Subp, Old_Subp : Entity_Id) is
      Tagged_Type : Entity_Id;
   begin

      if Is_Dispatching_Operation (Alias (Subp)) then
         Set_Scope (Subp, Current_Scope);
         Tagged_Type := Find_Dispatching_Type (Subp);
         Append_Elmt (Old_Subp, Primitive_Operations (Tagged_Type));
         if Present (Alias (Old_Subp)) then
            Set_Alias (Old_Subp, Alias (Subp));

            --  The derived subprogram should inherit the abstractness
            --  of the parent subprogram (except in the case of a function
            --  returning the type). This sets the abstractness properly
            --  for cases where a private extension may have inherited
            --  an abstract operation, but the full type is derived from
            --  a descendant type and inherits a nonabstract version.

            if Etype (Subp) /= Tagged_Type then
               Set_Is_Abstract (Old_Subp, Is_Abstract (Alias (Subp)));
            end if;
         end if;
      end if;
   end Check_Operation_From_Private_View;

   ----------------------------
   -- Check_Dispatching_Call --
   ----------------------------

   procedure Check_Dispatching_Call (N : Node_Id) is
      Actual  : Node_Id;
      Control : Node_Id := Empty;

   begin
      --  Find a controlling argument, if any

      if Present (Parameter_Associations (N)) then
         Actual := First_Actual (N);

         while Present (Actual) loop
            Control := Find_Controlling_Arg (Actual);
            exit when Present (Control);
            Actual := Next_Actual (Actual);
         end loop;

         if Present (Control) then

            --  Verify that no controlling arguments are statically tagged

            if Debug_Flag_E then
               Write_Str ("Found Dispatching call");
               Write_Int (Int (N));
               Write_Eol;
            end if;

            Actual := First_Actual (N);

            while Present (Actual) loop
               if Actual /= Control then

                  if not Is_Controlling_Actual (Actual) then
                     null; -- can be anything

                  elsif (Is_Dynamically_Tagged (Actual)) then
                     null; --  valid parameter

                  elsif Is_Tag_Indeterminate (Actual) then

                     --  The tag is inherited from the enclosing call (the
                     --  node we are currently analyzing). Explicitly expand
                     --  the actual, since the previous call to Expand
                     --  (from Resolve_Call) had no way of knowing about
                     --  the required dispatching.

                     Propagate_Tag (Control, Actual);

                  else
                     Error_Msg_N
                       ("controlling argument is not dynamically tagged",
                        Actual);
                     return;
                  end if;
               end if;

               Actual := Next_Actual (Actual);
            end loop;

            --  Mark call as a dispatching call

            Set_Controlling_Argument (N, Control);

         else
            --  The call is not dispatching, check that there isn't any
            --  tag indeterminate abstract call left

            Actual := First_Actual (N);

            while Present (Actual) loop
               if Is_Tag_Indeterminate (Actual)
                 and then Is_Abstract (Entity (Name (Original_Node (Actual))))
               then
                  Error_Msg_N (
                    "call to abstract function must be dispatching", N);
               end if;

               Actual := Next_Actual (Actual);
            end loop;
         end if;

      else
         --  If dispatching on result, the enclosing call, if any, will
         --  determine the controlling argument. Otherwise this is the
         --  primitive operation of the root type.

         null;
      end if;
   end Check_Dispatching_Call;

   ---------------------------
   -- Is_Dynamically_Tagged --
   ---------------------------

   function Is_Dynamically_Tagged (N : Node_Id) return Boolean is
   begin
      return Find_Controlling_Arg (N) /= Empty;
   end Is_Dynamically_Tagged;

   --------------------------
   -- Find_Controlling_Arg --
   --------------------------

   function Find_Controlling_Arg (N : Node_Id) return Node_Id is
      Orig_Node : constant Node_Id := Original_Node (N);
      Typ       : Entity_Id;

   begin
      if Nkind (Orig_Node) = N_Qualified_Expression then
         return Find_Controlling_Arg (Expression (Orig_Node));
      end if;

      --  Dispatching on result case

      if Nkind (Orig_Node) = N_Function_Call
        and then Present (Controlling_Argument (Orig_Node))
        and then Has_Controlling_Result (Entity (Name (Orig_Node)))
      then
         return Controlling_Argument (Orig_Node);

      --  Normal case

      elsif Is_Controlling_Actual (N) then

         Typ := Etype (N);
         if Is_Access_Type (Typ) then
            Typ := Designated_Type (Typ);
         end if;

         if Is_Class_Wide_Type (Typ) then
            return N;
         end if;
      end if;

      return Empty;
   end Find_Controlling_Arg;

   --------------------------
   -- Is_Tag_Indeterminate --
   --------------------------

   function Is_Tag_Indeterminate (N : Node_Id) return Boolean is
      Nam       : Entity_Id;
      Actual    : Node_Id;
      Orig_Node : constant Node_Id := Original_Node (N);

   begin
      if Nkind (Orig_Node) = N_Function_Call
        and then Is_Entity_Name (Name (Orig_Node))
      then
         Nam := Entity (Name (Orig_Node));

         if not Has_Controlling_Result (Nam) then
            return False;

         --  If there are no actuals, the call is tag-indeterminate

         elsif No (Parameter_Associations (Orig_Node)) then
            return True;

         else
            Actual := First_Actual (Orig_Node);

            while Present (Actual) loop
               if Is_Controlling_Actual (Actual)
                 and then not Is_Tag_Indeterminate (Actual)
               then
                  return False; -- one operand is dispatching
               end if;

               Actual := Next_Actual (Actual);
            end loop;

            return True;

         end if;

      elsif Nkind (Orig_Node) = N_Qualified_Expression then
         return Is_Tag_Indeterminate (Expression (Orig_Node));

      else
         return False;
      end if;
   end Is_Tag_Indeterminate;

   ------------------------------------
   -- Override_Dispatching_Operation --
   ------------------------------------

   procedure Override_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      Prev_Op     : Entity_Id;
      New_Op      : Entity_Id)
   is
      Op_Elmt   : Elmt_Id := First_Elmt (Primitive_Operations (Tagged_Type));
      Prev_Prev : Entity_Id;
      Prev_New  : Entity_Id;

   begin
      --  Patch the primitive operation list

      while Present (Op_Elmt)
        and then Node (Op_Elmt) /= Prev_Op
      loop
         Op_Elmt := Next_Elmt (Op_Elmt);
      end loop;

      --  If there is no previous operation to override, the type declaration
      --  was malformed, and an error must have been emitted already.

      if No (Op_Elmt) then
         return;
      end if;

      Replace_Elmt (Op_Elmt, New_Op);

      --  if New_Op is a private primitive that overriddes a nonprivate one
      --  make it visible by transfering it into the public part of the
      --  package. Since the overridden entity has been removed from the
      --  visiblity chain, this is the simplest way of implementing 3.9.2
      --  (20) and making sure that the operation is always visible.
      --  This is not completely accurate when default parameters are
      --  present since the one of the private primitive will be used
      --  instead of the inherited one the private one is not visible???

      if (Ekind (Current_Scope) /= E_Package
           and then Ekind (Current_Scope) /= E_Generic_Package)
        or else not In_Private_Part (Current_Scope)
      then
         --  not a private primitive

         null;

      else
         --  Find the place where the prev_op was in the entity chain

         Prev_Prev := First_Entity (Current_Scope);
         while Present (Prev_Prev)
           and then Next_Entity (Prev_Prev) /= Next_Entity (Prev_Op)
         loop
            Prev_Prev := Next_Entity (Prev_Prev);
         end loop;

         --  Insert New_Op where Prev_Op used to live

         if Present (Prev_Prev) and then Prev_Prev /= New_Op then

            Prev_New := Prev_Prev;
            while Next_Entity (Prev_New) /= New_Op loop
               Prev_New := Next_Entity (Prev_New);
            end loop;

            --  unchain New_Op

            Set_Next_Entity (Prev_New, Next_Entity (New_Op));

            --  rechain it at prev_op location

            Set_Next_Entity (New_Op, Next_Entity (Prev_Prev));
            Set_Next_Entity (Prev_Prev, New_Op);

            --  Patch the last entity

            while Present (Next_Entity (Prev_New)) loop
               Prev_New := Next_Entity (Prev_New);
            end loop;
            Set_Last_Entity (Current_Scope, Prev_New);
         end if;
      end if;
   end Override_Dispatching_Operation;

   -------------------
   -- Propagate_Tag --
   -------------------

   procedure Propagate_Tag (Control : Node_Id; Actual : Node_Id) is
      Call_Node : Node_Id;
      Arg       : Node_Id;

   begin
      if Nkind (Actual) = N_Function_Call then
         Call_Node := Actual;

      --  Only other possibility is parenthesized or qualified expression

      else
         Call_Node := Expression (Actual);
      end if;

      --  Do not set the Controlling_Argument if already set. This happens
      --  in the special case of _Input (see Exp_Attr, case Input).

      if No (Controlling_Argument (Call_Node)) then
         Set_Controlling_Argument (Call_Node, Control);
      end if;

      Arg := First_Actual (Call_Node);

      while Present (Arg) loop
         if Is_Tag_Indeterminate (Arg) then
            Propagate_Tag (Control,  Arg);
         end if;

         Arg := Next_Actual (Arg);
      end loop;

      Expand_Dispatch_Call (Call_Node);
   end Propagate_Tag;

end Sem_Disp;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ I N T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.16 $                             --
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

--  Processing for intrinsic subprogram declarations

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Fname;    use Fname;
with Hostparm; use Hostparm;
with Lib;      use Lib;
with Namet;    use Namet;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Uintp;    use Uintp;

package body Sem_Intr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Exception_Function (E : Entity_Id; N : Node_Id);
   --  Check use of intrinsic Exception_Message, Exception_Info or
   --  Exception_Name, as used in the DEC compatible Current_Exceptions
   --  package. In each case we must have a parameterless function that
   --  returns type String.

   procedure Check_Shift (E : Entity_Id; N : Node_Id);
   --  Check intrinsic shift subprogram, the two arguments are the same
   --  as for Check_Intrinsic_Subprogram (i.e. the entity of the subprogram
   --  declaration, and the node for the pragma argument, used for messages)

   procedure Errint (Msg : String; S : Node_Id; N : Node_Id);
   --  Post error message for bad intrinsic, the message itself is posted
   --  on the appropriate spec node and another message is placed on the
   --  pragma itself, referring to the spec. S is the node in the spec on
   --  which the message is to be placed, and N is the pragma argument node.

   ------------------------------
   -- Check_Exception_Function --
   ------------------------------

   procedure Check_Exception_Function (E : Entity_Id; N : Node_Id) is
   begin
      if Ekind (E) /= E_Function
        and then Ekind (E) /= E_Generic_Function
      then
         Errint
           ("intrinsic exception subprogram must be a function", E, N);

      elsif Present (First_Formal (E)) then
         Errint
           ("intrinsic exception subprogram may not have parameters",
            E, First_Formal (E));
         return;

      elsif Etype (E) /= Standard_String then
         Errint
           ("return type of exception subprogram must be String", E, N);
         return;
      end if;
   end Check_Exception_Function;

   --------------------------
   -- Check_Intrinsic_Call --
   --------------------------

   procedure Check_Intrinsic_Call (N : Node_Id) is
      Nam  : constant Entity_Id := Entity (Name (N));
      Cnam : constant Name_Id   := Chars (Nam);
      Arg1 : constant Node_Id   := First_Actual (N);

   begin
      --  For Import_xxx calls, argument must be static string

      if Cnam = Name_Import_Address
           or else
         Cnam = Name_Import_Largest_Value
           or else
         Cnam = Name_Import_Value
      then
         if Etype (Arg1) = Any_Type
           or else Raises_Constraint_Error (Arg1)
         then
            null;

         elsif not Is_Static_Expression (Arg1) then
            Error_Msg_NE
              ("call to & requires static string argument", N, Nam);

         elsif String_Length (Strval (Expr_Value_S (Arg1))) = 0 then
            Error_Msg_NE
              ("call to & does not permit null string", N, Nam);

         elsif (OpenVMS or Debug_Flag_M)
           and then String_Length (Strval (Expr_Value_S (Arg1))) > 31
         then
            Error_Msg_NE
              ("argument in call to & must be 31 characters or less", N, Nam);
         end if;

      --  For now, no other special checks are required

      else
         return;
      end if;
   end Check_Intrinsic_Call;

   --------------------------------
   -- Check_Intrinsic_Subprogram --
   --------------------------------

   procedure Check_Intrinsic_Subprogram (E : Entity_Id; N : Node_Id) is
      Spec : constant Node_Id := Specification (Get_Declaration_Node (E));
      Nam  : Name_Id;

   begin
      if Present (Spec)
        and then Present (Generic_Parent (Spec))
      then
         Nam := Chars (Generic_Parent (Spec));
      else
         Nam := Chars (E);
      end if;

      --  Check name is valid intrinsic name

      Get_Name_String (Nam);

      if Name_Buffer (1) /= 'O'
        and then Nam /= Name_Asm
        and then Nam not in First_Intrinsic_Name .. Last_Intrinsic_Name
      then
         Errint ("unrecognized intrinsic subprogram", E, N);

      --  We always allow intrinsic specifications in language defined units
      --  and in expanded code. We assume that the GNAT implemetors know what
      --  they are doing, and do not write or generate junk use of intrinsic!

      elsif not Comes_From_Source (E)
        or else not Comes_From_Source (N)
        or else Is_Predefined_File_Name
                  (Unit_File_Name (Get_Sloc_Unit_Number (Sloc (N))))
      then
         null;

      --  Shift cases. We allow user specification of intrinsic shift
      --  operators for any numeric types.

      elsif
        Nam = Name_Rotate_Left
          or else
        Nam = Name_Rotate_Right
          or else
        Nam = Name_Shift_Left
          or else
        Nam = Name_Shift_Right
          or else
        Nam = Name_Shift_Right_Arithmetic
      then
         Check_Shift (E, N);

      elsif
        Nam = Name_Exception_Information
          or else
        Nam = Name_Exception_Message
          or else
        Nam = Name_Exception_Name
      then
         Check_Exception_Function (E, N);

      --  For now, no other intrinsic subprograms are recognized in user code

      else
         Errint ("incorrect context for ""Intrinsic"" convention", E, N);
      end if;
   end Check_Intrinsic_Subprogram;

   -----------------
   -- Check_Shift --
   -----------------

   procedure Check_Shift (E : Entity_Id; N : Node_Id) is
      Arg1 : Node_Id;
      Arg2 : Node_Id;
      Size : Nat;

   begin
      if Ekind (E) /= E_Function
        and then Ekind (E) /= E_Generic_Function
      then
         Errint ("intrinsic shift subprogram must be a function", E, N);
         return;
      end if;

      Arg1 := First_Formal (E);

      if Present (Arg1) then
         Arg2 := Next_Formal (Arg1);
      else
         Arg2 := Empty;
      end if;

      if Arg1 = Empty or else Arg2 = Empty then
         Errint ("intrinsic shift function must have two arguments", E, N);
         return;
      end if;

      if not Is_Integer_Type (Etype (Arg1)) then
         Errint ("first argument to shift must be integer type", Arg1, N);
         return;
      end if;

      if Etype (Arg2) /= Standard_Natural then
         Errint ("second argument to shift must be type Natural", Arg2, N);
         return;
      end if;

      Size := UI_To_Int (Esize (Etype (Arg1)));

      if Size /= 8
        and then Size /= 16
        and then Size /= 32
        and then Size /= 64
      then
         Errint
           ("first argument for shift must have size 8, 16, 32 or 64",
             Parameter_Type (Arg1), N);
         return;

      elsif Etype (Arg1) /= Etype (E) then
         Errint
           ("return type of shift must match first argument", E, N);
         return;
      end if;
   end Check_Shift;

   ------------
   -- Errint --
   ------------

   procedure Errint (Msg : String; S : Node_Id; N : Node_Id) is
   begin
      Error_Msg_N (Msg, S);
      Error_Msg_N ("incorrect intrinsic subprogram, see spec", N);
   end Errint;

end Sem_Intr;

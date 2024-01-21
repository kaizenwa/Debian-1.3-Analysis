------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D B U G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.9 $                              --
--                                                                          --
--            Copyright (C) 1996 Free Software Foundation, Inc.             --
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
with Exp_Util; use Exp_Util;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Sem_Eval; use Sem_Eval;
with Sinfo;    use Sinfo;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Exp_Dbug is

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Add_Char_To_Buffer (C : Character);
   --  Add given character to Name_Buffer, updating Name_Len

   procedure Add_String_To_Buffer (S : String);
   --  Add given string to Name_Buffer, updating Name_Len

   procedure Add_Integer_To_Buffer (U : Uint);
   --  Add image of universal integer to Name_Buffer, updating Name_Len

   procedure Add_Real_To_Buffer (U : Ureal);
   --  Add nnn_ddd to Name_Buffer, where nnn and ddd are integer values of
   --  the normalized numerator and denominator of the given real value.

   function Bounds_Match_Size (E : Entity_Id) return  Boolean;
   --  Determine whether the bounds of E match the size of the type. This is
   --  used to determine whether encoding is required for a discrete type.

   ------------------------
   -- Add_Char_To_Buffer --
   ------------------------

   procedure Add_Char_To_Buffer (C : Character) is
   begin
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := C;
   end Add_Char_To_Buffer;

   ---------------------------
   -- Add_Integer_To_Buffer --
   ---------------------------

   procedure Add_Integer_To_Buffer (U : Uint) is
   begin
      if U < 0 then
         Add_Integer_To_Buffer (-U);
         Add_Char_To_Buffer ('m');

      else
         UI_Image (U, Decimal);
         Name_Buffer (Name_Len + 1 .. Name_Len + UI_Image_Length) :=
           UI_Image_Buffer (1 .. UI_Image_Length);
         Name_Len := Name_Len + UI_Image_Length;
      end if;
   end Add_Integer_To_Buffer;

   ------------------------
   -- Add_Real_To_Buffer --
   ------------------------

   procedure Add_Real_To_Buffer (U : Ureal) is
   begin
      Add_Integer_To_Buffer (Norm_Num (U));
      Add_String_To_Buffer ("_");
      Add_Integer_To_Buffer (Norm_Den (U));
   end Add_Real_To_Buffer;

   --------------------------
   -- Add_String_To_Buffer --
   --------------------------

   procedure Add_String_To_Buffer (S : String) is
   begin
      Name_Buffer (Name_Len + 1 .. Name_Len + S'Length) := S;
      Name_Len := Name_Len + S'Length;
   end Add_String_To_Buffer;

   -----------------------
   -- Bounds_Match_Size --
   -----------------------

   function Bounds_Match_Size (E : Entity_Id) return Boolean is
      Siz : Uint;

   begin
      if not Is_OK_Static_Subtype (E) then
         return False;

      elsif Is_Integer_Type (E)
        and then Subtypes_Statically_Match (E, Base_Type (E))
      then
         return True;

      --  Here we check if the static bounds match the natural size, which
      --  is the size passed through with the debugging information. This
      --  is the Esize rounded up to 8, 16, 32 or 64 as appropriate.

      else
         if Esize (E) <= 8 then
            Siz := Uint_8;
         elsif Esize (E) <= 16 then
            Siz := Uint_16;
         elsif Esize (E) <= 32 then
            Siz := Uint_32;
         else
            Siz := Uint_64;
         end if;

         if Is_Modular_Integer_Type (E) or else Is_Enumeration_Type (E) then
            return
              Expr_Rep_Value (Type_Low_Bound (E)) = 0
                and then
              2 ** Siz - Expr_Rep_Value (Type_High_Bound (E)) = 1;

         else
            return
              Expr_Rep_Value (Type_Low_Bound (E)) + 2 ** (Siz - 1) = 0
                and then
              2 ** (Siz - 1) - Expr_Rep_Value (Type_High_Bound (E)) = 1;
         end if;
      end if;
   end Bounds_Match_Size;

   ---------------------------
   -- Get_Encoded_Type_Name --
   ---------------------------

   --  Note: see spec for details on encodings

   function Get_Encoded_Type_Name (E : Entity_Id) return Boolean is
   begin
      Name_Len := 0;

      --  Fixed-point case

      if Is_Fixed_Point_Type (E) then

         Name_Len := 0;
         Add_String_To_Buffer ("XF_");
         Add_Real_To_Buffer (Delta_Value (E));

         if Small_Value (E) /= Delta_Value (E) then
            Add_String_To_Buffer ("_");
            Add_Real_To_Buffer (Small_Value (E));
         end if;

      --  Discrete case with bounds not matching size

      elsif Is_Discrete_Type (E) and then not Bounds_Match_Size (E) then
         Add_String_To_Buffer ("XD");

         declare
            Lo : constant Node_Id := Type_Low_Bound (E);
            Hi : constant Node_Id := Type_High_Bound (E);

            Lo_Stat : constant Boolean := Is_OK_Static_Expression (Lo);
            Hi_Stat : constant Boolean := Is_OK_Static_Expression (Hi);

            Lo_Discr : constant Boolean :=
                         Nkind (Lo) = N_Identifier
                           and then
                         Ekind (Entity (Lo)) = E_Discriminant;

            Hi_Discr : constant Boolean :=
                         Nkind (Hi) = N_Identifier
                           and then
                         Ekind (Entity (Hi)) = E_Discriminant;

            Lo_Encode : constant Boolean := Lo_Stat or Lo_Discr;
            Hi_Encode : constant Boolean := Hi_Stat or Hi_Discr;

         begin
            if Lo_Encode or Hi_Encode then
               if Lo_Encode then
                  if Hi_Encode then
                     Add_String_To_Buffer ("LU_");
                  else
                     Add_String_To_Buffer ("L_");
                  end if;
               else
                  Add_String_To_Buffer ("U_");
               end if;

               if Lo_Stat then
                  Add_Integer_To_Buffer (Expr_Rep_Value (Lo));
               elsif Lo_Discr then
                  Get_Name_String_And_Append (Chars (Entity (Lo)));
               end if;

               if Lo_Encode and Hi_Encode then
                  Add_String_To_Buffer ("__");
               end if;

               if Hi_Stat then
                  Add_Integer_To_Buffer (Expr_Rep_Value (Hi));
               elsif Hi_Discr then
                  Get_Name_String_And_Append (Chars (Entity (Hi)));
               end if;
            end if;
         end;


      --  For all other cases, the encoded name is the normal type name

      else
         return False;
      end if;

      --  If we fall through then the Name_Buffer contains the encoded name

      Name_Buffer (Name_Len + 1) := Ascii.Nul;

      if Debug_Flag_B then
         Write_Str ("**** type ");
         Write_Name (Chars (E));
         Write_Str (" is encoded as ");
         Write_Str (Name_Buffer (1 .. Name_Len));
         Write_Eol;
      end if;

      return True;

   end Get_Encoded_Type_Name;

end Exp_Dbug;

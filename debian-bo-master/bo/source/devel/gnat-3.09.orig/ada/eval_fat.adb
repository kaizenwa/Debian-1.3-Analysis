------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E V A L _ F A T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.7 $                              --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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

with Stand;  use Stand;
with Ttypef; use Ttypef;

package body Eval_Fat is

   Radix : Uint renames Uint_2;
   --  This code is currently only correct for the radix 2 case

   function Float_Radix return Ureal renames Ureal_2;
   --  Radix expressed in real form

   function Float_Radix_Inv return Ureal renames Ureal_Half;
   --  Inverse of radix expressed in real form

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Decompose
     (RT       : R;
      X        : in T;
      Fraction : out T;
      Exponent : out UI);
   --  Decomposes a floating-point number into fraction and exponent parts

   function Eps_Model (RT : R) return T;
   --  Return the smallest model number of R.

   function Eps_Denorm (RT : R) return T;
   --  Return the smallest denormal of type R.

   function Machine_Mantissa (RT : R) return UI;
   --  Get value of machine mantissa

   function Radix_To_M_Minus_1 (RT : R) return T;
   --  Returns value of radix to power of mantissa minus one

   --------------
   -- Adjacent --
   --------------

   function Adjacent (RT : R; X, Towards : T) return T is
   begin
      if Towards = X then
         return X;

      elsif Towards > X then
         return Succ (RT, X);

      else
         return Pred (RT, X);
      end if;
   end Adjacent;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (RT : R; X : T) return T is
      XT : constant T := Truncation (RT, X);

   begin
      if UR_Is_Negative (X) then
         return XT;

      elsif X = XT then
         return X;

      else
         return XT + Ureal_1;
      end if;
   end Ceiling;

   -------------
   -- Compose --
   -------------

   function Compose (RT : R; Fraction : T; Exponent : UI) return T is
      Arg_Frac : T;
      Arg_Exp  : UI;

   begin
      Decompose (RT, Fraction, Arg_Frac, Arg_Exp);
      return Scaling (RT, Arg_Frac, Exponent);
   end Compose;

   ---------------
   -- Copy_Sign --
   ---------------

   function Copy_Sign (RT : R; Value, Sign : T) return T is
      Result : T;

   begin
      Result := abs Value;

      if UR_Is_Negative (Sign) then
         return -Result;
      else
         return Result;
      end if;
   end Copy_Sign;

   ---------------
   -- Decompose --
   ---------------

   procedure Decompose
     (RT       : R;
      X        : in T;
      Fraction : out T;
      Exponent : out UI)
   is
      Factor   : T;
      My_Fract : T;
      Sign_X   : T;
      Abs_X    : T;
      Unit     : UI;
      My_Exp   : UI;
      Scale    : UI;

   begin
      if X = Ureal_0 then
         Fraction := Ureal_0;
         Exponent := Uint_0;
         return;
      end if;

      Abs_X := abs X;

      if UR_Is_Negative (X) then
         Sign_X := -Ureal_1;
      else
         Sign_X := Ureal_1;
      end if;

      if Abs_X >= Ureal_1 then
         Factor := Float_Radix_Inv;
         Unit := Uint_1;

      else
         Factor := Float_Radix;
         Unit := -Uint_1;
      end if;

      My_Fract := Abs_X;
      My_Exp := Uint_0;

      while My_Fract >= Ureal_1
        or else My_Fract < Ureal_Half
      loop
         My_Fract := My_Fract * Factor;
         My_Exp := My_Exp + Unit;
      end loop;

      --  The remaining step is to truncate the fraction to the appropriate
      --  number of digits, since we have been doing this in full precision.

      Scale := Radix ** Machine_Mantissa (RT);
      My_Fract := UR_From_Uint (UR_Trunc (My_Fract * Scale)) / Scale;

      Fraction := Sign_X * My_Fract;
      Exponent := My_Exp;
      return;

   end Decompose;

   ---------------
   -- Eps_Model --
   ---------------

   function Eps_Model (RT : R) return T is
      Eps_SF  : constant T :=
        Float_Radix ** UI_From_Int (Short_Float_Attr_Machine_Emin);
      Eps_F   : constant T :=
        Float_Radix ** UI_From_Int (Float_Attr_Machine_Emin);
      Eps_LF  : constant T :=
        Float_Radix ** UI_From_Int (Long_Float_Attr_Machine_Emin);
      Eps_LLF : constant T :=
        Float_Radix ** UI_From_Int (Long_Long_Float_Attr_Machine_Emin);

   begin
      if RT = Standard_Short_Float then
         return Eps_SF;

      elsif RT = Standard_Float then
         return Eps_F;

      elsif RT = Standard_Long_Float then
         return Eps_LF;

      else
         pragma Assert (RT = Standard_Long_Long_Float);
         return Eps_LLF;
      end if;
   end Eps_Model;

   ----------------
   -- Eps_Denorm --
   ----------------

   function Eps_Denorm (RT : R) return T is
      Eps_DSF  : constant T :=
        Float_Radix ** UI_From_Int (Short_Float_Attr_Machine_Emin -
                                      Short_Float_Attr_Mantissa);
      Eps_DF   : constant T :=
        Float_Radix ** UI_From_Int (Float_Attr_Machine_Emin -
                                      Float_Attr_Mantissa);
      Eps_DLF  : constant T :=
        Float_Radix ** UI_From_Int (Long_Float_Attr_Machine_Emin -
                                      Long_Float_Attr_Mantissa);
      Eps_DLLF : constant T :=
        Float_Radix ** UI_From_Int (Long_Long_Float_Attr_Machine_Emin -
                                      Long_Long_Float_Attr_Mantissa);
   begin
      if RT = Standard_Short_Float then
         return Eps_DSF;

      elsif RT = Standard_Float then
         return Eps_DF;

      elsif RT = Standard_Long_Float then
         return Eps_DLF;

      else
         pragma Assert (RT = Standard_Long_Long_Float);
         return Eps_DLLF;
      end if;
   end Eps_Denorm;

   --------------
   -- Exponent --
   --------------

   function Exponent (RT : R; X : T) return UI is
      X_Frac : T;
      X_Exp  : UI;

   begin
      Decompose (RT, X, X_Frac, X_Exp);
      return X_Exp;
   end Exponent;

   -----------
   -- Floor --
   -----------

   function Floor (RT : R; X : T) return T is
      XT : constant T := Truncation (RT, X);

   begin
      if UR_Is_Positive (X) then
         return XT;

      elsif XT = X then
         return X;

      else
         return XT - Ureal_1;
      end if;
   end Floor;

   --------------
   -- Fraction --
   --------------

   function Fraction (RT : R; X : T) return T is
      X_Frac : T;
      X_Exp  : UI;

   begin
      Decompose (RT, X, X_Frac, X_Exp);
      return X_Frac;
   end Fraction;

   ------------------
   -- Leading_Part --
   ------------------

   function Leading_Part (RT : R; X : T; Radix_Digits : UI) return T is
      L    : UI;
      Y, Z : T;

   begin
      if Radix_Digits >= Machine_Mantissa (RT) then
         return X;

      else
         L := Exponent (RT, X) - Radix_Digits;
         Y := Truncation (RT, Scaling (RT, X, -L));
         Z := Scaling (RT, Y, L);
         return Z;
      end if;

   end Leading_Part;

   -------------
   -- Machine --
   -------------

   function Machine (RT : R; X : T) return T is
      X_Frac : T;
      X_Exp  : UI;

   begin
      Decompose (RT, X, X_Frac, X_Exp);
      return Compose (RT, X_Frac, X_Exp);
   end Machine;

   ----------------------
   -- Machine_Mantissa --
   ----------------------

   function Machine_Mantissa (RT : R) return UI is
   begin
      if RT = Standard_Short_Float then
         return UI_From_Int (Short_Float_Attr_Machine_Mantissa);

      elsif RT = Standard_Float then
         return UI_From_Int (Float_Attr_Machine_Mantissa);

      elsif RT = Standard_Long_Float then
         return UI_From_Int (Long_Float_Attr_Machine_Mantissa);

      else
         pragma Assert (RT = Standard_Long_Long_Float);
         return UI_From_Int (Long_Long_Float_Attr_Machine_Mantissa);
      end if;
   end Machine_Mantissa;

   -----------
   -- Model --
   -----------

   function Model (RT : R; X : T) return T is
      X_Frac : T;
      X_Exp  : UI;

   begin
      Decompose (RT, X, X_Frac, X_Exp);
      return Compose (RT, X_Frac, X_Exp);
   end Model;

   ----------
   -- Pred --
   ----------

   function Pred (RT : R; X : T) return T is
      X_Frac : T;
      X_Exp  : UI;
      Mach   : T;

   begin
      Decompose (RT, X, X_Frac, X_Exp);
      Mach := Compose (RT, X_Frac, X_Exp);

      --  If number was not a machine number, then just return the machine
      --  number below it (compose always returns the machine number below)

      if X /= Mach then
         return Mach;

      else
         --  Subtract from the given number a number equivalent to the value
         --  of its least significant bit. Given that the most significant bit
         --  represents a value of 1.0 * radix ** (exp - 1), the value we want
         --  is obtained by shifting this by (mantissa-1) bits to the right,
         --  i.e. decreasing the exponent by that amount.

         return X - Scaling (RT, Ureal_1, X_Exp - Machine_Mantissa (RT));
      end if;
   end Pred;

   ------------------------
   -- Radix_To_M_Minus_1 --
   ------------------------

   function Radix_To_M_Minus_1 (RT : R) return T is
   begin
      return Float_Radix ** (Machine_Mantissa (RT) - 1);
   end Radix_To_M_Minus_1;

   ---------------
   -- Remainder --
   ---------------

   function Remainder (RT : R; X, Y : T) return T is
      A        : T;
      B        : T;
      Arg      : T;
      P        : T;
      Arg_Frac : T;
      P_Frac   : T;
      Sign_X   : T;
      IEEE_Rem : T;
      Arg_Exp  : UI;
      P_Exp    : UI;
      K        : UI;
      P_Even   : Boolean;

   begin
      if UR_Is_Positive (X) then
         Sign_X :=  Ureal_1;
      else
         Sign_X := -Ureal_1;
      end if;

      Arg := abs X;
      P   := abs Y;

      if Arg < P then
         P_Even := True;
         IEEE_Rem := Arg;
         P_Exp := Exponent (RT, P);

      else
         Decompose (RT, Arg, Arg_Frac, Arg_Exp);
         Decompose (RT, P,   P_Frac,   P_Exp);

         P := Compose (RT, P_Frac, Arg_Exp);
         K := Arg_Exp - P_Exp;
         P_Even := True;
         IEEE_Rem := Arg;

         for Cnt in reverse 0 .. UI_To_Int (K) loop
            if IEEE_Rem >= P then
               P_Even := False;
               IEEE_Rem := IEEE_Rem - P;
            else
               P_Even := True;
            end if;

            P := P * Ureal_Half;
         end loop;
      end if;

      --  That completes the calculation of modulus remainder. The final step
      --  is get the IEEE remainder. Here we compare Rem with (abs Y) / 2.

      if P_Exp >= 0 then
         A := IEEE_Rem;
         B := abs Y * Ureal_Half;

      else
         A := IEEE_Rem * Ureal_2;
         B := abs Y;
      end if;

      if A > B or else (A = B and then not P_Even) then
         IEEE_Rem := IEEE_Rem - abs Y;
      end if;

      return Sign_X * IEEE_Rem;

   end Remainder;

   --------------
   -- Rounding --
   --------------

   function Rounding (RT : R; X : T) return T is
      Result : T;
      Tail   : T;

   begin
      Result := Truncation (RT, abs X);
      Tail   := abs X - Result;

      if Tail >= Ureal_Half  then
         Result := Result + Ureal_1;
      end if;

      if UR_Is_Negative (X) then
         return -Result;
      else
         return Result;
      end if;

   end Rounding;

   -------------
   -- Scaling --
   -------------

   function Scaling (RT : R; X : T; Adjustment : UI) return T is
   begin
      if Adjustment >= 0 then
         return X * Uint_2 ** Adjustment;
      else
         return X / Uint_2 ** (-Adjustment);
      end if;
   end Scaling;

   ----------
   -- Succ --
   ----------

   function Succ (RT : R; X : T) return T is
      X_Frac : T;
      X_Exp  : UI;

   begin

      if abs X < Eps_Model (RT) then
         return X + Eps_Denorm (RT);

      else
         Decompose (RT, X, X_Frac, X_Exp);

         --  Note: this gives either X or the machine number just below it, so
         --  we want the machine number above this in either case. Computation
         --  of this is similar to that in Pred, except that we add the value
         --  of the least significant bit instead of subtracting it.

         return X + Scaling (RT, Ureal_1, X_Exp - Machine_Mantissa (RT));

      end if;
   end Succ;

   ----------------
   -- Truncation --
   ----------------

   function Truncation (RT : R; X : T) return T is
   begin
      return UR_From_Uint (UR_Trunc (X));
   end Truncation;

   -----------------------
   -- Unbiased_Rounding --
   -----------------------

   function Unbiased_Rounding (RT : R; X : T) return T is
      Abs_X  : constant T := abs X;
      Result : T;
      Tail   : T;

   begin
      Result := Truncation (RT, Abs_X);
      Tail   := Abs_X - Result;

      if Tail > Ureal_Half  then
         Result := Result + Ureal_1;

      elsif Tail = Ureal_Half then
         Result := Ureal_2 *
                     Truncation (RT, (Result / Ureal_2) + Ureal_Half);
      end if;

      if UR_Is_Negative (X) then
         return -Result;
      elsif UR_Is_Positive (X) then
         return Result;

      --  For zero case, make sure sign of zero is preserved

      else
         return X;
      end if;

   end Unbiased_Rounding;

end Eval_Fat;

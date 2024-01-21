------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                ADA.NUMERICS.GENERIC_ELEMENTARY_FUNCTIONS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.18 $                             --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This body is specifically for using an Ada interface to C math.h to get
--  the computation engine. Many special cases are handled locally to avoid
--  unnecessary calls. This is not a "strict" implementation, but takes full
--  advantage of the C functions, e.g. in providing interface to hardware
--  provided versions of the elementary functions.

--  A known weakness is that on the x86, all computation is done in Double,
--  which means that a lot of accuracy is lost for the Long_Long_Float case.

--  Uses functions sqrt, exp, log, pow, sin, asin, cos, acos, tan, atan,
--  sinh, cosh, tanh from C library via math.h

with Ada.Numerics.Aux;

package body Ada.Numerics.Generic_Elementary_Functions is

   Log_Two : constant := 0.69314_71805_59945_30941_72321_21458_17656_80755;
   Half_Log_Two : constant := Log_Two / 2;

   Two_Pi     : constant Float_Type'Base := 2.0 * Pi;
   Half_Pi    : constant Float_Type'Base := Pi / 2.0;
   Fourth_Pi  : constant Float_Type'Base := Pi / 4.0;
   Epsilon    : constant Float_Type'Base := Float_Type'Base'Epsilon;
   IEpsilon   : constant Float_Type'Base := 1.0 / Epsilon;

   subtype Double is Aux.Double;

   DEpsilon    : constant Double := Double (Epsilon);
   DIEpsilon   : constant Double := Double (IEpsilon);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Exact_Remainder
     (X    : Float_Type'Base;
      Y    : Float_Type'Base)
      return Float_Type'Base;
   --  Computes exact remainder of X divided by Y

   function Half_Log_Epsilon return Float_Type'Base;
   --  Function to provide constant: 0.5 * Log (Epsilon)

   function Local_Atan
     (Y    : Float_Type'Base;
      X    : Float_Type'Base := 1.0)
      return Float_Type'Base;
   --  Common code for arc tangent after cyele reduction

   function Log_Inverse_Epsilon return Float_Type'Base;
   --  Function to provide constant: Log (1.0 / Epsilon)

   function Square_Root_Epsilon return Float_Type'Base;
   --  Function to provide constant: Sqrt (Epsilon)

   ----------
   -- "**" --
   ----------

   function "**" (Left, Right : Float_Type'Base) return Float_Type'Base is

   begin
      if Left = 0.0
        and then Right = 0.0
      then
         raise Argument_Error;

      elsif Left < 0.0 then
         raise Argument_Error;

      elsif Right = 0.0 then
         return 1.0;

      elsif Left = 0.0 then
         if Right < 0.0 then
            raise Constraint_Error;
         else
            return 0.0;
         end if;

      elsif Left = 1.0 then
         return 1.0;

      elsif Right = 1.0 then
         return Left;

      else
         begin
            if Right = 2.0 then
               return Left * Left;
            else
               return
                 Float_Type'Base (Aux.pow (Double (Left), Double (Right)));
            end if;

         exception
            when others =>
               raise Constraint_Error;
         end;
      end if;
   end "**";

   ------------
   -- Arccos --
   ------------

   --  Natural cycle

   function Arccos (X : Float_Type'Base) return Float_Type'Base is
      Temp : Float_Type'Base;

   begin
      if abs X > 1.0 then
         raise Argument_Error;

      elsif abs X < Square_Root_Epsilon then
         return Pi / 2.0 - X;

      elsif X = 1.0 then
         return 0.0;

      elsif X = -1.0 then
         return Pi;
      end if;

      Temp := Float_Type'Base (Aux.acos (Double (X)));

      if Temp < 0.0 then
         Temp := Pi + Temp;
      end if;

      return Temp;
   end Arccos;

   --  Arbitrary cycle

   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base is
      Temp : Float_Type'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif abs X > 1.0 then
         raise Argument_Error;

      elsif abs X < Square_Root_Epsilon then
         return Cycle / 4.0;

      elsif X = 1.0 then
         return 0.0;

      elsif X = -1.0 then
         return Cycle / 2.0;
      end if;

      Temp := Arctan (Sqrt (1.0 - X * X) / X, 1.0, Cycle);

      if Temp < 0.0 then
         Temp := Cycle / 2.0 + Temp;
      end if;

      return Temp;
   end Arccos;

   -------------
   -- Arccosh --
   -------------

   function Arccosh (X : Float_Type'Base) return Float_Type'Base is
   begin

      --  Return positive branch of Log (X - Sqrt (X * X - 1.0)), or
      --  the proper approximation for X close to 1 or >> 1.

      if X < 1.0 then
         raise Argument_Error;

      elsif X < 1.0 + Square_Root_Epsilon then
         return Sqrt (2.0 * (X - 1.0));

      elsif  X > 1.0 / Square_Root_Epsilon then
         return Log (X) + Log_Two;

      else
         return Log (X + Sqrt (X * X - 1.0));
      end if;
   end Arccosh;

   ------------
   -- Arccot --
   ------------

   --  Natural cycle

   function Arccot
     (X    : Float_Type'Base;
      Y    : Float_Type'Base := 1.0)
      return Float_Type'Base
   is
   begin
      --  Just reverse arguments

      return Arctan (Y, X);
   end Arccot;

   --  Arbitrary cycle

   function Arccot
     (X     : Float_Type'Base;
      Y     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return  Float_Type'Base
   is
   begin
      --  Just reverse arguments

      return Arctan (Y, X, Cycle);
   end Arccot;

   -------------
   -- Arccoth --
   -------------

   function Arccoth (X : Float_Type'Base) return Float_Type'Base is
   begin

      if abs X > 2.0 then
         return Arctanh (1.0 / X);

      elsif abs X = 1.0 then
         raise Constraint_Error;

      elsif abs X < 1.0 then
         raise Argument_Error;

      else

         --  1.0 < abs X <= 2.0.  One of X + 1.0 and X - 1.0 is exact, the
         --  other has error 0 or Epsilon.

         return 0.5 * (Log (abs (X + 1.0)) - Log (abs (X - 1.0)));
      end if;
   end Arccoth;

   ------------
   -- Arcsin --
   ------------

   --  Natural cycle

   function Arcsin (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X > 1.0 then
         raise Argument_Error;

      elsif abs X < Square_Root_Epsilon then
         return X;

      elsif X = 1.0 then
         return Pi / 2.0;

      elsif X = -1.0 then
         return -Pi / 2.0;
      end if;

      return Float_Type'Base (Aux.asin (Double (X)));
   end Arcsin;

   --  Arbitrary cycle

   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif abs X > 1.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         return X;

      elsif X = 1.0 then
         return Cycle / 4.0;

      elsif X = -1.0 then
         return -Cycle / 4.0;
      end if;

      return Arctan (X / Sqrt (1.0 - X * X), 1.0, Cycle);
   end Arcsin;

   -------------
   -- Arcsinh --
   -------------

   function Arcsinh (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X < Square_Root_Epsilon then
         return X;

      elsif X > 1.0 / Square_Root_Epsilon then
         return Log (X) + Log_Two;

      elsif X < -1.0 / Square_Root_Epsilon then
         return -(Log (-X) + Log_Two);

      elsif X < 0.0 then
         return -Log (abs X + Sqrt (X * X + 1.0));

      else
         return Log (X + Sqrt (X * X + 1.0));
      end if;
   end Arcsinh;

   ------------
   -- Arctan --
   ------------

   --  Natural cycle

   function Arctan
     (Y    : Float_Type'Base;
      X    : Float_Type'Base := 1.0)
      return Float_Type'Base
   is
   begin
      if X = 0.0
        and then Y = 0.0
      then
         raise Argument_Error;

      elsif Y = 0.0 then
         if X > 0.0 then
            return 0.0;
         else -- X < 0.0
            return Pi;
         end if;

      elsif X = 0.0 then
         if Y > 0.0 then
            return Half_Pi;
         else -- Y < 0.0
            return -Half_Pi;
         end if;

      else
         return Local_Atan (Y, X);
      end if;
   end Arctan;

   --  Arbitrary cycle

   function Arctan
     (Y     : Float_Type'Base;
      X     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return  Float_Type'Base
   is
   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif X = 0.0
        and then Y = 0.0
      then
         raise Argument_Error;

      elsif Y = 0.0 then
         if X > 0.0 then
            return 0.0;
         else -- X < 0.0
            return Cycle / 2.0;
         end if;

      elsif X = 0.0 then
         if Y > 0.0 then
            return Cycle / 4.0;
         else -- Y < 0.0
            return -Cycle / 4.0;
         end if;

      else
         return Local_Atan (Y, X) *  Cycle / Two_Pi;
      end if;
   end Arctan;

   -------------
   -- Arctanh --
   -------------

   function Arctanh (X : Float_Type'Base) return Float_Type'Base is
      A, B, D, A_Plus_1, A_From_1 : Float_Type'Base;
      Mantissa : constant Integer := Float_Type'Machine_Mantissa;
   begin

      --  The naive formula:
      --   Arctanh (X) := (1/2) * Log  (1 + X) / (1 - X)
      --   is not well-behaved numerically when X < 0.5 and when X is close
      --   to one. The following is accurate but probably not optimal.

      if abs X = 1.0 then
         raise Constraint_Error;

      elsif abs X >= 1.0 - 2.0 ** (-Mantissa) then

         if abs X >= 1.0 then
            raise Argument_Error;
         else

            --  The one case that overflows if put through the method below:
            --  abs X = 1.0 - Epsilon.  In this case (1/2) log (2/Epsilon) is
            --  accurate. This simplifies to:

            return Float_Type'Base'Copy_Sign (
               Half_Log_Two * Float_Type'Base (Mantissa + 1), X);
         end if;

      --  elsif abs X <= 0.5 then
      else

         --  Use several piecewise linear approximations.
         --  A is close to X, chosen so 1.0 + A, 1.0 - A, and X - A are exact.
         --  The two scalings remove the low-order bits of X.

         A := Float_Type'Base'Scaling (
             Float_Type'Base (Long_Long_Integer
               (Float_Type'Base'Scaling (X, Mantissa - 1))), 1 - Mantissa);

         B := X - A;                --  This is exact; abs B <= 2**(-Mantissa).
         A_Plus_1 := 1.0 + A;       --  This is exact.
         A_From_1 := 1.0 - A;       --  Ditto.
         D := A_Plus_1 * A_From_1;  --  1 - A*A.

         --  use one term of the series expansion:
         --  f (x + e) = f(x) + e * f'(x) + ..

         --  The derivative of Arctanh at A is 1/(1-A*A). Next term is
         --  A*(B/D)**2 (if a quadratic approximation is ever needed).

         return 0.5 * (Log (A_Plus_1) - Log (A_From_1)) + B / D;

         --  else
         --  return 0.5 * Log ((X + 1.0) / (1.0 - X));
      end if;
   end Arctanh;

   ---------
   -- Cos --
   ---------

   --  Natural cycle

   function Cos (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X = 0.0 then
         return 1.0;

      elsif abs X < Square_Root_Epsilon then
         return 1.0;

      end if;

      return Float_Type'Base (Aux.Cos (Double (X)));
   end Cos;

   --  Arbitrary cycle

   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base is
      T : Float_Type'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         return 1.0;
      end if;

      T := Exact_Remainder (abs (X), Cycle) / Cycle;

      if T = 0.25
        or else T = 0.75
        or else T = -0.25
        or else T = -0.75
      then
         return 0.0;

      elsif T = 0.5 or T = -0.5 then
         return -1.0;
      end if;

      return Float_Type'Base (Aux.Cos (Double (T * Two_Pi)));
   end Cos;

   ----------
   -- Cosh --
   ----------

   function Cosh (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X < Square_Root_Epsilon then
         return 1.0;

      elsif abs X > Log_Inverse_Epsilon then
         return Exp ((abs X) - Log_Two);
      end if;

      return Float_Type'Base (Aux.cosh (Double (X)));

   exception
      when others =>
         raise Constraint_Error;
   end Cosh;

   ---------
   -- Cot --
   ---------

   --  Natural cycle

   function Cot (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X = 0.0 then
         raise Constraint_Error;

      elsif abs X < Square_Root_Epsilon then
         return 1.0 / X;
      end if;

      return 1.0 / Float_Type'Base (Aux.tan (Double (X)));
   end Cot;

   --  Arbitrary cycle

   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base is
      T : Float_Type'Base;
   begin

      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         raise Constraint_Error;

      elsif abs X < Square_Root_Epsilon then
         return 1.0 / X;
      end if;

      T := Exact_Remainder (X, Cycle) / Cycle;

      if T = 0.0 or T = 0.5 or T = -0.5 then
         raise Constraint_Error;
      else
         return  Cos (T * Two_Pi) / Sin (T * Two_Pi);
      end if;
   end Cot;

   ----------
   -- Coth --
   ----------

   function Coth (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X = 0.0 then
         raise Constraint_Error;

      elsif X < Half_Log_Epsilon then
         return -1.0;

      elsif X > -Half_Log_Epsilon then
         return 1.0;

      elsif abs X < Square_Root_Epsilon then
         return 1.0 / X;
      end if;

      return 1.0 / Float_Type'Base (Aux.tanh (Double (X)));
   end Coth;

   ---------------------
   -- Exact_Remainder --
   ---------------------

   function Exact_Remainder
     (X    : Float_Type'Base;
      Y    : Float_Type'Base)
      return Float_Type'Base
   is
      Denominator : Float_Type'Base := abs X;
      Divisor     : Float_Type'Base := abs Y;
      Reducer     : Float_Type'Base;
      Sign        : Float_Type'Base := 1.0;

   begin
      if Y = 0.0 then
         raise Constraint_Error;

      elsif X = 0.0 then
         return 0.0;

      elsif X = Y then
         return 0.0;

      elsif Denominator < Divisor then
         return X;
      end if;

      while Denominator >= Divisor loop

         --  Put divisors mantissa with denominators exponent to make reducer

         Reducer := Divisor;

         begin
            while Reducer * 1_048_576.0 < Denominator loop
               Reducer := Reducer * 1_048_576.0;
            end loop;

         exception
            when others => null;
         end;

         begin
            while Reducer * 1_024.0 < Denominator loop
               Reducer := Reducer * 1_024.0;
            end loop;

         exception
            when others => null;
         end;

         begin
            while Reducer * 2.0 < Denominator loop
               Reducer := Reducer * 2.0;
            end loop;

         exception
            when others => null;
         end;

         Denominator := Denominator - Reducer;
      end loop;

      if X < 0.0 then
         return -Denominator;
      else
         return Denominator;
      end if;
   end Exact_Remainder;

   ---------
   -- Exp --
   ---------

   function Exp (X : Float_Type'Base) return Float_Type'Base is
      Result : Float_Type'Base;

   begin
      if X = 0.0 then
         return 1.0;

      else
         Result := Float_Type'Base (Aux.Exp (Double (X)));

         --  The check here catches the case of Exp returning IEEE infinity

         if Result > Float_Type'Base'Last then
            raise Constraint_Error;
         else
            return Result;
         end if;
      end if;
   end Exp;

   ----------------------
   -- Half_Log_Epsilon --
   ----------------------

   --  Cannot precompute this constant, because this is required to be a
   --  pure package, which allows no state. A pity, but no way around it!

   function Half_Log_Epsilon return Float_Type'Base is
   begin
      return 0.5 * Float_Type'Base (Aux.Log (DEpsilon));
   end Half_Log_Epsilon;

   ----------------
   -- Local_Atan --
   ----------------

   function Local_Atan
     (Y    : Float_Type'Base;
      X    : Float_Type'Base := 1.0)
      return Float_Type'Base
   is
      Z        : Float_Type'Base;
      Raw_Atan : Float_Type'Base;

   begin
      if abs Y > abs X then
         Z := abs (X / Y);
      else
         Z := abs (Y / X);
      end if;

      if Z < Square_Root_Epsilon then
         Raw_Atan := Z;

      elsif Z = 1.0 then
         Raw_Atan := Pi / 4.0;

      elsif Z < Square_Root_Epsilon then
         Raw_Atan := Z;

      else
         Raw_Atan := Float_Type'Base (Aux.Atan (Double (Z)));
      end if;

      if abs Y > abs X then
         Raw_Atan := Half_Pi - Raw_Atan;
      end if;

      if X > 0.0 then
         if Y > 0.0 then
            return Raw_Atan;
         else                 --  Y < 0.0
            return -Raw_Atan;
         end if;

      else                    --  X < 0.0
         if Y > 0.0 then
            return Pi - Raw_Atan;
         else                  --  Y < 0.0
            return -(Pi - Raw_Atan);
         end if;
      end if;
   end Local_Atan;

   ---------
   -- Log --
   ---------

   --  Natural base

   function Log (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X < 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         raise Constraint_Error;

      elsif X = 1.0 then
         return 0.0;
      end if;

      return Float_Type'Base (Aux.Log (Double (X)));
   end Log;

   --  Arbitrary base

   function Log (X, Base : Float_Type'Base) return Float_Type'Base is
   begin
      if X < 0.0 then
         raise Argument_Error;

      elsif Base <= 0.0 or else Base = 1.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         raise Constraint_Error;

      elsif X = 1.0 then
         return 0.0;
      end if;

      return Float_Type'Base (Aux.Log (Double (X)) / Aux.Log (Double (Base)));
   end Log;

   -------------------------
   -- Log_Inverse_Epsilon --
   -------------------------

   --  Cannot precompute this constant, because this is required to be a
   --  pure package, which allows no state. A pity, but no way around it!

   function Log_Inverse_Epsilon return Float_Type'Base is
   begin
      return Float_Type'Base (Aux.Log (DIEpsilon));
   end Log_Inverse_Epsilon;

   ---------
   -- Sin --
   ---------

   --  Natural cycle

   function Sin (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X < Square_Root_Epsilon then
         return X;
      end if;

      return Float_Type'Base (Aux.Sin (Double (X)));
   end Sin;

   --  Arbitrary cycle

   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base is
      T : Float_Type'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         return X;
      end if;

      T := Exact_Remainder (X, Cycle) / Cycle;

      if T = 0.0 or T = 0.5 or T = -0.5 then
         return 0.0;

      elsif T = 0.25 or T = -0.75 then
         return 1.0;

      elsif T = -0.25 or T = 0.75 then
         return -1.0;

      end if;

      return  Float_Type'Base (Aux.Sin (Double (T * Two_Pi)));
   end Sin;

   ----------
   -- Sinh --
   ----------

   function Sinh (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X < Square_Root_Epsilon then
         return X;

      elsif  X > Log_Inverse_Epsilon then
         return Exp (X - Log_Two);

      elsif X < -Log_Inverse_Epsilon then
         return -Exp ((-X) - Log_Two);
      end if;

      return Float_Type'Base (Aux.Sinh (Double (X)));

   exception
      when others =>
         raise Constraint_Error;
   end Sinh;

   -------------------------
   -- Square_Root_Epsilon --
   -------------------------

   --  Cannot precompute this constant, because this is required to be a
   --  pure package, which allows no state. A pity, but no way around it!

   function Square_Root_Epsilon return Float_Type'Base is
   begin
      return Float_Type'Base (Aux.Sqrt (DEpsilon));
   end Square_Root_Epsilon;

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X < 0.0 then
         raise Argument_Error;

      --  Special case Sqrt (0.0) to preserve possible minus sign per IEEE

      elsif X = 0.0 then
         return X;

      --  Sqrt (1.0) must be exact for good complex accuracy

      elsif X = 1.0 then
         return 1.0;

      end if;

      return Float_Type'Base (Aux.Sqrt (Double (X)));
   end Sqrt;

   ---------
   -- Tan --
   ---------

   --  Natural cycle

   function Tan (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X < Square_Root_Epsilon then
         return X;

      elsif abs X = Pi / 2.0 then
         raise Constraint_Error;
      end if;

      return Float_Type'Base (Aux.tan (Double (X)));
   end Tan;

   --  Arbitrary cycle

   function Tan (X, Cycle : Float_Type'Base) return Float_Type'Base is
      T : Float_Type'Base;
   begin

      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         return X;
      end if;

      T := Exact_Remainder (X, Cycle) / Cycle;

      if T = 0.25
        or else T = 0.75
        or else T = -0.25
        or else T = -0.75
      then
         raise Constraint_Error;

      else
         return  Sin (T * Two_Pi) / Cos (T * Two_Pi);
      end if;
   end Tan;

   ----------
   -- Tanh --
   ----------

   function Tanh (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X < Half_Log_Epsilon then
         return -1.0;

      elsif X > -Half_Log_Epsilon then
         return 1.0;

      elsif abs X < Square_Root_Epsilon then
         return X;
      end if;

      return Float_Type'Base (Aux.tanh (Double (X)));
   end Tanh;

end Ada.Numerics.Generic_Elementary_Functions;

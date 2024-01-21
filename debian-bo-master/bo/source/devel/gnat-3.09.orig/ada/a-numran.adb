------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                  A D A . N U M E R I C S . R A N D O M                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
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

--  This implementation is derived from LSN 1055 written by Ken Dritz.

with Ada.Calendar;

package body Ada.Numerics.Random is

   ------------------------------
   -- Form of the Image String --
   ------------------------------

   --  The image string is of the form:

   --     nnn,nnn,nnn .... nnn,b

   --  There are Larger_Lag nnn components, where each component is a
   --  decimal integer representing the values of the Lagged_Outputs in
   --  the State_Vector, stored as rounded values * 2**24, in reverse order
   --  (i.e. highest indexed value comes first), b is the borrow (0/1)

   -----------
   -- Image --
   -----------

   function Image (S : State) return String is
      Result        : String (1 .. Max_Image_Width);
      Result_Length : Natural;

      procedure Encode (Value : Float);
      --  Add encoded float value to result string, using the float value
      --  multiplied by 2**24 as a rounded decimal integer string.

      procedure Encode (Value : Float) is
         Img : constant String := Int'Image (Int (2#1.0#E24 * Value));

      begin
         for J in 2 .. Img'Length loop
            Result_Length := Result_Length + 1;
            Result (Result_Length) := Img (J);
         end loop;
      end Encode;

   --  Start processing for Image

   begin
      Result_Length := 0;

      for J in Lag_Range loop
         Encode (S.Lagged_Outputs (S.R - J));
         Result_Length := Result_Length + 1;
         Result (Result_Length) := ',';
      end loop;

      Encode (S.Borrow);
      return Result (1 .. Result_Length);
   end Image;

   ----------------
   -- Make_State --
   ----------------

   function Make_State (Starter : Int := 3E+7) return State is
      Bit_Value      : Float;
      LCG_State      : Float;

      Result : State;

      function LCG_Random return Uniformly_Distributed;
      --  Needs comments???

      function LCG_Random return Uniformly_Distributed is
         LCG_Multiplier : constant := 16_807.0;
         LCG_Modulus    : constant := 2_147_483_647.0;
         T : Float;
         J : Int;

      begin
         T := LCG_State * LCG_Multiplier;
         J := Int (T / LCG_Modulus);
         LCG_State := T - Float (J) * LCG_Modulus;

         if LCG_State < 0.0 then
            LCG_State := LCG_State + LCG_Modulus;
         end if;

         return LCG_State / LCG_Modulus;
      end LCG_Random;

   --  Start of processing for Make_State

   begin
      LCG_State := Float (Starter);

      for J in Lag_Range loop
         Result.Lagged_Outputs (J) := 0.0;
         Bit_Value := 1.0;

         for K in 1 .. 24 loop
            Bit_Value := Bit_Value * 0.5;

            if LCG_Random >= 0.5 then
               Result.Lagged_Outputs (J) :=
                 Result.Lagged_Outputs (J) + Bit_Value;
            end if;
         end loop;
      end loop;

      Result.Borrow := 0.0;
      Result.R      := Lag_Range'Last;
      Result.S      := Smaller_Lag - 1;
      return Result;
   end Make_State;

   ------------
   -- Random --
   ------------

   procedure Random (S : in out State; U : out Uniformly_Distributed) is
      U1 : Uniformly_Distributed'Base;
   begin
      U1 := S.Lagged_Outputs (S.R) - S.Lagged_Outputs (S.S) - S.Borrow;

      if U1 < 0.0 then
         U1 := U1 + 1.0;
         S.Borrow := 2#1.0#e-24;
      else
         S.Borrow := 0.0;
      end if;

      U := U1;
      S.Lagged_Outputs (S.R) := U;
      S.R := S.R - 1;
      S.S := S.S - 1;
   end Random;

   -----------
   -- Reset --
   -----------

   procedure Reset (S : out State; Initiator : in Integer) is
   begin
      S := Make_State (Int (Initiator) mod 2_147_483_646 + 1);
   end Reset;

   procedure Reset (S : out State) is
      use Ada.Calendar;

      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Secs  : Day_Duration;

   begin
      Split (Clock, Year, Month, Day, Secs);
      S := Make_State (((Int (Year)   * 12 +
                         Int (Month)) * 32 +
                         Int (Day))   * 24 * 60 * 60 +
                         Int (Secs));
   end Reset;

   -----------
   -- Value --
   -----------

   function Value (S : String) return State is
      Result : State;
      Ptr    : Natural := S'First;

      function Decode_Component (Max : in Nat) return Float;
      --  Decode next component as a floating-point value, by reading an
      --  integer up to a comma or the end of the string, and converting
      --  it to float by dividing by 2**24. Ptr is the initial location for
      --  the scan, and is advanced past the termninator. Max is the maximum
      --  value of the component as an integer (2**24 - 1 for the lagged
      --  components, and 1 for the borrow).

      function Decode_Component (Max : in Nat) return Float is
         End_Ptr : Natural;
         Int_Val : Nat;

      begin
         --  Not enough components if past end of string

         if Ptr > S'Last then
            raise Constraint_Error;
         end if;

         End_Ptr := Ptr;

         while End_Ptr <= S'Last
           and then S (End_Ptr) /= ','
         loop
            End_Ptr := End_Ptr + 1;
         end loop;

         --  Make sure Length is in reasonable bounds (2**24 < 10**8)

         if End_Ptr = Ptr or else End_Ptr > Ptr + 8 then
            raise Constraint_Error;
         end if;

         Int_Val := Nat'Value (S (Ptr .. End_Ptr - 1));

         if Int_Val > Max then
            raise Constraint_Error;
         end if;

         Ptr := End_Ptr;
         return Float (Int_Val) * 2#1.0#e-24;
      end Decode_Component;

   --  Start of processing for Value

   begin
      for J in reverse Lag_Range loop
         Result.Lagged_Outputs (J) := Decode_Component (2**24 - 1);
      end loop;

      Result.Borrow := Decode_Component (1);

      --  Must be at end of string now!

      if Ptr <= S'Last then
         raise Constraint_Error;
      end if;

      Result.R := Lag_Range'Last;
      Result.S := Smaller_Lag - 1;
      return Result;
   end Value;

end Ada.Numerics.Random;

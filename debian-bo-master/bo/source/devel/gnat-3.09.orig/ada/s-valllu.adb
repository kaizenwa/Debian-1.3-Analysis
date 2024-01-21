------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L _ L L U                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.10 $                             --
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

with System.Unsigned_Types; use System.Unsigned_Types;
with System.Val_Util;       use System.Val_Util;

package body System.Val_LLU is

   -----------------------------
   -- Scan_Long_Long_Unsigned --
   -----------------------------

   function Scan_Long_Long_Unsigned
     (Str  : String;
      Ptr  : access Integer;
      Max  : Integer)
      return Long_Long_Unsigned
   is
      P : Integer;
      --  Local copy of the pointer

      Uval : Long_Long_Unsigned;
      --  Accumulated unsigned integer result (in the loop to scan out based
      --  numbers, this is the value of the base, scanned on entry)

      Bval : Long_Long_Unsigned;
      --  Value of based number accumulated

      New_Val : Long_Long_Unsigned;
      --  Used in checking overflow during accumulation of result

      Expon : Integer;
      --  Exponent value

      Minus : Boolean := False;
      --  Set to True if minus sign is present, otherwise to False. Note that
      --  a minus sign is permissible for the singular case of -0, and in any
      --  case the pointer is left pointing past a negative integer literal.

      Overflow : Boolean := False;
      --  Set True if overflow is detected at any point

      Start : Positive;
      --  Save location of first non-blank character, not used for this case

      Base_Char : Character;
      --  Base character (# or :) in based case

      Base : Long_Long_Unsigned := 10;
      --  Base value (reset in based case)

      Digit : Long_Long_Unsigned;
      --  Digit value (0..15) in based case

   begin
      Scan_Sign (Str, Ptr, Max, Minus, Start);

      if Str (Ptr.all) not in '0' .. '9' then
         Ptr.all := Start;
         raise Constraint_Error;
      end if;

      P := Ptr.all;
      Uval := Character'Pos (Str (P)) - Character'Pos ('0');
      P := P + 1;

      --  Loop to scan out digits of what is either the number or the base

      loop
         exit when P > Max;

         --  Non-digit encountered

         if Str (P) not in '0' .. '9' then
            if Str (P) = '_' then
               Scan_Underscore (Str, P, Ptr, Max, False);
            else
               exit;
            end if;

         --  Accumulate result unless we have overflow. Overflow is detected
         --  by the wrap around, which results in the a smaller value.

         else
            New_Val :=
              10 * Uval + Character'Pos (Str (P)) - Character'Pos ('0');

            if New_Val < Uval then
               Overflow := True;
            else
               Uval := New_Val;
            end if;

            P := P + 1;
         end if;
      end loop;

      Ptr.all := P;

      --  Deal with based case

      if P < Max and then (Str (P) = ':' or else Str (P) = '#') then
         Base_Char := Str (P);
         P := P + 1;
         Bval := 0;

         --  Check base value. Overflow is set True if we find a bad base, or
         --  a digit that is out of range of the base. That way, we scan out
         --  the numeral that is still syntactically correct, though illegal.

         if Uval not in 2 .. 16 then
            Overflow := True;
         end if;

         --  Loop to scan out based integer value

         loop
            --  We require a digit at this stage. If we don't have one, then
            --  it isn't a based number after all, so the number we scanned
            --  out as the base (still in Uval) is the value we wnat.

            if Str (P) in '0' .. '9' then
               Digit := Character'Pos (Str (P)) - Character'Pos ('0');

            elsif Str (P) in 'A' .. 'F' then
               Digit := Character'Pos (Str (P)) - (Character'Pos ('A') - 10);

            elsif Str (P) in 'a' .. 'f' then
               Digit := Character'Pos (Str (P)) - (Character'Pos ('a') - 10);
            else
               exit;
            end if;

            --  Here we accumulate the value, checking overflow (which
            --  is detected by wrap around leaving the result smaller)

            if Digit >= Uval then
               Overflow := True;
            else
               New_Val := Bval * Uval + Digit;

               if New_Val < Bval then
                  Overflow := True;
               else
                  Bval := New_Val;
               end if;
            end if;

            --  If at end of string with no base char, not a based number
            --  but we signal Constraint_Error and set the pointer past
            --  the end of the field, since this is what the ACVC tests
            --  seem to require, see CE3704N, line 204.

            P := P + 1;

            if P > Max then
               Ptr.all := P;
               raise Constraint_Error;
            end if;

            --  If terminating base character, we are done with loop

            if Str (P) = Base_Char then
               Ptr.all := P + 1;
               Base := Uval;
               Uval := Bval;
               exit;

            --  Deal with underscore

            elsif Str (P) = '_' then
               Scan_Underscore (Str, P, Ptr, Max, True);
            end if;

         end loop;
      end if;

      --  Come here with scanned unsigned value in Uval. The only remaining
      --  required step is to deal with exponent if one is present.

      Expon := Scan_Exponent (Str, Ptr, Max);

      if Expon /= 0 and then Uval /= 0 then

         --  For non-zero value, scale by exponent value. No need to do this
         --  efficiently, since use of exponent in integer literals is rare,
         --  and in any case the exponent cannot be very large.

         loop
            New_Val := Uval * Base;

            if New_Val < Uval then
               Overflow := True;
            else
               Uval := New_Val;
            end if;

            Expon := Expon - 1;
            exit when Expon = 0;
         end loop;
      end if;

      --  Return result, dealing with sign and overflow

      if Overflow or else (Minus and then Uval /= 0) then
         raise Constraint_Error;
      else
         return Uval;
      end if;

   end Scan_Long_Long_Unsigned;

   ------------------------------
   -- Value_Long_Long_Unsigned --
   ------------------------------

   function Value_Long_Long_Unsigned
     (Str : String)
     return Long_Long_Unsigned
   is
      V : Long_Long_Unsigned;
      P : aliased Integer := Str'First;

   begin
      V := Scan_Long_Long_Unsigned (Str, P'Access, Str'Last);
      Scan_Trailing_Blanks (Str, P);
      return V;

   end Value_Long_Long_Unsigned;

end System.Val_LLU;

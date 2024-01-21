------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . W C H _ S T W                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.11 $                             --
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

with System.WCh_Con; use System.WCh_Con;
with System.WCh_JIS; use System.WCh_JIS;

package body System.WCh_StW is

   ---------------------------
   -- String_To_Wide_String --
   ---------------------------

   function String_To_Wide_String
     (S    : String;
      EM   : WC_Encoding_Method)
      return Wide_String
   is
      R  : Wide_String (1 .. S'Length);
      RP : Natural;
      SP : Natural;

      Use_Hex : constant Boolean := (EM = WCEM_Hex or else EM = WCEM_None);

      function Get_Hex (C : Character) return Natural;
      --  Converts character from hex digit to value in range 0-15. The
      --  input must be in 0-9, A-F, or a-f, and no check is needed.

      function Get_Hex (C : Character) return Natural is
      begin
         if C in '0' .. '9' then
            return Character'Pos (C) - Character'Pos ('0');
         elsif C in 'A' .. 'F' then
            return Character'Pos (C) - Character'Pos ('A') + 10;
         else
            return Character'Pos (C) - Character'Pos ('a') + 10;
         end if;
      end Get_Hex;

   --  Start of processing for String_To_Wide_String

   begin
      SP := S'First;
      RP := 0;

      while SP <= S'Last loop
         RP := RP + 1;

         if (S (SP) = Ascii.ESC and then Use_Hex) then
            R (RP) := Wide_Character'Val (
               Get_Hex (S (SP + 4)) + 16 *
                 (Get_Hex (S (SP + 3)) + 16 *
                   (Get_Hex (S (SP + 2)) + 16 *
                     (Get_Hex (S (SP + 1))))));
            SP := SP + 5;

         --  Brackets representation, also allowed in "none" mode

         elsif (EM = WCEM_Brackets or else EM = WCEM_None)
           and then SP + 3 < S'Last
           and then S (SP) = '['
           and then S (SP + 1) = '"'
           and then S (SP + 2) /= '"'
         then
            declare
               Val : Integer;

            begin
               Val := Get_Hex (S (SP + 3)) + 16 * Get_Hex (S (SP + 2));

               if S (SP + 4) = '"' then
                  SP := SP + 6;

               else
                  Val :=
                    Val * 256 +
                    Get_Hex (S (SP + 5)) + 16 * Get_Hex (S (SP + 4));

                  SP := SP + 8;
               end if;

               R (RP) := Wide_Character'Val (Val);
            end;

         --  One-byte ASCII character

         elsif S (SP) <= Ascii.DEL or else Use_Hex then
            R (RP) := Wide_Character'Val (Character'Pos (S (SP)));
            SP := SP + 1;

            --  Upper bit shift, internal code = external code

         elsif EM = WCEM_Upper then
            R (RP) := Wide_Character'Val (
                        Character'Pos (S (SP)) * 256 +
                        Character'Pos (S (SP + 1)));
            SP := SP + 2;

         --  Upper bit shift, EUC

         elsif EM = WCEM_EUC then
            R (RP) := EUC_To_JIS (S (SP), S (SP + 1));
            SP := SP + 2;

         --  Upper bit shift, shift-JIS

         elsif EM = WCEM_Shift_JIS then
            R (RP) := Shift_JIS_To_JIS (S (SP), S (SP + 1));
            SP := SP + 2;

         --  This should be impossible

         else
            raise Program_Error;

         end if;
      end loop;

      return R (1 .. RP);
   end String_To_Wide_String;

end System.WCh_StW;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . W C H _ W T S                        --
--                                                                          --
--                                 B o d y                                  --
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

with System.WCh_Con; use System.WCh_Con;
with System.WCh_JIS; use System.WCh_JIS;

package body System.WCh_WtS is

   ---------------------------
   -- Wide_String_To_String --
   ---------------------------

   function Wide_String_To_String
     (S    : Wide_String;
      EM   : WC_Encoding_Method)
      return String
   is
      R  : String (1 .. 5 * S'Length); -- worst case length!
      RP : Natural;
      B1 : Natural;
      B2 : Natural;
      C1 : Character;
      C2 : Character;

   begin
      RP := 0;

      for SP in S'Range loop
         declare
            C   : constant Wide_Character := S (SP);
            CV  : constant Natural        := Wide_Character'Pos (C);
            Hex : constant array (0 .. 15) of Character := "0123456789ABCDEF";

         begin
            if CV <= 127
              or else (CV <= 255 and then EM = WCEM_Hex)
            then
               RP := RP + 1;
               R (RP) := Character'Val (CV);

            else
               B1 := CV / 256;
               B2 := CV mod 256;

               --  Hex ESC sequence encoding

               if EM = WCEM_Hex then
                  R (RP + 1) := Ascii.ESC;
                  R (RP + 2) := Hex (B1 / 16);
                  R (RP + 3) := Hex (B1 rem 16);
                  R (RP + 4) := Hex (B2 / 16);
                  R (RP + 5) := Hex (B2 rem 16);
                  RP := RP + 5;

               --  Upper bit shift (internal code = external code)

               elsif EM = WCEM_Upper then
                  R (RP + 1) := Character'Val (B1);
                  R (RP + 2) := Character'Val (B2);
                  RP := RP + 2;

               --  Upper bit shift (EUC)

               elsif EM = WCEM_EUC then
                  JIS_To_EUC (C, C1, C2);
                  R (RP + 1) := C1;
                  R (RP + 2) := C2;
                  RP := RP + 2;

               --  Upper bit shift (Shift-JIS)

               elsif EM = WCEM_Shift_JIS then
                  JIS_To_Shift_JIS (C, C1, C2);
                  R (RP + 1) := C1;
                  R (RP + 2) := C2;
                  RP := RP + 2;

               --  Brackets encoding (also used in default no encoding mode)

               else -- EM = WCEM_Brackets or else EM = WCEM_None
                  R (RP + 1) := '[';
                  R (RP + 2) := '"';

                  if B1 = 0 then
                     R (RP + 3) := Hex (B2 / 16);
                     R (RP + 4) := Hex (B2 rem 16);
                     R (RP + 5) := '"';
                     R (RP + 6) := ']';
                     RP := RP + 6;

                  else
                     R (RP + 3) := Hex (B1 / 16);
                     R (RP + 4) := Hex (B1 rem 16);
                     R (RP + 5) := Hex (B2 / 16);
                     R (RP + 6) := Hex (B2 rem 16);
                     R (RP + 7) := '"';
                     R (RP + 8) := ']';
                     RP := RP + 8;
                  end if;
               end if;
            end if;
         end;
      end loop;

      return R (1 .. RP);
   end Wide_String_To_String;

end System.WCh_WtS;

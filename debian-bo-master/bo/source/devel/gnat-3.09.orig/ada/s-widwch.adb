------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     S Y S T E M . W I D _ W C H A R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
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

package body System.Wid_WChar is

   --------------------------
   -- Width_Wide_Character --
   --------------------------

   function Width_Wide_Character
     (Lo, Hi : Wide_Character;
      EM     : WC_Encoding_Method)
      return   Natural
   is
      W : Natural;
      P : Natural;

   begin
      W := 0;

      for C in Lo .. Hi loop
         P := Wide_Character'Pos (C);

         --  If we are in wide character range, just use width of encoding
         --  sequence (currently a constant for all encoding methods) and
         --  we are done.

         if P > 255 then
            if EM = WCEM_Hex or else EM = WCEM_None then
               return Natural'Max (W, 5);

            elsif EM = WCEM_Brackets then
               return Natural'Max (W, 8);

            else
               return Natural'Max (W, 2);
            end if;

         --  If we are in character range then use length of character image

         else
            declare
               S : String := Character'Image (Character'Val (P));

            begin
               W := Natural'Max (W, S'Length);
            end;
         end if;
      end loop;

      return W;
   end Width_Wide_Character;

end System.Wid_WChar;

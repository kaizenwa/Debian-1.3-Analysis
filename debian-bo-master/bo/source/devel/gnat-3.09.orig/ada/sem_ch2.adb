------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 2                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
--                                                                          --
--        Copyright (C) 1992,1993,1994 Free Software Foundation, Inc.       --
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

with Atree; use Atree;
with Opt;   use Opt;
with Sinfo; use Sinfo;
with Stand; use Stand;

package body Sem_Ch2 is

   -------------------------------
   -- Analyze_Character_Literal --
   -------------------------------

   procedure Analyze_Character_Literal (N : Node_Id) is
   begin
      Set_Etype (N, Any_Character);
      Set_Is_Static_Expression (N);
   end Analyze_Character_Literal;

   -----------------------------
   -- Analyze_Integer_Literal --
   -----------------------------

   procedure Analyze_Integer_Literal (N : Node_Id) is
   begin
      Set_Etype (N, Universal_Integer);
      Set_Is_Static_Expression (N);
   end Analyze_Integer_Literal;

   --------------------------
   -- Analyze_Real_Literal --
   --------------------------

   procedure Analyze_Real_Literal (N : Node_Id) is
   begin
      Set_Etype (N, Universal_Real);
      Set_Is_Static_Expression (N);
   end Analyze_Real_Literal;

   ----------------------------
   -- Analyze_String_Literal --
   ----------------------------

   procedure Analyze_String_Literal (N : Node_Id) is
   begin

      --  The type is eventually inherited from the context. If expansion
      --  has already established the proper type, do not modify it.

      if No (Etype (N)) then
         Set_Etype (N, Any_String);
      end if;

      --  String literals are static in Ada 95. Note that if the subtype
      --  turns out to be non-static, then the Is_Static_Expression flag
      --  will be reset in Eval_String_Literal.

      if Ada_95 then
         Set_Is_Static_Expression (N);
      end if;
   end Analyze_String_Literal;

end Sem_Ch2;

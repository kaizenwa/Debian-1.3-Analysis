------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . T A S K _ C L O C K                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.13 $                             --
--                                                                          --
--      Copyright (C) 1991,1992,1993,1994,1995 Florida State University     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces; use Interfaces;

package body System.Task_Clock is

   -----------------------
   -- Stimespec_Seconds --
   -----------------------

   function Stimespec_Seconds (TV : Stimespec) return Integer is
      Tmp : Stimespec := TV / Stimespec_Sec_Unit;
   begin
      return  Integer (Tmp);
   end Stimespec_Seconds;

   ------------------------
   -- Stimespec_Nseconds --
   -------------------------

   function Stimespec_NSeconds (TV : Stimespec) return Integer is
   begin
      return
        Integer (TV - Stimespec (Stimespec_Seconds (TV)) *
          Stimespec_Sec_Unit);
   end Stimespec_NSeconds;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (S, NS : Integer) return Stimespec is
   begin
      return  Stimespec_Sec_Unit * Stimespec (S) + Stimespec (NS);
   end Time_Of;

   ---------------------------
   -- Stimespec_To_Duration --
   ---------------------------

   function Stimespec_To_Duration (TV : Stimespec) return Duration is
   begin
      return
        Duration (long_long_float (TV) / long_long_float (Stimespec_Sec_Unit));
   end Stimespec_To_Duration;

   ---------------------------
   -- Duration_To_Stimespec --
   ---------------------------

   function Duration_To_Stimespec (Time : Duration) return Stimespec is
   begin
      return Stimespec
        (long_long_float (Time) * long_long_float (Stimespec_Sec_Unit));
   end Duration_To_Stimespec;

   ---------
   -- "-" --
   ---------

   --  Unary minus
   function "-" (TV : Stimespec) return Stimespec is
   begin
      return Stimespec (-(Integer_64 (TV)));
   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (LTV, RTV : Stimespec) return Stimespec is
   begin
      return Stimespec (Integer_64 (LTV) + Integer_64 (RTV));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (LTV, RTV : Stimespec) return Stimespec is
   begin
      return Stimespec (Integer_64 (LTV) - Integer_64 (RTV));
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (TV : Stimespec; N : Integer) return Stimespec is
   begin
      return TV * Stimespec (N);
   end "*";

   ---------
   -- "/" --
   ---------

   --  Integer division of Stimespec

   function "/" (TV : Stimespec; N : Integer) return Stimespec is
   begin
      return TV / Stimespec (N);
   end "/";

   ---------
   -- "/" --
   ---------

   function "/" (LTV, RTV : Stimespec) return Integer is
      Tmp : Stimespec := LTV / RTV;
   begin
      return Integer (Tmp);
   end "/";

   ---------
   -- "<" --
   ---------

   function "<" (LTV, RTV : Stimespec) return Boolean is
   begin
      return Integer_64 (LTV) < Integer_64 (RTV);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (LTV, RTV : Stimespec) return Boolean is
   begin
      return LTV < RTV or else RTV = LTV;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (LTV, RTV : Stimespec) return Boolean is
   begin
      return Integer_64 (RTV) < Integer_64 (LTV);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (LTV, RTV : Stimespec) return Boolean is
   begin
      return LTV > RTV or LTV = RTV;
   end ">=";

end System.Task_Clock;

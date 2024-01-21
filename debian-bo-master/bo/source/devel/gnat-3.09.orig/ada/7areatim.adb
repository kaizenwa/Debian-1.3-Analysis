------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                          A D A . R E A L _ T I M E                       --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.21 $                            --
--                                                                          --
--   Copyright (C) 1991,1992,1993,1994,1995,1996 Florida State University   --
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

with System.Time_Operations;
--  used for Clock

package body Ada.Real_Time is

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Time (System.Time_Operations.Clock);
   end Clock;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) < Duration (Right);
   end "<";

   function "<" (Left, Right : Time_Span) return Boolean is
   begin
      return Duration (Left) < Duration (Right);
   end "<";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) > Duration (Right);
   end ">";

   function ">" (Left, Right : Time_Span) return Boolean is
   begin
      return Duration (Left) > Duration (Right);
   end ">";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) <= Duration (Right);
   end "<=";

   function "<=" (Left, Right : Time_Span) return Boolean is
   begin
      return Duration (Left) <= Duration (Right);
   end "<=";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) >= Duration (Right);
   end ">=";

   function ">=" (Left, Right : Time_Span) return Boolean is
   begin
      return Duration (Left) >= Duration (Right);
   end ">=";

   ---------
   -- "+" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "+" (Left : Time; Right : Time_Span) return Time is
   begin
      return Time (Duration (Left) + Duration (Right));
   end "+";

   function "+"  (Left : Time_Span; Right : Time) return Time is
   begin
      return Time (Duration (Left) + Duration (Right));
   end "+";

   function "+"  (Left, Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Duration (Left) + Duration (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "-"  (Left : Time; Right : Time_Span) return Time is
   begin
      return Time (Duration (Left) - Duration (Right));
   end "-";

   function "-"  (Left, Right : Time) return Time_Span is
   begin
      return Time_Span (Duration (Left) - Duration (Right));
   end "-";

   function "-"  (Left, Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Duration (Left) - Duration (Right));
   end "-";

   function "-"  (Right : Time_Span) return Time_Span is
   begin
      return Time_Span_Zero - Right;
   end "-";

   ---------
   -- "/" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "/"  (Left, Right : Time_Span) return integer is
   begin
      return integer (Duration (Left) / Duration (Right));
   end "/";

   function "/"  (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Time_Span (Duration (Left) / Right);
   end "/";

   ---------
   -- "*" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "*"  (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Time_Span (Duration (Left) * (Right));
   end "*";

   function "*"  (Left : Integer; Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Left * Duration (Right));
   end "*";

   -----------
   -- "abs" --
   -----------

   --  Note that Constraint_Error may be propagated

   function "abs" (Right : Time_Span) return Time_Span is
   begin
      if Right < Time_Span_Zero then
         return -Right;
      end if;

      return Right;
   end "abs";

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : Time_Span) return Duration is
   begin
      return Duration (TS);
   end To_Duration;

   ------------------
   -- To_Time_Span --
   ------------------

   function To_Time_Span (D : Duration) return Time_Span is
   begin
      return Time_Span (D);
   end To_Time_Span;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (NS : integer) return Time_Span is
   begin
      return Time_Span_Unit * NS;
   end Nanoseconds;

   ------------------
   -- Microseconds --
   ------------------

   function Microseconds  (US : integer) return Time_Span is
   begin
      return Nanoseconds (US) * 1000;
   end Microseconds;

   -------------------
   --  Milliseconds --
   -------------------

   function Milliseconds (MS : integer) return Time_Span is
   begin
      return Microseconds (MS) * 1000;
   end Milliseconds;

   -----------
   -- Split --
   -----------

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span) is
   begin
      SC := Seconds_Count (T);
      TS := T - Time (SC);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time is
   begin
      return Time (SC) + TS;
   end Time_Of;

end Ada.Real_Time;

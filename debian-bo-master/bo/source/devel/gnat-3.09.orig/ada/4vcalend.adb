------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 B o d y                                  --
--                         (Version for new GNARL)                          --
--                                                                          --
--                            $Revision: 1.4 $                              --
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

--  This is the Alpha/VMS version.

with System.Time_Operations;
--  used for Clock

with Unchecked_Conversion;

with Interfaces.C; use Interfaces.C;
--  We need to get int and long from Interfaces.C because they may not
--  match Integer and Long_Integer on all systems (e.g. VMS).

with System.Aux_DEC; use System.Aux_DEC;

with Ada.Calendar.Conv; use Ada.Calendar.Conv;

package body Ada.Calendar is

   ------------------------------
   -- Use of Pragma Unsuppress --
   ------------------------------

   --  This implementation of Calendar takes advantage of the permission in
   --  Ada 95 of using arithmetic overflow checks to check for out of bounds
   --  time values. This means that we must catch the constraint error that
   --  results from arithmetic overflow, so we use pragma Unsuppress to make
   --  sure that overflow is enabled, using software overflow checking if
   --  necessary. That way, compiling Calendar with options to suppress this
   --  checking will not affect its correctness.

   ------------------------
   -- Local Declarations --
   ------------------------

   Ada_Year_Min : constant := 1901;
   Ada_Year_Max : constant := 2099;

   --  Some basic constants used throughout

   Days_In_Month : constant array (Month_Number) of Day_Number :=
                     (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return (Left + To_Relative_Time (Right));

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   function "+" (Left : Duration; Right : Time) return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return (To_Relative_Time (Left) + Right);

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Duration)  return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return Left - To_Relative_Time (Right);

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   function "-" (Left : Time; Right : Time) return Duration is
      pragma Unsuppress (Overflow_Check);
   begin
      return To_Relative_Duration (Left - Right);

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time) return Boolean is
   begin
      return Long_Integer (Left) < Long_Integer (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time) return Boolean is
   begin
      return Long_Integer (Left) <= Long_Integer (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time) return Boolean is
   begin
      return Long_Integer (Left) > Long_Integer (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time) return Boolean is
   begin
      return Long_Integer (Left) >= Long_Integer (Right);
   end ">=";

   -----------
   -- Clock --
   -----------

   --  The Ada.Calendar.Clock function gets the time from the GNULLI
   --  interface routines. This ensures that Calendar is properly
   --  coordinated with the tasking runtime. Any system dependence
   --  involved in reading the clock is then hidden in the GNULLI
   --  implementation layer (in the body of System.Task_Clock).

   function Clock return Time is
   begin
      return To_Absolute_Time (System.Time_Operations.Clock);
   end Clock;

   ---------
   -- Day --
   ---------

   function Day (Date : Time) return Day_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;

   begin
      Split (Date, DY, DM, DD, DS);
      return DD;
   end Day;

   -----------
   -- Month --
   -----------

   function Month (Date : Time) return Month_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;

   begin
      Split (Date, DY, DM, DD, DS);
      return DM;
   end Month;

   -------------
   -- Seconds --
   -------------

   function Seconds (Date : Time) return Day_Duration is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;

   begin
      Split (Date, DY, DM, DD, DS);
      return DS;
   end Seconds;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration)
   is
      procedure Numtim (
        Status : out Unsigned_Longword;
        Timbuf : out Unsigned_Word_Array;
        Timadr : in  Time);

      pragma Interface (External, Numtim);

      pragma Import_Valued_Procedure (Numtim, "SYS$NUMTIM",
        (Unsigned_Longword, Unsigned_Word_Array, Time),
        (Value, Reference, Reference));

      Status   : Unsigned_Longword;
      Timbuf   : Unsigned_Word_Array (1 .. 7);

   begin
      Numtim (Status, Timbuf, Date);

      if Status mod 2 /= 1
        or else Timbuf (1) not in Ada_Year_Min .. Ada_Year_Max
      then
         raise Time_Error;
      end if;

      Seconds
        := Day_Duration (Timbuf (6) + 60 * (Timbuf (5) + 60 * Timbuf (4)))
           + Day_Duration (Timbuf (7)) / 100.0;
      Day       := Integer (Timbuf (3));
      Month     := Integer (Timbuf (2));
      Year      := Integer (Timbuf (1));
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0)
      return    Time
   is

      procedure Cvt_Vectim (
        Status         : out Unsigned_Longword;
        Input_Time     : in  Unsigned_Word_Array;
        Resultant_Time : out Time);

      pragma Interface (External, Cvt_Vectim);

      pragma Import_Valued_Procedure (Cvt_Vectim, "lib$cvt_vectim",
        (Unsigned_Longword, Unsigned_Word_Array, Time),
        (Value, Reference, Reference));

      Status      : Unsigned_Longword;
      Timbuf      : Unsigned_Word_Array (1 .. 7);
      Date        : Time;
      Int_Secs    : Integer;
   begin
      --  The following checks are redundant with respect to the constraint
      --  error checks that should normally be made on parameters, but we
      --  decide to raise Constraint_Error in any case if bad values come
      --  in (as a result of checks being off in the caller, or for other
      --  erroneous or bounded error cases).

      if        not Year   'Valid
        or else not Month  'Valid
        or else not Day    'Valid
        or else not Seconds'Valid
      then
         raise Constraint_Error;
      end if;

      --  Truncate seconds value, but be careful with 0.0 since that will
      --  give -1.0 unless it is treated specially.

      if Seconds > 0.0 then
         Int_Secs := Integer (Seconds - 0.5);
      else
         Int_Secs := Integer (Seconds);
      end if;

      Timbuf (1) := Unsigned_Word (Year);
      Timbuf (2) := Unsigned_Word (Month);
      Timbuf (3) := Unsigned_Word (Day);
      Timbuf (4) := Unsigned_Word (Int_Secs / 3600);
      Timbuf (5) := Unsigned_Word ((Int_Secs / 60) mod 60);
      Timbuf (6) := Unsigned_Word (Int_Secs mod 60);
      Timbuf (7) := Unsigned_Word
        (100.0 * Duration (Seconds - Day_Duration (Int_Secs)));

      Cvt_Vectim (Status, Timbuf, Date);

      if Status mod 2 /= 1 then
         raise Time_Error;
      end if;

      return Date;

   end Time_Of;

   ----------
   -- Year --
   ----------

   function Year (Date : Time) return Year_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;

   begin
      Split (Date, DY, DM, DD, DS);
      return DY;
   end Year;

end Ada.Calendar;

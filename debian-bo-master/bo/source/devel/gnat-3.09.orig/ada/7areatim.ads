------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUNTIME COMPONENTS                         --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                  S p e c                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.20 $                            --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------


package Ada.Real_Time is

   type Time is private;
   Time_First : constant Time;
   Time_Last  : constant Time;
   Time_Unit  : constant := 10#1.0#E-9;

   type Time_Span is private;
   Time_Span_First : constant Time_Span;
   Time_Span_Last  :  constant Time_Span;
   Time_Span_Zero  :  constant Time_Span;
   Time_Span_Unit  :  constant Time_Span;

   Tick : constant Time_Span;
   function Clock return Time;

   function "+"  (Left : Time;      Right : Time_Span) return Time;
   function "+"  (Left : Time_Span; Right : Time)      return Time;
   function "-"  (Left : Time;      Right : Time_Span) return Time;
   function "-"  (Left : Time;      Right : Time)      return Time_Span;

   function "<"  (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">"  (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   function "+"  (Left, Right : Time_Span)             return Time_Span;
   function "-"  (Left, Right : Time_Span)             return Time_Span;
   function "-"  (Right : Time_Span)                   return Time_Span;
   function "*"  (Left : Time_Span; Right : Integer)   return Time_Span;
   function "*"  (Left : Integer;   Right : Time_Span) return Time_Span;
   function "/"  (Left, Right : Time_Span)             return Integer;
   function "/"  (Left : Time_Span; Right : Integer)   return Time_Span;

   function "abs" (Right : Time_Span) return Time_Span;

   function "<"  (Left, Right : Time_Span) return Boolean;
   function "<=" (Left, Right : Time_Span) return Boolean;
   function ">"  (Left, Right : Time_Span) return Boolean;
   function ">=" (Left, Right : Time_Span) return Boolean;

   function To_Duration  (TS : Time_Span) return Duration;
   function To_Time_Span (D : Duration)   return Time_Span;

   function Nanoseconds  (NS : integer) return Time_Span;
   function Microseconds (US : integer) return Time_Span;
   function Milliseconds (MS : integer) return Time_Span;

   type Seconds_Count is new integer range -integer'Last .. integer'Last;

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span);
   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time;

private
   type Time is new Duration;

   Time_First : constant Time := Time'First;

   Time_Last :  constant Time := Time'Last;

   type Time_Span is new Duration;

   Time_Span_First :  constant Time_Span := Time_Span'First;

   Time_Span_Last  :  constant Time_Span := Time_Span'Last;

   Time_Span_Zero  :  constant Time_Span := 0.0;

   Time_Span_Unit  :  constant Time_Span := 10#1.0#E-9;

   Tick            :  constant Time_Span := 10#1.0#E-6;

   --  Time and Time_Span are represented in 64-bit Duration.
   --  time in nanoseconds. For example, 1 second and 1 nanosecond is
   --  represented as "1.000000001"

end Ada.Real_Time;

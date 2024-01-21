------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . T A S K _ C L O C K                    --
--                                                                          --
--                                  S p e c                                 --
--                           (Compiler Interface)                           --
--                                                                          --
--                             $Revision: 1.12 $                             --
--                                                                          --
--    Copyright (C) 1991, 92, 93, 94, 1995 Free Software Foundation, Inc.   --
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

--  This package interfaces with the Ada RTS and defines the low-level
--  timer operations.

--  Note: this package is not a direct part of the compiler interface, but
--  it is used by the System.Task_Timer spec, which is part of the interface.

with Interfaces;

package System.Task_Clock is

   type Stimespec is private;

   Stimespec_First : constant Stimespec;

   Stimespec_Last  : constant Stimespec;

   Stimespec_Zero  : constant Stimespec;

   Stimespec_Unit  : constant Stimespec;

   Stimespec_Sec_Unit : constant Stimespec;

   function Stimespec_Seconds (TV : Stimespec) return Integer;

   function Stimespec_NSeconds (TV : Stimespec) return Integer;

   function Time_Of (S, NS : Integer) return Stimespec;

   function Stimespec_To_Duration (TV : Stimespec) return Duration;

   function Duration_To_Stimespec (Time : Duration) return Stimespec;

   function "-"  (TV : Stimespec) return Stimespec;

   function "+"  (LTV, RTV : Stimespec) return Stimespec;

   function "-"  (LTV, RTV : Stimespec) return Stimespec;

   function "*"  (TV : Stimespec; N : Integer) return Stimespec;

   function "/"  (TV : Stimespec; N : integer) return Stimespec;

   function "/"  (LTV, RTV : Stimespec) return Integer;

   function "<"  (LTV, RTV : Stimespec) return Boolean;

   function "<=" (LTV, RTV : Stimespec) return Boolean;

   function ">"  (LTV, RTV : Stimespec) return Boolean;

   function ">=" (LTV, RTV : Stimespec) return Boolean;

private

   --  Stimespec is represented in 64-bit Integer. It represents time in
   --  nanoseconds. For example, 1 second and 1 nanosecond is represented
   --  as "1_000_000_001"

   type Stimespec is new Interfaces.Integer_64;

   Stimespec_First    : constant Stimespec := Stimespec'First;

   Stimespec_Last     : constant Stimespec := Stimespec'Last;

   Stimespec_Zero     : constant Stimespec := 0;

   Stimespec_Unit     : constant Stimespec := 1;

   Stimespec_Sec_Unit : constant Stimespec := 10#1#E9;

end System.Task_Clock;

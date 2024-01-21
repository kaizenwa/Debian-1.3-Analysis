------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--  S Y S T E M . T A S K _ C L O C K . M A C H I N E _ S P E C I F I C S   --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.2 $                             --
--                                                                          --
--            Copyright (C) 1991-1997, Florida State University             --
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

--  This package provides target machine specific Clock related definitions.
--  Portability of System.Task_Clock package is accomplished separating
--  this child package out. We only need to modify this package for
--  different targets.

--  This version of System.Task_Clock.Machine_Specifics supports
--  UNIX machines which provide microsecond timing through the
--  gettimeofday() function.

--  This is a HP-UX 9.05 version of this package.

with Interfaces.C; use Interfaces.C;

package body System.Task_Clock.Machine_Specifics is

   -----------
   -- Clock --
   -----------

   Microseconds_Per_Second : long := 10#1#E6;

   type timeval is record
      tv_sec : long;       -- seconds
      tv_usec : long;        -- and microseconds
   end record;

   type timezone is record
      tz_minuteswest : int;   -- of Greenwich
      tz_dsttime : int;       -- type of dst correction to apply
   end record;

   function gettimeofday
     (tp : access timeval;
      tzp : access timezone)
      return int;
   pragma Import (C, gettimeofday);

   function Clock return Stimespec is
      Now    : aliased timeval;
      Result : int;

   begin
      Result := gettimeofday (Now'Access, null);
      return
          Stimespec_Sec_Unit * Stimespec (Now.tv_sec) +
          Stimespec (Now.tv_usec) * Integer'(Stimespec_Sec_Unit /
            Stimespec (Microseconds_Per_Second));
   end Clock;

begin
   Stimespec_Ticks := Time_Of (0, 1000000);
   --  clock resolution of 10ms. ???
end System.Task_Clock.Machine_Specifics;

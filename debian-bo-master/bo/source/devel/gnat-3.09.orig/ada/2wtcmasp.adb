------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--  S Y S T E M . T A S K _ C L O C K . M A C H I N E _ S P E C I F I C S   --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.1 $                             --
--                                                                          --
--                               Win32 Version                              --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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

--  This version of Clock uses the GetSystemTimeAsFileTime() procedure from
--  the Win32.Winbase API.  NOT! use two functions to for older kernel32.lib

package body System.Task_Clock.Machine_Specifics is
   pragma Linker_Options ("-lkernel32");

   epoch_1970     : constant := 16#19D_B1DE_D53E_8000#;  -- win32 UTC epoch
   system_time_ns : constant := 100;   -- 100 ns per tick

   -----------
   -- Clock --
   -----------

   procedure GetSystemTime (TIME : Address);
   pragma Import (stdcall, GetSystemTime, "GetSystemTime");

   procedure SystemTimeToFileTime (STIME, FTIME : Address);
   pragma Import (stdcall, SystemTimeToFileTime, "SystemTimeToFileTime");

   function Clock return Stimespec is
      SYSTIME  : aliased array (0 .. 3) of INTEGER;  -- stored in 8 words
      NOW      : aliased LONG_LONG_INTEGER;
   begin
      GetSystemTime (SYSTIME (0)'Address);
      SystemTimeToFileTime (SYSTIME (0)'Address, NOW'Address);
      return Stimespec ((NOW - epoch_1970) * system_time_ns);
   end Clock;

--  More efficient version does this in one step, but is not available in
--  early Kernel32.lib (prior to MSVC2.2)

--  procedure GetSystemTimeAsFileTime (TIME : Address);
--  pragma Import (stdcall, GetSystemTimeAsFileTime,
--                 "GetSystemTimeAsFileTime");

--  function Clock return Stimespec is
--     NOW    : aliased LONG_LONG_INTEGER;
--  begin
--     GetSystemTimeAsFileTime (NOW'Address);
--     return Stimespec ((NOW - epoch_1970) * system_time_ns);
--  end Clock;

begin

   Stimespec_Ticks := Time_Of (0, 1_000_000);
   --  gnat_time has the resolution of 1 msec.

end System.Task_Clock.Machine_Specifics;

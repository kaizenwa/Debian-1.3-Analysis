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
--        Copyright (C) 1991,1992,1993,1994 Florida State University        --
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

--  This version of Clock uses the time() function, which is available
--  in most C libraries.  This is the "universal" version; it is
--  only accurate to 1 second and is not the same time base used by
--  the tasking library System.Real_Time.Clock and delays, but will
--  work on systems without tasking or POSIX.

with Interfaces.C; use Interfaces.C;

package body System.Task_Clock.Machine_Specifics is

   -----------
   -- Clock --
   -----------

   function Clock return Stimespec is

      type timeval is
         record
            tv_sec  : Integer;
            tv_usec : Integer;
         end record;

      tv : aliased timeval;

      procedure gettimeofday (tp : access timeval);
      pragma Import (C, gettimeofday, "gettimeofday", "gettimeofday");

   begin
      gettimeofday (tv'Access);
      return
        Time_Of (tv.tv_sec, tv.tv_usec * 1000); --  Convert to nanoseconds
   end Clock;


begin
   Stimespec_Ticks := Time_Of (0, 1000000);
   --  Irix has clock resoultion of 10ms.

end System.Task_Clock.Machine_Specifics;

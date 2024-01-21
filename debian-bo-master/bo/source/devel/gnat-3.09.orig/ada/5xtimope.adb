------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T I M E _ O P E R A T I O N S              --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.1 $                            --
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

--  This is a SNI (DCE THREAD) version of this package.

with System.Error_Reporting;
--  used for Shutdown

with System.OS_Interface;

with Interfaces.C;

package body System.Time_Operations is

   use System.Error_Reporting;
   use System.OS_Interface;
   use Interfaces.C;

   function pthread_delay
     (interval : access timespec) return Interfaces.C.int;
   pragma Import (C, pthread_delay, "pthread_delay_np");

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
      TS     : aliased timespec;
      Result : Interfaces.C.int;

   begin
      Result := clock_gettime (TS'Unchecked_Access);
      pragma Assert (Result = 0
        or else Shutdown ("GNULLI failure---clock_gettime"));
      return To_Duration (TS);
   exception
   when others =>
      pragma Assert (Shutdown ("exception in Clock"));
      return 0.0;
   end Clock;

   ---------------
   -- Delay_For --
   ---------------

   procedure Delay_For (Rel_Time : Duration) is
      Result      : Interfaces.C.int;
      Request     : aliased timespec;
   begin
      --  if the request is zero or negative, we need to add it to the
      --  tail of the ready queue for its priority.
      if Rel_Time <= 0.0 then
         Result := sched_yield;
         return;
      end if;

      Request := To_Timespec (Rel_Time + Clock_Delay_Correction);
      Result := pthread_delay (Request'Access);
      pragma Assert (Result = 0
        or else Shutdown ("GNULLI failure---Delay_For (nanosleep)"));
   end Delay_For;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Abs_Time : Duration) is
      Result       : Interfaces.C.int;
      Request      : aliased timespec;
      Current_Time : Duration := Clock;
   begin
      --  if the requested time has passed. We need to add it to the
      --  tail of the ready queue for its priority.
      if Abs_Time <= Current_Time + Clock_Delay_Correction then
         Result := sched_yield;
         return;
      end if;

      Request :=
        To_Timespec (Abs_Time - Current_Time + Clock_Delay_Correction);

      Result := pthread_delay (Request'Access);
      pragma Assert (Result = 0
        or else Shutdown ("GNULLI failure---Delay_Until (nanosleep)"));
   end Delay_Until;

begin

   Clock_Delay_Correction := 0.02;
   --  SunOS 4.1 has clock resolution of 20ms.
   --  This may work for Solaris also???
end System.Time_Operations;

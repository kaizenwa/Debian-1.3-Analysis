------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T I M E _ O P E R A T I O N S              --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.2 $                            --
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

--  This is a Linux version of this package.

with System.Error_Reporting;
--  used for Shutdown

with System.OS_Interface;

with Interfaces.C;

package body System.Time_Operations is

   use System.Error_Reporting;
   use System.OS_Interface;
   use Interfaces.C;

   subtype unsigned_long is Interfaces.C.unsigned_long;

   Microseconds_Per_Second : Duration := 1_000_000.0;

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
      TV     : aliased struct_timeval;
      Result : Interfaces.C.int;

   begin
      Result := gettimeofday (TV'Access, null);
      pragma Assert (Result = 0
        or else Shutdown ("GNULLI failure---clock_gettime"));
      return To_Duration (TV);
   exception
   when others =>
      pragma Assert (Shutdown ("exception in Clock"));
      return 0.0;
   end Clock;

   ---------------
   -- Delay_For --
   ---------------

   --  For systems do not provide nanosleep() or usleep(), we need to
   --  implement these using cond_timedwait. However, we have to make sure
   --  we do Defer_Abortion before we get the lock and do Undefer_Abortion
   --  afterward in order not to leave an inconsistant lock. ???
   --  We may use this routine in an ATC. That means it in in general not
   --  OK to use Self lock and CV in here. I know the overhead is high
   --  but do not see how we can avoid creating local lock here. ???

   --    CV      : aliased cond_t;
   --    L       : aliased mutex_t;
   --    Result1 : Interfaces.C.int;
   --    Result2 : Interfaces.C.int;
   --    Request : aliased timespec;
   --  begin
   --    Request := To_Timespec (Rel_Time + Clock + Clock_Delay_Correction);
   --    Result1 :=
   --      mutex_init (L'Access, USYNC_THREAD, System.Null_Address);
   --    pragma Assert (Result1 = 0
   --      or else Shutdown ("GNULLI failure---mutex_init"));
   --    Result1 := cond_init (CV'Access, USYNC_THREAD, 0);
   --    pragma Assert (Result1 = 0
   --      or else Shutdown ("GNULLI failure---cond_init"));
   --    Result1 := mutex_lock (L'Access);
   --    pragma Assert (Result1 = 0
   --      or else Shutdown ("GNULLI failure---mutex_lock"));
   --    loop
   --       Result2 := cond_timedwait
   --         (CV'Access, L'Access, Request'Access);
   --       exit when Result2 /= 0 and then Result2 /= EINTR;
   --    end loop;
   --    Result1 := mutex_unlock (L'Access);
   --    pragma Assert (Result1 = 0
   --      or else Shutdown ("GNULLI failure---mutex_unlock"));
   --    pragma Assert
   --      ((Result2 = ETIME and then Clock >= To_Duration (Request))
   --      or else Shutdown ("GNULLI failure---Sleep_Until (early)"));
   --

   procedure Delay_For (Rel_Time : Duration) is
      Result  : Interfaces.C.int;
      Request : Duration;

   begin
      --  if the request is zero or negative, we need to add it to the
      --  tail of the ready queue for its priority.
      if Rel_Time <= 0.0 then
         Result := sched_yield;
         return;
      end if;

      Request := Rel_Time + Clock_Delay_Correction;
      usleep (unsigned_long (Request * Microseconds_Per_Second));
   end Delay_For;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Abs_Time : Duration) is
      Result       : Interfaces.C.int;
      Request      : Duration;
      Current_Time : Duration := Clock;

   begin
      --  if the requested time has passed. We need to add it to the
      --  tail of the ready queue for its priority.
      if Abs_Time <= Current_Time + Clock_Delay_Correction then
         Result := sched_yield;
         return;
      end if;

      Request := Abs_Time - Current_Time + Clock_Delay_Correction;
      usleep (unsigned_long (Request * Microseconds_Per_Second));
   end Delay_Until;

begin

   Clock_Delay_Correction := 0.02;
   --  Not sure of the clock resolution of Linux.
   --  Temporarily use Sun os 4.1 value (20ms).
end System.Time_Operations;

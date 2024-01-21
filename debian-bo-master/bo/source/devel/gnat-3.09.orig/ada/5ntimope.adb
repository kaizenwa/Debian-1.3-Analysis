------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T I M E _ O P E R A T I O N S              --
--                                                                          --
--                                  B o d y                                 --
--                           (No Tasking version)                           --
--                                                                          --
--                             $Revision: 1.1 $                             --
--                                                                          --
--    Copyright (C) 1991,92,93,94,95,1996 Free Software Foundation, Inc.    --
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

with Interfaces.C;

with System.Error_Reporting;
--  used for Shutdown

with System.OS_Interface;

package body System.Time_Operations is

   use System.Error_Reporting;
   use System.OS_Interface;
   use Interfaces.C;

   -----------
   -- Clock --
   -----------
 
   function Clock return Duration is
      Now    : aliased struct_timeval;
      Result : Interfaces.C.int;
 
   begin
      Result := gettimeofday (Now'Access, null);
      pragma Assert (Result = 0
        or else Shutdown ("GNULLI failure---gettimeofday"));
      return To_Duration (Now);
   exception
      when others =>
         pragma Assert (Shutdown ("exception in Clock"));
         return 0.0;
   end Clock;

   function sleep (How_Long : Natural) return Interfaces.C.int;
   pragma Import (C, sleep, "sleep");
 
   ---------------
   -- Delay_For --
   ---------------
 
   procedure Delay_For (Rel_Time : Duration) is
      Result : Interfaces.C.int;
   begin
      if Rel_Time > 0.0 then
         Result := sleep (Natural (Rel_Time));
      end if;
   end Delay_For;
 
   -----------------
   -- Delay_Until --
   -----------------
 
   procedure Delay_Until (Abs_Time : Duration) is
      Result       : Interfaces.C.int;
      Current_Time : Duration := Clock;
   begin
      if Abs_Time > Current_Time then
         Result := sleep (Natural (Abs_Time - Current_Time));
      end if;
   end Delay_Until;

begin

   Clock_Delay_Correction := 0.5;
   --  To compansate truncation of duration due to the use of sleep().

end System.Time_Operations;

------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   A D A . C A L E N D A R . D E L A Y S                  --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.3 $                             --
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

--  This is the Alpha/VMS version.

with System.Task_Primitives.Operations;
--  Used for Yield

with System.Time_Operations;
--  Used for Delay_For
--           Delay_Until

with System.Task_Timer;
--  Used for Timer
--           Delay_Block
--           Max_Sensible_Delay

with Ada.Calendar.Conv; use Ada.Calendar.Conv;

package body Ada.Calendar.Delays is

   ------------------
   -- Delay_Object --
   ------------------

   protected body Delay_Object is
      entry Wait (T : Duration; D : access System.Task_Timer.Delay_Block)
        when True is
      begin
         if T <= 0.0 then
            System.Task_Primitives.Operations.Yield;
            return;
         end if;
         requeue System.Task_Timer.Timer.Enqueue_Duration with abort;
      end Wait;
   end Delay_Object;

   ------------------------
   -- Delay_Until_Object --
   ------------------------

   protected body Delay_Until_Object is
      entry Wait (T : Time; D : access System.Task_Timer.Delay_Block)
        when True is
      begin
         if T <= Clock then
            System.Task_Primitives.Operations.Yield;
            return;
         end if;
         requeue System.Task_Timer.Timer.Enqueue_Calendar_Time with abort;
      end Wait;
   end Delay_Until_Object;

   ---------------
   -- Delay_For --
   ---------------

   procedure Delay_For (D : Duration) is
   begin
      System.Time_Operations.Delay_For
        (Duration'Min (D, System.Task_Timer.Max_Sensible_Delay));
   end Delay_For;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
   begin
      System.Time_Operations.Delay_Until (To_Absolute_Duration (T));
   end Delay_Until;

end Ada.Calendar.Delays;

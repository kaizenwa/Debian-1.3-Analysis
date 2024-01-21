------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   A D A . R E A L _ T I M E . D E L A Y S                --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.21 $                            --
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

with System.Task_Primitives.Operations;
--  Used for Yield

with System.Task_Timer;
--  used for Timer
--           Delay_Block

with System.Time_Operations;
--  used for Delay_Until

package body Ada.Real_Time.Delays is

   ------------------
   -- Delay_Object --
   ------------------

   protected body Delay_Object is
      entry Wait (TS : Time_Span; D : access System.Task_Timer.Delay_Block)
        when True is
      begin
         if TS <= 0.0 then
            System.Task_Primitives.Operations.Yield;
            return;
         end if;
         requeue System.Task_Timer.Timer.Enqueue_Time_Span with abort;
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
         requeue System.Task_Timer.Timer.Enqueue_Real_Time with abort;
      end Wait;
   end Delay_Until_Object;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
   begin
      System.Time_Operations.Delay_Until (To_Duration (T));
   end Delay_Until;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : Time) return Duration is
   begin
      return To_Duration (Time_Span (T));
   end To_Duration;

end Ada.Real_Time.Delays;

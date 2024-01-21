------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   A D A . R E A L _ T I M E . D E L A Y S                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.17 $                            --
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

with System;
--  Used for, Priority

with System.Task_Timer;
--  Used for, Timer

with System.Task_Primitives;
--  Used for, Cond_Timed_Wait
--            Lock
--            Condition_Variable
--            Initialize_Lock
--            Initialize_Cond
--            Write_Lock
--            Unlock

with System.Task_Clock;
--  Used for, Stimespec

with Ada.Real_Time.Conv;
--  Used for, Time_To_Stimespec;

package body Ada.Real_Time.Delays is

   ------------------
   -- Delay_Object --
   ------------------

   protected body Delay_Object is
      entry Wait (TS : Time_Span; D : access System.Task_Timer.Delay_Block)
        when True is

      begin
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
         requeue System.Task_Timer.Timer.Enqueue_Real_Time with abort;
      end Wait;
   end Delay_Until_Object;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      L : System.Task_Primitives.Lock;
      C : System.Task_Primitives.Condition_Variable;
      Error, Result : Boolean;
   begin
      System.Task_Primitives.Initialize_Lock (System.Priority'Last, L);
      System.Task_Primitives.Initialize_Cond (C);

      System.Task_Primitives.Write_Lock (L, Error);
      System.Task_Primitives.Cond_Timed_Wait
        (C, L, Ada.Real_Time.Conv.Time_To_Stimespec (T), Result);
      System.Task_Primitives.Unlock (L);
   end Delay_Until;

end Ada.Real_Time.Delays;

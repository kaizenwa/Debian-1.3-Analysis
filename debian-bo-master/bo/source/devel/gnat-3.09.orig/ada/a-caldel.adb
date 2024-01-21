------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   A D A . C A L E N D A R . D E L A Y S                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.23 $                            --
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

with System;
--  Used for, Priority

with System.Task_Timer;
--  Used for, Timer
--            Max_Sensible_Delay

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

with System.Task_Clock.Machine_Specifics;
--  Used for, Stimespec_Ticks;

with Ada.Calendar.Conv;
--  Used for, Time_To_Stimespec

with Ada.Finalization;

package body Ada.Calendar.Delays is

   function "+" (L, R : System.Task_Clock.Stimespec) return
     System.Task_Clock.Stimespec renames System.Task_Clock."+";

   package Delay_Objects is

      type Delay_Mutex_CV is new Ada.Finalization.Limited_Controlled with
         record
            L : System.Task_Primitives.Lock;
            C : System.Task_Primitives.Condition_Variable;
         end record;

      procedure Initialize  (Object : in out Delay_Mutex_CV);
      procedure Finalize    (Object : in out Delay_Mutex_CV);
      procedure Delay_For   (Object : in out Delay_Mutex_CV; D : Duration);
      procedure Delay_Until (Object : in out Delay_Mutex_CV; T : Time);

   end Delay_Objects;

   package body Delay_Objects is

      procedure Initialize (Object : in out Delay_Mutex_CV) is
      begin
         System.Task_Primitives.Initialize_Lock
           (System.Priority'Last, Object.L);
         System.Task_Primitives.Initialize_Cond (Object.C);
      end Initialize;

      procedure Finalize   (Object : in out Delay_Mutex_CV) is
      begin
         System.Task_Primitives.Finalize_Cond (Object.C);
         System.Task_Primitives.Finalize_Lock (Object.L);
      end Finalize;

      procedure Delay_For (Object : in out Delay_Mutex_CV; D : Duration) is
         Error  : Boolean;
         Result : Boolean;
         New_T  : System.Task_Clock.Stimespec;

      begin
         System.Task_Primitives.Write_Lock (Object.L, Error);
         New_T :=
           Calendar.Conv.Time_To_Stimespec
             (Clock + Duration'Min (D, System.Task_Timer.Max_Sensible_Delay)) +
             System.Task_Clock.Machine_Specifics.Stimespec_Ticks;

         System.Task_Primitives.Cond_Timed_Wait
           (Object.C, Object.L, New_T, Result);

         System.Task_Primitives.Unlock (Object.L);
      end Delay_For;

      procedure Delay_Until (Object : in out Delay_Mutex_CV; T : Time) is
         Error, Result : Boolean;
      begin
         System.Task_Primitives.Write_Lock (Object.L, Error);
         System.Task_Primitives.Cond_Timed_Wait
           (Object.C,
            Object.L,
            Calendar.Conv.Time_To_Stimespec (T) +
              System.Task_Clock.Machine_Specifics.Stimespec_Ticks,
            Result);
         System.Task_Primitives.Unlock (Object.L);
      end Delay_Until;

   end Delay_Objects;

   ------------------
   -- Delay_Object --
   ------------------

   protected body Delay_Object is
      entry Wait (T : Duration; D : access System.Task_Timer.Delay_Block)
        when True is

      begin
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
         requeue System.Task_Timer.Timer.Enqueue_Calendar_Time with abort;
      end Wait;
   end Delay_Until_Object;

   ---------------
   -- Delay_For --
   ---------------

   procedure Delay_For (D : Duration) is
      DMCV : Delay_Objects.Delay_Mutex_CV;
   begin
      Delay_Objects.Delay_For (DMCV, D);
   end Delay_For;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      DMCV : Delay_Objects.Delay_Mutex_CV;
   begin
      Delay_Objects.Delay_Until (DMCV, T);
   end Delay_Until;

end Ada.Calendar.Delays;

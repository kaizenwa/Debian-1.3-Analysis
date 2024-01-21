------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . T A S K _ T I M E R                    --
--                                                                          --
--                                  S p e c                                 --
--                           (Compiler Interface)                           --
--                                                                          --
--                             $Revision: 1.7 $                             --
--                  (version not enabling use of nanosleep)                 --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

--  Note: there are two versions of this package, one enabling use of
--  nanosleep, and one that does not enable the use of nanosleep. The
--  file name for the nanosleep version is 9ntastim.ads. The file name
--  for the non-nanosleep version is s-tastim.ads.

with Ada.Finalization;

with Ada.Calendar;
--  Used for, Calendar.Time

with Ada.Real_Time;
--  Used for, Real_Time.Time
--            Real_Time.Time_Span

with System.Task_Clock;
--  Used for, Stimespec

package System.Task_Timer is

   --  The contents of this package, with the exception of Delay_Block,
   --  are internal to GNARL; Delay_Block is part of the compiler interface.
   --  Unfortunately, they must be visible so that they can be accessed
   --  from the body of Ada.Calendar.Delays and Ada.Real_Time.Delays,
   --  and they must be in the same package as
   --  Delay_Block so that they can be used to implement its finalization.

   type Delay_Block is limited private;
   --  An object of this type is passed by the compiler to the Wait
   --  entry of each delay object
   --  (e.g. Ada.Calendar.Delays.Delay_Object.Wait).  This is used by
   --  the delay object implementation to queue the delay request and
   --  suspend the task.

   Max_Sensible_Delay : constant Duration := 365 * 24 * 60 * 60.0;
   --  Max of one year delay, needed to prevent exceptions for large
   --  delay values. It seems unlikely that any test will notice this
   --  restriction.

   protected Timer is
      pragma Priority (Priority'Last);

      --  The following Enqueue entries enqueue elements in wake-up time
      --  order using a single timer queue (time in System.Real_Time.Time).

      entry Enqueue_Time_Span
        (T : in Ada.Real_Time.Time_Span;
         D : access Delay_Block);
      entry Enqueue_Duration
         (T : in Duration;
         D : access Delay_Block);
      entry Enqueue_Real_Time
        (T : in Ada.Real_Time.Time;
         D : access Delay_Block);
      entry Enqueue_Calendar_Time
        (T : in Ada.Calendar.Time;
         D : access Delay_Block);
      procedure Dequeue (D : access Delay_Block);
      procedure Service (T : out System.Task_Clock.Stimespec);
      function Empty return Boolean;
--  private
--  Private protected operations are not currently supported by the
--  compiler.
      procedure Time_Enqueue
        (T : in System.Task_Clock.Stimespec;
         D : access Delay_Block);
   end Timer;

private

   --  Signal_Object provides a simple suspension capability.

   protected type Signal_Object is
      pragma Priority (Priority'Last);
      entry Wait;
      procedure Signal;

   private
      Open   : Boolean := False;
   end Signal_Object;

   type Q_Link is access all Delay_Block;

   type Delay_Block is new Ada.Finalization.Limited_Controlled with record
      S_O      : Signal_Object;
      T        : System.Task_Clock.Stimespec;    --  wake up time
      Next     : Q_Link;
      Previous : Q_Link;
   end record;

   procedure Finalize (Object : in out Delay_Block);

end System.Task_Timer;

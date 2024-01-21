------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   A D A . C A L E N D A R . D E L A Y S                  --
--                                                                          --
--                                  S p e c                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.14 $                            --
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

--  This package implements Calendar.Time delays using protected objects.

--  Note: the compiler generates direct calls to this interface, in the
--  processing of time types.

with System.Task_Timer;
--  used for Delay_Block

package Ada.Calendar.Delays is

   --  The Wait entries suspend the caller until the requested timeout has
   --  expired.  The Delay_Block parameter provides the GNARL with working
   --  storage.

   protected Delay_Object is
      pragma Interrupt_Priority (System.Any_Priority'Last);
      entry Wait (T : Duration; D : access System.Task_Timer.Delay_Block);
   end Delay_Object;

   protected Delay_Until_Object is
      pragma Interrupt_Priority (System.Any_Priority'Last);
      entry Wait (T : Time; D : access System.Task_Timer.Delay_Block);
   end Delay_Until_Object;

   procedure Delay_For (D : Duration);

   procedure Delay_Until (T : Time);

   function To_Duration (T : Time) return Duration;

end Ada.Calendar.Delays;

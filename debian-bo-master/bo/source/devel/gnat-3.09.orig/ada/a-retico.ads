------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     A D A . R E A L _ T I M E . C O N V                  --
--                                                                          --
--                                  S p e c                                 --
--                           (Compiler Interface)                           --
--                                                                          --
--                             $Revision: 1.9 $                             --
--                                                                          --
--    Copyright (C) 1991, 92, 93, 94, 1995 Free Software Foundation, Inc.   --
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

--  Provides conversion between Task_Clock.Time and Real_Time.Time
--  and Real_Time.Time_Span

with System.Task_Clock;
--  Used for, Stimespec

package Ada.Real_Time.Conv is

   function Time_To_Stimespec
     (T    : Real_Time.time)
      return System.Task_Clock.Stimespec;

   function Stimespec_To_Time
     (T    : System.Task_Clock.Stimespec)
      return Real_Time.Time;

   function Time_Span_To_Stimespec
     (T    : Real_Time.Time_Span)
      return System.Task_Clock.Stimespec;

   function Stimespec_To_Time_Span
     (T   : System.Task_Clock.Stimespec)
      return Real_Time.Time_Span;

end Ada.Real_Time.Conv;

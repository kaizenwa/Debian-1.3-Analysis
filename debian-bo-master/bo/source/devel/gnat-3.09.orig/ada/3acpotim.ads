------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--             I N T E R F A C E S . C . P O S I X _ T I M E R S            --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.1 $                             --
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

--  This is a SGI IRIX version of this package.

--  This package provides an interface to POSIX definitions related to
--  time.

with System;

with Interfaces.C.POSIX_Error;
--  Used for Return_Code

with Interfaces.C.System_Constants;
--  For constants defining timespec layout

package Interfaces.C.POSIX_Timers is

   package PE renames Interfaces.C.POSIX_Error;

   Alignment : constant := Natural'Min (Standard'Maximum_Alignment, 8);

   type time_t is new int;

   type Nanoseconds is new long;

   subtype Fractional_Second is Nanoseconds range 0 .. 10#1#E9 - 1;
   --  This is dependent on the stdtypes.h header file.

   type timespec is record
      tv_sec : time_t;
      tv_nsec : Fractional_Second;
   end record;

   --  Lay out the POSIX-defined components of the timespec record to
   --  match their C definition, using information in the i-csycon.ads file.

   timespec_First : constant timespec :=
     timespec' (time_t'First, Fractional_Second'First);

   timespec_Last : constant timespec :=
     timespec' (time_t'Last, Fractional_Second'Last);

   timespec_Zero : constant timespec :=
     timespec' (time_t'First, Fractional_Second'First);

   timespec_Unit : constant timespec :=
     timespec' (time_t'First, Fractional_Second'First + 1);
   --  This is dependent on the POSIX.4 implementation; the draft standard
   --  only says that fields of these names and types (with Integer for long)
   --  will be in the record.  There may be other fields, and these do not
   --  have to be in the indicated position.  This should really be done by
   --  getting the sizes and offsets using get_POSIX_Constants and building
   --  the record to match using representation clauses.

   --  temporarily, should really only be for 1???
   type clock_id_t is private;

   CLOCK_REALTIME : constant clock_id_t;

   procedure clock_gettime
     (ID     : clock_id_t;
      CT     : out timespec;
      Result : out PE.Return_Code);

private

   type clock_id_t is new long;
   --  This clock_id_t is defined as an long in POSIX

   CLOCK_REALTIME : constant clock_id_t := 0;
   --  We currently implement only Realtime clock.

end Interfaces.C.POSIX_Timers;

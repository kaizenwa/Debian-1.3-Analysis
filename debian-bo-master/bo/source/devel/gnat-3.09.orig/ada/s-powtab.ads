------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . P O W T E N _ T A B L E                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a powers of ten table used for real conversions

package System.Powten_Table is
pragma Pure (Powten_Table);

   Maxpow : constant := 40;
   --  The number of entries in this table is chosen to be large enough to
   --  correspond to the number of decimal digits in a 128-bit integer. It
   --  seems unlikely that Long_Long_Integer will have more than 128-bits
   --  for some time to come!

   Powten : constant array (0 .. 40) of Long_Long_Float :=
      (00 => 1.0E+00,
       01 => 1.0E+01,
       02 => 1.0E+02,
       03 => 1.0E+03,
       04 => 1.0E+04,
       05 => 1.0E+05,
       06 => 1.0E+06,
       07 => 1.0E+07,
       08 => 1.0E+08,
       09 => 1.0E+09,
       10 => 1.0E+10,
       11 => 1.0E+11,
       12 => 1.0E+12,
       13 => 1.0E+13,
       14 => 1.0E+14,
       15 => 1.0E+15,
       16 => 1.0E+16,
       17 => 1.0E+17,
       18 => 1.0E+18,
       19 => 1.0E+19,
       20 => 1.0E+20,
       21 => 1.0E+21,
       22 => 1.0E+22,
       23 => 1.0E+23,
       24 => 1.0E+24,
       25 => 1.0E+25,
       26 => 1.0E+26,
       27 => 1.0E+27,
       28 => 1.0E+28,
       29 => 1.0E+29,
       30 => 1.0E+30,
       31 => 1.0E+31,
       32 => 1.0E+32,
       33 => 1.0E+33,
       34 => 1.0E+34,
       35 => 1.0E+35,
       36 => 1.0E+36,
       37 => 1.0E+37,
       38 => 1.0E+38,
       39 => 1.0E+39,
       40 => 1.0E+40);

end System.Powten_Table;

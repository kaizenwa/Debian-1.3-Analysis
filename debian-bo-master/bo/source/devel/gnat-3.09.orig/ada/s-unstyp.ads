------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUNTIME COMPONENTS                         --
--                                                                          --
--                S Y S T E M . U N S I G N E D _ T Y P E S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.16 $                             --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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

--  This package contains definitions of standard unsigned types that
--  correspond in size to the standard signed types declared in Standard.
--  and (unlike the types in Interfaces have corresponding names). It
--  also contains some related definitions for other specialized types
--  used only by the expander.

package System.Unsigned_Types is
pragma Pure (Unsigned_Types);

   type Short_Short_Unsigned is mod 2 ** Short_Short_Integer'Size;
   type Short_Unsigned       is mod 2 ** Short_Integer'Size;
   type Unsigned             is mod 2 ** Integer'Size;
   type Long_Unsigned        is mod 2 ** Long_Integer'Size;
   type Long_Long_Unsigned   is mod 2 ** Long_Long_Integer'Size;

   type Float_Unsigned       is mod 2 ** Float'Size;
   --  Used in the implementation of Is_Negative intrinsic (see Exp_Intr)

   type Packed_Byte is mod 2 ** 8;
   for Packed_Byte'Size use 8;
   --  Component type for Packed_Butes array

   type Packed_Bytes is array (Natural range <>) of Packed_Byte;
   --  This is the type used to implement packed arrays for component sizes
   --  other than 1, 2 or 4, for cases where the total length in bits exceeds
   --  64, or is unknown at compile time (for those cases where the length is
   --  known at compile time and is 64 or less, a modular type is used to
   --  to represent the packed array, see Exp_Pakd for details).

   for Packed_Bytes'Alignment use 4;
   --  We force packed byte arrays to be at least word aligned. This is
   --  required because the processing in some of the System.Pack_nn units
   --  assumes that the array that is passed is 2 or 4 byte aligned.

   type Packed_Bytes_Unaligned is array (Natural range <>) of Packed_Byte;
   --  This is the type used to implement packed arrays for component sizes
   --  of 1, 2 or 4, for cases where the total length in bits exceeds 64,
   --  or is unknown at compile time. The reason for the separate type is
   --  that we do not require the 4-byte alignment for these cases, and by
   --  avoiding it, we reduce restrictions on rep clauses for these common
   --  packed cases.

   type Bits_1 is mod 2**1;
   type Bits_2 is mod 2**2;
   type Bits_4 is mod 2**4;
   --  Types used for packed array conversions

   subtype Bytes_F is Packed_Bytes (1 .. Float'Size / 8);
   --  Type used in implementation of Is_Negative instrinsic (see Exp_Intr)

   function Shift_Left
     (Value  : Short_Short_Unsigned;
      Amount : Natural)
     return    Short_Short_Unsigned;

   function Shift_Right
     (Value  : Short_Short_Unsigned;
      Amount : Natural)
      return   Short_Short_Unsigned;

   function Shift_Right_Arithmetic
     (Value  : Short_Short_Unsigned;
      Amount : Natural)
      return   Short_Short_Unsigned;

   function Rotate_Left
     (Value  : Short_Short_Unsigned;
      Amount : Natural)
      return   Short_Short_Unsigned;

   function Rotate_Right
     (Value  : Short_Short_Unsigned;
      Amount : Natural)
      return   Short_Short_Unsigned;

   function Shift_Left
     (Value  : Short_Unsigned;
      Amount : Natural)
      return   Short_Unsigned;

   function Shift_Right
     (Value  : Short_Unsigned;
      Amount : Natural)
      return   Short_Unsigned;

   function Shift_Right_Arithmetic
     (Value  : Short_Unsigned;
      Amount : Natural)
      return   Short_Unsigned;

   function Rotate_Left
     (Value  : Short_Unsigned;
      Amount : Natural)
      return   Short_Unsigned;

   function Rotate_Right
     (Value  : Short_Unsigned;
      Amount : Natural)
      return   Short_Unsigned;

   function Shift_Left
     (Value  : Unsigned;
      Amount : Natural)
     return    Unsigned;

   function Shift_Right
     (Value  : Unsigned;
      Amount : Natural)
      return   Unsigned;

   function Shift_Right_Arithmetic
     (Value  : Unsigned;
      Amount : Natural)
      return   Unsigned;

   function Rotate_Left
     (Value  : Unsigned;
      Amount : Natural)
      return   Unsigned;

   function Rotate_Right
     (Value  : Unsigned;
      Amount : Natural)
      return   Unsigned;

   function Shift_Left
     (Value  : Long_Unsigned;
      Amount : Natural)
     return    Long_Unsigned;

   function Shift_Right
     (Value  : Long_Unsigned;
      Amount : Natural)
      return   Long_Unsigned;

   function Shift_Right_Arithmetic
     (Value  : Long_Unsigned;
      Amount : Natural)
      return   Long_Unsigned;

   function Rotate_Left
     (Value  : Long_Unsigned;
      Amount : Natural)
      return   Long_Unsigned;

   function Rotate_Right
     (Value  : Long_Unsigned;
      Amount : Natural)
      return   Long_Unsigned;

   function Shift_Left
     (Value  : Long_Long_Unsigned;
      Amount : Natural)
     return    Long_Long_Unsigned;

   function Shift_Right
     (Value  : Long_Long_Unsigned;
      Amount : Natural)
      return   Long_Long_Unsigned;

   function Shift_Right_Arithmetic
     (Value  : Long_Long_Unsigned;
      Amount : Natural)
      return   Long_Long_Unsigned;

   function Rotate_Left
     (Value  : Long_Long_Unsigned;
      Amount : Natural)
      return   Long_Long_Unsigned;

   function Rotate_Right
     (Value  : Long_Long_Unsigned;
      Amount : Natural)
      return   Long_Long_Unsigned;

   pragma Import (Intrinsic, Shift_Left);
   pragma Import (Intrinsic, Shift_Right);
   pragma Import (Intrinsic, Shift_Right_Arithmetic);
   pragma Import (Intrinsic, Rotate_Left);
   pragma Import (Intrinsic, Rotate_Right);

end System.Unsigned_Types;

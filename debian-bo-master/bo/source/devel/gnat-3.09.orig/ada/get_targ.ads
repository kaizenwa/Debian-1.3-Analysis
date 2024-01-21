------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G E T _ T A R G                              --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;

package Get_Targ is
--  This package provides an Import to the C functions which provide
--  values related to types on the target system.  It is only needed for
--  the elaboration of ttypes.

   function Get_Bits_Per_Unit return Pos;
   pragma Import (C, Get_Bits_Per_Unit, "get_target_bits_per_unit");

   function Get_Bits_Per_Word return Pos;
   pragma Import (C, Get_Bits_Per_Word, "get_target_bits_per_word");

   function Get_Char_Size return Pos;
   pragma Import (C, Get_Char_Size, "get_target_char_size");

   function Get_Short_Size return Pos;
   pragma Import (C, Get_Short_Size, "get_target_short_size");

   function Get_Int_Size return Pos;
   pragma Import (C, Get_Int_Size, "get_target_int_size");

   function Get_Long_Size return Pos;
   pragma Import (C, Get_Long_Size, "get_target_long_size");

   function Get_Long_Long_Size return Pos;
   pragma Import (C, Get_Long_Long_Size, "get_target_long_long_size");

   function Get_Float_Size return Pos;
   pragma Import (C, Get_Float_Size, "get_target_float_size");

   function Get_Double_Size return Pos;
   pragma Import (C, Get_Double_Size, "get_target_double_size");

   function Get_Long_Double_Size return Pos;
   pragma Import (C, Get_Long_Double_Size, "get_target_long_double_size");

   function Get_Pointer_Size return Pos;
   pragma Import (C, Get_Pointer_Size, "get_target_pointer_size");

   function Get_Maximum_Alignment return Pos;
   pragma Import (C, Get_Maximum_Alignment, "get_target_maximum_alignment");

   function Get_Float_Words_BE return Nat;
   pragma Import (C, Get_Float_Words_BE, "get_float_words_be");

   function Get_Words_BE return Nat;
   pragma Import (C, Get_Words_BE, "get_words_be");

   function Get_Bytes_BE return Nat;
   pragma Import (C, Get_Bytes_BE, "get_bytes_be");

   function Get_Bits_BE return Nat;
   pragma Import (C, Get_Bits_BE, "get_bits_be");

   function Width_From_Size  (Size : Pos) return Pos;
   function Digits_From_Size (Size : Pos) return Pos;
   --  Calculate values for 'Width or 'Digits from 'Size

end Get_Targ;

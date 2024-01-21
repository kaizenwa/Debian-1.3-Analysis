------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D B U G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
--                                                                          --
--            Copyright (C) 1996 Free Software Foundation, Inc.             --
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

--  Expand routines for generation of special declarations used by the
--  debugger. In accordance with the Dwarf 2.2 specification, certain
--  type names are encoded to provide information to the debugger.

with Types; use Types;

package Exp_Dbug is

   -------------------------
   -- Type Name Encodings --
   -------------------------

   --  In the following typ is the name of the type as normally encoded by
   --  the debugger rules, i.e. a non-qualified name, all in lower case,
   --  with standard encoding of upper half and wide characters

   --    Fixed-Point Types

   --         typ_XFX_nn_dd
   --         typ_XF_nn_dd_nn_dd

   --       The first form is used when small = delta. The value of delta (and
   --       small) is given by the rational nn/dd, where nn and dd are decimal
   --       integers. Negative numbers are represented using a trailing lower
   --       case m (for minus) after the decimal digits.
   --
   --       The second form is used if the small value is different from the
   --       delta. In this case, the first nn/dd rational value is for delta,
   --       and the second value is for small.

   --    Discrete Types

   --         typ_XD
   --         typ_XDL_lowerbound
   --         typ_XDU_upperbound
   --         typ_XDLU_lowerbound__upperbound

   --       If a discrete type is a natural machine type (i.e. its bounds
   --       correspond in a natural manner to its size), then it is left
   --       unencoded. The above encoding forms are used when there is a
   --       constrained range that does not correspond to the size or that
   --       has discriminant references or other non-static bounds.

   --       The first form is used if both bounds are dynamic, in which case
   --       two constant objects are present whose names are typ__L and
   --       typ__U in the same scope as typ, and the values of these constants
   --       indicate the bounds. As far as the debugger is concerned, these
   --       are simply variables that can be accessed like any other variables.
   --       Note that in the enumeration case, these values correspond to the
   --       Pos values for the lower and upper bounds.

   --       The second form is used if the lower bound is dynamic, but the
   --       upper bound is either constant or depends on a discriminant of
   --       the record with which the type is associated. The lower bound
   --       is stored in a constant object of name typ__L as previously
   --       described, but the upper bound is encoded directly into the
   --       name as either a decimal integer, or as the discriminant name.

   --       The third form is similarly used if the upper bound is dynamic,
   --       but the lower bound is static or a discrmiminant reference.

   --       The fourth form is used if both bounds are discriminant references
   --       or static values, with the encoding first for the lower bound,
   --       then for the upper bound, as previously described.

   function Get_Encoded_Type_Name (E : Entity_Id) return Boolean;
   --  Return True if type name needs to be encoded according to the above
   --  rules. In that case, the suffix for the encoded name is stored in
   --  Name_Buffer with the length of the name in Name_Len and an ASCII.NUL
   --  character stored following the name. If no encoding is required,
   --  then False is returned and the values in Name_Buffer and
   --  Name_Len are undefined.

end Exp_Dbug;

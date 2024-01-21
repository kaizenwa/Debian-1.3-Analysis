------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              C O M P E R R                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.12 $                             --
--                                                                          --
--        Copyright (C) 1992,1993,1994 Free Software Foundation, Inc.       --
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

--  This package contains the routine called when a fatal internal compiler
--  error is detected. Calls to this routines cause termination of the
--  current compilation with appropriate error output.

with Types; use Types;

package Comperr is

   procedure Compiler_Abort (X : String; Code : Integer := 0);
   --  Signals an internal compiler error. Never returns control. Depending
   --  on processing may end up raising Unrecoverable_Error, or exiting
   --  directly. The message output is "internal error: X" where X is the
   --  string passed as an argument. The node in Fatal_Error_Node is used
   --  to provide the location where the error should be signalled. The
   --  message includes the node id, and the code parameter if it is non-zero)
   --  Note that this is only used at the outer level (to handle constraint
   --  errors or assert errors etc.) In the normal logic of the compiler we
   --  always use pragma Assert to check for errors, and if necessary an
   --  explicit abort is achieved by pragma Assert (False).

end Comperr;

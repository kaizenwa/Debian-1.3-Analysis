
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 I N T E R F A C E S . C . P T H R E A D S                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.1 $                             --
--                                                                          --
--          Copyright (C) 1996 FSF Free Software Foundation, Inc.           --
--                                                                          --
-- GNARL is free software; you can redistribute it  and/or modify it  under --
-- terms  of  the  GNU  Library General Public License  as published by the --
-- Free Software  Foundation;  either version 2, or (at  your  option)  any --
-- later  version.  GNARL is distributed  in the hope that  it will be use- --
-- ful, but but WITHOUT ANY WARRANTY;  without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Gen- --
-- eral Library Public License  for more details.  You should have received --
-- a  copy of the GNU Library General Public License along with GNARL;  see --
-- file COPYING.LIB.  If not,  write to the  Free Software Foundation,  675 --
-- Mass Ave, Cambridge, MA 02139, USA.                                      --
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

package body Interfaces.C.Pthreads is

   pragma Linker_Options ("-lpthreads");
   pragma Linker_Options ("-lmach");
   pragma Linker_Options ("-lc_r");

   procedure Require_Package_Body is
   begin
      null;
   end Require_Package_Body;

end Interfaces.C.Pthreads;

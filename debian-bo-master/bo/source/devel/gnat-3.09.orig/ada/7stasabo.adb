------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              S Y S T E M . T A S K I N G . A B O R T I O N               --
--                                                                          --
--                                 B o d y                                  --
--                         (Version for new GNARL)                          --
--                                                                          --
--                            $Revision: 1.21 $                             --
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

with System.Tasking.Utilities;
--  used for Abort_Tasks

with System.Tasking.Initialization;
--  used for Defer_Abort
--           Undefer_Abort
--           Change_Base_Priority

package body System.Tasking.Abortion is

   --------------------------
   -- Change_Base_Priority --
   --------------------------

   procedure Change_Base_Priority (T : Task_ID) renames
     Initialization.Change_Base_Priority;

   --------------------
   -- Defer_Abortion --
   --------------------

   procedure Defer_Abortion renames Initialization.Defer_Abortion;

   ----------------------
   -- Undefer_Abortion --
   ----------------------

   procedure Undefer_Abortion renames Initialization.Undefer_Abortion;

   -----------------
   -- Abort_Tasks --
   -----------------

   --  Called to initiate abortion, however, the actual abortion
   --  is done by abortee by means of Abort_Handler

   procedure Abort_Tasks (Tasks : Task_List) renames Utilities.Abort_Tasks;

end System.Tasking.Abortion;

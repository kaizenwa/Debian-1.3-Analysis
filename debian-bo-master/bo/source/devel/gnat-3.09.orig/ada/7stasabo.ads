------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--               S Y S T E M . T A S K I N G . A B O R T I O N              --
--                                                                          --
--                                  S p e c                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.19 $                            --
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

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

package System.Tasking.Abortion is

   procedure Abort_Tasks (Tasks : Task_List);
   --  Abort_Tasks is called to initiate abortion, however, the actual
   --  abortion is done by abortee by means of Abort_Handler

   procedure Change_Base_Priority (T : Task_ID);
   --  Change the base priority of T.
   --  Has to be called with T.Lock write locked.

   procedure Defer_Abortion;
   --  Defer the affects of low-level abortion in the calling task until a
   --  matching Undefer_Abortion call is executed.  Defer_Abortion can be
   --  nested; abortion will be deferred until the calling task has
   --  called Undefer_Abortion for each outstanding call to
   --  Defer_Abortion.  Note that abortion must be deferred before
   --  calling any low-level (GNULLI) services.
   pragma Inline (Defer_Abortion);

   procedure Undefer_Abortion;
   --  Undo the effects of one call to Defer_Abortion.  When the calling
   --  task has called Undefer_Abortion for each outstanding call to
   --  Defer_Abortion, any pending low-level abortion will take effect,
   --  and subsequent low-level abortions will have an immediate
   --  asynchronous effect.
   pragma Inline (Undefer_Abortion);

end System.Tasking.Abortion;

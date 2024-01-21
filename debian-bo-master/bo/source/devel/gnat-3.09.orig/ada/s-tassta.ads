------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T A S K I N G . S T A G E S                --
--                                                                          --
--                                  S p e c                                 --
--                           (Compiler Interface)                           --
--                                                                          --
--                             $Revision: 1.24 $                            --
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

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

with System.Task_Info;
with System.Parameters;

package System.Tasking.Stages is
   --  This interface is described in the document
   --  Gnu Ada Runtime Library Interface (GNARLI).

   pragma Elaborate_Body (System.Tasking.Stages);

   function Current_Master return Master_ID;

   procedure Enter_Master;

   procedure Complete_Master;

   procedure Create_Task
     (Priority      : Integer;
      Size          : System.Parameters.Size_Type;
      Task_Info     : System.Task_Info.Task_Info_Type;
      Num_Entries   : Task_Entry_Index;
      Master        : Master_ID;
      State         : Task_Procedure_Access;
      Discriminants : System.Address;
      Elaborated    : Access_Boolean;
      Chain         : in out Activation_Chain;
      Created_Task  : out Task_ID);

   procedure Activate_Tasks (Chain_Access : Activation_Chain_Access);

   procedure Expunge_Unactivated_Tasks (Chain : in out Activation_Chain);

   procedure Complete_Activation;

   procedure Complete_Task;

   function Terminated (T : Task_ID) return Boolean;

   -------------------------------
   -- RTS Internal Declarations --
   -------------------------------

   --  These declarations are not part of the GNARLI.

   procedure Leave_Task;
   --  Export for abortion

   procedure Finalize_Global_Tasks;
   --  to be called by the binder

end System.Tasking.Stages;

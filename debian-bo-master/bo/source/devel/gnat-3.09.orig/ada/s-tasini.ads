------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--         S Y S T E M . T A S K I N G . I N I T I A L I Z A T I O N        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.6 $                             --
--                                                                          --
--      Copyright (C) 1991,1992,1993,1994,1995 Florida State University     --
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

--  This package provides overall initialization of the tasking portion of the
--  RTS. This package must be elaborated before any tasking features are used.
--  It also contains initialization for Ada Task Control Block (ATCB) records.

with System.Parameters;
with System.Task_Info;

package System.Tasking.Initialization is

   --  The following record holds the information used to initialize a task

   type ATCB_Init is record
      Task_Entry_Point : Task_Procedure_Access;
      Task_Arg         : System.Address;
      Stack_Size       : System.Parameters.Size_Type;
      Activator        : Task_ID;
      Parent           : Task_ID;
      Master_of_Task   : Master_ID;
      Elaborated       : Access_Boolean;
      Entry_Num        : Task_Entry_Index;
      Base_Priority    : System.Any_Priority;
      Current_Priority : System.Any_Priority;
      Task_Info        : System.Task_Info.Task_Info_Type;
   end record;

   -----------------------
   -- List of all Tasks --
   -----------------------

   All_Tasks_List : Task_ID;
   All_Tasks_L : System.Task_Primitives.Lock;
   --  All_Tasks_L should not be locked by a task that holds any other
   --  locks; in other words, All_Tasks_L should be the outermost lock.
   --  Currently, only ATCB locks are locked at the same time as All_Tasks_L.

   procedure Remove_From_All_Tasks_List
      (Source : Task_ID; Result : out Boolean);
   --  Remove an entry from the All_Tasks_List.

   ---------------------------------
   -- Tasking-Specific Soft Links --
   ---------------------------------

   --  These permit us to leave out certain portions of the tasking
   --  runtime system if they are not used.  They are only used internally
   --  by the tasking runtime system.
   --  So far, the only example is support for Ada.Task_Attributes.

   type Proc_T is access procedure (T : Task_ID);

   procedure Finalize_Attributes (T : Task_ID);
   procedure Initialize_Attributes (T : Task_ID);

   Finalize_Attributes_Link : Proc_T := Finalize_Attributes'Access;
   --  should be called with abortion deferred and T.L write-locked

   Initialize_Attributes_Link : Proc_T := Initialize_Attributes'Access;
   --  should be called with abortion deferred, but holding no locks

   -----------------------------
   -- ATCB related operations --
   -----------------------------

   procedure Initialize_ATCB
     (T    : Task_ID;
      Init : ATCB_Init);
   --  Initialize fields of a TCB and link into global TCB structures

   function New_ATCB
     (Init : ATCB_Init)
      return Task_ID;
   --  New_ATCB creates a new ATCB using Ada allocators and initializes
   --  it.

   function Unsafe_New_ATCB
     (Init : ATCB_Init)
      return Task_ID;
   --  Like New_ATCB, but without the initialization.

   procedure Free_ATCB (T : in out Task_ID);
   --  Release storage of a previously allocated ATCB

   ----------------------------------------------
   -- RTS routine to be used for pragma assert --
   ----------------------------------------------

   function Runtime_Assert_Shutdown (Msg : in String) return boolean;
   --  This function is used to shut down the runtime when there is
   --  an assertion error to be raise through "pragma Assert"
   --  Usage should be either
   --    pragma Assert (Runtime_Assert_Shutdown ("..."));
   --    --  uncoditional shutdown
   --  or
   --    pragma Assert
   --      (ASSERT_CONDITION or else Runtime_Assert_Shutdown ("..."));
   --    --  conditional shutdown. Shut down the runtime only when the
   --    --  ASSERT_CONDITION fails.

   --------------------------
   -- Master ID operations --
   --------------------------

   procedure Init_Master (M : out Master_ID);
   pragma Inline (Init_Master);

   function Increment_Master (M : Master_ID) return Master_ID;
   pragma Inline (Increment_Master);

   function Decrement_Master (M : Master_ID) return Master_ID;
   pragma Inline (Decrement_Master);

   -------------------------------
   -- Abortion related routines --
   -------------------------------

   procedure Abort_Handler (Context : System.Task_Primitives.Pre_Call_State);
   --  Handler to be installed at initialization; it is invoked by a task
   --  when it is the target of an Abort_Task low-level operation.

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
--    pragma Inline (Defer_Abortion); --  To allow breakpoints to be set. ???

   procedure Undefer_Abortion;
   --  Undo the effects of one call to Defer_Abortion.  When the calling
   --  task has called Undefer_Abortion for each outstanding call to
   --  Defer_Abortion, any pending low-level abortion will take effect,
   --  and subsequent low-level abortions will have an immediate
   --  asynchronous effect.
--    pragma Inline (Undefer_Abortion); --  To allow breakpoints to be set.

end System.Tasking.Initialization;

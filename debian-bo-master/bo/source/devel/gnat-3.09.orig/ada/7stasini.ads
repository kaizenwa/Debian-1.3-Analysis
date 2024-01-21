------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--         S Y S T E M . T A S K I N G . I N I T I A L I Z A T I O N        --
--                                                                          --
--                                  S p e c                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.8 $                             --
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

--  This package provides overall initialization of the tasking portion of the
--  RTS. This package must be elaborated before any tasking features are used.
--  It also contains initialization for Ada Task Control Block (ATCB) records.

with System.Task_Info;
--  used for Task_Info_Type

with System.Parameters;
--  used for Size_Type

with System.Task_Primitives;
--  used for RTS_Lock

package System.Tasking.Initialization is

   --  The following record holds the information used to initialize a task

   type ATCB_Init is record
      Task_Entry_Point : Task_Procedure_Access;
      Task_Arg         : System.Address;
      Activator        : Task_ID;
      Parent           : Task_ID;
      Master_of_Task   : Master_ID;
      Elaborated       : Access_Boolean;
      Entry_Num        : Task_Entry_Index;
      Base_Priority    : System.Any_Priority;
      Task_Info        : System.Task_Info.Task_Info_Type;
      Stack_Size       : System.Parameters.Size_Type;
   end record;

   -----------------------
   -- List of all Tasks --
   -----------------------

   All_Tasks_List : Task_ID;
   All_Tasks_L : aliased System.Task_Primitives.RTS_Lock;
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
     (Self_ID : Task_ID;
      T : Task_ID;
      Init : ATCB_Init);
   --  Initialize fields of a TCB and link into global TCB structures

   function New_ATCB (Self_ID : Task_ID; Init : ATCB_Init) return Task_ID;
   --  New_ATCB creates a new ATCB using Ada allocators and initializes
   --  it.

   function Unsafe_New_ATCB (Init : ATCB_Init) return Task_ID;
   --  Like New_ATCB, but without the initialization.

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

   procedure Change_Base_Priority (T : Task_ID);
   --  Change the base priority of T.
   --  Has to be called with T's ATCB write-locked.

--  ???? How is the above used?  Can we move it to Utilities?

   procedure Defer_Abort (Self_ID : Task_ID);
   --  Defer the affects of low-level abort and priority change
   --  in the calling task until a matching Undefer_Abor call is executed.
   --  Defer_Abort can be nested; abort will be deferred until the calling
   --  task has called Undefer_Abort for each outstanding call to
   --  Defer_Abort.  Note that abort must be deferred before
   --  calling any low-level (GNULLI) services.
   --  pragma Inline (Defer_Abort);
   --  No pragma Inline for now, to allow breakpoints to be set.

   --  Phase out all uses of the second version inside the RTS, ASAP
   --  Keep it around, as private to the body of this package, for
   --  making soft-link used by compiler-generated code.

   procedure Defer_Abortion;
   --  Same as Defer_Abort, with implicit call to fetch Self.
   --  Never pragma Inline here, to allow use of soft link to this version.
   --  Note : The same routines are defined in System.Tasking.Abortions.
   --  The reason we have two different definitions come from the confliction
   --  between Inlining and the use soft link. That is to say we want this
   --  to be soft linkable (so no inline) and also want to use in RTS code
   --  with Inlining (See the one in System.Tasking.Abortions).

   procedure Undefer_Abort (Self_ID : Task_ID);
   --  Undo the effects of one call to Defer_Abort.  When the calling
   --  task has called Undefer_Abort for each outstanding call to
   --  Defer_Abort, any pending low-level abort or priority change
   --  action will take effect, and subsequent low-level aborts
   --  will have an immediate asynchronous effect.

   --  pragma Inline (Undefer_Abort);
   --  No pragma Inline for now, to allow breakpoints to be set.

   --  Phase out all uses of the second version inside the RTS, ASAP.
   --  Keep it around, as private to the body of this package, for
   --  making soft-link used by compiler-generated code.

   procedure Undefer_Abortion;
   --  Same as above, with implicit call to fetch Self.
   --  Never pragma Inline here, to allow use of soft link to this version.
   --  Note : The same routines are defined in System.Tasking.Abortions.
   --  The reason we have two different definitions come from the confliction
   --  between Inlining and the use soft link. That is to say we want this
   --  to be soft linkable (so no inline) and also want to use in RTS code
   --  with Inlining (See the one in System.Tasking.Abortions).

end System.Tasking.Initialization;

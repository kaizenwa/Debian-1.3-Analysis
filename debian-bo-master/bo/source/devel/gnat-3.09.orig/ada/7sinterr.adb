------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.13 $                             --
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

--  Invariants:

--  All user-handleable interrupts are masked at all times in all
--  tasks/threads except possibly for the Interrupt_Manager task.

--  When a user task wants to have the effect of masking/unmasking an
--  interrupt, it must call Block_Interrupt/Unblock_Interrupt, which
--  will have the effect of unmasking/masking the interrupt in the
--  Interrupt_Manager task.

--  Note : Direct calls to sigaction, sigprocmask, pthread_sigsetmask or any
--  other low-level interface that changes the interrupt action or
--  interrupt mask needs a careful thought.
--  One may acheive the effect of system calls first masking RTS blocked
--  (by calling Block_Interrupt) for the interrupt under consideration.
--  This will make all the tasks in RTS blocked for the Interrupt.

--  Once we associate a Server_Task with an interrupt, the task never
--  goes away, and we never remove the association.

--  There is no more than one interrupt per Server_Task and no more than
--  one Server_Task per interrupt.

--  Within this package, the lock L is used to protect the various status
--  tables. If there is a Server_Task associated with an interrupt, we use
--  the per-task lock of the Server_Task instead so that we protect the
--  status between Interrupt_Manager and Server_Task. Protection among
--  service requests are done using User Request to Interrupt_Manager
--  rendezvous.

with System.Task_Primitives;
--  used for RTS_Lock;

with System.Interrupt_Management;
--  used for Reserve
--           Interrupt_ID
--           Interrupt_Mask
--           Abort_Task_Interrupt

with System.Interrupt_Management.Operations;
--  used for Thread_Block_Interrupt
--           Thread_Unblock_Interrupt
--           Install_Default_Action
--           Install_Ignore_Action
--           Copy_Interrupt_Mask
--           Set_Interrupt_Mask
--           Empty_Interrupt_Mask
--           Fill_Interrupt_Mask
--           Add_To_Interrupt_Mask
--           Delete_From_Interrupt_Mask
--           Interrupt_Wait
--           Interrupt_Self_Process
--           Get_Interrupt_Mask
--           Set_Interrupt_Mask
--           IS_Member

with System.Error_Reporting;
--  used for Shutdown

with System.Task_Primitives.Operations;
--  used for Write_Lock
--           Unlock
--           Wakeup
--           Abort_Task
--           Sleep
--           Initialize_Lock

with Interfaces.C;
--  used for int;

with System.Storage_Elements;
--  used for To_Address
--           To_Integer
--           Integer_Address

with System.Tasking;
--  used for Task_ID
--           Task_Entry_Index
--           Null_Task
--           Self

with System.Tasking.Abortion;
--  used for Defer_Abortion
--           Undefer_Abortion

with System.Tasking.Utilities;
--  used for Make_Independent

with System.Tasking.Rendezvous;
--  use for Call_Simple

with Ada.Task_Identification;
--  used for Task_ID

with Unchecked_Conversion;

package body System.Interrupts is

   use Tasking;
   use Interfaces.C;
   use System.Error_Reporting;

   function To_System is new Unchecked_Conversion
     (Ada.Task_Identification.Task_ID, Task_ID);

   package PRI renames System.Task_Primitives;
   package POP renames System.Task_Primitives.Operations;
   package IMNG renames System.Interrupt_Management;
   package IMOP renames System.Interrupt_Management.Operations;

   -----------------
   -- Local Tasks --
   -----------------

   task Interrupt_Manager is
      entry Initialize (Mask : IMNG.Interrupt_Mask);
      entry Attach_Handler
        (New_Handler : in Parameterless_Handler;
         Interrupt   : in Interrupt_ID;
         Static      : in Boolean);
      entry Exchange_Handler
        (Old_Handler : out Parameterless_Handler;
         New_Handler : in Parameterless_Handler;
         Interrupt   : in Interrupt_ID;
         Static      : in Boolean);
      entry Detach_Handler
        (Interrupt   : in Interrupt_ID;
         Static      : in Boolean);
      entry Bind_Interrupt_To_Entry
        (T         : Task_ID;
         E         : Task_Entry_Index;
         Interrupt : Interrupt_ID);
      entry Detach_Interrupt_Entries (T : Task_ID);
      entry Block_Interrupt (Interrupt : Interrupt_ID);
      entry Unblock_Interrupt (Interrupt : Interrupt_ID);
      entry Ignore_Interrupt (Interrupt : Interrupt_ID);
      entry Unignore_Interrupt (Interrupt : Interrupt_ID);

      pragma Interrupt_Priority (System.Interrupt_Priority'Last);
   end Interrupt_Manager;

   task type Server_Task (Interrupt : Interrupt_ID) is
      pragma Priority (System.Interrupt_Priority'Last);
   end Server_Task;

   type Server_Task_Access is access Server_Task;

   --------------------------------
   --  Local Types and Variables --
   --------------------------------

   type Entry_Assoc is record
      T : Task_ID;
      E : Task_Entry_Index;
   end record;

   type Handler_Assoc is record
      H      : Parameterless_Handler;
      Static : Boolean;   --  Indicates static binding;
   end record;

   User_Handler : array (Interrupt_ID'Range) of Handler_Assoc
     := (others => (null, Static => false));
   --  holds the protected procedure handler (if any) and its Static
   --  information  for each interrupt. A handler is a Static one if
   --  it is specified through the pragma Attach_Handler.
   --  Attach_Handler. Otherwise, not static)
   pragma Volatile_Components (User_Handler);

   User_Entry : array (Interrupt_ID'Range) of Entry_Assoc :=
     (others => (T => Null_Task, E => Null_Task_Entry));
   --  holds the task and entry index (if any) for each interrupt
   pragma Volatile_Components (User_Entry);

   Blocked : array (Interrupt_ID'Range) of Boolean := (others => false);
   --  is true iff the corresponding interrupt is blocked in the process level
   pragma Volatile_Components (Blocked);

   Ignored : array (Interrupt_ID'Range) of Boolean := (others => false);
   --  is true iff the corresponding interrupt is blocked in the process level
   pragma Volatile_Components (Blocked);

   Last_Unblocker :
     array (Interrupt_ID'Range) of Task_ID := (others => Null_Task);
   --  holds the ID of the last Task which Unblocked this Interrupt.
   --  It contains Null_Task if no tasks have ever requested the
   --  Unblocking operation or the Interrupt is currently Blocked.
   pragma Volatile_Components (Last_Unblocker);

   Server_ID : array (Interrupt_ID'Range) of Task_ID
       := (others => Null_Task);
   --  holds the Task_ID of the Server_Task for each interrupt.
   --  Task_ID is needed to accomplish locking per Interrupt base. Also
   --  is needed to decide whether to create a new Server_Task.
   pragma Atomic_Components (Server_ID);

   --  Type and Head, Tail of the list containing Registered Interrupt
   --  Handlers. These definitions are used to register the handlers
   --  specified by the pragma Interrupt_Handler.

   type Registered_Handler;
   type R_Link is access all Registered_Handler;

   type Registered_Handler is record
      H :    System.Address := System.Null_Address;
      Next : R_Link := null;
   end record;

   Registered_Handler_Head : R_Link := null;
   Registered_Handler_Tail : R_Link := null;

   Access_Hold : Server_Task_Access;
   --  variable used to allocate Server_Task using "new".

   L : aliased PRI.RTS_Lock;
   --  L protects contents in tables above corresponding to interrupts
   --  for which Server_ID (T) = null.
   --  If Server_ID (T) /= null then protection is via
   --  per-task (TCB) lock of Server_ID (T).
   --  For deadlock prevention, L should not be locked after
   --  any other lock is held.

   Task_Lock : array (Interrupt_ID'Range) of Boolean := (others => false);
   --  Boolean flags to give matching Locking and Unlocking. See the comments
   --  in Lock_Interrupt.

   Interrupt_Manager_ID : Task_ID;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Lock_Interrupt (Interrupt : Interrupt_ID);
   --  protect the tables using L or per-task lock. Set the boolean
   --  value Task_Lock if the lock is made using per-task lock.
   --  This information is needed so that Unlock_Interrupt
   --  performs unlocking on the same lock. The situation we are preventing
   --  is, for example, when Attach_Handler is called for the first time
   --  we lock L and create an Server_Task. For a matching unlocking, if we
   --  rely on the fact that there is a Server_Task, we will unlock the
   --  per-task lock.

   procedure Unlock_Interrupt (Interrupt : Interrupt_ID);

   function Is_Registered (Handler : Parameterless_Handler) return boolean;

   --------------------
   -- Lock_Interrupt --
   --------------------

   procedure Lock_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Tasking.Abortion.Defer_Abortion;

      POP.Write_Lock (L'Access);
      if Task_Lock (Interrupt) then
      --  We need to use per-task lock.
         POP.Unlock (L'Access);
         POP.Write_Lock (Server_ID (Interrupt));
         --  rely on the fact that once Server_ID is set to a non-null
         --  value it will never be set back to null.
      elsif Server_ID (Interrupt) /= Null_Task then
      --  We need to use per-task lock.
         Task_Lock (Interrupt) := true;
         POP.Unlock (L'Access);
         POP.Write_Lock (Server_ID (Interrupt));
      end if;
   end Lock_Interrupt;

   ----------------------
   -- Unlock_Interrupt --
   ----------------------

   procedure Unlock_Interrupt (Interrupt : Interrupt_ID) is
   begin
      if Task_Lock (Interrupt) then
         POP.Unlock (Server_ID (Interrupt));
      else
         POP.Unlock (L'Access);
      end if;
      Tasking.Abortion.Undefer_Abortion;
   end Unlock_Interrupt;

   ----------------------------------
   --  Register_Interrupt_Handler  --
   ----------------------------------

   procedure Register_Interrupt_Handler
     (Handler_Addr : System.Address) is
      New_Node_Ptr : R_Link;
      Ptr          : R_Link;

   begin
      --  This routine registers the Handler as usable for Dynamic
      --  Interrupt Handler. Routines attaching and detaching Handler
      --  dynamically should first consult if the Handler is rgistered.
      --  A Program Error should be raised if it is not registered.

      --  The pragma Interrupt_Handler can only appear in the library
      --  level PO definition and instantiation. Therefore, we do not need
      --  to implement Unregistering operation. Neither we need to
      --  protect the queue structure using a Lock.

      pragma Assert (Handler_Addr /= System.Null_Address or else
        Shutdown
          ("Interrupt Failure---a null handler should not be registered"));

      New_Node_Ptr := new Registered_Handler;
      New_Node_Ptr.H := Handler_Addr;

      if Registered_Handler_Head = null then
         Registered_Handler_Head := New_Node_Ptr;
         Registered_Handler_Tail := New_Node_Ptr;
      else
         Registered_Handler_Tail.Next := New_Node_Ptr;
         Registered_Handler_Tail := New_Node_Ptr;
      end if;

   end Register_Interrupt_Handler;

   -------------------
   -- Is_Registered --
   -------------------

   --  See if the Handler has been "pragma"ed using Interrupt_Handler.
   --  Always consider a null handler as registered.

   function Is_Registered (Handler : Parameterless_Handler) return boolean is
      type Fat_Ptr is record
         Object_Addr  : System.Address;
         Handler_Addr : System.Address;
      end record;
      function To_Fat_Ptr is new Unchecked_Conversion
        (Parameterless_Handler, Fat_Ptr);
      Ptr : R_Link;

      Fat : Fat_Ptr;
   begin
      if Handler = null then
         return true;
      end if;

      Fat := To_Fat_Ptr (Handler);

      Ptr := Registered_Handler_Head;

      while (Ptr /= null) loop
         if Ptr.H = Fat.Handler_Addr then
            return true;
         end if;
         Ptr := Ptr.Next;
      end loop;
      return false;

   end Is_Registered;

   -----------------
   -- Is_Reserved --
   -----------------

   function Is_Reserved (Interrupt : Interrupt_ID) return Boolean is
   begin
      return IMNG.Reserve (IMNG.Interrupt_ID (Interrupt));
   end Is_Reserved;

   -----------------------
   -- Is_Entry_Attached --
   -----------------------

   function Is_Entry_Attached (Interrupt : Interrupt_ID) return Boolean is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      return User_Entry (Interrupt).T /= Null_Task;
   end Is_Entry_Attached;

   -------------------------
   -- Is_Handler_Attached --
   -------------------------

   function Is_Handler_Attached (Interrupt : Interrupt_ID) return Boolean is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      return User_Handler (Interrupt).H /= null;
   end Is_Handler_Attached;

   ----------------
   -- Is_Blocked --
   ----------------

   function Is_Blocked (Interrupt : Interrupt_ID) return boolean is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      return Blocked (Interrupt);
   end Is_Blocked;

   ----------------
   -- Is_Ignored --
   ----------------

   function Is_Ignored (Interrupt : Interrupt_ID) return boolean is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      return Ignored (Interrupt);
   end Is_Ignored;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (Interrupt : Interrupt_ID)
     return Parameterless_Handler is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      return User_Handler (Interrupt).H;
   end Current_Handler;

   --------------------
   -- Attach_Handler --
   --------------------

   --  Calling this procedure with New_Handler = null and Static = true
   --  means we want to Detach the current handler regardless of
   --  the previous handler's binding status (ie. do not care if
   --  it is a dynamic or static handler).
   --  This option is needed so that during the finalization of a PO, we can
   --  detach handlers attached through pragma Attach_Handler.

   procedure Attach_Handler
     (New_Handler : in Parameterless_Handler;
      Interrupt   : in Interrupt_ID;
      Static      : in Boolean := False)
   is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Interrupt_Manager.Attach_Handler (New_Handler, Interrupt, Static);

   end Attach_Handler;

   ----------------------
   -- Exchange_Handler --
   ----------------------

   --  Calling this procedure with New_Handler = null and Static = true
   --  means we want to Detach the current handler regardless of
   --  the previous handler's binding status (ie. do not care if
   --  it is a dynamic or static handler).
   --  This option is needed so that during the finalization of a PO, we can
   --  detach handlers attached through pragma Attach_Handler.

   procedure Exchange_Handler
     (Old_Handler : out Parameterless_Handler;
      New_Handler : in Parameterless_Handler;
      Interrupt   : in Interrupt_ID;
      Static      : in Boolean := False)
   is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Interrupt_Manager.Exchange_Handler
        (Old_Handler, New_Handler, Interrupt, Static);

   end Exchange_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   --  Calling this procedure with Static = true
   --  means we want to Detach the current handler regardless of
   --  the previous handler's binding status (ie. do not care if
   --  it is a dynamic or static handler).
   --  This option is needed so that during the finalization of a PO, we can
   --  detach handlers attached through pragma Attach_Handler.

   procedure Detach_Handler
     (Interrupt : in Interrupt_ID;
      Static    : in Boolean := False)
   is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Interrupt_Manager.Detach_Handler (Interrupt, Static);

   end Detach_Handler;

   ---------------
   -- Reference --
   ---------------

   function Reference (Interrupt : Interrupt_ID) return System.Address is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      return Storage_Elements.To_Address
        (Storage_Elements.Integer_Address (Interrupt));
   end Reference;

   -----------------------------
   -- Bind_Interrupt_To_Entry --
   -----------------------------

   --  This procedure raises a Program_Error if it tries to
   --  bind an interrupt to which an Entry or a Procedure is
   --  already bound.

   procedure Bind_Interrupt_To_Entry
     (T       : Task_ID;
      E       : Task_Entry_Index;
      Int_Ref : System.Address)
   is
      Interrupt   : constant Interrupt_ID :=
        Interrupt_ID (Storage_Elements.To_Integer (Int_Ref));

   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Interrupt_Manager.Bind_Interrupt_To_Entry (T, E, Interrupt);

   end Bind_Interrupt_To_Entry;

   ------------------------------
   -- Detach_Interrupt_Entries --
   ------------------------------

   procedure Detach_Interrupt_Entries (T : Task_ID) is
   begin

      Interrupt_Manager.Detach_Interrupt_Entries (T);

   end Detach_Interrupt_Entries;

   ---------------------
   -- Block_Interrupt --
   ---------------------

   procedure Block_Interrupt (Interrupt : Interrupt_ID) is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Interrupt_Manager.Block_Interrupt (Interrupt);

   end Block_Interrupt;

   -----------------------
   -- Unblock_Interrupt --
   -----------------------

   procedure Unblock_Interrupt (Interrupt : Interrupt_ID) is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Interrupt_Manager.Unblock_Interrupt (Interrupt);
   end Unblock_Interrupt;

   ------------------
   -- Unblocked_By --
   ------------------

   function Unblocked_By (Interrupt : Interrupt_ID)
     return System.Tasking.Task_ID is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      return Last_Unblocker (Interrupt);
   end Unblocked_By;

   ----------------------
   -- Ignore_Interrupt --
   ----------------------

   procedure Ignore_Interrupt (Interrupt : Interrupt_ID) is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Interrupt_Manager.Ignore_Interrupt (Interrupt);
   end Ignore_Interrupt;

   ------------------------
   -- Unignore_Interrupt --
   ------------------------

   procedure Unignore_Interrupt (Interrupt : Interrupt_ID) is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Interrupt_Manager.Unignore_Interrupt (Interrupt);
   end Unignore_Interrupt;

   -----------------------
   -- Interrupt_Manager --
   -----------------------

   task body Interrupt_Manager is

      ---------------------
      --  Local Variable --
      ---------------------

      Intwait_Mask  : aliased IMNG.Interrupt_Mask;
      Ret_Interrupt : Interrupt_ID;

      ---------------------
      --  Local Routines --
      ---------------------

      procedure Bind_Handler (Interrupt : Interrupt_ID);
      --  This procedure does not do anything if the Interrupt is blocked.
      --  Otherwise, we have to interrupt Server_Task for status change through
      --  Wakeup interrupt.

      procedure Unbind_Handler (Interrupt : Interrupt_ID);
      --  This procedure does not do anything if the Interrupt is blocked.
      --  Otherwise, we have to interrupt Server_Task for status change
      --  through abort interrupt.

      --  Following two procedure are named Unproetcted... in order to
      --  indicate that Lock/Unlock_Interrupt operations are needed around.

      procedure Unprotected_Exchange_Handler
        (Old_Handler : out Parameterless_Handler;
         New_Handler : in  Parameterless_Handler;
         Interrupt   : in  Interrupt_ID;
         Static      : in  Boolean);

      procedure Unprotected_Detach_Handler
        (Interrupt   : in Interrupt_ID;
         Static      : in Boolean);

      procedure Bind_Handler (Interrupt : Interrupt_ID) is
      begin
         if not Blocked (Interrupt) then
            IMOP.Thread_Block_Interrupt (IMNG.Interrupt_ID (Interrupt));
            --  Mask this task for the given Interrupt so that all tasks
            --  are masked for the Interrupt and the actuall delivery of the
            --  Interrupt will be caught using "sigwait" by the
            --  corresponding Server_Task.

            POP.Wakeup (Server_ID (Interrupt));
            --  we have installed a Handler or an Entry before we called
            --  this procedure. If the Handler Task is waiting to be awakened,
            --  do it here. Otherwise, the interrupt will be discarded.
         end if;

      end Bind_Handler;

      procedure Unbind_Handler (Interrupt : Interrupt_ID) is
      begin
         if not Blocked (Interrupt) then

            POP.Abort_Task (Server_ID (Interrupt));
            --  Currently, there is a Handler or an Entry attached and
            --  corresponding Server_Task is waiting on "sigwait."
            --  We have to wake up the Server_Task and make it
            --  wait on condition variable by sending an
            --  Abort_Task_Interrupt

            Ret_Interrupt :=
              Interrupt_ID (IMOP.Interrupt_Wait (Intwait_Mask'Access));
            --  Make sure corresponding Server_Task is out of its own
            --  sigwait state.
            pragma Assert
              (Ret_Interrupt = Interrupt_ID (IMNG.Abort_Task_Interrupt)
                or else Shutdown ("Interrupt Failure---Interrupt_Wait"));

            IMOP.Install_Default_Action (IMNG.Interrupt_ID (Interrupt));
            --  We need to install default action.

            IMOP.Thread_Unblock_Interrupt (IMNG.Interrupt_ID (Interrupt));
            --  unmake the Interrupt for this task in order to
            --  allow default action again.

         else
            IMOP.Install_Default_Action (IMNG.Interrupt_ID (Interrupt));
            --  We need to install default action.
         end if;

      end Unbind_Handler;

      procedure Unprotected_Exchange_Handler
        (Old_Handler : out Parameterless_Handler;
         New_Handler : in  Parameterless_Handler;
         Interrupt   : in  Interrupt_ID;
         Static      : in  Boolean)
      is
      begin
         if User_Entry (Interrupt).T /= Null_Task then
         --  In case we have an Interrupt Entry already installed.
         --  raise a program error. (propagate it to the caller).
            Unlock_Interrupt (Interrupt);
            raise Program_Error;
         end if;

         --  Note : A null handler with Static = true will
         --  pass the following check. That is the case when we want to
         --  Detach a handler regardless of the Static status
         --  of the current_Handler.

         if not Static and then
           (User_Handler (Interrupt).Static
            --  tries to overwrite a static Interrupt Handler with a
            --  dynamic Handler
           or else not Is_Registered (New_Handler)) then
            --  The new handler is not specified as an Interrupt
            --  Handler by a pragma.

            Unlock_Interrupt (Interrupt);
            raise Program_Error;
         end if;

         Ignored (Interrupt) := false;
         --  The interrupt should no longer be ingnored if
         --  it was ever ignored.

         --  Save the old handler
         Old_Handler := User_Handler (Interrupt).H;

         --  The new handler
         User_Handler (Interrupt).H := New_Handler;

         if New_Handler = null then
            --  the null handler means we are detaching the handler.
            User_Handler (Interrupt).Static := False;
         else
            User_Handler (Interrupt).Static := Static;
         end if;

         --  Invoke a corresponding Server_Task if not yet created.
         --  Place Task_ID info in Server_ID array.
         if Server_ID (Interrupt) = Null_Task then
            Access_Hold := new Server_Task (Interrupt);
            Server_ID (Interrupt) := To_System (Access_Hold.all'Identity);
         end if;

         if (New_Handler = null) then
            if Old_Handler /= null then
               Unbind_Handler (Interrupt);
            end if;
            return;
         end if;

         if Old_Handler = null then
            Bind_Handler (Interrupt);
         end if;

      end Unprotected_Exchange_Handler;

      procedure Unprotected_Detach_Handler
        (Interrupt   : in Interrupt_ID;
         Static      : in Boolean)
      is
         Old_Handler : Parameterless_Handler;
      begin
         if User_Entry (Interrupt).T /= Null_Task then
         --  In case we have an Interrupt Entry installed.
         --  raise a program error. (propagate it to the caller).
            Unlock_Interrupt (Interrupt);
            raise Program_Error;
         end if;

         --  Note : Static = true will
         --  pass the following check. That is the case when we want to
         --  Detach a handler regardless of the Static status
         --  of the current_Handler.

         if not Static and then User_Handler (Interrupt).Static then
         --  tries to detach a static Interrupt Handler.
         --  raise a program error.
            Unlock_Interrupt (Interrupt);
            raise Program_Error;
         end if;

         Ignored (Interrupt) := false;
         --  The interrupt should no longer be ingnored if
         --  it was ever ignored.

         Old_Handler := User_Handler (Interrupt).H;

         --  The new handler
         User_Handler (Interrupt).H := null;
         User_Handler (Interrupt).Static := False;

         if Old_Handler /= null then
            Unbind_Handler (Interrupt);
         end if;

      end Unprotected_Detach_Handler;

   begin
      System.Tasking.Utilities.Make_Independent;
      --  By making this task independent of master, when the process
      --  goes away, the Interrupt_Manager will terminate gracefully.

      --  Rendezvous with environement task.
      --  Before rendezvous, environment task gets its own interrupt mask,
      --  saves that, and then masks all interrupts except the
      --  Keep_Unmasked set.
      --  During rendezvous, the Interrupt_Manager receives the old
      --  interrupt mask of the environment task, and sets its own
      --  interrupt mask to that value.
      --  The environment task will call the entry of Interrupt_Manager some
      --  during elaboration of the body of this package.

      accept Initialize (Mask : IMNG.Interrupt_Mask) do
         declare
            The_Mask : aliased IMNG.Interrupt_Mask;

         begin
            IMOP.Copy_Interrupt_Mask (The_Mask, Mask);
            IMOP.Set_Interrupt_Mask (The_Mask'Access);
         end;
      end Initialize;

      --  Note: All tasks in RTS will have all the Reserve Interrupts
      --   being masked (except the Interrupt_Manager) and Keep_Unmasked
      --   unmasked when created.
      --   Abort_Task_Interrupt is one of the Interrupt unmasked
      --   in all tasks. We mask the Interrupt in this particular task
      --   so that "sigwait" is possible to catch an explicitely sent
      --   Abort_Task_Interrupt from the Server_Tasks.
      --   This sigwaiting is needed so that we make sure a Server_Task is
      --   out of its own sigwait state. This extra synchronization is
      --   necessary to prevent following senarios.
      --   1) Interrupt_Manager sends an Abort_Task_Interrupt to the
      --      Server_Task then changes its own interrupt mask (OS level).
      --      If an interrupt (corresponding to the Server_Task) arrives
      --      in the nean time we have the Interrupt_Manager umnasked and
      --      the Server_Task waiting on sigwait.
      --   2) For unbinding handler, we install a default action in the
      --      Interrupt_Manager. POSIX.1c states that the result of using
      --      "sigwait" and "sigaction" simaltaneously on the same interrupt
      --      is undefined. Therefore, we need to be informed from the
      --      Server_Task of the fact that the Server_Task is out of its
      --      sigwait stage.

      IMOP.Empty_Interrupt_Mask (Intwait_Mask'Access);
      IMOP.Add_To_Interrupt_Mask
        (Intwait_Mask'Access, IMNG.Interrupt_ID (IMNG.Abort_Task_Interrupt));
      IMOP.Thread_Block_Interrupt
        (IMNG.Interrupt_ID (IMNG.Abort_Task_Interrupt));

      loop

         declare
            Old_Handler : Parameterless_Handler;

         begin
         --  A block is needed to absorb Program_Error exception.
            select

            accept Attach_Handler
               (New_Handler : in Parameterless_Handler;
                Interrupt   : in Interrupt_ID;
                Static      : in Boolean)
            do
               Lock_Interrupt (Interrupt);

               Unprotected_Exchange_Handler
                 (Old_Handler, New_Handler, Interrupt, Static);

               Unlock_Interrupt (Interrupt);

            end Attach_Handler;

            or accept Exchange_Handler
               (Old_Handler : out Parameterless_Handler;
                New_Handler : in Parameterless_Handler;
                Interrupt   : in Interrupt_ID;
                Static      : in Boolean)
            do
               Lock_Interrupt (Interrupt);

               Unprotected_Exchange_Handler
                 (Old_Handler, New_Handler, Interrupt, Static);

               Unlock_Interrupt (Interrupt);

            end Exchange_Handler;

            or accept Detach_Handler
               (Interrupt   : in Interrupt_ID;
                Static      : in Boolean)
            do
               Lock_Interrupt (Interrupt);

               Unprotected_Detach_Handler (Interrupt, Static);

               Unlock_Interrupt (Interrupt);
            end Detach_Handler;

            or accept Bind_Interrupt_To_Entry
              (T       : Task_ID;
               E       : Task_Entry_Index;
               Interrupt : Interrupt_ID)
            do
               Lock_Interrupt (Interrupt);

               if User_Handler (Interrupt).H /= null
                 or else User_Entry (Interrupt).T /= Null_Task then
               --  if there is a binding already (either a Procedure or an
               --  Entry), raise Program_Error. (propagate it to the caller).
                  Unlock_Interrupt (Interrupt);
                  raise Program_Error;
               end if;

               Ignored (Interrupt) := false;
               --  The interrupt should no longer be ingnored if
               --  it was ever ignored.

               User_Entry (Interrupt) := Entry_Assoc' (T => T, E => E);

               T.Interrupt_Entry := true;
               --  Indicate the attachment of Interrupt Entry in ATCB.
               --  This is need so that when an Interrupt Entry task terminates
               --  the binding can be cleaned. The call to unbinding is made
               --  during Terminate_Dependents (See s-tasuti.adb).

               --  Invoke a corresponding Server_Task if not yet created.
               --  Place Task_ID info in Server_ID array.
               if Server_ID (Interrupt) = Null_Task then
                  Access_Hold := new Server_Task (Interrupt);
                  Server_ID (Interrupt) :=
                    To_System (Access_Hold.all'Identity);
               end if;

               Bind_Handler (Interrupt);

               Unlock_Interrupt (Interrupt);

            end Bind_Interrupt_To_Entry;

            or accept Detach_Interrupt_Entries (T : Task_ID)
            do
               for I in Interrupt_ID'Range loop
                  if not Is_Reserved (I) then
                     Lock_Interrupt (I);
                     if User_Entry (I).T = T then
                        Ignored (I) := false;
                        --  The interrupt should no longer be ingnored if
                        --  it was ever ignored.
                        User_Entry (I) := Entry_Assoc'
                          (T => Null_Task, E => Null_Task_Entry);
                        Unbind_Handler (I);
                     end if;
                     Unlock_Interrupt (I);
                  end if;
               end loop;

               T.Interrupt_Entry := false;
               --  Indicate in ATCB that no Interrupt Entries are attached.

            end Detach_Interrupt_Entries;

            or accept Block_Interrupt (Interrupt : Interrupt_ID) do
               Lock_Interrupt (Interrupt);

               if Blocked (Interrupt) then
                  Unlock_Interrupt (Interrupt);
                  return;
               end if;

               Blocked (Interrupt) := True;
               Last_Unblocker (Interrupt) := Null_Task;

               IMOP.Thread_Block_Interrupt (IMNG.Interrupt_ID (Interrupt));
               --  Mask this task for the given Interrupt so that all tasks
               --  are masked for the Interrupt.

               if User_Handler (Interrupt).H /= null
                 or else  User_Entry (Interrupt).T /= Null_Task then
                  --  this is the case where the Server_Task is waiting on
                  --  "sigwait." Wake it up by sending an Abort_Task_Interrupt
                  --  so that the Server_Task waits on Cond.

                  POP.Abort_Task (Server_ID (Interrupt));

                  Ret_Interrupt :=
                    Interrupt_ID (IMOP.Interrupt_Wait (Intwait_Mask'Access));
                  --  Make sure corresponding Server_Task is out of its own
                  --  sigwait state.

                  pragma Assert
                    (Ret_Interrupt = Interrupt_ID (IMNG.Abort_Task_Interrupt)
                      or else Shutdown ("Interrupt Failure---Interrupt_Wait"));
               end if;

               Unlock_Interrupt (Interrupt);

            end Block_Interrupt;

            or accept Unblock_Interrupt (Interrupt : Interrupt_ID) do
               Lock_Interrupt (Interrupt);

               if not Blocked (Interrupt) then
                  Unlock_Interrupt (Interrupt);
                  return;
               end if;

               Blocked (Interrupt) := False;
               Last_Unblocker (Interrupt)
                 := To_System (Unblock_Interrupt'Caller);

               if User_Handler (Interrupt).H = null
                 and then User_Entry (Interrupt).T = Null_Task then
                  --  No handler is attached. Unmask the Interrupt so that
                  --  the default action can be carried out.
                  IMOP.Thread_Unblock_Interrupt
                    (IMNG.Interrupt_ID (Interrupt));
               else
                  POP.Wakeup (Server_ID (Interrupt));
                  --  The Server_Task must be waiting on the Cond variable
                  --  since it was being blocked and an Interrupt Hander or
                  --  an Entry was there. Wake it up and let it change
                  --  it place of waiting according to its new state.
               end if;

               Unlock_Interrupt (Interrupt);

            end Unblock_Interrupt;

            or accept Ignore_Interrupt (Interrupt : Interrupt_ID) do
               Lock_Interrupt (Interrupt);

               if Ignored (Interrupt) then
                  Unlock_Interrupt (Interrupt);
                  return;
               end if;

               Ignored (Interrupt) := true;

               --  if there is a handler associated with the Interrupt,
               --  detach it first. In this way we make sure that the
               --  Server_Task is not on sigwait. This is legal since
               --  Unignore_Interrupt is to install the default action.
               if User_Handler (Interrupt).H /= null then
                  Unprotected_Detach_Handler
                    (Interrupt => Interrupt, Static => True);
               elsif User_Entry (Interrupt).T /= Null_Task then
                  User_Entry (Interrupt) := Entry_Assoc'
                    (T => Null_Task, E => Null_Task_Entry);
                  Unbind_Handler (Interrupt);
               end if;

               IMOP.Install_Ignore_Action (IMNG.Interrupt_ID (Interrupt));

               Unlock_Interrupt (Interrupt);
            end Ignore_Interrupt;

            or accept Unignore_Interrupt (Interrupt : Interrupt_ID) do
               Lock_Interrupt (Interrupt);

               Ignored (Interrupt) := false;

               --  if there is a handler associated with the Interrupt,
               --  detach it first. In this way we make sure that the
               --  Server_Task is not on sigwait. This is legal since
               --  Unignore_Interrupt is to install the default action.
               if User_Handler (Interrupt).H /= null then
                  Unprotected_Detach_Handler
                    (Interrupt => Interrupt, Static => True);
               elsif User_Entry (Interrupt).T /= Null_Task then
                  User_Entry (Interrupt) := Entry_Assoc'
                    (T => Null_Task, E => Null_Task_Entry);
                  Unbind_Handler (Interrupt);
               end if;

               IMOP.Install_Default_Action (IMNG.Interrupt_ID (Interrupt));

               Unlock_Interrupt (Interrupt);
            end Unignore_Interrupt;

            end select;
         exception
            when Program_Error =>
            --  if there is a program error we just want to propagate it to
            --  the caller and do not want to stop this task.
               null;
            when others =>
               pragma Assert
                 (Shutdown ("Interrupt_Manager---exception not expected"));
               null;
         end;
      end loop;
      pragma Assert (Shutdown ("Interrupt_Manager---should not get here"));

   end Interrupt_Manager;

   -----------------
   -- Server_Task --
   -----------------

   task body Server_Task is
      Intwait_Mask    : aliased IMNG.Interrupt_Mask;
      Ret_Interrupt   : Interrupt_ID;
      Self_ID         : Task_ID := Self;
      Tmp_Handler     : Parameterless_Handler;
      Tmp_ID          : Task_ID;
      Tmp_Entry_Index : Task_Entry_Index;
   begin

      System.Tasking.Utilities.Make_Independent;
      --  By making this task independent of master, when the process
      --  goes away, the Server_Task will terminate gracefully.

      IMOP.Install_Default_Action (IMNG.Interrupt_ID (Interrupt));
      --  Install default action in system level.

      --  Note: All tasks in RTS will have all the Reserve Interrupts
      --   being masked (except the Interrupt_Manager) and Keep_Unmasked
      --   unmasked when created.
      --   Abort_Task_Interrupt is one of the Interrupt unmasked
      --   in all tasks. We mask the Interrupt in this particular task
      --   so that "sigwait" is possible to catch an explicitely sent
      --   Abort_Task_Interrupt from the Interrupt_Manager.
      --   There are two Interrupt interrupts that this task catch through
      --   "sigwait." One is the Interrupt this task is designated to catch
      --   in order to execure user handler or entry. The other one is the
      --   Abort_Task_Interrupt. This interrupt is being sent from the
      --   Interrupt_Manager to inform status changes (e.g: become Blocked,
      --   Handler or Entry is to be detached).

      --  Prepare a mask to used for sigwait.

      IMOP.Empty_Interrupt_Mask (Intwait_Mask'Access);
      IMOP.Add_To_Interrupt_Mask
        (Intwait_Mask'Access, IMNG.Interrupt_ID (Interrupt));
      IMOP.Add_To_Interrupt_Mask
        (Intwait_Mask'Access, IMNG.Interrupt_ID (IMNG.Abort_Task_Interrupt));

      IMOP.Thread_Block_Interrupt
        (IMNG.Interrupt_ID (IMNG.Abort_Task_Interrupt));

      POP.Write_Lock (Self_ID);

      loop
         if User_Handler (Interrupt).H = null
           and then User_Entry (Interrupt).T = Null_Task then
            --  No Interrupt binding. If there is an interrupt,
            --  Interrupt_Manager will take default action.

            POP.Sleep (Self_ID);

         elsif Blocked (Interrupt) then
            --  Interrupt is blocked.
            --  Stay here, so we won't catch the Interrupt.

            POP.Sleep (Self_ID);

         else
         --  A Handler or an Entry is installed. At this point all tasks mask
         --  for the Interrupt is masked. Catch the Interrupt using sigwait.
            --  This task may wake up from sigwait by receiving an interrupt
            --  (Abort_Task_Interrupt) from the Interrupt_Manager for unbinding
            --  a Procedure Handler or an Entry. Or it could be a wake up
            --  from status change (Unblocked -> Blocked). If that is not
            --  the case, we should exceute the attached Procedure or Entry.

            POP.Unlock (Self_ID);

            Ret_Interrupt :=
              Interrupt_ID (IMOP.Interrupt_Wait (Intwait_Mask'Access));

            if Ret_Interrupt = Interrupt_ID (IMNG.Abort_Task_Interrupt) then
               POP.Abort_Task (Interrupt_Manager_ID);
               --  Inform the Interrupt_Manager of wakeup from above sigwait.
               POP.Write_Lock (Self_ID);
            else
               pragma Assert
                 (Ret_Interrupt = Interrupt
                   or else Shutdown ("Interrupt Failure---Interrupt_Wait"));
               null;

               POP.Write_Lock (Self_ID);

               --  Even though we have received an Interrupt the status may
               --  have changed already before we got the Self_ID lock above.
               --  Therefore we make sure a Handler or an Entry is still
               --  there and make appropriate call.
               --  If there is no calls to make we need to regenerate the
               --  Interrupt in order not to lose it.

               if User_Handler (Interrupt).H /= null then
                  Tmp_Handler := User_Handler (Interrupt).H;
                  POP.Unlock (Self_ID);
                  --  RTS calls should not be made self being locked.
                  Tmp_Handler.all;
                  POP.Write_Lock (Self_ID);
               elsif User_Entry (Interrupt).T /= Null_Task then
                  Tmp_ID := User_Entry (Interrupt).T;
                  Tmp_Entry_Index := User_Entry (Interrupt).E;
                  POP.Unlock (Self_ID);
                  --  RTS calls should not be made self being locked.
                  System.Tasking.Rendezvous.Call_Simple
                    (Tmp_ID, Tmp_Entry_Index, System.Null_Address);
                  POP.Write_Lock (Self_ID);
               else
                  IMOP.Interrupt_Self_Process (IMNG.Interrupt_ID (Interrupt));
                  --  This is a situation that this task wake up
                  --  receiving an Interrupt and before it get the lock
                  --  the Interrupt is blocked. We do not
                  --  want to lose the interrupt in this case so that
                  --  regenerate the Interrupt to process level;
               end if;
            end if;

         end if;
      end loop;
      pragma Assert (Shutdown ("Server_Task---should not get here"));
   end Server_Task;

begin
   declare
      New_Mask : aliased IMNG.Interrupt_Mask;
      Old_Mask : aliased IMNG.Interrupt_Mask;

   begin

      --  Initialize the lock L.
      POP.Initialize_Lock (L'Access);

      --  During the elaboration of this package body we want RTS to
      --  inherit the interrupt mask from the Environment Task.
      --  The Environment Task should have gotten its mask from
      --  the enclosing process during the RTS start up. (See Initialize
      --  in s-taprop.adb).

      IMOP.Get_Interrupt_Mask (Old_Mask'Access);
      --  Get the inherited Interrupt_Mask of the Environment Task.

      IMOP.Fill_Interrupt_Mask (New_Mask'Access);
      --  Prepare a new mask for the Environment Task (All blocked initially).

      for Interrupt in Interrupt_ID'Range loop

         if IMNG.Keep_Unmasked (IMNG.Interrupt_ID (Interrupt)) then
            --  Unmask necessary Interrupts from the New_Mask

            IMOP.Delete_From_Interrupt_Mask
              (New_Mask'Access, IMNG.Interrupt_ID (Interrupt));

         elsif not Is_Reserved (Interrupt) then

            if IMOP.IS_Member
              (Old_Mask'Access, IMNG.Interrupt_ID (Interrupt)) then
               Blocked (Interrupt) := True;
            end if;

         end if;

      end loop;

      IMOP.Set_Interrupt_Mask (New_Mask'Access);
      --  mask all the non-Keep_Masked Interrupts in the Environment Task

      Interrupt_Manager_ID := To_System (Interrupt_Manager'Identity);
      --  Get Interrupt_Manager's ID so that Abort_Interrupt can be sent.

      Interrupt_Manager.Initialize (Old_Mask);
      --  Pass the Interrupt_Mask of the Envoronmental Task to the
      --  Interrupt_Manager.

   end;
end System.Interrupts;

------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.1 $                             --
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

--  This version is for HPUX 10.x

with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Ada.Interrupts; use Ada.Interrupts;

with System.Storage_Elements;
with Interfaces.C.POSIX_RTE;
with Interfaces.C.POSIX_Error;
with Interfaces.C.Pthreads;
with System.Task_Primitives; use System.Task_Primitives;
with System.Tasking;
with System.Tasking.Rendezvous;
with System.Tasking.Utilities;
with System.Error_Reporting; use System.Error_Reporting;

with Unchecked_Conversion;

package body System.Interrupts is

   package RTE renames Interfaces.c.POSIX_RTE;

   package POSIX_Error renames Interfaces.C.POSIX_Error;
   use type POSIX_Error.Return_Code;

   package Utilities renames System.Tasking.Utilities;

   Failure : Interfaces.C.POSIX_Error.Return_Code
      renames Interfaces.C.POSIX_Error.Failure;

   --  Locks and Cond variables for each Interrupt

   M : array (Ada.Interrupts.Interrupt_ID'Range) of Lock;

   C : array (Ada.Interrupts.Interrupt_ID'Range) of Condition_Variable;

   --  Interrupts to which a Handler or an Entry can be bound
   Usable_Interrupts : array (Ada.Interrupts.Interrupt_ID'Range) of Boolean;

   type Handler_Assoc is record
      H      : Ada.Interrupts.Parameterless_Handler;
      Static : Boolean;   --  Indicates static binding;
   end record;

   Null_Handler_Assoc : constant Handler_Assoc := Handler_Assoc'
     (H => null, Static => false);

   --  Table to maintain current Interrupt Handler binding
   User_Handlers :
     array (Ada.Interrupts.Interrupt_ID'Range) of Handler_Assoc
       := (others => Null_Handler_Assoc);

   type Entry_Assoc is record
      T : Tasking.Task_ID;
      E : Tasking.Task_Entry_Index;
   end record;

   Null_Entry_Assoc : constant Entry_Assoc := Entry_Assoc'
     (T => Tasking.Null_Task, E => Tasking.Null_Task_Entry);

   --  Table to maintain current Interrupt Entry binding
   User_Entries : array (Ada.Interrupts.Interrupt_ID'Range) of Entry_Assoc
     := (others => Null_Entry_Assoc);

   --  Table to maintain Task_ID of Handler_Task for each Interrupts.
   Handler_Task_IDs :
     array (Ada.Interrupts.Interrupt_ID'Range) of System.Tasking.Task_ID
       := (others => System.Tasking.Null_Task);

   --  Table to maintain the information if a signal is blocked.
   Status_Blocked :
     array (Ada.Interrupts.Interrupt_ID'Range) of Boolean
       := (others => false);

   --  Type and Head, Tail of the list containing Registered Interrupt
   --  Handlers.

   type Registered_Handler;
   type R_Link is access all Registered_Handler;

   type Registered_Handler is record
      H :    Ada.Interrupts.Parameterless_Handler := null;
      Next : R_Link := null;
   end record;

   Registered_Handler_Head : R_Link := null;
   Registered_Handler_Tail : R_Link := null;


   task Handler_Manager is
      entry Bind_Handler       (Interrupt : Ada.Interrupts.Interrupt_ID);
      entry Unbind_Handler     (Interrupt : Ada.Interrupts.Interrupt_ID);
      entry Block_Interrupt    (Interrupt : Ada.Interrupts.Interrupt_ID);
      entry Unblock_Interrupt  (Interrupt : Ada.Interrupts.Interrupt_ID);
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);
   end Handler_Manager;

   task type Handler_Task (Interrupt : Ada.Interrupts.Interrupt_ID) is
      pragma Interrupt_Priority (System.Interrupt_Priority'First);
   end Handler_Task;

   type Handler_Task_Access is access Handler_Task;

   Handler_Access :
     array (Ada.Interrupts.Interrupt_ID'Range) of  Handler_Task_Access
       := (others => null);


   --  local procedures

   ---------------------------
   -- Unmask_All_Interrupts --
   ---------------------------

   procedure Unmask_All_Interrupts;

   ----------------------------
   -- Thread_Block_Interrupt --
   ----------------------------

   procedure Thread_Block_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID);

   ------------------------------
   -- Thread_Unblock_Interrupt --
   ------------------------------

   procedure Thread_Unblock_Interrupt
     (Interrupt : Ada.Interrupts.Interrupt_ID);

   ----------------------------------
   -- Initialize_Usable_Interrupts --
   ----------------------------------

   procedure Initialize_Usable_Interrupts;

   --------------------
   -- User_Installed --
   --------------------

   --  return true if User_Handler or User_Entry is installed for the Interrupt
   function User_Installed (Interrupt : Ada.Interrupts.Interrupt_ID)
     return Boolean;

   -----------------
   -- Signal_Task --
   -----------------

   procedure Signal_Task
     (T : System.Tasking.Task_ID; Interrupt : Ada.Interrupts.Interrupt_ID);

   ----------------------------------
   -- Unprotected_Exchange_Handler --
   ----------------------------------

   procedure Unprotected_Exchange_Handler
     (Old_Handler : out Ada.Interrupts.Parameterless_Handler;
      New_Handler : in Ada.Interrupts.Parameterless_Handler;
      Interrupt   : in Ada.Interrupts.Interrupt_ID;
      Static      : in boolean := false);

   -------------------
   -- Is_Registered --
   -------------------

   --  See if the Handler has been "pragma"ed using Interrupt_Hanlder.
   --  Always consider a null handler as registered.

   function Is_Registered
     (Handler : Ada.Interrupts.Parameterless_Handler) return boolean;

   --  end of local procedure declarations.


   task body Handler_Manager is
      Default_Action : aliased RTE.struct_sigaction;
      Oact           : aliased RTE.struct_sigaction;
      Result         : Interfaces.C.POSIX_Error.Return_Code;
      Error          : Boolean;
   begin

      System.Tasking.Utilities.Make_Independent;

      Default_Action.sa_handler := Storage_Elements.To_Address (RTE.SIG_DFL);

      Unmask_All_Interrupts;
      --  Initially unmask all interrupts so that the default action
      --  is enforced.

      --  Notice : When a task is created it inherits its signal mask from the
      --  calling task and all usable interrupts are masked initially.
      --  (cf: Initialize_LL_Tasks, Create_LL_Task, LL_Wrapper in s-taspri.adb)

      loop

         select

         accept Bind_Handler (Interrupt : Ada.Interrupts.Interrupt_ID) do

            --  This entry is called only when the Interrupt is Unblocked on
            --  the process level.

            Thread_Block_Interrupt (Interrupt);
            --  Mask this task for the given Interrupt so that all tasks
            --  are masked for the Interrupt and the actuall delivery of the
            --  Interrupt will be caught using "sigwait" by the corresponding
            --  Handler_Task.

            Cond_Signal (C (Interrupt));
            --  we have installed a Handler or an Entry before we called
            --  this entry. If the Handler Task is waiting to be awakened,
            --  do it here. Otherwise, the signal will be discarded.

         end Bind_Handler;

         or accept Unbind_Handler (Interrupt : Ada.Interrupts.Interrupt_ID) do

            --  This entry is called only when the Interrupt is Unblocked on
            --  the process level.

            --  Currently, there is a Handler or an Entry attached and
            --  corresponding Hanlder_Task is waiting on "sigwait."

            Signal_Task (Handler_Task_IDs (Interrupt), Interrupt);
            --  We have to wake the Handler_Task up and make it
            --  wait on condition variable.

            RTE.sigaction
              (RTE.Signal (Interrupt),
               Default_Action'Access,
               Oact'Access,
               Result);
            pragma Assert (Result /= Failure or else
              Utilities.Runtime_Assert_Shutdown (
                "Interrupt Failure---sigaction"));
            --  restore the default action in case it is ruined.

            Thread_Unblock_Interrupt (Interrupt);
            --  unmake the Interrupt for this task in order to
            --  allow default action again.

         end Unbind_Handler;

         or accept Block_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID) do

            --  This entry is called only when the Interrupt is Unblocked on
            --  the process level.

            Thread_Block_Interrupt (Interrupt);
            --  Mask this task for the given Interrupt so that all tasks
            --  are masked for the Interrupt.

            if User_Installed (Interrupt) then
               --  this is the case where the Handler_Task is waiting on
               --  "sigwait." Wake it up and make it wait on Cond.
               Signal_Task (Handler_Task_IDs (Interrupt), Interrupt);
            end if;

         end Block_Interrupt;

         or accept
           Unblock_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID) do

            --  This entry is called only when the Interrupt is Blocked on
            --  the process level.

            if not User_Installed (Interrupt) then
               --  No handler is attached. Unmask the Interrupt so that
               --  the default action can be carried out.
               Thread_Unblock_Interrupt (Interrupt);
            end if;

            Cond_Signal (C (Interrupt));
            --  The Handler Task must be waiting on the Cond variable
            --  since it was being blocked. Wake it up and let it change
            --  it place of waiting according to its new state.
            --  If there is no Handler_Task being activated, this signal
            --  will be lost.

         end Unblock_Interrupt;

         end select;

      end loop;
   end Handler_Manager;

   task body Handler_Task is
      Sigwait_Mask   : aliased RTE.Signal_Set;
      Sigwait_Signal : RTE.Signal;
      Result         : Interfaces.C.POSIX_Error.Return_Code;
      Error          : Boolean;
   begin
      System.Tasking.Utilities.Make_Independent;
      --  By making this task independent of master, when the process
      --  goes away, the Handler_Task will terminate gracefully.

      Handler_Task_IDs (Interrupt) := System.Tasking.Self;
      --  Save the ID of this task so that others can explicitly
      --  send a signal to this task (thread) using Send_Signal (pthread_kill).

      RTE.sigemptyset (Sigwait_Mask'Access, Result);
      pragma Assert (Result /= Failure or else
        Utilities.Runtime_Assert_Shutdown ("Interrupt Failure---sigemptyset"));
      RTE.sigaddset (Sigwait_Mask'Access, RTE.Signal (Interrupt), Result);
      pragma Assert (Result /= Failure or else
        Utilities.Runtime_Assert_Shutdown ("Interrupt Failure---sigaddset"));

      loop
         if not User_Installed (Interrupt)
            --  No Interrupt binding. If there is an interrupt,
            --  Handler_Manager will take default action.

           or else Status_Blocked (Interrupt) then
            --  Interrupt is blocked.
            --  Stay here, so we won't catch the Interrupt.

            Write_Lock  (M (Interrupt), Error);
            Cond_Wait  (C (Interrupt), M (Interrupt));
            Unlock (M (Interrupt));

         else
            --  A Handler or an Entry is installed. At this point all tasks
            --  mask for the Interrupt is masked. Catch the Signal using
            --  "sigwait."

            Interfaces.C.Pthreads.sigwait
               (Sigwait_Mask, Sigwait_Signal, Result);
            pragma Assert (Result /= Failure or else
              Utilities.Runtime_Assert_Shutdown
                ("Interrupt Failure---sigwait"));

            --  This task may wake up from sigwait by receiving a signal
            --  from the Handler_Manager for unbinding a Interrupt Handler or
            --  an Entry. Or it could be a wake up from status change
            --  (Unblocked -> Blocked). If that is not the case, we should
            --  exceute the attached Procedure or Entry.

            if Status_Blocked (Interrupt) then
               null;
            elsif User_Handlers (Interrupt) /= Null_Handler_Assoc then
               User_Handlers (Interrupt).H.all;
            elsif User_Entries (Interrupt) /= Null_Entry_Assoc then
               System.Tasking.Rendezvous.Call_Simple
                 (User_Entries (Interrupt).T, User_Entries (Interrupt).E,
                  System.Null_Address);
            end if;

         end if;
      end loop;
   end Handler_Task;

   -----------------
   -- Is_Reserved --
   -----------------

   function Is_Reserved (Interrupt : Ada.Interrupts.Interrupt_ID)
     return Boolean is
   begin
      return not Usable_Interrupts (Interrupt);
   end Is_Reserved;

   -----------------
   -- Is_Attached --
   -----------------

   function Is_Attached (Interrupt : Ada.Interrupts.Interrupt_ID)
     return Boolean is
      Test  : Boolean;
      Error : Boolean;
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Write_Lock (M (Interrupt), Error);
      Test := User_Handlers (Interrupt) /= Null_Handler_Assoc;
      Unlock (M (Interrupt));
      return Test;
   end Is_Attached;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (Interrupt : Ada.Interrupts.Interrupt_ID)
     return Ada.Interrupts.Parameterless_Handler is

      Handler : Ada.Interrupts.Parameterless_Handler;
      Error   : Boolean;
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Write_Lock (M (Interrupt), Error);
      Handler := User_Handlers (Interrupt).H;
      Unlock (M (Interrupt));
      return Handler;
   end Current_Handler;

   --------------------
   -- Attach_Handler --
   --------------------

   --  Calling this procedure with New_Handler = null and Static = true
   --  means that we want to Detach the current handler regardless of
   --  the previous handler's binding status (ie. do not care if
   --  it is a dynamic or static handler).

   procedure Attach_Handler
     (New_Handler : in Ada.Interrupts.Parameterless_Handler;
      Interrupt   : in Ada.Interrupts.Interrupt_ID;
      Static      : in boolean := false) is

      Old_Handler : Ada.Interrupts.Parameterless_Handler;
      Error       : Boolean;
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Write_Lock (M (Interrupt), Error);

      --  In case we have an Interrupt Entry already installed,
      --  raise a program error.
      if User_Entries (Interrupt) /= Null_Entry_Assoc then
         Unlock (M (Interrupt));
         raise Program_Error;
      end if;

      if not Static and then
        (User_Handlers (Interrupt).Static or else
         --  tries to overwrite a static Interrupt Handler with a
         --  dynamic Handler
         not Is_Registered (New_Handler)) then
         --  The new handler is not specified as an Interrupt
         --  Handler by a pragma.

         Unlock (M (Interrupt));
         raise Program_Error;
      end if;

      Unprotected_Exchange_Handler
        (Old_Handler, New_Handler, Interrupt, Static);
      Unlock (M (Interrupt));
   end Attach_Handler;

   ----------------------
   -- Exchange_Handler --
   ----------------------

   --  Calling this procedure with New_Handler = null and Static = true
   --  means that we want to Detach the current handler regardless of
   --  the previous handler's binding status (ie. do not care if
   --  it is a dynamic or static handler).

   procedure Exchange_Handler
     (Old_Handler : out Ada.Interrupts.Parameterless_Handler;
      New_Handler : in Ada.Interrupts.Parameterless_Handler;
      Interrupt   : in Ada.Interrupts.Interrupt_ID;
      Static      : in boolean := false) is

      Error : Boolean;
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Write_Lock (M (Interrupt), Error);

      --  In case we have an Interrupt Entry already installed,
      --  raise a program error.
      if User_Entries (Interrupt) /= Null_Entry_Assoc then
         Unlock (M (Interrupt));
         raise Program_Error;
      end if;

      if not Static and then
        (User_Handlers (Interrupt).Static or else
         --  tries to overwrite a static Interrupt Handler with a
         --  dynamic Handler
         not Is_Registered (New_Handler)) then
         --  The new handler is not specified as an Interrupt
         --  Handler by a pragma.

         Unlock (M (Interrupt));
         raise Program_Error;
      end if;

      Unprotected_Exchange_Handler
        (Old_Handler, New_Handler, Interrupt, Static);
      Unlock (M (Interrupt));
   end Exchange_Handler;

   ----------------------------------
   -- Unprotected_Exchange_Handler --
   ----------------------------------

   procedure Unprotected_Exchange_Handler
     (Old_Handler : out Ada.Interrupts.Parameterless_Handler;
      New_Handler : in Ada.Interrupts.Parameterless_Handler;
      Interrupt   : in Ada.Interrupts.Interrupt_ID;
      Static      : in boolean := false) is
   begin

      --  Save the old handler
      Old_Handler := User_Handlers (Interrupt).H;

      --  The new handler
      User_Handlers (Interrupt).H := New_Handler;

      --  Consider null handler dynamic regardless of Static information.
      if New_Handler = null then
         User_Handlers (Interrupt).Static := false;
      else
         User_Handlers (Interrupt).Static := Static;
      end if;

      if Handler_Access (Interrupt) = null then
         --  if the Handler_Task is not yet created, do it now.
         Handler_Access (Interrupt) := new Handler_Task (Interrupt);
      end if;

      if Status_Blocked (Interrupt) then
         --  if the signal is currently blocked,
         --  no further operations are needed.
         return;
      end if;

      if (New_Handler = null) then
         if Old_Handler /= null then
            Handler_Manager.Unbind_Handler (Interrupt);
         end if;
         return;
      end if;

      if Old_Handler = null then
         Handler_Manager.Bind_Handler (Interrupt);
      end if;

   end Unprotected_Exchange_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   --  Calling this procedure with Static = true
   --  means that we want to Detach the current handler regardless of
   --  the previous handler's binding status (ie. do not care if
   --  it is a dynamic or static handler).

   procedure Detach_Handler
     (Interrupt : in Ada.Interrupts.Interrupt_ID;
      Static    : in boolean := false) is
      Old_Handler : Ada.Interrupts.Parameterless_Handler;
      Error : Boolean;
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Write_Lock (M (Interrupt), Error);

      --  In case we have an Interrupt Entry already installed,
      --  raise a program error.
      if User_Entries (Interrupt) /= Null_Entry_Assoc then
         Unlock (M (Interrupt));
         raise Program_Error;
      end if;

      if not Static and then User_Handlers (Interrupt).Static then
         --  tries to detach a static Interrupt Handler.

         Unlock (M (Interrupt));
         raise Program_Error;
      end if;

      Unprotected_Exchange_Handler (Old_Handler, null, Interrupt);
      Unlock (M (Interrupt));
   end Detach_Handler;

   ---------------
   -- Reference --
   ---------------

   function Reference (Interrupt : Ada.Interrupts.Interrupt_ID)
     return System.Address is
      Signal : System.Address :=
        System.Storage_Elements.To_Address
          (System.Storage_Elements.Integer_Address (Interrupt));
   begin
      if Is_Reserved (Interrupt) then
      --  Only usable Interrupts can be used for binding it to an Entry.
         raise Program_Error;
      end if;
      return Signal;
   end Reference;

   ----------------------------------
   --  Register_Interrupt_Handler  --
   ----------------------------------

   procedure Register_Interrupt_Handler
     (Handler : Ada.Interrupts.Parameterless_Handler) is
      New_Node_Ptr : R_Link;
      Ptr  : R_Link;
   begin
      --  This routine registers the Handler as usable for Dynamic
      --  Interrupt Handler. Routines attaching and detaching Handler
      --  dynamically should first consult if the Handler is rgistered.
      --  A Program Error should be raised if it is not registered.

      --  The pragma Interrupt_Handler can only appear in the library
      --  level PO definition and instantiation. Therefore, we do not need
      --  to implement Unregistering operation. Neither we need to
      --  protect the queue structure using a Write Lock.

      pragma Assert (Handler /= null or else
        Utilities.Runtime_Assert_Shutdown (
          "Interrupt Failure---a null handler should not be registered"));

      New_Node_Ptr := new Registered_Handler;
      New_Node_Ptr.H := Handler;

      if Registered_Handler_Head = null then
         Registered_Handler_Head := New_Node_Ptr;
         Registered_Handler_Tail := New_Node_Ptr;
      else
         Registered_Handler_Tail.Next := New_Node_Ptr;
         Registered_Handler_Tail := New_Node_Ptr;
      end if;

   end Register_Interrupt_Handler;

   function Is_Registered
     (Handler : Ada.Interrupts.Parameterless_Handler) return boolean is
      Ptr : R_Link;
   begin
      if Handler = null then
         return true;
      end if;

      Ptr := Registered_Handler_Head;

      while (Ptr /= null) loop
         if Ptr.H = Handler then
            return true;
         end if;
         Ptr := Ptr.Next;
      end loop;
      return false;

   end Is_Registered;

   ---------------------------
   -- Unmask_All_Interrupts --
   ---------------------------

   --  Unmask all usable interrupts for calling task (thread).

   procedure Unmask_All_Interrupts is
      Signal_Mask, Old_Set : aliased RTE.Signal_Set;
      Result : Interfaces.C.POSIX_Error.Return_Code;
   begin
      RTE.sigfillset (Signal_Mask'Access, Result);
      pragma Assert (Result /= Failure or else
        Utilities.Runtime_Assert_Shutdown ("Interrupt Failure---sigfillset"));

      RTE.sigprocmask (
        RTE.SIG_UNBLOCK, Signal_Mask'Access, Old_Set'Access, Result);
      pragma Assert (Result /= Failure or else
        Utilities.Runtime_Assert_Shutdown (
          "Interrupt Failure---sigprocmask"));
   end Unmask_All_Interrupts;

   ----------------------------
   -- Thread_Block_Interrupt --
   ----------------------------

   procedure Thread_Block_Interrupt
     (Interrupt : Ada.Interrupts.Interrupt_ID) is
      Signal_Mask, Old_Set : aliased RTE.Signal_Set;
      Result : Interfaces.C.POSIX_Error.Return_Code;
   begin
      RTE.sigemptyset (Signal_Mask'Access, Result);
      pragma Assert (Result /= Failure or else
        Utilities.Runtime_Assert_Shutdown ("Interrupt Failure---sigemptyset"));
      RTE.sigaddset (Signal_Mask'Access, RTE.Signal (Interrupt), Result);
      pragma Assert (Result /= Failure or else
        Utilities.Runtime_Assert_Shutdown ("Interrupt Failure---sigaddset"));
      RTE.sigprocmask (
        RTE.SIG_BLOCK, Signal_Mask'Access, Old_Set'Access, Result);
      pragma Assert (Result /= Failure or else
        Utilities.Runtime_Assert_Shutdown (
          "Interrupt Failure---sigprocmask"));
   end Thread_Block_Interrupt;

   ------------------------------
   -- Thread_Unblock_Interrupt --
   ------------------------------

   procedure Thread_Unblock_Interrupt
     (Interrupt : Ada.Interrupts.Interrupt_ID) is
      Signal_Mask, Old_Set : aliased RTE.Signal_Set;
      Result : Interfaces.C.POSIX_Error.Return_Code;
   begin
      RTE.sigemptyset (Signal_Mask'Access, Result);
      pragma Assert (Result /= Failure or else
        Utilities.Runtime_Assert_Shutdown ("Interrupt Failure---sigemptyset"));
      RTE.sigaddset (Signal_Mask'Access, RTE.Signal (Interrupt), Result);
      pragma Assert (Result /= Failure or else
        Utilities.Runtime_Assert_Shutdown ("Interrupt Failure---sigaddset"));
      RTE.sigprocmask (
        RTE.SIG_UNBLOCK, Signal_Mask'Access, Old_Set'Access, Result);
      pragma Assert (Result /= Failure or else
        Utilities.Runtime_Assert_Shutdown (
          "Interrupt Failure---sigprocmask"));
   end Thread_Unblock_Interrupt;

   --------------------
   -- User_Installed --
   --------------------

   function User_Installed (Interrupt : Ada.Interrupts.Interrupt_ID)
     return Boolean is
   begin
      return
        User_Handlers (Interrupt) /= Null_Handler_Assoc or else
          User_Entries (Interrupt) /= Null_Entry_Assoc;
   end User_Installed;

   -------------------
   --  Signal_Task  --
   -------------------

   procedure Signal_Task
     (T : System.Tasking.Task_ID;
      Interrupt : Ada.Interrupts.Interrupt_ID) is

      type ATCB_Ptr is access Tasking.Ada_Task_Control_Block;

      function Task_ID_To_ATCB_Ptr is new
        Unchecked_Conversion (Tasking.Task_ID, ATCB_Ptr);

      T_Access : Task_Primitives.TCB_Ptr :=
        Task_ID_To_ATCB_Ptr (T).LL_TCB'Unchecked_Access;
      Result : Interfaces.C.POSIX_Error.Return_Code;
   begin
      Interfaces.C.Pthreads.pthread_kill
         (T_Access.Thread, RTE.Signal (Interrupt), Result);
      pragma Assert (Result /= Failure or else
        Utilities.Runtime_Assert_Shutdown (
          "Interrupt Failure---pthread_kill"));
   end Signal_Task;

   -----------------------------
   -- Bind_Interrupt_To_Entry --
   -----------------------------

   --  This procedure raises a Program_Error if it tries to
   --  bind an interrupt to which an Interrupt Entry or a Protected
   --  Procedure is already bound.

   procedure Bind_Interrupt_To_Entry
     (T       : System.Tasking.Task_ID;
      E       : System.Tasking.Task_Entry_Index;
      Int_Ref : System.Address) is

      Interrupt :
        Ada.Interrupts.Interrupt_ID :=
          Ada.Interrupts.Interrupt_ID
            (System.Storage_Elements.To_Integer (Int_Ref));
      Error : Boolean;
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Write_Lock (M (Interrupt), Error);

      --  if there is a binding already (either a Procedure or an Entry),
      --  raise Program_Error.
      if User_Installed (Interrupt) then
         Unlock (M (Interrupt));
         raise Program_Error;
      end if;

      User_Entries (Interrupt) := Entry_Assoc' (T => T, E => E);

      --  Indicate the attachment of Interrupt Entry in ATCB.
      T.Interrupt_Entry := true;

      if Handler_Access (Interrupt) = null then
         Handler_Access (Interrupt) := new Handler_Task (Interrupt);
         --  Invoke the corresponding Handler_Task
      end if;

      if not Status_Blocked (Interrupt) then
         Handler_Manager.Bind_Handler (Interrupt);
      end if;

      Unlock (M (Interrupt));

   end Bind_Interrupt_To_Entry;

   ------------------------------
   -- Detach_Interrupt_Entries --
   ------------------------------

   procedure Detach_Interrupt_Entries (T : Tasking.Task_ID) is
   use Tasking;
      Error : Boolean;
   begin
      for I in Ada.Interrupts.Interrupt_ID'Range loop
         if not Is_Reserved (I) then
            Write_Lock (M (I), Error);
            if User_Entries (I) /= Null_Entry_Assoc and then
              User_Entries (I).T = T then
               User_Entries (I) := Null_Entry_Assoc;
               if not Status_Blocked (I) then
                  Handler_Manager.Unbind_Handler (I);
               end if;
            end if;
            Unlock (M (I));
         end if;
      end loop;

      --  Indicate in ATCB that no Interrupt Entries are attached.
      T.Interrupt_Entry := false;

   end Detach_Interrupt_Entries;

   ---------------------
   -- Block_Interrupt --
   ---------------------

   procedure Block_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID) is
      Error : Boolean;
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      if Is_Blocked (Interrupt) then
         return;
      end if;

      Write_Lock (M (Interrupt), Error);
      Status_Blocked (Interrupt) := true;
      Handler_Manager.Block_Interrupt (Interrupt);
      Unlock (M (Interrupt));

   end Block_Interrupt;

   -----------------------
   -- Unblock_Interrupt --
   -----------------------

   procedure Unblock_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID) is
      Error : Boolean;
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      if not Is_Blocked (Interrupt) then
         return;
      end if;

      Write_Lock (M (Interrupt), Error);
      Status_Blocked (Interrupt) := false;
      Handler_Manager.Unblock_Interrupt (Interrupt);
      Unlock (M (Interrupt));

   end Unblock_Interrupt;

   ----------------
   -- Is_Blocked --
   ----------------

   function Is_Blocked (Interrupt : Ada.Interrupts.Interrupt_ID)
     return boolean is
      Error : Boolean;
      Tmp   : Boolean;
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Write_Lock (M (Interrupt), Error);
      Tmp := Status_Blocked (Interrupt);
      Unlock (M (Interrupt));

      return Tmp;
   end Is_Blocked;

   ----------------------------------
   -- Initialize_Usable_Interrupts --
   ----------------------------------


   --  Only those interrupts classified as Asynchronous Signals in RTE
   --  can be used by users.

   procedure Initialize_Usable_Interrupts is
   begin
      Usable_Interrupts :=
        (SIGHUP | SIGQUIT | SIGPIPE | SIGTERM | SIGUSR2 |
         SIGCONT | SIGTSTP | SIGTTIN | SIGTTOU => true,
         others => false);

      --  Reflect OS specific Asynchronous signals
      for i in RTE.OS_Specific_Async_Signals'First + 1 ..
        RTE.OS_Specific_Async_Signals'Last loop
         Usable_Interrupts (Ada.Interrupts.Interrupt_ID
           (RTE.OS_Specific_Async_Signals (i))) := true;
      end loop;
   end Initialize_Usable_Interrupts;

begin
   Initialize_Usable_Interrupts;
end System.Interrupts;

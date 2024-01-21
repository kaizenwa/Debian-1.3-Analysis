------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                        S Y S T E M . T A S K I N G                       --
--                                                                          --
--                                  S p e c                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.50 $                            --
--                                                                          --
--          Copyright (C) 1992-1997, Free Software Foundation, Inc.         --
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

--  This package provides necessary type definitions for compiler interface.

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

with Ada.Exceptions;
--  used for Exception_Id

with Ada.Finalization;
--  used for Limited_Controlled

with System.Parameters;
--  used for Size_Type

with System.Task_Info;
--  used for Task_Info_Type

with System.Task_Primitives;
--  used for Task_Primitives.Private_Data
--           Lock

with System.Task_Specific_Data;
--  used for TSD
--  ???? can we split up types from operations?

package System.Tasking is

   ---------------------------------
   -- Task_ID related definitions --
   ---------------------------------

   type Ada_Task_Control_Block;

   type Task_ID is access Ada_Task_Control_Block;

   Null_Task : constant Task_ID;

   type Task_List is array (Positive range <>) of Task_ID;

   function Self return Task_ID;
   pragma Inline (Self);

   -----------------------------------
   -- Master_ID Related Definitions --
   -----------------------------------

   type Master_ID is private;

   ------------------------------
   -- Task size, priority info --
   ------------------------------

   Unspecified_Priority : constant Integer := System.Priority'First - 1;

   Priority_Not_Boosted : constant Integer := System.Priority'First - 1;
   --  Definition of Priority actually has to come from the RTS configuration.
   --  The current plan is to have System.Priority be defined from
   --  the MAX_PRIO, and MIN_PRIO defined in the System_OS_Interface.

   subtype Rendezvous_Priority is Integer
     range Priority_Not_Boosted .. System.Any_Priority'Last;

   -----------------------
   -- Enumeration types --
   -----------------------

   type Task_Stage is (
      Created,
      --  Task has been created but has not begun activation.

      Can_Activate,
      --  Task has begin activation.

      Active,
      --  Task has completed activation and is executing the task body.

      Await_Dependents,
      --  Task is trying to complete a task master other than itself,
      --  and is waiting for the tasks dependent on that master to become
      --  passive (be complete, terminated, or be waiting on a terminate
      --  alternative).

      Passive,
      --  The task is passive.

      Complete,
      --  The task is complete. The task and all of its dependents are
      --  passive; some dependents may still be waiting on terminate
      --  alternatives.

      Terminated);
      --  The task is terminated. All dependents waiting on terminate
      --  alternatives have been awakened and have terminated themselves.

   type Accepting_State is (
      Not_Accepting,    --  task is not ready to accept any entry call
      Trivial_Accept,   --  "accept E;"
      Simple_Accept,    --  "accept E do ... end E;"
      Select_Wait);     --  most general case

   type Call_Modes is (Simple_Call, Conditional_Call, Asynchronous_Call);

   type Select_Modes is (Simple_Mode, Else_Mode, Terminate_Mode, Delay_Mode);

   -----------------------------------
   -- ATC_Level related definitions --
   -----------------------------------

   Max_ATC_Nesting : constant Natural := 20;

   subtype ATC_Level_Base is Integer range 0 .. Max_ATC_Nesting;

   ATC_Level_Infinity : constant ATC_Level_Base := ATC_Level_Base'Last;

   subtype ATC_Level is ATC_Level_Base range
     ATC_Level_Base'First .. ATC_Level_Base'Last - 1;

   subtype ATC_Level_Index is ATC_Level
     range ATC_Level'First + 1 .. ATC_Level'Last;

   -------------------------------
   -- Entry related definitions --
   -------------------------------

   Null_Entry : constant := 0;

   Max_Entry : constant := Integer'Last;

   Interrupt_Entry : constant := -2;

   Cancelled_Entry : constant := -1;

   type Entry_Index is range Interrupt_Entry .. Max_Entry;

   type Entry_Call_Record;

   type Entry_Call_Link is access all Entry_Call_Record;

   type Entry_Queue is record
      Head : Entry_Call_Link;
      Tail : Entry_Call_Link;
   end record;

   ----------------------------
   -- PO related definitions --
   ----------------------------

   Null_Protected_Entry : constant := Null_Entry;

   Max_Protected_Entry : constant := Max_Entry;

   type Protected_Entry_Index is new Entry_Index
     range Null_Protected_Entry .. Max_Protected_Entry;

   subtype Positive_Protected_Entry_Index is
     Protected_Entry_Index range  1 .. Protected_Entry_Index'Last;

   type Protection (Num_Entries : Protected_Entry_Index) is limited private;
   --  This type contains the GNARL state of a protected object. The
   --  application-defined portion of the state (i.e. private objects)
   --  is maintained by the compiler-generated code.

   type Protection_Access is access all Protection;

   Null_PO : constant Protection_Access := null;

   type Communication_Block is private;
   --  Objects of this type are passed between GNARL calls to allow RTS
   --  information to be preserved.

   ------------------------------------
   -- Rendezvous related definitions --
   ------------------------------------

   Null_Task_Entry : constant := Null_Entry;

   Max_Task_Entry : constant := Max_Entry;

   type Task_Entry_Index is new Entry_Index
     range Null_Task_Entry .. Max_Task_Entry;

   type Task_Entry_Queue_Array is
     array (Task_Entry_Index range <>) of
     Entry_Queue;

   No_Rendezvous : constant := 0;

   Max_Select : constant Integer := Integer'Last;
   --  RTS-defined

   subtype Select_Index is Integer range No_Rendezvous .. Max_Select;
   --   type Select_Index is range No_Rendezvous .. Max_Select;

   subtype Positive_Select_Index is
     Select_Index range 1 .. Select_Index'Last;

   type Accept_Alternative is record --  should be packed
      Null_Body : Boolean;
      S : Task_Entry_Index;
   end record;

   type Accept_List is
     array (Positive_Select_Index range <>) of Accept_Alternative;

   type Accept_List_Access is access constant Accept_List;

   ----------------------------------
   -- Entry_Call_Record definition --
   ----------------------------------

   type Abort_Mode is (True, False, Never);

   type Entry_Call_Record is record

      Prev : Entry_Call_Link;

      Next : Entry_Call_Link;

      Self  : Task_ID;

      Level : ATC_Level;
      --  One of Self and Level are redundant in this implementation, since
      --  each Entry_Call_Record is at Self.Entry_Calls (Level). Since we must
      --  have access to the entry call record to be reading this, we could
      --  get Self from Level, or Level from Self. However, this requires
      --  non-portable address arithmetic.

      Mode : Call_Modes;

      Abortable : Abort_Mode;
      --  Indicates if a call is queued abortably.
      --  Protection: Acceptor.L. If the call is not on a queue, it should
      --  only be accessed by the task doing the call or requeue, and the
      --  mutex need not be locked in those cases.
      --  There are three modes available: True, False and Never. Never
      --  indicates that this call is made in a abort deferred region
      --  (see ARM 9.8(5-11), 9.8 (20)) and such a call is never abortable.

      Done : Boolean;
      --  The call has been completed.
      --  Protection : Self.L, except in certain circumstances where
      --  Self knows that the acceptor is suspended waiting for a call,
      --  and Self holds the acceptor's mutex.

      Has_Been_Abortable : Boolean;
      --  The call has been blocked abortably at some point.
      --  Currently only used for protected entry calls.
      --  Protection: Called_PO.L.

      E : Entry_Index;

      Prio : System.Any_Priority;

      --  The above fields are those that there may be some hope of packing.
      --  They are gathered together to allow for compilers that lay records
      --  out contiguously, to allow for such packing.

      Uninterpreted_Data : System.Address;

      Exception_To_Raise : Ada.Exceptions.Exception_Id;
      --  The exception to raise once this call has been completed without
      --  being aborted.

      --  Server : Server_Record;

      Called_Task : Task_ID;
      --  For task entry calls only. Only one of Called_Task and Called_PO
      --  are valid; the other must be Null_Task or null, respectively.
      --  In general, Called_Task must be either a legitimate Task_ID or
      --  Null_Task.  Both Called_Task and Called_PO must be null
      --  if the call record is not in use.
      --  Protection:  Called_Task.L. There are situations in which
      --  it is necessary to access this field given only an Entry_Call_Record.
      --  This is difficult, since Called_Task.L must be locked to access
      --  Called_Task. This is done by doing the lock and then checking
      --  to make sure that Called_Task has not changed; see
      --  System.Tasking.Utilities.Lock_Server.

      Acceptor_Prev_Call : Entry_Call_Link;
      --  For task entry calls only.

      Acceptor_Prev_Priority : Rendezvous_Priority := Priority_Not_Boosted;
      --  For task entry calls only.
      --  The priority of the most recent prior call being serviced.
      --  For protected entry calls, this function should be performed by
      --  GNULLI ceiling locking.

      Called_PO : Protection_Access;
      --  For protected entry calls only. Only one of Called_Task and
      --  Called_PO are valid; the other must be Null_Task or Null_PO,
      --  respectively. In general, Called_PO must be either a legitimate
      --  Protection_Access value or null.  Both Called_Task and
      --  Called_PO must be null if the call record is not in use.
      --  Protection: Called_PO.L. See notes under Called_Task, above.

   end record;

   ------------------------------------
   -- Task related other definitions --
   ------------------------------------

   type Activation_Chain is limited private;

   type Activation_Chain_Access is access all Activation_Chain;

   type Task_Procedure_Access is access procedure (Arg : System.Address);

   type Access_Boolean is access all Boolean;

   type Access_Address is access all System.Address;

   ----------------------------------------------
   -- Ada_Task_Control_Block (ATCB) definition --
   ----------------------------------------------

   type Entry_Call_Array is array (ATC_Level_Index) of
     aliased Entry_Call_Record;

   D_I_Count : constant := 2;
   --  This constant may be adjusted, to allow more Address-sized
   --  attributes to be stored directly in the task control block.

   subtype Direct_Index is Integer range 0 .. D_I_Count - 1;
   --  Attributes with indices in this range are stored directly in
   --  the task control block.  Such attributes must be Address-sized.
   --  Other attributes will be held in dynamically allocated records
   --  chained off of the task control block.

   type Direct_Attribute_Array is
     array (0 .. D_I_Count - 1) of aliased System.Address;

   type Direct_Index_Vector is mod 2 ** D_I_Count;
   --  This is a bit-vector type, used to store information about
   --  the usage of the direct attribute fields.

   --  Notes on protection (synchronization) of TRTS data structures.

   --  Any field of the TCB can be written by the activator of a task when the
   --  task is created, since no other task can access the new task's
   --  state until creation is complete.

   --  The protection for each field is described in a comment starting with
   --  "Protection:".

   --  When a lock is used to protect an ATCB field, this lock is simply named.

   --  Some protection is described in terms of tasks related to the
   --  ATCB being protected. These are:

   --    Self: The task which is controlled by this ATCB.
   --    Acceptor: A task accepting a call from Self.
   --    Caller: A task calling an entry of Self.
   --    Parent: The task executing the master on which Self depends.
   --    Dependent: A task dependent on Self.
   --    Activator: The task that created Self and initiated its activation.
   --    Created: A task created and activated by Self.

   type Ada_Task_Control_Block (Entry_Num : Task_Entry_Index) is record

      LL : aliased System.Task_Primitives.Private_Data;
      --  Control block used by the underlying low-level tasking service
      --  (GNULLI).
      --  Protection: This is used only by the GNULLI implementation, which
      --  takes care of all of its synchronization.

      Task_Arg : System.Address;
      --  The argument to task procedure. Currently unused; this will
      --  provide a handle for discriminant information.
      --  Protection: Part of the synchronization between Self and
      --  Activator. Activator writes it, once, before Self starts
      --  executing. Thereafter, Self only reads it.

      Task_Info : System.Task_Info.Task_Info_Type;
      --  System-specific attributes of the task as specified by the
      --  Task_Info pragma.

      Stack_Size : System.Parameters.Size_Type;
      --  Requested stack size.
      --  Protection: Only used by Self.

      Task_Entry_Point : Task_Procedure_Access;
      --  Information needed to call the procedure containing the code for
      --  the body of this task.
      --  Protection: Part of the synchronization between Self and
      --  Activator. Activator writes it, once, before Self starts
      --  executing. Self reads it, once, as part of its execution.

      Base_Priority : System.Any_Priority;
      --  Base priority, not changed during entry calls, only changed
      --  via dynamic priorities package.
      --  Protection: Only written by Self, accessed by anyone.

      New_Base_Priority : System.Any_Priority;
      --  New value for Base_Priority (for dynamic priorities package).
      --  Protection: Self.L.

      Compiler_Data : System.Task_Specific_Data.TSD;
      --  Task-specific data needed by the compiler to store
      --  per-task structures.
      --  Protection: Only accessed by Self.

      --  the following declarations are for Rendezvous

      All_Tasks_Link : Task_ID;
      --  Used to link this task to the list of all tasks in the system.
      --  Protection: All_Tasks.L.

      Global_Task_Lock_Nesting : Natural;
      --  This is the current nesting level of calls to
      --  System.Tasking.Stages.Lock_Task_T.
      --  This allows a task to call Lock_Task_T multiple times without
      --  deadlocking. A task only locks All_Task_Lock when its
      --  All_Tasks_Nesting goes from 0 to 1, and only unlocked when it
      --  goes from 1 to 0.
      --  Protection: Only accessed by Self.

      Activation_Link : Task_ID;
      --  Used to link this task to a list of tasks to be activated.
      --  Protection: Only used by Activator.

      Open_Accepts : Accept_List_Access;
      --  This points to the Open_Accepts array of accept alternatives passed
      --  to the RTS by the compiler-generated code to Selective_Wait.
      --  Protection: Self.L.

      Exception_To_Raise : Ada.Exceptions.Exception_Id;
      --  An exception which should be raised by this task when it regains
      --  control.
      --  Protection: Read only by Self, under circumstances where it will
      --  be notified by the writer when it is safe to read it:
      --  1. Written by Acceptor, when Self is suspended.
      --  2. Written by Notify_Exception, executed by Self through a
      --     synchronous signal handler, which redirects control to a
      --     routine to read it and raise the exception.

      Chosen_Index : Select_Index;
      --  The index in Open_Accepts of the entry call accepted by a selective
      --  wait executed by this task.
      --  Protection: Written by both Self and Caller. Usually protected
      --  by Self.L. However, once the selection is known to have been
      --  written it can be accessed without protection. This happens
      --  after Self has updated it itself using information from a suspended
      --  Caller, or after Caller has updated it and awakened Self.

      Call : Entry_Call_Link;
      --  The entry call that has been accepted by this task.
      --  Protection: Self.L. Self will modify this field
      --  when Self.Accepting is False, and will not need the mutex to do so.
      --  Once a task sets Stage=Complete, no other task can access this
      --  field.

      --  The following fields are used to manage the task's life cycle.

      Activator : Task_ID;
      --  The task that created this task, either by declaring it as a task
      --  object or by executing a task allocator.
      --  Protection: Set by Activator before Self is activated, and
      --  read after Self is activated.

      Parent : Task_ID;
      Master_of_Task : Master_ID;
      --  The task executing the master of this task, and the ID of this task's
      --  master (unique only among masters currently active within Parent).
      --  Protection: Set by Activator before Self is activated, and
      --  read after Self is activated.

      Master_Within : Master_ID;
      --  The ID of the master currently executing within this task; that is,
      --  the most deeply nested currently active master.
      --  Protection: Only written by Self, and only read by Self or by
      --  dependents when Self is attempting to exit a master. Since Self
      --  will not write this field until the master is complete, the
      --  synchronization should be adequate to prevent races.

      Activation_Count : Integer;
      --  This is the number of tasks that this task is activating, i.e. the
      --  children that have started activation but have not completed it.
      --  Protection: Self.L and Created.L. Both mutexes must be locked,
      --  since Self.Activation_Count and Created.Stage must be synchronized.

      Awake_Count : Integer;
      --  Number of tasks dependent on this task (including this task) that are
      --  still "awake": not terminated and not waiting on a terminate
      --  alternative.
      --  Protection: Self.L. Parent.L must also be locked when this is
      --  updated, so that it can be synchronized with
      --  Parent.Awaited_Dependent_Count, except under special circumstances
      --  where we know that the two can be out of sync without allowing the
      --  parent to terminate before its dependents.

      Awaited_Dependent_Count : Integer;
      --  This is the awake count of a master being completed by this task.
      --  Protection: Self.L. Dependent.L must also be locked so that
      --  this field and Dependent.Awake_Count can be synchronized, except
      --  under special circumstances where we know that the two can be out
      --  of sync without allowing the parent to terminate before its
      --  dependents.

      Terminating_Dependent_Count : Integer;
      --  This is the count of tasks dependent on a master being completed by
      --  this task which are waiting on a terminate alternative. Only valid
      --  when there none of the dependents are awake.
      --  Protection: Self.L.

      Pending_Priority_Change : Boolean;
      --  Flag to indicate pending priority change (for dynamic priorities
      --  package). The base priority is updated on the next abortion
      --  completion point (aka. synchronization point).
      --  Protection: Self.L.

      Pending_Action : Boolean;
      --  Unified flag indicating pending action on abortion completion
      --  point (aka. synchronization point). Currently set if:
      --  . Pending_Priority_Change is set or
      --  . Pending_ATC_Level is changed.
      --  . Requeue involving POs (Abortable field may have chaged and the
      --     Wait_Until_Abortable has to check the abortable status of the
      --     call)
      --  Protection: Self.L.

      Pending_ATC_Level : ATC_Level_Base;
      --  The ATC level to which this task is currently being aborted.
      --  Protection: Self.L.

      ATC_Nesting_Level : ATC_Level;
      --  The dynamic level of ATC nesting (currently executing nested
      --  asynchronous select statements) in this task.
      --  Protection: This is only used by Self. However, decrementing it
      --  in effect deallocates an Entry_Calls component, and care must be
      --  taken that all references to that component are eliminated before
      --  doing the decrement. This in turn will probably required locking
      --  a protected object (for a protected entry call) or the Acceptor's
      --  lock (for a task entry call). However, ATC_Nesting_Level itself can
      --  be accessed without a lock.

      Deferral_Level : Natural;
      --  This is the number of times that Defer_Abortion has been called by
      --  this task without a matching Undefer_Abortion call. Abortion is
      --  only allowed when this zero.
      --  Protection: Only updated by Self; access assumed to be atomic.

      Elaborated : Access_Boolean;
      --  Pointer to a flag indicating that this task's body has been
      --  elaborated. The flag is created and managed by the
      --  compiler-generated code.
      --  Protection: The field itself is only accessed by Activator. The flag
      --  that it points to is updated by Master and read by Activator; access
      --  is assumed to be atomic.

      Stage : Task_Stage;
      --  The general stage of the task in it's life cycle.
      --  Protection: Self.L.

      --  beginning of flags

      Cancel_Was_Successful : Boolean;
      --  This indicates that the last attempt to cancel an entry call was
      --  successful. It needs to be accurate between a call to
      --  *Cancel_*_Entry_Call and the following call to Complete_*_Entry_Call.
      --  These calls cannot be nested; that is, there can be no intervening
      --  *Cancel_*_Entry_Call, so this single field is adequate.
      --  Protection: Accessed only by Self.

      Accepting : Accepting_State;
      --  The ability of this task to accept an entry call.
      --  Protection: Self.L.

      Aborting : Boolean;
      --  Self is in the process of aborting. While set, prevents multiple
      --  abortion signals from being sent by different aborter while abortion
      --  is acted upon. This is essential since an aborter which calls
      --  Abort_To_Level could set the Pending_ATC_Level to yet a lower level
      --  (than the current level), may be preempted and would send the
      --  abortion signal when resuming execution. At this point, the abortee
      --  may have completed abortion to the proper level such that the
      --  signal (and resulting abortion exception) are not handled any more.
      --  In other words, the flag prevents a race between multiple aborters
      --  and the abortee.
      --  Protection: Self.L.

      Terminate_Alternative : Boolean;
      --  Task is accepting Select with Terminate Alternative.

      Interrupt_Entry : Boolean := false;
      --  Indicates if one or more Interrupt Entries are attached to
      --  the task. This flag is needed for cleaning up the Interrupt
      --  Entry bindings.

      --  end of flags

      Entry_Calls : Entry_Call_Array;
      --  An array of entry calls.
      --  Protection: The elements of this array are on entry call queues
      --  associated with protected objects or task entries, and are protected
      --  by the protected object lock or Acceptor.L, respectively.

      Entry_Queues : Task_Entry_Queue_Array (1 .. Entry_Num);
      --  An array of task entry queues.
      --  Protection: Self.L. Once a task has set Self.Stage to Completing, it
      --  has exclusive access to this field.

      Direct_Attributes : Direct_Attribute_Array;
      --  for task attributes that have same size as Address
      Is_Defined : Direct_Index_Vector := 0;
      --  bit I is 1 iff Direct_Attributes (I) is defined
      Indirect_Attributes : Access_Address;
      --  a pointer to chain of records for other attributes that
      --  are not address-sized, including all tagged types.

   end record;

   type Barrier_Function_Pointer is access
     function
       (O : System.Address;
        E : Protected_Entry_Index)
        return Boolean;
   --  Pointer to a function which evaluates the barrier of a protected
   --  entry body. O is a pointer to the compiler-generated record
   --  representing the protected object, and E is the index of the
   --  entry serviced by the body.

   type Entry_Action_Pointer is access
     procedure
       (O : System.Address;
        P : System.Address;
        E : Protected_Entry_Index);
   --  Pointer to a procedure which executes the sequence of statements
   --  of a protected entry body. O is a pointer to the compiler-generated
   --  record representing the protected object, P is a pointer to the
   --  record of entry parameters, and E is the index of the
   --  entry serviced by the body.

   type Entry_Body is record
      Barrier : Barrier_Function_Pointer;
      Action  : Entry_Action_Pointer;
   end record;
   --  The compiler-generated code passes objects of this type to the GNARL
   --  to allow it to access the executable code of an entry body.

   type Protected_Entry_Body_Array is
     array (Positive_Protected_Entry_Index range <>) of Entry_Body;
   --  This is an array of the executable code for all entry bodies of
   --  a protected type.

   type Protected_Entry_Body_Access is access all Protected_Entry_Body_Array;

private

   Null_Task : constant Task_ID := null;

   type Activation_Chain is new Task_ID;

   type Master_ID is new Integer;

   type Communication_Block is record
      Self      : Task_ID;
      Enqueued  : Boolean := False;
      Cancelled : Boolean := False;
   end record;
   pragma Volatile (Communication_Block);

   type Protected_Entry_Queue_Array is
        array (Protected_Entry_Index range <>) of
        Entry_Queue;

   type Protection (Num_Entries : Protected_Entry_Index) is new
     Ada.Finalization.Limited_Controlled
   with record
      L                 : aliased Task_Primitives.Lock;
      Compiler_Info     : System.Address;
      Call_In_Progress  : Entry_Call_Link;
      Ceiling           : System.Any_Priority;
      Old_Base_Priority : System.Any_Priority;
      Pending_Action    : Boolean;
      --  This flag is needed when we want to find information related
      --  to the object: The caller may have higher priority (while not
      --  directly involved in the protected calls: in such cases a
      --  locking violation will be checked!) than the PO (this is
      --  possible when a requeue has been made and a priority change
      --  to the original caller is also made). Under such conditions, we
      --  temporarily lower the priority of the caller to circumbent the
      --  undesired ceiling violation check. This flag indicates if such a
      --  temporary priority change is mad eor not. We set the priority
      --  back in Unlock call.
      Entry_Bodies      : Protected_Entry_Body_Access;
      Entry_Queues      : Protected_Entry_Queue_Array (1 .. Num_Entries);
   end record;
   pragma Volatile (Protection);

   procedure Finalize (Object : in out Protection);
   --  Clean up a Protection object; in particular, finalize the associated
   --  Lock object.

end System.Tasking;

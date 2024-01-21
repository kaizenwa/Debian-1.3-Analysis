------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S .O P E R A T O I N S     --
--                                                                          --
--                                  S p e c                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.3 $                            --
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

with Interfaces.C;
--  used for size_t

with System.OS_Interface;
--  used for thr_min_stack;

with System.Parameters;
--  used for Size_Type

with System.Tasking;
--  used for Task_ID

package System.Task_Primitives.Operations is

   pragma Elaborate_Body;

   subtype Task_ID is System.Tasking.Task_ID;

   procedure Initialize (Environment_Task : Task_ID);
   --  This must be called once, before any other subprograms of this
   --  package are called.

   procedure Create_Task
     (T          : Task_ID;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean);
   --  Create a new low-level task with Task_ID T.
   --  Place other needed information to the ATCB.
   --  A new thread of control is created, with a stack of at least Stack_Size
   --  storage units, and the procedure Wrapper is called
   --  by this new thread of control.
   --  If Stack_Size = Unspecified_Storage_Size, choose a default stack
   --  size; this may be effectively "unbounded" on some systems.

   --  The newly created low-level task is associated with the Task_ID T
   --  such that any subsequent call to Self from within the context of
   --  the low-level task returns T.
   --  The caller is responsible for ensuring that the storage of
   --  the Ada task control block object pointed to by T persists for the
   --  lifetime of the new task.

   --  Succeeded is set to true unless creation of the task failed,
   --  as it may if there are insufficient resources to create another task.

   procedure Enter_Task (Self_ID : Task_ID);
   --  Initialize data structures specific to the calling task.
   --  Self must be the ID of the calling task.
   --  It must be called (once) by the task immediately after creation,
   --  while abortion is still deferred.
   --  The effects of other operations defined below are not defined
   --  unless the caller has previously called Initialize_Task.
   pragma Inline (Enter_Task);

   procedure Exit_Task;
   --  Destroy the thread of control.
   --  Self must be the ID of the calling task.
   --  The effects of further calls to operations defined below
   --  on the task are undefined thereafter.
   pragma Inline (Exit_Task);


   procedure Initialize_TCB (Self_ID : Task_ID; Succeeded : out Boolean);
   --  Initialize all fields of the TCB

   procedure Finalize_TCB (T : Task_ID);
   --  Finalizes Private_Data of ATCB, and then deallocates it.
   --  This should be called when it is guaranteed that there is no further
   --  reference to ATCB. This should be called for a task that has been
   --  created. Activated tasks are responsible for finalizing using Exit_Task
   --  but the deallocation of data is responsible by calling this procedure.

   procedure Abort_Task (T : Task_ID);
   --  Abort the task specified by T (the target task). This causes
   --  the target task to asynchronously raise Abort_Signal if
   --  abort is not deferred, and causes the task to wake up if it
   --  is asleep, as in Sleep, Sleep_Until, Sleep_For,
   --  Delay_For, or Delay_Until.
   pragma Inline (Abort_Task);

   --  ??? modify GNARL to skip wakeup and always call Abort_Task

   function Self return Task_ID;
   --  Return a pointer to the Ada Task Control Block of the calling task.
   pragma Inline (Self);

   procedure Initialize_Lock (Prio : System.Any_Priority; L : access Lock);
   procedure Initialize_Lock (L : access RTS_Lock);
   --  Initialize a lock object.
   --  For Lock, Prio is the ceiling priority associated with the lock.
   --  For RTS_Lock, the ceiling is implicitly Priority'Last.

   --  If the underlying system does not support priority ceiling
   --  locking, the Prio parameters is ignored.

   --  The effect of either initialize operation is undefined unless L
   --  is a lock object that has not been initialized, or which has been
   --  finalized since it was last initialized.

   --  The effects of the other operations on lock objects
   --  are undefined unless the lock object has been initialized
   --  and has not since been finalized.

   --  Initialization of the per-task lock is implicit in Create_Task.

   --  These operations raise Storage_Error if a lack of storage is detected.
   pragma Inline (Initialize_Lock);

   procedure Finalize_Lock (L : access Lock);

   procedure Finalize_Lock (L : access RTS_Lock);
   --  Finalize a lock object, freeing any resources allocated by the
   --  corresponding Initialize_Lock operation.

   pragma Inline (Finalize_Lock);

   procedure Write_Lock (L : access Lock; Ceiling_Violation : out Boolean);
   procedure Write_Lock (L : access RTS_Lock);
   procedure Write_Lock (T : Task_ID);
   --  Lock a lock object for write access. After this operation returns,
   --  the calling task holds write permission for the lock object.
   --  No other Write_Lock or Read_Lock operation on the same lock object will
   --  return until this task executes an Unlock operation on the same object.
   --  The effect is undefined if the calling task already holds
   --  read or write permission for the lock object L.

   --  For the operation on Lock, Ceiling_Violation is set to true iff
   --  the operation failed, which will happen if there is a priority
   --  ceiling violation.

   --  For the operation on Task_ID, the lock is the special lock object
   --  associated with that task.
   --  This lock has effective ceiling priority high enough
   --  that it is safe to call by a task with any priority in the range
   --  System.Priority.  It is implicitly initialized by task creation.
   --  The effect is undefined if the calling task already holds T's lock,
   --  or has interrupt-level priority.
   --  Finalization of the per-task lock is implicit in Exit_Task.

   pragma Inline (Write_Lock);

   procedure Read_Lock (L : access Lock; Ceiling_Violation : out Boolean);
   --  Lock a lock object for read access.  After this operation returns,
   --  the calling task has non-exclusive read permission for the logical
   --  resources that are protected by the lock.  No other
   --  no other Write_Lock operation on the same object will return until
   --  this task and any other tasks with read permission for this
   --  lock have executed Unlock operation(s) on the lock object.
   --  A Read_Lock for a lock object may return immediately while there
   --  are tasks holding read permission, provided there are no tasks
   --  holding write permission for the object.
   --  The effect is undefined if the calling task already holds
   --  read or write permission for L.

   --  Alternatively: An implementation may treat Read_Lock identically to
   --  Write_Lock.

   --  Note that Read_Lock is not defined for RT_Lock and Task_ID.
   --  That is because (1) so far Read_Lock has always been implemented
   --  the same as Write_Lock, (2) most lock usage inside the RTS involves
   --  potential write access, and (3) implementations of priority ceiling
   --  locking that make a reader-writer distinction have higher overhead.

   pragma Inline (Read_Lock);

   procedure Unlock (L : access Lock);
   procedure Unlock (L : access RTS_Lock);
   procedure Unlock (T : Task_ID);
   --  Unlock a locked lock object.
   --  The effect is undefined unless the calling task holds read or
   --  write permission for the lock L, and L is the lock object most
   --  recently locked by the calling task for which the calling task
   --  still holds read or write permission.
   --  (That is, matching pairs of Lock and Unlock operations on each
   --  lock object must be properly nested.)
   pragma Inline (Unlock);

   --  ????   The following deserves more attention at some point.

   --  Note that Write_Lock for RTS_Lock does not have an out-parameter.
   --  RTS_Locks are used in situations where we have not made provision
   --  for recovery from ceiling violations.  We do not expect them to
   --  occur inside the runtime system, because all RTS locks have ceiling
   --  Priority'Last.

   --  There is one way there can be a ceiling violation.
   --  That is if the runtime system is called from a task that is
   --  executing in the Interrupt_Priority range.

   --  It is not clear what to do about ceiling violations due
   --  to RTS calls done at interrupt priority.  In general, it
   --  is not acceptable to give all RTS locks interrupt priority,
   --  since that whould give terrible performance on systems where
   --  this has the effect of masking hardware interrupts, though we
   --  could get away with allowing Interrupt_Priority'last where we
   --  are layered on an OS that does not allow us to mask interrupts.
   --  Ideally, we would like to raise Program_Error back at the
   --  original point of the RTS call, but this would require a lot of
   --  detailed analysis and recoding, with almost certain performance
   --  penalties.

   --  For POSIX systems, we considered just skipping setting a
   --  priority ceiling on RTS locks.  This would mean there is no
   --  ceiling violation, but we would end up with priority inversions
   --  inside the runtime system, resulting in failure to satisfy the
   --  Ada priority rules, and possible missed validation tests.
   --  This could be compensated-for by explicit priority-change calls
   --  to raise the caller to Priority'Last whenever it first enters
   --  the runtime system, but the expected overhead seems high, though
   --  it might be lower than using locks with ceilings if the underlying
   --  implementation of ceiling locks is an inefficient one.

   --  This issue should be reconsidered whenever we get around to
   --  checking for calls to potentially blocking operations from
   --  within protected operations.  If we check for such calls and
   --  catch them on entry to the OS, it may be that we can eliminate
   --  the possibility of ceiling violations inside the RTS.  For this
   --  to work, we would have to forbid explicitly setting the priority
   --  of a task to anything in the Interrupt_Priority range, at least.
   --  We would also have to check that there are no RTS-lock operations
   --  done inside any operations that are not treated as potentially
   --  blocking.

   --  The latter approach seems to be the best, i.e. to check on entry
   --  to RTS calls that may need to use locks that the priority is not
   --  in the interrupt range.  If there are RTS operations that NEED to
   --  be called from interrupt handlers, those few RTS locks should then
   --  be converted to PO-type locks, with ceiling Interrupt_Priority'Last.

   --  For now, we will just shut down the system if there is a
   --  ceiling violation.

   procedure Sleep (Self_ID : Task_ID);
   --  Wait until the current task, T,  is signaled to wake up.
   --  The effect is undefined unless the calling task is holding
   --  its own special lock (see Lock_Task).
   --  The effect is to atomically unlock T's lock and wait,
   --  so that another task that is able to lock T's lock
   --  can be assured that the wait has actually commenced, and that
   --  a Signal operation will cause the waiting task to become
   --  ready for execution once again. When Sleep returns,
   --  the waiting task will again hold its own special lock.
   --  The waiting task may become ready for execution at any time
   --  (that is, spurious wakeups are permitted), but it will definitely
   --  become ready for execution when a Wakeup operation is performed
   --  for the same task.
   pragma Inline (Sleep);

   procedure Sleep_For (Self_ID : Task_ID; Rel_Time : Duration);
   --  Combination of Sleep (above) and Delay_For (below).
   pragma Inline (Sleep_For);

   procedure Sleep_Until (Self_ID : Task_ID; Abs_Time : Duration);
   --  Combination of Sleep (above) and Delay_Until (below).
   pragma Inline (Sleep_Until);

   procedure Wakeup (T : Task_ID);
   --  Wake up task T if it is waiting on a Sleep call (of ordinary
   --  or timed variety), making it ready for execution once again.
   --  If the task T is not waiting on a Sleep, the operation has no effect.
   pragma Inline (Wakeup);

   procedure Yield;
   --  Yield the processor. Add the calling task to the tail of the
   --  ready queue for its active_priority.
   pragma Inline (Yield);

   procedure Set_Priority (T : Task_ID; Prio : System.Any_Priority);
   --  Set the priority of the task specified by T to T.Current_Priority.
   --  The priority set is what would correspond to the Ada concept of
   --  "base priority" in the terms of the lower layer system, but
   --  the operation may be used by the upper layer to implement
   --  changes in "active priority" that are not due to lock effects.
   pragma Inline (Set_Priority);

   function Get_Priority (T : Task_ID) return System.Any_Priority;
   --  Returns the priority last set by Set_Priority for this task.
   pragma Inline (Set_Priority);

end System.Task_Primitives.Operations;

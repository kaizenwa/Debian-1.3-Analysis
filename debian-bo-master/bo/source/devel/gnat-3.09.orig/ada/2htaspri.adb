------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                S Y S T E M . T A S K _ P R I M I T I V E S               --
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

--  This is a HP-UX 10.x version of this package.

with System.Task_Clock;
--  Used for, Stimespec,
--            Stimespec_Seconds,
--            Stimespec_NSeconds

with Interfaces.C.POSIX_timers;
--  Used for, timespec,
--            Nanoseconds

with Interfaces.C.POSIX_Error;
--  Used for, Return_Code,
--            Failure,
--            Get_Error_Code,
--            Interrupted_Function_Call,
--            Resource_Temporarily_Unavailable,
--            Priority_Ceiling_Violation

with Interfaces.C.POSIX_RTE;
--  Used for, Signal,
--            Signal_Set,
--            sigaddset,
--            sigdelset,
--            sigfillset,
--            sigemptyset,
--            sigprocmask,
--            siginfo_ptr,
--            struct_sigaction,
--            sigaction,
--            and various CONSTANTS

with Interfaces.C.Pthreads; use Interfaces.C.Pthreads;

with Unchecked_Deallocation;

with Unchecked_Conversion;

package body System.Task_Primitives is

   package RTE renames Interfaces.C.POSIX_RTE;

   Failure : Interfaces.C.POSIX_Error.Return_Code
      renames Interfaces.C.POSIX_Error.Failure;

   use type Interfaces.C.POSIX_Error.Return_Code;
   use type Interfaces.C.POSIX_Error.Error_Code;

   Test_And_Set_Mutex : Lock;
   --  Use a mutex to simulate test-and-set.  This is ridiculously inefficient;
   --  it is just here so that I can fix the syntax errors without having to
   --  worry about how to get machine code into the system in the absence
   --  of machine code inserts.

   Abort_Signal : constant RTE.Signal := RTE.SIGABRT;

   function "=" (L, R : System.Address) return Boolean
     renames System."=";

   ATCB_Key : pthread_key_t;

   Abort_Handler : Abort_Handler_Pointer;

   Unblocked_Signal_Mask : aliased RTE.Signal_Set;
   --  The set of signals that should be unblocked in a task.
   --  This is in general the signals that can be generated synchronously,
   --  and which should therefore be converted into Ada exceptions.
   --  It also includes the Abort_Signal, to allow asynchronous abortion.

   Reserved_Signals : aliased RTE.Signal_Set;
   --  The set of signals reserved for use by the runtime system.

   procedure Put_Character (C : Integer);
   pragma Import (C, Put_Character, "putchar");

   procedure Prog_Exit (Status : Integer);
   pragma Import (C, Prog_Exit, "exit");

   function Pointer_to_Address is new
     Unchecked_Conversion (TCB_Ptr, System.Address);

   function Address_to_Pointer is new
     Unchecked_Conversion (System.Address, TCB_Ptr);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Abort_Wrapper
     (signo   : Integer;
      info    : Integer;  --  eas RTE.siginfo_ptr;
      context : System.Address);
   --  This is a signal handler procedure which calls the user-specified
   --  abort handler procedure.

   function Get_Stack_Limit return System.Address;
   pragma Inline (Get_Stack_Limit);
   --  Obtains stack limit from TCB

   procedure LL_Wrapper (T : TCB_Ptr);
   --  A wrapper procedure that is called from a new low-level task.
   --  It performs initializations for the new task and calls the
   --  user-specified startup procedure.

   procedure Write_Character (C : Character);
   procedure Write_EOL;
   procedure Write_String (S : String);
   --  Debugging procedures used for assertion output

   function Stimespec_to_timespec (S : Task_Clock.Stimespec)
     return Interfaces.C.POSIX_timers.timespec;

   function timespec_to_Stimespec (S : Interfaces.C.POSIX_timers.timespec)
     return Task_Clock.Stimespec;

   ----------------------
   -- Runtime_Shutdown --
   ----------------------

   function Runtime_Assert_Shutdown (Msg : in String) return boolean;
   --  There is another copy of the same function in s-tasuti.ads which
   --  gnarl level routines use. These should be unified. However, we do not
   --  want to modify the interface for Task_Primitives without synchronizing
   --  with OS 2 runtime, hence created a duplicated local copy here
   --  temporarily.

   function Runtime_Assert_Shutdown (Msg : in String) return boolean is
   begin
      LL_Assert (false, Msg);
      --  This call should never return
      return false;
   end Runtime_Assert_Shutdown;

   ---------------------
   -- Write_Character --
   ---------------------

   procedure Write_Character (C : Character) is
   begin
      Put_Character (Character'Pos (C));
   end Write_Character;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_EOL is
   begin
      Write_Character (Ascii.LF);
   end Write_EOL;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String (S : String) is
   begin
      for J in S'Range loop
         Write_Character (S (J));
      end loop;
   end Write_String;

   ---------------
   -- LL_Assert --
   ---------------

   procedure LL_Assert (B : Boolean; M : String) is
   begin
      if not B then
         Write_String ("Failed Runtime Assertion: ");
         Write_String (M);
         Write_String (".");
         Write_EOL;
         Prog_Exit (1);
      end if;
   end LL_Assert;

   -------------------------
   -- Initialize_LL_Tasks --
   -------------------------

   procedure Initialize_LL_Tasks (T : TCB_Ptr) is
      Old_Set : aliased RTE.Signal_Set;
      Result  : Interfaces.C.POSIX_Error.Return_Code;

      Blocked_Signal_Mask : aliased RTE.Signal_Set;
      --  The set of signals that should always be blocked in a task.

   begin
   --  WARNING : SIGALRM should not be in the following mask.  SIGALRM should
   --          be a normal user signal under 1, and should be enabled
   --          by the client.  However, the current RTS built on 1
   --          uses nanosleep () and pthread_cond_wait (), which fail if all
   --          threads have SIGALRM masked. ???

      RTE.sigemptyset (Unblocked_Signal_Mask'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigemptyset"));
      RTE.sigaddset (Unblocked_Signal_Mask'Access, Abort_Signal, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));
      RTE.sigaddset (Unblocked_Signal_Mask'Access, RTE.SIGALRM, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));
      RTE.sigaddset (Unblocked_Signal_Mask'Access, RTE.SIGILL, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));
      RTE.sigaddset (Unblocked_Signal_Mask'Access, RTE.SIGFPE, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));
      RTE.sigaddset (Unblocked_Signal_Mask'Access, RTE.SIGSEGV, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));

      --  OS specific Synchronous signals.
      for i in RTE.OS_Specific_Sync_Signals'First + 1 ..
        RTE.OS_Specific_Sync_Signals'Last loop
         RTE.sigdelset
           (Unblocked_Signal_Mask'Access,
           RTE.OS_Specific_Sync_Signals (i),
           Result);
         pragma Assert (
           Result /= Failure or else Runtime_Assert_Shutdown (
             "GNULLI failure---sigdelset"));
      end loop;

      RTE.sigfillset (Blocked_Signal_Mask'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigfillset"));
      RTE.sigdelset (Blocked_Signal_Mask'Access, Abort_Signal, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigdelset"));
      RTE.sigdelset (Blocked_Signal_Mask'Access, RTE.SIGALRM, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigdelset"));
      RTE.sigdelset (Blocked_Signal_Mask'Access, RTE.SIGILL, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigdelset"));
      RTE.sigdelset (Blocked_Signal_Mask'Access, RTE.SIGFPE, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigdelset"));
      RTE.sigdelset (Blocked_Signal_Mask'Access, RTE.SIGSEGV, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigdelset"));

      RTE.sigdelset (Blocked_Signal_Mask'Access, RTE.SIGINT, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigdelset"));
      --  SIGINT usually allows a program to be interrupted by control-C.
      --  It is unfriendly to simply block it, but unblocking it
      --  would leave a window of vulnerability to this signal that the
      --  user could not control.  It is therefore inherited from the
      --  creating process, neither blocked nor unblocked here.

      --  OS specific Synchronous signals.
      for i in RTE.OS_Specific_Sync_Signals'First + 1 ..
        RTE.OS_Specific_Sync_Signals'Last loop
         RTE.sigdelset
           (Blocked_Signal_Mask'Access,
           RTE.OS_Specific_Sync_Signals (i),
           Result);
         pragma Assert (
           Result /= Failure or else Runtime_Assert_Shutdown (
             "GNULLI failure---sigdelset"));
      end loop;

      RTE.sigemptyset (Reserved_Signals'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigemptyset"));
      RTE.sigaddset (Unblocked_Signal_Mask'Access, Abort_Signal, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));
      RTE.sigaddset (Reserved_Signals'Access, RTE.SIGILL, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));
      RTE.sigaddset (Reserved_Signals'Access, RTE.SIGFPE, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));
      RTE.sigaddset (Reserved_Signals'Access, RTE.SIGSEGV, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));
      RTE.sigaddset (Reserved_Signals'Access, Abort_Signal, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));

      --  OS specific Synchronous signals.
      for i in RTE.OS_Specific_Sync_Signals'First + 1 ..
        RTE.OS_Specific_Sync_Signals'Last loop
         RTE.sigdelset
           (Reserved_Signals'Access, RTE.OS_Specific_Sync_Signals (i), Result);
         pragma Assert (
           Result /= Failure or else Runtime_Assert_Shutdown (
             "GNULLI failure---sigdelset"));
      end loop;

      pthread_key_create (ATCB_Key, System.Null_Address, Result);

      if Result = Failure then
         raise Storage_Error;               --  Insufficient resources.
      end if;

      RTE.sigprocmask (
        RTE.SIG_BLOCK, Blocked_Signal_Mask'Access, Old_Set'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigprocmask"));
      RTE.sigprocmask (
        RTE.SIG_UNBLOCK, Unblocked_Signal_Mask'Access, Old_Set'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigprocmask"));

      T.LL_Entry_Point := null;

      T.Thread := pthread_self;
      pthread_setspecific (ATCB_Key, Pointer_to_Address (T), Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_setspecific"));

   end Initialize_LL_Tasks;

   ----------
   -- Self --
   ----------

   function Self return TCB_Ptr is
      Temp   : System.Address;
      Result : Interfaces.C.POSIX_Error.Return_Code;

   begin
      pthread_getspecific (ATCB_Key, Temp, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_getspecific"));
      return Address_to_Pointer (Temp);
   end Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : in out Lock)
   is
      Attributes : pthread_mutexattr_t;
      Result     : Interfaces.C.POSIX_Error.Return_Code;

   begin
      pthread_mutexattr_init (Attributes, Result);
      if Result = Failure then
         raise STORAGE_ERROR;  --  should be ENOMEM
      end if;

      pthread_mutexattr_setprotocol (Attributes, PRIO_PROTECT, Result);

      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_mutexattr_setprotocol"));

      pthread_mutexattr_setprio_ceiling
         (Attributes, Interfaces.C.int (Prio), Result);

      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
        "GNULLI failure---pthread_mutexattr_setprio_ceiling"));

      pthread_mutex_init (pthread_mutex_t (L), Attributes, Result);

      if Result = Failure then
         raise STORAGE_ERROR;  --  should be ENOMEM ???
      end if;
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : in out Lock) is
      Result : Interfaces.C.POSIX_Error.Return_Code;

   begin
      pthread_mutex_destroy (pthread_mutex_t (L), Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_mutex_destroy"));
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   --  The error code EINVAL indicates either an uninitialized mutex or
   --  a priority ceiling violation. We assume that the former cannot
   --  occur in our system.
   procedure Write_Lock (L : in out Lock; Ceiling_Violation : out Boolean) is
      Result : Interfaces.C.POSIX_Error.Return_Code;
      Ceiling_Error : Boolean;
   begin
      pthread_mutex_lock (pthread_mutex_t (L), Result);
      Ceiling_Error := Result = Failure and then
        Interfaces.C.POSIX_Error.Get_Error_Code =
           Interfaces.C.POSIX_Error.Priority_Ceiling_Violation;
      pragma Assert (
        Result /= Failure or else Ceiling_Error
          or else Runtime_Assert_Shutdown (
            "GNULLI failure---pthread_mutex_lock"));

      Ceiling_Violation := Ceiling_Error;
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock (L : in out Lock; Ceiling_Violation : out Boolean) is
   begin
      Write_Lock (L, Ceiling_Violation);
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : in out Lock) is
      Result : Interfaces.C.POSIX_Error.Return_Code;

   begin
      pthread_mutex_unlock (pthread_mutex_t (L), Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_mutex_unlock"));
   end Unlock;

   ---------------------
   -- Initialize_Cond --
   ---------------------

   procedure Initialize_Cond (Cond : in out Condition_Variable) is
      Attributes : pthread_condattr_t;
      Result     : Interfaces.C.POSIX_Error.Return_Code;

   begin
      pthread_condattr_init (Attributes, Result);

      if Result = Failure then
         raise STORAGE_ERROR;  --  should be ENOMEM ???
      end if;

      pthread_cond_init (pthread_cond_t (Cond.CV), Attributes, Result);

      if Result = Failure then
         raise STORAGE_ERROR;  --  should be ENOMEM  ???
      end if;

      pthread_condattr_destroy (Attributes, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_condattr_destroy"));

      Cond.Someone_Is_Waiting := False;

   end Initialize_Cond;

   -------------------
   -- Finalize_Cond --
   -------------------

   procedure Finalize_Cond (Cond : in out Condition_Variable) is
      Result : Interfaces.C.POSIX_Error.Return_Code;

   begin
      pthread_cond_destroy (pthread_cond_t (Cond.CV), Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_cond_destroy"));
   end Finalize_Cond;

   ---------------
   -- Cond_Wait --
   ---------------

   procedure Cond_Wait
     (Cond : in out Condition_Variable;
      L    : in out Lock)
   is
      Result : Interfaces.C.POSIX_Error.Return_Code;

   begin

      --  Note that the following check is not perfect, since the
      --  Someone_Is_Waiting flag is reset without synchronization.  There is
      --  a window during which the flag is set but the wait has completed.
      --  However, the associated mutex is still held; another thread
      --  attempting to wait on the condition variable would have to use a
      --  different mutex, which is also illegal, so the worst that will
      --  happen is that the wrong error will be flagged.

      pragma Assert (
        not Cond.Someone_Is_Waiting or else Runtime_Assert_Shutdown (
          "GNULLI failure---More than one task" &
            " waiting on a condition variable"));
      Cond.Someone_Is_Waiting := True;

      pthread_cond_wait (
        pthread_cond_t (Cond.CV),
        pthread_mutex_t (L),
        Result);

      Cond.Someone_Is_Waiting := False;

      --  EINTR is not considered a failure.  We have been assured that
      --  Pthreads will soon guarantee that a thread will wake up from
      --  a condition variable wait after it handles a signal.  EINTR will
      --  probably go away at that point. ???

      pragma Assert (Result /= Failure or else
        Interfaces.C.POSIX_Error.Get_Error_Code =
           Interfaces.C.POSIX_Error.Interrupted_Function_Call or else
              Runtime_Assert_Shutdown (
        "GNULLI failure---pthread_cond_wait"));

   end Cond_Wait;

   -----------------------------
   --  Stimespec_to_timespec  --
   -----------------------------

   function Stimespec_to_timespec (S : Task_Clock.Stimespec)
     return Interfaces.C.POSIX_timers.timespec is
   begin
      return Interfaces.C.POSIX_timers.timespec'
        (tv_sec =>
            Interfaces.C.POSIX_timers.time_t
               (Task_Clock.Stimespec_Seconds (S)),
         tv_nsec =>
           Interfaces.C.POSIX_timers.Nanoseconds
              (Task_Clock.Stimespec_NSeconds (S)));
   end Stimespec_to_timespec;

   -----------------------------
   --  timespec_to_Stimespec  --
   -----------------------------

   function timespec_to_Stimespec (S : Interfaces.C.POSIX_timers.timespec)
     return Task_Clock.Stimespec is
   begin
      return Task_Clock.Time_Of (integer (S.tv_sec), integer (S.tv_nsec));
   end timespec_to_Stimespec;

   ---------------------
   -- Cond_Timed_Wait --
   ---------------------

   procedure Cond_Timed_Wait
     (Cond      : in out Condition_Variable;
      L         : in out Lock; Abs_Time : Task_Clock.Stimespec;
      Timed_Out : out Boolean)
   is
      Result : Interfaces.C.POSIX_Error.Return_Code;

   begin
      --  Note that the following check is not perfect, since the
      --  Someone_Is_Waiting flag is reset without synchronization.  There is
      --  a window during which the flag is set but the wait has completed.
      --  However, the associated mutex is still held; another thread
      --  attempting to wait on the condition variable would have to use a
      --  different mutex, which is also illegal, so the worst that will
      --  happen is that the wrong error will be flagged.

      pragma Assert (
        not Cond.Someone_Is_Waiting or else Runtime_Assert_Shutdown (
          "GNULLI failure---More than one task " &
            "waiting on a condition variable"));

      Cond.Someone_Is_Waiting := True;

      pthread_cond_timedwait (
        pthread_cond_t (Cond.CV),
        pthread_mutex_t (L),
        Stimespec_to_timespec (Abs_Time),
        Result);

      Cond.Someone_Is_Waiting := False;

      Timed_Out := Result = Failure and then
        Interfaces.C.POSIX_Error.Get_Error_Code =
          Interfaces.C.POSIX_Error.Resource_Temporarily_Unavailable;
      pragma Assert (Result /= Failure or else
            Interfaces.C.POSIX_Error.Get_Error_Code =
              Interfaces.C.POSIX_Error.Resource_Temporarily_Unavailable or else
                Runtime_Assert_Shutdown (
                  "GNULLI failure---pthread_cond_timedwait"));
   end Cond_Timed_Wait;

   -----------------
   -- Cond_Signal --
   -----------------

   procedure Cond_Signal (Cond : in out Condition_Variable) is
      Result : Interfaces.C.POSIX_Error.Return_Code;

   begin
      pthread_cond_signal (pthread_cond_t (Cond.CV), Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_cond_signal"));
   end Cond_Signal;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (T : TCB_Ptr; Prio : System.Any_Priority) is
      Attributes : pthread_attr_t;
      Result     : Interfaces.C.POSIX_Error.Return_Code;

   begin
      pthread_attr_init (Attributes, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_attr_init"));

      pthread_getschedattr (T.Thread, Attributes, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_getschedattr"));

      pthread_attr_setprio (Attributes, Priority_Type (Prio), Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_attr_setprio"));

      pthread_setschedattr (T.Thread, Attributes, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_setschedattr"));

      pthread_attr_destroy (Attributes, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_attr_destroy"));

   end Set_Priority;

   ----------------------
   -- Set_Own_Priority --
   ----------------------

   procedure Set_Own_Priority (Prio : System.Any_Priority) is
   begin
      Set_Priority (Self, Prio);
   end Set_Own_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : TCB_Ptr) return System.Any_Priority is
      Attributes : pthread_attr_t;
      Prio       : Priority_Type;
      Result     : Interfaces.C.POSIX_Error.Return_Code;

   begin
      pthread_attr_init (Attributes, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_attr_init"));

      pthread_getschedattr (T.Thread, Attributes, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_getschedattr"));

      pthread_attr_getprio (Attributes, Prio, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_getprio"));

      pthread_attr_destroy (Attributes, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_attr_destroy"));

      return System.Any_Priority (Prio);
   end Get_Priority;

   -----------------------
   --  Get_Own_Priority --
   -----------------------

   --  Note: this is specialized (rather than being done using a default
   --  parameter for Get_Priority) in case there is a specially efficient
   --  way of getting your own priority, which might well be the case in
   --  general (although is not the case in Pthreads).

   function Get_Own_Priority return System.Any_Priority is
   begin
      return Get_Priority (Self);
   end Get_Own_Priority;

   ----------------
   -- LL_Wrapper --
   ----------------

   procedure LL_Wrapper (T : TCB_Ptr) is
      Result  : Interfaces.C.POSIX_Error.Return_Code;
      Old_Set : aliased RTE.Signal_Set;

   begin
      pthread_setspecific (ATCB_Key, Pointer_to_Address (T), Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_setspecific"));

      RTE.sigprocmask (
        RTE.SIG_UNBLOCK, Unblocked_Signal_Mask'Access, Old_Set'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigprocmask"));

      --  Note that the following call may not return!

      T.LL_Entry_Point (T.LL_Arg);
   end LL_Wrapper;

   --------------------
   -- Create_LL_Task --
   --------------------

   procedure Create_LL_Task
     (Priority       : System.Any_Priority;
      Stack_Size     : Task_Storage_Size;
      Task_Info      : System.Task_Info.Task_Info_Type;
      LL_Entry_Point : LL_Task_Procedure_Access;
      Arg            : System.Address;
      T              : TCB_Ptr)
   is
      Attributes : pthread_attr_t;
      Result     : Interfaces.C.POSIX_Error.Return_Code;
      Old_Set    : aliased RTE.Signal_Set;

   begin
      T.LL_Entry_Point := LL_Entry_Point;
      T.LL_Arg := Arg;
      T.Stack_Size := Stack_Size;

      pthread_attr_init (Attributes, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_attr_init"));

      pthread_attr_setdetachstate (Attributes, 1, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_setdetachstate"));

      pthread_attr_setstacksize
         (Attributes, Interfaces.C.size_t (Stack_Size), Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_setstacksize"));

      pthread_attr_setprio (Attributes, Priority_Type (Priority), Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_attr_setprio"));

      --  It is not safe for the task to be created to accept signals until it
      --  has bound its TCB pointer to the thread with pthread_setspecific ().
      --  The handler wrappers use the TCB pointers to restore the stack limit.

      RTE.sigprocmask (
        RTE.SIG_BLOCK, Unblocked_Signal_Mask'Access, Old_Set'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigprocmask"));

      pthread_create (
        T.Thread,
        Attributes,
        LL_Wrapper'Address,
        Pointer_to_Address (T),
        Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_create"));

      pthread_attr_destroy (Attributes, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_attr_destroy"));

      RTE.sigprocmask (
        RTE.SIG_UNBLOCK, Unblocked_Signal_Mask'Access, Old_Set'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigprocmask"));

   end Create_LL_Task;

   ------------------
   -- Exit_LL_Task --
   ------------------

   procedure Exit_LL_Task is
   begin
      pthread_exit (System.Null_Address);
   end Exit_LL_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : TCB_Ptr) is
      Result : Interfaces.C.POSIX_Error.Return_Code;

   begin
      pthread_kill (T.Thread, Abort_Signal, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---pthread_kill"));
   end Abort_Task;

   ----------------
   -- Test_Abort --
   ----------------

   --  This procedure does nothing.  It is intended for systems without
   --  asynchronous abortion, where the runtime system would have to
   --  synchronously poll for pending abortions.  This should be done
   --  at least at every synchronization point.

   procedure Test_Abort is
   begin
      null;
   end Test_Abort;

   ---------------------
   -- Get_Stack_Limit --
   ---------------------

   function Get_Stack_Limit return System.Address is
   begin
      return Self.Stack_Limit;
   end Get_Stack_Limit;

   -------------------
   -- Abort_Wrapper --
   -------------------

   --  This is the handler called by the OS when an abort signal is
   --  received; it in turn calls the handler installed by the client.
   --  This procedure serves to isolate the client from the
   --  implementation-specific calling conventions of asynchronous
   --  handlers.

   procedure Abort_Wrapper
     (signo   : Integer;
      info    : integer;  --  eas RTE.siginfo_ptr;
      context : System.Address)
   is
      function Address_To_Call_State is new
        Unchecked_Conversion (System.Address, Pre_Call_State);

   begin
      Abort_Handler (Address_To_Call_State (context));
   end Abort_Wrapper;

   ---------------------------
   -- Install_Abort_Handler --
   ---------------------------

   procedure Install_Abort_Handler (Handler : Abort_Handler_Pointer) is
      act     : aliased RTE.struct_sigaction;
      old_act : aliased RTE.struct_sigaction;
      Result  : Interfaces.C.POSIX_Error.Return_Code;

   begin
      Abort_Handler := Handler;
      act.sa_handler := Abort_Wrapper'Address;
      RTE.sigemptyset (act.sa_mask'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigemptyset"));
      act.sa_flags := 0;

      RTE.sigaction (Abort_Signal, act'Access, old_act'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaction"));
   end Install_Abort_Handler;

   ---------------------------
   -- Install_Error_Handler --
   ---------------------------

   procedure Install_Error_Handler (Handler : System.Address) is
      act     : aliased RTE.struct_sigaction;
      old_act : aliased RTE.struct_sigaction;
      Result  : Interfaces.C.POSIX_Error.Return_Code;

   begin
      act.sa_handler := Handler;

      RTE.sigemptyset (act.sa_mask'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigemptyset"));
      RTE.sigaddset (act.sa_mask'Access, RTE.SIGILL, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));
      RTE.sigaddset (act.sa_mask'Access, RTE.SIGFPE, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));
      RTE.sigaddset (act.sa_mask'Access, RTE.SIGSEGV, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaddset"));
      act.sa_flags := 0;

      RTE.sigaction (RTE.SIGILL, act'Access, old_act'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaction"));

      RTE.sigaction (RTE.SIGFPE, act'Access, old_act'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaction"));

      RTE.sigaction (RTE.SIGSEGV, act'Access, old_act'Access, Result);
      pragma Assert (
        Result /= Failure or else Runtime_Assert_Shutdown (
          "GNULLI failure---sigaction"));

   end Install_Error_Handler;

   ------------------
   -- Test_And_Set --
   ------------------

   -------------------------
   -- Initialize_TAS_Cell --
   -------------------------
   procedure Initialize_TAS_Cell (Cell :    out TAS_Cell) is
   begin
      Cell.Value := False;
   end Initialize_TAS_Cell;
   -----------------------
   -- Finalize_TAS_Cell --
   -----------------------
   procedure Finalize_TAS_Cell   (Cell : in out TAS_Cell) is
   begin
      null;
   end Finalize_TAS_Cell;
   -----------
   -- Clear --
   -----------
   --
   --  This was not atomic with respect to another Test_and_Set in the
   --  original code.  Need it be???
   --
   procedure Clear        (Cell : in out TAS_Cell) is
   begin
      Cell.Value := False;
   end Clear;

   ------------
   -- Is_Set --
   ------------

   --
   --  This was not atomic with respect to another Test_and_Set in the
   --  original code.  Need it be???
   --
   function  Is_Set       (Cell : in     TAS_Cell) return Boolean is
   begin
      return Cell.Value;
   end Is_Set;
   ------------------
   -- Test_And_Set --
   ------------------
   procedure Test_And_Set (Cell : in out TAS_Cell; Result : out Boolean) is
      Error : Boolean;
   begin
      Write_Lock (Test_And_Set_Mutex, Error);

      if Cell.Value then
         Result := False;
      else
         Result :=  True;
         Cell.Value := True;
      end if;
      Unlock (Test_And_Set_Mutex);
   end Test_And_Set;

begin
   Initialize_Lock (System.Any_Priority'Last, Test_And_Set_Mutex);
end System.Task_Primitives;

------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                S Y S T E M . T A S K _ P R I M I T I V E S               --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.9 $                             --
--                                                                          --
--            Copyright (C) 1991-1997, Florida State University             --
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
with Interfaces.C.Extensions;
with Interfaces.C.POSIX_Error;
with Interfaces.C.POSIX_RTE;
with Interfaces.C.POSIX_timers;
with Interfaces.C.Pthreads;
with Interfaces.C.System_Constants;

with System.Compiler_Exceptions;
with System.Program_Info;
with System.Secondary_Stack;
with System.Storage_Elements;
with System.Task_Clock;
with System.Task_Specific_Data;
with System.Tasking;
with System.Tasking_Soft_Links;

with Unchecked_Conversion;

package body System.Task_Primitives is

   use Interfaces.C;
   use Interfaces.C.Extensions;
   use Interfaces.C.POSIX_RTE;
   use Interfaces.C.Pthreads;
   use Interfaces.C.System_Constants;

   use System.Compiler_Exceptions;
   use System.Secondary_Stack;
   use System.Storage_Elements;
   use System.Task_Clock;
   use System.Task_Specific_Data;
   use System.Tasking;

   package RTE  renames Interfaces.C.POSIX_RTE;
   package TSL  renames System.Tasking_Soft_Links;
   package CEXT renames Interfaces.C.Extensions;
   package CPTH renames Interfaces.C.Pthreads;

   Abort_Signal : constant := 34;
   --
   --  Serious MOJO:  The SGI pthreads library only supports the
   --                 unnamed signal number 34 for pthread_kill!
   --

   ATCB_Key : aliased pthread_key_t;

   procedure pthread_set_ada_tcb
     (Thread : pthread_t;
      TCB    : System.Address);

   pragma Import (C, pthread_set_ada_tcb, "pthread_set_ada_tcb",
     "pthread_set_ada_tcb");

   function To_void_ptr is new
     Unchecked_Conversion (TCB_Ptr, void_ptr);

   function To_TCB_Ptr is new
     Unchecked_Conversion (void_ptr, TCB_Ptr);

   function To_Task_ID is new
     Unchecked_Conversion (System.Address, Task_ID);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Initialize_Pthreads_Library;

   function Check_Abort_Status return Natural;

   procedure LL_Wrapper (T : TCB_Ptr);
   --  A wrapper procedure that is called from a new low-level task.
   --  It performs initializations for the new task and calls the
   --  user-specified startup procedure.

   -------------------------
   -- Initialize_LL_Tasks --
   -------------------------

   procedure Initialize_LL_Tasks (T : TCB_Ptr) is
      Result : int;

   begin
      T.LL_Entry_Point := null;
      T.Thread := pthread_self;

      Result := pthread_key_create (ATCB_Key'Access, null);

      if Result = FUNC_ERR then
         raise Storage_Error;               --  Insufficient resources.
      end if;

      T.Thread := pthread_self;

      pthread_set_ada_tcb (T.Thread, T.LL_Arg);

   end Initialize_LL_Tasks;

   ----------
   -- Self --
   ----------

   function Self return TCB_Ptr is

      function  pthread_get_current_ada_tcb  return System.Tasking.Task_ID;
      pragma Import (C,
                     pthread_get_current_ada_tcb,
                     "pthread_get_current_ada_tcb",
                     "pthread_get_current_ada_tcb");

   begin
      return To_TCB_Ptr (pthread_get_current_ada_tcb.LL_TCB'Address);
   end Self;

   ---------------------------------
   -- Initialize_Pthreads_Library --
   ---------------------------------

   procedure Initialize_Pthreads_Library is
      Init : aliased pthread_init;

      package PINF renames System.Program_Info;
      package C    renames Interfaces.C;

      Int_Result : C.Int;

      act     : aliased RTE.struct_sigaction;
      old_act : aliased RTE.struct_sigaction;
      use type Interfaces.C.POSIX_Error.Return_Code;

   begin
      --
      --  Make sure we have installed the abort handler prior to
      --  initializing the Pthreads library so any sproc'ed children
      --  will have the handler installed.
      --

--      act.sa_flags := 0;
--      act.sa_handler := Abort_Wrapper'Address;
--      RTE.sigemptyset (act.sa_mask'Access, Result);
--      pragma Assert (Result /= FUNC_ERR, "GNULLI failure---sigemptyset");
--
--      RTE.sigaction (Abort_Signal, act'Access, old_act'Access, Result);
--      pragma Assert (Result /= FUNC_ERR, "GNULLI failure---sigaction");

      Init.conf_initsize := C.Int (PINF.Pthread_Arena_Size);
      Init.max_sproc_count := C.Int (PINF.Max_Sproc_Count);
      Init.sproc_stack_size := C.size_t (PINF.Sproc_Stack_Size);
      Init.os_default_priority := C.Int (PINF.Os_Default_Priority);
      Init.os_sched_signal := C.Int (PINF.Pthread_Sched_Signal);
      Init.guard_pages :=  C.Int (PINF.Stack_Guard_Pages);
      Init.init_sproc_count := C.Int (PINF.Initial_Sproc_Count);
      Int_Result := pthread_exec_begin (Init'Access);
      pragma Assert (Int_Result /= FUNC_ERR,
                     "GNULLI failure --- pthread_exec_begin");
   end Initialize_Pthreads_Library;


   ---------------------
   -- Initialize_Lock --
   ---------------------

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : in out Lock)
   is
      Attributes : aliased CPTH.pthread_mutexattr_t;
      Result     : Interfaces.C.int;

      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__locking_policy",
                     "__locking_policy");

   begin
      Result := pthread_mutexattr_init (Attributes'Access);

      if Result = FUNC_ERR then
         raise STORAGE_ERROR;  --  should be ENOMEM
      end if;

      if Locking_Policy = 'C' then

         Result := pthread_mutexattr_setqueueorder
           (Attributes'Access, MUTEX_PRIORITY_CEILING);

         pragma Assert (Result /= FUNC_ERR,
           "GNULLI FUNC_ERR---pthread_mutexattr_setprotocol");

         Result := pthread_mutexattr_setceilingprio
            (Attributes'Access, Interfaces.C.int (Prio));

         pragma Assert (Result /= FUNC_ERR,
           "GNULLI FUNC_ERR---pthread_mutexattr_setprio_ceiling");

      end if;

      Result := pthread_mutex_init (L.mutex'Access, Attributes'Access);

      if Result = FUNC_ERR then
         Result := pthread_mutexattr_destroy (Attributes'Access);
         raise STORAGE_ERROR;  --  should be ENOMEM ???
      end if;

      Result := pthread_mutexattr_destroy (Attributes'Access);

   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : in out Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_destroy (L.mutex'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_mutex_destroy");
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   --  The error code EINVAL indicates either an uninitialized mutex or
   --  a priority ceiling violation. We assume that the former cannot
   --  occur in our system.
   procedure Write_Lock (L : in out Lock; Ceiling_Violation : out Boolean) is
      Result : Interfaces.C.int;
      Ceiling_Error : Boolean;
      use Interfaces.C.POSIX_Error;
   begin
      Result := pthread_mutex_lock (L.mutex'Access);

      Ceiling_Error := Result = FUNC_ERR and then
        Interfaces.C.POSIX_Error.Get_Error_Code =
           Interfaces.C.POSIX_Error.Priority_Ceiling_Violation;
      pragma Assert (Result /= FUNC_ERR,
        "GNULLI failure---pthread_mutex_lock");

      Ceiling_Violation := Ceiling_Error;
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock (L : in out Lock; Ceiling_Violation : out Boolean)
      renames Write_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : in out Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_unlock (L.mutex'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI FUNC_ERR---pthread_mutex_unlock");
   end Unlock;

   ---------------------
   -- Initialize_Cond --
   ---------------------

   procedure Initialize_Cond (Cond : in out Condition_Variable) is
      Attributes : aliased Interfaces.C.Pthreads.pthread_condattr_t;
      Result     : Interfaces.C.int;
   begin
      Result := pthread_condattr_init (Attributes'Access);

      if Result = FUNC_ERR then
         raise STORAGE_ERROR;  --  should be ENOMEM ???
      end if;

      Result := pthread_cond_init (Cond.CV'Access, Attributes'Access);

      if Result = FUNC_ERR then
         raise STORAGE_ERROR;  --  should be ENOMEM  ???
      end if;

      Result := pthread_condattr_destroy (Attributes'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI FUNC_ERR---pthread_condattr_destroy");

   end Initialize_Cond;

   -------------------
   -- Finalize_Cond --
   -------------------

   procedure Finalize_Cond (Cond : in out Condition_Variable) is
      Result : Interfaces.C.int;

   begin
      Result := pthread_cond_destroy (Cond.CV'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_cond_destroy");
   end Finalize_Cond;


   ---------------
   -- Cond_Wait --
   ---------------

   procedure Cond_Wait (Cond : in out Condition_Variable; L : in out Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_cond_wait (Cond.CV'Access, L.mutex'Access);
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI failure---pthread_cond_wait");
   end Cond_Wait;

   ---------------------
   -- Cond_Timed_Wait --
   ---------------------

   procedure Cond_Timed_Wait
     (Cond      : in out Condition_Variable;
      L         : in out Lock;
      Abs_Time  : System.Task_Clock.Stimespec;
      Timed_Out : out Boolean) is

      Result : Interfaces.C.int;
      TV     : aliased timeval;

      use Interfaces.C.POSIX_Error;

   begin
      Timed_Out := False;  --  Assume success until we know otherwise

      TV.tv_sec := C.Long (Interfaces.C.POSIX_timers.time_t
        (Task_Clock.Stimespec_Seconds (Abs_Time)));

      TV.tv_usec := C.Long (Interfaces.C.POSIX_timers.Nanoseconds
        (Task_Clock.Stimespec_NSeconds (Abs_Time) / 1000));

      Result := pthread_cond_timedwait
        (Cond.CV'Access, L.mutex'Access, TV'Access);

      if Result = FUNC_ERR then
         if Interfaces.C.POSIX_Error.Get_Error_Code =
           Interfaces.C.POSIX_Error.Resource_Temporarily_Unavailable then
            Timed_Out := True;
         else
            pragma Assert (False, "GNULLI failure---pthread_cond_timedwait");
            null;
         end if;
      end if;

   end Cond_Timed_Wait;

   -----------------
   -- Cond_Signal --
   -----------------

   procedure Cond_Signal (Cond : in out Condition_Variable) is
      Result : Interfaces.C.int;
   begin
      Result :=  pthread_cond_signal (Cond.CV'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_cond_signal");
   end Cond_Signal;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T : TCB_Ptr;
      Prio : System.Any_Priority) is

      Result : Interfaces.C.int;
      Thread : Interfaces.C.Pthreads.pthread_t renames T.Thread;

   begin
      T.LL_Priority := Prio;
      Result := pthread_setprio (Thread, Interfaces.C.int (Prio));
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_setprio");
   end Set_Priority;

   ----------------------
   -- Set_Own_Priority --
   ----------------------

   procedure Set_Own_Priority (Prio : System.Any_Priority) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_setprio (pthread_self, Interfaces.C.int (Prio));
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_setprio");
      Self.LL_TCB.LL_Priority := Prio;
   end Set_Own_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : TCB_Ptr) return System.Any_Priority is
   begin
      return T.LL_Priority;
   end Get_Priority;

   -----------------------
   --  Get_Own_Priority --
   -----------------------

   function Get_Own_Priority return System.Any_Priority is
   begin
      return Self.LL_TCB.LL_Priority;
   end Get_Own_Priority;

   --------------------
   -- Create_LL_Task --
   --------------------

   procedure Create_LL_Task
     (Priority       : System.Any_Priority;
      Stack_Size     : Task_Storage_Size;
      Task_Info      : System.Task_Info.Task_Info_Type;
      LL_Entry_Point : LL_Task_Procedure_Access;
      Arg            : System.Address;
      T              : TCB_Ptr) is

      use Interfaces.C.Pthreads;
      use System.Task_Info;

      Attributes : aliased CPTH.pthread_attr_t;
      Result     : Interfaces.C.int;

      function To_Start_Addr is new
        Unchecked_Conversion (System.Address, start_addr);

      function To_Resource_T is new Unchecked_Conversion
        (System.Task_Info.Resource_Vector_T, Interfaces.C.Pthreads.Resource_T);

   begin
      T.LL_Entry_Point := LL_Entry_Point;
      T.LL_Arg := Arg;
      T.Stack_Size := Stack_Size;
      T.LL_Priority := Priority;

      Result := pthread_attr_init (Attributes'Access);
      pragma Assert (Result /= FUNC_ERR, "GNULLI failure---pthread_attr_init");

      Result := pthread_attr_setdetachstate (Attributes'Access, 1);
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI failure---pthread_setdetachstate");

      Result := pthread_attr_setstacksize
        (Attributes'Access, Interfaces.C.size_t (Stack_Size));
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI failure---pthread_setstacksize");

      Result := pthread_attr_setprio
        (Attributes'Access, Interfaces.C.int (Priority));
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI failure---pthread_attr_setprio");

      if Task_Info /= null then

         Result := pthread_attr_setresources
           (Attributes'Access, To_Resource_T (Task_Info.Thread_Resources));
         pragma Assert (Result /= FUNC_ERR,
                      "GNULLI failure -- pthread_attr_setresources");

         if Task_Info.Thread_Timeslice /= 0.0 then
            declare
               Ts : Stimespec := Duration_To_Stimespec
                 (Task_Info.Thread_Timeslice);
               Tv : aliased timeval;
            begin
               Tv.tv_sec  := C.Long (Stimespec_Seconds (Ts));
               Tv.tv_usec := C.Long (Stimespec_NSeconds (Ts) / 1000);
               Result := pthread_attr_set_tslice
                 (Attributes'Access, Tv'Access);
            end;
         end if;

         if Task_Info.Bound_To_Sproc then
            Result := pthread_attr_set_boundtosproc
              (Attributes'Access, PTHREAD_BOUND);
            Result := pthread_attr_set_bsproc
              (Attributes'Access, Task_Info.Sproc);
         end if;

      end if;

      Result := pthread_create
        (T.Thread'Access,
         Attributes'Access,
         To_Start_Addr (LL_Wrapper'Address),
         T.all'Address);
      pragma Assert (Result /= FUNC_ERR, "GNULLI failure---pthread_create");

      Result := pthread_attr_destroy (Attributes'Access);
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI failure---pthread_attr_destroy");

   end Create_LL_Task;

   -----------------
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
      Result     : Interfaces.C.int;

   begin
      Result := pthread_kill (T.Thread, Abort_Signal);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_kill");
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

   ---------------------------
   -- Install_Abort_Handler --
   ---------------------------

   procedure Install_Abort_Handler (Handler : Abort_Handler_Pointer) is
   begin
      null;
   end Install_Abort_Handler;

   ---------------------------
   -- Install_Error_Handler --
   ---------------------------

   procedure Install_Error_Handler (Handler : System.Address) is

      Temp : Address;

      use Interfaces.C.Pthreads;

   begin
      --  Set up the soft links to tasking services used in the absence of
      --  tasking.  These replace tasking-free defaults.

      Temp := TSL.Get_Jmpbuf_Address.all;
      pthread_set_jumpbuf_address (Temp);

      Temp := TSL.Get_Sec_Stack_Addr.all;
      pthread_set_sec_stack_addr  (Temp);

      Temp := TSL.Get_Exc_Stack_Addr.all;
      pthread_set_exc_stack_addr  (Temp);

      TSL.Check_Abort_Status := Check_Abort_Status'Access;

      TSL.Get_Jmpbuf_Address := pthread_get_jumpbuf_address'Access;
      TSL.Set_Jmpbuf_Address := pthread_set_jumpbuf_address'Access;

      TSL.Get_Gnat_Exception := pthread_get_exception'Access;
      TSL.Set_Gnat_Exception := pthread_set_exception'Access;

      TSL.Get_Sec_Stack_Addr := pthread_get_sec_stack_addr'Access;
      TSL.Set_Sec_Stack_Addr := pthread_set_sec_stack_addr'Access;

      TSL.Get_Exc_Stack_Addr := pthread_get_exc_stack_addr'Access;
      TSL.Set_Exc_Stack_Addr := pthread_set_exc_stack_addr'Access;

   end Install_Error_Handler;


   ---------------
   -- LL_Assert --
   ---------------

   procedure LL_Assert (B : Boolean; M : String) is
   begin
      null;
   end LL_Assert;


   ----------------
   -- LL_Wrapper --
   ----------------

   procedure LL_Wrapper (T : TCB_Ptr) is
      Exc_Stack : String (1 .. 256);
      Exc_Base  : Address := Exc_Stack (Exc_Stack'Last)'Address + 1;
      Self      : Task_ID := To_Task_ID (T.LL_Arg);
   begin
      pthread_set_ada_tcb (T.Thread, T.LL_Arg);

      Exc_Base := Exc_Base - (Exc_Base mod 16);
      pthread_set_exc_stack_addr (Exc_Base);

      pthread_set_sec_stack_addr (Self.Compiler_Data.Sec_Stack_Addr);

      --  Note that the following call may not return!
      T.LL_Entry_Point (T.LL_Arg);
   end LL_Wrapper;

   function Check_Abort_Status return Natural is
      T : Task_ID := Self;
   begin
      if T /= null and then T.Deferral_Level = 0
        and then T.Pending_ATC_Level < T.ATC_Nesting_Level then
         return 1;
      else
         return 0;
      end if;
   end Check_Abort_Status;

   --------------------------
   -- Test and Set support --
   --------------------------

   --
   --  All of the Arena stuff should really be moved elsewhere!!!
   --

   CONF_INITUSERS       : constant :=  1;  --  set max # processes using a lock
   CONF_INITSIZE        : constant :=  2;  --  set size of arena
   CONF_GETUSERS        : constant :=  3;  --  return max # processes
   CONF_GETSIZE         : constant :=  4;  --  return size of arena
   CONF_HISTON          : constant :=  5;  --  enable global semaphore history
   CONF_HISTOFF         : constant :=  6;  --  disable global semaphore history
   CONF_HISTFETCH       : constant :=  7;  --  fetch history
   CONF_HISTRESET       : constant :=  8;  --  clear all semaphore history
   CONF_LOCKTYPE        : constant :=  9;  --  set locktype
   CONF_STHREADIOON     : constant :=  10; --  enable single-threading of stdio
   CONF_STHREADIOOFF    : constant :=  11; --  disable single-threading stdio
   CONF_STHREADMISCON   : constant :=  12; --  enable misc single threading
   CONF_STHREADMISCOFF  : constant :=  13; --  disable misc single threading
   CONF_STHREADMALLOCON : constant :=  14; --  enable malloc single threading
   CONF_STHREADMALLOCOFF : constant :=  15; --  disable malloc single threading
   CONF_ARENATYPE       : constant :=  16; --  set whether supports SHARED only
--                                             access
   CONF_CHMOD           : constant :=  17; --  set access rights to locks, etc.
   CONF_HISTSIZE        : constant :=  18; --  max # of entries in history
   CONF_ATTACHADDR      : constant :=  19; --  set arena attach address
   CONF_AUTOGROW        : constant :=  20; --  set whether arena autogrows
   CONF_AUTORESV        : constant :=  21; --  set whether arena autoresvs

   US_GENERAL        : constant :=  0;  --  both shared/unshared accedd
   US_SHAREDONLY     : constant :=  1;  --  only shared proc access
   US_TRACING        : constant :=  2;  --  shared tracing
   US_CASONLY        : constant :=  4;  --  arena supports only cas operations

   US_NODEBUG        : constant :=  0;  -- no debugging
   US_DEBUG          : constant :=  1;  -- default debug & meter
   US_DEBUGPLUS      : constant :=  2;  --  and check for double trip, etc
   US_CASTYPE        : constant :=  3;  -- internal only

   CLEAR_VAL : constant := 0;
   SET_VAL   : constant := 1;

   type Char_Ptr is access all Interfaces.C.char;

   type usptr_t is null record; -- opaque type
   type usptr_t_ptr is access all usptr_t;

   Arena_Name_Prefix : aliased Interfaces.C.char_array :=
     (1 => Interfaces.C.char'('t'),
      2 => Interfaces.C.char'('a'),
      3 => Interfaces.C.char'('s'),
      4 => Interfaces.C.char (Ascii.Nul));

   function tempnam (dir : Char_Ptr; pfx : Char_Ptr) return Char_Ptr;
   pragma Import (C, tempnam, "tempnam", "tempnam");

   function usinit (filename : Char_Ptr) return usptr_t_ptr;
   pragma Import (C, usinit, "usinit", "usinit");

   procedure usconfig (cmd : Interfaces.C.int; value : Interfaces.C.int);
   pragma Import (C, usconfig, "usconfig", "usconfig");

   type usnewlock_ptr is access function  (arena : usptr_t_ptr) return ulock_t;

   usnewlock : usnewlock_ptr;
   pragma Import (C, usnewlock, "_nlock", "_nlock");

   type usfreelock_ptr is access
     procedure (lock : ulock_t; arena : usptr_t_ptr);

   usfreelock : usfreelock_ptr;
   pragma Import (C, usfreelock, "_freelock", "_freelock");

   type uscas_ptr is access function
     (Swap_Loc  : System.Address;
      old_value : Interfaces.C.unsigned;
      new_value : Interfaces.C.unsigned;
      arena     : usptr_t_ptr)
      return      Interfaces.C.int;

   uscas : uscas_ptr;
   pragma Import (C, uscas, "_cas", "_cas");

   Arena_Name : constant Char_Ptr :=
     tempnam (null, Arena_Name_Prefix (Arena_Name_Prefix'First)'Access);

   TAS_usptr  : aliased usptr_t_ptr;

   function To_Address is new Unchecked_Conversion (ulock_t, System.Address);

   procedure Perror (Arg : String);
   pragma Import (C, Perror, "perror", "perror");

   procedure Initialize_TAS_Cell (Cell : out TAS_Cell) is
   begin
      null;
--      Cell.ulock := usnewlock (TAS_usptr);
--      pragma Assert (Cell.ulock /= null, "usnewlock failed");
   end Initialize_TAS_Cell;

   procedure Finalize_TAS_Cell (Cell : in out TAS_Cell) is
   begin
      null;
--      Clear (Cell);
--      usfreelock (Cell.ulock, TAS_usptr);
   end Finalize_TAS_Cell;

   procedure Clear (Cell : in out TAS_Cell) is
--      Old_Val : Interfaces.C.Unsigned;
   begin
--      loop
--         Old_Val := Cell.ulock.all;
--         exit when (uscas (Cell.ulock, Old_Val, CLEAR_VAL, TAS_usptr) /= 0);
--      end loop;
      Cell.Value := CLEAR_VAL;
   end Clear;

   procedure Test_And_Set (Cell : in out TAS_Cell; Result : out Boolean) is
      Old_Val : Interfaces.C.Unsigned;
   begin
--       loop
--          Old_Val := Cell.ulock.all;
--          exit when uscas (Cell.ulock, Old_Val, SET_VAL, TAS_usptr) /= 0;
--       end loop;
--       Result := Old_Val = CLEAR_VAL;
      loop
         Old_Val := Cell.Value;
         exit when
           uscas (Cell.Value'Address, Old_Val, SET_VAL, TAS_usptr) /= 0;
      end loop;
      Result := Old_Val = CLEAR_VAL;
   end Test_And_Set;

   function  Is_Set       (Cell : in     TAS_Cell) return Boolean is
   begin
--       return Cell.ulock.all = SET_VAL;
      return Cell.Value = SET_VAL;
   end Is_Set;

begin
   Initialize_Pthreads_Library;
end System.Task_Primitives;

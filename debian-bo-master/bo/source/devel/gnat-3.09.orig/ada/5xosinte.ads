------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.2 $                            --
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

--  This is a SNI (DCE THREAD) version of this package.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

--  This version is for POSIX-like operating systems

with Interfaces.C;
package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-mdce");

   subtype int            is Interfaces.C.int;
   subtype short          is Interfaces.C.short;
   subtype long           is Interfaces.C.long;
   subtype unsigned       is Interfaces.C.unsigned;
   subtype unsigned_short is Interfaces.C.unsigned_short;
   subtype unsigned_long  is Interfaces.C.unsigned_long;
   subtype unsigned_char  is Interfaces.C.unsigned_char;
   subtype plain_char     is Interfaces.C.plain_char;
   subtype size_t         is Interfaces.C.size_t;

   -----------
   -- Errno --
   -----------

   function errno return int;
   pragma Import (C, errno, "__get_errno");

   --  NAMEs not used are commented-out
   --  NAMEs not supported on this system have __NAME for value

   --  E2BIG    : constant := 7;
   --  EACCES   : constant := 13;
   EAGAIN   : constant := 11;
   --  EBADF    : constant := 9;
   --  EBUSY    : constant := 16;
   --  ECHILD   : constant := 10;
   --  EDEADLK  : constant := 45;
   --  EDOM     : constant := 33;
   --  EEXIST   : constant := 17;
   --  EFAULT   : constant := 14;
   --  EFBIG    : constant := 27;
   EINTR    : constant := 4;
   EINVAL   : constant := 22;
   --  EIO      : constant := 5;
   --  EISDIR   : constant := 21;
   --  EMFILE   : constant := 24;
   --  EMLINK   : constant := 31;
   --  ENAMETOOLONG : constant := 78;
   --  ENFILE   : constant := 23;
   --  ENODEV   : constant := 19;
   --  ENOENT   : constant := 2;
   --  ENOEXEC  : constant := 8;
   --  ENOLCK   : constant := 46;
   ENOMEM   : constant := 12;
   --  ENOSPC   : constant := 28;
   --  ENOSYS   : constant := 89;
   --  ENOTDIR  : constant := 20;
   --  ENOTEMPTY    : constant := 93;
   --  ENOTTY   : constant := 25;
   --  ENXIO    : constant := 6;
   --  EPERM    : constant := 1;
   --  EPIPE    : constant := 32;
   --  ERANGE   : constant := 34;
   --  EROFS    : constant := 30;
   --  ESPIPE   : constant := 29;
   --  ESRCH    : constant := 3;
   ETIMEDOUT    : constant := 145;
   --  EXDEV    : constant := 18;

   -------------
   -- Signals --
   -------------

   NSIG : constant := 32;
   type Signal is new int range 0 .. Interfaces.C."-" (NSIG, 1);
   for Signal'Size use int'Size;

   --  NAMEs not used are commented-out
   --  NAMEs not supported on this system have __NAME for value

   SIGHUP     : constant := 1; --  hangup
   SIGINT     : constant := 2; --  interrupt (rubout)
   SIGQUIT    : constant := 3; --  quit (ASCD FS)
   SIGILL     : constant := 4; --  illegal instruction (not reset)
   SIGTRAP    : constant := 5; --  trace trap (not reset)
   SIGIOT     : constant := 6; --  IOT instruction
   SIGABRT    : constant := 6; --  used by abort, replace SIGIOT in the  future
   SIGEMT     : constant := 7; --  EMT instruction
   SIGFPE     : constant := 8; --  floating point exception
   SIGKILL    : constant := 9; --  kill (cannot be caught or ignored)
   SIGBUS     : constant := 10; --  bus error
   SIGSEGV    : constant := 11; --  segmentation violation
   SIGSYS     : constant := 12; --  bad argument to system call
   SIGPIPE    : constant := 13; --  write on a pipe with no one to read it
   SIGALRM    : constant := 14; --  alarm clock
   SIGTERM    : constant := 15; --  software termination signal from kill
   SIGUSR1    : constant := 16; --  user defined signal 1
   SIGUSR2    : constant := 17; --  user defined signal 2
   SIGCLD     : constant := 18; --  alias for SIGCHLD
   SIGCHLD    : constant := 18; --  child status change
   SIGPWR     : constant := 19; --  power-fail restart
   SIGWINCH   : constant := 20; --  window size change
   SIGURG     : constant := 21; --  urgent condition on IO channel
   SIGPOLL    : constant := 22; --  pollable event occurred
   SIGIO      : constant := 22; --  I/O possible (Solaris SIGPOLL alias)
   SIGSTOP    : constant := 23; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 24; --  user stop requested from tty
   SIGCONT    : constant := 25; --  stopped process has been continued
   SIGTTIN    : constant := 26; --  background tty read attempted
   SIGTTOU    : constant := 27; --  background tty write attempted
   SIGVTALRM  : constant := 28; --  virtual timer expired
   SIGPROF    : constant := 29; --  profiling timer expired
   SIGXCPU    : constant := 30; --  CPU time limit exceeded
   SIGXFSZ    : constant := 31; --  filesize limit exceeded
   SIGWAITING : constant := 0;  --  process's lwps blocked (Solaris)
   SIGLWP     : constant := 0;  --  used by thread library (Solaris)
   SIGFREEZE  : constant := 0;  --  used by CPR (Solaris)
   SIGTHAW    : constant := 0;  --  used by CPR (Solaris)
   SIGCANCEL  : constant := 0;  --  used for thread cancel (Solaris)
   SIG32      : constant := 0;  --  reserved for kernel (IRIX)
   SIGUNUSED  : constant := 0;  --  unused signal (Linux)
   SIGSTKFLT  : constant := 0;  --  coprocessor stack fault (Linux)
   SIGLOST    : constant := 0;  --  SIGIO alias (Linux)

   type sigset_t is private;

   function sigaddset
     (set : access sigset_t;
      sig : Signal)
     return int;
   pragma Import (C, sigaddset, "sigaddset");

   function sigdelset
     (set : access sigset_t;
      sig : Signal)
     return int;
   pragma Import (C, sigdelset, "sigdelset");

   function sigfillset
     (set : access sigset_t)
     return int;
   pragma Import (C, sigfillset, "sigfillset");

   function sigismember
     (set : access sigset_t;
      sig : Signal)
     return int;
   pragma Import (C, sigismember, "sigismember");

   function sigemptyset
     (set : access sigset_t)
     return int;
   pragma Import (C, sigemptyset, "sigemptyset");

   type union_type_3 is new String (1 .. 116);
   type siginfo_t is record
      si_signo     : int;
      si_code      : int;
      si_errno     : int;
      X_data       : union_type_3;
   end record;
   pragma Convention (C, siginfo_t);

   --  The types mcontext_t and gregset_t are part of the ucontext_t
   --  information, which is specific to Solaris2.4 for SPARC
   --  The ucontext_t info seems to be used by the handler
   --  for SIGSEGV to decide whether it is a Storage_Error (stack overflow) or
   --  a Constraint_Error (bad pointer).  The original code that did this
   --  is suspect, so it is not clear whether we really need this part of
   --  the signal context information, or perhaps something else.
   --  More analysis is needed, after which these declarations may need to
   --  be changed.

   FPE_INTDIV  : constant := 1; --  integer divide by zero
   FPE_INTOVF  : constant := 2; --  integer overflow
   FPE_FLTDIV  : constant := 3; --  floating point divide by zero
   FPE_FLTOVF  : constant := 4; --  floating point overflow
   FPE_FLTUND  : constant := 5; --  floating point underflow
   FPE_FLTRES  : constant := 6; --  floating point inexact result
   FPE_FLTINV  : constant := 7; --  invalid floating point operation
   FPE_FLTSUB  : constant := 8; --  subscript out of range

   SEGV_MAPERR : constant := 1; --  address not mapped to object
   SEGV_ACCERR : constant := 2; --  invalid permissions

   BUS_ADRALN  : constant := 1; --  invalid address alignment
   BUS_ADRERR  : constant := 2; --  non-existent physical address
   BUS_OBJERR  : constant := 3; --  object specific hardware error

   ILL_ILLOPC  : constant := 1; --  illegal opcode
   ILL_ILLOPN  : constant := 2; --  illegal operand
   ILL_ILLADR  : constant := 3; --  illegal addressing mode
   ILL_ILLTRP  : constant := 4; --  illegal trap
   ILL_PRVOPC  : constant := 5; --  privileged opcode
   ILL_PRVREG  : constant := 6; --  privileged register
   ILL_COPROC  : constant := 7; --  co-processor
   ILL_BADSTK  : constant := 8; --  bad stack

   type greg_t is new int;
   type freg_t is new float;

   type gregset_t is array (integer range 0 .. 35) of greg_t;
   type fpregs_t is array (integer range 0 .. 31) of freg_t;

   type fpregset_t is record
      fp_regs  : fpregs_t;
      fp_csr   : unsigned;
      fp_pad   : unsigned;
   end record;
   pragma Convention (C, fpregset_t);

   type mcontext_t is record
      gpregs       : gregset_t;
      fpregs       : fpregset_t;
   end record;
   pragma Convention (C, mcontext_t);

   type stack_t is record
      ss_sp        : System.Address;
      ss_size      : int;
      ss_flags     : int;
   end record;
   pragma Convention (C, stack_t);

   type array_type_8 is array (integer range 0 .. 47) of long;
   type ucontext_t is record
      uc_flags     : unsigned_long;
      uc_link      : System.Address;
      uc_sigmask   : sigset_t;
      uc_stack     : stack_t;
      uc_mcontext  : mcontext_t;
      uc_filler    : array_type_8;
   end record;
   pragma Convention (C, ucontext_t);

   type Signal_Handler is access procedure
      (signo   : Signal;
       info    : access siginfo_t;
       context : access ucontext_t);

   type union_type_1 is new plain_char;
   type array_type_2 is array (integer range 0 .. 1) of int;
   type struct_sigaction is record
      sa_flags     : int;
      sa_handler   : System.Address;
      sa_mask      : sigset_t;
      sa_resv      : array_type_2;
   end record;
   pragma Convention (C, struct_sigaction);

   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;

   --  SA_NOCLDSTOP : constant := 131072;
   --  not used
   --  SA_SIGINFO : constant := 8;
   --  not used

   SIG_DFL : constant := 0;
   SIG_IGN : constant := 1;
   --  SIG_ERR : constant := -1;
   --  not used

   function sigaction
     (sig  : Signal;
      act  : access struct_sigaction;
      oact : access struct_sigaction)
     return int;
   pragma Import (C, sigaction, "sigaction");

   ----------
   -- Time --
   ----------

   type timespec is private;

   function clock_gettime
     (tp       : access timespec)
      return int;
   --  DCE_THREADS has a nonstandard clock_gettime

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   type struct_timezone is record
      tz_minuteswest  : int;
      tz_dsttime   : int;
   end record;
   pragma Convention (C, struct_timezone);
   type struct_timeval is private;
   --  This is needed on systems that do not have clock_gettime()
   --  but do have gettimeofday().

   function To_Duration (TV : struct_timeval) return Duration;
   pragma Inline (To_Duration);

   function To_Timeval (D : Duration) return struct_timeval;
   pragma Inline (To_Timeval);

   function gettimeofday
     (tv : access struct_timeval;
      tz : access struct_timezone) return int;
   pragma Import (C, gettimeofday, "gettimeofday");

   --  add a hook to locate the Epoch, for use with Calendar????

   -------------------------
   -- Priority Scheduling --
   -------------------------

   MIN_PRIO : constant := 0;
   MAX_PRIO : constant := 0;

   SCHED_FIFO  : constant := 0;
   SCHED_RR    : constant := 1;
   SCHED_OTHER : constant := 2;

   -------------
   -- Process --
   -------------

   type pid_t is private;

   function kill
     (pid : pid_t;
      sig : Signal)
   return int;
   pragma Import (C, kill, "kill");

   function getpid return pid_t;
   pragma Import (C, getpid, "getpid");

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;
   type pthread_t           is private;
   type pthread_mutex_t     is limited private;
   type pthread_cond_t      is limited private;
   type pthread_attr_t      is limited private;
   type pthread_mutexattr_t is limited private;
   type pthread_condattr_t  is limited private;
   type pthread_key_t       is private;

   ---------------------------
   --  POSIX.1c  Section 3  --
   ---------------------------

   function sigwait
     (set : access sigset_t;
      sig : access Signal)
     return int;
   --  DCE_THREADS has a nonstandard sigwait

   function pthread_kill
     (thread : pthread_t;
      sig    : Signal)
     return int;
   --  DCE_THREADS doesn't have pthread_kill

   function pthread_sigmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t)
     return int;
   --  DCE_THREADS does not have pthread_sigmask

   ----------------------------
   --  POSIX.1c  Section 11  --
   ----------------------------

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t)
     return int;
   --  DCE_THREADS has a nonstandard pthread_mutexattr_init.

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t)
     return int;
   --  DCE_THREADS has a nonstandard pthread_mutexattr_destroy

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t)
     return int;
   --  DCE_THREADS has a nonstandard pthread_mutex_init

   function pthread_mutex_destroy
     (mutex : access pthread_mutex_t)
     return  int;
   --  DCE_THREADS has a nonstandard pthread_mutex_destroy

   function pthread_mutex_lock
     (mutex : access pthread_mutex_t)
     return int;
   --  DCE_THREADS has nonstandard pthread_mutex_lock

   function pthread_mutex_unlock
     (mutex : access pthread_mutex_t)
     return int;
   --  DCE_THREADS has nonstandard pthread_mutex_lock

   function pthread_condattr_init
     (attr : access pthread_condattr_t)
     return int;
   --  DCE_THREADS has nonstandard pthread_condattr_init

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t)
     return int;
   --  DCE_THREADS has nonstandard pthread_condattr_destroy

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t)
     return int;
   --  DCE_THREADS has nonstandard pthread_cond_init

   function pthread_cond_destroy
     (cond : access pthread_cond_t)
     return int;
   --  DCE_THREADS has nonstandard pthread_cond_destroy

   function pthread_cond_signal
     (cond : access pthread_cond_t)
     return int;
   --  DCE_THREADS has nonstandard pthread_cond_signal

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t)
     return  int;
   --  DCE_THREADS has a nonstandard pthread_cond_wait

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec)
     return    int;
   --  DCE_THREADS has a nonstandard pthread_cond_timedwait

   ----------------------------
   --  POSIX.1c  Section 13  --
   ----------------------------

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int)
     return int;
   --  DCE_THREADS doesn't have pthread_mutexattr_setprotocol

   function pthread_mutexattr_getprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : access int)
     return int;
   --  DCE_THREADS doesn't have pthread_mutexattr_getprotocol

   function pthread_mutexattr_setprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : int)
     return int;
   --  DCE_THREADS doesn't have pthread_mutexattr_setprioceiling

   function pthread_mutexattr_getprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : access int)
     return int;
   --  DCE_THREADS doesn't have pthread_mutexattr_getprioceiling

   type struct_sched_param is record
      sched_priority : int;  --  scheduling priority
   end record;

   function pthread_getschedparam
     (thread : pthread_t;
      policy : access int;
      param  : access struct_sched_param)
     return int;
   --  DCE_THREADS does not have pthread_getschedparam

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param)
     return int;
   --  DCE_THREADS does not have pthread_setschedparam

   function pthread_attr_setscope
     (attr            : access pthread_attr_t;
      contentionscope : int)
     return int;

   function pthread_attr_getscope
     (attr            : access pthread_attr_t;
      contentionscope : access int)
     return int;

   function pthread_attr_setinheritsched
     (attr            : access pthread_attr_t;
      inheritsched : int)
     return int;
   --  DCE_THREADS has a nonstandard pthread_attr_setinheritsched

   function pthread_attr_getinheritsched
     (attr         : access pthread_attr_t;
      inheritsched : access int)
     return int;
   --  DCE_THREADS has a nonstandard pthread_attr_getinheritsched

   function pthread_attr_setschedpolicy
     (attr   : access pthread_attr_t;
      policy : int)
     return int;
   --  DCE_THREADS has a nonstandard pthread_attr_setschedpolicy

   function pthread_attr_getschedpolicy
     (attr   : access pthread_attr_t;
      policy : access int)
     return int;
   --  DCE_THREADS has a nonstandard pthread_attr_getschedpolicy

   function pthread_attr_setschedparam
     (attr        : access pthread_attr_t;
      sched_param : int)
     return int;

   function pthread_attr_getschedparam
     (attr        : access pthread_attr_t;
      sched_param : access int)
     return int;

   function sched_yield return int;
   --  DCE_THREADS has a nonstandard sched_yield

   -----------------------------
   --  P1003.1c - Section 16  --
   -----------------------------

   function pthread_attr_init
     (attributes : access pthread_attr_t)
     return int;

   function pthread_attr_destroy
     (attributes : access pthread_attr_t)
     return int;

   function pthread_attr_setdetachstate
     (attr        : access pthread_attr_t;
      detachstate : int)
     return int;
   --  DCE_THREADS has a nonstandard pthread_attr_setdetachstate

   function pthread_attr_getdetachstate
     (attr        : access pthread_attr_t;
      detachstate : access int)
     return int;

   function pthread_attr_getstacksize
     (attr      : access pthread_attr_t;
      stacksize : access size_t)
     return int;

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t)
     return int;

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address)
     return int;

   function pthread_detach (thread : pthread_t) return int;
   --  DCE_THREADS has a nonstandard pthread_detach

   procedure pthread_exit (status : System.Address);
   pragma Import (C, pthread_exit, "pthread_exit");

   function pthread_self return pthread_t;
   pragma Import (C, pthread_self, "pthread_self");

   function pthread_equal (t1 : pthread_t; t2 : pthread_t)
      return int;
   --  be careful not to use "=" on thread_t!
   pragma Import (C, pthread_equal, "pthread_equal");

   ----------------------------
   --  POSIX.1c  Section 17  --
   ----------------------------

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address)
     return  int;

   function pthread_getspecific (key : pthread_key_t) return System.Address;
   --  DCE_THREADS has a nonstandard pthread_getspecific

   type destructor_pointer is access
      procedure (arg : System.Address);

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer)
     return int;

private

   type array_type_1 is array (integer range 0 .. 3) of unsigned_long;
   type sigset_t is record
      X_X_sigbits  : array_type_1;
   end record;
   pragma Convention (C, sigset_t);

   type pid_t is new long;

   type time_t is new long;

   type timespec is record
      tv_sec       : time_t;
      tv_nsec      : long;
   end record;
   pragma Convention (C, timespec);

   type struct_timeval is record
      tv_sec       : long;
      tv_usec      : long;
   end record;
   pragma Convention (C, struct_timeval);

   type cma_t_address is new System.Address;

   type cma_t_handle is record
      field1 : cma_t_address;
      field2 : short_integer;
      field3 : short_integer;
   end record;
   for cma_t_handle'Size use 64;

   type pthread_attr_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_attr_t);

   type pthread_condattr_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_condattr_t);

   type pthread_mutexattr_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_mutexattr_t);

   type pthread_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_t);

   type pthread_mutex_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_mutex_t);

   type pthread_cond_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_cond_t);

   type pthread_key_t is new int;

end System.OS_Interface;

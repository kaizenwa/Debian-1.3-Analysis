------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.2 $                             --
--                                                                          --
--          Copyright (C) 1991 - 1997 Free Software Foundation, Inc.        --
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

--  This is the DEC Unix 4.0 version of this package.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

--  This version is for POSIX-like operating systems.

with Interfaces.C;
package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-lpthread");
   pragma Linker_Options ("-lrt");

   subtype int            is Interfaces.C.int;
   subtype short          is Interfaces.C.short;
   subtype long           is Interfaces.C.long;
   subtype unsigned       is Interfaces.C.unsigned;
   subtype unsigned_short is Interfaces.C.unsigned_short;
   subtype unsigned_long  is Interfaces.C.unsigned_long;
   subtype unsigned_char  is Interfaces.C.unsigned_char;
   subtype plain_char     is Interfaces.C.plain_char;
   subtype size_t         is Interfaces.C.size_t;
   subtype char_array     is Interfaces.C.char_array;

   -----------
   -- Errno --
   -----------

   function errno return int;
   pragma Import (C, errno, "_Geterrno");

   EAGAIN    : constant := 35;
   EINTR     : constant := 4;
   EINVAL    : constant := 22;
   ENOMEM    : constant := 12;
   ETIMEDOUT : constant := 60;

   -------------
   -- Signals --
   -------------

   NSIG : constant := 49;
   type Signal is new int range 0 .. Interfaces.C."-" (NSIG, 1);
   for Signal'Size use int'Size;

   --  NAMEs not used are commented-out
   --  NAMEs not supported on this system have __NAME for value

   SIGHUP     : constant := 1; --  hangup
   SIGINT     : constant := 2; --  interrupt (rubout)
   SIGQUIT    : constant := 3; --  quit (ASCD FS)
   SIGILL     : constant := 4; --  illegal instruction (not reset)
   SIGTRAP    : constant := 5; --  trace trap (not reset)
   SIGABRT    : constant := 6; --  used by abort, replace SIGIOT in the  future
   SIGIOT     : constant := 6; --  abort (terminate) process
   SIGLOST    : constant := 6; --  old BSD signal ??
   SIGEMT     : constant := 7; --  EMT instruction
   SIGFPE     : constant := 8; --  floating point exception
   SIGKILL    : constant := 9; --  kill (cannot be caught or ignored)
   SIGBUS     : constant := 10; --  bus error
   SIGSEGV    : constant := 11; --  segmentation violation
   SIGSYS     : constant := 12; --  bad argument to system call
   SIGPIPE    : constant := 13; --  write on a pipe with no one to read it
   SIGALRM    : constant := 14; --  alarm clock
   SIGTERM    : constant := 15; --  software termination signal from kill
   SIGURG     : constant := 16; --  urgent condition on IO channel
   SIGIOINT   : constant := 16; --  printer to backend error signal
   SIGSTOP    : constant := 17; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 18; --  user stop requested from tty
   SIGCONT    : constant := 19; --  stopped process has been continued
   SIGCHLD    : constant := 20; --  child status change
   SIGTTIN    : constant := 21; --  background tty read attempted
   SIGTTOU    : constant := 22; --  background tty write attempted
   SIGPOLL    : constant := 23; --  I/O possible, or completed
   SIGIO      : constant := 23; --  STREAMS version of SIGPOLL
   SIGAIO     : constant := 23; --  base lan i/o
   SIGPTY     : constant := 23; --  pty i/o
   SIGXCPU    : constant := 24; --  CPU time limit exceeded
   SIGXFSZ    : constant := 25; --  filesize limit exceeded
   SIGVTALRM  : constant := 26; --  virtual timer expired
   SIGPROF    : constant := 27; --  profiling timer expired
   SIGWINCH   : constant := 28; --  window size change
   SIGINFO    : constant := 29; --  information request
   SIGPWR     : constant := 29; --  Power Fail/Restart -- SVID3/SVR4
   SIGUSR1    : constant := 30; --  user defined signal 1
   SIGUSR2    : constant := 31; --  user defined signal 2
   SIGRESV    : constant := 32; --  reserved by Digital for future use

   type sigset_t is private;

   function sigaddset
     (set : access sigset_t;
      sig : Signal)
     return int;
   pragma Import (C, sigaddset);

   function sigdelset
     (set : access sigset_t;
      sig : Signal)
     return int;
   pragma Import (C, sigdelset);

   function sigfillset
     (set : access sigset_t)
     return int;
   pragma Import (C, sigfillset);

   function sigismember
     (set : access sigset_t;
      sig : Signal)
     return int;
   pragma Import (C, sigismember);

   function sigemptyset
     (set : access sigset_t)
     return int;
   pragma Import (C, sigemptyset);

   type union_type_3 is new String (1 .. 116);
   type siginfo_t is record
      si_signo     : int;
      si_errno     : int;
      si_code      : int;
      X_data       : union_type_3;
   end record;
   for siginfo_t'Size use 8 * 128;
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

   type long_array is array (Natural range <>) of long;

   type mcontext_t is record
      sc_onstack          : long;
      sc_mask             : long;
      sc_pc               : long;
      sc_ps               : long;
      sc_regs             : long_array (0 .. 31);
      sc_ownedfp          : long;
      sc_fpregs           : long_array (0 .. 31);
      sc_fpcr             : unsigned_long;
      sc_fp_control       : unsigned_long;
      sc_reserved1        : long;
      sc_kreserved1       : int;
      sc_kreserved2       : int;
      sc_ssize            : size_t;
      sc_sbase            : System.Address;
      sc_trap_arg_a0      : unsigned_long;
      sc_trap_arg_a1      : unsigned_long;
      sc_trap_arg_a2      : unsigned_long;
      sc_fp_trap_pc       : unsigned_long;
      sc_fp_trigger_sum   : unsigned_long;
      sc_fp_trigger_inst  : unsigned_long;
   end record;
   pragma Convention (C, mcontext_t);

   type stack_t is record
      ss_sp        : System.Address;
      ss_size      : int;
      ss_flags     : size_t;
   end record;
   pragma Convention (C, stack_t);

   type ucontext_t is record
      uc_flags     : unsigned_long;
      uc_link      : System.Address;
      uc_sigmask   : sigset_t;
      uc_stack     : stack_t;
      uc_mcontext  : mcontext_t;
      uc_filler    : long_array (0 .. 4);
   end record;
   pragma Convention (C, ucontext_t);

   type Signal_Handler is access procedure
      (signo   : Signal;
       info    : access siginfo_t;
       context : access ucontext_t);

   type struct_sigaction is record
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_flags   : int;
      sa_signo   : int;
   end record;
   pragma Convention (C, struct_sigaction);

   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;

   SIG_DFL : constant := 0;
   SIG_IGN : constant := 1;

   function sigaction
     (sig  : Signal;
      act  : access struct_sigaction;
      oact : access struct_sigaction)
     return int;
   pragma Import (C, sigaction);

   ----------
   -- Time --
   ----------

   type timespec is private;

   function nanosleep (rqtp, rmtp : access timespec)  return int;
   pragma Import (C, nanosleep);

   type clockid_t is private;

   CLOCK_REALTIME : constant clockid_t;

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec)
      return int;
   pragma Import (C, clock_gettime);

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   type struct_timezone is record
      tz_minuteswest  : int;
      tz_dsttime      : int;
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
   pragma Import (C, gettimeofday);

   --  add a hook to locate the Epoch, for use with Calendar????

   -------------------------
   -- Priority Scheduling --
   -------------------------

   SCHED_FIFO  : constant := 1;
   SCHED_RR    : constant := 2;
   SCHED_OTHER : constant := 3;

   -------------
   -- Process --
   -------------

   type pid_t is private;

   function kill
     (pid : pid_t;
      sig : Signal)
   return int;
   pragma Import (C, kill);

   function getpid return pid_t;
   pragma Import (C, getpid);

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

   PTHREAD_CREATE_DETACHED : constant := 1;
   PTHREAD_CREATE_JOINABLE : constant := 0;

   ---------------------------
   --  POSIX.1c  Section 3  --
   ---------------------------

   function sigwait
     (set : access sigset_t;
      sig : access Signal)
     return int;
   pragma Import (C, sigwait, "__sigwaitd10");

   function pthread_kill
     (thread : pthread_t;
      sig    : Signal)
     return   int;
   pragma Import (C, pthread_kill);

   function pthread_sigmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t)
     return int;
   pragma Import (C, pthread_sigmask);

   ----------------------------
   --  POSIX.1c  Section 11  --
   ----------------------------

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t)
     return int;
   pragma Import (C, pthread_mutexattr_init);

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t)
     return int;
   pragma Import (C, pthread_mutexattr_destroy);

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t)
     return int;
   pragma Import (C, pthread_mutex_init,
                     "__pthread_mutex_init");

   function pthread_mutex_destroy
     (mutex : access pthread_mutex_t)
     return  int;
   pragma Import (C, pthread_mutex_destroy,
                     "__pthread_mutex_destroy");

   function pthread_mutex_lock
     (mutex : access pthread_mutex_t)
     return int;
   pragma Import (C, pthread_mutex_lock,
                     "__pthread_mutex_lock");

   function pthread_mutex_unlock
     (mutex : access pthread_mutex_t)
     return int;
   pragma Import (C, pthread_mutex_unlock,
                     "__pthread_mutex_unlock");

   function pthread_condattr_init
     (attr : access pthread_condattr_t)
     return int;
   pragma Import (C, pthread_condattr_init);

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t)
     return int;
   pragma Import (C, pthread_condattr_destroy);

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t)
     return int;
   pragma Import (C, pthread_cond_init,
                     "__pthread_cond_init");

   function pthread_cond_destroy
     (cond : access pthread_cond_t)
     return int;
   pragma Import (C, pthread_cond_destroy,
                     "__pthread_cond_destroy");

   function pthread_cond_signal
     (cond : access pthread_cond_t)
     return int;
   pragma Import (C, pthread_cond_signal,
                     "__pthread_cond_signal");

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t)
     return  int;
   pragma Import (C, pthread_cond_wait,
                     "__pthread_cond_wait");

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec)
     return    int;
   pragma Import (C, pthread_cond_timedwait,
                     "__pthread_cond_timedwait");

   ----------------------------
   --  POSIX.1c  Section 13  --
   ----------------------------

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int)
     return int;
   pragma Import (C, pthread_mutexattr_setprotocol);

   function pthread_mutexattr_getprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : access int)
     return int;
   pragma Import (C, pthread_mutexattr_getprotocol);

   function pthread_mutexattr_setprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : int)
     return int;
   pragma Import (C, pthread_mutexattr_setprioceiling);

   function pthread_mutexattr_getprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : access int)
     return int;
   pragma Import (C, pthread_mutexattr_getprioceiling);

   type struct_sched_param is record
      sched_priority : int;  --  scheduling priority
   end record;

   function pthread_getschedparam
     (thread : pthread_t;
      policy : access int;
      param  : access struct_sched_param)
     return int;
   pragma Import (C, pthread_getschedparam);

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param)
     return int;
   pragma Import (C, pthread_setschedparam);

   function pthread_attr_setscope
     (attr            : access pthread_attr_t;
      contentionscope : int)
     return int;
   pragma Import (C, pthread_attr_setscope);

   function pthread_attr_getscope
     (attr            : access pthread_attr_t;
      contentionscope : access int)
     return int;
   pragma Import (C, pthread_attr_getscope);

   function pthread_attr_setinheritsched
     (attr            : access pthread_attr_t;
      inheritsched : int)
     return int;
   pragma Import (C, pthread_attr_setinheritsched,
                     "__pthread_attr_setinheritsched");

   function pthread_attr_getinheritsched
     (attr         : access pthread_attr_t;
      inheritsched : access int)
     return int;
   pragma Import (C, pthread_attr_getinheritsched,
                     "__pthread_attr_getinheritsched");

   function pthread_attr_setschedpolicy
     (attr   : access pthread_attr_t;
      policy : int)
     return int;
   pragma Import (C, pthread_attr_setschedpolicy);

   function pthread_attr_getschedpolicy
     (attr   : access pthread_attr_t;
      policy : access int)
     return int;
   pragma Import (C, pthread_attr_getschedpolicy);

   function pthread_attr_setschedparam
     (attr        : access pthread_attr_t;
      sched_param : int)
     return int;
   pragma Import (C, pthread_attr_setschedparam);

   function pthread_attr_getschedparam
     (attr        : access pthread_attr_t;
      sched_param : access int)
     return int;
   pragma Import (C, pthread_attr_getschedparam);

   function sched_yield return int;
   pragma Import (C, sched_yield);

   -----------------------------
   --  P1003.1c - Section 16  --
   -----------------------------

   function pthread_attr_init
     (attributes : access pthread_attr_t)
     return int;
   pragma Import (C, pthread_attr_init);

   function pthread_attr_destroy
     (attributes : access pthread_attr_t)
     return int;
   pragma Import (C, pthread_attr_destroy);

   function pthread_attr_setdetachstate
     (attr        : access pthread_attr_t;
      detachstate : int)
     return int;
   pragma Import (C, pthread_attr_setdetachstate);

   function pthread_attr_getdetachstate
     (attr        : access pthread_attr_t;
      detachstate : access int)
     return int;
   pragma Import (C, pthread_attr_getdetachstate);

   function pthread_attr_getstacksize
     (attr      : access pthread_attr_t;
      stacksize : access size_t)
     return int;
   pragma Import (C, pthread_attr_getstacksize,
                     "__pthread_attr_getstacksize");

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t)
     return int;
   pragma Import (C, pthread_attr_setstacksize,
                     "__pthread_attr_setstacksize");

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address)
     return int;
   pragma Import (C, pthread_create,
                     "__pthread_create");

   function pthread_detach (thread : pthread_t) return int;
   pragma Import (C, pthread_detach,
                     "__pthread_detach");

   procedure pthread_exit (status : System.Address);
   pragma Import (C, pthread_exit,
                     "__pthread_exit");

   function pthread_self return pthread_t;
   pragma Import (C, pthread_self,
                     "__pthread_self");

   function pthread_equal (t1 : pthread_t; t2 : pthread_t) return int;
   pragma Import (C, pthread_equal,
                     "__pthread_equal");

   ----------------------------
   --  POSIX.1c  Section 17  --
   ----------------------------

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address)
     return  int;
   pragma Import (C, pthread_setspecific,
                     "__pthread_setspecific");

   function pthread_getspecific (key : pthread_key_t) return System.Address;
   pragma Import (C, pthread_getspecific,
                     "__pthread_getspecific");

   type destructor_pointer is access procedure (arg : System.Address);

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer)
     return int;
   pragma Import (C, pthread_key_create);

private

   type sigset_t is new unsigned_long;

   type pid_t is new int;

   type time_t is new int;

   type timespec is record
      tv_sec       : time_t;
      tv_nsec      : long;
   end record;
   pragma Convention (C, timespec);

   type clockid_t is new int;
   CLOCK_REALTIME : constant clockid_t := 1;

   type struct_timeval is record
      tv_sec       : time_t;
      tv_usec      : int;
   end record;
   pragma Convention (C, struct_timeval);

   type unsigned_long_array is array (Natural range <>) of unsigned_long;

   type uupthread_t is record
      reserved1     : System.Address;
      reserved2     : System.Address;
      size          : unsigned_short;
      reserved3     : char_array (0 .. 1);
      reserved4     : char_array (0 .. 3);
      sequence      : unsigned_long;
      reserved5     : unsigned_long_array (0 .. 1);
      per_kt_area   : System.Address;
      stack_base    : System.Address;
      stack_reserve : System.Address;
      stack_yellow  : System.Address;
      stack_guard   : System.Address;
      stack_size    : unsigned_long;
      tsd_values    : System.Address;
      tsd_count     : unsigned_long;
      reserved6     : unsigned;
      reserved7     : unsigned;
      thread_flags  : unsigned;
   end record;
   for uupthread_t'Size use 8 * 128;
   pragma Convention (C, uupthread_t);

   type pthread_t is access all uupthread_t;

   type pthread_cond_t is record
      state    : unsigned;
      valid    : unsigned;
      name     : System.Address;
      arg      : unsigned;
      sequence : unsigned_long;
      block    : System.Address;
   end record;
   for pthread_cond_t'Size use 8 * 40;
   pragma Convention (C, pthread_cond_t);

   type pthread_attr_t is record
      valid    : long;
      name     : System.Address;
      arg      : unsigned_long;
      reserved : unsigned_long_array (0 .. 18);
   end record;
   for pthread_attr_t'Size use 8 * 176;
   pragma Convention (C, pthread_attr_t);

   type pthread_mutex_t is record
      lock     : unsigned;
      valid    : unsigned;
      name     : System.Address;
      arg      : unsigned;
      depth    : unsigned;
      sequence : unsigned_long;
      owner    : unsigned_long;
      block    : System.Address;
   end record;
   for pthread_mutex_t'Size use 8 * 48;
   pragma Convention (C, pthread_mutex_t);

   type pthread_mutexattr_t is record
      valid    : long;
      reserved : unsigned_long_array (0 .. 14);
   end record;
   for pthread_mutexattr_t'Size use 8 * 128;
   pragma Convention (C, pthread_mutexattr_t);

   type pthread_condattr_t is record
      valid    : long;
      reserved : unsigned_long_array (0 .. 12);
   end record;
   for pthread_condattr_t'Size use 8 * 112;
   pragma Convention (C, pthread_condattr_t);

   type pthread_key_t is new unsigned;
end System.OS_Interface;

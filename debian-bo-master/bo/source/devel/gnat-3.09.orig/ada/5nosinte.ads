------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                           (No Tasking version)                           --
--                                                                          --
--                             $Revision: 1.1 $                             --
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

package System.OS_Interface is
   pragma Preelaborate;

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
   ETIME    : constant := 62;
   ETIMEDOUT    : constant := 145;
   --  EXDEV    : constant := 18;

   -------------
   -- Signals --
   -------------

   NSIG : constant := 44;
   type Signal is new int range 0 .. Interfaces.C."-" (NSIG, 1);

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
   SIGWAITING : constant := 32; --  process's lwps blocked (Solaris)
   SIGLWP     : constant := 33; --  used by thread library (Solaris)
   SIGFREEZE  : constant := 34; --  used by CPR (Solaris)
   SIGTHAW    : constant := 35; --  used by CPR (Solaris)
   SIG32      : constant := 0; --  reserved for kernel (IRIX)
   SIGUNUSED  : constant := 0; --  unused signal (Linux)
   SIGSTKFLT  : constant := 0; --  coprocessor stack fault (Linux)
   SIGLOST    : constant := 0;   --  SIGIO alias (Linux)

   type sigset_t is private;

   function sigaddset
     (set : access sigset_t;
      sig : Signal)
     return int;

   function sigdelset
     (set : access sigset_t;
      sig : Signal)
     return int;

   function sigfillset
     (set : access sigset_t)
     return int;

   function sigismember
     (set : access sigset_t;
      sig : Signal)
     return int;

   function sigemptyset
     (set : access sigset_t)
     return int;

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

   EMT_TAGOVF  : constant := 1; --  tag overflow
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

   type gregset_t is array (integer range 0 .. 18) of greg_t;

   REG_O0 : constant := 11;
   --  index of saved register O0 in ucontext.uc_mcontext.gregs array

   type union_type_2 is new String (1 .. 128);
   type record_type_1 is record
      fpu_fr       : union_type_2;
      fpu_q        : System.Address;
      fpu_fsr      : unsigned;
      fpu_qcnt     : unsigned_char;
      fpu_q_entrysize  : unsigned_char;
      fpu_en       : unsigned_char;
   end record;
   pragma Convention (C, record_type_1);
   type array_type_7 is array (integer range 0 .. 20) of long;
   type mcontext_t is record
      gregs        : gregset_t;
      gwins        : System.Address;
      fpregs       : record_type_1;
      filler       : array_type_7;
   end record;
   pragma Convention (C, mcontext_t);

   type record_type_2 is record
      ss_sp        : System.Address;
      ss_size      : int;
      ss_flags     : int;
   end record;
   pragma Convention (C, record_type_2);
   type array_type_8 is array (integer range 0 .. 22) of long;
   type ucontext_t is record
      uc_flags     : unsigned_long;
      uc_link      : System.Address;
      uc_sigmask   : sigset_t;
      uc_stack     : record_type_2;
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
   --  SIG_ERR : constatn := -1;
   --  not used

   function sigaction
     (sig  : Signal;
      act  : access struct_sigaction;
      oact : access struct_sigaction)
     return int;

   ----------
   -- Time --
   ----------

   type timespec is private;

   function nanosleep (rqtp, rmtp : access timespec)  return int;
   type clockid_t is private;

   CLOCK_REALTIME : constant clockid_t;

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec)
      return int;

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


   --  add a hook to locate the Epoch, for use with Calendar????

   -------------------------
   -- Priority Scheduling --
   -------------------------

   MIN_PRIO : constant := 0;
   MAX_PRIO : constant := 0;

   SCHED_FIFO  : constant := 0;
   SCHED_RR    : constant := 0;
   SCHED_OTHER : constant := 0;

   -------------
   -- Process --
   -------------

   type pid_t is private;

   function kill
     (pid : pid_t;
      sig : Signal)
   return int;

   function getpid return pid_t;

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;


   THR_SUSPENDED : constant := 128;
   THR_DETACHED  : constant := 64;
   THR_BOUND     : constant := 1;
   THR_NEW_LWP   : constant := 2;
   THR_DAEMON    : constant := 256;
   USYNC_THREAD  : constant := 0;

   type thread_t is private;

   type mutex_t is limited private;

   type cond_t is limited private;

   type thread_key_t is private;

   function thr_create
     (stack_base    : System.Address;
      stack_size    : size_t;
      start_routine : Thread_Body;
      arg           : System.Address;
      flags         : int;
      new_thread    : access thread_t)
     return int;

   function thr_min_stack return size_t;

   function thr_self return thread_t;

   function mutex_init
     (mutex : access mutex_t;
      mtype : int;
      arg   : System.Address)
     return  int;

   function mutex_destroy (mutex : access mutex_t) return  int;

   function mutex_lock (mutex : access mutex_t) return int;

   function mutex_unlock (mutex : access mutex_t) return int;

   function cond_init
     (cond  : access cond_t;
      ctype : int;
      arg   : int)
     return int;

   function cond_wait
     (cond  : access cond_t;
      mutex : access mutex_t)
     return  int;

   function cond_timedwait
     (cond    : access cond_t;
      mutex   : access mutex_t;
      abstime : access timespec)
     return    int;

   function cond_signal (cond : access cond_t) return int;

   function cond_destroy (cond : access cond_t) return int;

   function thr_setspecific
     (key   : thread_key_t;
      value : System.Address)
     return  int;

   function thr_getspecific
     (key   : thread_key_t;
      value : access System.Address)
     return  int;

   function thr_keycreate
     (key        : access thread_key_t;
      destructor : System.Address)
     return       int;

   function thr_setprio
     (thread   : thread_t;
      priority : int)
     return     int;

   function thr_getprio
     (thread   : thread_t;
      priority : int)
     return int;

   procedure thr_exit (status : System.Address);

   function thr_setconcurrency (new_level : int) return int;

   function thr_getconcurrency return int;

   function sigwait (set : access sigset_t) return int;

   function thr_kill
     (thread : thread_t;
      sig    : Signal)
     return   int;

   function thr_sigsetmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t)
     return int;

   function thr_suspend (target_thread : thread_t) return int;

   function thr_continue (target_thread : thread_t) return int;

private

   type array_type_1 is array (integer range 0 .. 3) of unsigned_long;
   type sigset_t is record
      X_X_sigbits  : array_type_1;
   end record;
   pragma Convention (C, sigset_t);

   --  In Solaris 2.4 the component sa_handler turns out to
   --  be one a union type, and the selector is a macro:
   --  #define sa_handler __funcptr._handler
   --  #define sa_sigaction __funcptr._sigaction

   type pid_t is new long;

   type time_t is new long;

   type timespec is record
      tv_sec       : time_t;
      tv_nsec      : long;
   end record;
   pragma Convention (C, timespec);

   type clockid_t is new int;
   CLOCK_REALTIME : constant clockid_t := 0;


   type struct_timeval is record
      tv_sec       : long;
      tv_usec      : long;
   end record;
   pragma Convention (C, struct_timeval);


   type thread_t is new unsigned;

   type array_type_9 is array (integer range 0 .. 3) of unsigned_char;
   type record_type_3 is record
      flag         : array_type_9;
      Xtype        : unsigned_long;
   end record;
   pragma Convention (C, record_type_3);
   type union_type_4 is new String (1 .. 8);
   type mutex_t is record
      flags        : record_type_3;
      lock         : union_type_4;
      data         : String (1 .. 8);
   end record;
   pragma Convention (C, mutex_t);

   type array_type_10 is array (integer range 0 .. 3) of unsigned_char;
   type record_type_4 is record
      flag         : array_type_10;
      Xtype        : unsigned_long;
   end record;
   pragma Convention (C, record_type_4);
   type cond_t is record
      flags        : record_type_4;
      data         : String (1 .. 8);
   end record;
   pragma Convention (C, cond_t);

   type thread_key_t is new unsigned;

end System.OS_Interface;

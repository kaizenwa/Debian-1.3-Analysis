------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.4 $                            --
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

--  This is a OpenVMS/Alpha version of this package.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

--  This version is for POSIX-like operating systems

with Interfaces.C;
package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-Xlinker");
   pragma Linker_Options ("SYS$LIBRARY:PTHREAD$RTL/SHAREABLE");

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

   EPERM           : constant := 1;   --  Not owner
   ENOENT          : constant := 2;   --  No such file or directory
   ESRCH           : constant := 3;   --  No such process
   EINTR           : constant := 4;   --  Interrupted system call
   EIO             : constant := 5;   --  I/O error
   ENXIO           : constant := 6;   --  No such device or address
   E2BIG           : constant := 7;   --  Arg list too long
   ENOEXEC         : constant := 8;   --  Exec format error
   EBADF           : constant := 9;   --  Bad file number
   ECHILD          : constant := 10;  --  No children
   EAGAIN          : constant := 11;  --  No more processes
   ENOMEM          : constant := 12;  --  Not enough core
   EACCES          : constant := 13;  --  Permission denied
   EFAULT          : constant := 14;  --  Bad address
   ENOTBLK         : constant := 15;  --  Block device required
   EBUSY           : constant := 16;  --  Mount device busy
   EEXIST          : constant := 17;  --  File exists
   EXDEV           : constant := 18;  --  Cross-device link
   ENODEV          : constant := 19;  --  No such device
   ENOTDIR         : constant := 20;  --  Not a directory
   EISDIR          : constant := 21;  --  Is a directory
   EINVAL          : constant := 22;  --  Invalid argument
   ENFILE          : constant := 23;  --  File table overflow
   EMFILE          : constant := 24;  --  Too many open files
   ENOTTY          : constant := 25;  --  Not a typewriter
   ETXTBSY         : constant := 26;  --  Text file busy
   EFBIG           : constant := 27;  --  File too large
   ENOSPC          : constant := 28;  --  No space left on device
   ESPIPE          : constant := 29;  --  Illegal seek
   EROFS           : constant := 30;  --  Read-only file system
   EMLINK          : constant := 31;  --  Too many links
   EPIPE           : constant := 32;  --  Broken pipe
   EDOM            : constant := 33;  --  Math argument
   ERANGE          : constant := 34;  --  Result too large
   EWOULDBLOCK     : constant := 35;  --  I/O stream empty
   EINPROGRESS     : constant := 36;  --  Operation now in progress
   EALREADY        : constant := 37;  --  Operation already in progress
   ENOTSOCK        : constant := 38;  --  Socket operation on non-socket
   EDESTADDRREQ    : constant := 39;  --  Destination address required
   EMSGSIZE        : constant := 40;  --  Message too long
   EPROTOTYPE      : constant := 41;  --  Protocol wrong type for socket
   ENOPROTOOPT     : constant := 42;  --  Protocol not available
   EPROTONOSUPPORT : constant := 43;  --  Protocol not supported
   ESOCKTNOSUPPORT : constant := 44;  --  Socket type not supported
   EOPNOTSUPP      : constant := 45;  --  Operation not supported on socket
   EPFNOSUPPORT    : constant := 46;  --  Protocol family not supported
   EAFNOSUPPORT    : constant := 47;  --  Address family not supported
   EADDRINUSE      : constant := 48;  --  Address already in use
   EADDRNOTAVAIL   : constant := 49;  --  Can't assign requested address
   ENETDOWN        : constant := 50;  --  Network is down
   ENETUNREACH     : constant := 51;  --  Network is unreachable
   ENETRESET       : constant := 52;  --  Network dropped connection on reset
   ECONNABORTED    : constant := 53;  --  Software caused connection abort
   ECONNRESET      : constant := 54;  --  Connection reset by peer
   ENOBUFS         : constant := 55;  --  No buffer space available
   EISCONN         : constant := 56;  --  Socket is already connected
   ENOTCONN        : constant := 57;  --  Socket is not connected
   ESHUTDOWN       : constant := 58;  --  Can't send after socket shutdown
   ETOOMANYREFS    : constant := 59;  --  Too many references: can't splice
   ETIMEDOUT       : constant := 60;  --  Connection timed out
   ECONNREFUSED    : constant := 61;  --  Connection refused
   ELOOP           : constant := 62;  --  Too many levels of symbolic links
   ENAMETOOLONG    : constant := 63;  --  File name too long
   EHOSTDOWN       : constant := 64;  --  Host is down
   EHOSTUNREACH    : constant := 65;  --  No route to host
   ENOTEMPTY       : constant := 66;  --  Directory not empty
   EPROCLIM        : constant := 67;  --  Too many processes
   EUSERS          : constant := 68;  --  Too many users
   EDQUOT          : constant := 69;  --  Disk quota exceeded
   ENOMSG          : constant := 70;  --  No message of desired type
   EIDRM           : constant := 71;  --  Identifier removed
   EALIGN          : constant := 72;  --  Alignment error
   ESTALE          : constant := 73;  --  Stale NFS file handle
   EREMOTE         : constant := 74;  --  Too many levels of remote in path
   ENOLCK          : constant := 75;  --  No locks available
   ENOSYS          : constant := 76;  --  Function not implemented
   EFTYPE          : constant := 77;  --  Inappropriate operation for file type
   ECANCELED       : constant := 78;  --  Operation canceled
   EFAIL           : constant := 79;  --  Cannot start operation
   EINPROG         : constant := 80;  --  Asynchronous operation in progress
   ENOTSUP         : constant := 81;  --  Function not implemented
   EDEADLK         : constant := 82;  --  Resource deadlock avoided
   ENWAIT          : constant := 83;  --  No waiting processes
   EILSEQ          : constant := 84;  --  Illegal byte sequence
   EBADCAT         : constant := 85;  --  Bad message catalogue format [1]
   EBADMSG         : constant := 86;  --  Corrupted message detected

   -------------
   -- Signals --
   -------------

   NSIG : constant := 64;
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
   SIGCHLD    : constant := 20; --  child status change
   SIGWINCH   : constant := 20; --  window size change
   SIGCONT    : constant := 21; --  stopped process has been continued
   SIGSTOP    : constant := 22; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 23; --  user stop requested from tty
   SIGTTIN    : constant := 24; --  background tty read attempted
   SIGTTOU    : constant := 25; --  background tty write attempted
   SIGDEBUG   : constant := 26; --  permanently reserved to Digital

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

   type struct_sigaction is record
      sa_handler   : System.Address;
      sa_mask      : sigset_t;
      sa_flags     : int;
   end record;
   pragma Convention (C, struct_sigaction);

   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 4;

   --  SA_NOCLDSTOP : constant := 8;
   --  not used
   --  SA_SIGINFO : constant := 0;
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
   pragma Import (C, sigaction);

   ----------
   -- Time --
   ----------

   type timespec is private;

   type clockid_t is private;

   CLOCK_TIMEOFDAY : constant clockid_t;

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec)
      return int;
   pragma Import (C, clock_gettime, "getclock");

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
   pragma Import (C, gettimeofday);

   --  add a hook to locate the Epoch, for use with Calendar????

   -------------------------
   -- Priority Scheduling --
   -------------------------

   MIN_PRIO : constant := 16;
   MAX_PRIO : constant := 31;

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

   PTHREAD_CREATE_JOINABLE     : constant := 0;
   PTHREAD_CREATE_DETACHED     : constant := 1;

   PTHREAD_CANCEL_DISABLE      : constant := 0;
   PTHREAD_CANCEL_ENABLE       : constant := 1;

   PTHREAD_CANCEL_DEFERRED     : constant := 0;
   PTHREAD_CANCEL_ASYNCHRONOUS : constant := 1;

   --  Don't use ERRORCHECK mutexes, they don't work when a thread is not
   --  the owner.  AST's, at least, unlock others threads mutexes.  Even
   --  if the error is ignored, they don't work.
   PTHREAD_MUTEX_NORMAL_NP     : constant := 0;
   PTHREAD_MUTEX_RECURSIVE_NP  : constant := 1;
   PTHREAD_MUTEX_ERRORCHECK_NP : constant := 2;

   PTHREAD_INHERIT_SCHED       : constant := 0;
   PTHREAD_EXPLICIT_SCHED      : constant := 1;

   function pthread_cancel (thread : pthread_t) return int;
   pragma Import (C, pthread_cancel);

   procedure pthread_testcancel;
   pragma Import (C, pthread_testcancel);

   function pthread_setcancelstate
     (newstate : int; oldstate : access int) return int;
   pragma Import (C, pthread_setcancelstate);

   function pthread_setcanceltype
     (newtype : int; oldtype : access int) return int;
   pragma Import (C, pthread_setcanceltype);

   ---------------------------
   --  POSIX.1c  Section 3  --
   ---------------------------

   function sigwait
     (set : access sigset_t;
      sig : access Signal)
     return int;
   --  VMS pthreads does not have sigwait

   function pthread_kill
     (thread : pthread_t;
      sig    : Signal)
     return   int;
   --  VMS pthreads does not have pthread_kill

   function pthread_sigmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t)
     return int;
   --  VMS pthreads does not have pthread_sigmask

   function pthread_delay_np (interval : access timespec) return int;
   pragma Import (C, pthread_delay_np);

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

   function pthread_mutexattr_settype_np
     (attr      : access pthread_mutexattr_t;
      mutextype : int)
     return int;
   pragma Import (C, pthread_mutexattr_settype_np);

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t)
     return int;
   pragma Import (C, pthread_mutex_init);

   function pthread_mutex_destroy
     (mutex : access pthread_mutex_t)
     return  int;
   pragma Import (C, pthread_mutex_destroy);

   function pthread_mutex_lock
     (mutex : access pthread_mutex_t)
     return int;
   pragma Import (C, pthread_mutex_lock);

   function pthread_mutex_unlock
     (mutex : access pthread_mutex_t)
     return int;
   pragma Import (C, pthread_mutex_unlock);

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
   pragma Import (C, pthread_cond_init);

   function pthread_cond_destroy
     (cond : access pthread_cond_t)
     return int;
   pragma Import (C, pthread_cond_destroy);

   function pthread_cond_signal
     (cond : access pthread_cond_t)
     return int;
   pragma Import (C, pthread_cond_signal);

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t)
     return  int;
   pragma Import (C, pthread_cond_wait);

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec)
     return    int;
   pragma Import (C, pthread_cond_timedwait);

   ----------------------------
   --  POSIX.1c  Section 13  --
   ----------------------------

--   PTHREAD_PRIO_NONE    : constant := 0;
--   PTHREAD_PRIO_PROTECT : constant := 2;
--   PTHREAD_PRIO_INHERIT : constant := 1;

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

   type struct_sched_param is record
      sched_priority : int;  --  scheduling priority
   end record;
   for struct_sched_param'Size use 8*4;
   pragma Convention (C, struct_sched_param);

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
   pragma Import (C, pthread_attr_setinheritsched);

   function pthread_attr_getinheritsched
     (attr         : access pthread_attr_t;
      inheritsched : access int)
     return int;
   pragma Import (C, pthread_attr_getinheritsched);

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
   pragma Import (C, pthread_attr_getstacksize);

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t)
     return int;
   pragma Import (C, pthread_attr_setstacksize);

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address)
     return int;
   pragma Import (C, pthread_create);

   function pthread_detach (thread : pthread_t) return int;
   pragma Import (C, pthread_detach);

   procedure pthread_exit (status : System.Address);
   pragma Import (C, pthread_exit);

   function pthread_self return pthread_t;
   pragma Import (C, pthread_self);

   function pthread_equal (t1 : pthread_t; t2 : pthread_t) return int;
   pragma Import (C, pthread_equal);

   ----------------------------
   --  POSIX.1c  Section 17  --
   ----------------------------

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address)
     return  int;
   pragma Import (C, pthread_setspecific);

   function pthread_getspecific (key : pthread_key_t) return System.Address;
   pragma Import (C, pthread_getspecific);

   type destructor_pointer is access
      procedure (arg : System.Address);

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer)
     return int;
   pragma Import (C, pthread_key_create);

private

   type sigset_t is new int;

   type pid_t is new int;

   type time_t is new unsigned_long;

   type timespec is record
      tv_sec       : time_t;
      tv_nsec      : long;
   end record;
   pragma Convention (C, timespec);

   type clockid_t is new int;
   CLOCK_TIMEOFDAY : constant clockid_t := 1;

   type struct_timeval is record
      tv_sec       : long;
      tv_usec      : long;
   end record;
   pragma Convention (C, struct_timeval);

   type pthreadLongAddr_p is mod 2 ** Long_Integer'Size;

   type pthreadLongAddr_t is mod 2 ** Long_Integer'Size;
   type pthreadLongAddr_t_ptr is mod 2 ** Long_Integer'Size;

   type pthreadLongString_t is mod 2 ** Long_Integer'Size;

   type pthreadLongUint_t is mod 2 ** Long_Integer'Size;
   type pthreadLongUint_array is array (Natural range <>)
     of pthreadLongUint_t;

   type pthread_t is mod 2 ** Long_Integer'Size;

   type pthread_cond_t is record
      state    : unsigned;
      valid    : unsigned;
      name     : pthreadLongString_t;
      arg      : unsigned;
      sequence : unsigned;
      block    : pthreadLongAddr_t_ptr;
   end record;
   for pthread_cond_t'Size use 8*32;
   pragma Convention (C, pthread_cond_t);

   type pthread_attr_t is record
      valid    : long;
      name     : pthreadLongString_t;
      arg      : pthreadLongUint_t;
      reserved : pthreadLongUint_array (0 .. 18);
   end record;
   for pthread_attr_t'Size use 8*176;
   pragma Convention (C, pthread_attr_t);

   type pthread_mutex_t is record
      lock     : unsigned;
      valid    : unsigned;
      name     : pthreadLongString_t;
      arg      : unsigned;
      sequence : unsigned;
      block    : pthreadLongAddr_p;
      owner    : unsigned;
      depth    : unsigned;
   end record;
   for pthread_mutex_t'Size use 8*40;
   pragma Convention (C, pthread_mutex_t);

   type pthread_mutexattr_t is record
      valid    : long;
      reserved : pthreadLongUint_array (0 .. 14);
   end record;
   for pthread_mutexattr_t'Size use 8*128;
   pragma Convention (C, pthread_mutexattr_t);

   type pthread_condattr_t is record
      valid    : long;
      reserved : pthreadLongUint_array (0 .. 12);
   end record;
   for pthread_condattr_t'Size use 8*112;
   pragma Convention (C, pthread_condattr_t);

   type pthread_key_t is new unsigned;

end System.OS_Interface;

------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.1 $                            --
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

--  This is a SNI (DCE THREAD) version of this package.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  This version is for POSIX-like operating systems
--  The original file "s-osinte.ads_m4" contains conditional
--  macro calls that allow selection of various options.

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
package body System.OS_Interface is

   procedure Perror (S : chars_ptr);
   pragma Import (C, Perror, "perror");

   function clock_gettime (tp : access timespec) return int is
      type utc_t is array (Integer range 0 .. 15) of aliased Character;
      inaccsp : aliased timespec;
      tdf     : aliased long;
      function utc_bintime
        (timesp  : access timespec;
         inaccsp : access timespec;
         tdf     : access long;
         utc     : access utc_t := null)
        return int;
      pragma Import (C, utc_bintime, "utc_bintime");
   begin
      return utc_bintime (tp, inaccsp'Access, tdf'Access);
   end clock_gettime;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return Duration (TS.tv_sec) + Duration (TS.tv_nsec) / 10#1#E9;
   end To_Duration;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (D : Duration) return timespec is
      S : time_t;
      F : Duration;
   begin
      S := time_t (Long_Long_Integer (D));
      F := D - Duration (S);

      --  If F has negative value due to a round-up, adjust for positive F
      --  value.
      if F < 0.0 then S := S - 1; F := F + 1.0; end if;
      return timespec' (tv_sec => S,
        tv_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   function To_Duration (TV : struct_timeval) return Duration is
   begin
      return Duration (TV.tv_sec) + Duration (TV.tv_usec) / 10#1#E6;
   end To_Duration;

   function To_Timeval (D : Duration) return struct_timeval is
      S : long;
      F : Duration;
   begin
      S := long (Long_Long_Integer (D));
      F := D - Duration (S);

      --  If F has negative value due to a round-up, adjust for positive F
      --  value.
      if F < 0.0 then S := S - 1; F := F + 1.0; end if;
      return struct_timeval' (tv_sec => S,
        tv_usec => long (Long_Long_Integer (F * 10#1#E6)));
   end To_Timeval;

   ---------------------------
   --  POSIX.1c  Section 3  --
   ---------------------------

   function sigwait
     (set : access sigset_t;
      sig : access Signal)
     return int
   is
      Result : int;
      function sigwait_base (set : access sigset_t) return int;
      pragma Import (C, sigwait_base, "sigwait");

   begin
      Result := sigwait_base (set);
      if sig /= null then
         if Result = -1 then
            sig.all := 0;
         else
            sig.all := Signal (Result);
            Result := 0;
         end if;
      else
         Result := -1;
      end if;
      return Result;
   end sigwait;

   --  DCE_THREADS does not have pthread_kill. Instead, we just ignore it.
   function pthread_kill
     (thread : pthread_t;
      sig    : Signal)
     return int
   is
   begin
      return 0;
   end pthread_kill;

   --  DCE THREADS does not have pthread_sigmask. Instead, it uses
   --  sigprocmask to do the signal handling when the thread library is
   --  sucked in.
   function pthread_sigmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t)
     return int
   is
      function sigprocmask
        (how  : int;
         set  : access sigset_t;
         oset : access sigset_t)
        return int;
      pragma Import (C, sigprocmask, "sigprocmask");
   begin
      return sigprocmask (how, set, oset);
   end pthread_sigmask;

   ----------------------------
   --  POSIX.1c  Section 11  --
   ----------------------------

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t)
     return int
   is
      Result : int;
      function pthread_mutexattr_create
        (attr : access pthread_mutexattr_t)
        return int;
      pragma Import (C, pthread_mutexattr_create, "pthread_mutexattr_create");
   begin
      Result := pthread_mutexattr_create (attr);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_mutexattr_init;

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t)
     return int
   is
      Result : int;
      function pthread_mutexattr_delete
        (attr : access pthread_mutexattr_t)
        return int;
      pragma Import (C, pthread_mutexattr_delete, "pthread_mutexattr_delete");
   begin
      Result := pthread_mutexattr_delete (attr);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_mutexattr_destroy;

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t)
     return int
   is
      Result : int;
      function pthread_mutex_init_base
        (mutex : access pthread_mutex_t;
         attr  : pthread_mutexattr_t)
        return int;
      pragma Import (C, pthread_mutex_init_base, "pthread_mutex_init");
   begin
      Result := pthread_mutex_init_base (mutex, attr.all);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_mutex_init;

   function pthread_mutex_destroy
     (mutex : access pthread_mutex_t)
     return int
   is
      Result : int;
      function pthread_mutex_destroy_base
        (mutex : access pthread_mutex_t)
        return int;
      pragma Import (C, pthread_mutex_destroy_base, "pthread_mutex_destroy");
   begin
      Result := pthread_mutex_destroy_base (mutex);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_mutex_destroy;

   --  DCE_THREADS has nonstandard pthread_mutex_lock.
   --  It sets errno but the standard Posix requires it to be returned.
   function pthread_mutex_lock
     (mutex : access pthread_mutex_t)
     return int
   is
      Result : int;
      function pthread_mutex_lock_base
        (mutex : access pthread_mutex_t)
        return  int;
      pragma Import (C, pthread_mutex_lock_base, "pthread_mutex_lock");
   begin
      Result := pthread_mutex_lock_base (mutex);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_mutex_lock;

   --  DCE_THREADS has nonstandard pthread_mutex_unlock.
   --  It sets errno but the standard Posix requires it to be returned.
   function pthread_mutex_unlock
     (mutex : access pthread_mutex_t)
     return int
   is
      Result : int;
      function pthread_mutex_unlock_base
        (mutex : access pthread_mutex_t)
        return  int;
      pragma Import (C, pthread_mutex_unlock_base, "pthread_mutex_unlock");
   begin
      Result := pthread_mutex_unlock_base (mutex);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_mutex_unlock;

   function pthread_condattr_init
     (attr : access pthread_condattr_t)
     return int
   is
      Result : int;
      function pthread_condattr_create
        (attr : access pthread_condattr_t)
        return int;
      pragma Import (C, pthread_condattr_create, "pthread_condattr_create");
   begin
      Result := pthread_condattr_create (attr);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_condattr_init;

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t)
     return int
   is
      Result : int;
      function pthread_condattr_delete
        (attr : access pthread_condattr_t)
        return int;
      pragma Import (C, pthread_condattr_delete, "pthread_condattr_delete");
   begin
      Result := pthread_condattr_delete (attr);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_condattr_destroy;

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t)
     return int
   is
      Result : int;
      function pthread_cond_init_base
        (cond : access pthread_cond_t;
         attr : pthread_condattr_t)
        return int;
      pragma Import (C, pthread_cond_init_base, "pthread_cond_init");
   begin
      Result := pthread_cond_init_base (cond, attr.all);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_cond_init;

   function pthread_cond_destroy
     (cond : access pthread_cond_t)
     return int
   is
      Result : int;
      function pthread_cond_destroy_base
        (cond : access pthread_cond_t)
        return int;
      pragma Import (C, pthread_cond_destroy_base, "pthread_cond_destroy");
   begin
      Result := pthread_cond_destroy_base (cond);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_cond_destroy;

   function pthread_cond_signal
     (cond : access pthread_cond_t)
     return int
   is
      Result : int;
      function pthread_cond_signal_base
        (cond : access pthread_cond_t)
        return int;
      pragma Import (C, pthread_cond_signal_base, "pthread_cond_signal");
   begin
      Result := pthread_cond_signal_base (cond);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_cond_signal;

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t)
     return int
   is
      Result : int;
      function pthread_cond_wait_base
        (cond  : access pthread_cond_t;
         mutex : access pthread_mutex_t)
        return int;
      pragma Import (C, pthread_cond_wait_base, "pthread_cond_wait");
   begin
      Result := pthread_cond_wait_base (cond, mutex);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_cond_wait;

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec)
     return int
   is
      Result : int;
      function pthread_cond_timedwait_base
        (cond    : access pthread_cond_t;
         mutex   : access pthread_mutex_t;
         abstime : access timespec)
        return int;
      pragma Import (C, pthread_cond_timedwait_base, "pthread_cond_timedwait");
   begin
      Result := pthread_cond_timedwait_base (cond, mutex, abstime);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_cond_timedwait;

   ----------------------------
   --  POSIX.1c  Section 13  --
   ----------------------------

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int)
     return int
   is
   begin
      return 0;
   end pthread_mutexattr_setprotocol;

   function pthread_mutexattr_getprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : access int)
     return int
   is
   begin
      return 0;
   end pthread_mutexattr_getprotocol;

   function pthread_mutexattr_setprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : int)
     return int
   is
   begin
      return 0;
   end pthread_mutexattr_setprioceiling;

   function pthread_mutexattr_getprioceiling
     (attr        : access pthread_mutexattr_t;
      prioceiling : access int)
     return int
   is
   begin
      return 0;
   end pthread_mutexattr_getprioceiling;

   --  DCE_THREADS does not have pthread_getschedparam
   --  This routine returns a non-negative vaule upon failure
   --  but the error code can not be set comforming the POSIX standard.
   function pthread_getschedparam
     (thread : pthread_t;
      policy : access int;
      param  : access struct_sched_param)
     return int
   is
      Result : int;

      function pthread_getprio
        (thread : pthread_t)
         return int;
      pragma Import (C, pthread_getprio, "pthread_getprio");

      function pthread_getscheduler
        (thread : pthread_t)
         return int;
      pragma Import (C, pthread_getscheduler, "pthread_getscheduler");

   begin
      Result := pthread_getprio (thread);
      if Result = -1 then return errno; end if;
      param.sched_priority := Result;
      Result := pthread_getscheduler (thread);
      if Result = -1 then return errno; end if;
      policy.all := Result;
      return 0;
   end pthread_getschedparam;

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param)
     return int
   is
      Result : int;

      function pthread_setscheduler
        (thread   : pthread_t;
         policy   : int;
         priority : int)
         return int;
      pragma Import (C, pthread_setscheduler, "pthread_setscheduler");
   begin
      Result := pthread_setscheduler (thread, policy, param.sched_priority);
      if Result = -1 then
         Perror (New_String ("pthread_setscheduler"));
         return errno;
      end if;
      return 0;
   end pthread_setschedparam;

   function pthread_attr_setscope
     (attr            : access pthread_attr_t;
      contentionscope : int)
     return int
   is
   begin
      return 0;
   end pthread_attr_setscope;

   function pthread_attr_getscope
     (attr            : access pthread_attr_t;
      contentionscope : access int)
     return int
   is
   begin
      return 0;
   end pthread_attr_getscope;

   function pthread_attr_setinheritsched
     (attr         : access pthread_attr_t;
      inheritsched : int)
     return int
   is
      Result : int;
      function pthread_attr_setinheritsched_base
        (attr         : pthread_attr_t;
         inheritsched : int)
        return int;
      pragma Import (C, pthread_attr_setinheritsched_base,
                     "pthread_attr_setinheritsched");
   begin
      Result := pthread_attr_setinheritsched_base (attr.all, inheritsched);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_attr_setinheritsched;

   function pthread_attr_getinheritsched
     (attr         : access pthread_attr_t;
      inheritsched : access int)
     return int
   is
      Result : int;
      function pthread_attr_getinheritsched_base
        (attr         : pthread_attr_t)
        return int;
      pragma Import (C, pthread_attr_getinheritsched_base,
                     "pthread_attr_getinheritsched");
   begin
      Result := pthread_attr_getinheritsched_base (attr.all);
      if Result = -1 then
         return errno;
      end if;
      inheritsched.all := Result;
      return 0;
   end pthread_attr_getinheritsched;

   function pthread_attr_setschedpolicy
     (attr   : access pthread_attr_t;
      policy : int)
     return int
   is
      Result       : int;
      sched_policy : aliased int := policy;
      function pthread_attr_setsched
        (attr   : access pthread_attr_t;
         policy : access int)
        return int;
      pragma Import (C, pthread_attr_setsched, "pthread_attr_setsched");
   begin
      Result := pthread_attr_setsched (attr, sched_policy'Access);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_attr_setschedpolicy;

   function pthread_attr_getschedpolicy
     (attr   : access pthread_attr_t;
      policy : access int)
     return int
   is
      Result : int;
      function pthread_attr_getsched
        (attr   : pthread_attr_t)
        return int;
      pragma Import (C, pthread_attr_getsched, "pthread_attr_getsched");
   begin
      Result := pthread_attr_getsched (attr.all);
      if Result = -1 then
         return errno;
      end if;
      policy.all := Result;
      return 0;
   end pthread_attr_getschedpolicy;

   function pthread_attr_setschedparam
     (attr        : access pthread_attr_t;
      sched_param : int)
     return int
   is
   begin
      return 0;
   end pthread_attr_setschedparam;

   function pthread_attr_getschedparam
     (attr        : access pthread_attr_t;
      sched_param : access int)
     return int
   is
   begin
      return 0;
   end pthread_attr_getschedparam;

   function sched_yield return int is
      procedure pthread_yield;
      pragma Import (C, pthread_yield, "pthread_yield");
   begin
      pthread_yield;
      return 0;
   end sched_yield;

   -----------------------------
   --  P1003.1c - Section 16  --
   -----------------------------

   function pthread_attr_init
     (attributes : access pthread_attr_t)
     return int
   is
      Result : int;
      function pthread_attr_create
        (attributes : access pthread_attr_t)
        return int;
      pragma Import (C, pthread_attr_create, "pthread_attr_create");
   begin
      Result := pthread_attr_create (attributes);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_attr_init;

   function pthread_attr_destroy
     (attributes : access pthread_attr_t)
     return int
   is
      Result : int;
      function pthread_attr_delete
        (attributes : access pthread_attr_t)
        return int;
      pragma Import (C, pthread_attr_delete, "pthread_attr_delete");
   begin
      Result := pthread_attr_delete (attributes);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_attr_destroy;

   function pthread_attr_setdetachstate
     (attr        : access pthread_attr_t;
      detachstate : int)
     return int is
   begin
      return 0;
   end pthread_attr_setdetachstate;

   function pthread_attr_getdetachstate
     (attr        : access pthread_attr_t;
      detachstate : access int)
     return int
   is
   begin
      return 0;
   end pthread_attr_getdetachstate;

   function pthread_attr_getstacksize
     (attr      : access pthread_attr_t;
      stacksize : access size_t)
     return int
   is
      Result : int;
      function pthread_attr_getstacksize_base
        (attr      : pthread_attr_t)
        return int;
      pragma Import (C, pthread_attr_getstacksize_base,
                     "pthread_attr_getstacksize");
   begin
      Result := pthread_attr_getstacksize_base (attr.all);
      if Result = -1 then
         return errno;
      end if;
      stacksize.all := size_t (Result);
      return 0;
   end pthread_attr_getstacksize;

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t)
     return int
   is
      Result : int;
      function pthread_attr_setstacksize_base
        (attr      : access pthread_attr_t;
         stacksize : size_t)
        return int;
      pragma Import (C, pthread_attr_setstacksize_base,
                     "pthread_attr_setstacksize");
   begin
      Result := pthread_attr_setstacksize_base (attr, stacksize);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_attr_setstacksize;

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address)
     return int
   is
      Result : int;
      function pthread_create_base
        (thread        : access pthread_t;
         attributes    : pthread_attr_t;
         start_routine : Thread_Body;
         arg           : System.Address)
        return int;
      pragma Import (C, pthread_create_base, "pthread_create");
   begin
      Result := pthread_create_base
        (thread, attributes.all, start_routine, arg);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_create;

   --  DCE_THREADS has a nonstandard pthread_detach
   --  The DCE_THREADS version has a parameter of type
   --  "pthread_t *" where the POSIX.1c/D10 version has
   --  a parameter of type just "pthread_t".
   function pthread_detach (thread : pthread_t) return int is

      function pthread_detach_base (thread : access pthread_t) return int;
      pragma Import (C, pthread_detach_base, "pthread_detach");
      Tmp : aliased pthread_t := thread;

   begin
      return pthread_detach_base (Tmp'Unchecked_Access);
   end pthread_detach;

   ----------------------------
   --  POSIX.1c  Section 17  --
   ----------------------------

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address)
     return int
   is
      Result : int;
      function pthread_setspecific_base
        (key   : pthread_key_t;
         value : System.Address)
        return int;
      pragma Import (C, pthread_setspecific_base, "pthread_setspecific");
   begin
      Result := pthread_setspecific_base (key, value);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_setspecific;

   function pthread_getspecific (key : pthread_key_t) return System.Address is
      Result : int;
      function pthread_getspecific_base
        (key   : pthread_key_t;
         value : access System.Address)
        return  int;
      pragma Import (C, pthread_getspecific_base, "pthread_getspecific");
      Tmp : aliased System.Address;
   begin
      Result := pthread_getspecific_base (key, Tmp'Access);
      if Result /= 0 then return System.Null_Address; end if;
      return Tmp;
   end pthread_getspecific;

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer)
     return int
   is
      Result : int;
      function pthread_keycreate
        (key        : access pthread_key_t;
         destructor : destructor_pointer)
        return int;
      pragma Import (C, pthread_keycreate, "pthread_keycreate");
   begin
      Result := pthread_keycreate (key, destructor);
      if Result /= 0 then
         return errno;
      end if;
      return 0;
   end pthread_key_create;

end System.OS_Interface;

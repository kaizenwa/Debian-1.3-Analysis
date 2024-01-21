------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                           (No Tasking version)                           --
--                                                                          --
--                             $Revision: 1.2 $                             --
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

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

with Interfaces.C; use Interfaces.C;
package body System.OS_Interface is

   -------------------
   -- clock_gettime --
   -------------------

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec)
      return int is
   begin
      return 0;
   end clock_gettime;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return 0.0;
   end To_Duration;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (D : Duration) return timespec is
   begin
      return timespec' (0, 0);
   end To_Timespec;

   function To_Duration (TV : struct_timeval) return Duration is
   begin
      return 0.0;
   end To_Duration;

   function To_Timeval (D : Duration) return struct_timeval is
   begin
      return struct_timeval' (0, 0);
   end To_Timeval;

   function errno return int is
   begin
      return 0;
   end errno;

   function sigaddset
     (set : access sigset_t;
      sig : Signal)
     return int is
   begin
      return 0;
   end sigaddset;

   function sigdelset
     (set : access sigset_t;
      sig : Signal)
     return int is
   begin
      return 0;
   end sigdelset;

   function sigfillset
     (set : access sigset_t)
     return int is
   begin
      return 0;
   end sigfillset;

   function sigismember
     (set : access sigset_t;
      sig : Signal)
     return int is
   begin
      return 0;
   end sigismember;

   function sigemptyset
     (set : access sigset_t)
     return int is
   begin
      return 0;
   end sigemptyset;

   function sigaction
     (sig  : Signal;
      act  : access struct_sigaction;
      oact : access struct_sigaction)
     return int is
   begin
      return 0;
   end sigaction;

   function nanosleep (rqtp, rmtp : access timespec)  return int is
   begin
      return 0;
   end nanosleep;

   function gettimeofday
     (tv : access struct_timeval;
      tz : access struct_timezone) return int is
   begin
      return 0;
   end gettimeofday;

   function kill
     (pid : pid_t;
      sig : Signal)
   return int is
   begin
      return 0;
   end kill;

   function getpid return pid_t is
   begin
      return 0;
   end getpid;

   function thr_create
     (stack_base    : System.Address;
      stack_size    : size_t;
      start_routine : Thread_Body;
      arg           : System.Address;
      flags         : int;
      new_thread    : access thread_t)
     return int is
   begin
      return 0;
   end thr_create;

   function thr_min_stack return size_t is
   begin
      return 0;
   end thr_min_stack;

   function thr_self return thread_t is
   begin
      return 0;
   end thr_self;

   function mutex_init
     (mutex : access mutex_t;
      mtype : int;
      arg   : System.Address)
     return  int is
   begin
      return 0;
   end mutex_init;

   function mutex_destroy (mutex : access mutex_t) return  int is
   begin
      return 0;
   end mutex_destroy;

   function mutex_lock (mutex : access mutex_t) return int is
   begin
      return 0;
   end mutex_lock;

   function mutex_unlock (mutex : access mutex_t) return int is
   begin
      return 0;
   end mutex_unlock;

   function cond_init
     (cond  : access cond_t;
      ctype : int;
      arg   : int)
     return int is
   begin
      return 0;
   end cond_init;

   function cond_wait
     (cond  : access cond_t;
      mutex : access mutex_t)
     return  int is
   begin
      return 0;
   end cond_wait;

   function cond_timedwait
     (cond    : access cond_t;
      mutex   : access mutex_t;
      abstime : access timespec)
     return    int is
   begin
      return 0;
   end cond_timedwait;

   function cond_signal (cond : access cond_t) return int is
   begin
      return 0;
   end cond_signal;

   function cond_destroy (cond : access cond_t) return int is
   begin
      return 0;
   end cond_destroy;

   function thr_setspecific
     (key   : thread_key_t;
      value : System.Address)
     return  int is
   begin
      return 0;
   end thr_setspecific;

   function thr_getspecific
     (key   : thread_key_t;
      value : access System.Address)
     return  int is
   begin
      return 0;
   end thr_getspecific;

   function thr_keycreate
     (key        : access thread_key_t;
      destructor : System.Address)
     return       int is
   begin
      return 0;
   end thr_keycreate;

   function thr_setprio
     (thread   : thread_t;
      priority : int)
     return     int is
   begin
      return 0;
   end thr_setprio;

   function thr_getprio
     (thread   : thread_t;
      priority : int)
     return int is
   begin
      return 0;
   end thr_getprio;

   procedure thr_exit (status : System.Address) is
   begin
      null;
   end thr_exit;

   function thr_setconcurrency (new_level : int) return int is
   begin
      return 0;
   end thr_setconcurrency;

   function thr_getconcurrency return int is
   begin
      return 0;
   end thr_getconcurrency;

   function sigwait (set : access sigset_t) return int is
   begin
      return 0;
   end sigwait;

   function thr_kill
     (thread : thread_t;
      sig    : Signal)
     return   int is
   begin
      return 0;
   end thr_kill;

   function thr_sigsetmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t)
     return int is
   begin
      return 0;
   end thr_sigsetmask;

   function thr_suspend (target_thread : thread_t) return int is
   begin
      return 0;
   end thr_suspend;

   function thr_continue (target_thread : thread_t) return int is
   begin
      return 0;
   end thr_continue;

end System.OS_Interface;

------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 I N T E R F A C E S . C . P T H R E A D S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.7 $                             --
--                                                                          --
--          Copyright (C) 1996 FSF Free Software Foundation, Inc.           --
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
--  Generated from pthread.h
--  Date: Thu Feb 15 16:57:21 1996
--
--  Command line definitions:
--      -D__ANSI_C__ -D_LANGUAGE_C -DGENERATING_ADA_BINDING -D__unix -D__sgi
--      -D__mips -D__host_mips -D__EXTENSIONS__ -D__EDG -D__DSO__ -D__STDC__
--      -D_SYSTYPE_SVR4 -D_MODERN_C -D_MIPS_SZPTR=32 -D_MIPS_SZLONG=32
--      -D_MIPS_SZINT=32 -D_MIPS_SIM=_MIPS_SIM_ABI32
--      -D_MIPS_ISA=_MIPS_ISA_MIPS1 -D_MIPS_FPSET=16 -D_MIPSEB
--

with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
package Interfaces.C.Pthreads is

   pragma Elaborate_Body;

--
--  POSIX threads header file
--
--

   package C    renames Interfaces.C;
   package CEXT renames Interfaces.C.Extensions;
   package CSTR renames Interfaces.C.Strings;

--  From sys_time_h:

   type timeval is
      record
         tv_sec  : C.Long; --  seconds
         tv_usec : C.Long; --  and microseconds
      end record;

--  Pthread Types

   subtype pthread_t           is CEXT.Void_Ptr;    --   thread identifier
   subtype pthread_mutex_t     is CEXT.Void_Ptr;    --   mutex identifier
   subtype pthread_cond_t      is CEXT.Void_Ptr;    --   cond identifier
   subtype pthread_attr_t      is CEXT.Void_Ptr;    --   pthread attributes
   subtype pthread_mutexattr_t is CEXT.Void_Ptr;    --   mutex attributes
   subtype pthread_condattr_t  is CEXT.Void_Ptr;    --   mutex attributes
   subtype sem_t               is CEXT.Void_Ptr;    --   semaphore identifier
   subtype pthread_once_t      is C.Int; --   dynamic package initialization
   subtype pthread_key_t       is CEXT.Void_Ptr;    --   per thread key
   subtype resource_t          is C.Long;     --   sproc. resource info.
   type    start_addr          is
                  access function (arg : CEXT.Void_Ptr) return CEXT.Void_Ptr;
   type sproc_start_addr is
      access function (arg : CEXT.Void_Ptr) return C.Int;
   type callout_addr is
      access function (arg : CEXT.Void_Ptr;
                       arg1 : CEXT.Void_Ptr) return CEXT.Void_Ptr;

--  SGI specific types
   subtype sproc_t      is CEXT.Void_Ptr; --   sproc identifier
   subtype sproc_attr_t is CEXT.Void_Ptr; --   sproc attributes

   subtype spcb_p is CEXT.Void_Ptr;
   subtype ptcb_p is CEXT.Void_Ptr;

--  Pthread Error Types
   FUNC_OK  : constant := 0;
   FUNC_ERR : constant := -1;


--  pthread run-time initialization data structure
   type pthread_init is
      record
         conf_initsize       : C.Int; --  shared area size
         max_sproc_count     : C.Int; --  maximum number of sprocs
         sproc_stack_size    : size_t;  --  sproc stack size
         os_default_priority : C.Int; --  default IRIX pri for main process
         os_sched_signal     : C.Int; --  default OS scheduling signal
         guard_pages         : C.Int; --  number of guard pages per stack
         init_sproc_count    : C.Int; --  initial number of sprocs
      end record;




--
--  Pthread Attribute Initialize / Destroy
--
   function  pthread_attr_init (attr : access pthread_attr_t) return C.Int;
   pragma Import (C,
                  pthread_attr_init,
                  "pthread_attr_init",
                  "pthread_attr_init");

   function  pthread_attr_destroy
      (attr : access pthread_attr_t) return C.Int;
   pragma Import (C,
                  pthread_attr_destroy,
                  "pthread_attr_destroy",
                  "pthread_attr_destroy");


--
--  Thread Attributes
--
   function  pthread_attr_setstacksize
      (attr : access pthread_attr_t; stacksize : size_t) return C.Int;
   pragma Import (C,
                  pthread_attr_setstacksize,
                  "pthread_attr_setstacksize",
                  "pthread_attr_setstacksize");

   function  pthread_attr_getstacksize
      (attr : access pthread_attr_t; stacksize : access size_t) return C.Int;
   pragma Import (C,
                  pthread_attr_getstacksize,
                  "pthread_attr_getstacksize",
                  "pthread_attr_getstacksize");

   function  pthread_attr_setdetachstate
      (attr : access pthread_attr_t; detachstate : C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_setdetachstate,
                  "pthread_attr_setdetachstate",
                  "pthread_attr_setdetachstate");

   function pthread_attr_getdetachstate
      (attr        : access pthread_attr_t;
       detachstate : access C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_getdetachstate,
                  "pthread_attr_getdetachstate",
                  "pthread_attr_getdetachstate");

   function  pthread_attr_setname
      (attr : access pthread_attr_t; name : CSTR.Chars_Ptr) return C.Int;
   pragma Import (C,
                  pthread_attr_setname,
                  "pthread_attr_setname",
                  "pthread_attr_setname");

   function  pthread_attr_getname
      (attr : access pthread_attr_t; name : CSTR.Chars_Ptr) return C.Int;
   pragma Import (C,
                  pthread_attr_getname,
                  "pthread_attr_getname",
                  "pthread_attr_getname");


--
--  Thread Scheduling Attributes
--
   function  pthread_attr_setscope
      (attr : access pthread_attr_t; contentionscope : C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_setscope,
                  "pthread_attr_setscope",
                  "pthread_attr_setscope");

   function pthread_attr_getscope
      (attr            : access pthread_attr_t;
       contentionscope : access C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_getscope,
                  "pthread_attr_getscope",
                  "pthread_attr_getscope");

   function  pthread_attr_setinheritsched
      (attr : access pthread_attr_t; inherit : C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_setinheritsched,
                  "pthread_attr_setinheritsched",
                  "pthread_attr_setinheritsched");

   function  pthread_attr_getinheritsched
      (attr : access pthread_attr_t; inherit : access C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_getinheritsched,
                  "pthread_attr_getinheritsched",
                  "pthread_attr_getinheritsched");

   function  pthread_attr_setsched
      (attr : access pthread_attr_t; scheduler : C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_setsched,
                  "pthread_attr_setsched",
                  "pthread_attr_setsched");

   function pthread_attr_getsched
      (attr      : access pthread_attr_t;
       scheduler : access C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_getsched,
                  "pthread_attr_getsched",
                  "pthread_attr_getsched");

   function  pthread_attr_setprio
      (attr : access pthread_attr_t; priority : C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_setprio,
                  "pthread_attr_setprio",
                  "pthread_attr_setprio");

   function  pthread_attr_getprio
      (attr : access pthread_attr_t; priority : access C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_getprio,
                  "pthread_attr_getprio",
                  "pthread_attr_getprio");


--
--  SGI Extensions to Thread Attributes
--

--  Bound to sproc attribute values
   PTHREAD_BOUND     : constant := 1;
   PTHREAD_NOT_BOUND : constant := 0;

   function  pthread_attr_setresources
      (attr : access pthread_attr_t; resources : resource_t) return C.Int;
   pragma Import (C,
                  pthread_attr_setresources,
                  "pthread_attr_setresources",
                  "pthread_attr_setresources");

   function pthread_attr_getresources
      (attr      : access pthread_attr_t;
       resources : access resource_t) return C.Int;
   pragma Import (C,
                  pthread_attr_getresources,
                  "pthread_attr_getresources",
                  "pthread_attr_getresources");

   function  pthread_attr_set_boundtosproc
      (attr : access pthread_attr_t; bound_to_sproc : C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_set_boundtosproc,
                  "pthread_attr_set_boundtosproc",
                  "pthread_attr_set_boundtosproc");

   function pthread_attr_get_boundtosproc
      (attr           : access pthread_attr_t;
       bound_to_sproc : access C.Int) return C.Int;
   pragma Import (C,
                  pthread_attr_get_boundtosproc,
                  "pthread_attr_get_boundtosproc",
                  "pthread_attr_get_boundtosproc");

   function  pthread_attr_set_bsproc
      (attr : access pthread_attr_t; bsproc : spcb_p) return C.Int;
   pragma Import (C,
                  pthread_attr_set_bsproc,
                  "pthread_attr_set_bsproc",
                  "pthread_attr_set_bsproc");

   function  pthread_attr_get_bsproc
      (attr : access pthread_attr_t; bsproc : access spcb_p) return C.Int;
   pragma Import (C,
                  pthread_attr_get_bsproc,
                  "pthread_attr_get_bsproc",
                  "pthread_attr_get_bsproc");

   function pthread_attr_set_tslice
      (attr        : access pthread_attr_t;
       ts_interval : access timeval) return C.Int;
   pragma Import (C,
                  pthread_attr_set_tslice,
                  "pthread_attr_set_tslice",
                  "pthread_attr_set_tslice");

   function pthread_attr_get_tslice
      (attr        : access pthread_attr_t;
       ts_interval : access timeval) return C.Int;
   pragma Import (C,
                  pthread_attr_get_tslice,
                  "pthread_attr_get_tslice",
                  "pthread_attr_get_tslice");



--
--  Thread Creation & Management
--

   function pthread_create
      (thread        : access pthread_t;
       attr          : access pthread_attr_t;
       start_routine : start_addr;
       arg           : CEXT.Void_Ptr) return C.Int;
   pragma Import (C, pthread_create, "pthread_create", "pthread_create");

   function  pthread_join
      (thread : pthread_t; status : access CEXT.Void_Ptr) return C.Int;
   pragma Import (C, pthread_join, "pthread_join", "pthread_join");

   function  pthread_detach (thread : access pthread_t) return C.Int;
   pragma Import (C, pthread_detach, "pthread_detach", "pthread_detach");

   procedure  pthread_exit (status : CEXT.Void_Ptr);
   pragma Import (C, pthread_exit, "pthread_exit", "pthread_exit");

   procedure  pthred_yield (arg : CEXT.Void_Ptr);
   pragma Import (C, pthred_yield, "pthred_yield", "pthred_yield");


   function  pthread_self  return pthread_t;
   pragma Import (C, pthread_self, "pthread_self", "pthread_self");


   function  pthread_kill (thread : pthread_t; sig : C.Int) return C.Int;
   pragma Import (C, pthread_kill, "pthread_kill", "pthread_kill");


--
--  SGI Extensions to POSIX thread operations
--
   function  pthread_setprio
      (thread : pthread_t; priority : C.Int) return C.Int;
   pragma Import (C, pthread_setprio, "pthread_setprio", "pthread_setprio");

   function  pthread_getprio
      (thread : pthread_t; priority : access C.Int) return C.Int;
   pragma Import (C, pthread_getprio, "pthread_getprio", "pthread_getprio");


   function  pthread_suspend (thread : pthread_t) return C.Int;
   pragma Import (C, pthread_suspend, "pthread_suspend", "pthread_suspend");

   function  pthread_resume (thread : pthread_t) return C.Int;
   pragma Import (C, pthread_resume, "pthread_resume", "pthread_resume");


   function  pthread_set_info (index : C.Int; data : C.Int) return C.Int;
   pragma Import (C, pthread_set_info, "pthread_set_info", "pthread_set_info");

   function  pthread_get_info (index : C.Int) return C.Int;
   pragma Import (C, pthread_get_info, "pthread_get_info", "pthread_get_info");

   function  pthread_get_info_offset (index : C.Int) return C.Int;
   pragma Import (C,
                  pthread_get_info_offset,
                  "pthread_get_info_offset",
                  "pthread_get_info_offset");


   function  pthread_get_current_ada_tcb  return CEXT.Void_Ptr;
   pragma Import (C,
                  pthread_get_current_ada_tcb,
                  "pthread_get_current_ada_tcb",
                  "pthread_get_current_ada_tcb");

   function  pthread_get_ada_tcb (thread : pthread_t) return CEXT.Void_Ptr;
   pragma Import (C,
                  pthread_get_ada_tcb,
                  "pthread_get_ada_tcb",
                  "pthread_get_ada_tcb");

   function  pthread_set_ada_tcb
      (thread : pthread_t; data : CEXT.Void_Ptr) return C.Int;
   pragma Import (C,
                  pthread_set_ada_tcb,
                  "pthread_set_ada_tcb",
                  "pthread_set_ada_tcb");


   function  pthread_get_jumpbuf_address  return CEXT.Void_Ptr;
   pragma Import (Ada,
                  pthread_get_jumpbuf_address,
                  "pthread_get_jumpbuf_address",
                  "pthread_get_jumpbuf_address");

   procedure  pthread_set_jumpbuf_address (addr : CEXT.Void_Ptr);
   pragma Import (Ada,
                  pthread_set_jumpbuf_address,
                  "pthread_set_jumpbuf_address",
                  "pthread_set_jumpbuf_address");


   function  pthread_get_exception  return CEXT.Void_Ptr;
   pragma Import (Ada,
                  pthread_get_exception,
                  "pthread_get_exception",
                  "pthread_get_exception");

   procedure  pthread_set_exception (addr : CEXT.Void_Ptr);
   pragma Import (Ada,
                  pthread_set_exception,
                  "pthread_set_exception",
                  "pthread_set_exception");


   function  pthread_get_sec_stack_addr  return CEXT.Void_Ptr;
   pragma Import (Ada,
                  pthread_get_sec_stack_addr,
                  "pthread_get_sec_stack_addr",
                  "pthread_get_sec_stack_addr");

   procedure  pthread_set_sec_stack_addr (addr : CEXT.Void_Ptr);
   pragma Import (Ada,
                  pthread_set_sec_stack_addr,
                  "pthread_set_sec_stack_addr",
                  "pthread_set_sec_stack_addr");


   function  pthread_get_exc_stack_addr  return CEXT.Void_Ptr;
   pragma Import (Ada,
                  pthread_get_exc_stack_addr,
                  "pthread_get_exc_stack_addr",
                  "pthread_get_exc_stack_addr");

   procedure  pthread_set_exc_stack_addr (addr : CEXT.Void_Ptr);
   pragma Import (Ada,
                  pthread_set_exc_stack_addr,
                  "pthread_set_exc_stack_addr",
                  "pthread_set_exc_stack_addr");


--
--  Thread Scheduling Attributes
--
   function  pthread_getschedattr
      (thread : pthread_t; attr : access pthread_attr_t) return C.Int;
   pragma Import (C,
                  pthread_getschedattr,
                  "pthread_getschedattr",
                  "pthread_getschedattr");

   function  pthread_setschedattr
      (thread : pthread_t; attr : access pthread_attr_t) return C.Int;
   pragma Import (C,
                  pthread_setschedattr,
                  "pthread_setschedattr",
                  "pthread_setschedattr");


--
--  Scheduling lock/unlock/callout
--
   function  pthread_lock_scheduler  return C.Int;
   pragma Import (C,
                  pthread_lock_scheduler,
                  "pthread_lock_scheduler",
                  "pthread_lock_scheduler");

   function  pthread_unlock_scheduler  return C.Int;
   pragma Import (C,
                  pthread_unlock_scheduler,
                  "pthread_unlock_scheduler",
                  "pthread_unlock_scheduler");

   function  pthread_setsched_callout
      (new_call : callout_addr; old_call : access callout_addr) return C.Int;
   pragma Import (C,
                  pthread_setsched_callout,
                  "pthread_setsched_callout",
                  "pthread_setsched_callout");



--  Delay thread for specified time

   function  pthread_delay (dtime : access timeval) return C.Int;
   pragma Import (C, pthread_delay, "pthread_delay", "pthread_delay");



--  Mutex Initialization / Destruction

   function  pthread_mutexattr_init
      (attr : access pthread_mutexattr_t) return C.Int;
   pragma Import (C,
                  pthread_mutexattr_init,
                  "pthread_mutexattr_init",
                  "pthread_mutexattr_init");

   function  pthread_mutexattr_destroy
      (attr : access pthread_mutexattr_t) return C.Int;
   pragma Import (C,
                  pthread_mutexattr_destroy,
                  "pthread_mutexattr_destroy",
                  "pthread_mutexattr_destroy");

   function pthread_mutexattr_getpshared
      (attr    : access pthread_mutexattr_t;
       pshared : access C.Int) return C.Int;
   pragma Import (C,
                  pthread_mutexattr_getpshared,
                  "pthread_mutexattr_getpshared",
                  "pthread_mutexattr_getpshared");

   function  pthread_mutexattr_setpshared
      (attr : access pthread_mutexattr_t; pshared : C.Int) return C.Int;
   pragma Import (C,
                  pthread_mutexattr_setpshared,
                  "pthread_mutexattr_setpshared",
                  "pthread_mutexattr_setpshared");

   function  pthread_mutexattr_setspin
      (attr : access pthread_mutexattr_t; spincnt : C.Int) return C.Int;
   pragma Import (C,
                  pthread_mutexattr_setspin,
                  "pthread_mutexattr_setspin",
                  "pthread_mutexattr_setspin");

   function pthread_mutexattr_getspin
      (attr    : access pthread_mutexattr_t;
       spincnt : access C.Int) return C.Int;
   pragma Import (C,
                  pthread_mutexattr_getspin,
                  "pthread_mutexattr_getspin",
                  "pthread_mutexattr_getspin");

   function  pthread_mutexattr_setqueueorder
      (attr : access pthread_mutexattr_t; order : C.Int) return C.Int;
   pragma Import (C,
                  pthread_mutexattr_setqueueorder,
                  "pthread_mutexattr_setqueueorder",
                  "pthread_mutexattr_setqueueorder");

   function pthread_mutexattr_getqueueorder
      (attr  : access pthread_mutexattr_t;
       order : access C.Int) return C.Int;
   pragma Import (C,
                  pthread_mutexattr_getqueueorder,
                  "pthread_mutexattr_getqueueorder",
                  "pthread_mutexattr_getqueueorder");

   function  pthread_mutexattr_setceilingprio
      (attr : access pthread_mutexattr_t; priority : C.Int) return C.Int;
   pragma Import (C,
                  pthread_mutexattr_setceilingprio,
                  "pthread_mutexattr_setceilingprio",
                  "pthread_mutexattr_setceilingprio");

   function pthread_mutexattr_getceilingprio
      (attr     : access pthread_mutexattr_t;
       priority : access C.Int) return C.Int;
   pragma Import (C,
                  pthread_mutexattr_getceilingprio,
                  "pthread_mutexattr_getceilingprio",
                  "pthread_mutexattr_getceilingprio");


--  Mutex Attributes

--  Threads queueing order

   MUTEX_PRIORITY         : constant := 0; --   wait in priority order
   MUTEX_FIFO             : constant := 1; --   first-in-first-out
   MUTEX_PRIORITY_INHERIT : constant := 2; --   priority inhertance mutex
   MUTEX_PRIORITY_CEILING : constant := 3; --   priority ceiling mutex

--  Mutex debugging options

   MUTEX_NO_DEBUG  : constant := 0; --   no debugging on mutex
   MUTEX_DEBUG     : constant := 1; --   debugging is on

--  Mutex spin on lock operations

   MUTEX_NO_SPIN   : constant := 0;  --   no spin, try once only
   MUTEX_SPIN_ONLY : constant := -1; --   spin forever
--  cnt > 0, limited spin
--  Mutex sharing attributes

   MUTEX_SHARED    : constant := 0; --   shared between processes
   MUTEX_NOTSHARED : constant := 1; --   not shared between processes


--  Mutex Operations

   function pthread_mutex_init
      (mutex : access pthread_mutex_t;
       attr  : access pthread_mutexattr_t) return C.Int;
   pragma Import (C,
                  pthread_mutex_init,
                  "pthread_mutex_init",
                  "pthread_mutex_init");

   function  pthread_mutex_destroy
      (mutex : access pthread_mutex_t) return C.Int;
   pragma Import (C,
                  pthread_mutex_destroy,
                  "pthread_mutex_destroy",
                  "pthread_mutex_destroy");

   function  pthread_mutex_lock
      (mutex : access pthread_mutex_t) return C.Int;
   pragma Import (C,
                  pthread_mutex_lock,
                  "pthread_mutex_lock",
                  "pthread_mutex_lock");

   function  pthread_mutex_trylock
      (mutex : access pthread_mutex_t) return C.Int;
   pragma Import (C,
                  pthread_mutex_trylock,
                  "pthread_mutex_trylock",
                  "pthread_mutex_trylock");

   function  pthread_mutex_unlock
      (mutex : access pthread_mutex_t) return C.Int;
   pragma Import (C,
                  pthread_mutex_unlock,
                  "pthread_mutex_unlock",
                  "pthread_mutex_unlock");



--  Condition Initialization / Destruction

   function  pthread_condattr_init
      (attr : access pthread_condattr_t) return C.Int;
   pragma Import (C,
                  pthread_condattr_init,
                  "pthread_condattr_init",
                  "pthread_condattr_init");

   function  pthread_condattr_destroy
      (attr : access pthread_condattr_t) return C.Int;
   pragma Import (C,
                  pthread_condattr_destroy,
                  "pthread_condattr_destroy",
                  "pthread_condattr_destroy");

   function pthread_condattr_getpshared
      (attr    : access pthread_condattr_t;
       pshared : access C.Int) return C.Int;
   pragma Import (C,
                  pthread_condattr_getpshared,
                  "pthread_condattr_getpshared",
                  "pthread_condattr_getpshared");

   function  pthread_condattr_setpshared
      (attr : access pthread_condattr_t; pshared : C.Int) return C.Int;
   pragma Import (C,
                  pthread_condattr_setpshared,
                  "pthread_condattr_setpshared",
                  "pthread_condattr_setpshared");


--  Condition Attributes

   COND_PRIORITY  : constant := 0; --   wait in priority order
   COND_FIFO      : constant := 1; --   first-in-first-out

--  Condition debugging options

   COND_NO_DEBUG  : constant := 0; --   no debugging on mutex
   COND_DEBUG     : constant := 1; --   debugging is on

--  Condition sharing attributes

   COND_SHARED    : constant := 0; --   shared between processes
   COND_NOTSHARED : constant := 1; --   not shared between processes


--  Condition Operations

   function pthread_cond_init
      (cond : access pthread_cond_t;
       attr : access pthread_condattr_t) return C.Int;
   pragma Import (C,
                  pthread_cond_init,
                  "pthread_cond_init",
                  "pthread_cond_init");

   function  pthread_cond_destroy
      (cond : access pthread_cond_t) return C.Int;
   pragma Import (C,
                  pthread_cond_destroy,
                  "pthread_cond_destroy",
                  "pthread_cond_destroy");

   function  pthread_cond_signal (cond : access pthread_cond_t) return C.Int;
   pragma Import (C,
                  pthread_cond_signal,
                  "pthread_cond_signal",
                  "pthread_cond_signal");

   function  pthread_cond_broadcast
      (cond : access pthread_cond_t) return C.Int;
   pragma Import (C,
                  pthread_cond_broadcast,
                  "pthread_cond_broadcast",
                  "pthread_cond_broadcast");

   function pthread_cond_wait
      (cond  : access pthread_cond_t;
       mutex : access pthread_mutex_t) return C.Int;
   pragma Import (C,
                  pthread_cond_wait,
                  "pthread_cond_wait",
                  "pthread_cond_wait");

   function pthread_cond_timedwait
      (cond    : access pthread_cond_t;
       mutex   : access pthread_mutex_t;
       abstime : access timeval) return C.Int;
   pragma Import (C,
                  pthread_cond_timedwait,
                  "pthread_cond_timedwait",
                  "pthread_cond_timedwait");

   function pthread_cond_reltimedwait
      (cond    : access pthread_cond_t;
       mutex   : access pthread_mutex_t;
       reltime : access timeval) return C.Int;
   pragma Import (C,
                  pthread_cond_reltimedwait,
                  "pthread_cond_reltimedwait",
                  "pthread_cond_reltimedwait");

   function pthread_cond_signal_unlock
      (cond  : access pthread_cond_t;
       mutex : access pthread_mutex_t) return C.Int;
   pragma Import (C,
                  pthread_cond_signal_unlock,
                  "pthread_cond_signal_unlock",
                  "pthread_cond_signal_unlock");

   function pthread_cond_wait_unlock
      (cond  : access pthread_cond_t;
       mutex : access pthread_mutex_t) return C.Int;
   pragma Import (C,
                  pthread_cond_wait_unlock,
                  "pthread_cond_wait_unlock",
                  "pthread_cond_wait_unlock");



   PTHREAD_ONCE_INIT : constant := 0; --   set to one after init

--  Thread-Specific Data

   type foo_h_proc_1 is
      access procedure (value : CEXT.Void_Ptr);
   function  pthread_key_create
      (key : access pthread_key_t; destructor : foo_h_proc_1) return C.Int;
   pragma Import (C,
                  pthread_key_create,
                  "pthread_key_create",
                  "pthread_key_create");

   function  pthread_setspecific
      (key : pthread_key_t; value : CEXT.Void_Ptr) return C.Int;
   pragma Import (C,
                  pthread_setspecific,
                  "pthread_setspecific",
                  "pthread_setspecific");

   function  pthread_getspecific
      (key : pthread_key_t; value : access CEXT.Void_Ptr) return C.Int;
   pragma Import (C,
                  pthread_getspecific,
                  "pthread_getspecific",
                  "pthread_getspecific");

   type foo_h_proc_2 is
      access procedure;
   function pthread_once
      (once_control : access pthread_once_t;
       init_routine : foo_h_proc_2) return C.Int;
   pragma Import (C, pthread_once, "pthread_once", "pthread_once");



   function  pthread_exec_begin (init : access pthread_init) return C.Int;
   pragma Import (C,
                  pthread_exec_begin,
                  "pthread_exec_begin",
                  "pthread_exec_begin");


   function sproc_create
      (sproc_id      : access sproc_t;
       attr          : access sproc_attr_t;
       start_routine : sproc_start_addr;
       arg           : CEXT.Void_Ptr) return C.Int;
   pragma Import (C, sproc_create, "sproc_create", "sproc_create");

   function  sproc_self  return sproc_t;
   pragma Import (C, sproc_self, "sproc_self", "sproc_self");


--  if equal fast TRUE is returned - common case
--  if not equal thread resource must NOT be null in order to compare bits


--
--  Sproc attribute initialize / destroy
--
   function  sproc_attr_init (attr : access sproc_attr_t) return C.Int;
   pragma Import (C, sproc_attr_init, "sproc_attr_init", "sproc_attr_init");

   function  sproc_attr_destroy (attr : access sproc_attr_t) return C.Int;
   pragma Import (C,
                  sproc_attr_destroy,
                  "sproc_attr_destroy",
                  "sproc_attr_destroy");


   function  sproc_attr_setresources
      (attr : access sproc_attr_t; resources : resource_t) return C.Int;
   pragma Import (C,
                  sproc_attr_setresources,
                  "sproc_attr_setresources",
                  "sproc_attr_setresources");

   function sproc_attr_getresources
      (attr      : access sproc_attr_t;
       resources : access resource_t) return C.Int;
   pragma Import (C,
                  sproc_attr_getresources,
                  "sproc_attr_getresources",
                  "sproc_attr_getresources");


   function  sproc_attr_setcpu
      (attr : access sproc_attr_t; cpu_num : C.Int) return C.Int;
   pragma Import (C,
                  sproc_attr_setcpu,
                  "sproc_attr_setcpu",
                  "sproc_attr_setcpu");

   function  sproc_attr_getcpu
      (attr : access sproc_attr_t; cpu_num : access C.Int) return C.Int;
   pragma Import (C,
                  sproc_attr_getcpu,
                  "sproc_attr_getcpu",
                  "sproc_attr_getcpu");


   function  sproc_attr_setresident
      (attr : access sproc_attr_t; resident : C.Int) return C.Int;
   pragma Import (C,
                  sproc_attr_setresident,
                  "sproc_attr_setresident",
                  "sproc_attr_setresident");

   function  sproc_attr_getresident
      (attr : access sproc_attr_t; resident : access C.Int) return C.Int;
   pragma Import (C,
                  sproc_attr_getresident,
                  "sproc_attr_getresident",
                  "sproc_attr_getresident");


   function  sproc_attr_setname
      (attr : access sproc_attr_t; name : CSTR.Chars_Ptr) return C.Int;
   pragma Import (C,
                  sproc_attr_setname,
                  "sproc_attr_setname",
                  "sproc_attr_setname");

   function  sproc_attr_getname
      (attr : access sproc_attr_t; name : CSTR.Chars_Ptr) return C.Int;
   pragma Import (C,
                  sproc_attr_getname,
                  "sproc_attr_getname",
                  "sproc_attr_getname");


   function  sproc_attr_setstacksize
      (attr : access sproc_attr_t; stacksize : size_t) return C.Int;
   pragma Import (C,
                  sproc_attr_setstacksize,
                  "sproc_attr_setstacksize",
                  "sproc_attr_setstacksize");

   function  sproc_attr_getstacksize
      (attr : access sproc_attr_t; stacksize : access size_t) return C.Int;
   pragma Import (C,
                  sproc_attr_getstacksize,
                  "sproc_attr_getstacksize",
                  "sproc_attr_getstacksize");


   function  sproc_attr_setprio
      (attr : access sproc_attr_t; priority : C.Int) return C.Int;
   pragma Import (C,
                  sproc_attr_setprio,
                  "sproc_attr_setprio",
                  "sproc_attr_setprio");

   function  sproc_attr_getprio
      (attr : access sproc_attr_t; priority : access C.Int) return C.Int;
   pragma Import (C,
                  sproc_attr_getprio,
                  "sproc_attr_getprio",
                  "sproc_attr_getprio");


   function  sproc_attr_setbthread
      (attr : access sproc_attr_t; bthread : ptcb_p) return C.Int;
   pragma Import (C,
                  sproc_attr_setbthread,
                  "sproc_attr_setbthread",
                  "sproc_attr_setbthread");

   function  sproc_attr_getbthread
      (attr : access sproc_attr_t; bthread : access ptcb_p) return C.Int;
   pragma Import (C,
                  sproc_attr_getbthread,
                  "sproc_attr_getbthread",
                  "sproc_attr_getbthread");


   SPROC_NO_RESOURCES : constant := 0;
   SPROC_ANY_CPU      : constant := -1;
   SPROC_MY_PRIORITY  : constant := -1;
   SPROC_SWAPPED      : constant := 0;
   SPROC_RESIDENT     : constant := 1;


--  Semaphore Operations

   function sem_init
      (sem     : access sem_t;
       oflag   : C.Int;
       name    : CSTR.Chars_Ptr;
       pshared : C.Int;
       value   : C.Int) return C.Int;
   pragma Import (C, sem_init, "ath_sem_init", "ath_sem_init");

   function  sem_destroy (sem : access sem_t) return C.Int;
   pragma Import (C, sem_destroy, "ath_sem_destroy", "ath_sem_destroy");

   function  sem_unlink (name : CSTR.Chars_Ptr) return C.Int;
   pragma Import (C, sem_unlink, "ath_sem_unlink", "ath_sem_unlink");

   function  sem_lock (sem : access sem_t) return C.Int;
   pragma Import (C, sem_lock, "ath_sem_lock", "ath_sem_lock");

   function  sem_trylock (sem : access sem_t) return C.Int;
   pragma Import (C, sem_trylock, "ath_sem_trylock", "ath_sem_trylock");

   function  sem_unlock (sem : access sem_t) return C.Int;
   pragma Import (C, sem_unlock, "ath_sem_unlock", "ath_sem_unlock");

   function  sem_check_in_use
      (sem : access sem_t; status : access C.Int) return C.Int;
   pragma Import (C, sem_check_in_use,
     "ath_sem_check_in_use", "ath_sem_check_in_use");


--  Semaphore Waiting Queue

   SEMA_FIFO       : constant := 0; --   threads blocked in fifo order
   SEMA_PRIORITY   : constant := 1; --   threads blocked in priority order

--  Semaphore Shared Flag

   SEMA_NOT_SHARED : constant := 0; --   not shared between processes
   SEMA_SHARED     : constant := 1; --   semaphore shared


   type    isr_address is access procedure;

   function  intr_attach (sig : C.Int; isr : isr_address) return C.Int;
   pragma Import (C, intr_attach, "intr_attach", "intr_attach");

   function intr_exchange
      (sig  : C.Int;
       isr  : isr_address;
       oisr : access isr_address) return C.Int;
   pragma Import (C, intr_exchange, "intr_exchange", "intr_exchange");


   function intr_current_isr
      (sig  : C.Int;
       oisr : access isr_address)
       return C.Int;

   pragma Import (C, intr_current_isr, "intr_current_isr", "intr_current_isr");

end Interfaces.C.Pthreads;

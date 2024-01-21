------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 I N T E R F A C E S . C . P T H R E A D S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.2 $                             --
--                                                                          --
--          Copyright (C) 1996 FSF Free Software Foundation, Inc.           --
--                                                                          --
-- GNARL is free software; you can redistribute it  and/or modify it  under --
-- terms  of  the  GNU  Library General Public License  as published by the --
-- Free Software  Foundation;  either version 2, or (at  your  option)  any --
-- later  version.  GNARL is distributed  in the hope that  it will be use- --
-- ful, but but WITHOUT ANY WARRANTY;  without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Gen- --
-- eral Library Public License  for more details.  You should have received --
-- a  copy of the GNU Library General Public License along with GNARL;  see --
-- file COPYING.LIB.  If not,  write to the  Free Software Foundation,  675 --
-- Mass Ave, Cambridge, MA 02139, USA.                                      --
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

--  This is a DEC Unix version of this package.

--  This package interfaces with Pthreads. It is not a complete interface;
--  it only includes what is needed to implement the Ada runtime.

with Interfaces.C;
with System;
package Interfaces.C.Pthreads is

   --
   --  POSIX threads header file
   --

   Max_Alignment : constant := Standard'Maximum_Alignment;

   package C    renames Interfaces.C;

   subtype void_ptr is System.Address;
   for void_ptr'Alignment use Max_Alignment;

   --  Pthread Types

   type pthread_t is private;

   type pthread_cond_t is private;

   type pthread_attr_t is private;

   type pthread_mutex_t is private;

   type pthread_mutexattr_t is private;

   type pthread_condattr_t is private;

   type pthread_key_t is private;

   type pthread_addr_t is private;

   type pthread_startroutine_t is
      access function (arg : void_ptr) return void_ptr;

   subtype pthread_once_t      is C.int;    --  dynamic package initialization
   subtype msem_t              is void_ptr; --  semaphore identifier
   subtype resource_t          is C.long;   --  sproc. resource info.

   type void_ptr_void_ptr_Ptr is
      access function (arg : void_ptr) return void_ptr;
   subtype start_addr is void_ptr_void_ptr_Ptr;

   type int_void_ptr_Ptr is
      access function (arg : void_ptr) return C.int;

   subtype sproc_start_addr is int_void_ptr_Ptr;

   type void_ptr_void_ptr_void_ptr_Ptr is
      access function (arg   : void_ptr;
                       arg1  : void_ptr) return void_ptr;
   subtype callout_addr is void_ptr_void_ptr_void_ptr_Ptr;

   --  Pthread Error Types
   FUNC_OK  : constant := 0;
   FUNC_ERR : constant := -1;


   --
   --  Pthread Attribute Initialize / Destroy
   --
   function pthread_attr_init (attr : access pthread_attr_t) return C.int;
   pragma Import (C,
                  pthread_attr_init,
                  "pthread_attr_create",
                  "pthread_attr_create");

   function pthread_attr_destroy (attr : access pthread_attr_t) return C.int;
   pragma Import (C,
                  pthread_attr_destroy,
                  "pthread_attr_delete",
                  "pthread_attr_delete");


   --
   --  Thread Attributes
   --
   function pthread_attr_setstacksize
     (attr : access pthread_attr_t; stacksize : size_t) return C.int;
   pragma Import (C,
                  pthread_attr_setstacksize,
                  "pthread_attr_setstacksize",
                  "pthread_attr_setstacksize");

   function pthread_attr_getstacksize
     (attr : access pthread_attr_t; stacksize : access size_t) return C.int;
   pragma Import (C,
                  pthread_attr_getstacksize,
                  "pthread_attr_getstacksize",
                  "pthread_attr_getstacksize");

   function pthread_attr_setdetachstate
     (attr : access pthread_attr_t; detachstate : C.int) return C.int;
   pragma Import (C,
                  pthread_attr_setdetachstate,
                  "pthread_attr_setdetachstate",
                  "pthread_attr_setdetachstate");

   function pthread_attr_getdetachstate
     (attr : access pthread_attr_t; detachstate : access C.int) return C.int;
   pragma Import (C,
                  pthread_attr_getdetachstate,
                  "pthread_attr_getdetachstate",
                  "pthread_attr_getdetachstate");

   function pthread_attr_setname
     (attr : access pthread_attr_t; name : access char) return C.int;
   pragma Import (C,
                  pthread_attr_setname,
                  "pthread_attr_setname",
                  "pthread_attr_setname");

   function pthread_attr_getname
     (attr : access pthread_attr_t; name : access char) return C.int;
   pragma Import (C,
                  pthread_attr_getname,
                  "pthread_attr_getname",
                  "pthread_attr_getname");


   --
   --  Thread Scheduling Attributes
   --
   function pthread_attr_setscope
     (attr : access pthread_attr_t; contentionscope : C.int) return C.int;
   pragma Import (C,
                  pthread_attr_setscope,
                  "pthread_attr_setscope",
                  "pthread_attr_setscope");

   function pthread_attr_getscope
     (attr : access pthread_attr_t;
      contentionscope : access C.int)
      return C.int;
   pragma Import (C,
                  pthread_attr_getscope,
                  "pthread_attr_getscope",
                  "pthread_attr_getscope");


   PTHREAD_INHERIT_SCHED : constant := 0;
   PTHREAD_DEFAULT_SCHED : constant := 1;

   function pthread_attr_setinheritsched
     (attr : access pthread_attr_t; inherit : C.int) return C.int;
   pragma Import (C,
                  pthread_attr_setinheritsched,
                  "pthread_attr_setinheritsched",
                  "pthread_attr_setinheritsched");

   function pthread_attr_getinheritsched
     (attr : access pthread_attr_t; inherit : access C.int) return C.int;
   pragma Import (C,
                  pthread_attr_getinheritsched,
                  "pthread_attr_getinheritsched",
                  "pthread_attr_getinheritsched");

   SCHED_FIFO  : constant := 1;
   SCHED_RR    : constant := 2;
   SCHED_OTHER : constant := 3;

   function pthread_attr_setsched
     (attr : access pthread_attr_t; scheduler : C.int) return C.int;
   pragma Import (C,
                  pthread_attr_setsched,
                  "pthread_attr_setsched",
                  "pthread_attr_setsched");

   function pthread_attr_getsched
     (attr : access pthread_attr_t; scheduler : access C.int) return C.int;
   pragma Import (C,
                  pthread_attr_getsched,
                  "pthread_attr_getsched",
                  "pthread_attr_getsched");

   function pthread_attr_setprio
     (attr : access pthread_attr_t; priority : C.int) return C.int;
   pragma Import (C,
                  pthread_attr_setprio,
                  "pthread_attr_setprio",
                  "pthread_attr_setprio");

   function pthread_attr_getprio
     (attr : access pthread_attr_t; priority : access C.int) return C.int;
   pragma Import (C,
                  pthread_attr_getprio,
                  "pthread_attr_getprio",
                  "pthread_attr_getprio");


   type timespec is
     record
         tv_sec  : C.int;
         tv_nsec : C.long;
     end record;

   --
   --  Thread Creation & Management
   --

   function pthread_create
     (thread        : access pthread_t;
      attr          :        pthread_attr_t;
      start_routine :        start_addr;
      arg           :        void_ptr) return C.int;
   pragma Import (C, pthread_create, "pthread_create", "pthread_create");

   function pthread_join
     (thread : pthread_t; status : access void_ptr) return C.int;
   pragma Import (C, pthread_join, "pthread_join", "pthread_join");

   function pthread_detach (thread : access pthread_t) return C.int;
   pragma Import (C, pthread_detach, "pthread_detach", "pthread_detach");

   procedure pthread_exit (status : void_ptr);
   pragma Import (C, pthread_exit, "pthread_exit", "pthread_exit");

   procedure pthred_yield (arg : void_ptr);
   pragma Import (C, pthred_yield, "pthred_yield", "pthred_yield");

   function pthread_self return pthread_t;
   pragma Import (C, pthread_self, "pthread_self", "pthread_self");

   function pthread_kill (thread : pthread_t) return C.int;
   pragma Import (C, pthread_kill, "pthread_cancel", "pthread_cancel");

   function pthread_setprio
     (thread : pthread_t; priority : C.int) return C.int;
   pragma Import (C, pthread_setprio, "pthread_setprio", "pthread_setprio");

   function pthread_getprio
     (thread : pthread_t; priority : access C.int) return C.int;
   pragma Import (C, pthread_getprio, "pthread_getprio", "pthread_getprio");

   --  Mutex Initialization / Destruction

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t) return C.int;
   pragma Import (C,
                  pthread_mutexattr_init,
                  "pthread_mutexattr_create",
                  "pthread_mutexattr_create");

   function pthread_mutexattr_setkind
     (attr : access pthread_mutexattr_t; kind : int) return C.int;
   pragma Import (C,
                  pthread_mutexattr_setkind,
                  "pthread_mutexattr_setkind_np",
                  "pthread_mutexattr_setkind_np");

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t) return C.int;
   pragma Import (C,
     pthread_mutexattr_destroy,
                  "pthread_mutexattr_delete",
                  "pthread_mutexattr_delete");

   --  Mutex Attributes

   --  Threads queueing order

   MUTEX_PRIORITY : constant := 0; --  wait in priority order
   MUTEX_FIFO : constant := 1; --  first-in-first-out
   MUTEX_PRIORITY_INHERIT : constant := 2; --  priority inhertance mutex
   MUTEX_PRIORITY_CEILING : constant := 3; --  priority ceiling mutex

   --  Mutex debugging options

   MUTEX_NO_DEBUG : constant := 0; --  no debugging on mutex
   MUTEX_DEBUG : constant := 1; --  debugging is on

   --  Mutex Operations

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : pthread_mutexattr_t) return C.int;
   pragma Import
     (C, pthread_mutex_init, "pthread_mutex_init", "pthread_mutex_init");

   function pthread_mutex_destroy (mutex : access pthread_mutex_t)
     return C.int;
   pragma Import (C,
                  pthread_mutex_destroy,
                  "pthread_mutex_destroy",
                  "pthread_mutex_destroy");

   function pthread_mutex_lock (mutex : access pthread_mutex_t) return C.int;
   pragma Import
    (C, pthread_mutex_lock, "pthread_mutex_lock", "pthread_mutex_lock");

   function pthread_mutex_trylock (mutex : access pthread_mutex_t)
     return C.int;
   pragma Import (C,
                  pthread_mutex_trylock,
                  "pthread_mutex_trylock",
                  "pthread_mutex_trylock");

   function pthread_mutex_unlock (mutex : access pthread_mutex_t) return C.int;
   pragma Import (C,
                  pthread_mutex_unlock,
                  "pthread_mutex_unlock",
                  "pthread_mutex_unlock");



   --  Condition Initialization / Destruction

   function pthread_condattr_init
     (attr : access pthread_condattr_t) return C.int;
   pragma Import (C,
                  pthread_condattr_init,
                  "pthread_condattr_create",
                  "pthread_condattr_create");

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t) return C.int;
   pragma Import (C,
                  pthread_condattr_destroy,
                  "pthread_condattr_delete",
                  "pthread_condattr_delete");

   --  Condition Attributes

   COND_PRIORITY : constant := 0; --  wait in priority order
   COND_FIFO     : constant := 1; --  first-in-first-out

   --  Condition debugging options

   COND_NO_DEBUG : constant := 0; --  no debugging on mutex
   COND_DEBUG : constant := 1;    --  debugging is on

   --  Condition sharing attributes

   COND_SHARED : constant := 0;   --  shared between processes
   COND_NOTSHARED : constant := 1; --  not shared between processes


   --  Condition Operations

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : pthread_condattr_t) return C.int;
   pragma Import
     (C, pthread_cond_init, "pthread_cond_init", "pthread_cond_init");

   function pthread_cond_destroy (cond : access pthread_cond_t) return C.int;
   pragma Import (C,
                  pthread_cond_destroy,
                  "pthread_cond_destroy",
                  "pthread_cond_destroy");

   function pthread_cond_signal (cond : access pthread_cond_t) return C.int;
   pragma Import (C,
                  pthread_cond_signal,
                  "pthread_cond_signal",
                  "pthread_cond_signal");

   function pthread_cond_broadcast (cond : access pthread_cond_t) return C.int;
   pragma Import (C,
                  pthread_cond_broadcast,
                  "pthread_cond_broadcast",
                  "pthread_cond_broadcast");

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return C.int;
   pragma Import
     (C, pthread_cond_wait, "pthread_cond_wait", "pthread_cond_wait");

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec) return C.int;
   pragma Import (C,
                  pthread_cond_timedwait,
                  "pthread_cond_timedwait",
                  "pthread_cond_timedwait");

   function pthread_cond_signal_unlock
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return C.int;
   pragma Import (C,
                  pthread_cond_signal_unlock,
                  "pthread_cond_signal_unlock",
                  "pthread_cond_signal_unlock");

   function pthread_cond_wait_unlock
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return C.int;
   pragma Import (C,
                  pthread_cond_wait_unlock,
                  "pthread_cond_wait_unlock",
                  "pthread_cond_wait_unlock");



   PTHREAD_ONCE_INIT : constant := 0; --  set to one after init

   --  Thread-Specific Data

   type Proc_void_ptr_Ptr is access procedure (value : void_ptr);

   function pthread_key_create
     (key : access pthread_key_t; destructor : Proc_void_ptr_Ptr)
      return C.int;
   pragma Import (C,
                  pthread_key_create,
                  "pthread_keycreate",
                  "pthread_keycreate");

   function pthread_setspecific
     (key : pthread_key_t; value : void_ptr) return C.int;
   pragma Import (C,
                  pthread_setspecific,
                  "pthread_setspecific",
                  "pthread_setspecific");

   function pthread_getspecific
     (key : pthread_key_t; value : access void_ptr) return C.int;
   pragma Import (C,
                  pthread_getspecific,
                  "pthread_getspecific",
                  "pthread_getspecific");

   type Proc_Ptr is access procedure;

   function pthread_once
     (once_control : access pthread_once_t;
      init_routine :        Proc_Ptr) return C.int;
   pragma Import (C, pthread_once, "pthread_once", "pthread_once");

   --  Semaphore Operations

   procedure sem_init (sem : access msem_t; value : C.int);
   pragma Import (C, sem_init, "msem_init", "msem_init");

   function sem_destroy (sem : access msem_t) return C.int;
   pragma Import (C, sem_destroy, "msem_remove", "msem_remove");

   function sem_lock (sem : access msem_t; cond : C.int) return C.int;
   pragma Import (C, sem_lock, "msem_lock", "msem_lock");

   function sem_unlock (sem : access msem_t; cond : C.int) return C.int;
   pragma Import (C, sem_unlock, "msem_unlock", "msem_unlock");

   MSEM_UNLOCKED   : constant := 0;  -- Initialize the semahore to unlocked
   MSEM_LOCKED     : constant := 1;  -- Initialize the semahore to locked
   MSEM_IF_NOWAIT  : constant := 2;  -- Do not wait if semaphore is locked
   MSEM_IF_WAITERS : constant := 3;  -- Unlock only if there are waiters

private

   type cma_t_handle is
   record
      field1 : unsigned_long;
      field2 : unsigned_long;
   end record;
   for cma_t_handle'Alignment use Max_Alignment;

   type pthread_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_t);

   type pthread_cond_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_cond_t);

   type pthread_attr_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_attr_t);

   type pthread_mutex_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_mutex_t);

   type pthread_mutexattr_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_mutexattr_t);

   type pthread_condattr_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_condattr_t);

   type pthread_key_t is new unsigned_long;
   type pthread_protocol_t is new int;
   type pthread_addr_t is new void_ptr;

   procedure Require_Package_Body;
   --
   --  Just a dummy procedure specification to require a body for this
   --  package; thereby avoiding differences in the set of source files
   --  needed for the adaptation.
   --

end Interfaces.C.Pthreads;

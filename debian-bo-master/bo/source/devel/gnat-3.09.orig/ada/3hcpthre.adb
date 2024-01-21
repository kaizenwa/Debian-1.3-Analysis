------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 I N T E R F A C E S . C . P T H R E A D S                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.1 $                             --
--                                                                          --
--      Copyright (C) 1991,1992,1993,1994,1995 Florida State University     --
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

with Text_IO;
--  This is a HP-UX 9.05 version of this package.

with System;

with Interfaces.C.POSIX_RTE;
--  Used for, Signal,
--            Signal_Set

with Interfaces.C.POSIX_error; use Interfaces.C.POSIX_error;
--  Used for, Return_Code
--            Failure

with Interfaces.C.POSIX_Timers;
--  Used for, timespec

with Unchecked_Conversion;

package body Interfaces.C.Pthreads is
--  package llio is new Text_IO.Integer_IO (Long_Long_Integer);

   --  eas pragma Linker_Options ("-lgthreads");
   --  eas pragma Linker_Options ("-L.");
   --  eas pragma Linker_Options ("posixlib.o");
   pragma Linker_Options ("-lcma");
   pragma Linker_Options ("-lc_r");

   --  This is necessary to allow structures to be passed to
   --  C functions, since some compiler interfaces to C only allows scalers,
   --  access values, and values of type System.Address as actual parameters.

   Failure : POSIX_Error.Return_Code renames POSIX_Error.Failure;

   -----------------------
   -- pthread_attr_init --
   -----------------------

   procedure pthread_attr_init
     (attributes : out pthread_attr_t;
      result     : out Return_Code)
   is
      function pthread_attr_create
        (attr : System.Address)
         return Return_Code;
      pragma Import (C, pthread_attr_create, "pthread_attr_create");

   begin
--        llio.put (Long_Long_Integer (attributes), 16, 16);
--        Text_IO.put_line (" enter pthread_attr_init");
      result := pthread_attr_create (attributes'Address);
--        llio.put (Long_Long_Integer (attributes), 16, 16);
--        Text_IO.put_line (" leave pthread_attr_init");
   end pthread_attr_init;

   --------------------------
   -- pthread_attr_destroy --
   --------------------------

   procedure pthread_attr_destroy
     (attributes : in out pthread_attr_t;
      result     : out Return_Code)
   is
      function pthread_attr_delete
        (attr : System.Address)
         return Return_Code;
      pragma Import (C, pthread_attr_delete, "pthread_attr_delete");

   begin
--        Text_IO.put_line ("enter pthread_attr_destroy");
      result := pthread_attr_delete (attributes'Address);
--        Text_IO.put_line ("leave pthread_attr_destroy");
   end pthread_attr_destroy;

   -------------------------------
   -- pthread_attr_setstacksize --
   -------------------------------

   procedure pthread_attr_setstacksize
     (attr      : in out pthread_attr_t;
      stacksize : size_t;
      result    : out Return_Code)
   is
      function pthread_attr_setstacksize_base
        (attr      : System.Address;
         stacksize : size_t)
         return      Return_Code;
      pragma Import
        (C, pthread_attr_setstacksize_base, "pthread_attr_setstacksize");

   begin
--        Text_IO.put_line ("enter pthread_attr_setstacksize");
      result := pthread_attr_setstacksize_base (attr'Address, stacksize);
--        Text_IO.put_line ("leave pthread_attr_setstacksize");
   end pthread_attr_setstacksize;

   ---------------------------------
   -- pthread_attr_setdetachstate --
   ---------------------------------

   procedure pthread_attr_setdetachstate
     (attr        : in out pthread_attr_t;
      detachstate : int;
      result      : out Return_Code)
   is
   begin
--        Text_IO.put_line ("pthread_attr_setdetachstate");
      Result := 0;
   end pthread_attr_setdetachstate;

   --------------------
   -- pthread_create --
   --------------------

   procedure pthread_create
     (thread        : out pthread_t;
      attributes    : pthread_attr_t;
      start_routine : System.Address;
      arg           : System.Address;
      result        : out Return_Code)
   is
      function pthread_create_base
        (thread : System.Address;
         attr   : pthread_attr_t;
         start_routine, arg : System.Address)
         return          Return_Code;
      pragma Import (C, pthread_create_base, "pthread_create");

   begin
      --  Text_IO.put_line ("enter pthread_create");
      result := pthread_create_base (thread'Address, attributes,
         start_routine, arg);
      --  Text_IO.put_line (pthread_t'Image (thread));
      --  Text_IO.put_line ("leave pthread_create");
   end pthread_create;

   ------------------
   -- pthread_self --
   ------------------

   function pthread_self return pthread_t is
      function pthread_self_base return pthread_t;
      pragma Import (C, pthread_self_base, "pthread_self");
   begin
      --  Text_IO.put_line ("pthread_self");
      return pthread_self_base;
   end pthread_self;

   ------------------
   -- pthread_init --
   ------------------

   --  This procedure provides a hook into Pthreads initialization that allows
   --  the addition of initializations specific to the Ada Pthreads interface

   procedure pthread_init is

      procedure cma_init_base;
      pragma Import (C, cma_init_base, "cma_init");

   begin
--        Text_IO.put_line ("enter pthread_init");
      cma_init_base;
--        Text_IO.put_line ("leave pthread_init");
   end pthread_init;

   --------------------
   -- pthread_detach --
   --------------------

   procedure pthread_detach
     (thread : in out pthread_t;
      result : out Return_Code)
   is
      function pthread_detach_base
        (thread : System.Address)
         return   Return_Code;
      pragma Import (C, pthread_detach_base, "pthread_detach");

   begin
      --  Text_IO.put_line ("enter pthread_detach");
      result := pthread_detach_base (thread'Address);
      --  Text_IO.put_line ("leave pthread_detach");
   end pthread_detach;

   ----------------------------
   -- pthread_mutexattr_init --
   ----------------------------

   procedure pthread_mutexattr_init
     (attributes : out pthread_mutexattr_t;
      result     : out Return_Code)
   is
      function pthread_mutexattr_create
        (attr : System.Address)
         return Return_Code;
      pragma Import
         (C, pthread_mutexattr_create, "pthread_mutexattr_create");
   begin
--        Text_IO.put_line ("enter pthread_mutexattr_init");
      result := pthread_mutexattr_create (attributes'Address);
--        Text_IO.put_line ("leave pthread_mutexattr_init");
   end pthread_mutexattr_init;

   -----------------------------------
   -- pthread_mutexattr_setprotocol --
   -----------------------------------

   procedure pthread_mutexattr_setprotocol
     (attributes : in out pthread_mutexattr_t;
      protocol   : pthread_protocol_t;
      result     : out Return_Code)
   is
   begin
--        Text_IO.put_line ("pthread_mutexattr_setprotocol");
      result := 0;
   end pthread_mutexattr_setprotocol;

   ---------------------------------------
   -- pthread_mutexattr_setprio_ceiling --
   ---------------------------------------

   procedure pthread_mutexattr_setprio_ceiling
     (attributes   : in out pthread_mutexattr_t;
      prio_ceiling : int;
      result       : out Return_Code)
   is
   begin
--        Text_IO.put_line ("pthread_mutexattr_setprio_ceiling");
      result := 0;
   end pthread_mutexattr_setprio_ceiling;

   ------------------------
   -- pthread_mutex_init --
   ------------------------

   procedure pthread_mutex_init
     (mutex      : out pthread_mutex_t;
      attributes : pthread_mutexattr_t;
      result     : out Return_Code)
   is
      function pthread_mutex_init_base
        (mutex : System.Address;
         attr  : pthread_mutexattr_t)
         return  Return_Code;
      pragma Import
        (C, pthread_mutex_init_base, "pthread_mutex_init");

   begin
--        Text_IO.put_line ("enter pthread_mutex_init");
      result := pthread_mutex_init_base (mutex'Address, attributes);
--        Text_IO.put_line ("leave pthread_mutex_init");
   end pthread_mutex_init;

   ---------------------------
   -- pthread_mutex_destroy --
   ---------------------------

   procedure pthread_mutex_destroy
     (mutex  : in out pthread_mutex_t;
      result : out Return_Code)
   is
      function pthread_mutex_destroy_base
        (mutex : System.Address)
         return  Return_Code;
      pragma Import
         (C, pthread_mutex_destroy_base, "pthread_mutex_destroy");

   begin
--        Text_IO.put_line ("enter pthread_mutex_destroy");
      result := pthread_mutex_destroy_base (mutex'Address);
--        Text_IO.put_line ("leave pthread_mutex_destroy");
   end pthread_mutex_destroy;

   ---------------------------
   -- pthread_mutex_trylock --
   ---------------------------

   procedure pthread_mutex_trylock
     (mutex  : in out pthread_mutex_t;
      result : out Return_Code)
   is
      function pthread_mutex_trylock_base
        (mutex : System.Address)
         return  Return_Code;
      pragma Import
         (C, pthread_mutex_trylock_base, "pthread_mutex_trylock");

   begin
--        Text_IO.put_line ("enter pthread_mutex_trylock");
      result := pthread_mutex_trylock_base (mutex'Address);
--        Text_IO.put_line ("leave pthread_mutex_trylock");
   end pthread_mutex_trylock;

   ------------------------
   -- pthread_mutex_lock --
   ------------------------

   procedure pthread_mutex_lock
     (mutex  : in out pthread_mutex_t;
      result : out Return_Code)
   is
      function pthread_mutex_lock_base (mutex : System.Address)
         return Return_Code;
      pragma Import (C, pthread_mutex_lock_base, "pthread_mutex_lock");

   begin
--        Text_IO.put_line ("enter pthread_mutex_lock");
      result := pthread_mutex_lock_base (mutex'Address);
--        Text_IO.put_line ("leave pthread_mutex_lock");
   end pthread_mutex_lock;

   --------------------------
   -- pthread_mutex_unlock --
   --------------------------

   procedure pthread_mutex_unlock
     (mutex  : in out pthread_mutex_t;
      result : out Return_Code)
   is
      function pthread_mutex_unlock_base (mutex : System.Address)
         return Return_Code;
      pragma Import (C, pthread_mutex_unlock_base, "pthread_mutex_unlock");

   begin
--        Text_IO.put_line ("enter pthread_mutex_unlock");
      result := pthread_mutex_unlock_base (mutex'Address);
--        Text_IO.put_line ("leave pthread_mutex_unlock");
   end pthread_mutex_unlock;

   -----------------------
   -- pthread_cond_init --
   -----------------------

   procedure pthread_cond_init
     (condition  : out pthread_cond_t;
      attributes : pthread_condattr_t;
      result     : out Return_Code)
   is
      function pthread_cond_init_base
        (cond : System.Address;
         attr : pthread_condattr_t)
         return Return_Code;
      pragma Import (C, pthread_cond_init_base, "pthread_cond_init");

   begin
--        Text_IO.put_line ("enter pthread_cond_init");
      result := pthread_cond_init_base (condition'Address, attributes);
--        Text_IO.put_line ("leave pthread_cond_init");
   end pthread_cond_init;

   -----------------------
   -- pthread_cond_wait --
   -----------------------

   procedure pthread_cond_wait
     (condition : in out pthread_cond_t;
      mutex     : in out pthread_mutex_t;
      result    : out Return_Code)
   is
      function pthread_cond_wait_base (cond, mutex : System.Address)
         return  Return_Code;
      pragma Import (C, pthread_cond_wait_base, "pthread_cond_wait");

   begin
--        Text_IO.put_line ("enter pthread_cond_wait");
      result := pthread_cond_wait_base (condition'Address, mutex'Address);
--        Text_IO.put_line ("leave pthread_cond_wait");
   end pthread_cond_wait;

   ----------------------------
   -- pthread_cond_timedwait --
   ----------------------------

   procedure pthread_cond_timedwait
     (condition     : in out pthread_cond_t;
      mutex         : in out pthread_mutex_t;
      absolute_time : POSIX_Timers.timespec;
      result        : out Return_Code)
   is
      function pthread_cond_timedwait_base
         (cond, mutex, abstime : System.Address)
         return    Return_Code;
      pragma Import
         (C, pthread_cond_timedwait_base, "pthread_cond_timedwait");

   begin
--        Text_IO.put_line ("enter pthread_cond_timedwait");
      result := pthread_cond_timedwait_base (condition'Address,
         mutex'Address, absolute_time'Address);
--        Text_IO.put_line ("leave pthread_cond_timedwait");
   end pthread_cond_timedwait;

   -------------------------
   -- pthread_cond_signal --
   -------------------------

   procedure pthread_cond_signal
     (condition : in out pthread_cond_t;
      result    : out Return_Code)
   is
      function pthread_cond_signal_base (cond : System.Address)
         return Return_Code;
      pragma Import (C, pthread_cond_signal_base, "pthread_cond_signal");

   begin
--        Text_IO.put_line ("enter pthread_cond_signal");
      result := pthread_cond_signal_base (condition'Address);
--        Text_IO.put_line ("leave pthread_cond_signal");
   end pthread_cond_signal;

   ----------------------------
   -- pthread_cond_broadcast --
   ----------------------------

   procedure pthread_cond_broadcast
     (condition : in out pthread_cond_t;
      result    : out Return_Code)
   is
      function pthread_cond_broadcast_base (cond : System.Address)
         return Return_Code;
      pragma Import
         (C, pthread_cond_broadcast_base, "pthread_cond_broadcast");

   begin
--        Text_IO.put_line ("enter pthread_cond_broadcast");
      result := pthread_cond_broadcast_base (condition'Address);
--        Text_IO.put_line ("leave pthread_cond_broadcast");
   end pthread_cond_broadcast;

   --------------------------
   -- pthread_cond_destroy --
   --------------------------

   procedure pthread_cond_destroy
     (condition : in out pthread_cond_t;
      result    : out Return_Code)
   is
      function pthread_cond_destroy_base (cond : System.Address)
         return Return_Code;
      pragma Import (C, pthread_cond_destroy_base, "pthread_cond_destroy");

   begin
--        Text_IO.put_line ("enter pthread_cond_destroy");
      result := pthread_cond_destroy_base (condition'Address);
--        Text_IO.put_line ("leave pthread_cond_destroy");
   end pthread_cond_destroy;

   ---------------------------
   -- pthread_condattr_init --
   ---------------------------

   procedure pthread_condattr_init
     (attributes : out pthread_condattr_t;
      result     : out Return_Code)
   is
      function pthread_condattr_create (cond : System.Address)
         return Return_Code;
      pragma Import
         (C, pthread_condattr_create, "pthread_condattr_create");

   begin
--        Text_IO.put_line ("enter pthread_condattr_init");
      result := pthread_condattr_create (attributes'Address);
--        Text_IO.put_line ("leave pthread_condattr_init");
   end pthread_condattr_init;

   ------------------------------
   -- pthread_condattr_destroy --
   ------------------------------

   procedure pthread_condattr_destroy
     (attributes : in out pthread_condattr_t;
      result     : out Return_Code)
   is
      function pthread_condattr_delete_base (cond : System.Address)
         return Return_Code;
      pragma Import
        (C, pthread_condattr_delete_base, "pthread_condattr_delete");

   begin
--        Text_IO.put_line ("enter pthread_condattr_destroy");
      result := pthread_condattr_delete_base (attributes'Address);
--        Text_IO.put_line ("leave pthread_condattr_destroy");
   end pthread_condattr_destroy;

   -------------------------
   -- pthread_setspecific --
   -------------------------

   --  Suppress all checks to prevent stack check on entering routine
   --  which routine does this comment belong in???
   --  need pragma Suppress in spec for routine???
   --  Also need documentation of why suppress is needed ???

   procedure pthread_setspecific
     (key    : pthread_key_t;
      value  : System.Address;
      result : out Return_Code)
   is
      function pthread_setspecific_base
        (key   : pthread_key_t;
         value : System.Address)
         return  Return_Code;
      pragma Import (C, pthread_setspecific_base, "pthread_setspecific");

   begin
--        Text_IO.put_line ("enter pthread_setspecific");
      result := pthread_setspecific_base (key, value);
--        Text_IO.put_line ("leave pthread_setspecific");
   end pthread_setspecific;

   -------------------------
   -- pthread_getspecific --
   -------------------------

   procedure pthread_getspecific
     (key    : pthread_key_t;
      value  : out System.Address;
      result : out Return_Code)
   is
      function pthread_getspecific_base
        (key   : pthread_key_t;
         value : System.Address)
         return  Return_Code;
      pragma Import (C, pthread_getspecific_base, "pthread_getspecific");

   begin
--        Text_IO.put_line ("enter pthread_getspecific");
      result := pthread_getspecific_base (key, value'Address);
--        Text_IO.put_line ("leave pthread_getspecific");
   end pthread_getspecific;

   ------------------------
   -- pthread_key_create --
   ------------------------

   procedure pthread_key_create
     (key        : out pthread_key_t;
      destructor : System.Address;
      result     : out Return_Code)
   is
      function pthread_keycreate
        (key, destructor : System.Address)
         return       Return_Code;
      pragma Import (C, pthread_keycreate, "pthread_keycreate");

   begin
--        Text_IO.put_line ("enter pthread_key_create");
      result := pthread_keycreate (key'Address, destructor);
--        Text_IO.put_line ("leave pthread_key_create");
   end pthread_key_create;

   --------------------------
   -- pthread_attr_setprio --
   --------------------------

   procedure pthread_attr_setprio
     (attr     : in out pthread_attr_t;
      priority : Priority_Type;
      result   : out Return_Code)
   is
      pri_other_min : constant Priority_Type := 8;
      pri_other_max : constant Priority_Type := 15;

      prior : Priority_Type := priority;

      function pthread_attr_setprio_base
        (attr     : System.Address;
         priority : Priority_Type)
         return     Return_Code;
      pragma Import (C, pthread_attr_setprio_base, "pthread_attr_setprio");

   begin
      if prior = 0 then
         prior := pri_other_min;
      elsif prior > pri_other_max then
         prior := pri_other_max;
      elsif prior < pri_other_min then
         prior := pri_other_min;
      end if;
--        llio.put (Long_Long_Integer (attr), 16, 16);
--        Text_IO.put_line ("enter pthread_attr_setprio");
      result := pthread_attr_setprio_base (attr'Address, prior);
--        llio.put (Long_Long_Integer (attr), 16, 16);
--        Text_IO.put_line ("leave pthread_attr_setprio");
   end pthread_attr_setprio;

   --------------------------
   -- pthread_attr_getprio --
   --------------------------

   procedure pthread_attr_getprio
     (attr     : pthread_attr_t;
      priority : out Priority_Type;
      result   : out Return_Code)
   is
      Temp_Result : Return_Code;

      function pthread_attr_getprio_base (attr : pthread_attr_t)
         return Return_Code;
      pragma Import (C, pthread_attr_getprio_base, "pthread_attr_getprio");

   begin
--        Text_IO.put_line ("enter pthread_attr_getprio");
      Temp_Result := pthread_attr_getprio_base (attr);

      if Temp_Result /= Failure then
         priority := Priority_Type (Temp_Result);
         result := 0;

      --  For failure case, send out lowest priority (is it OK ???)

      else
         priority := Priority_Type'First;
         result := Failure;
      end if;

--        Text_IO.put_line ("leave pthread_attr_getprio");
   end pthread_attr_getprio;

   --------------------------
   -- pthread_setschedattr --
   --------------------------

   procedure pthread_setschedattr
     (thread     : pthread_t;
      attributes : pthread_attr_t;
      result     : out Return_Code)
   is
   begin
      --  Text_IO.put_line ("pthread_setschedattr");
      result := 0;
   end pthread_setschedattr;

   --------------------------
   -- pthread_getschedattr --
   --------------------------

   procedure pthread_getschedattr
     (thread      : pthread_t;
      attributes  : in out pthread_attr_t;
      result      : out Return_Code)
   is
   begin
--        llio.put (Long_Long_Integer (attributes), 16, 16);
      --  Text_IO.put_line ("pthread_getschedattr");
      result := 0;
   end pthread_getschedattr;

   -------------
   -- sigwait --
   -------------

   procedure sigwait
     (set         : POSIX_RTE.Signal_Set;
      sig         : out POSIX_RTE.Signal;
      result      : out Return_Code)
   is
      Temp_Result : Return_Code;

      function sigwait_base (set : System.Address) return Return_Code;
      pragma Import (C, sigwait_base, "cma_sigwait");

   begin
--        Text_IO.put_line ("enter sigwait");
      Temp_Result := sigwait_base (set'Address);

      if Temp_Result /= Failure then
         sig := POSIX_RTE.Signal (Temp_Result);
      else
         sig := 0;
      end if;

      result := Temp_Result;
--        Text_IO.put_line ("leave sigwait");
   end sigwait;

   ------------------
   -- pthread_kill --
   ------------------

   procedure pthread_kill
     (thread : pthread_t;
      sig    : POSIX_RTE.Signal;
      result : out Return_Code)
   is
      --  Currently can not send any signal except kill
      function pthread_kill_base
        (thread : System.Address)
         return   Return_Code;
      pragma Import (C, pthread_kill_base, "pthread_cancel");

   begin
      --  Text_IO.put_line ("enter pthread_kill");
      --  Text_IO.put_line (pthread_t'Image (thread));
      result := pthread_kill_base (thread'Address);
      --  Text_IO.put_line (pthread_t'Image (thread));
      --  Text_IO.put_line (Return_Code'Image (result));
      result := 0;
      --  Text_IO.put_line ("leave pthread_kill");
   end pthread_kill;

   --------------------------
   -- pthread_cleanup_push --
   --------------------------

   procedure pthread_cleanup_push
     (routine : System.Address;
      arg     : System.Address)
   is
   begin
--        Text_IO.put_line ("pthread_cleanup_push");
      null;
   end pthread_cleanup_push;

   -------------------------
   -- pthread_cleanup_pop --
   -------------------------

   procedure pthread_cleanup_pop (execute : int) is
   begin
--        Text_IO.put_line ("pthread_cleanup_pop");
      null;
   end pthread_cleanup_pop;

   -------------------
   -- pthread_yield --
   -------------------

   procedure pthread_yield is
      procedure pthread_yield_base;
      pragma Import (C, pthread_yield_base, "pthread_yield");

   begin
--        Text_IO.put_line ("enter pthread_yield");
      pthread_yield_base;
--        Text_IO.put_line ("leave pthread_yield");
   end pthread_yield;

begin
   pthread_init;
end Interfaces.C.Pthreads;

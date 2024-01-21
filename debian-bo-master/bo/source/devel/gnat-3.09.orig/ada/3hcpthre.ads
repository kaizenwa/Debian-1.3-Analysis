------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 I N T E R F A C E S . C . P T H R E A D S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.1 $                             --
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

--  This is a HP_UX 9.05 version of this package.

--  This package interfaces with Pthreads. It is not a complete interface;
--  it only includes what is needed to implement the Ada runtime.

with System;

with Interfaces.C.System_Constants;
--  Used for: Add_Prio
--            pthread_attr_t_size
--            pthread_mutexattr_t_size
--            pthread_mutex_t_size
--            pthread_condattr_t_size
--            pthread_cond_t_size
--            NO_PRIO_INHERIT
--            PRIO_INHERIT
--            PRIO_PROTECT

with Interfaces.C.POSIX_RTE;
--  Used for: Signal,
--            Signal_Set

with Interfaces.C.POSIX_Error;
--  Used for: Return_Code

with Interfaces.C.POSIX_Timers;
--  Used for: timespec

package Interfaces.C.Pthreads is

   package ICPE renames Interfaces.C.POSIX_Error;

   Alignment : constant := Natural'Min (Standard'Maximum_Alignment, 8);

   Pthreads_Error : exception;

   type Priority_Type is new int;

   type pthread_t       is private;
   type pthread_mutex_t is private;
   type pthread_cond_t  is private;

   type pthread_attr_t      is private;
   type pthread_mutexattr_t is private;
   type pthread_condattr_t  is private;
   type pthread_key_t       is private;
   type pthread_protocol_t  is private;

   NO_PRIO_INHERIT : constant pthread_protocol_t;
   PRIO_INHERIT    : constant pthread_protocol_t;
   PRIO_PROTECT    : constant pthread_protocol_t;

   procedure pthread_attr_init
     (attributes : out pthread_attr_t;
      result     : out ICPE.Return_Code);
   pragma Inline (pthread_attr_init);

   procedure pthread_attr_destroy
     (attributes : in out pthread_attr_t;
      result     : out ICPE.Return_Code);
   pragma Inline (pthread_attr_destroy);

   procedure pthread_attr_setstacksize
     (attr      : in out pthread_attr_t;
      stacksize : size_t;
      result    : out ICPE.Return_Code);
   pragma Inline (pthread_attr_setstacksize);

   procedure pthread_attr_setdetachstate
     (attr        : in out pthread_attr_t;
      detachstate : int;
      Result      : out ICPE.Return_Code);
   pragma Inline (pthread_attr_setdetachstate);

   procedure pthread_create
     (thread        : out pthread_t;
      attributes    : pthread_attr_t;
      start_routine : System.Address;
      arg           : System.Address;
      result        : out ICPE.Return_Code);
   pragma Inline (pthread_create);

   procedure pthread_init;

   function pthread_self return pthread_t;
   pragma Inline (pthread_self);

   procedure pthread_detach
     (thread : in out pthread_t;
      result : out ICPE.Return_Code);
   pragma Inline (pthread_detach);

   procedure pthread_mutexattr_init
     (attributes : out pthread_mutexattr_t;
      result     : out ICPE.Return_Code);
   pragma Inline (pthread_mutexattr_init);

   procedure pthread_mutexattr_setprotocol
     (attributes : in out pthread_mutexattr_t;
      protocol   : pthread_protocol_t;
      result     : out ICPE.Return_Code);
   pragma Inline (pthread_mutexattr_setprotocol);

   procedure pthread_mutexattr_setprio_ceiling
     (attributes   : in out pthread_mutexattr_t;
      prio_ceiling : int;
      result       : out ICPE.Return_Code);
   pragma Inline (pthread_mutexattr_setprio_ceiling);

   procedure pthread_mutex_init
     (mutex      : out pthread_mutex_t;
      attributes : pthread_mutexattr_t;
      result     : out ICPE.Return_Code);
   pragma Inline (pthread_mutex_init);

   procedure pthread_mutex_destroy
     (mutex  : in out pthread_mutex_t;
      result : out ICPE.Return_Code);
   pragma Inline (pthread_mutex_destroy);

   procedure pthread_mutex_trylock
     (mutex  : in out pthread_mutex_t;
      result : out ICPE.Return_Code);
   pragma Inline (pthread_mutex_trylock);

   procedure pthread_mutex_lock
     (mutex  : in out pthread_mutex_t;
      result : out ICPE.Return_Code);
   pragma Inline (pthread_mutex_lock);

   procedure pthread_mutex_unlock
     (mutex  : in out pthread_mutex_t;
      result : out ICPE.Return_Code);
   pragma Inline (pthread_mutex_unlock);

   procedure pthread_cond_init
     (condition  : out pthread_cond_t;
      attributes : pthread_condattr_t;
      result     : out ICPE.Return_Code);
   pragma Inline (pthread_cond_init);

   procedure pthread_cond_wait
     (condition : in out pthread_cond_t;
      mutex     : in out pthread_mutex_t;
      result    : out ICPE.Return_Code);
   pragma Inline (pthread_cond_wait);

   procedure pthread_cond_timedwait
     (condition     : in out pthread_cond_t;
      mutex         : in out pthread_mutex_t;
      absolute_time : Interfaces.C.POSIX_Timers.timespec;
      result        : out ICPE.Return_Code);
   pragma Inline (pthread_cond_timedwait);

   procedure pthread_cond_signal
     (condition : in out pthread_cond_t;
      result    : out ICPE.Return_Code);
   pragma Inline (pthread_cond_signal);

   procedure pthread_cond_broadcast
     (condition : in out pthread_cond_t;
      result    : out ICPE.Return_Code);
   pragma Inline (pthread_cond_broadcast);

   procedure pthread_cond_destroy
     (condition : in out pthread_cond_t;
      result    : out ICPE.Return_Code);
   pragma Inline (pthread_cond_destroy);

   procedure pthread_condattr_init
     (attributes : out pthread_condattr_t;
      result     : out ICPE.Return_Code);
   pragma Inline (pthread_condattr_init);

   procedure pthread_condattr_destroy
     (attributes : in out pthread_condattr_t;
      result     : out ICPE.Return_Code);
   pragma Inline (pthread_condattr_destroy);

   procedure pthread_setspecific
     (key    : pthread_key_t;
      value  : System.Address;
      result : out ICPE.Return_Code);
   pragma Inline (pthread_setspecific);

   procedure pthread_getspecific
     (key    : pthread_key_t;
      value  : out System.Address;
      result : out ICPE.Return_Code);
   pragma Inline (pthread_getspecific);

   procedure pthread_key_create
     (key        : out pthread_key_t;
      destructor : System.Address;
      result     : out ICPE.Return_Code);
   pragma Inline (pthread_key_create);

   procedure pthread_attr_setprio
     (attr     : in out pthread_attr_t;
      priority : Priority_Type;
      result   : out ICPE.Return_Code);
   pragma Inline (pthread_attr_setprio);

   procedure pthread_attr_getprio
     (attr     : pthread_attr_t;
      priority : out Priority_Type;
      result   : out ICPE.Return_Code);
   pragma Inline (pthread_attr_getprio);

   procedure pthread_setschedattr
     (thread     : pthread_t;
      attributes : pthread_attr_t;
      result     : out ICPE.Return_Code);
   pragma Inline (pthread_setschedattr);

   procedure pthread_getschedattr
     (thread     : pthread_t;
      attributes : in out pthread_attr_t;
      result     : out ICPE.Return_Code);
   pragma Inline (pthread_getschedattr);

   procedure pthread_exit (status : System.Address);
   pragma Interface (C, pthread_exit);
   pragma Interface_Name (pthread_exit, "pthread_exit");

   procedure sigwait
     (set    : Interfaces.C.POSIX_RTE.Signal_Set;
      sig    : out Interfaces.C.POSIX_RTE.Signal;
      result : out ICPE.Return_Code);
   pragma Inline (sigwait);

   procedure pthread_kill
     (thread : pthread_t; sig : Interfaces.C.POSIX_RTE.Signal;
      result : out ICPE.Return_Code);
   pragma Inline (pthread_kill);

   procedure pthread_cleanup_push
     (routine : System.Address;
      arg     : System.Address);
   pragma Inline (pthread_cleanup_push);

   procedure pthread_cleanup_pop (execute : int);
   pragma Inline (pthread_cleanup_pop);

   procedure pthread_yield;
   pragma Inline (pthread_yield);

private

   --  The use of longword alignment for the following C types is
   --  a stopgap measure which is not generally portable.  A portable
   --  solution will require some means of getting alignment information
   --  from the C compiler.

   type pthread_attr_t is new long_long_integer;
      --  eas array (1 .. System_Constants.pthread_attr_t_size)
      --  eas of unsigned_char;
   for pthread_attr_t'Alignment use Alignment;

   type pthread_mutexattr_t is new long_long_integer;
      --  eas array (1 .. System_Constants.pthread_mutexattr_t_size)
      --  eas of unsigned_char;
   for pthread_mutexattr_t'Alignment use Alignment;

   type pthread_mutex_t is
     array (1 .. System_Constants.pthread_mutex_t_size) of unsigned_char;
   for pthread_mutex_t'Alignment use Alignment;

   type pthread_condattr_t is new long_long_integer;
      --  eas array (1 .. System_Constants.pthread_condattr_t_size)
      --  eas of unsigned_char;
   for pthread_condattr_t'Alignment use Alignment;

   type pthread_cond_t is
     array (1 .. System_Constants.pthread_cond_t_size) of unsigned_char;
   for pthread_cond_t'Alignment use Alignment;

   type pthread_t is
     array (1 .. System_Constants.pthread_t_size) of unsigned_char;
--   type pthread_t is new unsigned;
   --  ??? The use of an array here seems to cause
   --  problems in the compiler.  It put a long word 4 in the
   --  instruction stream of Interfaces.C.Pthreads.pthread_self.  This
   --  is obviously meant to provide some clue as to what size
   --  item it is to return, but pthread_self tried to execute it.

   type pthread_key_t is new unsigned;
   --  This type is passed as a scaler into a C function. It must
   --  be declared as a scaler, not an array.  This being so, an unsigned
   --  should work.

   type pthread_protocol_t is new int;

   NO_PRIO_INHERIT : constant pthread_protocol_t :=
                                           System_Constants.NO_PRIO_INHERIT;
   PRIO_INHERIT    : constant pthread_protocol_t :=
                                           System_Constants.PRIO_INHERIT;
   PRIO_PROTECT     : constant pthread_protocol_t :=
                                           System_Constants.PRIO_PROTECT;

end Interfaces.C.Pthreads;

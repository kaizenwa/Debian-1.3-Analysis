------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                I N T E R F A C E S . C . P O S I X _ R T E               --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.3 $                             --
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

--  This package interfaces with the POSIX real-time extensions. It may
--  also implement some of them using UNIX operations. It is not a complete
--  interface, it only includes what is needed to implement the Ada runtime.

with System;

with Interfaces.C.POSIX_Error;
--  Used for: Return_Code

with Interfaces.C.Pthreads;

with Interfaces.C.System_Constants;
--  Used for: various constants

package Interfaces.C.POSIX_RTE is

   pragma Elaborate_Body (Interfaces.C.POSIX_RTE);

   package ICCS renames Interfaces.C.System_Constants;
   package ICPE renames Interfaces.C.POSIX_Error;

   Alignment : constant := Standard'Maximum_Alignment;

   NSIG : constant := ICCS.NSIG;
   --  Maximum number of signal entries

   type Signal is new int;

   type Signal_Set is private;

   procedure sigaddset
     (set    : access Signal_Set;
      sig    : in Signal;
      Result : out ICPE.Return_Code);

   procedure sigdelset
     (set    : access Signal_Set;
      sig    : in Signal;
      Result : out ICPE.Return_Code);

   procedure sigfillset
     (set : access Signal_Set;
      Result : out ICPE.Return_Code);

   procedure sigemptyset
     (set : access Signal_Set;
      Result : out ICPE.Return_Code);

   function sigismember
     (set  : access Signal_Set;
      sig  : Signal)
      return ICPE.Return_Code;
   pragma Import (C, sigismember, "sigismember");

   type sigval is record
      u0 : int;
   end record;
   --  This is not used at the moment, need to update to reflect
   --  any changes in the Pthreads signal.h in the future

   type struct_siginfo is record
      si_signo : Signal;
      si_errno : int;
      si_code  : int;
   end record;

   --  Lay out the POSIX-defined components of the struct_siginfo record to
   --  match their C definition, using information in the i-csycon.ads file.
   --  ??? This depends on constants describing the record computed
   --      by a program that must be run on the target architecture,
   --      and cannot handle records containing bit fields.
   --      This should be done by redefining the entire C structure as
   --      an Ada record and then naming it in a Convention C pragma.  This
   --      will have to be derived from the source code of the
   --      C header file, either automatically or manually.

   for struct_siginfo use record
      si_signo at ICCS.si_signo_offset / System.Storage_Unit
        range 0 .. Signal'Size - 1;
      si_errno at ICCS.si_errno_offset / System.Storage_Unit
        range 0 .. int'Size - 1;
      si_code at ICCS.si_code_offset / System.Storage_Unit
        range 0 .. int'Size - 1;
   end record;
   for struct_siginfo'Size use ICCS.siginfo_size * unsigned_char'Size;
   for struct_siginfo'Alignment use Alignment;

   type siginfo_ptr is access all struct_siginfo;

   type sigset_t_ptr is access Signal_Set;

   SIG_ERR : constant := ICCS.SIG_ERR;
   SIG_DFL : constant := ICCS.SIG_DFL;
   SIG_IGN : constant := ICCS.SIG_IGN;
   --  constants for sa_handler

   type struct_sigaction is
      record
         sa_handler : System.Address;     --  SIG_DFL, SIG_IGN, or *fn
         sa_mask    : aliased Signal_Set; --  additional set of sigs to be
         sa_flags   : int;                --  see below for values
      end record;

   type sigaction_ptr is access struct_sigaction;

   --  Signal catching function (signal handler) has the following profile :

   --  procedure Signal_Handler
   --    (signo   : Signal;
   --     info    : siginfo_ptr;
   --     context : sigcontext_ptr);

   SA_NOCLDSTOP : constant := ICCS.SA_NOCLDSTOP;
   --  Don't send a SIGCHLD on child stop

   SA_SIGINFO : constant := ICCS.SA_SIGINFO;
   --  sa_flags flags for struct_sigaction

   SIG_BLOCK   : constant := ICCS.SIG_BLOCK;
   SIG_UNBLOCK : constant := ICCS.SIG_UNBLOCK;
   SIG_SETMASK : constant := ICCS.SIG_SETMASK;
   --  sigprocmask flags (how)

   type jmp_buf is array (1 .. ICCS.jmp_buf_size) of unsigned_char;
   for jmp_buf'Alignment use Alignment;

   type sigjmp_buf is array (1 .. ICCS.sigjmp_buf_size) of unsigned_char;
   for sigjmp_buf'Alignment use Alignment;

   type jmp_buf_ptr is access jmp_buf;

   type sigjmp_buf_ptr is access sigjmp_buf;
   --  Environment for long jumps

   procedure sigaction
     (sig    : Signal;
      act    : access struct_sigaction;
      oact   : access struct_sigaction;
      Result : out ICPE.Return_Code);
   pragma Inline (sigaction);
   --  install new sigaction structure and obtain old one

   procedure sigaction
     (sig    : Signal;
      oact   : access struct_sigaction;
      Result : out ICPE.Return_Code);
   pragma Inline (sigaction);
   --  Same thing as above, but without the act parameter. By passing null
   --  pointer we can find out the action associated with it.
   --  WE WANT TO MAKE THIS VERSION TO INCLUDE THE PREVIOUS sigaction.
   --  TO BE FIXED LATER ???

   function sigtimedwait
     (sigset   : access Signal_Set;
      waittime : access Interfaces.C.Pthreads.timespec;
      siginfo  : access struct_siginfo) return int;
   pragma Import (C, sigtimedwait, "sigwaitprim", "sigwaitprim");

   procedure sigprocmask
     (how    : int;
      set    : access Signal_Set;
      oset   : access Signal_Set;
      Result : out ICPE.Return_Code);
   pragma Inline (sigprocmask);
   --  Install new signal mask and obtain old one

   procedure sigsuspend
     (mask   : access Signal_Set;
      Result : out ICPE.Return_Code);
   pragma Inline (sigsuspend);
   --  Suspend waiting for signals in mask and resume after
   --  executing handler or take default action

   procedure sigpending
     (set    : access Signal_Set;
      Result : out ICPE.Return_Code);
   pragma Inline (sigpending);
   --  get pending signals on thread and process

   procedure longjmp (env : jmp_buf; val : int);
   pragma Inline (longjmp);
   --  execute a jump across procedures according to setjmp

   procedure siglongjmp (env : sigjmp_buf; val : int);
   pragma Inline (siglongjmp);
   --  execute a jump across procedures according to sigsetjmp

   procedure setjmp (env : jmp_buf; Result : out ICPE.Return_Code);
   pragma Inline (setjmp);
   --  set up a jump across procedures and return here with longjmp

   procedure sigsetjmp
     (env      : sigjmp_buf;
      savemask : int;
      Result   : out ICPE.Return_Code);
   pragma Inline (sigsetjmp);
   --  Set up a jump across procedures and return here with siglongjmp

   SIGKILL                    : constant Signal := ICCS.SIGKILL;
   SIGSTOP                    : constant Signal := ICCS.SIGSTOP;
   --  Signals which cannot be masked

   --  Some synchronous signals (cannot be used for interrupt entries)

   SIGALRM                    : constant Signal := ICCS.SIGALRM;

   SIGILL                     : constant Signal := ICCS.SIGILL;
   SIGFPE                     : constant Signal := ICCS.SIGFPE;
   SIGSEGV                    : constant Signal := ICCS.SIGSEGV;

   SIGABRT                    : constant Signal := ICCS.SIGABRT;

   --  Signals which can be used for Interrupt Entries.

   SIGHUP                     : constant Signal := ICCS.SIGHUP;
   SIGINT                     : constant Signal := ICCS.SIGINT;
   SIGQUIT                    : constant Signal := ICCS.SIGQUIT;
   SIGPIPE                    : constant Signal := ICCS.SIGPIPE;
   SIGTERM                    : constant Signal := ICCS.SIGTERM;
   SIGUSR1                    : constant Signal := ICCS.SIGUSR1;
   SIGUSR2                    : constant Signal := ICCS.SIGUSR2;
   SIGCHLD                    : constant Signal := ICCS.SIGCHLD;
   SIGCONT                    : constant Signal := ICCS.SIGCONT;
   SIGTSTP                    : constant Signal := ICCS.SIGTSTP;
   SIGTTIN                    : constant Signal := ICCS.SIGTTIN;
   SIGTTOU                    : constant Signal := ICCS.SIGTTOU;

   --  OS specific signals

   type Signal_Array is array (positive range <>) of Signal;

   OS_Specific_Sync_Signals :
      Signal_Array (ICCS.OS_Specific_Sync_Sigs'Range);

   OS_Specific_Async_Signals :
      Signal_Array (ICCS.OS_Specific_Async_Sigs'Range);

private

   type Signal_Set is array
      (1 .. ICCS.sigset_t_size) of unsigned_char;
   for Signal_Set'Alignment use 4;

   --  Lay out the POSIX-defined components of the struct_sigaction record to
   --  match their C definition, using information in the i-csycon.ads file.
   --  ??? This depends on constants describing the record computed
   --      by a program that must be run on the target architecture,
   --      and cannot handle records containing bit fields.
   --      This should be done by redefining the entire C structure as
   --      an Ada record and then naming it in a Convention C pragma.  This
   --      will have to be derived from the source code of the
   --      C header file, either automatically or manually.

   for struct_sigaction use record
      sa_handler at ICCS.sa_handler_offset / System.Storage_Unit
        range 0 .. Standard'Address_Size - 1;
      sa_mask at ICCS.sa_mask_offset / System.Storage_Unit
        range 0 .. ICCS.Sigset_T_Size * System.Storage_Unit - 1;
      sa_flags at ICCS.sa_flags_offset / System.Storage_Unit
        range 0 .. int'Size - 1;
   end record;
   for struct_sigaction'Size use ICCS.sigaction_size * unsigned_char'Size;
   for struct_sigaction'Alignment use Alignment;

end Interfaces.C.POSIX_RTE;

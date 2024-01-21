------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                I N T E R F A C E S . C . P O S I X _ R T E               --
--                                                                          --
--                                  B o d y                                 --
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

with Interfaces.C.POSIX_Error;
--  Used for, Return_Code

with Unchecked_Conversion;

package body Interfaces.C.POSIX_RTE is

   use Interfaces.C;
   use Interfaces.C.POSIX_Error;

   function Address_to_Pointer is new
     Unchecked_Conversion (System.Address, sigaction_ptr);

   function Address_to_Pointer is new
     Unchecked_Conversion (System.Address, sigset_t_ptr);

   function Address_to_Pointer is new
     Unchecked_Conversion (System.Address, jmp_buf_ptr);

   function Address_to_Pointer is new
     Unchecked_Conversion (System.Address, sigjmp_buf_ptr);

   --  The following are P1003.5 interfaces.  I am not sure that this is a
   --  good idea, but these can't be exactly the same as the C functions
   --  in any case.

   procedure sigaddset
     (set : access Signal_Set;
      sig : in Signal;
      Result : out POSIX_Error.Return_Code)

   is
      function sigaddset_base
        (set : access Signal_Set;
         sig : Signal)
        return Return_Code;
      pragma Import (C, sigaddset_base, "sigaddset");

   begin
      Result := sigaddset_base (set, sig);
   end sigaddset;

   procedure sigdelset
     (set : access Signal_Set;
      sig : in Signal;
      Result : out POSIX_Error.Return_Code)
   is
      function sigdelset_base
        (set : access Signal_Set;
         sig : Signal)
        return Return_Code;
      pragma Import (C, sigdelset_base, "sigdelset");

   begin
      Result := sigdelset_base (set, sig);
   end sigdelset;

   procedure sigfillset
     (set : access Signal_Set;
      Result : out POSIX_Error.Return_Code)
   is
      function sigfillset_base
        (set : access Signal_Set)
        return Return_Code;
      pragma Import (C, sigfillset_base, "sigfillset");

   begin
      Result := sigfillset_base (set);
   end sigfillset;

   procedure sigemptyset
     (set : access Signal_Set;
      Result : out POSIX_Error.Return_Code)
   is
      function sigemptyset_base
        (set : access Signal_Set)
        return Return_Code;
      pragma Import (C, sigemptyset_base, "sigemptyset");

   begin
      Result := sigemptyset_base (set);
   end sigemptyset;

   ---------------
   -- sigaction --
   ---------------

   procedure sigaction
     (sig    : Signal;
      act    : access struct_sigaction;
      oact   : access struct_sigaction;
      Result : out POSIX_Error.Return_Code)
   is
      function sigaction_base
        (sig  : Signal;
         act  : access struct_sigaction;
         oact : access struct_sigaction) return POSIX_Error.Return_Code;
      pragma Import (C, sigaction_base, "sigaction");

   begin
      Result := sigaction_base (sig, act, oact);
   end sigaction;

   ---------------
   -- sigaction --
   ---------------

   procedure sigaction
     (sig    : Signal;
      oact   : access struct_sigaction;
      Result : out Return_Code) is

      function sigaction_base
        (sig  : Signal;
         act  : sigaction_ptr;
         oact : access struct_sigaction) return Return_Code;
      pragma Import (C, sigaction_base, "sigaction");

   begin
      Result := sigaction_base (sig, null, oact);
   end sigaction;

   -----------------
   -- sigprocmask --
   -----------------

   --  Install new signal mask and obtain old one

   procedure sigprocmask
     (how    : int;
      set    : access Signal_Set;
      oset   : access Signal_Set;
      Result : out POSIX_Error.Return_Code)
   is
      function sigprocmask_base
        (how  : int;
         set  : access Signal_Set;
         oset : access Signal_Set)
         return POSIX_Error.Return_Code;
      pragma Import (C, sigprocmask_base, "sigprocmask");

   begin
      Result := sigprocmask_base (how, set, oset);
   end sigprocmask;

   ----------------
   -- sigsuspend --
   ----------------

   --  Suspend waiting for signals in mask and resume after
   --  executing handler or take default action

   procedure sigsuspend
     (mask : access Signal_Set;
      Result : out POSIX_Error.Return_Code) is

      function sigsuspend_base
        (mask : access Signal_Set)
         return POSIX_Error.Return_Code;
      pragma Import (C, sigsuspend_base, "sigsuspend");

   begin
      Result := sigsuspend_base (mask);
   end sigsuspend;

   ----------------
   -- sigpending --
   ----------------

   --  Get pending signals on thread and process

   procedure sigpending
     (set    : access Signal_Set;
      Result : out POSIX_Error.Return_Code)
   is
      function sigpending_base
        (set  : access Signal_Set)
         return POSIX_Error.Return_Code;
      pragma Import (C, sigpending_base, "sigpending");

   begin
      Result := sigpending_base (set);
   end sigpending;

   -------------
   -- longjmp --
   -------------

   --  Execute a jump across procedures according to setjmp

   procedure longjmp (env : jmp_buf; val : int) is
      procedure longjmp_base (env : jmp_buf_ptr; val : int);
      pragma Import (C, longjmp_base, "longjmp");

   begin
      longjmp_base (Address_to_Pointer (env'Address), val);
   end longjmp;

   ----------------
   -- siglongjmp --
   ----------------

   --  Execute a jump across procedures according to sigsetjmp

   procedure siglongjmp (env : sigjmp_buf; val : int) is
      procedure siglongjmp_base (env : sigjmp_buf_ptr; val : int);
      pragma Import (C, siglongjmp_base, "siglongjmp");

   begin
      siglongjmp_base (Address_to_Pointer (env'Address), val);
   end siglongjmp;

   ------------
   -- setjmp --
   ------------

   --  Set up a jump across procedures and return here with longjmp

   procedure setjmp (env : jmp_buf; Result : out Return_Code) is
      function setjmp_base (env : jmp_buf_ptr) return Return_Code;
      pragma Import (C, setjmp_base, "setjmp");

   begin
      Result := setjmp_base (Address_to_Pointer (env'Address));
   end setjmp;

   ---------------
   -- sigsetjmp --
   ---------------

   --  Set up a jump across procedures and return here with siglongjmp

   procedure sigsetjmp
     (env      : sigjmp_buf;
      savemask : int;
      Result   : out Return_Code)
   is
      function sigsetjmp_base
        (env      : sigjmp_buf_ptr;
         savemask : int)
         return     Return_Code;
      pragma Import (C, sigsetjmp_base, "sigsetjmp");

   begin
      Result := sigsetjmp_base (Address_to_Pointer (env'Address), savemask);
   end sigsetjmp;

begin
   for i in OS_Specific_Sync_Signals'Range loop
      OS_Specific_Sync_Signals (i) :=
        Signal (System_Constants.OS_Specific_Sync_Sigs (i));
   end loop;

   for i in OS_Specific_Async_Signals'Range loop
      OS_Specific_Async_Signals (i) :=
        Signal (System_Constants.OS_Specific_Async_Sigs (i));
   end loop;
end Interfaces.C.POSIX_RTE;

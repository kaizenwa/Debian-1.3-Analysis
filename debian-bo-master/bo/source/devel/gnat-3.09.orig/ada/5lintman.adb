------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                         (Version for new GNARL)                          --
--                                                                          --
--                             $Revision: 1.3 $                            --
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

--  This is the Linux/MIT THREADS version of this package

--  This is only a first approximation.
--  I hope Sean will pick this up and make it work. --Ted Baker

--  This file performs the system-dependent translation between machine
--  exceptions and the Ada exceptions, if any, that should be raised when
--  they occur.  This version works for the i486 running linux.

--  PLEASE DO NOT add any dependences on other packages.
--  This package is designed to work with or without tasking support.

--  See the other warnings in the package specification before making
--  any modifications to this file.

--  Make a careful study of all signals available under the OS,
--  to see which need to be reserved, kept always unmasked,
--  or kept always unmasked.
--  Be on the lookout for special signals that
--  may be used by the thread library.

with Interfaces.C;
--  used for int and other types

with System.Error_Reporting;
--  used for Shutdown

with System.OS_Interface;
--  used for various Constants, Signal and types

package body System.Interrupt_Management is

   use Interfaces.C;
   use System.Error_Reporting;
   use System.OS_Interface;

   type Interrupt_List is array (Interrupt_ID range <>) of Interrupt_ID;
   Exception_Interrupts : constant Interrupt_List :=
     (SIGFPE, SIGILL, SIGSEGV);

   --  ?????
   --  The following type declaration is retained from an older
   --  Linux version, where it was apparently not used.
   --  The Linux man-pages and header-files don't document any
   --  context-parameter for signal handlers.  The structure below
   --  does appear in asm/signal.h for kernel code.  Looking at
   --  the kernel sources (arch/i386/kernel/signal.c), I see where
   --  optionally these values are pushed on the stack for a signal
   --  handler frame, but only if current->execution_domain is null.
   --  So far as I can tell, the normal value of this is non-null, so
   --  only the signal number is pushed.  In short, I don't think we
   --  will be able to use this "sigcontext" type, but I am not
   --  certain enough to rule it out.  I hope maybe somebody else
   --  will put in enough time to determine this with certainty.
   --  --Ted Baker

      type sigcontext is record
         gs            : Interfaces.C.unsigned_short;
         fs            : Interfaces.C.unsigned_short;
         es            : Interfaces.C.unsigned_short;
         ds            : Interfaces.C.unsigned_short;
         edi           : Interfaces.C.unsigned_long;
         esi           : Interfaces.C.unsigned_long;
         ebp           : Interfaces.C.unsigned_long;
         esp           : Interfaces.C.unsigned_long;
         ebx           : Interfaces.C.unsigned_long;
         edx           : Interfaces.C.unsigned_long;
         ecx           : Interfaces.C.unsigned_long;
         eax           : Interfaces.C.unsigned_long;
         trapno        : Interfaces.C.unsigned_long;
         err           : Interfaces.C.unsigned_long;
         eip           : Interfaces.C.unsigned_long;
         cs            : Interfaces.C.unsigned_short;
         eflags        : Interfaces.C.unsigned_long;
         esp_at_signal : Interfaces.C.unsigned_long;
         ss            : Interfaces.C.unsigned_short;
         i387          : Interfaces.C.unsigned_long;
         oldmask       : Interfaces.C.unsigned_long;
         cr2           : Interfaces.C.unsigned_long;
      end record;

   ----------------------
   -- Notify_Exception --
   ----------------------

   --  This function identifies the Ada exception to be raised using
   --  the information when the system received a synchronous signal.
   --  Since this function is machine and OS dependent, different code
   --  has to be provided for different target.

   procedure Notify_Exception
    (signo   : Signal);

   procedure Notify_Exception
     (signo   : Signal) is
   begin

      --  As long as we are using a longjmp to return control to the
      --  exception handler on the runtime stack, we are safe. The original
      --  signal mask (the one we had before coming into this signal catching
      --  function) will be restored by the longjmp. Therefore, raising
      --  an exception in this handler should be a safe operation.

      --  Check that treatment of exception propagation here
      --  is consistent with treatment of the abort signal in
      --  System.Task_Primitives.Operations.

      --  ?????
      --  The code below is first approximation.
      --  It would be nice to figure out more
      --  precisely what exception has occurred.
      --  One also should arrange to use an alternate stack for
      --  recovery from stack overflow.
      --  I don't understand the Linux kernel code well
      --  enough to figure out how to do this yet.
      --  I hope someone will look at this.  --Ted Baker

      case signo is
         when SIGFPE =>
            raise Constraint_Error;
         when SIGILL =>
            raise Constraint_Error;
         when SIGSEGV =>
            raise Storage_Error;
         when others =>
            pragma Assert (Shutdown ("Unexpected signal"));
            null;
      end case;
   end Notify_Exception;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      act     : aliased struct_sigaction;
      old_act : aliased struct_sigaction;
      mask    : aliased sigset_t;
      Result  : Interfaces.C.int;

   begin

      Abort_Task_Interrupt := SIGABRT;
      --  Change this if you want to use another signal for task abort.
      --  SIGTERM might be a good one.

      act.sa_handler := Notify_Exception'Address;

      act.sa_flags := 16#40000000#;
      --  Set sa_flags to SA_NODEFER so that during the handler execution
      --  we do not change the Signal_Mask to be masked for the Signal.
      --  This is a temporary fix to the problem that the Signal_Mask is
      --  not restored after the exception (longjmp) from the handler.
      --  The right fix should be made in sigsetjmp so that we save
      --  the Signal_Set and restore it after a longjmp.
      --  In that case, this field should be changed back to 0. ??? (Dong-Ik)

      Result := sigemptyset (mask'Access);
      pragma Assert (Result = 0
        or else Shutdown ("GNULLI failure---sigemptyset"));

      for I in Exception_Interrupts'Range loop
         Result := sigaddset (mask'Access, Signal (Exception_Interrupts (I)));
         pragma Assert (Result = 0
           or else Shutdown ("GNULLI failure---sigaddset"));
      end loop;

      act.sa_mask := mask;

      for I in Exception_Interrupts'Range loop
         Keep_Unmasked (Exception_Interrupts (I)) := True;
         Result :=
           sigaction
             (Signal (Exception_Interrupts (I)), act'Access, old_act'Access);
         pragma Assert (Result = 0
           or else Shutdown ("GNULLI failure---sigaction"));
      end loop;

      Keep_Unmasked (Abort_Task_Interrupt) := true;

      --  Linux for i386 does not actually generate SIGBUS
      Keep_Unmasked (SIGBUS)  := true;

      Keep_Unmasked (SIGSTOP) := true;
      Keep_Unmasked (SIGKILL) := true;
      Keep_Unmasked (SIGINT)  := true;

      --  Keep_Unmasked (SIGEMT) := true;
      --  Keep_Unmasked (SIGCHLD) := true;
      --  Keep_Unmasked (SIGALRM) := true;
      --  ???? The above signals have been found to need to be
      --  kept unmasked on some systems, per Dong-Ik Oh.
      --  I don't know whether the MIT/Provenzano threads
      --  need these or any other signals unmasked at the thread level.
      --  I hope somebody will take
      --  the time to look it up. -- Ted Baker

      Reserve (SIGALRM) := true;
      Reserve (SIGVTALRM) := true;
      Reserve (SIGUNUSED) := true;

      Reserve := Reserve or Keep_Unmasked or Keep_Masked;

      Reserve (0) := true;
      --  We do not have Signal 0 in reality. We just use this value
      --  to identify non-existent signals (see s-intnam.ads). Therefore,
      --  Signal 0 should not be used in all signal related operations hence
      --  mark it as reserved.

   end Initialize;

begin
   Initialize;
end System.Interrupt_Management;

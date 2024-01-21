------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
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

--  This is a OpenVMS/Alpha version of this package.

--  It is likely to need tailoring to fit each operating system
--  and machine architecture.

--  PLEASE DO NOT add any dependences on other packages.
--  This package is designed to work with or without tasking support.

--  See the other warnings in the package specification before making
--  any modifications to this file.

--  Make a careful study of all signals available under the OS,
--  to see which need to be reserved, kept always unmasked,
--  or kept always unmasked.
--  Be on the lookout for special signals that
--  may be used by the thread library.

--  This version is intended for the SPARC architecture under SunOS (POSIX).

with Ada.Exceptions;
--  used for Raise_Exception
--           Exception_ID

with Interfaces.C;
--  used for int

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

   ----------------------
   -- Notify_Exception --
   ----------------------

   --  This function identifies the Ada exception to be raised using
   --  the information when the system received a synchronous signal.
   --  Since this function is machine and OS dependent, different code
   --  has to be provided for different target.

   type args_array is array (Natural range <>) of Interfaces.C.int;

   procedure Notify_Exception
     (sigargs  : args_array;
      mechargs : args_array);

   procedure Notify_Exception
     (sigargs  : args_array;
      mechargs : args_array)
   is

      Current_Exception : Ada.Exceptions.Exception_Id;

      --  As long as we are using a longjmp to return control to the
      --  exception handler on the runtime stack, we are safe. The original
      --  signal mask (the one we had before coming into this signal catching
      --  function) will be restored by the longjmp. Therefore, raising
      --  an exception in this handler should be a safe operation.

      --  Check that treatment of exception propagation here
      --  is consistent with treatment of the abort signal in
      --  System.Task_Primitives.Operations.

   begin

      --  As long as we are using a longjmp to return control to the
      --  exception handler on the runtime stack, we are safe. The original
      --  signal mask (the one we had before coming into this signal catching
      --  function) will be restored by the longjmp. Therefore, raising
      --  an exception in this handler should be a safe operation.

      --  Check that treatment of exception propagation here
      --  is consistent with treatment of the abort signal in
      --  System.Task_Primitives.Operations.

      case sigargs (1) is
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

      act.sa_flags := 16;
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
      Keep_Unmasked (SIGBUS)  := true;

      Keep_Unmasked (SIGALRM) := true;
      Keep_Unmasked (SIGSTOP) := true;
      Keep_Unmasked (SIGKILL) := true;

      --  Reserve this not to interfere with thread scheduling

      --  ??? consider adding this to interrupt exceptions
      --  Keep_Unmasked (SIGALRM) := true;
      --  An earlier version had a comment about SIGALRM needing to be unmasked
      --  in at least one thread for cond_timedwait to work.
      --  It is unclear whether this is true for Solaris threads, FSU threads,
      --  both, or maybe just an old version of FSU threads. ????

      Keep_Unmasked (SIGEMT) := true;
      Keep_Unmasked (SIGCHLD) := true;
      --  Above signals should not be disturbed. Found through experiments.


      Reserve := Reserve or Keep_Unmasked or Keep_Masked;
      Reserve (0) := true;
      --  We do not have Signal 0 in reality. We just use this value
      --  to identify not existing signals (see s-intnam.ads). Therefore,
      --  Signal 0 should not be used in all signal related operations hence
      --  mark it as reserved.
   end Initialize;

begin
   Initialize;
end System.Interrupt_Management;

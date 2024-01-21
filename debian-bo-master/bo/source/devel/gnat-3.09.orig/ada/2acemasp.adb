------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
-- C O M P I L E R _ E X C E P T I O N S . M A C H I N E _ S P E C I F I C S--
--                                                                          --
--                                  B o d y                                 --
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

--  This is the DEC Unix version of this package.

--  This file performs the system-dependent translation between machine
--  exceptions and the Ada exceptions, if any, that should be raised when
--  they occur.  This version works for the i486 running linux.

--  ??? This should not be part of tasking, since it is needed whether tasking
--      is used or not.  This file will eventually go away or be incorporated
--      into the non-tasking runtime.

with Interfaces.C; use Interfaces.C;

with Interfaces.C.POSIX_RTE;

package body System.Compiler_Exceptions.Machine_Specifics is

   package RTE renames Interfaces.C.POSIX_RTE;

   ------------------------
   -- Identify_Exception --
   ------------------------

   --  This function identifies the Ada exception to be raised using
   --  the information when the system received a synchronous signal.
   --  Since this function is machine and OS dependent, different code
   --  has to be provided for different target.

   --  Following code is intended for DEC Alpha

   function Identify_Exception
     (Which              : System.Task_Primitives.Machine_Exceptions;
      Info               : System.Task_Primitives.Error_Information;
      Modified_Registers : Pre_Call_State) return Ada.Exceptions.Exception_Id
   is

      type regs_array is array (int range 1 .. 32) of long;

      type sigcontext is record
        sc_onstack    : long;
        sc_mask       : long;
        sc_pc         : long;
        sc_ps         : long;
        sc_regs       : regs_array;
        sc_ownedfp    : long;
        sc_fpregs     : regs_array;
        sc_fpcr       : unsigned_long;
        sc_fp_control : unsigned_long;
      end record;

      type sigcontext_ptr is access sigcontext;

      --  The above operations will be available as predefined operations on
      --  the modula Address type in GNARL, since this package is a child of
      --  System.

      FPE_INTOVF_TRAP         : constant int := 16#1#;
      FPE_INTDIV_TRAP         : constant int := 16#2#;
      FPE_FLTOVF_TRAP         : constant int := 16#3#;
      FPE_FLTDIV_TRAP         : constant int := 16#4#;
      FPE_FLTUND_TRAP         : constant int := 16#5#;
      FPE_DECOVF_TRAP         : constant int := 16#6#;
      FPE_SUBRNG_TRAP         : constant int := 16#7#;
      FPE_FLTOVF_FAULT        : constant int := 16#8#;
      FPE_FLTDIV_FAULT        : constant int := 16#9#;
      FPE_FLTUND_FAULT        : constant int := 16#a#;
      FPE_UNIMP_FAULT         : constant int := 16#b#;
      FPE_INVALID_FAULT       : constant int := 16#c#;
      FPE_INEXACT_FAULT       : constant int := 16#d#;
      FPE_HPARITH_TRAP        : constant int := 16#e#;
      FPE_INTOVF_FAULT        : constant int := 16#f#;
      FPE_ILLEGAL_SHADOW_TRAP : constant int := 16#10#;
      FPE_GENTRAP             : constant int := 16#11#;

      function Pre_Call_To_Context is new
        Unchecked_Conversion (Pre_Call_State, sigcontext_ptr);


      Current_Exception : Ada.Exceptions.Exception_Id;

      context : sigcontext_ptr :=
                  Pre_Call_To_Context (Modified_Registers);

      sig     : RTE.Signal := RTE.Signal (Which);

   begin

      --  As long as we are using a longjmp to return control to the
      --  exception handler on the runtime stack, we are safe. The original
      --  signal mask (the one we had before coming into this signal catching
      --  function) will be restored by the longjmp. Therefore, raising
      --  an exception in this handler should be a safe operation.

      case sig is

         when RTE.SIGFPE =>

            Current_Exception := Constraint_Error_Id;

         when RTE.SIGILL =>

            case Info.si_code is

               when others =>

                  pragma Assert (false, "Unexpected SIGILL signal");
                  null;
            end case;

         when RTE.SIGSEGV =>

            Current_Exception := Storage_Error_Id;

         --  If the address that caused the error was in the first page, this
         --  was caused by accessing a null pointer.

         when others =>

            pragma Assert (false, "Unexpected signal");
            null;
      end case;

      return Current_Exception;

   end Identify_Exception;

end System.Compiler_Exceptions.Machine_Specifics;

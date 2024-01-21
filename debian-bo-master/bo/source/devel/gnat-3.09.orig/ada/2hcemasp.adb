------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                SYSTEM.COMPILER_EXCEPTIONS.MACHINE_SPECIFICS              --
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

--  This file performs the system-dependent translation between machine
--  exceptions and the Ada exceptions, if any, that should be raised when
--  when they occur. This version works for HPUX 10.x.

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

   --  Following code is intended for HP_UX 9.05

   function Identify_Exception
     (Which              : System.Task_Primitives.Machine_Exceptions;
      Info               : System.Task_Primitives.Error_Information;
      Modified_Registers : Pre_Call_State) return Ada.Exceptions.Exception_Id
   is

      --  from /usr/include/machine/frame.h

      type frame_marker is record
         fm_edp  : int;                -- External DP
         fm_esr4 : int;                -- External SR4
         fm_erp  : int;                -- External/Stup RP (RP')
         fm_crp  : int;                -- Current RP
         fm_sl   : int;                -- Static Link
         fm_clup : int;                -- Clean Up
         fm_ep   : int;                -- Extension Pointer
         fm_psp  : int;                -- Previous SP
      end record;

      NUMARGREGS : constant := 4;      -- number of arguments is registers

      --  from /usr/include/machine/save_state.h

      type fp_int_block is record
         ss_fpstat    : int;        -- Must be double word aligned
         ss_fpexcept1 : int;
         ss_fpexcept2 : int;
         ss_fpexcept3 : int;
         ss_fpexcept4 : int;
         ss_fpexcept5 : int;
         ss_fpexcept6 : int;
         ss_fpexcept7 : int;
         ss_fp4_hi    : int;
         ss_fp4_lo    : int;
         ss_fp5_hi    : int;
         ss_fp5_lo    : int;
         ss_fp6_hi    : int;
         ss_fp6_lo    : int;
         ss_fp7_hi    : int;
         ss_fp7_lo    : int;
         ss_fp8_hi    : int;
         ss_fp8_lo    : int;
         ss_fp9_hi    : int;
         ss_fp9_lo    : int;
         ss_fp10_hi   : int;
         ss_fp10_lo   : int;
         ss_fp11_hi   : int;
         ss_fp11_lo   : int;
         ss_fp12_hi   : int;
         ss_fp12_lo   : int;
         ss_fp13_hi   : int;
         ss_fp13_lo   : int;
         ss_fp14_hi   : int;
         ss_fp14_lo   : int;
         ss_fp15_hi   : int;
         ss_fp15_lo   : int;
         ss_fp16_hi   : int;
         ss_fp16_lo   : int;
         ss_fp17_hi   : int;
         ss_fp17_lo   : int;
         ss_fp18_hi   : int;
         ss_fp18_lo   : int;
         ss_fp19_hi   : int;
         ss_fp19_lo   : int;
         ss_fp20_hi   : int;
         ss_fp20_lo   : int;
         ss_fp21_hi   : int;
         ss_fp21_lo   : int;
         ss_fp22_hi   : int;
         ss_fp22_lo   : int;
         ss_fp23_hi   : int;
         ss_fp23_lo   : int;
         ss_fp24_hi   : int;
         ss_fp24_lo   : int;
         ss_fp25_hi   : int;
         ss_fp25_lo   : int;
         ss_fp26_hi   : int;
         ss_fp26_lo   : int;
         ss_fp27_hi   : int;
         ss_fp27_lo   : int;
         ss_fp28_hi   : int;
         ss_fp28_lo   : int;
         ss_fp29_hi   : int;
         ss_fp29_lo   : int;
         ss_fp30_hi   : int;
         ss_fp30_lo   : int;
         ss_fp31_hi   : int;
         ss_fp31_lo   : int;
      end record;

      type save_state is record
         ss_flags       : int;     -- Save State Flags
         ss_gr1         : int;     -- General Registers
         ss_rp          : int;
         ss_gr3         : int;
         ss_gr4         : int;
         ss_gr5         : int;
         ss_gr6         : int;
         ss_gr7         : int;
         ss_gr8         : int;
         ss_gr9         : int;
         ss_gr10        : int;
         ss_gr11        : int;
         ss_gr12        : int;
         ss_gr13        : int;
         ss_gr14        : int;
         ss_gr15        : int;
         ss_gr16        : int;
         ss_gr17        : int;
         ss_gr18        : int;
         ss_gr19        : int;
         ss_gr20        : int;
         ss_gr21        : int;
         ss_gr22        : int;
         ss_arg3        : int;
         ss_arg2        : int;
         ss_arg1        : int;
         ss_arg0        : int;     -- Following on interrupt stack
         ss_dp          : int;
         ss_ret0        : int;
         ss_ret1        : int;
         ss_sp          : int;
         ss_gr31        : int;
         ss_cr11        : int;     -- Control Registers
         ss_pcoq_head   : int;
         ss_pcsq_head   : int;
         ss_pcoq_tail   : int;
         ss_pcsq_tail   : int;
         ss_cr15        : int;
         ss_cr19        : int;     -- For break and assist traps only
         ss_cr20        : int;     -- For data page fault only
         ss_cr21        : int;
         ss_cr22        : int;
         ss_cpustate    : int;     -- Local for thandler
         ss_sr4         : int;     -- Previous sr4 value
         ss_sr0         : int;
         ss_sr1         : int;
         ss_sr2         : int;
         ss_sr3         : int;
         ss_sr5         : int;
         ss_sr6         : int;
         ss_sr7         : int;
         ss_cr0         : int;
         ss_cr8         : int;
         ss_cr9         : int;
         ss_cr10        : int;
         ss_cr12        : int;
         ss_cr13        : int;
         ss_cr24        : int;
         ss_cr25        : int;
         ss_cr26        : int;
         ss_mpsfu_high  : int;
         ss_mpsfu_low   : int;
         ss_mpsfu_ovflo : int;
         ss_pad         : int;     -- For padding to double word boundary
         fpint          : fp_int_block;  --  Must be double word aligned
         ss_cr16        : int;
         ss_cr23        : int;
      end record;

      --  from /usr/include/sys/signal.h

      type sl_arg_array is array (0 .. NUMARGREGS - 1) of int;

      type siglocal is record          -- /usr/include/sys/signal.h
         sl_syscall        : int;      -- interrupted system call if any
         sl_onstack        : int;      -- sigstack state to restore
         sl_mask           : int;      -- signal mask to restore
         sl_syscall_action : char;     -- what to do after sys call
         sl_eosys          : char;
         sl_error          : short;
         sl_rval1          : int;
         sl_rval2          : int;
         sl_arg            : sl_arg_array;
         sl_ss             : save_state;  -- user saved state;
      end record;

      type sigcontext is record
         sc_sl   : siglocal;            -- local frame containing context
         sc_args : sl_arg_array;        -- arguments to handler
         sc_sfm  : frame_marker;
      end record;

      type sigcontext_ptr is access sigcontext;

      --  The above operations will be available as predefined operations on
      --  the modula Address type in GNARL, since this package is a child of
      --  System.

--      FPE_INTOVF_TRAP  : constant System.Task_Primitives.Error_Information :=
--         1;  -- integer overflow
--      FPE_INTDIV_TRAP  : constant System.Task_Primitives.Error_Information :=
--         2;  -- integer divide by zero
--      FPE_FLTOVF_TRAP  : constant System.Task_Primitives.Error_Information :=
--         3;  -- floating overflow
--      FPE_FLTDIV_TRAP  : constant System.Task_Primitives.Error_Information :=
--         4;  -- floating/decimal divide by zero
--      FPE_FLTUND_TRAP  : constant System.Task_Primitives.Error_Information :=
--         5;  -- floating underflow
--      FPE_DECOVF_TRAP  : constant System.Task_Primitives.Error_Information :=
--         6;  -- decimal overflow
--      FPE_SUBRNG_TRAP  : constant System.Task_Primitives.Error_Information :=
--         7;  -- subscript out of range
--      FPE_FLTOVF_FAULT : constant System.Task_Primitives.Error_Information :=
--         8;  -- floating overflow fault
--      FPE_FLTDIV_FAULT : constant System.Task_Primitives.Error_Information :=
--         9;  -- divide by zero floating fault
--      FPE_FLTUND_FAULT : constant System.Task_Primitives.Error_Information :=
--         10;  -- floating underflow fault


      ILL_ILL_INS_TRAP : constant System.Task_primitives.Error_Information :=
         8;
      ILL_BRK_INS_TRAP : constant System.Task_primitives.Error_Information :=
         9;
      ILL_PRV_OPR_TRAP : constant System.Task_primitives.Error_Information :=
         10;
      ILL_PRV_REG_TRAP : constant System.Task_primitives.Error_Information :=
         11;
      FPE_OVR_TRAP     : constant System.Task_primitives.Error_Information :=
         12;
      FPE_CON_TRAP     : constant System.Task_primitives.Error_Information :=
         13;
      FPE_ASS_EXC_TRAP : constant System.Task_primitives.Error_Information :=
         14;
      FPE_ASS_EMU_TRAP : constant System.Task_primitives.Error_Information :=
         22;

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

            --  eas case Info.si_code is
            case Info is

               when FPE_OVR_TRAP | FPE_CON_TRAP   =>

                  Current_Exception := Constraint_Error_Id;

               when others =>
                  pragma Assert (False, "Unexpected SIGFPE signal");
                  null;

            end case;

         when RTE.SIGILL =>

            --  eas case Info.si_code is
            case Info is

               when ILL_ILL_INS_TRAP =>
                  Current_Exception := Constraint_Error_Id;

               when others =>

                  pragma Assert (false, "Unexpected SIGILL signal");
                  null;
            end case;

         when RTE.SIGSEGV =>

         --  If the address that caused the error was in the first page, this
         --  was caused by accessing a null pointer.

            --  if context.sc_o0 >= 0 and context.sc_o0 < 16#2000# then
            --   Current_Exception := Constraint_Error_Id;

            --  else
            Current_Exception := Storage_Error_Id;
            --  end if;

         when others =>

            pragma Assert (false, "Unexpected signal");
            null;
      end case;

      return Current_Exception;

   end Identify_Exception;

end System.Compiler_Exceptions.Machine_Specifics;

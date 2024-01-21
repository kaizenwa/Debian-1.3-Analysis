------------------------------------------------------------------------------
--                                                                      --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS           --
--                                                                      --
--                   A D A . I N T E R R U P T S . N A M E S            --
--                                                                      --
--                                  S p e c                             --
--                                                                      --
--                             $Revision: 1.2 $                        --
--                                                                      --
--      Copyright (C) 1991,1992,1993,1994,1995 Florida State University     --
--                                                                      --
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

with System;

--  This implementation-define package spec contains interrups
--  supported by Irix 5.3

package Ada.Interrupts.Names is

   SIGHUP   : constant Interrupt_ID := 1;  --  hangup
   SIGINT   : constant Interrupt_ID := 2;  --  interrupt (rubout)
   SIGQUIT  : constant Interrupt_ID := 3;  --  quit (ASCII FS)
   SIGILL   : constant Interrupt_ID := 4;  --  illegal instruction (1)
   SIGTRAP  : constant Interrupt_ID := 5;  --  trace trap (1)
   SIGIOT   : constant Interrupt_ID := 6;  --  IOT instruction
   SIGABRT  : constant Interrupt_ID := 6;  --  replace SIGIOT in the  future
   SIGEMT   : constant Interrupt_ID := 7;  --  EMT instruction
   SIGFPE   : constant Interrupt_ID := 8;  --  floating point exception
   SIGKILL  : constant Interrupt_ID := 9;  --  kill (can't be caught/ignored)
   SIGBUS   : constant Interrupt_ID := 10; --  bus error
   SIGSEGV  : constant Interrupt_ID := 11; --  segmentation violation
   SIGSYS   : constant Interrupt_ID := 12; --  bad argument to system call
   SIGPIPE  : constant Interrupt_ID := 13; --  write on a pipe with no one
   --                                          to read it
   SIGALRM  : constant Interrupt_ID := 14; --  alarm clock
   SIGTERM  : constant Interrupt_ID := 15; --  software termination signal from
   --                                          kill
   SIGUSR1  : constant Interrupt_ID := 16; --  user defined signal 1
   SIGUSR2  : constant Interrupt_ID := 17; --  user defined signal 2
   SIGCLD   : constant Interrupt_ID := 18; --  death of a child
   SIGCHLD  : constant Interrupt_ID := 18; --  4.3BSD's/POSIX name
   SIGPWR   : constant Interrupt_ID := 19; --  power-fail restart
   SIGWINCH : constant Interrupt_ID := 20; --  window size changes
   SIGURG   : constant Interrupt_ID := 21; --  urgent condition on IO channel
   SIGPOLL  : constant Interrupt_ID := 22; --  pollable event occurred
   SIGIO    : constant Interrupt_ID := 22; --  input/output possible signal
   SIGSTOP  : constant Interrupt_ID := 23; --  stop signal not from tty
   SIGTSTP  : constant Interrupt_ID := 24; --  stop signal from tty
   SIGCONT  : constant Interrupt_ID := 25; --  continue a stopped process
   SIGTTIN  : constant Interrupt_ID := 26; --  to readers pgrp upon background
   --                                          tty read
   SIGTTOU  : constant Interrupt_ID := 27; --  like TTIN for output if
   --                                          (tp->t_local&LTOSTOP)
   SIGVTALRM : constant Interrupt_ID := 28; --  virtual time alarm
   SIGPROF  : constant Interrupt_ID := 29; --  profiling alarm
   SIGXCPU  : constant Interrupt_ID := 30; --  Cpu time limit exceeded
   SIGXFSZ  : constant Interrupt_ID := 31; --  Filesize limit exceeded
   SIG32    : constant Interrupt_ID := 32; --  Reserved for kernel usage

   --  (1) Not reset when caught

end Ada.Interrupts.Names;

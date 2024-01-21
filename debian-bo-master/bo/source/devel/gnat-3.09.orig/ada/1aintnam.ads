------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.1 $                             --
--                                                                          --
--   Copyright (C) 1991,1992,1993,1994,1995,1996 Florida State University   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
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

--  This implementation-define package spec contains interrupts
--  supported by DEC Unix

package Ada.Interrupts.Names is

   SIGHUP    : constant Interrupt_ID := 1;
   SIGINT    : constant Interrupt_ID := 2;
   SIGQUIT   : constant Interrupt_ID := 3;
   SIGILL    : constant Interrupt_ID := 4;
   SIGTRAP   : constant Interrupt_ID := 5;
   SIGABRT   : constant Interrupt_ID := 6;
   SIGEMT    : constant Interrupt_ID := 7;
   SIGFPE    : constant Interrupt_ID := 8;
   SIGKILL   : constant Interrupt_ID := 9;
   SIGBUS    : constant Interrupt_ID := 10;
   SIGSEGV   : constant Interrupt_ID := 11;
   SIGSYS    : constant Interrupt_ID := 12;
   SIGPIPE   : constant Interrupt_ID := 13;
   SIGALRM   : constant Interrupt_ID := 14;
   SIGTERM   : constant Interrupt_ID := 15;
   SIGURG    : constant Interrupt_ID := 16;
   SIGSTOP   : constant Interrupt_ID := 17;
   SIGTSTP   : constant Interrupt_ID := 18;
   SIGCONT   : constant Interrupt_ID := 19;
   SIGCHLD   : constant Interrupt_ID := 20;
   SIGTTIN   : constant Interrupt_ID := 21;
   SIGTTOU   : constant Interrupt_ID := 22;
   SIGIO     : constant Interrupt_ID := 23;
   SIGXCPU   : constant Interrupt_ID := 24;
   SIGXFSZ   : constant Interrupt_ID := 25;
   SIGVTALRM : constant Interrupt_ID := 26;
   SIGPROF   : constant Interrupt_ID := 27;
   SIGWINCH  : constant Interrupt_ID := 28;
   SIGINFO   : constant Interrupt_ID := 29;
   SIGUSR1   : constant Interrupt_ID := 30;
   SIGUSR2   : constant Interrupt_ID := 31;

end Ada.Interrupts.Names;

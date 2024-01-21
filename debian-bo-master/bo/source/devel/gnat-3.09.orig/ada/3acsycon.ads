-----------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--         I N T E R F A C E S . C . S Y S T E M _ C O N S T A N T S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
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

package Interfaces.C.System_Constants is

   jmp_buf_size : constant Integer := 672;
   sigjmp_buf_size : constant Integer := 672;
   sigset_t_size : constant Integer := 4;   --  This is in bytes.

   --  The sizes of structures  (in bytes) and offsets of their components
   --  (in bits).  These are used to lay out the equivalent Ada records.
   siginfo_size : constant Integer := 160;
   si_signo_offset : constant Integer := 0;
   si_errno_offset : constant Integer := 32;
   si_code_offset : constant Integer := 64;
   sigaction_size : constant Integer := 20;
   sa_handler_offset : constant Integer := 0;
   sa_mask_offset : constant Integer := 64;
   sa_flags_offset : constant Integer := 96;
   timespec_size : constant Integer := 16;
   tv_sec_offset : constant Integer := 0;
   tv_nsec_offset : constant Integer := 64;

   SIG_BLOCK : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;
   SA_NOCLDSTOP : constant := 4;
   SA_SIGINFO : constant := 64;
   SIG_ERR : constant := -1;
   SIG_DFL : constant := 0;
   SIG_IGN : constant := 1;
   SIGNULL : constant := 0;
   SIGHUP  : constant := 1;
   SIGINT  : constant := 2;
   SIGQUIT : constant := 3;
   SIGILL  : constant := 4;
   SIGABRT : constant := 6;
   SIGFPE  : constant := 8;
   SIGKILL : constant := 9;
   SIGSEGV : constant := 11;
   SIGPIPE : constant := 13;
   SIGALRM : constant := 14;
   SIGTERM : constant := 15;
   SIGSTOP : constant := 17;
   SIGTSTP : constant := 18;
   SIGCONT : constant := 19;
   SIGCHLD : constant := 20;
   SIGTTIN : constant := 21;
   SIGTTOU : constant := 22;
   SIGUSR1 : constant := 30;
   SIGUSR2 : constant := 31;
   NSIG    : constant := 32;
   --  OS specific signals represented as an array
   type Sig_Array is array (positive range <>) of integer;
   OS_Specific_Sync_Sigs : Sig_Array :=
     (NSIG, 5, 7, 10);
   OS_Specific_Async_Sigs : Sig_Array :=
     (NSIG, 12, 16, 23, 24, 25, 26, 27, 28, 6);
   --  End of OS specific signals representation
   E2BIG           : constant := 7;
   EACCES          : constant := 13;
   EAGAIN          : constant := 35;
   EBADF           : constant := 9;
   EBUSY           : constant := 16;
   ECHILD          : constant := 10;
   EDEADLK         : constant := 11;
   EDOM            : constant := 33;
   EEXIST          : constant := 17;
   EFAULT          : constant := 14;
   EFBIG           : constant := 27;
   EINTR           : constant := 4;
   EINVAL          : constant := 22;
   EIO             : constant := 5;
   EISDIR          : constant := 21;
   EMFILE          : constant := 24;
   EMLINK          : constant := 31;
   ENAMETOOLONG    : constant := 63;
   ENFILE          : constant := 23;
   ENODEV          : constant := 19;
   ENOENT          : constant := 2;
   ENOEXEC         : constant := 8;
   ENOLCK          : constant := 77;
   ENOMEM          : constant := 12;
   ENOSPC          : constant := 28;
   ENOSYS          : constant := 78;
   ENOTBLK         : constant := 15;
   ENOTDIR         : constant := 20;
   ENOTEMPTY       : constant := 66;
   ENOTSUP         : constant := 99;
   ENOTTY          : constant := 25;
   ENXIO           : constant := 6;
   EPERM           : constant := 1;
   EPIPE           : constant := 32;
   ERANGE          : constant := 34;
   EROFS           : constant := 30;
   ESPIPE          : constant := 29;
   ESRCH           : constant := 3;
   ETIMEDOUT       : constant := 60;
   ETXTBSY         : constant := 26;
   EXDEV           : constant := 18;
   NO_PRIO_INHERIT : constant := 0;
   PRIO_INHERIT    : constant := 0;
   PRIO_PROTECT    : constant := 0;
   Add_Prio        : constant Integer := 2;

end Interfaces.C.System_Constants;

------------------------------------------------------------------------------
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

   pthread_t_size : constant Integer := 4;
   pthread_attr_t_size : constant Integer := 52;
   pthread_mutexattr_t_size : constant Integer := 12;
   pthread_mutex_t_size : constant Integer := 32;
   pthread_condattr_t_size : constant Integer := 4;
   pthread_cond_t_size : constant Integer := 20;
   pthread_key_t_size : constant Integer := 4;
   jmp_buf_size : constant Integer := 36;
   sigjmp_buf_size : constant Integer := 40;
   sigset_t_size : constant Integer := 4;

   --  The sizes of structures  (in bytes) and offsets of their components
   --  (in bits).  These are used to lay out the equivalent Ada records.
   siginfo_size : constant Integer := 12;
   si_signo_offset : constant Integer := 0;
   si_code_offset : constant Integer := 32;
   sigaction_size : constant Integer := 12;
   sa_handler_offset : constant Integer := 0;
   sa_mask_offset : constant Integer := 32;
   sa_flags_offset : constant Integer := 64;
   timespec_size : constant Integer := 8;
   tv_sec_offset : constant Integer := 0;
   tv_nsec_offset : constant Integer := 32;

   SIG_BLOCK : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 4;
   SA_NOCLDSTOP : constant := 8;
   SA_SIGINFO : constant := 0;
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
     (NSIG, 12, 16, 23, 24, 25, 26, 27, 28, 29);
   --  End of OS specific signals representation
   EPERM    : constant := 1;
   ENOENT   : constant := 2;
   ESRCH    : constant := 3;
   EINTR    : constant := 4;
   EIO      : constant := 5;
   ENXIO    : constant := 6;
   E2BIG    : constant := 7;
   ENOEXEC  : constant := 8;
   EBADF    : constant := 9;
   ECHILD   : constant := 10;
   EAGAIN   : constant := 11;
   ENOMEM   : constant := 12;
   EACCES   : constant := 13;
   EFAULT   : constant := 14;
   ENOTBLK  : constant := 15;
   EBUSY    : constant := 16;
   EEXIST   : constant := 17;
   EXDEV    : constant := 18;
   ENODEV   : constant := 19;
   ENOTDIR  : constant := 20;
   EISDIR   : constant := 21;
   EINVAL   : constant := 22;
   ENFILE   : constant := 23;
   EMFILE   : constant := 24;
   ENOTTY   : constant := 25;
   ETXTBSY  : constant := 26;
   EFBIG    : constant := 27;
   ENOSPC   : constant := 28;
   ESPIPE   : constant := 29;
   EROFS    : constant := 30;
   EMLINK   : constant := 31;
   EPIPE    : constant := 32;
   ENAMETOOLONG : constant := 63;
   ENOTEMPTY    : constant := 66;
   EDEADLK  : constant := 78;
   ENOLCK   : constant := 79;
   ENOSYS   : constant := 90;
   ENOTSUP  : constant := 91;
   NO_PRIO_INHERIT : constant := 0;
   PRIO_INHERIT : constant := 1;
   PRIO_PROTECT : constant := 2;
   Add_Prio : constant Integer := 2;

end Interfaces.C.System_Constants;

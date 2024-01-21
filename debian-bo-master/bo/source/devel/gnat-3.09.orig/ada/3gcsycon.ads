-----------------------------------------------------------------------------
--                                                                         --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                         --
--         I n t e r f a c e s . C . S Y S T E M _ C o n s t a n t s       --
--                                                                         --
--                                 S p e c                                 --
--                                                                         --
--                            $Revision: 1.3 $                            --
--                                                                         --
--        Copyright (C) 1991,1992,1993,1994 Florida State University        --
--                                                                         --
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
   number_of_processors : constant Integer := 1;
   pthread_attr_t_size : constant Integer := 4;
   pthread_mutexattr_t_size : constant Integer := 4;
   pthread_mutex_t_size : constant Integer := 4;
   pthread_condattr_t_size : constant Integer := 4;
   pthread_cond_t_size : constant Integer := 4;
   pthread_key_t_size : constant Integer := 4;
   jmp_buf_size : constant Integer := 112;
   sigjmp_buf_size : constant Integer := 512;
   sigset_t_size : constant Integer := 16;

   --  The sizes of structures  (in bytes) and offsets of their components
   --  (in bits).  These are used to lay out the equivalent Ada records.
   siginfo_size : constant Integer := 128;
   si_signo_offset : constant Integer := 0;
   si_code_offset : constant Integer := 32;
   sigaction_size : constant Integer := 32;
   sa_handler_offset : constant Integer := 32;
   sa_mask_offset : constant Integer := 64;
   sa_flags_offset : constant Integer := 0;
   timespec_size : constant Integer := 8;
   tv_sec_offset : constant Integer := 0;
   tv_nsec_offset : constant Integer := 32;

   SIG_BLOCK : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;
   SA_NOCLDSTOP : constant := 131072;
   SA_SIGINFO : constant := 8;
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
   SIGSTOP : constant := 23;
   SIGTSTP : constant := 24;
   SIGCONT : constant := 25;
   SIGCHLD : constant := 18;
   SIGTTIN : constant := 26;
   SIGTTOU : constant := 27;
   SIGUSR1 : constant := 16;
   SIGUSR2 : constant := 17;
   NSIG    : constant := 65;
   --  OS specific signals represented as an array
   type Sig_Array is array (positive range <>) of integer;
   OS_Specific_Sync_Sigs : Sig_Array :=
     (NSIG, 5, 7, 10);
   OS_Specific_Async_Sigs : Sig_Array :=
     (NSIG, 12, 21, 22, 30, 31, 28, 29, 20);
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
   ENAMETOOLONG : constant := 78;
   ENOTEMPTY    : constant := 93;
   EDEADLK  : constant := 45;
   ENOLCK   : constant := 46;
   ENOSYS   : constant := 89;
   ENOTSUP  : constant := 0;
   ETIME    : constant := 62;
   ETIMEDOUT : constant := 145;
   NO_PRIO_INHERIT : constant := 0;
   PRIO_INHERIT : constant := 0;
   PRIO_PROTECT : constant := 0;
   Add_Prio : constant Integer := 2;

end Interfaces.C.System_Constants;

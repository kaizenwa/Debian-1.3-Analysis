-----------------------------------------------------------------------------
--                                                                         --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                         --
--         I n t e r f a c e s . C . S Y S T E M _ C o n s t a n t s       --
--                                                                         --
--                                 S p e c                                 --
--                                                                         --
--                            $Revision: 1.1 $                            --
--                                                                         --
--       Copyright (c) 1991,1992,1993,1994, FSU, All Rights Reserved       --
--                                                                         --
-- GNARL is free software; you can redistribute it and/or modify it  under --
-- terms  of  the  GNU  Library General Public License as published by the --
-- Free Software Foundation; either version 2, or  (at  your  option)  any --
-- later  version.   GNARL is distributed in the hope that it will be use- --
-- ful, but but WITHOUT ANY WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Gen- --
-- eral Library Public License for more details.  You should have received --
-- a  copy of the GNU Library General Public License along with GNARL; see --
-- file COPYING.LIB.  If not,  write to the  Free Software Foundation, 675 --
-- Mass Ave, Cambridge, MA 02139, USA.                                     --
--                                                                         --
-----------------------------------------------------------------------------

package Interfaces.C.System_Constants is

   pthread_t_size : constant Integer := 8;
   pthread_attr_t_size : constant Integer := 8;
   pthread_mutexattr_t_size : constant Integer := 8;
   pthread_mutex_t_size : constant Integer := 8;
   pthread_condattr_t_size : constant Integer := 8;
   pthread_cond_t_size : constant Integer := 8;
   pthread_key_t_size : constant Integer := 4;
   jmp_buf_size : constant Integer := 200;
   sigjmp_buf_size : constant Integer := 200;
   sigset_t_size : constant Integer := 32;

   --  The sizes of structures  (in bytes) and offsets of their components
   --  (in bits).  These are used to lay out the equivalent Ada records.
   --  eas siginfo_size : constant Integer := 0;
   sigaction_size : constant Integer := 40;
   sa_handler_offset : constant Integer := 0;
   sa_mask_offset : constant Integer := 32;
   sa_flags_offset : constant Integer := 288;
   timespec_size : constant Integer := 8;
   tv_sec_offset : constant Integer := 0;
   tv_nsec_offset : constant Integer := 32;

   SIG_BLOCK : constant := 0;
   SIG_UNBLOCK : constant := 1;
   SIG_SETMASK : constant := 2;
   SA_NOCLDSTOP : constant := 8;
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
   SIGSTOP : constant := 24;
   SIGTSTP : constant := 25;
   SIGCONT : constant := 26;
   SIGCHLD : constant := 18;
   SIGTTIN : constant := 27;
   SIGTTOU : constant := 28;
   SIGUSR1 : constant := 16;
   SIGUSR2 : constant := 17;
   NSIG    : constant := 31;
   --  OS specific signals represented as an array
   type Sig_Array is array (positive range <>) of integer;
   OS_Specific_Sync_Sigs : Sig_Array :=
     (NSIG, 5, 7, 10);
   OS_Specific_Async_Sigs : Sig_Array :=
     (NSIG, 12, 29, 22, 20, 21, 23, 30);
   --  End of OS specific signals representation
   E2BIG    : constant := 7;
   EACCES   : constant := 13;
   EAGAIN   : constant := 11;
   EBADF    : constant := 9;
   EBUSY    : constant := 16;
   ECHILD   : constant := 10;
   EDEADLK  : constant := 45;
   EDOM     : constant := 33;
   EEXIST   : constant := 17;
   EFAULT   : constant := 14;
   EFBIG    : constant := 27;
   EINTR    : constant := 4;
   EINVAL   : constant := 22;
   EIO      : constant := 5;
   EISDIR   : constant := 21;
   EMFILE   : constant := 24;
   EMLINK   : constant := 31;
   ENAMETOOLONG : constant := 248;
   ENFILE   : constant := 23;
   ENODEV   : constant := 19;
   ENOENT   : constant := 2;
   ENOEXEC  : constant := 8;
   ENOLCK   : constant := 46;
   ENOMEM   : constant := 12;
   ENOSPC   : constant := 28;
   ENOSYS   : constant := 251;
   ENOTDIR  : constant := 20;
   ENOTEMPTY    : constant := 247;
   ENOTTY   : constant := 25;
   ENXIO    : constant := 6;
   EPERM    : constant := 1;
   EPIPE    : constant := 32;
   ERANGE   : constant := 34;
   EROFS    : constant := 30;
   ESPIPE   : constant := 29;
   ESRCH    : constant := 3;
   EXDEV    : constant := 18;
   ETIME    : constant := 52;
   ETIMEDOUT : constant := 238;
   NO_PRIO_INHERIT : constant := 0;
   PRIO_INHERIT : constant := 0;
   PRIO_PROTECT : constant := 0;
   Add_Prio : constant Integer := 2;

end Interfaces.C.System_Constants;

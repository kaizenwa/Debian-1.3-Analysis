/****************************************************************************
 *                                                                          *
 *                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               *
 *                                                                          *
 *                               S Y S C O N                                *
 *                                                                          *
 *                          C   G e n e r a t e d                           *
 *                                                                          *
 *                             $Revision: 1.12 $                            *
 *                                                                          *
 *     Copyright (c) 1991,1992,1993,1994,1995 FSU, All Rights Reserved      *
 *                                                                          *
 * GNARL is free software;  you can redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion. GNARL is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License distributed with GNARL;  see file COPYING.  If not, write *
 * to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. *
 *                                                                          *
 ****************************************************************************
 */


/* This program generates the Ada spec file Interfaces.C.System_Constants,
   which contains system-specific constants and statistics about
   system-specific types. Interfaces.C.System_Constants is then used by
   other interface files to allow the definition of types that can
   be passed to imported C-language system bindings.  These are for the
   most part POSIX standard definitions.  This program also outputs
   some constants related to specific systems if they are defined.
*/


#define ADA_MIN_PRIO 0
#define ADA_MAX_PRIO 99
#define ADD_PRIO 2 
/* additional prio levels for int. entries, mutex ceiling */

/* pthread.h should be able to stand on its own, but it currently needs
   definitions from stddef.h and sys/time.h on the SGI Indy.  The Sun version
   seems to tolerate this.
*/
#include <stddef.h>
#include <sys/time.h>
#ifdef SOL_THR
#include <thread.h>
#else !SOL_THR
#include <pthread.h>
#endif

/* The Sun version of Pthreads defines its own jump buffers in pthread.h
   (which is probably wrong) whereas SGI uses the system definition.
   Bring in the system setjmp.h file only if needed.
*/
#ifndef jmp_buf
#include <setjmp.h>
#endif

#include <sys/errno.h>

/* The Sun version of Pthreads defines its own signal handling types and
   functions in pthreads.h (which is probably wrong),
   whereas SGI uses the system definition.
   Bring in the system signal.h file only if needed.
*/
#ifndef sigset_t
#include <signal.h>
#endif



/* The Sun version of Pthreads defines some POSIX error codes that are not
   provided by the SunOS include files.  This file only defines the
   error codes if they are not already defined.
*/

#ifndef SOL_THR
#include <pthread/errno.h>
#endif

main(argc, argv)
    int argc;
    char *argv[];
{

  int bits_per_byte;

#ifdef SOL_THR
  thread_t p = NULL;
#else !SOL_THR
  pthread_t p = NULL;
#endif
  if(argc > 2) {
    printf("Usage: syscon [-g]\n");
    exit(0);
  }
  else {
    printf("----------------------------------------");
    printf("-------------------------------------\n");
    printf("--                                      ");
    printf("                                   --\n");
    printf("--                GNU ADA RUNTIME LIBRAR");
    printf("Y (GNARL) COMPONENTS               --\n");
    printf("--                                     ");
    printf("                                    --\n");
    printf("--         I n t e r f a c e s . C . S Y");
    printf(" S T E M _ C o n s t a n t s       --\n");
    printf("--                                     ");
    printf("                                    --\n");
    printf("--                                 S p e");
    printf(" c                                 --\n");
    printf("--                                      ");
    printf("                                   --\n");
    printf("--                            $Revision:");
    printf(" 1.1 $                            --\n");
    printf("--                                      ");
    printf("                                   --\n");
    printf("--       Copyright (c) 1991,1992,1993,1994");
    printf(", FSU, All Rights Reserved       --\n");
    printf("--                                      ");
    printf("                                   --\n");
    printf("-- GNARL is free software; you can redis");
    printf("tribute it and/or modify it  under --\n");
    printf("-- terms  of  the  GNU  Library General ");
    printf("Public License as published by the --\n");
    printf("-- Free Software Foundation; either vers");
    printf("ion 2, or  (at  your  option)  any --\n");
    printf("-- later  version.   GNARL is distribute");
    printf("d in the hope that it will be use- --\n");
    printf("-- ful, but but WITHOUT ANY WARRANTY; wi");
    printf("thout even the implied warranty of --\n");
    printf("-- MERCHANTABILITY  or FITNESS FOR A PAR");
    printf("TICULAR PURPOSE.  See the GNU Gen- --\n");
    printf("-- eral Library Public License for more ");
    printf("details.  You should have received --\n");
    printf("-- a  copy of the GNU Library General Pu");
    printf("blic License along with GNARL; see --\n");
    printf("-- file COPYING.LIB.  If not,  write to");
    printf(" the  Free Software Foundation, 675 --\n");
    printf("-- Mass Ave, Cambridge, MA 02139, USA.  ");
    printf("                                   --\n");
    printf("--                                      ");
    printf("                                   --\n");
    printf("----------------------------------------");
    printf("-------------------------------------\n");
    printf("\n");
    printf("package Interfaces.C.System_Constants is\n\n");

    /* Figure out the number of bits in a byte. */
     {
	unsigned char b;

	bits_per_byte = 0;
	b = 1;
	while (b)
	    {
	       bits_per_byte++;
	       b = b << 1;
	    }
     }

#ifndef SOL_THR
    printf("   pthread_t_size : constant Integer := %d;\n", 
      sizeof(pthread_t));
    printf("   pthread_attr_t_size : constant Integer := %d;\n", 
      sizeof(pthread_attr_t));
    printf("   pthread_mutexattr_t_size : constant Integer := %d;\n", 
      sizeof(pthread_mutexattr_t));
    printf("   pthread_mutex_t_size : constant Integer := %d;\n", 
      sizeof(pthread_mutex_t));
    printf("   pthread_condattr_t_size : constant Integer := %d;\n", 
      sizeof(pthread_condattr_t));
    printf("   pthread_cond_t_size : constant Integer := %d;\n", 
      sizeof(pthread_cond_t));
    printf("   pthread_key_t_size : constant Integer := %d;\n", 
      sizeof(pthread_key_t));
#else SOL_THR
    printf("   thread_t_size : constant Integer := %d;\n", 
      sizeof(thread_t));
    printf("   mutex_t_size : constant Integer := %d;\n", 
      sizeof(mutex_t));
    printf("   cond_t_size : constant Integer := %d;\n", 
      sizeof(cond_t));
    printf("   thread_key_t_size : constant Integer := %d;\n", 
      sizeof(thread_key_t));
    printf("   THR_DETACHED : constant Integer := %d;\n",
      THR_DETACHED);
    printf("   USYNC_THREAD : constant Integer := %d;\n",
      USYNC_THREAD);
#endif
    printf("   jmp_buf_size : constant Integer := %d;\n",
      sizeof(jmp_buf));
    printf("   sigjmp_buf_size : constant Integer := %d;\n",
      sizeof(sigjmp_buf));
    printf("   sigset_t_size : constant Integer := %d;\n",
      sizeof(sigset_t));

    printf("\n");
    printf("   --  The sizes of structures  (in ");
    printf("bytes) and offsets of their components\n");
    printf("   --  (in bits).  These are used to lay ");
    printf("out the equivalent Ada records.\n");

    printf("   siginfo_size : constant Integer := %d;\n",
      sizeof(struct siginfo));
     {
	struct siginfo s;
        printf("   si_signo_offset : constant Integer := %d;\n",
          ((char *)&s.si_signo - (char *)&s) * bits_per_byte);
        printf("   si_code_offset : constant Integer := %d;\n",
          ((char *)&s.si_code - (char *)&s) * bits_per_byte);

        /* Solaris does not have an si_value field, even though it
           is defined in POSIX.  It is implementation defined, and not
           used at the moment, so we should be able to do without it.
        */

/*      printf("   si_value_offset : constant Integer := %d;\n",
          ((char *)&s.si_value - (char *)&s) * bits_per_byte);
*/
     }

    printf("   sigaction_size : constant Integer := %d;\n",
      sizeof(struct sigaction));
     {
	struct sigaction s;
	printf("   sa_handler_offset : constant Integer := %d;\n",
	  ((char *)&s.sa_handler - (char *)&s) * bits_per_byte);
	printf("   sa_mask_offset : constant Integer := %d;\n",
	  ((char *)&s.sa_mask - (char *)&s) * bits_per_byte);
	printf("   sa_flags_offset : constant Integer := %d;\n",
          ((char *)&s.sa_flags - (char *)&s) * bits_per_byte);

     }

    printf("   timespec_size : constant Integer := %d;\n",
      sizeof(struct timespec));
     {
	struct timespec t;
	printf("   tv_sec_offset : constant Integer := %d;\n",
	  ((int)&t.tv_sec - (int)&t) * bits_per_byte);
	printf("   tv_nsec_offset : constant Integer := %d;\n",
	  ((int)&t.tv_nsec - (int)&t) * bits_per_byte);
     }

    printf("\n");

    printf("   SIG_BLOCK : constant := %d;\n", SIG_BLOCK);
    printf("   SIG_UNBLOCK : constant := %d;\n", SIG_UNBLOCK);
    printf("   SIG_SETMASK : constant := %d;\n", SIG_SETMASK);

    printf("   SA_NOCLDSTOP : constant := %d;\n", SA_NOCLDSTOP);
    printf("   SA_SIGINFO : constant := %d;\n", SA_SIGINFO);

    printf("   SIG_ERR : constant := %d;\n", SIG_ERR);
    printf("   SIG_DFL : constant := %d;\n", SIG_DFL);
    printf("   SIG_IGN : constant := %d;\n", SIG_IGN);

    printf("   SIGNULL : constant := %d;\n", 0);
    printf("   SIGHUP  : constant := %d;\n", SIGHUP);
    printf("   SIGINT  : constant := %d;\n", SIGINT);
    printf("   SIGQUIT : constant := %d;\n", SIGQUIT);
    printf("   SIGILL  : constant := %d;\n", SIGILL);
    printf("   SIGABRT : constant := %d;\n", SIGABRT);
    printf("   SIGFPE  : constant := %d;\n", SIGFPE);
    printf("   SIGKILL : constant := %d;\n", SIGKILL);
    printf("   SIGSEGV : constant := %d;\n", SIGSEGV);
    printf("   SIGPIPE : constant := %d;\n", SIGPIPE);
    printf("   SIGALRM : constant := %d;\n", SIGALRM);
    printf("   SIGTERM : constant := %d;\n", SIGTERM);
    printf("   SIGSTOP : constant := %d;\n", SIGSTOP);
    printf("   SIGTSTP : constant := %d;\n", SIGTSTP);
    printf("   SIGCONT : constant := %d;\n", SIGCONT);
    printf("   SIGCHLD : constant := %d;\n", SIGCHLD);
    printf("   SIGTTIN : constant := %d;\n", SIGTTIN);
    printf("   SIGTTOU : constant := %d;\n", SIGTTOU);
    printf("   SIGUSR1 : constant := %d;\n", SIGUSR1);
    printf("   SIGUSR2 : constant := %d;\n", SIGUSR2);

    /* Number of OS signals. This is not POSIX but we
       need this for each OS on which we are porting RTS */
    printf("   NSIG    : constant := %d;\n", NSIG);

    /* OS specific signals */

    /* Creating arrays for Synchronous and Asynchronous 
       signals assuming there exists at least one such
       for each array */

    printf("   --  OS specific signals represented as an array\n");

    printf("   type Sig_Array is array ");
    printf("(positive range <>) of integer;\n");

    /* Creating an array for Synchronous Signals */

    printf("   OS_Specific_Sync_Sigs : Sig_Array :=\n");
    printf("     (");

    printf("NSIG");
#ifdef SIGTRAP
    printf(", %u", SIGTRAP);
#endif    

#ifdef SIGEMT
    printf(", %u", SIGEMT);
#endif

#ifdef SIGBUS
    printf(", %u", SIGBUS);
#endif    
    printf(");\n");


    /* Creating an array for Synchronous Signals */

    printf("   OS_Specific_Async_Sigs : Sig_Array :=\n");
    printf("     (");

    printf("NSIG");
#ifdef SIGSYS
    printf(", %u", SIGSYS);
#endif    

#ifdef SIGURG
    printf(", %u", SIGURG);
#endif    

#ifdef SIGIO
    printf(", %u", SIGIO);
#endif    

#ifdef SIGXCPU
    printf(", %u", SIGXCPU);
#endif    

#ifdef SIGXFSZ
    printf(", %u", SIGXFSZ);
#endif    

#ifdef SIGVTALRM
    printf(", %u", SIGVTALRM);
#endif    

#ifdef SIGPROF
    printf(", %u", SIGPROF);
#endif    

#ifdef SIGWINCH
    printf(", %u", SIGWINCH);
#endif    

#ifdef SIGLOST
    printf(", %u", SIGLOST);
#endif
    printf(");\n");
    printf("   --  End of OS specific signals representation\n");

    printf("   E2BIG    : constant := %d;\n", E2BIG    );
    printf("   EACCES   : constant := %d;\n", EACCES   );
    printf("   EAGAIN   : constant := %d;\n", EAGAIN   );
    printf("   EBADF    : constant := %d;\n", EBADF    );
    printf("   EBUSY    : constant := %d;\n", EBUSY    );
    printf("   ECHILD   : constant := %d;\n", ECHILD   );
    printf("   EDEADLK  : constant := %d;\n", EDEADLK   );
    printf("   EDOM     : constant := %d;\n", EDOM   );
    printf("   EEXIST   : constant := %d;\n", EEXIST   );
    printf("   EFAULT   : constant := %d;\n", EFAULT   );
    printf("   EFBIG    : constant := %d;\n", EFBIG    );
    printf("   EINTR    : constant := %d;\n", EINTR    );
    printf("   EINVAL   : constant := %d;\n", EINVAL   );
    printf("   EIO      : constant := %d;\n", EIO      );
    printf("   EISDIR   : constant := %d;\n", EISDIR   );
    printf("   EMFILE   : constant := %d;\n", EMFILE   );
    printf("   EMLINK   : constant := %d;\n", EMLINK   );
    printf("   ENAMETOOLONG : constant := %d;\n", ENAMETOOLONG   );
    printf("   ENFILE   : constant := %d;\n", ENFILE   );
    printf("   ENODEV   : constant := %d;\n", ENODEV   );
    printf("   ENOENT   : constant := %d;\n", ENOENT   );
    printf("   ENOEXEC  : constant := %d;\n", ENOEXEC  );
    printf("   ENOLCK   : constant := %d;\n", ENOLCK   );
    printf("   ENOMEM   : constant := %d;\n", ENOMEM   );
    printf("   ENOSPC   : constant := %d;\n", ENOSPC   );
    printf("   ENOSYS   : constant := %d;\n", ENOSYS   );
    printf("   ENOTDIR  : constant := %d;\n", ENOTDIR  );
    printf("   ENOTEMPTY    : constant := %d;\n", ENOTEMPTY   );
    printf("   ENOTTY   : constant := %d;\n", ENOTTY   );
    printf("   ENXIO    : constant := %d;\n", ENXIO    );
    printf("   EPERM    : constant := %d;\n", EPERM    );
    printf("   EPIPE    : constant := %d;\n", EPIPE    );
    printf("   ERANGE   : constant := %d;\n", ERANGE   );
    printf("   EROFS    : constant := %d;\n", EROFS    );
    printf("   ESPIPE   : constant := %d;\n", ESPIPE   );
    printf("   ESRCH    : constant := %d;\n", ESRCH    );
    printf("   EXDEV    : constant := %d;\n", EXDEV    );

/* SGI doesn't define these, so put in dummy values if they are undefined. */
#ifdef NO_PRIO_INHERIT
    printf("   NO_PRIO_INHERIT : constant := %d;\n", NO_PRIO_INHERIT);
    printf("   PRIO_INHERIT : constant := %d;\n", PRIO_INHERIT);
    printf("   PRIO_PROTECT : constant := %d;\n", PRIO_PROTECT);
#else
    printf("   NO_PRIO_INHERIT : constant := 0;\n");
    printf("   PRIO_INHERIT : constant := 0;\n");
    printf("   PRIO_PROTECT : constant := 0;\n");
#endif

/* A kludge to get the SGI version working; it appears to implement
   priorities, but does not have the usual POSIX definitions for it. */
#ifdef SCHED_FIFO
    if (sched_get_priority_min(SCHED_FIFO) > ADA_MIN_PRIO ||
        sched_get_priority_max(SCHED_FIFO) < ADA_MAX_PRIO + ADD_PRIO)
      fprintf(stderr, "ERROR: Pthreads priority range too small\n");
    else
#endif
      printf("   Add_Prio : constant Integer := %d;\n", ADD_PRIO);
    printf("\nend Interfaces.C.System_Constants;\n");

  }
  return(0);
}

#include <stdio.h>
#include <thread.h>
#include <time.h>
#include <sys/ucontext.h>

void
disp_context (ucontext_t *ucp)
{
 printf ("uc_flags = 0x%08lx\n", ucp->uc_flags);
 printf ("uc_link = 0x%08x\n", (int)ucp->uc_link);
 printf ("uc_stack.ss_sp = 0x%08x\n", (int)ucp->uc_stack.ss_sp);
 printf ("uc_stack.ss_size = 0x%08x\n", (int)ucp->uc_stack.ss_size); 
 printf ("uc_stack.ss_flags = 0x%08x\n", (int)ucp->uc_stack.ss_flags);
 printf ("uc_mcontext.gregs[REG_O6] = 0x%08x\n", ucp->uc_mcontext.gregs[REG_O6]);
 printf ("uc_mcontext.gregs[REG_PC] = 0x%08x\n", ucp->uc_mcontext.gregs[REG_PC]);
 printf ("uc_mcontext.gregs[REG_nPC] = 0x%08x\n", ucp->uc_mcontext.gregs[REG_nPC]);
}

void
new_thread (int num)
{
 int i;
 ucontext_t uc;
 
 getcontext (&uc);
 disp_context (&uc);
 
 for (i = 0; i < 5; i++)
   {
     printf ("Thread %d yield.\n", num);
     thr_yield ();
   }
 thr_exit (10);
}

main (int argc, char *argv[])
{
 thread_t tid;
 int err;
 int i;
 time_t t;
 stime (&t);
 
 for (i = 0; i < 5; i++)
   {
     err = thr_create (NULL, 0, new_thread, (void*)i, THR_BOUND, &tid);
   }
 while (thr_join (0, 0, 0) != 0)
  ;
}

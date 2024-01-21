#include <sys/ucontext.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

disp_uc (ucontext_t *ucp)
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
disp_ss (stack_t *ss)
{
  printf ("ss_sp:    0x%08x\n", (int)ss->ss_sp);
  printf ("ss_size:  0x%08x\n", ss->ss_size);
  printf ("ss_flags: 0x%08x\n", ss->ss_flags);
}

void
sig_proc (int sig, siginfo_t *info, ucontext_t *uc)
{
  ucontext_t u;
  
  printf ("\nhandler... (uc=0x%08x)\n", (uint)uc);
  disp_uc (uc);
  
  getcontext (&u);
  disp_uc (&u);
}

int
main()
{
 ucontext_t ucp;
 stack_t ss;
 struct sigaction action;
 
 getcontext (&ucp);
#if 0
 sigaltstack (0, &ss);
#endif
 
 printf ("Context:\n");
 disp_uc (&ucp);
 disp_ss (&(ucp.uc_stack));
 printf ("alt stack:\n");
 disp_ss (&ss);

#if 0
 ss.ss_sp = (char*)malloc (SIGSTKSZ);
 ss.ss_size = SIGSTKSZ;
 ss.ss_flags = 0; 
 sigaltstack(&ss, 0);
#endif
 
 action.sa_handler = sig_proc;
 sigemptyset(&action.sa_mask);
#if 0
 action.sa_flags = SA_ONSTACK;
#endif

 sigaction (SIGHUP, &action, 0);
 kill (getpid(), SIGHUP);
 return 0;
}

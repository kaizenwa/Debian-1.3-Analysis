#include <signal.h>
#include <stdio.h>

int foo(int *j, int b)
{
 int k;
 int l= 1;
 for(k = 1; k < b; k++)
   l += k * b * *j;
}

void show_sig(int sig)
{
 printf("Receive sig %d\n", sig);
}

void main()
{
  int j;
  struct sigaction action;
  
  action.sa_handler = show_sig;
  action.sa_mask = sigmask(SIGHUP);
  action.sa_flags = 0;
  if(sigaction(SIGHUP, &action, 0))
    {
      perror("sigaction");
      exit (0);
    }
  
  pause();
  
  printf("Now I do a lot of memory access error!\n");
  foo(&j, 100);
}
 
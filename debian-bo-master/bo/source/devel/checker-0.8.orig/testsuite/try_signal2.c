#include <signal.h>
#include <sys/time.h>

void rec_sig()
{
  char *b;
  printf("SIGALRM recu\n");
  b = malloc(20);
  free(b);
}

main()
{
  struct itimerval rttimer;
  struct sigaction action;
  char *b;
  int i;
  
  action.sa_handler = rec_sig;
  sigemptyset(&action.sa_mask);
  sigaddset(&action.sa_mask, SIGALRM);

  action.sa_flags = 0;
  if(sigaction(SIGALRM, &action, 0))
    {
      perror("sigaction");
      exit (0);
    }
  
  rttimer.it_value.tv_sec = 1;
  rttimer.it_value.tv_usec = 0;
  rttimer.it_interval.tv_sec = 0;
  rttimer.it_interval.tv_usec = 100000;
  if (setitimer(ITIMER_REAL, &rttimer, 0))
    {
      perror("setitimer");
      exit (0);
    }
  
  for (i = 100000; i ; i--)
    {
      b = malloc(10);
      free(b);
    }
}

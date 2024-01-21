#include <signal.h>
#include <sys/time.h>
#include <unistd.h>
#include <errno.h>

int ppid;
int sent = 0;
int received = 0;

void rec_sig_parent(int nsig)
{
  received++;
  printf("Parent: Receive sig #%d (%d)\n", nsig, received);
}

int foo(int *j, int b)
{
 int k;
 int l= 1;
 for(k = 1; k < b; k++)
   l += k * b * *j;
}

main()
{
  struct sigaction action;
  char *b;
  int i;
  
  ppid = getpid();
  printf("My pid: %d\n", ppid);
  
  if (fork())
    {
      /* The parent */
      action.sa_handler = rec_sig_parent;
      action.sa_mask = sigmask(SIGHUP);
      action.sa_flags = 0;
      if (sigaction(SIGHUP, &action, 0))
        {
          perror("sigaction");
          exit (0);
        }
      printf("Just a pause\n");
      pause();
        
      printf("Now I do a lot of memory access error!\n");
      i = 1;
      while (1)
        foo(&i, 100);
    }
  else
    {
      /* The child */
      printf("********* STOP-ME ***********\n");
      sleep (1);
      for (i = 0; i < 40; i++)
        {
          usleep(250000);
          sent++;
          printf("Child: Send SIGHUP to %d (%d)\n", ppid, sent);
          if (kill (ppid, SIGHUP) == -1 && errno == ESRCH)
            exit(1);
        }
      kill (ppid, SIGINT);
      exit(0);
    }
}

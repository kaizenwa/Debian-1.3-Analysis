#include <signal.h>
#include <sys/time.h>
#include <unistd.h>

int ppid;

void rec_sig_parent()
{
  printf("Parent: Receive sig\n");
}

int foo(int *j, int b)
{
 int k;
 int l= 1;
 for(k = 1; k < b; k++)
   l += k * b * *j;
}

main(int argc, char *argv[])
{
  struct sigaction action;
  char *b;
  int i;
  int n;
  int forever = 0;
  
  ppid = getpid();
  printf("My pid: %d\n", ppid);
  
  if (argc > 1)
    {
      if (*argv[1] == '-')
        {
          forever = 1;
          argv[1]++;
        }
      n = atoi (argv[1]);
      if (n == 0)
        n = 1;
     }
  else
    n = 40;
  action.sa_handler = rec_sig_parent;
  sigemptyset (&action.sa_mask);
  sigaddset (&action.sa_mask, SIGHUP);
  action.sa_flags = 0;
  if (sigaction(SIGHUP, &action, 0))
    {
      perror("sigaction");
      exit (0);
    }
  if (fork ())
    {
      /* The parent  */
      printf("Just a pause\n");
      pause();
        
      printf("Now I do a lot of memory access error!\n");
      i = 1;
      do
        foo(&i, 100);
      while (forever);
    }
  else
    {
      /* The child */
      for (i = 0; i < n; i++)
        {
#ifdef __linux__        
          usleep (100000);
#else
	  sleep (1);
#endif
          printf ("Child: Send SIGHUP to %d\n", ppid);
          if (kill (ppid, SIGHUP) == -1)
            break;
        }
    }
  exit(0);
}

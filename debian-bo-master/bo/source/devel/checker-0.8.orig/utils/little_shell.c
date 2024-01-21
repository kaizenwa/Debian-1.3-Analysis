#include <unistd.h>
#include <string.h>

int
main (int argc, char *argv[])
{
#define MESSAGE "Usage: little_shell program [args]\n"
  if (argc < 2)
    {
      write (2, MESSAGE, sizeof (MESSAGE) - 1);
      exit (33);
    }
    
  putenv ("LD_PRELOAD=/usr/home/tristan/Checker-0.6/tmp/checker.so.1");
  execv (argv[1], argv + 1);
}

#if 0
  int i;
  for (i = 0; i < argc; i++)
    {
      write(2, argv[i], strlen(argv[i]));
      write(2, "\n", 1);
    }
  if (argc == 3 && strcmp(argv[1], "-c") == 0 && strncmp(argv[2], "exec ", 5) == 0)
    {
      argv[2] += 5;
      for (i = 2; i < argc; i++)
        {
          write(2, argv[i], strlen(argv[i]));
          write(2, "\n", 1);
        } 
      execv(argv[2], &argv[2]);
      exit(57);
    }
  else
    exit(1);
  return 2;
}
#endif

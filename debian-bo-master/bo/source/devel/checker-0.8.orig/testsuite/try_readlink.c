#include <unistd.h>
#include <stdio.h>

main(int argc, char *argv[])
{
 char buf[1024];
 int len;
 if (argc != 2)
   {
     fprintf(stderr, "usage: %s symlinkfile\n",  argv[0]);
     exit(1);
   }
 len = readlink(argv[1], buf, 1024);
 if (len < 0)
   {
     perror("readlink");
     exit(2);
   }
 buf[len] = '\0';
 printf("%s -> %s\n", argv[1], buf);
}

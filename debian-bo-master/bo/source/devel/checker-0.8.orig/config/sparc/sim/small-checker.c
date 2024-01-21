#include <unistd.h>
#include <stdlib.h>

void
main (int argc, char *argv[])
{
  char buf[2048];

  strcpy (buf, "LD_PRELOAD=");	
  strcat (buf, getenv ("HOME"));
  strcat (buf, "/Checker-0.8/tmp/checker.so.1");
  putenv (buf);
  execv(argv[1], argv + 1);
}

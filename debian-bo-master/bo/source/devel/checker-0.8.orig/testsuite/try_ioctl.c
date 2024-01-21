#include <stdio.h>
#include <fcntl.h>

int _fcntl(int a, int b) { return 2; }
#define FCNTL fcntl

int
main(void)
{
  int val;
  val = FCNTL(0, F_GETFL);
  printf("GETFL = %d\n", val);
  return 0;
}

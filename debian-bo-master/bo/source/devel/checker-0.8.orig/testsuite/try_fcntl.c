#include <fcntl.h>

main(void)
{
  int val;
  val = fcntl(0, F_GETFL);
  printf("GETFL = %d\n", val);
  return 0;
}
                                                                                         
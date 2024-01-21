typedef unsigned int size_t;

#define HASH_SIZE 20

#include "bitops.h"

main ()
{
  int i;
  for (i = 0; i <= 30; i++)
    printf ("i:%d log:%d\n", 1 << i, log_size (1 << i));
}

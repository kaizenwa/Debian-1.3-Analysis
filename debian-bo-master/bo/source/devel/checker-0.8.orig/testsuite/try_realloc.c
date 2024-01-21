#include <stdio.h>
#include <stdlib.h>

main ()
{
  int *ptr;

  ptr = malloc (sizeof (int));
  *ptr = 1;
  ptr = realloc (ptr, 2 * sizeof (int));
  printf ("ptr[0] = %d.\n", ptr[0]);
}

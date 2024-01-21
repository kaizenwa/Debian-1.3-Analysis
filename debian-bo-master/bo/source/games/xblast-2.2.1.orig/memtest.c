#include <stdio.h>
#include <stdlib.h>

main ()
{
  int meg = 0;
  int *ptr;


  while (NULL != (ptr = malloc(1024*1024))) {
    fprintf ("Allocated %d MB\n", ++meg);
    for (i=0; i<(256*1024); i++) {
      ptr[i] = 0;
    }
  }
}

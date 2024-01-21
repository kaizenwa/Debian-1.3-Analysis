#include <stdlib.h>

main()
{
  int a,b,c,d,e;
  while (1)
    {
      a = rand();
      b = rand();
      c = rand();
      d = a * b;
      printf ("ffs(0x%08x)=%d\n", a, ffs(a));
      printf ("%d*%d=%d\n", a, b, d);
      e = d / c;
      printf ("%d/%d=%d\n", d, c, e);
      printf ("%d*%d=%d\n", c, e, c * e);
      if (c * e > d || c * (e+1) < d)
        return;
      
    }
}

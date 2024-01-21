#include <alloca.h>

char *find_null(char *s)
{
  while (*s)
    s++;
  return s;
}
 
int mul(int a, int b)
{
  static int c;
  c += a;
  return c * b;
}

int main()
{
 char c;
 char *zone=__alloca(20);
 char buffer[]="Hello!";
 int i=2;

 zone[0]= (char)i;
 i=5;
 c=zone[i];	/* error: read an uninitialized char */
 mul(c,i);
 find_null(buffer);
 _exit(0);
}

 
 
#include <stdio.h>

void main(void)
{
  union test {
    unsigned long a;
    unsigned char b[4];
  } et;

  et.a=0x12345678;

  if (et.b[0] == 0x12) printf("BIG_ENDIAN\n");
  else if (et.b[0] == 0x78) printf("LITTLE_ENDIAN\n");
  else printf("Odd architecture; possibly 64 bit long?");
}
  

/*
** Generates a passwd file with a lot of entrys
*/

#include <stdio.h>

void main()
{
  int i, j;

  for(i = 1; i <= 10000; i++)
    {
       printf("grp%d:*:%d:",i,i);
       for(j = 1; j <= 20; j++) printf("user%d,",j);
       printf("user4000\n");
    }
}

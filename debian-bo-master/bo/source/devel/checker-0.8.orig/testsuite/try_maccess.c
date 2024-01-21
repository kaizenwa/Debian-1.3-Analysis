#include <stdio.h>
#include <malloc.h>

#define MESSAGE(t) fprintf(stderr,t) 

void wait_key()
{
 fprintf(stderr,"Press return to continue...");
 getchar();
 fputc('\n',stderr);
}

int main()
{
 char *zone=malloc(20);
 char *ptr=NULL;
 int i;
 char c;
 
 MESSAGE("***Read an uninitialized byte\n");
 c=zone[1];	/* error */
 wait_key();
 
 MESSAGE("***Read before the block\n");
 c=zone[-2];	/* error */
 wait_key();
 
 MESSAGE("***Write after the block\n");
 zone[25]=' ';  /* error */
 wait_key();

 MESSAGE("***Free the block and write inside the block\n");
 free(zone);
 zone[4]='h';
 wait_key();
 
 MESSAGE("***Use a NULL pointer\n");
 *ptr = 2;
 wait_key();
 
 MESSAGE("***Write in text segment\n");
 ptr = (char*)main;
 (*ptr)++;
 wait_key();
  
/*
 for(i=0; i<9; i++)
  zone[i]='\0';
 for(i=11; i<22; i++)
  zone[i]='\0';
 c=zone[10];
*/
 exit(0);
}

 
 
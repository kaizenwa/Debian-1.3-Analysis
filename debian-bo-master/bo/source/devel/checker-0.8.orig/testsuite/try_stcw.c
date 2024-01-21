#include <stdio.h>

void f( double d )
{
   long L;

   printf( "&d = %p, &L = %p\n", &d, &L );
   L = (long)d;	/* error */
   printf( "L = %ld\n", L );
#if 1
   asm ("fnstcw (%ebp)");	/* Just for testing as */
#endif   
   d -= L;
}

int main( void )
{
   f( 4.5 );
   return 0;
}

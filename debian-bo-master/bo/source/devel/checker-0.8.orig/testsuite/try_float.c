/* very ** 3 dumb program to test floatting point instructions. */
#include <float.h>
#include <math.h>

static void
chkr_set_disassemble_level (int level)
{
  asm (
  	"mov %0, %%o0\n\t"
  	"mov %1, %%g1\n\t"
  	"ta 121\n\t"
  	:
  	: "r" (level), "I" (40));
}

int
main(int argc, char *argv[])
{
 int level = 1;
 double f;
 
 if (argc == 2)
   level = atoi (argv[1]);
   
 chkr_set_disassemble_level (level);
 
 f = cos(0);
 
 while (f > 0)
   {
     printf ("%g\n", f);
     f = f / 2;
   }
}

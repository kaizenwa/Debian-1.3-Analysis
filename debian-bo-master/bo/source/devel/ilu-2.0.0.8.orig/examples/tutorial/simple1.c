/* simple1.c, a simple program that demonstrates the use of the
   Tutorial true module as a library.
*/

#include <stdio.h>	/* for NULL */
#include <stdlib.h>	/* for atof */

/* Include the header file for the Tutorial interface, so that our
 * types and methods will be defined.
 */

#include <Tutorial.h>

/* We should also define a prototype for the Create function
 * exported from the implementation of the Tutorial module.
 */

extern Tutorial_Calculator Create_Tutorial_Calculator(void);

/* A simple program:
 *  1)  make an instance of Tutorial.Calculator
 *  2)  add all the arguments by invoking the Add method
 *  3)  print the resultant value.
 */

int main (int argc, char **argv)
{
  Tutorial_Calculator c;
  CORBA_double v;
  char **arg;
  ILU_C_ENVIRONMENT env;

  /* Initialize the Tutorial module */

  Tutorial__InitializeServer();

  /* Create a calculator object */

  if ((c = Create_Tutorial_Calculator()) == NULL)
    {
      fprintf (stderr, "Couldn't create calculator!\n");
      exit(1);
    }

  /* clear the calculator before using it */

  Tutorial_Calculator_SetValue (c, 0.0, &env);

  /* now loop over the arguments, adding each in turn */

  for (arg = ++argv;  *arg != NULL;  arg++)
    {
      v = atof (*arg);
      Tutorial_Calculator_Add (c, v, &env);
    }

  /* and print the result */

  v = Tutorial_Calculator_GetValue (c, &env);
  printf ("the sum is %.5e\n", v);

  exit (0);  
}

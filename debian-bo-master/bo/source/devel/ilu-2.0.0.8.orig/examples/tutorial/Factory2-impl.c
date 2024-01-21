/* Factory2-impl.c */

/* Include the Tutorial header file, to get all the defined
 * types and function prototypes.
 */

#include <Tutorial2.h>

/* Code for the Factory object type */

extern Tutorial2_TapeCalculator Create_Tutorial2_TapeCalculator(void);

Tutorial_Calculator server_Tutorial2_Factory_CreateCalculator (Tutorial2_Factory f, ILU_C_ENVIRONMENT *env)
{
  return ((Tutorial_Calculator) Create_Tutorial2_TapeCalculator());
}

Tutorial2_TapeCalculator server_Tutorial2_Factory_CreateTapeCalculator (Tutorial2_Factory f, ILU_C_ENVIRONMENT *env)
{
  return (Create_Tutorial2_TapeCalculator());
}


/* Factory-impl.c */

/* Include the Tutorial header file, to get all the defined
 * types and function prototypes.
 */

#include <Tutorial.h>

/* Code for the Factory object type */

extern Tutorial_Calculator Create_Tutorial_Calculator(void);

  Tutorial_Calculator
server_Tutorial_Factory_CreateCalculator (
  Tutorial_Factory self,
  ILU_C_ENVIRONMENT *env)
{
  return (Create_Tutorial_Calculator());
}

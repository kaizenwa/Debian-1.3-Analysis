/* $Id: multlang.c,v 1.5 1996/05/04 05:16:38 janssen Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include "multlang.h"

ilu_Server theServer;

CORBA_unsigned_long
  server_multlang_Squarer_ObtainSquare (multlang_Squarer handle,
					CORBA_unsigned_long val,
					ILU_C_ENVIRONMENT *env)
{
  /* dummy routine; never called */
  fprintf (stderr, "C routine server_multlang_Squarer_ObtainSquare called!\n");
  exit(1);
}

CORBA_unsigned_long
  server_multlang_Multiplier_Multiply (multlang_Multiplier handle,
				       CORBA_unsigned_long val1,
				       CORBA_unsigned_long val2,
				       ILU_C_ENVIRONMENT *env)
{
  return (val1 * val2);
}

int main(int ac, char **av)
{
  multlang_Squarer	squarer;
  multlang_Multiplier	mult;
  ILU_C_ENVIRONMENT	env;
  CORBA_unsigned_long	input, val;
  CORBA_boolean		stat;

  /*********** set up the C side of the world */

  multlang__Initialize();
  multlang__InitializeServer();

  theServer = ILU_C_InitializeServer("Server1", NULL, NULL, NULL, NULL, ilu_TRUE);
  if (theServer == NULL)
    {
      fprintf (stderr, "Can't create a server.\n");
      exit(1);
    }
  mult = multlang_Multiplier__CreateTrue ("theMultiplierObject", theServer, NULL);
  if (mult == NULL)
    {
      fprintf (stderr, "Can't create the multiplier object\n");
      exit(1);
    }
  else
    {
      printf ("Created Multiplier object <%s>\n",
	      ILU_C_SBHOfObject(mult));
    }
  if (ILU_C_PublishObject(mult) == NULL)
    {
      fprintf (stderr, "Can't publish the multiplier object\n");
      exit(1);
    }

  /*********** now set up the Python side of the world */

  Py_Initialize();
  PyRun_SimpleString("import multlangimpl");

  squarer = ILU_C_LookupObject ("Server2", "theSquarerObject",
				multlang_Squarer__MSType);
  if (squarer == NULL)
    {
      fprintf (stderr, "Can't find local squarer object\n");
      exit(1);
    }

  /*********** OK, ready to run */

  stat = ilu_TRUE;

  input = 21;
  val = multlang_Squarer_ObtainSquare (squarer, input, &env);
  if (ILU_C_SUCCESSFUL(&env)) {
    printf ("square of %u is %u.\n", input, val);
    stat = stat && ilu_TRUE;
  } else {
    printf("exception on multlang_Squarer_ObtainSquare(%u): \"%s\"\n",
	   input, env.returnCode);
    stat = stat & ilu_FALSE;
  }

  input = 0xFFFFFFF3;
  val = multlang_Squarer_ObtainSquare (squarer, input, &env);
  if (ILU_C_SUCCESSFUL(&env)) {
    printf ("square of %u is %u.\n", input, val);
    stat = stat & ilu_FALSE;
  } else {
    printf("exception on multlang_Squarer_ObtainSquare(%u), \"%s\"\n",
	   input, env.returnCode);
    stat = stat && ilu_TRUE;
  }

  input = 0xFFF3;
  val = multlang_Squarer_ObtainSquare (squarer, input, &env);
  if (ILU_C_SUCCESSFUL(&env)) {
    printf ("square of %u is %u.\n", input, val);
    stat = stat && ilu_TRUE;
  } else {
    printf("exception on multlang_Squarer_ObtainSquare(%u), \"%s\"\n",
	   input, env.returnCode);
    stat = stat & ilu_FALSE;
  }

  if (stat)
    printf ("All calls behaved as expected.\n");
  else
    printf ("Unexpected result for some call.\n");
  return ((stat) ? 0 : 1);
}

/* simple4.c */

#include <stdio.h>	/* for NULL */
#include <stdlib.h>	/* for atof */

/* Include the header file for the Tutorial interface, so that our
 * types and methods will be defined.
 */

#include <Tutorial2.h>

/* We define a new routine, "Get_Tutorial_Calculator", which 
 * finds the tutorial factory, then creates a new Calculator
 * object for us.
 */

  static Tutorial_Calculator
Get_Tutorial_Calculator (char *sid, char *ih)
{
  Tutorial_Factory f;
  Tutorial2_TapeCalculator c;
  ILU_C_ENVIRONMENT env;

  /* We have to call ILU_C_LookupObject() with the object ID of
   * the factory object, and the ``type'' of the object we're looking
   * for, which is always available as "TYPENAME__MSType".
   */

  f = ILU_C_LookupObject (sid, ih, Tutorial2_Factory__MSType);

  if (f == NULL)
    {
      fprintf (stderr, "Couldn't find Factory object <%s %s>.\n",
	       sid, ih);
      return (NULL);
    }

  /* Now call the CreateCalculator method on the factory, and check
   * the result...
   */

  c = Tutorial2_Factory_CreateTapeCalculator (f, &env);
  if (! ILU_C_SUCCESSFUL(&env))
    {
      fprintf (stderr, "Call to CreateCalculator failed with exception <%s>.\n",
	       ILU_C_EXCEPTION_ID(&env));
      return (NULL);
    }
  else
    printf ("Got calculator object \"%s\" of type \"%s\".\n",
	    ILU_C_SBHOfObject(c), ILU_C_ClassName(c));

  /* And return the calculator */

  return ((Tutorial_Calculator) c);
}

/* A routine to print an operation from a Tutorial2.RegisterTape */

static char *NameOfOp (Tutorial2_OpType ot)
{
  static struct _ops { char *name; Tutorial2_OpType ot; } ops[] = {
    { "Add", Tutorial2_Add },
    { "Subtract", Tutorial2_Subtract },
    { "Divide", Tutorial2_Divide },
    { "Multiply", Tutorial2_Multiply },
    { "SetValue", Tutorial2_SetValue } };
  int opslen = sizeof(ops)/sizeof(struct _ops);
  int i;
  char *result;

  for (i = 0;  i < opslen;  i++)
    if (ops[i].ot == ot)
      return (ops[i].name);
  return ILU_NIL;
}

static void PrintOperation (Tutorial2_Operation *op, void *userArg)
{
  char *opname = NameOfOp (op->op);

  if (opname == ILU_NIL)
    printf ("** Unrecognized operation, op = %d!\n", op->op);
  else
    printf ("  %s(%.5f) => %.5f\n", opname, op->value, op->accumulator);
}

int main (int argc, char **argv)
{
  Tutorial_Calculator c;
  ILU_C_ENVIRONMENT e;
  char *line, *sid, *ih;
  char buf[1000];
  CORBA_double val;
  CORBA_double newval = 0.0;

  if (argc < 3)
    {
      fprintf (stderr, "Usage:  %s FACTORY-OBJECT-SID FACTORY-OBJECT-IH\n",
	       argv[0]);
      exit(1);
    }

  Tutorial__Initialize();
  Tutorial2__Initialize();

  sid = *++argv;
  ih = *++argv;
  if ((c = Get_Tutorial_Calculator(sid, ih)) == NULL)
    {
      fprintf (stderr, "Couldn't create calculator!\n");
      exit(1);
    }

  Tutorial_Calculator_SetValue(c, 0.0, &e);
  do {
    printf ("%.5f\n> ", newval);
    fflush(stdout);

    *buf = 'q';
    line = gets(buf);

    switch (buf[0]) {

    case '+':
      val = atof(buf+1);
      if (!((Tutorial_Calculator_Add(c, val, &e), ILU_C_SUCCESSFUL(&e))
	    && (newval = Tutorial_Calculator_GetValue(c, &e),
		ILU_C_SUCCESSFUL(&e))))
	{
	  fprintf (stderr, "Operation <%s> signals error <%s>.\n",
		   buf, ILU_C_EXCEPTION_ID(&e));
	}	
      break;

    case '-':
      val = atof(buf+1);
      if (!((Tutorial_Calculator_Subtract(c, val, &e), ILU_C_SUCCESSFUL(&e))
	    && (newval = Tutorial_Calculator_GetValue(c, &e),
		ILU_C_SUCCESSFUL(&e))))
	{
	  fprintf (stderr, "Operation <%s> signals error <%s>.\n",
		   buf, ILU_C_EXCEPTION_ID(&e));
	}	
      break;

    case '*':
      val = atof(buf+1);
      if (!((Tutorial_Calculator_Multiply(c, val, &e), ILU_C_SUCCESSFUL(&e))
	    && (newval = Tutorial_Calculator_GetValue(c, &e),
		ILU_C_SUCCESSFUL(&e))))
	{
	  fprintf (stderr, "Operation <%s> signals error <%s>.\n",
		   buf, ILU_C_EXCEPTION_ID(&e));
	}	
      break;

    case '/':
      val = atof(buf+1);
      if (!((Tutorial_Calculator_Divide(c, val, &e), ILU_C_SUCCESSFUL(&e))
	    && (newval = Tutorial_Calculator_GetValue(c, &e),
		ILU_C_SUCCESSFUL(&e))))
	{
	  fprintf (stderr, "Operation <%s> signals error <%s>.\n",
		   buf, ILU_C_EXCEPTION_ID(&e));
	}	
      break;

    case 'c':
      if (!(Tutorial_Calculator_SetValue(c, 0.0, &e), ILU_C_SUCCESSFUL(&e)))
	{
	  fprintf (stderr, "Operation <%s> signals error <%s>.\n",
		   buf, ILU_C_EXCEPTION_ID(&e));
	}	
      newval = 0.0;
      break;

    case 'q':
      line = NULL;
      break;

    case 't':
      /* get the register tape and print it out */
      {
	Tutorial2_TapeCalculator tc = (Tutorial2_TapeCalculator) c;
	Tutorial2_RegisterTape *rt;

	rt = Tutorial2_TapeCalculator_GetTape (c, &e);
	if (! ILU_C_SUCCESSFUL(&e))
	  {
	    fprintf (stderr, "Operation <%s> signals error <%s>.\n",
		     buf, ILU_C_EXCEPTION_ID(&e));
	  }
	else
	  {
	    Tutorial2_RegisterTape_Every (rt, PrintOperation, ILU_NIL);
	    Tutorial2_RegisterTape__Free (rt);
	  }
      }
      break;

    default:
      fprintf (stderr, "Invalid operation <%s>\n", buf);
      fprintf (stderr, "Valid ops are +, -, *, /, tape, clear, quit\n");
    };

  } while (line != NULL);

  return (0);
}

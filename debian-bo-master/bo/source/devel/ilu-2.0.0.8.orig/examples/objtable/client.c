#include <string.h>
#include <stdio.h>

#include "objtable.h"

int main (int ac, char **av)
{
  objtable_server s;
  objtable_file f1, f2, f3;
  objtable_filename name;
  char *sbh;
  ILU_C_ENVIRONMENT status;

  if (ac < 2)
    {
      fprintf (stderr, "Usage:  client SID\n");
      return 1;
    }

  objtable__Initialize();

  s = (objtable_server) ILU_C_LookupObject (av[1], "----", objtable_server__MSType);
  if (s == ILU_NIL)
    {
      fprintf (stderr, "Can't create object from SBH <%s>\n", av[1]);
      return 1;
    }

  f1 = objtable_server_find_file (s, "foo", &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "objtable_server_find_file(s, \"foo\") fails with exception <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  name = objtable_file_name (f1, &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "objtable_file_name(f1) fails with exception <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  else
    fprintf (stderr, "name of file f1 is <%s>\n", name);

  f2 = ILU_C_CreateSurrogateObject (objtable_file__MSType,
				    "bar", f1->server, &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "Can't create new surrogate object with ih \"bar\"\n, error <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  name = objtable_file_name (f2, &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "objtable_file_name(f2) fails with exception <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  else
    fprintf (stderr, "name of file f2 is <%s>\n", name);

  f3 = ILU_C_CreateSurrogateObject (objtable_file__MSType,
				    "bletch", f1->server, &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "Can't create new surrogate object with ih \"bletch\"\n, error <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  name = objtable_file_name (f3, &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "objtable_file_name(f3) fails with exception <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  else
    fprintf (stderr, "name of file f3 is <%s>\n", name);

  return 0;
}


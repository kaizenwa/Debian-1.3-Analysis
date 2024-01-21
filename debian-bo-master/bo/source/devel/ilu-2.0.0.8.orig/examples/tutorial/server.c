/* server.c */

#include <stdio.h>	/* for stderr and NULL */

/* Include the Tutorial header file, to get all the defined
 * types and function prototypes.
 */

#include <Tutorial.h>

int main (int ac, char **av)
{
  Tutorial_Factory theFactory;
  ilu_Server theServer;
  int stop;

  /* this program is to be called with one argument, the server ID
     to use ("Tutorial.foo.something.com" or something like that.)
  */
  
  if (ac < 2)
    {
      fprintf (stderr, "Usage:  server SERVER-ID\n");
      exit(1);
    }

  /* In any server program, we have to initialize each interface
     that we're providing types from, by calling the InitializeServer
     method on that interface.  In this case, it's just the
     Tutorial interface.
  */

  Tutorial__InitializeServer();

  /* We make a "kernel server", using the server ID that was
     passed in on the command line, the default "object table",
     the default protocol for data pickling and message packets,
     the default transport system for getting data back and forth,
     and we make this kernel server the default server for the
     program.
  */

  theServer = ILU_C_InitializeServer(av[1],
				     /* the server ID */
				     NULL,
				     /* use std object table */
				     NULL,
				     /* use default protocol */
				     NULL,
				     /* use default transport */
				     NULL,
				     /* don't supply any identification */
				     ilu_TRUE
				     /* establish as default server */
				     );

  /* Now that we have a server, we create an instance of the
     Factory object type, with the instance handle of "Factory",
     by calling the automatically generated procedure
     "Tutorial_Factory__CreateTrue()".
  */

  theFactory = Tutorial_Factory__CreateTrue ("theFactory",
					     /* instance handle */
					     theServer,
					     /* server to use */
					     NULL
					     /* no user data */
					     );

  /* Now make the Factory object "well-known" by publishing it.
     PublishObject will return NULL if it can't publish the
     object; otherwise it will return a pointer to a string,
     which is the "publish proof".
  */

  if (ILU_C_PublishObject(theFactory) == NULL)
    {
      fprintf (stderr, "Can't publish theFactory object.\n");
      exit(1);
    }
  else
    {
      /* Now we print the string binding handle (the object's name plus
	 its location) of the new instance.
      */

      printf ("Factory instance published.\n");
      printf ("Its SBH is \"%s\".\n", ILU_C_SBHOfObject(theFactory));

      /* ilu_RunMainLoop() is an event dispatching loop.  It may
	 be exited by invoking ilu_ExitMainLoop() passing the same
	 int * RunMainLoop was invoked with.  */

      ilu_RunMainLoop (&stop);
    }
}


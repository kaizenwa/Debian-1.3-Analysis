/* $Id: server.c,v 1.5 1996/05/04 05:16:10 janssen Exp $ */
/* Last edited by Mike Spreitzer December 18, 1995 2:28 pm PST */

#include <stdio.h>
#include <unistd.h>	/* for gethostname */

#include "objtable.h"

#define ILU_TEST_DOMAIN "parc.xerox.com"

static CORBA_Object
  object_of_ih (ilu_string ih, ilu_private rock)
{
  return (objtable_file__OTCreateTrue (ih,
				       *((ilu_Server *) rock),
				       ILU_C_Strdup(ih)));
}

static void
  free_self (ilu_private self)
{
  return;
}

objtable_filename
  server_objtable_file_name (objtable_file self,
			     ILU_C_ENVIRONMENT *status)
{
  ILU_C_SET_SUCCESSFUL(status);
  return (ILU_C_Strdup((char *) objtable_file__GetUserData(self)));
}

objtable_file
  server_objtable_server_find_file (objtable_file self,
				    objtable_filename name,
				    ILU_C_ENVIRONMENT *status)
{
  ILU_C_SET_SUCCESSFUL(status);
  return (objtable_file__CreateTrue (name,
				     self->server,
				     ILU_C_Strdup(name)));
}

int main (int ac, char **av)
{
  static ilu_Server ks;
  objtable_server s;
  objtable_file f;
  char *sid;
  ILU_C_ObjectTable ot = ILU_C_CreateObjectTable (object_of_ih, free_self, (ilu_private) &ks);

  objtable__InitializeServer();

  sid = ilu_InventID();
  ks = ILU_C_InitializeServer (sid, ot, ILU_NIL, ILU_NIL, ILU_NIL, ilu_TRUE);
  s = objtable_server__CreateTrue ( "----", ks, NULL );
  ILU_C_PublishObject (s);

  if (s != NULL)
    {
      printf ("SID is %s\n", sid);
      ILU_C_Run( );
    }
  else
    {
      printf ("couldn't create server object -- exiting\n");
      exit(1);
    }
  return 1;
}


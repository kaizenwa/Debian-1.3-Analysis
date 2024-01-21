/*$Id: c_help.cc,v 11.24 96/02/25 14:09:30 al Exp $ -*- C++ -*-
 * Accesses a help file to provide on-line documentation.
 * Will follow the executables path to find it.
 */
#include "ap.h"
#include "error.h"
#include "io.h"
#include "c_comand.h"
/*--------------------------------------------------------------------------*/
//	void	CMD::help(CS&);
/*--------------------------------------------------------------------------*/
void CMD::help(CS& cmd)
{
  static FILE *hfn;
  char buffer[BUFLEN];
  char *name;
  
  if (hfn)
    fclose(hfn);
  name = findfile(HELPFILE, HELPPATH, R_OK);
  if (!name)
    error(bERROR, "Help not available\n");
  hfn = fopen(name, "r");
  if (!hfn)
    error(bERROR, "internal error: Can't open help file %s\n", name);
  if (cmd.more())
    do {
      if (!fgets(buffer, BUFLEN, hfn)) {
	error(bWARNING, "No help on %s\n", cmd.tail());
	break;
      }
    } while (buffer[0]!=':'  ||  !cmd.pmatch(&buffer[1]));

  for (int line = 0; ; ++line){
    if (!fgets(buffer,BUFLEN,hfn))
      break;
    if (buffer[0]==':')
      break;
    if (line%23 == 22){		/* BUG: fixed screen length */
      CS nil("");
      pause(nil);
    }
    mputs(buffer,IO::mstderr);
  }
  fclose(hfn);
  hfn = (FILE*)NULL;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

/*
 *   This program demonstrates a bug in the Tcl regexp cache. The culprit
 *	appears to be the final regexp compiled below though it could
 *	just be random.
 *
 *   On Linux, it core dumps right away.
 *   On SunOS, I get
 *   $ ./regtest
 *   newMesg regexp is 1a1d0
 *   % mfv_test
 *   newMesg regexp is 1a1d0
 *   memcmp returns 48
 *   Regexp newMesg not working
 *   error while matching regular expression: memory corruption
 *
 */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>

#ifdef MEMCHECK
#include "tclRegexp.h"
static struct regexp newMesg_keep;
#endif

static char smline[] = "From user@wrkst.cr  Wed Jul 12 00:56:22 1995\n";

static Tcl_RegExp newMesg;
static Tcl_RegExp emptyLine;
static Tcl_RegExp fullName[4];


int Mfv_Test(ClientData cd, Tcl_Interp *interp, int argc, char *argv[])
{
#ifdef MEMCHECK
  fprintf(stderr, "newMesg regexp is %x\n", newMesg);
  fprintf(stderr, "memcmp returns %d\n",
      memcmp(newMesg,&newMesg_keep,sizeof(struct regexp)));
#endif
  if ((Tcl_RegExpExec(interp,newMesg,smline,smline)) < 1) {
    fprintf(stderr, "Regexp newMesg not working\n");
  }

  return TCL_OK;
}

int Mfv_CompileRegExps(Tcl_Interp *interp)
{
  char *p;
  
  newMesg = Tcl_RegExpCompile(interp,
      "^From +[^ ]+ +[A-Za-z]+ +[A-Za-z]+ +[ 0-9A-Za-z:+-]+[ \t\n]*$");
  if (newMesg == NULL ) {
    badregexp:
    interp->result = "internal error : bad regexp";
    return TCL_ERROR;
  }
  
  emptyLine = Tcl_RegExpCompile(interp, "^[ \t\n]*$");
  if (emptyLine == NULL ) goto badregexp;

  fullName[0] = Tcl_RegExpCompile(interp, "\"(.+)\" <.*>");
  if (fullName[0] == NULL ) goto badregexp;
  fullName[1] = Tcl_RegExpCompile(interp, "(.+) <.*>");
  if (fullName[1] == NULL ) goto badregexp;
  fullName[2] = Tcl_RegExpCompile(interp, "^[^\\(]*\\((.+)\\)[^\\)]*$");
  if (fullName[2] == NULL ) goto badregexp;

  /* This appears to be the culprit. Remove these 2 lines and things work */
  fullName[3] = Tcl_RegExpCompile(interp, "<(.+)>");
  if (fullName[3] == NULL ) goto badregexp;
 
  return TCL_OK;
}

int Mfv_Init(Tcl_Interp *interp)
{
  if (Mfv_CompileRegExps(interp) != TCL_OK) return TCL_ERROR;
  
  Tcl_CreateCommand(interp, "mfv_test", Mfv_Test, (ClientData) NULL,
      (Tcl_CmdDeleteProc *)NULL);
  
  if ((Tcl_RegExpExec(interp,newMesg,smline,smline)) < 1) {
    fprintf(stderr, "Regexp newMesg not working\n");
  }
#ifdef MEMCHECK
  fprintf(stderr, "newMesg regexp is %x\n", newMesg);
  memcpy(&newMesg_keep,newMesg,sizeof(struct regexp));
#endif
  
  return TCL_OK;
}

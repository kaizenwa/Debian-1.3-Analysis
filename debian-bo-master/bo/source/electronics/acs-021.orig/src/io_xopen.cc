/*$Id: io_xopen.cc,v 11.28 96/03/03 23:07:55 al Exp $ -*- C++ -*-
 * scan a string for a file name
 * fill in extension, if necessary
 * open file
 */
#include "md.h"
#include "constant.h"
#include "ap.h"
#include "l_lib.h"
#include "error.h"
#include <time.h>
#include "io.h"
#include "declare.h"	/* getcmd */
/*--------------------------------------------------------------------------*/
	void	xclose(FILE**);
	FILE*	xopen(CS&,const char*,const char*);
/*--------------------------------------------------------------------------*/
void xclose(FILE **fn)
{
  if (*fn){
    fclose(*fn);
    *fn = (FILE*)NULL;
  }
}
/*--------------------------------------------------------------------------*/
/* xopen: open a file from a command string
 * 	scan and eat up the name from the command
 *	add default extension if appropriate
 *	if there is no name, prompt for one
 *	trap errors of not found and clobber
 */
FILE *xopen(CS& cmd, const char *ext, const char *how)
{
  int i;	/* an index */
  int defalt;	/* flag: we need to add the default extension */
  FILE *code;	/* a file pointer for the file we found */
  char fname[MAXPATHLEN];
  
  cmd.skipbl();
  if (cmd.end()){
    cmd = CS(getcmd ("file name?  ",fname, MAXPATHLEN));
  }
					/* copy the name		    */
					/* and while we're at it ...	    */
  cmd.skipbl();				/* find out if we want to add the   */
  defalt = true;			/* default extension		    */
  for (i = 0;   i < MAXPATHLEN;   ){
    char c;
    c = cmd.ctoc();
    if (!c || isspace(c))
      break;
    if (c == '$'){
      sprintf(&(fname[i]), "%ld", (long)time((time_t*)NULL));
      i = strlen(fname);
    }else{				/* we want to add the extension	    */
      fname[i++] = c;			/* if it doesn't already have one,  */
      if (c == '.')			/* as determined by a '.'	    */
	defalt = false;			/* not before the directory	    */
      else if (strchr(ENDDIR,c))	/* separator-terminator character   */
	defalt = true;			/* '\' or '/' for msdos,	    */
    }  					/* ']' or '/' for vms,		    */
  }					/* '/' for unix  (in ENDDIR)	    */
  cmd.skip(-1);
  if (defalt && ext && *ext && i+strlen(ext)+2 < MAXPATHLEN){
    fname[i++] = '.';			/* add the extension (maybe)	    */
    strcpy(&fname[i],ext);
  }else{
    fname[i] = '\0';
  }
  
  trim(fname);
  cmd.skipcom();
  
  if ((*how == 'w')   &&   (access(fname,F_OK) == GOOD)){
    char buffer[BUFLEN];		
    error(bWARNING, "%s exists.  replace? ", fname);
    getcmd("", buffer, BUFLEN);
    if (pmatch(buffer,"Yes"))		/* should be new file, but	    */
      code = fopen(fname,how);		/* file already exists,  ask	    */
    else
      return (FILE*)NULL;
  }else{
    code = fopen(fname,how);
  }
  
  if (code && fileno(code)>MAXHANDLE){
    error(bWARNING, "internal error: files: %d\n", fileno(code));
    fclose(code);
    code = (FILE*)NULL;
  }
  if (code)
    IO::stream[(int)fileno(code)] = code;
  else
    error(bWARNING, "can't open %s\n", fname);
  return code;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

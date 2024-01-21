/*$Id: c_file.cc,v 11.24 96/02/25 14:09:26 al Exp $ -*- C++ -*-
 * log and > commands
 * log == commands log to a file
 * >   == all output to a file (redirect stdout)
 * bare command closes the file
 * can be nested RECURSE deep
 */
#include "ap.h"
#include "error.h"
#include "io.h"
#include "l_lib.h"
#include "c_comand.h"
#include "declare.h"	/* getcmd (self) */
#include "l_astack.h"
/*--------------------------------------------------------------------------*/
//	void	CMD::logger(CS&);
//	void	CMD::file(CS&);
	char*	getcmd(const char*,char*,int);
/*--------------------------------------------------------------------------*/
static int   mout;		/* > file bitmap		*/
static int   mlog;		/* log file bitmap		*/
/*--------------------------------------------------------------------------*/
/* cmd_log: "log" command processing
 * open a file for logging (history)
 * arg is name of file
 * no arg closes the one most recently opened
 * the file will contain a list of commands executed, for use by "<"
 * multiple files can be open, they are nested, output to all.
 */
void CMD::logger(CS& cmd)
{
  static ASTACK<FILE*> filestack(RECURSE);

  if (cmd.more()){			/* a file name .. open it */
    if (filestack.IsFull()){
      error(bWARNING, "too many files open\n");
    }else{
      const char *access;
      access = "w";
      while (cmd.match('>')){
	access = "a";
	cmd.skip();
	cmd.skipbl();
      }
      FILE* newfile = xopen(cmd,"",access);
      if (newfile){
        filestack << newfile;
	mlog |= 1<<fileno(newfile);
      }
    }
  }else{				/* empty command -- close a file */
    if (filestack.IsEmpty()){
      error(bWARNING, "no files open\n");
    }else{
      FILE* oldfile;
      filestack >> oldfile;
      mlog &= ~(1<<fileno(oldfile));
      fclose(oldfile);
    }
  }
}
/*--------------------------------------------------------------------------*/
/* cmd_file: ">" command processing
 * open a file for all output
 * the file will contain a copy of all screen output.
 * arg is name of file
 * no arg closes it
 * the file will contain all that would go to stdout
 */
void CMD::file(CS& cmd)
{
  static ASTACK<FILE*> filestack(RECURSE);
  
  if (cmd.more()){			/* a file name .. open it */
    if (filestack.IsFull()){
      error(bWARNING, "too many files open\n");
    }else{
      const char *access;
      access = "w";
      while (cmd.match('>')){
	access = "a";
	cmd.skip();
	cmd.skipbl();
      }
      FILE* newfile = xopen(cmd,"",access);
      if (newfile){
        filestack << newfile;
	mout        |= 1<<fileno(newfile);
	IO::mstdout |= 1<<fileno(newfile);
      }
    }
  }else{				/* empty command -- close a file */
    if (filestack.IsEmpty()){
      error(bWARNING, "no files open\n");
    }else{
      FILE* oldfile;
      filestack >> oldfile;
      mout        &= ~(1<<fileno(oldfile));
      IO::mstdout &= ~(1<<fileno(oldfile));
      fclose(oldfile);
    }
  }
}
/*--------------------------------------------------------------------------*/
/* getcmd: get a command.
 * if "fin" is stdin, display a prompt first.
 * Also, actually do logging, echo, etc.
 */
char *getcmd(const char *prompt, char *buffer, int buflen)
{
  pllocate();
  mputs( prompt, IO::mstdout );
  mputs( " \b",  IO::mstdout );		/* flush out buffer */
  if (!fgets(buffer, buflen, stdin)){
    error(bEXIT, "\n");
  }
  mputc('\r', IO::mstdout&~mout);	/* reset col counter */
  trim(buffer);
  mprintf(mlog | mout, "%s\n", buffer);
  return buffer;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

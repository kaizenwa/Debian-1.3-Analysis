/*$Id: c_system.cc,v 11.22 96/02/18 11:42:29 al Exp $ -*- C++ -*-
 * system calls: change directory, invoke another program, invoke editor, etc.
 */
#include "ap.h"
#include "error.h"
#include "io.h"
#include "c_comand.h"
#include "declare.h"	/* shell */
/*--------------------------------------------------------------------------*/
//	void	CMD::edit(CS&);
//	void	CMD::system(CS&);
//	void	CMD::chdir(CS&);
/*--------------------------------------------------------------------------*/
extern char e_int[];
/*--------------------------------------------------------------------------*/
/* cmd_edit: (command) invoke user defined editor on the netlist
 * if command has an argument, it edits that file instead
 * else actually edits a temporary file, and reads it back.
 */
void CMD::edit(CS& cmd)
{
  char *editor = getenv("EDITOR");
  if (!editor)
    error(bERROR, "no editor defined\n");
  
  if (cmd.more()){
    char args[BUFLEN];
    sprintf(args, "%s %s\n", editor, cmd.tail());
    ::system(args);
  }else{
    char args[BUFLEN];
    static char name[L_tmpnam] = "";
    if (!*name)
      tmpnam(name);
    sprintf(args, "save %s", name);
    cmdproc(args);
    sprintf(args, "%s %s", editor, name);
    ::system(args);
    sprintf(args, "get %s quiet", name);
    cmdproc(args);
    unlink(name);
  }
}
/*--------------------------------------------------------------------------*/
void CMD::system(CS& cmd)
{
  if (cmd.more())
    ::system(cmd.tail());
  else
    shell();
}
/*--------------------------------------------------------------------------*/
void CMD::chdir(CS& cmd)
{
  char buf[MAXPATHLEN];
  
  if (cmd.more())
    ::chdir(cmd.ctostr(buf, MAXPATHLEN-1, ""));
  if (getcwd(buf,MAXPATHLEN))
    mprintf(IO::mstderr, "%s\n", buf);
  else
    error(bERROR, e_int, "getcwd");
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

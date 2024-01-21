    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "tweak.h"
#include "client_def.h"
#include "c_extern.h"
#include "bsd_extern.h"

static  int     optletter;
extern  int     optind;

static void dont_die PROTO1(int, signum)
{
}
     
static int get_file PROTO4(char *, path, struct stat *, sbufp, int, mode, int, level)
{
  util_download(path,stdout,0);
}

static int rec PROTO3 (char *, name, struct stat *, sbufp, u_long *, mode)
{
    if (*mode != RECURSIVE) return (-1);
    return (0);
}

int main PROTO3(int, argc, char **, argv, char **, envp)
{
  char **av, *av2[2];
  int mode;

  env_client();
  
  signal(SIGPIPE,dont_die);
  if(isatty(1)) client_trace = 0;

/* Parse options
 * -r   recursively show files 
 */
  while ((optletter=getopt(argc, argv,"r")) != EOF)
   switch (optletter)
   {
    case 'r':       
	mode=RECURSIVE;
        break;
   }

  argv+=(optind-1);
 
  while(*++argv) {
    if(!(av = glob(*argv))) {
      av = av2;
      av2[0] = *argv; 
      av2[1] = 0;
    }
    while(*av) util_process_file(*av++, mode, get_file, rec, 0L, 0);
  }
  
  client_done();
  
  exit(0);
}

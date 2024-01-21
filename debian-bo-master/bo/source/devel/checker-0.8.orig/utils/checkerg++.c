/* checkergcc.c - "exec gcc -B/usr/local/lib/checker/ $*" */

#include <unistd.h>

#ifndef GXX_PATH
#define GXX_PATH "/usr/bin/g++"
#endif

#ifndef CHECKER_PATH
#define CHECKER_PATH "/usr/local/lib/checker/"
#endif

/* Number of args added */
#define NEWARGS 1

int
main (int argc, char *argv[], char *envp[])
{
 char *newargv[argc + NEWARGS];
 int i;
 
 newargv[0] = argv[0];
 newargv[1] = "-B" CHECKER_PATH "/";
 
 for (i = 1; i < argc; i++)
   newargv[NEWARGS + i] = argv[i];
   
 execve (GXX_PATH, newargv, envp);
 exit (1);
}

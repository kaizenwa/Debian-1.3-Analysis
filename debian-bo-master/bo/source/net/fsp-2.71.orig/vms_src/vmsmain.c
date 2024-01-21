#include <stdio.h>

int
main(int argc,char **argv,char **envp)
{
char *arg,*here;
int ac=1,count=1;	/* argument counts */
char *av[255]; /* will go wrong when you have more than 255 arguments.. */

av[0]=argv[0]; /* 0th argument always name of command */

while (count<argc)
{
 arg=here=argv[count]; /* get the 1st argument */

 while (*here==' ') { arg++; here++; } /* string leading spaces */

 while (*here)
 {
  if ((*here)==' ') /* arguments separated with spaces */
  { *here=0;	  /* terminate this one */
    av[ac++]=arg; /* add it to the new argument list */
    arg=here+1;   /* hold the next arg. */
  }
  here++;
 }
 if (*arg) av[ac++]=arg; /* last of this argument */
 count++;
}

return(vms_main(ac,av,envp));
}

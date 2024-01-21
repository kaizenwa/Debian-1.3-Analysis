/* LShell 2.01 */
/* Copyright (C) 1995 -- Joel Katz -- Stimpson@Panix.com */
/* Freely Redistributable */
/* Several changes by MM -- Michael Meskes -- meskes@informatik.rwth-aachen.de */

#include <stdio.h>
#include <stdlib.h>
#include <pwd.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#ifndef CONFIG_FILE
#define CONFIG_FILE "/etc/lshell.conf"
#endif

#ifndef SHELL_DIR
#define SHELL_DIR "/bin/shells/"
#endif

/* #define EXEMPT_ROOT */

char *strip(char *ptr)
{ /* return ptr if ptr contains no '/' otherwise return pointer */
  /* to first character after ptr */
  char *ptr2;
  char buf[256];
  buf[0]='/';
  strcpy(buf+1,ptr);
  ptr2=buf+strlen(buf)-1;
  while(*ptr2!='/') ptr2--;
  return ptr+(ptr2-buf);
}

void process(char *buf)
{
 char *pp;
 /* We are passed a string of the form [Fw][Px][Cy][Dz] where */
 /* X is the max number of processes, Y is the max number of */
 /* cpu minutes and Z is the max number of megabytes of virtual memory */
 /* W is the max number of open files */
 int i;
 struct rlimit rlim;
 pp=buf;
 while(*pp!=0)
 {
  int i;
  
  switch(*pp++)
  {
   case 'f':
   case 'F': i=atoi(pp);
   	     if(!i) break;
             rlim.rlim_cur=i;
             rlim.rlim_max=i;
             setrlimit(RLIMIT_NOFILE,&rlim);
             break;
   case 'p':
   case 'P': i=atoi(pp);
   	     if(!i) break;
             rlim.rlim_cur=i;
             rlim.rlim_max=i;
             setrlimit(RLIMIT_NPROC,&rlim);
   	     break;
   case 'c':
   case 'C': i=atoi(pp);
             if(!i) break;
             i*=60;
             rlim.rlim_cur=i;
             rlim.rlim_max=i;
             setrlimit(RLIMIT_CPU,&rlim);
             break;
   case 'd':
   case 'D': i=atoi(pp);
   	     if(!i) break;
   	     i*=1024*1024;
   	     rlim.rlim_cur=i;
   	     rlim.rlim_max=i;
   	     setrlimit(RLIMIT_DATA,&rlim);
             break;
  }
 }
}

void set_em(char *unm)
{
 FILE *fil;
 int i;
 char buf[1024],name[1024],limits[1024];
 fil=fopen(CONFIG_FILE,"r");
 if(fil==NULL)
 {
  return;
 }
 while(fgets(buf,1024,fil)!=NULL)
 {
  i=sscanf(buf,"%s %s",name,limits);
  if(!strcmp(name,unm))
  {
   if(i==2) process(limits);
   fclose(fil);
   return;
  }
 }
 fclose(fil);
 process(limits); /* Last line is default */
}

void main(int argc, char *argv[], char *environ[])
{
 int i;
 char *ptr, **nargv, *shell;
#if 0
 char full[64];
#endif
 struct passwd *j;
 char **env;

  /* correct SHELL environment variable and learn who we are MM */ 

  /* (HS) Behaviour like the `old' lshell, but w/o destroying the
   * environment.  (The `old' lshell destroyed the TERM environment,
   * under some cirumstances.  */
  ptr = getenv("SHELL");
  if (ptr) {
	shell = strdup(ptr);
	if (ptr = strstr(shell, "/lshells"))  {
		i = strlen("/lshells");
		memmove(ptr, ptr + i, 1 + strlen(shell) - i - (ptr - shell));
	}
	setenv("SHELL", shell, 1);
  } else shell = NULL;
    
 if (argv[0][0]=='-')
 { /* We are a login shell */
  j=getpwuid(getuid());
  ptr=j->pw_name;

#ifdef EXEMPT_ROOT
  if(strcmp(ptr,"root"))
#endif
  if(ptr!=NULL)
   set_em(ptr);
#if 0 /* original code */
  strcpy(full,SHELL_DIR);
  strcat(full,strip(argv[0]+1));
#endif
 }
 else
 {
  /* We aren't a login shell
  /* We need to assemble our new name and pass our parameters */
  
#if 0 /* original code */
  strcpy(full,SHELL_DIR);
  strcat(full,strip(argv[0]));
#endif
 }
 nargv=malloc((argc+1)*sizeof(char *));
 for(i=1; i<argc; i++) nargv[i]=argv[i];
 nargv[argc]=NULL; /* argv is not null terminated :-( */
 nargv[0]=strip(argv[0]);
#if 0 /* original code */
 execv(full,(char **) nargv);
#endif
 execve(shell,(char **) nargv, environ);
 fprintf(stderr, "Shell %s does not exist. Falling back to sh.\n",shell);
 execve("/bin/sh",(char **) nargv, environ);
}

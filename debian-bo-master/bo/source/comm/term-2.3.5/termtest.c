/*  test.c - start two connected term daemons for testing purposes 

This program is for testing term. It starts two term daemons that are
connected. The "local" one listens on server (socket) name "" and
writes its output to "local.log". The "remote" one listens on server
name "remote", is started with the -r switch and writes its output to
"remote.log". This means that clients started without special
arguments connect to the "local" term daemon.

"test" understands the following options:
-1 program     - use "program"as the primary term daemon (default ./term)
-local option  - use "option" for the primary term daemon
-2 program     - use "option" as the secondary term daemon (default ./term)
-remote option - use "option" for the secondary term daemon

All other options will be given to both daemons.

This program prints the actual command lines that are started for both
daemons, then backgrounds itself. Killing either of its processes
or using "tshutdown" kills all of them. */

#define I_ERRNO
#define I_IOCTL
#define I_SIGNAL
#include "includes.h"

int childa=0, childb=0;

static void killall(int i) {
  kill(childa, SIGKILL);
  kill(childb, SIGKILL);
  exit(0);
}

int main(int argc, char *argv[]) {
    int i,j, use_defaults=1;
    int soc[2];
    char path[256];
    char *args[256];

#ifdef SOCKS
  SOCKSinit(argv[0]);
#endif
    S_Pipe(soc);

    if (!(childa = x__fork())) {
	sprintf(path, "%s/.term/socket", getenv("HOME"));
	unlink(path);
        args[(j=0)] = "./term";
        args[++j] = "-s";
        args[++j] = "off";
        for (i = 1; i < argc; ++i)
            if (!strcmp(argv[i],"-1")) {
              args[0] = argv[++i];
              j = -i;
              use_defaults = 0;
            }else if (!strcmp(argv[i],"--local")) {
              j--;
            }else if (!strcmp(argv[i],"--remote")||!strcmp(argv[i],"-2")) {
              i++;
              j -= 2;
            }else {
              args[i+j] = argv[i];
            }
        args[i+j] = NULL;
	sleep(1);
	x__dup2(soc[0], 0);
	x__dup2(soc[0], 1);
       	i = open("local.log", O_RDWR | O_CREAT | O_TRUNC , 0666);
	if (i < 0) {
	  x__perror("Open");
	  exit(1);
	}
	x__dup2(i, 2);
	fprintf(stderr,"Executing (%d): %s",getpid(),args[0]);
	for (i=1;args[i] != NULL; ++i) fprintf(stderr," %s",args[i]);
        fprintf(stderr,"\n");
	sleep(1);
	execv(args[0], args);
	x__perror("local execv failed");
	exit(1);
	}
    if (!(childb=x__fork())) {
	sprintf(path, "%s/.term/sockettest", getenv("HOME"));
	unlink(path);
        args[(j=0)] = "./term";
        args[++j] = "-r";
        args[++j] = "-s";
        args[++j] = "off";
        for (i = 1; i < argc; ++i)
            if (!strcmp(argv[i],"-2")) {
              args[0] = argv[++i];
              j = -i;
	      use_defaults = 0;
            }else if (!strcmp(argv[i],"--remote")) {
              j--;
            }else if (!strcmp(argv[i],"--local")||!strcmp(argv[i],"-1")) {
              i++;
              j -= 2;
            }else {
              args[i+j] = argv[i];
            }
        if (use_defaults) args[i+(j++)] = "test";
        args[i+j] = NULL;
	x__dup2(soc[1], 0);
	x__dup2(soc[1], 1);
       	i = open("remote.log", O_RDWR | O_CREAT | O_TRUNC , 0666);
	if (i < 0) {
	  x__perror("Open");
	  exit(1);
	}
	x__dup2(i, 2);
	fprintf(stderr,"Executing (%d): %s",getpid(),args[0]);
	for (i=1;args[i] != NULL; ++i) fprintf(stderr," %s",args[i]);
        fprintf(stderr,"\n");
	execv(args[0], args);
	x__perror("remote execv failed");
	exit(1);
	}
    signal(SIGCHLD, killall);
    signal(SIGTERM, killall);
    signal(SIGHUP, killall);
    signal(SIGQUIT, killall);
    signal(SIGTERM, killall);
    pause();
    killall(0);
    exit(0);
}
	

/* killall.c - kill processes by name or list PIDs */

/* Copyright 1993-1997 Werner Almesberger. See file COPYING for details. */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "comm.h"
#include "signals.h"


#define PROC_BASE "/proc"
#define MAX_NAMES (sizeof(unsigned long)*8)


static int verbose = 0,exact = 0,interactive = 0,quiet = 0,pidof;


static int ask(char *name,pid_t pid)
{
    int ch,c;

    do {
        printf("Kill %s(%d) ? (y/n) ",name,pid);
        fflush(stdout);
	do if ((ch = getchar()) == EOF) exit(0);
	while (ch == '\n' || ch == '\t' || ch == ' ');
	do if ((c = getchar()) == EOF) exit(0);
	while (c != '\n');
    }
    while (ch != 'y' && ch != 'n' && ch != 'Y' && ch != 'N');
    return ch == 'y' || ch == 'Y';
}


static int kill_all(int signal,int names,char **namelist)
{
    DIR *dir;
    struct dirent *de;
    FILE *file;
    struct stat st,sts[MAX_NAMES];
    int *name_len;
    char path[PATH_MAX+1],comm[COMM_LEN];
    char command_buf[PATH_MAX+1];
    char *command;
    pid_t pid,self;
    int empty,i,okay,length,got_long;
    unsigned long found;

    if (!(name_len = malloc(sizeof(int)*names))) {
	perror("malloc");
	exit(1);
    }
    for (i = 0; i < names; i++)
	if (!strchr(namelist[i],'/')) {
	    sts[i].st_dev = 0;
	    name_len[i] = strlen(namelist[i]);
	}
	else if (stat(namelist[i],&sts[i]) < 0) {
		perror(namelist[i]);
		exit(1);
	    }
    self = getpid();
    found = 0;
    if (!(dir = opendir(PROC_BASE))) {
	perror(PROC_BASE);
	exit(1);
    }
    empty = 1;
    while (de = readdir(dir)) {
	if (!(pid = atoi(de->d_name)) || pid == self) continue;
	sprintf(path,"%s/%d/stat",PROC_BASE,pid);
	if (!(file = fopen(path,"r"))) continue;
	empty = 0;
	okay = fscanf(file,"%*d (%[^)]",comm) == 1;
	(void) fclose(file);
	if (!okay) continue;
	got_long = 0;
	command = NULL; /* make gcc happy */
	length = strlen(comm);
	if (length == COMM_LEN-1) {
	    sprintf(path,"%s/%d/cmdline",PROC_BASE,pid);
	    if (!(file = fopen(path,"r"))) continue;
	    okay = fscanf(file,"%s",command_buf) == 1;
	    (void) fclose(file);
	    if (exact && !okay) {
		if (verbose)
		    fprintf(stderr,"skipping partial match %s(%d)\n",comm,pid);
		continue;
	    }
	    got_long = okay;
	    if (okay) {
		command = strrchr(command_buf,'/');
		if (command) command++;
		else command = command_buf;
	    }
	}
	for (i = 0; i < names; i++) {
	    if (!sts[i].st_dev) {
		if (length != COMM_LEN-1 || name_len[i] < COMM_LEN-1) {
		    if (strcmp(namelist[i],comm)) continue;
		}
		else if (got_long ? strcmp(namelist[i],command) :
		      strncmp(namelist[i],comm,COMM_LEN-1)) continue;
	    }
	    else {
		sprintf(path,"%s/%d/exe",PROC_BASE,pid);
		if (stat(path,&st) < 0) continue;
		if (sts[i].st_dev != st.st_dev || sts[i].st_ino != st.st_ino)
		    continue;
	    }
	    if (interactive && !ask(comm,pid)) continue;
	    if (pidof) {
		if (found) putchar(' ');
		printf("%d",pid);
		found |= 1 << i;
	    }
	    else if (kill(pid,signal) >= 0) {
		    if (verbose)
			fprintf(stderr,"Killed %s(%d)\n",got_long ? comm :
			  command,pid);
		    found |= 1 << i;
		}
		else if (errno != ESRCH || interactive)
			fprintf(stderr,"%s(%d): %s\n",got_long ? comm :
			  command,pid,strerror(errno));
	}
    }
    (void) closedir(dir);
    if (empty) {
	fprintf(stderr,PROC_BASE " is empty (not mounted ?)\n");
	exit(1);
    }
    if (!quiet && !pidof)
	for (i = 0; i < names; i++)
	    if (!(found & (1 << i)))
		fprintf(stderr,"%s: no process killed\n",namelist[i]);
    if (pidof) putchar('\n');
    return found == ((1 << (names-1)) | ((1 << (names-1))-1)) ? 0 : 1;
}


static void usage_pidof(void)
{
    fprintf(stderr,"usage: pidof [ -e ] name ...\n");
    fprintf(stderr,"       pidof -V\n\n");
    fprintf(stderr,"    -e      require exact match for very long names;\n");
    fprintf(stderr,"            skip if the command line is unavailable\n");
    fprintf(stderr,"    -V      display version information\n\n");
}


static void usage_killall(void)
{
    fprintf(stderr,"usage: killall [ -eiqv ] [ -signal ] name ...\n");
    fprintf(stderr,"       killall -l\n");
    fprintf(stderr,"       killall -V\n\n");
    fprintf(stderr,"    -e      require exact match for very long names;\n");
    fprintf(stderr,"            skip if the command line is unavailable\n");
    fprintf(stderr,"    -i      ask for confirmation before killing\n");
    fprintf(stderr,"    -l      list all known signal names\n");
    fprintf(stderr,"    -q      quiet; don't print complaints\n");
    fprintf(stderr,"    -signal send signal instead of SIGTERM\n");
    fprintf(stderr,"    -v      report if the signal was successfully sent\n");
    fprintf(stderr,"    -V      display version information\n\n");
}


static void usage(void)
{
    if (pidof) usage_pidof();
    else usage_killall();
    exit(1);
}


int main(int argc,char **argv)
{
    char *name,*walk;
    int sig_num;

    name = strrchr(*argv,'/');
    if (name) name++;
    else name = *argv;
    pidof = strcmp(name,"killall");
    if (argc == 2 && !strcmp(argv[1],"-l")) {
	if (pidof) usage();
	list_signals();
	return 0;
    }
    if (argc == 2 && !strcmp(argv[1],"-V")) {
	fprintf(stderr,"%s from psmisc version " PSMISC_VERSION "\n",
	  pidof ? "pidof" : "killall");
	return 0;
    }
    sig_num = SIGTERM;
    while (argc > 1 && *argv[1] == '-') {
	argc--;
	argv++;
	if (**argv == '-') {
	    for (walk = *argv+1; *walk && strchr("eiqv",*walk); walk++) {
		switch (*walk) {
		    case 'e':
			exact = 1;
			break;
		    case 'i':
			if (pidof) usage();
			interactive = 1;
			break;
		    case 'q':
			if (pidof) usage();
			quiet = 1;
			break;
		    case 'v':
			if (pidof) usage();
			verbose = 1;
			break;
		}
	    }
	    if (*walk)
		if (walk != *argv+1 || pidof) usage();
		else sig_num = get_signal(*argv+1,"killall");
	}
    }
    if (argc < 2) usage();
    if (argc > MAX_NAMES+1) {
	fprintf(stderr,"Maximum number of names is %d\n",MAX_NAMES);
	exit(1);
    }
    return kill_all(sig_num,argc-1,argv+1);
}

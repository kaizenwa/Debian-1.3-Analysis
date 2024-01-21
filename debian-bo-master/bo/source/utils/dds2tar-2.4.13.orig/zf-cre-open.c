
/*
 * This file is part of dds2tar.
 * Copyright by J"org Weule
 *
 * Copyright: GPL
 */

/*
 * If you compile this file with -DTEST, you will get a test program:
 *
 *     cc dds_fio.c -DTEST -o dds_fio && dds_fio
 *
 * The test shows the use of ccopen(...) to pipe stdout through /bin/grep.
 *
 * The interface should be useful in many cases.
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/mtio.h>
#include <string.h>
#include <unistd.h>		/* pipe() ... */
#include <sys/types.h>		/* size_t, open() */
#include <sys/wait.h>		/* wait() */
#include <sys/stat.h>		/* open() */
#include <fcntl.h>		/* open() */

#include "zf-cre-open.h"

#define streq(a,b) (!strcmp((a),(b)))

FILE   *
zfopen(
	      char const *const name,
	      int const compressed,
	      char const *const open_mode
)
{

	FILE   *fp;
	char   *b;

	b = malloc(1024);
	if (b == NULL)
		exit(20);

	if (compressed == T_MODE) {
		if ((name != NULL) && (strcmp(name, "-"))) {
			fp = fopen(name, open_mode);
			if (fp == NULL) {
				perror("dds2tar");
				exit(21);
			}
		} else if (streq(open_mode, "w")) {
			fp = stdout;
		} else {
			fp = stdin;
		}
		return fp;
	}
	if (streq(open_mode, "w")) {
		strcpy(b, "gzip ");
		if ((name != NULL) && (strcmp(name, "-"))) {
			strcat(b, " > ");
			strcat(b, name);
		}
	} else if (streq(open_mode, "r")) {
		strcpy(b, "gunzip --force --decompress --stdout ");
		if ((name != NULL) && (strcmp(name, "-"))) {
			strcat(b, name);
		}
	}
	fp = popen(b, open_mode);
	if (fp == NULL) {
		perror("dds2tar");
		exit(22);
	}
	free(b);
	return fp;
}


int
cclose(int const fd)
{
	int     status = 0;

	close(fd);
	wait(&status);
	return status;
}

int
reopen(
	      int const stdfd,
	      char const *const output_file,
	      int const mode,
	      int const flags
)
{
	int     fd;

	if (!strcmp(output_file, "-")) {
		/* use stdout */
		fd = 1;
	} else {
		/* open file */
		if ((fd = open(output_file, mode, flags)) == -1) {
			perror("dds2tar");
			exit(23);
		}
		/* don't know why this happens. Any idea? */
		if (fd == 0) {	/* normally 0 is stdin!! */
			perror("dds2tar");
			exit(24);
		}
	}
	if (0 <= stdfd) {
		dup2(fd, stdfd);
		close(fd);
		fd = stdfd;
	}
	return fd;
}

/*
 * creopen opens a pipe to a child process with the file number 'stdfd'
 * on file number 'stdfd_child' of the child.
 * The files are close as needed (see dup2(2)).
 */

int
creopen(
	       int const stdfd_parent,	/* stdfd should not be == 3 */
	       int const stdfd_child,	/* stdfd_child must be 1 or 2 */
	       char const *const filename,
	       char const *const *argv
)
{
	int     fd;
	int     pid;
	int     pipefd[2];
	int     pipe_parent;

	if ((stdfd_child & 0xfffffffe) != 0 || stdfd_child == stdfd_parent) {
		fprintf(stderr, "creopen not useful with that parameters\n");
		exit(25);
	}
	pipe_parent = stdfd_child ^ 1;
	if (pipe(pipefd) < 0) {	/* create pipe with two fd's */
		perror("dds2tar");
		exit(26);
	}
	if ((pid = fork()) == 0) {	/* we are the child process */
		/* reconnect pipe to child */
		dup2(pipefd[stdfd_child], stdfd_child);
		close(pipefd[0]);	/* close input of pipe */
		close(pipefd[1]);	/* close output of pipe */
		/* the prototype of execv is wrong */
		execv(filename, (char *const *) argv);
		perror("dds2tar");
		exit(27);
	}
	if (pid <= 0) {
		perror("dds2tar");
		exit(28);
	}
	/*
	 * We are the parent process.
	 */
	close(stdfd_child);
	fd = pipefd[pipe_parent];
	if ((0 <= stdfd_parent) && (stdfd_parent <= 1)) {
		dup2(fd, stdfd_parent);
		close(fd);
		fd = stdfd_parent;
		close(pipefd[0]);
		close(pipefd[1]);
	}
	return fd;
}

#ifdef TEST

static char *a[] =
{"grep", "allo", NULL};

main(int argc, char **argv, char **envp)
{

	int     status;
	int     pid;
	int     fd;

	if (!strcmp(argv[1], "-p")) {
	} else {
		if (!strcmp(argv[1], "-o"))
			reopen(1, argv[2], O_RDONLY, 0);
		else
			creopen(1, 0, "/usr/bin/grep", a);
		write(1, "Hallo World -1- \n", 17);
		write(1, "Hi    World -2- \n", 17);
		write(1, "Hallo World -3- \n", 17);
		write(1, "Morgen Welt -4- \n", 17);
		write(1, "Hallo World -5- \n", 17);
		printf("Hallo World =1= \n");
		printf("Hi    World =2= \n");
		printf("Hallo World =3= \n");
		printf("Morgen Welt =4= \n");
		printf("Hallo World =5= \n");
		fflush(stdout);
		if (!strcmp(argv[1], "-o")) {
			close(1);
		} else {
			cclose(1);
		}
	}
}

#endif

/*
 * realpath.c -- output the real path of a filename.
 * Lars Wirzenius.
 */
 
#include <sys/param.h>
#include <unistd.h>
#include <stdio.h>

int main(int argc, char **argv) {
	char buf[10240];
	int i;

	if (argc != 2) {
		fprintf(stderr, "usage: %s filename ...\n", argv[0]);
		return 1;
	}

	for (i = 1; i < argc; ++i) {	
		if (realpath(argv[i], buf) == NULL) {
			perror("realpath");
			return 1;
		}
		
		printf("%s\n", buf);
		fflush(stdout);
		if (ferror(stdout)) {
			perror("stdout");
			return 1;
		}
	}

	return 0;
}

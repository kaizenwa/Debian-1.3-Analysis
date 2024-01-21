#include "internal.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <setjmp.h>

static int	tryopen(const char * name, int doRead, int openForWrite);

const char	tryopen_usage[] = "tryopen [-r] file [file ...]";

int
tryopen_main(struct FileInfo * i, int argc, char * * argv)
{
	int	openedOne = 0;
	int	doRead = 0;
	int openForWrite = 0;

	sync();	/* Just in case I crash the system by opening a bad device */

	while ( argc > 1 && argv[1][0] == '-' ) {
		if ( argv[1][1] == 'r' && argv[1][2] == '\0' ) {
			doRead = 1;
			argc--;
			argv++;
		}
		else if ( argv[1][1] == 'w' && argv[1][2] == '\0' ) {
			openForWrite = 1;
			argc--;
			argv++;
		}
		else
			break;
	}

	while ( argc > 1 ) {
		if ( tryopen(argv[1], doRead, openForWrite) ) {
			printf("%s\n", argv[1]);
			fflush(stdout);
			openedOne = 1;
		}
		argc--;
		argv++;
	}
	fprintf(stderr, "\rDone.                     \n");

	exit ( !openedOne );
}

static jmp_buf	jmpBuf;

static void
gotAlarm(int signalNumber)
{
	longjmp(jmpBuf, 1);
}

static int
tryopen(const char * name, int doRead, int openForWrite)
{

	fprintf(stderr, "\rTrying %s           ", name);
	if ( setjmp(jmpBuf) == 0 ) {
		int	fd;

		signal(SIGALRM, gotAlarm);
		alarm(10);
		fd = open(name, openForWrite ? O_RDWR : O_RDONLY);
		alarm(0);
		if ( fd >= 0 ) {
			int		status = 1;
			char	c;

			if ( doRead )
				status = read(fd, &c, 1);

			close(fd);

			return status;
		}
	}
	return 0;
}

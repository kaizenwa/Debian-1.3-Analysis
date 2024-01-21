/*
 * disalloc.c - aeb - 940501 - Disallocate virtual terminal(s)
 */
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <linux/vt.h>
#include <stdio.h>

extern int getfd(void);
char *progname;

void
usage(){
    fprintf(stderr, "usage: %s [N1 N2 ...]\n", progname);
    exit(1);
}

int
main(int argc, char *argv[]) {
    int fd, num, i;

    if (argc < 1)		/* unlikely */
      exit(1);
    progname = argv[0];

    fd = getfd();

    if (argc == 1) {
	/* disallocate all unused consoles */
	if (ioctl(fd,VT_DISALLOCATE,0)) {
	    perror("VT_DISALLOCATE");
	    fprintf(stderr,
		    "%s: disallocating all unused consoles failed\n",
		    progname);
	    exit(1);
	}
    } else
    for (i = 1; i < argc; i++) {
	num = atoi(argv[i]);
	if (num == 0)
	    fprintf(stderr, "%s: 0: illegal VT number\n", progname);
	else if (num == 1)
	    fprintf(stderr,
		    "%s: VT 1 is the console can cannot be disallocated\n",
		    progname);
	else
	if (ioctl(fd,VT_DISALLOCATE,num)) {
	    perror("VT_DISALLOCATE");
	    fprintf(stderr, "%s: could not disallocate console %d\n",
		    progname, num);
	    exit(1);
	}
    }
    exit(0);
}

/*
 * clrunimap.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <linux/kd.h>
#include <sys/ioctl.h>

extern int getfd(void);

int
main(int argc, char *argv[]) {
	struct unimapinit advice;
	int fd;

	fd = getfd();

	advice.advised_hashsize = 0;
	advice.advised_hashstep = 0;
	advice.advised_hashlevel = 0;

	if(ioctl(fd, PIO_UNIMAPCLR, &advice)) {
	    perror("PIO_UNIMAPCLR");
	    exit(1);
	}
}

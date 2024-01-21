/*
 * chvt.c - aeb - 940227 - Change virtual terminal
 */
#include <sys/types.h>
#include <sys/ioctl.h>
#include <linux/vt.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

extern int getfd(void);

int
main(int argc, char *argv[]) {
    int fd, num;

    if (argc != 2) {
	fprintf(stderr, "usage: chvt N\n");
	exit(1);
    }
    fd = getfd();
    num = atoi(argv[1]);
    if (ioctl(fd,VT_ACTIVATE,num)) {
	perror("chvt: VT_ACTIVATE");
	exit(1);
    }
    if (ioctl(fd,VT_WAITACTIVE,num)) {
	perror("VT_WAITACTIVE");
	exit(1);
    }
    exit(0);
}

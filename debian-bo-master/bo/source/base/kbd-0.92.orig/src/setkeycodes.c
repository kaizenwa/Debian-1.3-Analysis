/*
 * call: setkeycode scancode keycode ...
 *  (where scancode is either xx or e0xx, given in hexadecimal,
 *   and keycode is given in decimal)
 *
 * aeb, 941108
 */
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/kd.h>

extern int getfd();

void
usage(char *s) {
    fprintf(stderr, "setkeycode: %s\n", s);
    fprintf(stderr, "\
usage: setkeycode scancode keycode ...\n\
 (where scancode is either xx or e0xx, given in hexadecimal,
  and keycode is given in decimal)\n");
    exit(1);
}

int
main(int argc, char **argv) {
    char *ep;
    int fd, sc;
    struct kbkeycode a;

    if (argc % 2 != 1)
      usage("even number of arguments expected");
    fd = getfd();

    while (argc > 2) {
	a.keycode = atoi(argv[2]);
	a.scancode = sc = strtol(argv[1], &ep, 16);
	if (*ep)
	  usage("error reading scancode");
	if (a.scancode > 127) {
	    a.scancode -= 0xe000;
	    a.scancode += 128;
	}
	if (a.scancode > 255 || a.keycode > 127)
	  usage("code outside bounds");
	if (ioctl(fd,KDSETKEYCODE,&a)) {
	    perror("KDSETKEYCODE");
	    fprintf(stderr, "failed to set scancode %x to keycode %d\n",
		    sc, a.keycode);
	    exit(1);
	}
	argc -= 2;
	argv += 2;
    }
    return 0;
}

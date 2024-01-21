/*
 * setvesablank.c - aeb - 941230
 *
 * usage: setvesablank ON|on|off
 */
#include <stdio.h>
#include <linux/termios.h>

extern int getfd();

main(int argc, char *argv[]) {
    int fd;
    struct { char ten, onoff; } arg;

    if (argc != 2) {
       fprintf(stderr, "usage: setvesablank ON|on|off\n");
       exit(1);
    }
    fd = getfd();
    arg.ten = 10;
    arg.onoff = 0;
    if (!strcmp(argv[1], "on"))
      arg.onoff = 1;
    else if (!strcmp(argv[1], "ON"))
      arg.onoff = 2;
    if (ioctl(fd, TIOCLINUX, &arg)) {
       perror("setvesablank: TIOCLINUX");
       exit(1);
    }
    exit(0);
}

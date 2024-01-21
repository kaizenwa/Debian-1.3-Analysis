/*
 * Tiny test program for the `spawn console' key
 * (should not use signal; should not use sleep)
 * aeb - 941025
 *
 */
#include <signal.h>
#include <linux/kd.h>

void
sighup(){
    system("open -s -l -- login -h spawn");
    signal(SIGHUP, sighup);
}

main(){
    int fd;

    fd = open("/dev/console", 0);
    if (fd < 0)
      fd = 0;
    signal(SIGHUP, sighup);
    ioctl(fd, KDSIGACCEPT, (long) SIGHUP);
    while(1)
      sleep(3600);
}

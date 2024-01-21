#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<unistd.h>
#include	<sys/vt.h>
#include	<fcntl.h>
#include	<signal.h>
#include	<termio.h>
#include	<sys/stat.h>
#include	<sys/ioctl.h>
#include	<sys/kd.h>

u_int a;

main()
{
    struct stat chkbuf;
    int major, minor;
    int fd;

/*
    printf("%X,", ioctl(0, KDSETMODE, KD_GRAPHICS));
    printf("%X,%X\n", ioctl(0, KDGETMODE, &a), a);
    printf("%X,", ioctl(0, KDSETMODE, KD_TEXT));
*/
    printf("%X,%X\n", ioctl(0, KDGETMODE, &a), a);

#if 0
    fd = dup(2);	/* stderr */
    fstat(2, &chkbuf);
    major = chkbuf.st_rdev >> 8;
    minor = chkbuf.st_rdev & 0xff;	/* console number */

    printf("major = %d, minor = %d\n", major, minor);
    if (major != 4 || minor >= 64) {
	printf("Not running in graphics-capable virtual console.\n");
	exit(-1);
    }
    close(fd);
#endif
}

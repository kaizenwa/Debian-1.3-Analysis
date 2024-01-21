/*
 * fgconsole.c - aeb - 960123 - Print foreground console
 */
#include <sys/ioctl.h>
#include <linux/vt.h>

int
main(){
    struct vt_stat vtstat;
    /* replace 0 by getfd() for a somewhat more robust version */
    if (ioctl(0, VT_GETSTATE, &vtstat)) {
        perror("fgconsole: VT_GETSTATE");
	exit(1);
    }
    printf("%d\n", vtstat.v_active);
    return 0;
}

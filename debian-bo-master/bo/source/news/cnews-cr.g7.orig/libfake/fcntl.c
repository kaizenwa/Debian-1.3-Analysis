#include <stdio.h>
#include <sys/types.h>
#include <sgtty.h>
#include <fcntl.h>

/*
 * fake fcntl(), just smart enough to do set-close-on-exec
 *
 * Advanced cleverness here...  Our <fcntl.h> defines fcntl to cnewsfcntl.
 * If you use our <fcntl.h> but not our fcntl.c, the linker will barf.  If
 * you use the system <fcntl.h>, presumably indicating that you have a
 * real fcntl(), it will get called, not this.
 */
void
cnewsfcntl(fd, code, value)
int fd;
int code;
int value;
{
	if (code == F_SETFD && value == 1)
		(void) ioctl(fd, FIOCLEX, (char *)NULL);
	else
		error("invalid call to faked fcntl()", "");
}

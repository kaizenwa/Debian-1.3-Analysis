#include <termios.h>
#include <sys/ioctl.h>

int
__tcgetattr(int fildes, struct termios *termios_p)
{
	return __ioctl(fildes, TCGETS, termios_p);
}

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__tcgetattr, tcgetattr);
#endif

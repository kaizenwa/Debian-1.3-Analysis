#include <termios.h>

speed_t cfgetospeed(struct termios *tp)
{
    return (tp->c_cflag & CBAUD);
}

speed_t cfgetispeed(struct termios *tp)
{
    return (tp->c_cflag & CBAUD);
}

int cfsetospeed(struct termios *tp, speed_t speed)
{
#ifdef B460800
#define _BMAX B460800
#else
#define _BMAX B230400
#endif

#ifdef CBAUDEX
    if ((speed & ~CBAUD) || 
	((speed & CBAUDEX) && (speed < B57600 || speed > _BMAX)))
	return 0;
#else
    if (speed & ~CBAUD)
	return 0;
#endif
    tp->c_cflag &= ~CBAUD;
    tp->c_cflag |= speed;

    return 0;
}

int cfsetispeed(struct termios *tp, speed_t speed)
{
    return cfsetospeed(tp, speed);
}

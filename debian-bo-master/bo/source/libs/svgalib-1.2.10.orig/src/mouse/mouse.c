/* hhanemaa@cs.ruu.nl */
/* Mouse library functions */

#include <signal.h>
#include <sys/time.h>
#include <sys/types.h>
#include <vga.h>
#include "vgamouse.h"

static void (*__mouse_eventhandler) (int, int, int);

#include "ms.c"			/* include low-level mouse driver */


static struct sigaction oldsiga;
static void (*currentinthandler) (int);

static void inthandler(int signal)
{
    mouse_close();
    /* restore old interrupt */
    sigaction(SIGINT, &oldsiga, NULL);
    raise(SIGINT);
}

static void default_handler(int, int, int);

int mouse_init_return_fd(char *dev, int type, int samplerate)
{
    struct sigaction siga;

    if (strcmp(dev, "") == 0)
	mdev = "/dev/mouse";
    else
	mdev = dev;
    mtype = type & MOUSE_TYPE_MASK;
    m_modem_ctl = type & ~MOUSE_TYPE_MASK;

    msample = samplerate;

    currentinthandler = NULL;

    /* Initialize mouse device. */
    if (mtype < MOUSE_MICROSOFT || mtype > MOUSE_LOGIMAN)
	return -1;
    if (ms_init())
	return -1;

    /* Install default mouse handler. Old coordinates are preserved. */
    __mouse_eventhandler = default_handler;

    /* Install interrupt handler. */
    currentinthandler = inthandler;
    siga.sa_handler = inthandler;
    siga.sa_flags = 0;
    siga.sa_mask = 0;
    sigaction(SIGINT, &siga, &oldsiga);

    return __svgalib_mouse_fd;	/* Return mouse fd. */
}

/* Old compatible mouse_init, returns 0 if succesful, -1 on error. */

int mouse_init(char *dev, int type, int samplerate)
{
    if (mouse_init_return_fd(dev, type, samplerate) == -1)
	return -1;
    else
	return 0;
}

/* For now, we assume there's no console switching. */

int mouse_update()
{
    int result;
    result = get_ms_event(0);
    return result;
}

void mouse_waitforupdate()
{
#if 0
    fd_set *readfds, writefds, exceptfds;
    struct timeval timeout;
    FD_ZERO(readfds);
    FD_ZERO(writefds);
    FD_ZERO(exceptfds);
    FD_SET(__svgalib_mouse_fd, readfds);
    /* need to setup timeout. */
    select(readfds, writefds, exceptfds, &timeout);
#else
    get_ms_event(1);
#endif
    return;
}

void mouse_seteventhandler(void (*handler) (int, int, int))
{
    __mouse_eventhandler = handler;
}

void mouse_close()
{
    ms_close();
    if (currentinthandler != NULL)
	/* Restore old interrupt. */
	sigaction(SIGINT, &oldsiga, NULL);
}


/* Default event handler. */

void mouse_setdefaulteventhandler()
{
    __mouse_eventhandler = default_handler;
}

static int mouse_x = 0;
static int mouse_y = 0;
static int mouse_button = 0;
static int scale = 1;
static int minx = 0;
static int maxx = 32767;
static int miny = 0;
static int maxy = 32767;
static int wrap = 0;

void mouse_setwrap(int state)
{
    wrap = state;
}

static void default_handler(int button, int dx, int dy)
{
    mouse_button = button;
    mouse_x += dx;
    mouse_y += dy;
    if (mouse_x / scale > maxx)
	if (wrap & MOUSE_WRAPX)
	    mouse_x -= (maxx - minx) * scale;
	else
	    mouse_x = maxx * scale;
    /* - 1; ??? */
    if (mouse_x / scale < minx)
	if (wrap & MOUSE_WRAPY)
	    mouse_x += (maxx - minx) * scale;
	else
	    mouse_x = minx * scale;
    if (mouse_y / scale > maxy)
	if (wrap & MOUSE_WRAPX)
	    mouse_y -= (maxy - miny) * scale;
	else
	    mouse_y = maxy * scale;
    /*  - 1; ??? */
    if (mouse_y / scale < miny)
	if (wrap & MOUSE_WRAPY)
	    mouse_y += (maxy - miny) * scale;
	else
	    mouse_y = miny * scale;
}

void mouse_setposition(int x, int y)
{
    mouse_x = x;
    mouse_y = y;
}

void mouse_setxrange(int x1, int x2)
{
    minx = x1;
    maxx = x2;
}

void mouse_setyrange(int y1, int y2)
{
    miny = y1;
    maxy = y2;
}

void mouse_setscale(int s)
{
    scale = s;
}

int mouse_getx()
{
    return mouse_x / scale;
}

int mouse_gety()
{
    return mouse_y / scale;
}

int mouse_getbutton()
{
    return mouse_button;
}

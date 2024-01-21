/* Written by Michael Weller and Harm Hanemaayer. */


#include "stdarg.h"
#include "vga.h"
#include "driver.h"
#include "timing.h"
#include "accel.h"


/*
 * This calls one of the acceleration interface functions.
 */

int vga_accel(unsigned operation,...)
{
    va_list params;

    va_start(params, operation);

    /* Do a quick availability check to avoid disasters. */
    if (driverspecs->accelspecs == 0)
	return -1;
    /* Check for operation availability flag. */
    if (!(driverspecs->accelspecs->operations & (1 << (operation - 1))))
	return -1;

    vga_lockvc();

    /*
     * gcc doesn't produce glorious code here, it's much better with
     * only one va_arg traversal in a function.
     */

    switch (operation) {
    case ACCEL_FILLBOX:
	{
	    int x, y, w, h;
	    x = va_arg(params, int);
	    y = va_arg(params, int);
	    w = va_arg(params, int);
	    h = va_arg(params, int);
	    (*driverspecs->accelspecs->FillBox) (x, y, w, h);
	    break;
	}
    case ACCEL_SCREENCOPY:
	{
	    int x1, y1, x2, y2, w, h;
	    x1 = va_arg(params, int);
	    y1 = va_arg(params, int);
	    x2 = va_arg(params, int);
	    y2 = va_arg(params, int);
	    w = va_arg(params, int);
	    h = va_arg(params, int);
	    (*driverspecs->accelspecs->ScreenCopy) (x1, y1, x2, y2, w, h);
	    break;
	}
    case ACCEL_PUTIMAGE:
	{
	    int x, y, w, h;
	    void *p;
	    x = va_arg(params, int);
	    y = va_arg(params, int);
	    w = va_arg(params, int);
	    h = va_arg(params, int);
	    p = va_arg(params, void *);
	    (*driverspecs->accelspecs->PutImage) (x, y, w, h, p);
	    break;
	}
    case ACCEL_DRAWLINE:
	{
	    int x1, x2, y1, y2;
	    x1 = va_arg(params, int);
	    y1 = va_arg(params, int);
	    x2 = va_arg(params, int);
	    y2 = va_arg(params, int);
	    (*driverspecs->accelspecs->DrawLine) (x1, y1, x2, y2);
	    break;
	}
    case ACCEL_SETFGCOLOR:
	{
	    int c;
	    c = va_arg(params, int);
	    (*driverspecs->accelspecs->SetFGColor) (c);
	    break;
	}
    case ACCEL_SETBGCOLOR:
	{
	    int c;
	    c = va_arg(params, int);
	    (driverspecs->accelspecs->SetBGColor) (c);
	    break;
	}
    case ACCEL_SETTRANSPARENCY:
	{
	    int m, c;
	    m = va_arg(params, int);
	    c = va_arg(params, int);
	    (*driverspecs->accelspecs->SetTransparency) (m, c);
	    break;
	}
    case ACCEL_SETRASTEROP:
	{
	    int r;
	    r = va_arg(params, int);
	    (*driverspecs->accelspecs->SetRasterOp) (r);
	    break;
	}
    case ACCEL_PUTBITMAP:
	{
	    int x, y, w, h;
	    void *p;
	    x = va_arg(params, int);
	    y = va_arg(params, int);
	    w = va_arg(params, int);
	    h = va_arg(params, int);
	    p = va_arg(params, void *);
	    (*driverspecs->accelspecs->PutBitmap) (x, y, w, h, p);
	    break;
	}
    case ACCEL_SCREENCOPYBITMAP:
	{
	    int x1, y1, x2, y2, w, h;
	    x1 = va_arg(params, int);
	    y1 = va_arg(params, int);
	    x2 = va_arg(params, int);
	    y2 = va_arg(params, int);
	    w = va_arg(params, int);
	    h = va_arg(params, int);
	    (*driverspecs->accelspecs->ScreenCopyBitmap) (x1, y1, x2, y2, w, h);
	    break;
	}
    case ACCEL_DRAWHLINELIST:
	{
	    int y, n, *x1, *x2;
	    y = va_arg(params, int);
	    n = va_arg(params, int);
	    x1 = va_arg(params, int *);
	    x2 = va_arg(params, int *);
	    (*driverspecs->accelspecs->DrawHLineList) (y, n, x1, x2);
	    break;
	}
    case ACCEL_SETMODE:
	{
	    int m;
	    /* This isn't sent to the chipset-specific driver. */
	    m = va_arg(params, int);
	    if ((accel_mode & BLITS_IN_BACKGROUND)
		&& !(m & BLITS_IN_BACKGROUND))
		/* Make sure background blits are finished. */
		(*driverspecs->accelspecs->Sync) ();
	    accel_mode = m;
	    break;
	}
    case ACCEL_SYNC:
	(*driverspecs->accelspecs->Sync) ();
	break;
    }				/* switch */

    va_end(params);

    vga_unlockvc();

    return 0;
}

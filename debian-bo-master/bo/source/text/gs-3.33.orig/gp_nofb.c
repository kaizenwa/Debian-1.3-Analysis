/* Copyright (C) 1993 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gp_nofb.c */
/* Dummy routines for Ghostscript platforms with no frame buffer management */
#include "gx.h"
#include "gp.h"
#include "gxdevice.h"

/* ------ Screen management ------ */

/* Initialize the console. */
void
gp_init_console(void)
{
}

/* Write a string to the console. */
void
gp_console_puts(const char *str, uint size)
{	fwrite(str, 1, size, stdout);
}

/* Make the console current on the screen. */
int
gp_make_console_current(gx_device *dev)
{	return 0;
}

/* Make the graphics current on the screen. */
int
gp_make_graphics_current(gx_device *dev)
{	return 0;
}

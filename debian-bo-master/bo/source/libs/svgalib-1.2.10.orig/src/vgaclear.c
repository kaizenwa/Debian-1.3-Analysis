/* VGAlib version 1.2 - (c) 1993 Tommy Frandsen                    */
/*                                                                 */
/* This library is free software; you can redistribute it and/or   */
/* modify it without any restrictions. This library is distributed */
/* in the hope that it will be useful, but without any warranty.   */

/* Multi-chipset support Copyright 1993 Harm Hanemaayer */
/* partially copyrighted (C) 1993 by Hartmut Schirmer */

#include <stdio.h>
#include <string.h>
#include "vga.h"
#include "libvga.h"

int vga_clear(void)
{
    vga_screenoff();

    if (MODEX)
	goto modeX;
    switch (CM) {
    case G320x200x256:
    case G320x240x256:
    case G320x400x256:
    case G360x480x256:
      modeX:
	/* write to all planes */
	port_out(0x02, SEQ_I);
	port_out(0x0F, SEQ_D);

	/* clear video memory */
	memset(GM, 0, 65536);
	break;

    default:
	switch (CI.colors) {
	case 2:
	case 16:
	    vga_setcolor(0);

	    /* write to all bits */
	    port_out(0x08, GRA_I);
	    port_out(0xFF, GRA_D);

	default:
	    {
		int i;
		int pages = (CI.ydim * CI.xbytes + 65535) >> 16;
#if defined(CONFIG_ALPHA_JENSEN)
		int j;
#endif

		for (i = 0; i < pages; ++i) {
		    vga_setpage(i);

#if defined(CONFIG_ALPHA_JENSEN)
		    for (j = 0; j < 65536; j += 2)
			gr_writew(0, j);
#else
		    /* clear video memory */
		    memset(GM, 0, 65536);
#endif
		}
	    }
	    break;
	}
	break;
    }

    vga_setcolor(15);

    vga_screenon();

    return 0;
}

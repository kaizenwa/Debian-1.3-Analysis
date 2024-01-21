/* 
 *     XaoS, a fast portable realtime fractal zoomer 
 *                  Copyright (C) 1996,1997 by
 *
 *      Jan Hubicka          (hubicka@paru.cas.cz)
 *      Thomas Marsh         (tmarsh@austin.ibm.com)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#include "aconfig.h"
#ifdef DOG_DRIVER
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include "allegro.h"
#include <string.h>
#include "zoom.h"
#include "gif.h"
#include "ui.h"
#include "palette.h"
#include "ctrl87.h"

static int mode = -1;
static zoom_context *context;
static int mousex, mousey, mousebuttons;
static width = 640, height = 480;
static int currentbuff = 0;
static int ncolors = 0;
BITMAP *buffers[2];

/*make startup faster */
int _stklen = 8192;
/*int _crt0_startup_flags = _CRT0_FLAG_LOCK_MEMORY; */
void __crt0_load_environment_file(char *_app_name)
{
    return;
}
/*
   void __crt0_setup_arguments(void)
   {
   return;
   } */

char **
 __crt0_glob_function(char *_arg)
{
    return 0;
}



typedef struct GFX_MODE_DATA {
    int w;
    int h;
    char *s;
} GFX_MODE_DATA;





static GFX_MODE_DATA gfx_mode_data[] =
{
    {320, 200, "320x200"},
    {320, 240, "320x240"},
    {640, 400, "640x400"},
    {640, 480, "640x480"},
    {800, 600, "800x600"},
    {1024, 768, "1024x768"},
    {1280, 1024, "1280x1024"},
    {1600, 1200, "1600x1200"},
    {256, 200, "256x200"},
    {256, 224, "256x224"},
    {256, 240, "256x240"},
    {256, 256, "256x256"},
    {320, 400, "320x400"},
    {320, 480, "320x480"},
    {360, 200, "360x200"},
    {360, 240, "360x240"},
    {360, 270, "360x270",},
    {360, 360, "360x360"},
    {360, 400, "360x400"},
    {360, 480, "360x480"},
    {376, 282, "376x282"},
    {376, 308, "376x308"},
    {376, 564, "376x564"},
    {400, 150, "400x150"},
    {400, 300, "400x300"},
    {400, 600, "400x600"}
};





/* gfx_mode_getter:
 *  Listbox data getter routine for the graphics mode list.
 */
static char *
 gfx_mode_getter(int index, int *list_size)
{
    if (index < 0) {
	if (list_size)
	    *list_size = sizeof(gfx_mode_data) / sizeof(GFX_MODE_DATA);
	return NULL;
    }
    return gfx_mode_data[index].s;
}





/* gfx_card_getter:
 *  Listbox data getter routine for the graphics card list.
 */
static char *
 gfx_card_getter(int index, int *list_size)
{


    static char *card[] =
    {
	"Autodetect",
	"VGA mode 13h",
	"Mode-X",
	"VESA 1.x",
	"VESA 2.0 (banked)",
	"VESA 2.0 (linear)",
	"VBE/AF",
	"Xtended mode",
	"ATI 18800/28800",
	"ATI mach64",
	"Cirrus 64xx",
	"Cirrus 54xx",
	"S3",
	"Trident",
	"Tseng ET3000",
	"Tseng ET4000",
	"Video-7"
    };



    if (index < 0) {
	if (list_size)
	    *list_size = sizeof(card) / sizeof(char *);
	return NULL;
    }
    return card[index];

}





static DIALOG gfx_mode_dialog[] =
{
   /* (dialog proc)     (x)   (y)   (w)   (h)   (fg)  (bg)  (key) (flags)  (d1)  (d2)  (dp) */
    {d_shadow_box_proc, 0, 0, 280, 150, 0, 0, 0, 0, 0, 0, NULL},
    {d_ctext_proc, 140, 8, 1, 1, 0, 0, 0, 0, 0, 0, "Graphics Mode"},
    {d_button_proc, 184, 97, 80, 16, 0, 0, 0, D_EXIT, 0, 0, "OK"},
    {d_button_proc, 184, 119, 80, 16, 0, 0, 27, D_EXIT, 0, 0, "Cancel"},
 {d_list_proc, 16, 28, 152, 107, 0, 0, 0, D_EXIT, 0, 0, gfx_card_getter},
  {d_list_proc, 184, 28, 80, 59, 0, 0, 0, D_EXIT, 0, 0, gfx_mode_getter},
    {NULL}
};



#define GFX_CANCEL         3
#define GFX_DRIVER_LIST    4
#define GFX_MODE_LIST      5



/* gfx_mode_select:
 *  Displays the Allegro graphics mode selection dialog, which allows the
 *  user to select a screen mode and graphics card. Stores the selection
 *  in the three variables, and returns zero if it was closed with the 
 *  Cancel button, or non-zero if it was OK'd.
 *  It was modified to use 320x200 as default
 */
static int my_gfx_mode_select(int *card, int *w, int *h)
{
    int ret;
    clear_keybuf();
    do {
    }
    while (mouse_b);
    centre_dialog(gfx_mode_dialog);
    set_dialog_color(gfx_mode_dialog, gui_fg_color, gui_bg_color);
    ret = do_dialog(gfx_mode_dialog, GFX_DRIVER_LIST);
    *card = gfx_mode_dialog[GFX_DRIVER_LIST].d1;

    *w = gfx_mode_data[gfx_mode_dialog[GFX_MODE_LIST].d1].w;
    *h = gfx_mode_data[gfx_mode_dialog[GFX_MODE_LIST].d1].h;


    if (ret == GFX_CANCEL)
	return FALSE;
    else
	return TRUE;

}




static void dog_print(int x, int y, char *text)
{
    show_mouse(NULL);
    textout(screen, font, text, x, y, 255);
    show_mouse(screen);
}



static void dog_display()
{
    show_mouse(NULL);
    blit(buffers[currentbuff], screen, 0, 0, 0, 0, SCREEN_W, SCREEN_H);
    show_mouse(screen);
}
static PALETTE palette;



void set_gui_colors()
{
    static RGB black =
    {0, 0, 0};
    static RGB grey =
    {48, 48, 48};
    static RGB white =
    {63, 63, 63};

    set_color(0, &black);
    set_color(16, &black);
    set_color(1, &grey);
    set_color(255, &white);
    gui_fg_color = 0;
    gui_bg_color = 1;
}




static int dog_set_color(int r, int g, int b, int init)
{
    if (init)
	ncolors = 2;
    if (ncolors >= 255)
	return (-1);
    palette[ncolors].r = r / 4;
    palette[ncolors].g = g / 4;
    palette[ncolors].b = b / 4;
    set_color(ncolors, &palette[ncolors]);
    return (ncolors++);
}

static void dog_rotate(int direction)
{
    int i;
    zoom_context *c;
    c = ui_getcontext();
    for (i = 3; i < c->num_colors + 2; i++) {
	palette[i].r = c->cmap[0][i] / 4;
	palette[i].g = c->cmap[1][i] / 4;
	palette[i].b = c->cmap[2][i] / 4;
    }
    set_palette(palette);
}


static void fix_palette()
{
    int i;
    for (i = 0; i < 2; i++) {
	palette[i].r = 0;
	palette[i].g = 0;
	palette[i].b = 0;
	set_color(i, &palette[i]);
    }
    palette[255].r = 63;
    palette[255].g = 63;
    palette[255].b = 63;
    set_color(255, &palette[255]);
    set_palette(palette);

}

static void dog_flip_buffers()
{
    currentbuff ^= 1;
}



static void dog_get_size(int *wi, int *he)
{
    int i, high;
    int w, c, h, vh;
    show_mouse(NULL);
  again:
    if ((!keypressed()) && (!joy_b1) && (!joy_b2)) {
	rest(500);
    }
    clear(screen);
    set_gui_colors();
    if (!my_gfx_mode_select(&c, &w, &h)) {
	goto again;
    }
    vh = h;
    if (set_gfx_mode(c, w, h, 0, 0) != 0) {
	set_gfx_mode(GFX_VGA, 320, 200, 0, 0);
	goto again;
    }
    width = SCREEN_W;
    height = SCREEN_H;
    clear(screen);
    show_mouse(screen);
    fix_palette();
    *wi = w;
    *he = h;
}



static void dog_uninitialise()
{

    text_mode(0);
    allegro_exit();
    install_keyboard();
    printf("IMPORTANT NOTE\n\n"
	   "One of the main purposes for making this port was to get some money for\n"
	   "buying better computer. XaoS on my 486 works too slowly. But later I decided\n"
	   "to release it freely to show GNU licence to MS-DOS users. So please if you like\n"
	   "this program and want to have more such software send small amount of money to:\n"
	   "Jan Hubicka\n"
	   "Dukeslkych bojovniku 1944\n"
	   "Tabor 390 03\n"
	   "Czech Republic\n\n"
	   "WWW PAGE: http://www.paru.cas.cz/~hubicka/XaoS\n"
	   "EMAIL:    hubicka@paru.cas.cz\n"
	   "TIP:      In xaos pres h for help :)\n\n"
	   "Press almost any key to continue\n"
	);
    while (keypressed());
    while (!keypressed());
    allegro_exit();
}


void dog_free(char *b1, char *b2)
{
    destroy_bitmap(buffers[0]);
    destroy_bitmap(buffers[1]);
}
int dog_alloc(char **b1, char **b2)
{
    buffers[0] = create_bitmap(SCREEN_W, SCREEN_H);
    buffers[1] = create_bitmap(SCREEN_W, SCREEN_H);
    currentbuff = 0;
    *b1 = buffers[0]->line[0];
    *b2 = buffers[1]->line[0];
    return width;
}

#if 0
static void resize()
{
    destroy_bitmap(buffers[0]);
    destroy_bitmap(buffers[1]);
    show_mouse(NULL);
    init_mode();
    currentbuff = 0;
    resize_to(context, width, height, buffers[0]->line[0], buffers[1]->line[0]);
    ui_updateparameters();
    ui_message();
    ui_do_fractal();
    ui_tbreak();
    fix_palette();


}

#endif				/*  */


static void dog_processevents(int wait, int *x, int *y, int *b, int *k)
{
    *x = mouse_x;
    *y = mouse_y;
    *b = 0;
    while (keypressed()) {
	if (ui_key(readkey()) == 2)
	    return;
    }
    if (mouse_b == 1)
	*b |= BUTTON1;
    if (mouse_b == 2)
	*b |= BUTTON3;
    if (mouse_b == 3 || mouse_b & 4)
	*b |= BUTTON2;
    *k = (key[KEY_LEFT] != 0) + 2 * (key[KEY_RIGHT] != 0) + 4 * (key[KEY_UP] != 0) + 8 * (key[KEY_DOWN] != 0);

}

static void dog_getmouse(int *x, int *y, int *b)
{
    *x = mouse_x;
    *y = mouse_y;
    *b = 0;
    if (mouse_b == 1)
	*b |= BUTTON1;
    if (mouse_b == 2)
	*b |= BUTTON3;
    if (mouse_b == 3 || mouse_b & 4)
	*b |= BUTTON2;
}





int dog_init()
{
    allegro_init();
    _control87(MCW_EM, MCW_EM);
    if (windows_version == 3) {

	printf("\nYou seem to be running me under Windows 3.1. This is a Bad Thing.\nYour operating system is broken and needs to be replaced...\n\n");
	sleep(1);
    }
    install_keyboard();
    install_mouse();
    install_timer();

    set_gfx_mode(GFX_VGA, 320, 200, 0, 0);
    signal(SIGFPE, SIG_IGN);
    fix_palette();
    return (1);
}

static char *helptext[] =
{
    "CHAPPI DRIVER VERSION 2.2              ",
    "=========================              ",
    " This is fully featured driver for     ",
    " djgpp and allegro. Please read        ",
    " README.DOS for more informations about",
    " GNU. It has few limitations:          ",
    " o Trident and ATI driver don't work   ",
    "    Please use some vesa bios extension",
    "    on this videocards. (program called",
    "    univbe is recomended.) This is not ",
    "    xaos bug but problem of allegro    ",
    "    (the graphics library that xaos    ",
    "     uses)                             ",
    " o Some DPMI servers may cause problems",
    "    Some DPMI servers like one from    ",
    "    novell dos are buggy. Use clean DOS",
    "    instead and XaoS will automatically",
    "    start cwsdpmi                      ",
    " o Higher resolutions don't work       ",
    "    You have unsuported videocard.     ",
    "    Try to use univbe or other vesa    ",
    "    bios extensions                    ",
    " o XaoS needs coprocesor               ",
    "    I don't distribute coprocesor      ",
    "    library linked into XaoS because it",
    "    is too slow for realtime zoomer.   ",
    "    Coprocesor emulation will not help,",
    "    because xaos works in protected    ",
    "    mode.                              ",
    " o W/O mouse driver is XaoS bit        ",
    "    unusable                           ",
    "                                       ",
    " chappi driver was done by Jan Hubicka ",
    "              (C) 1997                 ",
};

#define UGLYTEXTSIZE (sizeof(helptext)/sizeof(char *))
static struct params params[] =
{
    {NULL, 0, NULL, NULL}
};

struct ui_driver dog_driver =
{
    "MS-DOG",
    dog_init,
    dog_get_size,
    dog_processevents,
    dog_getmouse,
    dog_uninitialise,
    dog_set_color,
    dog_print,
    dog_display,
    dog_alloc,
    dog_free,
    dog_flip_buffers,
    dog_rotate,
    256,
    8,
    helptext,
    UGLYTEXTSIZE,
    params,
    FULLSCREEN | UPDATE_AFTER_RESIZE | PALETTE_ROTATION | ROTATE_INSIDE_CALCULATION,
    0.0, 0.0,
    0, 0
};

#endif

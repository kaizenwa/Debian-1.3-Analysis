#include "aconfig.h"
#ifdef TEMPLATE_DRIVER
/*includes */
#include "zoom.h"
#include "ui.h"

static int template_set_color(int r, int g, int b, int init)
{
    return ( /*pixel value or -1 for full palette */ -1);
}

static void template_print(int x, int y, char *text)
{
}

static void template_display()
{
}

static void template_flip_buffers()
{
}
void template_free_buffers(char *b1, char *b2)
{
}
int template_alloc_buffers(char **b1, char **b2)
{
}
static void template_getsize(int *w, int *h)
{
}

static void template_processevents(int wait, int *mx, int *my, int *mb, int *k)
{
}
int template_init()
{
    return ( /*1 for sucess 0 for fail */ 1);
}
void template_uninitialise()
{
}
void template_getmouse(int *x, int *y, int *b)
{
}
static char *helptext[] =
{
    "TEMPLATE DRIVER VERSION 1.0            ",
    "======================                 ",
    "blah....                               ",
    "                                       ",
    "                                       ",
};
#define UGLYTEXTSIZE (sizeof(helptext)/sizeof(char *))
static struct params params[] =
{
    {"-flag", P_SWITCH, &variable, "Example flag..."},
    {NULL, 0, NULL, NULL}
};

struct ui_driver x11_driver =
{
    "Template",
    template_init,
    template_getsize,
    template_processevents,
    template_getmouse,
    template_uninitialise,
    template_set_color,
    template_print,
    template_display,
    template_alloc_buffers,
    template_free_buffers,
    template_flip_buffers,
    template_rotate_palette,
    256,			/*maximum palette size */
    8,				/*text height */
    helptext,
    UGLYTEXTSIZE,
    params,
    0,				/*flags...see ui.h */
    0.0, 0.0,			/*width/height of screen in centimeters */
    0, 0,			/*resolution of screen for windowed systems */
};

#endif

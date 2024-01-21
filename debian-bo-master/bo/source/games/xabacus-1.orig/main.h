#include "maindefs.h"

typedef unsigned int Column;		/* int's MUST be more than 9 bits wide*/

extern Column column[MAXCOLS];

extern Display *display;
extern Window base, demobase, child, root;
extern int screen;
Widget topLevel;
extern XSizeHints size_hints;
extern XFontStruct *font_info;
extern GC gc[MAXCOLORS];
extern Cursor Hand;
extern unsigned int mask;
extern char colors[MAXCOLORS][COLORSTRLEN];
extern unsigned int width, height, demowidth,demoheight, posx, posy;
extern int framewidth, beadwidth, beadheight, midframey, ncols, colgap, rowgap;
extern Boolean demo;
extern Boolean script;
extern char demofont[128];

extern struct _resources {
	String display;
    int beadwidth;
    int beadheight;
    int framewidth;
    int ncols;
    String colors[4];
	String demo;
	String demofont;
	Boolean script;
} app_resources;
 

/* Some global variables for the communication with the X server */

#ifndef _GLOBAL_H
#define _GLOBAL_H

char *myname;
Display *display;
int screen;
Window root;

/* The font structure to hold the default font */
XFontStruct *default_font; 

#endif
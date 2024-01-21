/*
   windows.h
   Header file with variable/function declarations for windows.c
*/

#ifndef _WINDOWS_H
#define _WINDOWS_H

/* Data structure for windows */

typedef struct {
  char name[40];    
  int x, y;
  int w, h;
  Window window; 
  XSizeHints hints;
  XFontStruct *font;
  GC gc;
} Win;

extern void create_window (Win *win, Window parent, int x, int y, int w, 
  int h, char *label, unsigned long bg, unsigned long fg, 
  XFontStruct *font);
extern int map_window (Win *win, int raised);
extern void discard_exposures (Window window);

#endif
              









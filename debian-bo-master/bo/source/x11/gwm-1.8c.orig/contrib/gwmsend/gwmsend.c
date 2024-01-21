/* 
   gwmsend.c
   Author: Anders Holst (aho@nada.kth.se)
   Copyright (C) 1994  Anders Holst 
     This file is copyrighted under the same terms as the rest of GWM
     (see the X Inc license for details). There is no warranty that it
     works. 
   Compiles with:
     gcc -o gwmsend gwmsend.c -L/usr/lib/X11R6 -lX11
   Usage:
     gwmsend 'WOOL-expression'
 */

/* Colas Nahaboo <colas@sa.inria.fr> 22 July 1995:
 * XChangeProperty now in PropModeAppend to avoid race conditions
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xatom.h>
#include <X11/X.h>
#include <X11/Xlib.h>

Display* display;
char* dispName;
Atom gwmprop;
char buf[512];

void die(char* str) 
{ 
  fprintf(stderr, str);
  exit(1);
}

int initDisplay()
{
  if (!(dispName = getenv("DISPLAY")))
    return 0;
  if (!(display = XOpenDisplay(dispName)))
    return 0;
  gwmprop = XInternAtom(display, "GWM_EXECUTE", 1); 
  if (gwmprop == None)
    return 0;
  return 1;
}

void sendGwm(char* str)
{
  int len;
  sprintf(buf, "(? %s \"\\n\")", str);
  len = strlen(buf);
  XChangeProperty(display, DefaultRootWindow(display),
                  gwmprop, XA_STRING, 8, PropModeAppend, buf, len);
  XFlush(display); 
}

void main(int argc, char** argv)
{
  if (argc != 2)
    die("Usage: gwmsend 'expression'\n");
  if (!initDisplay())
    die("Could not connect to server. Check your DISPLAY environment variable.\n");
  sendGwm(argv[1]);
}

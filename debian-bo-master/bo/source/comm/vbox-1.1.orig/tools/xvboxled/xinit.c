/* 
   xinit.c
   General initialization functions for the communication with the
   X server.
*/

#include <stdio.h>
#include <X11/Xlib.h>

#include "global.h"

/*---------------------------------------------------------------------*/
/* This function tries to open the connection to the X server and open */
/* the display <displayname>.                                          */
/* Returns nothing (exits if not successful)                           */
/*---------------------------------------------------------------------*/
void connect_server (char *displayname) {
  /* first of all, try to open the display. */
  display = XOpenDisplay(displayname);
  /* If this didn't succeed (XOpenDisplay returned NULL pointer), there */
  /* is no hope. Write error message to stderr and exit.                */
  if (display == NULL) {
    fprintf(stderr,"%s: cannot connect to X server %s\n", myname,
      XDisplayName(displayname));
    exit(-1);
  }
  /* Now set some other global variables. */
  screen = DefaultScreen(display);
  root = RootWindow(display,screen);
  fprintf(stderr,"%s: connection to X server %s established.\n", myname,
    XDisplayName(displayname));
  return;
}


/*----------------------------------------------------------------------*/
/* The counterpart to connect_server (obviously). Disconnects. Returns  */
/* nothing. Simple.                                                     */
/*----------------------------------------------------------------------*/
void disconnect_server (void) {
  fprintf(stderr,"%s: disconnecting.\n",myname); 
  XCloseDisplay(display);
}

/*----------------------------------------------------------------------*/
/* This loads a font to the given XFontStruct. Tries to load fixed font */
/* if the font given as <fontname> couldn't be loaded.                  */
/* Returns 0 if not even the fixed font could be loaded, otherwise 1.   */
/*----------------------------------------------------------------------*/
int load_font (XFontStruct **font, char *fontname) {
  *font = XLoadQueryFont(display,fontname);
  if (*font == NULL) {
    fprintf(stderr,"%s: cannot load font %s, trying fixed...\n", myname,
      fontname);
    *font = XLoadQueryFont(display,"fixed");
  }
  if (*font == NULL) {
    fprintf(stderr,"%s: cannot load fixed font either.\n", myname);
    return(0);
  }
  return(1);
}
                                                  









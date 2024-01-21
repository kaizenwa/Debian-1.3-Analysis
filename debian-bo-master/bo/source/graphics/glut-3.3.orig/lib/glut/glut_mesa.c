
/* Copyright (c) Mark J. Kilgard, 1996. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */

#include <stdlib.h>
#include <string.h>
#include "glutint.h"

int __glutMesaSwapHackSupport = 0;  /* Not supported until
                                       proven otherwise. */

/* Use the "Mesa swap hack" if reasonable if and only if
   MESA_SWAP_HACK is set to something whose first character is
   not "N" or "n" AND "Brian Paul" is the vendor string AND
   "Mesa" is the renderer string.

   Anyone who modifies Mesa so that glXSwapBuffers does not
   simply blit the previously rendered back buffer should
   change either their vendor or renderer string to avoid
   confusing GLUT. */

void
__glutDetermineMesaSwapHackSupport(void)
{
  static int doneAlready = 0;
  char *env, *vendor, *renderer;

  if (doneAlready)
    return;
  env = getenv("MESA_SWAP_HACK");
  if (env) {
    if ((env[0] != 'n') && (env[0] != 'N')) {
      vendor = (char *) glGetString(GL_VENDOR);
      renderer = (char *) glGetString(GL_RENDERER);
      if (!strcmp(vendor, "Brian Paul") && !strcmp(renderer, "Mesa"))
        __glutMesaSwapHackSupport = 1;
    }
  }
  doneAlready = 1;
}

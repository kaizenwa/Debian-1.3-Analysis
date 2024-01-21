#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <malloc.h>
#include <string.h>
#include <fcntl.h>

#include <X11/xpm.h>
#include <X11/Xlib.h>
#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/xpm.h>
#include <X11/cursorfont.h>
#include <X11/keysymdef.h>
#include <X11/Xatom.h>

#define XK_MISCELLANY
#include <sys/types.h>
#include <sys/time.h>
#ifdef ISC
#include <sys/bsdtypes.h> /* Saul */
#endif
#if defined ___AIX || defined _AIX || defined __QNX__ || defined ___AIXV3 || defined AIXV3 || defined _SEQUENT_
#include <sys/select.h>
#endif

#include "Widgets/Widget.h"
#include "../../fvwm/module.h"
#include "../../configure.h"
#include "../../libs/fvwmlib.h"       
#include "../../version.h"



/* Declaration des types utilisees */
typedef struct
{
  Display *display ;
  int screen ;
  Window root ;
  Window win ;
  Colormap colormap ;
  GC gc ;
  int depth ;
  unsigned long WhitePix ;
  unsigned long BlackPix ;
  XColor TabColor[6];
  XSizeHints size;
  char *backcolor;
  char *forecolor;
  char *shadcolor;
  char *licolor;
  char *font;
  char *icon;
  XFontStruct *xfont;
  char *title;
} X11base ;








/* the pixmap paste thingy */

#include "xb_config.h"

#include <stdio.h>

#include <X11/Xlib.h>

#ifdef HAS_XPM
#include <X11/xpm.h>
#endif

#include "xbanner.h"

#define MAX_PATH_LEN 1024

/* defined outside the #ifdef to avoid ANSI's forbidden empty source files */
int	px = PX_DEFVAL,				/* X location of the image */
	py = PY_DEFVAL,				/* Y location of the image */
	ph,pw;					/* pixmap width/height     */
char	PIXFILE[MAX_PATH_LEN]=PIXNAME_DEFVAL;	/* filename...		   */
char	BGPIXFNAME[MAX_PATH_LEN]=BGPIXFN_DEFVAL;	/* filename for default bg */

Bool	dopix=DOPIX_DEFVAL;			/* do we put one on or not?*/

#ifdef HAS_XPM

void DoPasteOnePixmap(char *fname,int px,int py)
{
  int 		stat;		/* status returned from XpmReadFileToPixmap */
  Pixmap	pix,shape;	/* the pixmap and shapemask		    */
  XGCValues	gcv;		/* needed to set a clipping region	    */
  XpmAttributes xpm_attr;	/* needed to ask for size and set closeness */

  xpm_attr.valuemask = XpmCloseness|XpmSize; /* ask for size and set closeness */
  xpm_attr.closeness = CLOSENESS_DEFVAL;     /* wanted closeness value */

  /* do it! and check the results */
  stat=XpmReadFileToPixmap(disp,rootpix,fname,&pix,&shape,&xpm_attr);
  switch(stat)
  {
    case XpmFileInvalid:
      error(disp,mgc,"XpmFileInvalid - Invalid XPM file",False);
      return;
      break;
    case XpmOpenFailed:
      error(disp,mgc,"XpmOpenFailed - Can't open pixmap file",False);
      return;
      break;
    case XpmNoMemory:
      error(disp,mgc,"XpmNoMemory - Memory allocation error trying to load pixmap",False);
      return;
      break;
    case XpmColorError:
      error(disp,mgc,"XpmColorError - Color allocation error",False);
      return;
      break;
    case XpmColorFailed:
      error(disp,mgc,"XpmColorFailed - Could not allocate correct colors for pixmap",False);
      return;
      break;
    default:
      break;
  }
  
  /* for ease of use */
  ph = xpm_attr.height;
  pw = xpm_attr.width;

  /* did the image have a "None" color? */
  if(shape!=0)		/* I am not sure about this, but it seems to work */
  {
    gcv.clip_mask=shape;	/* set clip mask */
    gcv.clip_x_origin=px;	/* and clip origin */
    gcv.clip_y_origin=py;
    XChangeGC(disp,mgc,GCClipMask|GCClipXOrigin|GCClipYOrigin,&gcv);
  }

  /* paste it on screen - straight to the root window */
  XCopyArea(disp,pix,root,mgc,0,0,pw,ph,px,py);

  /* free stuff - not colors, though */
  if(shape!=0)		/* I am not sure about this, but it seems to work */
    XFreePixmap(disp,shape);
  XFreePixmap(disp,pix);
  XpmFreeAttributes(&xpm_attr);
}

void DoPastePixmap(void)
{
  FILE *fin;
  char path[MAX_PATH_LEN];
  char line[MAX_PATH_LEN+10]; /* two 4-digit numbers and space following name */
  int pix_x,pix_y;

  /* do we really have to do this??? */
  if(dopix==False)
    return;		/* guess not */

  if(PIXFILE[0]!='@')
  {
    DoPasteOnePixmap(PIXFILE,px,py);
    return;
  }

  fin = fopen(PIXFILE+1,"r");
  if(fin==NULL || ferror(fin))
  {
    error(disp,DefaultGC(disp,DefaultScreen(disp)),"Can't read PIXlist",False);
    return;
  }
  while((fgets(line,MAX_PATH_LEN+10,fin))!=NULL)
  {
    sscanf(line,"%s %d %d",path,&pix_x,&pix_y);
    DoPasteOnePixmap(path,pix_x,pix_y);
  }
  fclose(fin);
}

void DoTilePixmap(Display *disp, Window w)
{
  int 		stat;		/* status returned from XpmReadFileToPixmap */
  Pixmap	pix,shape;	/* the pixmap and shapemask		    */
  XpmAttributes xpm_attr;	/* needed to ask for size and set closeness */

  xpm_attr.valuemask = XpmCloseness;		/* set closeness */
  xpm_attr.closeness = CLOSENESS_DEFVAL;	/* wanted closeness value */

  /* do it! and check the results */
  stat=XpmReadFileToPixmap(disp,w,BGPIXFNAME,&pix,&shape,&xpm_attr);
  switch(stat)
  {
    case XpmFileInvalid:
      error(disp,mgc,"XpmFileInvalid - Invalid XPM file",False);
      return;
      break;
    case XpmOpenFailed:
      error(disp,mgc,"XpmOpenFailed - Can't open pixmap file",False);
      return;
      break;
    case XpmNoMemory:
      error(disp,mgc,"XpmNoMemory - Memory allocation error trying to load pixmap",False);
      return;
      break;
    case XpmColorError:
      error(disp,mgc,"XpmColorError - Color allocation error",False);
      return;
      break;
    case XpmColorFailed:
      error(disp,mgc,"XpmColorFailed - Could not allocate correct colors for pixmap",False);
      return;
      break;
    default:
      break;
  }

  /* set the tile */
  XSetWindowBackgroundPixmap(disp,root,pix);

  /* now fill a rectangle the size of the entire screen */
  XClearWindow(disp,w);

  /* free stuff - not colors, though */
  if(shape!=0)		/* I am not sure about this, but it seems to work */
    XFreePixmap(disp,shape);
  XFreePixmap(disp,pix);
  XpmFreeAttributes(&xpm_attr);
}

#endif /* ifdef HAS_XPM */

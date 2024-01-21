#include "Picture.h"
#include "objects.h"

void Picture::load(const char *name, int index) {
	static char *dir = strdup(
		access(XBILL_HOME "pixmaps/logo.xpm", R_OK) ? "" : XBILL_HOME);
	int i;
	char file[255];
	Pixmap mask;
	XpmAttributes attr;
	unsigned long gcmask;
	XGCValues gcval;
	gcmask = GCForeground|GCBackground|GCGraphicsExposures;
	gcval.graphics_exposures = False;
	attr.valuemask= XpmCloseness | XpmReturnPixels | XpmColormap | XpmDepth;
	attr.closeness = 65535;
	attr.colormap = ui.colormap;
	attr.depth = ui.depth;
	if (index>=0)
		sprintf (file, "%spixmaps/%s_%d.xpm", dir, name, index);
	else sprintf
		(file, "%spixmaps/%s.xpm", dir, name);
	i = XpmReadFileToPixmap(ui.display, ui.rootwindow, file, &pix,
		&mask, &attr);
	if (i<0) {
		printf ("cannot open %s\n", file);
		exit(1);
	}
	gc = XCreateGC (ui.display, ui.offscreen, gcmask, &gcval);
	XSetClipMask(ui.display, gc, mask);
	width = attr.width;
	height = attr.height;
}

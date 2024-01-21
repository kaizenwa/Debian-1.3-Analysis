#include "MCursor.h"
#include "objects.h"

void MCursor::load(const char *name, int masked) {
	static char *dir = strdup(
		access(XBILL_HOME "pixmaps/logo.xpm", R_OK) ? "" : XBILL_HOME);
	Pixmap bitmap, mask;
	int i, xh, yh;
	unsigned width, height;
	char file[255];
	char mfile[255];
	sprintf (file, "%sbitmaps/%s.xbm", dir, name);
	i = XReadBitmapFile (ui.display, ui.rootwindow, file,
		&width, &height, &bitmap, &xh, &yh);
	if (i == BitmapOpenFailed) {
		printf ("cannot open %s\n", file);
		exit(1);
	}
	if (masked == SEP_MASK) {
		sprintf (mfile, "%sbitmaps/%s_mask.xbm", dir, name);
		i = XReadBitmapFile (ui.display, ui.rootwindow,
			mfile, &width, &height, &mask, &xh, &yh);
	}
	else
		mask = bitmap;
	if (i == BitmapOpenFailed) {
		printf ("cannot open %s\n", file);
		exit(1);
	}
	cursor = XCreatePixmapCursor(ui.display, bitmap, mask,
		&ui.black, &ui.white, width/2, height/2);
}


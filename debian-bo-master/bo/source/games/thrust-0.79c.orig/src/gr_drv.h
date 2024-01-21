
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifndef GR_DRV_H
#define GR_DRV_H

#ifdef __STDC__
void clearscr(void);
void putarea(unsigned char *source,
	     int x, int y, int width, int height, int bytesperline,
	     int destx, int desty);
void puthline(unsigned char *source, int x, int y, int length);
void putpixel(int x, int y, unsigned char color);
int getpixel(int x, int y);
void syncscreen(void);
void displayscreen(void);
void fade_in(void);
void fade_out(void);
void fadepalette(int first, int last,
		 unsigned char *RGBtable,
		 int fade, int flag);
void graphics_preinit(void);
int graphicsinit(int argc, char **argv);
int graphicsclose(void);
char *graphicsname(void);
#endif

#endif
